use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::Range;
use std::ops::RangeInclusive;
use std::sync::Arc;

use dprint_core::configuration::NewLineKind;
use dprint_core::configuration::resolve_new_line_kind;
use dprint_swc_ext::common::RootNode;
use dprint_swc_ext::common::SourceRanged;
use dprint_swc_ext::common::SourceRangedForSpanned;
use dprint_swc_ext::common::StartSourcePos;
use dprint_swc_ext::view::*;

use crate::Result;

use super::configuration::Configuration;
use super::configuration::SortOrder;
use super::format_text::ExternalFormatter;
use super::format_text::FormatTextOptions;
use super::format_text::format_text;
use super::generation::StmtGroupKind;
use super::generation::get_stmt_group_kind;
use super::generation::get_stmt_range_with_decorators;
use super::parsing::ParseOptions;
use super::parsing::parse_program;
use super::utils::file_text_has_ignore_comment;

/// Start of the parsed text, which the byte indexes into it are relative to.
const START: StartSourcePos = StartSourcePos::START_SOURCE_POS;

/// Formats only the part of the text within the provided byte range.
///
/// The range is in bytes of the text, including any byte order mark. It's widened to the lines of
/// the members it touches in the innermost statement list, switch statement, class, interface,
/// type literal, enum, object literal, or array literal without holes that contains it, along with
/// the comments it touches around them, and the text outside of those lines is left as it was.
/// Members sharing a line with the touched ones are formatted along with them. The lines are what
/// formatting the whole file gives, so when they don't fit in with the text around them (ex. the
/// container is formatted on a single line or gets indented differently, or the file would format
/// differently with them in place), the member holding the container is formatted instead, and so
/// on out to the whole file, which is also what happens once a few of those don't fit since each
/// try formats the whole file again.
///
/// Import declarations are only sorted or grouped when the range touches all of the module's ones
/// that formatting would sort, since sorting some of them moves declarations outside of the range,
/// and the same goes for re-export declarations. Declarations with an ignore comment don't count
/// since they're never sorted.
///
/// A range that only touches the space, comments, or empty statements next to members on its line
/// formats those members, and one on the line of a container's bracket formats the member holding
/// the container, while one that only touches the lines between members formats nothing. A range
/// covering the whole text formats the whole file like [`format_text`]. The byte order mark is kept
/// when only part of the file is formatted and removed like [`format_text`] does when the whole
/// file is. A file with carriage return only line endings is always formatted as a whole.
///
/// Returns the file text, or `None` when nothing changed or the file has the ignore file comment.
/// Since the lines are taken from formatting the whole file, this errors when anything in the file
/// fails to parse or format, including outside of the range (ex. an embedded template the external
/// formatter fails on).
pub fn format_text_range(options: FormatTextOptions, range: Range<usize>) -> Result<Option<String>> {
  let FormatTextOptions {
    path,
    extension,
    text,
    config,
    external_formatter,
    arena_pool,
  } = options;
  // the whole file is formatted more than once, so this keeps the embedded templates from being
  // formatted again each time
  let cache = RefCell::new(HashMap::new());
  let cached_external_formatter = external_formatter.map(|external_formatter| {
    move |extension: &str, text: String, config: &Configuration| format_embedded_with_cache(&cache, external_formatter, extension, text, config)
  });
  let external_formatter = cached_external_formatter.as_ref().map(|formatter| formatter as &ExternalFormatter);
  let format_with_config = |text: Arc<str>, config: &Configuration| {
    format_text(FormatTextOptions {
      path,
      extension,
      text,
      config,
      external_formatter,
      arena_pool,
    })
  };
  if file_text_has_ignore_comment(&text, &config.ignore_file_comment_text) {
    return Ok(None);
  }
  let bom_len = if text.starts_with('\u{FEFF}') { '\u{FEFF}'.len_utf8() } else { 0 };
  let body: Arc<str> = if bom_len == 0 { text.clone() } else { text[bom_len..].into() };
  let end = range.end.saturating_sub(bom_len).min(body.len());
  let range = range.start.saturating_sub(bom_len).min(end)..end;
  if range.start == 0 && range.end == body.len() {
    return format_with_config(text, config);
  }

  let parsed_source = parse_program(ParseOptions {
    path,
    extension,
    text: body.clone(),
  })?;
  let unsorted_config;
  let config = match parsed_source.with_view(|program| touches_all_sorted_declarations(program, &range, config)) {
    (true, true) => config,
    (touches_imports, touches_exports) => {
      unsorted_config = without_sorting(config, touches_imports, touches_exports);
      &unsorted_config
    }
  };
  let format = |text: Arc<str>| format_with_config(text, config);
  if has_lone_carriage_return(&body) {
    return format(text);
  }
  let Some(levels) = parsed_source.with_view(|program| find_levels(program, &body, &range, config)) else {
    return Ok(None);
  };
  let Some(formatted) = format(body.clone())? else {
    // the whole file is already formatted, so the range is too
    return Ok(None);
  };
  let Ok(formatted_source) = parse_program(ParseOptions {
    path,
    extension,
    text: formatted.as_str().into(),
  }) else {
    return Ok(Some(formatted));
  };
  let formatted_placements = formatted_source.with_view(|program| {
    levels
      .iter()
      .map(|level| find_formatted_placement(program, &formatted, level))
      .collect::<Vec<_>>()
  });

  let indent_width = config.indent_width as usize;
  // each check formats the whole file, so when the lines keep not fitting (ex. formatting the file
  // isn't stable), this gives up on formatting part of it after a few
  let mut checks_left = 3;
  for (level, formatted_placement) in levels.iter().zip(formatted_placements).rev() {
    let Some(formatted_placement) = formatted_placement else {
      continue;
    };
    if level.container_moves(&body, &formatted, &formatted_placement, indent_width) {
      continue;
    }
    let Some(result) = replace_lines(&body, &level.original, &formatted, &formatted_placement) else {
      continue;
    };
    if result == *body {
      return Ok(None);
    }
    // the lines might not fit in with the text around them (ex. a comment moves to the other side
    // of a member or blank lines around them change), so they're only kept when the file still
    // formats the same with them in place
    let formats_same = match format(result.as_str().into()) {
      Ok(Some(reformatted)) => reformatted == formatted,
      Ok(None) => result == formatted,
      Err(_) => false,
    };
    if formats_same {
      return Ok(Some(format!("{}{}", &text[..bom_len], result)));
    }
    checks_left -= 1;
    if checks_left == 0 {
      break;
    }
  }
  Ok(Some(formatted))
}

/// Gets whether the range touches all of the module's import declarations that formatting can
/// sort, and the same for its re-export declarations.
fn touches_all_sorted_declarations(program: Program, range: &Range<usize>, config: &Configuration) -> (bool, bool) {
  let Program::Module(module) = program else {
    return (true, true);
  };
  let (mut imports, mut exports) = (true, true);
  for item in module.body {
    let node = Node::from(item);
    let touched = || touches(&node.range().as_byte_range(START), range);
    match get_stmt_group_kind(node, program, config) {
      StmtGroupKind::Imports => imports &= touched(),
      StmtGroupKind::Exports => exports &= touched(),
      StmtGroupKind::Other => {}
    }
  }
  (imports, exports)
}

/// Gets the configuration without sorting or grouping the import or re-export declarations the
/// range doesn't all touch.
fn without_sorting(config: &Configuration, touches_imports: bool, touches_exports: bool) -> Configuration {
  let mut config = config.clone();
  if !touches_imports {
    config.module_sort_import_declarations = SortOrder::Maintain;
    config.module_import_groups = Vec::new();
    // the clone shares the groups compiled from the original configuration
    config.module_import_groups_cache = Default::default();
  }
  if !touches_exports {
    config.module_sort_export_declarations = SortOrder::Maintain;
  }
  config
}

/// A node holding a list of members (ex. a block's statements) along with the members of it to
/// format.
struct Level {
  /// How to get from the program to the node, which finds it again in the formatted text.
  path: Vec<Step>,
  /// Kind of the node, which is checked along with the kinds of its members to know the node in
  /// the formatted program is the same one.
  kind: NodeKind,
  /// Kinds of the members, which are compared using [`comparable_kind`].
  member_kinds: Vec<NodeKind>,
  /// Indexes of the members to format, which are the touched ones, or the ones beside the range on
  /// its line, along with the ones on their lines. For all but the innermost level, the only
  /// touched one is the member holding the next level.
  to_format: RangeInclusive<usize>,
  /// The comments before the first member to format along with it.
  leading_comments: LeadingComments,
  /// How many of the comments on their own lines after the last member to format along with it,
  /// which are the ones the range touches.
  trailing_comments: usize,
  /// Where the members to format are in the original text.
  original: Placement,
}

impl Level {
  /// Gets whether formatting moves the container's own lines (ex. wraps it in parentheses), which
  /// would leave the formatted lines indented differently than the ones around them.
  fn container_moves(&self, text: &str, formatted_text: &str, formatted: &Placement, indent_width: usize) -> bool {
    let line_moves =
      |index: usize, formatted_index: usize| line_indent(text, index, indent_width) != line_indent(formatted_text, formatted_index, indent_width);
    let container = &self.original.container;
    !self.path.is_empty()
      && (line_moves(container.start, formatted.container.start)
        // a switch case doesn't have a closing token, so its end is on its last member's lines
        || self.kind != NodeKind::SwitchCase && line_moves(container.end - 1, formatted.container.end - 1))
  }
}

/// A step from a node to one of its members or children, along with the kind of node it goes to.
#[derive(Clone, Copy)]
enum Step {
  Member(usize, NodeKind),
  Child(usize, NodeKind),
}

/// Which of the comments before the first member to format along with it.
#[derive(Clone, Copy)]
enum LeadingComments {
  /// The given number of the comments closest to the member, which are the ones the range touches
  /// along with any needed for the lines to start at a comment or the member (ex. the comment in
  /// `/* a\n */ const b = 1;`).
  Closest(usize),
  /// The ones attached to the member without a blank line between, which formatting moves along
  /// with it when it groups imports.
  Attached,
}

/// Where a level's members to format are in a text.
struct Placement {
  /// From the start of the first member, or the first comment before it to format, to the end of
  /// the last member, or the last comment after it to format.
  members: Range<usize>,
  /// Start of the first member, after the comments before it.
  first_member_start: usize,
  /// End of the last member, which is before the comments after it.
  last_member_end: usize,
  /// End of the last member before its semicolon when that's on a later line (ex. the one before
  /// `[1]` in `foo()\n;[1].forEach()`), where it goes with what comes after.
  end_before_semicolon: usize,
  /// Index of the semicolon right before the first member when it's on the first line of the
  /// members or a later one (ex. the one before `[1]` in `foo()\n;[1].forEach()`), which ends the
  /// member before or is an empty statement.
  previous_semicolon: Option<usize>,
  /// Whether the previous semicolon ends the member before, which means removing it would join the
  /// members (ex. `foo()\n[1].forEach()`).
  previous_semicolon_ends_member: bool,
  /// Byte range of the node holding the members.
  container: Range<usize>,
}

/// A member of a node, which leaves out empty statements since formatting removes them.
struct Member<'a> {
  node: Node<'a>,
  /// Byte range of the member, including the decorators before `export`.
  range: Range<usize>,
  /// Start of the member or the empty statements before it.
  start_with_empty_statements: usize,
}

/// Finds each level from the program to the innermost one containing the range, or `None` when
/// the range only touches the lines between members or the program doesn't have any.
fn find_levels(program: Program, text: &str, range: &Range<usize>, config: &Configuration) -> Option<Vec<Level>> {
  let mut levels = Vec::new();
  let mut path = Vec::new();
  let mut container = Node::from(program);
  loop {
    let members = members(container)?;
    let mut touched = members
      .iter()
      .enumerate()
      .filter(|(_, member)| touches(&member.range, range))
      .map(|(index, _)| index);
    // the line of a bracket goes with the member holding the container (ex. the cursor in `{|\n`)
    if !levels.is_empty() && reaches_bracket_line(program, text, container, &members, range) {
      return Some(levels);
    }
    let (first, last) = match touched.next() {
      Some(first) => (first, touched.next_back().unwrap_or(first)),
      None => members_beside_on_line(program, text, &members, range)?,
    };
    let to_format = with_members_on_same_lines(program, text, &members, first, last);
    let first_member = &members[*to_format.start()];
    let leading_comments = if matches!(container, Node::Module(_))
      && !config.module_import_groups.is_empty()
      && get_stmt_group_kind(first_member.node, program, config) == StmtGroupKind::Imports
    {
      LeadingComments::Attached
    } else {
      closest_leading_comments(text, &comments_before(program, text, first_member), first_member, range)
    };
    let last_member = &members[*to_format.end()];
    let trailing_comments = comments_after(program, text, last_member)
      .iter()
      .take_while(|comment| comment.start < range.end)
      .count();
    levels.push(Level {
      path: path.clone(),
      kind: container.kind(),
      member_kinds: members.iter().map(|member| comparable_kind(member.node)).collect(),
      original: placement(program, text, container, &members, &to_format, leading_comments, trailing_comments)?,
      to_format,
      leading_comments,
      trailing_comments,
    });
    if first != last {
      return Some(levels);
    }
    let member = members[first].node;
    path.push(Step::Member(first, comparable_kind(member)));
    match find_container(program, text, member, range, &mut path) {
      Some(inner) => container = inner,
      None => return Some(levels),
    }
  }
}

/// Gets whether the range touches the member, which for an empty range (ex. the cursor) includes
/// being right at its start or end.
fn touches(member: &Range<usize>, range: &Range<usize>) -> bool {
  if range.is_empty() {
    member.start <= range.start && range.start <= member.end
  } else {
    range.start < member.end && range.end > member.start
  }
}

/// Gets whether the range reaches from before the members onto the line of the container's
/// opening bracket, or the colon of a switch case, or from after them onto the line of the closing
/// bracket, which a switch case doesn't have. A non-empty range ending in the indentation of the
/// closing bracket's line doesn't count since that's where selecting the lines before it ends.
fn reaches_bracket_line(program: Program, text: &str, container: Node, members: &[Member], range: &Range<usize>) -> bool {
  let Some(inner) = inner_range(program, text, container, members) else {
    return false;
  };
  let first_start = members[0].start_with_empty_statements;
  let last_end = members[members.len() - 1].range.end;
  let reaches_open = range.start < first_start && !has_line_break(text, inner.start, range.start);
  let reaches_close = !matches!(container, Node::SwitchCase(_))
    && range.end > last_end
    && (range.is_empty() || range.end > indentation_end(text, inner.end))
    && !has_line_break(text, range.end, inner.end);
  reaches_open || reaches_close
}

/// Gets the members beside the range on its line when it only touches the space or comments
/// between them (ex. the cursor in `a();  |  b();`) or next to one at the start or end of its line,
/// since that's formatted along with them.
fn members_beside_on_line(program: Program, text: &str, members: &[Member], range: &Range<usize>) -> Option<(usize, usize)> {
  if has_line_break(text, range.start, range.end) {
    return None;
  }
  // no member is touched, so the one before the first after the range ends before the range
  let next = members.iter().position(|member| member.range.start >= range.end);
  let previous = next.unwrap_or(members.len()).checked_sub(1);
  let previous = previous.filter(|index| !has_line_break(text, end_before_next_line_semicolon(program, text, &members[*index].range), range.start));
  let next = next.filter(|index| !has_line_break(text, range.end, members[*index].range.start));
  match (previous, next) {
    (Some(previous), Some(next)) => Some((previous, next)),
    (Some(index), None) | (None, Some(index)) => Some((index, index)),
    (None, None) => None,
  }
}

/// Widens the touched members to the members on the same lines as them (ex. `b()` in `a(); b();`),
/// which the lines being replaced include.
fn with_members_on_same_lines(program: Program, text: &str, members: &[Member], mut first: usize, mut last: usize) -> RangeInclusive<usize> {
  while first > 0 && on_same_line(program, text, &members[first - 1], &members[first]) {
    first -= 1;
  }
  while last + 1 < members.len() && on_same_line(program, text, &members[last], &members[last + 1]) {
    last += 1;
  }
  first..=last
}

/// Gets whether the second member starts on the line the first one ends on, not counting a
/// semicolon of the first one on a later line.
fn on_same_line(program: Program, text: &str, first: &Member, second: &Member) -> bool {
  !has_line_break(text, end_before_next_line_semicolon(program, text, &first.range), second.range.start)
}

/// Gets how many of the comments before the member to format along with it, which are the ones
/// the range touches along with any needed for the lines to start at a comment or the member.
fn closest_leading_comments(text: &str, comments: &[Range<usize>], member: &Member, range: &Range<usize>) -> LeadingComments {
  let mut count = comments.iter().filter(|comment| comment.end > range.start).count();
  while count < comments.len() {
    let start = match count {
      0 => member.range.start,
      count => comments[comments.len() - count].start,
    };
    // a comment before the start on its line can't be left out (ex. the one in `/* a */ ;b()`)
    let has_comment_on_line = comments[comments.len() - count - 1].end > line_start(text, start);
    if starts_line(text, start) && !has_comment_on_line {
      break;
    }
    count += 1;
  }
  LeadingComments::Closest(count)
}

/// Gets the kind of a member to compare with the one in the formatted program, which is the same
/// for kinds formatting changes between (ex. it sorts `export *` and `export { .. } from` among
/// each other).
fn comparable_kind(node: Node) -> NodeKind {
  match node.kind() {
    NodeKind::ExportAll => NodeKind::NamedExport,
    // formatting changes `{ a = 1 }` to `{ a: 1 }`
    NodeKind::AssignProp => NodeKind::KeyValueProp,
    kind => kind,
  }
}

/// Finds the outermost node in the node, including itself, whose members are what's around the
/// range, adding the steps to it to the path.
fn find_container<'a>(program: Program<'a>, text: &str, node: Node<'a>, range: &Range<usize>, path: &mut Vec<Step>) -> Option<Node<'a>> {
  if let Some(members) = members(node)
    && let Some(inner) = inner_range(program, text, node, &members)
    && inner.start <= range.start
    && range.end <= inner.end
  {
    return Some(node);
  }
  for (index, child) in child_nodes(node).into_iter().enumerate() {
    let child_range = child.range().as_byte_range(START);
    if child_range.start <= range.start && range.end <= child_range.end {
      path.push(Step::Child(index, child.kind()));
      if let Some(container) = find_container(program, text, child, range, path) {
        return Some(container);
      }
      path.pop();
    }
  }
  None
}

/// Finds where the level is in the formatted program, or `None` when its members changed.
fn find_formatted_placement(program: Program, text: &str, level: &Level) -> Option<Placement> {
  let mut node = Node::from(program);
  for step in &level.path {
    let (next, next_kind, kind) = match *step {
      Step::Member(index, kind) => {
        let next = members(node)?.get(index)?.node;
        (next, comparable_kind(next), kind)
      }
      Step::Child(index, kind) => {
        let next = *child_nodes(node).get(index)?;
        (next, next.kind(), kind)
      }
    };
    if next_kind != kind {
      return None;
    }
    node = next;
  }
  let members = members(node)?;
  if node.kind() != level.kind || !members.iter().map(|member| comparable_kind(member.node)).eq(level.member_kinds.iter().copied()) {
    return None;
  }
  placement(program, text, node, &members, &level.to_format, level.leading_comments, level.trailing_comments)
}

/// Gets where the members are in the text, or `None` when there aren't as many comments around
/// them as needed.
fn placement(
  program: Program,
  text: &str,
  container: Node,
  members: &[Member],
  to_format: &RangeInclusive<usize>,
  leading_comments: LeadingComments,
  trailing_comments: usize,
) -> Option<Placement> {
  let first = &members[*to_format.start()];
  let last = &members[*to_format.end()];
  let start = comments_start(text, &comments_before(program, text, first), leading_comments, first)?;
  let end = match trailing_comments {
    0 => last.range.end,
    count => comments_after(program, text, last).get(count - 1)?.end,
  };
  let previous_semicolon = program
    .token_container()
    .get_previous_token(START + first.range.start)
    .map(|token| token.start().as_byte_index(START))
    .filter(|index| text.as_bytes()[*index] == b';' && *index >= line_start(text, start));
  let previous_semicolon_ends_member = previous_semicolon.is_some_and(|index| {
    let previous_member = to_format.start().checked_sub(1).map(|previous| &members[previous]);
    previous_member.is_some_and(|member| member.range.end == index + 1)
  });
  Some(Placement {
    members: start..end,
    first_member_start: first.range.start,
    last_member_end: last.range.end,
    end_before_semicolon: end_before_next_line_semicolon(program, text, &last.range),
    previous_semicolon,
    previous_semicolon_ends_member,
    container: container.range().as_byte_range(START),
  })
}

/// Gets where the comments before the member to format start, which is the member's start when
/// there are none, or `None` when there aren't as many as needed.
fn comments_start(text: &str, comments: &[Range<usize>], leading_comments: LeadingComments, member: &Member) -> Option<usize> {
  let count = match leading_comments {
    LeadingComments::Closest(count) => count,
    LeadingComments::Attached => {
      let mut next_start = member.range.start;
      let attached = comments.iter().rev().take_while(|comment| {
        let is_attached = text[comment.end..next_start].matches('\n').count() < 2;
        next_start = comment.start;
        is_attached
      });
      attached.count()
    }
  };
  match count {
    0 => Some(member.range.start),
    count => Some(comments.get(comments.len().checked_sub(count)?)?.start),
  }
}

/// Gets the members of a node that holds them, or `None` when the node doesn't hold members, holds
/// none, or is an array with holes.
fn members(node: Node) -> Option<Vec<Member>> {
  let nodes: Vec<Node> = match node {
    Node::Module(module) => module.body.iter().map(Node::from).collect(),
    Node::Script(script) => script.body.iter().map(Node::from).collect(),
    Node::BlockStmt(block) => block.stmts.iter().map(Node::from).collect(),
    Node::FunctionBody(body) => body.stmts.iter().map(Node::from).collect(),
    Node::TsModuleBlock(block) => block.body.iter().map(Node::from).collect(),
    Node::SwitchStmt(switch) => switch.cases.iter().map(|case| Node::from(*case)).collect(),
    Node::SwitchCase(case) => case.cons.iter().map(Node::from).collect(),
    Node::Class(class) => class.body.iter().map(Node::from).collect(),
    Node::TsInterfaceBody(body) => body.body.iter().map(Node::from).collect(),
    Node::TsTypeLit(type_lit) => type_lit.members.iter().map(Node::from).collect(),
    Node::TsEnumDecl(decl) => decl.members.iter().map(|member| Node::from(*member)).collect(),
    Node::ObjectLit(object) => object.props.iter().map(Node::from).collect(),
    // holes don't have a node to format
    Node::ArrayLit(array) => array.elems.iter().map(|elem| elem.map(Node::from)).collect::<Option<_>>()?,
    _ => return None,
  };
  let mut members: Vec<Member> = Vec::with_capacity(nodes.len());
  let mut empty_statements_start = None;
  for node in nodes {
    let range = get_stmt_range_with_decorators(node).as_byte_range(START);
    if node.kind() == NodeKind::EmptyStmt {
      empty_statements_start = empty_statements_start.or(Some(range.start));
    } else {
      members.push(Member {
        node,
        start_with_empty_statements: empty_statements_start.take().unwrap_or(range.start),
        range,
      });
    }
  }
  (!members.is_empty()).then_some(members)
}

/// Gets the byte range between the brackets of a node with members, or from after the colon of a
/// switch case to the start of the line after it, which is where selecting its lines ends.
fn inner_range(program: Program, text: &str, node: Node, members: &[Member]) -> Option<Range<usize>> {
  let open = program.token_container().get_previous_token(START + members[0].start_with_empty_statements)?;
  let start = open.end().as_byte_index(START);
  let end = node.end().as_byte_index(START);
  let end = match node {
    Node::SwitchCase(_) => {
      let next_line_start = text.as_bytes()[end..]
        .iter()
        .position(|byte| *byte == b'\n')
        .map_or(text.len(), |offset| end + offset + 1);
      let next_token_start = program
        .token_container()
        .get_next_token(node.end())
        .map_or(text.len(), |token| token.start().as_byte_index(START));
      next_line_start.min(next_token_start)
    }
    _ => end - 1,
  };
  Some(start..end)
}

/// Gets the children of a node, skipping what formatting adds and removes: parentheses (ex. around
/// multi-line JSX) and whitespace-only JSX text.
fn child_nodes(node: Node) -> Vec<Node> {
  let mut children = node.children();
  children.retain(|child| !matches!(child, Node::JSXText(text) if text.inner.raw.trim().is_empty()));
  for child in &mut children {
    while let Node::ParenExpr(paren) = child {
      *child = Node::from(&paren.expr);
    }
  }
  children
}

/// Gets the byte ranges of the comments before a member, which are the ones after the token before
/// it or before the semicolon starting its line (ex. with `semiColons: asi`), leaving out ones that
/// follow code on their line (ex. a trailing comment of the member before).
fn comments_before(program: Program, text: &str, member: &Member) -> Vec<Range<usize>> {
  let comment_container = program.comment_container();
  let comments_at = |index: usize| {
    comment_container
      .leading_comments(START + index)
      .map(|comment| comment.range().as_byte_range(START))
  };
  let mut comments = Vec::new();
  if let Some(previous) = program.token_container().get_previous_token(START + member.range.start) {
    let previous = previous.range().as_byte_range(START);
    if &text[previous.clone()] == ";" && previous.start >= line_start(text, member.range.start) {
      comments.extend(comments_at(previous.start));
    }
  }
  comments.extend(comments_at(member.range.start));
  // once a comment starts its line, the ones after it do too or follow it
  let own_line_index = comments.iter().position(|comment| starts_line(text, comment.start)).unwrap_or(comments.len());
  comments.drain(..own_line_index);
  comments
}

/// Gets the byte ranges of the comments on their own lines after a member, which are the ones
/// after its separator (ex. `,`), or before its semicolon when that's on a later line (ex. with
/// `semiColons: asi`).
fn comments_after(program: Program, text: &str, member: &Member) -> Vec<Range<usize>> {
  let end_before_semicolon = end_before_next_line_semicolon(program, text, &member.range);
  let (position, limit) = if end_before_semicolon != member.range.end {
    (end_before_semicolon, member.range.end - 1)
  } else {
    let separator_end = program
      .token_container()
      .get_next_token(START + member.range.end)
      .map(|token| token.range().as_byte_range(START))
      .filter(|token| &text[token.clone()] == ",")
      .map_or(member.range.end, |token| token.end);
    (separator_end, text.len())
  };
  // these are the comments after the position along with the ones before the next token
  program
    .comment_container()
    .trailing_comments(START + position)
    .map(|comment| comment.range().as_byte_range(START))
    .filter(|comment| comment.end <= limit)
    .skip_while(|comment| !starts_line(text, comment.start))
    .take_while(|comment| starts_line(text, comment.start) && line_end(text, comment.end).is_some())
    .collect()
}

/// Replaces the lines the members are on in the text with the lines they're on in the formatted
/// text, or `None` when other code is on those lines.
fn replace_lines(text: &str, original: &Placement, formatted_text: &str, formatted: &Placement) -> Option<String> {
  let original_lines = Lines::new(text, original)?;
  let formatted_lines = Lines::new(formatted_text, formatted)?;
  let replacement = formatted_lines.replacement_for(&original_lines, formatted_text);
  Some(format!(
    "{}{}{}",
    &text[..original_lines.range.start],
    with_file_new_lines(&replacement, text),
    &text[original_lines.range.end..]
  ))
}

/// The lines a level's members to format are on.
struct Lines<'a> {
  placement: &'a Placement,
  /// From the start of the first line to the end of the last, without its line break.
  range: Range<usize>,
}

impl<'a> Lines<'a> {
  /// Gets the lines the members are on, or `None` when code outside of them is on the lines.
  ///
  /// Semicolons and comments are fine since a semicolon at the start of a line goes with the
  /// statement before it (ex. with `semiColons: asi`), as does one at the end of a line along
  /// with the separators between members.
  fn new(text: &str, placement: &'a Placement) -> Option<Self> {
    let members = &placement.members;
    if !starts_line(text, members.start) {
      return None;
    }
    let end = match line_end(text, members.end) {
      Some(end) => end,
      // the last member's semicolon starts the line of what comes after it
      None if members.end == placement.last_member_end && placement.end_before_semicolon != members.end => line_end(text, placement.end_before_semicolon)?,
      None => return None,
    };
    Some(Lines {
      placement,
      range: line_start(text, members.start)..end,
    })
  }

  /// Gets whether the last member's semicolon is on a later line than these lines.
  fn semicolon_after(&self) -> bool {
    let placement = self.placement;
    placement.end_before_semicolon != placement.last_member_end && placement.last_member_end > self.range.end
  }

  /// Gets the text of these formatted lines to replace the original lines with, which keeps the
  /// semicolons ending the statements before and after the members where the original lines have
  /// them since that depends on the style of the rest of the file (ex. `;[1]` versus `foo();`).
  fn replacement_for<'b>(&self, original: &Lines, text: &'b str) -> Cow<'b, str> {
    let placement = self.placement;
    let mut edits = Vec::with_capacity(2);
    // the original lines keep a semicolon ending the member before since that's needed there, while
    // one that's an empty statement can go (ex. `;;b();`)
    let original_has_previous_semicolon = original.placement.previous_semicolon_ends_member;
    match (original_has_previous_semicolon, placement.previous_semicolon) {
      (false, Some(index)) => edits.push(SemicolonEdit::Remove(index)),
      (true, None) => edits.push(SemicolonEdit::Insert(placement.first_member_start)),
      _ => {}
    }
    match (original.semicolon_after(), self.semicolon_after()) {
      (false, true) => edits.push(SemicolonEdit::Insert(placement.end_before_semicolon)),
      (true, false) if text.as_bytes()[placement.last_member_end - 1] == b';' => edits.push(SemicolonEdit::Remove(placement.last_member_end - 1)),
      _ => {}
    }
    if edits.is_empty() {
      return Cow::Borrowed(&text[self.range.clone()]);
    }
    let mut replacement = String::with_capacity(self.range.len() + 1);
    let mut index = self.range.start;
    for edit in edits {
      match edit {
        SemicolonEdit::Insert(position) => {
          replacement.push_str(&text[index..position]);
          replacement.push(';');
          index = position;
        }
        SemicolonEdit::Remove(position) => {
          replacement.push_str(&text[index..position]);
          index = position + 1;
        }
      }
    }
    replacement.push_str(&text[index..self.range.end]);
    Cow::Owned(replacement)
  }
}

/// A change to the formatted lines that keeps a semicolon going with the statements around them
/// where the original lines have it.
enum SemicolonEdit {
  Insert(usize),
  Remove(usize),
}

/// Gets whether there's a line break between the byte indexes, which don't need to be at character
/// boundaries.
fn has_line_break(text: &str, start: usize, end: usize) -> bool {
  text.as_bytes()[start..end].contains(&b'\n')
}

/// Gets whether only spaces, tabs, semicolons, and comments come before the index on its line.
fn starts_line(text: &str, index: usize) -> bool {
  skip_inline_trivia(&text[..index], line_start(text, index), b";") == index
}

/// Gets the start of the line the index is on.
fn line_start(text: &str, index: usize) -> usize {
  text.as_bytes()[..index].iter().rposition(|byte| *byte == b'\n').map_or(0, |index| index + 1)
}

/// Gets the end of the indentation of the line the index is on.
fn indentation_end(text: &str, index: usize) -> usize {
  let start = line_start(text, index);
  start + text.as_bytes()[start..].iter().take_while(|byte| matches!(byte, b' ' | b'\t')).count()
}

/// Gets the end of the range before its semicolon when that's on a later line than the token
/// before it (ex. the one before `[1]` in `foo()\n;[1].forEach()`), which is on the line of what
/// comes after it.
fn end_before_next_line_semicolon(program: Program, text: &str, range: &Range<usize>) -> usize {
  if text.as_bytes()[range.end - 1] == b';'
    && let Some(previous) = program.token_container().get_previous_token(START + (range.end - 1))
  {
    let previous_end = previous.end().as_byte_index(START);
    if previous_end > range.start && text[previous_end..range.end - 1].contains('\n') {
      return previous_end;
    }
  }
  range.end
}

/// Gets the end of the line the index is on, without the line break, when only separators and
/// comments are between the index and it.
fn line_end(text: &str, index: usize) -> Option<usize> {
  let index = skip_inline_trivia(text, index, b",;");
  let end = if text[index..].starts_with("//") {
    text[index..].find('\n').map_or(text.len(), |offset| index + offset)
  } else {
    index
  };
  let end = if end > 0 && text.as_bytes()[end - 1] == b'\r' { end - 1 } else { end };
  match text.as_bytes().get(end..) {
    Some([] | [b'\n', ..] | [b'\r', b'\n', ..]) => Some(end),
    _ => None,
  }
}

/// Skips the spaces, tabs, separators, and comments that don't go to another line, returning the
/// index after them.
fn skip_inline_trivia(text: &str, mut index: usize, separators: &[u8]) -> usize {
  let bytes = text.as_bytes();
  loop {
    match bytes.get(index) {
      Some(b' ' | b'\t') => index += 1,
      Some(byte) if separators.contains(byte) => index += 1,
      Some(b'/') if bytes.get(index + 1) == Some(&b'*') => match text[index + 2..].find("*/") {
        Some(offset) if !text[index..index + 2 + offset].contains('\n') => index += 2 + offset + 2,
        _ => return index,
      },
      _ => return index,
    }
  }
}

/// Gets the width of the indentation of the line the index is on.
fn line_indent(text: &str, index: usize, indent_width: usize) -> usize {
  text.as_bytes()[line_start(text, index)..]
    .iter()
    .map_while(|byte| match byte {
      b' ' => Some(1),
      b'\t' => Some(indent_width),
      _ => None,
    })
    .sum()
}

/// Gets whether the text has a carriage return without a line feed after it.
fn has_lone_carriage_return(text: &str) -> bool {
  let bytes = text.as_bytes();
  bytes
    .iter()
    .enumerate()
    .any(|(index, byte)| *byte == b'\r' && bytes.get(index + 1) != Some(&b'\n'))
}

/// Keeps the line endings of the rest of the file since changing those is up to formatting the
/// whole file.
fn with_file_new_lines<'a>(formatted: &'a str, file_text: &str) -> Cow<'a, str> {
  if !file_text.contains('\n') {
    return Cow::Borrowed(formatted);
  }
  match resolve_new_line_kind(file_text, NewLineKind::Auto) {
    "\r\n" if formatted.contains('\n') && !formatted.contains("\r\n") => Cow::Owned(formatted.replace('\n', "\r\n")),
    "\n" if formatted.contains("\r\n") => Cow::Owned(formatted.replace("\r\n", "\n")),
    _ => Cow::Borrowed(formatted),
  }
}

type EmbeddedResult = std::result::Result<Option<String>, String>;

/// Formats an embedded template with the external formatter, reusing the result when the same
/// text was already formatted.
fn format_embedded_with_cache(
  cache: &RefCell<HashMap<(String, String), EmbeddedResult>>,
  external_formatter: &ExternalFormatter,
  extension: &str,
  text: String,
  config: &Configuration,
) -> std::result::Result<Option<String>, Box<dyn std::error::Error + Send + Sync>> {
  let key = (extension.to_string(), text);
  if let Some(result) = cache.borrow().get(&key) {
    return result.clone().map_err(Into::into);
  }
  let result = external_formatter(extension, key.1.clone(), config).map_err(|err| err.to_string());
  cache.borrow_mut().insert(key, result.clone());
  result.map_err(Into::into)
}

#[cfg(test)]
mod tests {
  use std::cell::Cell;
  use std::path::Path;

  use super::*;
  use crate::configuration::ConfigurationBuilder;

  #[test]
  fn keeps_bom_outside_range() {
    // the spec files can't express this since editors strip the bom
    let config = ConfigurationBuilder::new().build();
    let output = format_const_b(&config, "\u{FEFF}const  a  =  1;\nconst  b  =  2;\n").unwrap().unwrap();
    assert_eq!(output, "\u{FEFF}const  a  =  1;\nconst b = 2;\n");
  }

  #[test]
  fn keeps_file_line_endings() {
    // the spec files can't express this since they normalize line endings
    let config = ConfigurationBuilder::new().build();
    let output = format_const_b(&config, "const  a  =  1;\r\nconst  b  =  {\r\nc:1};\r\n").unwrap().unwrap();
    assert_eq!(output, "const  a  =  1;\r\nconst b = {\r\n  c: 1,\r\n};\r\n");

    let config = ConfigurationBuilder::new().new_line_kind(NewLineKind::CarriageReturnLineFeed).build();
    let output = format_const_b(&config, "const  a  =  1;\nconst  b  =  {\nc:1};\n").unwrap().unwrap();
    assert_eq!(output, "const  a  =  1;\nconst b = {\n  c: 1,\n};\n");
  }

  #[test]
  fn formats_whole_file_with_carriage_return_line_endings() {
    let config = ConfigurationBuilder::new().build();
    let output = format_const_b(&config, "const  a  =  1;\rconst  b  =  2;\r").unwrap().unwrap();
    assert_eq!(output, "const a = 1;\nconst b = 2;\n");
  }

  #[test]
  fn formats_nothing_with_ignore_file_comment() {
    // the text doesn't parse, so this would error if the file was looked at
    let config = ConfigurationBuilder::new().build();
    assert_eq!(
      format_const_b(&config, "// dprint-ignore-file\nconst  a  =  1;\nconst  b  =  ;\n").unwrap(),
      None
    );
  }

  #[test]
  fn errors_when_failing_to_parse() {
    let config = ConfigurationBuilder::new().build();
    assert!(format_const_b(&config, "const  a  =  1;\nconst  b  =  ;\n").is_err());
  }

  #[test]
  fn clamps_range_past_end() {
    let config = ConfigurationBuilder::new().build();
    let text = "const  a  =  1;\nconst  b  =  2;\n";
    let start = text.find("const  b").unwrap();
    let output = format_range(&config, text, start..text.len() + 10, None).unwrap().unwrap();
    assert_eq!(output, "const  a  =  1;\nconst b = 2;\n");
    // a range past the end of the text still covers all of it
    let output = format_range(&config, text, 0..text.len() + 10, None).unwrap().unwrap();
    assert_eq!(output, "const a = 1;\nconst b = 2;\n");
    assert_eq!(format_range(&config, text, text.len() + 5..text.len() + 10, None).unwrap(), None);
  }

  #[test]
  fn removes_bom_when_formatting_whole_text() {
    let config = ConfigurationBuilder::new().build();
    let text = "\u{FEFF}const  a  =  1;\n";
    let output = format_range(&config, text, 0..text.len(), None).unwrap().unwrap();
    assert_eq!(output, "const a = 1;\n");
  }

  #[test]
  fn formats_each_embedded_template_once() {
    let config = ConfigurationBuilder::new().build();
    let calls = Cell::new(0);
    let external_formatter = |_: &str, text: String, _: &Configuration| -> std::result::Result<Option<String>, Box<dyn std::error::Error + Send + Sync>> {
      calls.set(calls.get() + 1);
      Ok(Some(text.trim().to_string()))
    };
    let text = "const  a  =  css` a `;\nconst  c  =  css` c `;\nconst  b  =  2;\n";
    let start = text.find("const  b").unwrap();
    let output = format_range(&config, text, start..start + 8, Some(&external_formatter)).unwrap().unwrap();
    assert_eq!(output, "const  a  =  css` a `;\nconst  c  =  css` c `;\nconst b = 2;\n");
    assert_eq!(calls.get(), 2);
  }

  #[test]
  fn errors_when_an_embedded_template_outside_the_range_fails_to_format() {
    let config = ConfigurationBuilder::new().build();
    let external_formatter =
      |_: &str, _: String, _: &Configuration| -> std::result::Result<Option<String>, Box<dyn std::error::Error + Send + Sync>> { Err("bad css".into()) };
    let text = "const  a  =  css` a `;\nconst  b  =  2;\n";
    let start = text.find("const  b").unwrap();
    let err = format_range(&config, text, start..start + 8, Some(&external_formatter)).unwrap_err();
    assert!(err.to_string().contains("bad css"), "{err}");
  }

  fn format_const_b(config: &Configuration, text: &str) -> Result<Option<String>> {
    let start = text.find("const  b").unwrap();
    format_range(config, text, start..start + 8, None)
  }

  fn format_range(config: &Configuration, text: &str, range: Range<usize>, external_formatter: Option<&ExternalFormatter>) -> Result<Option<String>> {
    format_text_range(
      FormatTextOptions {
        path: Path::new("file.ts"),
        extension: None,
        text: text.into(),
        config,
        external_formatter,
        arena_pool: None,
      },
      range,
    )
  }
}
