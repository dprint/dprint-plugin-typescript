use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::Range;
use std::ops::RangeInclusive;
use std::sync::Arc;

use dprint_core::configuration::NewLineKind;
use dprint_core::configuration::resolve_new_line_kind;
use dprint_core::formatting::ir_helpers::text_has_dprint_ignore;
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
use super::generation::get_stmt_range_with_decorators;
use super::parsing::ParseOptions;
use super::parsing::parse_program;
use super::utils::file_text_has_ignore_comment;

/// Formats only the part of the text within the provided byte range.
///
/// The range is in bytes of the text, including any byte order mark. It's widened to the lines of
/// the members it touches in the innermost statement list, switch statement, class, interface,
/// type literal, enum, object literal, or array literal without holes that contains it, along with
/// the comments it touches before them, and the text outside of those lines is left as it was.
/// Members sharing a line with the touched ones are formatted along with them. The lines are what
/// formatting the whole file gives, so when they don't fit in with the text around them (ex. the
/// container is formatted on a single line or gets indented differently, or the file would format
/// differently with them in place), the member holding the container is formatted instead, and so
/// on out to the whole file.
///
/// Import declarations are only sorted or grouped when the range touches all of the module's ones
/// that formatting would sort, since sorting some of them moves declarations outside of the range,
/// and the same goes for re-export declarations. Declarations with an ignore comment don't count
/// since they're never sorted.
///
/// A range that only touches the space, comments, or empty statements between members formats
/// nothing, while one covering the whole text formats the whole file like [`format_text`]. The byte
/// order mark is kept when only part of the file is formatted and removed like [`format_text`]
/// does when the whole file is. A file with carriage return only line endings is always formatted
/// as a whole.
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
  for (level, formatted_placement) in levels.iter().zip(formatted_placements).rev() {
    let Some(formatted_placement) = formatted_placement else {
      continue;
    };
    // the lines would be indented differently than the ones around them when formatting moves the
    // container's own lines (ex. it gets wrapped in parentheses)
    let container_moves = !level.path.is_empty()
      && [
        (level.original.container.start, formatted_placement.container.start),
        (level.original.container.end - 1, formatted_placement.container.end - 1),
      ]
      .into_iter()
      .any(|(original, formatted_index)| line_indent(&body, original, indent_width) != line_indent(&formatted, formatted_index, indent_width));
    if container_moves {
      continue;
    }
    let (Some(original_lines), Some(formatted_lines)) = (Lines::new(&body, &level.original), Lines::new(&formatted, &formatted_placement)) else {
      continue;
    };
    let replacement = formatted_lines.replacement_for(&original_lines, &formatted);
    let result = format!(
      "{}{}{}",
      &body[..original_lines.range.start],
      with_file_new_lines(&replacement, &body),
      &body[original_lines.range.end..]
    );
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
    let touched = || touches(&node.range().as_byte_range(StartSourcePos::START_SOURCE_POS), range);
    match sorted_declaration(program, node, config) {
      Some(SortedDeclaration::Import) => imports &= touched(),
      Some(SortedDeclaration::Export) => exports &= touched(),
      None => {}
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

/// The kinds of declarations formatting sorts along with the ones around them.
#[derive(Clone, Copy, PartialEq)]
enum SortedDeclaration {
  Import,
  Export,
}

/// Gets the kind of declaration formatting sorts the statement with, which is none for a side
/// effect import or one with an ignore comment.
fn sorted_declaration(program: Program, node: Node, config: &Configuration) -> Option<SortedDeclaration> {
  let kind = match node {
    Node::ImportDecl(decl) if !decl.specifiers.is_empty() => SortedDeclaration::Import,
    Node::ExportAll(_) => SortedDeclaration::Export,
    Node::NamedExport(export) if export.src.is_some() => SortedDeclaration::Export,
    _ => return None,
  };
  let is_ignored = node
    .leading_comments_fast(program)
    .any(|comment| text_has_dprint_ignore(&comment.text, &config.ignore_node_comment_text));
  (!is_ignored).then_some(kind)
}

/// A node holding a list of members (ex. a block's statements) along with the members of it to
/// format.
struct Level {
  /// How to get from the program to the node, which finds it again in the formatted text.
  path: Vec<Step>,
  /// Kind of the node, which is checked along with the kinds of its members to know the node in
  /// the formatted program is the same one.
  kind: NodeKind,
  member_kinds: Vec<NodeKind>,
  /// Indexes of the members to format, which are the touched ones along with the ones on their
  /// lines. For all but the innermost level, the only touched one is the member holding the next
  /// level.
  touched: RangeInclusive<usize>,
  /// The comments before the first touched member to format along with it.
  leading_comments: LeadingComments,
  original: Placement,
}

/// A step from a node to one of its members or children, along with the kind of node it goes to.
#[derive(Clone, Copy)]
enum Step {
  Member(usize, NodeKind),
  Child(usize, NodeKind),
}

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

/// Where a level's touched members are in a text.
struct Placement {
  /// From the start of the first touched member, or the first comment before it to format, to the
  /// end of the last touched member.
  members: Range<usize>,
  first_member_start: usize,
  /// End of the last touched member before its semicolon when that's on a later line (ex. the one
  /// before `[1]` in `foo()\n;[1].forEach()`), where it goes with what comes after.
  end_before_semicolon: usize,
  /// Index of the semicolon ending the member before when it's on the lines (ex. the one before
  /// `[1]` in `foo()\n;[1].forEach()`).
  previous_semicolon: Option<usize>,
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
/// the range only touches the space, comments, or empty statements between members.
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
    let first = touched.next()?;
    let last = touched.next_back().unwrap_or(first);
    let on_lines = with_members_on_same_lines(program, text, &members, first, last);
    let first_member = &members[*on_lines.start()];
    let leading_comments = if matches!(container, Node::Module(_))
      && !config.module_import_groups.is_empty()
      && sorted_declaration(program, first_member.node, config) == Some(SortedDeclaration::Import)
    {
      LeadingComments::Attached
    } else {
      let comments = leading_comments(program, text, first_member);
      let mut count = comments.iter().filter(|comment| comment.end > range.start).count();
      while count < comments.len() && !starts_line(text, comments_start(text, &comments, LeadingComments::Closest(count), first_member)?) {
        count += 1;
      }
      LeadingComments::Closest(count)
    };
    levels.push(Level {
      path: path.clone(),
      kind: container.kind(),
      member_kinds: members.iter().map(|member| comparable_kind(member.node)).collect(),
      original: placement(program, text, container, &members, &on_lines, leading_comments)?,
      touched: on_lines,
      leading_comments,
    });
    if first != last {
      return Some(levels);
    }
    let member = members[first].node;
    path.push(Step::Member(first, member.kind()));
    match find_container(program, member, range, &mut path) {
      Some(inner) => container = inner,
      None => return Some(levels),
    }
  }
}

/// Widens the touched members to the members on the same lines as them (ex. `b()` in `a(); b();`),
/// which the lines being replaced include.
fn with_members_on_same_lines(program: Program, text: &str, members: &[Member], mut first: usize, mut last: usize) -> RangeInclusive<usize> {
  let on_same_line = |a: &Member, b: &Member| !text[end_before_next_line_semicolon(program, text, &a.range)..b.range.start].contains('\n');
  while first > 0 && on_same_line(&members[first - 1], &members[first]) {
    first -= 1;
  }
  while last + 1 < members.len() && on_same_line(&members[last], &members[last + 1]) {
    last += 1;
  }
  first..=last
}

/// Gets the kind of a member to compare with the one in the formatted program, which is the same
/// for `export *` and `export { .. } from` since formatting can sort those among each other.
fn comparable_kind(node: Node) -> NodeKind {
  match node.kind() {
    NodeKind::ExportAll => NodeKind::NamedExport,
    kind => kind,
  }
}

/// Finds the outermost node in the node, including itself, whose members are what's around the
/// range, adding the steps to it to the path.
fn find_container<'a>(program: Program<'a>, node: Node<'a>, range: &Range<usize>, path: &mut Vec<Step>) -> Option<Node<'a>> {
  if let Some(members) = members(node)
    && let Some(inner) = inner_range(program, node, &members)
    && inner.start <= range.start
    && range.end <= inner.end
  {
    return Some(node);
  }
  for (index, child) in child_nodes(node).into_iter().enumerate() {
    let child_range = child.range().as_byte_range(StartSourcePos::START_SOURCE_POS);
    if child_range.start <= range.start && range.end <= child_range.end {
      path.push(Step::Child(index, child.kind()));
      if let Some(container) = find_container(program, child, range, path) {
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
    let (next, kind) = match *step {
      Step::Member(index, kind) => (members(node)?.get(index)?.node, kind),
      Step::Child(index, kind) => (*child_nodes(node).get(index)?, kind),
    };
    if next.kind() != kind {
      return None;
    }
    node = next;
  }
  let members = members(node)?;
  if node.kind() != level.kind || !members.iter().map(|member| comparable_kind(member.node)).eq(level.member_kinds.iter().copied()) {
    return None;
  }
  placement(program, text, node, &members, &level.touched, level.leading_comments)
}

/// Gets where the members are in the text, or `None` when there aren't as many comments before
/// them as needed.
fn placement(
  program: Program,
  text: &str,
  container: Node,
  members: &[Member],
  touched: &RangeInclusive<usize>,
  leading_comments: LeadingComments,
) -> Option<Placement> {
  let first = &members[*touched.start()];
  let last = &members[*touched.end()];
  let start = comments_start(text, &self::leading_comments(program, text, first), leading_comments, first)?;
  let previous_semicolon = touched
    .start()
    .checked_sub(1)
    .map(|index| members[index].range.end - 1)
    .filter(|index| text.as_bytes()[*index] == b';' && *index >= line_start(text, start));
  Some(Placement {
    members: start..last.range.end,
    first_member_start: first.range.start,
    end_before_semicolon: end_before_next_line_semicolon(program, text, &last.range),
    previous_semicolon,
    container: container.range().as_byte_range(StartSourcePos::START_SOURCE_POS),
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
    let range = get_stmt_range_with_decorators(node).as_byte_range(StartSourcePos::START_SOURCE_POS);
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

/// Gets the byte range between the brackets of a node with members, or after the colon of a
/// switch case.
fn inner_range(program: Program, node: Node, members: &[Member]) -> Option<Range<usize>> {
  let open = program
    .token_container()
    .get_previous_token(StartSourcePos::START_SOURCE_POS + members[0].start_with_empty_statements)?;
  let end = match node {
    Node::SwitchCase(_) => node.end(),
    _ => node.end() - 1,
  };
  Some(open.end().as_byte_index(StartSourcePos::START_SOURCE_POS)..end.as_byte_index(StartSourcePos::START_SOURCE_POS))
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
fn leading_comments(program: Program, text: &str, member: &Member) -> Vec<Range<usize>> {
  let comment_container = program.comment_container();
  let comments_at = |index: usize| {
    comment_container
      .leading_comments(StartSourcePos::START_SOURCE_POS + index)
      .map(|comment| comment.range().as_byte_range(StartSourcePos::START_SOURCE_POS))
  };
  let mut comments = Vec::new();
  if let Some(previous) = program
    .token_container()
    .get_previous_token(StartSourcePos::START_SOURCE_POS + member.range.start)
  {
    let previous = previous.range().as_byte_range(StartSourcePos::START_SOURCE_POS);
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

/// Gets whether the range touches the member, which for an empty range (ex. the cursor) includes
/// being right at its start or end.
fn touches(member: &Range<usize>, range: &Range<usize>) -> bool {
  if range.is_empty() {
    member.start <= range.start && range.start <= member.end
  } else {
    range.start < member.end && range.end > member.start
  }
}

/// The lines a level's members are on.
struct Lines {
  /// From the start of the first line to the end of the last, without its line break.
  range: Range<usize>,
  first_member_start: usize,
  /// Where the members end, which is before the last one's semicolon when that's on a later line.
  members_end: usize,
  /// Index of the semicolon ending the member before when it's on the lines.
  previous_semicolon: Option<usize>,
  /// Whether the last member's semicolon is on a later line.
  semicolon_after: bool,
}

impl Lines {
  /// Gets the lines the members are on, or `None` when code outside of them is on the lines.
  ///
  /// Semicolons and comments are fine since a semicolon at the start of a line goes with the
  /// statement before it (ex. with `semiColons: asi`), as does one at the end of a line along
  /// with the separators between members.
  fn new(text: &str, placement: &Placement) -> Option<Lines> {
    let members = &placement.members;
    if !starts_line(text, members.start) {
      return None;
    }
    let (members_end, end) = match line_end(text, members.end) {
      Some(end) => (members.end, end),
      None if placement.end_before_semicolon != members.end => (placement.end_before_semicolon, line_end(text, placement.end_before_semicolon)?),
      None => return None,
    };
    Some(Lines {
      range: line_start(text, members.start)..end,
      first_member_start: placement.first_member_start,
      members_end,
      previous_semicolon: placement.previous_semicolon,
      semicolon_after: members_end != members.end,
    })
  }

  /// Gets the text of these formatted lines to replace the original lines with, which keeps the
  /// semicolons ending the statements before and after the members where the original lines have
  /// them since that depends on the style of the rest of the file (ex. `;[1]` versus `foo();`).
  fn replacement_for<'a>(&self, original: &Lines, text: &'a str) -> Cow<'a, str> {
    if original.previous_semicolon.is_some() == self.previous_semicolon.is_some() && original.semicolon_after == self.semicolon_after {
      return Cow::Borrowed(&text[self.range.clone()]);
    }
    let mut replacement = String::with_capacity(self.range.len() + 1);
    match (original.previous_semicolon.is_some(), self.previous_semicolon) {
      (false, Some(index)) => {
        replacement.push_str(&text[self.range.start..index]);
        replacement.push_str(&text[index + 1..self.members_end]);
      }
      (true, None) => {
        replacement.push_str(&text[self.range.start..self.first_member_start]);
        replacement.push(';');
        replacement.push_str(&text[self.first_member_start..self.members_end]);
      }
      _ => replacement.push_str(&text[self.range.start..self.members_end]),
    }
    match (original.semicolon_after, self.semicolon_after) {
      (false, true) => replacement.push(';'),
      (true, false) if replacement.ends_with(';') => {
        replacement.pop();
      }
      _ => {}
    }
    replacement.push_str(&text[self.members_end..self.range.end]);
    Cow::Owned(replacement)
  }
}

/// Gets whether only spaces, tabs, semicolons, and comments come before the index on its line.
fn starts_line(text: &str, index: usize) -> bool {
  skip_inline_trivia(&text[..index], line_start(text, index), b";") == index
}

/// Gets the start of the line the index is on.
fn line_start(text: &str, index: usize) -> usize {
  text.as_bytes()[..index].iter().rposition(|byte| *byte == b'\n').map_or(0, |index| index + 1)
}

/// Gets the end of the range before its semicolon when that's on a later line than the token
/// before it (ex. the one before `[1]` in `foo()\n;[1].forEach()`), which is on the line of what
/// comes after it.
fn end_before_next_line_semicolon(program: Program, text: &str, range: &Range<usize>) -> usize {
  if text.as_bytes()[range.end - 1] == b';'
    && let Some(previous) = program.token_container().get_previous_token(StartSourcePos::START_SOURCE_POS + (range.end - 1))
  {
    let previous_end = previous.end().as_byte_index(StartSourcePos::START_SOURCE_POS);
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
