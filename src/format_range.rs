use std::borrow::Cow;
use std::ops::Range;
use std::ops::RangeInclusive;
use std::sync::Arc;

use dprint_core::configuration::NewLineKind;
use dprint_core::configuration::resolve_new_line_kind;
use dprint_swc_ext::common::RootNode;
use dprint_swc_ext::common::SourcePos;
use dprint_swc_ext::common::SourceRanged;
use dprint_swc_ext::common::SourceRangedForSpanned;
use dprint_swc_ext::common::StartSourcePos;
use dprint_swc_ext::view::*;

use crate::Result;

use super::configuration::Configuration;
use super::configuration::SortOrder;
use super::format_text::FormatTextOptions;
use super::format_text::format_text;
use super::generation::get_stmt_range_with_decorators;
use super::parsing::ParseOptions;
use super::parsing::parse_program;
use super::utils::file_text_has_ignore_comment;

/// Formats only the part of the text within the provided byte range.
///
/// The range is widened to the lines of the members it touches in the innermost statement list,
/// switch statement, class, interface, type literal, enum, object literal, or array literal that
/// contains it, and the text outside of those lines is left as it was. The lines are what
/// formatting the whole file gives, so when they don't fit in with the text around them (ex. the
/// container is formatted on a single line or the file would format differently with them in
/// place), the member holding the container is formatted instead, and so on out to the whole file.
/// A range that only touches the space between members formats nothing.
///
/// Returns the file text or `None` when nothing changed, along with an error when it failed to
/// parse.
pub fn format_text_range(options: FormatTextOptions, range: Range<usize>) -> Result<Option<String>> {
  let FormatTextOptions {
    path,
    extension,
    text,
    config,
    external_formatter,
    arena_pool,
  } = options;
  let format = |text: Arc<str>| {
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
    return format(text);
  }

  let parsed_source = parse_program(ParseOptions {
    path,
    extension,
    text: body.clone(),
  })?;
  let Some(levels) = parsed_source.with_view(|program| find_levels(program, &range, config)) else {
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
  let formatted_members = formatted_source.with_view(|program| levels.iter().map(|level| find_formatted_members(program, level)).collect::<Vec<_>>());

  for (level, formatted_members) in levels.iter().zip(formatted_members).rev() {
    let Some(formatted_members) = formatted_members else {
      continue;
    };
    let (Some(original_lines), Some(formatted_lines)) = (lines_of(&body, &level.members), lines_of(&formatted, &formatted_members)) else {
      continue;
    };
    let result = format!(
      "{}{}{}",
      &body[..original_lines.start],
      with_file_new_lines(&formatted[formatted_lines], &body),
      &body[original_lines.end..]
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

/// A node holding a list of members (ex. a block's statements) along with the members of it to
/// format.
struct Level {
  /// How to get from the program to the node, which finds it again in the formatted text.
  path: Vec<Step>,
  kind: NodeKind,
  member_count: usize,
  /// Indexes of the members to format. For all but the innermost level, this is only the member
  /// holding the next level.
  touched: RangeInclusive<usize>,
  /// Byte range from the start of the first touched member to the end of the last.
  members: Range<usize>,
}

#[derive(Clone, Copy)]
enum Step {
  Member(usize),
  Child(usize),
}

/// Finds each level from the program to the innermost one containing the range, or `None` when
/// the range only touches the space between members.
fn find_levels(program: Program, range: &Range<usize>, config: &Configuration) -> Option<Vec<Level>> {
  let mut levels = Vec::new();
  let mut path = Vec::new();
  let mut container = Node::from(program);
  loop {
    let members = members(container)?;
    let ranges = members.iter().map(|member| member_range(*member)).collect::<Vec<_>>();
    let mut touched = ranges.iter().enumerate().filter(|(_, member)| touches(member, range)).map(|(index, _)| index);
    let first = touched.next()?;
    let last = touched.next_back().unwrap_or(first);
    let (first, last) = match container {
      Node::Module(_) => with_sorted_declarations(&members, first, last, config),
      _ => (first, last),
    };
    levels.push(Level {
      path: path.clone(),
      kind: container.kind(),
      member_count: members.len(),
      touched: first..=last,
      members: ranges[first].start..ranges[last].end,
    });
    if first != last {
      return Some(levels);
    }
    path.push(Step::Member(first));
    match find_container(program, members[first], range, &mut path) {
      Some(inner) => container = inner,
      None => return Some(levels),
    }
  }
}

/// Widens the touched statements of a module to the run of import or re-export declarations around
/// them when formatting sorts those, since the declarations at the same indexes in the formatted
/// text are only the same ones when taken as a whole run.
fn with_sorted_declarations(members: &[Node], mut first: usize, mut last: usize, config: &Configuration) -> (usize, usize) {
  let is_sorted = |index: usize| match members[index] {
    Node::ImportDecl(decl) => !decl.specifiers.is_empty() && config.module_sort_import_declarations != SortOrder::Maintain,
    Node::ExportAll(_) => config.module_sort_export_declarations != SortOrder::Maintain,
    Node::NamedExport(export) => export.src.is_some() && config.module_sort_export_declarations != SortOrder::Maintain,
    _ => false,
  };
  if (first..=last).any(is_sorted) {
    while first > 0 && is_sorted(first - 1) {
      first -= 1;
    }
    while last + 1 < members.len() && is_sorted(last + 1) {
      last += 1;
    }
  }
  (first, last)
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
    if byte_index(child.start()) <= range.start && range.end <= byte_index(child.end()) {
      path.push(Step::Child(index));
      if let Some(container) = find_container(program, child, range, path) {
        return Some(container);
      }
      path.pop();
    }
  }
  None
}

/// Finds the byte range of the level's touched members in the formatted program.
fn find_formatted_members(program: Program, level: &Level) -> Option<Range<usize>> {
  let mut node = Node::from(program);
  for step in &level.path {
    node = match *step {
      Step::Member(index) => *members(node)?.get(index)?,
      Step::Child(index) => *child_nodes(node).get(index)?,
    };
  }
  if node.kind() != level.kind {
    return None;
  }
  let members = members(node)?;
  if members.len() != level.member_count {
    return None;
  }
  let first = member_range(members[*level.touched.start()]);
  let last = member_range(members[*level.touched.end()]);
  Some(first.start..last.end)
}

fn members(node: Node) -> Option<Vec<Node>> {
  let members: Vec<Node> = match node {
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
  (!members.is_empty()).then_some(members)
}

/// Gets the byte range between the brackets of a node with members, or after the colon of a
/// switch case.
fn inner_range(program: Program, node: Node, members: &[Node]) -> Option<Range<usize>> {
  let first_start = get_stmt_range_with_decorators(members[0]).start;
  let open = program.token_container().get_previous_token(first_start)?;
  let end = match node {
    Node::SwitchCase(_) => node.end(),
    _ => node.end() - 1,
  };
  Some(byte_index(open.end())..byte_index(end))
}

/// Gets the children of a node, skipping parentheses since formatting adds and removes them
/// (ex. around multi-line JSX).
fn child_nodes(node: Node) -> Vec<Node> {
  let mut children = node.children();
  for child in &mut children {
    while let Node::ParenExpr(paren) = child {
      *child = Node::from(&paren.expr);
    }
  }
  children
}

/// Gets the byte range of a member, which includes the decorators before `export`.
fn member_range(node: Node) -> Range<usize> {
  let range = get_stmt_range_with_decorators(node);
  byte_index(range.start)..byte_index(range.end)
}

fn touches(member: &Range<usize>, range: &Range<usize>) -> bool {
  if range.is_empty() {
    member.start <= range.start && range.start <= member.end
  } else {
    range.start < member.end && range.end > member.start
  }
}

/// Gets the lines the members are on, without the last line's line break, or `None` when they
/// share them with other code.
fn lines_of(text: &str, members: &Range<usize>) -> Option<Range<usize>> {
  let start = text[..members.start].rfind('\n').map_or(0, |index| index + 1);
  if !text[start..members.start].chars().all(|c| c == ' ' || c == '\t') {
    return None;
  }
  Some(start..line_end(text, members.end)?)
}

/// Gets the end of the line at the index when only separators and comments come before it.
fn line_end(text: &str, mut index: usize) -> Option<usize> {
  let bytes = text.as_bytes();
  loop {
    match bytes.get(index) {
      None | Some(b'\n') => return Some(index),
      Some(b'\r') if bytes.get(index + 1) == Some(&b'\n') => return Some(index),
      Some(b' ' | b'\t' | b',' | b';') => index += 1,
      Some(b'/') if bytes.get(index + 1) == Some(&b'/') => {
        let end = text[index..].find('\n').map_or(text.len(), |offset| index + offset);
        return Some(if end > 0 && bytes[end - 1] == b'\r' { end - 1 } else { end });
      }
      Some(b'/') if bytes.get(index + 1) == Some(&b'*') => {
        let end = index + 2 + text[index + 2..].find("*/")? + 2;
        if text[index..end].contains('\n') {
          return None;
        }
        index = end;
      }
      _ => return None,
    }
  }
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

fn byte_index(pos: SourcePos) -> usize {
  pos - StartSourcePos::START_SOURCE_POS
}

#[cfg(test)]
mod tests {
  use std::path::Path;

  use super::*;
  use crate::configuration::Configuration;
  use crate::configuration::ConfigurationBuilder;

  #[test]
  fn keeps_bom_outside_range() {
    // the spec files can't express this since editors strip the bom
    let config = ConfigurationBuilder::new().build();
    let output = format_b(&config, "\u{FEFF}const  a  =  1;\nconst  b  =  2;\n").unwrap();
    assert_eq!(output, "\u{FEFF}const  a  =  1;\nconst b = 2;\n");
  }

  #[test]
  fn keeps_file_line_endings() {
    // the spec files can't express this since they normalize line endings
    let config = ConfigurationBuilder::new().build();
    let output = format_b(&config, "const  a  =  1;\r\nconst  b  =  {\r\nc:1};\r\n").unwrap();
    assert_eq!(output, "const  a  =  1;\r\nconst b = {\r\n  c: 1,\r\n};\r\n");

    let config = ConfigurationBuilder::new().new_line_kind(NewLineKind::CarriageReturnLineFeed).build();
    let output = format_b(&config, "const  a  =  1;\nconst  b  =  {\nc:1};\n").unwrap();
    assert_eq!(output, "const  a  =  1;\nconst b = {\n  c: 1,\n};\n");
  }

  #[test]
  fn formats_nothing_with_ignore_file_comment() {
    let config = ConfigurationBuilder::new().build();
    assert_eq!(format_b(&config, "// dprint-ignore-file\nconst  a  =  1;\nconst  b  =  2;\n"), None);
  }

  fn format_b(config: &Configuration, text: &str) -> Option<String> {
    let start = text.find("const  b").unwrap();
    format_text_range(
      FormatTextOptions {
        path: Path::new("file.ts"),
        extension: None,
        text: text.into(),
        config,
        external_formatter: None,
        arena_pool: None,
      },
      start..start + 8,
    )
    .unwrap()
  }
}
