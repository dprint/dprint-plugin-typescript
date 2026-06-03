use deno_ast::oxc::ast::ast::Argument;
use deno_ast::oxc::ast::ast::CallExpression;
use deno_ast::oxc::ast::ast::Comment;
use deno_ast::oxc::ast::ast::Expression;
use deno_ast::oxc::ast::ast::JSXExpression;
use deno_ast::oxc::ast::ast::Statement;

use super::oxc_helpers::CommentExt;
use super::oxc_helpers::Node;
use super::oxc_helpers::ProgramInfo;
use super::oxc_helpers::SourcePos;
use super::oxc_helpers::SourceRanged;
use super::oxc_helpers::TokenExt;
use super::to_node::binding_pattern_to_node;

pub fn is_first_node_on_line(node: &impl SourceRanged, program: ProgramInfo) -> bool {
  let start = node.start() as usize;
  let source_file_text = program.text().as_bytes();

  for i in (0..start).rev() {
    let c = source_file_text[i];
    if c != b' ' && c != b'\t' {
      return c == b'\n';
    }
  }

  true
}

pub fn has_separating_blank_line(first_node: &impl SourceRanged, second_node: &impl SourceRanged, program: ProgramInfo) -> bool {
  return get_second_start_line(first_node, second_node, program) > first_node.end_line_fast(program) + 1;

  fn get_second_start_line(first_node: &impl SourceRanged, second_node: &impl SourceRanged, program: ProgramInfo) -> usize {
    let leading_comments = second_node.leading_comments_fast(program);

    for comment in leading_comments {
      let comment_start_line = comment.start_line_fast(program);
      if comment_start_line > first_node.end_line_fast(program) {
        return comment_start_line;
      }
    }

    second_node.start_line_fast(program)
  }
}

pub fn get_use_new_lines_for_nodes(first_node: &impl SourceRanged, second_node: &impl SourceRanged, program: ProgramInfo) -> bool {
  first_node.end_line_fast(program) != second_node.start_line_fast(program)
}

pub fn has_leading_comment_on_different_line<'a>(node: &impl SourceRanged, comments_to_ignore: Option<&[&'a Comment]>, program: ProgramInfo<'a>) -> bool {
  get_leading_comment_on_different_line(node, comments_to_ignore, program).is_some()
}

pub fn get_leading_comment_on_different_line<'a>(
  node: &impl SourceRanged,
  comments_to_ignore: Option<&[&'a Comment]>,
  program: ProgramInfo<'a>,
) -> Option<&'a Comment> {
  let comments_to_ignore: Option<Vec<SourcePos>> = comments_to_ignore.map(|x| x.iter().map(|c| c.start()).collect());
  let node_start_line = node.start_line_fast(program);
  for comment in node.leading_comments_fast(program) {
    if let Some(comments_to_ignore) = &comments_to_ignore {
      if comments_to_ignore.contains(&comment.start()) {
        continue;
      }
    }

    if comment.start_line_fast(program) < node_start_line {
      return Some(comment);
    }
  }

  None
}

pub fn has_surrounding_comments<'a>(node: Node<'a>, program: ProgramInfo<'a>) -> bool {
  !node.leading_comments_fast(program).is_empty() || !node.trailing_comments_fast(program).is_empty()
}

pub fn has_surrounding_different_line_comments<'a>(node: Node<'a>, program: ProgramInfo<'a>) -> bool {
  let leading_comments = node.leading_comments_fast(program);
  if !leading_comments.is_empty() {
    let start_line = node.start_line_fast(program);
    for leading_comment in leading_comments {
      if leading_comment.start_line_fast(program) != start_line {
        return true;
      }
    }
  }
  let trailing_comments = node.trailing_comments_fast(program);
  if !trailing_comments.is_empty() {
    let end_line = node.end_line_fast(program);
    for trailing_comment in trailing_comments {
      if trailing_comment.is_line() || trailing_comment.end_line_fast(program) != end_line {
        return true;
      }
    }
  }

  false
}

pub fn has_jsx_space_expr_text(node: Node, program: ProgramInfo) -> bool {
  get_jsx_space_expr_space_count(node, program) > 0
}

pub fn get_jsx_space_expr_space_count(node: Node, program: ProgramInfo) -> usize {
  // A "JSX space expression" is a JSXExpressionContainer with
  // a string literal containing only spaces.
  // * {" "}
  // * {"      "}
  match node {
    Node::JSXExpressionContainer(container) => {
      let JSXExpression::StringLiteral(text) = &container.expression else {
        return 0;
      };
      let mut space_count = 0;
      let text = remove_quotes_from_str(text.text_fast(program));
      for c in text.chars() {
        if c == ' ' {
          space_count += 1;
        } else {
          return 0; // must be all spaces
        }
      }
      space_count
    }
    _ => 0,
  }
}

fn remove_quotes_from_str(text: &str) -> &str {
  if text.is_empty() {
    text
  } else if text.starts_with("'") && text.ends_with("'") || text.starts_with("\"") && text.ends_with("\"") {
    &text[1..text.len() - 1]
  } else {
    text
  }
}

pub fn count_spaces_between_jsx_children<'a>(previous_node: Node<'a>, next_node: Node<'a>, program: ProgramInfo<'a>) -> usize {
  let mut count = count_jsx_space_exprs_in_text(&program.text()[previous_node.end() as usize..next_node.start() as usize]);
  if jsx_text_has_trailing_space(previous_node, program) {
    count += 1;
  }
  if jsx_text_has_leading_space(next_node, program) {
    count += 1;
  }
  count
}

fn jsx_text_has_trailing_space(node: Node, program: ProgramInfo) -> bool {
  if let Node::JSXText(text) = node {
    let text = text.text_fast(program);
    crate::utils::has_no_new_lines_in_trailing_whitespace(text) && text.ends_with(' ')
  } else {
    false
  }
}

fn jsx_text_has_leading_space(node: Node, program: ProgramInfo) -> bool {
  if let Node::JSXText(text) = node {
    let text = text.text_fast(program);
    crate::utils::has_no_new_lines_in_leading_whitespace(text) && text.starts_with(' ')
  } else {
    false
  }
}

fn count_jsx_space_exprs_in_text(text: &str) -> usize {
  let mut count = 0;
  let mut current_space_run = String::new();
  let chars = text.char_indices().collect::<Vec<_>>();
  let mut i = 0;

  while i < chars.len() {
    let (char_start, c) = chars[i];
    if c == '{' {
      count += count_same_line_space_run(&current_space_run);
      current_space_run.clear();

      if let Some((space_count, next_byte_index)) = parse_jsx_space_expr(&text[char_start..]) {
        count += space_count;
        i = chars.partition_point(|(byte_index, _)| *byte_index < char_start + next_byte_index);
        continue;
      }
    }

    if c.is_whitespace() {
      current_space_run.push(c);
    } else {
      current_space_run.clear();
    }
    i += 1;
  }

  count + count_same_line_space_run(&current_space_run)
}

fn count_same_line_space_run(text: &str) -> usize {
  if !text.is_empty() && text.chars().all(|c| c == ' ' || c == '\t') {
    1
  } else {
    0
  }
}

fn parse_jsx_space_expr(text: &str) -> Option<(usize, usize)> {
  let bytes = text.as_bytes();
  if bytes.first().copied() != Some(b'{') {
    return None;
  }

  let mut i = 1;
  skip_ascii_whitespace(bytes, &mut i);
  let quote = *bytes.get(i)?;
  if quote != b'\'' && quote != b'"' {
    return None;
  }
  i += 1;

  let space_start = i;
  while bytes.get(i).copied() == Some(b' ') {
    i += 1;
  }
  let space_count = i - space_start;
  if space_count == 0 || bytes.get(i).copied() != Some(quote) {
    return None;
  }
  i += 1;
  skip_ascii_whitespace(bytes, &mut i);
  if bytes.get(i).copied() != Some(b'}') {
    return None;
  }

  Some((space_count, i + 1))
}

fn skip_ascii_whitespace(bytes: &[u8], i: &mut usize) {
  while matches!(bytes.get(*i), Some(b' ' | b'\t' | b'\n' | b'\r')) {
    *i += 1;
  }
}

/// Tests if this is a call expression from common test libraries.
pub fn is_test_library_call_expr<'a>(node: &CallExpression<'a>, program: ProgramInfo<'a>) -> bool {
  // Be very strict here to allow the user to opt out if they'd like.
  if node.arguments.len() != 2 || node.type_arguments.is_some() || !is_valid_callee(&node.callee) {
    return false;
  }
  if !matches!(node.arguments[0], Argument::StringLiteral(_) | Argument::TemplateLiteral(_)) {
    return false;
  }

  match &node.arguments[1] {
    Argument::FunctionExpression(fn_expr) => {
      if !fn_expr.params.items.is_empty() {
        return false;
      }
    }
    Argument::ArrowFunctionExpression(arrow_expr) => {
      if arrow_expr.params.items.len() > 1 {
        return false;
      }
      // allow something like `Deno.test("desc", (t) => {})`
      if let Some(param) = arrow_expr.params.items.first() {
        let has_open_paren_comment = param
          .previous_token_fast(program)
          .is_some_and(|token| token.text_fast(program) == "(" && !token.trailing_comments_fast(program).is_empty());
        if has_open_paren_comment
          || has_surrounding_comments(Node::FormalParameter(param), program)
          || has_surrounding_comments(binding_pattern_to_node(&param.pattern), program)
        {
          return false;
        }
      }
    }
    _ => return false,
  }

  return node.start_line_fast(program) == node.arguments[1].start_line_fast(program);

  fn is_valid_callee(callee: &Expression) -> bool {
    return match get_first_identifier_text_from_expr(callee) {
      Some("it") | Some("describe") | Some("test") => true,
      _ => {
        // support call expressions like `Deno.test("description", ...)`
        matches!(get_last_identifier_text(callee), Some("test"))
      }
    };

    fn get_first_identifier_text_from_expr<'a, 'b>(expr: &'b Expression<'a>) -> Option<&'b str> {
      match expr {
        Expression::Identifier(ident) => Some(ident.name.as_str()),
        Expression::StaticMemberExpression(member) => get_first_identifier_text_from_expr(&member.object),
        _ => None,
      }
    }

    fn get_last_identifier_text<'a, 'b>(expr: &'b Expression<'a>) -> Option<&'b str> {
      match expr {
        Expression::Identifier(ident) => Some(ident.name.as_str()),
        Expression::StaticMemberExpression(member) if matches!(member.object, Expression::Identifier(_)) => Some(member.property.name.as_str()),
        _ => None,
      }
    }
  }
}

pub fn is_expr_stmt_or_body_with_single_expr_stmt(node: Node) -> bool {
  match node {
    Node::ExpressionStatement(_) => true,
    Node::BlockStatement(block) => block.body.len() == 1 && matches!(block.body[0], Statement::ExpressionStatement(_)),
    _ => false,
  }
}
