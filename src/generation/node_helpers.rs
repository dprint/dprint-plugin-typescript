use crate::ast::*;

pub fn is_first_node_on_line(node: &impl SourceRanged, program: ProgramInfo) -> bool {
  let start = node.start().as_index();
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
  get_separating_blank_line_count(first_node, second_node, program) > 0
}

/// Gets the number of blank lines between two nodes, where the second node starts
/// at its first leading comment on a later line than the first node.
pub fn get_separating_blank_line_count(first_node: &impl SourceRanged, second_node: &impl SourceRanged, program: ProgramInfo) -> usize {
  let second_start_line = get_second_start_line(first_node, second_node, program);
  // this is a cheaper check that's needed for there to be a blank line
  if second_start_line <= first_node.end_line_fast(program) + 1 {
    return 0;
  }
  let first_end_line = get_first_end_line_with_comments_before_comma(first_node, program);
  return second_start_line.saturating_sub(first_end_line + 1);

  /// The comments between the first node and a comma after it go after the comma,
  /// so they're with the first node (ex. `// c` in `A\n// c\n, B`).
  fn get_first_end_line_with_comments_before_comma(first_node: &impl SourceRanged, program: ProgramInfo) -> usize {
    let end_line = first_node.end_line_fast(program);
    match first_node.next_token_fast(program) {
      Some(token) if token.kind == TokenKind::Comma => match token.start().leading_comments_fast(program).peek_last_comment() {
        Some(comment) => comment.end_line_fast(program).max(end_line),
        None => end_line,
      },
      _ => end_line,
    }
  }

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

pub fn has_leading_comment_on_different_line<'a>(node: &impl SourceRanged, comments_to_ignore: Option<&[&'a Comment<'a>]>, program: ProgramInfo<'a>) -> bool {
  get_leading_comment_on_different_line(node, comments_to_ignore, program).is_some()
}

pub fn get_leading_comment_on_different_line<'a>(
  node: &impl SourceRanged,
  comments_to_ignore: Option<&[&'a Comment<'a>]>,
  program: ProgramInfo<'a>,
) -> Option<&'a Comment<'a>> {
  let comments_to_ignore: Option<Vec<SourcePos>> = comments_to_ignore.map(|x| x.iter().map(|c| c.start()).collect());
  let node_start_line = node.start_line_fast(program);
  for comment in node.leading_comments_fast(program) {
    if let Some(comments_to_ignore) = &comments_to_ignore
      && comments_to_ignore.contains(&comment.start())
    {
      continue;
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
      if trailing_comment.kind == CommentKind::Line || trailing_comment.end_line_fast(program) != end_line {
        return true;
      }
    }
  }

  false
}

pub fn nodes_have_only_spaces_between<'a>(previous_node: Node<'a>, next_node: Node<'a>, program: ProgramInfo<'a>) -> bool {
  if let Node::JSXText(previous_node) = previous_node {
    crate::utils::has_jsx_trailing_space(previous_node.text_fast(program))
  } else if let Node::JSXText(next_node) = next_node {
    crate::utils::has_jsx_leading_space(next_node.text_fast(program))
  } else {
    let range = SourceRange::new(previous_node.end(), next_node.start());
    crate::utils::is_jsx_space(range.text_fast(program))
  }
}

/// Gets the jsx children between the two jsx children.
pub fn get_siblings_between<'a>(node_a: Node<'a>, node_b: Node<'a>, program: ProgramInfo<'a>) -> Vec<Node<'a>> {
  let children: &'a [JSXChild<'a>] = match node_a.parent(program) {
    Some(Node::JSXElement(element)) => &element.children,
    Some(Node::JSXFragment(fragment)) => &fragment.children,
    _ => return Vec::new(),
  };
  let children = children.iter().map(|c| c.into_node()).collect::<Vec<_>>();
  let a_index = children.iter().position(|c| c.is_same(node_a));
  let b_index = children.iter().position(|c| c.is_same(node_b));
  match (a_index, b_index) {
    (Some(a_index), Some(b_index)) if a_index < b_index => children[a_index + 1..b_index].to_vec(),
    _ => Vec::new(),
  }
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
    Node::JSXExpressionContainer(JSXExpressionContainer {
      expression: JSXExpression::StringLiteral(text),
      ..
    }) => {
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

pub fn count_spaces_between_jsx_children<'a>(previous_node: Node<'a>, next_node: Node<'a>, program: ProgramInfo<'a>) -> usize {
  let all_siblings_between = get_siblings_between(previous_node, next_node, program);
  let siblings_between = all_siblings_between
    .into_iter()
    // ignore empty JSXText
    .filter(|n| !crate::utils::is_jsx_whitespace(n.text_fast(program)))
    .collect::<Vec<_>>();

  let mut count = 0;
  let mut previous_node = previous_node;

  for node in siblings_between {
    count += get_jsx_space_expr_space_count(node, program);

    if nodes_have_only_spaces_between(previous_node, node, program) {
      count += 1;
    }

    previous_node = node;
  }

  // check the spaces between the previously looked at node and last node
  if nodes_have_only_spaces_between(previous_node, next_node, program) {
    count += 1;
  }

  count
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
      if !fn_expr.params.items.is_empty() || fn_expr.params.rest.is_some() || fn_expr.this_param.is_some() {
        return false;
      }
    }
    Argument::ArrowFunctionExpression(arrow_expr) => {
      let params = &arrow_expr.params;
      if params.items.len() + usize::from(params.rest.is_some()) > 1 {
        return false;
      }
      // allow something like `Deno.test("desc", (t) => {})`
      if let Some(param) = params.items.first()
        && has_surrounding_comments(param.into_node(), program)
      {
        return false;
      }
      if let Some(rest) = &params.rest
        && has_surrounding_comments(rest.into_node(), program)
      {
        return false;
      }
    }
    _ => return false,
  }

  return node.start_line_fast(program) == node.arguments[1].start_line_fast(program);

  fn is_valid_callee<'a>(callee: &Expression<'a>) -> bool {
    return match get_first_identifier_text_from_expr(callee) {
      Some("it") | Some("describe") | Some("test") => true,
      _ => {
        // support call expressions like `Deno.test("description", ...)`
        matches!(get_last_identifier_text(callee), Some("test"))
      }
    };

    fn get_first_identifier_text_from_expr<'a>(expr: &Expression<'a>) -> Option<&'a str> {
      match expr {
        Expression::Identifier(ident) => Some(ident.name.as_str()),
        Expression::StaticMemberExpression(member) => get_first_identifier_text_from_expr(&member.object),
        _ => None,
      }
    }

    fn get_last_identifier_text<'a>(expr: &Expression<'a>) -> Option<&'a str> {
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

fn remove_quotes_from_str(text: &str) -> &str {
  if text.is_empty() {
    text
  } else if text.starts_with("'") && text.ends_with("'") || text.starts_with("\"") && text.ends_with("\"") {
    &text[1..text.len() - 1]
  } else {
    text
  }
}
