use deno_ast::swc::common::comments::Comment;
use deno_ast::swc::common::comments::CommentKind;
use deno_ast::view::*;
use deno_ast::SourcePos;
use deno_ast::SourceRange;
use deno_ast::SourceRanged;
use deno_ast::SourceRangedForSpanned;
use deno_ast::SourceTextInfoProvider;

pub fn is_first_node_on_line(node: &impl SourceRanged, program: Program) -> bool {
  let text_info = program.text_info();
  let start = node.start().as_byte_index(text_info.range().start);
  let source_file_text = text_info.text_str().as_bytes();

  for i in (0..start).rev() {
    let c = source_file_text[i];
    if c != b' ' && c != b'\t' {
      return c == b'\n';
    }
  }

  true
}

pub fn has_separating_blank_line(first_node: &impl SourceRanged, second_node: &impl SourceRanged, program: Program) -> bool {
  return get_second_start_line(first_node, second_node, program) > first_node.end_line_fast(program) + 1;

  fn get_second_start_line(first_node: &impl SourceRanged, second_node: &impl SourceRanged, program: Program) -> usize {
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

pub fn get_use_new_lines_for_nodes(first_node: &impl SourceRanged, second_node: &impl SourceRanged, program: Program) -> bool {
  first_node.end_line_fast(program) != second_node.start_line_fast(program)
}

pub fn has_leading_comment_on_different_line<'a>(node: &impl SourceRanged, comments_to_ignore: Option<&[&'a Comment]>, program: Program<'a>) -> bool {
  get_leading_comment_on_different_line(node, comments_to_ignore, program).is_some()
}

pub fn get_leading_comment_on_different_line<'a>(
  node: &impl SourceRanged,
  comments_to_ignore: Option<&[&'a Comment]>,
  program: Program<'a>,
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

pub fn has_surrounding_comments<'a>(node: Node<'a>, program: Program<'a>) -> bool {
  !node.leading_comments_fast(program).is_empty() || !node.trailing_comments_fast(program).is_empty()
}

pub fn has_surrounding_different_line_comments<'a>(node: Node<'a>, program: Program<'a>) -> bool {
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

pub fn nodes_have_only_spaces_between<'a>(previous_node: Node<'a>, next_node: Node<'a>, program: Program<'a>) -> bool {
  if let Node::JSXText(previous_node) = previous_node {
    let previous_node_text = previous_node.text_fast(program);
    crate::utils::has_no_new_lines_in_trailing_whitespace(previous_node_text) && previous_node_text.ends_with(' ')
  } else if let Node::JSXText(next_node) = next_node {
    let next_node_text = next_node.text_fast(program);
    crate::utils::has_no_new_lines_in_leading_whitespace(next_node_text) && next_node_text.starts_with(' ')
  } else {
    let text_info = program.text_info();
    let range = SourceRange::new(previous_node.end(), next_node.start());
    crate::utils::is_not_empty_and_only_spaces(text_info.range_text(&range))
  }
}

pub fn get_siblings_between<'a, 'b>(node_a: Node<'a>, node_b: Node<'b>) -> Vec<Node<'a>> {
  let mut parent_children = node_a.parent().unwrap().children();
  parent_children.drain(node_a.child_index() + 1..node_b.child_index()).collect()
}

pub fn has_jsx_space_expr_text(node: Node) -> bool {
  get_jsx_space_expr_space_count(node) > 0
}

pub fn get_jsx_space_expr_space_count(node: Node) -> usize {
  // A "JSX space expression" is a JSXExprContainer with
  // a string literal containing only spaces.
  // * {" "}
  // * {"      "}
  match node {
    Node::JSXExprContainer(JSXExprContainer {
      expr: JSXExpr::Expr(Expr::Lit(Lit::Str(text))),
      ..
    }) => {
      let mut space_count = 0;
      for c in text.value().chars() {
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

pub fn count_spaces_between_jsx_children<'a>(previous_node: Node<'a>, next_node: Node<'a>, program: Program<'a>) -> usize {
  let all_siblings_between = get_siblings_between(previous_node, next_node);
  let siblings_between = all_siblings_between
    .into_iter()
    // ignore empty JSXText
    .filter(|n| !n.text_fast(program).trim().is_empty())
    .collect::<Vec<_>>();

  let mut count = 0;
  let mut previous_node = previous_node;

  for node in siblings_between {
    count += get_jsx_space_expr_space_count(node);

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
pub fn is_test_library_call_expr<'a>(node: &CallExpr<'a>, program: Program<'a>) -> bool {
  // Be very strict here to allow the user to opt out if they'd like.
  if node.args.len() != 2 || node.type_args.is_some() || !is_valid_callee(&node.callee) {
    return false;
  }
  if node.args[0].expr.kind() != NodeKind::Str && !node.args[0].expr.is::<Tpl>() {
    return false;
  }

  match node.args[1].expr {
    Expr::Fn(fn_expr) => {
      if !fn_expr.function.params.is_empty() {
        return false;
      }
    }
    Expr::Arrow(arrow_expr) => {
      if arrow_expr.params.len() > 1 {
        return false;
      }
      // allow something like `Deno.test("desc", (t) => {})`
      if let Some(param) = arrow_expr.params.first() {
        if has_surrounding_comments(param.into(), program) {
          return false;
        }
      }
    }
    _ => return false,
  }

  return node.start_line_fast(program) == node.args[1].start_line_fast(program);

  fn is_valid_callee<'a>(callee: &Callee<'a>) -> bool {
    return match get_first_identifier_text_from_callee(callee) {
      Some("it") | Some("describe") | Some("test") => true,
      _ => {
        // support call expressions like `Deno.test("description", ...)`
        matches!(get_last_identifier_text(callee), Some("test"))
      }
    };

    fn get_first_identifier_text_from_callee<'a>(callee: &Callee<'a>) -> Option<&'a str> {
      match callee {
        Callee::Super(_) | Callee::Import(_) => None,
        Callee::Expr(expr) => get_first_identifier_text_from_expr(expr),
      }
    }

    fn get_first_identifier_text_from_expr<'a>(expr: &Expr<'a>) -> Option<&'a str> {
      match expr {
        Expr::Ident(ident) => Some(ident.sym()),
        Expr::Member(member) if matches!(member.prop, MemberProp::Ident(_)) => get_first_identifier_text_from_expr(&member.obj),
        _ => None,
      }
    }

    fn get_last_identifier_text<'a>(callee: &Callee<'a>) -> Option<&'a str> {
      return match callee {
        Callee::Expr(expr) => from_expr(expr),
        Callee::Super(_) | Callee::Import(_) => None,
      };

      fn from_expr<'a>(expr: &Expr<'a>) -> Option<&'a str> {
        match expr {
          Expr::Ident(ident) => Some(ident.sym()),
          Expr::Member(member) if matches!(member.obj, Expr::Ident(_)) => from_member_prop(&member.prop),
          _ => None,
        }
      }

      fn from_member_prop<'a>(member_prop: &MemberProp<'a>) -> Option<&'a str> {
        match member_prop {
          MemberProp::Ident(ident) => Some(ident.sym()),
          _ => None,
        }
      }
    }
  }
}

pub fn is_expr_stmt_or_body_with_single_expr_stmt(node: Node) -> bool {
  match node {
    Node::ExprStmt(_) => true,
    Node::BlockStmt(block) => block.stmts.len() == 1 && block.stmts[0].is::<ExprStmt>(),
    _ => false,
  }
}
