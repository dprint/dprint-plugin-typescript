use super::super::Context;
use deno_ast::swc::parser::token::TokenAndSpan;
use deno_ast::view::*;

pub struct AssignExprItem<'a> {
  pub post_op: Option<&'a TokenAndSpan>,
  pub expr: Node<'a>,
}

pub fn get_flattened_assign_expr<'a, 'b>(node: &'b AssignExpr<'a>, context: &mut Context<'a>) -> Vec<AssignExprItem<'a>> {
  assert_eq!(node.op(), AssignOp::Assign);
  let mut items = Vec::new();
  let mut current_node = Some(node);
  while let Some(node) = current_node.take() {
    let operator_token = context.token_finder.get_first_operator_after(&node.left, node.op().as_str()).unwrap();
    items.push(AssignExprItem {
      post_op: Some(operator_token),
      expr: node.left.into(),
    });

    if let Expr::Assign(node) = node.right {
      if node.op() == AssignOp::Assign {
        current_node = Some(node);
      }
    }

    if current_node.is_none() {
      // add the last item
      items.push(AssignExprItem {
        post_op: None,
        expr: node.right.into(),
      });
    }
  }

  items
}
