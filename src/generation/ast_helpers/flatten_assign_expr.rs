use super::super::Context;
use crate::ast::*;

pub struct AssignExprItem<'a> {
  pub post_op: Option<&'a TokenAndSpan>,
  pub expr: Node<'a>,
}

pub fn get_flattened_assign_expr<'a>(node: &'a AssignmentExpression<'a>, context: &mut Context<'a>) -> Vec<AssignExprItem<'a>> {
  assert_eq!(node.operator, AssignmentOperator::Assign);
  let mut items = Vec::new();
  let mut current_node = Some(node);
  while let Some(node) = current_node.take() {
    let operator_token = context.token_finder.get_first_operator_after(&node.left, node.operator.as_str()).unwrap();
    items.push(AssignExprItem {
      post_op: Some(operator_token),
      expr: (&node.left).into_node(),
    });

    if let Expression::AssignmentExpression(node) = &node.right
      && node.operator == AssignmentOperator::Assign
    {
      current_node = Some(node);
    }

    if current_node.is_none() {
      // add the last item
      items.push(AssignExprItem {
        post_op: None,
        expr: (&node.right).into_node(),
      });
    }
  }

  items
}
