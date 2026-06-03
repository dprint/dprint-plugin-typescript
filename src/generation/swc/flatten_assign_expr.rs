use deno_ast::oxc::ast::ast::AssignmentExpression;
use deno_ast::oxc::ast::ast::AssignmentOperator;
use deno_ast::oxc::ast::ast::Expression;
use deno_ast::oxc::parser::Token;

use super::super::Context;
use crate::generation::oxc_helpers::Node;
use crate::generation::to_node::assign_target_to_node;
use crate::generation::to_node::expr_to_node;

pub struct AssignExprItem<'a> {
  pub post_op: Option<&'a Token>,
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
      expr: assign_target_to_node(&node.left),
    });

    if let Expression::AssignmentExpression(inner) = &node.right {
      if inner.operator == AssignmentOperator::Assign {
        current_node = Some(&**inner);
      }
    }

    if current_node.is_none() {
      // add the last item
      items.push(AssignExprItem {
        post_op: None,
        expr: expr_to_node(&node.right),
      });
    }
  }

  items
}
