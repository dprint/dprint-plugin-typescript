use deno_ast::oxc::ast::ast::BinaryExpression;
use deno_ast::oxc::ast::ast::Expression;
use deno_ast::oxc::ast::ast::LogicalExpression;
use deno_ast::oxc::parser::Token;
use deno_ast::oxc::span::GetSpan;
use deno_ast::oxc::span::Span;
use deno_ast::oxc::syntax::operator::BinaryOperator;
use deno_ast::oxc::syntax::operator::LogicalOperator;

use super::super::Context;
use super::extensions::*;
use crate::generation::oxc_helpers::Node;
use crate::generation::oxc_helpers::PosExt;
use crate::generation::oxc_helpers::SourceRanged;
use crate::generation::to_node::expr_to_node;
use crate::configuration::*;

/// SWC unified binary and logical expressions as `BinExpr`; oxc keeps them as
/// distinct `BinaryExpression` / `LogicalExpression` nodes. This enum recovers
/// the unified view the flattening logic needs.
#[derive(Copy, Clone)]
pub enum BinaryLikeExpr<'a> {
  Binary(&'a BinaryExpression<'a>),
  Logical(&'a LogicalExpression<'a>),
}

impl<'a> BinaryLikeExpr<'a> {
  pub fn from_expr(expr: &'a Expression<'a>) -> Option<BinaryLikeExpr<'a>> {
    match expr {
      Expression::BinaryExpression(node) => Some(BinaryLikeExpr::Binary(node)),
      Expression::LogicalExpression(node) => Some(BinaryLikeExpr::Logical(node)),
      _ => None,
    }
  }

  pub fn left(&self) -> &'a Expression<'a> {
    match self {
      BinaryLikeExpr::Binary(node) => &node.left,
      BinaryLikeExpr::Logical(node) => &node.left,
    }
  }

  pub fn right(&self) -> &'a Expression<'a> {
    match self {
      BinaryLikeExpr::Binary(node) => &node.right,
      BinaryLikeExpr::Logical(node) => &node.right,
    }
  }

  pub fn op(&self) -> BinaryLikeOp {
    match self {
      BinaryLikeExpr::Binary(node) => BinaryLikeOp::Binary(node.operator),
      BinaryLikeExpr::Logical(node) => BinaryLikeOp::Logical(node.operator),
    }
  }
}

impl<'a> GetSpan for BinaryLikeExpr<'a> {
  fn span(&self) -> Span {
    match self {
      BinaryLikeExpr::Binary(node) => node.span,
      BinaryLikeExpr::Logical(node) => node.span,
    }
  }
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum BinaryLikeOp {
  Binary(BinaryOperator),
  Logical(LogicalOperator),
}

impl BinaryLikeOp {
  pub fn as_str(&self) -> &'static str {
    match self {
      BinaryLikeOp::Binary(op) => op.as_str(),
      BinaryLikeOp::Logical(op) => op.as_str(),
    }
  }

  pub fn is_add_sub(&self) -> bool {
    matches!(self, BinaryLikeOp::Binary(op) if op.is_add_sub())
  }

  pub fn is_mul_div(&self) -> bool {
    matches!(self, BinaryLikeOp::Binary(op) if op.is_mul_div())
  }

  pub fn is_logical(&self) -> bool {
    matches!(self, BinaryLikeOp::Logical(_))
  }

  pub fn is_bitwise_or_arithmetic(&self) -> bool {
    matches!(self, BinaryLikeOp::Binary(op) if op.is_bitwise_or_arithmetic())
  }

  pub fn is_bit_logical(&self) -> bool {
    matches!(self, BinaryLikeOp::Binary(op) if op.is_bit_logical())
  }

  pub fn is_bit_shift(&self) -> bool {
    matches!(self, BinaryLikeOp::Binary(op) if op.is_bit_shift())
  }

  pub fn is_equality(&self) -> bool {
    matches!(self, BinaryLikeOp::Binary(op) if op.is_equality())
  }
}

pub struct BinExprItem<'a> {
  pub pre_op: Option<BinExprOp<'a>>,
  pub post_op: Option<BinExprOp<'a>>,
  pub expr: Node<'a>,
}

#[derive(Clone)]
pub struct BinExprOp<'a> {
  pub token: &'a Token,
  pub op: BinaryLikeOp,
}

pub fn get_flattened_bin_expr<'a>(node: BinaryLikeExpr<'a>, context: &mut Context<'a>) -> Vec<BinExprItem<'a>> {
  let mut items = Vec::new();
  let operator_token = BinExprOp {
    token: find_operator_token_after(node.left(), node.op(), context),
    op: node.op(),
  };
  let is_op_same_line = get_operator_position(node, operator_token.token, context) == OperatorPosition::SameLine;
  let mut handled_left = false;
  let mut handled_right = false;

  if let Some(left_bin) = BinaryLikeExpr::from_expr(node.left()) {
    if is_expression_breakable(node.op(), left_bin.op()) {
      items.extend(get_flattened_bin_expr(left_bin, context));
      if is_op_same_line {
        items.last_mut().unwrap().post_op = Some(operator_token.clone());
      }
      handled_left = true;
    }
  }

  if !handled_left {
    items.push(BinExprItem {
      pre_op: None,
      post_op: if is_op_same_line { Some(operator_token.clone()) } else { None },
      expr: expr_to_node(node.left()),
    });
  }

  if let Some(right_bin) = BinaryLikeExpr::from_expr(node.right()) {
    if is_expression_breakable(node.op(), right_bin.op()) {
      let mut right_items = get_flattened_bin_expr(right_bin, context);
      if !is_op_same_line {
        right_items.first_mut().unwrap().pre_op = Some(operator_token.clone());
      }
      items.extend(right_items);
      handled_right = true;
    }
  }

  if !handled_right {
    items.push(BinExprItem {
      pre_op: if !is_op_same_line { Some(operator_token) } else { None },
      post_op: None,
      expr: expr_to_node(node.right()),
    });
  }

  return items;

  /// Locate the binary operator's leading token, immediately after the left
  /// operand. Most of the time this is just `get_first_operator_after` with
  /// the operator's full text (e.g. `<=`), but the lexer can split
  /// multi-character operators into separate tokens when the operator
  /// follows a TypeScript type position. For example `0 as number <= 1`
  /// tokenizes `<=` as `<` then `=`. In that case we fall back to matching
  /// just the operator's first character (`<`), which is always a single
  /// token whose start position is what callers actually need (e.g. for
  /// line lookups).
  fn find_operator_token_after<'a>(left: &impl SourceRanged, op: BinaryLikeOp, context: &mut Context<'a>) -> &'a Token {
    let op_text = op.as_str();
    if let Some(tok) = context.token_finder.get_first_operator_after(left, op_text) {
      return tok;
    }
    if op_text.len() > 1 {
      let first_char_text = &op_text[..1];
      if let Some(tok) = context.token_finder.get_first_operator_after(left, first_char_text) {
        return tok;
      }
    }
    panic!("could not locate operator token for binary op `{op_text}`");
  }

  fn is_expression_breakable(top_op: BinaryLikeOp, op: BinaryLikeOp) -> bool {
    if top_op.is_add_sub() {
      op.is_add_sub()
    } else if top_op.is_mul_div() {
      op.is_mul_div()
    } else {
      top_op == op
    }
  }

  fn get_operator_position(node: BinaryLikeExpr, operator_token: &Token, context: &Context) -> OperatorPosition {
    match context.config.binary_expression_operator_position {
      OperatorPosition::NextLine => OperatorPosition::NextLine,
      OperatorPosition::SameLine => OperatorPosition::SameLine,
      OperatorPosition::Maintain => {
        if node.left().end_line_fast(context.program) == operator_token.start().line_fast(context.program) {
          OperatorPosition::SameLine
        } else {
          OperatorPosition::NextLine
        }
      }
    }
  }
}
