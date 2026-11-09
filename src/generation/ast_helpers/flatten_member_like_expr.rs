use dprint_core::formatting::bumpalo::Bump;

use super::super::node_helpers;
use crate::ast::*;
use crate::generation::context::BumpVec;

pub struct FlattenedMemberLikeExpr<'a> {
  pub nodes: BumpVec<'a, MemberLikeExprItem<'a>>,
}

pub enum MemberLikeExprItem<'a> {
  Node(Node<'a>),
  Token(&'a TokenAndSpan),
  /// The `[expr]` of a computed member expression.
  Computed(&'a MemberLikeExprItemComputed<'a>),
  CallExpr(&'a MemberLikeExprItemCallExpr<'a>),
}

impl<'a> SourceRanged for MemberLikeExprItem<'a> {
  fn start(&self) -> SourcePos {
    match self {
      MemberLikeExprItem::Node(node) => node.start(),
      MemberLikeExprItem::Token(token) => token.start(),
      MemberLikeExprItem::Computed(computed) => computed.range.start,
      MemberLikeExprItem::CallExpr(call_expr) => call_expr.callee.start(),
    }
  }

  fn end(&self) -> SourcePos {
    match self {
      MemberLikeExprItem::Node(node) => node.end(),
      MemberLikeExprItem::Token(token) => token.end(),
      MemberLikeExprItem::Computed(computed) => computed.range.end,
      MemberLikeExprItem::CallExpr(call_expr) => call_expr.original_call_expr.end(),
    }
  }
}

impl<'a> MemberLikeExprItem<'a> {
  pub fn is_computed(&self) -> bool {
    matches!(self, MemberLikeExprItem::Computed(_))
  }

  /// Gets if this is accessed with an optional chain (ex. `b` in `a?.b`).
  pub fn is_optional(&self, program: ProgramInfo<'a>) -> bool {
    match self {
      MemberLikeExprItem::Node(node) => match node.parent(program) {
        Some(Node::StaticMemberExpression(member)) => member.optional && member.property.span == oxc_span::GetSpan::span(node),
        Some(Node::PrivateFieldExpression(member)) => member.optional && member.field.span == oxc_span::GetSpan::span(node),
        _ => false,
      },
      MemberLikeExprItem::Token(_) => false,
      MemberLikeExprItem::Computed(computed) => computed.member.optional,
      MemberLikeExprItem::CallExpr(call_expr) => call_expr.original_call_expr.optional,
    }
  }
}

pub struct MemberLikeExprItemComputed<'a> {
  pub member: &'a ComputedMemberExpression<'a>,
  /// Range from the open bracket to the close bracket.
  pub range: SourceRange,
}

pub struct MemberLikeExprItemCallExpr<'a> {
  pub original_call_expr: &'a CallExpression<'a>,
  pub callee: MemberLikeExprItem<'a>,
}

/// Takes a member expression and flattens it out.
/// This is done to prevent a stack overflow when someone has many chained member expressions.
pub fn flatten_member_like_expr<'a>(node: Node<'a>, program: ProgramInfo<'a>, bump: &'a Bump) -> FlattenedMemberLikeExpr<'a> {
  let mut nodes = BumpVec::new_in(bump);
  push_descendant_nodes(node, &mut nodes, program);

  FlattenedMemberLikeExpr { nodes }
}

fn push_descendant_nodes<'a>(node: Node<'a>, nodes: &mut BumpVec<'a, MemberLikeExprItem<'a>>, program: ProgramInfo<'a>) {
  match node {
    Node::StaticMemberExpression(member_expr) => {
      push_descendant_nodes((&member_expr.object).into_node(), nodes, program);
      nodes.push(MemberLikeExprItem::Node((&member_expr.property).into_node()));
    }
    Node::PrivateFieldExpression(member_expr) => {
      push_descendant_nodes((&member_expr.object).into_node(), nodes, program);
      nodes.push(MemberLikeExprItem::Node((&member_expr.field).into_node()));
    }
    Node::ComputedMemberExpression(member_expr) => {
      push_descendant_nodes((&member_expr.object).into_node(), nodes, program);
      let open_bracket = member_expr
        .expression
        .previous_tokens_fast(program)
        .iter()
        .rev()
        .find(|t| t.kind == TokenKind::LBrack)
        .unwrap();
      let computed = nodes.bump().alloc(MemberLikeExprItemComputed {
        member: member_expr,
        range: SourceRange::new(open_bracket.start(), member_expr.end()),
      });
      nodes.push(MemberLikeExprItem::Computed(computed));
    }
    Node::ImportMeta(_) | Node::NewTarget(_) => {
      let tokens = node.tokens_fast(program);
      debug_assert_eq!(tokens.len(), 3);
      for token in tokens {
        if token.kind != TokenKind::Dot {
          nodes.push(MemberLikeExprItem::Token(token));
        }
      }
    }
    Node::ChainExpression(chain_expr) => {
      push_descendant_nodes((&chain_expr.expression).into_node(), nodes, program);
    }
    Node::CallExpression(call_expr) => {
      // leave test library call expressions as-is
      if node_helpers::is_test_library_call_expr(call_expr, program) {
        nodes.push(MemberLikeExprItem::Node(node));
      } else {
        push_descendant_nodes_for_call_expr(call_expr, nodes, program);
      }
    }
    node => {
      nodes.push(MemberLikeExprItem::Node(node));
    }
  }
}

fn push_descendant_nodes_for_call_expr<'a>(call_expr: &'a CallExpression<'a>, nodes: &mut BumpVec<'a, MemberLikeExprItem<'a>>, program: ProgramInfo<'a>) {
  push_descendant_nodes((&call_expr.callee).into_node(), nodes, program);
  let new_call_expr_callee = nodes.pop().unwrap();
  let call_expr = nodes.bump().alloc(MemberLikeExprItemCallExpr {
    original_call_expr: call_expr,
    callee: new_call_expr_callee,
  });
  nodes.push(MemberLikeExprItem::CallExpr(call_expr));
}
