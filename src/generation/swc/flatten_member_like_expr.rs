use deno_ast::oxc::ast::ast::ChainElement;
use deno_ast::oxc::span::GetSpan;
use deno_ast::oxc::span::Span;

use crate::generation::generate_types::CallOrOptCallExpr;
use crate::generation::oxc_helpers::Node;
use crate::generation::oxc_helpers::ProgramInfo;
use crate::generation::oxc_helpers::SourceRanged;
use crate::generation::oxc_helpers::TokenExt;
use crate::generation::to_node::expr_to_node;

use super::super::node_helpers;

pub struct FlattenedMemberLikeExpr<'a> {
  pub nodes: Vec<MemberLikeExprItem<'a>>,
}

pub enum MemberLikeExprItem<'a> {
  Node {
    node: Node<'a>,
    is_optional: bool,
  },
  /// A computed member access expression (`[expr]`).
  Computed {
    node: Node<'a>,
    range: Span,
    is_optional: bool,
  },
  CallExpr(Box<MemberLikeExprItemCallExpr<'a>>),
}

impl<'a> GetSpan for MemberLikeExprItem<'a> {
  fn span(&self) -> Span {
    match self {
      MemberLikeExprItem::Node { node, .. } => node.span(),
      MemberLikeExprItem::Computed { range, .. } => *range,
      MemberLikeExprItem::CallExpr(call_expr) => Span::new(call_expr.callee.span().start, call_expr.original_call_expr.end()),
    }
  }
}

impl<'a> MemberLikeExprItem<'a> {
  pub fn is_computed(&self) -> bool {
    matches!(self, MemberLikeExprItem::Computed { .. })
  }

  pub fn is_optional(&self) -> bool {
    match self {
      MemberLikeExprItem::Node { is_optional, .. } | MemberLikeExprItem::Computed { is_optional, .. } => *is_optional,
      MemberLikeExprItem::CallExpr(call_expr) => call_expr.original_call_expr.is_optional(),
    }
  }
}

pub struct MemberLikeExprItemCallExpr<'a> {
  pub original_call_expr: CallOrOptCallExpr<'a>,
  pub callee: MemberLikeExprItem<'a>,
}

/// Takes a member expression and flattens it out.
/// This is done to prevent a stack overflow when someone has many chained member expressions.
pub fn flatten_member_like_expr<'a>(node: Node<'a>, program: ProgramInfo<'a>) -> FlattenedMemberLikeExpr<'a> {
  let mut nodes = Vec::new();
  push_descendant_nodes(node, &mut nodes, program);

  FlattenedMemberLikeExpr { nodes }
}

fn push_descendant_nodes<'a>(node: Node<'a>, nodes: &mut Vec<MemberLikeExprItem<'a>>, program: ProgramInfo<'a>) {
  match node {
    Node::StaticMemberExpression(member_expr) => {
      push_descendant_nodes(expr_to_node(&member_expr.object), nodes, program);
      nodes.push(MemberLikeExprItem::Node {
        node: Node::IdentifierName(&member_expr.property),
        is_optional: member_expr.optional,
      });
    }
    Node::ComputedMemberExpression(member_expr) => {
      push_descendant_nodes(expr_to_node(&member_expr.object), nodes, program);
      let range_start = member_expr
        .expression
        .previous_token_fast(program)
        .map(|open_bracket| {
          if member_expr.optional {
            open_bracket
              .previous_token_fast(program)
              .filter(|token| token.text_fast(program) == "?.")
              .unwrap_or(open_bracket)
              .start()
          } else {
            open_bracket.start()
          }
        })
        .unwrap_or(member_expr.expression.start());
      nodes.push(MemberLikeExprItem::Computed {
        node: expr_to_node(&member_expr.expression),
        range: Span::new(range_start, member_expr.end()),
        is_optional: member_expr.optional,
      });
    }
    Node::PrivateFieldExpression(member_expr) => {
      push_descendant_nodes(expr_to_node(&member_expr.object), nodes, program);
      nodes.push(MemberLikeExprItem::Node {
        node: Node::PrivateIdentifier(&member_expr.field),
        is_optional: member_expr.optional,
      });
    }
    Node::MetaProperty(meta_prop_expr) => {
      nodes.push(MemberLikeExprItem::Node {
        node: Node::IdentifierName(&meta_prop_expr.meta),
        is_optional: false,
      });
      nodes.push(MemberLikeExprItem::Node {
        node: Node::IdentifierName(&meta_prop_expr.property),
        is_optional: false,
      });
    }
    Node::ChainExpression(chain_expr) => {
      push_descendant_nodes(chain_element_to_node(&chain_expr.expression), nodes, program);
    }
    Node::CallExpression(call_expr) => {
      // leave test library call expressions as-is
      if node_helpers::is_test_library_call_expr(call_expr, program) {
        nodes.push(MemberLikeExprItem::Node { node, is_optional: false });
      } else {
        push_descendant_nodes_for_call_expr(CallOrOptCallExpr(call_expr), nodes, program);
      }
    }
    node => {
      nodes.push(MemberLikeExprItem::Node { node, is_optional: false });
    }
  }
}

fn push_descendant_nodes_for_call_expr<'a>(call_expr: CallOrOptCallExpr<'a>, nodes: &mut Vec<MemberLikeExprItem<'a>>, program: ProgramInfo<'a>) {
  push_descendant_nodes(expr_to_node(call_expr.callee()), nodes, program);
  let new_call_expr_callee = nodes.pop().unwrap();
  nodes.push(MemberLikeExprItem::CallExpr(Box::new(MemberLikeExprItemCallExpr {
    original_call_expr: call_expr,
    callee: new_call_expr_callee,
  })));
}

pub fn chain_element_to_node<'a>(element: &'a ChainElement<'a>) -> Node<'a> {
  match element {
    ChainElement::CallExpression(node) => Node::CallExpression(node),
    ChainElement::TSNonNullExpression(node) => Node::TSNonNullExpression(node),
    ChainElement::ComputedMemberExpression(node) => Node::ComputedMemberExpression(node),
    ChainElement::StaticMemberExpression(node) => Node::StaticMemberExpression(node),
    ChainElement::PrivateFieldExpression(node) => Node::PrivateFieldExpression(node),
  }
}
