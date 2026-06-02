use deno_ast::oxc::ast::ast::ChainElement;
use deno_ast::oxc::parser::Token;
use deno_ast::oxc::span::GetSpan;
use deno_ast::oxc::span::Span;

use crate::generation::generate_types::CallOrOptCallExpr;
use crate::generation::oxc_helpers::Node;
use crate::generation::oxc_helpers::ProgramInfo;
use crate::generation::oxc_helpers::SourceRanged;
use crate::generation::to_node::expr_to_node;

use super::super::node_helpers;

pub struct FlattenedMemberLikeExpr<'a> {
  pub nodes: Vec<MemberLikeExprItem<'a>>,
}

pub enum MemberLikeExprItem<'a> {
  Node(Node<'a>),
  /// A computed member access expression (`[expr]`).
  Computed(Node<'a>),
  Token(&'a Token),
  CallExpr(Box<MemberLikeExprItemCallExpr<'a>>),
}

impl<'a> GetSpan for MemberLikeExprItem<'a> {
  fn span(&self) -> Span {
    match self {
      MemberLikeExprItem::Node(node) | MemberLikeExprItem::Computed(node) => node.span(),
      MemberLikeExprItem::Token(token) => Span::new(token.start(), token.end()),
      MemberLikeExprItem::CallExpr(call_expr) => Span::new(call_expr.callee.span().start, call_expr.original_call_expr.end()),
    }
  }
}

impl<'a> MemberLikeExprItem<'a> {
  pub fn is_computed(&self) -> bool {
    matches!(self, MemberLikeExprItem::Computed(_))
  }

  pub fn is_optional(&self) -> bool {
    // oxc records optional chaining as a `.optional` flag on the member/call
    // node itself (rather than a wrapping `OptChainExpr` as SWC did).
    match self.get_top_node() {
      Some(Node::StaticMemberExpression(node)) => node.optional,
      Some(Node::ComputedMemberExpression(node)) => node.optional,
      Some(Node::PrivateFieldExpression(node)) => node.optional,
      Some(Node::CallExpression(node)) => node.optional,
      _ => false,
    }
  }

  fn get_top_node(&self) -> Option<Node<'a>> {
    match self {
      MemberLikeExprItem::Node(node) | MemberLikeExprItem::Computed(node) => Some(*node),
      MemberLikeExprItem::Token(_) => None,
      MemberLikeExprItem::CallExpr(call_expr) => Some(Node::CallExpression(call_expr.original_call_expr.inner())),
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
      nodes.push(MemberLikeExprItem::Node(Node::IdentifierName(&member_expr.property)));
    }
    Node::ComputedMemberExpression(member_expr) => {
      push_descendant_nodes(expr_to_node(&member_expr.object), nodes, program);
      nodes.push(MemberLikeExprItem::Computed(expr_to_node(&member_expr.expression)));
    }
    Node::PrivateFieldExpression(member_expr) => {
      push_descendant_nodes(expr_to_node(&member_expr.object), nodes, program);
      nodes.push(MemberLikeExprItem::Node(Node::PrivateIdentifier(&member_expr.field)));
    }
    Node::MetaProperty(meta_prop_expr) => {
      nodes.push(MemberLikeExprItem::Node(Node::IdentifierName(&meta_prop_expr.meta)));
      nodes.push(MemberLikeExprItem::Node(Node::IdentifierName(&meta_prop_expr.property)));
    }
    Node::ChainExpression(chain_expr) => {
      push_descendant_nodes(chain_element_to_node(&chain_expr.expression), nodes, program);
    }
    Node::CallExpression(call_expr) => {
      // leave test library call expressions as-is
      if node_helpers::is_test_library_call_expr(call_expr, program) {
        nodes.push(MemberLikeExprItem::Node(node));
      } else {
        push_descendant_nodes_for_call_expr(CallOrOptCallExpr(call_expr), nodes, program);
      }
    }
    node => {
      nodes.push(MemberLikeExprItem::Node(node));
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

fn chain_element_to_node<'a>(element: &'a ChainElement<'a>) -> Node<'a> {
  match element {
    ChainElement::CallExpression(node) => Node::CallExpression(node),
    ChainElement::TSNonNullExpression(node) => Node::TSNonNullExpression(node),
    ChainElement::ComputedMemberExpression(node) => Node::ComputedMemberExpression(node),
    ChainElement::StaticMemberExpression(node) => Node::StaticMemberExpression(node),
    ChainElement::PrivateFieldExpression(node) => Node::PrivateFieldExpression(node),
  }
}
