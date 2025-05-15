use deno_ast::swc::parser::token::Token;
use deno_ast::swc::parser::token::TokenAndSpan;
use deno_ast::view::*;
use deno_ast::SourcePos;
use deno_ast::SourceRanged;
use deno_ast::SourceRangedForSpanned;

use crate::generation::generate_types::CallOrOptCallExpr;

use super::super::node_helpers;

pub struct FlattenedMemberLikeExpr<'a> {
  pub nodes: Vec<MemberLikeExprItem<'a>>,
}

pub enum MemberLikeExprItem<'a> {
  Node(Node<'a>),
  Token(&'a TokenAndSpan),
  CallExpr(Box<MemberLikeExprItemCallExpr<'a>>),
}

impl<'a> SourceRanged for MemberLikeExprItem<'a> {
  fn start(&self) -> SourcePos {
    match self {
      MemberLikeExprItem::Node(node) => node.start(),
      MemberLikeExprItem::Token(token) => token.start(),
      MemberLikeExprItem::CallExpr(call_expr) => call_expr.callee.start(),
    }
  }

  fn end(&self) -> SourcePos {
    match self {
      MemberLikeExprItem::Node(node) => node.end(),
      MemberLikeExprItem::Token(token) => token.end(),
      MemberLikeExprItem::CallExpr(call_expr) => call_expr.original_call_expr.end(),
    }
  }
}

impl<'a> MemberLikeExprItem<'a> {
  pub fn is_computed(&self) -> bool {
    matches!(self, MemberLikeExprItem::Node(Node::ComputedPropName(_)))
  }

  pub fn is_optional(&self) -> bool {
    let Some(top_node) = self.get_top_node() else {
      return false;
    };
    let Node::OptChainExpr(expr) = top_node.parent().unwrap().parent().unwrap() else {
      return false;
    };
    expr.optional()
  }

  fn get_top_node(&self) -> Option<Node<'a>> {
    match self {
      MemberLikeExprItem::Node(node) => Some(*node),
      MemberLikeExprItem::Token(_) => None,
      MemberLikeExprItem::CallExpr(call_expr) => Some(call_expr.original_call_expr.into()),
    }
  }
}

pub struct MemberLikeExprItemCallExpr<'a> {
  pub original_call_expr: CallOrOptCallExpr<'a>,
  pub callee: MemberLikeExprItem<'a>,
}

/// Takes a member expression and flattens it out.
/// This is done to prevent a stack overflow when someone has many chained member expressions.
pub fn flatten_member_like_expr<'a>(node: Node<'a>, program: Program<'a>) -> FlattenedMemberLikeExpr<'a> {
  let mut nodes = Vec::new();
  push_descendant_nodes(node, &mut nodes, program);

  FlattenedMemberLikeExpr { nodes }
}

fn push_descendant_nodes<'a>(node: Node<'a>, nodes: &mut Vec<MemberLikeExprItem<'a>>, program: Program<'a>) {
  match node {
    Node::MemberExpr(member_expr) => {
      push_descendant_nodes(member_expr.obj.into(), nodes, program);
      if let MemberProp::Computed(computed) = member_expr.prop {
        nodes.push(MemberLikeExprItem::Node(computed.into()));
      } else {
        push_descendant_nodes(member_expr.prop.into(), nodes, program);
      }
    }
    Node::SuperPropExpr(super_expr) => {
      push_descendant_nodes(super_expr.obj.into(), nodes, program);
      if let SuperProp::Computed(computed) = super_expr.prop {
        nodes.push(MemberLikeExprItem::Node(computed.into()));
      } else {
        push_descendant_nodes(super_expr.prop.into(), nodes, program);
      }
    }
    Node::MetaPropExpr(meta_prop_expr) => {
      let tokens = meta_prop_expr.tokens_fast(program);
      debug_assert_eq!(tokens.len(), 3);
      for token in tokens {
        match &token.token {
          Token::Word(_) => {
            nodes.push(MemberLikeExprItem::Token(token));
          }
          Token::Dot => {}
          _ => {
            if cfg!(debug_assertions) {
              panic!("Unexpected token {}.", node.kind());
            }
          }
        }
      }
    }
    Node::OptChainExpr(opt_chain_expr) => {
      push_descendant_nodes(opt_chain_expr.base.into(), nodes, program);
    }
    Node::OptCall(call_expr) => {
      push_descendant_nodes_for_call_expr(call_expr.into(), nodes, program);
    }
    Node::CallExpr(call_expr) => {
      // leave test library call expressions as-is
      if node_helpers::is_test_library_call_expr(call_expr, program) {
        nodes.push(MemberLikeExprItem::Node(call_expr.into()));
      } else {
        push_descendant_nodes_for_call_expr(call_expr.into(), nodes, program);
      }
    }
    node => {
      nodes.push(MemberLikeExprItem::Node(node));
    }
  }
}

fn push_descendant_nodes_for_call_expr<'a>(call_expr: CallOrOptCallExpr<'a>, nodes: &mut Vec<MemberLikeExprItem<'a>>, program: Program<'a>) {
  push_descendant_nodes(call_expr.callee().into(), nodes, program);
  let new_call_expr_callee = nodes.pop().unwrap();
  nodes.push(MemberLikeExprItem::CallExpr(Box::new(MemberLikeExprItemCallExpr {
    original_call_expr: call_expr,
    callee: new_call_expr_callee,
  })));
}
