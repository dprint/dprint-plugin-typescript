use super::super::node_helpers;
use swc_ast_view::*;
use swc_common::Span;
use swc_common::Spanned;

pub struct FlattenedMemberLikeExpr<'a> {
  pub node: Node<'a>,

  pub nodes: Vec<MemberLikeExprItem<'a>>,
}

pub enum MemberLikeExprItem<'a> {
  Computed(MemberLikeExprItemComputed<'a>),
  Node(Node<'a>),
  CallExpr(Box<MemberLikeExprItemCallExpr<'a>>),
}

impl<'a> Spanned for MemberLikeExprItem<'a> {
  fn span(&self) -> Span {
    match self {
      MemberLikeExprItem::Computed(node) => node.span,
      MemberLikeExprItem::Node(node) => node.span(),
      MemberLikeExprItem::CallExpr(call_expr) => Span::new(call_expr.callee.span().lo(), call_expr.original_call_expr.hi(), Default::default()),
    }
  }
}

impl<'a> MemberLikeExprItem<'a> {
  pub fn is_computed(&self) -> bool {
    matches!(self, MemberLikeExprItem::Computed(_))
  }

  pub fn is_optional(&self) -> bool {
    self.get_top_node().parent().unwrap().parent().unwrap().kind() == NodeKind::OptChainExpr
  }

  fn get_top_node(&self) -> Node<'a> {
    match self {
      MemberLikeExprItem::Computed(node) => node.inner_node,
      MemberLikeExprItem::Node(node) => *node,
      MemberLikeExprItem::CallExpr(call_expr) => call_expr.original_call_expr.into(),
    }
  }
}

pub struct MemberLikeExprItemComputed<'a> {
  pub span: Span,
  pub inner_node: Node<'a>,
}

pub struct MemberLikeExprItemCallExpr<'a> {
  pub original_call_expr: &'a CallExpr<'a>,
  pub callee: MemberLikeExprItem<'a>,
}

/// Takes a member expression and flattens it out.
/// This is done to prevent a stack overflow when someone has many chained member expressions.
pub fn flatten_member_like_expr<'a>(node: Node<'a>, module: &Module<'a>) -> FlattenedMemberLikeExpr<'a> {
  let mut nodes = Vec::new();
  push_descendant_nodes(node, &mut nodes, module);

  FlattenedMemberLikeExpr { node, nodes }
}

fn push_descendant_nodes<'a>(node: Node<'a>, nodes: &mut Vec<MemberLikeExprItem<'a>>, module: &Module<'a>) {
  match node {
    Node::MemberExpr(member_expr) => {
      push_descendant_nodes(member_expr.obj.into(), nodes, module);
      if member_expr.computed() {
        // get the '[' and ']' tokens for the span
        let previous_token = member_expr.prop.previous_token_fast(module).unwrap();
        let next_token = member_expr.prop.next_token_fast(module).unwrap();
        nodes.push(MemberLikeExprItem::Computed(MemberLikeExprItemComputed {
          span: Span::new(previous_token.lo(), next_token.hi(), Default::default()),
          inner_node: member_expr.prop.into(),
        }));
      } else {
        push_descendant_nodes(member_expr.prop.into(), nodes, module);
      }
    }
    Node::MetaPropExpr(meta_prop_expr) => {
      push_descendant_nodes(meta_prop_expr.meta.into(), nodes, module);
      push_descendant_nodes(meta_prop_expr.prop.into(), nodes, module);
    }
    Node::OptChainExpr(opt_chain_expr) => {
      push_descendant_nodes(opt_chain_expr.expr.into(), nodes, module);
    }
    Node::CallExpr(call_expr) => {
      // leave test library call expressions as-is
      if node_helpers::is_test_library_call_expr(call_expr, module) {
        nodes.push(MemberLikeExprItem::Node(node));
      } else {
        push_descendant_nodes(call_expr.callee.into(), nodes, module);
        let new_call_expr_callee = nodes.pop().unwrap();
        nodes.push(MemberLikeExprItem::CallExpr(Box::new(MemberLikeExprItemCallExpr {
          original_call_expr: call_expr,
          callee: new_call_expr_callee,
        })));
      }
    }
    node => {
      nodes.push(MemberLikeExprItem::Node(node));
    }
  }
}
