use swc_ast_view::*;

pub struct FlattenedMemberLikeExpr<'a> {
    pub node: Node<'a>,

    pub nodes: Vec<MemberLikeExprItem<'a>>,
}

pub struct MemberLikeExprItem<'a> {
    pub is_computed: bool,
    pub node: Node<'a>,
}

/// Takes a member expression and flattens it out.
/// This is done to prevent a stack overflow when someone has many chained member expressions.
pub fn flatten_member_expr<'a>(member_expr: &MemberExpr<'a>) -> FlattenedMemberLikeExpr<'a> {
    let mut nodes = Vec::new();
    push_descendant_nodes(member_expr.into(), &mut nodes);

    FlattenedMemberLikeExpr {
        node: member_expr.into(),
        nodes,
    }
}

pub fn flatten_meta_prop_expr<'a>(meta_prop_expr: &MetaPropExpr<'a>) -> FlattenedMemberLikeExpr<'a> {
    let mut nodes = Vec::new();
    push_descendant_nodes(meta_prop_expr.into(), &mut nodes);

    FlattenedMemberLikeExpr {
        node: meta_prop_expr.into(),
        nodes,
    }
}

fn push_descendant_nodes<'a>(node: Node<'a>, nodes: &mut Vec<MemberLikeExprItem<'a>>) {
    match node {
        Node::MemberExpr(member_expr) => {
            push_descendant_nodes(member_expr.obj.into(), nodes);
            if member_expr.computed() {
                nodes.push(MemberLikeExprItem {
                    node: member_expr.prop.into(),
                    is_computed: true,
                });
            } else {
                push_descendant_nodes(member_expr.prop.into(), nodes);
            }
        }
        Node::MetaPropExpr(meta_prop_expr) => {
            push_descendant_nodes(meta_prop_expr.meta.into(), nodes);
            push_descendant_nodes(meta_prop_expr.prop.into(), nodes);
        }
        Node::OptChainExpr(opt_chain_expr) => {
            push_descendant_nodes(opt_chain_expr.expr.into(), nodes);
        }
        node => {
            nodes.push(MemberLikeExprItem {
                is_computed: false,
                node,
            });
        },
    }
}