use swc_common::{Span, Spanned};
use std::cmp::Ordering;

use super::*;
use crate::configuration::*;

pub fn get_node_sorter_from_order<'a>(order: SortOrder) -> Option<Box<dyn Fn(&Option<Node<'a>>, &Option<Node<'a>>, &Context<'a>) -> Ordering>> {
    match order {
        SortOrder::Maintain => None,
        SortOrder::CaseInsensitive => Some(sort_by_text_case_insensitive()),
        SortOrder::CaseSensitive => Some(sort_by_text_case_sensitive()),
    }
}

fn sort_by_text_case_insensitive<'a>() -> Box<dyn Fn(&Option<Node<'a>>, &Option<Node<'a>>, &Context<'a>) -> Ordering> {
    Box::new(|a, b, context| {
        cmp_optional_nodes(a, b, context, cmp_nodes_case_insensitive)
    })
}

fn sort_by_text_case_sensitive<'a>() -> Box<dyn Fn(&Option<Node<'a>>, &Option<Node<'a>>, &Context<'a>) -> Ordering> {
    Box::new(|a, b, context| {
        cmp_optional_nodes(a, b, context, cmp_nodes_case_sensitive)
    })
}

fn cmp_optional_nodes<'a>(
    a: &Option<Node<'a>>,
    b: &Option<Node<'a>>,
    context: &Context<'a>,
    cmp_func: impl Fn(&dyn Ranged, &dyn Ranged, &Context<'a>) -> Ordering,
) -> Ordering {
    if let Some(a) = a.as_ref() {
        if let Some(b) = b.as_ref() {
            cmp_nodes(&a, &b, context, cmp_func)
        } else {
            Ordering::Greater
        }
    } else {
        if b.is_none() {
            Ordering::Equal
        } else {
            Ordering::Less
        }
    }
}

fn cmp_nodes<'a>(
    a: &Node<'a>,
    b: &Node<'a>,
    context: &Context<'a>,
    cmp_func: impl Fn(&dyn Ranged, &dyn Ranged, &Context<'a>) -> Ordering,
) -> Ordering {
    let a_nodes = get_comparison_nodes(a);
    let b_nodes = get_comparison_nodes(b);

    for (i, a) in a_nodes.iter().enumerate() {
        if let Some(b) = b_nodes.get(i) {
            let cmp_result = cmp_func(a, b, context);
            if cmp_result != Ordering::Equal {
                return cmp_result;
            }
        } else {
            return Ordering::Greater;
        }
    }

    if a_nodes.len() < b_nodes.len() {
        Ordering::Less
    } else {
        Ordering::Equal
    }
}

fn get_comparison_nodes<'a>(node: &Node<'a>) -> Vec<Span> {
    match node {
        Node::ImportNamedSpecifier(node) => {
            if let Some(imported) = &node.imported {
                vec![imported.span(), node.local.span()]
            } else {
                vec![node.local.span()]
            }
        },
        Node::ExportNamedSpecifier(node) => {
            if let Some(exported) = &node.exported {
                vec![node.orig.span(), exported.span()]
            } else {
                vec![node.orig.span()]
            }
        },
        _ => {
            #[cfg(debug_assertions)]
            unimplemented!("Not implemented sort node.");
            #[cfg(not(debug_assertions))]
            vec![node.span_data()]
        }
    }
}

fn cmp_nodes_case_sensitive<'a>(a: &dyn Ranged, b: &dyn Ranged, context: &Context) -> Ordering {
    a.text(context).cmp(&b.text(context))
}

fn cmp_nodes_case_insensitive<'a>(a: &dyn Ranged, b: &dyn Ranged, context: &Context) -> Ordering {
    let case_insensitive_result = a.text(context).to_lowercase().cmp(&b.text(context).to_lowercase());
    if case_insensitive_result == Ordering::Equal {
        cmp_nodes_case_sensitive(a, b, context)
    } else {
        case_insensitive_result
    }
}
