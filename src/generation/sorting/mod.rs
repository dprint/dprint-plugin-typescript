mod module_specifiers;
use module_specifiers::*;

mod sorter;
use sorter::*;

use deno_ast::view::*;
use deno_ast::SourceRange;
use deno_ast::SourceRanged;
use std::cmp::Ordering;

use crate::configuration::*;

pub use sorter::NodeSorter;

// very rough... this should be improved to not allocate so much
// and be cleaner

pub fn get_node_sorter_from_order(order: SortOrder, named_type_imports_exports_order: NamedTypeImportsExportsOrder) -> Option<NodeSorter> {
  match order {
    SortOrder::Maintain => None,
    SortOrder::CaseSensitive => Some(NodeSorter::new(NodeSorterMode::CaseSensitive, named_type_imports_exports_order)),
    SortOrder::CaseInsensitive => Some(NodeSorter::new(NodeSorterMode::CaseInsensitive, named_type_imports_exports_order)),
  }
}

fn cmp_optional_nodes<'a>(
  a: Option<Node<'a>>,
  b: Option<Node<'a>>,
  program: Program<'a>,
  named_type_imports_exports_order: NamedTypeImportsExportsOrder,
  cmp_func: impl Fn(&SourceRange, &SourceRange, Program<'a>) -> Ordering,
) -> Ordering {
  if let Some(a) = a {
    if let Some(b) = b {
      cmp_nodes(a, b, program, named_type_imports_exports_order, cmp_func)
    } else {
      Ordering::Greater
    }
  } else if b.is_none() {
    Ordering::Equal
  } else {
    Ordering::Less
  }
}

fn cmp_nodes<'a>(
  a: Node<'a>,
  b: Node<'a>,
  program: Program<'a>,
  named_type_imports_exports_order: NamedTypeImportsExportsOrder,
  cmp_func: impl Fn(&SourceRange, &SourceRange, Program<'a>) -> Ordering,
) -> Ordering {
  let a_nodes = get_comparison_nodes(a);
  let b_nodes = get_comparison_nodes(b);

  for (i, a) in a_nodes.iter().enumerate() {
    if let Some(b) = b_nodes.get(i) {
      match (a, b) {
        (ComparisonNode::HasType, ComparisonNode::NoType) => match named_type_imports_exports_order {
          NamedTypeImportsExportsOrder::First => return Ordering::Less,
          NamedTypeImportsExportsOrder::Last => return Ordering::Greater,
          NamedTypeImportsExportsOrder::None => {}
        },
        (ComparisonNode::NoType, ComparisonNode::HasType) => match named_type_imports_exports_order {
          NamedTypeImportsExportsOrder::First => return Ordering::Greater,
          NamedTypeImportsExportsOrder::Last => return Ordering::Less,
          NamedTypeImportsExportsOrder::None => {}
        },
        (ComparisonNode::Node(a), ComparisonNode::Node(b)) => {
          let cmp_result = cmp_func(a, b, program);
          if cmp_result != Ordering::Equal {
            return cmp_result;
          }
        }
        _ => {}
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

enum ComparisonNode {
  HasType,
  NoType,
  Node(SourceRange),
}

fn get_comparison_nodes(node: Node) -> Vec<ComparisonNode> {
  match node {
    Node::ImportNamedSpecifier(node) => {
      let first_node = if node.is_type_only() {
        ComparisonNode::HasType
      } else {
        ComparisonNode::NoType
      };
      if let Some(imported) = &node.imported {
        vec![first_node, ComparisonNode::Node(imported.range()), ComparisonNode::Node(node.local.range())]
      } else {
        vec![first_node, ComparisonNode::Node(node.local.range())]
      }
    }
    Node::ExportNamedSpecifier(node) => {
      let first_node = if node.is_type_only() {
        ComparisonNode::HasType
      } else {
        ComparisonNode::NoType
      };
      if let Some(exported) = &node.exported {
        vec![first_node, ComparisonNode::Node(node.orig.range()), ComparisonNode::Node(exported.range())]
      } else {
        vec![first_node, ComparisonNode::Node(node.orig.range())]
      }
    }
    Node::ImportDecl(node) => {
      vec![ComparisonNode::Node(node.src.range())]
    }
    Node::NamedExport(node) => {
      if let Some(src) = &node.src {
        vec![ComparisonNode::Node(src.range())]
      } else if cfg!(debug_assertions) {
        unimplemented!("Should not call this for named exports with src.");
      } else {
        vec![ComparisonNode::Node(node.range())]
      }
    }
    Node::ExportAll(node) => {
      vec![ComparisonNode::Node(node.src.range())]
    }
    _ => {
      if cfg!(debug_assertions) {
        unimplemented!("Not implemented sort node.");
      } else {
        vec![ComparisonNode::Node(node.range())]
      }
    }
  }
}

fn is_import_or_export_declaration(node: &Option<Node>) -> bool {
  matches!(node, Some(Node::ImportDecl(_) | Node::NamedExport(_) | Node::ExportAll(_)))
}
