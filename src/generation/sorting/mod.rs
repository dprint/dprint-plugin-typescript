mod module_specifiers;
use module_specifiers::*;

use deno_ast::oxc::ast::ast::ImportOrExportKind;
use std::cmp::Ordering;

use super::oxc_helpers::Node;
use super::oxc_helpers::ProgramInfo;
use super::oxc_helpers::SourceRange;
use super::oxc_helpers::SourceRanged;
use crate::configuration::*;

// very rough... this should be improved to not allocate so much
// and be cleaner

pub fn get_node_sorter_from_order<'a>(
  order: SortOrder,
  named_type_imports_exports_order: NamedTypeImportsExportsOrder,
) -> Option<Box<dyn Fn((usize, Option<Node<'a>>), (usize, Option<Node<'a>>), ProgramInfo<'a>) -> Ordering>> {
  // todo: how to reduce code duplication here?
  match order {
    SortOrder::Maintain => None,
    SortOrder::CaseInsensitive => Some(Box::new(move |(a_index, a), (b_index, b), program| {
      let result = if is_import_or_export_declaration(&a) {
        cmp_optional_nodes(a, b, program, named_type_imports_exports_order, |a, b, module| {
          cmp_module_specifiers(a.text_fast(module), b.text_fast(module), cmp_text_case_insensitive)
        })
      } else {
        cmp_optional_nodes(a, b, program, named_type_imports_exports_order, |a, b, module| {
          cmp_text_case_insensitive(a.text_fast(module), b.text_fast(module))
        })
      };
      if result == Ordering::Equal {
        a_index.cmp(&b_index)
      } else {
        result
      }
    })),
    SortOrder::CaseSensitive => Some(Box::new(move |(a_index, a), (b_index, b), program| {
      let result = if is_import_or_export_declaration(&a) {
        cmp_optional_nodes(a, b, program, named_type_imports_exports_order, |a, b, module| {
          cmp_module_specifiers(a.text_fast(module), b.text_fast(module), cmp_text_case_sensitive)
        })
      } else {
        cmp_optional_nodes(a, b, program, named_type_imports_exports_order, |a, b, module| {
          cmp_text_case_sensitive(a.text_fast(module), b.text_fast(module))
        })
      };
      if result == Ordering::Equal {
        a_index.cmp(&b_index)
      } else {
        result
      }
    })),
  }
}

fn cmp_optional_nodes<'a>(
  a: Option<Node<'a>>,
  b: Option<Node<'a>>,
  program: ProgramInfo<'a>,
  named_type_imports_exports_order: NamedTypeImportsExportsOrder,
  cmp_func: impl Fn(&SourceRange, &SourceRange, ProgramInfo<'a>) -> Ordering,
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
  program: ProgramInfo<'a>,
  named_type_imports_exports_order: NamedTypeImportsExportsOrder,
  cmp_func: impl Fn(&SourceRange, &SourceRange, ProgramInfo<'a>) -> Ordering,
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
    Node::ImportSpecifier(node) => {
      let first_node = if node.import_kind == ImportOrExportKind::Type {
        ComparisonNode::HasType
      } else {
        ComparisonNode::NoType
      };
      // oxc always populates `imported`; aliasing (`import { a as b }`) is
      // detected by the imported/local spans differing.
      if node.imported.start() != node.local.start() {
        vec![first_node, ComparisonNode::Node(node.imported.range()), ComparisonNode::Node(node.local.range())]
      } else {
        vec![first_node, ComparisonNode::Node(node.local.range())]
      }
    }
    Node::ExportSpecifier(node) => {
      let first_node = if node.export_kind == ImportOrExportKind::Type {
        ComparisonNode::HasType
      } else {
        ComparisonNode::NoType
      };
      // `local` is the original name; aliasing detected via differing spans.
      if node.local.start() != node.exported.start() {
        vec![first_node, ComparisonNode::Node(node.local.range()), ComparisonNode::Node(node.exported.range())]
      } else {
        vec![first_node, ComparisonNode::Node(node.local.range())]
      }
    }
    Node::ImportDeclaration(node) => {
      vec![ComparisonNode::Node(node.source.range())]
    }
    Node::ExportNamedDeclaration(node) => {
      if let Some(src) = &node.source {
        vec![ComparisonNode::Node(src.range())]
      } else if cfg!(debug_assertions) {
        unimplemented!("Should not call this for named exports with src.");
      } else {
        vec![ComparisonNode::Node(node.range())]
      }
    }
    Node::ExportAllDeclaration(node) => {
      vec![ComparisonNode::Node(node.source.range())]
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

fn cmp_text_case_sensitive(a: &str, b: &str) -> Ordering {
  a.cmp(b)
}

fn cmp_text_case_insensitive(a: &str, b: &str) -> Ordering {
  let case_insensitive_result = a.to_lowercase().cmp(&b.to_lowercase());
  if case_insensitive_result == Ordering::Equal {
    cmp_text_case_sensitive(a, b)
  } else {
    case_insensitive_result
  }
}

fn is_import_or_export_declaration(node: &Option<Node>) -> bool {
  matches!(
    node,
    Some(Node::ImportDeclaration(_) | Node::ExportNamedDeclaration(_) | Node::ExportAllDeclaration(_))
  )
}
