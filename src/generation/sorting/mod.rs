pub(crate) mod module_specifiers;
use module_specifiers::*;

use deno_ast::view::*;
use deno_ast::SourceRange;
use deno_ast::SourceRanged;
use std::cmp::Ordering;

use crate::configuration::*;
use crate::utils;

// very rough... this should be improved to not allocate so much
// and be cleaner

#[derive(Clone, Copy)]
pub struct NodeSorter {
  order: SortOrder,
  named_type_imports_exports_order: NamedTypeImportsExportsOrder,
}

pub fn get_node_sorter_from_order(order: SortOrder, named_type_imports_exports_order: NamedTypeImportsExportsOrder) -> Option<NodeSorter> {
  match order {
    SortOrder::Maintain => None,
    SortOrder::CaseInsensitive | SortOrder::CaseSensitive => Some(NodeSorter {
      order,
      named_type_imports_exports_order,
    }),
  }
}

impl NodeSorter {
  pub fn get_sorted_indexes<'a>(&self, nodes: impl Iterator<Item = Option<Node<'a>>>, program: Program<'a>) -> utils::VecMap<usize> {
    let nodes_with_indexes = nodes.enumerate().collect::<Vec<_>>();
    let mut old_to_new_index = utils::VecMap::with_capacity(nodes_with_indexes.len());

    if nodes_with_indexes.len() <= 1 {
      for (index, _) in nodes_with_indexes {
        old_to_new_index.insert(index, index);
      }
      return old_to_new_index;
    }

    let mut nodes_with_indexes = nodes_with_indexes
      .into_iter()
      .map(|(index, node)| SortItem {
        index,
        key: node.map(|node| self.get_node_sort_key(node, program)),
      })
      .collect::<Vec<_>>();
    nodes_with_indexes.sort_unstable_by(|a, b| self.cmp_sort_items(a, b));

    for (new_index, old_index) in nodes_with_indexes.into_iter().map(|item| item.index).enumerate() {
      old_to_new_index.insert(old_index, new_index);
    }

    old_to_new_index
  }

  fn cmp_sort_items(&self, a: &SortItem, b: &SortItem) -> Ordering {
    let result = match (&a.key, &b.key) {
      (Some(a), Some(b)) => self.cmp_node_sort_keys(a, b),
      (Some(_), None) => Ordering::Greater,
      (None, Some(_)) => Ordering::Less,
      (None, None) => Ordering::Equal,
    };
    if result == Ordering::Equal {
      a.index.cmp(&b.index)
    } else {
      result
    }
  }

  pub(crate) fn cmp_node_sort_keys(&self, a: &NodeSortKey, b: &NodeSortKey) -> Ordering {
    for (i, a) in a.parts.iter().enumerate() {
      let Some(b) = b.parts.get(i) else {
        return Ordering::Greater;
      };
      match (a, b) {
        (ComparisonPart::HasType, ComparisonPart::NoType) => match self.named_type_imports_exports_order {
          NamedTypeImportsExportsOrder::First => return Ordering::Less,
          NamedTypeImportsExportsOrder::Last => return Ordering::Greater,
          NamedTypeImportsExportsOrder::None => {}
        },
        (ComparisonPart::NoType, ComparisonPart::HasType) => match self.named_type_imports_exports_order {
          NamedTypeImportsExportsOrder::First => return Ordering::Greater,
          NamedTypeImportsExportsOrder::Last => return Ordering::Less,
          NamedTypeImportsExportsOrder::None => {}
        },
        (ComparisonPart::Text(a), ComparisonPart::Text(b)) => {
          let cmp_result = self.cmp_text(a, b);
          if cmp_result != Ordering::Equal {
            return cmp_result;
          }
        }
        (ComparisonPart::ModuleSpecifier(a), ComparisonPart::ModuleSpecifier(b)) => {
          let cmp_result = match self.order {
            SortOrder::Maintain => Ordering::Equal,
            SortOrder::CaseSensitive => cmp_module_specifier_infos(a, b, cmp_text_case_sensitive),
            SortOrder::CaseInsensitive => cmp_module_specifier_infos(a, b, cmp_text_case_insensitive),
          };
          if cmp_result != Ordering::Equal {
            return cmp_result;
          }
        }
        _ => {}
      }
    }

    if a.parts.len() < b.parts.len() {
      Ordering::Less
    } else {
      Ordering::Equal
    }
  }

  fn cmp_text(&self, a: &str, b: &str) -> Ordering {
    match self.order {
      SortOrder::Maintain => Ordering::Equal,
      SortOrder::CaseSensitive => cmp_text_case_sensitive(a, b),
      SortOrder::CaseInsensitive => cmp_text_case_insensitive(a, b),
    }
  }

  pub(crate) fn get_node_sort_key<'a>(&self, node: Node<'a>, program: Program<'a>) -> NodeSortKey<'a> {
    NodeSortKey {
      parts: get_comparison_parts(node, is_import_or_export_declaration(node), program),
    }
  }
}

struct SortItem<'a> {
  index: usize,
  key: Option<NodeSortKey<'a>>,
}

pub(crate) struct NodeSortKey<'a> {
  parts: Vec<ComparisonPart<'a>>,
}

enum ComparisonPart<'a> {
  HasType,
  NoType,
  Text(&'a str),
  ModuleSpecifier(ModuleSpecifierInfo<'a>),
}

fn get_comparison_parts<'a>(node: Node<'a>, use_module_specifier: bool, program: Program<'a>) -> Vec<ComparisonPart<'a>> {
  fn get_part(range: SourceRange, use_module_specifier: bool, program: Program<'_>) -> ComparisonPart<'_> {
    let text = range.text_fast(program);
    if use_module_specifier {
      ComparisonPart::ModuleSpecifier(get_module_specifier_info(text))
    } else {
      ComparisonPart::Text(text)
    }
  }

  match node {
    Node::ImportNamedSpecifier(node) => {
      let first_node = if node.is_type_only() {
        ComparisonPart::HasType
      } else {
        ComparisonPart::NoType
      };
      if let Some(imported) = &node.imported {
        vec![
          first_node,
          get_part(imported.range(), use_module_specifier, program),
          get_part(node.local.range(), use_module_specifier, program),
        ]
      } else {
        vec![first_node, get_part(node.local.range(), use_module_specifier, program)]
      }
    }
    Node::ExportNamedSpecifier(node) => {
      let first_node = if node.is_type_only() {
        ComparisonPart::HasType
      } else {
        ComparisonPart::NoType
      };
      if let Some(exported) = &node.exported {
        vec![
          first_node,
          get_part(node.orig.range(), use_module_specifier, program),
          get_part(exported.range(), use_module_specifier, program),
        ]
      } else {
        vec![first_node, get_part(node.orig.range(), use_module_specifier, program)]
      }
    }
    Node::ImportDecl(node) => {
      vec![get_part(node.src.range(), use_module_specifier, program)]
    }
    Node::NamedExport(node) => {
      if let Some(src) = &node.src {
        vec![get_part(src.range(), use_module_specifier, program)]
      } else if cfg!(debug_assertions) {
        unimplemented!("Should not call this for named exports with src.");
      } else {
        vec![get_part(node.range(), use_module_specifier, program)]
      }
    }
    Node::ExportAll(node) => {
      vec![get_part(node.src.range(), use_module_specifier, program)]
    }
    _ => {
      if cfg!(debug_assertions) {
        unimplemented!("Not implemented sort node.");
      } else {
        vec![get_part(node.range(), use_module_specifier, program)]
      }
    }
  }
}

fn cmp_text_case_sensitive(a: &str, b: &str) -> Ordering {
  a.cmp(b)
}

pub(crate) fn cmp_text_case_insensitive(a: &str, b: &str) -> Ordering {
  let case_insensitive_result = if a.is_ascii() && b.is_ascii() {
    cmp_ascii_case_insensitive(a.as_bytes(), b.as_bytes())
  } else {
    a.to_lowercase().cmp(&b.to_lowercase())
  };
  if case_insensitive_result == Ordering::Equal {
    cmp_text_case_sensitive(a, b)
  } else {
    case_insensitive_result
  }
}

fn cmp_ascii_case_insensitive(a: &[u8], b: &[u8]) -> Ordering {
  for (a, b) in a.iter().zip(b) {
    let result = a.to_ascii_lowercase().cmp(&b.to_ascii_lowercase());
    if result != Ordering::Equal {
      return result;
    }
  }
  a.len().cmp(&b.len())
}

fn is_import_or_export_declaration(node: Node) -> bool {
  matches!(node, Node::ImportDecl(_) | Node::NamedExport(_) | Node::ExportAll(_))
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn should_compare_ascii_case_insensitive_without_changing_tie_breaking() {
    assert_eq!(cmp_text_case_insensitive("a", "B"), Ordering::Less);
    assert_eq!(cmp_text_case_insensitive("B", "a"), Ordering::Greater);
    assert_eq!(cmp_text_case_insensitive("a", "A"), Ordering::Greater);
    assert_eq!(cmp_text_case_insensitive("A", "a"), Ordering::Less);
  }
}
