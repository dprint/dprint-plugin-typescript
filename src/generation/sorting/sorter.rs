use deno_ast::view::*;
use deno_ast::SourceRanged;
use std::cmp::Ordering;

use super::cmp_module_specifiers;
use super::cmp_optional_nodes;
use super::is_import_or_export_declaration;
use super::NamedTypeImportsExportsOrder;

pub struct NodeSorter {
  mode: NodeSorterMode,
  order: NamedTypeImportsExportsOrder,
}

pub enum NodeSorterMode {
  CaseInsensitive,
  CaseSensitive,
}

impl NodeSorter {
  pub fn new(mode: NodeSorterMode, order: NamedTypeImportsExportsOrder) -> Self {
    Self { mode, order }
  }

  pub fn compare<'a>(&self, (a_index, a): (usize, Option<Node<'a>>), (b_index, b): (usize, Option<Node<'a>>), program: Program<'a>) -> Ordering {
    let result = if is_import_or_export_declaration(&a) {
      cmp_optional_nodes(a, b, program, self.order, |a, b, module| {
        cmp_module_specifiers(a.text_fast(module), b.text_fast(module), self)
      })
    } else {
      cmp_optional_nodes(a, b, program, self.order, |a, b, module| {
        self.cmp_text(a.text_fast(module), b.text_fast(module))
      })
    };

    if result == Ordering::Equal {
      a_index.cmp(&b_index)
    } else {
      result
    }
  }
}

pub trait CompareText {
  fn cmp_text(self, a: &str, b: &str) -> Ordering;
}

impl CompareText for &NodeSorter {
  fn cmp_text(self, a: &str, b: &str) -> Ordering {
    match self.mode {
      NodeSorterMode::CaseInsensitive => a.to_lowercase().cmp(&b.to_lowercase()).then_with(|| a.cmp(b)),
      NodeSorterMode::CaseSensitive => a.cmp(b),
    }
  }
}

impl<T> CompareText for T
where
  T: Fn(&str, &str) -> Ordering,
{
  fn cmp_text(self, a: &str, b: &str) -> Ordering {
    self(a, b)
  }
}
