//! Pure classification of an import declaration into one of the resolved groups.

use crate::configuration::{BuiltinCategory, BuiltinsRuntime, TypeImportsMode};
use crate::generation::imports::resolved::ResolvedGroups;
use crate::utils::builtins::is_builtin;

/// Classify a single import: return the index in `resolved.groups`.
pub fn classify(
  src: &str,
  is_type_only: bool,
  type_imports_mode: TypeImportsMode,
  builtins_runtime: BuiltinsRuntime,
  resolved: &ResolvedGroups,
) -> usize {
  let category = base_category(src, is_type_only, type_imports_mode, builtins_runtime);
  for (i, g) in resolved.groups.iter().enumerate() {
    if g.categories.contains(&category) {
      return i;
    }
    if g.has_globs && g.globs.is_match(src) {
      return i;
    }
  }
  resolved.unknown_index
}

fn base_category(
  src: &str,
  is_type_only: bool,
  type_imports_mode: TypeImportsMode,
  builtins_runtime: BuiltinsRuntime,
) -> BuiltinCategory {
  if is_type_only && matches!(type_imports_mode, TypeImportsMode::Separate) {
    return BuiltinCategory::Type;
  }
  if is_builtin(src, builtins_runtime) {
    return BuiltinCategory::Builtin;
  }
  if src.starts_with("../") || src == ".." {
    return BuiltinCategory::Parent;
  }
  if is_index_path(src) {
    return BuiltinCategory::Index;
  }
  if src.starts_with("./") {
    return BuiltinCategory::Sibling;
  }
  BuiltinCategory::External
}

fn is_index_path(src: &str) -> bool {
  if src == "." || src == "./" || src == "./index" {
    return true;
  }
  for ext in [".ts", ".tsx", ".js", ".jsx", ".mjs", ".cjs", ".mts", ".cts"] {
    if src == format!("./index{ext}") {
      return true;
    }
  }
  false
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::configuration::*;
  use crate::generation::imports::resolved::compile;
  use dprint_core::configuration::ConfigKeyMap;

  fn classify_with(json: serde_json::Value, src: &str, is_type: bool) -> usize {
    let map: ConfigKeyMap = serde_json::from_value(json).unwrap();
    let cfg = resolve_config(map, &Default::default()).config;
    let mut diags = Vec::new();
    let r = compile(&cfg, &mut diags).unwrap();
    classify(src, is_type, cfg.module_type_imports, cfg.module_builtins_runtime, &r)
  }

  fn eslint_mirror() -> serde_json::Value {
    serde_json::json!({
      "module.importGroups": [
        { "match": "builtin" },
        { "match": "external" },
        { "match": "parent" },
        { "match": ["sibling", "index"] }
      ]
    })
  }

  #[test]
  fn builtins_first() {
    assert_eq!(classify_with(eslint_mirror(), "fs", false), 0);
    assert_eq!(classify_with(eslint_mirror(), "node:path", false), 0);
  }

  #[test]
  fn external_second() {
    assert_eq!(classify_with(eslint_mirror(), "react", false), 1);
    assert_eq!(classify_with(eslint_mirror(), "@scope/pkg", false), 1);
  }

  #[test]
  fn parent_third() {
    assert_eq!(classify_with(eslint_mirror(), "../a", false), 2);
    assert_eq!(classify_with(eslint_mirror(), "../../b", false), 2);
  }

  #[test]
  fn sibling_and_index_share_fourth() {
    assert_eq!(classify_with(eslint_mirror(), "./a", false), 3);
    assert_eq!(classify_with(eslint_mirror(), "./index", false), 3);
    assert_eq!(classify_with(eslint_mirror(), ".", false), 3);
    assert_eq!(classify_with(eslint_mirror(), "./index.ts", false), 3);
  }

  #[test]
  fn unmatched_goes_to_implicit_unknown() {
    let cfg = serde_json::json!({
      "module.importGroups": [{ "match": "builtin" }]
    });
    assert_eq!(classify_with(cfg, "react", false), 1);
  }

  #[test]
  fn type_separate_routes_to_type_group() {
    let cfg = serde_json::json!({
      "module.importGroups": [
        { "match": "external" },
        { "match": "type" }
      ]
    });
    assert_eq!(classify_with(cfg.clone(), "react", false), 0);
    assert_eq!(classify_with(cfg, "react", true), 1);
  }

  #[test]
  fn type_interleave_classifies_by_path() {
    let cfg = serde_json::json!({
      "module.importGroups": [{ "match": "external" }],
      "module.typeImports": "interleave"
    });
    assert_eq!(classify_with(cfg, "react", true), 0);
  }

  #[test]
  fn pattern_glob_first_match_wins() {
    let cfg = serde_json::json!({
      "module.importGroups": [
        { "match": "external" },
        { "match": { "pattern": "@app/**" } }
      ]
    });
    assert_eq!(classify_with(cfg, "@app/foo", false), 0);
  }

  #[test]
  fn pattern_glob_before_external() {
    let cfg = serde_json::json!({
      "module.importGroups": [
        { "match": { "pattern": "@app/**" } },
        { "match": "external" }
      ]
    });
    assert_eq!(classify_with(cfg.clone(), "@app/foo", false), 0);
    assert_eq!(classify_with(cfg, "react", false), 1);
  }
}
