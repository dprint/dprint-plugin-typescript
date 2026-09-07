//! Compiled form of `module.importGroups` ready for fast classification.

use globset::{Glob, GlobSet, GlobSetBuilder};
use rustc_hash::FxHashSet;

use crate::configuration::{BuiltinCategory, Configuration, ImportGroupMatch, ImportMatcher, TypeImportsMode};

/// One resolved group: a set of categories + a glob set, in user-listed order.
pub struct ResolvedGroup {
  pub categories: Vec<BuiltinCategory>,
  pub globs: GlobSet,
  pub has_globs: bool,
}

/// Result of compiling `module.importGroups` into matcher-friendly form.
pub struct ResolvedGroups {
  pub groups: Vec<ResolvedGroup>,
  /// Index into `groups` that catches imports matching no listed category.
  pub unknown_index: usize,
}

/// Compile config's `module.importGroups` into resolved form, along with any
/// diagnostics about duplicate categories or invalid globs. The groups are
/// `None` when the feature is disabled (empty list).
pub fn compile_import_groups(config: &Configuration) -> (Option<ResolvedGroups>, Vec<String>) {
  let mut diagnostics = Vec::new();
  if config.module_import_groups.is_empty() {
    return (None, diagnostics);
  }

  let interleave_mode = matches!(config.module_type_imports, TypeImportsMode::Interleave);

  let mut groups: Vec<ResolvedGroup> = Vec::new();
  let mut explicit_unknown: Option<usize> = None;
  let mut seen_categories: FxHashSet<BuiltinCategory> = Default::default();

  for (i, group) in config.module_import_groups.iter().enumerate() {
    let matchers = match &group.matchers {
      ImportGroupMatch::Single(m) => std::slice::from_ref(m),
      ImportGroupMatch::Multiple(v) => v.as_slice(),
    };

    let mut categories = Vec::new();
    let mut builder = GlobSetBuilder::new();
    let mut has_globs = false;

    for m in matchers {
      match m {
        ImportMatcher::Category(c) => {
          if *c == BuiltinCategory::Type && interleave_mode {
            diagnostics.push("Category \"type\" is ignored under module.typeImports=\"interleave\".".to_string());
            continue;
          }
          if !seen_categories.insert(*c) {
            diagnostics.push(format!("Category {c:?} listed more than once; using first occurrence."));
            continue;
          }
          if *c == BuiltinCategory::Unknown {
            explicit_unknown = Some(i);
          }
          categories.push(*c);
        }
        ImportMatcher::Pattern { pattern } => match Glob::new(pattern) {
          Ok(g) => {
            builder.add(g);
            has_globs = true;
          }
          Err(e) => diagnostics.push(format!("Invalid glob `{pattern}`: {e}")),
        },
      }
    }

    let globs = match builder.build() {
      Ok(globs) => globs,
      Err(err) => {
        diagnostics.push(format!("Could not build the glob set for group {i}: {err}"));
        has_globs = false;
        GlobSet::empty()
      }
    };
    groups.push(ResolvedGroup { categories, globs, has_globs });
  }

  let unknown_index = match explicit_unknown {
    Some(i) => i,
    None => {
      groups.push(ResolvedGroup {
        categories: vec![BuiltinCategory::Unknown],
        globs: GlobSet::empty(),
        has_globs: false,
      });
      groups.len() - 1
    }
  };

  (Some(ResolvedGroups { groups, unknown_index }), diagnostics)
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::configuration::*;
  use dprint_core::configuration::ConfigKeyMap;

  fn build(json: serde_json::Value) -> Configuration {
    let map: ConfigKeyMap = serde_json::from_value(json).unwrap();
    let r = resolve_config(map, &Default::default());
    r.config
  }

  #[test]
  fn empty_returns_none() {
    let cfg = build(serde_json::json!({}));
    assert!(compile_import_groups(&cfg).0.is_none());
  }

  #[test]
  fn appends_implicit_unknown_at_end() {
    let cfg = build(serde_json::json!({
      "module.importGroups": [{ "match": "builtin" }]
    }));
    let r = compile_import_groups(&cfg).0.unwrap();
    assert_eq!(r.groups.len(), 2);
    assert_eq!(r.unknown_index, 1);
  }

  #[test]
  fn duplicate_category_diagnostic() {
    let cfg = build(serde_json::json!({
      "module.importGroups": [
        { "match": "builtin" },
        { "match": "builtin" }
      ]
    }));
    let (r, diags) = compile_import_groups(&cfg);
    let r = r.unwrap();
    assert_eq!(diags.len(), 1);
    assert_eq!(r.groups[0].categories, vec![BuiltinCategory::Builtin]);
    assert!(r.groups[1].categories.is_empty());
  }

  #[test]
  fn invalid_glob_diagnostic_leaves_the_group_empty() {
    let cfg = build(serde_json::json!({
      "module.importGroups": [{ "match": { "pattern": "[unclosed" } }, { "match": "external" }]
    }));
    let (r, diags) = compile_import_groups(&cfg);
    let r = r.unwrap();
    assert_eq!(diags.len(), 1);
    assert!(diags[0].contains("Invalid glob `[unclosed`"), "{:?}", diags[0]);
    assert!(r.groups[0].categories.is_empty());
    assert!(!r.groups[0].has_globs);
  }

  #[test]
  fn type_category_under_interleave_diagnostic() {
    let cfg = build(serde_json::json!({
      "module.importGroups": [{ "match": "external" }, { "match": "type" }],
      "module.typeImports": "interleave"
    }));
    let (_, diags) = compile_import_groups(&cfg);
    assert!(diags.iter().any(|d| d.contains("type") && d.contains("interleave")));
  }
}
