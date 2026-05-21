//! Compiled form of `module.importGroups` ready for fast classification.

use globset::{Glob, GlobSet, GlobSetBuilder};

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

/// Compile config's `module.importGroups` into resolved form. Returns `None`
/// when the feature is disabled (empty list). Appends diagnostic strings on
/// duplicate categories or invalid globs.
pub fn compile(config: &Configuration, diagnostics: &mut Vec<String>) -> Option<ResolvedGroups> {
  if config.module_import_groups.is_empty() {
    return None;
  }

  let interleave_mode = matches!(config.module_type_imports, TypeImportsMode::Interleave);

  let mut groups: Vec<ResolvedGroup> = Vec::new();
  let mut explicit_unknown: Option<usize> = None;
  let mut seen_categories: std::collections::HashSet<BuiltinCategory> = Default::default();

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
            diagnostics.push(format!(
              "module.importGroups: category \"type\" is ignored under module.typeImports=\"interleave\"."
            ));
            continue;
          }
          if !seen_categories.insert(*c) {
            diagnostics.push(format!(
              "module.importGroups: category {c:?} listed more than once; using first occurrence."
            ));
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
          Err(e) => diagnostics.push(format!("module.importGroups: invalid glob `{pattern}`: {e}")),
        },
      }
    }

    groups.push(ResolvedGroup {
      categories,
      globs: builder.build().unwrap_or_else(|_| GlobSet::empty()),
      has_globs,
    });
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

  Some(ResolvedGroups { groups, unknown_index })
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
    let mut diags = Vec::new();
    assert!(compile(&cfg, &mut diags).is_none());
  }

  #[test]
  fn appends_implicit_unknown_at_end() {
    let cfg = build(serde_json::json!({
      "module.importGroups": [{ "match": "builtin" }]
    }));
    let mut diags = Vec::new();
    let r = compile(&cfg, &mut diags).unwrap();
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
    let mut diags = Vec::new();
    let r = compile(&cfg, &mut diags).unwrap();
    assert_eq!(diags.len(), 1);
    assert_eq!(r.groups[0].categories, vec![BuiltinCategory::Builtin]);
    assert!(r.groups[1].categories.is_empty());
  }

  #[test]
  fn type_category_under_interleave_diagnostic() {
    let cfg = build(serde_json::json!({
      "module.importGroups": [{ "match": "external" }, { "match": "type" }],
      "module.typeImports": "interleave"
    }));
    let mut diags = Vec::new();
    let _ = compile(&cfg, &mut diags).unwrap();
    assert!(diags.iter().any(|d| d.contains("type") && d.contains("interleave")));
  }
}
