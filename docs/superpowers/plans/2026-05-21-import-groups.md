# Import Grouping Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add ESLint-`import/order`-style grouping of TypeScript/JavaScript
import declarations to dprint-plugin-typescript, with optional Biome-style
merging and per-runtime builtin classification.

**Architecture:** Extend the existing `get_stmt_groups` /
`StmtGroupKind::Imports` path in `src/generation/generate.rs`. A new pure
classifier+partitioner runs over each consecutive run of `ImportDecl`s,
producing subgroups. The blank-line predicate is taught to force a blank
between subgroups. Side-effect imports already split runs naturally, so they
remain barriers. Everything is opt-in via a new `module.importGroups` config
key — default empty = byte-identical output to today's release.

**Tech Stack:** Rust 2024, `deno_ast`/SWC views, `dprint-core` PrintItems,
`globset` (new dep), `phf` (new dep, for compile-time core-module hash set),
the existing dprint spec test harness in `tests/spec_test.rs`.

**Spec:** `docs/superpowers/specs/2026-05-21-import-groups-design.md`.

---

## File Structure

| File | Status | Responsibility |
|---|---|---|
| `src/utils/builtins.rs` | new | Node 22 / Bun builtin module lists, `is_builtin(src, runtime)`. |
| `src/utils/mod.rs` | modify | `pub mod builtins;` |
| `src/configuration/types.rs` | modify | New enums `TypeImportsMode`, `BuiltinsRuntime`; new struct `ImportGroup` + `ImportMatcher`; new fields on `Configuration`. |
| `src/configuration/builder.rs` | modify | Builder methods + defaults for the four new keys. |
| `src/configuration/resolve_config.rs` | modify | Parse, validate, compile patterns into resolved import groups; diagnostics. |
| `src/generation/imports/mod.rs` | new | Sub-module root: `pub mod classify; pub mod partition; pub mod merge;` |
| `src/generation/imports/classify.rs` | new | Pure classifier: `(src, is_type, &ResolvedGroups, &Config) → usize`. |
| `src/generation/imports/partition.rs` | new | Stable partition + within-group sort. Returns `(Vec<Node>, Vec<usize> boundaries)`. |
| `src/generation/imports/merge.rs` | new | Optional merge pass over a classified subgroup. |
| `src/generation/mod.rs` | modify | `pub mod imports;` |
| `src/generation/generate.rs` | modify | Extend `StmtGroup`, call `partition_import_group`, force blank line at subgroup boundary. Header-comment pinning. |
| `tests/specs/declarations/import/ImportGroups_*.txt` | new | Spec-test files. |
| `deployment/schema.json` | modify | Add JSON schema entries for the four new keys (if file exists in repo). |
| `Cargo.toml` | modify | Add `globset` (and `phf` if not in deno_ast transitive). |

---

## Conventions Used Throughout

- All new code lives in modules small enough to hold in head; each new file
  has one clear responsibility.
- TDD: every behavior change starts with a failing test (Rust unit test or
  dprint spec test) before the implementation step.
- Commit after every passing test step. Conventional Commits prefixes:
  `feat:`, `test:`, `refactor:`, `docs:`. Issue tag `(#493)` in subject of
  the last user-visible commit per phase.
- Branch already exists: `feat-import-groups`. Run `git switch
  feat-import-groups` before starting.
- `cargo test --test specs` runs the dprint spec suite; `cargo test --lib`
  runs Rust unit tests.

---

## Phase 0: Branch & Baseline

### Task 0.1: Switch to branch, run baseline tests green

**Files:** none.

- [ ] **Step 1: Switch branch**

```bash
cd /Users/todor.andonov/projects/oss/dprint-plugin-typescript
git switch feat-import-groups
```

- [ ] **Step 2: Run full test suite to confirm green baseline**

```bash
cargo test --release
```

Expected: all tests pass. If not, stop and investigate — do not start the
feature on a red baseline.

---

## Phase 1: Configuration types (no behavior change)

### Task 1.1: Add `TypeImportsMode` and `BuiltinsRuntime` enums

**Files:**
- Modify: `src/configuration/types.rs`

- [ ] **Step 1: Add a failing unit test for round-trip string conversion**

Append to `src/configuration/types.rs`:

```rust
#[cfg(test)]
mod import_group_enum_tests {
  use super::*;

  #[test]
  fn type_imports_mode_round_trip() {
    assert_eq!(TypeImportsMode::from_str("separate"), Ok(TypeImportsMode::Separate));
    assert_eq!(TypeImportsMode::from_str("interleave"), Ok(TypeImportsMode::Interleave));
    assert_eq!(TypeImportsMode::Separate.to_string(), "separate");
  }

  #[test]
  fn builtins_runtime_round_trip() {
    assert_eq!(BuiltinsRuntime::from_str("node"), Ok(BuiltinsRuntime::Node));
    assert_eq!(BuiltinsRuntime::from_str("deno"), Ok(BuiltinsRuntime::Deno));
    assert_eq!(BuiltinsRuntime::from_str("bun"), Ok(BuiltinsRuntime::Bun));
    assert_eq!(BuiltinsRuntime::from_str("none"), Ok(BuiltinsRuntime::None));
    assert_eq!(BuiltinsRuntime::Node.to_string(), "node");
  }
}
```

- [ ] **Step 2: Run the test, confirm it fails**

```bash
cargo test --lib import_group_enum_tests
```

Expected: compile error — `TypeImportsMode` / `BuiltinsRuntime` undefined.

- [ ] **Step 3: Add the enums and `generate_str_to_from!` invocations**

Insert before the `Configuration` struct (around line 309) in
`src/configuration/types.rs`:

```rust
/// How type-only imports are classified by `module.importGroups`.
#[derive(Clone, Copy, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum TypeImportsMode {
  /// Type-only imports form a distinct implicit category `type`.
  Separate,
  /// Type-only imports are classified by source path like value imports.
  Interleave,
}

generate_str_to_from![TypeImportsMode, [Separate, "separate"], [Interleave, "interleave"]];

/// Which runtime's built-in modules count as `builtin` for grouping.
#[derive(Clone, Copy, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum BuiltinsRuntime {
  /// `node:` prefix or Node core module list (default).
  Node,
  /// `node:` prefix only.
  Deno,
  /// `node:` prefix, `bun:` prefix, or Node core module list.
  Bun,
  /// Nothing matches `builtin`.
  None,
}

generate_str_to_from![
  BuiltinsRuntime,
  [Node, "node"],
  [Deno, "deno"],
  [Bun, "bun"],
  [None, "none"]
];
```

- [ ] **Step 4: Run the test, confirm it passes**

```bash
cargo test --lib import_group_enum_tests
```

Expected: 2 tests pass.

- [ ] **Step 5: Commit**

```bash
git add src/configuration/types.rs
git commit -m "feat(config): add TypeImportsMode and BuiltinsRuntime enums"
```

### Task 1.2: Add `ImportGroup` / `ImportMatcher` value types

**Files:**
- Modify: `src/configuration/types.rs`

- [ ] **Step 1: Add a failing unit test**

Append to the same `#[cfg(test)] mod import_group_enum_tests` block:

```rust
#[test]
fn import_matcher_variants() {
  let _c = ImportMatcher::Category(BuiltinCategory::External);
  let _p = ImportMatcher::Pattern("foo/*".to_string());
}

#[test]
fn builtin_category_round_trip() {
  assert_eq!(BuiltinCategory::from_str("builtin"), Ok(BuiltinCategory::Builtin));
  assert_eq!(BuiltinCategory::from_str("external"), Ok(BuiltinCategory::External));
  assert_eq!(BuiltinCategory::from_str("parent"), Ok(BuiltinCategory::Parent));
  assert_eq!(BuiltinCategory::from_str("sibling"), Ok(BuiltinCategory::Sibling));
  assert_eq!(BuiltinCategory::from_str("index"), Ok(BuiltinCategory::Index));
  assert_eq!(BuiltinCategory::from_str("type"), Ok(BuiltinCategory::Type));
  assert_eq!(BuiltinCategory::from_str("unknown"), Ok(BuiltinCategory::Unknown));
}
```

- [ ] **Step 2: Confirm test fails**

```bash
cargo test --lib import_group_enum_tests
```

Expected: compile error — types undefined.

- [ ] **Step 3: Add `BuiltinCategory`, `ImportMatcher`, `ImportGroup`**

In `src/configuration/types.rs`, after the `BuiltinsRuntime` block:

```rust
/// Built-in category strings allowed in `module.importGroups[].match`.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum BuiltinCategory {
  Builtin,
  External,
  Parent,
  Sibling,
  Index,
  Type,
  Unknown,
}

generate_str_to_from![
  BuiltinCategory,
  [Builtin, "builtin"],
  [External, "external"],
  [Parent, "parent"],
  [Sibling, "sibling"],
  [Index, "index"],
  [Type, "type"],
  [Unknown, "unknown"]
];

/// A single matcher inside a group's `match` value.
#[derive(Clone, Debug)]
pub enum ImportMatcher {
  Category(BuiltinCategory),
  /// Raw glob pattern string. Compiled lazily by resolve_config into a globset.
  Pattern(String),
}

/// One resolved import group, in user-listed order.
#[derive(Clone, Debug)]
pub struct ImportGroup {
  pub matchers: Vec<ImportMatcher>,
}
```

- [ ] **Step 4: Test passes**

```bash
cargo test --lib import_group_enum_tests
```

Expected: all 4 tests pass.

- [ ] **Step 5: Commit**

```bash
git add src/configuration/types.rs
git commit -m "feat(config): add ImportGroup, ImportMatcher, BuiltinCategory"
```

### Task 1.3: Add config fields to `Configuration`

**Files:**
- Modify: `src/configuration/types.rs`

- [ ] **Step 1: Add fields**

Find the `/* sorting */` block (around line 344) in `Configuration`. Insert
after `export_declaration_sort_type_only_exports: NamedTypeImportsExportsOrder,`:

```rust
  #[serde(rename = "module.importGroups", default, skip_serializing_if = "Vec::is_empty")]
  pub module_import_groups: Vec<ImportGroup>,
  #[serde(rename = "module.typeImports", default = "default_type_imports_mode")]
  pub module_type_imports: TypeImportsMode,
  #[serde(rename = "module.mergeImports", default)]
  pub module_merge_imports: bool,
  #[serde(rename = "module.builtinsRuntime", default = "default_builtins_runtime")]
  pub module_builtins_runtime: BuiltinsRuntime,
```

At the end of the file (before any `#[cfg(test)]` blocks), add:

```rust
fn default_type_imports_mode() -> TypeImportsMode {
  TypeImportsMode::Separate
}

fn default_builtins_runtime() -> BuiltinsRuntime {
  BuiltinsRuntime::Node
}
```

- [ ] **Step 2: Manually add `Serialize` / `Deserialize` derive on `ImportGroup` and `ImportMatcher`**

Replace:

```rust
#[derive(Clone, Debug)]
pub enum ImportMatcher { ... }

#[derive(Clone, Debug)]
pub struct ImportGroup { ... }
```

with:

```rust
#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(untagged)]
pub enum ImportMatcher {
  Category(BuiltinCategory),
  Pattern { pattern: String },
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ImportGroup {
  /// Either a single matcher or a list (list = merged into one group).
  #[serde(rename = "match")]
  pub matchers: ImportGroupMatch,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(untagged)]
pub enum ImportGroupMatch {
  Single(ImportMatcher),
  Multiple(Vec<ImportMatcher>),
}
```

Update the earlier unit test that used `ImportMatcher::Pattern("foo/*".to_string())` to:

```rust
let _p = ImportMatcher::Pattern { pattern: "foo/*".to_string() };
```

- [ ] **Step 3: Run cargo check**

```bash
cargo check --lib
```

Expected: compiles. Resolve any errors before continuing.

- [ ] **Step 4: Run lib tests**

```bash
cargo test --lib
```

Expected: all existing tests still pass + the new ones from 1.1/1.2.

- [ ] **Step 5: Commit**

```bash
git add src/configuration/types.rs
git commit -m "feat(config): add module.importGroups/typeImports/mergeImports/builtinsRuntime fields"
```

### Task 1.4: Builder methods

**Files:**
- Modify: `src/configuration/builder.rs`

- [ ] **Step 1: Find existing builder defaults block**

Look at lines 65–75 of `src/configuration/builder.rs` — there's a default
chain calling `.module_sort_import_declarations(SortOrder::Maintain)`. Add
the four new defaults right after it.

- [ ] **Step 2: Append four builder methods**

In `src/configuration/builder.rs`, after
`pub fn export_declaration_sort_type_only_exports(...)` (around line 577),
insert:

```rust
  /// Ordered groups for `module.importGroups`. Empty = feature disabled.
  ///
  /// Default: `[]`
  pub fn module_import_groups(&mut self, value: Vec<crate::configuration::ImportGroup>) -> &mut Self {
    self.insert("module.importGroups", serde_json::to_value(value).unwrap().into())
  }

  /// How type-only imports are classified.
  ///
  /// Default: `Separate`
  pub fn module_type_imports(&mut self, value: TypeImportsMode) -> &mut Self {
    self.insert("module.typeImports", value.to_string().into())
  }

  /// Merge multiple imports from the same source into one declaration.
  ///
  /// Default: `false`
  pub fn module_merge_imports(&mut self, value: bool) -> &mut Self {
    self.insert("module.mergeImports", value.into())
  }

  /// Which runtime's built-in modules count as `builtin`.
  ///
  /// Default: `Node`
  pub fn module_builtins_runtime(&mut self, value: BuiltinsRuntime) -> &mut Self {
    self.insert("module.builtinsRuntime", value.to_string().into())
  }
```

Also add `BuiltinsRuntime, TypeImportsMode` to the `use` imports at the top
of the file.

- [ ] **Step 3: cargo check**

```bash
cargo check --lib
```

Expected: compiles.

- [ ] **Step 4: Commit**

```bash
git add src/configuration/builder.rs
git commit -m "feat(config): add builder methods for import grouping config"
```

### Task 1.5: Resolve config (parse + diagnostics)

**Files:**
- Modify: `src/configuration/resolve_config.rs`

- [ ] **Step 1: Add `get_value` calls for the three scalar keys**

Find the `/* sorting */` block in `resolve_config` (around line 118). After
the `export_declaration_sort_type_only_exports` block (which spans roughly
129–134), insert:

```rust
    module_import_groups: parse_import_groups(&mut config, &mut diagnostics),
    module_type_imports: get_value(&mut config, "module.typeImports", TypeImportsMode::Separate, &mut diagnostics),
    module_merge_imports: get_value(&mut config, "module.mergeImports", false, &mut diagnostics),
    module_builtins_runtime: get_value(&mut config, "module.builtinsRuntime", BuiltinsRuntime::Node, &mut diagnostics),
```

- [ ] **Step 2: Add `parse_import_groups` helper at the bottom of the file**

At the end of `src/configuration/resolve_config.rs`:

```rust
fn parse_import_groups(
  config: &mut ConfigKeyMap,
  diagnostics: &mut Vec<ConfigurationDiagnostic>,
) -> Vec<ImportGroup> {
  let Some(raw) = config.shift_remove("module.importGroups") else {
    return Vec::new();
  };
  match serde_json::from_value::<Vec<ImportGroup>>(raw.clone().into()) {
    Ok(groups) => groups,
    Err(err) => {
      diagnostics.push(ConfigurationDiagnostic {
        property_name: "module.importGroups".to_string(),
        message: format!("Invalid import groups configuration: {err}"),
      });
      Vec::new()
    }
  }
}
```

Add `use crate::configuration::{ImportGroup, TypeImportsMode, BuiltinsRuntime};` to the imports at top if not already present.

- [ ] **Step 3: cargo check**

```bash
cargo check --lib
```

Expected: compiles.

- [ ] **Step 4: Add a config-resolution unit test**

Append to `src/configuration/resolve_config.rs`:

```rust
#[cfg(test)]
mod import_groups_resolution_tests {
  use super::*;
  use dprint_core::configuration::ConfigKeyMap;

  fn resolve(json: serde_json::Value) -> ResolveConfigurationResult<Configuration> {
    let map: ConfigKeyMap = serde_json::from_value(json).unwrap();
    resolve_config(map, &Default::default())
  }

  #[test]
  fn empty_import_groups_default() {
    let r = resolve(serde_json::json!({}));
    assert!(r.config.module_import_groups.is_empty());
    assert_eq!(r.config.module_type_imports, TypeImportsMode::Separate);
    assert!(!r.config.module_merge_imports);
    assert_eq!(r.config.module_builtins_runtime, BuiltinsRuntime::Node);
    assert!(r.diagnostics.is_empty());
  }

  #[test]
  fn parses_basic_eslint_mirror() {
    let r = resolve(serde_json::json!({
      "module.importGroups": [
        { "match": "builtin" },
        { "match": "external" },
        { "match": ["sibling", "index"] }
      ]
    }));
    assert!(r.diagnostics.is_empty());
    assert_eq!(r.config.module_import_groups.len(), 3);
  }

  #[test]
  fn invalid_import_groups_emits_diagnostic() {
    let r = resolve(serde_json::json!({
      "module.importGroups": "not-an-array"
    }));
    assert_eq!(r.config.module_import_groups.len(), 0);
    assert_eq!(r.diagnostics.len(), 1);
    assert_eq!(r.diagnostics[0].property_name, "module.importGroups");
  }
}
```

- [ ] **Step 5: Run tests**

```bash
cargo test --lib import_groups_resolution_tests
```

Expected: 3 tests pass.

- [ ] **Step 6: Commit**

```bash
git add src/configuration/resolve_config.rs
git commit -m "feat(config): resolve module.importGroups and related keys"
```

### Task 1.6: Regression check — feature off must be byte-identical

**Files:** none.

- [ ] **Step 1: Run full spec suite**

```bash
cargo test --test specs
```

Expected: every existing spec still passes. (Feature is opt-in via empty
`module.importGroups`; default is empty; nothing in generation has changed.)

- [ ] **Step 2: Tag the byte-identical baseline**

```bash
git tag baseline-pre-import-groups
```

Used later to compare against feature-off output.

---

## Phase 2: Builtins utility

### Task 2.1: Add `phf` and `globset` deps

**Files:**
- Modify: `Cargo.toml`

- [ ] **Step 1: Add deps**

Edit `Cargo.toml` `[dependencies]` to add:

```toml
globset = "0.4"
phf = { version = "0.11", features = ["macros"] }
```

- [ ] **Step 2: cargo check**

```bash
cargo check --lib
```

Expected: deps resolve and compile.

- [ ] **Step 3: Commit**

```bash
git add Cargo.toml Cargo.lock
git commit -m "build: add globset and phf dependencies"
```

### Task 2.2: Implement `builtins.rs` with Node core list

**Files:**
- Create: `src/utils/builtins.rs`
- Modify: `src/utils/mod.rs`

- [ ] **Step 1: Add to module root**

In `src/utils/mod.rs`, append:

```rust
pub mod builtins;
```

- [ ] **Step 2: Create `src/utils/builtins.rs` with failing tests**

```rust
//! Built-in module classification.
//!
//! Node core list is a snapshot of `module.builtinModules` from Node 22 LTS.
//! Bun core list is the documented set of `bun:*` namespaces as of Bun 1.1.

use crate::configuration::BuiltinsRuntime;

/// Returns true if `src` (the bare specifier string, without surrounding
/// quotes) is a built-in module under the given runtime.
pub fn is_builtin(src: &str, runtime: BuiltinsRuntime) -> bool {
  match runtime {
    BuiltinsRuntime::Node => has_node_prefix(src) || NODE_CORE.contains(src),
    BuiltinsRuntime::Deno => has_node_prefix(src),
    BuiltinsRuntime::Bun => has_node_prefix(src) || has_bun_prefix(src) || NODE_CORE.contains(src),
    BuiltinsRuntime::None => false,
  }
}

fn has_node_prefix(src: &str) -> bool {
  src.starts_with("node:")
}

fn has_bun_prefix(src: &str) -> bool {
  src.starts_with("bun:")
}

/// Node 22 LTS `module.builtinModules` snapshot (no `node:` prefix).
static NODE_CORE: phf::Set<&'static str> = phf::phf_set! {
  "assert", "assert/strict", "async_hooks", "buffer", "child_process",
  "cluster", "console", "constants", "crypto", "dgram", "diagnostics_channel",
  "dns", "dns/promises", "domain", "events", "fs", "fs/promises", "http",
  "http2", "https", "inspector", "inspector/promises", "module", "net", "os",
  "path", "path/posix", "path/win32", "perf_hooks", "process", "punycode",
  "querystring", "readline", "readline/promises", "repl", "stream",
  "stream/consumers", "stream/promises", "stream/web", "string_decoder",
  "sys", "test", "timers", "timers/promises", "tls", "trace_events", "tty",
  "url", "util", "util/types", "v8", "vm", "wasi", "worker_threads", "zlib",
};

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn node_runtime_recognizes_node_prefix() {
    assert!(is_builtin("node:fs", BuiltinsRuntime::Node));
    assert!(is_builtin("node:path/posix", BuiltinsRuntime::Node));
  }

  #[test]
  fn node_runtime_recognizes_bare_core() {
    assert!(is_builtin("fs", BuiltinsRuntime::Node));
    assert!(is_builtin("path", BuiltinsRuntime::Node));
    assert!(is_builtin("util/types", BuiltinsRuntime::Node));
    assert!(!is_builtin("react", BuiltinsRuntime::Node));
  }

  #[test]
  fn deno_runtime_only_node_prefix() {
    assert!(is_builtin("node:fs", BuiltinsRuntime::Deno));
    assert!(!is_builtin("fs", BuiltinsRuntime::Deno));
    assert!(!is_builtin("npm:react", BuiltinsRuntime::Deno));
    assert!(!is_builtin("jsr:@std/path", BuiltinsRuntime::Deno));
    assert!(!is_builtin("https://deno.land/x/foo/mod.ts", BuiltinsRuntime::Deno));
  }

  #[test]
  fn bun_runtime_recognizes_bun_prefix() {
    assert!(is_builtin("bun:test", BuiltinsRuntime::Bun));
    assert!(is_builtin("bun:sqlite", BuiltinsRuntime::Bun));
    assert!(is_builtin("node:fs", BuiltinsRuntime::Bun));
    assert!(is_builtin("fs", BuiltinsRuntime::Bun));
  }

  #[test]
  fn none_runtime_matches_nothing() {
    assert!(!is_builtin("fs", BuiltinsRuntime::None));
    assert!(!is_builtin("node:fs", BuiltinsRuntime::None));
    assert!(!is_builtin("bun:test", BuiltinsRuntime::None));
  }
}
```

- [ ] **Step 3: Run unit tests**

```bash
cargo test --lib utils::builtins
```

Expected: 5 tests pass.

- [ ] **Step 4: Commit**

```bash
git add src/utils/mod.rs src/utils/builtins.rs
git commit -m "feat: add is_builtin classifier with Node/Deno/Bun runtimes"
```

---

## Phase 3: Classifier (pure)

### Task 3.1: Skeleton module

**Files:**
- Create: `src/generation/imports/mod.rs`
- Modify: `src/generation/mod.rs`

- [ ] **Step 1: Create sub-module**

`src/generation/imports/mod.rs`:

```rust
pub mod classify;
pub mod partition;
pub mod merge;
pub mod resolved;
```

- [ ] **Step 2: Register in parent**

In `src/generation/mod.rs`, append:

```rust
pub mod imports;
```

- [ ] **Step 3: cargo check fails because submodules don't exist**

Expected. Continue.

### Task 3.2: Resolved-config compile step (compile patterns, append `unknown`)

**Files:**
- Create: `src/generation/imports/resolved.rs`

- [ ] **Step 1: Write failing test**

Create `src/generation/imports/resolved.rs`:

```rust
//! Compiled form of `module.importGroups` ready for fast classification.

use globset::{Glob, GlobSet, GlobSetBuilder};

use crate::configuration::{BuiltinCategory, Configuration, ImportGroup, ImportGroupMatch, ImportMatcher};

/// One resolved group: a set of categories + a glob set, in user-listed order.
/// `unknown_index` records which resolved group catches unmatched imports.
#[derive(Debug)]
pub struct ResolvedGroup {
  pub categories: Vec<BuiltinCategory>,
  pub globs: GlobSet,
  pub has_globs: bool,
}

#[derive(Debug)]
pub struct ResolvedGroups {
  pub groups: Vec<ResolvedGroup>,
  pub unknown_index: usize,
}

/// Compile config's `module.importGroups` into resolved form.
/// `diagnostics` is appended to on bad globs or duplicate categories.
pub fn compile(config: &Configuration, diagnostics: &mut Vec<String>) -> Option<ResolvedGroups> {
  if config.module_import_groups.is_empty() {
    return None;
  }

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
          if !seen_categories.insert(*c) {
            diagnostics.push(format!("Category `{c:?}` listed more than once in module.importGroups; using first occurrence."));
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
  use crate::configuration::ConfigurationBuilder;

  fn build(json: serde_json::Value) -> Configuration {
    let mut b = ConfigurationBuilder::new();
    let map: dprint_core::configuration::ConfigKeyMap = serde_json::from_value(json).unwrap();
    b.global_config(Default::default());
    for (k, v) in map.into_iter() {
      b.insert(&k, v.into());
    }
    b.build()
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
    // First group still got the builtin; second has empty categories.
    assert_eq!(r.groups[0].categories, vec![BuiltinCategory::Builtin]);
    assert!(r.groups[1].categories.is_empty());
  }
}
```

- [ ] **Step 2: Run tests**

```bash
cargo test --lib generation::imports::resolved
```

Expected: 3 tests pass.

- [ ] **Step 3: Commit**

```bash
git add src/generation/imports/mod.rs src/generation/imports/resolved.rs src/generation/mod.rs
git commit -m "feat(imports): compile import groups into resolved form"
```

### Task 3.3: Classifier function

**Files:**
- Create: `src/generation/imports/classify.rs`

- [ ] **Step 1: Write file with tests + implementation**

```rust
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
  if is_type_only && type_imports_mode == TypeImportsMode::Separate {
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
    let candidate = format!("./index{ext}");
    if src == candidate {
      return true;
    }
  }
  false
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::configuration::{ConfigurationBuilder, BuiltinsRuntime, TypeImportsMode};
  use crate::generation::imports::resolved::compile;

  fn classify_with(json: serde_json::Value, src: &str, is_type: bool) -> usize {
    let cfg = {
      let mut b = ConfigurationBuilder::new();
      b.global_config(Default::default());
      let map: dprint_core::configuration::ConfigKeyMap = serde_json::from_value(json).unwrap();
      for (k, v) in map.into_iter() {
        b.insert(&k, v.into());
      }
      b.build()
    };
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
    // `react` is external, not builtin → falls to implicit unknown (index 1).
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
    // Value import of external → 0; type import → 1.
    assert_eq!(classify_with(cfg.clone(), "react", false), 0);
    assert_eq!(classify_with(cfg, "react", true), 1);
  }

  #[test]
  fn type_interleave_classifies_by_path() {
    let cfg = serde_json::json!({
      "module.importGroups": [
        { "match": "external" }
      ],
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
    // `@app/foo` matches `external` first → 0.
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
    assert_eq!(classify_with(cfg, "@app/foo", false), 0);
    assert_eq!(classify_with(cfg, "react", false), 1);
  }
}
```

- [ ] **Step 2: Run tests**

```bash
cargo test --lib generation::imports::classify
```

Expected: 9 tests pass.

- [ ] **Step 3: Commit**

```bash
git add src/generation/imports/classify.rs
git commit -m "feat(imports): pure classifier of import sources into resolved groups"
```

---

## Phase 4: Partitioner

### Task 4.1: `partition_import_group`

**Files:**
- Create: `src/generation/imports/partition.rs`

- [ ] **Step 1: Write skeleton + tests**

Because partitioning operates on SWC `Node`s with lifetimes, the tests will
use a thin abstraction. Write:

```rust
//! Stable partition of an import-group's nodes by classified group index.

/// Given a list of (group_index, original_index) pairs, return a new ordering
/// of original indices that:
///   1. groups items by group_index (in ascending order of group_index),
///   2. within each group, sorts using `cmp_within_group` (stable),
///   3. records the start index of each non-empty group as a boundary.
///
/// `cmp_within_group` may return `Equal` to mean "preserve source order".
pub fn partition_indices<F>(
  classified: &[(usize, usize)], // (group_index, original_index)
  num_groups: usize,
  mut cmp_within_group: F,
) -> (Vec<usize>, Vec<usize>)
where
  F: FnMut(usize, usize) -> std::cmp::Ordering,
{
  // Bucket by group_index, preserving relative order (stable).
  let mut buckets: Vec<Vec<usize>> = (0..num_groups).map(|_| Vec::new()).collect();
  for &(g, orig) in classified {
    buckets[g].push(orig);
  }

  // Sort within each bucket using the provided comparator.
  for b in buckets.iter_mut() {
    b.sort_by(|&a, &b| cmp_within_group(a, b));
  }

  // Flatten + record boundaries.
  let mut ordered = Vec::with_capacity(classified.len());
  let mut boundaries = Vec::new();
  for b in buckets.into_iter() {
    if b.is_empty() {
      continue;
    }
    boundaries.push(ordered.len());
    ordered.extend(b);
  }
  (ordered, boundaries)
}

#[cfg(test)]
mod tests {
  use super::*;
  use std::cmp::Ordering;

  fn equal(_a: usize, _b: usize) -> Ordering {
    Ordering::Equal
  }

  #[test]
  fn three_groups_preserves_within_group_order_when_equal() {
    // Originals classified as group 0, 2, 1, 0, 2.
    let input = vec![(0, 0), (2, 1), (1, 2), (0, 3), (2, 4)];
    let (ordered, boundaries) = partition_indices(&input, 3, equal);
    // Group 0 first (originals 0, 3); then group 1 (2); then group 2 (1, 4).
    assert_eq!(ordered, vec![0, 3, 2, 1, 4]);
    assert_eq!(boundaries, vec![0, 2, 3]);
  }

  #[test]
  fn empty_buckets_omitted_from_boundaries() {
    let input = vec![(0, 0), (2, 1)];
    let (ordered, boundaries) = partition_indices(&input, 3, equal);
    assert_eq!(ordered, vec![0, 1]);
    // Group 1 is empty so only two boundary indices (one per non-empty bucket).
    assert_eq!(boundaries, vec![0, 1]);
  }

  #[test]
  fn within_group_sort_applies() {
    // Single group of 3 items; sort descending by original index.
    let input = vec![(0, 0), (0, 1), (0, 2)];
    let (ordered, _) = partition_indices(&input, 1, |a, b| b.cmp(&a));
    assert_eq!(ordered, vec![2, 1, 0]);
  }
}
```

- [ ] **Step 2: Run tests**

```bash
cargo test --lib generation::imports::partition
```

Expected: 3 tests pass.

- [ ] **Step 3: Commit**

```bash
git add src/generation/imports/partition.rs
git commit -m "feat(imports): stable partition of classified imports"
```

---

## Phase 5: Integration into `gen_statements`

### Task 5.1: Stub `merge` module

**Files:**
- Create: `src/generation/imports/merge.rs`

- [ ] **Step 1: Empty placeholder**

```rust
//! Merge pass for `module.mergeImports: true`. Implemented in Phase 8.
```

- [ ] **Step 2: Commit**

```bash
git add src/generation/imports/merge.rs
git commit -m "chore(imports): stub merge module"
```

### Task 5.2: Extend `StmtGroup` with subgroup boundaries

**Files:**
- Modify: `src/generation/generate.rs`

- [ ] **Step 1: Update struct**

Locate `struct StmtGroup<'a>` (around line 7416). Replace with:

```rust
struct StmtGroup<'a> {
  kind: StmtGroupKind,
  nodes: Vec<Node<'a>>,
  /// Indices into `nodes` marking start of each subgroup. Only Some for
  /// import groups when `module.importGroups` is non-empty.
  subgroup_boundaries: Option<Vec<usize>>,
}
```

Update the three construction sites in `get_stmt_groups` to include
`subgroup_boundaries: None,`.

- [ ] **Step 2: cargo check**

```bash
cargo check --lib
```

Expected: compiles.

- [ ] **Step 3: Commit**

```bash
git add src/generation/generate.rs
git commit -m "refactor(generate): add subgroup_boundaries to StmtGroup"
```

### Task 5.3: Partition imports during `get_stmt_groups`

**Files:**
- Modify: `src/generation/generate.rs`

- [ ] **Step 1: Add classification + partition call**

At the end of `get_stmt_groups`, just before `groups` is returned, insert:

```rust
  // Apply import-group partitioning when enabled.
  if let Some(resolved) = context.resolved_import_groups.as_ref() {
    for g in groups.iter_mut() {
      if g.kind != StmtGroupKind::Imports {
        continue;
      }
      let classified: Vec<(usize, usize)> = g
        .nodes
        .iter()
        .enumerate()
        .map(|(i, node)| {
          let (src, is_type) = if let Node::ImportDecl(d) = node {
            (d.src.value().as_str().to_string(), d.type_only())
          } else {
            (String::new(), false)
          };
          let idx = crate::generation::imports::classify::classify(
            &src,
            is_type,
            context.config.module_type_imports,
            context.config.module_builtins_runtime,
            resolved,
          );
          (idx, i)
        })
        .collect();
      let (ordered, boundaries) = crate::generation::imports::partition::partition_indices(
        &classified,
        resolved.groups.len(),
        |_a, _b| std::cmp::Ordering::Equal, // intra-group sort handled by existing sorter later
      );
      // Reorder nodes in place.
      let mut new_nodes: Vec<Node> = Vec::with_capacity(g.nodes.len());
      for orig in &ordered {
        new_nodes.push(g.nodes[*orig]);
      }
      g.nodes = new_nodes;
      g.subgroup_boundaries = Some(boundaries);
    }
  }
```

- [ ] **Step 2: Add `resolved_import_groups` to `Context`**

In `src/generation/context.rs`, add to the `Context` struct (find the
`struct Context<'a>` definition):

```rust
  pub resolved_import_groups: Option<crate::generation::imports::resolved::ResolvedGroups>,
```

Initialize it from `Context::new` — find the constructor, add a parameter or
compute from config. The simplest path: compute inline at the call site in
`gen_program` (look for `Context::new(...)`) and pass it in.

**Where to wire it (concrete instructions):**

1. Find `Context::new` definition in `src/generation/context.rs`. It currently
   takes a number of args (`file_path`, `program`, `config`, etc.). Do NOT
   add another argument — instead, compute `resolved_import_groups` from
   `config` inside `Context::new` itself. Add this line just before the
   struct literal `Context { ... }` is returned:

   ```rust
   let mut _import_group_diags: Vec<String> = Vec::new();
   let resolved_import_groups = crate::generation::imports::resolved::compile(config, &mut _import_group_diags);
   // diagnostics dropped here for now; surfaced via resolve_config in Task 9.3
   ```

2. Add `resolved_import_groups,` to the struct literal.

This avoids touching every `Context::new` caller in this task.

- [ ] **Step 3: cargo build**

```bash
cargo build --lib
```

Expected: compiles. Tests not run yet — behavior change comes in Task 5.4.

- [ ] **Step 4: Commit**

```bash
git add src/generation/generate.rs src/generation/context.rs
git commit -m "feat(imports): partition imports during get_stmt_groups when enabled"
```

### Task 5.4: Force blank line at subgroup boundary

**Files:**
- Modify: `src/generation/generate.rs`

- [ ] **Step 1: Update blank-line decision inside `gen_members`-for-statements loop**

Locate the block at the line numbers around 7310–7314 (currently shown
above), where `has_separating_blank_line` decides whether to push a second
NewLine. Replace that conditional with one that also consults the subgroup
boundary:

```rust
        if let Some(last_node) = &last_node {
          separator_items.push_signal(Signal::NewLine);
          let crosses_subgroup_boundary = stmt_group
            .subgroup_boundaries
            .as_ref()
            .map(|bs| bs.contains(&i))
            .unwrap_or(false);
          if crosses_subgroup_boundary
            || node_helpers::has_separating_blank_line(last_node, &node, context.program)
          {
            separator_items.push_signal(Signal::NewLine);
          }
          generated_line_separators.insert(i, separator_items);
        }
```

Important: `i` is the *post-reorder* index. `subgroup_boundaries` stores
post-reorder indices, so this is correct. The first boundary (index 0) is
naturally skipped because `last_node` is `None` then.

- [ ] **Step 2: cargo build**

```bash
cargo build --lib
```

Expected: compiles.

- [ ] **Step 3: First end-to-end spec test**

Create `tests/specs/declarations/import/ImportGroups_Basic.txt`:

```
~~ lineWidth: 80, module.importGroups: [{"match":"builtin"},{"match":"external"},{"match":["sibling","index"]}], module.sortImportDeclarations: maintain ~~
== reorders into builtin / external / sibling+index with blank lines ==
import { c } from "./c";
import { x } from "react";
import { fs } from "node:fs";
import { d } from "./index";

[expect]
import { fs } from "node:fs";

import { x } from "react";

import { c } from "./c";
import { d } from "./index";
```

- [ ] **Step 4: Run new spec**

```bash
cargo test --test specs declarations::import::ImportGroups_Basic
```

Expected: passes. If output diff appears, inspect — likely the blank line is
in the wrong place or sorter is reordering further.

- [ ] **Step 5: Run full spec suite**

```bash
cargo test --test specs
```

Expected: all existing specs still pass. Feature-off byte-identical.

- [ ] **Step 6: Commit**

```bash
git add src/generation/generate.rs tests/specs/declarations/import/ImportGroups_Basic.txt
git commit -m "feat(imports): force blank line at subgroup boundary (#493)"
```

---

## Phase 6: Within-group sort + spec coverage

### Task 6.1: Wire intra-subgroup sorter

**Files:**
- Modify: `src/generation/generate.rs`

- [ ] **Step 1: Replace the `Equal` placeholder in Task 5.3's partition call**

Find the `|_a, _b| std::cmp::Ordering::Equal` closure. Replace with a real
comparator that:

- Reads `context.config.module_sort_import_declarations`.
- If `Maintain`: returns `Equal` (preserves source order in each bucket).
- Else: uses `cmp_module_specifiers` (existing helper in
  `src/generation/sorting/module_specifiers.rs`) over the two nodes' source
  strings, with `str::cmp` for `CaseSensitive` and case-insensitive cmp for
  `CaseInsensitive`.

```rust
      use crate::configuration::SortOrder;
      use crate::generation::sorting::module_specifiers::cmp_module_specifiers;

      let cmp = move |a_orig: usize, b_orig: usize| -> std::cmp::Ordering {
        let sort = context.config.module_sort_import_declarations;
        if sort == SortOrder::Maintain {
          return a_orig.cmp(&b_orig);
        }
        let src_a = node_src_with_quotes(&g.nodes[a_orig], context);
        let src_b = node_src_with_quotes(&g.nodes[b_orig], context);
        match sort {
          SortOrder::CaseSensitive => cmp_module_specifiers(&src_a, &src_b, |x, y| x.cmp(y)),
          SortOrder::CaseInsensitive => cmp_module_specifiers(&src_a, &src_b, |x, y| x.to_lowercase().cmp(&y.to_lowercase())),
          SortOrder::Maintain => unreachable!(),
        }
      };
```

`node_src_with_quotes` helper (add near `partition_indices` usage):

```rust
fn node_src_with_quotes<'a>(node: &Node<'a>, context: &Context<'a>) -> String {
  if let Node::ImportDecl(d) = node {
    // cmp_module_specifiers wants text including the surrounding quotes.
    d.src.text_fast(context.program).to_string()
  } else {
    String::new()
  }
}
```

NOTE: this duplicates the existing sorter behavior found via
`get_node_sorter_from_order` — when the existing sorter at the end of
`gen_statements` runs, it will reorder *again* if we don't disable it for
import groups when the feature is on. Disable it: in `get_node_sorter`
(around line 7382), short-circuit when `subgroup_boundaries.is_some()`:

```rust
  fn get_node_sorter<'a>(
    group_kind: StmtGroupKind,
    stmt_group: &StmtGroup<'a>,
    context: &Context<'a>,
  ) -> Option<...> {
    if stmt_group.subgroup_boundaries.is_some() {
      return None; // Already sorted by partitioner.
    }
    match group_kind { ... }
  }
```

Update the call site of `get_node_sorter` to pass `&stmt_group`.

- [ ] **Step 2: Build**

```bash
cargo build --lib
```

Expected: compiles.

- [ ] **Step 3: Spec test for within-group sort**

Append to `ImportGroups_Basic.txt`:

```
== with caseInsensitive sort within each group ==
~~ lineWidth: 80, module.importGroups: [{"match":"external"},{"match":["sibling","index"]}], module.sortImportDeclarations: caseInsensitive ~~
import { B } from "./b";
import { z } from "zlib2";
import { a } from "./a";
import { Alpha } from "alpha";

[expect]
import { Alpha } from "alpha";
import { z } from "zlib2";

import { a } from "./a";
import { B } from "./b";
```

(Adjust the spec format if the harness expects a single config header per
file — split into two files if needed.)

- [ ] **Step 4: Run**

```bash
cargo test --test specs declarations::import::ImportGroups_Basic
```

Expected: passes.

- [ ] **Step 5: Commit**

```bash
git add src/generation/generate.rs tests/specs/declarations/import/ImportGroups_Basic.txt
git commit -m "feat(imports): within-subgroup sort honors module.sortImportDeclarations"
```

### Task 6.2: Spec coverage for type-only modes

**Files:**
- Create: `tests/specs/declarations/import/ImportGroups_TypeImports.txt`

- [ ] **Step 1: Write spec file with 4 sub-tests**

```
~~ lineWidth: 80, module.importGroups: [{"match":"external"},{"match":"type"}], module.sortImportDeclarations: caseInsensitive ~~
== typeImports separate (default): pulls import type into type group ==
import { a } from "alpha";
import type { B } from "beta";
import { c } from "gamma";

[expect]
import { a } from "alpha";
import { c } from "gamma";

import type { B } from "beta";

== mixed default + type specifier stays value (no type-only flag on decl) ==
import Foo, { type Bar } from "alpha";
import type { Baz } from "beta";

[expect]
import Foo, { type Bar } from "alpha";

import type { Baz } from "beta";
```

```
~~ lineWidth: 80, module.importGroups: [{"match":"external"}], module.typeImports: interleave ~~
== typeImports interleave: type and value imports mix in the external group ==
import { a } from "alpha";
import type { B } from "beta";
import { c } from "gamma";

[expect]
import { a } from "alpha";
import type { B } from "beta";
import { c } from "gamma";
```

(Split into two files if the spec runner requires a single header.)

- [ ] **Step 2: Run**

```bash
cargo test --test specs declarations::import::ImportGroups_TypeImports
```

Expected: passes.

- [ ] **Step 3: Commit**

```bash
git add tests/specs/declarations/import/ImportGroups_TypeImports.txt
git commit -m "test(imports): coverage for typeImports separate/interleave modes"
```

### Task 6.3: Spec coverage for builtinsRuntime

**Files:**
- Create: `tests/specs/declarations/import/ImportGroups_BuiltinsRuntime_Node.txt`
- Create: `tests/specs/declarations/import/ImportGroups_BuiltinsRuntime_Deno.txt`
- Create: `tests/specs/declarations/import/ImportGroups_BuiltinsRuntime_Bun.txt`
- Create: `tests/specs/declarations/import/ImportGroups_BuiltinsRuntime_None.txt`

- [ ] **Step 1: Write four spec files**

For each runtime, write one spec where the input contains:

```
import fs from "fs";
import { test } from "bun:test";
import { x } from "node:path";
import { y } from "npm:react";
import { z } from "react";
```

and the expected output groups them per the runtime table:

| Runtime | `fs` | `bun:test` | `node:path` | `npm:react` | `react` |
|---|---|---|---|---|---|
| node | builtin | unknown* | builtin | unknown* | external |
| deno | external | external | builtin | external | external |
| bun | builtin | builtin | builtin | external | external |
| none | external | external | external | external | external |

(*Under `node`, `bun:test` and `npm:react` aren't recognized → external.
Under `deno`/`bun`, `npm:react` is just external by virtue of not matching
any builtin rule.)

Use config:

```
module.importGroups: [{"match":"builtin"},{"match":"external"}], module.builtinsRuntime: <runtime>, module.sortImportDeclarations: caseInsensitive
```

- [ ] **Step 2: Run**

```bash
cargo test --test specs declarations::import::ImportGroups_BuiltinsRuntime
```

Expected: 4 spec files pass.

- [ ] **Step 3: Commit**

```bash
git add tests/specs/declarations/import/ImportGroups_BuiltinsRuntime_*.txt
git commit -m "test(imports): coverage for module.builtinsRuntime values"
```

### Task 6.4: Spec coverage for pattern groups and first-match-wins

**Files:**
- Create: `tests/specs/declarations/import/ImportGroups_Patterns.txt`

- [ ] **Step 1: Write spec**

```
~~ lineWidth: 80, module.importGroups: [{"match":"external"},{"match":{"pattern":"@app/**"}},{"match":"parent"}], module.sortImportDeclarations: caseInsensitive ~~
== pattern group positioned after external ==
import { c } from "@app/foo";
import { a } from "react";
import { b } from "../shared";

[expect]
import { a } from "react";

import { c } from "@app/foo";

import { b } from "../shared";

~~ lineWidth: 80, module.importGroups: [{"match":{"pattern":"@app/**"}},{"match":"external"}], module.sortImportDeclarations: caseInsensitive ~~
== pattern group positioned before external ==
import { c } from "@app/foo";
import { a } from "react";

[expect]
import { c } from "@app/foo";

import { a } from "react";
```

- [ ] **Step 2: Run + commit**

```bash
cargo test --test specs declarations::import::ImportGroups_Patterns
git add tests/specs/declarations/import/ImportGroups_Patterns.txt
git commit -m "test(imports): pattern matchers + first-match-wins ordering"
```

### Task 6.5: Spec coverage for side-effect barriers, `// dprint-ignore`, header comments

**Files:**
- Create: `tests/specs/declarations/import/ImportGroups_Barriers.txt`

- [ ] **Step 1: Write tests**

Each test uses the ESLint mirror config. Cover:

- Side-effect import in the middle of a run: imports above & below are
  grouped independently; the side-effect stays put with no reorder around it.
- `// dprint-ignore` on a single import: it stays in source position, acts
  as barrier.
- License header (`/* @license */`) followed by blank line then imports:
  header pinned to file start; imports reordered below it.
- `// @ts-check` shebang-style on first line: preserved.

(Write 4 sub-tests with concrete inputs and expecteds. Use existing test
files for tone/format.)

- [ ] **Step 2: Run + commit**

```bash
cargo test --test specs declarations::import::ImportGroups_Barriers
git add tests/specs/declarations/import/ImportGroups_Barriers.txt
git commit -m "test(imports): side-effect barrier, dprint-ignore, header comment cases"
```

---

## Phase 7: Header-comment pinning

### Task 7.1: Detect detached file-leading comments and pin them

**Files:**
- Modify: `src/generation/generate.rs`

- [ ] **Step 1: Identify how leading comments attach to first import today**

In `gen_statements` (the loop that processes statements), the first node's
leading comments come via `node.leading_comments_fast(context.program)`.
After reorder, if a non-first import becomes first, its leading comments
travel with it — which is what we want for *attached* comments but not for
*detached* file-header comments.

- [ ] **Step 2: Build a "header bag"**

Before the partition step in `get_stmt_groups` (the new code added in
Task 5.3), introduce:

```rust
fn split_header_comments<'a>(
  first_import: &Node<'a>,
  context: &Context<'a>,
) -> Vec<deno_ast::swc::common::comments::Comment> {
  // Take leading comments of the first import. Walk backward from the import
  // start, collecting consecutive comments. A comment is "detached" if there
  // is a blank line between it and the import (i.e., comment.hi_line + 2 <=
  // import.start_line).
  let comments = first_import.leading_comments_fast(context.program);
  let import_start_line = first_import.start_line_fast(context.program);
  let mut detached = Vec::new();
  for c in comments.into_iter() {
    let c_end_line = c.end_line_fast(context.program);
    if c_end_line + 1 < import_start_line {
      detached.push(c.clone());
    }
  }
  detached
}
```

(`end_line_fast` may need adaptation depending on the comment helper API.
Use the existing helpers in `src/generation/comments.rs` for guidance.)

- [ ] **Step 3: Emit detached comments as the very first items of the import block**

Inside the partition branch added in Task 5.3, before reordering nodes:

```rust
      let detached = if let Some(first) = g.nodes.first() {
        split_header_comments(first, context)
      } else { Vec::new() };
      // Stash detached comments in the StmtGroup for emission at index 0.
      g.detached_header_comments = detached;
```

Add a `detached_header_comments: Vec<Comment>` field on `StmtGroup` (use
the same comment type as the SWC `comments::Comment`).

In the emission loop, *before* the first node, emit those detached comments
verbatim and suppress them from the first node's `leading_comments_fast`
result. The suppression can be done by tracking a `HashSet<BytePos>` of
already-emitted comment positions on the `Context` for the duration of the
group emission.

- [ ] **Step 4: Run header-comment spec test from Task 6.5**

```bash
cargo test --test specs declarations::import::ImportGroups_Barriers
```

Expected: now passes.

- [ ] **Step 5: Commit**

```bash
git add src/generation/generate.rs
git commit -m "feat(imports): pin detached file-header comments above first import after reorder"
```

---

## Phase 8: Merge pass (`module.mergeImports: true`)

### Task 8.1: Eligibility check

**Files:**
- Modify: `src/generation/imports/merge.rs`

- [ ] **Step 1: Write a pure eligibility test**

```rust
//! Merge pass for `module.mergeImports: true`.

/// A simplified, pure model of merge eligibility for testing. The real entry
/// point operates on `ImportDecl` nodes; this struct lets us unit-test the
/// rules without an AST.
#[derive(Clone)]
pub struct MergeCandidate {
  pub src: String,
  pub attrs: Option<String>, // canonicalized attribute fingerprint
  pub has_default: bool,
  pub default_name: Option<String>,
  pub has_ignore_comment: bool,
}

pub fn can_merge(a: &MergeCandidate, b: &MergeCandidate) -> bool {
  if a.src != b.src { return false; }
  if a.attrs != b.attrs { return false; }
  if a.has_ignore_comment || b.has_ignore_comment { return false; }
  if a.has_default && b.has_default && a.default_name != b.default_name {
    return false;
  }
  true
}

#[cfg(test)]
mod tests {
  use super::*;

  fn cand(src: &str) -> MergeCandidate {
    MergeCandidate {
      src: src.to_string(),
      attrs: None,
      has_default: false,
      default_name: None,
      has_ignore_comment: false,
    }
  }

  #[test]
  fn same_src_no_default_merges() {
    assert!(can_merge(&cand("./x"), &cand("./x")));
  }

  #[test]
  fn different_src_blocks() {
    assert!(!can_merge(&cand("./x"), &cand("./y")));
  }

  #[test]
  fn conflicting_defaults_block() {
    let a = MergeCandidate { has_default: true, default_name: Some("Foo".into()), ..cand("x") };
    let b = MergeCandidate { has_default: true, default_name: Some("Bar".into()), ..cand("x") };
    assert!(!can_merge(&a, &b));
  }

  #[test]
  fn same_default_merges() {
    let a = MergeCandidate { has_default: true, default_name: Some("Foo".into()), ..cand("x") };
    let b = MergeCandidate { has_default: true, default_name: Some("Foo".into()), ..cand("x") };
    assert!(can_merge(&a, &b));
  }

  #[test]
  fn different_attrs_block() {
    let mut a = cand("x");
    a.attrs = Some("type=json".into());
    let mut b = cand("x");
    b.attrs = Some("type=css".into());
    assert!(!can_merge(&a, &b));
  }

  #[test]
  fn dprint_ignore_blocks() {
    let mut a = cand("x");
    a.has_ignore_comment = true;
    assert!(!can_merge(&a, &cand("x")));
  }
}
```

- [ ] **Step 2: Run**

```bash
cargo test --lib generation::imports::merge
```

Expected: 6 tests pass.

- [ ] **Step 3: Commit**

```bash
git add src/generation/imports/merge.rs
git commit -m "feat(imports): merge eligibility predicate"
```

### Task 8.2: AST-level merge synthesis

**Files:**
- Modify: `src/generation/imports/merge.rs`
- Modify: `src/generation/generate.rs`

This is the most involved task because it produces a new `PrintItems` chunk
representing the merged declaration, since dprint can't mutate the AST.

- [ ] **Step 1: Design the entry point**

In `merge.rs`, add:

```rust
use deno_ast::view::*;
use crate::generation::context::Context;
use dprint_core::formatting::PrintItems;

/// Given a contiguous run of import decls already classified into the same
/// subgroup and sorted by within-group order, return:
///   - a Vec<MergeBucket> where each bucket is either a single decl (unmerged)
///     or a list of decls to be emitted as one merged declaration.
pub enum MergeBucket<'a> {
  Single(&'a ImportDecl<'a>),
  Merged(Vec<&'a ImportDecl<'a>>),
}

pub fn build_buckets<'a>(
  decls: &[&'a ImportDecl<'a>],
  context: &Context<'a>,
) -> Vec<MergeBucket<'a>>;

/// Generate the print items for a merged group of decls. Synthesises a
/// single declaration with the union of specifiers, defaults-first ordering,
/// type markers preserved, and concatenated leading comments.
pub fn gen_merged(
  decls: &[&ImportDecl],
  context: &mut Context,
) -> PrintItems;
```

- [ ] **Step 2: Implement `build_buckets` (pure)**

Walk the slice; for each pair of adjacent decls, call `can_merge` (after
building a `MergeCandidate` from the `ImportDecl`). Extend the current
bucket if eligible, else start a new one.

Add a helper:

```rust
fn candidate_for<'a>(decl: &'a ImportDecl<'a>, context: &Context<'a>) -> MergeCandidate {
  // src
  let src = decl.src.value().to_string();
  // attrs: canonicalize to sorted key=value pairs
  let attrs = decl.with.as_ref().map(|w| {
    let mut pairs: Vec<(String, String)> = w.props.iter().filter_map(|p| {
      // Only ImportAttribute pairs; if SWC view exposes them differently, adapt.
      match p { /* ImportAttribute kv */ _ => None }
    }).collect();
    pairs.sort();
    pairs.into_iter().map(|(k, v)| format!("{k}={v}")).collect::<Vec<_>>().join(",")
  });
  // default specifier
  let default_spec = decl.specifiers.iter().find_map(|s| match s {
    ImportSpecifier::Default(d) => Some(d.local.sym().to_string()),
    _ => None,
  });
  // dprint-ignore
  let has_ignore = context.has_ignore_comment(&decl.range());
  MergeCandidate {
    src,
    attrs,
    has_default: default_spec.is_some(),
    default_name: default_spec,
    has_ignore_comment: has_ignore,
  }
}
```

(`has_ignore_comment` may be a helper on `Context`; see
`src/utils/file_text_has_ignore_comment.rs` for the existing pattern.)

- [ ] **Step 3: Implement `gen_merged`**

```rust
pub fn gen_merged(
  decls: &[&ImportDecl],
  context: &mut Context,
) -> PrintItems {
  // Union of specifiers.
  let mut default_spec: Option<&ImportDefaultSpecifier> = None;
  let mut namespace_spec: Option<&ImportStarAsSpecifier> = None;
  let mut named: Vec<(&ImportNamedSpecifier, bool /* is_type */)> = Vec::new();

  for d in decls {
    for s in d.specifiers.iter() {
      match s {
        ImportSpecifier::Default(x) => {
          if default_spec.is_none() { default_spec = Some(x); }
        }
        ImportSpecifier::Namespace(x) => {
          if namespace_spec.is_none() { namespace_spec = Some(x); }
        }
        ImportSpecifier::Named(x) => {
          let is_type = d.type_only() || x.is_type_only();
          named.push((x, is_type));
        }
      }
    }
  }

  // Sort named per importDeclaration.sortNamedImports.
  // (Reuse the existing helper that emits a named-imports block, but feed
  // it the merged specifier list. The simplest approach is to synthesize a
  // string for the merged declaration here and call back into the existing
  // generator. Concretely: build a textual ImportDecl source, then reparse
  // — but that loses comments. Instead, manually build PrintItems mirroring
  // gen_import_decl's structure with our merged specifier list.)

  // For v1 of merge, implement the simple text-emission path:
  //   - emit leading comments (concatenation from all merged decls).
  //   - emit `import` keyword.
  //   - if any merged decl was fully type-only, decide: if ALL decls are
  //     type-only, emit `import type {...}`; else emit value form and tag
  //     individual specifiers with `type`.
  //   - emit default + namespace + named.
  //   - emit `from "<src>"`.
  //   - emit attrs if any (must all be equal — eligibility ensured this).
  //   - emit semicolon per existing config.

  let all_type_only = decls.iter().all(|d| d.type_only());

  // Reuse the existing helpers from src/generation/generate.rs that emit
  // the keyword + specifier list + `from "src"` for an ImportDecl. The
  // cleanest mechanism is to pick one of the merged decls (the first) as
  // the "host" and call `gen_import_decl(host, context)` after temporarily
  // swapping its specifier list with the merged specifier list.
  //
  // Since the SWC view nodes are immutable, the actual approach is:
  //   1. Build a synthetic source string for the merged declaration:
  //        `import <default>, * as <ns>, { a, type B, c } from "<src>"<attrs>;`
  //      Pick defaults/namespace/named from the union built above.
  //   2. Parse it with deno_ast::parse_module (a single decl).
  //   3. Generate via gen_import_decl over the parsed synthetic node.
  //   4. Prepend concatenated leading comments from the merged decls.
  //
  // Use synthesize_merged_source(decls) and parse_synthetic_decl(src) as
  // helpers to keep gen_merged short. Tests in Task 8.3 verify roundtrip.
  let synth_src = synthesize_merged_source(decls, default_spec, namespace_spec, &named, all_type_only);
  let synth_decl = parse_synthetic_decl(&synth_src, context);
  let mut items = PrintItems::new();
  items.extend(concatenated_leading_comments(decls, context));
  items.extend(crate::generation::generate::gen_import_decl(&synth_decl, context));
  items
}
```

The three helpers (`synthesize_merged_source`, `parse_synthetic_decl`,
`concatenated_leading_comments`) are private to `merge.rs`. Sketch:

```rust
fn synthesize_merged_source(
  decls: &[&ImportDecl],
  default: Option<&ImportDefaultSpecifier>,
  namespace: Option<&ImportStarAsSpecifier>,
  named: &[(&ImportNamedSpecifier, bool)],
  all_type_only: bool,
) -> String {
  let mut out = String::from("import ");
  if all_type_only { out.push_str("type "); }
  let mut parts: Vec<String> = Vec::new();
  if let Some(d) = default { parts.push(d.local.sym().to_string()); }
  if let Some(ns) = namespace { parts.push(format!("* as {}", ns.local.sym())); }
  if !named.is_empty() {
    let inner: Vec<String> = named.iter().map(|(n, is_type)| {
      let prefix = if *is_type && !all_type_only { "type " } else { "" };
      match &n.imported {
        Some(orig) => format!("{prefix}{} as {}", orig.sym(), n.local.sym()),
        None => format!("{prefix}{}", n.local.sym()),
      }
    }).collect();
    parts.push(format!("{{ {} }}", inner.join(", ")));
  }
  out.push_str(&parts.join(", "));
  out.push_str(&format!(" from {:?}", decls[0].src.value()));
  // Attributes: eligibility ensured all equal, so take from first.
  if let Some(attrs_text) = serialize_attrs(decls[0]) {
    out.push_str(&format!(" with {}", attrs_text));
  }
  out.push(';');
  out
}

fn parse_synthetic_decl<'a>(src: &str, context: &Context<'a>) -> ImportDecl<'a> {
  // Use deno_ast::parse_module with the same syntax flags as the host
  // program. Pull the first item, downcast to ImportDecl. The parsed
  // module's lifetime is unrelated to 'a — store it on Context's arena
  // (add a Vec<ParsedSource> field for synthetic merges).
  todo!("see comments in this file for the arena trick")
}
```

The arena trick: add a `synthetic_arena: Vec<deno_ast::ParsedSource>` to
`Context` so synthetic decls live long enough. Push the parsed module into
the arena, return a reference to the parsed `ImportDecl`.

**Acknowledged complexity:** this task is intentionally larger than the
2–5 minute target. Budget ~half a day. If the engineer hits trouble with
the arena/lifetime dance, consider falling back to hand-building
`PrintItems` directly using helpers from `gen_import_decl` for the named
specifier block — but the synthetic-parse path keeps comment and attribute
handling consistent with the rest of dprint-plugin-typescript.

- [ ] **Step 4: Wire `build_buckets` into the partition emission**

In `gen_statements` (the loop), when iterating an Imports group with
`subgroup_boundaries.is_some()` and `context.config.module_merge_imports`,
walk each subgroup's slice through `build_buckets`. For each:
- `Single(d)`: emit via existing `gen_node(*d, context)`.
- `Merged(ds)`: emit via `gen_merged(ds, context)`.

- [ ] **Step 5: Add spec for basic merge**

`tests/specs/declarations/import/ImportGroups_Merge_Basic.txt`:

```
~~ lineWidth: 80, module.importGroups: [{"match":"external"}], module.sortImportDeclarations: maintain, module.mergeImports: true ~~
== merges two imports from same source ==
import { a } from "x";
import { b } from "x";

[expect]
import { a, b } from "x";
```

- [ ] **Step 6: Run**

```bash
cargo test --test specs declarations::import::ImportGroups_Merge
```

Expected: passes for the basic case.

- [ ] **Step 7: Commit**

```bash
git add src/generation/imports/merge.rs src/generation/generate.rs tests/specs/declarations/import/ImportGroups_Merge_Basic.txt
git commit -m "feat(imports): merge multiple imports from same source when enabled"
```

### Task 8.3: Merge edge-case specs

**Files:**
- Create: `tests/specs/declarations/import/ImportGroups_Merge_*.txt` (six files)

- [ ] **Step 1: Write specs for**

1. Side-effect + named → merged to named: `import "./x"; import { a } from "./x";` → `import { a } from "./x";`.
2. Default + namespace → `import x, * as y from "z"`.
3. Value + type-only → `import { a, type B } from "x"`.
4. All type-only → `import type { A, B } from "x"`.
5. Conflicting defaults → both kept, diagnostic in `r.diagnostics`.
6. Different `with { ... }` attrs → both kept.

- [ ] **Step 2: Run + commit**

```bash
cargo test --test specs declarations::import::ImportGroups_Merge
git add tests/specs/declarations/import/ImportGroups_Merge_*.txt
git commit -m "test(imports): merge edge cases (side-effect, type, conflicts, attrs)"
```

---

## Phase 9: Diagnostics & invalid configs

### Task 9.1: Unknown category string diagnostic

**Files:**
- Modify: `src/configuration/resolve_config.rs`

- [ ] **Step 1: Add a test**

In `import_groups_resolution_tests`:

```rust
#[test]
fn unknown_category_string_diagnostic() {
  let r = resolve(serde_json::json!({
    "module.importGroups": [{ "match": "buildin" }]
  }));
  assert!(!r.diagnostics.is_empty());
  assert!(r.diagnostics[0].message.contains("buildin"));
}
```

- [ ] **Step 2: Update `parse_import_groups` to validate string categories**

After serde deserialization succeeds, walk each `ImportMatcher::Category`
value and verify it's a known variant. Since the enum is closed at the
serde level, an unknown variant currently errors out at deserialization —
test it surfaces the variant name in the error message.

Alternatively: relax the enum to `Category(String)` for parsing, and
validate in `compile`.

- [ ] **Step 3: Verify the test passes**

```bash
cargo test --lib import_groups_resolution_tests
```

- [ ] **Step 4: Commit**

```bash
git add src/configuration/resolve_config.rs
git commit -m "feat(config): diagnostic for unknown category strings in module.importGroups"
```

### Task 9.2: `type` listed under `typeImports: interleave` diagnostic

**Files:**
- Modify: `src/generation/imports/resolved.rs`

- [ ] **Step 1: Add test**

```rust
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
```

- [ ] **Step 2: Implement**

In `compile`, when iterating matchers, if `c == BuiltinCategory::Type` and
`config.module_type_imports == TypeImportsMode::Interleave`, push a diagnostic
and skip.

- [ ] **Step 3: Run + commit**

```bash
cargo test --lib generation::imports::resolved
git add src/generation/imports/resolved.rs
git commit -m "feat(imports): diagnostic for \"type\" group under typeImports=interleave"
```

### Task 9.3: Bubble compile diagnostics into resolve_config diagnostics list

**Files:**
- Modify: `src/configuration/resolve_config.rs`
- Modify: `src/generation/generate.rs`

- [ ] **Step 1: Move compile out of `gen_program`**

Move the call to `compile(...)` from `gen_program` (added in Task 5.3) into
`resolve_config` so diagnostics surface through the normal mechanism. Cache
the result on the resolved config (e.g. via a new field
`resolved_import_groups: Option<ResolvedGroups>`).

Adjust `Configuration` to either hold the resolved groups or to be paired
with a `ResolvedConfiguration` wrapper. The simplest path:

- Keep `module_import_groups: Vec<ImportGroup>` on `Configuration` (the
  serialized form).
- Add a non-serialized `#[serde(skip)] pub resolved_import_groups: Option<ResolvedGroups>` on `Configuration`.
- Populate during `resolve_config`.
- Generation reads from `context.config.resolved_import_groups`.

- [ ] **Step 2: Wire diagnostics**

In `parse_import_groups` (or a new function called immediately after), call
`compile` and append any returned diagnostic strings to the
`diagnostics: &mut Vec<ConfigurationDiagnostic>` with property name
`"module.importGroups"`.

- [ ] **Step 3: Run all unit + spec tests**

```bash
cargo test
```

Expected: green.

- [ ] **Step 4: Commit**

```bash
git add src/configuration/resolve_config.rs src/generation/generate.rs
git commit -m "refactor(config): compile import groups during resolve, bubble diagnostics"
```

---

## Phase 10: Remaining spec coverage

### Task 10.1: Catch-all and unknown position

**Files:**
- Create: `tests/specs/declarations/import/ImportGroups_Unknown.txt`

- [ ] **Step 1: Tests**

- Implicit catch-all at end (when no `unknown` listed): unmatched import
  appears in a final group.
- Explicit `unknown` placement: place `{ "match": "unknown" }` at the
  beginning; unmatched imports appear first.

- [ ] **Step 2: Run + commit**

```bash
cargo test --test specs declarations::import::ImportGroups_Unknown
git add tests/specs/declarations/import/ImportGroups_Unknown.txt
git commit -m "test(imports): implicit and explicit unknown group placement"
```

### Task 10.2: Multi-chunk + non-import barrier

**Files:**
- Create: `tests/specs/declarations/import/ImportGroups_MultiChunk.txt`

Cover: imports, then non-import statement, then more imports. Each chunk
grouped independently; no cross-chunk reorder.

- [ ] Run + commit.

### Task 10.3: Import attributes

**Files:**
- Create: `tests/specs/declarations/import/ImportGroups_Attributes.txt`

Cover: `import x from "y" with { type: "json" }`. Classified by path,
attribute preserved. With `mergeImports: true`, two such imports with
different attribute values don't merge.

- [ ] Run + commit.

### Task 10.4: `.d.ts` and `declare module`

**Files:**
- Create: `tests/specs/declarations/import/ImportGroups_DeclarationFiles.txt`

Cover: `.d.ts` file with imports → grouped; imports inside `declare module
"foo" { ... }` → untouched.

- [ ] Run + commit.

### Task 10.5: Interaction with existing knobs

**Files:**
- Create: `tests/specs/declarations/import/ImportGroups_KnobInteractions.txt`

Cover:
- `importDeclaration.forceSingleLine: true` with grouping: declarations
  still single-lined per knob.
- `importDeclaration.sortNamedImports: caseInsensitive` with grouping: each
  decl's specifier list still sorted.
- `module.sortImportDeclarations: maintain` with grouping: within-group
  order preserved; only cross-group reorder happens.

- [ ] Run + commit.

### Task 10.6: Idempotence guarantee

**Files:** none.

- [ ] **Step 1: Confirm `format_twice: true` in spec_test.rs**

`tests/spec_test.rs` already passes `format_twice: true`, so all specs
already verify idempotence. If any spec breaks under second-pass formatting,
treat it as a feature bug: classification must produce the same output on
already-formatted input.

```bash
cargo test --test specs
```

Expected: all pass under double-format.

---

## Phase 11: Documentation

### Task 11.1: README + JSON schema

**Files:**
- Modify: `README.md`
- Modify: `deployment/schema.json` (if it exists)

- [ ] **Step 1: README**

Add a section "Import grouping" with the four config keys, a basic example,
and a migration table from ESLint `import/order`. Keep concise — link to the
design doc for the full spec.

- [ ] **Step 2: schema.json**

If `deployment/schema.json` exists, add entries for:
- `module.importGroups` — array of objects with `match`.
- `module.typeImports` — `"separate" | "interleave"`.
- `module.mergeImports` — boolean.
- `module.builtinsRuntime` — `"node" | "deno" | "bun" | "none"`.

- [ ] **Step 3: Commit**

```bash
git add README.md deployment/schema.json
git commit -m "docs(imports): README and JSON schema for module.importGroups (#493)"
```

---

## Phase 12: Final verification

### Task 12.1: Full test run

- [ ] **Step 1: Run everything**

```bash
cargo test --release
```

Expected: all unit + spec tests pass.

- [ ] **Step 2: Compare against baseline-pre-import-groups for feature-off identity**

```bash
git diff baseline-pre-import-groups -- tests/specs/  # any unintended changes?
```

Expected: only new spec files added under `tests/specs/declarations/import/ImportGroups_*.txt`; no existing specs modified.

- [ ] **Step 3: Clippy clean**

```bash
cargo clippy --all-targets -- -D warnings
```

Expected: no warnings.

- [ ] **Step 4: Format the repo with itself**

```bash
cargo run -- fmt
```

(Or whatever dprint self-format invocation the repo uses — check `.github/`
or `scripts/`.)

- [ ] **Step 5: Push branch + open draft PR**

```bash
git push -u origin feat-import-groups
gh pr create --draft --title "feat: import grouping (#493)" --body "Closes #493. See docs/superpowers/specs/2026-05-21-import-groups-design.md."
```

---

## Out-of-scope reminders (do not implement here)

- CJS `require(...)`, dynamic `import()`, TS `import = require()`.
- Module resolver / tsconfig paths.
- Natural sort, descending sort.
- Imports inside `declare module "..."` bodies (skipped by design).
- Webpack-style alias resolution beyond raw glob.

If any of these come up during implementation, file an issue and move on.
