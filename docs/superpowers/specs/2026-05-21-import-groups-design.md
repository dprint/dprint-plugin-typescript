# Import Grouping (issue #493)

Date: 2026-05-21
Tracking: https://github.com/dprint/dprint-plugin-typescript/issues/493

## Goal

Add ESLint-`import/order`-style import grouping to dprint-plugin-typescript.
dprint will classify every ES import declaration, reorder them across the
import block to match a user-declared group order, and insert exactly one
blank line between groups. Eliminates the need for `eslint-plugin-import`'s
`order` rule for users on dprint.

## Non-goals

- CommonJS `require(...)` ordering.
- Dynamic `import()` expressions.
- Webpack/TS-resolver-based classification (no module resolution performed).
- `eslint-plugin-import` options without a clean dprint analog:
  `warnOnUnassignedImports`, `consolidateIslands`, `pathGroupsExcludedImportTypes`,
  descending alphabetize, `newlines-between: "never" | "ignore" | "always-and-inside-groups"`.
- TypeScript `import X = require(...)` / `export X = ...`.
- Re-exports `export ... from "..."` (handled by the existing `Exports` group;
  not regrouped by this feature).
- **Natural sort** of import sources. Existing `SortOrder` (lexicographic,
  case-sensitive/-insensitive) only.
- Classifying imports inside nested `declare module "..."` bodies. Only the
  top-level statement list of a program is partitioned.

## Configuration

```jsonc
{
  // Ordered list of groups. Empty/absent = feature off; existing behavior preserved.
  "module.importGroups": [
    { "match": "builtin" },
    { "match": "external" },
    { "match": "parent" },
    { "match": ["sibling", "index"] }
  ],

  // How type-only imports are classified.
  //   "separate"  (default): a distinct implicit category "type"; user places it in the list.
  //   "interleave"         : classified by source path the same as value imports.
  "module.typeImports": "separate",

  // Merge multiple imports from the same source into one declaration.
  //   false (default) : leave as written (matches ESLint import/order).
  //   true            : merge compatible duplicates (matches Biome organizeImports).
  "module.mergeImports": false
}
```

### `match` value forms

- String: one of `"builtin" | "external" | "parent" | "sibling" | "index" | "type" | "unknown"`.
- Array: union of strings and/or pattern objects, merged into one group (no blank line between).
- Pattern object: `{ "pattern": "<glob>" }` — matched against the import source literal (no resolution).
- Arrays may mix: `["external", { "pattern": "@app/**" }]`.

### Built-in categories

| Category    | Match condition                                                                 |
|-------------|---------------------------------------------------------------------------------|
| `builtin`   | source starts with `node:` OR is in hardcoded Node core list                    |
| `external`  | bare specifier not matched as builtin (e.g. `react`, `@scope/pkg`)              |
| `parent`    | source starts with `../`                                                        |
| `sibling`   | source starts with `./` and is not an index path                                |
| `index`     | source is `.`, `./`, `./index`, or `./index.{ts,tsx,js,jsx,mjs,cjs,mts,cts}`    |
| `type`      | `import type` / `export type` declaration, only when `typeImports: "separate"`  |
| `unknown`   | implicit catch-all; placed at end if not listed; user may insert explicitly     |

### Resolution & precedence

- Parsed once in `resolve_config` into `Vec<ResolvedGroup>` (`ResolvedGroup` =
  `Vec<Matcher>`, `Matcher` = `Category(BuiltinCategory) | Pattern(GlobPattern)`).
- Append implicit `unknown` group if the user did not list one.
- First-match-wins across the resolved list (positional precedence replaces
  ESLint `pathGroupsExcludedImportTypes`).
- Diagnostic (warning, first occurrence wins) when the same category appears twice.
- Glob via `globset` crate.

### Defaults & opt-in

- Default `module.importGroups` is empty → feature off → byte-identical output
  vs. the previous version on the full existing spec suite.
- Default `module.typeImports` is `"separate"` but only takes effect when the
  feature is enabled.

## Approach (locked)

Approach **A** — subgrouping inside the existing `StmtGroup::Imports` run.

Rationale:

- `get_stmt_groups` in `src/generation/generate.rs` already groups consecutive
  import declarations into a single `StmtGroup` and excludes side-effect
  imports (`!decl.specifiers.is_empty()` filter), so side-effect imports
  already act as positional barriers.
- Adding a classify+partition step inside that branch reuses all existing
  blank-line, comment, and sort machinery.
- No new global passes; no synthetic-node mechanism needed.

## Algorithm

```text
classify(import_decl, config) -> usize  // index into resolved groups
  src     = import_decl.src.value
  is_type = import_decl.type_only
            || (typeImports == "separate" && every specifier is `type`)

  category =
    if is_type && typeImports == "separate":            "type"
    elif src.starts_with("node:"):                       "builtin"
    elif NODE_CORE_LIST.contains(src):                   "builtin"
    elif src.starts_with("../"):                         "parent"
    elif src is "." || "./" || "./index"
         || matches "./index.{ts,tsx,js,jsx,mjs,cjs,mts,cts}":
                                                         "index"
    elif src.starts_with("./"):                          "sibling"
    else:                                                "external"

  // Walk resolved group list in order; return first index where any matcher
  // is `Category(category)` or `Pattern(glob)` matching src.
  // If none match: index of `unknown` group (implicit end if absent).
```

Within each non-empty partition apply
`get_node_sorter_from_order(module_sort_import_declarations, NamedTypeImportsExportsOrder::None)`
(existing helper).

### Merge pass (when `module.mergeImports = true`)

Runs once per subgroup, after the within-group sort. Walks consecutive
declarations; merges runs sharing the same `(source, attributes)` key.

**Merge eligibility** — two adjacent decls `A` and `B` may merge iff all hold:

- `A.src.value == B.src.value` (string equality).
- Their import-attributes clauses (`with { ... }`) are structurally equal
  (same keys, same string values, same order is not required).
- They do not both declare a default specifier with different local names
  (e.g. `import x from "y"` + `import z from "y"` — two defaults, conflict).
- Neither carries `// dprint-ignore`.

**Merge result:**

- Specifier set = union of all specifiers from merged decls.
- Specifier order = defaults first, then namespace, then named (sorted by
  existing `importDeclaration.sortNamedImports`).
- Type-only mixing: if at least one merged decl is value and at least one is
  `import type` (or has per-specifier `type` markers), result is a value
  declaration with `type` markers preserved per specifier
  (`import { a, type B } from "x"`). If **all** merged decls are
  `import type`, result is `import type { ... } from "x"`.
- Leading comments: concatenated in source order above the merged decl, with
  blank lines between author-separated blocks preserved.
- Trailing same-line comment: only one allowed; if multiple, keep the first
  and emit the rest as preceding line comments of the merged decl, source
  order preserved.
- Side-effect import (`import "./x"`) followed by named import from same
  source: merged to the named import (named import already triggers eval).

**Skip cases** (no merge, original decls kept; emit info diagnostic):

- Two default specifiers with different local names.
- Different attribute clauses.
- Either decl has `// dprint-ignore`.

## Emission

Touch points (`src/generation/generate.rs`, ~7300–7470):

1. Extend `StmtGroup`:
   ```rust
   struct StmtGroup<'a> {
     kind: StmtGroupKind,
     nodes: Vec<Node<'a>>,
     subgroup_boundaries: Option<Vec<usize>>, // indices into `nodes` where a new subgroup starts
   }
   ```
   `subgroup_boundaries` is `None` unless `kind == Imports && !config.import_groups.is_empty()`.

2. Add `partition_import_group(nodes, context) -> (Vec<Node>, Vec<usize>)`:
   - `classify` each node → `(group_idx, node)`.
   - Stable-partition by `group_idx` preserving original index for ties.
   - Apply the existing node sorter within each partition.
   - Concatenate in resolved-group order, recording boundary indices for non-empty partitions.

3. `get_stmt_groups` calls `partition_import_group` when the group is an
   `Imports` group and the feature is enabled; replaces `nodes` and sets
   `subgroup_boundaries`.

4. The `should_use_blank_line` predicate for an `Imports` group:
   - Same subgroup → existing behavior (no forced blank line).
   - Straddles a boundary → force exactly one blank line.
   - Author-written blank lines inside a subgroup are normalized away (the
     reorder makes preserving them meaningless).

## Edge cases

- **Empty config / feature off**: existing behavior preserved (regression test).
- **All imports one category**: no blank lines inserted (single non-empty subgroup).
- **Side-effect imports in the middle of imports**: already split the
  `StmtGroupKind::Imports` run; each side classified independently; positions
  preserved. (Matches `import/order` default behavior for side effects.)
- **`// dprint-ignore` on an import**: classification skipped for that node; it
  acts as a barrier (preserves position, splits the run).
- **Author-written blank lines inside an import run, feature ON**: ignored;
  blank lines are driven by group boundaries. Documented as a behavior change.
- **`import * as X from "..."`**: classified by source like any other import.
- **`import Foo, { type Bar } from "..."`**: `decl.type_only` is false →
  classified as value. Only fully `import type` lines hit the `type` category.
- **Glob matches multiple groups**: first listed wins.
- **Category listed twice**: diagnostic, first occurrence used.
- **`unknown` listed explicitly**: that position is used instead of implicit end.
- **`"type"` listed under `typeImports: "interleave"`**: diagnostic; the
  category never matches anything in this mode and is ignored.
- **Unknown category string** (e.g. `"buildin"` typo): config-resolve
  diagnostic; entry ignored.
- **File header comments** (license, `// @ts-check`, shebang) above the first
  import: detect "detached" leading comments — comments separated from the
  first import by at least one blank line — and pin them to the file start.
  Only comments adjacent (no blank line) to an import travel with that import
  during reorder.
- **Import attributes** (`import x from "y" with { type: "json" }`):
  classification reads only `decl.src.value`; attributes are not part of the
  category decision. Decls are reordered intact — attribute clauses pass
  through verbatim. Attributes do participate in the merge eligibility check
  when `mergeImports = true` (two decls with non-equal attribute clauses are
  never merged even if their sources match).
- **Multiple import chunks separated by non-import statements**: each chunk
  grouped independently (existing `get_stmt_groups` chunk boundary). No
  cross-chunk reorder.
- **`.d.ts` declaration files**: same code path; no special handling.
- **Imports inside `declare module "..."`**: not classified; nested module
  bodies are skipped (top-level program only).
- **TS `import equals`**: not in the current `Imports` `StmtGroupKind`;
  unaffected. Out of scope.
- **`export ... from`**: handled by the `Exports` `StmtGroupKind`; unaffected.

## Interaction with existing knobs

| Knob                                              | Interaction                                          |
|---------------------------------------------------|-------------------------------------------------------|
| `module.sortImportDeclarations`                   | Within-group sort. `Maintain` keeps source order.    |
| `module.sortExportDeclarations`                   | Unchanged (exports unaffected).                      |
| `importDeclaration.sortNamedImports`              | Unchanged. Specifier sort still applies.             |
| `importDeclaration.sortTypeOnlyImports`           | Unchanged.                                           |
| `importDeclaration.forceSingleLine` / `preferHanging` / `preferSingleLine` | Orthogonal. Apply per-decl after reorder. |

## Performance

One classification call per import: string-prefix checks + `NODE_CORE_LIST`
lookup + globset match. Linear in number of imports; negligible vs. full
pretty-print.

## Testing

Specs live in `tests/specs/modules/imports/ImportGroups_*.txt` using the
existing dprint spec test format (input → expected, per-spec config).

### Coverage matrix

| # | Scenario |
|---|---|
| 1 | Feature off (empty/absent config) — identity on a mixed import block |
| 2 | ESLint mirror `[builtin, external, parent, [sibling, index]]` — reorders, inserts blanks, collapses extras |
| 3 | Single populated category — no blank lines |
| 4 | All imports unmatched — catch-all at end |
| 5 | Explicit `unknown` placement |
| 6 | `node:` prefix and bare core (`fs`) both classified `builtin` |
| 7 | Non-core bare (`react`) → `external` |
| 8 | Pattern glob `@app/**` first-match-wins |
| 9 | Category appearing twice — diagnostic + first wins |
| 10 | `typeImports: "separate"` pulls `import type` into `type` group |
| 11 | `typeImports: "interleave"` mixes `import type` with value by path |
| 12 | Mixed default+type specifier stays value |
| 13 | Side-effect import barrier — each side classified independently |
| 14 | Author-written blank lines normalized to group boundaries when feature on |
| 15 | Leading comments follow their node across reorder |
| 16 | `// dprint-ignore` import excluded and acts as barrier |
| 17 | `module.sortImportDeclarations = Maintain` — cross-group reorder, intra preserves source |
| 18 | `module.sortImportDeclarations = CaseInsensitive` — alphabetical within each group |
| 19 | TS `import equals` unaffected |
| 20 | `export ... from` unaffected |
| 21 | Reverse default order |
| 22 | Pattern group between named groups |
| 23 | Pattern group merged with named via nested array vs separate (distinctGroup analog) |
| 24 | Scoped package `@scope/pkg` → external |
| 25 | Resolver alias `@/foo` via pattern |
| 26 | `react` vs `react-dom` ordering under each `SortOrder` |
| 27 | Multi-line `import { a, b, c } from "..."` straddling a group boundary |
| 28 | Unassigned (side-effect) import between two value imports of different groups |
| 29 | First-match-wins when an import matches two pattern groups |
| 30 | Comments between two imports of different groups |
| 31 | Trailing comment on last import of a group placed correctly with blank line |
| 32 | Mixed `import` and `import type` from the same source path (both modes) |
| 33 | File with a single import — no-op |
| 34 | Interaction with `importDeclaration.forceSingleLine` (width orthogonal) |
| 35 | Interaction with `importDeclaration.sortNamedImports` (specifier sort still applies) |
| 36 | License header comment above first import stays pinned to file start after reorder |
| 37 | `// @ts-check` / shebang preservation |
| 38 | Comment directly adjacent to an import (no blank line) travels with it |
| 39 | Import attributes `import x from "y" with { type: "json" }` classification + passthrough |
| 40 | Multiple import chunks separated by a non-import statement — each chunk grouped independently |
| 41 | `.d.ts` declaration file — same behavior |
| 42 | Imports inside `declare module "..."` body — untouched |
| 43 | Unknown category string in config (typo) — diagnostic, entry ignored |
| 44 | `typeImports: "interleave"` with `"type"` listed — diagnostic, ignored |
| 45 | Duplicate-source imports with `mergeImports: false` (default) — left as-is |
| 46 | `mergeImports: true` — basic merge of `import {a} from "x"; import {b} from "x"` → `import {a, b} from "x"` |
| 47 | `mergeImports: true` — side-effect + named from same source merge to named |
| 48 | `mergeImports: true` — default + namespace from same source merge to `import x, * as y from "z"` |
| 49 | `mergeImports: true` — value + `import type` merge with per-specifier `type` markers |
| 50 | `mergeImports: true` — all-`import type` decls merge to single `import type {...}` |
| 51 | `mergeImports: true` — two conflicting defaults left unmerged + diagnostic |
| 52 | `mergeImports: true` — different `with { ... }` attributes left unmerged |
| 53 | `mergeImports: true` — `// dprint-ignore` on either decl prevents merge |
| 54 | `mergeImports: true` — comments on merged decls preserved above result |
| 55 | `mergeImports: true` interaction with `importDeclaration.sortNamedImports` — merged specifier list sorted |

### Unit tests (`#[cfg(test)]`)

- `classify` table tests over `(src, is_type, typeImports_mode) → category`.
- `node_builtins::is_node_builtin(name)` known + unknown cases.
- Config resolution: invalid `match` shapes → diagnostics with location.

### Snapshot stability

Full existing spec suite must produce zero diff with the feature disabled.

## Files touched (estimate)

- `src/configuration/types.rs` — add `ImportGroup`, `ImportMatcher`, `TypeImportsMode`,
  config fields.
- `src/configuration/builder.rs` — builder methods + defaults.
- `src/configuration/resolve_config.rs` — parse + validate `module.importGroups`,
  `module.typeImports`, `module.mergeImports`; emit diagnostics.
- `src/generation/generate.rs` — extend `StmtGroup`, add
  `partition_import_group`, classifier, blank-line predicate update.
- `src/utils/node_builtins.rs` — new file: Node core list + helper.
- `tests/specs/modules/imports/ImportGroups_*.txt` — new spec files.

## Migration notes for ESLint users

| ESLint `import/order` option        | dprint equivalent                                          |
|--------------------------------------|------------------------------------------------------------|
| `groups`                             | `module.importGroups` (string entries; nested arrays merge) |
| `pathGroups`                         | `{ pattern: "..." }` entries placed positionally in `importGroups` |
| `pathGroupsExcludedImportTypes`      | Not applicable — list order is precedence                   |
| `newlines-between: "always"`         | Default behavior when feature enabled                       |
| `newlines-between: "never"/"ignore"` | Set `module.importGroups` to empty (feature off)            |
| `alphabetize.order: "asc"`           | `module.sortImportDeclarations` = `CaseInsensitive` / `CaseSensitive` |
| `alphabetize.order: "desc"`          | Not supported                                               |
| `distinctGroup` (default true)       | Default; flatten by nesting array entries to merge          |
| `warnOnUnassignedImports`            | Not supported (dprint is a formatter, not a linter)         |
| `consolidateIslands`                 | Not supported                                               |
