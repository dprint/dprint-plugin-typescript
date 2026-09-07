# dprint-plugin-typescript

[![](https://img.shields.io/crates/v/dprint-plugin-typescript.svg)](https://crates.io/crates/dprint-plugin-typescript) [![npm version](https://img.shields.io/npm/v/@dprint/typescript.svg)](https://www.npmjs.com/package/@dprint/typescript) [![CI](https://github.com/dprint/dprint-plugin-typescript/workflows/CI/badge.svg)](https://github.com/dprint/dprint-plugin-typescript/actions?query=workflow%3ACI)

TypeScript formatting plugin for dprint.

This uses the [swc](https://github.com/swc-project/swc) parser for TypeScript written in Rust (it's super fast).

## Install

[Install](https://dprint.dev/install/) and [setup](https://dprint.dev/setup/) dprint.

Then in your project's directory with a dprint.json file, run:

```shellsession
dprint add typescript
```

See https://dprint.dev/plugins/typescript/ for more information.

## Development

The tests are in the `./tests/specs` folder. To run the tests, run `cargo test`.

### Building Wasm file

You may wish to try out the plugin by building from source:

1. Run `cargo build --target wasm32-unknown-unknown --release --features "wasm"`
1. Reference the file at `./target/wasm32-unknown-unknown/release/dprint_plugin_typescript.wasm` in a dprint configuration file.

## Import Grouping

This plugin can automatically group import declarations into logical sections separated by blank lines, similar to ESLint's `import/order` rule.

### Quick start

```jsonc
{
  "module.importGroups": [
    { "match": "builtin" },
    { "match": "external" },
    { "match": "parent" },
    { "match": ["sibling", "index"] }
  ]
}
```

This reorders imports across the import block into the listed groups and inserts exactly one blank line between groups.

### Options

| Key | Type | Default | Description |
|---|---|---|---|
| `module.importGroups` | array | `[]` (off) | Ordered list of groups. Empty disables the feature. |
| `module.typeImports` | `"separate"` \| `"interleave"` | `"separate"` | Whether `import type` lines form their own category. |
| `module.builtinsRuntime` | `"node"` \| `"deno"` \| `"bun"` \| `"none"` | `"node"` | Which runtime's built-in module list classifies as `builtin`. |
| `module.importGroupsNewlinesBetween` | `"always"` \| `"alwaysAndInsideGroups"` \| `"never"` \| `"ignore"` | `"always"` | Blank lines between the grouped imports, mirroring ESLint's `newlines-between`. |

### Built-in categories

Categories are matched against the raw specifier string only — no module resolution happens.

| Category | Matches |
|---|---|
| `builtin` | Depends on `module.builtinsRuntime`. `node`: a `node:` prefix or a Node core module name (`fs`, `path/posix`, ...). `deno`: a `node:` prefix only. `bun`: either of those plus a `bun:` prefix. `none`: nothing. |
| `parent` | `..`, or anything starting with `../`. |
| `index` | `.`, `./`, or `./index` with an optional `.ts`/`.tsx`/`.js`/`.jsx`/`.mjs`/`.cjs`/`.mts`/`.cts` extension. |
| `sibling` | Anything else starting with `./`. |
| `type` | An `import type` declaration, when `module.typeImports` is `"separate"`. Takes precedence over the path-based categories above. |
| `external` | Everything left over. |
| `unknown` | Nothing — it is the bucket for imports no listed group claims. See below. |

Because `external` is the fallthrough, it also claims specifiers that other tools categorize separately: Deno's `npm:`, `jsr:` and `https://` specifiers, and Node subpath imports like `#internal/foo`. There is no `internal` category; use a pattern group for those.

`unknown` only matters when you leave a category out of your list. Imports that match no group land in the `unknown` group, which is appended at the end unless you place `{ "match": "unknown" }` somewhere yourself.

Use a string in `match` for a single category, or an array to merge multiple categories into one group (no blank line between):

```jsonc
{ "match": ["sibling", "index"] }
```

For pattern-based groups, use a glob:

```jsonc
{ "match": { "pattern": "@app/**" } }
```

First-match-wins across the list, so position determines precedence.

Patterns are [globset](https://docs.rs/globset) globs, **not** minimatch. The difference that matters in practice: a single `*` crosses `/`, so `@app/*` also matches `@app/deep/thing`. Write `@app/*/` style patterns only if you have verified the behavior you want; when in doubt use `**`.

### What counts as one import block

Grouping happens within a contiguous run of import declarations. A run ends at:

- any non-import statement,
- a side-effect import (`import "./polyfill"`), which is left in place because its position is usually load-order significant,
- an import preceded by a `// dprint-ignore` comment (see below),
- **a comment on its own line between two imports.**

Each run is grouped and reordered independently, so imports never move across one of these. The last one is easy to trip over — a stray `// ...` line in the middle of an import block splits it in two, and each half is grouped separately.

Blank lines do **not** end a run: like ESLint, the formatter treats them as style rather than structure, merges across them, and `module.importGroupsNewlinesBetween` decides what the output looks like:

- `"always"` (default) — exactly one blank line between groups, none inside a group.
- `"alwaysAndInsideGroups"` — blank lines between groups, and blank lines inside a group are kept.
- `"never"` — no blank lines anywhere in the import block.
- `"ignore"` — blank lines are neither added nor removed.

Under `"alwaysAndInsideGroups"` and `"ignore"`, a source blank line survives only when the two imports around it are still next to each other after reordering.

### Pinning an import

A `// dprint-ignore` comment on an import pins it: it stays at its position and ends the run around it, so the imports before it and the imports after it are each grouped on their own.

```ts
// dprint-ignore
import { keepMeFirst } from "./bootstrap";
import { b } from "bbb";
import { a } from "aaa";
```

The pinned import does not move, and `aaa`/`bbb` still group and sort normally. This applies to `module.sortImportDeclarations` and `module.sortExportDeclarations` too, not only to grouping.

`// dprint-ignore-start` and `// dprint-ignore-end` are matched by the same check, so they pin whichever declaration follows them. This plugin has no notion of an ignored *region*, though — the declarations between the two markers are not otherwise protected.

### Migration from ESLint `import/order`

| ESLint option | dprint equivalent |
|---|---|
| `groups` | `module.importGroups` (strings; nested arrays merge) |
| `pathGroups` | `{ "pattern": "..." }` entries placed positionally — but see the glob note above, the pattern syntax is not minimatch |
| `newlines-between` | `module.importGroupsNewlinesBetween` — same four values, camelCased (`"always-and-inside-groups"` is `"alwaysAndInsideGroups"`) |
| `alphabetize.order: "asc"` | Existing `module.sortImportDeclarations` |
| `alphabetize.order: "desc"` | Not supported |
| `groups: ["internal"]` | No equivalent category; use a `{ "pattern": "..." }` group |
| `// eslint-disable-next-line import/order` | `// dprint-ignore` on the import |

### Limitations

- CommonJS `require(...)` and dynamic `import()` are not reordered.
- Module resolver / tsconfig paths not consulted (raw source string only).
- Descending sort not supported.
- TS `import X = require(...)` not reordered.
- Imports inside nested `declare module "..."` bodies are not classified.
- `export ... from "..."` declarations are never grouped. `module.sortExportDeclarations` still applies to them.
- There is no ignored-*region* concept, so `// dprint-ignore-start` / `// dprint-ignore-end` only pins the declaration directly after each marker rather than everything between them.
