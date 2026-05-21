# dprint-plugin-typescript

[![](https://img.shields.io/crates/v/dprint-plugin-typescript.svg)](https://crates.io/crates/dprint-plugin-typescript) [![CI](https://github.com/dprint/dprint-plugin-typescript/workflows/CI/badge.svg)](https://github.com/dprint/dprint-plugin-typescript/actions?query=workflow%3ACI)

TypeScript formatting plugin for dprint.

This uses the [swc](https://github.com/swc-project/swc) parser for TypeScript written in Rust (it's super fast).

## Install

See the GitHub [releases](https://github.com/dprint/dprint-plugin-typescript/releases).

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
| `module.mergeImports` | bool | `false` | Merge multiple imports from the same source (Biome-style). Currently detection only; emission TBD. |
| `module.builtinsRuntime` | `"node"` \| `"deno"` \| `"bun"` \| `"none"` | `"node"` | Which runtime's built-in module list classifies as `builtin`. |

### Built-in categories

`builtin`, `external`, `parent`, `sibling`, `index`, `type`, `unknown`.

Use a string in `match` for a single category, or an array to merge multiple categories into one group (no blank line between):

```jsonc
{ "match": ["sibling", "index"] }
```

For pattern-based groups, use a glob:

```jsonc
{ "match": { "pattern": "@app/**" } }
```

First-match-wins across the list, so position determines precedence.

### Migration from ESLint `import/order`

| ESLint option | dprint equivalent |
|---|---|
| `groups` | `module.importGroups` (strings; nested arrays merge) |
| `pathGroups` | `{ "pattern": "..." }` entries placed positionally |
| `newlines-between: "always"` | Default when feature is enabled |
| `newlines-between: "never"`/`"ignore"` | Set `module.importGroups` to `[]` (feature off) |
| `alphabetize.order: "asc"` | Existing `module.sortImportDeclarations` |
| `alphabetize.order: "desc"` | Not supported |

### Limitations

- CommonJS `require(...)` and dynamic `import()` are not reordered.
- Module resolver / tsconfig paths not consulted (raw source string only).
- Descending sort not supported.
- TS `import X = require(...)` not reordered.
- Imports inside nested `declare module "..."` bodies are not classified.
- `module.mergeImports` is currently detection-only; merged emission TBD in a follow-up.
- Currently, an import with `// dprint-ignore` is reordered like any other; barrier treatment is planned for a follow-up.
