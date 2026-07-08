# dprint-plugin-typescript

[![](https://img.shields.io/crates/v/dprint-plugin-typescript.svg)](https://crates.io/crates/dprint-plugin-typescript) [![CI](https://github.com/dprint/dprint-plugin-typescript/workflows/CI/badge.svg)](https://github.com/dprint/dprint-plugin-typescript/actions?query=workflow%3ACI)

TypeScript formatting plugin for dprint.

This uses the [swc](https://github.com/swc-project/swc) parser for TypeScript written in Rust (it's super fast).

## Install

[Install](https://dprint.dev/install/) and [setup](https://dprint.dev/setup/) dprint.

Then in your project's directory with a dprint.json file, run:

```shellsession
dprint add typescript
# or install from npm
dprint add npm:@dprint/typescript
```

See https://dprint.dev/plugins/typescript/ for more information.

## Configuration

Set `jsx.sortClassNames` to `tailwind` to sort JSX `class` and `className` values using Tailwind CSS ordering.
Helper calls and tagged templates are sorted only when their leading identifier is listed in `jsx.sortClassNames.functions`.

```json
{
  "typescript": {
    "jsx.sortClassNames": "tailwind",
    "jsx.sortClassNames.functions": ["cn", "classnames", "tw"]
  }
}
```

## Development

The tests are in the `./tests/specs` folder. To run the tests, run `cargo test`.

### Building Wasm file

You may wish to try out the plugin by building from source:

1. Run `cargo build --target wasm32-unknown-unknown --release --features "wasm"`
1. Reference the file at `./target/wasm32-unknown-unknown/release/dprint_plugin_typescript.wasm` in a dprint configuration file.
