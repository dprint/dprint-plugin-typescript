# dprint-plugin-typescript

[![](https://img.shields.io/crates/v/dprint-plugin-typescript.svg)](https://crates.io/crates/dprint-plugin-typescript) [![CI](https://github.com/dprint/dprint-plugin-typescript/workflows/CI/badge.svg)](https://github.com/dprint/dprint-plugin-typescript/actions?query=workflow%3ACI)

TypeScript formatting plugin for dprint.

This uses the [swc](https://github.com/swc-project/swc) parser for TypeScript written in Rust (it's super fast).

## Install

See the GitHub [releases](https://github.com/dprint/dprint-plugin-typescript/releases).

## Development

The tests are in the `./tests/specs` folder. To run the tests, run `cargo test`.

### Concepts

#### `gen_separated_values`

A general function that is both in dprint core and the typescript plugin. It's used for everything from arrays, arguments, type parameters, objects, etc. The general vibe is to use `multiline_options` to control its behaviour from more specific callees like `gen_parameters_or_arguments`.

#### Inline vs not-inline

todo

#### Singleline vs hanging vs multiline

todo

### Tips

1. To use `println` together with `cargo test`, you need to invoke tests in this strange way [ (for reasons) ](https://github.com/rust-lang/cargo/issues/296):

```sh
cargo test -- --nocapture
```

2. Make sure your test files end in a final blank line, otherwise you'll get an error during testing with a diff that appears identical.

### Debugging

Live-step debugging is especially useful for a couple use cases:
1. Seeing what AST nodes are generated from source code
2. Seeing what IR (intermediate representation) is generated from AST nodes
2. Seeing how the printer converts the IR into a formatted file

#### Seeing what AST nodes or IR is generated from source code

Here's how to get debugging the first two use cases (on VSCode):

1. Add a test to a new file `tests/specs/debug.txt` and give it the `(only)` suffix. Note that an empty line is required at the end of the file. For example:

```
// tests/specs/debug.txt
~~ lineWidth: 40, arguments.preferHanging: true ~~
== investigate the node types generated from source code (only) ==
let a = b + c;

[expect]
let a = b + c;

```

2. Install the [rust-analyzer plugin](https://marketplace.visualstudio.com/items?itemName=rust-lang.rust-analyzer).
3. Add a breakpoint somewhere, e.g. in `src/generation/generate.rs` inside the `gen_node_inner` function to see what sort of AST nodes correspond to source code.
4. Head to `tests/test.rs` and click the `Debug` button hovering above the implementation for the `test_specs` function.

And now you can step to your heart's delight!

#### Seeing what output code is printed from IR (and which AST nodes it came from)

tldr:
- use `items.push_info(LineNumber::new("START"))` 

### Building Wasm file

You may wish to try out the plugin by building from source:

1. Run `cargo build --target wasm32-unknown-unknown --release --features "wasm"`
1. Reference the file at `./target/wasm32-unknown-unknown/release/dprint_plugin_typescript.wasm` in a dprint configuration file.
