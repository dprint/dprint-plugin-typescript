# dprint-plugin-typescript

[![](https://img.shields.io/crates/v/dprint-plugin-typescript.svg)](https://crates.io/crates/dprint-plugin-typescript) [![CI](https://github.com/dprint/dprint-plugin-typescript/workflows/CI/badge.svg)](https://github.com/dprint/dprint-plugin-typescript/actions?query=workflow%3ACI)

TypeScript formatting plugin for dprint.

This uses the [swc](https://github.com/swc-project/swc) parser for TypeScript written in Rust (it's super fast).

## Install

See the GitHub [releases](https://github.com/dprint/dprint-plugin-typescript/releases).

## Development

The tests are in the `./tests/specs` folder. To run the tests, run `cargo test`.

### Concepts

#### Formatting modes

Formatting is ultimately just a game of taking source code and printing it with the best combinations of whitespace (spaces, tabs, newlines, indentations) and separators (commas, semicolons, pipes, etc.). How do we decide what sort of combinations are allowed? What happens if we try one combination but it's not good enough (e.g. it exceeds the line width)?

In dprint, we specify all the possible whitespace combinations on a per-node basis. Some nodes are always printed on the same line.

```typescript
const myArray = [1,2,3];
```

In the above example:
- `const myArray = [1,2,3];` is a `VarDecl`. It can be on the same line or multiple lines.
- `myArray` is an `Ident`. It is always on the same line and can't be broken up.
- `[1,2,3]` is an `ArrayLit`. It can be on the same line or multiple lines.

What's more, there are multiple ways to format some nodes on multiple lines.

```typescript
// VarDecl is in inline mode
const myArray = [1,2,3];

// VarDecl is in hanging mode
const myArray =
    [1,2,3];

// ArrayLit is in hanging mode
const myArray = [1,2,
    3];

// ArrayLit is in multiline mode
const myArray = [
    1,
    2,
    3,
];
```

There are three different modes above:
1. A node is **inline** or **single-line** if the whole expression is printed on the same line. Otherwise, it's spread over **multiple lines**; this is not (always) the same thing as being multi-line, since it could be hanging instead.
2. A node is **hanging** or **wrapping** if it can be broken up by breaking up one of its subnodes. For example, the `ArrayLit` node is a subnode of the `VarDecl` node.
3. A node is **multi-line** if it is broken up over multiple lines in a conservative manner. A node formatted in multi-line typically takes up more lines than if it were inline or hanging.

#### Separated values

Sometimes we handle multiple types of AST nodes in a similar way. In Typescript, a parsed node is a **separated values node** if it is:
- an object or object destructure
- an object type, or interface type with a body `{ ... }`
- an array or array destructure
- arguments (in a function call) or parameters (in a function definition)

We format these nodes similarly because they all
- containg a list of values
- are separated by non-whitespace (either a comma or semicolon)
- make sense to format in singleline, hanging and multiline modes

`gen_separated_values` is a general function that handles separated values. It is both in `dprint-plugin-typescript` but also in `dprint-core`. It's used for everything from arrays, arguments, type parameters, objects, etc. The general vibe is to use options like `multiline_options` to control its behaviour from more specific callees like `gen_parameters_or_arguments` or `gen_object_lit`.

```typescript
// object 
type Opts = { type: 'warning' | 'error'; source: string }; // object type
function log(messages: string[], opts: Opts) // parameters
{
    //...
}
log("an error message", {type: "error", source: "README.md"}); // arguments containing an object
```


### Tips

1. To use `println` together with `cargo test`, you need to invoke tests in this strange way [ (for reasons) ](https://github.com/rust-lang/cargo/issues/296):

```sh
cargo test -- --nocapture
```

2. Make sure your test files end in a final blank line, otherwise you'll get an error during testing with a diff that appears identical.

### Debugging: spec files

If you want to test the plugin's behaviour on a particular snippet of source code and a particular config, you can create any `.txt` file under the `tests/specs/` folder tree. For example, to test the `arguments.preferHanging` config, add a test to a new file `tests/specs/debug.txt`. We give it the `(only)` suffix so that no other tests run (remember to remove this later!).

```
// tests/specs/debug.txt
~~ lineWidth: 40, arguments.preferHanging: true ~~
== my test source code snippet ==
bake(['flour', 'butter', 'sugar', 'drinking chocolate'], '200ºC', '10m');

[expect]
bake(['flour', 'butter', 'sugar',
    'drinking chocolate'], '200ºC', '10m');

```

Then, run tests like so:

```sh
cargo test
```

If you're using `println` or creating output inside the source code, you'll need to run tests like this to see the output on the command line [(because reasons)](https://github.com/rust-lang/cargo/issues/296):

```sh
cargo test -- --nocapture
```

### Debugging: live step

Live-step debugging is especially useful for a couple use cases:
1. Seeing what AST nodes are generated from source code
2. Seeing what IR (intermediate representation) is generated from AST nodes
2. Seeing how the printer converts the IR into a formatted file

#### Seeing what AST nodes or IR is generated from source code

Here's how to get debugging the first two use cases (on VSCode):

1. Add a test to a new file `tests/specs/debug.txt` and give it the `(only)` suffix. Note that an empty line is required at the end of the file. For example:

```
// tests/specs/debug.txt
~~ lineWidth: 40 ~~
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

1. In the generation logic for your node, place code like this:

```rust
items.push_info(LineNumber::new("EASY TO SPOT MESSAGE"));
```

2. Add the `(trace) (only)` suffix to your test.
3. Then run:

```sh
cargo test --features tracing -- --nocapture
```

You should see an output file like:

```
Trace output ready! Please open your browser to: file:///var/folders/some_gibberish_blablabla/dprint-core-trace.html
```

Open that file and step through the generation to locate your `EASY TO SPOT MESSAGE`. Since the IR is heaps long, you can also use the 'Inspect element' tool and then search the contents of the HTML for `EASY TO SPOT MESSAGE` instead.

### Building Wasm file

You may wish to try out the plugin on some real code by building from source:

1. Run `cargo build --target wasm32-unknown-unknown --release --features "wasm"`
1. Reference the file at `./target/wasm32-unknown-unknown/release/dprint_plugin_typescript.wasm` in a dprint configuration file.
