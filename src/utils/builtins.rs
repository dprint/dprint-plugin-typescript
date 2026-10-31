//! Built-in module classification.
//!
//! The Node core list is a snapshot of `module.builtinModules` from Node 22 LTS,
//! excluding the internal modules that start with an underscore.

use crate::configuration::BuiltinsRuntime;

/// Returns true if `src` (the bare specifier string, without surrounding
/// quotes) is a built-in module under the given runtime.
pub fn is_builtin(src: &str, runtime: BuiltinsRuntime) -> bool {
  match runtime {
    BuiltinsRuntime::Node => has_node_prefix(src) || is_node_core_module(src),
    BuiltinsRuntime::Deno => has_node_prefix(src),
    BuiltinsRuntime::Bun => has_node_prefix(src) || is_bun_module(src) || is_node_core_module(src),
    BuiltinsRuntime::None => false,
  }
}

fn has_node_prefix(src: &str) -> bool {
  src.starts_with("node:")
}

fn is_bun_module(src: &str) -> bool {
  src == "bun" || src.starts_with("bun:")
}

fn is_node_core_module(src: &str) -> bool {
  NODE_CORE_MODULES.binary_search(&src).is_ok()
}

/// Node 22 LTS `module.builtinModules` snapshot (no `node:` prefix), sorted so
/// it can be binary searched. Modules that can only be imported with the `node:`
/// prefix (ex. `node:test`) are covered by the prefix check instead.
static NODE_CORE_MODULES: &[&str] = &[
  "assert",
  "assert/strict",
  "async_hooks",
  "buffer",
  "child_process",
  "cluster",
  "console",
  "constants",
  "crypto",
  "dgram",
  "diagnostics_channel",
  "dns",
  "dns/promises",
  "domain",
  "events",
  "fs",
  "fs/promises",
  "http",
  "http2",
  "https",
  "inspector",
  "inspector/promises",
  "module",
  "net",
  "os",
  "path",
  "path/posix",
  "path/win32",
  "perf_hooks",
  "process",
  "punycode",
  "querystring",
  "readline",
  "readline/promises",
  "repl",
  "stream",
  "stream/consumers",
  "stream/promises",
  "stream/web",
  "string_decoder",
  "sys",
  "timers",
  "timers/promises",
  "tls",
  "trace_events",
  "tty",
  "url",
  "util",
  "util/types",
  "v8",
  "vm",
  "wasi",
  "worker_threads",
  "zlib",
];

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
    // only available with the prefix
    assert!(!is_builtin("test", BuiltinsRuntime::Node));
    assert!(is_builtin("node:test", BuiltinsRuntime::Node));
    assert!(!is_builtin("bun", BuiltinsRuntime::Node));
  }

  #[test]
  fn node_core_modules_sorted() {
    let mut sorted = NODE_CORE_MODULES.to_vec();
    sorted.sort_unstable();
    assert_eq!(sorted, NODE_CORE_MODULES);
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
    assert!(is_builtin("bun", BuiltinsRuntime::Bun));
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
