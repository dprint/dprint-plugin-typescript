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
