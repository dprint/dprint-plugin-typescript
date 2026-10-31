//! Built-in module classification.
//!
//! The Node core list is a snapshot of `module.builtinModules` from Node 26.

/// Returns true if `src` (the bare specifier string, without surrounding
/// quotes) is a built-in module of Node, Deno, Bun, or Cloudflare Workers.
pub fn is_builtin(src: &str) -> bool {
  src.starts_with("node:") || src == "bun" || src.starts_with("bun:") || src.starts_with("cloudflare:") || is_node_core_module(src)
}

fn is_node_core_module(src: &str) -> bool {
  NODE_CORE_MODULES.binary_search(&src).is_ok()
}

/// Node 26 `module.builtinModules` snapshot (no `node:` prefix), sorted so
/// it can be binary searched. Modules that can only be imported with the `node:`
/// prefix (ex. `node:test`) are covered by the prefix check instead.
static NODE_CORE_MODULES: &[&str] = &[
  "_http_agent",
  "_http_client",
  "_http_common",
  "_http_incoming",
  "_http_outgoing",
  "_http_server",
  "_tls_common",
  "_tls_wrap",
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
  fn recognizes_prefixes() {
    assert!(is_builtin("node:fs"));
    assert!(is_builtin("node:path/posix"));
    assert!(is_builtin("node:test"));
    assert!(is_builtin("bun"));
    assert!(is_builtin("bun:test"));
    assert!(is_builtin("bun:sqlite"));
    assert!(is_builtin("cloudflare:workers"));
    assert!(is_builtin("cloudflare:sockets"));
  }

  #[test]
  fn recognizes_bare_node_core_modules() {
    assert!(is_builtin("fs"));
    assert!(is_builtin("path"));
    assert!(is_builtin("util/types"));
    assert!(is_builtin("_http_agent"));
    // only available with the prefix
    assert!(!is_builtin("test"));
  }

  #[test]
  fn other_specifiers_are_not_builtin() {
    assert!(!is_builtin("react"));
    assert!(!is_builtin("bunyan"));
    assert!(!is_builtin("fs-extra"));
    assert!(!is_builtin("npm:react"));
    assert!(!is_builtin("jsr:@std/path"));
    assert!(!is_builtin("https://deno.land/x/foo/mod.ts"));
  }

  #[test]
  fn node_core_modules_sorted() {
    let mut sorted = NODE_CORE_MODULES.to_vec();
    sorted.sort_unstable();
    assert_eq!(sorted, NODE_CORE_MODULES);
  }
}
