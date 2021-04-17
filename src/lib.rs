extern crate dprint_core;

pub mod configuration;
mod parsing;
mod format_text;
mod swc;
mod utils;

pub use format_text::format_text;

#[cfg(feature = "tracing")]
pub use format_text::trace_file;

#[cfg(feature = "wasm")]
#[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
mod wasm_plugin;

#[cfg(feature = "wasm")]
#[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
pub use wasm_plugin::*;
