extern crate dprint_core;

pub mod configuration;
mod format_text;
mod generation;
mod swc;
mod utils;

pub use format_text::format_parsed_source;
pub use format_text::format_text;

#[cfg(feature = "tracing")]
pub use format_text::trace_file;

#[cfg(feature = "wasm")]
#[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
mod wasm_plugin;

#[cfg(feature = "wasm")]
#[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
pub use wasm_plugin::*;
