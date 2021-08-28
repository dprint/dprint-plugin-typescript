extern crate dprint_core;

pub mod configuration;
mod format_text;
mod parsing;
mod swc;
mod utils;

pub use format_text::format_parsed_file;
pub use format_text::format_text;
pub use format_text::SourceFileInfo;

#[cfg(feature = "tracing")]
pub use format_text::trace_file;

#[cfg(feature = "wasm")]
#[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
mod wasm_plugin;

#[cfg(feature = "wasm")]
#[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
pub use wasm_plugin::*;
