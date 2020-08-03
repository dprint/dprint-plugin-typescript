extern crate dprint_core;

pub mod configuration;
mod parsing;
mod formatter;
mod swc;
mod utils;

pub use formatter::Formatter;

#[cfg(feature = "wasm")]
#[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
mod wasm_plugin;

#[cfg(feature = "wasm")]
#[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
pub use wasm_plugin::*;
