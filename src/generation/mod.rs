// oxc-port: the generation layer is being rewritten module-by-module onto oxc
// `AstKind`. Ported modules are enabled below; the remaining SWC-based ones stay
// commented out (kept on disk as the porting reference) and `generate` is a stub
// so the rest of the crate compiles and can be exercised.

#[allow(dead_code)]
mod comments;
#[allow(dead_code)]
mod context;
#[allow(dead_code)]
mod oxc_helpers;
#[allow(dead_code)]
mod tokens;

#[allow(dead_code)]
mod swc;

// mod generate;
// mod generate_types;
// mod node_helpers;
// mod sorting;

use comments::*;
use tokens::*;

pub use context::ExternalFormatter;

use deno_ast::ParsedSource;
use dprint_core::formatting::PrintItems;

use crate::configuration::Configuration;

/// oxc-port stub. Real implementation lives in `generate.rs` and is being
/// rewritten onto oxc `AstKind`.
pub fn generate<'a>(_parsed_source: &'a ParsedSource<'a>, _config: &Configuration, _external_formatter: Option<&ExternalFormatter>) -> anyhow::Result<PrintItems> {
  todo!("oxc-port: generation layer not yet rewritten")
}
