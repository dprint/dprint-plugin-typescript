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
mod to_node;
#[allow(dead_code)]
mod tokens;

#[allow(dead_code)]
mod swc;

#[allow(dead_code)]
mod sorting;

#[allow(dead_code)]
mod generate_types;
#[allow(dead_code)]
mod node_helpers;

mod generate;

use comments::*;
use context::*;
use tokens::*;

pub use context::ExternalFormatter;
pub use generate::generate;
