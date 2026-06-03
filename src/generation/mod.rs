// oxc-port: the generation layer is being rewritten module-by-module onto oxc
// `AstKind`. Ported modules are enabled below; the remaining SWC-based ones stay
// commented out (kept on disk as the porting reference) and `generate` is a stub
// so the rest of the crate compiles and can be exercised.

mod comments;
mod context;
mod oxc_helpers;
mod to_node;
mod tokens;

mod swc;

mod sorting;

mod generate_types;
mod node_helpers;

mod generate;

use comments::*;
use context::*;
use tokens::*;

pub use context::ExternalFormatter;
pub use generate::generate;
