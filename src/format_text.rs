use std::path::Path;

use dprint_core::formatting::*;
use dprint_core::types::ErrBox;
use dprint_core::configuration::{resolve_new_line_kind};

use super::parsing::parse;
use super::swc::parse_swc_ast;
use super::configuration::Configuration;

/// Formats a file.
///
/// Returns the file text `Ok(formatted_text)` or an error when it failed to parse.
///
/// # Example
///
/// ```
/// use std::path::PathBuf;
/// use dprint_plugin_typescript::*;
/// use dprint_plugin_typescript::configuration::*;
///
/// // build the configuration once
/// let config = ConfigurationBuilder::new()
///     .line_width(80)
///     .prefer_hanging(true)
///     .prefer_single_line(false)
///     .quote_style(QuoteStyle::PreferSingle)
///     .next_control_flow_position(NextControlFlowPosition::SameLine)
///     .build();
///
/// // now format many files (it is recommended to parallelize this)
/// let files_to_format = vec![(PathBuf::from("path/to/file.ts"), "const  t  =  5 ;")];
/// for (file_path, file_text) in files_to_format.iter() {
///     let result = format_text(file_path, file_text, &config);
///     // save result here...
/// }
/// ```
pub fn format_text(file_path: &Path, file_text: &str, config: &Configuration) -> Result<String, ErrBox> {
    if super::utils::file_text_has_ignore_comment(file_text, &config.ignore_file_comment_text) {
        return Ok(String::from(file_text));
    }

    let parsed_source_file = parse_swc_ast(file_path, file_text)?;
    Ok(dprint_core::formatting::format(|| {
        let print_items = parse(&parsed_source_file, config);
        // println!("{}", print_items.get_as_text());
        print_items
    }, config_to_print_options(file_text, config)))
}

#[cfg(feature = "tracing")]
pub fn trace_file(file_path: &Path, file_text: &str, config: &Configuration) -> dprint_core::formatting::TracingResult {
    let parsed_source_file = parse_swc_ast(file_path, file_text).expect("Expected to parse to SWC AST.");
    dprint_core::formatting::trace_printing(
        || parse(&parsed_source_file, config),
        config_to_print_options(file_text, config),
    )
}

fn config_to_print_options(file_text: &str, config: &Configuration) -> PrintOptions {
    PrintOptions {
        indent_width: config.indent_width,
        max_width: config.line_width,
        use_tabs: config.use_tabs,
        new_line_text: resolve_new_line_kind(file_text, config.new_line_kind),
    }
}