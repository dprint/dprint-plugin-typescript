use std::path::Path;

use deno_ast::ParsedSource;
use dprint_core::configuration::resolve_new_line_kind;
use dprint_core::formatting::*;
use dprint_core::types::ErrBox;

use crate::swc::ensure_no_specific_syntax_errors;

use super::configuration::Configuration;
use super::parsing::parse;
use super::swc::parse_swc_ast;

/// Formats a file.
///
/// Returns the file text or an error when it failed to parse.
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
    Ok(String::from(file_text))
  } else {
    let parsed_source = parse_swc_ast(file_path, file_text)?;
    inner_format(&parsed_source, config)
  }
}

/// Formats an already parsed source. This is useful as a performance optimization.
pub fn format_parsed_source(source: &ParsedSource, config: &Configuration) -> Result<String, ErrBox> {
  if super::utils::file_text_has_ignore_comment(source.source().text_str(), &config.ignore_file_comment_text) {
    Ok(source.source().text_str().to_string())
  } else {
    inner_format(source, config)
  }
}

fn inner_format(parsed_source: &ParsedSource, config: &Configuration) -> Result<String, ErrBox> {
  ensure_no_specific_syntax_errors(parsed_source)?;

  Ok(dprint_core::formatting::format(
    || {
      let print_items = parse(&parsed_source, config);
      // println!("{}", print_items.get_as_text());
      print_items
    },
    config_to_print_options(parsed_source.source().text_str(), config),
  ))
}

#[cfg(feature = "tracing")]
pub fn trace_file(file_path: &Path, file_text: &str, config: &Configuration) -> dprint_core::formatting::TracingResult {
  let parsed_source = parse_swc_ast(file_path, file_text).unwrap();
  ensure_no_specific_syntax_errors(parsed_source).unwrap();
  dprint_core::formatting::trace_printing(|| parse(&parsed_source, config), config_to_print_options(file_text, config))
}

fn config_to_print_options(file_text: &str, config: &Configuration) -> PrintOptions {
  PrintOptions {
    indent_width: config.indent_width,
    max_width: config.line_width,
    use_tabs: config.use_tabs,
    new_line_text: resolve_new_line_kind(file_text, config.new_line_kind),
  }
}

#[cfg(test)]
mod test {
  use std::path::PathBuf;

  use crate::configuration::ConfigurationBuilder;

  use super::*;

  #[test]
  fn it_should_error_for_no_equals_sign_in_var_decl() {
    run_diagnostic_test(
      "./test.ts",
      "const Methods {\nf: (x, y) => x + y,\n};",
      concat!("Line 1, column 15: Expected a semicolon\n", "\n", "  const Methods {\n", "                ~"),
    );
  }

  #[test]
  fn it_should_error_when_var_stmts_sep_by_comma() {
    run_diagnostic_test(
      "./test.ts",
      "let a = 0, let b = 1;",
      concat!(
        "Line 1, column 16: Expected a semicolon\n",
        "\n",
        "  let a = 0, let b = 1;\n",
        "                 ~"
      ),
    );
  }

  #[test]
  fn it_should_error_for_exected_expr_issue_121() {
    run_diagnostic_test(
      "./test.ts",
      "type T =\n  | unknown\n  { } & unknown;",
      concat!("Line 3, column 7: Expression expected\n", "\n", "    { } & unknown;\n", "        ~"),
    );
  }

  fn run_diagnostic_test(file_path: &str, text: &str, expected: &str) {
    let file_path = PathBuf::from(file_path);
    let parsed_source = crate::swc::parse_swc_ast(&file_path, text).unwrap();
    let config = ConfigurationBuilder::new().build();
    // ensure it happens for both of these methods
    assert_eq!(format_text(&file_path, text, &config).err().unwrap().to_string(), expected);
    assert_eq!(format_parsed_source(&parsed_source, &config).err().unwrap().to_string(), expected);
  }
}
