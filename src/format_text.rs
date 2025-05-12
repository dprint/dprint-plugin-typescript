use std::path::Path;
use std::sync::Arc;

use anyhow::Result;
use deno_ast::ParsedSource;
use dprint_core::configuration::resolve_new_line_kind;
use dprint_core::formatting::*;

use crate::swc::ensure_no_specific_syntax_errors;

use super::configuration::Configuration;
use super::generation::generate;
pub use super::generation::ExternalFormatter;
use super::swc::parse_swc_ast;

pub struct FormatTextOptions<'a> {
  pub path: &'a Path,
  pub extension: Option<&'a str>,
  pub text: String,
  pub config: &'a Configuration,
  pub external_formatter: Option<&'a ExternalFormatter>,
}

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
/// for (file_path, file_text) in files_to_format {
///     let result = format_text(FormatTextOptions {
///         path: &file_path,
///         extension: None,
///         text: file_text.into(),
///         config: &config,
///         external_formatter: None,
///     });
///     // save result here...
/// }
/// ```
pub fn format_text(options: FormatTextOptions) -> Result<Option<String>> {
  let FormatTextOptions {
    path: file_path,
    extension: file_extension,
    text: file_text,
    config,
    external_formatter,
  } = options;
  if super::utils::file_text_has_ignore_comment(&file_text, &config.ignore_file_comment_text) {
    Ok(None)
  } else {
    let had_bom = file_text.starts_with("\u{FEFF}");
    let file_text = if had_bom { file_text[3..].to_string() } else { file_text };
    let file_text: Arc<str> = file_text.into();
    let parsed_source = parse_swc_ast(file_path, file_extension, file_text)?;
    match inner_format(&parsed_source, config, external_formatter)? {
      Some(new_text) => Ok(Some(new_text)),
      None => {
        if had_bom {
          Ok(Some(parsed_source.text().to_string()))
        } else {
          Ok(None)
        }
      }
    }
  }
}

/// Formats an already parsed source. This is useful as a performance optimization.
pub fn format_parsed_source(source: &ParsedSource, config: &Configuration, external_formatter: Option<&ExternalFormatter>) -> Result<Option<String>> {
  if super::utils::file_text_has_ignore_comment(source.text(), &config.ignore_file_comment_text) {
    Ok(None)
  } else {
    ensure_no_specific_syntax_errors(source)?;
    inner_format(source, config, external_formatter)
  }
}

fn inner_format(parsed_source: &ParsedSource, config: &Configuration, external_formatter: Option<&ExternalFormatter>) -> Result<Option<String>> {
  let mut maybe_err: Box<Option<anyhow::Error>> = Box::new(None);
  let result = dprint_core::formatting::format(
    || match generate(parsed_source, config, external_formatter) {
      Ok(print_itmes) => print_itmes,
      Err(e) => {
        maybe_err.replace(e);
        PrintItems::default()
      }
    },
    config_to_print_options(parsed_source.text(), config),
  );
  if let Some(e) = maybe_err.take() {
    return Err(e);
  }
  if result == parsed_source.text().as_ref() {
    Ok(None)
  } else {
    Ok(Some(result))
  }
}

#[cfg(feature = "tracing")]
pub fn trace_file(file_path: &Path, file_text: &str, config: &Configuration) -> dprint_core::formatting::TracingResult {
  let parsed_source = parse_swc_ast(file_path, None, file_text.into()).unwrap();
  ensure_no_specific_syntax_errors(&parsed_source).unwrap();
  dprint_core::formatting::trace_printing(|| generate(&parsed_source, config, None), config_to_print_options(file_text, config))
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
  use super::*;

  #[test]
  fn strips_bom() {
    for input_text in ["\u{FEFF}const t = 5;\n", "\u{FEFF}const t =   5;"] {
      let config = crate::configuration::ConfigurationBuilder::new().build();
      let result = format_text(FormatTextOptions {
        path: &std::path::PathBuf::from("test.ts"),
        extension: None,
        text: input_text.into(),
        config: &config,
        external_formatter: None,
      })
      .unwrap()
      .unwrap();
      assert_eq!(result, "const t = 5;\n");
    }
  }

  #[test]
  fn syntax_error_from_external_formatter() {
    let config = crate::configuration::ConfigurationBuilder::new().build();
    let result = format_text(FormatTextOptions {
      path: &std::path::PathBuf::from("test.ts"),
      extension: None,
      text: "const content = html`<div>broken html</p>`".into(),
      config: &config,
      external_formatter: Some(&|media_type, _text, _config| {
        assert!(matches!(media_type, deno_ast::MediaType::Html));
        Err(anyhow::anyhow!("Syntax error from external formatter"))
      }),
    });
    assert!(result.is_err());
    assert_eq!(result.unwrap_err().to_string(), "Error formatting tagged template literal at line 0: Syntax error from external formatter");
  }
}
