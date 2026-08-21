use std::path::Path;
use std::sync::Arc;

use dprint_core::configuration::resolve_new_line_kind;
use dprint_core::formatting::*;
use dprint_swc_ext::common::SourceTextInfoProvider;
use dprint_swc_ext::view::Program;
use dprint_swc_ext::view::ProgramInfoProvider;

use crate::FormatError;
use crate::MediaType;
use crate::Result;

use super::configuration::Configuration;
use super::generation::generate;
pub use super::generation::ExternalFormatter;
use super::parsing::parse_swc_ast;

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
    let (parsed_source, media_type) = parse_swc_ast(file_path, file_extension, file_text)?;
    let formatted = parsed_source.with_view(|program| inner_format_program(program, media_type, config, external_formatter))?;
    match formatted {
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

pub struct FormatParsedSourceOptions<'a, TSource: ProgramInfoProvider> {
  pub source: &'a TSource,
  pub media_type: MediaType,
  pub config: &'a Configuration,
  pub external_formatter: Option<&'a ExternalFormatter>,
}

/// Formats an already parsed source. This is useful as a performance optimization.
///
/// Any parsed source implementing `ProgramInfoProvider` works here, including
/// `deno_ast::ParsedSource`, so that the text does not need to be parsed twice.
///
/// Note that for a source this crate did not parse, it's up to the caller to first
/// check the parse diagnostics with
/// [`is_unsupported_syntax_error`](crate::is_unsupported_syntax_error).
pub fn format_parsed_source<TSource: ProgramInfoProvider>(options: FormatParsedSourceOptions<TSource>) -> Result<Option<String>> {
  let FormatParsedSourceOptions {
    source,
    media_type,
    config,
    external_formatter,
  } = options;
  source.with_view(|program| {
    format_program(FormatProgramOptions {
      program,
      media_type,
      config,
      external_formatter,
    })
  })
}

pub struct FormatProgramOptions<'a> {
  pub program: Program<'a>,
  pub media_type: MediaType,
  pub config: &'a Configuration,
  pub external_formatter: Option<&'a ExternalFormatter>,
}

/// Formats an ast view of an already parsed source.
///
/// Use this when the source was parsed elsewhere (ex. with `deno_ast`). The
/// program must have been parsed with tokens and comments captured.
///
/// Note that unlike the other entrypoints, this does not check for syntax
/// errors that swc recovered from. Use [`is_unsupported_syntax_error`](crate::is_unsupported_syntax_error)
/// on the parse diagnostics beforehand in order to do that.
///
/// # Example
///
/// ```ignore
/// parsed_source.with_view(|program| {
///   format_program(FormatProgramOptions {
///     program,
///     media_type: MediaType::TypeScript,
///     config: &config,
///     external_formatter: None,
///   })
/// })
/// ```
pub fn format_program(options: FormatProgramOptions) -> Result<Option<String>> {
  let FormatProgramOptions {
    program,
    media_type,
    config,
    external_formatter,
  } = options;
  let file_text = program.text_info().text_str();
  if super::utils::file_text_has_ignore_comment(file_text, &config.ignore_file_comment_text) {
    return Ok(None);
  }
  inner_format_program(program, media_type, config, external_formatter)
}

#[cfg(feature = "tracing")]
pub fn trace_file(file_path: &Path, file_text: &str, config: &Configuration) -> dprint_core::formatting::TracingResult {
  let (parsed_source, media_type) = parse_swc_ast(file_path, None, file_text.into()).unwrap();
  dprint_core::formatting::trace_printing(
    || parsed_source.with_view(|program| generate(program, media_type, config, None)).unwrap(),
    config_to_print_options(file_text, config),
  )
}

fn inner_format_program<'a>(
  program: Program<'a>,
  media_type: MediaType,
  config: &'a Configuration,
  external_formatter: Option<&'a ExternalFormatter>,
) -> Result<Option<String>> {
  let file_text = program.text_info().text_str();
  let mut maybe_err: Box<Option<FormatError>> = Box::new(None);
  let result = dprint_core::formatting::format(
    || match generate(program, media_type, config, external_formatter) {
      Ok(print_items) => print_items,
      Err(e) => {
        maybe_err.replace(e);
        PrintItems::default()
      }
    },
    config_to_print_options(file_text, config),
  );
  if let Some(e) = maybe_err.take() {
    return Err(e);
  }
  if result == file_text {
    Ok(None)
  } else {
    Ok(Some(result))
  }
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
      external_formatter: Some(&|lang, _text, _config| {
        assert!(matches!(lang, "html"));
        Err("Syntax error from external formatter".into())
      }),
    });
    assert!(result.is_err());
    assert_eq!(
      result.unwrap_err().to_string(),
      "Error formatting tagged template literal at line 1: Syntax error from external formatter"
    );
  }

  #[test]
  fn format_program_from_ast_view() {
    let config = crate::configuration::ConfigurationBuilder::new().build();
    let (parsed_source, media_type) = crate::parsing::parse_swc_ast(&std::path::PathBuf::from("test.ts"), None, "const  t  =  5 ;".into()).unwrap();
    let result = parsed_source
      .with_view(|program| {
        format_program(FormatProgramOptions {
          program,
          media_type,
          config: &config,
          external_formatter: None,
        })
      })
      .unwrap()
      .unwrap();
    assert_eq!(result, "const t = 5;\n");
  }
}
