use std::path::Path;
use std::sync::Arc;

use dprint_core::configuration::resolve_new_line_kind;
use dprint_core::formatting::*;
use oxc_ast::ast::Program;
use oxc_parser::Token;

use crate::Result;
use crate::ast::ProgramData;
use crate::ast::ProgramInfo;

use super::configuration::Configuration;
pub use super::generation::ExternalFormatter;
use super::generation::generate;
use super::parsing::ParseOptions;
use super::parsing::ParsedSource;
use super::parsing::parse_program;

pub struct FormatTextOptions<'a> {
  pub path: &'a Path,
  pub extension: Option<&'a str>,
  /// Text of the file. Any byte order mark is stripped before parsing.
  ///
  /// This is an `Arc<str>` because the text has to end up in one in order to
  /// be shared with the ast view, so a caller that already has one saves a copy.
  pub text: Arc<str>,
  pub config: &'a Configuration,
  pub external_formatter: Option<&'a ExternalFormatter<'a>>,
  /// The pool to create the arena for formatting from, which reuses its memory
  /// between formats. When `None`, the memory is freed once the file is formatted.
  pub arena_pool: Option<&'a ArenaPool>,
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
///         arena_pool: None,
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
    arena_pool,
  } = options;
  if super::utils::file_text_has_ignore_comment(&file_text, &config.ignore_file_comment_text) {
    Ok(None)
  } else {
    // stripping the byte order mark is the only thing that copies the text here
    let had_bom = file_text.starts_with('\u{FEFF}');
    let file_text = if had_bom { file_text['\u{FEFF}'.len_utf8()..].into() } else { file_text };
    let parsed_source = parse_program(ParseOptions {
      path: file_path,
      extension: file_extension,
      text: file_text,
    })?;
    let arena = create_arena(arena_pool);
    let formatted = parsed_source.with_program(|program, tokens| inner_format_program(&arena, program, tokens, config, external_formatter))?;
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

pub struct FormatParsedSourceOptions<'a> {
  pub source: &'a ParsedSource,
  pub config: &'a Configuration,
  pub external_formatter: Option<&'a ExternalFormatter<'a>>,
  /// The pool to create the arena for formatting from, which reuses its memory
  /// between formats. When `None`, the memory is freed once the file is formatted.
  pub arena_pool: Option<&'a ArenaPool>,
}

/// Formats a source already parsed by this crate.
pub fn format_parsed_source(options: FormatParsedSourceOptions) -> Result<Option<String>> {
  let FormatParsedSourceOptions {
    source,
    config,
    external_formatter,
    arena_pool,
  } = options;
  let arena = create_arena(arena_pool);
  source.with_program(|program, tokens| {
    format_program(FormatProgramOptions {
      program,
      tokens,
      config,
      external_formatter,
      arena: &arena,
    })
  })
}

pub struct FormatProgramOptions<'a> {
  /// The program, which must have been parsed with the options from
  /// [`parse_options`](crate::parsing::parse_options) (ex. with parenthesized
  /// expressions preserved).
  pub program: &'a Program<'a>,
  /// The tokens of the program, which are collected by parsing with
  /// `oxc_parser::config::TokensParserConfig`.
  pub tokens: &'a [Token],
  pub config: &'a Configuration,
  pub external_formatter: Option<&'a ExternalFormatter<'a>>,
  /// The arena to allocate the print items in.
  pub arena: &'a Arena,
}

/// Formats an already parsed program.
///
/// Use this when the source was parsed elsewhere, so that the text does not
/// need to be parsed twice. The program must have been parsed with the tokens
/// collected and with [`parse_options`](crate::parsing::parse_options).
///
/// Note that this does not check for syntax errors. Check that the parser
/// didn't have a fatal error beforehand.
pub fn format_program(options: FormatProgramOptions) -> Result<Option<String>> {
  let FormatProgramOptions {
    program,
    tokens,
    config,
    external_formatter,
    arena,
  } = options;
  if !program.source_text.is_empty() && tokens.is_empty() && !program.body.is_empty() {
    return Err("The tokens must be collected in order to format a program.".into());
  }
  if super::utils::file_text_has_ignore_comment(program.source_text, &config.ignore_file_comment_text) {
    return Ok(None);
  }
  inner_format_program(arena, program, tokens, config, external_formatter)
}

#[cfg(feature = "tracing")]
pub fn trace_file(file_path: &Path, file_text: &str, config: &Configuration) -> dprint_core::formatting::TracingResult {
  let parsed_source = parse_program(ParseOptions {
    path: file_path,
    extension: None,
    text: file_text.into(),
  })
  .unwrap();
  let arena = Arena::new();
  parsed_source.with_program(|program, tokens| {
    let data = ProgramData::new(program, tokens);
    dprint_core::formatting::trace_printing(
      generate(&arena, ProgramInfo::new(&data), config, None).unwrap(),
      config_to_print_options(file_text, config),
    )
  })
}

fn inner_format_program<'a>(
  arena: &'a Arena,
  program: &'a Program<'a>,
  tokens: &'a [Token],
  config: &'a Configuration,
  external_formatter: Option<&'a ExternalFormatter<'a>>,
) -> Result<Option<String>> {
  let file_text = program.source_text;
  let data = ProgramData::new(program, tokens);
  let print_items = generate(arena, ProgramInfo::new(&data), config, external_formatter)?;
  let result = dprint_core::formatting::print(print_items, config_to_print_options(file_text, config));
  if result == file_text { Ok(None) } else { Ok(Some(result)) }
}

fn create_arena(arena_pool: Option<&ArenaPool>) -> Arena {
  match arena_pool {
    Some(arena_pool) => arena_pool.create_arena(),
    None => Arena::new(),
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
        arena_pool: None,
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
      arena_pool: None,
    });
    assert!(result.is_err());
    assert_eq!(
      result.unwrap_err().to_string(),
      "Error formatting tagged template literal at line 1: Syntax error from external formatter"
    );
  }

  #[test]
  fn format_parsed_program() {
    let config = crate::configuration::ConfigurationBuilder::new().build();
    let parsed_source = parse_program(ParseOptions {
      path: &std::path::PathBuf::from("test.ts"),
      extension: None,
      text: "const  t  =  5 ;".into(),
    })
    .unwrap();
    let arena = Arena::new();
    let result = parsed_source
      .with_program(|program, tokens| {
        format_program(FormatProgramOptions {
          program,
          tokens,
          config: &config,
          external_formatter: None,
          arena: &arena,
        })
      })
      .unwrap()
      .unwrap();
    assert_eq!(
      result,
      "const t = 5;
"
    );
  }
}
