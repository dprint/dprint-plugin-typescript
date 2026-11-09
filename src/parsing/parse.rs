use std::sync::Arc;

use oxc_allocator::Allocator;
use oxc_parser::ParseOptions;
use oxc_parser::Parser;
use oxc_parser::config::TokensParserConfig;
use oxc_span::SourceType;

use super::ParseDiagnostic;
use super::ParseDiagnosticsError;
use super::ParsedProgram;
use super::ParsedSource;
use super::ParsedSourceCell;
use super::ParsedSourceOwner;
use crate::Result;
use crate::ast::SourceTextInfo;

/// Parses the provided text with the given source type.
pub(super) fn parse_source_type(specifier: Arc<str>, text: Arc<str>, source_type: SourceType) -> Result<ParsedSource> {
  // the positions would all be offset by the byte order mark, so strip it. This
  // only reallocates in the rare case that one is actually present.
  let text: Arc<str> = match text.strip_prefix('\u{FEFF}') {
    Some(stripped) => stripped.into(),
    None => text,
  };
  // like swc, a file that could be a module or a script is parsed as a module
  // when it parses without errors that way, which makes `await (x)` at the top
  // level an await expression (`let` followed by a newline and `[a] = 1` only
  // parses as an assignment in a script)
  if source_type.is_unambiguous()
    && let Ok(parsed_source) = parse_source_type_inner(specifier.clone(), text.clone(), source_type.with_module(true))
    && parsed_source.diagnostics.is_empty()
  {
    return Ok(parsed_source);
  }
  parse_source_type_inner(specifier, text, source_type)
}

fn parse_source_type_inner(specifier: Arc<str>, text: Arc<str>, source_type: SourceType) -> Result<ParsedSource> {
  let owner = ParsedSourceOwner {
    allocator: Allocator::default(),
    text: text.clone(),
  };
  let mut diagnostics = Vec::new();
  let mut is_fatal = false;
  let mut loses_text = false;
  let cell = ParsedSourceCell::new(owner, |owner| {
    let ret = Parser::new(&owner.allocator, &owner.text, source_type)
      .with_options(parse_options())
      .with_config(TokensParserConfig)
      .parse();
    is_fatal = ret.fatal_error;
    diagnostics = ret.diagnostics.into_vec();
    if !is_fatal && !diagnostics.is_empty() {
      loses_text = has_text_outside_tokens(&owner.text, &ret.program, &ret.tokens);
    }
    ParsedProgram {
      program: ret.program,
      tokens: ret.tokens,
    }
  });

  let diagnostics = if diagnostics.is_empty() {
    Vec::new()
  } else {
    let diagnostics = diagnostics
      .into_iter()
      .filter(|d| d.severity == oxc_diagnostics::Severity::Error)
      .collect::<Vec<_>>();
    let text_info = SourceTextInfo::new(text.clone());
    let to_parse_diagnostic = |d: oxc_diagnostics::OxcDiagnostic| ParseDiagnostic::from_oxc_diagnostic(d, &specifier, text_info.clone());
    // when the parser dropped some of the text while recovering, all the
    // diagnostics are shown since it's not known which one caused it
    if is_fatal || loses_text {
      return Err(ParseDiagnosticsError(diagnostics.into_iter().map(to_parse_diagnostic).collect()).into());
    }
    // these ones mean the ast no longer represents the original text
    let (unsupported, diagnostics) = diagnostics.into_iter().partition::<Vec<_>, _>(is_unsupported_syntax_error);
    if !unsupported.is_empty() {
      return Err(ParseDiagnosticsError(unsupported.into_iter().map(to_parse_diagnostic).collect()).into());
    }
    diagnostics.into_iter().map(to_parse_diagnostic).collect()
  };

  Ok(ParsedSource {
    specifier,
    source_type,
    cell,
    diagnostics,
  })
}

/// Gets whether there's text other than whitespace that's not in a token,
/// comment, or the hashbang, which happens when the parser drops text while
/// recovering from a syntax error (ex. `10._1;`).
fn has_text_outside_tokens(text: &str, program: &oxc_ast::ast::Program, tokens: &[oxc_parser::Token]) -> bool {
  let mut ranges = tokens
    .iter()
    .filter(|t| !t.kind().is_eof())
    .map(|t| (t.start(), t.end()))
    .chain(program.comments.iter().map(|c| (c.span.start, c.span.end)))
    .chain(program.hashbang.iter().map(|h| (h.span.start, h.span.end)))
    .collect::<Vec<_>>();
  ranges.sort_unstable();
  let mut pos = 0;
  for (start, end) in ranges.into_iter().chain(std::iter::once((text.len() as u32, text.len() as u32))) {
    let start = start as usize;
    if start > pos
      && text
        .get(pos..start)
        .is_some_and(|gap| !gap.chars().all(|c| c.is_whitespace() || c == '\u{FEFF}'))
    {
      return true;
    }
    pos = pos.max(end as usize);
  }
  false
}

/// Gets whether the provided syntax error that the parser recovered from stops
/// the AST from representing the original text, which means formatting would
/// cause more harm than good.
///
/// Most of the errors the parser recovers from are harmless to format through
/// (ex. a `const` without an initializer), but these ones are not. Check this
/// on the diagnostics of a program parsed elsewhere before formatting it with
/// [`format_program`](crate::format_program).
pub fn is_unsupported_syntax_error(diagnostic: &oxc_diagnostics::OxcDiagnostic) -> bool {
  const UNSUPPORTED_MESSAGE_PREFIXES: &[&str] = &[
    "Unterminated string",
    "Unterminated regular expression",
    "Unterminated multiline comment",
    "Unterminated template",
    "Invalid Character",
    "Invalid Unicode escape sequence",
    "Unexpected end of file",
    "Unexpected token",
    "Expected corresponding JSX closing tag",
    "Expected corresponding closing tag for JSX fragment",
    "Adjacent JSX elements",
    "Encountered diff marker",
    // `{ a = 1 }` in an object literal, which loses the initializer
    "Invalid assignment in object literal",
  ];
  UNSUPPORTED_MESSAGE_PREFIXES.iter().any(|prefix| diagnostic.message.starts_with(prefix))
}

/// Options used when parsing, which a program parsed elsewhere should also use.
///
/// Parenthesized expressions must be preserved in the ast and tokens must be
/// collected (see [`TokensParserConfig`]).
pub fn parse_options() -> ParseOptions {
  ParseOptions {
    preserve_parens: true,
    allow_return_outside_function: true,
    ..Default::default()
  }
}

#[cfg(test)]
mod test {
  use pretty_assertions::assert_eq;

  use super::*;

  #[test]
  fn parses_with_tokens_and_comments() {
    let parsed_source = parse_ts("// 1\n1 + 1\n// 2").unwrap();
    assert_eq!(parsed_source.specifier(), "file:///my_file.ts");
    assert_eq!(parsed_source.text().as_ref(), "// 1\n1 + 1\n// 2");
    parsed_source.with_program(|program, tokens| {
      assert_eq!(tokens.len(), 3);
      assert_eq!(program.comments.len(), 2);
    });
    assert!(parsed_source.diagnostics().is_empty());
  }

  #[test]
  fn strips_byte_order_mark() {
    let parsed_source = parse_ts("\u{FEFF}const t = 5;").unwrap();
    assert_eq!(parsed_source.text().as_ref(), "const t = 5;");
    parsed_source.with_program(|program, _| {
      assert_eq!(program.source_text, "const t = 5;");
    });
  }

  #[test]
  fn keeps_recovered_diagnostics_that_are_safe_to_format() {
    let parsed_source = parse_ts("using test").unwrap();
    assert_eq!(parsed_source.diagnostics().len(), 1);
    assert_eq!(parsed_source.diagnostics()[0].message(), "Using declarations must have an initializer.");
  }

  #[test]
  fn errors_when_recovering_drops_text() {
    // the parser recovers from this by dropping the number
    let err = parse_ts("10._1;").err().unwrap();
    assert!(err.to_string().starts_with("Invalid characters after number"), "{}", err);
  }

  #[test]
  fn errors_for_fatal_diagnostic() {
    let err = parse_ts("test;\nas#;").err().unwrap();
    assert!(err.to_string().contains("at file:///my_file.ts:2:4"), "{}", err);
  }

  #[test]
  fn does_not_panic_rendering_diagnostics_for_odd_input() {
    for text in ["\u{FEFF}{)", "\u{FEFF}édeclare(]\"&😀", "\r\n|[", "%*/"] {
      match parse_ts(text) {
        Ok(parsed_source) => {
          for diagnostic in parsed_source.diagnostics() {
            let _ignore = diagnostic.to_string();
          }
        }
        Err(err) => {
          let _ignore = err.to_string();
        }
      }
    }
  }

  fn parse_ts(text: &str) -> Result<ParsedSource> {
    parse_source_type("file:///my_file.ts".into(), text.into(), SourceType::ts())
  }
}
