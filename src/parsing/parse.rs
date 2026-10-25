use std::cell::OnceCell;
use std::sync::Arc;

use dprint_swc_ext::common::SourceTextInfo;
use dprint_swc_ext::common::StartSourcePos;
use dprint_swc_ext::swc::ast::EsVersion;
use dprint_swc_ext::swc::ast::Program;
use dprint_swc_ext::swc::common::comments::SingleThreadedComments;
use dprint_swc_ext::swc::common::input::StringInput;
use dprint_swc_ext::swc::lexer::common::parser::Parser as _;
use dprint_swc_ext::swc::parser::error::Error as SwcError;
use dprint_swc_ext::swc::parser::error::SyntaxError;
use dprint_swc_ext::swc::parser::token::TokenAndSpan;
use dprint_swc_ext::swc::parser::Syntax;

use super::ParseDiagnostic;
use super::ParseDiagnosticsError;
use super::ParsedComments;
use super::ParsedSource;
use crate::Result;

/// Ecmascript version used for lexing and parsing.
const ES_VERSION: EsVersion = EsVersion::Es2021;

/// Whether to parse the source as a module, a script, or let swc decide.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum ParseMode {
  Program,
  Module,
  Script,
}

/// Parses the provided text with the given syntax.
pub(super) fn parse_syntax(specifier: Arc<str>, text: Arc<str>, syntax: Syntax, mode: ParseMode) -> Result<ParsedSource> {
  // swc's positions would all be offset by the byte order mark, so strip it. This
  // only reallocates in the rare case that one is actually present.
  let text: Arc<str> = match text.strip_prefix('\u{FEFF}') {
    Some(stripped) => stripped.into(),
    None => text,
  };
  let input = StringInput::new(
    text.as_ref(),
    StartSourcePos::START_SOURCE_POS.as_byte_pos(),
    (StartSourcePos::START_SOURCE_POS + text.len()).as_byte_pos(),
  );
  let (comments, program, tokens, errors) = parse_string_input(input, syntax, mode).map_err(|err| {
    let text_info = SourceTextInfo::new(text.clone());
    ParseDiagnostic::from_swc_error(err, &specifier, text_info)
  })?;
  // pre-populate the text info when it had to be created for diagnostics anyway
  let text_info: OnceCell<SourceTextInfo> = Default::default();
  let diagnostics = if errors.is_empty() {
    Vec::new()
  } else {
    let source_text_info = SourceTextInfo::new(text.clone());
    let diagnostics = errors
      .into_iter()
      .map(|err| ParseDiagnostic::from_swc_error(err, &specifier, source_text_info.clone()))
      .collect::<Vec<_>>();
    let _ = text_info.set(source_text_info);
    // these ones mean the ast no longer represents the original text
    let (unsupported, diagnostics) = diagnostics.into_iter().partition::<Vec<_>, _>(|d| is_unsupported_syntax_error(d.kind()));
    if !unsupported.is_empty() {
      return Err(ParseDiagnosticsError(unsupported).into());
    }
    diagnostics
  };

  Ok(ParsedSource {
    specifier,
    text,
    syntax,
    text_info,
    program,
    comments: ParsedComments::from_single_threaded(comments),
    tokens,
    diagnostics,
  })
}

/// Gets whether the provided syntax error stops the AST from representing the
/// original text, which means formatting would cause more harm than good.
///
/// swc recovers from many syntax errors. Most are harmless to format through,
/// but these ones are not.
pub fn is_unsupported_syntax_error(kind: &SyntaxError) -> bool {
  matches!(
    kind,
    // unexpected eof
    SyntaxError::Eof |
    // expected identifier
    SyntaxError::TS1003 |
    SyntaxError::ExpectedIdent |
    // expected semi-colon
    SyntaxError::TS1005 |
    SyntaxError::ExpectedSemi |
    // expected expression
    SyntaxError::TS1109 |
    // expected token
    SyntaxError::Expected(_, _) |
    // various expected
    SyntaxError::ExpectedDigit { .. } |
    SyntaxError::ExpectedSemiForExprStmt { .. } |
    SyntaxError::ExpectedUnicodeEscape |
    // various unterminated
    SyntaxError::UnterminatedStrLit |
    SyntaxError::UnterminatedBlockComment |
    SyntaxError::UnterminatedJSXContents |
    SyntaxError::UnterminatedRegExp |
    SyntaxError::UnterminatedTpl |
    // unexpected token
    SyntaxError::Unexpected { .. } |
    // Merge conflict marker
    SyntaxError::TS1185
  )
}

#[allow(clippy::type_complexity)]
fn parse_string_input(
  input: StringInput,
  syntax: Syntax,
  mode: ParseMode,
) -> std::result::Result<(SingleThreadedComments, Program, Vec<TokenAndSpan>, Vec<SwcError>), SwcError> {
  let comments = SingleThreadedComments::default();
  let lexer = dprint_swc_ext::swc::lexer::Lexer::new(syntax, ES_VERSION, input, Some(&comments));
  let lexer = dprint_swc_ext::swc::lexer::Capturing::new(lexer);
  let mut parser = dprint_swc_ext::swc::lexer::Parser::new_from(lexer);
  let program = match mode {
    ParseMode::Program => parser.parse_program()?,
    ParseMode::Module => Program::Module(parser.parse_module()?),
    ParseMode::Script => Program::Script(parser.parse_script()?),
  };
  let iter = &mut parser.input_mut().iter;
  let tokens = dprint_swc_ext::swc::lexer::Capturing::take(iter);
  let errors = parser.take_errors();

  Ok((comments, program, tokens, errors))
}

#[cfg(test)]
mod test {
  use dprint_swc_ext::common::SourceRanged;
  use dprint_swc_ext::view::ProgramInfoProvider;
  use pretty_assertions::assert_eq;

  use super::*;

  #[test]
  fn parses_with_tokens_and_comments() {
    let parsed_source = parse_ts("// 1\n1 + 1\n// 2").unwrap();
    assert_eq!(parsed_source.specifier(), "file:///my_file.ts");
    assert_eq!(parsed_source.text().as_ref(), "// 1\n1 + 1\n// 2");
    assert_eq!(parsed_source.tokens().len(), 3);
    assert!(parsed_source.diagnostics().is_empty());
  }

  #[test]
  fn strips_byte_order_mark() {
    let parsed_source = parse_ts("\u{FEFF}const t = 5;").unwrap();
    assert_eq!(parsed_source.text().as_ref(), "const t = 5;");
    // the text and the text info must agree or every position is off by three
    assert_eq!(parsed_source.text_info().text_str(), parsed_source.text().as_ref());
    assert_eq!(parsed_source.text_info().range().end, parsed_source.range().end);
    parsed_source.with_view(|program| {
      assert_eq!(program.text_fast(program), "const t = 5;");
    });
  }

  #[test]
  fn keeps_recovered_diagnostics_that_are_safe_to_format() {
    // swc recovers from this without losing any of the original text
    let parsed_source = parse_ts("using test").unwrap();
    assert_eq!(parsed_source.diagnostics().len(), 1);
    assert_eq!(parsed_source.diagnostics()[0].message(), "Using declaration requires initializer");
    // the text info is pre-populated in this case, but must still be correct
    assert_eq!(parsed_source.text_info().text_str(), "using test");
  }

  #[test]
  fn errors_for_recovered_diagnostics_that_lose_text() {
    // swc recovers from this, but the ast no longer represents the original
    // text, so formatting would cause more harm than good
    let err = parse_ts("var foo = 'test").err().unwrap();
    assert_eq!(
      err.to_string(),
      concat!(
        "Unterminated string constant at file:///my_file.ts:1:11

",
        "  var foo = 'test
",
        "            ~~~~~"
      )
    );
  }

  #[test]
  fn errors_for_fatal_diagnostic() {
    let err = parse_ts(
      "test;
as#;",
    )
    .err()
    .unwrap();
    assert_eq!(
      err.to_string(),
      concat!(
        "Expected ';', '}' or <eof> at file:///my_file.ts:2:3

",
        "  as#;
",
        "    ~"
      )
    );
  }

  #[test]
  fn does_not_panic_rendering_diagnostics_for_odd_input() {
    // swc will sometimes hand back a dummy or out of range span, which the
    // diagnostic must not blindly index the text with
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
    parse_syntax(
      "file:///my_file.ts".into(),
      text.into(),
      Syntax::Typescript(Default::default()),
      ParseMode::Program,
    )
  }
}
