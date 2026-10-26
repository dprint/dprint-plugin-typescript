use std::cell::OnceCell;
use std::ops::Range;
use std::sync::Arc;

use dprint_swc_ext::common::SourcePos;
use dprint_swc_ext::common::SourceRangedForSpanned;
use dprint_swc_ext::common::SourceTextInfo;
use dprint_swc_ext::common::StartSourcePos;
use dprint_swc_ext::swc::ast::Decl;
use dprint_swc_ext::swc::ast::EsVersion;
use dprint_swc_ext::swc::ast::ExportDecl;
use dprint_swc_ext::swc::ast::ModuleDecl;
use dprint_swc_ext::swc::ast::ModuleItem;
use dprint_swc_ext::swc::ast::Program;
use dprint_swc_ext::swc::ast::Stmt;
use dprint_swc_ext::swc::ast::TsNamespaceBody;
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
  let (comments, program, tokens, mut errors) = parse_string_input(input, syntax, mode).map_err(|err| {
    let text_info = SourceTextInfo::new(text.clone());
    ParseDiagnostic::from_swc_error(err, &specifier, text_info)
  })?;
  if syntax.typescript() {
    let dropped_errors = get_speculative_export_decl_errors(&text, syntax, &program, &tokens, &errors);
    errors.extend(dropped_errors);
  }
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

/// Gets the syntax errors swc dropped while parsing the `export` declarations
/// that it parses speculatively.
///
/// swc parses `export namespace`, `export module`, `export enum`,
/// `export interface`, `export type`, and `export abstract class` with
/// recoverable errors ignored, then keeps the result when that succeeds, so any
/// errors it recovered from within them are lost (ex. a missing close brace at
/// the end of the file). Without the `export` keyword swc parses these normally,
/// so this re-parses the text with those keywords blanked out and returns the
/// errors that only show up then.
fn get_speculative_export_decl_errors(text: &str, syntax: Syntax, program: &Program, tokens: &[TokenAndSpan], errors: &[SwcError]) -> Vec<SwcError> {
  let mut collector = SpeculativeExportDeclCollector {
    text,
    tokens,
    blank_ranges: Vec::new(),
  };
  let mode = match program {
    Program::Module(module) => {
      collector.visit_module_items(&module.body);
      // keep it a module even if every import and export gets blanked out
      ParseMode::Module
    }
    Program::Script(script) => {
      for stmt in &script.body {
        if let Stmt::Decl(decl) = stmt {
          collector.visit_decl(decl);
        }
      }
      ParseMode::Script
    }
  };
  if collector.blank_ranges.is_empty() {
    return Vec::new();
  }

  // replacing with spaces keeps every position the same as in the original text
  let mut blanked_text = text.as_bytes().to_vec();
  for range in collector.blank_ranges {
    blanked_text[range].fill(b' ');
  }
  // only whole tokens, whitespace, and comments were replaced, so this is still utf-8
  let blanked_text = String::from_utf8(blanked_text).unwrap();
  let input = StringInput::new(
    &blanked_text,
    StartSourcePos::START_SOURCE_POS.as_byte_pos(),
    (StartSourcePos::START_SOURCE_POS + blanked_text.len()).as_byte_pos(),
  );
  let reparse_errors = match parse_errors_only(input, syntax, mode) {
    Ok(errors) => errors,
    Err(err) => vec![err],
  };
  // everything outside these declarations parses the same, so only keep the new errors
  reparse_errors.into_iter().filter(|err| !errors.contains(err)).collect()
}

/// Collects the text ranges to blank out so that swc parses each speculatively
/// parsed `export` declaration normally.
struct SpeculativeExportDeclCollector<'a> {
  text: &'a str,
  tokens: &'a [TokenAndSpan],
  blank_ranges: Vec<Range<usize>>,
}

impl SpeculativeExportDeclCollector<'_> {
  /// Visits the module items of a module or of a namespace body, which are the
  /// only places an export declaration can be.
  fn visit_module_items(&mut self, items: &[ModuleItem]) {
    for item in items {
      match item {
        ModuleItem::ModuleDecl(ModuleDecl::ExportDecl(export_decl)) => {
          self.visit_export_decl(export_decl);
          self.visit_decl(&export_decl.decl);
        }
        ModuleItem::Stmt(Stmt::Decl(decl)) => self.visit_decl(decl),
        _ => {}
      }
    }
  }

  fn visit_decl(&mut self, decl: &Decl) {
    if let Decl::TsModule(module_decl) = decl
      && let Some(body) = &module_decl.body
    {
      self.visit_namespace_body(body);
    }
  }

  fn visit_namespace_body(&mut self, body: &TsNamespaceBody) {
    match body {
      TsNamespaceBody::TsModuleBlock(block) => self.visit_module_items(&block.body),
      TsNamespaceBody::TsNamespaceDecl(decl) => self.visit_namespace_body(&decl.body),
    }
  }

  fn visit_export_decl(&mut self, export_decl: &ExportDecl) {
    // find the `export` keyword, which may come after any decorators
    let start_index = self.tokens.partition_point(|t| t.start() < export_decl.start());
    let decl_tokens = &self.tokens[start_index..];
    let Some(export_index) = decl_tokens
      .iter()
      .take_while(|t| t.start() < export_decl.end())
      .position(|t| self.token_text(t) == "export")
    else {
      return;
    };
    let (Some(export), Some(keyword), Some(after_keyword)) = (
      decl_tokens.get(export_index),
      decl_tokens.get(export_index + 1),
      decl_tokens.get(export_index + 2),
    ) else {
      return;
    };
    // `export declare` and the rest are parsed normally
    if !matches!(self.token_text(keyword), "namespace" | "module" | "enum" | "interface" | "type" | "abstract") {
      return;
    }
    self.blank_ranges.push(to_text_index(export.start())..to_text_index(keyword.start()));
    // without `export`, swc doesn't allow a line break after the keyword (ex. `type\nT = string`)
    self.blank_ranges.push(to_text_index(keyword.end())..to_text_index(after_keyword.start()));
  }

  fn token_text(&self, token: &TokenAndSpan) -> &str {
    &self.text[to_text_index(token.start())..to_text_index(token.end())]
  }
}

/// Parses the input only for its syntax errors, which skips capturing the tokens and comments.
fn parse_errors_only(input: StringInput, syntax: Syntax, mode: ParseMode) -> std::result::Result<Vec<SwcError>, SwcError> {
  let lexer = dprint_swc_ext::swc::lexer::Lexer::new(syntax, ES_VERSION, input, None);
  let mut parser = dprint_swc_ext::swc::lexer::Parser::new_from(lexer);
  match mode {
    ParseMode::Program => drop(parser.parse_program()?),
    ParseMode::Module => drop(parser.parse_module()?),
    ParseMode::Script => drop(parser.parse_script()?),
  }
  Ok(parser.take_errors())
}

fn to_text_index(pos: SourcePos) -> usize {
  pos - StartSourcePos::START_SOURCE_POS
}

#[cfg(test)]
mod test {
  use dprint_swc_ext::common::SourceRanged;
  use dprint_swc_ext::swc::parser::TsSyntax;
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
  fn errors_for_diagnostics_swc_drops_in_speculatively_parsed_exports() {
    // swc parses these speculatively and drops the errors it recovers from in
    // them, but they lose text the same as without the `export` keyword
    let err = parse_ts("export namespace Foo {\n  const x = 1;\n").err().unwrap();
    assert_eq!(
      err.to_string(),
      concat!(
        "Expected '}', got '<eof>' at file:///my_file.ts:2:13\n\n",
        "    const x = 1;\n",
        "              ~"
      )
    );
    for (text, message) in [
      ("export module Foo {\n  const x = 1;\n", "Expected '}', got '<eof>'"),
      ("export namespace A.B {\n  const x = 1;\n", "Expected '}', got '<eof>'"),
      ("export namespace\nFoo {\n  const x = 1;\n", "Expected '}', got '<eof>'"),
      ("export namespace Foo {\n  function f() {\n}\n", "Expected '}', got '<eof>'"),
      ("export namespace Foo {\n  const a = 1 const b = 2;\n}\n", "Expected a semicolon"),
      ("export interface I {\n  a: 'test\n}\n", "Unterminated string constant"),
      ("export type T = 'test\n", "Unterminated string constant"),
      ("export enum E {\n  A = 'test\n}\n", "Unterminated string constant"),
      ("export abstract class X {\n  a = 'test\n}\n", "Unterminated string constant"),
      ("@dec export abstract class X {\n  a = 'test\n}\n", "Unterminated string constant"),
      // in a script
      ("namespace A {\n  export namespace B {\n    'test;\n  }\n}\n", "Unterminated string constant"),
      // nested in another speculatively parsed declaration
      (
        "export namespace A {\n  export namespace B {\n    'test;\n  }\n}\n",
        "Unterminated string constant",
      ),
      (
        "declare module 'x' {\n  export namespace B {\n    'test;\n  }\n}\n",
        "Unterminated string constant",
      ),
    ] {
      let err = parse_ts(text).err().unwrap_or_else(|| panic!("expected an error for {text:?}"));
      assert!(err.to_string().starts_with(message), "unexpected error for {text:?}: {err}");
    }
  }

  #[test]
  fn keeps_speculatively_parsed_exports_that_have_no_diagnostics() {
    // blanking out the `export` keyword to re-parse these must not cause errors
    for text in [
      "export type\nT = keyof X;",
      "export abstract\nclass X {}",
      "export interface\nI {}",
      "export namespace\nFoo {}",
      "export module\nFoo {}",
      "export enum\nE {}",
      "export /* a */ namespace /* b\n */ Foo {}",
      "@dec export abstract class X {}",
      "@dec\nexport abstract class X {}",
      "export type { A } from './a';\nexport type * from './b';",
      "export namespace Foo {\n  export namespace Bar {}\n}",
    ] {
      let parsed_source = parse_ts(text).unwrap();
      let diagnostics = parsed_source.diagnostics();
      assert!(diagnostics.is_empty(), "unexpected diagnostics for {text:?}: {diagnostics:?}");
    }
  }

  #[test]
  fn format_errors_for_missing_brace_swc_does_not_report() {
    // a program parsed elsewhere (ex. by deno_ast) still has swc drop this error,
    // so generation must error instead of panicking
    let text = "export namespace Foo {\n  const x = 1;\n";
    let syntax = Syntax::Typescript(Default::default());
    let input = StringInput::new(
      text,
      StartSourcePos::START_SOURCE_POS.as_byte_pos(),
      (StartSourcePos::START_SOURCE_POS + text.len()).as_byte_pos(),
    );
    let (comments, program, tokens, errors) = parse_string_input(input, syntax, ParseMode::Program).unwrap();
    assert!(errors.is_empty());
    let parsed_source = ParsedSource {
      specifier: "file:///my_file.ts".into(),
      text: text.into(),
      syntax,
      text_info: Default::default(),
      program,
      comments: ParsedComments::from_single_threaded(comments),
      tokens,
      diagnostics: Vec::new(),
    };
    let config = crate::configuration::ConfigurationBuilder::new().build();
    let err = parsed_source
      .with_view(|program| {
        crate::format_program(crate::format_text::FormatProgramOptions {
          program,
          syntax,
          config: &config,
          external_formatter: None,
        })
      })
      .err()
      .unwrap();
    assert_eq!(
      err.to_string(),
      "Expected to find the open and close brace tokens of the body starting on line 1."
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
      // the same as for a .ts file
      Syntax::Typescript(TsSyntax {
        decorators: true,
        ..Default::default()
      }),
      ParseMode::Program,
    )
  }
}
