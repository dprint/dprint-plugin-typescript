//! Verifies that a source parsed by another crate (ex. `deno_ast`) can be
//! handed to this crate without it having to parse the text again.
//!
//! This stands in for such a crate so that the dependency isn't needed here.

use std::sync::Arc;

use dprint_plugin_typescript::configuration::ConfigurationBuilder;
use dprint_plugin_typescript::dprint_swc_ext::common::SourceTextInfo;
use dprint_plugin_typescript::dprint_swc_ext::swc::ast::EsVersion;
use dprint_plugin_typescript::dprint_swc_ext::swc::ast::Module;
use dprint_plugin_typescript::dprint_swc_ext::swc::common::comments::SingleThreadedComments;
use dprint_plugin_typescript::dprint_swc_ext::swc::common::comments::SingleThreadedCommentsMapInner;
use dprint_plugin_typescript::dprint_swc_ext::swc::lexer::common::parser::Parser as _;
use dprint_plugin_typescript::dprint_swc_ext::swc::lexer::Capturing;
use dprint_plugin_typescript::dprint_swc_ext::swc::lexer::Lexer;
use dprint_plugin_typescript::dprint_swc_ext::swc::lexer::Parser;
use dprint_plugin_typescript::dprint_swc_ext::swc::parser::token::TokenAndSpan;
use dprint_plugin_typescript::dprint_swc_ext::swc::parser::Syntax;
use dprint_plugin_typescript::dprint_swc_ext::swc::parser::TsSyntax;
use dprint_plugin_typescript::dprint_swc_ext::view::Comments;
use dprint_plugin_typescript::dprint_swc_ext::view::ProgramInfo;
use dprint_plugin_typescript::dprint_swc_ext::view::ProgramInfoProvider;
use dprint_plugin_typescript::dprint_swc_ext::view::ProgramRef;
use dprint_plugin_typescript::FormatParsedSourceOptions;

#[test]
fn formats_a_foreign_parsed_source() {
  let source = OtherCrateParsedSource::parse("const  t  =  5 ; // comment");
  let config = ConfigurationBuilder::new().build();
  let result = dprint_plugin_typescript::format_parsed_source(FormatParsedSourceOptions {
    source: &source,
    syntax: source.syntax,
    config: &config,
    external_formatter: None,
  })
  .unwrap()
  .unwrap();
  assert_eq!(result, "const t = 5; // comment\n");
}

#[test]
fn respects_the_ignore_file_comment() {
  let source = OtherCrateParsedSource::parse("// dprint-ignore-file\nconst  t  =  5 ;");
  let config = ConfigurationBuilder::new().build();
  let result = dprint_plugin_typescript::format_parsed_source(FormatParsedSourceOptions {
    source: &source,
    syntax: source.syntax,
    config: &config,
    external_formatter: None,
  })
  .unwrap();
  assert_eq!(result, None);
}

#[test]
fn errors_when_the_provider_does_not_capture_everything() {
  struct MissingTokens(OtherCrateParsedSource);

  impl ProgramInfoProvider for MissingTokens {
    fn program_info(&self) -> ProgramInfo<'_> {
      ProgramInfo {
        tokens: None,
        comments: None,
        ..self.0.program_info()
      }
    }
  }

  let source = MissingTokens(OtherCrateParsedSource::parse("const t = 5;"));
  let config = ConfigurationBuilder::new().build();
  let err = dprint_plugin_typescript::format_parsed_source(FormatParsedSourceOptions {
    source: &source,
    syntax: source.0.syntax,
    config: &config,
    external_formatter: None,
  })
  .err()
  .unwrap();
  assert_eq!(err.to_string(), "The tokens must be captured in order to format a program.");
}

/// Stands in for a parsed source owned by another crate.
struct OtherCrateParsedSource {
  syntax: Syntax,
  module: Module,
  text_info: SourceTextInfo,
  tokens: Vec<TokenAndSpan>,
  leading: SingleThreadedCommentsMapInner,
  trailing: SingleThreadedCommentsMapInner,
}

impl ProgramInfoProvider for OtherCrateParsedSource {
  fn program_info(&self) -> ProgramInfo<'_> {
    ProgramInfo {
      program: ProgramRef::Module(&self.module),
      text_info: Some(&self.text_info),
      tokens: Some(&self.tokens),
      comments: Some(Comments {
        leading: &self.leading,
        trailing: &self.trailing,
      }),
    }
  }
}

impl OtherCrateParsedSource {
  pub fn parse(text: &str) -> Self {
    let syntax = Syntax::Typescript(TsSyntax::default());
    let text_info = SourceTextInfo::new(Arc::from(text));
    let comments = SingleThreadedComments::default();
    let lexer = Capturing::new(Lexer::new(syntax, EsVersion::Es2021, text_info.as_string_input(), Some(&comments)));
    let mut parser = Parser::new_from(lexer);
    let module = parser.parse_module().unwrap();
    let tokens = Capturing::take(&mut parser.input_mut().iter);
    let (leading, trailing) = comments.take_all();
    Self {
      syntax,
      module,
      text_info,
      tokens,
      leading: std::rc::Rc::try_unwrap(leading).unwrap().into_inner(),
      trailing: std::rc::Rc::try_unwrap(trailing).unwrap().into_inner(),
    }
  }
}
