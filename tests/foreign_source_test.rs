//! Verifies that a program parsed by another crate (ex. `deno_ast`) can be
//! handed to this crate without it having to parse the text again.

use dprint_core::formatting::Arena;
use dprint_plugin_typescript::FormatProgramOptions;
use dprint_plugin_typescript::configuration::ConfigurationBuilder;
use dprint_plugin_typescript::parsing::parse_options;
use oxc_allocator::Allocator;
use oxc_parser::Parser;
use oxc_parser::config::TokensParserConfig;
use oxc_span::SourceType;

#[test]
fn formats_a_foreign_parsed_program() {
  let result = format_foreign("const  t  =  5 ; // comment", true).unwrap().unwrap();
  assert_eq!(result, "const t = 5; // comment\n");
}

#[test]
fn respects_the_ignore_file_comment() {
  let result = format_foreign("// dprint-ignore-file\nconst  t  =  5 ;", true).unwrap();
  assert_eq!(result, None);
}

#[test]
fn errors_when_the_tokens_were_not_collected() {
  let err = format_foreign("const t = 5;", false).err().unwrap();
  assert_eq!(err.to_string(), "The tokens must be collected in order to format a program.");
}

/// Parses the text the way another crate would, then formats it.
fn format_foreign(text: &str, collect_tokens: bool) -> Result<Option<String>, dprint_plugin_typescript::FormatError> {
  let allocator = Allocator::default();
  let parser = Parser::new(&allocator, text, SourceType::ts()).with_options(parse_options());
  let config = ConfigurationBuilder::new().build();
  let arena = Arena::new();
  if collect_tokens {
    let ret = parser.with_config(TokensParserConfig).parse();
    dprint_plugin_typescript::format_program(FormatProgramOptions {
      program: &ret.program,
      tokens: &ret.tokens,
      config: &config,
      external_formatter: None,
      arena: &arena,
    })
  } else {
    let ret = parser.parse();
    dprint_plugin_typescript::format_program(FormatProgramOptions {
      program: &ret.program,
      tokens: &[],
      config: &config,
      external_formatter: None,
      arena: &arena,
    })
  }
}
