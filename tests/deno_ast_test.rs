//! Verifies that someone using `deno_ast` can hand an already parsed
//! AST (with comments) to this crate without it having to reparse.

use deno_ast::MediaType as DenoMediaType;
use deno_ast::ModuleSpecifier;
use deno_ast::ParseParams;
use dprint_plugin_typescript::configuration::ConfigurationBuilder;
use dprint_plugin_typescript::FormatProgramOptions;
use dprint_plugin_typescript::MediaType;

/// Once `deno_ast::ParsedSource` implements `ProgramInfoProvider`, this
/// becomes a single `format_parsed_source` call with `source: &parsed_source`.
#[test]
fn formats_a_deno_ast_parsed_source() {
  let parsed_source = parse("file:///test.ts", "const  t  =  5 ; // comment");
  let config = ConfigurationBuilder::new().build();
  let result = parsed_source
    .with_view(|program| {
      dprint_plugin_typescript::format_program(FormatProgramOptions {
        program,
        media_type: MediaType::TypeScript,
        config: &config,
        external_formatter: None,
      })
    })
    .unwrap()
    .unwrap();
  assert_eq!(
    result,
    "const t = 5; // comment
"
  );
}

#[test]
fn respects_the_ignore_file_comment() {
  let parsed_source = parse(
    "file:///test.ts",
    "// dprint-ignore-file
const  t  =  5 ;",
  );
  let config = ConfigurationBuilder::new().build();
  let result = parsed_source
    .with_view(|program| {
      dprint_plugin_typescript::format_program(FormatProgramOptions {
        program,
        media_type: MediaType::TypeScript,
        config: &config,
        external_formatter: None,
      })
    })
    .unwrap();
  assert_eq!(result, None);
}

#[test]
fn caller_can_check_for_unsupported_syntax_errors() {
  // this crate refuses to format these because swc recovered from them in a way
  // that loses text, but for a source it didn't parse it's up to the caller to check
  let parsed_source = parse("file:///test.ts", "var foo = 'test");
  let unsupported = parsed_source
    .diagnostics()
    .iter()
    .filter(|d| dprint_plugin_typescript::is_unsupported_syntax_error(d.kind()))
    .count();
  assert_eq!(unsupported, 1);
}

fn parse(specifier: &str, text: &str) -> deno_ast::ParsedSource {
  deno_ast::parse_program(ParseParams {
    specifier: ModuleSpecifier::parse(specifier).unwrap(),
    text: text.into(),
    media_type: DenoMediaType::TypeScript,
    capture_tokens: true,
    maybe_syntax: None,
    scope_analysis: false,
  })
  .unwrap()
}
