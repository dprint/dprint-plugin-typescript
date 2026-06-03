use anyhow::Result;
use anyhow::anyhow;
use anyhow::bail;
use deno_ast::ModuleSpecifier;
use deno_ast::ParseParams;
use deno_ast::ParsedSource;
use deno_ast::oxc::allocator::Allocator;
use std::path::Path;
use std::sync::Arc;

pub fn parse_ast<'a>(allocator: &'a Allocator, file_path: &Path, file_extension: Option<&str>, file_text: Arc<str>) -> Result<ParsedSource<'a>> {
  match parse_inner(allocator, file_path, file_extension, file_text.clone()) {
    Ok(result) => Ok(result),
    Err(err) => {
      let lowercase_ext = file_extension.map(|ext| ext.to_string()).or_else(|| get_lowercase_extension(file_path));
      let new_file_path = match lowercase_ext.as_deref() {
        Some("ts") | Some("cts") | Some("mts") => file_path.with_extension("tsx"),
        Some("js") | Some("cjs") | Some("mjs") => file_path.with_extension("jsx"),
        _ => return Err(err),
      };
      // try to parse as jsx
      match parse_inner(allocator, &new_file_path, None, file_text) {
        Ok(result) => Ok(result),
        Err(_) => Err(err), // return the original error
      }
    }
  }
}

fn parse_inner<'a>(allocator: &'a Allocator, file_path: &Path, file_extension: Option<&str>, text: Arc<str>) -> Result<ParsedSource<'a>> {
  let parsed_source = parse_inner_no_diagnostic_check(allocator, file_path, file_extension, text)?;
  ensure_no_specific_syntax_errors(&parsed_source)?;
  Ok(parsed_source)
}

fn parse_inner_no_diagnostic_check<'a>(allocator: &'a Allocator, file_path: &Path, file_extension: Option<&str>, text: Arc<str>) -> Result<ParsedSource<'a>> {
  let media_type = if let Some(file_extension) = file_extension {
    deno_ast::MediaType::from_path(&file_path.with_extension(file_extension))
  } else {
    deno_ast::MediaType::from_path(file_path)
  };

  deno_ast::parse_program(
    allocator,
    ParseParams {
      specifier: path_to_specifier(file_path)?,
      capture_tokens: true,
      maybe_source_type: None,
      media_type,
      scope_analysis: false,
      text,
    },
  )
  .map_err(|diagnostic| anyhow!("{}", &diagnostic))
}

fn path_to_specifier(path: &Path) -> Result<ModuleSpecifier> {
  if let Some(specifier) = from_file_path(path) {
    Ok(specifier)
  } else if let Some(file_name) = path.file_name() {
    match ModuleSpecifier::parse(&format!("file:///{}", file_name.to_string_lossy())) {
      Ok(specifier) => Ok(specifier),
      Err(err) => bail!("could not convert path to specifier: '{}', error: {:#}", path.display(), err),
    }
  } else {
    bail!("could not convert path to specifier: '{}'", path.display())
  }
}

fn from_file_path(path: &Path) -> Option<ModuleSpecifier> {
  #[cfg(target_arch = "wasm32")]
  {
    from_file_path_wasm(path)
  }
  #[cfg(not(target_arch = "wasm32"))]
  {
    ModuleSpecifier::from_file_path(path).ok()
  }
}

#[allow(unused)]
#[cfg(any(target_arch = "wasm32", test))]
fn from_file_path_wasm(path: &Path) -> Option<ModuleSpecifier> {
  // being lazy because this doesn't need to be exactly correct
  let mut parts = Vec::new();
  for component in path.components() {
    match component {
      std::path::Component::Prefix(prefix) => {
        let prefix = prefix.as_os_str().to_string_lossy();
        parts.push(percent_encoding::utf8_percent_encode(prefix.as_ref(), percent_encoding::CONTROLS).to_string());
      }
      std::path::Component::RootDir => {
        // ignore
      }
      std::path::Component::CurDir | std::path::Component::ParentDir => {
        return None;
      }
      std::path::Component::Normal(part) => {
        parts.push(percent_encoding::percent_encode(part.as_encoded_bytes(), percent_encoding::CONTROLS).to_string());
      }
    }
  }
  ModuleSpecifier::parse(&format!("file:///{}", parts.join("/"))).ok()
}

/// Surfaces blocking parse diagnostics as a formatting error.
///
/// The formatter intentionally runs on some legacy spec inputs that SWC parsed
/// even though they are invalid TypeScript/JavaScript. Oxc keeps a recovered AST
/// for these diagnostics, so allow the known recoverable cases and keep blocking
/// everything else.
pub fn ensure_no_specific_syntax_errors(parsed_source: &ParsedSource) -> Result<()> {
  let diagnostics = parsed_source.diagnostics();

  let blocking_diagnostics = diagnostics
    .iter()
    .filter(|diagnostic| !is_recoverable_parse_diagnostic(diagnostic))
    .collect::<Vec<_>>();

  if blocking_diagnostics.is_empty() {
    Ok(())
  } else {
    let mut final_message = String::new();
    for diagnostic in blocking_diagnostics {
      if !final_message.is_empty() {
        final_message.push_str("\n\n");
      }
      final_message.push_str(&format!("{diagnostic}"));
    }
    bail!("{}", final_message)
  }
}

fn is_recoverable_parse_diagnostic(diagnostic: &deno_ast::ParseDiagnostic) -> bool {
  if let Some(code) = diagnostic.diagnostic_code() {
    return matches!(
      (code.scope(), code.number()),
      (
        Some("TS"),
        Some(
          "1049" // setter must have exactly one parameter
            | "1095" // setter return type
            | "1108" // top-level return in legacy statement fixtures
            | "1243" // invalid modifier combination
            | "2206" // redundant `type` in `import type`
            | "2207" // redundant `type` in `export type`
        )
      )
    );
  }

  // Oxc currently does not assign codes to all parser diagnostics. Keep these
  // exact-message fallbacks local to the formatter: they are legacy spec inputs
  // that SWC accepted and oxc can still recover an AST for.
  matches!(
    diagnostic.message(),
    "Missing initializer in const declaration"
      | "Using declarations must have an initializer."
      | "The left-hand side of a for...in statement cannot be an using declaration."
      | "The left-hand side of a for...in statement cannot be an await using declaration."
      | "Invalid assignment in object literal"
      | "`await` is only allowed within async functions and at the top levels of modules"
  )
}

fn get_lowercase_extension(file_path: &Path) -> Option<String> {
  file_path.extension().and_then(|e| e.to_str()).map(|f| f.to_lowercase())
}

#[cfg(test)]
mod tests {
  use crate::configuration::ConfigurationBuilder;
  use deno_ast::oxc::allocator::Allocator;
  use pretty_assertions::assert_eq;

  use super::*;
  use std::path::PathBuf;

  #[test]
  fn test_from_file_path_wasm() {
    fn run_test(path: &str, expected: Option<&str>) {
      let actual = from_file_path_wasm(&PathBuf::from(path));
      assert_eq!(actual.as_ref().map(|d| d.as_str()), expected);
    }

    run_test("C:\\Users\\user\\file.ts", Some("file:///C:/Users/user/file.ts"));
    run_test("/file/other.ts", Some("file:///file/other.ts"));
  }

  // todo(oxc-port): the exact diagnostic message/snippet formatting differs from
  // SWC. These tests assert SWC-specific strings and need to be updated to oxc's
  // messages once the parse layer is validated against the spec suite.
  #[test]
  #[ignore = "oxc-port: diagnostic messages differ from SWC"]
  fn should_error_on_syntax_diagnostic() {
    run_fatal_diagnostic_test(
      "./test.ts",
      "test;\nas#;",
      concat!("Expected ';', '}' or <eof> at file:///test.ts:2:3\n", "\n", "  as#;\n", "    ~"),
    );
  }

  #[test]
  #[ignore = "oxc-port: diagnostic messages differ from SWC"]
  fn should_error_on_unary_expression_dot() {
    // issue #391
    run_fatal_diagnostic_test(
      "./test.ts",
      "+value.",
      concat!(
        // comment to keep this multi-line
        "Expected ident at file:///test.ts:1:2\n\n",
        "  +value.\n",
        "   ~~~~~"
      ),
    );
  }

  #[test]
  #[ignore = "oxc-port: diagnostic messages differ from SWC"]
  fn should_error_on_unary_expression_dot_semicolon() {
    // issue #391
    run_fatal_diagnostic_test(
      "./test.ts",
      "+value.;",
      concat!("Expected ident at file:///test.ts:1:8\n\n", "  +value.;\n", "         ~"),
    );
  }

  #[test]
  #[ignore = "oxc-port: diagnostic messages differ from SWC"]
  fn it_should_error_without_issue_when_there_exists_multi_byte_char_on_line_with_syntax_error() {
    run_fatal_diagnostic_test(
      "./test.ts",
      concat!(
        "test;\n",
        r#"console.log('x', `duration ${d} not in range - ${min} ≥ ${d} && ${max} ≥ ${d}`),;"#,
      ),
      concat!(
        "Expression expected at file:///test.ts:2:81\n",
        "\n",
        "  console.log('x', `duration ${d} not in range - ${min} ≥ ${d} && ${max} ≥ ${d}`),;\n",
        "                                                                                  ~",
      ),
    );
  }

  #[test]
  #[ignore = "oxc-port: diagnostic messages differ from SWC"]
  fn it_should_error_closing_paren_missing() {
    // issue 498
    run_fatal_diagnostic_test(
      "./test.ts",
      r#"const foo = <T extends {}>() => {
    if (bar() {
        console.log(1);
    }
};"#,
      concat!(
        "An arrow function is not allowed here at file:///test.ts:1:27\n",
        "\n",
        "  const foo = <T extends {}>() => {\n",
        "                            ~~"
      ),
    );
  }

  #[test]
  #[ignore = "oxc-port: diagnostic messages differ from SWC"]
  fn it_should_error_when_var_stmts_sep_by_comma() {
    run_fatal_diagnostic_test(
      "./test.ts",
      "let a = 0, let b = 1;",
      concat!(
        "Unexpected token `let`. Expected let is reserved in const, let, class declaration at file:///test.ts:1:12\n",
        "\n",
        "  let a = 0, let b = 1;\n",
        "             ~~~"
      ),
    );
  }

  #[track_caller]
  fn run_fatal_diagnostic_test(file_path: &str, text: &str, expected: &str) {
    let allocator = Allocator::default();
    let file_path = PathBuf::from(file_path);
    assert_eq!(parse_ast(&allocator, &file_path, None, text.into()).err().unwrap().to_string(), expected);
  }

  #[test]
  #[ignore = "oxc-port: diagnostic messages differ from SWC"]
  fn it_should_error_for_no_equals_sign_in_var_decl() {
    run_non_fatal_diagnostic_test(
      "./test.ts",
      "const Methods {\nf: (x, y) => x + y,\n};",
      concat!(
        "Expected a semicolon at file:///test.ts:1:15\n",
        "\n",
        "  const Methods {\n",
        "                ~"
      ),
    );
  }

  #[test]
  #[ignore = "oxc-port: diagnostic messages differ from SWC"]
  fn it_should_error_for_exected_expr_issue_121() {
    run_non_fatal_diagnostic_test(
      "./test.ts",
      "type T =\n  | unknown\n  { } & unknown;",
      concat!("Expression expected at file:///test.ts:3:7\n\n", "    { } & unknown;\n", "        ~"),
    );
  }

  #[test]
  fn file_extension_overwrite() {
    let allocator = Allocator::default();
    let file_path = PathBuf::from("./test.js");
    assert!(parse_ast(&allocator, &file_path, Some("ts"), "const foo: string = 'bar';".into()).is_ok());
  }

  #[test]
  #[ignore = "oxc-port: diagnostic messages differ from SWC"]
  fn it_should_error_for_exected_close_brace() {
    // swc can parse this, but we explicitly fail formatting
    // in this scenario because I believe it might cause more
    // harm than good.
    run_fatal_diagnostic_test(
      "./test.ts",
      "class Test {",
      concat!("Unexpected eof at file:///test.ts:1:13\n\n", "  class Test {\n", "              ~"),
    );
  }

  #[test]
  #[ignore = "oxc-port: diagnostic messages differ from SWC"]
  fn it_should_error_for_exected_string_literal() {
    run_non_fatal_diagnostic_test(
      "./test.ts",
      "var foo = 'test",
      concat!(
        "Unterminated string constant at file:///test.ts:1:11\n\n",
        "  var foo = 'test\n",
        "            ~~~~~"
      ),
    );
  }

  #[test]
  #[ignore = "oxc-port: diagnostic messages differ from SWC"]
  fn it_should_error_for_merge_conflict_marker() {
    run_non_fatal_diagnostic_test(
      "./test.ts",
      r#"class Test {
<<<<<<< HEAD
    v = 1;
=======
    v = 2;
>>>>>>> Branch-a
}
"#,
      r#"Merge conflict marker encountered. at file:///test.ts:2:1

  <<<<<<< HEAD
  ~~~~~~~

Merge conflict marker encountered. at file:///test.ts:4:1

  =======
  ~~~~~~~

Merge conflict marker encountered. at file:///test.ts:6:1

  >>>>>>> Branch-a
  ~~~~~~~"#,
    );
  }

  #[track_caller]
  fn run_non_fatal_diagnostic_test(file_path: &str, text: &str, expected: &str) {
    let allocator = Allocator::default();
    let file_path = PathBuf::from(file_path);
    assert_eq!(format!("{}", parse_ast(&allocator, &file_path, None, text.into()).err().unwrap()), expected);

    // this error should also be surfaced in `format_parsed_source` if someone provides
    // a source file that had a non-fatal diagnostic
    let allocator = Allocator::default();
    let parsed_source = parse_inner_no_diagnostic_check(&allocator, &file_path, None, text.into()).unwrap();
    let config = ConfigurationBuilder::new().build();
    assert_eq!(crate::format_parsed_source(&parsed_source, &config, None).err().unwrap().to_string(), expected);
  }
}
