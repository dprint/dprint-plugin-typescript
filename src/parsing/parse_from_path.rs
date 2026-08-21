use std::path::Path;
use std::sync::Arc;

use crate::parsing::parse_program;
use crate::parsing::ParseOptions;
use crate::parsing::ParsedSource;
use crate::MediaType;
use crate::Result;

pub fn parse_swc_ast(file_path: &Path, file_extension: Option<&str>, file_text: Arc<str>) -> Result<(ParsedSource, MediaType)> {
  match parse_inner(file_path, file_extension, file_text.clone()) {
    Ok(result) => Ok(result),
    Err(err) => {
      let lowercase_ext = file_extension.map(|ext| ext.to_string()).or_else(|| get_lowercase_extension(file_path));
      let new_file_path = match lowercase_ext.as_deref() {
        Some("ts") | Some("cts") | Some("mts") => file_path.with_extension("tsx"),
        Some("js") | Some("cjs") | Some("mjs") => file_path.with_extension("jsx"),
        _ => return Err(err),
      };
      // try to parse as jsx
      match parse_inner(&new_file_path, None, file_text) {
        Ok(result) => Ok(result),
        Err(_) => Err(err), // return the original error
      }
    }
  }
}

fn parse_inner(file_path: &Path, file_extension: Option<&str>, text: Arc<str>) -> Result<(ParsedSource, MediaType)> {
  let media_type = if let Some(file_extension) = file_extension {
    MediaType::from_path(&file_path.with_extension(file_extension))
  } else {
    MediaType::from_path(file_path)
  };

  let parsed_source = parse_program(ParseOptions {
    specifier: path_to_specifier(file_path),
    text,
    media_type,
  })?;
  Ok((parsed_source, media_type))
}

/// Creates a `file:` url for the path, which is only used for display
/// purposes in diagnostics.
fn path_to_specifier(path: &Path) -> String {
  fn encode(text: &str) -> String {
    percent_encoding::utf8_percent_encode(text, percent_encoding::CONTROLS).to_string()
  }

  let mut parts = Vec::new();
  for component in path.components() {
    match component {
      std::path::Component::Prefix(prefix) => {
        parts.push(encode(prefix.as_os_str().to_string_lossy().as_ref()));
      }
      std::path::Component::RootDir => {
        // ignore
      }
      std::path::Component::CurDir | std::path::Component::ParentDir => {
        // being lazy because this doesn't need to be exactly correct
        parts.clear();
      }
      std::path::Component::Normal(part) => {
        parts.push(encode(part.to_string_lossy().as_ref()));
      }
    }
  }
  format!("file:///{}", parts.join("/"))
}

fn get_lowercase_extension(file_path: &Path) -> Option<String> {
  file_path.extension().and_then(|e| e.to_str()).map(|f| f.to_lowercase())
}

#[cfg(test)]
mod tests {
  use pretty_assertions::assert_eq;

  use super::*;
  use std::path::PathBuf;

  #[test]
  fn test_path_to_specifier() {
    fn run_test(path: &str, expected: &str) {
      assert_eq!(path_to_specifier(&PathBuf::from(path)), expected);
    }

    #[cfg(windows)]
    run_test("C:\\Users\\user\\file.ts", "file:///C:/Users/user/file.ts");
    run_test("/file/other.ts", "file:///file/other.ts");
    run_test("./test.ts", "file:///test.ts");
  }

  #[test]
  fn should_error_on_syntax_diagnostic() {
    run_fatal_diagnostic_test(
      "./test.ts",
      "test;\nas#;",
      concat!("Expected ';', '}' or <eof> at file:///test.ts:2:3\n", "\n", "  as#;\n", "    ~"),
    );
  }

  #[test]
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
  fn should_error_on_unary_expression_dot_semicolon() {
    // issue #391
    run_fatal_diagnostic_test(
      "./test.ts",
      "+value.;",
      concat!("Expected ident at file:///test.ts:1:8\n\n", "  +value.;\n", "         ~"),
    );
  }

  #[test]
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
    let file_path = PathBuf::from(file_path);
    assert_eq!(parse_swc_ast(&file_path, None, text.into()).err().unwrap().to_string(), expected);
  }

  #[test]
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
  fn it_should_error_for_exected_expr_issue_121() {
    run_non_fatal_diagnostic_test(
      "./test.ts",
      "type T =\n  | unknown\n  { } & unknown;",
      concat!("Expression expected at file:///test.ts:3:7\n\n", "    { } & unknown;\n", "        ~"),
    );
  }

  #[test]
  fn file_extension_overwrite() {
    let file_path = PathBuf::from("./test.js");
    assert!(parse_swc_ast(&file_path, Some("ts"), "const foo: string = 'bar';".into()).is_ok());
  }

  #[test]
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

  /// These are syntax errors that swc recovers from, but which stop the ast
  /// from representing the original text, so formatting is refused.
  #[track_caller]
  fn run_non_fatal_diagnostic_test(file_path: &str, text: &str, expected: &str) {
    let file_path = PathBuf::from(file_path);
    assert_eq!(format!("{}", parse_swc_ast(&file_path, None, text.into()).err().unwrap()), expected);
  }
}
