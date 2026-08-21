use std::path::Path;
use std::sync::Arc;

use super::parse_syntax;
use super::MediaType;
use super::ParsedSource;
use crate::Result;

pub struct ParseOptions<'a> {
  /// Path of the file, which determines the syntax used when parsing.
  pub path: &'a Path,
  /// Extension to use instead of the one on the path, if any.
  pub extension: Option<&'a str>,
  /// Text of the file. Any byte order mark is stripped before parsing.
  pub text: Arc<str>,
}

/// Parses a file for formatting.
///
/// Errors when the text could not be parsed at all, and also when swc recovered
/// from a syntax error that would stop the AST from representing the original
/// text (see [`is_unsupported_syntax_error`](super::is_unsupported_syntax_error)).
/// Any remaining recovered errors are available on [`ParsedSource::diagnostics`].
pub fn parse_program(options: ParseOptions) -> Result<ParsedSource> {
  let ParseOptions { path, extension, text } = options;
  parse_swc_ast(path, extension, text)
}

fn parse_swc_ast(file_path: &Path, file_extension: Option<&str>, file_text: Arc<str>) -> Result<ParsedSource> {
  match parse_inner(file_path, file_extension, file_text.clone()) {
    Ok(result) => Ok(result),
    Err(err) => {
      let extension = file_extension.or_else(|| file_path.extension().and_then(|e| e.to_str()));
      let matches = |candidates: &[&str]| candidates.iter().any(|c| extension.is_some_and(|e| e.eq_ignore_ascii_case(c)));
      let new_file_path = if matches(&["ts", "cts", "mts"]) {
        file_path.with_extension("tsx")
      } else if matches(&["js", "cjs", "mjs"]) {
        file_path.with_extension("jsx")
      } else {
        return Err(err);
      };
      // try to parse as jsx
      match parse_inner(&new_file_path, None, file_text) {
        Ok(result) => Ok(result),
        Err(_) => Err(err), // return the original error
      }
    }
  }
}

fn parse_inner(file_path: &Path, file_extension: Option<&str>, text: Arc<str>) -> Result<ParsedSource> {
  let media_type = if let Some(file_extension) = file_extension {
    MediaType::from_path(&file_path.with_extension(file_extension))
  } else {
    MediaType::from_path(file_path)
  };

  parse_syntax(path_to_specifier(file_path), text, media_type.syntax(), media_type.parse_mode())
}

/// Creates a `file:` url for the path, which is only used for display
/// purposes in diagnostics.
fn path_to_specifier(path: &Path) -> Arc<str> {
  use std::fmt::Write;

  let mut specifier = String::from("file:///");
  let start_len = specifier.len();
  for component in path.components() {
    let part = match component {
      std::path::Component::Prefix(prefix) => prefix.as_os_str().to_string_lossy(),
      std::path::Component::Normal(part) => part.to_string_lossy(),
      std::path::Component::RootDir => continue,
      std::path::Component::CurDir | std::path::Component::ParentDir => {
        // being lazy because this doesn't need to be exactly correct
        specifier.truncate(start_len);
        continue;
      }
    };
    if specifier.len() > start_len {
      specifier.push('/');
    }
    // ignore the error because writing to a string can't fail
    let _ = write!(
      specifier,
      "{}",
      percent_encoding::utf8_percent_encode(part.as_ref(), percent_encoding::CONTROLS)
    );
  }
  specifier.into()
}

#[cfg(test)]
mod tests {
  use pretty_assertions::assert_eq;

  use super::*;
  use std::path::PathBuf;

  #[test]
  fn test_path_to_specifier() {
    fn run_test(path: &str, expected: &str) {
      assert_eq!(path_to_specifier(&PathBuf::from(path)).as_ref(), expected);
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
