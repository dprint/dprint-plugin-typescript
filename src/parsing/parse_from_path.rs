use std::path::Path;
use std::sync::Arc;

use dprint_swc_ext::swc::parser::EsSyntax;
use dprint_swc_ext::swc::parser::Syntax;
use dprint_swc_ext::swc::parser::TsSyntax;

use super::parse_syntax;
use super::ParseMode;
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
  match parse_inner(path, extension, text.clone()) {
    Ok(result) => Ok(result),
    Err(err) => {
      // a file may contain jsx despite its extension, so try again as jsx
      let jsx_extension = match normalize_extension(resolve_extension(path, extension)) {
        "ts" | "cts" | "mts" => "tsx",
        "js" | "cjs" | "mjs" => "jsx",
        _ => return Err(err),
      };
      match parse_inner(path, Some(jsx_extension), text) {
        Ok(result) => Ok(result),
        Err(_) => Err(err), // return the original error
      }
    }
  }
}

fn parse_inner(file_path: &Path, file_extension: Option<&str>, text: Arc<str>) -> Result<ParsedSource> {
  let (syntax, mode) = resolve_syntax(file_path, file_extension);
  parse_syntax(path_to_specifier(file_path), text, syntax, mode)
}

/// Resolves the syntax and parse mode to use for a file.
///
/// This matches what `deno_ast` resolves for the media types this crate formats.
fn resolve_syntax(file_path: &Path, file_extension: Option<&str>) -> (Syntax, ParseMode) {
  match normalize_extension(resolve_extension(file_path, file_extension)) {
    extension @ ("ts" | "tsx" | "mts" | "cts") => {
      // ex. `file.d.ts`. Note that `with_extension` never changes the file stem,
      // so this is still correct when the extension was overwritten.
      let is_declaration = extension != "tsx" && has_declaration_file_stem(file_path);
      // jsx-like syntax is reserved in .mts and .cts files:
      // https://babeljs.io/docs/babel-preset-typescript#disallowambiguousjsxlike
      let is_module_only = matches!(extension, "mts" | "cts");
      let syntax = Syntax::Typescript(TsSyntax {
        decorators: true,
        disallow_ambiguous_jsx_like: is_module_only,
        dts: is_declaration,
        tsx: extension == "tsx",
        no_early_errors: false,
      });
      // cts files can contain module declarations like
      // `import x = require("./x.ts");` or `export = 5;`, so they need to be
      // parsed as modules (this may change in the future once
      // https://github.com/swc-project/swc/issues/9694 is resolved)
      let mode = if is_module_only && !is_declaration {
        ParseMode::Module
      } else {
        ParseMode::Program
      };
      (syntax, mode)
    }
    // anything else is parsed as javascript
    extension => {
      let syntax = Syntax::Es(EsSyntax {
        allow_return_outside_function: true,
        allow_super_outside_method: true,
        auto_accessors: true,
        decorators: true,
        decorators_before_export: false,
        export_default_from: true,
        fn_bind: false,
        import_attributes: true,
        jsx: extension == "jsx",
        explicit_resource_management: true,
      });
      let mode = match extension {
        "mjs" => ParseMode::Module,
        "cjs" => ParseMode::Script,
        _ => ParseMode::Program,
      };
      (syntax, mode)
    }
  }
}

fn resolve_extension<'a>(file_path: &'a Path, file_extension: Option<&'a str>) -> &'a str {
  file_extension.or_else(|| file_path.extension().and_then(|e| e.to_str())).unwrap_or_default()
}

/// Matches the extension against the ones this crate knows about, so that the
/// rest of the resolving can compare with `==` and stay case insensitive.
fn normalize_extension(extension: &str) -> &'static str {
  const EXTENSIONS: [&str; 8] = ["js", "jsx", "mjs", "cjs", "ts", "tsx", "mts", "cts"];
  EXTENSIONS.iter().copied().find(|e| extension.eq_ignore_ascii_case(e)).unwrap_or("")
}

/// Gets whether the file stem ends with `.d`, as in a `file.d.ts` declaration file.
fn has_declaration_file_stem(file_path: &Path) -> bool {
  let Some(stem) = file_path.file_stem().and_then(|s| s.to_str()) else {
    return false;
  };
  let bytes = stem.as_bytes();
  bytes.len() >= 2 && bytes[bytes.len() - 2] == b'.' && bytes[bytes.len() - 1].eq_ignore_ascii_case(&b'd')
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
    assert_eq!(parse_file(&file_path, None, text).err().unwrap().to_string(), expected);
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
    assert!(parse_file(&file_path, Some("ts"), "const foo: string = 'bar';").is_ok());
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
    assert_eq!(format!("{}", parse_file(&file_path, None, text).err().unwrap()), expected);
  }

  #[track_caller]
  fn parse_file(path: &Path, extension: Option<&str>, text: &str) -> Result<ParsedSource> {
    parse_program(ParseOptions {
      path,
      extension,
      text: text.into(),
    })
  }

  #[test]
  fn resolves_syntax_from_the_extension() {
    #[track_caller]
    fn run_test(path: &str, expected: (Syntax, ParseMode)) {
      assert_eq!(resolve_syntax(&PathBuf::from(path), None), expected, "path: {}", path);
    }

    fn es(jsx: bool) -> Syntax {
      Syntax::Es(EsSyntax {
        allow_return_outside_function: true,
        allow_super_outside_method: true,
        auto_accessors: true,
        decorators: true,
        decorators_before_export: false,
        export_default_from: true,
        fn_bind: false,
        import_attributes: true,
        jsx,
        explicit_resource_management: true,
      })
    }

    fn ts(tsx: bool, dts: bool, disallow_ambiguous_jsx_like: bool) -> Syntax {
      Syntax::Typescript(TsSyntax {
        decorators: true,
        disallow_ambiguous_jsx_like,
        dts,
        tsx,
        no_early_errors: false,
      })
    }

    run_test("t.js", (es(false), ParseMode::Program));
    run_test("t.jsx", (es(true), ParseMode::Program));
    run_test("t.mjs", (es(false), ParseMode::Module));
    run_test("t.cjs", (es(false), ParseMode::Script));
    run_test("t.json", (es(false), ParseMode::Program));
    run_test("t", (es(false), ParseMode::Program));

    run_test("t.ts", (ts(false, false, false), ParseMode::Program));
    run_test("t.TS", (ts(false, false, false), ParseMode::Program));
    run_test("t.tsx", (ts(true, false, false), ParseMode::Program));
    run_test("t.mts", (ts(false, false, true), ParseMode::Module));
    run_test("t.cts", (ts(false, false, true), ParseMode::Module));
    run_test("t.d.ts", (ts(false, true, false), ParseMode::Program));
    run_test("t.D.TS", (ts(false, true, false), ParseMode::Program));
    run_test("t.d.mts", (ts(false, true, true), ParseMode::Program));
    run_test("t.d.cts", (ts(false, true, true), ParseMode::Program));
    // the .d only counts when it is its own part of the file name
    run_test("td.ts", (ts(false, false, false), ParseMode::Program));
  }

  #[test]
  fn extension_overwrites_the_one_on_the_path() {
    let path = PathBuf::from("t.d.js");
    assert_eq!(resolve_syntax(&path, Some("ts")), resolve_syntax(&PathBuf::from("t.d.ts"), None));
  }
}
