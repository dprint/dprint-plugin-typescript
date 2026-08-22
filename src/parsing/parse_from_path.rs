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
      let Some(jsx_extension) = jsx_retry_extension(path, extension) else {
        return Err(err);
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
/// This resolves what `deno_media_type` and `deno_ast::get_syntax` resolve for
/// the media types this crate formats.
fn resolve_syntax(file_path: &Path, file_extension: Option<&str>) -> (Syntax, ParseMode) {
  let file_name = file_path.file_name().and_then(|f| f.to_str()).unwrap_or_default();
  match file_extension {
    // replace the extension on the file name, the same as `Path::with_extension`
    Some(file_extension) => {
      let mut file_name = match file_name.rfind('.') {
        Some(index) => file_name[..=index].to_string(),
        None => format!("{}.", file_name),
      };
      file_name.push_str(file_extension);
      syntax_for_file_name(&file_name)
    }
    None => syntax_for_file_name(file_name),
  }
}

fn syntax_for_file_name(file_name: &str) -> (Syntax, ParseMode) {
  let Some(last_dot_index) = file_name.rfind('.') else {
    return (es_syntax(false), ParseMode::Program);
  };
  let (stem, extension) = file_name.split_at(last_dot_index + 1);
  match normalize_extension(extension) {
    extension @ ("ts" | "mts" | "cts") => {
      // a file containing `.d.` is always a declaration file. See
      // https://github.com/microsoft/TypeScript/issues/53319#issuecomment-1474174018
      let is_declaration = stem.contains(".d.");
      // jsx-like syntax is reserved in .mts and .cts files, but not in their
      // declaration files:
      // https://babeljs.io/docs/babel-preset-typescript#disallowambiguousjsxlike
      let is_module_only = !is_declaration && matches!(extension, "mts" | "cts");
      let syntax = Syntax::Typescript(TsSyntax {
        decorators: true,
        disallow_ambiguous_jsx_like: is_module_only,
        dts: is_declaration,
        tsx: false,
        no_early_errors: false,
      });
      // cts files can contain module declarations like
      // `import x = require("./x.ts");` or `export = 5;`, so they need to be
      // parsed as modules (this may change in the future once
      // https://github.com/swc-project/swc/issues/9694 is resolved)
      let mode = if is_module_only { ParseMode::Module } else { ParseMode::Program };
      (syntax, mode)
    }
    "tsx" => {
      let syntax = Syntax::Typescript(TsSyntax {
        decorators: true,
        disallow_ambiguous_jsx_like: false,
        dts: false,
        tsx: true,
        no_early_errors: false,
      });
      (syntax, ParseMode::Program)
    }
    "jsx" => (es_syntax(true), ParseMode::Program),
    "mjs" => (es_syntax(false), ParseMode::Module),
    "cjs" => (es_syntax(false), ParseMode::Script),
    // "js" and anything this crate doesn't know about are parsed as javascript
    _ => (es_syntax(false), ParseMode::Program),
  }
}

fn es_syntax(jsx: bool) -> Syntax {
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

/// Gets the extension the jsx retry should use, if any.
fn jsx_retry_extension(file_path: &Path, file_extension: Option<&str>) -> Option<&'static str> {
  let extension = match file_extension {
    Some(extension) => extension,
    None => {
      let file_name = file_path.file_name().and_then(|f| f.to_str())?;
      let last_dot_index = file_name.rfind('.')?;
      &file_name[last_dot_index + 1..]
    }
  };
  match normalize_extension(extension) {
    "ts" | "cts" | "mts" => Some("tsx"),
    "js" | "cjs" | "mjs" => Some("jsx"),
    _ => None,
  }
}

/// Matches the extension against the ones this crate knows about, so that the
/// rest of the resolving can compare with `==` and stay case insensitive.
fn normalize_extension(extension: &str) -> &'static str {
  const EXTENSIONS: [&str; 8] = ["js", "jsx", "mjs", "cjs", "ts", "tsx", "mts", "cts"];
  EXTENSIONS.iter().copied().find(|e| extension.eq_ignore_ascii_case(e)).unwrap_or("")
}

/// Creates a `file:` url for the path, which is only used for display
/// purposes in diagnostics.
fn path_to_specifier(path: &Path) -> Arc<str> {
  // matches the set that the `url` crate encodes for a path, so that the
  // result stays a valid url
  const ENCODE_SET: percent_encoding::AsciiSet = percent_encoding::CONTROLS
    .add(b' ')
    .add(b'"')
    .add(b'#')
    .add(b'<')
    .add(b'>')
    .add(b'`')
    .add(b'?')
    .add(b'{')
    .add(b'}')
    .add(b'%')
    .add(b'/')
    .add(b'\\');

  fn push_encoded(specifier: &mut String, part: &std::ffi::OsStr) {
    specifier.extend(percent_encoding::percent_encode(part.as_encoded_bytes(), &ENCODE_SET));
  }

  let mut specifier = String::from("file:///");
  let start_len = specifier.len();
  for component in path.components() {
    match component {
      std::path::Component::Prefix(prefix) => push_encoded(&mut specifier, prefix.as_os_str()),
      std::path::Component::Normal(part) => {
        if specifier.len() > start_len {
          specifier.push('/');
        }
        push_encoded(&mut specifier, part);
      }
      std::path::Component::RootDir => {}
      // a relative path has no file url, so fall back to just the file name
      std::path::Component::CurDir | std::path::Component::ParentDir => {
        specifier.truncate(start_len);
        if let Some(file_name) = path.file_name() {
          push_encoded(&mut specifier, file_name);
        }
        return specifier.into();
      }
    }
  }
  specifier.into()
}

#[cfg(test)]
mod tests {
  use pretty_assertions::assert_eq;

  use super::*;
  use std::path::PathBuf;

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
  fn parses_a_file_name_that_is_only_an_extension() {
    // `Path::extension` returns nothing for these, so the file name has to be
    // split on its last dot instead
    assert!(parse_file(Path::new(".ts"), None, "const a: number = 1;").is_ok());
    assert!(parse_file(Path::new(".tsx"), None, "const a = <div />;").is_ok());
  }

  #[test]
  fn parses_with_a_dotted_extension_overwrite() {
    assert!(parse_file(Path::new("t.md"), Some("d.ts"), "declare const a: number;").is_ok());
  }

  #[test]
  fn resolves_syntax_from_the_extension() {
    #[track_caller]
    fn run_test(path: &str, expected: (Syntax, ParseMode)) {
      assert_eq!(resolve_syntax(&PathBuf::from(path), None), expected, "path: {}", path);
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

    run_test("t.js", (es_syntax(false), ParseMode::Program));
    run_test("t.jsx", (es_syntax(true), ParseMode::Program));
    run_test("t.mjs", (es_syntax(false), ParseMode::Module));
    run_test("t.cjs", (es_syntax(false), ParseMode::Script));
    run_test("t.json", (es_syntax(false), ParseMode::Program));
    run_test("t", (es_syntax(false), ParseMode::Program));

    run_test("t.ts", (ts(false, false, false), ParseMode::Program));
    run_test("t.TS", (ts(false, false, false), ParseMode::Program));
    run_test("t.tsx", (ts(true, false, false), ParseMode::Program));
    run_test("t.mts", (ts(false, false, true), ParseMode::Module));
    run_test("t.cts", (ts(false, false, true), ParseMode::Module));

    // jsx-like syntax is only reserved in non-declaration .mts and .cts files
    run_test("t.d.ts", (ts(false, true, false), ParseMode::Program));
    run_test("t.d.mts", (ts(false, true, false), ParseMode::Program));
    run_test("t.d.cts", (ts(false, true, false), ParseMode::Program));
    // a `.d.` anywhere in the name means a declaration file, and it's case sensitive
    run_test("t.d.css.ts", (ts(false, true, false), ParseMode::Program));
    run_test("t.D.TS", (ts(false, false, false), ParseMode::Program));
    run_test("td.ts", (ts(false, false, false), ParseMode::Program));
    // `.d.tsx` is not a declaration file
    run_test("t.d.tsx", (ts(true, false, false), ParseMode::Program));

    // a file name that is only an extension
    run_test(".ts", (ts(false, false, false), ParseMode::Program));
    run_test(".tsx", (ts(true, false, false), ParseMode::Program));
    run_test(".js", (es_syntax(false), ParseMode::Program));
  }

  #[test]
  fn extension_overwrites_the_one_on_the_path() {
    #[track_caller]
    fn run_test(path: &str, extension: &str, expected_same_as: &str) {
      assert_eq!(
        resolve_syntax(&PathBuf::from(path), Some(extension)),
        resolve_syntax(&PathBuf::from(expected_same_as), None),
        "path: {}, extension: {}",
        path,
        extension
      );
    }

    run_test("t.js", "ts", "t.ts");
    run_test("t.d.js", "ts", "t.d.ts");
    // an extension may contain dots, the same as `Path::with_extension`
    run_test("t.md", "d.ts", "t.d.ts");
    run_test("t", "ts", "t.ts");
  }

  #[test]
  fn creates_a_specifier_for_display() {
    #[track_caller]
    fn run_test(path: &str, expected: &str) {
      assert_eq!(path_to_specifier(&PathBuf::from(path)).as_ref(), expected, "path: {}", path);
    }

    #[cfg(windows)]
    run_test(r"C:\Users\user\file.ts", "file:///C:/Users/user/file.ts");
    run_test("/file/other.ts", "file:///file/other.ts");
    // a relative path has no file url, so just the file name is used
    run_test("./test.ts", "file:///test.ts");
    run_test("../up/test.ts", "file:///test.ts");
    // the result has to stay a valid url
    run_test("/my file.ts", "file:///my%20file.ts");
    run_test("/a#b.ts", "file:///a%23b.ts");
  }
}
