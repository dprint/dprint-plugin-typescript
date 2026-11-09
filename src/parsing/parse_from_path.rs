use std::path::Path;
use std::sync::Arc;

use oxc_span::SourceType;

use super::ParsedSource;
use super::parse_source_type;
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
/// Errors when the text could not be parsed. Any errors the parser recovered
/// from are available on [`ParsedSource::diagnostics`].
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
  let source_type = resolve_source_type(file_path, file_extension);
  parse_source_type(path_to_specifier(file_path), text, source_type)
}

/// Resolves the source type to use for a file.
fn resolve_source_type(file_path: &Path, file_extension: Option<&str>) -> SourceType {
  let file_name = file_path.file_name().and_then(|f| f.to_str()).unwrap_or_default();
  match file_extension {
    // replace the extension on the file name, the same as `Path::with_extension`
    Some(file_extension) => {
      let mut file_name = match file_name.rfind('.') {
        Some(index) => file_name[..=index].to_string(),
        None => format!("{}.", file_name),
      };
      file_name.push_str(file_extension);
      source_type_for_file_name(&file_name)
    }
    None => source_type_for_file_name(file_name),
  }
}

fn source_type_for_file_name(file_name: &str) -> SourceType {
  let Some((stem, extension)) = split_known_extension(file_name) else {
    return SourceType::from_extension("js").unwrap();
  };
  let extension = if extension.is_empty() { "js" } else { extension };
  let source_type = SourceType::from_extension(extension).unwrap();
  match extension {
    // a file containing `.d.` is always a declaration file. See
    // https://github.com/microsoft/TypeScript/issues/53319#issuecomment-1474174018
    "ts" | "mts" | "cts" => source_type.with_typescript_definition(stem.contains(".d.")),
    _ => source_type,
  }
}

/// Gets the extension the jsx retry should use, if any.
fn jsx_retry_extension(file_path: &Path, file_extension: Option<&str>) -> Option<&'static str> {
  let extension = match file_extension {
    Some(extension) => extension,
    None => {
      let file_name = file_path.file_name().and_then(|f| f.to_str())?;
      split_known_extension(file_name)?.1
    }
  };
  match normalize_extension(extension) {
    "ts" | "cts" | "mts" => Some("tsx"),
    "js" | "cjs" | "mjs" => Some("jsx"),
    _ => None,
  }
}

/// Splits the file name into the stem up to and including the dot before the
/// extension and the normalized extension that decides the syntax.
///
/// When the last extension isn't one this crate knows about, the one before it
/// is used instead. This allows a file associated with this plugin, such as
/// `file.ts.txt`, to still be parsed with the syntax of its inner extension.
fn split_known_extension(file_name: &str) -> Option<(&str, &'static str)> {
  match split_extension(file_name)? {
    (stem, "") => split_extension(&stem[..stem.len() - 1]).filter(|(_, extension)| !extension.is_empty()),
    known => Some(known),
  }
}

/// Splits the file name on its last dot, normalizing the extension.
fn split_extension(file_name: &str) -> Option<(&str, &'static str)> {
  let last_dot_index = file_name.rfind('.')?;
  let (stem, extension) = file_name.split_at(last_dot_index + 1);
  Some((stem, normalize_extension(extension)))
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
      concat!("Invalid Character `;` at file:///test.ts:2:4\n", "\n", "  as#;\n", "     ~"),
    );
  }

  #[test]
  fn should_error_on_unary_expression_dot() {
    // issue #391
    run_fatal_diagnostic_test(
      "./test.ts",
      "+value.",
      concat!("Unexpected token at file:///test.ts:1:8\n\n", "  +value.\n", "         ~"),
    );
  }

  #[test]
  fn should_error_on_unary_expression_dot_semicolon() {
    // issue #391
    run_fatal_diagnostic_test(
      "./test.ts",
      "+value.;",
      concat!("Unexpected token at file:///test.ts:1:8\n\n", "  +value.;\n", "         ~"),
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
        "Unexpected token at file:///test.ts:2:81\n",
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
        "Expected `)` but found `{` at file:///test.ts:2:15\n",
        "\n",
        "      if (bar() {\n",
        "                ~"
      ),
    );
  }

  #[test]
  fn it_should_error_when_var_stmts_sep_by_comma() {
    run_fatal_diagnostic_test(
      "./test.ts",
      "let a = 0, let b = 1;",
      concat!(
        "Expected a semicolon or an implicit semicolon after a statement, but found none at file:///test.ts:1:15\n",
        "\n",
        "  let a = 0, let b = 1;\n",
        "                ~"
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
    run_fatal_diagnostic_test(
      "./test.ts",
      "const Methods {\nf: (x, y) => x + y,\n};",
      concat!(
        "Missing initializer in const declaration at file:///test.ts:1:7\n",
        "\n",
        "  const Methods {\n",
        "        ~~~~~~~\n",
        "\n",
        "Expected a semicolon or an implicit semicolon after a statement, but found none at file:///test.ts:1:14\n",
        "\n",
        "  const Methods {\n",
        "               ~"
      ),
    );
  }

  #[test]
  fn it_should_error_for_exected_expr_issue_121() {
    run_fatal_diagnostic_test(
      "./test.ts",
      "type T =\n  | unknown\n  { } & unknown;",
      concat!("Unexpected token at file:///test.ts:3:7\n\n", "    { } & unknown;\n", "        ~"),
    );
  }

  #[test]
  fn file_extension_overwrite() {
    let file_path = PathBuf::from("./test.js");
    assert!(parse_file(&file_path, Some("ts"), "const foo: string = 'bar';").is_ok());
  }

  #[test]
  fn it_should_error_for_exected_close_brace() {
    run_fatal_diagnostic_test(
      "./test.ts",
      "class Test {",
      concat!(
        "Expected `}` but found `EOF` at file:///test.ts:1:13\n\n",
        "  class Test {\n",
        "              ~"
      ),
    );
  }

  #[test]
  fn it_should_error_for_exected_string_literal() {
    run_non_fatal_diagnostic_test(
      "./test.ts",
      "var foo = 'test",
      concat!("Unterminated string at file:///test.ts:1:11\n\n", "  var foo = 'test\n", "            ~~~~~"),
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
      concat!("Encountered diff marker at file:///test.ts:2:1\n\n", "  <<<<<<< HEAD\n", "  ~~~~~~~"),
    );
  }

  #[test]
  fn it_should_error_for_an_assignment_in_an_object_literal() {
    // the parser recovers from this, but drops the initializer
    run_non_fatal_diagnostic_test(
      "./test.ts",
      "call({ a = 1 });",
      concat!(
        "Invalid assignment in object literal at file:///test.ts:1:8\n\n",
        "  call({ a = 1 });\n",
        "         ~~~~~"
      ),
    );
  }

  /// These are syntax errors that a parser could recover from, but which would
  /// stop the ast from representing the original text, so formatting is refused.
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
  fn parses_a_file_with_an_unknown_extension_after_a_known_one() {
    // issue #637
    assert!(parse_file(Path::new("t.ts.txt"), None, "import { type A } from './a';").is_ok());
    // the jsx retry uses the known extension too
    assert!(parse_file(Path::new("t.ts.txt"), None, "const a: number = <div />;").is_ok());
    assert!(parse_file(Path::new("t.js.txt"), None, "const a = <div />;").is_ok());
  }

  #[test]
  fn parses_with_a_dotted_extension_overwrite() {
    assert!(parse_file(Path::new("t.md"), Some("d.ts"), "declare const a: number;").is_ok());
  }

  #[test]
  fn resolves_source_type_from_the_extension() {
    #[track_caller]
    fn run_test(path: &str, expected: SourceType) {
      assert_eq!(resolve_source_type(&PathBuf::from(path), None), expected, "path: {}", path);
    }

    fn js(extension: &str) -> SourceType {
      SourceType::from_extension(extension).unwrap()
    }

    fn ts(extension: &str, dts: bool) -> SourceType {
      SourceType::from_extension(extension).unwrap().with_typescript_definition(dts)
    }

    run_test("t.js", js("js"));
    run_test("t.jsx", js("jsx"));
    run_test("t.mjs", js("mjs"));
    run_test("t.cjs", js("cjs"));
    run_test("t.json", js("js"));
    run_test("t", js("js"));

    run_test("t.ts", ts("ts", false));
    run_test("t.TS", ts("ts", false));
    run_test("t.tsx", ts("tsx", false));
    run_test("t.mts", ts("mts", false));
    run_test("t.cts", ts("cts", false));

    run_test("t.d.ts", ts("ts", true));
    run_test("t.d.mts", ts("mts", true));
    run_test("t.d.cts", ts("cts", true));
    // a `.d.` anywhere in the name means a declaration file, and it's case sensitive
    run_test("t.d.css.ts", ts("ts", true));
    run_test("t.D.TS", ts("ts", false));
    run_test("td.ts", ts("ts", false));
    // `.d.tsx` is not a declaration file
    run_test("t.d.tsx", ts("tsx", false));

    // a file name that is only an extension
    run_test(".ts", ts("ts", false));
    run_test(".tsx", ts("tsx", false));
    run_test(".js", js("js"));

    // an unknown extension falls back to the one before it, such as for a
    // file associated with this plugin (issue #637)
    run_test("t.ts.txt", ts("ts", false));
    run_test("t.TS.txt", ts("ts", false));
    run_test("t.tsx.txt", ts("tsx", false));
    run_test("t.mts.txt", ts("mts", false));
    run_test("t.cts.txt", ts("cts", false));
    run_test("t.d.ts.txt", ts("ts", true));
    run_test("t.jsx.txt", js("jsx"));
    run_test("t.mjs.txt", js("mjs"));
    run_test("t.cjs.txt", js("cjs"));
    run_test(".ts.txt", ts("ts", false));
    // only the extension directly before the unknown one is used
    run_test("t.ts.orig.txt", js("js"));
    run_test("t.ts.json", ts("ts", false));
    run_test("ts.txt", js("js"));
    run_test("t.d.txt", js("js"));
  }

  #[test]
  fn extension_overwrites_the_one_on_the_path() {
    #[track_caller]
    fn run_test(path: &str, extension: &str, expected_same_as: &str) {
      assert_eq!(
        resolve_source_type(&PathBuf::from(path), Some(extension)),
        resolve_source_type(&PathBuf::from(expected_same_as), None),
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
