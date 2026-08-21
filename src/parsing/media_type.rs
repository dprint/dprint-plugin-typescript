use std::path::Path;

use super::ParseMode;
use dprint_swc_ext::swc::parser::EsSyntax;
use dprint_swc_ext::swc::parser::Syntax;
use dprint_swc_ext::swc::parser::TsSyntax;

/// The kind of file being parsed, which is only used to resolve the
/// syntax and parse mode from a file path.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MediaType {
  JavaScript,
  Jsx,
  Mjs,
  Cjs,
  TypeScript,
  Mts,
  Cts,
  Dts,
  Dmts,
  Dcts,
  Tsx,
  /// Not a media type this formatter recognizes. It will be
  /// parsed as JavaScript.
  Unknown,
}

impl MediaType {
  /// Resolves the media type from a file path's extension.
  pub fn from_path(path: &Path) -> MediaType {
    let Some(extension) = path.extension().and_then(|e| e.to_str()) else {
      return MediaType::Unknown;
    };
    // a declaration file is one like `file.d.ts`
    let is_declaration = || {
      path
        .file_stem()
        .and_then(|s| s.to_str())
        .map(|s| s.len() >= 2 && s.as_bytes()[s.len() - 2] == b'.' && s.as_bytes()[s.len() - 1].eq_ignore_ascii_case(&b'd'))
        .unwrap_or(false)
    };
    if extension.eq_ignore_ascii_case("js") {
      MediaType::JavaScript
    } else if extension.eq_ignore_ascii_case("jsx") {
      MediaType::Jsx
    } else if extension.eq_ignore_ascii_case("mjs") {
      MediaType::Mjs
    } else if extension.eq_ignore_ascii_case("cjs") {
      MediaType::Cjs
    } else if extension.eq_ignore_ascii_case("tsx") {
      MediaType::Tsx
    } else if extension.eq_ignore_ascii_case("ts") {
      if is_declaration() {
        MediaType::Dts
      } else {
        MediaType::TypeScript
      }
    } else if extension.eq_ignore_ascii_case("mts") {
      if is_declaration() {
        MediaType::Dmts
      } else {
        MediaType::Mts
      }
    } else if extension.eq_ignore_ascii_case("cts") {
      if is_declaration() {
        MediaType::Dcts
      } else {
        MediaType::Cts
      }
    } else {
      MediaType::Unknown
    }
  }

  /// Gets the swc syntax to use when parsing this media type.
  pub fn syntax(&self) -> Syntax {
    match self {
      MediaType::TypeScript | MediaType::Mts | MediaType::Cts | MediaType::Dts | MediaType::Dmts | MediaType::Dcts | MediaType::Tsx => {
        Syntax::Typescript(TsSyntax {
          decorators: true,
          // should be true for mts and cts:
          // https://babeljs.io/docs/babel-preset-typescript#disallowambiguousjsxlike
          disallow_ambiguous_jsx_like: matches!(self, MediaType::Mts | MediaType::Cts),
          dts: matches!(self, MediaType::Dts | MediaType::Dmts | MediaType::Dcts),
          tsx: *self == MediaType::Tsx,
          no_early_errors: false,
        })
      }
      MediaType::JavaScript | MediaType::Mjs | MediaType::Cjs | MediaType::Jsx | MediaType::Unknown => Syntax::Es(EsSyntax {
        allow_return_outside_function: true,
        allow_super_outside_method: true,
        auto_accessors: true,
        decorators: true,
        decorators_before_export: false,
        export_default_from: true,
        fn_bind: false,
        import_attributes: true,
        jsx: *self == MediaType::Jsx,
        explicit_resource_management: true,
      }),
    }
  }

  /// Gets whether the source should be parsed as a module, a script,
  /// or whether swc should decide.
  pub fn parse_mode(&self) -> ParseMode {
    match self {
      MediaType::Cjs => ParseMode::Script,
      // cts files can contain module declarations like
      // `import x = require("./x.ts");` or `export = 5;`, so we need to parse
      // them as modules (this may change in the future once
      // https://github.com/swc-project/swc/issues/9694 is resolved)
      MediaType::Cts | MediaType::Mjs | MediaType::Mts => ParseMode::Module,
      _ => ParseMode::Program,
    }
  }
}

#[cfg(test)]
mod test {
  use pretty_assertions::assert_eq;

  use super::*;
  use std::path::PathBuf;

  #[test]
  fn from_path() {
    #[track_caller]
    fn run_test(path: &str, expected: MediaType) {
      assert_eq!(MediaType::from_path(&PathBuf::from(path)), expected, "path: {}", path);
    }

    run_test("test.js", MediaType::JavaScript);
    run_test("test.JS", MediaType::JavaScript);
    run_test("test.D.TS", MediaType::Dts);
    run_test("test.jsx", MediaType::Jsx);
    run_test("test.mjs", MediaType::Mjs);
    run_test("test.cjs", MediaType::Cjs);
    run_test("test.ts", MediaType::TypeScript);
    run_test("test.tsx", MediaType::Tsx);
    run_test("test.mts", MediaType::Mts);
    run_test("test.cts", MediaType::Cts);
    run_test("test.d.ts", MediaType::Dts);
    run_test("test.d.mts", MediaType::Dmts);
    run_test("test.d.cts", MediaType::Dcts);
    run_test("test.json", MediaType::Unknown);
    run_test("test", MediaType::Unknown);
  }
}
