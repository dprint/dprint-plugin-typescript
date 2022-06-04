use anyhow::anyhow;
use anyhow::bail;
use anyhow::Result;
use deno_ast::swc::parser::error::SyntaxError;
use deno_ast::Diagnostic;
use deno_ast::ParsedSource;
use deno_ast::SourceTextInfo;
use std::path::Path;

pub fn parse_swc_ast(file_path: &Path, file_text: &str) -> Result<ParsedSource> {
  let file_text = SourceTextInfo::from_string(file_text.to_string());
  match parse_inner(file_path, file_text.clone()) {
    Ok(result) => Ok(result),
    Err(err) => {
      let lowercase_ext = get_lowercase_extension(file_path);
      let new_file_path = match lowercase_ext.as_deref() {
        Some("ts") | Some("cts") | Some("mts") => file_path.with_extension("tsx"),
        Some("js") | Some("cjs") | Some("mjs") => file_path.with_extension("jsx"),
        _ => return Err(err),
      };
      // try to parse as jsx
      match parse_inner(&new_file_path, file_text) {
        Ok(result) => Ok(result),
        Err(_) => Err(err), // return the original error
      }
    }
  }
}

fn parse_inner(file_path: &Path, text_info: SourceTextInfo) -> Result<ParsedSource> {
  let parsed_source = parse_inner_no_diagnostic_check(file_path, text_info)?;
  ensure_no_specific_syntax_errors(&parsed_source)?;
  Ok(parsed_source)
}

fn parse_inner_no_diagnostic_check(file_path: &Path, text_info: SourceTextInfo) -> Result<ParsedSource> {
  deno_ast::parse_program(deno_ast::ParseParams {
    specifier: file_path.to_string_lossy().to_string(),
    capture_tokens: true,
    maybe_syntax: None,
    media_type: file_path.into(),
    scope_analysis: false,
    text_info: text_info.clone(),
  })
  .map_err(|diagnostic| anyhow!("{}", format_diagnostic(&diagnostic, &text_info)))
}

pub fn ensure_no_specific_syntax_errors(parsed_source: &ParsedSource) -> Result<()> {
  let diagnostics = parsed_source
    .diagnostics()
    .iter()
    .filter(|e| {
      matches!(
        e.kind,
        // expected identifier
        SyntaxError::TS1003 |
          // expected semi-colon
          SyntaxError::TS1005 |
          // expected expression
          SyntaxError::TS1109 |
          // expected token
          SyntaxError::Expected(_, _) |
          // unexpected token
          SyntaxError::Unexpected { .. }
      )
    })
    .collect::<Vec<_>>();

  if diagnostics.is_empty() {
    Ok(())
  } else {
    let mut final_message = String::new();
    for error in diagnostics {
      if !final_message.is_empty() {
        final_message.push_str("\n\n");
      }
      final_message.push_str(&format_diagnostic(error, parsed_source.text_info()));
    }
    bail!("{}", final_message)
  }
}

fn get_lowercase_extension(file_path: &Path) -> Option<String> {
  file_path.extension().and_then(|e| e.to_str()).map(|f| f.to_lowercase())
}

fn format_diagnostic(error: &Diagnostic, text_info: &SourceTextInfo) -> String {
  let file_text = text_info.text_str();
  let range = error.range.as_byte_range(text_info.range().start);
  dprint_core::formatting::utils::string_utils::format_diagnostic(Some((range.start, range.end)), &error.message(), file_text)
}

#[cfg(test)]
mod tests {
  use crate::configuration::ConfigurationBuilder;

  use super::*;
  use std::path::PathBuf;

  #[test]
  fn should_error_on_syntax_diagnostic() {
    run_fatal_diagnostic_test(
      "./test.ts",
      "test;\nas#;",
      concat!("Line 2, column 3: Expected ';', '}' or <eof>\n", "\n", "  as#;\n", "    ~"),
    );
  }

  #[test]
  fn it_should_error_without_issue_when_there_exists_multi_byte_char_on_line_with_syntax_error() {
    run_fatal_diagnostic_test(
      "./test.ts",
      concat!(
        "test;\n",
        r#"console.log("x", `duration ${d} not in range - ${min} ≥ ${d} && ${max} ≥ ${d}`),;"#,
      ),
      concat!(
        // quite the diagnostic!
        "Line 2, column 81: Unexpected token `;`. Expected this, import, async, function, [ for array literal, ",
        "{ for object literal, @ for decorator, function, class, null, true, false, number, bigint, string, ",
        "regexp, ` for template literal, (, or an identifier\n",
        "\n",
        "   && ${max} ≥ ${d}`),;\n",
        "                      ~"
      ),
    );
  }

  fn run_fatal_diagnostic_test(file_path: &str, text: &str, expected: &str) {
    let file_path = PathBuf::from(file_path);
    assert_eq!(parse_swc_ast(&file_path, text).err().unwrap().to_string(), expected);
  }

  #[test]
  fn it_should_error_for_no_equals_sign_in_var_decl() {
    run_non_fatal_diagnostic_test(
      "./test.ts",
      "const Methods {\nf: (x, y) => x + y,\n};",
      concat!("Line 1, column 15: Expected a semicolon\n", "\n", "  const Methods {\n", "                ~"),
    );
  }

  #[test]
  fn it_should_error_when_var_stmts_sep_by_comma() {
    run_non_fatal_diagnostic_test(
      "./test.ts",
      "let a = 0, let b = 1;",
      concat!(
        "Line 1, column 16: Expected a semicolon\n",
        "\n",
        "  let a = 0, let b = 1;\n",
        "                 ~"
      ),
    );
  }

  #[test]
  fn it_should_error_for_exected_expr_issue_121() {
    run_non_fatal_diagnostic_test(
      "./test.ts",
      "type T =\n  | unknown\n  { } & unknown;",
      concat!("Line 3, column 7: Expression expected\n\n", "    { } & unknown;\n", "        ~"),
    );
  }

  #[test]
  fn it_should_error_for_exected_close_brace() {
    // swc can parse this, but we explicitly fail formatting
    // in this scenario because I believe it might cause more
    // harm than good.
    run_non_fatal_diagnostic_test(
      "./test.ts",
      "class Test {",
      concat!("Line 1, column 12: Expected '}', got '<eof>'\n\n", "  class Test {\n", "             ~"),
    );
  }

  fn run_non_fatal_diagnostic_test(file_path: &str, text: &str, expected: &str) {
    let file_path = PathBuf::from(file_path);
    assert_eq!(parse_swc_ast(&file_path, text).err().unwrap().to_string(), expected);

    // this error should also be surfaced in `format_parsed_source` if someone provides
    // a source file that had a non-fatal diagnostic
    let parsed_source = parse_inner_no_diagnostic_check(&file_path, SourceTextInfo::from_string(text.to_string())).unwrap();
    let config = ConfigurationBuilder::new().build();
    assert_eq!(crate::format_parsed_source(&parsed_source, &config).err().unwrap().to_string(), expected);
  }
}
