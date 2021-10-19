use deno_ast::swc::parser::error::SyntaxError;
use deno_ast::{Diagnostic, ParsedSource, SourceTextInfo};
use dprint_core::types::{ErrBox, Error};
use std::path::Path;

pub fn parse_swc_ast(file_path: &Path, file_text: &str) -> Result<ParsedSource, ErrBox> {
  let file_text = SourceTextInfo::from_string(file_text.to_string());
  match parse_inner(file_path, file_text.clone()) {
    Ok(result) => Ok(result),
    Err(err) => {
      let lowercase_ext = get_lowercase_extension(file_path);
      let new_file_path = match lowercase_ext.as_ref().map(|e| e.as_str()) {
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

fn parse_inner(file_path: &Path, file_text: SourceTextInfo) -> Result<ParsedSource, ErrBox> {
  let parsed_source = deno_ast::parse_program(deno_ast::ParseParams {
    specifier: file_path.to_string_lossy().to_string(),
    capture_tokens: true,
    maybe_syntax: None,
    media_type: file_path.into(),
    scope_analysis: false,
    source: file_text.clone(),
  })
  .map_err(|diagnostic| format_diagnostic(&diagnostic, file_text.text_str()))?;
  Ok(parsed_source)
}

pub fn ensure_no_specific_syntax_errors(parsed_source: &ParsedSource) -> Result<(), ErrBox> {
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
          SyntaxError::TS1109
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
      final_message.push_str(&format_diagnostic(error, parsed_source.source().text_str()));
    }
    Err(Error::new(final_message))
  }
}

fn get_lowercase_extension(file_path: &Path) -> Option<String> {
  file_path.extension().and_then(|e| e.to_str()).map(|f| f.to_lowercase())
}

fn format_diagnostic(error: &Diagnostic, file_text: &str) -> String {
  let error_span = error.span;
  dprint_core::formatting::utils::string_utils::format_diagnostic(Some((error_span.lo().0 as usize, error_span.hi().0 as usize)), &error.message(), file_text)
}

#[cfg(test)]
mod tests {
  use super::*;
  use std::path::PathBuf;

  #[test]
  fn should_error_on_syntax_diagnostic() {
    let message = parse_swc_ast(&PathBuf::from("./test.ts"), "test;\nas#;").err().unwrap().to_string();
    assert_eq!(message, concat!("Line 2, column 3: Expected ';', '}' or <eof>\n", "\n", "  as#;\n", "    ~"));
  }

  #[test]
  fn it_should_error_without_issue_when_there_exists_multi_byte_char_on_line_with_syntax_error() {
    let message = parse_swc_ast(
      &PathBuf::from("./test.ts"),
      concat!(
        "test;\n",
        r#"console.log("x", `duration ${d} not in range - ${min} ≥ ${d} && ${max} ≥ ${d}`),;"#,
      ),
    )
    .err()
    .unwrap()
    .to_string();
    assert_eq!(
      message,
      concat!(
        // quite the diagnostic!
        "Line 2, column 81: Unexpected token `;`. Expected this, import, async, function, [ for array literal, ",
        "{ for object literal, @ for decorator, function, class, null, true, false, number, bigint, string, ",
        "regexp, ` for template literal, (, or an identifier\n",
        "\n",
        "  & ${max} ≥ ${d}`),;\n",
        "                    ~"
      )
    );
  }
}
