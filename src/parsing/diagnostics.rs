use std::borrow::Cow;
use std::fmt;
use std::sync::Arc;

use dprint_swc_ext::common::LineAndColumnDisplay;
use dprint_swc_ext::common::SourceRange;
use dprint_swc_ext::common::SourceRangedForSpanned;
use dprint_swc_ext::common::SourceTextInfo;
use dprint_swc_ext::swc::parser::error::Error as SwcError;
use dprint_swc_ext::swc::parser::error::SyntaxError;

/// A syntax error that occurred while parsing.
#[derive(Debug, Clone)]
pub struct ParseDiagnostic(Box<ParseDiagnosticInner>);

#[derive(Debug, Clone)]
struct ParseDiagnosticInner {
  specifier: Arc<str>,
  range: SourceRange,
  kind: SyntaxError,
  text_info: SourceTextInfo,
}

impl ParseDiagnostic {
  pub(crate) fn from_swc_error(err: SwcError, specifier: &Arc<str>, text_info: SourceTextInfo) -> ParseDiagnostic {
    ParseDiagnostic(Box::new(ParseDiagnosticInner {
      range: err.range(),
      specifier: specifier.clone(),
      kind: err.into_kind(),
      text_info,
    }))
  }

  /// Specifier of the source the diagnostic occurred in.
  pub fn specifier(&self) -> &str {
    &self.0.specifier
  }

  /// Range of the diagnostic.
  pub fn range(&self) -> SourceRange {
    self.0.range
  }

  /// Swc syntax error.
  pub fn kind(&self) -> &SyntaxError {
    &self.0.kind
  }

  /// Text info of the source the diagnostic occurred in.
  pub fn text_info(&self) -> &SourceTextInfo {
    &self.0.text_info
  }

  /// Human readable message describing the error.
  pub fn message(&self) -> Cow<'_, str> {
    self.0.kind.msg()
  }

  /// 1-indexed display position the diagnostic occurred at.
  pub fn display_position(&self) -> LineAndColumnDisplay {
    self.0.text_info.line_and_column_display(self.clamped_range().start)
  }

  /// swc will sometimes provide a dummy or out of range span, so ensure the
  /// range is within the text before using it to index into the text.
  fn clamped_range(&self) -> SourceRange {
    let text_range = self.0.text_info.range();
    let start = self.0.range.start.clamp(text_range.start.as_source_pos(), text_range.end);
    let end = self.0.range.end.clamp(start, text_range.end);
    SourceRange::new(start, end)
  }
}

impl Eq for ParseDiagnostic {}

impl PartialEq for ParseDiagnostic {
  fn eq(&self, other: &Self) -> bool {
    // excludes the text info
    self.0.specifier == other.0.specifier && self.0.range == other.0.range && self.0.kind == other.0.kind
  }
}

impl std::error::Error for ParseDiagnostic {}

impl fmt::Display for ParseDiagnostic {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let display_position = self.display_position();
    write!(
      f,
      "{} at {}:{}:{}\n\n{}",
      self.message(),
      self.specifier(),
      display_position.line_number,
      display_position.column_number,
      get_range_text_highlight(self.text_info(), self.clamped_range())
        .lines()
        // indent two spaces
        .map(|l| if l.trim().is_empty() { String::new() } else { format!("  {}", l) })
        .collect::<Vec<_>>()
        .join("\n"),
    )
  }
}

/// Multiple syntax errors that occurred while parsing.
#[derive(Debug, Clone)]
pub struct ParseDiagnosticsError(pub Vec<ParseDiagnostic>);

impl std::error::Error for ParseDiagnosticsError {}

impl fmt::Display for ParseDiagnosticsError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    for (i, diagnostic) in self.0.iter().enumerate() {
      if i > 0 {
        write!(f, "\n\n")?;
      }

      write!(f, "{}", diagnostic)?
    }

    Ok(())
  }
}

/// Code in this function was adapted from:
/// https://github.com/dprint/dprint/blob/a026a1350d27a61ea18207cb31897b18eaab51a1/crates/core/src/formatting/utils/string_utils.rs#L62
///
/// The range must be within the text info.
fn get_range_text_highlight(text_info: &SourceTextInfo, byte_range: SourceRange) -> String {
  let (sub_text, (error_start, error_end)) = get_text_and_error_range(text_info, byte_range);

  let mut result = String::new();
  // don't use .lines() here because it will trim any empty
  // lines, which might for some reason be part of the range
  let lines = sub_text.split('\n').collect::<Vec<_>>();
  let line_count = lines.len();
  for (i, mut line) in lines.into_iter().enumerate() {
    if line.ends_with('\r') {
      line = &line[..line.len() - 1]; // trim the \r
    }
    let is_last_line = i == line_count - 1;
    // don't show all the lines if there are more than 3 lines
    if i > 2 && !is_last_line {
      continue;
    }
    if i > 0 {
      result.push('\n');
    }
    if i == 2 && !is_last_line {
      result.push_str("...");
      continue;
    }

    let mut error_start_char_index = if i == 0 { get_column_index_of_pos(sub_text, error_start) } else { 0 };
    let mut error_end_char_index = if is_last_line {
      get_column_index_of_pos(sub_text, error_end)
    } else {
      line.chars().count()
    };
    let line_char_count = line.chars().count();
    if line_char_count > 90 {
      let start_char_index = if error_start_char_index > 60 {
        std::cmp::min(error_start_char_index - 20, line_char_count - 80)
      } else {
        0
      };
      error_start_char_index = error_start_char_index.saturating_sub(start_char_index);
      error_end_char_index = error_end_char_index.saturating_sub(start_char_index);
      let code_text = line.chars().skip(start_char_index).take(80).collect::<String>();
      let mut line_text = String::new();
      if start_char_index > 0 {
        line_text.push_str("...");
        error_start_char_index += 3;
        error_end_char_index += 3;
      }
      line_text.push_str(&code_text);
      if line_char_count > start_char_index + code_text.chars().count() {
        error_end_char_index = std::cmp::min(error_end_char_index, line_text.chars().count());
        line_text.push_str("...");
      }
      result.push_str(&line_text);
    } else {
      result.push_str(line);
    }
    result.push('\n');

    result.push_str(&" ".repeat(error_start_char_index));
    result.push_str(
      // a zero width range means it's the end of the line, so display a single ~
      &"~".repeat(error_end_char_index.saturating_sub(error_start_char_index).max(1)),
    );
  }
  result
}

fn get_text_and_error_range(text_info: &SourceTextInfo, byte_range: SourceRange) -> (&str, (usize, usize)) {
  let mut first_line_index = text_info.line_index(byte_range.start);
  let mut first_line_start = text_info.line_start(first_line_index);
  let last_line_end = text_info.line_end(text_info.line_index(byte_range.end));
  let mut sub_text = text_info.range_text(&SourceRange::new(first_line_start, last_line_end));

  // while the text is empty, show the previous line
  while sub_text.trim().is_empty() && first_line_index > 0 {
    first_line_index -= 1;
    first_line_start = text_info.line_start(first_line_index);
    sub_text = text_info.range_text(&SourceRange::new(first_line_start, last_line_end));
  }

  let error_start = byte_range.start - first_line_start;
  let error_end = error_start + (byte_range.end - byte_range.start);
  (sub_text, (error_start, error_end))
}

fn get_column_index_of_pos(text: &str, pos: usize) -> usize {
  let line_start_byte_pos = get_line_start_byte_pos(text, pos);
  text[line_start_byte_pos..pos].chars().count()
}

fn get_line_start_byte_pos(text: &str, pos: usize) -> usize {
  let text_bytes = text.as_bytes();
  for i in (0..pos).rev() {
    if text_bytes.get(i) == Some(&b'\n') {
      return i + 1;
    }
  }

  0
}

#[cfg(test)]
mod test {
  use pretty_assertions::assert_eq;

  use super::*;

  #[test]
  fn range_highlight_all_text() {
    let text = text_info(concat!(
      "Line 0 - Testing this out with a long line testing0 testing1 testing2 testing3 testing4 testing5 testing6
",
      "Line 1
",
      "Line 2
",
      "Line 3
",
      "Line 4"
    ));
    assert_eq!(
      get_range_text_highlight(&text, SourceRange::new(text.line_start(0), text.line_end(4))),
      concat!(
        "Line 0 - Testing this out with a long line testing0 testing1 testing2 testing3 t...
",
        "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
",
        "Line 1
",
        "~~~~~~
",
        "...
",
        "Line 4
",
        "~~~~~~",
      )
    );
  }

  #[test]
  fn range_highlight_all_text_last_line_long() {
    let text = text_info(concat!(
      "Line 0
",
      "Line 1
",
      "Line 2
",
      "Line 3
",
      "Line 4 - Testing this out with a long line testing0 testing1 testing2 testing3 testing4 testing5 testing6
",
    ));
    assert_eq!(
      get_range_text_highlight(&text, SourceRange::new(text.line_start(0), text.line_end(4))),
      concat!(
        "Line 0
",
        "~~~~~~
",
        "Line 1
",
        "~~~~~~
",
        "...
",
        "Line 4 - Testing this out with a long line testing0 testing1 testing2 testing3 t...
",
        "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~",
      )
    );
  }

  #[test]
  fn range_highlight_range_start_long_line() {
    let text = text_info("Testing this out with a long line testing0 testing1 testing2 testing3 testing4 testing5 testing6 testing7");
    assert_eq!(
      get_range_text_highlight(&text, SourceRange::new(text.line_start(0), text.line_start(0) + 1)),
      concat!(
        "Testing this out with a long line testing0 testing1 testing2 testing3 testing4 t...
",
        "~",
      )
    );
  }

  #[test]
  fn range_highlight_range_end_long_line() {
    let text = text_info("Testing this out with a long line testing0 testing1 testing2 testing3 testing4 testing5 testing6 testing7");
    assert_eq!(
      get_range_text_highlight(&text, SourceRange::new(text.line_end(0) - 1, text.line_end(0))),
      concat!(
        "...ong line testing0 testing1 testing2 testing3 testing4 testing5 testing6 testing7
",
        "                                                                                  ~",
      )
    );
  }

  #[test]
  fn range_highlight_whitespace_start_line() {
    let text = text_info(
      "  testing
test",
    );
    assert_eq!(
      get_range_text_highlight(&text, SourceRange::new(text.line_end(0) - 1, text.line_end(1))),
      concat!(
        "  testing
",
        "        ~
",
        "test
",
        "~~~~",
      )
    );
  }

  #[test]
  fn range_end_of_line() {
    let text = text_info("  testingtestingtestingtesting");
    assert_eq!(
      get_range_text_highlight(&text, SourceRange::new(text.line_end(0), text.line_end(0))),
      concat!(
        "  testingtestingtestingtesting
",
        "                              ~",
      )
    );
  }

  #[test]
  fn range_outside_of_text_is_clamped() {
    // swc will sometimes provide a dummy span, which must not panic when rendered
    let text_info = text_info("const t = 5;");
    let out_of_range = SourceRange::new(text_info.range().end + 100, text_info.range().end + 200);
    let diagnostic = ParseDiagnostic(Box::new(ParseDiagnosticInner {
      specifier: "file:///test.ts".into(),
      range: out_of_range,
      kind: SyntaxError::Eof,
      text_info,
    }));
    assert_eq!(
      diagnostic.to_string(),
      concat!(
        "Unexpected eof at file:///test.ts:1:13

",
        "  const t = 5;
",
        "              ~"
      )
    );
  }

  fn text_info(text: &str) -> SourceTextInfo {
    SourceTextInfo::from_string(text.to_string())
  }
}
