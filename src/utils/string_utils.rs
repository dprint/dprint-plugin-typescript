/// Gets if the character is whitespace in JSX text.
///
/// Other whitespace, like a non-breaking space, is content in JSX text.
pub fn is_jsx_whitespace_char(c: char) -> bool {
  matches!(c, ' ' | '\t' | '\n' | '\r')
}

/// Gets if the character is JSX whitespace that isn't a line break.
pub fn is_jsx_space_char(c: char) -> bool {
  matches!(c, ' ' | '\t')
}

/// Gets if the text is empty or only whitespace in JSX text.
pub fn is_jsx_whitespace(text: &str) -> bool {
  text.chars().all(is_jsx_whitespace_char)
}

/// Gets if the text is only spaces and tabs, which JSX keeps
/// when they're not beside a line break.
pub fn is_jsx_space(text: &str) -> bool {
  !text.is_empty() && text.chars().all(is_jsx_space_char)
}

/// Gets the whitespace at the start of JSX text.
pub fn get_jsx_leading_whitespace(text: &str) -> &str {
  &text[..text.len() - text.trim_start_matches(is_jsx_whitespace_char).len()]
}

/// Gets the whitespace at the end of JSX text.
pub fn get_jsx_trailing_whitespace(text: &str) -> &str {
  &text[text.trim_end_matches(is_jsx_whitespace_char).len()..]
}

/// Gets if the JSX text starts with a space that JSX keeps.
pub fn has_jsx_leading_space(text: &str) -> bool {
  is_jsx_space(get_jsx_leading_whitespace(text))
}

/// Gets if the JSX text ends with a space that JSX keeps.
pub fn has_jsx_trailing_space(text: &str) -> bool {
  is_jsx_space(get_jsx_trailing_whitespace(text))
}

#[derive(Debug, PartialEq, Eq)]
pub struct SplitLinesItem<'a> {
  pub text: &'a str,
  pub line_index: usize,
  pub is_last: bool,
}

pub struct SplitLinesIterator<'a> {
  text: &'a str,
  inner: std::str::Split<'a, char>,
  line_index: usize,
  byte_index: usize,
}

impl<'a> Iterator for SplitLinesIterator<'a> {
  type Item = SplitLinesItem<'a>;

  fn next(&mut self) -> Option<Self::Item> {
    let line_index = self.line_index;
    let line = self.inner.next();
    match line {
      Some(line) => {
        if self.line_index == 0 {
          self.byte_index += line.len();
        } else {
          self.byte_index += 1 + line.len();
        }
        self.line_index += 1;
        Some(SplitLinesItem {
          is_last: self.byte_index == self.text.len(),
          line_index,
          text: line.strip_suffix('\r').unwrap_or(line),
        })
      }
      None => None,
    }
  }
}

pub fn split_lines(text: &str) -> SplitLinesIterator<'_> {
  SplitLinesIterator {
    inner: text.split('\n'),
    line_index: 0,
    text,
    byte_index: 0,
  }
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn split_lines_empty_last_line() {
    let text = "a\r\nb\nc\r\n";
    let lines = split_lines(text).collect::<Vec<_>>();
    assert_eq!(
      lines,
      vec![
        SplitLinesItem {
          text: "a",
          is_last: false,
          line_index: 0,
        },
        SplitLinesItem {
          text: "b",
          is_last: false,
          line_index: 1,
        },
        SplitLinesItem {
          text: "c",
          is_last: false,
          line_index: 2
        },
        // includes last line
        SplitLinesItem {
          text: "",
          is_last: true,
          line_index: 3,
        }
      ]
    );
  }

  #[test]
  fn split_lines_non_empty_last_line() {
    let text = "a \n   b\nc";
    let lines = split_lines(text).collect::<Vec<_>>();
    assert_eq!(
      lines,
      vec![
        SplitLinesItem {
          text: "a ",
          line_index: 0,
          is_last: false,
        },
        SplitLinesItem {
          text: "   b",
          line_index: 1,
          is_last: false,
        },
        SplitLinesItem {
          text: "c",
          line_index: 2,
          is_last: true,
        }
      ]
    );
  }

  #[test]
  fn jsx_whitespace() {
    assert!(is_jsx_whitespace(""));
    assert!(is_jsx_whitespace(" \t\r\n"));
    assert!(!is_jsx_whitespace(" a "));
    // a non-breaking space is content
    assert!(!is_jsx_whitespace("\u{a0}"));
  }

  #[test]
  fn jsx_space() {
    assert!(is_jsx_space(" "));
    assert!(is_jsx_space(" \t "));
    assert!(!is_jsx_space(""));
    assert!(!is_jsx_space(" \n "));
    assert!(!is_jsx_space(" \r\n "));
    assert!(!is_jsx_space("\u{a0}"));
  }

  #[test]
  fn jsx_leading_and_trailing_whitespace() {
    assert_eq!(get_jsx_leading_whitespace(" \n\ta b \t"), " \n\t");
    assert_eq!(get_jsx_trailing_whitespace(" \n\ta b \t"), " \t");
    assert_eq!(get_jsx_leading_whitespace("\u{a0} a"), "");
    assert_eq!(get_jsx_trailing_whitespace("a \u{a0}"), "");
    assert_eq!(get_jsx_leading_whitespace("  "), "  ");
    assert_eq!(get_jsx_trailing_whitespace("  "), "  ");
    assert_eq!(get_jsx_leading_whitespace(""), "");
  }

  #[test]
  fn jsx_leading_and_trailing_space() {
    assert!(has_jsx_leading_space(" a"));
    assert!(has_jsx_leading_space("\ta"));
    assert!(has_jsx_leading_space(" "));
    assert!(!has_jsx_leading_space("a "));
    assert!(!has_jsx_leading_space(" \n a"));
    assert!(!has_jsx_leading_space("\u{a0}a"));
    assert!(has_jsx_trailing_space("a "));
    assert!(has_jsx_trailing_space("a\t"));
    assert!(!has_jsx_trailing_space(" a"));
    assert!(!has_jsx_trailing_space("a \n "));
    assert!(!has_jsx_trailing_space("a \r\n"));
  }
}
