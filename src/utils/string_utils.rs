pub fn has_new_line_occurrences_in_leading_whitespace(text: &str, occurrences: i8) -> bool {
  if occurrences == 0 {
    return has_no_new_lines_in_leading_whitespace(text);
  }

  let mut found_occurrences = 0;
  for c in text.chars() {
    if !c.is_whitespace() {
      return false;
    }
    if c == '\n' {
      found_occurrences += 1;
      if found_occurrences >= occurrences {
        return true;
      }
    }
  }

  false
}

pub fn has_no_new_lines_in_leading_whitespace(text: &str) -> bool {
  for c in text.chars() {
    if !c.is_whitespace() {
      return true;
    }
    if c == '\n' {
      return false;
    }
  }

  true
}

pub fn has_new_line_occurrences_in_trailing_whitespace(text: &str, occurrences: i8) -> bool {
  if occurrences == 0 {
    return has_no_new_lines_in_trailing_whitespace(text);
  }

  let mut found_occurrences = 0;
  for c in text.chars().rev() {
    if !c.is_whitespace() {
      return false;
    }
    if c == '\n' {
      found_occurrences += 1;
      if found_occurrences >= occurrences {
        return true;
      }
    }
  }

  false
}

pub fn has_no_new_lines_in_trailing_whitespace(text: &str) -> bool {
  for c in text.chars().rev() {
    if !c.is_whitespace() {
      return true;
    }
    if c == '\n' {
      return false;
    }
  }

  true
}

pub fn is_not_empty_and_only_spaces(text: &str) -> bool {
  if text.is_empty() {
    return false;
  }

  for c in text.chars() {
    if c != ' ' {
      return false;
    }
  }

  true
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
}
