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

pub struct SplitLinesIterator<'a> {
  inner: std::str::Split<'a, char>,
}

impl<'a> Iterator for SplitLinesIterator<'a> {
  type Item = &'a str;

  fn next(&mut self) -> Option<Self::Item> {
    let line = self.inner.next();
    line.map(|line| if line.ends_with('\r') { &line[..line.len() - 1] } else { line })
  }
}

pub fn split_lines<'a>(text: &'a str) -> SplitLinesIterator<'a> {
  SplitLinesIterator { inner: text.split('\n') }
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn split_lines_empty_last_line() {
    let text = "a\r\nb\nc\r\n";
    let lines = split_lines(&text).collect::<Vec<_>>();
    assert_eq!(lines, vec!["a", "b", "c", ""]); // includes last line
  }

  #[test]
  fn split_lines_non_empty_last_line() {
    let text = "a \n   b\nc";
    let lines = split_lines(&text).collect::<Vec<_>>();
    assert_eq!(lines, vec!["a ", "   b", "c"]);
  }
}
