use std::sync::Arc;

use super::SourcePos;
use super::SourceRange;

/// Text with pre-computed positions of the start of each line.
#[derive(Clone)]
pub struct SourceTextInfo {
  text: Arc<str>,
  line_starts: Arc<[u32]>,
}

impl std::fmt::Debug for SourceTextInfo {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.debug_struct("SourceTextInfo").field("text", &self.text).finish()
  }
}

/// A 0-indexed line and column.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LineAndColumnIndex {
  pub line_index: usize,
  pub column_index: usize,
}

/// A 1-indexed line and column for display.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LineAndColumnDisplay {
  pub line_number: usize,
  pub column_number: usize,
}

impl SourceTextInfo {
  pub fn new(text: Arc<str>) -> Self {
    let line_starts = get_line_starts(&text);
    Self {
      text,
      line_starts: line_starts.into(),
    }
  }

  pub fn text(&self) -> &Arc<str> {
    &self.text
  }

  pub fn text_str(&self) -> &str {
    &self.text
  }

  pub fn range(&self) -> SourceRange {
    SourceRange::new(SourcePos::START, SourcePos::from_index(self.text.len()))
  }

  pub fn lines_count(&self) -> usize {
    self.line_starts.len()
  }

  pub fn line_index(&self, pos: SourcePos) -> usize {
    line_index(&self.line_starts, pos)
  }

  pub fn line_start(&self, line_index: usize) -> SourcePos {
    SourcePos::new(self.line_starts[line_index])
  }

  /// Gets the end of the line, which is before the newline character(s).
  pub fn line_end(&self, line_index: usize) -> SourcePos {
    line_end(&self.text, &self.line_starts, line_index)
  }

  pub fn line_text(&self, line_index: usize) -> &str {
    &self.text[self.line_start(line_index).as_index()..self.line_end(line_index).as_index()]
  }

  pub fn range_text(&self, range: &SourceRange) -> &str {
    &self.text[range.as_byte_range()]
  }

  /// Gets the 0-indexed line and column (in chars) of the provided position.
  pub fn line_and_column_index(&self, pos: SourcePos) -> LineAndColumnIndex {
    let line_index = self.line_index(pos);
    let line_start = self.line_starts[line_index] as usize;
    LineAndColumnIndex {
      line_index,
      column_index: self.text[line_start..pos.as_index()].chars().count(),
    }
  }

  /// Gets the 0-indexed line and column of the provided position where the
  /// column is the number of UTF-16 code units from the start of the line.
  pub fn line_and_column_index_utf16(&self, pos: SourcePos) -> LineAndColumnIndex {
    let line_index = self.line_index(pos);
    let line_start = self.line_starts[line_index] as usize;
    LineAndColumnIndex {
      line_index,
      column_index: self.text[line_start..pos.as_index()].chars().map(|c| c.len_utf16()).sum(),
    }
  }

  pub fn line_and_column_display(&self, pos: SourcePos) -> LineAndColumnDisplay {
    let index = self.line_and_column_index(pos);
    LineAndColumnDisplay {
      line_number: index.line_index + 1,
      column_number: index.column_index + 1,
    }
  }
}

/// Line information borrowed from the text, which is what formatting uses.
pub struct TextLines<'a> {
  text: &'a str,
  line_starts: Vec<u32>,
}

impl<'a> TextLines<'a> {
  pub fn new(text: &'a str) -> Self {
    Self {
      text,
      line_starts: get_line_starts(text),
    }
  }

  pub fn line_index(&self, pos: SourcePos) -> usize {
    line_index(&self.line_starts, pos)
  }

  /// Gets the 0-indexed column of the position, which is the number of
  /// chars from the start of the line.
  pub fn column_index(&self, pos: SourcePos) -> usize {
    let line_start = self.line_starts[self.line_index(pos)] as usize;
    self.text[line_start..pos.as_index()].chars().count()
  }
}

fn get_line_starts(text: &str) -> Vec<u32> {
  let mut line_starts = Vec::with_capacity(text.len() / 30 + 1);
  line_starts.push(0);
  for (i, b) in text.bytes().enumerate() {
    if b == b'\n' {
      line_starts.push((i + 1) as u32);
    }
  }
  line_starts
}

fn line_index(line_starts: &[u32], pos: SourcePos) -> usize {
  line_starts.partition_point(|&start| start <= pos.as_u32()) - 1
}

fn line_end(text: &str, line_starts: &[u32], line_index: usize) -> SourcePos {
  match line_starts.get(line_index + 1) {
    Some(&next_start) => {
      let mut end = next_start as usize - 1; // the \n
      if end > 0 && text.as_bytes()[end - 1] == b'\r' {
        end -= 1;
      }
      SourcePos::from_index(end)
    }
    None => SourcePos::from_index(text.len()),
  }
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn line_and_column_index() {
    let text_info = SourceTextInfo::new("12\n3\r\nβ\n5".into());
    let assert_pos = |pos: u32, line_index: usize, column_index: usize| {
      assert_eq!(
        text_info.line_and_column_index(SourcePos::new(pos)),
        LineAndColumnIndex { line_index, column_index },
        "pos {pos}"
      );
    };
    assert_pos(0, 0, 0);
    assert_pos(2, 0, 2);
    assert_pos(3, 1, 0);
    assert_pos(5, 1, 2);
    assert_pos(6, 2, 0);
    assert_pos(8, 2, 1); // after the two byte char
    assert_pos(9, 3, 0);
    assert_pos(10, 3, 1);
    assert_eq!(text_info.line_end(1), SourcePos::new(4));
    assert_eq!(text_info.line_text(1), "3");
    assert_eq!(text_info.line_end(3), SourcePos::new(10));
  }
}
