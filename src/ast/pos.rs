use std::fmt;
use std::ops::Add;
use std::ops::Sub;

use oxc_span::Span;

use super::CommentsIterator;
use super::ProgramInfo;
use super::TokenAndSpan;

/// A byte position in the source text, starting at zero.
#[derive(Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SourcePos(u32);

impl SourcePos {
  pub const START: SourcePos = SourcePos(0);

  pub const fn new(value: u32) -> Self {
    Self(value)
  }

  pub fn from_index(index: usize) -> Self {
    Self(index as u32)
  }

  pub const fn as_u32(self) -> u32 {
    self.0
  }

  /// Gets the byte index of this position in the source text.
  pub const fn as_index(self) -> usize {
    self.0 as usize
  }
}

impl fmt::Debug for SourcePos {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "SourcePos({})", self.0)
  }
}

impl fmt::Display for SourcePos {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.0)
  }
}

impl Add<usize> for SourcePos {
  type Output = SourcePos;

  fn add(self, rhs: usize) -> Self::Output {
    SourcePos(self.0 + rhs as u32)
  }
}

impl Sub<usize> for SourcePos {
  type Output = SourcePos;

  fn sub(self, rhs: usize) -> Self::Output {
    SourcePos(self.0 - rhs as u32)
  }
}

impl Sub<SourcePos> for SourcePos {
  type Output = usize;

  fn sub(self, rhs: SourcePos) -> Self::Output {
    (self.0 - rhs.0) as usize
  }
}

/// A range of byte positions in the source text.
#[derive(Clone, Copy, Default, PartialEq, Eq, Hash)]
pub struct SourceRange {
  pub start: SourcePos,
  pub end: SourcePos,
}

impl SourceRange {
  pub fn new(start: SourcePos, end: SourcePos) -> Self {
    Self { start, end }
  }

  pub fn from_span(span: Span) -> Self {
    Self::new(SourcePos(span.start), SourcePos(span.end))
  }

  /// Gets if this range contains the provided range.
  pub fn contains(&self, other: &SourceRange) -> bool {
    self.start <= other.start && self.end >= other.end
  }

  pub fn as_byte_range(&self) -> std::ops::Range<usize> {
    self.start.as_index()..self.end.as_index()
  }
}

impl fmt::Debug for SourceRange {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "SourceRange({}..{})", self.start.0, self.end.0)
  }
}

/// Something that has a range in the source text.
pub trait SourceRanged {
  fn start(&self) -> SourcePos;
  fn end(&self) -> SourcePos;

  fn range(&self) -> SourceRange {
    SourceRange::new(self.start(), self.end())
  }

  fn text_fast<'a>(&self, program: ProgramInfo<'a>) -> &'a str {
    &program.text()[self.start().as_index()..self.end().as_index()]
  }

  fn start_line_fast(&self, program: ProgramInfo<'_>) -> usize {
    program.text_info().line_index(self.start())
  }

  fn end_line_fast(&self, program: ProgramInfo<'_>) -> usize {
    program.text_info().line_index(self.end())
  }

  fn start_column_fast(&self, program: ProgramInfo<'_>) -> usize {
    program.text_info().column_index(self.start())
  }

  fn tokens_fast<'a>(&self, program: ProgramInfo<'a>) -> &'a [TokenAndSpan] {
    program.token_container().get_tokens_in_range(self.start(), self.end())
  }

  fn previous_token_fast<'a>(&self, program: ProgramInfo<'a>) -> Option<&'a TokenAndSpan> {
    program.token_container().get_previous_token(self.start())
  }

  fn next_token_fast<'a>(&self, program: ProgramInfo<'a>) -> Option<&'a TokenAndSpan> {
    program.token_container().get_next_token(self.end())
  }

  fn previous_tokens_fast<'a>(&self, program: ProgramInfo<'a>) -> &'a [TokenAndSpan] {
    program.token_container().get_previous_tokens(self.start())
  }

  fn next_tokens_fast<'a>(&self, program: ProgramInfo<'a>) -> &'a [TokenAndSpan] {
    program.token_container().get_next_tokens(self.end())
  }

  fn leading_comments_fast<'a>(&self, program: ProgramInfo<'a>) -> CommentsIterator<'a> {
    program.comment_container().leading_comments(self.start())
  }

  fn trailing_comments_fast<'a>(&self, program: ProgramInfo<'a>) -> CommentsIterator<'a> {
    program.comment_container().trailing_comments(self.end())
  }
}

impl SourceRanged for SourcePos {
  fn start(&self) -> SourcePos {
    *self
  }

  fn end(&self) -> SourcePos {
    *self
  }
}

impl SourceRanged for SourceRange {
  fn start(&self) -> SourcePos {
    self.start
  }

  fn end(&self) -> SourcePos {
    self.end
  }
}

impl SourceRanged for Span {
  fn start(&self) -> SourcePos {
    SourcePos(self.start)
  }

  fn end(&self) -> SourcePos {
    SourcePos(self.end)
  }
}

impl<T: SourceRanged + ?Sized> SourceRanged for &T {
  fn start(&self) -> SourcePos {
    (**self).start()
  }

  fn end(&self) -> SourcePos {
    (**self).end()
  }
}

impl<T: SourceRanged> SourceRanged for oxc_allocator::Box<'_, T> {
  fn start(&self) -> SourcePos {
    (**self).start()
  }

  fn end(&self) -> SourcePos {
    (**self).end()
  }
}

impl SourceRanged for oxc_ast::AstKind<'_> {
  fn start(&self) -> SourcePos {
    SourcePos(oxc_span::GetSpan::span(self).start)
  }

  fn end(&self) -> SourcePos {
    SourcePos(oxc_span::GetSpan::span(self).end)
  }
}
