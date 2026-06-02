// oxc-port foundation.
//
// The SWC-based code relied on `deno_ast::view` (a `Node` enum with parent
// pointers) plus deno_ast's `SourcePos`/`SourceRange`/`SourceRanged` API and
// `*_fast(program)` helpers for source text, line numbers, comments and tokens.
//
// oxc provides none of that, so this module rebuilds the minimal surface the
// generation layer needs, directly on top of oxc:
//
// * `SourcePos` = byte offset (`u32`), `SourceRange` = oxc `Span`.
// * `ProgramInfo<'a>` — a `Copy` bundle of borrows (program, source text info,
//   tokens, comments) taken straight from the `ParsedSource`. It replaces the
//   old `view::Program` handle that was threaded through everything.
// * `SourceRanged` — blanket-implemented for every `T: GetSpan` (all oxc AST
//   nodes, `AstKind`, `Span`). Provides `start/end/range` plus the
//   `*_fast(program)` helpers. Comments don't implement `GetSpan`, so they get
//   the parallel `CommentExt` trait.
//
// Comment attachment: oxc keeps comments in a flat, source-ordered list and
// only computes *leading* attachment. We reconstruct leading/trailing
// attachment on demand with the same rule SWC used: a comment is *trailing* of
// the preceding token when there is no line break between that token and the
// comment, otherwise it is *leading* of the following token.

use deno_ast::oxc::ast::ast::Comment;
use deno_ast::oxc::ast::ast::CommentKind;
use deno_ast::oxc::ast::ast::Program;
use deno_ast::oxc::ast::AstKind;
use deno_ast::oxc::parser::Token;
use deno_ast::oxc::span::GetSpan;
use deno_ast::oxc::span::Span;
use deno_ast::SourceTextInfo;

pub type SourcePos = u32;
pub type SourceRange = Span;

/// The generation layer's unified node handle. The SWC code used
/// `deno_ast::view::Node`; on oxc the equivalent is `AstKind`. Parent tracking
/// (which the view layer provided for free) is done manually via
/// `Context::parent_stack`.
pub type Node<'a> = AstKind<'a>;

/// Constructs a [`SourceRange`] from start/end byte offsets.
#[inline]
pub fn source_range(start: SourcePos, end: SourcePos) -> SourceRange {
  Span::new(start, end)
}

/// A `Copy` handle to the parsed program and its derived lookup data.
///
/// All fields borrow from the `ParsedSource` (and thus the arena), so this is
/// cheap to copy and is passed by value to the `*_fast` helpers, mirroring how
/// the old `view::Program` handle was used.
#[derive(Clone, Copy)]
pub struct ProgramInfo<'a> {
  program: &'a Program<'a>,
  text_info: &'a SourceTextInfo,
  tokens: &'a [Token],
  comments: &'a [Comment],
}

impl<'a> ProgramInfo<'a> {
  pub fn new(program: &'a Program<'a>, text_info: &'a SourceTextInfo, tokens: &'a [Token], comments: &'a [Comment]) -> ProgramInfo<'a> {
    ProgramInfo {
      program,
      text_info,
      tokens,
      comments,
    }
  }

  #[inline]
  pub fn program(&self) -> &'a Program<'a> {
    self.program
  }

  #[inline]
  pub fn text_info(&self) -> &'a SourceTextInfo {
    self.text_info
  }

  #[inline]
  pub fn text(&self) -> &'a str {
    self.text_info.text_str()
  }

  #[inline]
  pub fn tokens(&self) -> &'a [Token] {
    self.tokens
  }

  #[inline]
  pub fn comments(&self) -> &'a [Comment] {
    self.comments
  }

  /// Byte offset of the start of the program (after any leading whitespace is
  /// irrelevant here; this is simply the program span start, usually 0).
  #[inline]
  pub fn lo(&self) -> SourcePos {
    self.program.span.start
  }

  /// Byte offset of the end of the program.
  #[inline]
  pub fn hi(&self) -> SourcePos {
    self.program.span.end
  }

  #[inline]
  fn slice(&self, start: SourcePos, end: SourcePos) -> &'a str {
    &self.text()[start as usize..end as usize]
  }

  /// End offset of the last token entirely before `pos` (or the program start
  /// when there is none).
  fn prev_token_end(&self, pos: SourcePos) -> SourcePos {
    let idx = self.tokens.partition_point(|t| t.end() <= pos);
    if idx > 0 {
      self.tokens[idx - 1].end()
    } else {
      self.lo()
    }
  }

  /// Start offset of the first token at or after `pos` (or the program end when
  /// there is none).
  fn next_token_start(&self, pos: SourcePos) -> SourcePos {
    let idx = self.tokens.partition_point(|t| t.start() < pos);
    if idx < self.tokens.len() {
      self.tokens[idx].start()
    } else {
      self.hi()
    }
  }

  fn has_newline_between(&self, start: SourcePos, end: SourcePos) -> bool {
    if start >= end {
      return false;
    }
    self.slice(start, end).bytes().any(|b| b == b'\n' || b == b'\r')
  }

  /// Comments that lead the position `pos` (which is expected to be a token /
  /// node start). These are the comments in the gap before `pos` that are not
  /// trailing comments of the previous token.
  fn leading_comments(&self, pos: SourcePos) -> Vec<&'a Comment> {
    let prev_end = self.prev_token_end(pos);
    let has_prev = self.tokens.partition_point(|t| t.end() <= pos) > 0;
    let mut result = Vec::new();
    for comment in self.comments.iter() {
      let c_start = comment.span.start;
      if c_start < prev_end {
        continue;
      }
      if comment.span.end > pos {
        break;
      }
      // leading iff there's a line break before it (or no previous token).
      if !has_prev || self.has_newline_between(prev_end, c_start) {
        result.push(comment);
      }
    }
    result
  }

  /// Comments that trail the position `pos` (which is expected to be a token /
  /// node end). These are the comments in the gap after `pos`, on the same line
  /// as the preceding token, up to the next token.
  fn trailing_comments(&self, pos: SourcePos) -> Vec<&'a Comment> {
    let next_start = self.next_token_start(pos);
    let mut result = Vec::new();
    for comment in self.comments.iter() {
      let c_start = comment.span.start;
      if c_start < pos {
        continue;
      }
      if c_start >= next_start {
        break;
      }
      // trailing iff there's no line break between the preceding token and it.
      if !self.has_newline_between(pos, c_start) {
        result.push(comment);
      }
    }
    result
  }
}

/// An iterator over comments. Mirrors the subset of `deno_ast::CommentsIterator`
/// used by the generation layer (`is_empty`, `extend`, `Iterator`, `Clone`).
#[derive(Clone)]
pub struct CommentsIterator<'a> {
  comments: Vec<&'a Comment>,
  index: usize,
}

impl<'a> CommentsIterator<'a> {
  pub fn new(comments: Vec<&'a Comment>) -> CommentsIterator<'a> {
    CommentsIterator { comments, index: 0 }
  }

  pub fn empty() -> CommentsIterator<'a> {
    CommentsIterator::new(Vec::new())
  }

  pub fn is_empty(&self) -> bool {
    self.index >= self.comments.len()
  }

  pub fn extend(&mut self, other: impl IntoIterator<Item = &'a Comment>) {
    self.comments.extend(other);
  }

  /// Remaining comments without consuming the iterator.
  pub fn as_slice(&self) -> &[&'a Comment] {
    &self.comments[self.index..]
  }
}

impl<'a> Iterator for CommentsIterator<'a> {
  type Item = &'a Comment;

  fn next(&mut self) -> Option<Self::Item> {
    if self.index < self.comments.len() {
      let comment = self.comments[self.index];
      self.index += 1;
      Some(comment)
    } else {
      None
    }
  }
}

impl<'a> DoubleEndedIterator for CommentsIterator<'a> {
  fn next_back(&mut self) -> Option<Self::Item> {
    if self.index < self.comments.len() {
      self.comments.pop()
    } else {
      None
    }
  }
}

/// Source-range helpers, blanket-implemented for every `T: GetSpan`.
pub trait SourceRanged {
  fn start(&self) -> SourcePos;
  fn end(&self) -> SourcePos;

  fn range(&self) -> SourceRange {
    source_range(self.start(), self.end())
  }

  fn text_fast<'a>(&self, program: ProgramInfo<'a>) -> &'a str {
    &program.text()[self.start() as usize..self.end() as usize]
  }

  /// 0-based line index of the start position.
  fn start_line_fast(&self, program: ProgramInfo) -> usize {
    program.text_info().line_index(self.start() as usize)
  }

  /// 0-based line index of the end position.
  fn end_line_fast(&self, program: ProgramInfo) -> usize {
    program.text_info().line_index(self.end() as usize)
  }

  /// 0-based byte column of the start position.
  fn start_column_fast(&self, program: ProgramInfo) -> usize {
    let start = self.start() as usize;
    let line = program.text_info().line_index(start);
    start - program.text_info().line_start(line)
  }

  fn leading_comments_fast<'a>(&self, program: ProgramInfo<'a>) -> CommentsIterator<'a> {
    CommentsIterator::new(program.leading_comments(self.start()))
  }

  fn trailing_comments_fast<'a>(&self, program: ProgramInfo<'a>) -> CommentsIterator<'a> {
    CommentsIterator::new(program.trailing_comments(self.end()))
  }

  fn previous_token_fast<'a>(&self, program: ProgramInfo<'a>) -> Option<&'a Token> {
    let pos = self.start();
    let idx = program.tokens.partition_point(|t| t.end() <= pos);
    if idx > 0 {
      Some(&program.tokens[idx - 1])
    } else {
      None
    }
  }

  fn next_token_fast<'a>(&self, program: ProgramInfo<'a>) -> Option<&'a Token> {
    let pos = self.end();
    let idx = program.tokens.partition_point(|t| t.start() < pos);
    program.tokens.get(idx)
  }

  /// All tokens before this node.
  fn previous_tokens_fast<'a>(&self, program: ProgramInfo<'a>) -> &'a [Token] {
    let pos = self.start();
    let idx = program.tokens.partition_point(|t| t.start() < pos);
    &program.tokens[..idx]
  }

  /// All tokens after this node.
  fn next_tokens_fast<'a>(&self, program: ProgramInfo<'a>) -> &'a [Token] {
    let pos = self.end();
    let idx = program.tokens.partition_point(|t| t.start() < pos);
    &program.tokens[idx..]
  }

  /// Tokens contained within this node's range.
  fn tokens_fast<'a>(&self, program: ProgramInfo<'a>) -> &'a [Token] {
    let lo = program.tokens.partition_point(|t| t.start() < self.start());
    let hi = program.tokens.partition_point(|t| t.start() < self.end());
    &program.tokens[lo..hi]
  }
}

impl<T: GetSpan + ?Sized> SourceRanged for T {
  #[inline]
  fn start(&self) -> SourcePos {
    GetSpan::span(self).start
  }

  #[inline]
  fn end(&self) -> SourcePos {
    GetSpan::span(self).end
  }
}

/// Comment helpers. Comments don't implement `GetSpan`, so they can't use the
/// blanket `SourceRanged` impl; this provides the same method surface.
pub trait CommentExt {
  fn start(&self) -> SourcePos;
  fn end(&self) -> SourcePos;
  fn range(&self) -> SourceRange;
  fn text_fast<'a>(&self, program: ProgramInfo<'a>) -> &'a str;
  fn start_line_fast(&self, program: ProgramInfo) -> usize;
  fn end_line_fast(&self, program: ProgramInfo) -> usize;
  fn is_line(&self) -> bool;
  fn is_block(&self) -> bool;
}

impl CommentExt for Comment {
  #[inline]
  fn start(&self) -> SourcePos {
    self.span.start
  }

  #[inline]
  fn end(&self) -> SourcePos {
    self.span.end
  }

  #[inline]
  fn range(&self) -> SourceRange {
    self.span
  }

  fn text_fast<'a>(&self, program: ProgramInfo<'a>) -> &'a str {
    &program.text()[self.span.start as usize..self.span.end as usize]
  }

  fn start_line_fast(&self, program: ProgramInfo) -> usize {
    program.text_info().line_index(self.span.start as usize)
  }

  fn end_line_fast(&self, program: ProgramInfo) -> usize {
    program.text_info().line_index(self.span.end as usize)
  }

  #[inline]
  fn is_line(&self) -> bool {
    self.kind == CommentKind::Line
  }

  #[inline]
  fn is_block(&self) -> bool {
    self.kind != CommentKind::Line
  }
}

/// oxc's `Span` has no containment check, but the old `SourceRange` did; this
/// restores `range.contains(&other)` (true when `self` fully covers `other`).
pub trait RangeContains {
  fn contains(&self, other: &SourceRange) -> bool;
}

impl RangeContains for SourceRange {
  #[inline]
  fn contains(&self, other: &SourceRange) -> bool {
    self.start <= other.start && other.end <= self.end
  }
}

/// Position helpers. A bare byte offset (`SourcePos`) can't use the blanket
/// `SourceRanged` impl (it isn't a `GetSpan` type), so the lookups that the old
/// code invoked directly on positions live here. There's no ambiguity with
/// `SourceRanged` because `u32` doesn't implement `GetSpan`.
pub trait PosExt {
  fn as_source_pos(&self) -> SourcePos;
  fn leading_comments_fast<'a>(&self, program: ProgramInfo<'a>) -> CommentsIterator<'a>;
  fn trailing_comments_fast<'a>(&self, program: ProgramInfo<'a>) -> CommentsIterator<'a>;
  fn previous_token_fast<'a>(&self, program: ProgramInfo<'a>) -> Option<&'a Token>;
  fn next_token_fast<'a>(&self, program: ProgramInfo<'a>) -> Option<&'a Token>;
  fn line_fast(&self, program: ProgramInfo) -> usize;
}

impl PosExt for SourcePos {
  #[inline]
  fn as_source_pos(&self) -> SourcePos {
    *self
  }

  fn leading_comments_fast<'a>(&self, program: ProgramInfo<'a>) -> CommentsIterator<'a> {
    CommentsIterator::new(program.leading_comments(*self))
  }

  fn trailing_comments_fast<'a>(&self, program: ProgramInfo<'a>) -> CommentsIterator<'a> {
    CommentsIterator::new(program.trailing_comments(*self))
  }

  fn previous_token_fast<'a>(&self, program: ProgramInfo<'a>) -> Option<&'a Token> {
    let idx = program.tokens.partition_point(|t| t.end() <= *self);
    if idx > 0 {
      Some(&program.tokens[idx - 1])
    } else {
      None
    }
  }

  fn next_token_fast<'a>(&self, program: ProgramInfo<'a>) -> Option<&'a Token> {
    let idx = program.tokens.partition_point(|t| t.start() < *self);
    program.tokens.get(idx)
  }

  fn line_fast(&self, program: ProgramInfo) -> usize {
    program.text_info().line_index(*self as usize)
  }
}

/// Text helper for tokens (which don't implement `GetSpan`).
pub trait TokenExt {
  fn text_fast<'a>(&self, program: ProgramInfo<'a>) -> &'a str;
}

impl TokenExt for Token {
  fn text_fast<'a>(&self, program: ProgramInfo<'a>) -> &'a str {
    &program.text()[self.start() as usize..self.end() as usize]
  }
}
