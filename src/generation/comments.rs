use deno_ast::oxc::parser::Kind;
use deno_ast::oxc::parser::Token;

use super::oxc_helpers::CommentsIterator;
use super::oxc_helpers::PosExt;
use super::oxc_helpers::ProgramInfo;
use super::oxc_helpers::SourcePos;

pub struct CommentTracker<'a> {
  program: ProgramInfo<'a>,
  tokens: &'a [Token],
  token_index: usize,
}

impl<'a> CommentTracker<'a> {
  pub fn new(program: ProgramInfo<'a>, tokens: &'a [Token]) -> CommentTracker<'a> {
    CommentTracker {
      program,
      tokens,
      token_index: 0,
    }
  }

  /// Gets the leading comments and all previously unhandled comments.
  pub fn leading_comments_with_previous(&mut self, pos: SourcePos) -> CommentsIterator<'a> {
    let mut iterator = CommentsIterator::empty();

    if self.token_index == 0 {
      // get any comments stored at the beginning of the file
      // todo: investigate what's required here
      let file_start = self.program.lo();
      iterator.extend(file_start.leading_comments_fast(self.program));
      iterator.extend(file_start.trailing_comments_fast(self.program));
    } else if let Some(previous_token) = self.tokens.get(self.token_index - 1) {
      iterator.extend(previous_token.end().trailing_comments_fast(self.program));
    }

    while let Some(token) = self.tokens.get(self.token_index) {
      iterator.extend(token.start().leading_comments_fast(self.program));

      let token_end = token.end();
      if token_end < pos {
        iterator.extend(token_end.trailing_comments_fast(self.program));
        self.token_index += 1;
      } else {
        break;
      }
    }

    iterator
  }

  /// Gets the trailing comments and all previously unhandled comments
  pub fn trailing_comments_with_previous(&mut self, end: SourcePos) -> CommentsIterator<'a> {
    let mut iterator = CommentsIterator::empty();

    while let Some(token) = self.tokens.get(self.token_index) {
      iterator.extend(token.start().leading_comments_fast(self.program));

      let is_comma = token.kind() == Kind::Comma;
      if !is_comma && token.start() >= end {
        break;
      }

      let token_end = token.end();
      if is_comma || token_end <= end {
        iterator.extend(token.end().trailing_comments_fast(self.program));
        self.token_index += 1;
      } else {
        break;
      }
    }

    iterator
  }
}
