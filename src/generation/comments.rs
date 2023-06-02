use deno_ast::swc::parser::token::Token;
use deno_ast::swc::parser::token::TokenAndSpan;
use deno_ast::view::*;
use deno_ast::CommentsIterator;
use deno_ast::SourcePos;
use deno_ast::SourceRanged;
use deno_ast::SourceRangedForSpanned;
use deno_ast::SourceTextInfoProvider;

pub struct CommentTracker<'a> {
  program: Program<'a>,
  tokens: &'a [TokenAndSpan],
  token_index: usize,
}

impl<'a> CommentTracker<'a> {
  pub fn new(program: Program<'a>, tokens: &'a [TokenAndSpan]) -> CommentTracker<'a> {
    CommentTracker {
      program,
      tokens,
      token_index: 0,
    }
  }

  /// Gets the leading comments and all previously unhandled comments.
  pub fn leading_comments_with_previous(&mut self, pos: SourcePos) -> CommentsIterator<'a> {
    let mut iterator = CommentsIterator::new(Vec::new());

    if self.token_index == 0 {
      // get any comments stored at the beginning of the file
      // todo: investigate what's required here
      let file_start = self.program.text_info().range().start.as_source_pos();
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
    let mut iterator = CommentsIterator::new(Vec::new());

    while let Some(token) = self.tokens.get(self.token_index) {
      iterator.extend(token.start().leading_comments_fast(self.program));

      let is_comma = token.token == Token::Comma;
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
