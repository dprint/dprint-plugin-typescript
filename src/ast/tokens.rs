use oxc_parser::Kind;

use super::SourcePos;
use super::SourceRanged;

/// A token in the source text.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TokenAndSpan {
  pub kind: Kind,
  pub start: SourcePos,
  pub end: SourcePos,
}

impl TokenAndSpan {
  pub fn from_oxc(token: &oxc_parser::Token) -> Self {
    Self {
      kind: token.kind(),
      start: SourcePos::new(token.start()),
      end: SourcePos::new(token.end()),
    }
  }
}

impl SourceRanged for TokenAndSpan {
  fn start(&self) -> SourcePos {
    self.start
  }

  fn end(&self) -> SourcePos {
    self.end
  }
}

/// The tokens of a program, in source order.
pub struct TokenContainer {
  tokens: Vec<TokenAndSpan>,
}

impl TokenContainer {
  /// Creates the container from the tokens of a program, along with a token
  /// for its hashbang, which swc had a token for.
  pub fn new(tokens: &[oxc_parser::Token], hashbang: Option<oxc_span::Span>) -> Self {
    let hashbang_token = hashbang.map(|span| TokenAndSpan {
      kind: Kind::HashbangComment,
      start: SourcePos::new(span.start),
      end: SourcePos::new(span.end),
    });
    Self {
      tokens: hashbang_token
        .into_iter()
        .chain(tokens.iter().filter(|t| !t.kind().is_eof()).map(TokenAndSpan::from_oxc))
        .collect(),
    }
  }

  pub fn tokens(&self) -> &[TokenAndSpan] {
    &self.tokens
  }

  /// Gets the tokens within the provided range.
  pub fn get_tokens_in_range(&self, start: SourcePos, end: SourcePos) -> &[TokenAndSpan] {
    let start_index = self.tokens.partition_point(|t| t.start < start);
    let end_index = self.tokens.partition_point(|t| t.end <= end).max(start_index);
    &self.tokens[start_index..end_index]
  }

  /// Gets the last token that ends at or before the provided position.
  pub fn get_previous_token(&self, pos: SourcePos) -> Option<&TokenAndSpan> {
    let index = self.tokens.partition_point(|t| t.end <= pos);
    if index == 0 { None } else { self.tokens.get(index - 1) }
  }

  /// Gets the first token that starts at or after the provided position.
  pub fn get_next_token(&self, pos: SourcePos) -> Option<&TokenAndSpan> {
    let index = self.tokens.partition_point(|t| t.start < pos);
    self.tokens.get(index)
  }

  /// Gets the tokens that start before the provided position.
  pub fn get_previous_tokens(&self, pos: SourcePos) -> &[TokenAndSpan] {
    let index = self.tokens.partition_point(|t| t.start < pos);
    &self.tokens[..index]
  }

  /// Gets the tokens that start at or after the provided position.
  pub fn get_next_tokens(&self, pos: SourcePos) -> &[TokenAndSpan] {
    let index = self.tokens.partition_point(|t| t.start < pos);
    &self.tokens[index..]
  }
}
