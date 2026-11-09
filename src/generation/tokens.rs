use crate::ast::*;

pub struct TokenFinder<'a> {
  program: ProgramInfo<'a>,
}

impl<'a> TokenFinder<'a> {
  pub fn new(program: ProgramInfo<'a>) -> TokenFinder<'a> {
    TokenFinder { program }
  }

  pub fn get_previous_token_if_open_paren(&self, node: &impl SourceRanged) -> Option<&'a TokenAndSpan> {
    self.get_previous_token_if(node, |token| token.kind == TokenKind::LParen)
  }

  pub fn get_next_token_if_close_paren(&self, node: &impl SourceRanged) -> Option<&'a TokenAndSpan> {
    self.get_next_token_if(node, |token| token.kind == TokenKind::RParen)
  }

  pub fn get_previous_token_if_open_brace(&self, node: &impl SourceRanged) -> Option<&'a TokenAndSpan> {
    self.get_previous_token_if(node, |token| token.kind == TokenKind::LCurly)
  }

  pub fn get_previous_token_if_open_bracket(&self, node: &impl SourceRanged) -> Option<&'a TokenAndSpan> {
    self.get_previous_token_if(node, |token| token.kind == TokenKind::LBrack)
  }

  pub fn get_previous_token_if_close_brace(&self, node: &impl SourceRanged) -> Option<&'a TokenAndSpan> {
    self.get_previous_token_if(node, |token| token.kind == TokenKind::RCurly)
  }

  pub fn get_previous_token_if_from_keyword(&self, node: &impl SourceRanged) -> Option<&'a TokenAndSpan> {
    self.get_previous_token_if(node, |token| token.text_fast(self.program) == "from")
  }

  pub fn get_previous_token_if_colon(&self, node: &impl SourceRanged) -> Option<&'a TokenAndSpan> {
    self.get_previous_token_if(node, |token| token.kind == TokenKind::Colon)
  }

  pub fn get_previous_token_if_operator(&self, node: &impl SourceRanged, operator_text: &str) -> Option<&'a TokenAndSpan> {
    self.get_previous_token_if(node, |token| token.text_fast(self.program) == operator_text)
  }

  #[inline]
  pub fn get_previous_token(&self, node: &impl SourceRanged) -> Option<&'a TokenAndSpan> {
    node.previous_token_fast(self.program)
  }

  pub fn get_next_token_if_comma(&self, node: &impl SourceRanged) -> Option<&'a TokenAndSpan> {
    self.get_next_token_if(node, |token| token.kind == TokenKind::Comma)
  }

  pub fn get_next_token_if_close_bracket(&self, node: &impl SourceRanged) -> Option<&'a TokenAndSpan> {
    self.get_next_token_if(node, |token| token.kind == TokenKind::RBrack)
  }

  pub fn get_first_open_brace_token_within(&self, node: &impl SourceRanged) -> Option<&'a TokenAndSpan> {
    self.get_first_token_within(node, |token| token.kind == TokenKind::LCurly)
  }

  pub fn get_last_token_within_if_comma(&self, node: &impl SourceRanged) -> Option<&'a TokenAndSpan> {
    self.get_last_token_within_if(node, |token| token.kind == TokenKind::Comma)
  }

  pub fn get_first_semi_colon_after(&self, node: &impl SourceRanged) -> Option<&'a TokenAndSpan> {
    self.get_first_token_after(node, |token| token.kind == TokenKind::Semicolon)
  }

  pub fn get_first_colon_token_after(&self, node: &impl SourceRanged) -> Option<&'a TokenAndSpan> {
    self.get_first_token_after(node, |token| token.kind == TokenKind::Colon)
  }

  pub fn get_first_colon_token_within(&self, node: &impl SourceRanged) -> Option<&'a TokenAndSpan> {
    self.get_first_token_within(node, |token| token.kind == TokenKind::Colon)
  }

  pub fn get_first_operator_after(&self, node: &impl SourceRanged, operator_text: &str) -> Option<&'a TokenAndSpan> {
    self.get_first_token_after_with_text(node, operator_text)
  }

  pub fn get_first_keyword_after(&self, node: &impl SourceRanged, keyword_text: &str) -> Option<&'a TokenAndSpan> {
    self.get_first_token_after_with_text(node, keyword_text)
  }

  pub fn get_first_open_paren_before(&self, node: &impl SourceRanged) -> Option<&'a TokenAndSpan> {
    self.get_first_token_before(node, |token| token.kind == TokenKind::LParen)
  }

  pub fn get_first_close_paren_before(&self, node: &impl SourceRanged) -> Option<&'a TokenAndSpan> {
    self.get_first_token_before(node, |token| token.kind == TokenKind::RParen)
  }

  pub fn get_first_close_paren_after(&self, node: &impl SourceRanged) -> Option<&'a TokenAndSpan> {
    self.get_first_token_after(node, |token| token.kind == TokenKind::RParen)
  }

  pub fn get_previous_token_end_before(&self, node: &impl SourceRanged) -> SourcePos {
    let previous_token = self.get_previous_token(node);
    if let Some(token) = previous_token { token.end() } else { SourcePos::START }
  }

  pub fn get_next_token_pos_after(&self, node: &impl SourceRanged) -> SourcePos {
    let next_token = self.get_next_token(node);
    if let Some(token) = next_token { token.start() } else { self.program.end() }
  }

  #[inline]
  fn get_first_token_after_with_text(&self, node: &impl SourceRanged, text: &str) -> Option<&'a TokenAndSpan> {
    self.get_first_token_after(node, |token| token.text_fast(self.program) == text)
  }

  #[inline]
  fn get_next_token_if(&self, node: &impl SourceRanged, is_match: impl FnOnce(&TokenAndSpan) -> bool) -> Option<&'a TokenAndSpan> {
    if let Some(next_token) = node.next_token_fast(self.program)
      && is_match(next_token)
    {
      return Some(next_token);
    }
    None
  }

  #[inline]
  fn get_previous_token_if(&self, node: &impl SourceRanged, is_match: impl FnOnce(&TokenAndSpan) -> bool) -> Option<&'a TokenAndSpan> {
    if let Some(previous_token) = node.previous_token_fast(self.program)
      && is_match(previous_token)
    {
      return Some(previous_token);
    }
    None
  }

  #[inline]
  fn get_next_token(&self, node: &impl SourceRanged) -> Option<&'a TokenAndSpan> {
    node.next_token_fast(self.program)
  }

  #[inline]
  fn get_first_token_before(&self, node: &impl SourceRanged, is_match: impl Fn(&TokenAndSpan) -> bool) -> Option<&'a TokenAndSpan> {
    node
      .previous_tokens_fast(self.program)
      .iter()
      .rev()
      .find(|token_and_span| is_match(token_and_span))
  }

  #[inline]
  fn get_first_token_after(&self, node: &impl SourceRanged, is_match: impl Fn(&'a TokenAndSpan) -> bool) -> Option<&'a TokenAndSpan> {
    node.next_tokens_fast(self.program).iter().find(|token_and_span| is_match(token_and_span))
  }

  #[inline]
  fn get_first_token_within(&self, node: &impl SourceRanged, is_match: impl Fn(&'a TokenAndSpan) -> bool) -> Option<&'a TokenAndSpan> {
    node.tokens_fast(self.program).iter().find(|token_and_span| is_match(token_and_span))
  }

  #[inline]
  fn get_last_token_within_if(&self, node: &impl SourceRanged, is_match: impl Fn(&'a TokenAndSpan) -> bool) -> Option<&'a TokenAndSpan> {
    node.tokens_fast(self.program).last().filter(|token| is_match(token))
  }
}
