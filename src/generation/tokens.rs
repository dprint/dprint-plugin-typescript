use deno_ast::swc::parser::token::Token;
use deno_ast::swc::parser::token::TokenAndSpan;
use deno_ast::view::*;
use deno_ast::SourcePos;
use deno_ast::SourceRanged;
use deno_ast::SourceRangedForSpanned;
use deno_ast::SourceTextInfoProvider;
use std::str;

// todo: This is legacy from when swc-ecma-ast-view wasn't used.
// Eventually this should be phased out.

pub struct TokenFinder<'a> {
  program: Program<'a>,
}

impl<'a> TokenFinder<'a> {
  pub fn new(program: Program<'a>) -> TokenFinder<'a> {
    TokenFinder { program }
  }

  pub fn get_previous_token_if_open_paren(&self, node: &impl SourceRanged) -> Option<&'a TokenAndSpan> {
    self.get_previous_token_if(node, |token| token.token == Token::LParen)
  }

  pub fn get_next_token_if_close_paren(&self, node: &impl SourceRanged) -> Option<&'a TokenAndSpan> {
    self.get_next_token_if(node, |token| token.token == Token::RParen)
  }

  pub fn get_previous_token_if_open_brace(&self, node: &impl SourceRanged) -> Option<&'a TokenAndSpan> {
    self.get_previous_token_if(node, |token| token.token == Token::LBrace)
  }

  pub fn get_previous_token_if_open_bracket(&self, node: &impl SourceRanged) -> Option<&'a TokenAndSpan> {
    self.get_previous_token_if(node, |token| token.token == Token::LBracket)
  }

  pub fn get_previous_token_if_close_brace(&self, node: &impl SourceRanged) -> Option<&'a TokenAndSpan> {
    self.get_previous_token_if(node, |token| token.token == Token::RBrace)
  }

  pub fn get_previous_token_if_from_keyword(&self, node: &impl SourceRanged) -> Option<&'a TokenAndSpan> {
    self.get_previous_token_if(node, |token| token.text_fast(self.program) == "from")
  }

  pub fn get_previous_token_if_colon(&self, node: &impl SourceRanged) -> Option<&'a TokenAndSpan> {
    self.get_previous_token_if(node, |token| token.token == Token::Colon)
  }

  pub fn get_previous_token_if_operator(&self, node: &impl SourceRanged, operator_text: &str) -> Option<&'a TokenAndSpan> {
    self.get_previous_token_if(node, |token| token.text_fast(self.program) == operator_text)
  }

  #[inline]
  pub fn get_previous_token(&self, node: &impl SourceRanged) -> Option<&'a TokenAndSpan> {
    node.previous_token_fast(self.program)
  }

  pub fn get_next_token_if_comma(&self, node: &impl SourceRanged) -> Option<&'a TokenAndSpan> {
    self.get_next_token_if(node, |token| token.token == Token::Comma)
  }

  pub fn get_next_token_if_close_bracket(&self, node: &impl SourceRanged) -> Option<&'a TokenAndSpan> {
    self.get_next_token_if(node, |token| token.token == Token::RBracket)
  }

  pub fn get_first_open_brace_token_within(&self, node: &impl SourceRanged) -> Option<&'a TokenAndSpan> {
    self.get_first_token_within(node, |token| token.token == Token::LBrace)
  }

  pub fn get_last_token_within_if_comma(&self, node: &impl SourceRanged) -> Option<&'a TokenAndSpan> {
    self.get_last_token_within_if(node, |token| token.token == Token::Comma)
  }

  pub fn get_first_semi_colon_after(&self, node: &impl SourceRanged) -> Option<&'a TokenAndSpan> {
    self.get_first_token_after(node, |token| token.token == Token::Semi)
  }

  pub fn get_first_colon_token_after(&self, node: &impl SourceRanged) -> Option<&'a TokenAndSpan> {
    self.get_first_token_after(node, |token| token.token == Token::Colon)
  }

  pub fn get_first_colon_token_within(&self, node: &impl SourceRanged) -> Option<&'a TokenAndSpan> {
    self.get_first_token_within(node, |token| token.token == Token::Colon)
  }

  pub fn get_first_operator_after(&self, node: &impl SourceRanged, operator_text: &str) -> Option<&'a TokenAndSpan> {
    self.get_first_token_after_with_text(node, operator_text)
  }

  pub fn get_first_keyword_after(&self, node: &impl SourceRanged, keyword_text: &str) -> Option<&'a TokenAndSpan> {
    self.get_first_token_after_with_text(node, keyword_text)
  }

  pub fn get_first_open_paren_before(&self, node: &impl SourceRanged) -> Option<&'a TokenAndSpan> {
    self.get_first_token_before(node, |token| token.token == Token::LParen)
  }

  pub fn get_first_close_paren_before(&self, node: &impl SourceRanged) -> Option<&'a TokenAndSpan> {
    self.get_first_token_before(node, |token| token.token == Token::RParen)
  }

  pub fn get_first_close_paren_after(&self, node: &impl SourceRanged) -> Option<&'a TokenAndSpan> {
    self.get_first_token_after(node, |token| token.token == Token::RParen)
  }

  pub fn get_previous_token_end_before(&self, node: &impl SourceRanged) -> SourcePos {
    let previous_token = self.get_previous_token(node);
    if let Some(token) = previous_token {
      token.end()
    } else {
      self.program.text_info().range().start.into()
    }
  }

  pub fn get_next_token_pos_after(&self, node: &impl SourceRanged) -> SourcePos {
    let next_token = self.get_next_token(node);
    if let Some(token) = next_token {
      token.start()
    } else {
      self.program.end()
    }
  }

  #[inline]
  fn get_first_token_after_with_text(&self, node: &impl SourceRanged, text: &str) -> Option<&'a TokenAndSpan> {
    self.get_first_token_after(node, |token| token.text_fast(self.program) == text)
  }

  #[inline]
  fn get_next_token_if(&self, node: &impl SourceRanged, is_match: impl FnOnce(&TokenAndSpan) -> bool) -> Option<&'a TokenAndSpan> {
    if let Some(next_token) = node.next_token_fast(self.program) {
      if is_match(next_token) {
        return Some(next_token);
      }
    }
    None
  }

  #[inline]
  fn get_previous_token_if(&self, node: &impl SourceRanged, is_match: impl FnOnce(&TokenAndSpan) -> bool) -> Option<&'a TokenAndSpan> {
    if let Some(previous_token) = node.previous_token_fast(self.program) {
      if is_match(previous_token) {
        return Some(previous_token);
      }
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
