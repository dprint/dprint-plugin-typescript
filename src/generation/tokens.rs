use deno_ast::oxc::parser::Kind;
use deno_ast::oxc::parser::Token;

use super::oxc_helpers::ProgramInfo;
use super::oxc_helpers::SourcePos;
use super::oxc_helpers::SourceRanged;
use super::oxc_helpers::TokenExt;

// todo: This is legacy from when swc-ecma-ast-view wasn't used.
// Eventually this should be phased out.

pub struct TokenFinder<'a> {
  program: ProgramInfo<'a>,
}

impl<'a> TokenFinder<'a> {
  pub fn new(program: ProgramInfo<'a>) -> TokenFinder<'a> {
    TokenFinder { program }
  }

  pub fn get_previous_token_if_open_paren(&self, node: &impl SourceRanged) -> Option<&'a Token> {
    self.get_previous_token_if(node, |token| token.kind() == Kind::LParen)
  }

  pub fn get_next_token_if_close_paren(&self, node: &impl SourceRanged) -> Option<&'a Token> {
    self.get_next_token_if(node, |token| token.kind() == Kind::RParen)
  }

  pub fn get_previous_token_if_open_brace(&self, node: &impl SourceRanged) -> Option<&'a Token> {
    self.get_previous_token_if(node, |token| token.kind() == Kind::LCurly)
  }

  pub fn get_previous_token_if_open_bracket(&self, node: &impl SourceRanged) -> Option<&'a Token> {
    self.get_previous_token_if(node, |token| token.kind() == Kind::LBrack)
  }

  pub fn get_previous_token_if_close_brace(&self, node: &impl SourceRanged) -> Option<&'a Token> {
    self.get_previous_token_if(node, |token| token.kind() == Kind::RCurly)
  }

  pub fn get_previous_token_if_from_keyword(&self, node: &impl SourceRanged) -> Option<&'a Token> {
    self.get_previous_token_if(node, |token| token.text_fast(self.program) == "from")
  }

  pub fn get_previous_token_if_colon(&self, node: &impl SourceRanged) -> Option<&'a Token> {
    self.get_previous_token_if(node, |token| token.kind() == Kind::Colon)
  }

  pub fn get_previous_token_if_operator(&self, node: &impl SourceRanged, operator_text: &str) -> Option<&'a Token> {
    self.get_previous_token_if(node, |token| token.text_fast(self.program) == operator_text)
  }

  #[inline]
  pub fn get_previous_token(&self, node: &impl SourceRanged) -> Option<&'a Token> {
    node.previous_token_fast(self.program)
  }

  pub fn get_next_token_if_comma(&self, node: &impl SourceRanged) -> Option<&'a Token> {
    self.get_next_token_if(node, |token| token.kind() == Kind::Comma)
  }

  pub fn get_next_token_if_close_bracket(&self, node: &impl SourceRanged) -> Option<&'a Token> {
    self.get_next_token_if(node, |token| token.kind() == Kind::RBrack)
  }

  pub fn get_first_open_brace_token_within(&self, node: &impl SourceRanged) -> Option<&'a Token> {
    self.get_first_token_within(node, |token| token.kind() == Kind::LCurly)
  }

  pub fn get_last_token_within_if_comma(&self, node: &impl SourceRanged) -> Option<&'a Token> {
    self.get_last_token_within_if(node, |token| token.kind() == Kind::Comma)
  }

  pub fn get_first_semi_colon_after(&self, node: &impl SourceRanged) -> Option<&'a Token> {
    self.get_first_token_after(node, |token| token.kind() == Kind::Semicolon)
  }

  pub fn get_first_colon_token_after(&self, node: &impl SourceRanged) -> Option<&'a Token> {
    self.get_first_token_after(node, |token| token.kind() == Kind::Colon)
  }

  pub fn get_first_colon_token_within(&self, node: &impl SourceRanged) -> Option<&'a Token> {
    self.get_first_token_within(node, |token| token.kind() == Kind::Colon)
  }

  pub fn get_first_operator_after(&self, node: &impl SourceRanged, operator_text: &str) -> Option<&'a Token> {
    self.get_first_token_after_with_text(node, operator_text)
  }

  pub fn get_first_keyword_after(&self, node: &impl SourceRanged, keyword_text: &str) -> Option<&'a Token> {
    self.get_first_token_after_with_text(node, keyword_text)
  }

  pub fn get_first_open_paren_before(&self, node: &impl SourceRanged) -> Option<&'a Token> {
    self.get_first_token_before(node, |token| token.kind() == Kind::LParen)
  }

  pub fn get_first_close_paren_before(&self, node: &impl SourceRanged) -> Option<&'a Token> {
    self.get_first_token_before(node, |token| token.kind() == Kind::RParen)
  }

  pub fn get_first_close_paren_after(&self, node: &impl SourceRanged) -> Option<&'a Token> {
    self.get_first_token_after(node, |token| token.kind() == Kind::RParen)
  }

  pub fn get_previous_token_end_before(&self, node: &impl SourceRanged) -> SourcePos {
    let previous_token = self.get_previous_token(node);
    if let Some(token) = previous_token {
      token.end()
    } else {
      self.program.lo()
    }
  }

  pub fn get_next_token_pos_after(&self, node: &impl SourceRanged) -> SourcePos {
    let next_token = self.get_next_token(node);
    if let Some(token) = next_token {
      token.start()
    } else {
      self.program.hi()
    }
  }

  #[inline]
  fn get_first_token_after_with_text(&self, node: &impl SourceRanged, text: &str) -> Option<&'a Token> {
    self.get_first_token_after(node, |token| token.text_fast(self.program) == text)
  }

  #[inline]
  fn get_next_token_if(&self, node: &impl SourceRanged, is_match: impl FnOnce(&Token) -> bool) -> Option<&'a Token> {
    if let Some(next_token) = node.next_token_fast(self.program) {
      if is_match(next_token) {
        return Some(next_token);
      }
    }
    None
  }

  #[inline]
  fn get_previous_token_if(&self, node: &impl SourceRanged, is_match: impl FnOnce(&Token) -> bool) -> Option<&'a Token> {
    if let Some(previous_token) = node.previous_token_fast(self.program) {
      if is_match(previous_token) {
        return Some(previous_token);
      }
    }
    None
  }

  #[inline]
  fn get_next_token(&self, node: &impl SourceRanged) -> Option<&'a Token> {
    node.next_token_fast(self.program)
  }

  #[inline]
  fn get_first_token_before(&self, node: &impl SourceRanged, is_match: impl Fn(&Token) -> bool) -> Option<&'a Token> {
    node.previous_tokens_fast(self.program).iter().rev().find(|token| is_match(token))
  }

  #[inline]
  fn get_first_token_after(&self, node: &impl SourceRanged, is_match: impl Fn(&Token) -> bool) -> Option<&'a Token> {
    node.next_tokens_fast(self.program).iter().find(|token| is_match(token))
  }

  #[inline]
  fn get_first_token_within(&self, node: &impl SourceRanged, is_match: impl Fn(&Token) -> bool) -> Option<&'a Token> {
    node.tokens_fast(self.program).iter().find(|token| is_match(token))
  }

  #[inline]
  fn get_last_token_within_if(&self, node: &impl SourceRanged, is_match: impl Fn(&Token) -> bool) -> Option<&'a Token> {
    node.tokens_fast(self.program).last().filter(|token| is_match(token))
  }
}
