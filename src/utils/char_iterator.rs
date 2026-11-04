use std::iter::Peekable;
use std::str::Chars;

pub struct IteratorCharExt<'a>(Peekable<Chars<'a>>);

impl<'a> IteratorCharExt<'a> {
  pub fn new(chars: Peekable<Chars<'a>>) -> Self {
    Self(chars)
  }

  pub fn next(&mut self) -> Option<char> {
    self.0.next()
  }

  pub fn peek(&mut self) -> Option<&char> {
    self.0.peek()
  }

  pub fn check_text(&mut self, text: &str) -> bool {
    for text_ch in text.chars() {
      match self.peek() {
        Some(ch) if *ch == text_ch => self.next(),
        _ => return false,
      };
    }
    true
  }

  pub fn skip_whitespace(&mut self) {
    self.skip_while(char::is_whitespace);
  }

  pub fn skip_spaces(&mut self) {
    self.skip_while(|c| c == ' ');
  }

  pub fn skip_all_until_new_line(&mut self) {
    self.skip_while(|c| c != '\n');
  }

  pub fn skip_while<P: Fn(char) -> bool>(&mut self, predicate: P) {
    while let Some(ch) = self.peek() {
      if !predicate(*ch) {
        break;
      }
      self.next();
    }
  }
}
