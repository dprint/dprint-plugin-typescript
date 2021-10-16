use std::{iter::Peekable, str::Chars};

pub trait IteratorCharExt: Iterator<Item = char> {
  fn peek(&mut self) -> Option<&char>;

  fn check_text(&mut self, text: &str) -> bool {
    let mut text_iter = text.chars();
    while let Some(text_ch) = text_iter.next() {
      match self.peek() {
        Some(ch) if *ch == text_ch => self.next(),
        _ => return false,
      };
    }
    true
  }

  fn skip_whitespace(&mut self) {
    self.skip_while(char::is_whitespace);
  }

  fn skip_spaces(&mut self) {
    self.skip_while(|c| c == ' ');
  }

  fn skip_all_until_new_line(&mut self) {
    self.skip_while(|c| c != '\n');
  }

  fn skip_while<P: Fn(char) -> bool>(&mut self, predicate: P) {
    while let Some(ch) = self.peek() {
      if !predicate(*ch) {
        break;
      }
      self.next();
    }
  }
}

impl<'a> IteratorCharExt for Peekable<Chars<'a>> {
  fn peek(&mut self) -> Option<&char> {
    self.peek()
  }
}
