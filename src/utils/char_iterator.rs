// todo: This is not good at all and very specific to it's use at the moment.
// Over time this should be improved.

pub struct CharIterator<'a> {
    chars: std::str::Chars<'a>,
    next_char: Option<char>,
}

impl<'a> CharIterator<'a> {
    pub fn new(chars: std::str::Chars<'a>) -> CharIterator<'a> {
        CharIterator {
            chars,
            next_char: None,
        }
    }

    pub fn skip_whitespace(&mut self) {
        while let Some(c) = self.peek_next() {
            if !c.is_whitespace() {
                return;
            } else {
                self.move_next();
            }
        }
    }

    pub fn skip_spaces(&mut self) {
        while let Some(c) = self.peek_next() {
            if c != ' ' {
                return;
            } else {
                self.move_next();
            }
        }
    }

    pub fn skip_all_until_new_line(&mut self) {
        while let Some(c) = self.move_next() {
            if c == '\r' {
                if self.peek_next() == Some('\n') {
                    self.move_next();
                }
                return;
            } else if c == '\n' {
                return;
            }
        }
    }

    pub fn check_text(&mut self, text: &str) -> bool {
        for c in text.chars() {
            if let Some(comparison_char) = self.peek_next() {
                if comparison_char != c {
                    return false;
                } else {
                    self.move_next();
                }
            } else {
                return false;
            }
        }

        true
    }

    pub fn move_next(&mut self) -> Option<char> {
        self.ensure_next_char();
        let current_char = self.next_char;
        self.next_char = self.chars.next();
        current_char
    }

    pub fn peek_next(&mut self) -> Option<char> {
        self.ensure_next_char();
        self.next_char
    }

    fn ensure_next_char(&mut self) {
        if self.next_char.is_none() {
            self.next_char = self.chars.next();
        }
    }
}
