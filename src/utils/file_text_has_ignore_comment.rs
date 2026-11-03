use crate::utils::char_iterator::IteratorCharExt;
use std::iter::Iterator;

pub fn file_text_has_ignore_comment(file_text: &str, ignore_text: &str) -> bool {
  let mut chars = IteratorCharExt::new(file_text.chars().peekable());

  // skip over the shebang
  if file_text.starts_with("#!") {
    chars.next();
    chars.next();
    chars.skip_all_until_new_line();
  }

  // now handle the comments
  while chars.peek().is_some() {
    chars.skip_whitespace();
    if chars.next() != Some('/') {
      return false;
    }
    match chars.next() {
      Some('/') => {
        if check_single_line_comment(&mut chars, ignore_text) {
          return true;
        }
      }
      Some('*') => {
        if check_multi_line_comment(&mut chars, ignore_text) {
          return true;
        }
      }
      _ => return false,
    }
  }
  return false;

  fn check_single_line_comment(chars: &mut IteratorCharExt, ignore_text: &str) -> bool {
    chars.skip_spaces(); // only spaces, not whitespace
    if chars.check_text(ignore_text) {
      return true;
    }
    chars.skip_all_until_new_line();
    false
  }

  fn check_multi_line_comment(chars: &mut IteratorCharExt, ignore_text: &str) -> bool {
    chars.skip_whitespace();
    if chars.check_text(ignore_text) {
      return true;
    }

    while let Some(c) = chars.next() {
      if c == '*' && chars.peek() == Some(&'/') {
        chars.next();
        return false;
      }
    }
    false
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  fn run_test(text: &str, expected_result: bool) {
    let actual_result = file_text_has_ignore_comment(text, "ignore-file");
    assert_eq!(actual_result, expected_result);
  }

  #[test]
  fn it_should_be_true_when_first_comment_in_file() {
    run_test("// ignore-file\ntest;", true);
  }

  #[test]
  fn it_should_be_true_when_not_first_and_other_types_of_comments() {
    run_test("// test\r\n\n  \n  //\n/*testing\nthis out\r\ntest\n*/\n   // ignore-file\n", true);
  }

  #[test]
  fn it_should_be_true_when_multi_line_comment() {
    run_test("/* ignore-file */\ntest;", true);
  }

  #[test]
  fn it_should_be_true_when_multi_line_comment_and_new_lines_before() {
    run_test("/*\n\r\nignore-file */\ntest;", true);
  }

  #[test]
  fn it_should_skip_over_shebang() {
    run_test("#!/usr/bin/env node\n//ignore-file", true);
  }
}
