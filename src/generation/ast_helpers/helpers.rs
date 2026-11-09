use oxc_syntax::identifier::is_identifier_part;
use oxc_syntax::identifier::is_identifier_start;

use crate::ast::*;

/// Gets if the node contains a line comment or multi-line block comment.
pub fn contains_line_or_multiline_comment<'a>(node: Node<'a>, program: ProgramInfo<'a>) -> bool {
  let tokens = node.tokens_fast(program);
  let comments = program.comment_container();
  for (i, token) in tokens.iter().enumerate() {
    if i > 0 {
      let mut leading_comments = comments.leading_comments(token.start());
      if leading_comments.any(|c| c.kind == CommentKind::Line || c.text.contains('\n')) {
        return true;
      }
    }
    if i < tokens.len() - 1 {
      let mut trailing_comments = comments.trailing_comments(token.start());
      if trailing_comments.any(|c| c.kind == CommentKind::Line || c.text.contains('\n')) {
        return true;
      }
    }
  }
  false
}

pub fn is_text_valid_identifier(string_value: &str) -> bool {
  if string_value.is_empty() {
    return false;
  }
  for (i, c) in string_value.chars().enumerate() {
    if (i == 0 && !is_identifier_start(c)) || !is_identifier_part(c) {
      return false;
    }
  }
  true
}
