use deno_ast::oxc::syntax::identifier::is_identifier_part;
use deno_ast::oxc::syntax::identifier::is_identifier_start;

use crate::generation::oxc_helpers::CommentExt;
use crate::generation::oxc_helpers::Node;
use crate::generation::oxc_helpers::ProgramInfo;
use crate::generation::oxc_helpers::SourceRanged;

/** Gets if the node contains a line comment or multi-line block comment */
pub fn contains_line_or_multiline_comment<'a>(node: Node<'a>, program: ProgramInfo<'a>) -> bool {
  let range = node.range();
  program
    .comments()
    .iter()
    .any(|comment| range.start < comment.start() && comment.end() < range.end && (comment.is_line() || comment.text_fast(program).contains('\n')))
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
