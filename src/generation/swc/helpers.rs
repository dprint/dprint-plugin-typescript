use deno_ast::swc::common::comments::CommentKind;
use deno_ast::swc::parser::lexer::util::CharExt;
use deno_ast::view::Node;
use deno_ast::view::Program;
use deno_ast::RootNode;
use deno_ast::SourceRanged;
use deno_ast::SourceRangedForSpanned;

/** Gets if the node contains a line comment or multi-line block comment */
pub fn contains_line_or_multiline_comment<'a>(node: Node<'a>, program: Program<'a>) -> bool {
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
    if (i == 0 && !c.is_ident_start()) || !c.is_ident_part() {
      return false;
    }
  }
  true
}
