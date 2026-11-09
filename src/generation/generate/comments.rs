use super::*;

/* comments */

pub(super) fn gen_leading_comments<'a>(node: &SourceRange, context: &mut Context<'a>) -> PrintItems<'a> {
  let leading_comments = node.leading_comments_fast(context.program);
  gen_comments_as_leading(node, leading_comments, context)
}

pub(super) fn gen_comments_as_leading<'a>(node: &SourceRange, comments: CommentsIterator<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  if let Some(last_comment) = comments.peek_last_comment() {
    let last_comment_previously_handled = context.has_handled_comment(last_comment);

    items.extend(gen_comment_collection(comments, None, Some(node), context));

    // todo: this doesn't seem exactly right...
    if !last_comment_previously_handled {
      let node_start_line = node.start_line_fast(context.program);
      let last_comment_end_line = last_comment.end_line_fast(context.program);
      if node_start_line > last_comment_end_line {
        items.push_condition(if_true_or(
          "spaceIfForcingNoNewlines",
          condition_resolvers::is_forcing_no_newlines(),
          Signal::SpaceIfNotTrailing.into_print_items(context.arena),
          {
            let mut items = PrintItems::new(context.arena);
            items.push_signal(Signal::NewLine);
            push_blank_lines(&mut items, cap_blank_line_count(node_start_line - last_comment_end_line - 1, context));
            items
          },
        ));
      } else if last_comment.kind == CommentKind::Block {
        if node_start_line == last_comment_end_line {
          items.push_signal(Signal::SpaceIfNotTrailing);
        } else {
          // use a space if no newlines are currently being forced
          items.push_condition(if_true(
            "conditionalCommentBlockSpace",
            condition_resolvers::is_forcing_no_newlines(),
            Signal::SpaceIfNotTrailing.into_print_items(context.arena),
          ));
        }
      }
    }
  }

  items
}

pub(super) fn gen_trailing_comments_as_statements<'a>(node: &SourceRange, context: &mut Context<'a>) -> PrintItems<'a> {
  let comments = get_trailing_comments_as_statements(node, context);
  gen_comments_as_statements(comments.into_iter(), Some(node), context)
}

pub(super) fn gen_leading_comments_on_previous_lines<'a>(node: &SourceRange, context: &mut Context<'a>) -> PrintItems<'a> {
  let unhandled_comments = get_leading_comments_on_previous_lines(node, context);
  gen_comments_as_statements(unhandled_comments.into_iter(), None, context)
}

pub(super) fn get_leading_comments_on_previous_lines<'a>(node: &SourceRange, context: &mut Context<'a>) -> Vec<&'a Comment<'a>> {
  let leading_comments = node.leading_comments_fast(context.program);
  if leading_comments.is_empty() {
    return Vec::new(); // avoid extra calculations
  }
  let node_start_line = node.start_line_fast(context.program);
  let previous_token_line = node.previous_token_fast(context.program).map(|t| t.end_line_fast(context.program));
  leading_comments
    .take_while(|c| {
      let line = c.start_line_fast(context.program);
      line < node_start_line && previous_token_line.map(|line| line < node_start_line).unwrap_or(true)
    })
    .collect::<Vec<_>>()
}

pub(super) fn get_leading_comments_on_same_line<'a>(node: &SourceRange, context: &mut Context<'a>) -> Vec<&'a Comment<'a>> {
  let leading_comments = node.leading_comments_fast(context.program);
  if leading_comments.is_empty() {
    Vec::new()
  } else {
    let node_start_line = node.start_line_fast(context.program);
    leading_comments
      .filter(|c| c.start_line_fast(context.program) == node_start_line)
      .collect::<Vec<_>>()
  }
}

pub(super) fn get_trailing_comments_on_same_line<'a>(node: &SourceRange, context: &mut Context<'a>) -> Vec<&'a Comment<'a>> {
  let trailing_comments = node.trailing_comments_fast(context.program);
  if trailing_comments.is_empty() {
    Vec::new()
  } else {
    let node_start_line = node.start_line_fast(context.program);
    trailing_comments
      .take_while(|c| c.start_line_fast(context.program) == node_start_line)
      .collect::<Vec<_>>()
  }
}

pub(super) fn get_trailing_comments_as_statements<'a>(node: &SourceRange, context: &mut Context<'a>) -> Vec<&'a Comment<'a>> {
  let mut comments = Vec::new();
  let node_end_line = node.end_line_fast(context.program);
  for comment in node.trailing_comments_fast(context.program) {
    if !context.has_handled_comment(comment) && node_end_line < comment.end_line_fast(context.program) {
      comments.push(comment);
    }
  }
  comments
}

pub(super) fn gen_comments_as_statements<'a>(
  comments: impl Iterator<Item = &'a Comment<'a>>,
  last_node: Option<&SourceRange>,
  context: &mut Context<'a>,
) -> PrintItems<'a> {
  let mut last_node = last_node.map(|l| l.range());
  let mut items = PrintItems::new(context.arena);
  let mut was_last_block_comment = false;
  for comment in comments {
    if !context.has_handled_comment(comment) {
      items.extend(gen_comment_based_on_last_node(
        comment,
        &last_node,
        GenCommentBasedOnLastNodeOptions { separate_with_newlines: true },
        context,
      ));
      last_node = Some(comment.range());
      was_last_block_comment = comment.kind == CommentKind::Block;
    }
  }
  if was_last_block_comment {
    items.push_signal(Signal::ExpectNewLine);
  }
  items
}

/// How many of an import's leading comments are *detached* from it, meaning a
/// blank line separates them from it (ex. a file header). Those stay where they
/// are when the imports get reordered, while the rest are attached to the import
/// and travel with it.
pub(crate) fn detached_preamble_len(comments: &[&Comment], node: Node, program: ProgramInfo) -> usize {
  let mut next_line = node.start_line_fast(program);
  for (i, comment) in comments.iter().enumerate().rev() {
    if next_line > comment.end_line_fast(program) + 1 {
      return i + 1;
    }
    next_line = comment.start_line_fast(program);
  }
  0
}

pub(super) fn gen_comments_between_lines_indented<'a>(start_between_pos: SourcePos, context: &mut Context<'a>) -> PrintItems<'a> {
  let trailing_comments = get_comments_between_lines(start_between_pos, context);
  let mut items = PrintItems::new(context.arena);

  if !trailing_comments.is_empty() {
    items.extend(with_indent({
      let mut items = PrintItems::new(context.arena);
      if let Some(first_comment) = trailing_comments.first()
        && first_comment.kind == CommentKind::Block
      {
        items.push_signal(Signal::SpaceIfNotTrailing);
      }
      items.extend(gen_comment_collection(
        trailing_comments.into_iter(),
        Some(&start_between_pos.range()),
        None,
        context,
      ));
      items
    }));
    items.push_signal(Signal::NewLine);
  }

  return items;

  fn get_comments_between_lines<'a>(previous_token_end: SourcePos, context: &mut Context<'a>) -> Vec<&'a Comment<'a>> {
    let mut comments = Vec::new();
    let trailing_comments = previous_token_end.trailing_comments_fast(context.program);
    if !trailing_comments.is_empty() {
      let next_token_pos = context.token_finder.get_next_token_pos_after(&previous_token_end);
      let next_token_start_line = next_token_pos.start_line_fast(context.program);

      for comment in trailing_comments {
        if !context.has_handled_comment(comment) && comment.start_line_fast(context.program) < next_token_start_line {
          comments.push(comment);
        }
      }
    }
    comments
  }
}

pub(super) fn gen_comment_collection<'a>(
  comments: impl Iterator<Item = &'a Comment<'a>>,
  last_node: Option<&SourceRange>,
  next_node: Option<&SourceRange>,
  context: &mut Context<'a>,
) -> PrintItems<'a> {
  let mut last_node = last_node.map(|l| l.range());
  let mut items = PrintItems::new(context.arena);
  let next_node_start_line = next_node.map(|n| n.start_line_fast(context.program));
  for comment in comments {
    if !context.has_handled_comment(comment) {
      items.extend(gen_comment_based_on_last_node(
        comment,
        &last_node,
        GenCommentBasedOnLastNodeOptions {
          separate_with_newlines: if let Some(next_node_start_line) = next_node_start_line {
            comment.start_line_fast(context.program) != next_node_start_line
          } else {
            false
          },
        },
        context,
      ));
      last_node = Some(comment.range());
    }
  }
  items
}

pub(super) fn gen_leading_comments_same_line<'a>(node: &SourceRange, context: &mut Context<'a>) -> PrintItems<'a> {
  let comments = get_leading_comments_on_same_line(node, context);
  if comments.is_empty() {
    PrintItems::new(context.arena)
  } else {
    let mut items = gen_comments_same_line(comments, context);
    if !items.is_empty() {
      items.push_space();
    }
    items
  }
}

pub(super) fn gen_trailing_comments_same_line<'a>(node: &SourceRange, context: &mut Context<'a>) -> PrintItems<'a> {
  let comments = get_trailing_comments_on_same_line(node, context);
  if comments.is_empty() {
    PrintItems::new(context.arena)
  } else {
    let mut items = PrintItems::new(context.arena);
    let comment_items = gen_comments_same_line(comments, context);
    if !comment_items.is_empty() {
      items.push_space();
      items.extend(comment_items);
    }
    items
  }
}

pub(super) fn gen_trailing_comments_if_line_comment_same_line<'a>(node: &SourceRange, context: &mut Context<'a>) -> PrintItems<'a> {
  let comments = get_trailing_comments_on_same_line(node, context);
  if !comments.iter().any(|c| c.kind == CommentKind::Line) {
    PrintItems::new(context.arena)
  } else {
    let mut items = PrintItems::new(context.arena);
    let comment_items = gen_comments_same_line(comments, context);
    if !comment_items.is_empty() {
      items.push_space();
      items.extend(comment_items);
    }
    items
  }
}

pub(super) fn gen_comments_same_line<'a>(comments: Vec<&'a Comment<'a>>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  let mut had_comment_last = false;
  for comment in comments {
    if had_comment_last {
      items.push_space();
    }
    if let Some(comment) = gen_comment(comment, context) {
      items.extend(comment);
      had_comment_last = true;
    } else {
      had_comment_last = false;
    }
  }
  items
}

pub(super) struct GenCommentBasedOnLastNodeOptions {
  pub(super) separate_with_newlines: bool,
}

pub(super) fn gen_comment_based_on_last_node<'a>(
  comment: &'a Comment<'a>,
  last_node: &Option<SourceRange>,
  opts: GenCommentBasedOnLastNodeOptions,
  context: &mut Context<'a>,
) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  let mut pushed_ignore_new_lines = false;

  if let Some(last_node) = last_node {
    let comment_start_line = comment.start_line_fast(context.program);
    let last_node_end_line = last_node.end_line_fast(context.program);

    if opts.separate_with_newlines || comment_start_line > last_node_end_line {
      items.push_signal(Signal::NewLine);
      push_blank_lines(
        &mut items,
        cap_blank_line_count(comment_start_line.saturating_sub(last_node_end_line + 1), context),
      );
    } else if comment.kind == CommentKind::Line {
      items.push_signal(Signal::StartForceNoNewLines);
      items.push_space();
      pushed_ignore_new_lines = true;
    } else if last_node.text_fast(context.program).starts_with("/*") {
      items.push_space();
    }
  }

  if let Some(generated_comment) = gen_comment(comment, context) {
    items.extend(generated_comment);
  }

  if pushed_ignore_new_lines {
    items.push_signal(Signal::FinishForceNoNewLines);
  }

  items
}

pub(super) fn gen_comment<'a>(comment: &'a Comment<'a>, context: &mut Context<'a>) -> Option<PrintItems<'a>> {
  // only generate if handled
  if context.has_handled_comment(comment) {
    return None;
  }

  // mark handled and generate
  context.mark_comment_handled(comment);

  return Some(match comment.kind {
    CommentKind::Block => {
      if has_leading_astrisk_each_line(comment.text) {
        gen_js_doc_or_multiline_block(comment, context)
      } else {
        // Single-line comment block
        ir_helpers::gen_js_like_comment_block(context.arena, comment.text)
      }
    }
    CommentKind::Line => ir_helpers::gen_js_like_comment_line(context.arena, comment.text, context.config.comment_line_force_space_after_slashes),
  });

  fn has_leading_astrisk_each_line(text: &str) -> bool {
    if !text.contains('\n') {
      return false;
    }

    for line in text.trim().split('\n') {
      let first_non_whitespace = line.trim_start().chars().next();
      if !matches!(first_non_whitespace, Some('*')) {
        return false;
      }
    }

    true
  }
}

pub(super) fn gen_js_doc_or_multiline_block<'a>(comment: &'a Comment<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  debug_assert_eq!(comment.kind, CommentKind::Block);
  let is_js_doc = comment.text.starts_with('*');
  return lines_to_print_items(context.arena, is_js_doc, build_lines(comment, is_js_doc));

  fn build_lines<'a>(comment: &Comment<'a>, is_js_doc: bool) -> Vec<&'a str> {
    let mut lines: Vec<&'a str> = Vec::new();
    let mut markdown_state = JsDocMarkdownState::default();
    let mut comment_lines = utils::split_lines(comment.text)
      .map(|line| {
        let text = if line.line_index == 0 && !line.text.starts_with('*') {
          line.text
        } else {
          &line.text[get_line_start_index(line.text)..]
        };
        (text, line.is_last)
      })
      .peekable();

    while let Some((text, is_last)) = comment_lines.next() {
      let is_markdown_text = is_js_doc && markdown_state.is_text_line(text);
      let text = if is_last && !text.trim().is_empty() {
        text
      } else if is_markdown_text && comment_lines.peek().is_some_and(|(next_text, _)| can_follow_hard_line_break(next_text)) {
        trim_end_keeping_hard_line_break(text)
      } else {
        text.trim_end()
      };

      if !text.is_empty() || !lines.last().map(|l| l.is_empty()).unwrap_or(false) {
        lines.push(text);
      }
    }

    lines
  }

  fn get_line_start_index(text: &str) -> usize {
    for (byte_index, c) in text.char_indices() {
      if c == '*' {
        return byte_index + 1;
      } else if !c.is_whitespace() {
        return byte_index;
      }
    }

    0
  }

  /// Trims the end of the line, but keeps two spaces when it ends with a
  /// markdown hard line break, which is two or more spaces.
  fn trim_end_keeping_hard_line_break(text: &str) -> &str {
    let trimmed = text.trim_end();
    let trailing_whitespace = &text[trimmed.len()..];
    if !trimmed.is_empty() && trailing_whitespace.len() >= 2 && trailing_whitespace.bytes().all(|b| b == b' ') {
      &text[..trimmed.len() + 2]
    } else {
      trimmed
    }
  }

  /// A hard line break only does something when text follows it, and
  /// a block tag ends the text before it.
  fn can_follow_hard_line_break(next_text: &str) -> bool {
    let next_text = next_text.trim();
    !next_text.is_empty() && !next_text.starts_with('@')
  }

  /// Tracks which JSDoc lines are markdown text rather than code.
  #[derive(Default)]
  struct JsDocMarkdownState {
    fence: Option<MarkdownFence>,
    is_in_example: bool,
  }

  impl JsDocMarkdownState {
    /// Updates the state with the next line and gets if it's markdown text.
    fn is_text_line(&mut self, text: &str) -> bool {
      let text = text.trim_start();
      if let Some(fence) = &self.fence {
        if fence.is_closed_by(text) {
          self.fence = None;
        }
        return false;
      }
      if let Some(fence) = MarkdownFence::from_line(text) {
        self.fence = Some(fence);
        return false;
      }
      if text.starts_with('@') {
        // the code of an example without a fence goes until the next block tag
        self.is_in_example = text
          .strip_prefix("@example")
          .is_some_and(|rest| !rest.starts_with(|c: char| c.is_alphanumeric()));
      }
      !self.is_in_example
    }
  }

  struct MarkdownFence {
    marker: char,
    len: usize,
  }

  impl MarkdownFence {
    fn from_line(text: &str) -> Option<MarkdownFence> {
      let marker = text.chars().next().filter(|c| matches!(c, '`' | '~'))?;
      let len = text.chars().take_while(|c| *c == marker).count();
      (len >= 3).then_some(MarkdownFence { marker, len })
    }

    /// A closing fence uses the same marker, is at least as long as
    /// the opening fence, and has nothing after it.
    fn is_closed_by(&self, text: &str) -> bool {
      MarkdownFence::from_line(text).is_some_and(|fence| fence.marker == self.marker && fence.len >= self.len && text[fence.len..].trim().is_empty())
    }
  }

  fn lines_to_print_items<'a>(arena: &'a Arena, is_js_doc: bool, lines: Vec<&'a str>) -> PrintItems<'a> {
    let mut items = PrintItems::new(arena);

    items.push_sc(sc!("/*"));

    for (i, line) in lines.iter().enumerate() {
      if i > 0 {
        items.push_signal(Signal::NewLine);
      }
      // leading asterisk on the first line for jsdoc only, then the line start space
      let is_space_or_asterisk = matches!(line.chars().next(), Some('*' | ' '));
      let has_line_start_space = !line.is_empty() && !is_space_or_asterisk;
      items.push_sc(match (is_js_doc && i == 0, i > 0, has_line_start_space) {
        (true, _, false) => sc!("*"),
        (true, _, true) => sc!("* "),
        (false, true, false) => sc!(" *"),
        (false, true, true) => sc!(" * "),
        (false, false, false) => sc!(""),
        (false, false, true) => sc!(" "),
      });

      // line text
      if !line.is_empty() {
        items.extend(gen_from_raw_string(arena, line));
      }
    }

    items.push_sc(if lines.len() > 1 && lines.last().map(|l| l.is_empty()).unwrap_or(false) {
      sc!("/")
    } else if lines.len() > 1 && lines.last().map(|l| l.ends_with('*') || l.ends_with(' ')).unwrap_or(false) {
      sc!("*/")
    } else {
      sc!(" */")
    });

    items
  }
}

pub(super) fn gen_trailing_comments<'a>(node: &SourceRange, context: &mut Context<'a>) -> PrintItems<'a> {
  let trailing_comments = node.trailing_comments_fast(context.program);
  gen_comments_as_trailing(node, trailing_comments, context)
}

pub(super) fn gen_comments_as_trailing<'a>(node: &SourceRange, trailing_comments: CommentsIterator<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);

  // don't do extra work
  if trailing_comments.is_empty() {
    return items;
  }

  let comments_before_comma = get_trailing_comments_before_comma(node, trailing_comments.clone(), context);
  let trailing_comments_on_same_line = get_trailing_comments_same_line(node, trailing_comments, context);

  if !trailing_comments_on_same_line.is_empty() {
    let first_unhandled_comment = trailing_comments_on_same_line.iter().find(|c| !context.has_handled_comment(c));

    if let Some(first_unhandled_comment) = first_unhandled_comment
      && first_unhandled_comment.kind == CommentKind::Block
    {
      items.push_space();
    }

    items.extend(gen_comment_collection(trailing_comments_on_same_line.into_iter(), Some(node), None, context));
  }

  // nothing else generates these because the comma they're before is printed beside the node
  if let Some((comma_token, comments)) = comments_before_comma {
    items.extend(gen_comments_after_comma(node, comments, comma_token, context));
  }

  items
}

pub(super) fn get_trailing_comments_same_line<'a>(
  node: &SourceRange,
  trailing_comments: CommentsIterator<'a>,
  context: &mut Context<'a>,
) -> Vec<&'a Comment<'a>> {
  // use the roslyn definition of trailing comments
  let node_end_line = node.end_line_fast(context.program);
  let trailing_comments_on_same_line = trailing_comments
    .into_iter()
    .filter(|c| c.start_line_fast(context.program) <= node_end_line) // less than or equal instead of just equal in order to include "forgotten" comments
    .collect::<Vec<_>>();

  // don't do extra work
  if trailing_comments_on_same_line.is_empty() {
    return trailing_comments_on_same_line;
  }

  // block comments after a comma on the same line as the next token are not considered a trailing comment of this node
  // ex. `a, /* 1 */ b`, the comment belongs to `b` and not `a`
  let comma_end = if node.text_fast(context.program) == "," {
    Some(node.end())
  } else {
    context.token_finder.get_next_token_if_comma(&node.range()).map(|t| t.end())
  };
  if let Some(comma_end) = comma_end {
    let next_token_pos = context.token_finder.get_next_token_pos_after(&comma_end);
    let next_token_pos_line_start = next_token_pos.start_line_fast(context.program);
    if next_token_pos_line_start == node_end_line {
      return trailing_comments_on_same_line.into_iter().filter(|c| c.start() < comma_end).collect::<Vec<_>>();
    }
  }
  trailing_comments_on_same_line
}

/// Gets the trailing comments on the lines after the node that are before the comma
/// following it (ex. `// c` in `a\n// c\n, b`) along with the comma.
pub(super) fn get_trailing_comments_before_comma<'a>(
  node: &SourceRange,
  trailing_comments: CommentsIterator<'a>,
  context: &mut Context<'a>,
) -> Option<(&'a TokenAndSpan, Vec<&'a Comment<'a>>)> {
  let program = context.program;
  let node_end_line = node.end_line_fast(program);
  let mut comments = trailing_comments.filter(|c| c.start_line_fast(program) > node_end_line).peekable();
  // only look for the comma when there are comments
  comments.peek()?;
  // a comma after a comma is for an array hole, which is generated with its comments
  if node.text_fast(program) == "," {
    return None;
  }
  let comma_token = context.token_finder.get_next_token_if_comma(&node.range())?;
  let comments = comments.filter(|c| c.start() < comma_token.start()).collect::<Vec<_>>();
  (!comments.is_empty()).then_some((comma_token, comments))
}

/// Gets the comments between a token and the token before it.
pub(super) fn get_comments_before_token<'a>(token: &TokenAndSpan, context: &Context<'a>) -> Vec<&'a Comment<'a>> {
  let program = context.program;
  match token.previous_token_fast(program) {
    Some(previous_token) => previous_token
      .end()
      .trailing_comments_fast(program)
      .chain(token.start().leading_comments_fast(program))
      .collect(),
    None => Vec::new(),
  }
}

/// Gets if there should be a space after a keyword that's before an open paren, which there
/// always is when comments are before the paren so they're not beside the keyword (ex. `for /* c */ (`).
pub(super) fn use_space_after_keyword<'a>(is_configured: bool, open_paren_token: Option<&TokenAndSpan>, context: &Context<'a>) -> bool {
  is_configured || open_paren_token.is_some_and(|token| !get_comments_before_token(token, context).is_empty())
}

/// Generates comments that were before or beside a comma for when the comma is printed
/// before them, where the ones on a line after what's before them go on their own line.
/// Gets if a comment between the node and the comma after it is on its own line,
/// which is generated after the comma (ex. `// c` in `a\n// c\n, b`).
pub(super) fn has_comment_on_later_line_before_comma(node: Node, program: ProgramInfo) -> bool {
  // the comma is within some nodes (ex. type literal members)
  let (comma_token, mut previous_end_line) = match node.tokens_fast(program) {
    [.., previous_token, last_token] if last_token.kind == TokenKind::Comma => (last_token, previous_token.end_line_fast(program)),
    _ => match node.next_token_fast(program) {
      Some(token) if token.kind == TokenKind::Comma => (token, node.end_line_fast(program)),
      _ => return false,
    },
  };
  let next_token_start_line = comma_token.next_token_fast(program).map(|token| token.start_line_fast(program));
  for comment in comma_token.start().leading_comments_fast(program) {
    let is_on_later_line = comment.start_line_fast(program) > previous_end_line;
    let is_beside_next_token = Some(comment.end_line_fast(program)) == next_token_start_line;
    if is_on_later_line && !(comment.kind == CommentKind::Block && is_beside_next_token) {
      return true;
    }
    previous_end_line = comment.end_line_fast(program);
  }
  false
}

pub(super) fn gen_comments_after_comma<'a>(
  previous: &SourceRange,
  comments: Vec<&'a Comment<'a>>,
  comma_token: &TokenAndSpan,
  context: &mut Context<'a>,
) -> PrintItems<'a> {
  let program = context.program;
  let next_token_start_line = comma_token.next_token_fast(program).map(|token| token.start_line_fast(program));
  let mut items = PrintItems::new(context.arena);
  let mut previous = previous.range();
  let mut has_comment_on_later_line = false;
  for comment in comments {
    if context.has_handled_comment(comment) {
      continue;
    }
    let is_on_later_line = comment.start_line_fast(program) > previous.end_line_fast(program);
    // a block comment beside what follows the comma stays beside it like a leading comment (ex. `a\n/* c */, b`)
    let is_beside_next_token = Some(comment.end_line_fast(program)) == next_token_start_line;
    if comment.kind == CommentKind::Block && (!is_on_later_line || is_beside_next_token) {
      if let Some(generated_comment) = gen_comment(comment, context) {
        items.push_space();
        items.extend(generated_comment);
      }
    } else if is_on_later_line {
      items.push_signal(Signal::NewLine);
      if context.force_blank_line_in_list && !has_comment_on_later_line {
        // the forced blank line to the next value goes before the comments, since they're
        // with the next value when formatted again
        items.push_signal(Signal::NewLine);
      } else if context.allow_blank_lines_in_list {
        // only where the list keeps blank lines, as formatting again would otherwise remove it
        let blank_line_count = comment.start_line_fast(program).saturating_sub(previous.end_line_fast(program) + 1);
        push_blank_lines(&mut items, cap_blank_line_count(blank_line_count, context));
      }
      has_comment_on_later_line = true;
      items.extend(gen_comment(comment, context).unwrap_or_else(|| PrintItems::new(context.arena)));
    } else {
      let opts = GenCommentBasedOnLastNodeOptions { separate_with_newlines: false };
      items.extend(gen_comment_based_on_last_node(comment, &Some(previous), opts, context));
    }
    previous = comment.range();
  }
  items
}

pub(super) fn get_jsx_empty_expr_comments<'a>(node: &JSXEmptyExpression, context: &Context<'a>) -> CommentsIterator<'a> {
  node.end().leading_comments_fast(context.program)
}

/// Gets if the empty expression only has block comments that follow each other on
/// the same line (ex. `/* a */ /*` then `b */` on the next line).
pub(super) fn has_only_same_line_block_comments(node: &JSXEmptyExpression, context: &Context) -> bool {
  let mut previous_end_line = None;
  for comment in get_jsx_empty_expr_comments(node, context) {
    if comment.kind != CommentKind::Block || previous_end_line.is_some_and(|line| line != comment.start_line_fast(context.program)) {
      return false;
    }
    previous_end_line = Some(comment.end_line_fast(context.program));
  }
  true
}
