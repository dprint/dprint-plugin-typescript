use rustc_hash::FxHashMap;

use super::SourcePos;
use super::SourceRanged;
use super::TokenAndSpan;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CommentKind {
  /// `// comment`
  Line,
  /// `/* comment */`
  Block,
}

/// A comment in the source text.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Comment<'a> {
  pub kind: CommentKind,
  pub start: SourcePos,
  pub end: SourcePos,
  /// Text of the comment without the comment delimiters.
  pub text: &'a str,
}

impl SourceRanged for Comment<'_> {
  fn start(&self) -> SourcePos {
    self.start
  }

  fn end(&self) -> SourcePos {
    self.end
  }
}

/// The comments of a program, with each one attached either as a leading
/// comment of the token after it or a trailing comment of the token before it.
pub struct CommentContainer<'a> {
  comments: Vec<Comment<'a>>,
  /// Token start to the range of indexes in `comments`.
  leading: FxHashMap<SourcePos, (u32, u32)>,
  /// Token end to the range of indexes in `comments`.
  trailing: FxHashMap<SourcePos, (u32, u32)>,
}

impl<'a> CommentContainer<'a> {
  pub fn new(text: &'a str, oxc_comments: &[oxc_ast::ast::Comment], tokens: &[TokenAndSpan]) -> Self {
    let comments = oxc_comments
      .iter()
      .map(|c| {
        let (kind, text) = match c.kind {
          oxc_ast::ast::CommentKind::Line => (CommentKind::Line, &text[c.span.start as usize + 2..c.span.end as usize]),
          // html-like comments (ex. `<!-- comment`), which swc gave the text after the delimiter
          oxc_ast::ast::CommentKind::HtmlOpen => (CommentKind::Line, &text[c.span.start as usize + "<!--".len()..c.span.end as usize]),
          oxc_ast::ast::CommentKind::HtmlClose => (CommentKind::Line, &text[c.span.start as usize + "-->".len()..c.span.end as usize]),
          oxc_ast::ast::CommentKind::SingleLineBlock | oxc_ast::ast::CommentKind::MultiLineBlock => {
            (CommentKind::Block, &text[c.span.start as usize + 2..c.span.end as usize - 2])
          }
        };
        Comment {
          kind,
          start: SourcePos::new(c.span.start),
          end: SourcePos::new(c.span.end),
          text,
        }
      })
      .collect::<Vec<_>>();
    let (leading, trailing) = attach_comments(&comments, tokens);
    Self { comments, leading, trailing }
  }

  pub fn leading_comments(&self, pos: SourcePos) -> CommentsIterator<'_> {
    match self.leading.get(&pos) {
      Some(&(start, end)) => CommentsIterator::new(&self.comments[start as usize..end as usize]),
      None => CommentsIterator::empty(),
    }
  }

  pub fn trailing_comments(&self, pos: SourcePos) -> CommentsIterator<'_> {
    match self.trailing.get(&pos) {
      Some(&(start, end)) => CommentsIterator::new(&self.comments[start as usize..end as usize]),
      None => CommentsIterator::empty(),
    }
  }
}

type CommentMap = FxHashMap<SourcePos, (u32, u32)>;

/// Gets the comments between each pair of tokens, which are the leading comments
/// at the start of the token after them and the trailing comments at the end of
/// the token before them.
///
/// The comments before the first token are leading comments of it (or of the
/// start of the file when there are no tokens) and the comments after the last
/// token are trailing comments of it.
fn attach_comments(comments: &[Comment], tokens: &[TokenAndSpan]) -> (CommentMap, CommentMap) {
  let mut leading = CommentMap::default();
  let mut trailing = CommentMap::default();
  let mut comment_index = 0;
  let mut token_index = 0;
  while comment_index < comments.len() {
    let comment_start = comments[comment_index].start;
    // find the token after the comment
    while token_index < tokens.len() && tokens[token_index].start < comment_start {
      token_index += 1;
    }
    let next = tokens.get(token_index);
    // a comment within a token (ex. in jsx text) isn't between tokens
    if token_index > 0 && tokens[token_index - 1].end > comment_start {
      comment_index += 1;
      continue;
    }
    let range_start = comment_index as u32;
    while comment_index < comments.len() && next.is_none_or(|next| comments[comment_index].start < next.start) {
      comment_index += 1;
    }
    let range = (range_start, comment_index as u32);
    match next {
      Some(next) => {
        leading.insert(next.start, range);
      }
      None if tokens.is_empty() => {
        leading.insert(SourcePos::START, range);
      }
      None => {}
    }
    if token_index > 0 {
      trailing.insert(tokens[token_index - 1].end, range);
    }
  }
  (leading, trailing)
}

/// An iterator over slices of comments.
#[derive(Clone)]
pub struct CommentsIterator<'a> {
  slices: smallvec_lite::SmallSlices<'a>,
  outer_index: usize,
  inner_index: usize,
}

impl<'a> CommentsIterator<'a> {
  pub fn empty() -> Self {
    Self {
      slices: Default::default(),
      outer_index: 0,
      inner_index: 0,
    }
  }

  pub fn new(comments: &'a [Comment<'a>]) -> Self {
    let mut iterator = Self::empty();
    iterator.push_slice(comments);
    iterator
  }

  pub fn extend(&mut self, iterator: CommentsIterator<'a>) {
    for slice in iterator.slices.iter() {
      self.slices.push(slice);
    }
  }

  pub fn is_empty(&self) -> bool {
    self.slices.is_empty()
  }

  pub fn peek_last_comment(&self) -> Option<&'a Comment<'a>> {
    self.slices.iter().last().and_then(|c| c.last())
  }

  fn push_slice(&mut self, comments: &'a [Comment<'a>]) {
    if !comments.is_empty() {
      self.slices.push(comments);
    }
  }
}

impl<'a> Iterator for CommentsIterator<'a> {
  type Item = &'a Comment<'a>;

  fn next(&mut self) -> Option<Self::Item> {
    loop {
      let slice = self.slices.get(self.outer_index)?;
      if let Some(comment) = slice.get(self.inner_index) {
        self.inner_index += 1;
        return Some(comment);
      }
      self.outer_index += 1;
      self.inner_index = 0;
    }
  }

  fn size_hint(&self) -> (usize, Option<usize>) {
    let mut count = 0;
    for (i, slice) in self.slices.iter().enumerate().skip(self.outer_index) {
      count += if i == self.outer_index {
        slice.len() - self.inner_index.min(slice.len())
      } else {
        slice.len()
      };
    }
    (count, Some(count))
  }
}

impl DoubleEndedIterator for CommentsIterator<'_> {
  fn next_back(&mut self) -> Option<Self::Item> {
    // only used rarely, so collect what's remaining and pop from the end
    let mut remaining = self.clone().collect::<Vec<_>>();
    let last = remaining.pop()?;
    // rebuild this iterator without the last comment
    let mut new_iterator = CommentsIterator::empty();
    let all = remaining;
    *self = CommentsIterator::empty();
    for comment in all {
      new_iterator.push_slice(std::slice::from_ref(comment));
    }
    *self = new_iterator;
    Some(last)
  }
}

mod smallvec_lite {
  use super::Comment;

  const INLINE_LEN: usize = 4;

  /// A small vector of comment slices that avoids allocating in the common case.
  #[derive(Clone)]
  pub enum SmallSlices<'a> {
    Inline { slices: [&'a [Comment<'a>]; INLINE_LEN], len: usize },
    Heap(Vec<&'a [Comment<'a>]>),
  }

  impl Default for SmallSlices<'_> {
    fn default() -> Self {
      SmallSlices::Inline {
        slices: [&[]; INLINE_LEN],
        len: 0,
      }
    }
  }

  impl<'a> SmallSlices<'a> {
    pub fn push(&mut self, slice: &'a [Comment<'a>]) {
      match self {
        SmallSlices::Inline { slices, len } => {
          if *len < INLINE_LEN {
            slices[*len] = slice;
            *len += 1;
          } else {
            let mut vec = slices.to_vec();
            vec.push(slice);
            *self = SmallSlices::Heap(vec);
          }
        }
        SmallSlices::Heap(vec) => vec.push(slice),
      }
    }

    pub fn as_slice(&self) -> &[&'a [Comment<'a>]] {
      match self {
        SmallSlices::Inline { slices, len } => &slices[..*len],
        SmallSlices::Heap(vec) => vec,
      }
    }

    pub fn iter(&self) -> impl Iterator<Item = &'a [Comment<'a>]> + '_ {
      self.as_slice().iter().copied()
    }

    pub fn get(&self, index: usize) -> Option<&'a [Comment<'a>]> {
      self.as_slice().get(index).copied()
    }

    pub fn is_empty(&self) -> bool {
      self.as_slice().is_empty()
    }
  }
}
