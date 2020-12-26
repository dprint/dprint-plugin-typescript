use swc_ast_view::*;

pub struct CommentTracker<'a> {
    module: &'a Module<'a>,
    tokens: &'a [TokenAndSpan],
    token_index: usize,
}

impl<'a> CommentTracker<'a> {
    pub fn new(
        module: &'a Module<'a>,
        tokens: &'a [TokenAndSpan],
    ) -> CommentTracker<'a> {
        // println!("Leading: {:?}", leading);
        // println!("Trailing: {:?}", trailing);
        CommentTracker {
            module,
            tokens,
            token_index: 0,
        }
    }

    /// Gets the leading comments and all previously unhandled comments.
    pub fn leading_comments_with_previous(&mut self, pos: BytePos) -> CommentsIterator<'a> {
        let mut iterator = CommentsIterator::new(Vec::new());

        if self.token_index == 0 {
            // get any comments stored at the beginning of the file
            // todo: investigate what's required here
            let file_start = BytePos(0);
            iterator.extend(file_start.leading_comments_fast(self.module));
            iterator.extend(file_start.trailing_comments_fast(self.module));
        } else if let Some(previous_token) = self.tokens.get(self.token_index - 1) {
            iterator.extend(previous_token.hi().trailing_comments_fast(self.module));
        }

        loop {
            if let Some(token) = self.tokens.get(self.token_index) {
                iterator.extend(token.lo().leading_comments_fast(self.module));

                let token_hi = token.hi();
                if token_hi < pos {
                    iterator.extend(token_hi.trailing_comments_fast(self.module));
                    self.token_index += 1;
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        iterator
    }

    /// Gets the trailing comments and all previously unhandled comments
    pub fn trailing_comments_with_previous(&mut self, end: BytePos) -> CommentsIterator<'a> {
        let mut iterator = CommentsIterator::new(Vec::new());

        loop {
            if let Some(token) = self.tokens.get(self.token_index) {
                iterator.extend(token.lo().leading_comments_fast(self.module));

                let is_comma = token.token == Token::Comma;
                if !is_comma && token.lo() >= end {
                    break;
                }

                let token_hi = token.hi();
                if is_comma || token_hi <= end {
                    iterator.extend(token.hi().trailing_comments_fast(self.module));
                    self.token_index += 1;
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        iterator
    }
}
