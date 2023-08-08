use deno_ast::swc::common::comments::Comment;
use deno_ast::swc::parser::token::TokenAndSpan;
use deno_ast::view::*;
use deno_ast::MediaType;
use deno_ast::SourcePos;
use deno_ast::SourceRange;
use deno_ast::SourceRanged;
use deno_ast::SourceRangedForSpanned;
use dprint_core::formatting::ConditionReference;
use dprint_core::formatting::IndentLevel;
use dprint_core::formatting::IsStartOfLine;
use dprint_core::formatting::LineNumber;
use dprint_core::formatting::LineStartIndentLevel;
use rustc_hash::FxHashMap;
use rustc_hash::FxHashSet;

use super::*;
use crate::configuration::*;
use crate::utils::Stack;

pub struct Context<'a> {
  pub media_type: MediaType,
  pub program: Program<'a>,
  pub config: &'a Configuration,
  pub comments: CommentTracker<'a>,
  pub token_finder: TokenFinder<'a>,
  pub current_node: Node<'a>,
  pub parent_stack: Stack<Node<'a>>,
  /// Stores whether the parent requires all properties have consistent quoting.
  consistent_quote_props_stack: Stack<bool>,
  handled_comments: FxHashSet<SourcePos>,
  stored_ln_ranges: FxHashMap<(SourcePos, SourcePos), (LineNumber, LineNumber)>,
  stored_lsil: FxHashMap<(SourcePos, SourcePos), LineStartIndentLevel>,
  stored_ln: FxHashMap<(SourcePos, SourcePos), LineNumber>,
  stored_il: FxHashMap<(SourcePos, SourcePos), IndentLevel>,
  pub end_statement_or_member_lns: Stack<LineNumber>,
  before_comments_start_info_stack: Stack<(SourceRange, LineNumber, IsStartOfLine)>,
  if_stmt_last_brace_condition_ref: Option<ConditionReference>,
  expr_stmt_single_line_parent_brace_ref: Option<ConditionReference>,
  /// Used for ensuring nodes are parsed in order.
  #[cfg(debug_assertions)]
  pub last_generated_node_pos: SourcePos,
}

impl<'a> Context<'a> {
  pub fn new(media_type: MediaType, tokens: &'a [TokenAndSpan], current_node: Node<'a>, program: Program<'a>, config: &'a Configuration) -> Context<'a> {
    Context {
      media_type,
      program,
      config,
      comments: CommentTracker::new(program, tokens),
      token_finder: TokenFinder::new(program),
      current_node,
      parent_stack: Default::default(),
      consistent_quote_props_stack: Default::default(),
      handled_comments: FxHashSet::default(),
      stored_ln_ranges: FxHashMap::default(),
      stored_lsil: FxHashMap::default(),
      stored_ln: FxHashMap::default(),
      stored_il: FxHashMap::default(),
      end_statement_or_member_lns: Default::default(),
      before_comments_start_info_stack: Default::default(),
      if_stmt_last_brace_condition_ref: None,
      expr_stmt_single_line_parent_brace_ref: None,
      #[cfg(debug_assertions)]
      last_generated_node_pos: deno_ast::SourceTextInfoProvider::text_info(&program).range().start.into(),
    }
  }

  pub fn is_jsx(&self) -> bool {
    matches!(self.media_type, MediaType::Tsx | MediaType::Jsx | MediaType::JavaScript)
  }

  pub fn parent(&self) -> Node<'a> {
    *self.parent_stack.peek().unwrap()
  }

  pub fn has_handled_comment(&self, comment: &Comment) -> bool {
    self.handled_comments.contains(&comment.start())
  }

  pub fn mark_comment_handled(&mut self, comment: &Comment) {
    self.handled_comments.insert(comment.start());
  }

  pub fn store_info_range_for_node(&mut self, node: &impl SourceRanged, lns: (LineNumber, LineNumber)) {
    self.stored_ln_ranges.insert((node.start(), node.end()), lns);
  }

  pub fn get_ln_range_for_node(&self, node: &impl SourceRanged) -> Option<(LineNumber, LineNumber)> {
    self.stored_ln_ranges.get(&(node.start(), node.end())).map(|x| x.to_owned())
  }

  pub fn store_lsil_for_node(&mut self, node: &impl SourceRanged, lsil: LineStartIndentLevel) {
    self.stored_lsil.insert((node.start(), node.end()), lsil);
  }

  pub fn get_lsil_for_node(&self, node: &impl SourceRanged) -> Option<LineStartIndentLevel> {
    self.stored_lsil.get(&(node.start(), node.end())).map(|x| x.to_owned())
  }

  pub fn store_ln_for_node(&mut self, node: &impl SourceRanged, ln: LineNumber) {
    self.stored_ln.insert((node.start(), node.end()), ln);
  }

  pub fn get_ln_for_node(&self, node: &impl SourceRanged) -> Option<LineNumber> {
    self.stored_ln.get(&(node.start(), node.end())).map(|x| x.to_owned())
  }

  pub fn store_il_for_node(&mut self, node: &impl SourceRanged, il: IndentLevel) {
    self.stored_il.insert((node.start(), node.end()), il);
  }

  pub fn get_il_for_node(&self, node: &impl SourceRanged) -> Option<IndentLevel> {
    self.stored_il.get(&(node.start(), node.end())).map(|x| x.to_owned())
  }

  pub fn store_if_stmt_last_brace_condition_ref(&mut self, condition_reference: ConditionReference) {
    self.if_stmt_last_brace_condition_ref = Some(condition_reference);
  }

  pub fn take_if_stmt_last_brace_condition_ref(&mut self) -> Option<ConditionReference> {
    self.if_stmt_last_brace_condition_ref.take()
  }

  pub fn store_expr_stmt_single_line_parent_brace_ref(&mut self, condition_reference: ConditionReference) {
    self.expr_stmt_single_line_parent_brace_ref = Some(condition_reference);
  }

  pub fn take_expr_stmt_single_line_parent_brace_ref(&mut self) -> Option<ConditionReference> {
    self.expr_stmt_single_line_parent_brace_ref.take()
  }

  pub fn get_or_create_current_before_comments_start_info(&mut self) -> (LineNumber, IsStartOfLine) {
    let current_range = self.current_node.range();
    if let Some((range, ln, isol)) = self.before_comments_start_info_stack.peek() {
      if *range == current_range {
        return (*ln, *isol);
      }
    }

    let new_ln = LineNumber::new("beforeComments");
    let new_isol = IsStartOfLine::new("beforeComments");
    self.before_comments_start_info_stack.push((current_range, new_ln, new_isol));
    (new_ln, new_isol)
  }

  pub fn take_current_before_comments_start_info(&mut self) -> Option<(LineNumber, IsStartOfLine)> {
    let mut had_range = false;
    if let Some((range, _, _)) = self.before_comments_start_info_stack.peek() {
      if *range == self.current_node.range() {
        had_range = true;
      }
    }

    if had_range {
      let (_, ln, isol) = self.before_comments_start_info_stack.pop();
      Some((ln, isol))
    } else {
      None
    }
  }

  pub fn use_consistent_quote_props(&self) -> Option<bool> {
    self.consistent_quote_props_stack.peek().copied()
  }

  pub fn with_maybe_consistent_props<TState, TReturn>(
    &mut self,
    state: TState,
    use_consistent_quotes: impl FnOnce(&TState) -> bool,
    action: impl FnOnce(&mut Self, TState) -> TReturn,
  ) -> TReturn {
    if self.config.quote_props == QuoteProps::Consistent {
      self.consistent_quote_props_stack.push((use_consistent_quotes)(&state));
      let result = action(self, state);
      self.consistent_quote_props_stack.pop();
      result
    } else {
      action(self, state)
    }
  }

  // do any assertions for how the state of this context should be at the end of the file
  #[cfg(debug_assertions)]
  pub fn assert_end_of_file_state(&self) {
    if self.before_comments_start_info_stack.iter().next().is_some() {
      panic!("Debug panic! There were infos in the before comments start info stack.");
    }
  }

  #[cfg(debug_assertions)]
  pub fn assert_text(&self, range: SourceRange, expected_text: &str) {
    let actual_text = range.text_fast(self.program);
    if actual_text != expected_text {
      panic!("Debug Panic Expected text `{expected_text}`, but found `{actual_text}`")
    }
  }
}
