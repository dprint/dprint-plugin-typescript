use oxc_ast::AstKind;
use oxc_ast::ast::Program;
use oxc_ast_visit::Visit;
use oxc_span::SourceType;

use super::CommentContainer;
use super::Node;
use super::SourcePos;
use super::SourceRange;
use super::SourceRanged;
use super::TextLines;
use super::TokenAndSpan;
use super::TokenContainer;

/// Information about a program that's needed in order to format it, which
/// isn't stored in the ast (ex. the parent of each node).
pub struct ProgramData<'a> {
  program: &'a Program<'a>,
  text_lines: TextLines<'a>,
  tokens: TokenContainer,
  comments: CommentContainer<'a>,
  parents: Vec<Option<Node<'a>>>,
  nodes: Vec<Option<Node<'a>>>,
}

impl<'a> ProgramData<'a> {
  /// Creates the data for a program that was parsed with tokens.
  pub fn new(program: &'a Program<'a>, tokens: &[oxc_parser::Token]) -> Self {
    let text = program.source_text;
    let tokens = TokenContainer::new(tokens, program.hashbang.as_ref().map(|h| h.span));
    let comments = CommentContainer::new(text, &program.comments, tokens.tokens());
    let mut parent_collector = ParentCollector {
      stack: Vec::with_capacity(32),
      parents: Vec::new(),
      nodes: Vec::new(),
    };
    parent_collector.visit_program(program);
    Self {
      program,
      text_lines: TextLines::new(text),
      tokens,
      comments,
      parents: parent_collector.parents,
      nodes: parent_collector.nodes,
    }
  }
}

/// A cheap to copy handle to a program and its information.
#[derive(Clone, Copy)]
pub struct ProgramInfo<'a> {
  data: &'a ProgramData<'a>,
}

impl<'a> ProgramInfo<'a> {
  pub fn new(data: &'a ProgramData<'a>) -> Self {
    Self { data }
  }

  pub fn program(&self) -> &'a Program<'a> {
    self.data.program
  }

  pub fn node(&self) -> Node<'a> {
    AstKind::Program(self.data.program)
  }

  pub fn source_type(&self) -> SourceType {
    self.data.program.source_type
  }

  pub fn text(&self) -> &'a str {
    self.data.program.source_text
  }

  pub fn text_info(&self) -> &'a TextLines<'a> {
    &self.data.text_lines
  }

  pub fn tokens(&self) -> &'a [TokenAndSpan] {
    self.data.tokens.tokens()
  }

  pub fn token_container(&self) -> &'a TokenContainer {
    &self.data.tokens
  }

  pub fn comment_container(&self) -> &'a CommentContainer<'a> {
    &self.data.comments
  }

  /// Gets the parent of the provided node.
  pub fn parent(&self, node: Node<'a>) -> Option<Node<'a>> {
    self.data.parents.get(node.node_id().index()).copied().flatten()
  }

  /// Gets the children of the provided node in source order.
  ///
  /// Note that this searches every node, so it should only be used sparingly.
  pub fn children(&self, node: Node<'a>) -> Vec<Node<'a>> {
    let node_id = node.node_id();
    let mut children = self
      .data
      .parents
      .iter()
      .zip(self.data.nodes.iter())
      .filter_map(|(parent, child)| match (parent, child) {
        (Some(parent), Some(child)) if parent.node_id() == node_id => Some(*child),
        _ => None,
      })
      .collect::<Vec<_>>();
    children.sort_by_key(|child| (child.start(), std::cmp::Reverse(child.end())));
    children
  }
}

impl SourceRanged for ProgramInfo<'_> {
  fn start(&self) -> SourcePos {
    SourcePos::START
  }

  fn end(&self) -> SourcePos {
    SourcePos::from_index(self.text().len())
  }

  fn range(&self) -> SourceRange {
    SourceRange::new(self.start(), self.end())
  }
}

struct ParentCollector<'a> {
  stack: Vec<Node<'a>>,
  parents: Vec<Option<Node<'a>>>,
  nodes: Vec<Option<Node<'a>>>,
}

impl<'a> Visit<'a> for ParentCollector<'a> {
  fn enter_node(&mut self, kind: AstKind<'a>) {
    let index = kind.node_id().index();
    if self.parents.len() <= index {
      self.parents.resize(index + 1, None);
      self.nodes.resize(index + 1, None);
    }
    self.parents[index] = self.stack.last().copied();
    self.nodes[index] = Some(kind);
    self.stack.push(kind);
  }

  fn leave_node(&mut self, _kind: AstKind<'a>) {
    self.stack.pop();
  }
}
