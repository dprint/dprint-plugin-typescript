//! A view over oxc's ast that has what's needed for formatting: the parent of
//! each node, the tokens, comments attached to tokens, and line information.

mod comments;
mod generated;
mod pos;
mod program;
mod text_info;
mod tokens;

pub use comments::Comment;
pub use comments::CommentContainer;
pub use comments::CommentKind;
pub use comments::CommentsIterator;
pub use pos::*;
pub use program::*;
pub use text_info::*;
pub use tokens::*;

pub use oxc_ast::AstKind;
pub use oxc_ast::AstType as NodeKind;
pub use oxc_ast::ast::*;
pub use oxc_parser::Kind as TokenKind;

/// A node in the ast.
pub type Node<'a> = AstKind<'a>;

/// Converts a reference to an ast struct or enum to a [`Node`].
pub trait IntoNode<'a> {
  fn into_node(self) -> Node<'a>;
}

impl<'a> IntoNode<'a> for Node<'a> {
  #[inline]
  fn into_node(self) -> Node<'a> {
    self
  }
}

impl<'a, T: 'a> IntoNode<'a> for &'a oxc_allocator::Box<'a, T>
where
  &'a T: IntoNode<'a>,
{
  #[inline]
  fn into_node(self) -> Node<'a> {
    (&**self).into_node()
  }
}

/// Extension methods for navigating the ast from a node.
pub trait NodeExt<'a> {
  fn parent(&self, program: ProgramInfo<'a>) -> Option<Node<'a>>;
  fn ancestors(&self, program: ProgramInfo<'a>) -> AncestorIterator<'a>;
  fn kind(&self) -> NodeKind;
  /// Gets if the nodes are the same node.
  fn is_same(&self, other: Node<'a>) -> bool;
}

impl<'a> NodeExt<'a> for Node<'a> {
  #[inline]
  fn parent(&self, program: ProgramInfo<'a>) -> Option<Node<'a>> {
    program.parent(*self)
  }

  fn ancestors(&self, program: ProgramInfo<'a>) -> AncestorIterator<'a> {
    AncestorIterator {
      program,
      current: program.parent(*self),
    }
  }

  #[inline]
  fn kind(&self) -> NodeKind {
    self.ty()
  }

  #[inline]
  fn is_same(&self, other: Node<'a>) -> bool {
    self.ty() == other.ty() && self.node_id() == other.node_id()
  }
}

pub struct AncestorIterator<'a> {
  program: ProgramInfo<'a>,
  current: Option<Node<'a>>,
}

impl<'a> Iterator for AncestorIterator<'a> {
  type Item = Node<'a>;

  fn next(&mut self) -> Option<Self::Item> {
    let current = self.current?;
    self.current = self.program.parent(current);
    Some(current)
  }
}
