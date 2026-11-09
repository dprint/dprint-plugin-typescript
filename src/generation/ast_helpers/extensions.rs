use crate::ast::*;

/// A binary or logical operator, which swc represented together and the
/// formatter treats the same.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
  Binary(BinaryOperator),
  Logical(LogicalOperator),
}

impl BinaryOp {
  pub fn as_str(&self) -> &'static str {
    match self {
      BinaryOp::Binary(op) => op.as_str(),
      BinaryOp::Logical(op) => op.as_str(),
    }
  }

  pub fn is_add_sub(&self) -> bool {
    matches!(self, BinaryOp::Binary(BinaryOperator::Addition | BinaryOperator::Subtraction))
  }

  pub fn is_mul_div(&self) -> bool {
    matches!(self, BinaryOp::Binary(BinaryOperator::Multiplication | BinaryOperator::Division))
  }

  pub fn is_bitwise_or_arithmetic(&self) -> bool {
    matches!(
      self,
      BinaryOp::Binary(
        BinaryOperator::ShiftLeft
          | BinaryOperator::ShiftRight
          | BinaryOperator::ShiftRightZeroFill
          | BinaryOperator::Addition
          | BinaryOperator::Subtraction
          | BinaryOperator::Multiplication
          | BinaryOperator::Division
          | BinaryOperator::Remainder
          | BinaryOperator::BitwiseOR
          | BinaryOperator::BitwiseXOR
          | BinaryOperator::BitwiseAnd
      )
    )
  }

  /// `&&` or `||`
  pub fn is_logical(&self) -> bool {
    matches!(self, BinaryOp::Logical(LogicalOperator::And | LogicalOperator::Or))
  }

  pub fn is_bit_logical(&self) -> bool {
    matches!(
      self,
      BinaryOp::Binary(BinaryOperator::BitwiseOR | BinaryOperator::BitwiseAnd | BinaryOperator::BitwiseXOR)
    )
  }

  pub fn is_bit_shift(&self) -> bool {
    matches!(
      self,
      BinaryOp::Binary(BinaryOperator::ShiftLeft | BinaryOperator::ShiftRight | BinaryOperator::ShiftRightZeroFill)
    )
  }

  pub fn is_equality(&self) -> bool {
    matches!(
      self,
      BinaryOp::Binary(
        BinaryOperator::Equality
          | BinaryOperator::Inequality
          | BinaryOperator::StrictEquality
          | BinaryOperator::StrictInequality
          | BinaryOperator::LessThan
          | BinaryOperator::LessEqualThan
          | BinaryOperator::GreaterThan
          | BinaryOperator::GreaterEqualThan
      )
    )
  }
}

/// A binary, logical, or private in expression (ex. `#x in obj`), which swc
/// represented together and the formatter treats the same.
#[derive(Clone, Copy)]
pub enum BinExpr<'a> {
  Binary(&'a BinaryExpression<'a>),
  Logical(&'a LogicalExpression<'a>),
  PrivateIn(&'a PrivateInExpression<'a>),
}

impl<'a> BinExpr<'a> {
  pub fn from_node(node: Node<'a>) -> Option<Self> {
    match node {
      Node::BinaryExpression(expr) => Some(BinExpr::Binary(expr)),
      Node::LogicalExpression(expr) => Some(BinExpr::Logical(expr)),
      Node::PrivateInExpression(expr) => Some(BinExpr::PrivateIn(expr)),
      _ => None,
    }
  }

  pub fn left(&self) -> Node<'a> {
    match self {
      BinExpr::Binary(expr) => (&expr.left).into_node(),
      BinExpr::Logical(expr) => (&expr.left).into_node(),
      BinExpr::PrivateIn(expr) => (&expr.left).into_node(),
    }
  }

  pub fn right(&self) -> Node<'a> {
    match self {
      BinExpr::Binary(expr) => (&expr.right).into_node(),
      BinExpr::Logical(expr) => (&expr.right).into_node(),
      BinExpr::PrivateIn(expr) => (&expr.right).into_node(),
    }
  }

  pub fn op(&self) -> BinaryOp {
    match self {
      BinExpr::Binary(expr) => BinaryOp::Binary(expr.operator),
      BinExpr::Logical(expr) => BinaryOp::Logical(expr.operator),
      BinExpr::PrivateIn(_) => BinaryOp::Binary(BinaryOperator::In),
    }
  }

  pub fn node(&self) -> Node<'a> {
    match self {
      BinExpr::Binary(expr) => expr.into_node(),
      BinExpr::Logical(expr) => expr.into_node(),
      BinExpr::PrivateIn(expr) => expr.into_node(),
    }
  }

  pub fn parent(&self, program: ProgramInfo<'a>) -> Node<'a> {
    self.node().parent(program).unwrap()
  }
}

impl SourceRanged for BinExpr<'_> {
  fn start(&self) -> SourcePos {
    self.node().start()
  }

  fn end(&self) -> SourcePos {
    self.node().end()
  }
}

/// Gets if the node is a binary, logical, or private in expression.
pub fn is_bin_expr_node(node: Node) -> bool {
  matches!(node, Node::BinaryExpression(_) | Node::LogicalExpression(_) | Node::PrivateInExpression(_))
}
