use deno_ast::oxc::syntax::operator::BinaryOperator;

pub trait BinaryOpExtensions {
  fn is_add_sub(&self) -> bool;
  fn is_mul_div(&self) -> bool;
  fn is_bitwise_or_arithmetic(&self) -> bool;
  fn is_logical(&self) -> bool;
  fn is_bit_logical(&self) -> bool;
  fn is_bit_shift(&self) -> bool;
  fn is_equality(&self) -> bool;
}

// Note: SWC's `BinaryOp` unified logical and binary operators; oxc splits them
// into `BinaryOperator` and `LogicalOperator`. Logical operators are therefore
// handled via `LogicalExpression` nodes, and `is_logical` here is always false.
impl BinaryOpExtensions for BinaryOperator {
  fn is_add_sub(&self) -> bool {
    matches!(self, BinaryOperator::Addition | BinaryOperator::Subtraction)
  }

  fn is_mul_div(&self) -> bool {
    matches!(self, BinaryOperator::Multiplication | BinaryOperator::Division)
  }

  fn is_bitwise_or_arithmetic(&self) -> bool {
    matches!(
      self,
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
  }

  fn is_logical(&self) -> bool {
    false
  }

  fn is_bit_logical(&self) -> bool {
    matches!(self, BinaryOperator::BitwiseOR | BinaryOperator::BitwiseAnd | BinaryOperator::BitwiseXOR)
  }

  fn is_bit_shift(&self) -> bool {
    matches!(self, BinaryOperator::ShiftLeft | BinaryOperator::ShiftRight | BinaryOperator::ShiftRightZeroFill)
  }

  fn is_equality(&self) -> bool {
    matches!(
      self,
      BinaryOperator::Equality
        | BinaryOperator::Inequality
        | BinaryOperator::StrictEquality
        | BinaryOperator::StrictInequality
        | BinaryOperator::LessThan
        | BinaryOperator::LessEqualThan
        | BinaryOperator::GreaterThan
        | BinaryOperator::GreaterEqualThan
    )
  }
}
