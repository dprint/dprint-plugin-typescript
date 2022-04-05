use deno_ast::view::*;

pub trait BinaryOpExtensions {
  fn is_add_sub(&self) -> bool;
  fn is_mul_div(&self) -> bool;
  fn is_bitwise_or_arithmetic(&self) -> bool;
  fn is_logical(&self) -> bool;
  fn is_bit_logical(&self) -> bool;
  fn is_bit_shift(&self) -> bool;
  fn is_equality(&self) -> bool;
}

impl BinaryOpExtensions for BinaryOp {
  fn is_add_sub(&self) -> bool {
    matches!(self, BinaryOp::Add | BinaryOp::Sub)
  }

  fn is_mul_div(&self) -> bool {
    matches!(self, BinaryOp::Mul | BinaryOp::Div)
  }

  fn is_bitwise_or_arithmetic(&self) -> bool {
    matches!(
      self,
      BinaryOp::LShift
        | BinaryOp::RShift
        | BinaryOp::ZeroFillRShift
        | BinaryOp::Add
        | BinaryOp::Sub
        | BinaryOp::Mul
        | BinaryOp::Div
        | BinaryOp::Mod
        | BinaryOp::BitOr
        | BinaryOp::BitXor
        | BinaryOp::BitAnd
    )
  }

  fn is_logical(&self) -> bool {
    matches!(self, BinaryOp::LogicalAnd | BinaryOp::LogicalOr)
  }

  fn is_bit_logical(&self) -> bool {
    matches!(self, BinaryOp::BitOr | BinaryOp::BitAnd | BinaryOp::BitXor)
  }

  fn is_bit_shift(&self) -> bool {
    matches!(self, BinaryOp::LShift | BinaryOp::RShift | BinaryOp::ZeroFillRShift)
  }

  fn is_equality(&self) -> bool {
    matches!(
      self,
      BinaryOp::EqEq | BinaryOp::NotEq | BinaryOp::EqEqEq | BinaryOp::NotEqEq | BinaryOp::Lt | BinaryOp::LtEq | BinaryOp::Gt | BinaryOp::GtEq
    )
  }
}
