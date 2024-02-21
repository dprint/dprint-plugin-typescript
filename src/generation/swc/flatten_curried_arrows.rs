use deno_ast::view::*;
use deno_ast::SourceRange;
use deno_ast::SourceRanged;
use deno_ast::SourceRangedForSpanned;

use crate::generation::context::Context;

pub struct CurriedArrowExpr<'a> {
  pub signatures: Vec<ArrowSignature<'a>>,
}

impl<'a> CurriedArrowExpr<'a> {
  pub fn last_arrow(&self) -> &'a ArrowExpr<'a> {
    self.signatures.last().unwrap().inner
  }
}

// Signature including arrow
// ex. `(a: string) =>`
pub struct ArrowSignature<'a> {
  pub inner: &'a ArrowExpr<'a>,
}

impl<'a> ArrowSignature<'a> {
  pub fn new(inner: &'a ArrowExpr<'a>) -> Self {
    Self { inner }
  }

  pub fn is_async(&self) -> bool {
    self.inner.is_async()
  }

  pub fn params(&self) -> &[Pat<'a>] {
    self.inner.params
  }

  pub fn type_params(&self) -> Option<&'a TsTypeParamDecl<'a>> {
    self.inner.type_params
  }

  pub fn return_type(&self) -> Option<&'a TsTypeAnn<'a>> {
    self.inner.return_type
  }

  pub fn range(&self, context: &Context<'a>) -> SourceRange {
    let start = self.inner.start();
    let last_token = self.inner.body.previous_token_fast(context.program).unwrap();
    debug_assert_eq!(last_token.token, deno_ast::swc::parser::token::Token::Arrow);
    SourceRange::new(start, last_token.end())
  }
}

pub fn get_curried_arrow_expr<'a>(mut arrow_func: &'a ArrowExpr<'a>) -> Option<CurriedArrowExpr<'a>> {
  let mut signatures = Vec::new();

  while let BlockStmtOrExpr::Expr(Expr::Arrow(inner)) = arrow_func.body {
    signatures.push(ArrowSignature::new(arrow_func));
    arrow_func = inner;
  }

  if signatures.is_empty() {
    None
  } else {
    signatures.push(ArrowSignature::new(arrow_func));
    Some(CurriedArrowExpr { signatures })
  }
}
