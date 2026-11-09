use crate::ast::*;
use crate::generation::context::Context;

pub struct CurriedArrowExpr<'a> {
  pub signatures: Vec<ArrowSignature<'a>>,
}

impl<'a> CurriedArrowExpr<'a> {
  pub fn last_arrow(&self) -> &'a ArrowFunctionExpression<'a> {
    self.signatures.last().unwrap().inner
  }
}

// Signature including arrow
// ex. `(a: string) =>`
pub struct ArrowSignature<'a> {
  pub inner: &'a ArrowFunctionExpression<'a>,
}

impl<'a> ArrowSignature<'a> {
  pub fn new(inner: &'a ArrowFunctionExpression<'a>) -> Self {
    Self { inner }
  }

  pub fn is_async(&self) -> bool {
    self.inner.r#async
  }

  pub fn params(&self) -> &'a FormalParameters<'a> {
    &self.inner.params
  }

  pub fn type_params(&self) -> Option<&'a TSTypeParameterDeclaration<'a>> {
    self.inner.type_parameters.as_deref()
  }

  pub fn return_type(&self) -> Option<&'a TSTypeAnnotation<'a>> {
    self.inner.return_type.as_deref()
  }

  pub fn range(&self, context: &Context<'a>) -> SourceRange {
    let start = self.inner.start();
    let last_token = self.inner.body.previous_token_fast(context.program).unwrap();
    debug_assert_eq!(last_token.kind, TokenKind::Arrow);
    SourceRange::new(start, last_token.end())
  }
}

pub fn get_curried_arrow_expr<'a>(mut arrow_func: &'a ArrowFunctionExpression<'a>) -> Option<CurriedArrowExpr<'a>> {
  let mut signatures = Vec::new();

  while let ArrowFunctionBody::ArrowFunctionExpression(inner) = &arrow_func.body {
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
