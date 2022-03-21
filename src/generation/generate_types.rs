use deno_ast::swc::common::comments::CommentKind;
use deno_ast::swc::common::BytePos;
use deno_ast::swc::common::Span;
use deno_ast::swc::common::Spanned;
use deno_ast::view::*;

use super::*;

pub trait SpannedExtensions {
  fn start_line_with_comments(&self, context: &mut Context) -> usize;
  fn end_line_with_comments(&self, context: &mut Context) -> usize;
}

impl<T> SpannedExtensions for T
where
  T: Spanned,
{
  /// Gets the start line with possible first leading comment start.
  fn start_line_with_comments(&self, context: &mut Context) -> usize {
    // The start position with comments is the next non-whitespace position
    // after the previous token's trailing comments. The trailing comments
    // are similar to the Roslyn definition where it's any comments on the
    // same line or a single multi-line block comment that begins on the trailing line.
    let mut leading_comments = self.leading_comments_fast(context.program);
    if leading_comments.is_empty() {
      self.start_line_fast(context.program)
    } else {
      let lo = self.lo();
      let previous_token = context.token_finder.get_previous_token(&lo);
      if let Some(previous_token) = previous_token {
        let previous_end_line = previous_token.end_line_fast(context.program);
        let mut past_trailing_comments = false;
        for comment in leading_comments {
          let comment_start_line = comment.start_line_fast(context.program);
          if !past_trailing_comments && comment_start_line <= previous_end_line {
            let comment_end_line = comment.end_line_fast(context.program);
            if comment_end_line > previous_end_line {
              past_trailing_comments = true;
            }
          } else {
            return comment_start_line;
          }
        }

        self.start_line_fast(context.program)
      } else {
        leading_comments.next().unwrap().start_line_fast(context.program)
      }
    }
  }

  /// Gets the end line with possible trailing multi-line block comment end.
  fn end_line_with_comments(&self, context: &mut Context) -> usize {
    // start searching from after the trailing comma if it exists
    let search_end = context.token_finder.get_next_token_if_comma(self).map(|x| x.hi()).unwrap_or_else(|| self.hi());
    let trailing_comments = search_end.trailing_comments_fast(context.program);
    let previous_end_line = search_end.end_line_fast(context.program);
    for comment in trailing_comments {
      // optimization
      if comment.kind == CommentKind::Line {
        break;
      }

      let comment_start_line = comment.start_line_fast(context.program);
      if comment_start_line <= previous_end_line {
        let comment_end_line = comment.end_line_fast(context.program);
        if comment_end_line > previous_end_line {
          return comment_end_line; // should only include the first multi-line comment block
        }
      } else {
        break;
      }
    }

    previous_end_line
  }
}

/* custom enums */

pub enum TypeParamNode<'a> {
  Instantiation(&'a TsTypeParamInstantiation<'a>),
  Decl(&'a TsTypeParamDecl<'a>),
}

impl<'a> Spanned for TypeParamNode<'a> {
  fn span(&self) -> Span {
    match self {
      TypeParamNode::Instantiation(node) => node.span(),
      TypeParamNode::Decl(node) => node.span(),
    }
  }
}

impl<'a> TypeParamNode<'a> {
  pub fn params(&self) -> Vec<Node<'a>> {
    match self {
      TypeParamNode::Instantiation(node) => node.params.iter().map(|p| p.into()).collect(),
      TypeParamNode::Decl(node) => node.params.iter().map(|&p| p.into()).collect(),
    }
  }

  pub fn parent(&self) -> Node<'a> {
    match self {
      TypeParamNode::Instantiation(node) => node.parent(),
      TypeParamNode::Decl(node) => node.parent(),
    }
  }
}

/* InnerSpanned */

pub trait InnerSpanned {
  fn get_inner_span(&self, context: &mut Context) -> Span;
}

impl<'a> InnerSpanned for &BlockStmt<'a> {
  fn get_inner_span(&self, _: &mut Context) -> Span {
    get_inner_span_for_object_like(&self.span())
  }
}

impl<'a> InnerSpanned for &ObjectLit<'a> {
  fn get_inner_span(&self, _: &mut Context) -> Span {
    get_inner_span_for_object_like(&self.span())
  }
}

impl<'a> InnerSpanned for &ObjectPat<'a> {
  fn get_inner_span(&self, _: &mut Context) -> Span {
    get_inner_span_for_object_like(&self.span())
  }
}

fn get_inner_span_for_object_like(span: &Span) -> Span {
  Span {
    lo: BytePos(span.lo.0 + 1),
    hi: BytePos(span.hi.0 - 1),
    ctxt: Default::default(),
  }
}

pub trait NodeExtensions<'a> {
  fn get_type_parameters(&self) -> &Option<&'a TsTypeParamDecl>;
}

impl<'a> NodeExtensions<'a> for Node<'a> {
  fn get_type_parameters(&self) -> &Option<&'a TsTypeParamDecl> {
    match self {
      Node::ClassDecl(node) => &node.class.type_params,
      Node::Class(node) => &node.type_params,
      Node::TsInterfaceDecl(node) => &node.type_params,
      Node::ClassExpr(node) => &node.class.type_params,
      Node::FnDecl(node) => &node.function.type_params,
      Node::Function(node) => &node.type_params,
      Node::ClassMethod(node) => &node.function.type_params,
      Node::TsTypeAliasDecl(node) => &node.type_params,
      Node::ArrowExpr(node) => &node.type_params,
      Node::TsCallSignatureDecl(node) => &node.type_params,
      Node::TsConstructSignatureDecl(node) => &node.type_params,
      Node::TsMethodSignature(node) => &node.type_params,
      Node::MethodProp(node) => &node.function.type_params,
      Node::TsConstructorType(node) => &node.type_params,
      Node::TsFnType(node) => &node.type_params,
      _ => &None,
    }
  }
}

/* ParametersSpanned */

pub trait ParametersSpanned {
  fn get_parameters_span(&self, context: &mut Context) -> Option<Span>;
}

impl<'a> ParametersSpanned for &Function<'a> {
  fn get_parameters_span(&self, context: &mut Context) -> Option<Span> {
    get_params_or_args_span(
      self.lo(),
      self.params.iter().map(|x| x.span()).collect(),
      self
        .return_type
        .map(|t| t.lo())
        .or_else(|| self.body.as_ref().map(|b| b.lo()))
        .unwrap_or_else(|| self.hi()),
      context,
    )
  }
}

impl<'a> ParametersSpanned for &PrivateMethod<'a> {
  fn get_parameters_span(&self, context: &mut Context) -> Option<Span> {
    self.function.get_parameters_span(context)
  }
}

impl<'a> ParametersSpanned for &ClassMethod<'a> {
  fn get_parameters_span(&self, context: &mut Context) -> Option<Span> {
    self.function.get_parameters_span(context)
  }
}

impl<'a> ParametersSpanned for &Constructor<'a> {
  fn get_parameters_span(&self, context: &mut Context) -> Option<Span> {
    get_params_or_args_span(
      self.lo(),
      self.params.iter().map(|x| x.span()).collect(),
      self.body.as_ref().map(|t| t.lo()).unwrap_or_else(|| self.hi()),
      context,
    )
  }
}

impl<'a> ParametersSpanned for &MethodProp<'a> {
  fn get_parameters_span(&self, context: &mut Context) -> Option<Span> {
    self.function.get_parameters_span(context)
  }
}

impl<'a> ParametersSpanned for &GetterProp<'a> {
  fn get_parameters_span(&self, context: &mut Context) -> Option<Span> {
    get_params_or_args_span(
      self.lo(),
      vec![],
      self
        .type_ann
        .as_ref()
        .map(|t| t.lo())
        .or_else(|| self.body.as_ref().map(|t| t.lo()))
        .unwrap_or_else(|| self.hi()),
      context,
    )
  }
}

impl<'a> ParametersSpanned for &SetterProp<'a> {
  fn get_parameters_span(&self, context: &mut Context) -> Option<Span> {
    get_params_or_args_span(
      self.lo(),
      vec![self.param.span()],
      self.body.as_ref().map(|t| t.lo()).unwrap_or_else(|| self.hi()),
      context,
    )
  }
}

impl<'a> ParametersSpanned for &ArrowExpr<'a> {
  fn get_parameters_span(&self, context: &mut Context) -> Option<Span> {
    get_params_or_args_span(
      self.lo(),
      self.params.iter().map(|x| x.span()).collect(),
      self.return_type.as_ref().map(|t| t.lo()).unwrap_or_else(|| self.body.lo()),
      context,
    )
  }
}

impl<'a> ParametersSpanned for CallOrOptCallExpr<'a> {
  fn get_parameters_span(&self, context: &mut Context) -> Option<Span> {
    get_params_or_args_span(self.lo(), self.args().iter().map(|a| a.span()).collect(), self.hi(), context)
  }
}

impl<'a> ParametersSpanned for &NewExpr<'a> {
  fn get_parameters_span(&self, context: &mut Context) -> Option<Span> {
    get_params_or_args_span(
      self.lo(),
      self.args.as_ref().map(|args| args.iter().map(|a| a.span()).collect()).unwrap_or_default(),
      self.hi(),
      context,
    )
  }
}

impl<'a> ParametersSpanned for &TsCallSignatureDecl<'a> {
  fn get_parameters_span(&self, context: &mut Context) -> Option<Span> {
    get_params_or_args_span(
      self.lo(),
      self.params.iter().map(|x| x.span()).collect(),
      self.type_ann.as_ref().map(|t| t.lo()).unwrap_or_else(|| self.hi()),
      context,
    )
  }
}

impl<'a> ParametersSpanned for &TsConstructSignatureDecl<'a> {
  fn get_parameters_span(&self, context: &mut Context) -> Option<Span> {
    get_params_or_args_span(
      self.lo(),
      self.params.iter().map(|x| x.span()).collect(),
      self.type_ann.as_ref().map(|t| t.lo()).unwrap_or_else(|| self.hi()),
      context,
    )
  }
}

impl<'a> ParametersSpanned for &TsGetterSignature<'a> {
  fn get_parameters_span(&self, context: &mut Context) -> Option<Span> {
    get_params_or_args_span(
      self.lo(),
      Vec::with_capacity(0),
      self.type_ann.as_ref().map(|t| t.lo()).unwrap_or_else(|| self.hi()),
      context,
    )
  }
}

impl<'a> ParametersSpanned for &TsSetterSignature<'a> {
  fn get_parameters_span(&self, context: &mut Context) -> Option<Span> {
    get_params_or_args_span(self.lo(), vec![self.param.span()], self.hi(), context)
  }
}

impl<'a> ParametersSpanned for &TsMethodSignature<'a> {
  fn get_parameters_span(&self, context: &mut Context) -> Option<Span> {
    get_params_or_args_span(
      self.lo(),
      self.params.iter().map(|x| x.span()).collect(),
      self.type_ann.as_ref().map(|t| t.lo()).unwrap_or_else(|| self.hi()),
      context,
    )
  }
}

impl<'a> ParametersSpanned for &TsConstructorType<'a> {
  fn get_parameters_span(&self, context: &mut Context) -> Option<Span> {
    get_params_or_args_span(self.lo(), self.params.iter().map(|x| x.span()).collect(), self.type_ann.lo(), context)
  }
}

impl<'a> ParametersSpanned for &TsFnType<'a> {
  fn get_parameters_span(&self, context: &mut Context) -> Option<Span> {
    get_params_or_args_span(self.lo(), self.params.iter().map(|x| x.span()).collect(), self.type_ann.lo(), context)
  }
}

fn get_params_or_args_span(start_pos: BytePos, params: Vec<Span>, following_pos: BytePos, context: &mut Context) -> Option<Span> {
  let close_token_end = {
    let close_paren = if let Some(last_param) = params.last() {
      context.token_finder.get_first_close_paren_after(last_param)
    } else {
      context.token_finder.get_first_close_paren_before(&following_pos)
    };
    if let Some(close_paren) = close_paren {
      let end = close_paren.hi();
      if end > start_pos {
        Some(end)
      } else {
        None
      }
    } else {
      None
    }
  }?;
  let close_token_start = {
    let open_paren = context.token_finder.get_first_open_paren_before(&{
      if let Some(first_param) = params.first() {
        first_param.lo()
      } else {
        close_token_end
      }
    });

    if let Some(open_paren) = open_paren {
      let pos = open_paren.lo();
      if pos >= start_pos {
        Some(pos)
      } else {
        None
      }
    } else {
      None
    }
  }?;

  Some(Span {
    lo: close_token_start,
    hi: close_token_end,
    ctxt: Default::default(),
  })
}

#[derive(Copy, Clone)]
pub enum CallOrOptCallExpr<'a> {
  CallExpr(&'a CallExpr<'a>),
  OptCall(&'a OptCall<'a>),
}

impl<'a> CallOrOptCallExpr<'a> {
  pub fn is_optional(&self) -> bool {
    matches!(self, CallOrOptCallExpr::OptCall(_))
  }

  pub fn type_args(&self) -> Option<&'a TsTypeParamInstantiation<'a>> {
    match self {
      CallOrOptCallExpr::CallExpr(node) => node.type_args,
      CallOrOptCallExpr::OptCall(node) => node.type_args,
    }
  }

  pub fn args(&self) -> &Vec<&'a ExprOrSpread<'a>> {
    match self {
      CallOrOptCallExpr::CallExpr(node) => &node.args,
      CallOrOptCallExpr::OptCall(node) => &node.args,
    }
  }

  pub fn callee(&self) -> Callee<'a> {
    match self {
      CallOrOptCallExpr::CallExpr(node) => node.callee,
      CallOrOptCallExpr::OptCall(node) => Callee::Expr(node.callee),
    }
  }
}

impl<'a> Spanned for CallOrOptCallExpr<'a> {
  fn span(&self) -> deno_ast::swc::common::Span {
    match self {
      CallOrOptCallExpr::CallExpr(node) => node.span(),
      CallOrOptCallExpr::OptCall(node) => node.span(),
    }
  }
}

impl<'a> From<&'a CallExpr<'a>> for CallOrOptCallExpr<'a> {
  fn from(call_expr: &'a CallExpr<'a>) -> Self {
    CallOrOptCallExpr::CallExpr(call_expr)
  }
}

impl<'a> From<&'a OptCall<'a>> for CallOrOptCallExpr<'a> {
  fn from(opt_call: &'a OptCall<'a>) -> Self {
    CallOrOptCallExpr::OptCall(opt_call)
  }
}

#[allow(clippy::from_over_into)]
impl<'a> Into<Node<'a>> for CallOrOptCallExpr<'a> {
  fn into(self) -> Node<'a> {
    match self {
      CallOrOptCallExpr::CallExpr(node) => node.into(),
      CallOrOptCallExpr::OptCall(node) => node.into(),
    }
  }
}
