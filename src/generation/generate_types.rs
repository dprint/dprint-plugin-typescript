use deno_ast::swc::common::comments::CommentKind;
use deno_ast::view::*;
use deno_ast::SourcePos;
use deno_ast::SourceRange;
use deno_ast::SourceRanged;
use deno_ast::SourceRangedForSpanned;

use super::*;

pub trait RangedExtensions {
  fn start_line_with_comments(&self, context: &mut Context) -> usize;
  fn end_line_with_comments(&self, context: &mut Context) -> usize;
}

impl<T> RangedExtensions for T
where
  T: SourceRanged,
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
      let lo = self.start();
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
    let search_end = context
      .token_finder
      .get_next_token_if_comma(self)
      .map(|x| x.end())
      .unwrap_or_else(|| self.end());
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

impl<'a> SourceRanged for TypeParamNode<'a> {
  fn start(&self) -> SourcePos {
    match self {
      TypeParamNode::Instantiation(node) => node.start(),
      TypeParamNode::Decl(node) => node.start(),
    }
  }

  fn end(&self) -> SourcePos {
    match self {
      TypeParamNode::Instantiation(node) => node.end(),
      TypeParamNode::Decl(node) => node.end(),
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

/* InnerRanged */

pub trait InnerRanged {
  fn get_inner_range(&self, context: &mut Context) -> SourceRange;
}

impl<'a> InnerRanged for &BlockStmt<'a> {
  fn get_inner_range(&self, _: &mut Context) -> SourceRange {
    get_inner_range_for_object_like(&self.range())
  }
}

impl<'a> InnerRanged for &ObjectLit<'a> {
  fn get_inner_range(&self, _: &mut Context) -> SourceRange {
    get_inner_range_for_object_like(&self.range())
  }
}

impl<'a> InnerRanged for &ObjectPat<'a> {
  fn get_inner_range(&self, _: &mut Context) -> SourceRange {
    get_inner_range_for_object_like(&self.range())
  }
}

fn get_inner_range_for_object_like(range: &SourceRange) -> SourceRange {
  SourceRange::new(range.start + 1, range.end - 1)
}

pub trait NodeExtensions<'a> {
  fn get_type_parameters(&self) -> Option<&'a TsTypeParamDecl<'a>>;
}

impl<'a> NodeExtensions<'a> for Node<'a> {
  fn get_type_parameters(&self) -> Option<&'a TsTypeParamDecl<'a>> {
    match self {
      Node::ClassDecl(node) => node.class.type_params,
      Node::Class(node) => node.type_params,
      Node::TsInterfaceDecl(node) => node.type_params,
      Node::ClassExpr(node) => node.class.type_params,
      Node::FnDecl(node) => node.function.type_params,
      Node::Function(node) => node.type_params,
      Node::ClassMethod(node) => node.function.type_params,
      Node::TsTypeAliasDecl(node) => node.type_params,
      Node::ArrowExpr(node) => node.type_params,
      Node::TsCallSignatureDecl(node) => node.type_params,
      Node::TsConstructSignatureDecl(node) => node.type_params,
      Node::TsMethodSignature(node) => node.type_params,
      Node::MethodProp(node) => node.function.type_params,
      Node::TsConstructorType(node) => node.type_params,
      Node::TsFnType(node) => node.type_params,
      _ => None,
    }
  }
}

/* ParametersRanged */

pub trait ParametersRanged {
  fn get_parameters_range(&self, context: &mut Context) -> Option<SourceRange>;
}

impl<'a> ParametersRanged for &Function<'a> {
  fn get_parameters_range(&self, context: &mut Context) -> Option<SourceRange> {
    get_params_or_args_range(
      self.start(),
      self.params.iter().map(|x| x.range()).collect(),
      self
        .return_type
        .map(|t| t.start())
        .or_else(|| self.body.as_ref().map(|b| b.start()))
        .unwrap_or_else(|| self.end()),
      context,
    )
  }
}

impl<'a> ParametersRanged for &PrivateMethod<'a> {
  fn get_parameters_range(&self, context: &mut Context) -> Option<SourceRange> {
    self.function.get_parameters_range(context)
  }
}

impl<'a> ParametersRanged for &ClassMethod<'a> {
  fn get_parameters_range(&self, context: &mut Context) -> Option<SourceRange> {
    self.function.get_parameters_range(context)
  }
}

impl<'a> ParametersRanged for &Constructor<'a> {
  fn get_parameters_range(&self, context: &mut Context) -> Option<SourceRange> {
    get_params_or_args_range(
      self.start(),
      self.params.iter().map(|x| x.range()).collect(),
      self.body.as_ref().map(|t| t.start()).unwrap_or_else(|| self.end()),
      context,
    )
  }
}

impl<'a> ParametersRanged for &MethodProp<'a> {
  fn get_parameters_range(&self, context: &mut Context) -> Option<SourceRange> {
    self.function.get_parameters_range(context)
  }
}

impl<'a> ParametersRanged for &GetterProp<'a> {
  fn get_parameters_range(&self, context: &mut Context) -> Option<SourceRange> {
    get_params_or_args_range(
      self.start(),
      vec![],
      self
        .type_ann
        .as_ref()
        .map(|t| t.start())
        .or_else(|| self.body.as_ref().map(|t| t.start()))
        .unwrap_or_else(|| self.end()),
      context,
    )
  }
}

impl<'a> ParametersRanged for &SetterProp<'a> {
  fn get_parameters_range(&self, context: &mut Context) -> Option<SourceRange> {
    get_params_or_args_range(
      self.start(),
      vec![self.param.range()],
      self.body.as_ref().map(|t| t.start()).unwrap_or_else(|| self.end()),
      context,
    )
  }
}

impl<'a> ParametersRanged for &ArrowExpr<'a> {
  fn get_parameters_range(&self, context: &mut Context) -> Option<SourceRange> {
    get_params_or_args_range(
      self.start(),
      self.params.iter().map(|x| x.range()).collect(),
      self.return_type.as_ref().map(|t| t.start()).unwrap_or_else(|| self.body.start()),
      context,
    )
  }
}

impl<'a> ParametersRanged for CallOrOptCallExpr<'a> {
  fn get_parameters_range(&self, context: &mut Context) -> Option<SourceRange> {
    get_params_or_args_range(self.start(), self.args().iter().map(|a| a.range()).collect(), self.end(), context)
  }
}

impl<'a> ParametersRanged for &NewExpr<'a> {
  fn get_parameters_range(&self, context: &mut Context) -> Option<SourceRange> {
    get_params_or_args_range(
      self.start(),
      self.args.as_ref().map(|args| args.iter().map(|a| a.range()).collect()).unwrap_or_default(),
      self.end(),
      context,
    )
  }
}

impl<'a> ParametersRanged for &TsCallSignatureDecl<'a> {
  fn get_parameters_range(&self, context: &mut Context) -> Option<SourceRange> {
    get_params_or_args_range(
      self.start(),
      self.params.iter().map(|x| x.range()).collect(),
      self.type_ann.as_ref().map(|t| t.start()).unwrap_or_else(|| self.end()),
      context,
    )
  }
}

impl<'a> ParametersRanged for &TsConstructSignatureDecl<'a> {
  fn get_parameters_range(&self, context: &mut Context) -> Option<SourceRange> {
    get_params_or_args_range(
      self.start(),
      self.params.iter().map(|x| x.range()).collect(),
      self.type_ann.as_ref().map(|t| t.start()).unwrap_or_else(|| self.end()),
      context,
    )
  }
}

impl<'a> ParametersRanged for &TsGetterSignature<'a> {
  fn get_parameters_range(&self, context: &mut Context) -> Option<SourceRange> {
    get_params_or_args_range(
      self.start(),
      Vec::with_capacity(0),
      self.type_ann.as_ref().map(|t| t.start()).unwrap_or_else(|| self.end()),
      context,
    )
  }
}

impl<'a> ParametersRanged for &TsSetterSignature<'a> {
  fn get_parameters_range(&self, context: &mut Context) -> Option<SourceRange> {
    get_params_or_args_range(self.start(), vec![self.param.range()], self.end(), context)
  }
}

impl<'a> ParametersRanged for &TsMethodSignature<'a> {
  fn get_parameters_range(&self, context: &mut Context) -> Option<SourceRange> {
    get_params_or_args_range(
      self.start(),
      self.params.iter().map(|x| x.range()).collect(),
      self.type_ann.as_ref().map(|t| t.start()).unwrap_or_else(|| self.end()),
      context,
    )
  }
}

impl<'a> ParametersRanged for &TsConstructorType<'a> {
  fn get_parameters_range(&self, context: &mut Context) -> Option<SourceRange> {
    get_params_or_args_range(self.start(), self.params.iter().map(|x| x.range()).collect(), self.type_ann.start(), context)
  }
}

impl<'a> ParametersRanged for &TsFnType<'a> {
  fn get_parameters_range(&self, context: &mut Context) -> Option<SourceRange> {
    get_params_or_args_range(self.start(), self.params.iter().map(|x| x.range()).collect(), self.type_ann.start(), context)
  }
}

fn get_params_or_args_range(start_pos: SourcePos, params: Vec<SourceRange>, following_pos: SourcePos, context: &mut Context) -> Option<SourceRange> {
  let close_token_end = {
    let close_paren = if let Some(last_param) = params.last() {
      context.token_finder.get_first_close_paren_after(last_param)
    } else {
      context.token_finder.get_first_close_paren_before(&following_pos)
    };
    if let Some(close_paren) = close_paren {
      let end = close_paren.end();
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
        first_param.start()
      } else {
        close_token_end
      }
    });

    if let Some(open_paren) = open_paren {
      let pos = open_paren.start();
      if pos >= start_pos {
        Some(pos)
      } else {
        None
      }
    } else {
      None
    }
  }?;

  Some(SourceRange::new(close_token_start, close_token_end))
}

#[derive(Copy, Clone)]
pub enum CallOrOptCallExpr<'a> {
  CallExpr(&'a CallExpr<'a>),
  OptCall(&'a OptCall<'a>),
}

impl<'a> CallOrOptCallExpr<'a> {
  pub fn is_optional(&self) -> bool {
    let Self::OptCall(opt_call) = self else {
      return false;
    };
    opt_call.parent().optional()
  }

  pub fn type_args(&self) -> Option<&'a TsTypeParamInstantiation<'a>> {
    match self {
      Self::CallExpr(node) => node.type_args,
      Self::OptCall(node) => node.type_args,
    }
  }

  pub fn args(&self) -> &[&'a ExprOrSpread<'a>] {
    match self {
      Self::CallExpr(node) => node.args,
      Self::OptCall(node) => node.args,
    }
  }

  pub fn callee(&self) -> Callee<'a> {
    match self {
      Self::CallExpr(node) => node.callee,
      Self::OptCall(node) => Callee::Expr(node.callee),
    }
  }
}

impl<'a> SourceRanged for CallOrOptCallExpr<'a> {
  fn start(&self) -> SourcePos {
    match self {
      Self::CallExpr(node) => node.start(),
      Self::OptCall(node) => node.start(),
    }
  }

  fn end(&self) -> SourcePos {
    match self {
      Self::CallExpr(node) => node.end(),
      Self::OptCall(node) => node.end(),
    }
  }
}

impl<'a> From<&'a CallExpr<'a>> for CallOrOptCallExpr<'a> {
  fn from(call_expr: &'a CallExpr<'a>) -> Self {
    Self::CallExpr(call_expr)
  }
}

impl<'a> From<&'a OptCall<'a>> for CallOrOptCallExpr<'a> {
  fn from(opt_call: &'a OptCall<'a>) -> Self {
    Self::OptCall(opt_call)
  }
}

#[allow(clippy::from_over_into)]
impl<'a> Into<Node<'a>> for CallOrOptCallExpr<'a> {
  fn into(self) -> Node<'a> {
    match self {
      Self::CallExpr(node) => node.into(),
      Self::OptCall(node) => node.into(),
    }
  }
}
