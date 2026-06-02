use deno_ast::oxc::ast::ast::ArrowFunctionExpression;
use deno_ast::oxc::ast::ast::BlockStatement;
use deno_ast::oxc::ast::ast::CallExpression;
use deno_ast::oxc::ast::ast::Expression;
use deno_ast::oxc::ast::ast::Function;
use deno_ast::oxc::ast::ast::FunctionBody;
use deno_ast::oxc::ast::ast::ImportExpression;
use deno_ast::oxc::ast::ast::NewExpression;
use deno_ast::oxc::ast::ast::ObjectExpression;
use deno_ast::oxc::ast::ast::ObjectPattern;
use deno_ast::oxc::ast::ast::TSCallSignatureDeclaration;
use deno_ast::oxc::ast::ast::TSConstructSignatureDeclaration;
use deno_ast::oxc::ast::ast::TSConstructorType;
use deno_ast::oxc::ast::ast::TSFunctionType;
use deno_ast::oxc::ast::ast::TSMethodSignature;
use deno_ast::oxc::ast::ast::TSTypeParameterDeclaration;
use deno_ast::oxc::ast::ast::TSTypeParameterInstantiation;
use deno_ast::oxc::ast::ast::TSTypeParameterInstantiation as TypeArgs;
use deno_ast::oxc::span::GetSpan;
use deno_ast::oxc::span::Span;

use super::Context;
use super::oxc_helpers::CommentExt;
use super::oxc_helpers::Node;
use super::oxc_helpers::PosExt;
use super::oxc_helpers::SourcePos;
use super::oxc_helpers::SourceRange;
use super::oxc_helpers::SourceRanged;
use super::to_node::ts_type_to_node;

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
      let previous_token = lo.previous_token_fast(context.program);
      if let Some(previous_token) = previous_token {
        let previous_end_line = previous_token.end().line_fast(context.program);
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
    let previous_end_line = search_end.line_fast(context.program);
    for comment in trailing_comments {
      // optimization
      if comment.is_line() {
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
  Instantiation(&'a TSTypeParameterInstantiation<'a>),
  Decl(&'a TSTypeParameterDeclaration<'a>),
}

impl<'a> GetSpan for TypeParamNode<'a> {
  fn span(&self) -> Span {
    match self {
      TypeParamNode::Instantiation(node) => node.span,
      TypeParamNode::Decl(node) => node.span,
    }
  }
}

impl<'a> TypeParamNode<'a> {
  pub fn params(&self) -> Vec<Node<'a>> {
    match self {
      TypeParamNode::Instantiation(node) => node.params.iter().map(ts_type_to_node).collect(),
      TypeParamNode::Decl(node) => node.params.iter().map(Node::TSTypeParameter).collect(),
    }
  }

  /// The parent of a type parameter node is whatever is currently on the
  /// context's parent stack while it is being generated.
  pub fn parent(&self, context: &Context<'a>) -> Node<'a> {
    context.parent()
  }
}

/* InnerRanged */

pub trait InnerRanged {
  fn get_inner_range(&self, context: &mut Context) -> SourceRange;
}

impl<'a> InnerRanged for &BlockStatement<'a> {
  fn get_inner_range(&self, _: &mut Context) -> SourceRange {
    get_inner_range_for_object_like(&self.range())
  }
}

impl<'a> InnerRanged for &FunctionBody<'a> {
  fn get_inner_range(&self, _: &mut Context) -> SourceRange {
    get_inner_range_for_object_like(&self.range())
  }
}

impl<'a> InnerRanged for &ObjectExpression<'a> {
  fn get_inner_range(&self, _: &mut Context) -> SourceRange {
    get_inner_range_for_object_like(&self.range())
  }
}

impl<'a> InnerRanged for &ObjectPattern<'a> {
  fn get_inner_range(&self, _: &mut Context) -> SourceRange {
    get_inner_range_for_object_like(&self.range())
  }
}

fn get_inner_range_for_object_like(range: &SourceRange) -> SourceRange {
  SourceRange::new(range.start + 1, range.end - 1)
}

pub trait NodeExtensions<'a> {
  fn get_type_parameters(&self) -> Option<&'a TSTypeParameterDeclaration<'a>>;
}

impl<'a> NodeExtensions<'a> for Node<'a> {
  fn get_type_parameters(&self) -> Option<&'a TSTypeParameterDeclaration<'a>> {
    match self {
      Node::Class(node) => node.type_parameters.as_deref(),
      Node::TSInterfaceDeclaration(node) => node.type_parameters.as_deref(),
      Node::Function(node) => node.type_parameters.as_deref(),
      Node::MethodDefinition(node) => node.value.type_parameters.as_deref(),
      Node::TSTypeAliasDeclaration(node) => node.type_parameters.as_deref(),
      Node::ArrowFunctionExpression(node) => node.type_parameters.as_deref(),
      Node::TSCallSignatureDeclaration(node) => node.type_parameters.as_deref(),
      Node::TSConstructSignatureDeclaration(node) => node.type_parameters.as_deref(),
      Node::TSMethodSignature(node) => node.type_parameters.as_deref(),
      Node::TSConstructorType(node) => node.type_parameters.as_deref(),
      Node::TSFunctionType(node) => node.type_parameters.as_deref(),
      _ => None,
    }
  }
}

/* ParametersRanged */

pub trait ParametersRanged {
  fn get_parameters_range(&self, context: &mut Context) -> Option<SourceRange>;
}

impl<'a> ParametersRanged for Function<'a> {
  fn get_parameters_range(&self, context: &mut Context) -> Option<SourceRange> {
    get_params_or_args_range(
      self.start(),
      self.params.items.iter().map(|x| x.range()).collect(),
      self
        .return_type
        .as_ref()
        .map(|t| t.start())
        .or_else(|| self.body.as_ref().map(|b| b.start()))
        .unwrap_or_else(|| self.end()),
      context,
    )
  }
}

impl<'a> ParametersRanged for ArrowFunctionExpression<'a> {
  fn get_parameters_range(&self, context: &mut Context) -> Option<SourceRange> {
    get_params_or_args_range(
      self.start(),
      self.params.items.iter().map(|x| x.range()).collect(),
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

impl<'a> ParametersRanged for NewExpression<'a> {
  fn get_parameters_range(&self, context: &mut Context) -> Option<SourceRange> {
    get_params_or_args_range(self.start(), self.arguments.iter().map(|a| a.range()).collect(), self.end(), context)
  }
}

impl<'a> ParametersRanged for ImportExpression<'a> {
  fn get_parameters_range(&self, context: &mut Context) -> Option<SourceRange> {
    let mut args = vec![self.source.range()];
    if let Some(options) = &self.options {
      args.push(options.range());
    }
    get_params_or_args_range(self.start(), args, self.end(), context)
  }
}

impl<'a> ParametersRanged for TSCallSignatureDeclaration<'a> {
  fn get_parameters_range(&self, context: &mut Context) -> Option<SourceRange> {
    get_params_or_args_range(
      self.start(),
      self.params.items.iter().map(|x| x.range()).collect(),
      self.return_type.as_ref().map(|t| t.start()).unwrap_or_else(|| self.end()),
      context,
    )
  }
}

impl<'a> ParametersRanged for TSConstructSignatureDeclaration<'a> {
  fn get_parameters_range(&self, context: &mut Context) -> Option<SourceRange> {
    get_params_or_args_range(
      self.start(),
      self.params.items.iter().map(|x| x.range()).collect(),
      self.return_type.as_ref().map(|t| t.start()).unwrap_or_else(|| self.end()),
      context,
    )
  }
}

impl<'a> ParametersRanged for TSMethodSignature<'a> {
  fn get_parameters_range(&self, context: &mut Context) -> Option<SourceRange> {
    get_params_or_args_range(
      self.start(),
      self.params.items.iter().map(|x| x.range()).collect(),
      self.return_type.as_ref().map(|t| t.start()).unwrap_or_else(|| self.end()),
      context,
    )
  }
}

impl<'a> ParametersRanged for TSConstructorType<'a> {
  fn get_parameters_range(&self, context: &mut Context) -> Option<SourceRange> {
    get_params_or_args_range(
      self.start(),
      self.params.items.iter().map(|x| x.range()).collect(),
      self.return_type.start(),
      context,
    )
  }
}

impl<'a> ParametersRanged for TSFunctionType<'a> {
  fn get_parameters_range(&self, context: &mut Context) -> Option<SourceRange> {
    get_params_or_args_range(
      self.start(),
      self.params.items.iter().map(|x| x.range()).collect(),
      self.return_type.start(),
      context,
    )
  }
}

fn get_params_or_args_range(start_pos: SourcePos, params: Vec<SourceRange>, following_pos: SourcePos, context: &mut Context) -> Option<SourceRange> {
  let close_token_end = {
    let close_paren = if let Some(last_param) = params.last() {
      context.token_finder.get_first_close_paren_after(last_param)
    } else {
      context
        .token_finder
        .get_first_close_paren_before(&SourceRange::new(following_pos, following_pos))
    };
    if let Some(close_paren) = close_paren {
      let end = close_paren.end();
      if end > start_pos { Some(end) } else { None }
    } else {
      None
    }
  }?;
  let close_token_start = {
    let before_pos = if let Some(first_param) = params.first() {
      first_param.start()
    } else {
      close_token_end
    };
    let open_paren = context.token_finder.get_first_open_paren_before(&SourceRange::new(before_pos, before_pos));

    if let Some(open_paren) = open_paren {
      let pos = open_paren.start();
      if pos >= start_pos { Some(pos) } else { None }
    } else {
      None
    }
  }?;

  Some(SourceRange::new(close_token_start, close_token_end))
}

/// In SWC this distinguished `CallExpr` from `OptCall`. oxc has no separate
/// optional-call node — optional chaining is represented with a `.optional`
/// flag and a wrapping `ChainExpression` — so this simply wraps a
/// `CallExpression`.
#[derive(Copy, Clone)]
pub struct CallOrOptCallExpr<'a>(pub &'a CallExpression<'a>);

impl<'a> CallOrOptCallExpr<'a> {
  pub fn inner(&self) -> &'a CallExpression<'a> {
    self.0
  }

  pub fn is_optional(&self) -> bool {
    self.0.optional
  }

  pub fn type_args(&self) -> Option<&'a TypeArgs<'a>> {
    self.0.type_arguments.as_deref()
  }

  pub fn args(&self) -> &'a [deno_ast::oxc::ast::ast::Argument<'a>] {
    &self.0.arguments
  }

  pub fn callee(&self) -> &'a Expression<'a> {
    &self.0.callee
  }
}

impl<'a> GetSpan for CallOrOptCallExpr<'a> {
  fn span(&self) -> Span {
    self.0.span
  }
}

impl<'a> From<&'a CallExpression<'a>> for CallOrOptCallExpr<'a> {
  fn from(call_expr: &'a CallExpression<'a>) -> Self {
    Self(call_expr)
  }
}
