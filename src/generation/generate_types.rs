use super::*;
use crate::ast::*;

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

#[derive(Clone, Copy)]
pub enum TypeParamNode<'a> {
  Instantiation(&'a TSTypeParameterInstantiation<'a>),
  Decl(&'a TSTypeParameterDeclaration<'a>),
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
      TypeParamNode::Instantiation(node) => node.params.iter().map(|p| p.into_node()).collect(),
      TypeParamNode::Decl(node) => node.params.iter().map(|p| p.into_node()).collect(),
    }
  }

  pub fn node(&self) -> Node<'a> {
    match self {
      TypeParamNode::Instantiation(node) => node.into_node(),
      TypeParamNode::Decl(node) => node.into_node(),
    }
  }

  pub fn parent(&self, program: ProgramInfo<'a>) -> Node<'a> {
    self.node().parent(program).unwrap()
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

impl<'a> InnerRanged for &ObjectAssignmentTarget<'a> {
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

impl<'a> ParametersRanged for &Function<'a> {
  fn get_parameters_range(&self, context: &mut Context) -> Option<SourceRange> {
    get_formal_params_range(
      self.start(),
      self.this_param.as_deref(),
      &self.params,
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

impl<'a> ParametersRanged for &ArrowFunctionExpression<'a> {
  fn get_parameters_range(&self, context: &mut Context) -> Option<SourceRange> {
    get_formal_params_range(
      self.start(),
      None,
      &self.params,
      self.return_type.as_ref().map(|t| t.start()).unwrap_or_else(|| self.body.start()),
      context,
    )
  }
}

impl<'a> ParametersRanged for &CallExpression<'a> {
  fn get_parameters_range(&self, context: &mut Context) -> Option<SourceRange> {
    get_params_or_args_range(
      self.start(),
      self.arguments.first().map(|x| x.range()),
      self.arguments.last().map(|x| x.range()),
      self.end(),
      context,
    )
  }
}

impl<'a> ParametersRanged for &NewExpression<'a> {
  fn get_parameters_range(&self, context: &mut Context) -> Option<SourceRange> {
    get_params_or_args_range(
      self.start(),
      self.arguments.first().map(|x| x.range()),
      self.arguments.last().map(|x| x.range()),
      self.end(),
      context,
    )
  }
}

impl<'a> ParametersRanged for &TSCallSignatureDeclaration<'a> {
  fn get_parameters_range(&self, context: &mut Context) -> Option<SourceRange> {
    get_formal_params_range(
      self.start(),
      self.this_param.as_deref(),
      &self.params,
      self.return_type.as_ref().map(|t| t.start()).unwrap_or_else(|| self.end()),
      context,
    )
  }
}

impl<'a> ParametersRanged for &TSConstructSignatureDeclaration<'a> {
  fn get_parameters_range(&self, context: &mut Context) -> Option<SourceRange> {
    get_formal_params_range(
      self.start(),
      None,
      &self.params,
      self.return_type.as_ref().map(|t| t.start()).unwrap_or_else(|| self.end()),
      context,
    )
  }
}

impl<'a> ParametersRanged for &TSMethodSignature<'a> {
  fn get_parameters_range(&self, context: &mut Context) -> Option<SourceRange> {
    get_formal_params_range(
      self.start(),
      self.this_param.as_deref(),
      &self.params,
      self.return_type.as_ref().map(|t| t.start()).unwrap_or_else(|| self.end()),
      context,
    )
  }
}

impl<'a> ParametersRanged for &TSConstructorType<'a> {
  fn get_parameters_range(&self, context: &mut Context) -> Option<SourceRange> {
    get_formal_params_range(self.start(), None, &self.params, self.return_type.start(), context)
  }
}

impl<'a> ParametersRanged for &TSFunctionType<'a> {
  fn get_parameters_range(&self, context: &mut Context) -> Option<SourceRange> {
    get_formal_params_range(self.start(), self.this_param.as_deref(), &self.params, self.return_type.start(), context)
  }
}

/// Gets the range of the parameters, which includes a `this` parameter.
fn get_formal_params_range(
  start_pos: SourcePos,
  this_param: Option<&TSThisParameter>,
  params: &FormalParameters,
  following_pos: SourcePos,
  context: &mut Context,
) -> Option<SourceRange> {
  let this_param_range = this_param.map(|p| p.range());
  let first_item_range = params.items.first().map(|x| x.range()).or_else(|| params.rest.as_ref().map(|r| r.range()));
  let last_item_range = params.rest.as_ref().map(|r| r.range()).or_else(|| params.items.last().map(|x| x.range()));
  get_params_or_args_range(
    start_pos,
    this_param_range.or(first_item_range),
    last_item_range.or(this_param_range),
    following_pos,
    context,
  )
}

fn get_params_or_args_range(
  start_pos: SourcePos,
  first_param: Option<SourceRange>,
  last_param: Option<SourceRange>,
  following_pos: SourcePos,
  context: &mut Context,
) -> Option<SourceRange> {
  let close_token_end = {
    let close_paren = if let Some(last_param) = &last_param {
      context.token_finder.get_first_close_paren_after(last_param)
    } else {
      context.token_finder.get_first_close_paren_before(&following_pos)
    };
    if let Some(close_paren) = close_paren {
      let end = close_paren.end();
      if end > start_pos { Some(end) } else { None }
    } else {
      None
    }
  }?;
  let close_token_start = {
    let open_paren = context.token_finder.get_first_open_paren_before(&{
      if let Some(first_param) = first_param {
        first_param.start()
      } else {
        close_token_end
      }
    });

    if let Some(open_paren) = open_paren {
      let pos = open_paren.start();
      if pos >= start_pos { Some(pos) } else { None }
    } else {
      None
    }
  }?;

  Some(SourceRange::new(close_token_start, close_token_end))
}
