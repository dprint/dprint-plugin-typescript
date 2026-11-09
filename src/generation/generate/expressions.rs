use super::*;

/* expressions */

pub(super) fn gen_array_expr<'a>(node: &'a ArrayExpression<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let prefer_hanging = match context.config.array_expression_prefer_hanging {
    PreferHanging::Never => false,
    PreferHanging::OnlySingleItem => node.elements.len() == 1,
    PreferHanging::Always => true,
  };
  gen_array_like_nodes(
    GenArrayLikeNodesOptions {
      node: node.into_node(),
      range: node.range(),
      // holes are elisions in oxc, but they're generated as separators
      nodes: node
        .elements
        .iter()
        .map(|elem| match elem {
          ArrayExpressionElement::Elision(_) => None,
          elem => Some(elem.into_node()),
        })
        .collect(),
      prefer_hanging,
      prefer_single_line: context.config.array_expression_prefer_single_line,
      trailing_commas: context.config.array_expression_trailing_commas,
      space_around: context.config.array_expression_space_around,
    },
    context,
  )
}

pub(super) fn gen_arrow_func_expr<'a>(node: &'a ArrowFunctionExpression<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let items = gen_inner(node, context);
  return if should_add_parens_around_expr(node.into_node(), context) {
    surround_with_parens(items)
  } else {
    items
  };

  fn gen_inner<'a>(node: &'a ArrowFunctionExpression<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
    if let Some(node) = get_curried_arrow_expr(node) {
      // handle arrow functions that are curried differently
      gen_curried_arrow(node, context)
    } else {
      gen_inner_arrow(node, context)
    }
  }

  fn gen_curried_arrow<'a>(node: CurriedArrowExpr<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
    let assignment_end_ln = match context.assignment_curried_arrow_end_ln {
      Some((start, end_ln)) if start == node.signatures[0].inner.start() => context.assignment_curried_arrow_end_ln.take().map(|_| end_ln),
      _ => None,
    };
    let mut last_header_start_lsil = None;
    let mut force_use_new_lines = false;
    let last_arrow = node.last_arrow();
    let mut lines = Vec::with_capacity(node.signatures.len());
    for item in node.signatures.into_iter() {
      let range = item.range(context);
      let comment_items = gen_leading_comments_on_previous_lines(&range, context);
      if !comment_items.is_empty() {
        force_use_new_lines = true;
        lines.push(comment_items);
      }

      let (header_start_lsil, signature_items) = gen_arrow_signature(&item, context);
      last_header_start_lsil = Some(header_start_lsil);
      let mut items = signature_items;
      items.extend(gen_trailing_comments(&range, context));

      lines.push(items);
    }

    let mut items = PrintItems::new(context.arena);
    if force_use_new_lines {
      for (i, line) in lines.into_iter().enumerate() {
        if i > 0 {
          items.push_signal(Signal::NewLine);
        }
        items.extend(line);
      }
    } else {
      let start_ln = LineNumber::new("startCurry");
      let end_ln = LineNumber::new("endCurry");
      items.push_info(start_ln);
      items.extend(gen_line_numbers_region_start(context.arena, &[end_ln]));
      let lines = lines.into_iter().map(|l| l.into_rc_path()).collect::<Vec<_>>();
      items.push_condition(if_true_or(
        "multipleLinesCurrying",
        context
          .arena
          .alloc_resolver(move |context| condition_helpers::is_multiple_lines(context, start_ln, end_ln)),
        {
          let mut items = PrintItems::new(context.arena);
          for (i, line) in lines.clone().into_iter().enumerate() {
            if i > 0 {
              items.push_signal(Signal::NewLine);
            }
            items.push_optional_path(line);
          }
          items
        },
        {
          let mut items = PrintItems::new(context.arena);
          for (i, line) in lines.into_iter().enumerate() {
            if i > 0 {
              items.push_signal(Signal::SpaceOrNewLine);
            }
            items.push_optional_path(line);
          }
          items
        },
      ));
      items.push_info(end_ln);
    }
    if let Some(assignment_end_ln) = assignment_end_ln {
      items.push_info(assignment_end_ln);
    }

    let mut items = ir_helpers::new_line_group(items);
    items.extend(gen_arrow_body(last_arrow, last_header_start_lsil.unwrap(), context));
    items
  }

  fn gen_inner_arrow<'a>(node: &'a ArrowFunctionExpression<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
    let (header_start_lsil, header_items) = gen_arrow_signature(&ArrowSignature::new(node), context);

    // there's no wrapper node around arguments in oxc, so the parent is the call expression
    let is_arrow_in_test_call_expr = match node.into_node().parent(context.program) {
      Some(Node::CallExpression(call_expr)) => !is_optional_chain_call_expr(call_expr) && node_helpers::is_test_library_call_expr(call_expr, context.program),
      _ => false,
    };
    let mut items = if is_arrow_in_test_call_expr {
      ir_helpers::with_no_new_lines(header_items)
    } else if is_hugged_last_arg_arrow(node, context) {
      // the arguments aren't in a new line group so the body can break inside its parens, but
      // the arguments should still break before the header rather than inside its parameters
      ir_helpers::new_line_group(header_items)
    } else {
      header_items
    };

    items.extend(gen_arrow_body(node, header_start_lsil, context));

    items
  }

  fn gen_arrow_body<'a>(node: &'a ArrowFunctionExpression<'a>, header_start_lsil: LineStartIndentLevel, context: &mut Context<'a>) -> PrintItems<'a> {
    let mut items = PrintItems::new(context.arena);
    let generated_body = gen_node((&node.body).into_node(), context);
    let generated_body = if use_new_line_group_for_arrow_body(node, context) {
      new_line_group(generated_body)
    } else {
      generated_body
    }
    .into_rc_path();
    let open_brace_token = match &node.body {
      ArrowFunctionBody::FunctionBody(body) => context.token_finder.get_first_open_brace_token_within(body),
      _ => None,
    };

    if open_brace_token.is_some() {
      items.extend(gen_brace_separator(
        GenBraceSeparatorOptions {
          brace_position: context.config.arrow_function_brace_position,
          open_brace_token,
          start_header_lsil: Some(header_start_lsil),
        },
        context,
      ));

      items.extend(generated_body.into_print_items(context.arena));
    } else {
      let start_body_ln = LineNumber::new("startBody");
      let end_body_ln = LineNumber::new("endBody");
      items.push_info(start_body_ln);

      if is_hugging_call_body(&node.body, context) {
        // stays on the same line unless the text before the last argument doesn't fit
        items.push_signal(Signal::SpaceOrNewLine);
      } else if should_not_newline_after_arrow(&node.body, context) {
        items.push_space();
      } else {
        // a call that hugs its last argument moves to the next line when it doesn't fit on the arrow's line,
        // so it must be allowed to stay there when formatting again
        let short_header_separator = if is_call_hugging_last_arg(&node.body, context) {
          Signal::SpaceOrNewLine.into_print_items(context.arena)
        } else {
          " ".into_print_items(context.arena)
        };
        items.extend(gen_line_numbers_region_start(context.arena, &[end_body_ln]));
        items.push_condition(conditions::if_above_width_or(
          context.config.indent_width,
          if_true_or(
            "newlineOrSpace",
            context
              .arena
              .alloc_resolver(move |context| condition_helpers::is_multiple_lines(context, start_body_ln, end_body_ln)),
            Signal::NewLine.into_print_items(context.arena),
            Signal::SpaceOrNewLine.into_print_items(context.arena),
          )
          .into_print_items(context.arena),
          short_header_separator,
        ));
      }

      items.push_condition(conditions::indent_if_start_of_line(generated_body.into_print_items(context.arena)));
      items.push_info(end_body_ln);
    }
    items
  }

  fn gen_arrow_signature<'a>(node: &ArrowSignature<'a>, context: &mut Context<'a>) -> (LineStartIndentLevel, PrintItems<'a>) {
    let header_start_lsil = LineStartIndentLevel::new("arrowFunctionExpressionHeaderStart");
    let mut items = PrintItems::new(context.arena);
    let should_use_parens = get_should_use_parens(node, context);

    items.push_info(header_start_lsil);
    if node.is_async() {
      items.push_sc(sc!("async "));
    }
    if let Some(type_params) = node.type_params() {
      items.extend(gen_node(type_params.into_node(), context));
    }

    if should_use_parens {
      // need to check if there are parens because gen_parameters_or_arguments depends on the parens existing
      if has_parens(node, context.program) {
        let param_count = get_param_count(node.params());
        items.extend(gen_parameters_or_arguments(
          GenParametersOrArgumentsOptions {
            node: node.inner.into_node(),
            range: node.inner.get_parameters_range(context),
            nodes: get_param_nodes(node.params()).collect_in(context.arena.bump()),
            custom_close_paren: |context| {
              Some(gen_close_paren_with_type(
                GenCloseParenWithTypeOptions {
                  start_lsil: header_start_lsil,
                  type_node: node.return_type().map(|x| x.into_node()),
                  type_node_separator: None,
                  param_count,
                },
                context,
              ))
            },
            is_parameters: true,
          },
          context,
        ));
      } else {
        // todo: this should probably use more of the same logic as in gen_parameters_or_arguments
        // there will only be one param in this case
        items.extend(surround_with_parens(gen_node(get_param_nodes(node.params()).next().unwrap(), context)));
      }
    } else {
      items.extend(gen_node(get_param_nodes(node.params()).next().unwrap(), context));
    }

    items.push_sc(sc!(" =>"));
    (header_start_lsil, items)
  }

  fn should_not_newline_after_arrow<'a>(body: &'a ArrowFunctionBody<'a>, context: &Context<'a>) -> bool {
    return match body {
      ArrowFunctionBody::FunctionBody(_) => true,
      body => {
        let expr = body.as_expression().unwrap();
        match expr {
          // removed parens are formatted like the jsx without them
          Expression::ParenthesizedExpression(paren) => !is_jsx_paren_expr_removed(paren, context),
          Expression::ArrayExpression(_) => true,
          // the text of a template literal isn't indented when it moves to the next line
          Expression::TemplateLiteral(tpl) => is_multi_line_tpl(tpl),
          Expression::TaggedTemplateExpression(tagged_tpl) => is_multi_line_tpl(&tagged_tpl.quasi),
          _ => is_jsx_paren_expr_handled_node(expr.into_node(), context),
        }
      }
    };

    fn is_multi_line_tpl(tpl: &TemplateLiteral) -> bool {
      tpl.quasis.iter().any(|quasi| quasi.value.raw.as_str().contains('\n'))
    }
  }

  /// Gets if the body is a call on the same line as the arrow with its last argument starting on that
  /// line and continuing on the next ones (ex. `a => b({\n  c,\n})`), which keeps its line breaks.
  fn is_hugging_call_body<'a>(body: &'a ArrowFunctionBody<'a>, context: &Context<'a>) -> bool {
    let Some(expr) = body.as_expression() else {
      return false;
    };
    let program = context.program;
    let start_line = expr.start_line_fast(program);
    let is_on_arrow_line = context
      .token_finder
      .get_previous_token(expr)
      .is_some_and(|arrow_token| arrow_token.end_line_fast(program) == start_line);
    is_on_arrow_line && is_call_hugging_last_arg(body, context)
  }

  /// Gets if the body is a call with its last argument starting on the call's first line and continuing
  /// on the next ones (ex. `b({\n  c,\n})`).
  fn is_call_hugging_last_arg<'a>(body: &'a ArrowFunctionBody<'a>, context: &Context<'a>) -> bool {
    let Some(expr) = body.as_expression() else {
      return false;
    };
    // a `new` expression without parens has no arguments
    let (last_arg, last_arg_expr, arg_count) = match expr {
      Expression::CallExpression(call_expr) => match get_last_arg(&call_expr.arguments) {
        Some((last_arg, last_arg_expr)) => (last_arg, last_arg_expr, call_expr.arguments.len()),
        None => return false,
      },
      Expression::NewExpression(new_expr) => match get_last_arg(&new_expr.arguments) {
        Some((last_arg, last_arg_expr)) => (last_arg, last_arg_expr, new_expr.arguments.len()),
        None => return false,
      },
      // swc had `import(...)` as a call
      Expression::ImportExpression(import_expr) => match &import_expr.options {
        Some(options) => (options.into_node(), options.into_node(), 2),
        None => ((&import_expr.source).into_node(), (&import_expr.source).into_node(), 1),
      },
      _ => return false,
    };
    let program = context.program;
    let start_line = expr.start_line_fast(program);
    return last_arg.start_line_fast(program) == start_line
      && last_arg.end_line_fast(program) > start_line
      && allows_inline_multi_line(last_arg_expr, context, arg_count > 1);

    /// Gets the last argument along with its expression, which is what swc unwrapped its
    /// argument nodes to, including for spreads.
    fn get_last_arg<'a>(args: &'a [Argument<'a>]) -> Option<(Node<'a>, Node<'a>)> {
      let last_arg = args.last()?;
      let last_arg_expr = match last_arg {
        Argument::SpreadElement(spread) => (&spread.argument).into_node(),
        last_arg => last_arg.into_node(),
      };
      Some((last_arg.into_node(), last_arg_expr))
    }
  }

  fn get_should_use_parens<'a>(node: &ArrowSignature<'a>, context: &Context<'a>) -> bool {
    return match context.config.arrow_function_use_parentheses {
      UseParentheses::Force => true,
      UseParentheses::PreferNone => {
        get_param_count(node.params()) != 1
          || node.return_type().is_some()
          || is_first_param_not_identifier_or_has_type_annotation(node.params(), node, context.program)
      }
      UseParentheses::Maintain => has_parens(node, context.program),
    };

    fn is_first_param_not_identifier_or_has_type_annotation<'a>(
      params: &'a FormalParameters<'a>,
      arrow: &ArrowSignature<'a>,
      program: ProgramInfo<'a>,
    ) -> bool {
      // a rest parameter is never an identifier
      match params.items.first() {
        // a default value (ex. `(a = 1) =>`) was an assignment pattern in swc
        Some(param) if matches!(param.pattern, BindingPattern::BindingIdentifier(_)) && param.initializer.is_none() => {
          param.type_annotation.is_some() || param.optional || param_has_surrounding_comments(param.into_node(), program) && has_parens(arrow, program)
        }
        _ => true,
      }
    }

    fn param_has_surrounding_comments<'a>(param: Node<'a>, program: ProgramInfo<'a>) -> bool {
      if node_helpers::has_surrounding_comments(param, program) {
        true
      } else if let Some(comma_token) = param.next_token_fast(program).filter(|t| t.kind == TokenKind::Comma) {
        !comma_token.trailing_comments_fast(program).is_empty()
      } else {
        false
      }
    }
  }

  fn has_parens<'a>(node: &ArrowSignature<'a>, program: ProgramInfo<'a>) -> bool {
    let params = node.params();
    if get_param_count(params) != 1 {
      true
    } else {
      // the parameters' range includes the parens when they exist
      params.text_fast(program).starts_with('(')
    }
  }

  fn get_param_nodes<'a>(params: &'a FormalParameters<'a>) -> impl Iterator<Item = Node<'a>> {
    params
      .items
      .iter()
      .map(|param| param.into_node())
      .chain(params.rest.iter().map(|rest| rest.into_node()))
  }

  fn get_param_count(params: &FormalParameters) -> usize {
    params.items.len() + usize::from(params.rest.is_some())
  }
}

pub(super) fn gen_as_expr<'a>(node: &'a TSAsExpression<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  // `as const` is an as expression with a `const` type reference in oxc
  gen_as_expr_like(
    AsExprLike {
      expr: (&node.expression).into_node(),
      type_ann: (&node.type_annotation).into_node(),
    },
    context,
  )
}

pub(super) struct AsExprLike<'a> {
  pub(super) expr: Node<'a>,
  pub(super) type_ann: Node<'a>,
}

pub(super) fn gen_as_expr_like<'a>(node: AsExprLike<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = gen_node(node.expr, context);
  // todo: remove this condition once https://github.com/swc-project/swc/issues/6238 is fixed
  let is_as = node
    .expr
    .next_token_fast(context.program)
    .map(|t| t.text_fast(context.program) == "as")
    .unwrap_or_default();
  if is_as {
    items.push_sc(sc!(" as"));
  } else {
    items.push_sc(sc!(" satisfies"));
  }
  items.push_signal(Signal::SpaceIfNotTrailing);
  items.push_condition(conditions::with_indent_if_start_of_line_indented(gen_node(node.type_ann, context)));
  items
}

pub(super) fn gen_satisfies_expr<'a>(node: &'a TSSatisfiesExpression<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = gen_node((&node.expression).into_node(), context);
  items.push_sc(sc!(" satisfies"));
  items.push_signal(Signal::SpaceIfNotTrailing);
  items.push_condition(conditions::with_indent_if_start_of_line_indented(gen_node(
    (&node.type_annotation).into_node(),
    context,
  )));
  items
}

pub(super) fn gen_assignment_expr<'a>(node: &'a AssignmentExpression<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  // check for a nested assignment (ex. `a = b = c`)
  if node.operator == AssignmentOperator::Assign
    && let Expression::AssignmentExpression(right) = &node.right
    && right.operator == AssignmentOperator::Assign
  {
    let flattened_items = get_flattened_assign_expr(node, context);
    // if the number of assignments chained is excessive, then it's probably minified
    // so force using newlines to optimize formatting
    let force_use_new_lines =
      flattened_items.len() > 4 || node_helpers::get_use_new_lines_for_nodes(&flattened_items[0].expr, &flattened_items.last().unwrap().expr, context.program);
    let indent_width = context.config.indent_width;
    let arena = context.arena;
    return ir_helpers::gen_separated_values(
      |_| {
        let mut generated_nodes = BumpVec::with_capacity_in(flattened_items.len(), context.arena.bump());
        for item in flattened_items.into_iter() {
          let lines_span = Some(ir_helpers::LinesSpan {
            start_line: item.expr.range().start_line_fast(context.program),
            end_line: item.expr.range().end_line_fast(context.program),
          });
          let mut items = if item.post_op.is_some() {
            gen_assignment_target(item.expr, context)
          } else {
            gen_node(item.expr, context)
          };
          if let Some(op) = item.post_op {
            items.push_sc(sc!(" ="));
            items.extend(gen_trailing_comments(&op.range(), context));
          } else {
            items = indent_if_start_of_line(items).into_print_items(context.arena);
          }

          generated_nodes.push(ir_helpers::GeneratedValue {
            items,
            lines_span,
            allow_inline_multi_line: true,
            allow_inline_single_line: true,
            is_known_multi_line: false,
          });
        }

        generated_nodes
      },
      ir_helpers::GenSeparatedValuesOptions {
        prefer_hanging: false,
        force_use_new_lines,
        max_blank_lines: 0,
        indent_width,
        single_line_options: ir_helpers::SingleLineOptions::separated_same_line(Signal::SpaceOrNewLine.into_print_items(arena)),
        multi_line_options: ir_helpers::MultiLineOptions::same_line_start_hanging_indent(),
        force_possible_newline_at_start: false,
      },
    )
    .items;
  }

  // parse a single assignment (ex. `a = b` and not `a = b = c`)
  let mut items = PrintItems::new(context.arena);
  items.extend(gen_assignment_target((&node.left).into_node(), context));
  items.extend(gen_assignment((&node.right).into_node(), assign_op_sc(node.operator), context));
  items
}

pub(super) fn gen_await_expr<'a>(node: &'a AwaitExpression<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  items.push_sc(sc!("await "));
  items.extend(gen_node((&node.argument).into_node(), context));
  items
}

pub(super) fn gen_binary_expr<'a>(node: BinExpr<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let flattened_binary_expr = get_flattened_bin_expr(node, context);
  gen_flattened_binary_expr(node.start(), node.op(), node.parent(context.program), flattened_binary_expr, context)
}

pub(super) fn gen_flattened_binary_expr<'a>(
  node_start: SourcePos,
  node_op: BinaryOp,
  node_parent: Node<'a>,
  flattened_binary_expr: BumpVec<'a, BinExprItem<'a>>,
  context: &mut Context<'a>,
) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  // println!("Bin expr: {:?}", flattened_binary_expr.iter().map(|x| x.expr.text(context)).collect::<Vec<_>>());
  let line_per_expression = context.config.binary_expression_line_per_expression;
  let force_use_new_lines = !context.config.binary_expression_prefer_single_line
    && node_helpers::get_use_new_lines_for_nodes(
      &flattened_binary_expr[0].expr,
      if line_per_expression {
        &flattened_binary_expr[1].expr
      } else {
        &flattened_binary_expr.last().unwrap().expr
      },
      context.program,
    );
  // A comment on its own line before an operand can't be kept when the operands are on the same
  // line, so that operand makes the expression multi-line. This isn't forced from the start like
  // above so it doesn't change the layout of an expression that's multi-line anyway.
  let has_own_line_comment_before_operands = get_has_own_line_comment_before_operands(&flattened_binary_expr, context.program);
  let indent_width = context.config.indent_width;
  let arena = context.arena;
  let binary_expr_start_il = IndentLevel::new("binExprStart");
  let binary_expr_start_isol = IsStartOfLine::new("binExprStart");
  let allow_no_indent = get_allow_no_indent(node_op, node_parent, context.program);
  let use_space_surrounding_operator = get_use_space_surrounding_operator(&node_op, context);
  let is_parent_bin_expr = is_bin_expr_node(node_parent);
  // a binary expression nested in a parent with a different precedence indents from its first
  // operand on a new line, so a multi-line operand before that isn't indented
  let nested_extra_indent_ref = match context.nested_bin_expr_extra_indent {
    Some((start, extra_indent_ref)) if start == node_start => context.nested_bin_expr_extra_indent.take().map(|_| extra_indent_ref),
    _ => None,
  };
  let mut nested_operand_isols = Vec::new();
  let operand_count = flattened_binary_expr.len();
  let multi_line_options = {
    let mut options = if line_per_expression {
      ir_helpers::MultiLineOptions::same_line_no_indent()
    } else {
      ir_helpers::MultiLineOptions::maintain_line_breaks()
    };
    options.with_hanging_indent = if is_parent_bin_expr {
      BoolOrCondition::Bool(false) // let the parent handle the indent
    } else {
      BoolOrCondition::Condition(context.arena.alloc_resolver(move |condition_context| {
        if allow_no_indent && condition_context.resolved_is_start_of_line(binary_expr_start_isol)? {
          return Some(false);
        }
        Some(condition_context.writer_info.is_start_of_line())
      }))
    };
    options
  };

  items.push_info(binary_expr_start_il);
  items.push_info(binary_expr_start_isol);

  items.extend(
    ir_helpers::gen_separated_values(
      |_| {
        let mut generated_nodes = BumpVec::with_capacity_in(flattened_binary_expr.len(), context.arena.bump());
        let mut bin_expr_items_iter = flattened_binary_expr
          .into_iter()
          .zip(has_own_line_comment_before_operands)
          .enumerate()
          .peekable();
        while let Some((i, (bin_expr_item, has_own_line_comment_before))) = bin_expr_items_iter.next() {
          let lines_span = Some(ir_helpers::LinesSpan {
            start_line: bin_expr_item.expr.start_line_fast(context.program),
            end_line: bin_expr_item.expr.end_line_fast(context.program),
          });
          let mut items = PrintItems::new(context.arena);
          if let Some(extra_indent_ref) = nested_extra_indent_ref
            && i > 0
          {
            items.extend(gen_cancel_nested_operand_indent(&mut nested_operand_isols, extra_indent_ref, context));
          }

          let pre_op = bin_expr_item.pre_op;
          let post_op = bin_expr_item.post_op;
          let (leading_pre_op_comments, trailing_pre_op_comments) = if let Some(op) = &pre_op {
            (
              gen_leading_comments_same_line(&op.token.range(), context),
              gen_trailing_comments_same_line(&op.token.range(), context),
            )
          } else {
            (PrintItems::new(context.arena), PrintItems::new(context.arena))
          };
          let inner_bin_expr_extra_indent = if is_bin_expr_node(bin_expr_item.expr) {
            let mut condition = get_inner_bin_expr_extra_indent(allow_no_indent, binary_expr_start_il, binary_expr_start_isol, context);
            context.nested_bin_expr_extra_indent = Some((bin_expr_item.expr.start(), condition.create_reference()));
            Some(condition)
          } else {
            None
          };
          items.extend(gen_node_with_inner_gen(bin_expr_item.expr, context, |node_items, context| {
            let mut items = PrintItems::new(context.arena);
            if let Some(op) = pre_op {
              if !leading_pre_op_comments.is_empty() {
                items.extend(leading_pre_op_comments);
              }
              items.push_sc(binary_op_sc(op.op));
              if trailing_pre_op_comments.is_empty() {
                if use_space_surrounding_operator {
                  items.push_space();
                }
              } else {
                items.extend(trailing_pre_op_comments);
                items.push_space();
              }
            }

            if let Some(condition) = inner_bin_expr_extra_indent {
              items.push_condition(condition);
            }
            items.extend(node_items);

            if let Some(op) = post_op {
              let leading_post_op_comments = gen_leading_comments_same_line(&op.token.range(), context);
              let trailing_post_op_comments = gen_trailing_comments_same_line(&op.token.range(), context);
              if leading_post_op_comments.is_empty() {
                if use_space_surrounding_operator {
                  items.push_space();
                }
              } else {
                items.push_space();
                items.extend(leading_post_op_comments);
              }
              items.push_sc(binary_op_sc(op.op));
              if !trailing_post_op_comments.is_empty() {
                items.extend(trailing_post_op_comments);
              }
            } else if let Some((_, (next_item, _))) = bin_expr_items_iter.peek() {
              // put any line comments after the next pre-op on this line instead
              if let Some(pre_op) = &next_item.pre_op {
                items.extend(gen_trailing_comments_if_line_comment_same_line(&pre_op.token.range(), context));
              }
            }

            items
          }));

          if let Some(extra_indent_ref) = nested_extra_indent_ref
            && i + 1 < operand_count
          {
            items.extend(gen_start_nested_operand_indent(&nested_operand_isols, extra_indent_ref, context));
          }

          let items = if should_newline_group_bin_item_expr(bin_expr_item.expr, context) {
            ir_helpers::new_line_group(items)
          } else {
            items
          };

          generated_nodes.push(ir_helpers::GeneratedValue {
            items,
            lines_span,
            allow_inline_multi_line: !has_own_line_comment_before,
            allow_inline_single_line: true,
            is_known_multi_line: false,
          });
        }

        generated_nodes
      },
      ir_helpers::GenSeparatedValuesOptions {
        prefer_hanging: false,
        force_use_new_lines,
        max_blank_lines: 0,
        indent_width,
        single_line_options: ir_helpers::SingleLineOptions::separated_same_line(if use_space_surrounding_operator {
          Signal::SpaceOrNewLine.into_print_items(arena)
        } else {
          PrintItems::new(arena)
        }),
        multi_line_options,
        force_possible_newline_at_start: false,
      },
    )
    .items,
  );

  if let Some(extra_indent_ref) = nested_extra_indent_ref
    && !nested_operand_isols.is_empty()
  {
    items.extend(gen_finish_nested_operand_indent(&nested_operand_isols, extra_indent_ref, context));
  }

  return if node_op.is_equality() { ir_helpers::new_line_group(items) } else { items };

  /// Gets, for each operand, if a comment between it and the previous operand is on its own line
  /// (ex. `// c` in `a\n// c\n|| b`).
  fn get_has_own_line_comment_before_operands<'a>(items: &[BinExprItem<'a>], program: ProgramInfo<'a>) -> Vec<bool> {
    let operand_pairs = items.windows(2).map(|pair| has_own_line_comment_between_operands(&pair[0], &pair[1], program));
    std::iter::once(false).chain(operand_pairs).collect()
  }

  fn has_own_line_comment_between_operands<'a>(previous: &BinExprItem<'a>, item: &BinExprItem<'a>, program: ProgramInfo<'a>) -> bool {
    let Some(op) = item.pre_op.as_ref().or(previous.post_op.as_ref()).map(|op| op.token) else {
      return false;
    };
    let comments_before_op = previous
      .expr
      .end()
      .trailing_comments_fast(program)
      .chain(op.start().leading_comments_fast(program));
    let comments_after_op = op.end().trailing_comments_fast(program).chain(item.expr.start().leading_comments_fast(program));
    let (op_start_line, op_end_line) = (op.start_line_fast(program), op.end_line_fast(program));
    has_own_line_comment(previous.expr.end_line_fast(program), comments_before_op, op_start_line, program)
      || has_own_line_comment(op_end_line, comments_after_op, item.expr.start_line_fast(program), program)
  }

  /// Gets if any of the comments, which are between a token ending on `last_end_line` and one
  /// starting on `next_start_line`, has nothing else on its lines.
  fn has_own_line_comment<'a>(
    mut last_end_line: usize,
    comments: impl Iterator<Item = &'a Comment<'a>>,
    next_start_line: usize,
    program: ProgramInfo<'a>,
  ) -> bool {
    // a comment can be both a trailing comment of one token and a leading comment of the next
    let mut last_comment_end = None;
    let mut comments = comments
      .filter(|c| {
        let is_new = last_comment_end.is_none_or(|end| c.start() >= end);
        if is_new {
          last_comment_end = Some(c.end());
        }
        is_new
      })
      .peekable();
    while let Some(comment) = comments.next() {
      let comment_end_line = comment.end_line_fast(program);
      let next_start_line = comments.peek().map(|c| c.start_line_fast(program)).unwrap_or(next_start_line);
      if comment.start_line_fast(program) > last_end_line && next_start_line > comment_end_line {
        return true;
      }
      last_end_line = comment_end_line;
    }
    false
  }

  /// Gets the condition for whether an inner binary expression with a different precedence
  /// indents its operands an extra level, which it does when this binary expression isn't
  /// already hanging.
  fn get_inner_bin_expr_extra_indent<'a>(
    allow_no_indent: bool,
    binary_expr_start_il: IndentLevel,
    binary_expr_start_isol: IsStartOfLine,
    context: &Context<'a>,
  ) -> Condition<'a> {
    Condition::new(
      "innerBinExprExtraIndent",
      ConditionProperties {
        condition: context.arena.alloc_resolver(move |context| {
          if allow_no_indent && context.resolved_is_start_of_line(binary_expr_start_isol)? {
            return Some(false);
          }
          let binary_expr_start_il = context.resolved_indent_level(binary_expr_start_il)?;
          let is_hanging = binary_expr_start_il < context.writer_info.indent_level;
          Some(!is_hanging)
        }),
        true_path: None,
        false_path: None,
      },
    )
  }

  /// Starts the indent of a nested binary expression's operands at the end of an operand
  /// when none of the operands after the first are on a new line yet. It's started before
  /// the separator so the line of the next operand is indented when it goes on a new line.
  fn gen_start_nested_operand_indent<'a>(operand_isols: &[IsStartOfLine], extra_indent_ref: ConditionReference, context: &Context<'a>) -> PrintItems<'a> {
    let operand_isols: &[IsStartOfLine] = context.arena.bump().alloc_slice_copy(operand_isols);
    if_true(
      "startNestedBinExprIndent",
      context.arena.alloc_resolver(move |context| is_none_start_of_line(context, operand_isols)),
      gen_nested_operand_indent_signals(Signal::StartIndent, extra_indent_ref, context),
    )
    .into_print_items(context.arena)
  }

  /// Cancels the indent started at the end of the previous operand when this operand
  /// stays on the same line.
  fn gen_cancel_nested_operand_indent<'a>(
    operand_isols: &mut Vec<IsStartOfLine>,
    extra_indent_ref: ConditionReference,
    context: &Context<'a>,
  ) -> PrintItems<'a> {
    let isol = IsStartOfLine::new("nestedBinExprOperand");
    let previous_isols: &[IsStartOfLine] = context.arena.bump().alloc_slice_copy(operand_isols);
    operand_isols.push(isol);
    let mut items = PrintItems::new(context.arena);
    items.push_info(isol);
    items.push_condition(if_true(
      "cancelNestedBinExprIndent",
      context.arena.alloc_resolver(move |context| {
        let is_start_of_line = context.resolved_is_start_of_line(isol)?;
        Some(!is_start_of_line && is_none_start_of_line(context, previous_isols)?)
      }),
      gen_nested_operand_indent_signals(Signal::FinishIndent, extra_indent_ref, context),
    ));
    items
  }

  fn gen_finish_nested_operand_indent<'a>(operand_isols: &[IsStartOfLine], extra_indent_ref: ConditionReference, context: &Context<'a>) -> PrintItems<'a> {
    let operand_isols: &[IsStartOfLine] = context.arena.bump().alloc_slice_copy(operand_isols);
    if_true(
      "finishNestedBinExprIndent",
      context
        .arena
        .alloc_resolver(move |context| Some(!is_none_start_of_line(context, operand_isols)?)),
      gen_nested_operand_indent_signals(Signal::FinishIndent, extra_indent_ref, context),
    )
    .into_print_items(context.arena)
  }

  fn gen_nested_operand_indent_signals<'a>(signal: Signal, extra_indent_ref: ConditionReference, context: &Context<'a>) -> PrintItems<'a> {
    let mut items = PrintItems::new(context.arena);
    items.push_signal(signal);
    items.push_condition(if_true(
      "nestedBinExprExtraIndent",
      context.arena.alloc_resolver(move |context| context.resolved_condition(&extra_indent_ref)),
      signal.into_print_items(context.arena),
    ));
    items
  }

  fn is_none_start_of_line(context: &mut ConditionResolverContext, isols: &[IsStartOfLine]) -> Option<bool> {
    for isol in isols {
      if context.resolved_is_start_of_line(*isol)? {
        return Some(false);
      }
    }
    Some(true)
  }

  fn get_allow_no_indent<'a>(op: BinaryOp, parent: Node<'a>, program: ProgramInfo<'a>) -> bool {
    if !op.is_add_sub()
      && !op.is_mul_div()
      && !op.is_logical()
      && !op.is_bit_logical()
      && !op.is_bit_shift()
      && !op.is_equality()
      && op != BinaryOp::Binary(BinaryOperator::Remainder)
    {
      false
    } else if parent.kind() == NodeKind::ExpressionStatement || is_bin_expr_node(parent) {
      false
    } else {
      // get if in an argument (there's no wrapper node around arguments in oxc)
      match parent {
        Node::CallExpression(_) | Node::NewExpression(_) | Node::ImportExpression(_) => false,
        Node::SpreadElement(_) => !matches!(
          parent.parent(program).map(|p| p.kind()),
          Some(NodeKind::CallExpression | NodeKind::NewExpression | NodeKind::ImportExpression)
        ),
        _ => true,
      }
    }
  }

  fn get_use_space_surrounding_operator<'a>(op: &BinaryOp, context: &Context<'a>) -> bool {
    if op.is_bitwise_or_arithmetic() {
      context.config.binary_expression_space_surrounding_bitwise_and_arithmetic_operator
    } else {
      true
    }
  }

  fn should_newline_group_bin_item_expr<'a>(node: Node<'a>, context: &Context<'a>) -> bool {
    if let Node::ParenthesizedExpression(node) = node {
      return should_newline_group_bin_item_expr((&node.expression).into_node(), context);
    }

    if is_jsx_paren_expr_handled_node(node, context) {
      // prefer using the possible newline at the start of the element
      return false;
    }

    true
  }
}

pub(super) fn gen_call_or_opt_expr<'a>(node: &'a CallExpression<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  // swc only did this for calls that weren't in an optional chain
  if !is_optional_chain_call_expr(node) && node_helpers::is_test_library_call_expr(node, context.program) {
    return gen_test_library_call_expr(node, context);
  }

  // flatten the call expression and check if it should be generated as a flattened member like expression
  let flattened_call_expr = flatten_member_like_expr(node.into_node(), context.program, context.arena.bump());
  return if flattened_call_expr.nodes.len() > 1 {
    gen_for_flattened_member_like_expr(flattened_call_expr, context)
  } else {
    gen_call_expr_like(
      CallExprLike {
        original_call_expr: node,
        generated_callee: gen_node((&node.callee).into_node(), context),
      },
      context,
    )
  };

  fn gen_test_library_call_expr<'a>(node: &'a CallExpression<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
    let mut items = PrintItems::new(context.arena);
    items.extend(gen_test_library_callee(&node.callee, context));
    items.extend(gen_test_library_arguments(&node.arguments, context));
    return items;

    fn gen_test_library_callee<'a>(callee: &'a Expression<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
      match callee {
        Expression::StaticMemberExpression(member_expr) => {
          let mut items = PrintItems::new(context.arena);
          items.extend(gen_node((&member_expr.object).into_node(), context));
          items.push_sc(sc!("."));
          items.extend(gen_node((&member_expr.property).into_node(), context));
          items
        }
        _ => gen_node(callee.into_node(), context),
      }
    }

    fn gen_test_library_arguments<'a>(args: &'a [Argument<'a>], context: &mut Context<'a>) -> PrintItems<'a> {
      let mut items = PrintItems::new(context.arena);
      items.extend(gen_node_with_inner_gen((&args[0]).into_node(), context, |items, _| {
        let mut new_items = ir_helpers::with_no_new_lines(items);
        new_items.push_sc(sc!(","));
        new_items
      }));
      items.push_space();
      items.extend(gen_node((&args[1]).into_node(), context));

      surround_with_parens(items)
    }
  }
}

/// Gets if the call expression is in an optional chain (ex. `a?.()` or `a?.b()`),
/// which swc represented as a separate node from a call expression.
pub(super) fn is_optional_chain_call_expr(node: &CallExpression) -> bool {
  if node.optional {
    return true;
  }
  let mut expr = &node.callee;
  loop {
    match expr {
      Expression::StaticMemberExpression(member) => {
        if member.optional {
          return true;
        }
        expr = &member.object;
      }
      Expression::ComputedMemberExpression(member) => {
        if member.optional {
          return true;
        }
        expr = &member.object;
      }
      Expression::PrivateFieldExpression(member) => {
        if member.optional {
          return true;
        }
        expr = &member.object;
      }
      Expression::CallExpression(call_expr) => {
        if call_expr.optional {
          return true;
        }
        expr = &call_expr.callee;
      }
      Expression::TSNonNullExpression(non_null) => {
        expr = &non_null.expression;
      }
      _ => return false,
    }
  }
}

pub(super) struct CallExprLike<'a> {
  pub(super) original_call_expr: &'a CallExpression<'a>,
  pub(super) generated_callee: PrintItems<'a>,
}

pub(super) fn gen_call_expr_like<'a>(node: CallExprLike<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  let call_expr = node.original_call_expr;

  items.extend(node.generated_callee);

  if call_expr.optional {
    items.push_sc(sc!("?."));
  }

  if let Some(type_args) = &call_expr.type_arguments {
    items.extend(gen_node(type_args.into_node(), context));
  }

  items.push_condition(conditions::with_indent_if_start_of_line_indented(gen_parameters_or_arguments(
    GenParametersOrArgumentsOptions {
      node: call_expr.into_node(),
      range: call_expr.get_parameters_range(context),
      nodes: call_expr.arguments.iter().map(|node| node.into_node()).collect_in(context.arena.bump()),
      custom_close_paren: |_| None,
      is_parameters: false,
    },
    context,
  )));

  items
}

/// Generates a dynamic import (ex. `import("./a.js")` or `import.source("./a.js")`),
/// which swc represented as a call expression with an import callee.
pub(super) fn gen_import_expr<'a>(node: &'a ImportExpression<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let tokens = node.tokens_fast(context.program);
  let open_paren_index = tokens
    .iter()
    .position(|t| t.kind == TokenKind::LParen)
    .expect("Expected to find the open paren of the import expression.");
  let callee_range = SourceRange::new(node.start(), tokens[open_paren_index - 1].end());

  let mut items = gen_import_callee(node, context);
  // the callee was a node in swc, so generate its trailing comments like it was
  items.extend(gen_trailing_comments(&callee_range, context));

  let mut nodes = BumpVec::with_capacity_in(2, context.arena.bump());
  nodes.push((&node.source).into_node());
  if let Some(options) = &node.options {
    nodes.push(options.into_node());
  }
  items.push_condition(conditions::with_indent_if_start_of_line_indented(gen_parameters_or_arguments(
    GenParametersOrArgumentsOptions {
      node: node.into_node(),
      range: Some(SourceRange::new(tokens[open_paren_index].start(), node.end())),
      nodes,
      custom_close_paren: |_| None,
      is_parameters: false,
    },
    context,
  )));

  items
}

pub(super) fn gen_import_callee<'a>(node: &'a ImportExpression<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  items.push_sc(sc!("import"));
  // for now, just be simple and force this on one line
  match node.phase {
    None => {
      // nothing to do
    }
    Some(ImportPhase::Source) => {
      items.push_sc(sc!(".source"));
    }
    Some(ImportPhase::Defer) => {
      items.push_sc(sc!(".defer"));
    }
  }
  items
}

pub(super) fn gen_class_expr<'a>(node: &'a Class<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let is_class_decl = matches!(node.into_node().parent(context.program), Some(Node::ExportDefaultDeclaration(_))) && node.id.is_some();
  let items = gen_class_decl_or_expr(
    ClassDeclOrExpr {
      node: node.into_node(),
      member_node: node.into_node(),
      decorators: &node.decorators,
      is_class_decl,
      is_declare: false,
      is_abstract: node.r#abstract,
      ident: node.id.as_ref().map(|x| x.into_node()),
      type_params: node.type_parameters.as_ref().map(|x| x.into_node()),
      super_class: node.heritage.as_ref().map(|x| (&x.expression).into_node()),
      super_type_params: node.heritage.as_ref().and_then(|x| x.type_arguments.as_ref()).map(|x| x.into_node()),
      implements: node.implements.iter().map(|x| x.into_node()).collect(),
      members: get_class_members(&node.body, context),
      brace_position: if is_class_decl {
        context.config.class_declaration_brace_position
      } else {
        context.config.class_expression_brace_position
      },
    },
    context,
  );

  if should_add_parens_around_expr(node.into_node(), context) {
    if context.config.paren_expression_space_around {
      surround_with_parens(surround_with_spaces(items))
    } else {
      surround_with_parens(items)
    }
  } else {
    items
  }
}
