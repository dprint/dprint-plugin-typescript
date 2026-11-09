use super::*;

pub(super) fn gen_conditional_expr<'a>(node: &'a ConditionalExpression<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let question_token = context.token_finder.get_first_operator_after(&node.test, "?").unwrap();
  let colon_token = context.token_finder.get_first_operator_after(&node.consequent, ":").unwrap();
  let line_per_expression = context.config.conditional_expression_line_per_expression;
  let has_newline_test_cons = node_helpers::get_use_new_lines_for_nodes(&node.test, &node.consequent, context.program);
  let has_newline_cons_alt = node_helpers::get_use_new_lines_for_nodes(&node.consequent, &node.alternate, context.program);
  let use_new_lines_for_nested_conditional = context.config.conditional_expression_use_nested_indentation && {
    node.into_node().parent(context.program).map(|p| p.kind()) == Some(NodeKind::ConditionalExpression)
      || matches!(node.consequent, Expression::ConditionalExpression(_))
      || matches!(node.alternate, Expression::ConditionalExpression(_))
  };
  let mut force_test_cons_newline =
    has_newline_test_cons && (!context.config.conditional_expression_prefer_single_line || use_new_lines_for_nested_conditional);
  let mut force_cons_alt_newline = has_newline_cons_alt && (!context.config.conditional_expression_prefer_single_line || use_new_lines_for_nested_conditional);
  if line_per_expression && (force_test_cons_newline || force_cons_alt_newline) {
    // for line per expression, if one is true then both should be true
    force_test_cons_newline = true;
    force_cons_alt_newline = true;
  }

  let (question_position, colon_position) = get_operator_position(node, question_token, colon_token, context);
  let top_most_data = get_top_most_data(node, context);
  let before_alternate_ln = LineNumber::new("beforeAlternate");
  let end_ln = LineNumber::new("endConditionalExpression");
  let question_comment_items = gen_cond_token_comments(question_token, context, Some(top_most_data.il));
  let colon_comment_items = gen_cond_token_comments(colon_token, context, Some(top_most_data.il));
  let mut items = PrintItems::new(context.arena);

  if top_most_data.is_top_most {
    items.push_info(top_most_data.ln);
    items.push_info(top_most_data.il);
  }

  let top_most_il = top_most_data.il;

  items.extend(ir_helpers::new_line_group(with_queued_indent({
    let mut items = gen_node((&node.test).into_node(), context);
    if question_position == OperatorPosition::SameLine {
      items.push_sc(sc!(" ?"));
    }
    items.extend(question_comment_items.trailing_line);
    items
  })));

  items.extend(question_comment_items.previous_lines);

  // these are only shifted when the expression moves to another line and not cleared when it moves
  // to another column like in `gen_line_numbers_region_start`, which formats a conditional expression
  // in arguments differently (see the specs for issue036 and deno's issue014291)
  items.push_anchor(LineNumberAnchor::new(end_ln));
  items.push_anchor(LineNumberAnchor::new(before_alternate_ln));

  let multi_line_reevaluation = if force_test_cons_newline {
    items.push_signal(Signal::NewLine);
    None
  } else if line_per_expression {
    let mut condition = conditions::new_line_if_multiple_lines_space_or_new_line_otherwise(context.arena, top_most_data.ln, Some(before_alternate_ln));
    let reevaluation = condition.create_reevaluation();
    items.push_condition(condition);
    Some(reevaluation)
  } else {
    items.push_signal(Signal::SpaceOrNewLine);
    None
  };

  let cons_and_alt_items = {
    let mut items = PrintItems::new(context.arena);

    // add any preceeding comments of the question token
    items.extend({
      let operator_token_leading_comments = get_leading_comments_on_previous_lines(&question_token.range(), context);
      let mut items = gen_comment_collection(operator_token_leading_comments.into_iter(), None, None, context);
      if !items.is_empty() {
        items.push_signal(Signal::NewLine);
      }
      items
    });

    items.push_condition({
      let mut items = PrintItems::new(context.arena);
      let is_start_of_line = IsStartOfLine::new("conditionalConsequentStart");
      items.push_info(is_start_of_line);
      items.extend(question_comment_items.leading_line);
      if question_position == OperatorPosition::NextLine {
        items.push_sc(sc!("? "));
      }
      items.extend(gen_branch(&node.consequent, question_position, is_start_of_line, context));
      if colon_position == OperatorPosition::SameLine {
        items.push_sc(sc!(" :"));
      }
      items.extend(colon_comment_items.trailing_line);
      indent_if_sol_and_same_indent_as_top_most(ir_helpers::new_line_group(items), top_most_il)
    });

    items.extend(colon_comment_items.previous_lines);

    if force_cons_alt_newline {
      items.push_signal(Signal::NewLine);
    } else if line_per_expression {
      items.push_condition(conditions::new_line_if_multiple_lines_space_or_new_line_otherwise(
        context.arena,
        top_most_data.ln,
        Some(before_alternate_ln),
      ));
    } else {
      items.push_signal(Signal::SpaceOrNewLine);
    }

    items.push_condition({
      let mut items = PrintItems::new(context.arena);
      let is_start_of_line = IsStartOfLine::new("conditionalAlternateStart");
      items.push_info(is_start_of_line);
      items.extend(colon_comment_items.leading_line);
      if colon_position == OperatorPosition::NextLine {
        items.push_sc(sc!(": "));
      }
      items.push_info(before_alternate_ln);
      items.extend(gen_branch(&node.alternate, colon_position, is_start_of_line, context));
      indent_if_sol_and_same_indent_as_top_most(ir_helpers::new_line_group(items), top_most_il)
    });
    items.push_info(end_ln);

    if let Some(reevaluation) = multi_line_reevaluation {
      items.push_reevaluation(reevaluation);
    }

    items
  };

  if top_most_data.is_top_most || use_new_lines_for_nested_conditional {
    items.push_condition(conditions::indent_if_start_of_line(cons_and_alt_items));
  } else {
    items.push_condition(indent_if_sol_and_same_indent_as_top_most(cons_and_alt_items, top_most_data.il));
  }

  return items;

  /// Generates the consequent or alternate of a conditional expression.
  ///
  /// The hanging content of a multi-line branch (ex. the properties of an object argument) is
  /// indented from the start of the operator's line, so it lines up with the start of the branch
  /// when the operator is before the branch and an indent is as wide as the operator and its
  /// space. In that case, when the operator starts the line and `indentBranchContent` is enabled,
  /// the lines after the branch's first line are indented once more.
  fn gen_branch<'a>(
    branch: &'a Expression<'a>,
    operator_position: OperatorPosition,
    operator_is_start_of_line: IsStartOfLine,
    context: &mut Context<'a>,
  ) -> PrintItems<'a> {
    let items = gen_node(branch.into_node(), context);
    if !context.config.conditional_expression_indent_branch_content
      || operator_position != OperatorPosition::NextLine
      // nested conditionals line their operators up with the top most conditional instead
      || matches!(branch, Expression::ConditionalExpression(_))
      || context.config.indent_width > 2
    {
      return items;
    }
    let arena = context.arena;
    let items = items.into_rc_path();
    let mut new_items = PrintItems::new(arena);
    new_items.push_condition(if_true_or(
      "indentConditionalBranchIfOperatorStartOfLine",
      arena.alloc_resolver(move |context| context.resolved_is_start_of_line(operator_is_start_of_line)),
      with_indent(items.into_print_items(arena)),
      items.into_print_items(arena),
    ));
    new_items
  }

  struct TopMostData {
    ln: LineNumber,
    il: IndentLevel,
    is_top_most: bool,
  }

  fn get_top_most_data<'a>(node: &'a ConditionalExpression<'a>, context: &mut Context<'a>) -> TopMostData {
    // The "top most" node in nested conditionals follows the ancestors up through
    // the alternate expressions.
    let mut top_most_node: &'a ConditionalExpression<'a> = node;

    for ancestor in context.parent_stack.iter() {
      if let Node::ConditionalExpression(parent) = *ancestor {
        if parent.alternate.start() == top_most_node.start() {
          top_most_node = parent;
        } else {
          break;
        }
      } else {
        break;
      }
    }

    let is_top_most = top_most_node.range() == node.range();
    let (top_most_ln, top_most_il) = get_or_set_top_most(top_most_node.start(), is_top_most, context);

    return TopMostData {
      is_top_most,
      ln: top_most_ln,
      il: top_most_il,
    };

    fn get_or_set_top_most<'a>(top_most_expr_start: SourcePos, is_top_most: bool, context: &mut Context<'a>) -> (LineNumber, IndentLevel) {
      if is_top_most {
        let ln = LineNumber::new("conditionalExprStart");
        let il = IndentLevel::new("conditionalExprStart");
        context.store_ln_for_node(&top_most_expr_start, ln);
        context.store_il_for_node(&top_most_expr_start, il);
        (ln, il)
      } else {
        (
          context.get_ln_for_node(&top_most_expr_start).unwrap(),
          context.get_il_for_node(&top_most_expr_start).unwrap(),
        )
      }
    }
  }

  fn get_operator_position<'a>(
    node: &ConditionalExpression<'a>,
    question_token: &TokenAndSpan,
    colon_token: &TokenAndSpan,
    context: &mut Context<'a>,
  ) -> (OperatorPosition, OperatorPosition) {
    fn get_maintain_position<'a>(node: &ConditionalExpression<'a>, expr: &Expression<'a>, token: &TokenAndSpan, context: &mut Context<'a>) -> OperatorPosition {
      if expr.end_line_fast(context.program) == token.start_line_fast(context.program) {
        if node.start_line_fast(context.program) == node.end_line_fast(context.program) {
          // prefer the dprint default when going from one to multiple lines
          OperatorPosition::NextLine
        } else {
          OperatorPosition::SameLine
        }
      } else {
        OperatorPosition::NextLine
      }
    }

    match context.config.conditional_expression_operator_position {
      OperatorPosition::NextLine => (OperatorPosition::NextLine, OperatorPosition::NextLine),
      OperatorPosition::SameLine => (OperatorPosition::SameLine, OperatorPosition::SameLine),
      OperatorPosition::Maintain => (
        get_maintain_position(node, &node.test, question_token, context),
        get_maintain_position(node, &node.consequent, colon_token, context),
      ),
    }
  }
}

pub(super) struct ConditionalTokenComments<'a> {
  pub(super) previous_lines: PrintItems<'a>,
  pub(super) leading_line: PrintItems<'a>,
  pub(super) trailing_line: PrintItems<'a>,
}

/// Generates the comments used for tokens in conditional expressions and types.
///
/// The comments on the lines before the branch are indented when at the same indentation as
/// `top_most_il`, or never indented when it is `None`.
pub(super) fn gen_cond_token_comments<'a>(token: &TokenAndSpan, context: &mut Context<'a>, top_most_il: Option<IndentLevel>) -> ConditionalTokenComments<'a> {
  let token_line = token.end_line_fast(context.program);
  let previous_token = token.previous_token_fast(context.program).unwrap();
  let next_token = token.next_token_fast(context.program).unwrap();
  let previous_token_line = previous_token.end_line_fast(context.program);
  let next_token_line = next_token.start_line_fast(context.program);
  let program = context.program;
  return if token_line > previous_token_line {
    ConditionalTokenComments {
      previous_lines: {
        let leading_comments = token.leading_comments_fast(program).filter(|c| {
          let comment_line = c.start_line_fast(program);
          comment_line > previous_token_line && comment_line < next_token_line
        });
        let trailing_comments = token.trailing_comments_fast(program).filter(|c| c.start_line_fast(program) < next_token_line);
        gen_previous_lines(leading_comments.chain(trailing_comments), context, top_most_il)
      },
      leading_line: if token_line == next_token_line {
        gen_leading_comments_same_line(&token.range(), context)
      } else {
        PrintItems::new(context.arena)
      },
      trailing_line: PrintItems::new(context.arena),
    }
  } else if token_line < next_token_line {
    // the token is at the end of a line, so the comments on the lines after it belong to the
    // next branch and go before the token like when it's on its own line (ex. when it moves
    // to the next line)
    let trailing_line = gen_trailing_comments_same_line(&token.range(), context);
    let trailing_comments = token.trailing_comments_fast(program).filter(|c| {
      let comment_line = c.start_line_fast(program);
      comment_line > token_line && comment_line < next_token_line
    });
    ConditionalTokenComments {
      previous_lines: gen_previous_lines(trailing_comments, context, top_most_il),
      leading_line: PrintItems::new(context.arena),
      trailing_line,
    }
  } else {
    // do nothing
    ConditionalTokenComments {
      previous_lines: PrintItems::new(context.arena),
      leading_line: PrintItems::new(context.arena),
      trailing_line: PrintItems::new(context.arena),
    }
  };

  fn gen_previous_lines<'a>(comments: impl Iterator<Item = &'a Comment<'a>>, context: &mut Context<'a>, top_most_il: Option<IndentLevel>) -> PrintItems<'a> {
    let items = gen_comments_as_statements(comments, None, context);
    if items.is_empty() {
      items
    } else {
      let mut new_items = PrintItems::new(context.arena);
      new_items.push_signal(Signal::NewLine);
      match top_most_il {
        Some(top_most_il) => new_items.push_condition(indent_if_sol_and_same_indent_as_top_most(items, top_most_il)),
        None => new_items.extend(items),
      }
      new_items
    }
  }
}

pub(super) fn indent_if_sol_and_same_indent_as_top_most<'a>(items: PrintItems<'a>, indent_level: IndentLevel) -> Condition<'a> {
  let arena = items.arena();
  let items = items.into_rc_path();
  if_true_or(
    "indentIfSameIndentationLevelAndStartOfLine",
    arena.alloc_resolver(move |context| {
      if context.writer_info.is_start_of_line() {
        let top_most_il = context.resolved_indent_level(indent_level)?;
        Some(context.writer_info.indent_level == top_most_il)
      } else {
        Some(false)
      }
    }),
    with_indent(items.into_print_items(arena)),
    items.into_print_items(arena),
  )
}

pub(super) fn gen_class_implements<'a>(node: &'a TSClassImplements<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  gen_expr_with_type_args((&node.expression).into_node(), node.type_arguments.as_deref(), context)
}

pub(super) fn gen_interface_heritage<'a>(node: &'a TSInterfaceHeritage<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  gen_expr_with_type_args((&node.type_name).into_node(), node.type_arguments.as_deref(), context)
}

/// Generates an expression with type arguments (ex. `B<T>` in `class A implements B<T>`).
pub(super) fn gen_expr_with_type_args<'a>(
  expr: Node<'a>,
  type_args: Option<&'a TSTypeParameterInstantiation<'a>>,
  context: &mut Context<'a>,
) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  items.extend(gen_node(expr, context));
  if let Some(type_args) = type_args {
    items.extend(gen_node(type_args.into_node(), context));
  }
  items
}

pub(super) fn is_iife_fn_expr<'a>(node: &'a Function<'a>, program: ProgramInfo<'a>) -> bool {
  let mut current: Node<'a> = node.into_node();
  while let Some(parent) = current.parent(program) {
    if parent.kind() == NodeKind::ParenthesizedExpression {
      current = parent;
      continue;
    }
    return match parent {
      // optional calls are call expressions too
      Node::CallExpression(call) => call.callee.range() == current.range(),
      _ => false,
    };
  }
  false
}

pub(super) fn gen_fn_expr<'a>(node: &'a Function<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  if context.config.function_expression_flat_iife && is_iife_fn_expr(node, context.program) {
    context.skip_iife_body_indent = true;
  }

  let items = gen_function_decl_or_expr(
    FunctionDeclOrExprNode {
      node: node.into_node(),
      is_func_decl: node.into_node().parent(context.program).map(|p| p.kind()) == Some(NodeKind::ExportDefaultDeclaration) && node.id.is_some(),
      ident: node.id.as_ref(),
      declare: false,
      func: node,
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

pub(super) fn should_add_parens_around_expr<'a>(node: Node<'a>, context: &Context<'a>) -> bool {
  let original_node = node;
  for node in node.ancestors(context.program) {
    match node {
      Node::ParenthesizedExpression(paren_expr) => {
        if !should_skip_paren_expr(paren_expr, context) {
          return false;
        }
      }
      // optional calls are call expressions too
      Node::CallExpression(call_expr) => {
        if !call_expr.callee.range().contains(&original_node.range()) {
          // it's in an argument, so don't add parens
          return false;
        }
      }
      Node::NewExpression(new_expr) => {
        if !new_expr.callee.range().contains(&original_node.range()) {
          // it's in an argument, so don't add parens
          return false;
        }
      }
      Node::ExpressionStatement(_) => return true,
      Node::ComputedMemberExpression(expr) => {
        if expr.expression.range().contains(&original_node.range()) {
          return false;
        }
      }
      Node::StaticMemberExpression(_) | Node::PrivateFieldExpression(_) => {
        // continue searching
      }
      Node::ConditionalExpression(cond_expr) => {
        return cond_expr.test.range().contains(&original_node.range());
      }
      Node::BinaryExpression(_) | Node::LogicalExpression(_) | Node::PrivateInExpression(_) => {
        let bin_expr = BinExpr::from_node(node).unwrap();
        // we only care to add parens when it's the left most expr
        if bin_expr.right().range().contains(&original_node.range()) {
          return false;
        }
      }
      Node::ChainExpression(_) => {
        // continue searching
      }
      // the expression is always the left most part of these
      Node::TSAsExpression(_) | Node::TSSatisfiesExpression(_) | Node::TSNonNullExpression(_) | Node::TSInstantiationExpression(_) => {
        // continue searching
      }
      Node::TaggedTemplateExpression(tagged_tpl) => {
        if !tagged_tpl.tag.range().contains(&original_node.range()) {
          return false;
        }
      }
      _ => {
        return false;
      }
    }
  }

  false
}

/// Generates a property of an object literal (ex. `a: 1`, `a`, `a() {}`, `get a() {}`, or `set a(v) {}`).
pub(super) fn gen_object_property<'a>(node: &'a ObjectProperty<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  match node.kind {
    PropertyKind::Get => gen_getter_prop(node, context),
    PropertyKind::Set => gen_setter_prop(node, context),
    PropertyKind::Init => {
      if node.method {
        gen_method_prop(node, context)
      } else if node.shorthand {
        // the key and value are the same identifier
        gen_node((&node.value).into_node(), context)
      } else {
        gen_key_value_prop(node, context)
      }
    }
  }
}

pub(super) fn gen_getter_prop<'a>(node: &'a ObjectProperty<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let function = get_object_property_function(node);
  gen_class_or_object_method(
    ClassOrObjectMethod {
      node: node.into_node(),
      parameters_range: function.get_parameters_range(context),
      decorators: None,
      accessibility: None,
      is_static: false,
      is_async: false,
      is_abstract: false,
      kind: ClassOrObjectMethodKind::Getter,
      is_generator: false,
      is_optional: false,
      is_override: false,
      key: (&node.key).into_node(),
      computed: node.computed,
      type_params: None,
      params: get_function_params(function, context),
      return_type: function.return_type.as_ref().map(|x| x.into_node()),
      body: function.body.as_ref().map(|x| x.into_node()),
    },
    context,
  )
}

pub(super) fn gen_key_value_prop<'a>(node: &'a ObjectProperty<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  items.extend(gen_object_property_key(node, context));
  items.extend(gen_assignment((&node.value).into_node(), sc!(":"), context));
  items
}

/// Generates the key of an object property, which is surrounded by brackets when computed.
pub(super) fn gen_object_property_key<'a>(node: &'a ObjectProperty<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  if node.computed {
    gen_computed_prop_name((&node.key).into_node(), context)
  } else {
    gen_quotable_prop((&node.key).into_node(), context)
  }
}

/// Gets the function of an object literal's method, getter, or setter.
pub(super) fn get_object_property_function<'a>(node: &'a ObjectProperty<'a>) -> &'a Function<'a> {
  match &node.value {
    Expression::FunctionExpression(function) => function,
    _ => unreachable!("expected the value of a method, getter, or setter to be a function"),
  }
}

pub(super) fn gen_member_expr<'a>(node: Node<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let flattened_member_expr = flatten_member_like_expr(node, context.program, context.arena.bump());
  gen_for_flattened_member_like_expr(flattened_member_expr, context)
}

pub(super) fn gen_meta_prop_expr<'a>(node: Node<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let flattened_meta_prop_expr = flatten_member_like_expr(node, context.program, context.arena.bump());
  gen_for_flattened_member_like_expr(flattened_meta_prop_expr, context)
}

pub(super) fn gen_new_expr<'a>(node: &'a NewExpression<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  items.push_sc(sc!("new "));
  items.extend(gen_node((&node.callee).into_node(), context));
  if let Some(type_args) = &node.type_arguments {
    items.extend(gen_node(type_args.into_node(), context));
  }
  // `new Foo` without parens has no arguments, which is generated the same as empty parens
  let args: BumpVec<'a, Node<'a>> = node.arguments.iter().map(|arg| arg.into_node()).collect_in(context.arena.bump());
  items.extend(gen_parameters_or_arguments(
    GenParametersOrArgumentsOptions {
      node: node.into_node(),
      range: node.get_parameters_range(context),
      nodes: args,
      custom_close_paren: |_| None,
      is_parameters: false,
    },
    context,
  ));
  items
}

pub(super) fn gen_non_null_expr<'a>(node: &'a TSNonNullExpression<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  items.extend(gen_node((&node.expression).into_node(), context));
  items.push_sc(sc!("!"));
  items
}

pub(super) fn gen_object_lit<'a>(node: &'a ObjectExpression<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let items = context.with_maybe_consistent_props(
    node,
    |node| use_consistent_quotes_for_members(node.properties.iter().map(|p| p.into_node())),
    |context, node| {
      gen_object_like_node(
        GenObjectLikeNodeOptions {
          node: node.into_node(),
          members: node.properties.iter().map(|x| x.into_node()).collect(),
          separator: context.config.object_expression_trailing_commas.into(),
          prefer_hanging: context.config.object_expression_prefer_hanging,
          prefer_single_line: context.config.object_expression_prefer_single_line,
          force_single_line: false,
          force_multi_line: is_node_definitely_above_line_width(node.range(), context),
          surround_single_line_with_spaces: context.config.object_expression_space_surrounding_properties,
          blank_lines: ListBlankLines::Maintain,
          node_sorter: None,
        },
        context,
      )
    },
  );

  if should_add_parens_around_expr(node.into_node(), context) {
    surround_with_parens(if context.config.paren_expression_space_around {
      surround_with_spaces(items)
    } else {
      items
    })
  } else {
    items
  }
}

pub(super) fn gen_paren_expr<'a>(node: &'a ParenthesizedExpression<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let end_ln = match context.assignment_paren_end_ln {
    Some((start, end_ln)) if start == node.start() => context.assignment_paren_end_ln.take().map(|_| end_ln),
    _ => None,
  };
  let mut items = gen_paren_expr_inner(node, context);
  if let Some(end_ln) = end_ln {
    items.push_info(end_ln);
  }
  items
}

pub(super) fn gen_paren_expr_inner<'a>(node: &'a ParenthesizedExpression<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  if should_skip_paren_expr(node, context) {
    return gen_node((&node.expression).into_node(), context);
  }

  let generated_items = conditions::with_indent_if_start_of_line_indented(gen_node_in_parens(
    |context| gen_node((&node.expression).into_node(), context),
    GenNodeInParensOptions {
      inner_range: node.expression.range(),
      prefer_hanging: true,
      allow_open_paren_trailing_comments: true,
      single_line_space_around: context.config.paren_expression_space_around,
    },
    context,
  ))
  .into_print_items(context.arena);

  return if get_use_new_line_group(node, context) {
    new_line_group(generated_items)
  } else {
    generated_items
  };

  fn get_use_new_line_group<'a>(node: &'a ParenthesizedExpression<'a>, context: &Context<'a>) -> bool {
    if let Some(Node::ArrowFunctionExpression(arrow_expr)) = node.into_node().parent(context.program) {
      debug_assert!(arrow_expr.body.start() == node.start());
      use_new_line_group_for_arrow_body(arrow_expr, context)
    } else {
      true
    }
  }
}

pub(super) fn should_skip_paren_expr<'a>(node: &'a ParenthesizedExpression<'a>, context: &Context<'a>) -> bool {
  // swc didn't keep the parens directly after the `@` of a decorator (see `gen_decorator`)
  if is_decorator_paren_expr(node, context.program) {
    return true;
  }

  if node_helpers::has_surrounding_different_line_comments((&node.expression).into_node(), context.program) {
    return false;
  }

  // keep parens around any destructuring assignments
  if let Expression::AssignmentExpression(assign_expr) = &node.expression
    && matches!(assign_expr.left, AssignmentTarget::ObjectAssignmentTarget(_))
  {
    return false;
  }

  if matches!(node.expression, Expression::SequenceExpression(_)) {
    // don't care about extra logic for sequence expressions
    return false;
  }

  // keep for `(let)[0] = 1;` or `(let[0] = 1);` because `let[0] = 1;` starts a declaration
  if starts_with_let_ident(&node.expression, context) {
    return false;
  }

  // keep when there is a JSDoc because it could be a type assertion or satisfies
  for c in node.leading_comments_fast(context.program) {
    if c.kind == CommentKind::Block && c.text.starts_with('*') {
      return false;
    }
  }

  // keep for `(val as number)++` or `(<number>val)++`
  // (oxc doesn't have a paren expr in these, so `gen_update_expr` adds the parens instead)
  let parent = node.into_node().parent(context.program).unwrap();
  if parent.kind() == NodeKind::UpdateExpression && matches!(node.expression, Expression::TSAsExpression(_) | Expression::TSTypeAssertion(_)) {
    return false;
  }

  // a paren expr in the parens after the `@` (ex. `@((a.b))`) was the decorator's expression in swc
  let is_decorator_expr = match parent {
    Node::Decorator(_) => true,
    Node::ParenthesizedExpression(paren_expr) => is_decorator_paren_expr(paren_expr, context.program),
    _ => false,
  };
  if is_decorator_expr {
    return is_bare_decorator_expr(&node.expression, context.program);
  }

  if matches!(node.expression, Expression::ArrayExpression(_) | Expression::Identifier(_)) {
    return true;
  }

  if is_non_super_member_expr(parent) && is_non_super_member_expr((&node.expression).into_node()) {
    return true;
  }

  // skip over any paren exprs within paren exprs and needless paren exprs
  if matches!(
    parent.kind(),
    NodeKind::ParenthesizedExpression
      | NodeKind::ExpressionStatement
      | NodeKind::JSXElement
      | NodeKind::JSXFragment
      | NodeKind::JSXExpressionContainer
      | NodeKind::UpdateExpression
      | NodeKind::ObjectProperty
  ) || is_in_computed_key(parent, node.range())
  {
    return true;
  }

  if is_jsx_paren_expr_removed(node, context) {
    return true;
  }

  // skip explicitly parsing this as a paren expr as that will be handled
  // in the JSX element/fragment and it might collapse back to not having a paren expr
  if matches!(node.expression, Expression::JSXElement(_) | Expression::JSXFragment(_)) {
    return is_jsx_paren_expr_handled_node((&node.expression).into_node(), context);
  }

  if let Node::AssignmentExpression(assign_expr) = parent
    && assign_expr.right.range().contains(&node.range())
  {
    return true;
  }

  if let Node::VariableDeclarator(var_decl) = parent
    && !matches!(node.expression, Expression::AssignmentExpression(_))
    && let Some(init) = &var_decl.init
    && init.range().contains(&node.range())
  {
    return true;
  }

  // skip over an argument or array element if not a spread (the parent is
  // the spread element for those)
  let is_argument_or_array_element = match parent {
    Node::CallExpression(call_expr) => !call_expr.callee.range().contains(&node.range()),
    Node::NewExpression(new_expr) => !new_expr.callee.range().contains(&node.range()),
    // swc represented `import(x)` as a call expression
    Node::ArrayExpression(_) | Node::ImportExpression(_) => true,
    _ => false,
  };
  if is_argument_or_array_element {
    return true;
  }

  if let Node::ComputedMemberExpression(member_expr) = parent
    && !matches!(member_expr.object, Expression::Super(_))
    && member_expr.expression.range().contains(&node.range())
  {
    return true;
  }

  return false;

  /// Gets if the node is a member expression that isn't a property of `super` (swc's `MemberExpr`).
  fn is_non_super_member_expr(node: Node) -> bool {
    match node {
      Node::StaticMemberExpression(expr) => !matches!(expr.object, Expression::Super(_)),
      Node::ComputedMemberExpression(expr) => !matches!(expr.object, Expression::Super(_)),
      Node::PrivateFieldExpression(expr) => !matches!(expr.object, Expression::Super(_)),
      _ => false,
    }
  }

  /// Gets if the range is within the computed key of the node (swc's `ComputedPropName`).
  fn is_in_computed_key(node: Node, range: SourceRange) -> bool {
    let (computed, key_range) = match node {
      Node::ObjectProperty(prop) => (prop.computed, prop.key.range()),
      Node::BindingProperty(prop) => (prop.computed, prop.key.range()),
      Node::AssignmentTargetPropertyProperty(prop) => (prop.computed, prop.name.range()),
      Node::MethodDefinition(method) => (method.computed, method.key.range()),
      Node::PropertyDefinition(prop) => (prop.computed, prop.key.range()),
      Node::AccessorProperty(prop) => (prop.computed, prop.key.range()),
      _ => return false,
    };
    computed && key_range.contains(&range)
  }
}

/// Gets if the expression is the `let` identifier or starts with it followed by a `[`.
pub(super) fn starts_with_let_ident<'a>(expr: &Expression<'a>, context: &Context<'a>) -> bool {
  match expr {
    Expression::Identifier(ident) => ident.name.as_str() == "let",
    _ => expr
      .text_fast(context.program)
      .strip_prefix("let")
      .is_some_and(|rest| rest.trim_start().starts_with('[')),
  }
}

pub(super) fn gen_sequence_expr<'a>(node: &'a SequenceExpression<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  gen_separated_values(
    GenSeparatedValuesParams {
      nodes: node.expressions.iter().map(|x| NodeOrSeparator::Node(x.into_node())).collect(),
      prefer_hanging: context.config.sequence_expression_prefer_hanging,
      force_use_new_lines: is_node_definitely_above_line_width(node.range(), context),
      blank_lines: ListBlankLines::Remove,
      separator: TrailingCommas::Never.into(),
      single_line_options: ir_helpers::SingleLineOptions::same_line_maybe_space_separated(context.arena),
      multi_line_options: ir_helpers::MultiLineOptions::same_line_start_hanging_indent(),
      force_possible_newline_at_start: false,
      node_sorter: None,
    },
    context,
  )
}

pub(super) fn gen_setter_prop<'a>(node: &'a ObjectProperty<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let function = get_object_property_function(node);
  gen_class_or_object_method(
    ClassOrObjectMethod {
      node: node.into_node(),
      parameters_range: function.get_parameters_range(context),
      decorators: None,
      accessibility: None,
      is_static: false,
      is_async: false,
      is_abstract: false,
      kind: ClassOrObjectMethodKind::Setter,
      is_generator: false,
      is_optional: false,
      is_override: false,
      key: (&node.key).into_node(),
      computed: node.computed,
      type_params: None,
      params: get_function_params(function, context),
      return_type: function.return_type.as_ref().map(|x| x.into_node()),
      body: function.body.as_ref().map(|x| x.into_node()),
    },
    context,
  )
}

pub(super) fn gen_spread_element<'a>(node: &'a SpreadElement<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  items.push_sc(sc!("..."));
  items.extend(gen_node((&node.argument).into_node(), context));
  items
}

/// Formats the tagged template literal using an external formatter.
/// Detects the type of embedded language automatically.
pub(super) fn maybe_gen_tagged_tpl_with_external_formatter<'a>(node: &'a TaggedTemplateExpression<'a>, context: &mut Context<'a>) -> Option<PrintItems<'a>> {
  if !context.config.tagged_template_format_embedded {
    return None;
  }
  let external_formatter = context.external_formatter.as_ref()?;
  let tag = get_embedded_language_tag(node)?;
  let extension = context.config.tagged_template_embedded_languages.get(tag)?.as_str();

  let placeholder_css = "@dpr1nt_";
  let placeholder_other = "dpr1nt_";
  // First creates text with placeholders for the expressions.
  let placeholder_text = match extension {
    "css" | "less" | "scss" | "sass" => placeholder_css,
    _ => placeholder_other,
  };
  let text = capacity_builder::StringBuilder::<String>::build(|builder| {
    let expr_len = node.quasi.expressions.len();
    for (i, quasi) in node.quasi.quasis.iter().enumerate() {
      builder.append(quasi.value.raw.as_str());
      if i < expr_len {
        builder.append(placeholder_text);
        if i < 10 {
          // increase chance all placeholders have the same length
          builder.append("0");
        }
        builder.append(i); // give each placeholder a unique name so the formatter doesn't remove duplicates
        builder.append("_d");
      }
    }
  })
  .unwrap();

  // Then formats the text with the external formatter, which gets the escapes that only
  // exist because of the template literal unescaped (ex. Tailwind's `\\[\\#86efac\\]`
  // as `\[\#86efac\]`) and the other escapes as is (ex. `\n` and `\e600`).
  let text = unescape_embedded_text(&text)?;
  let formatted_tpl = match external_formatter(extension, text, context.config) {
    Ok(formatted_tpl) => escape_embedded_text(&formatted_tpl?),
    Err(err) => {
      context.diagnostics.push(context::GenerateDiagnostic {
        message: format!(
          "Error formatting tagged template literal at line {}: {}",
          node.start_line_fast(context.program) + 1,
          err
        ),
      });
      return None;
    }
  };

  let mut items = PrintItems::new(context.arena);
  let mut expr_index = 0;
  let formatted_tpl = context.alloc_str(&formatted_tpl);
  items.push_sc(sc!("`"));

  // a template on a single line stays on a single line when its formatted text does too
  let single_line_text = formatted_tpl.trim();
  let is_single_line = !single_line_text.contains('\n') && !node.quasi.quasis.iter().any(|quasi| quasi.value.raw.as_str().contains('\n'));
  if is_single_line {
    gen_embedded_line(single_line_text, 0, placeholder_text, node, &mut expr_index, &mut items, context);
    items.push_sc(sc!("`"));
    return Some(items);
  }

  items.push_signal(Signal::NewLine);
  items.push_signal(Signal::StartIndent);
  let mut current_indent_level = 0;
  let use_tabs = context.config.use_tabs;
  let indent_width = if use_tabs { 1 } else { context.config.indent_width };
  let indent_char = if use_tabs { '\t' } else { ' ' };
  for line in formatted_tpl.lines() {
    // count indent characters
    let pos = line.chars().take_while(|ch| *ch == indent_char).count();
    let indent_level = if indent_width == 0 { 0 } else { pos / indent_width as usize };
    while indent_level > current_indent_level {
      items.push_signal(Signal::StartIndent);
      current_indent_level += 1;
    }
    while indent_level < current_indent_level {
      items.push_signal(Signal::FinishIndent);
      current_indent_level -= 1;
    }
    gen_embedded_line(line, pos, placeholder_text, node, &mut expr_index, &mut items, context);
    items.push_signal(Signal::NewLine);
  }
  while current_indent_level > 0 {
    items.push_signal(Signal::FinishIndent);
    current_indent_level -= 1;
  }
  items.push_signal(Signal::FinishIndent);
  items.push_sc(sc!("`"));
  return Some(items);

  /// Generates a line of the formatted text from the position, replacing the
  /// placeholders with the template's expressions.
  fn gen_embedded_line<'a>(
    line: &'a str,
    mut pos: usize,
    placeholder_text: &str,
    node: &'a TaggedTemplateExpression<'a>,
    expr_index: &mut usize,
    items: &mut PrintItems<'a>,
    context: &mut Context<'a>,
  ) {
    let mut parts = line[pos..].split(placeholder_text).enumerate().peekable();
    while let Some((i, part)) = parts.next() {
      let end = pos + part.len();
      if i > 0 {
        pos += part.find("_d").unwrap() + 2;
      }
      let text = &line[pos..end];
      if !text.is_empty() {
        items.extend(gen_from_raw_string(context.arena, text));
      }
      if parts.peek().is_some() {
        items.push_sc(sc!("${"));
        items.extend(gen_node((&node.quasi.expressions[*expr_index]).into_node(), context));
        items.push_sc(sc!("}"));
        pos = end + placeholder_text.len();
        *expr_index += 1;
      }
    }
  }
}

/// Gets the tag that's looked up in the `taggedTemplate.embeddedLanguages` configuration,
/// which is the tag's name or `"css"` and `"graphql"` for the patterns of those languages.
pub(super) fn get_embedded_language_tag<'a>(node: &TaggedTemplateExpression<'a>) -> Option<&'a str> {
  match &node.tag {
    Expression::Identifier(ident) => Some(ident.name.as_str()),
    member_expr @ (Expression::StaticMemberExpression(_) | Expression::ComputedMemberExpression(_) | Expression::PrivateFieldExpression(_)) => {
      // styled.foo`...`, styled["foo"]`...`, Button.extend`...`
      if is_styled_member_expr(member_expr) {
        return Some("css");
      }
      let Some(Expression::Identifier(obj)) = get_member_obj(member_expr) else {
        return None;
      };
      match (obj.name.as_str(), get_member_prop_name(member_expr)?) {
        ("css", "global" | "resolve") => Some("css"), // css.global`...` (styled-jsx)
        ("graphql", "experimental") => Some("graphql"),
        _ => None,
      }
    }
    Expression::CallExpression(call_expr) => {
      match &call_expr.callee {
        // styled(Button)`...`
        Expression::Identifier(ident) if ident.name.as_str() == "styled" => Some("css"),
        // styled.foo.attrs({})`...`, Button.extend.attrs({})`...`, styled(Button).attrs({})`...`
        callee => match get_member_obj(callee) {
          Some(obj) if get_member_obj(obj).is_some() && is_styled_member_expr(obj) => Some("css"),
          Some(Expression::CallExpression(obj)) if matches!(&obj.callee, Expression::Identifier(ident) if ident.name.as_str() == "styled") => Some("css"),
          _ => None,
        },
      }
    }
    _ => None,
  }
}

/// Gets if the member expression is `styled.foo` or `Button.extend`.
pub(super) fn is_styled_member_expr(member_expr: &Expression) -> bool {
  let Some(Expression::Identifier(obj)) = get_member_obj(member_expr) else {
    return false;
  };
  let obj_name = obj.name.as_str();
  obj_name == "styled" || get_member_prop_name(member_expr) == Some("extend") && obj_name.starts_with(|c: char| c.is_ascii_uppercase())
}

/// Gets the object of a member expression or `None` when the expression isn't one.
pub(super) fn get_member_obj<'b, 'a>(member_expr: &'b Expression<'a>) -> Option<&'b Expression<'a>> {
  match member_expr {
    Expression::StaticMemberExpression(expr) => Some(&expr.object),
    Expression::ComputedMemberExpression(expr) => Some(&expr.object),
    Expression::PrivateFieldExpression(expr) => Some(&expr.object),
    _ => None,
  }
}

pub(super) fn get_member_prop_name<'a>(member_expr: &Expression<'a>) -> Option<&'a str> {
  match member_expr {
    Expression::StaticMemberExpression(expr) => Some(expr.property.name.as_str()),
    _ => None,
  }
}

/// Stands in for the backslash of an escape that the embedded language gets as is (ex. `\n`),
/// which keeps it apart from a backslash that was escaped (ex. `\\`).
pub(super) const EMBEDDED_ESCAPE_SENTINEL: char = '\u{E000}';

/// Unescapes what a template literal requires to be escaped (`\\`, `` \` ``, and `\${`), which
/// is what the embedded language sees. The other escapes are kept so that `escape_embedded_text`
/// can restore them exactly. Returns `None` when the text already has the sentinel.
pub(super) fn unescape_embedded_text(text: &str) -> Option<String> {
  if text.contains(EMBEDDED_ESCAPE_SENTINEL) {
    return None;
  }
  let mut result = String::with_capacity(text.len());
  let mut rest = text;
  while let Some(index) = rest.find('\\') {
    result.push_str(&rest[..index]);
    let escaped = &rest[index + 1..];
    if escaped.starts_with(['\\', '`']) || escaped.starts_with("${") {
      result.push_str(&escaped[..1]);
      rest = &escaped[1..];
    } else {
      result.push(EMBEDDED_ESCAPE_SENTINEL);
      rest = escaped;
    }
  }
  result.push_str(rest);
  Some(result)
}

/// Escapes the formatted text of an embedded language for a template literal,
/// which is the reverse of `unescape_embedded_text`.
pub(super) fn escape_embedded_text(text: &str) -> String {
  let mut result = String::with_capacity(text.len());
  let mut chars = text.chars().peekable();
  while let Some(c) = chars.next() {
    match c {
      EMBEDDED_ESCAPE_SENTINEL => result.push('\\'),
      '\\' | '`' => {
        result.push('\\');
        result.push(c);
      }
      '$' if chars.peek() == Some(&'{') => result.push_str("\\$"),
      _ => result.push(c),
    }
  }
  result
}

pub(super) fn gen_tagged_tpl<'a>(node: &'a TaggedTemplateExpression<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let use_space = context.config.tagged_template_space_before_literal;
  let mut items = gen_node((&node.tag).into_node(), context);
  if let Some(type_params) = &node.type_arguments {
    items.extend(gen_node(type_params.into_node(), context));
  }

  let generated_between_comments = gen_comments_between_lines_indented(node.tag.end(), context);
  if generated_between_comments.is_empty() {
    if use_space {
      items.push_signal(Signal::SpaceIfNotTrailing);
    }
  } else {
    items.extend(generated_between_comments);
  }

  if let Some(formatted_tpl) = maybe_gen_tagged_tpl_with_external_formatter(node, context) {
    items.push_condition(conditions::indent_if_start_of_line(formatted_tpl));
    return items;
  }

  items.push_condition(conditions::indent_if_start_of_line(gen_node((&node.quasi).into_node(), context)));
  items
}

pub(super) fn gen_tpl<'a>(node: &'a TemplateLiteral<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let class_names_sort_options = get_class_names_sort_options(node.into_node(), context).map(|options| (node.start(), options));
  let previous_class_names_sort_options = std::mem::replace(&mut context.tpl_class_names_sort_options, class_names_sort_options);
  let items = gen_template_literal(
    node.quasis.iter().map(|n| n.into_node()),
    node.expressions.iter().map(|x| x.into_node()),
    context,
  );
  context.tpl_class_names_sort_options = previous_class_names_sort_options;
  items
}

pub(super) fn gen_tpl_element<'a>(node: &'a TemplateElement<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let text = node.text_fast(context.program);
  match get_tpl_element_class_names_sort_options(node, context) {
    Some(options) => {
      let text = tailwind::sort_class_names(text, &options, &get_tailwind_project(context));
      gen_from_raw_string(context.arena, context.alloc_cow_str(text))
    }
    None => gen_from_raw_string(context.arena, text),
  }
}

pub(super) fn gen_template_literal<'a>(
  quasis: impl Iterator<Item = Node<'a>>,
  exprs: impl Iterator<Item = Node<'a>>,
  context: &mut Context<'a>,
) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  let mut is_head = true;
  // the indentation of the template's current line when that line starts in the template's text
  let mut line_indent: Option<TplLineIndent> = None;
  // the indentation the previous expression was generated at, which its closing brace is written at too
  let mut expr_line_indent: Option<TplLineIndent> = None;
  for node in get_nodes(quasis, exprs) {
    if let Node::TemplateElement(node_tpl) = node {
      let previous_expr_line_indent = expr_line_indent.take();
      items.extend(gen_node_with_inner_gen(node, context, |node_items, _| {
        let mut items = PrintItems::new(context.arena);
        items.push_sc(if is_head { sc!("`") } else { sc!("}") });
        if let Some(previous_expr_line_indent) = previous_expr_line_indent {
          previous_expr_line_indent.finish(&mut items);
        }
        items.push_signal(Signal::StartIgnoringIndent);
        items.extend(node_items);
        items.push_sc(if node_tpl.tail { sc!("`") } else { sc!("${") });
        items.push_signal(Signal::FinishIgnoringIndent);
        items
      }));
      let raw = node_tpl.value.raw.as_str();
      if let Some(index) = raw.rfind('\n') {
        line_indent = Some(TplLineIndent::new(&raw[index + 1..], context.config.indent_width));
      }
    } else {
      let mut expr_items = PrintItems::new(context.arena);
      let leading_comment_items = gen_leading_comments_on_previous_lines(&node.range(), context);
      let trailing_comment_items = gen_trailing_comments_as_statements(&node.range(), context);
      // a line comment after the expression is generated with it and forces a newline before the closing brace
      let has_trailing_line_comment = node
        .trailing_comments_fast(context.program)
        .any(|c| c.kind == CommentKind::Line && !context.has_handled_comment(c));
      let generated_expr = gen_node(node, context);
      if !leading_comment_items.is_empty() || !trailing_comment_items.is_empty() || has_trailing_line_comment {
        // the comments make it multi-line, so put the expression on its own line
        expr_items.push_signal(Signal::StartIndent);
        if leading_comment_items.is_empty() {
          expr_items.push_signal(Signal::NewLine);
        } else {
          expr_items.extend(surround_with_new_lines(leading_comment_items));
        }
        expr_items.extend(generated_expr);
        if trailing_comment_items.is_empty() {
          expr_items.push_signal(Signal::NewLine);
        } else {
          expr_items.extend(trailing_comment_items);
        }
        expr_items.push_signal(Signal::FinishIndent);
      } else {
        let keep_on_one_line = get_keep_on_one_line(node);
        let possible_surround_newlines = get_possible_surround_newlines(node);
        expr_items.extend(if keep_on_one_line {
          with_no_new_lines(generated_expr)
        } else if possible_surround_newlines {
          ir_helpers::surround_with_newlines_indented_if_multi_line(new_line_group(generated_expr), context.config.indent_width)
        } else {
          generated_expr
        });
      }

      // indent the expression relative to the line it's on in the template's text instead of the code
      if let Some(line_indent) = line_indent {
        line_indent.start(&mut items);
        expr_line_indent = Some(line_indent);
      }
      items.extend(expr_items);
    }
    is_head = false;
  }
  return items;

  /// The indentation of a line of a template literal's text.
  #[derive(Clone, Copy)]
  struct TplLineIndent {
    indent_levels: u8,
    align_columns: u8,
  }

  impl TplLineIndent {
    fn new(line_text: &str, indent_width: u8) -> Self {
      let indent_width = std::cmp::max(indent_width, 1) as u32;
      let columns = line_text
        .chars()
        .map_while(|c| match c {
          ' ' => Some(1),
          '\t' => Some(indent_width),
          _ => None,
        })
        .sum::<u32>();
      TplLineIndent {
        indent_levels: std::cmp::min(columns / indent_width, u8::MAX as u32) as u8,
        align_columns: (columns % indent_width) as u8,
      }
    }

    fn start(&self, items: &mut PrintItems) {
      items.push_signal(Signal::StartResettingIndent);
      for _ in 0..self.indent_levels {
        items.push_signal(Signal::StartIndent);
      }
      if self.align_columns > 0 {
        items.push_signal(Signal::StartAlign(self.align_columns));
      }
    }

    fn finish(&self, items: &mut PrintItems) {
      if self.align_columns > 0 {
        items.push_signal(Signal::FinishAlign);
      }
      for _ in 0..self.indent_levels {
        items.push_signal(Signal::FinishIndent);
      }
      items.push_signal(Signal::FinishResettingIndent);
    }
  }

  /// Gets the quasis and expressions in the order they're in the text, without allocating.
  fn get_nodes<'a>(quasis: impl Iterator<Item = Node<'a>>, exprs: impl Iterator<Item = Node<'a>>) -> impl Iterator<Item = Node<'a>> {
    let mut quasis = quasis.peekable();
    let mut exprs = exprs.peekable();
    std::iter::from_fn(move || match (quasis.peek(), exprs.peek()) {
      (Some(quasi), Some(expr)) => {
        if quasi.start() < expr.start() {
          quasis.next()
        } else {
          exprs.next()
        }
      }
      (Some(_), None) => quasis.next(),
      (None, _) => exprs.next(),
    })
  }

  // handle this on a case by case basis for now
  fn get_keep_on_one_line(node: Node) -> bool {
    match node {
      Node::IdentifierReference(_)
      | Node::IdentifierName(_)
      | Node::ThisExpression(_)
      | Node::ImportMeta(_)
      | Node::NewTarget(_)
      | Node::StringLiteral(_)
      | Node::PrivateIdentifier(_) => true,
      Node::TSAnyKeyword(_)
      | Node::TSBigIntKeyword(_)
      | Node::TSBooleanKeyword(_)
      | Node::TSIntrinsicKeyword(_)
      | Node::TSNeverKeyword(_)
      | Node::TSNullKeyword(_)
      | Node::TSNumberKeyword(_)
      | Node::TSObjectKeyword(_)
      | Node::TSStringKeyword(_)
      | Node::TSSymbolKeyword(_)
      | Node::TSUndefinedKeyword(_)
      | Node::TSUnknownKeyword(_)
      | Node::TSVoidKeyword(_)
      | Node::TSThisType(_)
      | Node::TSQualifiedName(_) => true,
      Node::TSLiteralType(lit_type) => !matches!(lit_type.literal, TSLiteral::TemplateLiteral(_)),
      Node::TSTypeReference(type_ref) => type_ref.type_arguments.is_none(),
      Node::TSTypeQuery(type_query) => type_query.type_arguments.is_none() && !matches!(type_query.expr_name, TSTypeQueryExprName::TSImportType(_)),
      Node::TSTypeOperator(type_operator) => get_keep_on_one_line((&type_operator.type_annotation).into_node()),
      Node::ChainExpression(expr) => get_keep_on_one_line((&expr.expression).into_node()),
      Node::StaticMemberExpression(_) | Node::ComputedMemberExpression(_) | Node::PrivateFieldExpression(_) => keep_member_expr_on_one_line(node),
      Node::CallExpression(expr) => keep_call_expr_on_one_line(expr),
      _ => false,
    }
  }

  fn get_possible_surround_newlines(node: Node) -> bool {
    match node {
      Node::ChainExpression(expr) => get_possible_surround_newlines((&expr.expression).into_node()),
      Node::ConditionalExpression(_) | Node::BinaryExpression(_) | Node::LogicalExpression(_) | Node::PrivateInExpression(_) => true,
      Node::TSConditionalType(_) | Node::TSUnionType(_) | Node::TSIntersectionType(_) | Node::TSIndexedAccessType(_) => true,
      Node::TSTypeReference(_) | Node::TSTypeQuery(_) => !get_keep_on_one_line(node),
      Node::TSTypeOperator(type_operator) => get_possible_surround_newlines((&type_operator.type_annotation).into_node()),
      Node::StaticMemberExpression(_) | Node::ComputedMemberExpression(_) | Node::PrivateFieldExpression(_) => !keep_member_expr_on_one_line(node),
      Node::CallExpression(expr) => !keep_call_expr_on_one_line(expr),
      _ => false,
    }
  }

  fn keep_member_expr_on_one_line(node: Node) -> bool {
    match node {
      // a property of `super` (swc's `SuperPropExpr`) is always kept on one line
      Node::StaticMemberExpression(expr) => matches!(expr.object, Expression::Super(_)) || get_keep_on_one_line((&expr.object).into_node()),
      Node::PrivateFieldExpression(expr) => matches!(expr.object, Expression::Super(_)) || get_keep_on_one_line((&expr.object).into_node()),
      Node::ComputedMemberExpression(expr) => matches!(expr.object, Expression::Super(_)),
      _ => false,
    }
  }

  fn keep_call_expr_on_one_line(expr: &CallExpression) -> bool {
    expr.arguments.is_empty() && get_keep_on_one_line((&expr.callee).into_node())
  }
}

pub(super) fn gen_type_assertion<'a>(node: &'a TSTypeAssertion<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  items.push_sc(sc!("<"));
  items.extend(gen_node((&node.type_annotation).into_node(), context));
  items.push_sc(sc!(">"));
  if context.config.type_assertion_space_before_expression {
    items.push_space();
  }
  items.extend(gen_node((&node.expression).into_node(), context));
  items
}

pub(super) fn gen_unary_expr<'a>(node: &'a UnaryExpression<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  items.push_sc(get_operator_text(node.operator));
  if is_argument_same_sign(node) {
    // keep `- -1` from becoming `--1`
    items.push_space();
  }
  items.extend(gen_node((&node.argument).into_node(), context));

  if get_should_use_parens(node, context) {
    items = surround_with_parens(items);
  }

  return items;

  fn get_operator_text(op: UnaryOperator) -> &'static StringContainer<'static> {
    match op {
      UnaryOperator::Void => sc!("void "),
      UnaryOperator::Typeof => sc!("typeof "),
      UnaryOperator::Delete => sc!("delete "),
      UnaryOperator::LogicalNot => sc!("!"),
      UnaryOperator::UnaryPlus => sc!("+"),
      UnaryOperator::UnaryNegation => sc!("-"),
      UnaryOperator::BitwiseNot => sc!("~"),
    }
  }

  /// Gets if the argument starts with the same sign as the operator (ex. `- -1` or `+ ++a`).
  fn is_argument_same_sign(node: &UnaryExpression) -> bool {
    match (node.operator, &node.argument) {
      (UnaryOperator::UnaryNegation, Expression::UnaryExpression(arg)) => arg.operator == UnaryOperator::UnaryNegation,
      (UnaryOperator::UnaryPlus, Expression::UnaryExpression(arg)) => arg.operator == UnaryOperator::UnaryPlus,
      (UnaryOperator::UnaryNegation, Expression::UpdateExpression(arg)) => arg.prefix && arg.operator == UpdateOperator::Decrement,
      (UnaryOperator::UnaryPlus, Expression::UpdateExpression(arg)) => arg.prefix && arg.operator == UpdateOperator::Increment,
      _ => false,
    }
  }

  fn get_should_use_parens<'a>(node: &'a UnaryExpression<'a>, context: &Context<'a>) -> bool {
    if let Some(Node::BinaryExpression(parent)) = node.into_node().parent(context.program) {
      return matches!(parent.operator, BinaryOperator::In | BinaryOperator::Instanceof);
    }
    false
  }
}

pub(super) fn gen_update_expr<'a>(node: &'a UpdateExpression<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  let operator_text = get_operator_text(node.operator);
  if node.prefix {
    items.push_sc(operator_text);
  }
  items.extend(gen_argument(&node.argument, context));
  if !node.prefix {
    items.push_sc(operator_text);
  }
  return items;

  fn get_operator_text(operator: UpdateOperator) -> &'static StringContainer<'static> {
    match operator {
      UpdateOperator::Decrement => sc!("--"),
      UpdateOperator::Increment => sc!("++"),
    }
  }

  /// oxc doesn't keep the parens of `(val as number)++` or `(<number>val)++` in the ast
  /// (swc had a paren expr that was kept), so they're generated here.
  fn gen_argument<'a>(argument: &'a SimpleAssignmentTarget<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
    let argument_node = argument.into_node();
    if !matches!(
      argument,
      SimpleAssignmentTarget::TSAsExpression(_) | SimpleAssignmentTarget::TSSatisfiesExpression(_) | SimpleAssignmentTarget::TSTypeAssertion(_)
    ) {
      return gen_node(argument_node, context);
    }

    new_line_group(
      conditions::with_indent_if_start_of_line_indented(gen_node_in_parens(
        |context| gen_node(argument_node, context),
        GenNodeInParensOptions {
          inner_range: argument_node.range(),
          prefer_hanging: true,
          allow_open_paren_trailing_comments: true,
          single_line_space_around: context.config.paren_expression_space_around,
        },
        context,
      ))
      .into_print_items(context.arena),
    )
  }
}

pub(super) fn gen_yield_expr<'a>(node: &'a YieldExpression<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  items.push_sc(sc!("yield"));
  if node.delegate {
    items.push_sc(sc!("*"));
  }
  if let Some(arg) = &node.argument {
    items.push_space();
    items.extend(gen_node(arg.into_node(), context));
  }
  items
}

/* exports */

// gen_export_named_specifier is in declarations.rs

/* imports */

pub(super) fn gen_import_named_specifier<'a>(node: &'a ImportSpecifier<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);

  let is_parent_type_only = match node.into_node().parent(context.program) {
    Some(Node::ImportDeclaration(parent)) => parent.import_kind.is_type(),
    _ => false,
  };
  if node.import_kind.is_type() && !is_parent_type_only {
    items.push_sc(sc!("type "));
  }

  // the imported and local names are the same node when there's no alias
  if node.imported.range() != node.local.range() {
    items.extend(gen_node((&node.imported).into_node(), context));
    items.push_signal(Signal::SpaceOrNewLine);
    items.push_condition(conditions::indent_if_start_of_line({
      let mut items = PrintItems::new(context.arena);
      items.push_sc(sc!("as "));
      items.extend(gen_node((&node.local).into_node(), context));
      items
    }));
  } else {
    items.extend(gen_node((&node.local).into_node(), context));
  }

  items
}

pub(super) fn gen_import_namespace_specifier<'a>(node: &'a ImportNamespaceSpecifier<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  items.push_sc(sc!("* as "));
  items.extend(gen_node((&node.local).into_node(), context));
  items
}

pub(super) fn gen_external_module_ref<'a>(node: &'a TSExternalModuleReference<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  // force everything on a single line
  let mut items = PrintItems::new(context.arena);
  items.push_sc(sc!("require("));
  items.extend(gen_node((&node.expression).into_node(), context));
  items.push_sc(sc!(")"));
  items
}
