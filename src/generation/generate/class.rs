use super::*;

/* class */

pub(super) fn gen_class_method<'a>(node: &'a MethodDefinition<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  // handles methods, getters, setters, constructors, private methods, and abstract methods
  let func: &'a Function<'a> = &node.value;
  let is_constructor = matches!(node.kind, MethodDefinitionKind::Constructor);
  gen_class_or_object_method(
    ClassOrObjectMethod {
      node: node.into_node(),
      parameters_range: func.get_parameters_range(context),
      decorators: if is_constructor { None } else { Some(node.decorators.as_slice()) },
      accessibility: node.accessibility,
      is_static: node.r#static,
      is_async: func.r#async,
      is_abstract: matches!(node.r#type, MethodDefinitionType::TSAbstractMethodDefinition),
      kind: match node.kind {
        MethodDefinitionKind::Constructor => ClassOrObjectMethodKind::Constructor,
        MethodDefinitionKind::Method => ClassOrObjectMethodKind::Method,
        MethodDefinitionKind::Get => ClassOrObjectMethodKind::Getter,
        MethodDefinitionKind::Set => ClassOrObjectMethodKind::Setter,
      },
      is_generator: func.generator,
      is_optional: node.optional,
      is_override: node.r#override,
      computed: node.computed,
      key: (&node.key).into_node(),
      type_params: func.type_parameters.as_ref().map(|x| x.into_node()),
      params: get_function_params(func, context),
      return_type: func.return_type.as_ref().map(|x| x.into_node()),
      body: func.body.as_ref().map(|x| x.into_node()),
    },
    context,
  )
}

pub(super) fn gen_auto_accessor<'a>(node: &'a AccessorProperty<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  gen_class_prop_common(
    GenClassPropCommon {
      original: node.into_node(),
      key: (&node.key).into_node(),
      value: node.value.as_ref(),
      type_ann: node.type_annotation.as_deref(),
      is_static: node.r#static,
      decorators: &node.decorators,
      computed: node.computed,
      is_auto_accessor: true,
      is_declare: false,
      accessibility: node.accessibility,
      is_abstract: matches!(node.r#type, AccessorPropertyType::TSAbstractAccessorProperty),
      is_optional: false,
      is_override: node.r#override,
      readonly: false,
      definite: node.definite,
    },
    context,
  )
}

pub(super) fn gen_class_prop<'a>(node: &'a PropertyDefinition<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  // handles public and private (ex. `#prop`) properties
  gen_class_prop_common(
    GenClassPropCommon {
      original: node.into_node(),
      key: (&node.key).into_node(),
      value: node.value.as_ref(),
      type_ann: node.type_annotation.as_deref(),
      is_static: node.r#static,
      decorators: &node.decorators,
      computed: node.computed,
      is_auto_accessor: false,
      is_declare: node.declare,
      accessibility: node.accessibility,
      is_abstract: matches!(node.r#type, PropertyDefinitionType::TSAbstractPropertyDefinition),
      is_optional: node.optional,
      is_override: node.r#override,
      readonly: node.readonly,
      definite: node.definite,
    },
    context,
  )
}

pub(super) fn gen_decorator<'a>(node: &'a Decorator<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  items.push_sc(sc!("@"));
  // the parser keeps the parens of `@(expr)`, but they're skipped when generating (see
  // is_decorator_paren_expr) so they're added back when the expression isn't valid without
  // them (a paren expr inside them is from extra parens and handles itself)
  let expr = skip_decorator_paren_expr(&node.expression, context.program);
  let needs_parens = !matches!(expr, Expression::ParenthesizedExpression(_)) && !is_bare_decorator_expr(&node.expression, context.program);
  if needs_parens {
    items.push_sc(sc!("("));
  }
  items.extend(gen_node((&node.expression).into_node(), context));
  if needs_parens {
    items.push_sc(sc!(")"));
  }
  items
}

/// Gets if the expression can be used in a decorator without parens, which is an identifier,
/// a chain of property accesses on one (ex. `a.b.#c`), or such a chain followed by a call.
///
/// The parens directly after the `@` (ex. `@(a.b)`) are looked through because they're
/// skipped when generating (see `is_decorator_paren_expr`).
pub(super) fn is_bare_decorator_expr<'a>(expr: &'a Expression<'a>, program: ProgramInfo<'a>) -> bool {
  return match skip_decorator_paren_expr(expr, program) {
    Expression::CallExpression(call_expr) => is_member_chain(&call_expr.callee, false, program),
    expr => is_member_chain(expr, false, program),
  };

  fn is_member_chain<'a>(expr: &'a Expression<'a>, is_member_obj: bool, program: ProgramInfo<'a>) -> bool {
    match expr {
      Expression::Identifier(_) => true,
      Expression::StaticMemberExpression(member_expr) => is_member_chain(&member_expr.object, true, program),
      Expression::PrivateFieldExpression(member_expr) => is_member_chain(&member_expr.object, true, program),
      // the parens directly after the `@` are skipped when generating
      Expression::ParenthesizedExpression(paren_expr) if is_decorator_paren_expr(paren_expr, program) => {
        is_member_chain(&paren_expr.expression, is_member_obj, program)
      }
      // these parens are removed when generating (see should_skip_paren_expr)
      Expression::ParenthesizedExpression(paren_expr) => match &paren_expr.expression {
        Expression::Identifier(_) => true,
        Expression::StaticMemberExpression(_) | Expression::PrivateFieldExpression(_) if is_member_obj => {
          is_member_chain(&paren_expr.expression, true, program)
        }
        _ => false,
      },
      _ => false,
    }
  }
}

/// Gets if the paren expr directly follows the `@` of a decorator (ex. `@(a.b)` or `@(a)(b)`).
///
/// swc didn't keep these parens, so they should always be skipped when generating. The decorator
/// adds parens back when the expression needs them, and a paren expr directly inside these ones
/// (ex. `@((a.b))`) should be treated as the decorator's expression (it's skipped when
/// `is_bare_decorator_expr` is true for its expression).
pub(super) fn is_decorator_paren_expr<'a>(node: &ParenthesizedExpression<'a>, program: ProgramInfo<'a>) -> bool {
  node.previous_token_fast(program).is_some_and(|token| token.kind == TokenKind::At)
}

/// Skips the parens directly after the `@` of a decorator, which swc didn't keep.
pub(super) fn skip_decorator_paren_expr<'a>(expr: &'a Expression<'a>, program: ProgramInfo<'a>) -> &'a Expression<'a> {
  match expr {
    Expression::ParenthesizedExpression(paren_expr) if is_decorator_paren_expr(paren_expr, program) => &paren_expr.expression,
    _ => expr,
  }
}

/// Generates a parameter that's also a property (ex. `private readonly a: string` in a constructor).
pub(super) fn gen_parameter_prop<'a>(node: &'a FormalParameter<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  items.extend(gen_decorators(&node.decorators, DecoratorsSeparator::Space, context));
  if let Some(accessibility) = node.accessibility {
    items.push_str_runtime_width_computed(accessibility_to_str(accessibility));
    items.push_space();
  }
  if node.r#override {
    items.push_sc(sc!("override "));
  }
  if node.readonly {
    items.push_sc(sc!("readonly "));
  }
  items.extend(gen_param_without_decorators(node, context));
  items
}

pub(super) fn gen_private_name<'a>(node: &'a PrivateIdentifier<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  items.push_sc(sc!("#"));
  items.push_str_runtime_width_computed(node.name.as_str());
  items
}

pub(super) struct GenClassPropCommon<'a> {
  pub original: Node<'a>,
  pub key: Node<'a>,
  pub value: Option<&'a Expression<'a>>,
  pub type_ann: Option<&'a TSTypeAnnotation<'a>>,
  pub is_static: bool,
  pub decorators: &'a [Decorator<'a>],
  pub computed: bool,
  pub is_declare: bool,
  pub accessibility: Option<TSAccessibility>,
  pub is_auto_accessor: bool,
  pub is_abstract: bool,
  pub is_optional: bool,
  pub is_override: bool,
  pub readonly: bool,
  pub definite: bool,
}

pub(super) fn gen_class_prop_common<'a>(node: GenClassPropCommon<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  items.extend(gen_decl_decorators(node.decorators, context));
  if node.is_declare {
    items.push_sc(sc!("declare "));
  }
  if let Some(accessibility) = node.accessibility {
    items.push_str_runtime_width_computed(accessibility_to_str(accessibility));
    items.push_space();
  }
  if node.is_static {
    items.push_sc(sc!("static "));
  }
  if node.is_abstract {
    items.push_sc(sc!("abstract "));
  }
  if node.is_override {
    items.push_sc(sc!("override "));
  }
  if node.readonly {
    items.push_sc(sc!("readonly "));
  }
  if node.is_auto_accessor {
    items.push_sc(sc!("accessor "));
  }
  items.extend(if node.computed {
    if node.is_auto_accessor {
      // swc generated an auto accessor's computed key as a node, which handles its comments
      gen_computed_prop_name(node.key, context)
    } else {
      // the key is the expression within the brackets
      let inner_key_node = node.key;
      gen_computed_prop_like(
        |context| gen_node(inner_key_node, context),
        GenComputedPropLikeOptions {
          inner_node_range: inner_key_node.range(),
        },
        context,
      )
    }
  } else {
    gen_node(node.key, context)
  });
  if node.is_optional {
    items.push_sc(sc!("?"));
  }
  if node.definite {
    items.push_sc(sc!("!"));
  }
  items.extend(gen_type_ann_with_colon_if_exists(node.type_ann, context));

  if let Some(value) = node.value {
    items.extend(gen_assignment(value.into_node(), sc!("="), context));
  }

  // the empty members after the property are removed, so what's after them is the next member
  let should_semi = context.config.semicolons.is_true()
    || matches!(
      node
        .original
        .next_tokens_fast(context.program)
        .iter()
        .find(|token| token.kind != TokenKind::Semicolon),
      Some(TokenAndSpan {
        kind: TokenKind::LBrack | TokenKind::Star,
        ..
      })
    );
  if should_semi {
    items.push_sc(sc!(";"));
  }

  items
}

pub(super) fn gen_static_block<'a>(node: &'a StaticBlock<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  let start_header_lsil = LineStartIndentLevel::new("staticBlockStart");
  items.push_info(start_header_lsil);
  items.push_sc(sc!("static"));

  // oxc has no block statement node for the body, so generate the block here
  // like a block statement (see gen_block_stmt)
  let open_brace_token = context.token_finder.get_first_open_brace_token_within(node);
  items.extend(gen_brace_separator(
    GenBraceSeparatorOptions {
      brace_position: context.config.static_block_brace_position,
      open_brace_token,
      start_header_lsil: Some(start_header_lsil),
    },
    context,
  ));

  let block_range = SourceRange::new(open_brace_token.map(|t| t.start()).unwrap_or_else(|| node.start()), node.end());
  items.extend(gen_range_with_comments(
    block_range,
    |context| {
      gen_block(
        |stmts, context| gen_statements(SourceRange::new(block_range.start + 1, block_range.end - 1), stmts, context),
        GenBlockOptions {
          range: Some(block_range),
          children: node.body.iter().map(|x| x.into_node()).collect(),
          block_padding: context.config.static_block_block_padding,
          keep_blank_lines_before_close_token_comments: true,
        },
        context,
      )
    },
    context,
  ));

  items
}

/* clauses */

pub(super) fn gen_catch_clause<'a>(node: &'a CatchClause<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  // a bit overkill since the param will currently always just be an identifer
  let start_header_ln = LineNumber::new("catchClauseHeaderStart");
  let start_header_lsil = LineStartIndentLevel::new("catchClauseHeaderStart");
  let end_header_ln = LineNumber::new("catchClauseHeaderEnd");
  let mut items = PrintItems::new(context.arena);

  items.push_info(start_header_ln);
  items.push_info(start_header_lsil);
  items.push_sc(sc!("catch"));

  if let Some(param) = &node.param {
    let open_paren_token = context.token_finder.get_previous_token_if_open_paren(&param.range());
    if use_space_after_keyword(context.config.catch_clause_space_after_catch_keyword, open_paren_token, context) {
      items.push_space();
    }
    if let Some(open_paren_token) = open_paren_token {
      items.extend(gen_leading_comments(&open_paren_token.range(), context));
    }
    items.push_sc(sc!("("));
    if context.config.catch_clause_space_around {
      items.push_space();
    }
    items.extend(gen_node(param.into_node(), context));
    if context.config.catch_clause_space_around {
      items.push_space();
    }
    items.push_sc(sc!(")"));
  }
  items.push_info(end_header_ln);

  let has_finalizer = match node.into_node().parent(context.program) {
    Some(Node::TryStatement(try_stmt)) => try_stmt.finalizer.is_some(),
    _ => false,
  };
  let single_body_position = if has_finalizer { Some(SameOrNextLinePosition::NextLine) } else { None };

  // not conditional... required
  items.extend(
    gen_conditional_brace_body(
      GenConditionalBraceBodyOptions {
        body_node: (&node.body).into_node(),
        use_braces: UseBraces::Always,
        brace_position: context.config.try_statement_brace_position,
        block_padding: context.config.try_statement_block_padding,
        single_body_position,
        if_chain_branch: None,
        start_header_info: Some((start_header_ln, start_header_lsil)),
        end_header_info: Some(end_header_ln),
      },
      context,
    )
    .generated_node,
  );

  items
}

pub(super) fn gen_catch_parameter<'a>(node: &'a CatchParameter<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  items.extend(gen_node((&node.pattern).into_node(), context));
  items.extend(gen_type_ann_with_colon_if_exists(node.type_annotation.as_deref(), context));
  items
}

/* common */

/// Generates a computed property name (ex. `[a]`) given the expression within its brackets.
///
/// oxc doesn't have a node for the brackets, so the brackets are found from the tokens
/// around the key and their comments are handled the way a node's would be.
pub(super) fn gen_computed_prop_name<'a>(key: Node<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let open_bracket = context.token_finder.get_previous_token_if_open_bracket(&key);
  let close_bracket = context.token_finder.get_next_token_if_close_bracket(&key);
  let range = match (open_bracket, close_bracket) {
    (Some(open_bracket), Some(close_bracket)) => SourceRange::new(open_bracket.start(), close_bracket.end()),
    _ => key.range(),
  };
  gen_range_with_comments(
    range,
    |context| {
      gen_computed_prop_like(
        |context| gen_node(key, context),
        GenComputedPropLikeOptions { inner_node_range: key.range() },
        context,
      )
    },
    context,
  )
}

/// Generates the leading and trailing comments of a range the same way `gen_node`
/// does for a node, which is for syntax that was a node in swc, but isn't in oxc.
pub(super) fn gen_range_with_comments<'a>(
  range: SourceRange,
  gen_inner: impl FnOnce(&mut Context<'a>) -> PrintItems<'a>,
  context: &mut Context<'a>,
) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  let leading_comments = context.comments.leading_comments_with_previous(range.start);
  items.extend(gen_comments_as_leading(&range, leading_comments, context));
  items.extend(gen_inner(context));
  // the current node is what would have been the parent node
  let parent = context.current_node;
  if range.end != parent.end() || parent.kind() == NodeKind::Program {
    let trailing_comments = context.comments.trailing_comments_with_previous(range.end);
    items.extend(gen_comments_as_trailing(&range, trailing_comments, context));
  }
  items
}

pub(super) fn gen_identifier<'a>(node: &'a IdentifierReference<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  // note: swc stored whether an identifier was optional (ex. `a?: string`), but oxc stores it on
  // the parent (ex. FormalParameter), which generates the `?`
  let mut items = PrintItems::new(context.arena);
  items.push_str_runtime_width_computed(node.name.as_str());
  items
}

pub(super) fn gen_ident_name<'a>(node: &'a IdentifierName<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  items.push_str_runtime_width_computed(node.name.as_str());
  items
}

pub(super) fn gen_binding_identifier<'a>(node: &'a BindingIdentifier<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  // note: swc's binding identifier included the definite assignment (`!`) and the type
  // annotation, which oxc stores on the parent (ex. VariableDeclarator, FormalParameter)
  let mut items = PrintItems::new(context.arena);
  items.push_str_runtime_width_computed(node.name.as_str());
  items
}

pub(super) fn gen_label_identifier<'a>(node: &'a LabelIdentifier<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  items.push_str_runtime_width_computed(node.name.as_str());
  items
}
