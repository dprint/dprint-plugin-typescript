use super::*;

/* types */

pub(super) fn gen_array_type<'a>(node: &'a TSArrayType<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  items.extend(gen_node((&node.element_type).into_node(), context));
  items.push_sc(sc!("[]"));
  items
}

pub(super) fn gen_conditional_type<'a>(node: &'a TSConditionalType<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let top_most_data = get_top_most_data(node, context);
  let is_parent_conditional_type = node
    .into_node()
    .parent(context.program)
    .is_some_and(|p| p.kind() == NodeKind::TSConditionalType);
  let line_per_expression = context.config.conditional_type_line_per_expression;
  let is_flat_line_per_expression = line_per_expression && !context.config.conditional_type_use_nested_indentation;
  let use_new_lines_for_nested_conditional = context.config.conditional_type_use_nested_indentation && {
    is_parent_conditional_type || matches!(node.true_type, TSType::TSConditionalType(_)) || matches!(node.false_type, TSType::TSConditionalType(_))
  };
  let prefer_single_line = context.config.conditional_type_prefer_single_line;
  let has_newline_extends_true = node_helpers::get_use_new_lines_for_nodes(&node.extends_type, &node.true_type, context.program);
  let has_newline_true_false = node_helpers::get_use_new_lines_for_nodes(&node.true_type, &node.false_type, context.program);
  // only forced for nested indentation, which otherwise keeps the true type on the extends type's line when it fits
  let force_newline_extends_true = has_newline_extends_true && use_new_lines_for_nested_conditional;
  let force_newline_true_false = has_newline_true_false && (!prefer_single_line || use_new_lines_for_nested_conditional);

  let mut items = PrintItems::new(context.arena);
  let before_false_ln = LineNumber::new("beforeFalse");
  let question_token = context.token_finder.get_first_operator_after(&node.extends_type, "?").unwrap();
  let colon_token = context.token_finder.get_first_operator_after(&node.true_type, ":").unwrap();
  let (question_position, colon_position) = get_operator_position(node, question_token, colon_token, context);
  // the false type keeps the indentation of a parent conditional type unless using nested indentation
  let is_false_type_indented = use_new_lines_for_nested_conditional || !is_parent_conditional_type;
  let question_comment_items = gen_cond_token_comments(question_token, context, Some(top_most_data.il));
  let colon_comment_items = gen_cond_token_comments(colon_token, context, is_false_type_indented.then_some(top_most_data.il));

  // main area
  items.extend(ir_helpers::new_line_group(gen_node((&node.check_type).into_node(), context)));
  items.push_sc(sc!(" extends")); // do not newline before because it's a parsing error
  items.push_signal(Signal::SpaceOrNewLine);

  if top_most_data.is_top_most {
    items.push_info(top_most_data.ln);
    items.push_info(top_most_data.il);
  }

  items.push_condition(conditions::indent_if_start_of_line(ir_helpers::new_line_group({
    let mut items = gen_node((&node.extends_type).into_node(), context);
    if question_position == OperatorPosition::SameLine {
      items.push_sc(sc!(" ?"));
    }
    items.extend(question_comment_items.trailing_line);
    items
  })));

  if !question_comment_items.previous_lines.is_empty() {
    items.extend(question_comment_items.previous_lines);
  } else if line_per_expression {
    items.push_condition(conditions::new_line_if_multiple_lines_space_or_new_line_otherwise(
      context.arena,
      top_most_data.ln,
      Some(before_false_ln),
    ));
  } else if force_newline_extends_true {
    items.push_signal(Signal::NewLine);
  } else {
    items.push_signal(Signal::SpaceOrNewLine);
  }

  items.push_condition({
    let inner_items = {
      let mut items = PrintItems::new(context.arena);
      items.extend(question_comment_items.leading_line);
      if question_position == OperatorPosition::NextLine {
        items.push_sc(sc!("?"));
        items.push_signal(Signal::SpaceIfNotTrailing);
      }
      items.extend(ir_helpers::new_line_group(gen_node((&node.true_type).into_node(), context)));
      if colon_position == OperatorPosition::SameLine {
        items.push_sc(sc!(" :"));
      }
      items.extend(colon_comment_items.trailing_line);
      items
    }
    .into_rc_path();

    if_true_or(
      "isStartOfLineAndTopLevelOrNestedConditionalIndentElseQueue",
      context.arena.alloc_resolver(move |context| {
        // a line per expression without nested indentation keeps a nested conditional type flat
        Some(context.writer_info.is_start_of_line() && !(is_flat_line_per_expression && is_parent_conditional_type))
      }),
      with_indent(inner_items.into_print_items(context.arena)),
      with_queued_indent(inner_items.into_print_items(context.arena)),
    )
  });

  items.extend(colon_comment_items.previous_lines);

  // false type
  if force_newline_true_false {
    items.push_signal(Signal::NewLine);
  } else {
    items.push_condition(conditions::new_line_if_multiple_lines_space_or_new_line_otherwise(
      context.arena,
      top_most_data.ln,
      Some(before_false_ln),
    ));
  }

  // add any preceeding comments of the colon token
  {
    let comment_items = gen_leading_comments_on_previous_lines(&colon_token.range(), context);
    if !comment_items.is_empty() {
      items.extend(with_indent(comment_items));
    }
  }

  let false_type_generated = {
    let mut items = PrintItems::new(context.arena);
    items.push_info(before_false_ln);
    items.extend(colon_comment_items.leading_line);
    if colon_position == OperatorPosition::NextLine {
      items.push_sc(sc!(":"));
      items.push_signal(Signal::SpaceIfNotTrailing);
    }
    items.extend(ir_helpers::new_line_group(gen_node((&node.false_type).into_node(), context)));
    items
  };

  // Unless `use_nested_indentation` is enabled, we keep the indentation the same. e.g.:
  // type A<T> = T extends string ? 'string'
  //     : T extends number ? 'number'
  //     : T extends boolean ? 'boolean';
  if !is_false_type_indented {
    items.extend(false_type_generated);
  } else {
    items.push_condition(conditions::indent_if_start_of_line(false_type_generated));
  }

  return items;

  struct TopMostData {
    ln: LineNumber,
    il: IndentLevel,
    is_top_most: bool,
  }

  fn get_top_most_data<'a>(node: &'a TSConditionalType<'a>, context: &mut Context<'a>) -> TopMostData {
    // todo: consolidate with conditional expression
    // The "top most" node in nested conditionals follows the ancestors up through
    // the false expressions.
    let mut top_most_node = node;

    for ancestor in context.parent_stack.iter() {
      if let Node::TSConditionalType(parent) = *ancestor {
        if parent.false_type.start() == top_most_node.start() {
          top_most_node = parent;
        } else {
          break;
        }
      } else {
        break;
      }
    }

    let is_top_most = top_most_node.range() == node.range();
    let (ln, il) = get_or_set_top_most_ln(top_most_node.start(), is_top_most, context);

    return TopMostData { is_top_most, ln, il };

    fn get_or_set_top_most_ln<'a>(top_most_expr_start: SourcePos, is_top_most: bool, context: &mut Context<'a>) -> (LineNumber, IndentLevel) {
      if is_top_most {
        let ln = LineNumber::new("conditionalTypeStart");
        let il = IndentLevel::new("conditionalTypeStart");
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
    node: &TSConditionalType<'a>,
    question_token: &TokenAndSpan,
    colon_token: &TokenAndSpan,
    context: &mut Context<'a>,
  ) -> (OperatorPosition, OperatorPosition) {
    fn get_maintain_position<'a>(node: &TSConditionalType<'a>, ts_type: &TSType<'a>, token: &TokenAndSpan, context: &mut Context<'a>) -> OperatorPosition {
      if ts_type.end_line_fast(context.program) == token.start_line_fast(context.program) {
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

    match context.config.conditional_type_operator_position {
      OperatorPosition::NextLine => (OperatorPosition::NextLine, OperatorPosition::NextLine),
      OperatorPosition::SameLine => (OperatorPosition::SameLine, OperatorPosition::SameLine),
      OperatorPosition::Maintain => (
        get_maintain_position(node, &node.extends_type, question_token, context),
        get_maintain_position(node, &node.true_type, colon_token, context),
      ),
    }
  }
}

pub(super) fn gen_constructor_type<'a>(node: &'a TSConstructorType<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let start_lsil = LineStartIndentLevel::new("startConstructorType");
  let mut items = PrintItems::new(context.arena);
  items.push_info(start_lsil);
  if node.r#abstract {
    items.push_sc(sc!("abstract "));
  }
  items.push_sc(sc!("new"));
  if context.config.constructor_type_space_after_new_keyword {
    items.push_space();
  }
  if let Some(type_params) = &node.type_parameters {
    items.extend(gen_node(type_params.into_node(), context));
  }

  let param_nodes = get_type_fn_param_nodes(None, &node.params, context);
  let param_count = param_nodes.len();
  items.extend(gen_parameters_or_arguments(
    GenParametersOrArgumentsOptions {
      node: node.into_node(),
      range: node.get_parameters_range(context),
      nodes: param_nodes,
      custom_close_paren: |context| {
        Some(gen_close_paren_with_type(
          GenCloseParenWithTypeOptions {
            start_lsil,
            // the return type's range includes the `=>`, like in swc
            type_node: Some((&node.return_type).into_node()),
            type_node_separator: Some({
              let mut items = PrintItems::new(context.arena);
              items.push_sc(sc!(" =>"));
              items.push_signal(Signal::SpaceIfNotTrailing);
              items.push_signal(Signal::PossibleNewLine);
              items
            }),
            param_count,
          },
          context,
        ))
      },
      is_parameters: true,
    },
    context,
  ));

  items
}

pub(super) fn gen_function_type<'a>(node: &'a TSFunctionType<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let start_lsil = LineStartIndentLevel::new("startFunctionType");
  let mut items = PrintItems::new(context.arena);
  let mut indent_after_arrow_condition = if_true(
    "indentIfIsStartOfLineAfterArrow",
    condition_resolvers::is_start_of_line(),
    Signal::StartIndent.into_print_items(context.arena),
  );
  let indent_after_arrow_condition_ref = indent_after_arrow_condition.create_reference();

  items.push_info(start_lsil);
  if let Some(type_params) = &node.type_parameters {
    items.extend(gen_node(type_params.into_node(), context));
  }
  let param_nodes = get_type_fn_param_nodes(node.this_param.as_deref(), &node.params, context);
  let param_count = param_nodes.len();
  items.extend(gen_parameters_or_arguments(
    GenParametersOrArgumentsOptions {
      node: node.into_node(),
      range: node.get_parameters_range(context),
      nodes: param_nodes,
      custom_close_paren: |context| {
        Some(gen_close_paren_with_type(
          GenCloseParenWithTypeOptions {
            start_lsil,
            // the return type's range includes the `=>`, like in swc
            type_node: Some((&node.return_type).into_node()),
            type_node_separator: {
              let mut items = PrintItems::new(context.arena);
              items.push_sc(sc!(" =>"));
              items.push_signal(Signal::SpaceIfNotTrailing);
              items.push_signal(Signal::PossibleNewLine);
              items.push_condition(indent_after_arrow_condition);
              Some(items)
            },
            param_count,
          },
          context,
        ))
      },
      is_parameters: true,
    },
    context,
  ));

  items.push_condition(if_true(
    "shouldFinishIndent",
    context
      .arena
      .alloc_resolver(move |context| context.resolved_condition(&indent_after_arrow_condition_ref)),
    Signal::FinishIndent.into_print_items(context.arena),
  ));

  items
}

/// Gets the parameters of a function or constructor type as nodes, which
/// includes the `this` parameter and the rest parameter.
pub(super) fn get_type_fn_param_nodes<'a>(
  this_param: Option<&'a TSThisParameter<'a>>,
  params: &'a FormalParameters<'a>,
  context: &Context<'a>,
) -> BumpVec<'a, Node<'a>> {
  let mut nodes = BumpVec::with_capacity_in(params.items.len() + 2, context.arena.bump());
  if let Some(this_param) = this_param {
    nodes.push(this_param.into_node());
  }
  nodes.extend(params.items.iter().map(|param| param.into_node()));
  if let Some(rest) = &params.rest {
    nodes.push(rest.into_node());
  }
  nodes
}

/// Generates the keyword types (ex. `TSAnyKeyword`, `TSStringKeyword`, etc.).
pub(super) fn gen_keyword_type<'a>(node: Node<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  // will be a keyword like "any", "unknown", "number", etc...
  node.text_fast(context.program).into_print_items(context.arena)
}

pub(super) fn gen_import_type<'a>(node: &'a TSImportType<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  items.push_sc(sc!("import("));
  items.extend(gen_node((&node.source).into_node(), context));
  if let Some(options) = &node.options {
    items.push_sc(sc!(", "));
    match get_ts_import_type_options_with_value(options) {
      Some(with_value) => {
        // the options are generated without gen_node (like swc's TsImportCallOptions), so generate their comments here
        let options_range = options.range();
        let leading_comments = context.comments.leading_comments_with_previous(options_range.start);
        items.extend(gen_comments_as_leading(&options_range, leading_comments, context));
        items.extend(gen_ts_import_call_options(options, with_value, context));
        let trailing_comments = context.comments.trailing_comments_with_previous(options_range.end);
        items.extend(gen_comments_as_trailing(&options_range, trailing_comments, context));
      }
      None => {
        items.extend(gen_node(options.into_node(), context));
      }
    }
  }
  items.push_sc(sc!(")"));

  if let Some(qualifier) = &node.qualifier {
    items.push_sc(sc!("."));
    items.extend(gen_node(qualifier.into_node(), context));
  }

  if let Some(type_args) = &node.type_arguments {
    items.extend(gen_node(type_args.into_node(), context));
  }
  items
}

/// Gets the value of the `with` property when the import type's options
/// are only that property (ex. `{ with: { "resolution-mode": "import" } }`).
pub(super) fn get_ts_import_type_options_with_value<'a>(options: &'a ObjectExpression<'a>) -> Option<&'a Expression<'a>> {
  if options.properties.len() != 1 {
    return None;
  }
  match &options.properties[0] {
    ObjectPropertyKind::ObjectProperty(prop) if prop.kind == PropertyKind::Init && !prop.computed && !prop.method && !prop.shorthand => match &prop.key {
      PropertyKey::StaticIdentifier(ident) if ident.name.as_str() == "with" => Some(&prop.value),
      _ => None,
    },
    _ => None,
  }
}

/// Generates the options of an import type (ex. `{ with: { "resolution-mode": "import" } }`).
pub(super) fn gen_ts_import_call_options<'a>(node: &'a ObjectExpression<'a>, with_value: &'a Expression<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let first_member_tokens = node.tokens_fast(context.program);
  let with_token = first_member_tokens.iter().find(|t| t.text_fast(context.program) == "with").unwrap();
  let member_range = SourceRange::new(with_token.start(), with_value.end());
  let force_multi_line = get_use_new_lines_for_nodes_with_preceeding_token("{", &[member_range], context.config.object_expression_prefer_single_line, context);
  gen_surrounded_by_tokens(
    |context| {
      let mut items = PrintItems::new(context.arena);
      let start_ln = LineNumber::new("objectStart");
      let end_ln = LineNumber::new("objectEnd");
      if force_multi_line {
        items.push_signal(Signal::NewLine);
      } else {
        items.push_info(start_ln);
        items.push_condition(conditions::new_line_if_multiple_lines_space_or_new_line_otherwise(
          context.arena,
          start_ln,
          Some(end_ln),
        ));
      }
      items.push_condition(indent_if_start_of_line_or_start_of_line_indented({
        let mut items = PrintItems::new(context.arena);
        items.push_sc(sc!("with: "));
        items.extend(gen_node(with_value.into_node(), context));
        match context.config.object_expression_trailing_commas {
          TrailingCommas::OnlyMultiLine => {
            if force_multi_line {
              items.push_sc(sc!(","));
            } else {
              items.push_condition(conditions::if_true(
                "trailingComma",
                context
                  .arena
                  .alloc_resolver(move |context| condition_helpers::is_on_different_line(context, start_ln)),
                sc!(",").into_print_items(context.arena),
              ));
            }
          }
          TrailingCommas::Always => {
            items.push_sc(sc!(","));
          }
          TrailingCommas::Never => {}
        }
        items
      }));
      if force_multi_line {
        items.push_signal(Signal::NewLine);
      } else {
        items.push_condition(conditions::if_true(
          "newlineIfMultipleLines",
          context
            .arena
            .alloc_resolver(move |context| condition_helpers::is_on_different_line(context, start_ln)),
          Signal::NewLine.into_print_items(context.arena),
        ));
        items.push_info(end_ln);
      }
      items
    },
    |_| None,
    GenSurroundedByTokensOptions {
      open_token: sc!("{"),
      close_token: sc!("}"),
      range: Some(node.range()),
      first_member: Some(member_range),
      prefer_single_line_when_empty: false,
      allow_open_token_trailing_comments: true,
      single_line_space_around: false,
      comments_only_padding: BlockPaddingBlankLines::default(),
      keep_blank_lines_before_close_token_comments: false,
    },
    context,
  )
}

pub(super) fn gen_import_type_qualified_name<'a>(node: &'a TSImportTypeQualifiedName<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  items.extend(gen_node((&node.left).into_node(), context));
  items.push_sc(sc!("."));
  items.extend(gen_node((&node.right).into_node(), context));
  items
}

pub(super) fn gen_indexed_access_type<'a>(node: &'a TSIndexedAccessType<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  items.extend(gen_node((&node.object_type).into_node(), context));
  items.extend(gen_computed_prop_like(
    |context| gen_node((&node.index_type).into_node(), context),
    GenComputedPropLikeOptions {
      inner_node_range: node.index_type.range(),
    },
    context,
  ));
  items
}

pub(super) fn gen_infer_type<'a>(node: &'a TSInferType<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  items.push_sc(sc!("infer "));
  items.extend(gen_node((&node.type_parameter).into_node(), context));
  items
}

pub(super) fn gen_ts_instantiation<'a>(node: &'a TSInstantiationExpression<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = gen_node((&node.expression).into_node(), context);
  items.extend(gen_node((&node.type_arguments).into_node(), context));
  items
}

pub(super) fn gen_intersection_type<'a>(node: &'a TSIntersectionType<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  gen_union_or_intersection_type(
    UnionOrIntersectionType {
      node: node.into_node(),
      types: &node.types,
      is_union: false,
    },
    context,
  )
}

pub(super) fn gen_lit_type<'a>(node: &'a TSLiteralType<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  match &node.literal {
    // need to do this in order to support negative numbers
    TSLiteral::NumericLiteral(_) | TSLiteral::BigIntLiteral(_) | TSLiteral::UnaryExpression(_) => {
      node.text_fast(context.program).into_print_items(context.arena)
    }
    _ => gen_node((&node.literal).into_node(), context),
  }
}

pub(super) fn gen_mapped_type<'a>(node: &'a TSMappedType<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  // the range of the key and its constraint (ex. `K in keyof T`), which was a type parameter in swc
  let type_param_range = SourceRange::new(node.key.start(), node.constraint.end());
  let force_use_new_lines =
    !context.config.mapped_type_prefer_single_line && node_helpers::get_use_new_lines_for_nodes(&node.start(), &type_param_range, context.program);

  let range = node.range();
  let mut items = PrintItems::new(context.arena);
  let start_ln = LineNumber::new("startMappedType");
  items.push_info(start_ln);
  items.extend(gen_surrounded_by_tokens(
    |context| {
      let is_different_line_than_start: ConditionResolver<'a> = if force_use_new_lines {
        condition_resolvers::true_resolver()
      } else {
        context
          .arena
          .alloc_resolver(move |context| condition_helpers::is_on_different_line(context, start_ln))
      };
      let inner_items = {
        let mut items = PrintItems::new(context.arena);

        // There could be, or could not be a semi-colon here. Gen the second
        // last token's trailing comments in order to get the comments that
        // should always appear after a semi-colon when it appears and potentially
        // steal the inner node's comments.
        let generated_semi_colon_comments = {
          let node_tokens = node.tokens_fast(context.program);
          gen_trailing_comments(&node_tokens[node_tokens.len() - 2].range(), context)
        };

        if let Some(readonly) = node.readonly {
          items.push_sc(match readonly {
            TSMappedTypeModifierOperator::True => sc!("readonly "),
            TSMappedTypeModifierOperator::Plus => sc!("+readonly "),
            TSMappedTypeModifierOperator::Minus => sc!("-readonly "),
          });
        }

        let computed_inner_range = SourceRange::new(type_param_range.start, node.name_type.as_ref().map(|t| t.end()).unwrap_or(type_param_range.end));
        items.extend(gen_computed_prop_like(
          |context| {
            let mut items = PrintItems::new(context.arena);
            if let Some(name_type) = &node.name_type {
              // same as gen_as_expr_like with the key and constraint as the expression
              items.extend(gen_mapped_type_param(node, context));
              items.push_sc(sc!(" as"));
              items.push_signal(Signal::SpaceIfNotTrailing);
              items.push_condition(conditions::with_indent_if_start_of_line_indented(gen_node(name_type.into_node(), context)));
            } else {
              items.extend(gen_mapped_type_param(node, context));
            }
            items
          },
          GenComputedPropLikeOptions {
            inner_node_range: computed_inner_range,
          },
          context,
        ));

        if let Some(optional) = node.optional {
          items.push_sc(match optional {
            TSMappedTypeModifierOperator::True => sc!("?"),
            TSMappedTypeModifierOperator::Plus => sc!("+?"),
            TSMappedTypeModifierOperator::Minus => sc!("-?"),
          });
        }

        items.extend(gen_type_ann_with_colon_if_exists_for_type(node.type_annotation.as_ref(), context));
        items.extend(get_generated_semi_colon(
          context.arena,
          context.config.semicolons,
          true,
          &is_different_line_than_start,
        ));
        items.extend(generated_semi_colon_comments);

        let inner_items = items.into_rc_path();
        if_true_or(
          "noSpacesWhenMultiLine",
          is_different_line_than_start,
          inner_items.into_print_items(context.arena),
          {
            let mut items = PrintItems::new(context.arena);
            let surrounding_signal = if context.config.mapped_type_space_surrounding_properties {
              Signal::SpaceOrNewLine
            } else {
              Signal::PossibleNewLine
            };
            items.push_signal(surrounding_signal);
            items.push_optional_path(inner_items);
            items.push_signal(surrounding_signal);
            items
          },
        )
        .into_print_items(context.arena)
      };

      if force_use_new_lines {
        surround_with_new_lines(with_indent(inner_items))
      } else {
        ir_helpers::surround_with_newlines_indented_if_multi_line(inner_items, context.config.indent_width)
      }
    },
    |_| None,
    GenSurroundedByTokensOptions {
      open_token: sc!("{"),
      close_token: sc!("}"),
      range: Some(range),
      first_member: Some(type_param_range),
      prefer_single_line_when_empty: false,
      allow_open_token_trailing_comments: true,
      single_line_space_around: false,
      comments_only_padding: BlockPaddingBlankLines::default(),
      keep_blank_lines_before_close_token_comments: false,
    },
    context,
  ));
  items
}

/// Generates the key and constraint of a mapped type (ex. `K in keyof T`) the
/// same way as a type parameter, which is what they were in swc's ast.
pub(super) fn gen_mapped_type_param<'a>(node: &'a TSMappedType<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  items.extend(gen_node((&node.key).into_node(), context));
  items.push_signal(Signal::SpaceOrNewLine);
  items.push_condition(conditions::indent_if_start_of_line({
    let mut items = PrintItems::new(context.arena);
    items.push_sc(sc!("in"));
    items.push_signal(Signal::SpaceIfNotTrailing);
    items.extend(gen_node((&node.constraint).into_node(), context));
    items
  }));
  items
}

pub(super) fn gen_optional_type<'a>(node: &'a TSOptionalType<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  items.extend(gen_node((&node.type_annotation).into_node(), context));
  items.push_sc(sc!("?"));
  items
}

pub(super) fn gen_qualified_name<'a>(node: &'a TSQualifiedName<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  items.extend(gen_node((&node.left).into_node(), context));
  items.push_sc(sc!("."));
  items.extend(gen_node((&node.right).into_node(), context));
  items
}

pub(super) fn gen_parenthesized_type<'a>(node: &'a TSParenthesizedType<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let end_ln = match context.assignment_paren_end_ln {
    Some((start, end_ln)) if start == node.start() => context.assignment_paren_end_ln.take().map(|_| end_ln),
    _ => None,
  };
  let mut items = gen_parenthesized_type_inner(node, context);
  if let Some(end_ln) = end_ln {
    items.push_info(end_ln);
  }
  items
}

pub(super) fn gen_parenthesized_type_inner<'a>(node: &'a TSParenthesizedType<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  if should_skip_parenthesized_type(node, context) {
    return gen_node((&node.type_annotation).into_node(), context);
  }

  let generated_type = conditions::with_indent_if_start_of_line_indented(gen_node_in_parens(
    |context| gen_node((&node.type_annotation).into_node(), context),
    GenNodeInParensOptions {
      inner_range: node.type_annotation.range(),
      prefer_hanging: true,
      allow_open_paren_trailing_comments: true,
      single_line_space_around: false,
    },
    context,
  ))
  .into_print_items(context.arena);

  return if use_new_line_group(node, context) {
    new_line_group(generated_type)
  } else {
    generated_type
  };

  fn use_new_line_group<'a>(node: &'a TSParenthesizedType<'a>, context: &Context<'a>) -> bool {
    !matches!(node.into_node().parent(context.program), Some(Node::TSTypeAliasDeclaration(_)))
  }
}

pub(super) fn should_skip_parenthesized_type<'a>(node: &'a TSParenthesizedType<'a>, context: &mut Context<'a>) -> bool {
  if node_helpers::has_surrounding_different_line_comments((&node.type_annotation).into_node(), context.program) {
    return false;
  }

  // keep for `() => (infer U extends string) ? A : B` because the constraint would otherwise be part of a conditional type
  if let TSType::TSInferType(infer_type) = &node.type_annotation
    && infer_type.type_parameter.constraint.is_some()
  {
    return false;
  }

  matches!(
    node.into_node().parent(context.program).map(|p| p.kind()),
    Some(NodeKind::TSTypeAnnotation | NodeKind::TSTypeAliasDeclaration | NodeKind::TSParenthesizedType)
  )
}

pub(super) fn gen_rest_type<'a>(node: &'a TSRestType<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  items.push_sc(sc!("..."));
  items.extend(gen_node((&node.type_annotation).into_node(), context));
  items
}

pub(super) fn gen_ts_this_param<'a>(node: &'a TSThisParameter<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  items.push_sc(sc!("this"));
  items.extend(gen_type_ann_with_colon_if_exists(node.type_annotation.as_deref(), context));
  items
}

pub(super) fn gen_tpl_lit_type<'a>(node: &'a TSTemplateLiteralType<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  gen_template_literal(node.quasis.iter().map(|x| x.into_node()), node.types.iter().map(|x| x.into_node()), context)
}

pub(super) fn gen_tuple_type<'a>(node: &'a TSTupleType<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let prefer_hanging = match context.config.tuple_type_prefer_hanging {
    PreferHanging::Never => false,
    PreferHanging::OnlySingleItem => node.element_types.len() == 1,
    PreferHanging::Always => true,
  };
  gen_array_like_nodes(
    GenArrayLikeNodesOptions {
      node: node.into_node(),
      range: node.range(),
      nodes: node.element_types.iter().map(|x| Some(x.into_node())).collect(),
      prefer_hanging,
      prefer_single_line: context.config.tuple_type_prefer_single_line,
      trailing_commas: context.config.tuple_type_trailing_commas,
      space_around: context.config.tuple_type_space_around,
    },
    context,
  )
}

/// Generates a labeled tuple element (ex. `label?: type`).
pub(super) fn gen_named_tuple_member<'a>(node: &'a TSNamedTupleMember<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  items.extend(gen_node((&node.label).into_node(), context));
  if node.optional {
    items.push_sc(sc!("?"));
  }
  // same as gen_type_ann_with_colon_for_type, but the element type is a tuple element rather than a type
  if context.config.type_annotation_space_before_colon {
    items.push_space();
  }
  let colon_token = context.token_finder.get_previous_token_if_colon(&node.element_type);
  #[cfg(debug_assertions)]
  assert_has_op(":", colon_token, context);
  items.extend(gen_type_ann_with_colon((&node.element_type).into_node(), colon_token, context));
  items
}

pub(super) fn gen_type_ann<'a>(node: &'a TSTypeAnnotation<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  gen_node((&node.type_annotation).into_node(), context)
}

pub(super) fn gen_type_param<'a>(node: &'a TSTypeParameter<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);

  if node.r#const {
    items.push_sc(sc!("const "));
  }
  if node.r#in {
    items.push_sc(sc!("in "));
  }
  if node.out {
    items.push_sc(sc!("out "));
  }

  items.extend(gen_node((&node.name).into_node(), context));

  if let Some(constraint) = &node.constraint {
    items.push_signal(Signal::SpaceOrNewLine);
    items.push_condition(conditions::indent_if_start_of_line({
      let mut items = PrintItems::new(context.arena);
      // the `in` of a mapped type is generated in gen_mapped_type_param because it's not a type parameter in oxc
      items.push_sc(sc!("extends"));
      items.push_signal(Signal::SpaceIfNotTrailing);
      items.extend(gen_node(constraint.into_node(), context));
      items
    }));
  }

  if let Some(default) = &node.default {
    items.extend(gen_assignment(default.into_node(), sc!("="), context));
  }

  items
}

pub(super) fn gen_type_parameters<'a>(node: TypeParamNode<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let params = node.params();
  let force_use_new_lines = get_use_new_lines(&node, &params, context);
  let mut items = PrintItems::new(context.arena);
  let prefer_hanging_config = context.config.type_parameters_prefer_hanging;
  let prefer_hanging = match prefer_hanging_config {
    PreferHanging::Never => false,
    PreferHanging::OnlySingleItem => params.len() == 1,
    PreferHanging::Always => true,
  };

  items.push_sc(sc!("<"));
  items.extend(gen_separated_values(
    GenSeparatedValuesParams {
      nodes: params.into_iter().map(NodeOrSeparator::Node).collect(),
      prefer_hanging,
      force_use_new_lines,
      blank_lines: ListBlankLines::Remove,
      separator: get_trailing_commas(node, context).into(),
      single_line_options: ir_helpers::SingleLineOptions::same_line_maybe_space_separated(context.arena),
      multi_line_options: ir_helpers::MultiLineOptions::surround_newlines_indented(),
      force_possible_newline_at_start: false,
      node_sorter: None,
    },
    context,
  ));
  items.push_sc(sc!(">"));

  return items;

  fn get_trailing_commas<'a>(node: TypeParamNode<'a>, context: &Context<'a>) -> TrailingCommas {
    let parent = node.parent(context.program);
    // trailing commas should be allowed in type parameters only—not arguments
    match parent.get_type_parameters() {
      Some(type_params) if type_params.start() == node.start() => {
        // Use a trailing comma for an arrow function in a JSX file if the absence of
        // one would lead to a parsing ambiguity. This also prevents the "This syntax is
        // reserved in files with the .mts or .cts extension." diagnostic. Function and
        // class expressions aren't ambiguous because of their keyword.
        let is_arrow_fn = parent.kind() == NodeKind::ArrowFunctionExpression;
        if is_arrow_fn && (context.is_jsx() || context.disallows_ambiguous_jsx_like()) {
          // It is not ambiguous if there are multiple type parameters.
          if type_params.params.len() == 1 {
            let type_param = &type_params.params[0];
            // We have a possible ambiguity iff this type parameter is just an identifier.
            // (modifiers like `const` don't count, ex. `<const T,>`)
            if type_param.constraint.is_none() && type_param.default.is_none() {
              return TrailingCommas::Always;
            }
          }
        }

        // Unambiguous. Use the preferred style.
        context.config.type_parameters_trailing_commas
      }
      // Type arguments. Trailing commas not allowed.
      _ => TrailingCommas::Never,
    }
  }

  fn get_use_new_lines<'a>(node: &TypeParamNode<'a>, params: &[Node<'a>], context: &mut Context<'a>) -> bool {
    if context.config.type_parameters_prefer_single_line || params.is_empty() || is_in_tpl_lit_type_substitution(node, context.program) {
      false
    } else {
      let first_param = &params[0];
      let angle_bracket_pos = node.start();
      node_helpers::get_use_new_lines_for_nodes(&angle_bracket_pos, first_param, context.program)
    }
  }

  /// A substitution in a template literal type goes on its own line when it doesn't fit, so its
  /// type arguments don't keep the line breaks that older versions put in them instead.
  fn is_in_tpl_lit_type_substitution<'a>(node: &TypeParamNode<'a>, program: ProgramInfo<'a>) -> bool {
    let TypeParamNode::Instantiation(_) = node else {
      return false;
    };
    let parent = node.parent(program);
    matches!(parent.kind(), NodeKind::TSTypeReference | NodeKind::TSTypeQuery)
      && parent.parent(program).is_some_and(|p| p.kind() == NodeKind::TSTemplateLiteralType)
  }
}

pub(super) fn gen_type_operator<'a>(node: &'a TSTypeOperator<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  items.push_sc(match node.operator {
    TSTypeOperatorOperator::Keyof => sc!("keyof"),
    TSTypeOperatorOperator::Unique => sc!("unique"),
    TSTypeOperatorOperator::Readonly => sc!("readonly"),
  });
  items.push_signal(Signal::SpaceIfNotTrailing);
  items.extend(gen_node((&node.type_annotation).into_node(), context));
  items
}

pub(super) fn gen_type_predicate<'a>(node: &'a TSTypePredicate<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  if node.asserts {
    items.push_sc(sc!("asserts "));
  }
  items.extend(gen_node((&node.parameter_name).into_node(), context));
  if let Some(type_ann) = &node.type_annotation {
    items.push_sc(sc!(" is"));
    items.push_signal(Signal::SpaceIfNotTrailing);
    items.extend(gen_node(type_ann.into_node(), context));
  }
  items
}

pub(super) fn gen_type_query<'a>(node: &'a TSTypeQuery<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  items.push_sc(sc!("typeof"));
  items.push_signal(Signal::SpaceIfNotTrailing);
  items.extend(gen_node((&node.expr_name).into_node(), context));
  if let Some(type_args) = &node.type_arguments {
    items.extend(gen_node(type_args.into_node(), context));
  }
  items
}

pub(super) fn gen_type_reference<'a>(node: &'a TSTypeReference<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  items.extend(gen_node((&node.type_name).into_node(), context));
  if let Some(type_params) = &node.type_arguments {
    items.extend(gen_node(type_params.into_node(), context));
  }
  items
}

pub(super) fn gen_union_type<'a>(node: &'a TSUnionType<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  gen_union_or_intersection_type(
    UnionOrIntersectionType {
      node: node.into_node(),
      types: &node.types,
      is_union: true,
    },
    context,
  )
}

pub(super) struct UnionOrIntersectionType<'a> {
  pub node: Node<'a>,
  pub types: &'a [TSType<'a>],
  pub is_union: bool,
}

pub(super) fn gen_union_or_intersection_type<'a>(node: UnionOrIntersectionType<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  let force_use_new_lines = get_force_use_new_lines(&node, context);
  let separator = if node.is_union { sc!("|") } else { sc!("&") };
  let trailing_separator = if node.is_union { sc!(" |") } else { sc!(" &") };

  let indent_width = context.config.indent_width;
  let arena = context.arena;
  let prefer_hanging = context.config.union_and_intersection_type_prefer_hanging;
  let operator_position = context.config.union_and_intersection_type_operator_position;
  let is_parent_union_or_intersection = matches!(
    node.node.parent(context.program).unwrap().kind(),
    NodeKind::TSUnionType | NodeKind::TSIntersectionType
  );
  let multi_line_options = if !is_parent_union_or_intersection {
    if use_surround_newlines(node.node, context) {
      ir_helpers::MultiLineOptions::surround_newlines_indented()
    } else {
      ir_helpers::MultiLineOptions::new_line_start()
    }
  } else {
    ir_helpers::MultiLineOptions::same_line_start_hanging_indent()
  };
  let gen_result = ir_helpers::gen_separated_values(
    |is_multi_line_or_hanging_ref| {
      let is_multi_line_or_hanging = is_multi_line_or_hanging_ref.create_resolver(context.arena);
      let types_count = node.types.len();

      // For each pair (between types[i-1] and types[i], i >= 1), decide whether the
      // separator should be at the end of the previous line (SameLine) or at the
      // start of the next line (NextLine). Index 0 is unused.
      let pair_positions: Vec<OperatorPosition> = node
        .types
        .iter()
        .enumerate()
        .map(|(i, type_node)| {
          if i == 0 {
            operator_position
          } else {
            resolve_pair_position(&node, i, type_node, operator_position, separator.text, context)
          }
        })
        .collect();

      // Whether to emit the conditional leading separator on the first value when
      // wrapping. NextLine = always (current behavior, leading-`|` hanging style).
      // SameLine = never. Maintain = follow the source's leading-`|` presence.
      let leading_first_when_multi_line = match operator_position {
        OperatorPosition::NextLine => true,
        OperatorPosition::SameLine => false,
        OperatorPosition::Maintain => {
          if node.node.start_line_fast(context.program) == node.node.end_line_fast(context.program) {
            true
          } else {
            node
              .types
              .first()
              .and_then(|t| context.token_finder.get_previous_token_if_operator(&t.range(), separator.text))
              .is_some()
          }
        }
      };

      let mut generated_nodes = BumpVec::with_capacity_in(types_count, context.arena.bump());
      for (i, type_node) in node.types.iter().enumerate() {
        let (allow_inline_multi_line, allow_inline_single_line) = {
          let is_last_value = i + 1 == types_count; // allow the last type to be single line
          (allows_inline_multi_line(type_node.into_node(), context, types_count > 1), is_last_value)
        };
        // the comments on the same line as a separator at the end of the previous line are
        // generated with it, so only the ones on other lines are generated here for it
        let separator_token = context.token_finder.get_previous_token_if_operator(&type_node.range(), separator.text);
        let start_lc = LineAndColumn::new("start");
        let after_separator_ln = LineNumber::new("afterSeparator");
        let mut items = PrintItems::new(context.arena);
        items.push_line_and_column(start_lc);
        if let Some(separator_token) = separator_token {
          items.extend(gen_leading_comments(&separator_token.range(), context));
        }

        let emit_leading_separator = i > 0 && matches!(pair_positions[i], OperatorPosition::NextLine);
        if i == 0 && !is_parent_union_or_intersection && leading_first_when_multi_line {
          items.push_condition(if_true("separatorIfMultiLine", is_multi_line_or_hanging, {
            // todo: .into() implementation for StringContainer
            let mut items = PrintItems::new(context.arena);
            items.push_sc(separator);
            items
          }));
        } else if emit_leading_separator {
          items.push_sc(separator);
        }

        if let Some(separator_token) = separator_token {
          items.extend(gen_trailing_comments(&separator_token.range(), context));
        }
        items.push_info(after_separator_ln);

        // the separator is at the end of the previous line for the same line position
        let has_separator_before = i == 0 || !matches!(pair_positions[i], OperatorPosition::SameLine);
        if has_separator_before {
          items.push_condition(if_true(
            "afterSeparatorSpace",
            context.arena.alloc_resolver(move |condition_context| {
              let is_on_same_line = condition_helpers::is_on_same_line(condition_context, after_separator_ln)?;
              let is_at_same_position = condition_helpers::is_at_same_position(condition_context, start_lc)?;
              Some(is_on_same_line && !is_at_same_position)
            }),
            Signal::SpaceIfNotTrailing.into_print_items(context.arena),
          ));
        }
        items.extend(gen_node(type_node.into_node(), context));

        let next_is_same_line = i + 1 < types_count && matches!(pair_positions[i + 1], OperatorPosition::SameLine);
        if next_is_same_line {
          let next_separator_token = context.token_finder.get_previous_token_if_operator(&node.types[i + 1].range(), separator.text);
          if let Some(next_separator_token) = next_separator_token {
            let comments = get_leading_comments_on_same_line(&next_separator_token.range(), context);
            let comment_items = gen_comments_same_line(comments, context);
            if !comment_items.is_empty() {
              items.push_space();
              items.extend(comment_items);
            }
          }
          items.push_sc(trailing_separator);
          if let Some(next_separator_token) = next_separator_token {
            items.extend(gen_trailing_comments_same_line(&next_separator_token.range(), context));
          }
        }

        generated_nodes.push(ir_helpers::GeneratedValue {
          items,
          lines_span: None,
          allow_inline_multi_line,
          allow_inline_single_line,
          is_known_multi_line: false,
        });
      }

      generated_nodes
    },
    ir_helpers::GenSeparatedValuesOptions {
      prefer_hanging,
      force_use_new_lines,
      max_blank_lines: 0,
      indent_width,
      single_line_options: ir_helpers::SingleLineOptions::separated_same_line(Signal::SpaceOrNewLine.into_print_items(arena)),
      multi_line_options,
      force_possible_newline_at_start: false,
    },
  );

  items.extend(gen_result.items);

  return items;

  fn get_force_use_new_lines<'a>(node: &UnionOrIntersectionType<'a>, context: &mut Context<'a>) -> bool {
    let types = node.types;
    if context.config.union_and_intersection_type_prefer_hanging && !context.config.union_and_intersection_type_prefer_single_line && types.len() >= 2 {
      // a hanging type puts the second type on the next line when it doesn't fit, which isn't
      // multi-line unless the first type is also on the line after what's before it (ex. `=`)
      let is_first_on_next_line = match context.token_finder.get_previous_token(&node.node) {
        Some(previous_token) => types[0].start_line_fast(context.program) > previous_token.end_line_fast(context.program),
        None => false,
      };
      return is_first_on_next_line && get_use_new_lines_for_nodes(types, false, context);
    }
    if !context.config.union_and_intersection_type_prefer_single_line || types.len() < 2 {
      return get_use_new_lines_for_nodes(types, false, context);
    }
    // only look at the comments within the type because the ones before
    // or after it might move (ex. a separator or semi-colon gets added)
    let has_leading_separator = node.node.start() < types[0].start();
    (has_leading_separator && has_leading_comment_on_previous_line(&types[0], context))
      || has_any_node_trailing_comment_on_different_line(&types[..types.len() - 1], context)
  }

  fn use_surround_newlines<'a>(node: Node<'a>, context: &mut Context<'a>) -> bool {
    let parent = node.parent(context.program).unwrap();
    match parent {
      Node::TSTypeAssertion(_) => true,
      Node::TSParenthesizedType(paren_type) => !should_skip_parenthesized_type(paren_type, context),
      _ => false,
    }
  }

  fn resolve_pair_position<'a>(
    node: &UnionOrIntersectionType<'a>,
    i: usize,
    type_node: &TSType<'a>,
    operator_position: OperatorPosition,
    separator_text: &str,
    context: &mut Context<'a>,
  ) -> OperatorPosition {
    match operator_position {
      OperatorPosition::NextLine => OperatorPosition::NextLine,
      OperatorPosition::SameLine => OperatorPosition::SameLine,
      OperatorPosition::Maintain => {
        // When the whole union/intersection is on one source line, prefer dprint's
        // default (NextLine) for the wrapping layout — matches how `Maintain` is
        // handled for conditional expressions/types.
        if node.node.start_line_fast(context.program) == node.node.end_line_fast(context.program) {
          return OperatorPosition::NextLine;
        }
        match context.token_finder.get_previous_token_if_operator(&type_node.range(), separator_text) {
          Some(sep_token) => {
            let prev_end_line = node.types[i - 1].end_line_fast(context.program);
            if prev_end_line == sep_token.start_line_fast(context.program) {
              OperatorPosition::SameLine
            } else {
              OperatorPosition::NextLine
            }
          }
          None => OperatorPosition::NextLine,
        }
      }
    }
  }
}
