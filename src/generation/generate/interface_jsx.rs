use super::*;

/* interface / type element */

pub(super) fn gen_call_signature_decl<'a>(node: &'a TSCallSignatureDeclaration<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  let start_lsil = LineStartIndentLevel::new("startCallSignature");

  items.push_info(start_lsil);
  if let Some(type_params) = &node.type_parameters {
    items.extend(gen_node(type_params.into_node(), context));
  }
  let params = get_ts_signature_param_nodes(node.this_param.as_deref(), &node.params, context);
  let param_count = params.len();
  items.extend(gen_parameters_or_arguments(
    GenParametersOrArgumentsOptions {
      node: node.into_node(),
      range: node.get_parameters_range(context),
      nodes: params,
      custom_close_paren: |context| {
        Some(gen_close_paren_with_type(
          GenCloseParenWithTypeOptions {
            start_lsil,
            type_node: node.return_type.as_ref().map(|x| x.into_node()),
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

  items
}

pub(super) fn gen_construct_signature_decl<'a>(node: &'a TSConstructSignatureDeclaration<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  let start_lsil = LineStartIndentLevel::new("startConstructSignature");

  items.push_info(start_lsil);
  items.push_sc(sc!("new"));
  if context.config.construct_signature_space_after_new_keyword {
    items.push_space();
  }
  if let Some(type_params) = &node.type_parameters {
    items.extend(gen_node(type_params.into_node(), context));
  }
  let params = get_ts_signature_param_nodes(None, &node.params, context);
  let param_count = params.len();
  items.extend(gen_parameters_or_arguments(
    GenParametersOrArgumentsOptions {
      node: node.into_node(),
      range: node.get_parameters_range(context),
      nodes: params,
      custom_close_paren: |context| {
        Some(gen_close_paren_with_type(
          GenCloseParenWithTypeOptions {
            start_lsil,
            type_node: node.return_type.as_ref().map(|x| x.into_node()),
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

  items
}

/// Gets the parameter nodes of a signature, which are the `this` parameter
/// followed by the parameters and the rest parameter.
pub(super) fn get_ts_signature_param_nodes<'a>(
  this_param: Option<&'a TSThisParameter<'a>>,
  params: &'a FormalParameters<'a>,
  context: &Context<'a>,
) -> BumpVec<'a, Node<'a>> {
  let mut nodes = BumpVec::with_capacity_in(params.items.len() + 2, context.arena.bump());
  if let Some(this_param) = this_param {
    nodes.push(this_param.into_node());
  }
  nodes.extend(params.items.iter().map(|p| p.into_node()));
  if let Some(rest) = &params.rest {
    nodes.push(rest.into_node());
  }
  nodes
}

pub(super) fn gen_index_signature<'a>(node: &'a TSIndexSignature<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);

  if node.r#static {
    items.push_sc(sc!("static "));
  }
  if node.readonly {
    items.push_sc(sc!("readonly "));
  }

  let param: Node<'a> = (&node.parameter).into_node();
  items.extend(gen_computed_prop_like(
    |context| gen_node(param, context),
    GenComputedPropLikeOptions {
      inner_node_range: param.range(),
    },
    context,
  ));
  items.extend(gen_type_ann_with_colon_if_exists(Some(&*node.type_annotation), context));

  if matches!(node.into_node().parent(context.program), Some(Node::ClassBody(_))) && context.config.semicolons.is_true() {
    items.push_sc(sc!(";"));
  }

  items
}

/// Generates the parameter of an index signature (ex. `key: string` in `[key: string]: number`).
pub(super) fn gen_index_signature_name<'a>(node: &'a TSIndexSignatureName<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  items.push_str_runtime_width_computed(node.name.as_str());
  items.extend(gen_type_ann_with_colon_if_exists(Some(&*node.type_annotation), context));
  items
}

pub(super) fn gen_method_signature<'a>(node: &'a TSMethodSignature<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  gen_method_signature_like(
    MethodSignatureLike {
      node: node.into_node(),
      method_kind: match node.kind {
        TSMethodSignatureKind::Method => MethodSignatureLikeKind::Method,
        TSMethodSignatureKind::Get => MethodSignatureLikeKind::Getter,
        TSMethodSignatureKind::Set => MethodSignatureLikeKind::Setter,
      },
      computed: node.computed,
      optional: node.optional,
      key: (&node.key).into_node(),
      parameters_range: node.get_parameters_range(context),
      type_params: node.type_parameters.as_ref().map(|p| p.into_node()),
      params: get_ts_signature_param_nodes(node.this_param.as_deref(), &node.params, context),
      type_ann: node.return_type.as_ref().map(|p| p.into_node()),
    },
    context,
  )
}

pub(super) enum MethodSignatureLikeKind {
  Method,
  Getter,
  Setter,
}

pub(super) struct MethodSignatureLike<'a> {
  node: Node<'a>,
  method_kind: MethodSignatureLikeKind,
  computed: bool,
  optional: bool,
  key: Node<'a>,
  type_params: Option<Node<'a>>,
  parameters_range: Option<SourceRange>,
  params: BumpVec<'a, Node<'a>>,
  type_ann: Option<Node<'a>>,
}

pub(super) fn gen_method_signature_like<'a>(node: MethodSignatureLike<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  let start_lsil = LineStartIndentLevel::new("startMethodSignature");
  items.push_info(start_lsil);

  match node.method_kind {
    MethodSignatureLikeKind::Getter => items.push_sc(sc!("get ")),
    MethodSignatureLikeKind::Setter => items.push_sc(sc!("set ")),
    MethodSignatureLikeKind::Method => {}
  }

  items.extend(if node.computed {
    gen_computed_prop_like(
      |context| gen_node(node.key, context),
      GenComputedPropLikeOptions {
        inner_node_range: node.key.range(),
      },
      context,
    )
  } else {
    gen_quotable_prop(node.key, context)
  });

  if node.optional {
    items.push_sc(sc!("?"));
  }
  if let Some(type_params) = node.type_params {
    items.extend(gen_node(type_params, context));
  }

  let param_count = node.params.len();
  items.extend(gen_parameters_or_arguments(
    GenParametersOrArgumentsOptions {
      node: node.node,
      range: node.parameters_range,
      nodes: node.params,
      custom_close_paren: {
        let type_node = node.type_ann;
        move |context| {
          Some(gen_close_paren_with_type(
            GenCloseParenWithTypeOptions {
              start_lsil,
              type_node,
              type_node_separator: None,
              param_count,
            },
            context,
          ))
        }
      },
      is_parameters: true,
    },
    context,
  ));

  items
}

pub(super) fn gen_property_signature<'a>(node: &'a TSPropertySignature<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  if node.readonly {
    items.push_sc(sc!("readonly "));
  }

  items.extend(if node.computed {
    gen_computed_prop_like(
      |context| gen_node((&node.key).into_node(), context),
      GenComputedPropLikeOptions {
        inner_node_range: node.key.range(),
      },
      context,
    )
  } else {
    gen_quotable_prop((&node.key).into_node(), context)
  });

  if node.optional {
    items.push_sc(sc!("?"));
  }
  items.extend(gen_type_ann_with_colon_if_exists(node.type_annotation.as_deref(), context));

  items
}

pub(super) fn gen_quotable_prop<'a>(node: Node<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  match context.config.quote_props {
    QuoteProps::AsNeeded => match node {
      Node::StringLiteral(str_lit) => {
        if let Some(text) = str_lit.value.as_str()
          && is_text_valid_identifier(text)
        {
          gen_from_raw_string(context.arena, text)
        } else {
          gen_node(node, context)
        }
      }
      _ => gen_node(node, context),
    },
    QuoteProps::Consistent => match context.use_consistent_quote_props() {
      Some(true) => match node {
        // add quotes
        Node::IdentifierReference(ident) => string_literal::gen_non_jsx_text(ident.name.as_str(), context),
        Node::IdentifierName(ident) => string_literal::gen_non_jsx_text(ident.name.as_str(), context),
        _ => gen_node(node, context),
      },
      Some(false) => match node {
        // remove quotes
        Node::StringLiteral(str_lit) => match str_lit.value.as_str() {
          Some(text) => gen_from_raw_string(context.arena, text),
          None => gen_node(node, context),
        },
        _ => gen_node(node, context),
      },
      None => {
        debug_assert!(false, "should always have a value");
        gen_node(node, context)
      }
    },
    QuoteProps::Preserve => gen_node(node, context),
  }
}

pub(super) fn gen_interface_body<'a>(node: &'a TSInterfaceBody<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let start_header_lsil = get_parent_lsil(node, context);

  return context.with_maybe_consistent_props(
    node,
    |node| use_consistent_quotes_for_members(node.body.iter().map(|n| n.into_node())),
    |context, node| {
      gen_membered_body(
        GenMemberedBodyOptions {
          node: node.into_node(),
          members: node.body.iter().map(|x| x.into_node()).collect(),
          start_header_lsil,
          brace_position: context.config.interface_declaration_brace_position,
          block_padding: context.config.interface_declaration_block_padding,
          get_forced_blank_line_count: |_, _, context| get_member_spacing_forced_blank_line_count(context.config.interface_declaration_member_spacing),
          separator: context.config.semicolons.into(),
          keep_blank_lines_before_close_token_comments: true,
        },
        context,
      )
    },
  );

  fn get_parent_lsil<'a>(node: &'a TSInterfaceBody<'a>, context: &mut Context<'a>) -> Option<LineStartIndentLevel> {
    for ancestor in node.into_node().ancestors(context.program) {
      if let Node::TSInterfaceDeclaration(ancestor) = ancestor {
        return context.get_lsil_for_node(ancestor).map(|x| x.to_owned());
      }
    }
    None
  }
}

pub(super) fn use_consistent_quotes_for_members<'a>(mut members: impl Iterator<Item = Node<'a>>) -> bool {
  // swc had an expression as the key of a signature, which included computed keys
  fn check_expr(key: &PropertyKey) -> bool {
    match key {
      PropertyKey::StaticIdentifier(ident) => !is_text_valid_identifier(ident.name.as_str()),
      PropertyKey::Identifier(ident) => !is_text_valid_identifier(ident.name.as_str()),
      PropertyKey::StringLiteral(str) => match str.value.as_str() {
        Some(text) => !is_text_valid_identifier(text),
        None => true,
      },
      _ => false,
    }
  }

  // swc had a computed key as a separate kind of property name, which is never checked
  fn check_prop_name(key: &PropertyKey, computed: bool) -> bool {
    !computed && check_expr(key)
  }

  members.any(|m| match m {
    // class members
    // Do not match class properties (Node::PropertyDefinition)
    // With `--strictPropertyInitialization`, TS treats properties
    // with quoted names differently than unquoted ones.
    // See https://github.com/microsoft/TypeScript/pull/20075
    Node::MethodDefinition(method) => method.kind != MethodDefinitionKind::Constructor && check_prop_name(&method.key, method.computed),
    // interface members
    Node::TSPropertySignature(signature) => check_expr(&signature.key),
    Node::TSMethodSignature(signature) => signature.kind != TSMethodSignatureKind::Method && check_expr(&signature.key),
    // object members
    Node::ObjectProperty(prop) => !prop.shorthand && check_prop_name(&prop.key, prop.computed),
    _ => false,
  })
}

pub(super) fn gen_type_lit<'a>(node: &'a TSTypeLiteral<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  return context.with_maybe_consistent_props(
    node,
    |node| use_consistent_quotes_for_members(node.members.iter().map(|n| n.into_node())),
    |context, node| {
      gen_object_like_node(
        GenObjectLikeNodeOptions {
          node: node.into_node(),
          members: node.members.iter().map(|m| m.into_node()).collect(),
          separator: Separator {
            single_line: Some(semi_colon_or_comma_to_separator_value(
              context.config.type_literal_separator_kind_single_line,
              context,
            )),
            multi_line: Some(semi_colon_or_comma_to_separator_value(
              context.config.type_literal_separator_kind_multi_line,
              context,
            )),
          },
          prefer_hanging: context.config.type_literal_prefer_hanging,
          prefer_single_line: context.config.type_literal_prefer_single_line,
          force_single_line: false,
          force_multi_line: false,
          surround_single_line_with_spaces: context.config.type_literal_space_surrounding_properties,
          blank_lines: context.config.type_literal_member_spacing.into(),
          node_sorter: None,
        },
        context,
      )
    },
  );

  fn semi_colon_or_comma_to_separator_value<'a>(value: SemicolonOrComma, context: &mut Context<'a>) -> SeparatorValue {
    match value {
      SemicolonOrComma::Comma => SeparatorValue::Comma(context.config.type_literal_trailing_commas),
      SemicolonOrComma::Semicolon => SeparatorValue::Semicolon(context.config.semicolons),
    }
  }
}

/* jsx */

pub(super) fn gen_jsx_attribute<'a>(node: &'a JSXAttribute<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  items.extend(gen_node((&node.name).into_node(), context));
  if let Some(value) = &node.value {
    items.push_sc(sc!("="));
    let surround_with_braces = context.token_finder.get_previous_token_if_open_brace(value).is_some();
    let inner_items = gen_node(value.into_node(), context);
    items.extend(if surround_with_braces {
      gen_as_jsx_expr_container(node.range(), inner_items, context)
    } else {
      inner_items
    });
  }
  items
}

pub(super) fn gen_jsx_closing_element<'a>(node: &'a JSXClosingElement<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  items.push_sc(sc!("</"));
  items.extend(gen_jsx_tag_comments(node.name.range(), context));
  items.extend(gen_node((&node.name).into_node(), context));
  items.push_sc(sc!(">"));
  items
}

pub(super) fn gen_jsx_closing_fragment<'a>(node: &'a JSXClosingFragment, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  items.push_sc(sc!("</"));
  items.extend(gen_jsx_tag_comments(SourceRange::new(node.end() - 1, node.end()), context));
  items.push_sc(sc!(">"));
  items
}

/// Generates the comments in a jsx tag that are before the given range (ex. before the
/// name or `>`) with a space before them, so a comment after the `/` of a closing tag
/// doesn't merge with it.
pub(super) fn gen_jsx_tag_comments<'a>(next_range: SourceRange, context: &mut Context<'a>) -> PrintItems<'a> {
  let comments = context.comments.leading_comments_with_previous(next_range.start);
  if !comments.clone().any(|c| !context.has_handled_comment(c)) {
    return PrintItems::new(context.arena);
  }
  let mut items = PrintItems::new(context.arena);
  items.push_space();
  items.extend(gen_comments_as_leading(&next_range, comments, context));
  items
}

pub(super) fn handle_jsx_surrounding_parens<'a>(inner_items: PrintItems<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  if !is_jsx_paren_expr_handled_node(context.current_node, context) {
    if should_jsx_surround_newlines(context.current_node, context) {
      return surround_with_newlines_indented_if_multi_line(inner_items, context.config.indent_width);
    } else {
      return inner_items;
    }
  }

  if let Node::JSXExpressionContainer(container) = context.parent()
    && context.config.jsx_multi_line_parens != JsxMultiLineParens::Always
  {
    // the container already puts the jsx on its own lines
    if should_surround_jsx_expr_container_expr_with_newlines(container, context) {
      return inner_items;
    }
    return surround_with_newlines_indented_if_multi_line(inner_items, context.config.indent_width);
  }

  // ex. `(<div />).props`
  let are_parens_required = match context.parent() {
    Node::ParenthesizedExpression(paren_expr) => {
      let paren_expr = paren_expr.into_node();
      is_jsx_paren_expr_required(paren_expr, paren_expr.parent(context.program).unwrap())
    }
    _ => false,
  };
  let start_ln = LineNumber::new("conditionalParenStart");
  let end_ln = LineNumber::new("conditionalParenEnd");
  let mut items = PrintItems::new(context.arena);
  let inner_items_rc = inner_items.into_rc_path();

  items.push_info(start_ln);
  items.extend(gen_line_numbers_region_start(context.arena, &[end_ln]));
  let mut condition = if_true_or(
    "parensOrNewlinesIfMultipleLines",
    context
      .arena
      .alloc_resolver(move |context| condition_helpers::is_multiple_lines(context, start_ln, end_ln)),
    surround_with_parens(surround_with_new_lines(with_indent(inner_items_rc.into_print_items(context.arena)))),
    if are_parens_required {
      let items = inner_items_rc.into_print_items(context.arena);
      surround_with_parens(if context.config.paren_expression_space_around {
        surround_with_spaces(items)
      } else {
        items
      })
    } else {
      let mut items = PrintItems::new(context.arena);
      items.push_signal(Signal::PossibleNewLine);
      items.extend(inner_items_rc.into_print_items(context.arena));
      items
    },
  );
  let condition_reevaluation = condition.create_reevaluation();
  items.push_condition(condition);

  items.push_info(end_ln);
  // the end line changes when the line breaks after the jsx, which
  // happens after the condition was evaluated with the end line on the first line
  items.push_reevaluation(condition_reevaluation);
  return items;

  fn should_jsx_surround_newlines<'a>(node: Node<'a>, context: &Context<'a>) -> bool {
    let mut parent = node.parent(context.program).unwrap();
    while let Node::ParenthesizedExpression(paren_expr) = parent {
      if node_helpers::has_surrounding_comments((&paren_expr.expression).into_node(), context.program) {
        return false;
      }
      parent = parent.parent(context.program).unwrap();
    }

    match parent {
      // the container already puts the jsx on its own lines
      Node::JSXExpressionContainer(container) => !should_surround_jsx_expr_container_expr_with_newlines(container, context),
      _ => false,
    }
  }
}

pub(super) fn is_jsx_paren_expr_handled_node<'a>(node: Node<'a>, context: &Context<'a>) -> bool {
  if context.config.jsx_multi_line_parens == JsxMultiLineParens::Never {
    return false;
  }

  if !matches!(node.kind(), NodeKind::JSXElement | NodeKind::JSXFragment) {
    return false;
  }

  let mut parent = node.parent(context.program).unwrap();
  // Only wrap the top-level JSX element in parens
  if matches!(parent.kind(), NodeKind::JSXElement | NodeKind::JSXFragment) {
    return false;
  }

  if node_helpers::has_surrounding_comments(node, context.program) {
    return false;
  }

  while parent.kind() == NodeKind::ParenthesizedExpression {
    if node_helpers::has_surrounding_comments(parent, context.program) {
      return false;
    }
    parent = parent.parent(context.program).unwrap();
  }

  if context.config.jsx_multi_line_parens == JsxMultiLineParens::Always {
    return true;
  }

  // do not allow in expr statement, argument, attributes, jsx exprs, or member exprs
  !(matches!(parent.kind(), NodeKind::ExpressionStatement | NodeKind::JSXExpressionContainer)
    || is_jsx_arg_or_array_element(node, parent, context.program)
    || is_jsx_member_expr_object(node, parent))
}

/// Gets if the node is an argument or array element, including a spread one (ex. `f(...<div />)`),
/// which is where swc had the `ExprOrSpread` node as the parent.
pub(super) fn is_jsx_arg_or_array_element<'a>(node: Node<'a>, parent: Node<'a>, program: ProgramInfo<'a>) -> bool {
  match parent {
    Node::CallExpression(call_expr) => !call_expr.callee.range().contains(&node.range()),
    Node::NewExpression(new_expr) => !new_expr.callee.range().contains(&node.range()),
    Node::ImportExpression(_) | Node::ArrayExpression(_) => true,
    Node::SpreadElement(_) => matches!(
      parent.parent(program),
      Some(Node::CallExpression(_) | Node::NewExpression(_) | Node::ArrayExpression(_))
    ),
    _ => false,
  }
}

/// Gets if the node is the object of a member expression (ex. `<div />.props`), which is
/// where swc had the `MemberExpr` node as the parent (its computed property had its own parent).
pub(super) fn is_jsx_member_expr_object<'a>(node: Node<'a>, parent: Node<'a>) -> bool {
  match parent {
    Node::StaticMemberExpression(_) | Node::PrivateFieldExpression(_) => true,
    Node::ComputedMemberExpression(member_expr) => member_expr.object.range().contains(&node.range()),
    _ => false,
  }
}

/// Gets if the parens around a top-level JSX element or fragment (ex. `(<div />)`
/// or `((<div />))`) are removed because of `jsx.multiLineParens: never`.
pub(super) fn is_jsx_paren_expr_removed<'a>(node: &'a ParenthesizedExpression<'a>, context: &Context<'a>) -> bool {
  if context.config.jsx_multi_line_parens != JsxMultiLineParens::Never {
    return false;
  }

  // start from the innermost parens so all the parens are checked
  let mut node = node;
  while let Expression::ParenthesizedExpression(inner) = &node.expression {
    node = &**inner;
  }

  if !matches!(node.expression, Expression::JSXElement(_) | Expression::JSXFragment(_)) {
    return false;
  }

  if node_helpers::has_surrounding_comments((&node.expression).into_node(), context.program) {
    return false;
  }

  // keep when any of the parens have comments
  let mut child: Node<'a> = node.into_node();
  let mut parent = child.parent(context.program).unwrap();
  loop {
    if node_helpers::has_surrounding_comments(child, context.program) {
      return false;
    }
    if parent.kind() != NodeKind::ParenthesizedExpression {
      break;
    }
    child = parent;
    parent = parent.parent(context.program).unwrap();
  }

  match parent {
    // swc's spread `ExprOrSpread` (ex. `f(...(<div />))`)
    Node::SpreadElement(_) if is_jsx_arg_or_array_element(child, parent, context.program) => false,
    _ => !is_jsx_paren_expr_required(child, parent),
  }
}

/// Gets if the parens around a JSX element or fragment are required because
/// the JSX starts a member, call, or similar expression (ex. `(<div />).props`).
pub(super) fn is_jsx_paren_expr_required(paren_expr: Node, parent: Node) -> bool {
  let paren_range = paren_expr.range();
  match parent {
    Node::StaticMemberExpression(member_expr) => member_expr.object.range() == paren_range,
    Node::ComputedMemberExpression(member_expr) => member_expr.object.range() == paren_range,
    Node::PrivateFieldExpression(member_expr) => member_expr.object.range() == paren_range,
    Node::CallExpression(call_expr) => call_expr.callee.range() == paren_range,
    Node::NewExpression(new_expr) => new_expr.callee.range() == paren_range,
    Node::TaggedTemplateExpression(tagged_tpl) => tagged_tpl.tag.range() == paren_range,
    Node::TSNonNullExpression(_) | Node::TSInstantiationExpression(_) => true,
    _ => false,
  }
}

pub(super) fn gen_jsx_element<'a>(node: &'a JSXElement<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let items = if let Some(closing) = &node.closing_element {
    // pre element bodies should be formatted as-is
    if node.opening_element.name.text_fast(context.program) == "pre" {
      let mut items = gen_node((&node.opening_element).into_node(), context);
      let in_between_range = SourceRange::new(node.opening_element.end(), closing.start());
      let in_between_text = in_between_range.text_fast(context.program);
      items.extend(gen_file_text_as_is(in_between_text, closing.start(), TrimLineEnds::Yes, context));
      items.extend(gen_node(closing.into_node(), context));
      items
    } else {
      let result = gen_jsx_with_opening_and_closing(
        GenJsxWithOpeningAndClosingOptions {
          opening_element: (&node.opening_element).into_node(),
          closing_element: closing.into_node(),
          children: node.children.iter().map(|x| x.into_node()).collect(),
        },
        context,
      );
      context.store_info_range_for_node(node, (result.start_ln, result.end_ln));
      result.items
    }
  } else {
    let start_ln = LineNumber::new("jsxElementStart");
    let end_ln = LineNumber::new("jsxElementEnd");
    let mut items = PrintItems::new(context.arena);

    context.store_info_range_for_node(node, (start_ln, end_ln));

    items.push_info(start_ln);
    items.extend(gen_node((&node.opening_element).into_node(), context));
    items.push_info(end_ln);
    items
  };

  handle_jsx_surrounding_parens(items, context)
}

pub(super) fn gen_jsx_empty_expr<'a>(node: &'a JSXEmptyExpression, context: &mut Context<'a>) -> PrintItems<'a> {
  gen_comment_collection(get_jsx_empty_expr_comments(node, context), None, None, context)
}

pub(super) fn gen_jsx_expr_container<'a>(node: &'a JSXExpressionContainer<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  // Don't send JSX empty expressions to gen_node because it will not handle comments
  // the way they should be specifically handled for empty expressions.
  match &node.expression {
    JSXExpression::EmptyExpression(expr) => {
      // swc's empty expression was empty at the close brace, which its comments are before,
      // while oxc's spans the text between the braces
      let expr_range = SourceRange::new(expr.end(), expr.end());
      // keep the braces beside block comments (ex. `{/*` and `*/}`) so they work with an editor's toggle comment
      let surround_with_new_lines =
        !has_only_same_line_block_comments(expr, context) && should_surround_jsx_expr_container_with_newlines(expr_range, context.program);
      let gen_inner = gen_jsx_empty_expr(expr, context);
      let surround_with_space = context.config.jsx_expression_container_space_surrounding_expression;
      gen_as_jsx_expr_container_with_new_lines(expr_range, gen_inner, surround_with_new_lines, surround_with_space, context)
    }
    jsx_expr => {
      let expr = jsx_expr.as_expression().expect("Expected the jsx expression to be an expression.");
      let surround_with_new_lines = should_surround_jsx_expr_container_expr_with_newlines(node, context);
      let surround_with_space = match get_jsx_expr_container_literal(expr, context) {
        Some(Expression::ObjectExpression(_)) => context.config.jsx_expression_container_space_surrounding_object_literal,
        Some(Expression::ArrayExpression(_)) => context.config.jsx_expression_container_space_surrounding_array_literal,
        _ => context.config.jsx_expression_container_space_surrounding_expression,
      };
      let gen_inner = gen_node(expr.into_node(), context);
      gen_as_jsx_expr_container_with_new_lines(node.expression.range(), gen_inner, surround_with_new_lines, surround_with_space, context)
    }
  }
}

pub(super) fn gen_as_jsx_expr_container<'a>(inner_range: SourceRange, inner_items: PrintItems<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let surround_with_new_lines = should_surround_jsx_expr_container_with_newlines(inner_range, context.program);
  let surround_with_space = context.config.jsx_expression_container_space_surrounding_expression;
  gen_as_jsx_expr_container_with_new_lines(inner_range, inner_items, surround_with_new_lines, surround_with_space, context)
}

pub(super) fn gen_as_jsx_expr_container_with_new_lines<'a>(
  inner_range: SourceRange,
  inner_items: PrintItems<'a>,
  surround_with_new_lines: bool,
  surround_with_space: bool,
  context: &mut Context<'a>,
) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  items.push_sc(sc!("{"));
  if surround_with_new_lines {
    items.push_signal(Signal::NewLine);
    items.push_signal(Signal::StartIndent);
    items.extend(inner_items);
    items.extend(gen_trailing_comments_as_statements(&inner_range, context));
    items.push_signal(Signal::NewLine);
    items.push_signal(Signal::FinishIndent);
  } else if surround_with_space {
    // no space at the end or start of a line (ex. when the expression starts or ends on another line)
    items.push_signal(Signal::SpaceIfNotTrailing);
    items.extend(inner_items);
    items.push_condition(if_false(
      "spaceIfNotStartOfLine",
      condition_resolvers::is_start_of_line(),
      Signal::SpaceIfNotTrailing.into_print_items(context.arena),
    ));
  } else {
    items.extend(inner_items);
  }
  items.push_sc(sc!("}"));

  items
}

pub(super) fn should_surround_jsx_expr_container_with_newlines(expr: SourceRange, program: ProgramInfo) -> bool {
  let expr_start_line = expr.start_line_fast(program);
  for comment in expr.leading_comments_fast(program) {
    if comment.kind == CommentKind::Line {
      return true;
    } else if comment.start_line_fast(program) < expr_start_line {
      return true;
    }
  }
  let expr_end_line = expr.end_line_fast(program);
  for comment in expr.trailing_comments_fast(program) {
    if comment.kind == CommentKind::Line || comment.end_line_fast(program) > expr_end_line {
      return true;
    }
  }

  false
}

/// Gets if the braces of a container with an expression go on their own lines
/// because of comments or a newline after the open brace that's maintained.
pub(super) fn should_surround_jsx_expr_container_expr_with_newlines<'a>(node: &'a JSXExpressionContainer<'a>, context: &Context<'a>) -> bool {
  should_surround_jsx_expr_container_with_newlines(node.expression.range(), context.program)
    || !context.config.jsx_expression_container_prefer_single_line
      && has_new_line_after_jsx_expr_container_open_brace(node, context.program)
      // the parens already put multi-line jsx on its own lines (ex. `{(` and `)}`)
      && !is_jsx_surrounded_by_multi_line_parens(node, context)
}

/// Gets if the container's expression starts on a line after its open brace.
pub(super) fn has_new_line_after_jsx_expr_container_open_brace(node: &JSXExpressionContainer, program: ProgramInfo) -> bool {
  match &node.expression {
    JSXExpression::EmptyExpression(_) => false,
    expr => node.start_line_fast(program) != expr.start_line_fast(program),
  }
}

/// Gets if the container's expression is jsx that `jsx.multiLineParens: always`
/// surrounds with parens when it's on multiple lines.
pub(super) fn is_jsx_surrounded_by_multi_line_parens<'a>(node: &'a JSXExpressionContainer<'a>, context: &Context<'a>) -> bool {
  if context.config.jsx_multi_line_parens != JsxMultiLineParens::Always {
    return false;
  }
  let Some(mut expr) = node.expression.as_expression() else {
    return false;
  };
  while let Expression::ParenthesizedExpression(paren_expr) = expr {
    expr = &paren_expr.expression;
  }
  is_jsx_paren_expr_handled_node(expr.into_node(), context)
}

/// Gets the object or array literal the expression will be generated as
/// when there are no comments between it and the braces of its container.
pub(super) fn get_jsx_expr_container_literal<'a>(expr: &'a Expression<'a>, context: &Context<'a>) -> Option<&'a Expression<'a>> {
  let mut expr = expr;
  loop {
    if node_helpers::has_surrounding_comments(expr.into_node(), context.program) {
      return None;
    }
    match expr {
      Expression::ObjectExpression(_) | Expression::ArrayExpression(_) => return Some(expr),
      // look through parens that won't be kept (ex. `{({ value: true })}`)
      Expression::ParenthesizedExpression(paren_expr) if should_skip_paren_expr(paren_expr, context) => expr = &paren_expr.expression,
      _ => return None,
    }
  }
}

pub(super) fn gen_jsx_fragment<'a>(node: &'a JSXFragment<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let result = gen_jsx_with_opening_and_closing(
    GenJsxWithOpeningAndClosingOptions {
      opening_element: (&node.opening_fragment).into_node(),
      closing_element: (&node.closing_fragment).into_node(),
      children: node.children.iter().map(|x| x.into_node()).collect(),
    },
    context,
  );

  context.store_info_range_for_node(node, (result.start_ln, result.end_ln));

  handle_jsx_surrounding_parens(result.items, context)
}

/// Generates the name of a jsx element or attribute (swc used an identifier for these).
pub(super) fn gen_jsx_identifier<'a>(node: &'a JSXIdentifier<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  items.push_str_runtime_width_computed(node.name.as_str());
  items
}

pub(super) fn gen_jsx_member_expr<'a>(node: &'a JSXMemberExpression<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  items.extend(gen_node((&node.object).into_node(), context));
  items.push_sc(sc!("."));
  items.extend(gen_node((&node.property).into_node(), context));
  items
}

pub(super) fn gen_jsx_namespaced_name<'a>(node: &'a JSXNamespacedName<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  items.extend(gen_node((&node.namespace).into_node(), context));
  items.push_sc(sc!(":"));
  items.extend(gen_node((&node.name).into_node(), context));
  items
}

pub(super) fn gen_jsx_opening_element<'a>(node: &'a JSXOpeningElement<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let space_before_self_closing_tag_slash = context.config.jsx_self_closing_element_space_before_slash;
  let is_self_closing = is_jsx_opening_element_self_closing(node, context.program);
  let force_use_new_lines = get_force_is_multi_line(node, context);
  let prefer_newline_before_close_bracket = get_should_prefer_newline_before_close_bracket(node, is_self_closing, context);
  let start_lsil = LineStartIndentLevel::new("openingElementStart");
  let mut items = PrintItems::new(context.arena);

  items.push_info(start_lsil);
  items.push_sc(sc!("<"));
  items.extend(gen_jsx_tag_comments(node.name.range(), context));
  items.extend(gen_node((&node.name).into_node(), context));
  if let Some(type_args) = &node.type_arguments {
    items.extend(gen_node(type_args.into_node(), context));
  }

  if node.attributes.len() == 1 && node.type_arguments.is_none() && is_jsx_attr_with_string(&node.attributes[0]) {
    items.push_space();
    items.extend(gen_node((&node.attributes[0]).into_node(), context));
  } else if !node.attributes.is_empty() {
    let mut multi_line_options = ir_helpers::MultiLineOptions::surround_newlines_indented();
    multi_line_options.newline_at_end = prefer_newline_before_close_bracket;
    items.extend(gen_separated_values(
      GenSeparatedValuesParams {
        nodes: node.attributes.iter().map(|p| NodeOrSeparator::Node(p.into_node())).collect(),
        prefer_hanging: context.config.jsx_attributes_prefer_hanging,
        force_use_new_lines,
        blank_lines: ListBlankLines::Remove,
        separator: Separator::none(),
        single_line_options: ir_helpers::SingleLineOptions::separated_line_starting_with_space(context.arena),
        multi_line_options,
        force_possible_newline_at_start: false,
        node_sorter: None,
      },
      context,
    ));
  }

  // generate trailing comments on different lines
  let name_or_type_arg_end = node.type_arguments.as_ref().map(|t| t.end()).unwrap_or_else(|| node.name.end());
  let last_node_end = node
    .attributes
    .last()
    .map(|n| get_jsx_attr_range(n.into_node(), context).end)
    .unwrap_or(name_or_type_arg_end);

  let generated_comments = gen_comments_as_statements(last_node_end.trailing_comments_fast(context.program), None, context);
  if !generated_comments.is_empty() {
    if node.attributes.is_empty() {
      items.push_signal(Signal::NewLine);
    }
    items.extend(with_indent(generated_comments));
    items.push_signal(Signal::NewLine);
  }

  // when hanging, the attributes don't end with a new line before the bracket
  if context.config.jsx_attributes_prefer_hanging && prefer_newline_before_close_bracket {
    items.push_condition(conditions::new_line_if_hanging(context.arena, start_lsil, None));
  }
  if is_self_closing {
    if space_before_self_closing_tag_slash {
      items.push_force_current_line_indentation();
      items.extend(space_if_not_start_line(context.arena));
    }
    items.push_sc(sc!("/"));
  }
  items.push_sc(sc!(">"));

  return items;

  fn get_force_is_multi_line<'a>(node: &'a JSXOpeningElement<'a>, context: &mut Context<'a>) -> bool {
    if context.config.jsx_attributes_prefer_single_line {
      false
    } else if let Some(first_attrib) = node.attributes.first() {
      node_helpers::get_use_new_lines_for_nodes(&node.name, &get_jsx_attr_range(first_attrib.into_node(), context), context.program)
    } else {
      false
    }
  }

  fn get_should_prefer_newline_before_close_bracket<'a>(node: &'a JSXOpeningElement<'a>, is_self_closing: bool, context: &mut Context<'a>) -> bool {
    let bracket_pos_config = match is_self_closing {
      true => context.config.jsx_self_closing_element_bracket_position,
      false => context.config.jsx_opening_element_bracket_position,
    };
    match bracket_pos_config {
      SameOrNextLinePosition::Maintain => {
        if let Some(last_attr) = node.attributes.last() {
          get_jsx_attr_range(last_attr.into_node(), context).end_line_fast(context.program) < node.end_line_fast(context.program)
        } else {
          false
        }
      }
      SameOrNextLinePosition::NextLine => true,
      SameOrNextLinePosition::SameLine => false,
    }
  }

  fn is_jsx_attr_with_string(node: &JSXAttributeItem) -> bool {
    if let JSXAttributeItem::Attribute(attrib) = node
      && let Some(value) = &attrib.value
    {
      return matches!(value, JSXAttributeValue::StringLiteral(_));
    }
    false
  }
}

/// Gets if the opening element is self closing (ex. `<div />`), which is
/// when its element has no closing element.
pub(super) fn is_jsx_opening_element_self_closing<'a>(node: &'a JSXOpeningElement<'a>, program: ProgramInfo<'a>) -> bool {
  match node.into_node().parent(program) {
    Some(Node::JSXElement(element)) => element.closing_element.is_none(),
    _ => false,
  }
}

pub(super) fn gen_jsx_opening_fragment<'a>(node: &'a JSXOpeningFragment, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  items.push_sc(sc!("<"));
  items.extend(gen_jsx_tag_comments(SourceRange::new(node.end() - 1, node.end()), context));
  items.push_sc(sc!(">"));
  items
}

pub(super) fn gen_jsx_spread_child<'a>(node: &'a JSXSpreadChild<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  let dot3_token = context.token_finder.get_previous_token(&node.expression).filter(|t| t.kind == TokenKind::Dot3);
  // the comments before the spread operator go before it rather than before the expression
  if let Some(dot3_token) = dot3_token {
    let leading_comments = context.comments.leading_comments_with_previous(dot3_token.start());
    items.extend(gen_comments_as_leading(&dot3_token.range(), leading_comments, context));
  }
  items.push_sc(sc!("..."));
  items.extend(gen_node((&node.expression).into_node(), context));
  let inner_start = dot3_token.map(|t| t.start()).unwrap_or_else(|| node.expression.start());
  gen_as_jsx_expr_container(SourceRange::new(inner_start, node.expression.end()), items, context)
}

pub(super) fn gen_jsx_text<'a>(node: &'a JSXText<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);

  for (i, line) in get_lines(node.text_fast(context.program)).into_iter().enumerate() {
    if i > 0 {
      items.push_signal(Signal::NewLine);
      items.push_signal(Signal::NewLine);
    }

    let mut was_last_space_or_newline = true;
    for word in line.split(utils::is_jsx_space_char) {
      if !was_last_space_or_newline {
        items.push_signal(Signal::SpaceOrNewLine);
        was_last_space_or_newline = true;
      }
      if !word.is_empty() {
        items.push_str(word);
        was_last_space_or_newline = false;
      }
    }
  }

  return ir_helpers::new_line_group(items);

  fn get_lines(node_text: &str) -> Vec<String> {
    let mut past_line: Option<&str> = None;
    let lines = node_text
      .trim_matches(utils::is_jsx_whitespace_char)
      .lines()
      .map(|line| line.trim_matches(utils::is_jsx_whitespace_char));
    let mut result = Vec::new();
    let mut current_line = String::new();

    for line in lines {
      if let Some(past_line) = past_line
        && !line.is_empty()
        && past_line.is_empty()
        && !current_line.is_empty()
      {
        result.push(current_line);
        current_line = String::new();
      }

      if !line.is_empty() {
        if !current_line.is_empty() {
          current_line.push(' ');
        }
        current_line.push_str(line);
      }

      past_line.replace(line);
    }

    if !current_line.is_empty() {
      result.push(current_line);
    }

    result
  }
}
