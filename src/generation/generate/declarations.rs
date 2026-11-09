use super::*;

/* declarations */

pub(super) fn gen_class_decl<'a>(node: &'a Class<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  // swc parsed an anonymous default exported class (ex. `export default class {}`)
  // as a class expression, so keep generating it like one
  let is_class_decl = node.id.is_some() || !matches!(node.into_node().parent(context.program), Some(Node::ExportDefaultDeclaration(_)));
  let brace_position = if is_class_decl {
    context.config.class_declaration_brace_position
  } else {
    context.config.class_expression_brace_position
  };
  gen_class_decl_or_expr(ClassDeclOrExpr::new(node, is_class_decl, brace_position, context), context)
}

pub(super) struct ClassDeclOrExpr<'a> {
  pub node: Node<'a>,
  pub member_node: Node<'a>,
  pub decorators: &'a [Decorator<'a>],
  pub is_class_decl: bool,
  pub is_declare: bool,
  pub is_abstract: bool,
  pub ident: Option<Node<'a>>,
  pub type_params: Option<Node<'a>>,
  pub super_class: Option<Node<'a>>,
  pub super_type_params: Option<Node<'a>>,
  pub implements: Vec<Node<'a>>,
  pub members: Vec<Node<'a>>,
  pub brace_position: BracePosition,
}

impl<'a> ClassDeclOrExpr<'a> {
  pub fn new(node: &'a Class<'a>, is_class_decl: bool, brace_position: BracePosition, context: &Context<'a>) -> Self {
    ClassDeclOrExpr {
      node: node.into_node(),
      // the class body has the braces of the members
      member_node: (&node.body).into_node(),
      decorators: &node.decorators,
      is_class_decl,
      is_declare: node.declare,
      is_abstract: node.r#abstract,
      ident: node.id.as_ref().map(|x| x.into_node()),
      type_params: node.type_parameters.as_ref().map(|x| x.into_node()),
      super_class: node.heritage.as_ref().map(|x| (&x.expression).into_node()),
      super_type_params: node.heritage.as_ref().and_then(|x| x.type_arguments.as_ref()).map(|x| x.into_node()),
      implements: node.implements.iter().map(|x| x.into_node()).collect(),
      members: get_class_members(&node.body, context),
      brace_position,
    }
  }
}

pub(super) fn gen_class_decl_or_expr<'a>(node: ClassDeclOrExpr<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  let (mut start_before_owned_comments_ln, mut start_before_owned_comments_isol) = context.get_or_create_current_before_comments_start_info();

  // generate decorators (an exported class's decorators before `export` are generated before the export)
  let parent = node.node.parent(context.program).unwrap();
  if matches!(parent.kind(), NodeKind::ExportDeclaration | NodeKind::ExportDefaultDeclaration) {
    // the decorators after `export` are within the class (ex. `export @dec class A {}`)
    if !node.decorators.is_empty() && !are_decorators_before(node.decorators, parent.start()) {
      items.extend(gen_decl_decorators(node.decorators, context));
      // swc didn't include these decorators in the class, so the class starts after them
      start_before_owned_comments_ln = LineNumber::new("afterExportDecorators");
      start_before_owned_comments_isol = IsStartOfLine::new("afterExportDecorators");
      items.push_info(start_before_owned_comments_ln);
      items.push_info(start_before_owned_comments_isol);
    }
  } else if node.is_class_decl {
    items.extend(gen_decl_decorators(node.decorators, context));
  } else {
    items.extend(gen_decorators(node.decorators, DecoratorsSeparator::Space, context));
  }

  // generate header and body
  let start_header_lsil = LineStartIndentLevel::new("startHeader");
  items.push_info(start_header_lsil);
  let start_header_ln = LineNumber::new("startHeader");
  items.push_info(start_header_ln);

  if node.is_declare {
    items.push_sc(sc!("declare "));
  }
  if node.is_abstract {
    items.push_sc(sc!("abstract "));
  }

  items.push_sc(sc!("class"));

  if let Some(ident) = node.ident {
    items.push_space();
    items.extend(gen_node(ident, context));
  }
  if let Some(type_params) = node.type_params {
    items.extend(gen_node(type_params, context));
  }
  if let Some(super_class) = node.super_class {
    items.push_condition(conditions::new_line_if_hanging_space_otherwise(
      context.arena,
      conditions::NewLineIfHangingSpaceOtherwiseOptions {
        start_lsil: start_header_lsil,
        end_lsil: None,
        space_char: Some(
          conditions::if_above_width_or(
            context.config.indent_width,
            Signal::SpaceOrNewLine.into_print_items(context.arena),
            " ".into_print_items(context.arena),
          )
          .into_print_items(context.arena),
        ),
      },
    ));
    items.push_condition(conditions::indent_if_start_of_line({
      let mut items = PrintItems::new(context.arena);
      items.push_sc(sc!("extends "));
      items.extend(new_line_group({
        let mut items = PrintItems::new(context.arena);
        items.extend(gen_node(super_class, context));
        if let Some(super_type_params) = node.super_type_params {
          items.extend(gen_node(super_type_params, context));
        }
        items
      }));
      items
    }));
  }
  items.extend(gen_extends_or_implements(
    GenExtendsOrImplementsOptions {
      text: sc!("implements"),
      type_items: node.implements,
      start_header_lsil,
      prefer_hanging: context.config.implements_clause_prefer_hanging,
    },
    context,
  ));

  let (member_spacing, block_padding) = if node.is_class_decl {
    (context.config.class_declaration_member_spacing, context.config.class_declaration_block_padding)
  } else {
    (context.config.class_expression_member_spacing, context.config.class_expression_block_padding)
  };
  items.extend(context.with_maybe_consistent_props(
    node.members,
    |members| use_consistent_quotes_for_members(members.iter().copied()),
    |context, members| {
      gen_membered_body(
        GenMemberedBodyOptions {
          node: node.member_node,
          members,
          start_header_lsil: Some(start_header_lsil),
          brace_position: node.brace_position,
          block_padding,
          get_forced_blank_line_count: move |_, _, _| get_member_spacing_forced_blank_line_count(member_spacing),
          separator: Separator::none(),
          keep_blank_lines_before_close_token_comments: true,
        },
        context,
      )
    },
  ));

  if node.is_class_decl {
    items
  } else {
    let items = items.into_rc_path();
    if_true_or(
      "classExprConditionalIndent",
      context.arena.alloc_resolver(move |context| {
        if context.resolved_is_start_of_line(start_before_owned_comments_isol)? {
          Some(false)
        } else {
          condition_helpers::is_multiple_lines(context, start_before_owned_comments_ln, start_header_ln)
        }
      }),
      with_indent(items.into_print_items(context.arena)),
      items.into_print_items(context.arena),
    )
    .into_print_items(context.arena)
  }
}

pub(super) fn gen_export_decl<'a>(node: &'a ExportDeclaration<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  // note: this is also the `export` of `export import a = b;`, which swc stored on the import equals declaration
  items.push_sc(sc!("export "));
  // decorators before `export` are handled in gen_node because their starts come before the ExportDeclaration
  // and the ones after it are generated with the class
  items.extend(gen_node((&node.declaration).into_node(), context));
  items
}

pub(super) fn gen_export_default_decl<'a>(node: &'a ExportDefaultDeclaration<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  items.push_sc(sc!("export default "));
  match &node.declaration {
    ExportDefaultDeclarationKind::FunctionDeclaration(_)
    | ExportDefaultDeclarationKind::ClassDeclaration(_)
    | ExportDefaultDeclarationKind::TSInterfaceDeclaration(_) => {
      // decorators before `export` are handled in gen_node because their starts come before the ExportDefaultDeclaration
      // and the ones after it are generated with the class
      items.extend(gen_node((&node.declaration).into_node(), context));
    }
    _ => {
      // an expression (swc's ExportDefaultExpr)
      items.extend(gen_node((&node.declaration).into_node(), context));
      if context.config.semicolons.is_true() {
        items.push_sc(sc!(";"));
      }
    }
  }
  items
}

pub(super) fn gen_enum_decl<'a>(node: &'a TSEnumDeclaration<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let start_header_lsil = LineStartIndentLevel::new("startHeader");
  let mut items = PrintItems::new(context.arena);

  // header
  items.push_info(start_header_lsil);

  if node.declare {
    items.push_sc(sc!("declare "));
  }
  if node.r#const {
    items.push_sc(sc!("const "));
  }
  items.push_sc(sc!("enum "));
  items.extend(gen_node((&node.id).into_node(), context));

  // body
  let member_spacing = context.config.enum_declaration_member_spacing;
  items.extend(gen_membered_body(
    GenMemberedBodyOptions {
      // the enum body has the braces of the members
      node: (&node.body).into_node(),
      members: node.body.members.iter().map(|x| x.into_node()).collect(),
      start_header_lsil: Some(start_header_lsil),
      brace_position: context.config.enum_declaration_brace_position,
      block_padding: context.config.enum_declaration_block_padding,
      get_forced_blank_line_count: move |_, _, _| get_member_spacing_forced_blank_line_count(member_spacing),
      separator: context.config.enum_declaration_trailing_commas.into(),
      keep_blank_lines_before_close_token_comments: ListBlankLines::from(member_spacing) != ListBlankLines::Remove,
    },
    context,
  ));

  items
}

pub(super) fn gen_enum_member<'a>(node: &'a TSEnumMember<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  match &node.id {
    // swc gave a computed string key (ex. ["a"]) a string spanning only the string, which removed the brackets
    TSEnumMemberName::ComputedString(str) => {
      items.extend(gen_node(str.into_node(), context));
    }
    // swc gave a template literal key (ex. [`a`]) a string spanning only its content, which was generated in brackets
    TSEnumMemberName::ComputedTemplateString(tpl) => {
      items.push_sc(sc!("["));
      items.extend(gen_node(tpl.into_node(), context));
      items.push_sc(sc!("]"));
    }
    _ => items.extend(gen_node((&node.id).into_node(), context)),
  }

  if let Some(init) = &node.initializer {
    items.extend(gen_assignment(init.into_node(), sc!("="), context));
  }

  items
}

pub(super) fn gen_export_named_decl<'a>(node: &'a ExportNamedDeclaration<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  gen_export_named_or_from_decl(
    ExportNamedOrFromDecl {
      node: node.into_node(),
      specifiers: &node.specifiers,
      src: None,
      with_clause: None,
      is_type_only: matches!(node.export_kind, ImportOrExportKind::Type),
    },
    context,
  )
}

pub(super) fn gen_export_from_decl<'a>(node: &'a ExportFromDeclaration<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  gen_export_named_or_from_decl(
    ExportNamedOrFromDecl {
      node: node.into_node(),
      specifiers: &node.specifiers,
      src: Some(&node.source),
      with_clause: node.with_clause.as_deref(),
      is_type_only: matches!(node.export_kind, ImportOrExportKind::Type),
    },
    context,
  )
}

pub(super) struct ExportNamedOrFromDecl<'a> {
  pub node: Node<'a>,
  pub specifiers: &'a [ExportSpecifier<'a>],
  pub src: Option<&'a StringLiteral<'a>>,
  pub with_clause: Option<&'a WithClause<'a>>,
  pub is_type_only: bool,
}

/// Generates swc's NamedExport, which is an ExportNamedDeclaration or ExportFromDeclaration in oxc.
///
/// Note that swc's default export specifier (ex. `export v from "mod"`) doesn't exist in oxc and its
/// namespace export specifier (ex. `export * as ns from "mod"`) is in an ExportAllDeclaration.
pub(super) fn gen_export_named_or_from_decl<'a>(node: ExportNamedOrFromDecl<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  // fill specifiers
  let named_exports = node.specifiers;

  let force_single_line = context.config.export_declaration_force_single_line && !contains_line_or_multiline_comment(node.node, context.program);

  let force_multi_line = !force_single_line
    && ((context.config.export_declaration_force_multi_line == ForceMultiLine::Always)
      || (named_exports.len() > 1 && context.config.export_declaration_force_multi_line == ForceMultiLine::WhenMultiple));

  let should_single_line = force_single_line
    || (!force_multi_line
      && (named_exports.len() <= 1 && context.config.export_declaration_force_multi_line == ForceMultiLine::Never)
      && node.node.start_line_fast(context.program) == node.node.end_line_fast(context.program));

  // generate
  let mut items = PrintItems::new(context.arena);

  items.push_sc(sc!("export "));
  if node.is_type_only {
    items.push_sc(sc!("type "));
  }

  if !named_exports.is_empty() {
    items.extend(gen_named_import_or_export_specifiers(
      GenNamedImportOrExportSpecifierOptions {
        parent: node.node,
        specifiers: named_exports.iter().map(|x| x.into_node()).collect(),
        force_single_line,
        force_multi_line_specifiers: force_multi_line,
      },
      context,
    ));
  } else {
    items.push_sc(sc!("{}"));
  }

  if let Some(src) = node.src {
    items.push_sc(sc!(" from "));
    items.extend(gen_node(src.into_node(), context));
  }

  if let Some(with_clause) = node.with_clause {
    items.extend(gen_with_clause_with_keyword(with_clause, context));
  }

  if context.config.semicolons.is_true() {
    items.push_sc(sc!(";"));
  }

  if should_single_line { with_no_new_lines(items) } else { items }
}

pub(super) fn gen_function_decl<'a>(node: &'a Function<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  // swc parsed an anonymous default exported function (ex. `export default function() {}`)
  // as a function expression, so keep generating it like one
  let is_func_decl = node.id.is_some() || !matches!(node.into_node().parent(context.program), Some(Node::ExportDefaultDeclaration(_)));
  gen_function_decl_or_expr(
    FunctionDeclOrExprNode {
      node: node.into_node(),
      is_func_decl,
      ident: node.id.as_ref(),
      declare: node.declare,
      func: node,
    },
    context,
  )
}

pub(super) struct FunctionDeclOrExprNode<'a> {
  pub node: Node<'a>,
  pub is_func_decl: bool,
  pub ident: Option<&'a BindingIdentifier<'a>>,
  pub declare: bool,
  pub func: &'a Function<'a>,
}

pub(super) fn gen_function_decl_or_expr<'a>(node: FunctionDeclOrExprNode<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  let start_header_lsil = LineStartIndentLevel::new("functionHeaderStart");
  let func = node.func;
  let space_after_function_keyword = !node.is_func_decl && context.config.function_expression_space_after_function_keyword;

  items.push_info(start_header_lsil);
  if node.declare {
    items.push_sc(sc!("declare "));
  }
  if func.r#async {
    items.push_sc(sc!("async "));
  }
  items.push_sc(sc!("function"));
  if func.generator {
    items.push_sc(sc!("*"));
  }
  if space_after_function_keyword {
    items.push_space()
  }
  if let Some(ident) = node.ident {
    if !space_after_function_keyword {
      items.push_space();
    }
    items.extend(gen_node(ident.into_node(), context));
  }
  if let Some(type_params) = &func.type_parameters {
    items.extend(gen_node(type_params.into_node(), context));
  }
  #[allow(clippy::collapsible_if)]
  if get_use_space_before_parens(node.is_func_decl, context) {
    if node.ident.is_some() || func.type_parameters.is_some() || !space_after_function_keyword {
      items.push_space();
    }
  }

  // the `this` parameter and rest parameter are counted like swc did
  let param_count = func.params.items.len() + usize::from(func.params.rest.is_some()) + usize::from(func.this_param.is_some());
  items.extend(gen_parameters_or_arguments(
    GenParametersOrArgumentsOptions {
      node: node.node,
      nodes: get_function_params(func, context),
      range: func.get_parameters_range(context),
      custom_close_paren: |context| {
        Some(gen_close_paren_with_type(
          GenCloseParenWithTypeOptions {
            start_lsil: start_header_lsil,
            type_node: func.return_type.as_ref().map(|x| x.into_node()),
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

  if let Some(body) = &func.body {
    let brace_position = if node.is_func_decl {
      context.config.function_declaration_brace_position
    } else {
      context.config.function_expression_brace_position
    };
    let open_brace_token = context.token_finder.get_first_open_brace_token_within(body);

    items.extend(gen_brace_separator(
      GenBraceSeparatorOptions {
        brace_position,
        open_brace_token,
        start_header_lsil: Some(start_header_lsil),
      },
      context,
    ));

    items.extend(gen_node(body.into_node(), context));
  } else if context.config.semicolons.is_true() {
    items.push_sc(sc!(";"));
  }

  return items;

  fn get_use_space_before_parens<'a>(is_func_decl: bool, context: &mut Context<'a>) -> bool {
    if is_func_decl {
      context.config.function_declaration_space_before_parentheses
    } else {
      context.config.function_expression_space_before_parentheses
    }
  }
}

pub(super) fn gen_param<'a>(node: &'a FormalParameter<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  // swc had a separate node for a parameter that's also a property
  if node.accessibility.is_some() || node.readonly || node.r#override {
    return gen_parameter_prop(node, context);
  }

  let mut items = PrintItems::new(context.arena);
  items.extend(gen_decorators(&node.decorators, DecoratorsSeparator::Space, context));
  items.extend(gen_param_without_decorators(node, context));
  items
}

/// Generates the pattern, optional `?`, type annotation, and initializer of a parameter,
/// which swc stored in the parameter's pattern (ex. an AssignPat with a BindingIdent).
pub(super) fn gen_param_without_decorators<'a>(node: &'a FormalParameter<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  items.extend(gen_node((&node.pattern).into_node(), context));
  if node.optional {
    items.push_sc(sc!("?"));
  }
  items.extend(gen_type_ann_with_colon_if_exists(node.type_annotation.as_deref(), context));
  if let Some(initializer) = &node.initializer {
    let initializer: &'a Expression<'a> = initializer;
    items.extend(gen_assignment(initializer.into_node(), sc!("="), context));
  }
  items
}

/// Generates a rest parameter (ex. `...a: string[]`), which swc stored as a
/// parameter with a rest pattern that had the type annotation.
pub(super) fn gen_formal_parameter_rest<'a>(node: &'a FormalParameterRest<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  items.extend(gen_decorators(&node.decorators, DecoratorsSeparator::Space, context));
  items.extend(gen_node((&node.rest).into_node(), context));
  items.extend(gen_type_ann_with_colon_if_exists(node.type_annotation.as_deref(), context));
  items
}

pub(super) fn gen_import_decl<'a>(node: &'a ImportDeclaration<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  // fill specifiers
  let mut default_import: Option<&'a ImportDefaultSpecifier<'a>> = None;
  let mut namespace_import: Option<&'a ImportNamespaceSpecifier<'a>> = None;
  let mut named_imports: Vec<&'a ImportSpecifier<'a>> = Vec::with_capacity(node.specifiers.as_ref().map(|s| s.len()).unwrap_or(0));

  if let Some(specifiers) = &node.specifiers {
    for specifier in specifiers.iter() {
      match specifier {
        ImportDeclarationSpecifier::ImportDefaultSpecifier(node) => default_import = Some(&**node),
        ImportDeclarationSpecifier::ImportNamespaceSpecifier(node) => namespace_import = Some(&**node),
        ImportDeclarationSpecifier::ImportSpecifier(node) => named_imports.push(&**node),
      }
    }
  }

  let force_single_line = context.config.import_declaration_force_single_line && !contains_line_or_multiline_comment(node.into_node(), context.program);

  let force_multi_line = context.config.import_declaration_force_multi_line == ForceMultiLine::Always
    || (named_imports.len() > 1 && context.config.import_declaration_force_multi_line == ForceMultiLine::WhenMultiple);

  let should_single_line = force_single_line
    || (default_import.is_none()
      && namespace_import.is_none()
      && !force_multi_line
      && (named_imports.len() <= 1 && context.config.import_declaration_force_multi_line == ForceMultiLine::Never)
      && node.start_line_fast(context.program) == node.end_line_fast(context.program));

  let has_named_imports = !named_imports.is_empty() || {
    let from_keyword = context.token_finder.get_previous_token_if_from_keyword(&node.source);
    if let Some(from_keyword) = from_keyword {
      context.token_finder.get_previous_token_if_close_brace(&from_keyword.range()).is_some()
    } else {
      false
    }
  };
  let has_from = default_import.is_some() || namespace_import.is_some() || has_named_imports;
  let mut items = PrintItems::new(context.arena);

  items.push_sc(sc!("import "));
  if matches!(node.import_kind, ImportOrExportKind::Type) {
    items.push_sc(sc!("type "));
  }

  match node.phase {
    None => {
      // ignore
    }
    Some(ImportPhase::Source) => {
      items.push_sc(sc!("source "));
    }
    Some(ImportPhase::Defer) => {
      items.push_sc(sc!("defer "));
    }
  }

  if let Some(default_import) = default_import {
    items.extend(gen_node(default_import.into_node(), context));
    if namespace_import.is_some() || has_named_imports {
      items.push_sc(sc!(", "));
    }
  }
  if let Some(namespace_import) = namespace_import {
    items.extend(gen_node(namespace_import.into_node(), context));
  }

  if has_named_imports {
    items.extend(gen_named_import_or_export_specifiers(
      GenNamedImportOrExportSpecifierOptions {
        parent: node.into_node(),
        specifiers: named_imports.into_iter().map(|x| x.into_node()).collect(),
        force_single_line,
        force_multi_line_specifiers: force_multi_line,
      },
      context,
    ));
  }

  if has_from {
    items.push_sc(sc!(" from "));
  }

  items.extend(gen_node((&node.source).into_node(), context));

  if let Some(with_clause) = &node.with_clause {
    items.extend(gen_with_clause_with_keyword(with_clause, context));
  }

  if context.config.semicolons.is_true() {
    items.push_sc(sc!(";"));
  }

  if should_single_line { with_no_new_lines(items) } else { items }
}

/// Generates the keyword of an import attributes clause followed by the clause (ex. ` with { type: "json" }`).
pub(super) fn gen_with_clause_with_keyword<'a>(node: &'a WithClause<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  items.push_sc(match node.keyword {
    WithClauseKeyword::Assert => sc!(" assert "),
    WithClauseKeyword::With => sc!(" with "),
  });
  items.extend(gen_node(node.into_node(), context));
  items
}

/// Generates the object of an import attributes clause (ex. `{ type: "json" }`), which swc
/// stored as an object literal, so this is generated like one (see gen_object_lit).
pub(super) fn gen_with_clause<'a>(node: &'a WithClause<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  return context.with_maybe_consistent_props(
    node,
    |node| use_consistent_quotes_for_attributes(node),
    |context, node| {
      gen_object_like_node(
        GenObjectLikeNodeOptions {
          node: node.into_node(),
          members: node.with_entries.iter().map(|x| x.into_node()).collect(),
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

  /// The same as use_consistent_quotes_for_members for swc's key value properties.
  fn use_consistent_quotes_for_attributes(node: &WithClause) -> bool {
    node.with_entries.iter().any(|attribute| match &attribute.key {
      ImportAttributeKey::Identifier(ident) => !is_text_valid_identifier(ident.name.as_str()),
      ImportAttributeKey::StringLiteral(str) => match str.value.as_str() {
        Some(text) => !is_text_valid_identifier(text),
        None => true,
      },
    })
  }
}

/// Generates an import attribute (ex. `type: "json"`), which swc stored as a key value property.
pub(super) fn gen_import_attribute<'a>(node: &'a ImportAttribute<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  items.extend(gen_quotable_prop((&node.key).into_node(), context));
  items.extend(gen_assignment((&node.value).into_node(), sc!(":"), context));
  items
}

pub(super) fn gen_import_equals_decl<'a>(node: &'a TSImportEqualsDeclaration<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  // note: `export import a = b;` is in an ExportDeclaration, which generates the `export`

  items.push_sc(sc!("import "));
  if matches!(node.import_kind, ImportOrExportKind::Type) {
    items.push_sc(sc!("type "));
  }
  items.extend(gen_node((&node.id).into_node(), context));
  items.push_sc(sc!(" = ")); // keep on one line
  items.extend(gen_node((&node.module_reference).into_node(), context));

  if context.config.semicolons.is_true() {
    items.push_sc(sc!(";"));
  }

  items
}

pub(super) fn gen_interface_decl<'a>(node: &'a TSInterfaceDeclaration<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  let start_header_lsil = LineStartIndentLevel::new("startHeader");
  items.push_info(start_header_lsil);
  context.store_lsil_for_node(node, start_header_lsil);

  if node.declare {
    items.push_sc(sc!("declare "));
  }
  items.push_sc(sc!("interface "));
  items.extend(gen_node((&node.id).into_node(), context));
  if let Some(type_params) = &node.type_parameters {
    items.extend(gen_node(type_params.into_node(), context));
  }
  items.extend(gen_extends_or_implements(
    GenExtendsOrImplementsOptions {
      text: sc!("extends"),
      type_items: node.extends.iter().map(|x| x.into_node()).collect(),
      start_header_lsil,
      prefer_hanging: context.config.extends_clause_prefer_hanging,
    },
    context,
  ));
  items.extend(gen_node((&node.body).into_node(), context));

  items
}

/// Generates a module declaration with a string name (ex. `declare module "a" {}`).
pub(super) fn gen_external_module_decl<'a>(node: &'a TSExternalModuleDeclaration<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  gen_module_or_namespace_decl(
    ModuleOrNamespaceDecl {
      declare: node.declare,
      keyword: Some(sc!("module ")),
      id: ModuleOrNamespaceDeclId::Node((&node.id).into_node()),
      body: node.body.as_deref().map(ModuleOrNamespaceBody::ModuleBlock),
    },
    context,
  )
}

/// Generates a namespace or module declaration (ex. `namespace a.b.c {}` or `module a {}`).
pub(super) fn gen_namespace_decl<'a>(node: &'a TSNamespaceDeclaration<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  gen_module_or_namespace_decl(
    ModuleOrNamespaceDecl {
      declare: node.declare,
      keyword: Some(match node.kind {
        TSNamespaceDeclarationKind::Namespace => sc!("namespace "),
        TSNamespaceDeclarationKind::Module => sc!("module "),
      }),
      id: ModuleOrNamespaceDeclId::Node((&node.id).into_node()),
      body: Some(ModuleOrNamespaceBody::from_namespace_body(&node.body)),
    },
    context,
  )
}

/// Generates a global declaration (ex. `declare global {}`).
pub(super) fn gen_global_decl<'a>(node: &'a TSGlobalDeclaration<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  gen_module_or_namespace_decl(
    ModuleOrNamespaceDecl {
      declare: node.declare,
      keyword: None,
      id: ModuleOrNamespaceDeclId::Global(SourceRange::from_span(node.global_span)),
      body: Some(ModuleOrNamespaceBody::ModuleBlock(&node.body)),
    },
    context,
  )
}

pub(super) struct ModuleOrNamespaceDecl<'a> {
  pub declare: bool,
  /// The `module` or `namespace` keyword, which `global` doesn't have.
  pub keyword: Option<&'static StringContainer<'static>>,
  pub id: ModuleOrNamespaceDeclId<'a>,
  pub body: Option<ModuleOrNamespaceBody<'a>>,
}

pub(super) enum ModuleOrNamespaceDeclId<'a> {
  Node(Node<'a>),
  /// The range of the `global` keyword, which swc had an identifier for.
  Global(SourceRange),
}

#[derive(Clone, Copy)]
pub(super) enum ModuleOrNamespaceBody<'a> {
  ModuleBlock(&'a TSModuleBlock<'a>),
  /// The next namespace of a nested namespace declaration (ex. `b.c {}` in `namespace a.b.c {}`).
  NamespaceDecl(&'a TSNamespaceDeclaration<'a>),
}

impl<'a> ModuleOrNamespaceBody<'a> {
  fn from_namespace_body(body: &'a TSNamespaceDeclarationBody<'a>) -> Self {
    match body {
      TSNamespaceDeclarationBody::TSModuleBlock(block) => ModuleOrNamespaceBody::ModuleBlock(block),
      TSNamespaceDeclarationBody::TSNamespaceDeclaration(decl) => ModuleOrNamespaceBody::NamespaceDecl(decl),
    }
  }
}

pub(super) fn gen_module_or_namespace_decl<'a>(node: ModuleOrNamespaceDecl<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);

  let start_header_lsil = LineStartIndentLevel::new("startHeader");
  items.push_info(start_header_lsil);

  if node.declare {
    items.push_sc(sc!("declare "));
  }
  if let Some(keyword) = node.keyword {
    items.push_sc(keyword);
  }

  match node.id {
    ModuleOrNamespaceDeclId::Node(id) => items.extend(gen_node(id, context)),
    ModuleOrNamespaceDeclId::Global(range) => items.extend(gen_range_with_comments(
      range,
      |context| {
        let mut items = PrintItems::new(context.arena);
        items.push_sc(sc!("global"));
        items
      },
      context,
    )),
  }
  items.extend(gen_body(node.body, start_header_lsil, context));

  return items;

  fn gen_body<'a>(body: Option<ModuleOrNamespaceBody<'a>>, start_header_lsil: LineStartIndentLevel, context: &mut Context<'a>) -> PrintItems<'a> {
    let mut items = PrintItems::new(context.arena);
    if let Some(body) = body {
      match body {
        ModuleOrNamespaceBody::ModuleBlock(block) => {
          items.extend(gen_membered_body(
            GenMemberedBodyOptions {
              node: block.into_node(),
              // directives (ex. "use strict") always come before the statements
              members: block
                .directives
                .iter()
                .map(|x| x.into_node())
                .chain(block.body.iter().map(|x| x.into_node()))
                .collect(),
              start_header_lsil: Some(start_header_lsil),
              brace_position: context.config.module_declaration_brace_position,
              block_padding: context.config.module_declaration_block_padding,
              get_forced_blank_line_count: |_, _, _| None,
              separator: Separator::none(),
              keep_blank_lines_before_close_token_comments: true,
            },
            context,
          ));
        }
        ModuleOrNamespaceBody::NamespaceDecl(decl) => {
          items.push_sc(sc!("."));
          items.extend(gen_node((&decl.id).into_node(), context));
          items.extend(gen_body(
            Some(ModuleOrNamespaceBody::from_namespace_body(&decl.body)),
            start_header_lsil,
            context,
          ));
        }
      }
    } else if context.config.semicolons.is_true() {
      items.push_sc(sc!(";"));
    }

    items
  }
}

pub(super) fn gen_type_alias<'a>(node: &'a TSTypeAliasDeclaration<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  if node.declare {
    items.push_sc(sc!("declare "));
  }
  items.push_sc(sc!("type "));
  items.extend(gen_node((&node.id).into_node(), context));
  if let Some(type_params) = &node.type_parameters {
    items.extend(gen_node(type_params.into_node(), context));
  }

  items.extend(gen_assignment((&node.type_annotation).into_node(), sc!("="), context));

  if context.config.semicolons.is_true() {
    items.push_sc(sc!(";"));
  }

  items
}

/* exports */

pub(super) struct GenNamedImportOrExportSpecifierOptions<'a> {
  pub parent: Node<'a>,
  pub specifiers: Vec<Node<'a>>,
  pub force_single_line: bool,
  pub force_multi_line_specifiers: bool,
}

pub(super) fn gen_named_import_or_export_specifiers<'a>(opts: GenNamedImportOrExportSpecifierOptions<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  return gen_object_like_node(
    GenObjectLikeNodeOptions {
      node: opts.parent,
      members: opts.specifiers,
      separator: get_trailing_commas(opts.parent, context).into(),
      prefer_hanging: get_prefer_hanging(opts.parent, context),
      prefer_single_line: get_prefer_single_line(opts.parent, context),
      force_single_line: opts.force_single_line,
      force_multi_line: opts.force_multi_line_specifiers,
      surround_single_line_with_spaces: get_use_space(opts.parent, context),
      blank_lines: ListBlankLines::Remove,
      node_sorter: get_node_sorter(opts.parent, context),
    },
    context,
  );

  fn get_trailing_commas<'a>(parent_decl: Node, context: &Context<'a>) -> TrailingCommas {
    match parent_decl {
      Node::ExportNamedDeclaration(_) | Node::ExportFromDeclaration(_) => context.config.export_declaration_trailing_commas,
      Node::ImportDeclaration(_) => context.config.import_declaration_trailing_commas,
      _ => unreachable!(),
    }
  }

  fn get_use_space<'a>(parent_decl: Node, context: &Context<'a>) -> bool {
    match parent_decl {
      Node::ExportNamedDeclaration(_) | Node::ExportFromDeclaration(_) => context.config.export_declaration_space_surrounding_named_exports,
      Node::ImportDeclaration(_) => context.config.import_declaration_space_surrounding_named_imports,
      _ => unreachable!(),
    }
  }

  fn get_prefer_hanging<'a>(parent_decl: Node, context: &Context<'a>) -> bool {
    match parent_decl {
      Node::ExportNamedDeclaration(_) | Node::ExportFromDeclaration(_) => context.config.export_declaration_prefer_hanging,
      Node::ImportDeclaration(_) => context.config.import_declaration_prefer_hanging,
      _ => unreachable!(),
    }
  }

  fn get_prefer_single_line<'a>(parent_decl: Node, context: &Context<'a>) -> bool {
    match parent_decl {
      Node::ExportNamedDeclaration(_) | Node::ExportFromDeclaration(_) => context.config.export_declaration_prefer_single_line,
      Node::ImportDeclaration(_) => context.config.import_declaration_prefer_single_line,
      _ => unreachable!(),
    }
  }

  fn get_node_sorter(parent_decl: Node, context: &Context) -> Option<NodeSorter> {
    match parent_decl {
      Node::ExportNamedDeclaration(_) | Node::ExportFromDeclaration(_) => get_node_sorter_from_order(
        context.config.export_declaration_sort_named_exports,
        context.config.export_declaration_sort_type_only_exports,
      ),
      Node::ImportDeclaration(_) => get_node_sorter_from_order(
        context.config.import_declaration_sort_named_imports,
        context.config.import_declaration_sort_type_only_imports,
      ),
      _ => unreachable!(),
    }
  }
}

pub(super) fn gen_export_named_specifier<'a>(node: &'a ExportSpecifier<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);

  let is_parent_type_only = match node.into_node().parent(context.program) {
    Some(Node::ExportNamedDeclaration(parent)) => matches!(parent.export_kind, ImportOrExportKind::Type),
    Some(Node::ExportFromDeclaration(parent)) => matches!(parent.export_kind, ImportOrExportKind::Type),
    _ => false,
  };
  if matches!(node.export_kind, ImportOrExportKind::Type) && !is_parent_type_only {
    items.push_sc(sc!("type "));
  }

  items.extend(gen_node((&node.local).into_node(), context));
  // oxc has the same range for both names when there's no alias
  if node.exported.range() != node.local.range() {
    items.push_signal(Signal::SpaceOrNewLine);
    items.push_condition(conditions::indent_if_start_of_line({
      let mut items = PrintItems::new(context.arena);
      items.push_sc(sc!("as "));
      items.extend(gen_node((&node.exported).into_node(), context));
      items
    }));
  }

  items
}
