use deno_ast::MediaType;
use deno_ast::ParsedSource;
use deno_ast::oxc::ast::AstKind;
use deno_ast::oxc::ast::ast::Comment;
use deno_ast::oxc::ast::ast::CommentKind;
use deno_ast::oxc::ast::ast::*;
use deno_ast::oxc::parser::Kind;
use deno_ast::oxc::parser::Token;
use dprint_core::formatting::condition_resolvers;
use dprint_core::formatting::conditions::*;
use dprint_core::formatting::ir_helpers::*;
use dprint_core::formatting::*;
use dprint_core_macros::sc;
use std::rc::Rc;

use super::generate_types::*;
use super::node_helpers;
use super::oxc_helpers::*;
use super::sorting::*;
use super::swc::get_flattened_bin_expr;
use super::swc::*;
use super::to_node::*;
use super::*;
use crate::configuration::*;
use crate::utils;

pub fn generate<'a>(
  parsed_source: &'a ParsedSource<'a>,
  config: &'a Configuration,
  external_formatter: Option<&'a ExternalFormatter>,
) -> anyhow::Result<PrintItems> {
  let program = parsed_source.program();
  let program_info = ProgramInfo::new(program, parsed_source.text_info_lazy(), parsed_source.tokens(), parsed_source.comments());
  let program_node = AstKind::Program(program);
  let mut context = Context::new(
    parsed_source.media_type(),
    parsed_source.tokens(),
    program_node,
    program_info,
    config,
    external_formatter,
  );
  let mut items = gen_node(program_node, &mut context);
  items.push_condition(if_true(
    "endOfFileNewLine",
    Rc::new(|context| Some(context.writer_info.column_number > 0 || context.writer_info.line_number > 0)),
    Signal::NewLine.into(),
  ));

  #[cfg(debug_assertions)]
  context.assert_end_of_file_state();

  if let Some(diagnostic) = context.diagnostics.pop() {
    return Err(anyhow::anyhow!(diagnostic.message));
  }

  if config.file_indent_level > 0 {
    Ok(with_indent_times(items, config.file_indent_level))
  } else {
    Ok(items)
  }
}

fn gen_node<'a>(node: Node<'a>, context: &mut Context<'a>) -> PrintItems {
  gen_node_with_inner_gen(node, context, |items, _| items)
}

fn gen_node_with_inner_gen<'a>(node: Node<'a>, context: &mut Context<'a>, inner_gen: impl FnOnce(PrintItems, &mut Context<'a>) -> PrintItems) -> PrintItems {
  let is_program = matches!(node, Node::Program(_));
  let is_jsx_text = matches!(node, Node::JSXText(_));
  let is_spread_element = matches!(node, Node::SpreadElement(_));

  // store info
  let past_current_node = std::mem::replace(&mut context.current_node, node);
  let parent_end = past_current_node.end();
  context.parent_stack.push(past_current_node);

  // handle decorators (since their starts can come before their parent)
  let mut items = handle_decorators_if_necessary(node, context);

  // now that decorators might have been generated, assert the node order to ensure comments are generated correctly
  #[cfg(debug_assertions)]
  assert_generated_in_order(node, context);

  // parse item
  let node_range = node.range();
  let node_start = node_range.start;
  let node_end = node_range.end;
  let mut has_ignore_comment = false;

  // do not get the comments for modules as this will be handled in gen_statements
  if !is_program {
    // get the leading comments
    if does_first_child_own_leading_comments_on_same_line(node, context) {
      // Some block comments should belong to the first child rather than the
      // parent node because their first child may end up on the next line.
      let leading_comments = context.comments.leading_comments_with_previous(node_start);
      has_ignore_comment = get_has_ignore_comment(&leading_comments, node, context);
      let program = context.program;
      let node_start_line = node.start_line_fast(program);
      let leading_comments_on_previous_lines = leading_comments.take_while(|c| c.is_line() || c.start_line_fast(program) < node_start_line);
      items.extend(gen_comment_collection(leading_comments_on_previous_lines, None, None, context));
    } else {
      let leading_comments = context.comments.leading_comments_with_previous(node_start);
      has_ignore_comment = get_has_ignore_comment(&leading_comments, node, context);
      items.extend(gen_comments_as_leading(&node_range, leading_comments, context));
    }
  }

  // generate the node
  if has_ignore_comment {
    items.push_force_current_line_indentation();
    let node_text = if is_jsx_text {
      // keep the leading text, but leave the trailing text to be formatted if on a separate line
      let node_text = node.text_fast(context.program);
      let end_trim = node_text.trim_end();
      if node_text[end_trim.len()..].contains('\n') { end_trim } else { node_text }
    } else {
      node.text_fast(context.program)
    };
    items.extend(inner_gen(ir_helpers::gen_from_raw_string(node_text), context));

    // mark any previous comments as handled
    for comment in context.comments.trailing_comments_with_previous(node_end) {
      if comment.start() < node_end {
        context.mark_comment_handled(comment);
      }
    }
  } else {
    items.extend(inner_gen(gen_node_inner(node, context), context));
  }

  // Get the trailing comments -- This needs to be done based on the parse
  // stack order because certain nodes like binary expressions are flattened
  if node_end != parent_end || matches!(context.parent(), Node::Program(_)) {
    let trailing_comments = context.comments.trailing_comments_with_previous(node_end);
    items.extend(gen_comments_as_trailing(&node_range, trailing_comments, context));
  }

  let items = if let Some((ln, isol)) = context.take_current_before_comments_start_info() {
    let mut new_items = PrintItems::new();
    new_items.push_info(ln);
    new_items.push_info(isol);
    new_items.extend(items);
    new_items
  } else {
    items
  };

  // pop info
  context.current_node = context.parent_stack.pop();

  // need to ensure a jsx spread element's comments are generated within the braces since swc
  // has no representation of a JSX spread attribute and goes straight to the spread element
  // (note: oxc does model JSX spread attributes, so this generally won't trigger)
  return if is_spread_element && matches!(context.current_node, Node::JSXOpeningElement(_)) {
    gen_as_jsx_expr_container(node, items, None, context)
  } else {
    items
  };

  fn gen_node_inner<'a>(node: Node<'a>, context: &mut Context<'a>) -> PrintItems {
    match node {
      /* top level */
      Node::Program(node) => gen_program_node(node, context),
      /* class */
      Node::Decorator(node) => gen_decorator(node, context),
      Node::MethodDefinition(node) => gen_method_definition(node, context),
      Node::PropertyDefinition(node) => gen_class_prop(node, context),
      Node::AccessorProperty(node) => gen_auto_accessor(node, context),
      Node::PrivateIdentifier(node) => gen_private_name(node, context),
      Node::StaticBlock(node) => gen_static_block(node, context),
      Node::ClassBody(node) => gen_class_body(node, context),
      /* clauses */
      Node::CatchClause(node) => gen_catch_clause(node, context),
      /* common */
      Node::IdentifierReference(node) => gen_identifier(node, context),
      Node::IdentifierName(node) => gen_ident_name(node, context),
      Node::BindingIdentifier(node) => gen_binding_identifier(node, context),
      Node::LabelIdentifier(node) => gen_from_raw_string(node.text_fast(context.program)),
      /* declarations (Function/Class handle both decl & expr based on context) */
      Node::Function(node) => gen_function(node, context),
      Node::Class(node) => gen_class(node, context),
      Node::TSImportEqualsDeclaration(node) => gen_import_equals_decl(node, context),
      Node::TSInterfaceDeclaration(node) => gen_interface_decl(node, context),
      Node::TSGlobalDeclaration(node) => gen_global_decl(node, context),
      Node::TSModuleDeclaration(node) => gen_module_decl(node, context),
      Node::TSTypeAliasDeclaration(node) => gen_type_alias(node, context),
      Node::VariableDeclaration(node) => gen_var_decl(node, context),
      Node::VariableDeclarator(node) => gen_var_declarator(node, context),
      /* expressions */
      Node::ArrayExpression(node) => gen_array_expr(node, context),
      Node::ArrowFunctionExpression(node) => gen_arrow_func_expr(node, context),
      Node::AssignmentExpression(node) => gen_assignment_expr(node, context),
      Node::AwaitExpression(node) => gen_await_expr(node, context),
      Node::BinaryExpression(node) => gen_binary_expr(BinaryLikeExpr::Binary(node), context),
      Node::LogicalExpression(node) => gen_binary_expr(BinaryLikeExpr::Logical(node), context),
      Node::CallExpression(node) => gen_call_or_opt_expr(CallOrOptCallExpr(node), context),
      Node::ChainExpression(node) => gen_node(chain_element_to_node(&node.expression), context),
      Node::ConditionalExpression(node) => gen_conditional_expr(node, context),
      Node::ImportExpression(node) => gen_import_expr(node, context),
      Node::StaticMemberExpression(node) => gen_member_expr(MemberExpr::Static(node), context),
      Node::ComputedMemberExpression(node) => gen_member_expr(MemberExpr::Computed(node), context),
      Node::PrivateFieldExpression(node) => gen_member_expr(MemberExpr::Private(node), context),
      Node::MetaProperty(node) => gen_meta_prop_expr(node, context),
      Node::NewExpression(node) => gen_new_expr(node, context),
      Node::ObjectExpression(node) => gen_object_lit(node, context),
      Node::ObjectProperty(node) => gen_object_property(node, context),
      Node::ParenthesizedExpression(node) => gen_paren_expr(node, context),
      Node::PrivateInExpression(node) => gen_private_in_expr(node, context),
      Node::SequenceExpression(node) => gen_sequence_expr(node, context),
      Node::SpreadElement(node) => gen_spread_element(node, context),
      Node::Super(_) => "super".into(),
      Node::TaggedTemplateExpression(node) => gen_tagged_tpl(node, context),
      Node::ThisExpression(_) => "this".into(),
      Node::TemplateLiteral(node) => gen_tpl(node, context),
      Node::TemplateElement(node) => gen_tpl_element(node, context),
      Node::TSAsExpression(node) => gen_as_expr(node, context),
      Node::TSSatisfiesExpression(node) => gen_satisfies_expr(node, context),
      Node::TSNonNullExpression(node) => gen_non_null_expr(node, context),
      Node::TSTypeAssertion(node) => gen_type_assertion(node, context),
      Node::TSInstantiationExpression(node) => gen_ts_instantiation(node, context),
      Node::UnaryExpression(node) => gen_unary_expr(node, context),
      Node::UpdateExpression(node) => gen_update_expr(node, context),
      Node::YieldExpression(node) => gen_yield_expr(node, context),
      /* imports / exports */
      Node::ImportDeclaration(node) => gen_import_decl(node, context),
      Node::ImportSpecifier(node) => gen_import_named_specifier(node, context),
      Node::ImportDefaultSpecifier(node) => gen_node(Node::BindingIdentifier(&node.local), context),
      Node::ImportNamespaceSpecifier(node) => gen_import_namespace_specifier(node, context),
      Node::ExportNamedDeclaration(node) => gen_export_named_decl(node, context),
      Node::ExportDefaultDeclaration(node) => gen_export_default_decl(node, context),
      Node::ExportAllDeclaration(node) => gen_export_all(node, context),
      Node::ExportSpecifier(node) => gen_export_named_specifier(node, context),
      Node::TSExportAssignment(node) => gen_export_assignment(node, context),
      Node::TSNamespaceExportDeclaration(node) => gen_namespace_export(node, context),
      Node::TSExternalModuleReference(node) => gen_external_module_ref(node, context),
      /* interface / type element */
      Node::TSCallSignatureDeclaration(node) => gen_call_signature_decl(node, context),
      Node::TSConstructSignatureDeclaration(node) => gen_construct_signature_decl(node, context),
      Node::TSIndexSignature(node) => gen_index_signature(node, context),
      Node::TSIndexSignatureName(node) => gen_index_signature_name(node, context),
      Node::TSInterfaceBody(node) => gen_interface_body(node, context),
      Node::TSClassImplements(node) => gen_class_implements(node, context),
      Node::TSInterfaceHeritage(node) => gen_interface_heritage(node, context),
      Node::TSMethodSignature(node) => gen_method_signature(node, context),
      Node::TSPropertySignature(node) => gen_property_signature(node, context),
      Node::TSTypeLiteral(node) => gen_type_lit(node, context),
      /* jsx */
      Node::JSXAttribute(node) => gen_jsx_attribute(node, context),
      Node::JSXClosingElement(node) => gen_jsx_closing_element(node, context),
      Node::JSXClosingFragment(node) => gen_jsx_closing_fragment(node, context),
      Node::JSXElement(node) => gen_jsx_element(node, context),
      Node::JSXEmptyExpression(node) => gen_jsx_empty_expr(node, context),
      Node::JSXExpressionContainer(node) => gen_jsx_expr_container(node, context),
      Node::JSXFragment(node) => gen_jsx_fragment(node, context),
      Node::JSXMemberExpression(node) => gen_jsx_member_expr(node, context),
      Node::JSXNamespacedName(node) => gen_jsx_namespaced_name(node, context),
      Node::JSXOpeningElement(node) => gen_jsx_opening_element(node, context),
      Node::JSXOpeningFragment(node) => gen_jsx_opening_fragment(node, context),
      Node::JSXSpreadAttribute(node) => gen_jsx_spread_attribute(node, context),
      Node::JSXSpreadChild(node) => gen_jsx_spread_child(node, context),
      Node::JSXText(node) => gen_jsx_text(node, context),
      Node::JSXIdentifier(node) => gen_from_raw_string(node.text_fast(context.program)),
      /* literals */
      Node::BigIntLiteral(node) => gen_big_int_literal(node, context),
      Node::BooleanLiteral(node) => gen_bool_literal(node),
      Node::NullLiteral(_) => "null".into(),
      Node::NumericLiteral(node) => gen_num_literal(node, context),
      Node::RegExpLiteral(node) => gen_reg_exp_literal(node, context),
      Node::StringLiteral(node) => gen_string_literal(node, context),
      /* patterns */
      Node::ArrayPattern(node) => gen_array_pat(node, context),
      Node::AssignmentPattern(node) => gen_assign_pat(node, context),
      Node::ArrayAssignmentTarget(node) => gen_array_assignment_target(node, context),
      Node::AssignmentTargetRest(node) => gen_assignment_target_rest(node, context),
      Node::AssignmentTargetWithDefault(node) => gen_assignment_target_with_default(node, context),
      Node::ObjectAssignmentTarget(node) => gen_object_assignment_target(node, context),
      Node::AssignmentTargetPropertyIdentifier(node) => gen_assignment_target_property_identifier(node, context),
      Node::AssignmentTargetPropertyProperty(node) => gen_assignment_target_property_property(node, context),
      Node::ObjectPattern(node) => gen_object_pat(node, context),
      Node::BindingProperty(node) => gen_key_value_pat_prop(node, context),
      Node::BindingRestElement(node) => gen_rest_pat(node, context),
      Node::FormalParameter(node) => gen_param(node, context),
      Node::FormalParameterRest(node) => gen_formal_parameter_rest(node, context),
      Node::ImportAttribute(node) => gen_import_attribute(node, context),
      /* statements */
      Node::BlockStatement(node) => gen_block_stmt(node, context),
      Node::FunctionBody(node) => gen_function_body(node, context),
      Node::BreakStatement(node) => gen_break_stmt(node, context),
      Node::ContinueStatement(node) => gen_continue_stmt(node, context),
      Node::DebuggerStatement(node) => gen_debugger_stmt(node, context),
      Node::Directive(node) => gen_directive(node, context),
      Node::DoWhileStatement(node) => gen_do_while_stmt(node, context),
      Node::ExpressionStatement(node) => gen_expr_stmt(node, context),
      Node::EmptyStatement(node) => gen_empty_stmt(node, context),
      Node::ForInStatement(node) => gen_for_in_stmt(node, context),
      Node::ForOfStatement(node) => gen_for_of_stmt(node, context),
      Node::ForStatement(node) => gen_for_stmt(node, context),
      Node::IfStatement(node) => gen_if_stmt(node, context),
      Node::LabeledStatement(node) => gen_labeled_stmt(node, context),
      Node::ReturnStatement(node) => gen_return_stmt(node, context),
      Node::SwitchStatement(node) => gen_switch_stmt(node, context),
      Node::SwitchCase(node) => gen_switch_case(node, context),
      Node::ThrowStatement(node) => gen_throw_stmt(node, context),
      Node::TryStatement(node) => gen_try_stmt(node, context),
      Node::WhileStatement(node) => gen_while_stmt(node, context),
      /* enums / modules */
      Node::TSEnumDeclaration(node) => gen_enum_decl(node, context),
      Node::TSEnumMember(node) => gen_enum_member(node, context),
      /* types */
      Node::TSArrayType(node) => gen_array_type(node, context),
      Node::TSConditionalType(node) => gen_conditional_type(node, context),
      Node::TSConstructorType(node) => gen_constructor_type(node, context),
      Node::TSFunctionType(node) => gen_function_type(node, context),
      Node::TSImportType(node) => gen_import_type(node, context),
      Node::TSImportTypeQualifiedName(node) => gen_import_type_qualified_name(node, context),
      Node::TSIndexedAccessType(node) => gen_indexed_access_type(node, context),
      Node::TSInferType(node) => gen_infer_type(node, context),
      Node::TSIntersectionType(node) => gen_intersection_type(node, context),
      Node::TSLiteralType(node) => gen_lit_type(node, context),
      Node::TSMappedType(node) => gen_mapped_type(node, context),
      Node::TSOptionalType(node) => gen_optional_type(node, context),
      Node::TSQualifiedName(node) => gen_qualified_name(node, context),
      Node::TSParenthesizedType(node) => gen_parenthesized_type(node, context),
      Node::TSRestType(node) => gen_rest_type(node, context),
      Node::TSThisType(_) => "this".into(),
      Node::TSTemplateLiteralType(node) => gen_tpl_lit_type(node, context),
      Node::TSTupleType(node) => gen_tuple_type(node, context),
      Node::TSNamedTupleMember(node) => gen_tuple_element(node, context),
      Node::TSTypeAnnotation(node) => gen_type_ann(node, context),
      Node::TSTypeParameter(node) => gen_type_param(node, context),
      Node::TSTypeParameterDeclaration(node) => gen_type_parameters(TypeParamNode::Decl(node), context),
      Node::TSTypeParameterInstantiation(node) => gen_type_parameters(TypeParamNode::Instantiation(node), context),
      Node::TSTypeOperator(node) => gen_type_operator(node, context),
      Node::TSTypePredicate(node) => gen_type_predicate(node, context),
      Node::TSTypeQuery(node) => gen_type_query(node, context),
      Node::TSTypeReference(node) => gen_type_reference(node, context),
      Node::TSUnionType(node) => gen_union_type(node, context),
      /* keyword types */
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
      | Node::TSVoidKeyword(_) => gen_keyword_type(node, context),
      /* These should never be matched. Return its text if so */
      _ => {
        if cfg!(debug_assertions) {
          panic!("Debug panic! Did not expect to generate IR for this node type.");
        }

        gen_from_raw_string(node.text_fast(context.program))
      }
    }
  }

  #[inline]
  fn handle_decorators_if_necessary<'a>(node: Node<'a>, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();

    // decorators in these cases will have starts before their parent so they need to be handled specially
    match node {
      Node::ExportNamedDeclaration(decl) => {
        if let Some(Declaration::ClassDeclaration(class)) = &decl.declaration {
          items.extend(gen_decorators(&class.decorators, false, context));
        }
      }
      Node::ExportDefaultDeclaration(decl) => {
        if let ExportDefaultDeclarationKind::ClassDeclaration(class) = &decl.declaration {
          items.extend(gen_decorators(&class.decorators, false, context));
        }
      }
      _ => {}
    }

    items
  }

  #[inline]
  fn does_first_child_own_leading_comments_on_same_line(node: Node, context: &mut Context) -> bool {
    match node {
      Node::TSUnionType(_) | Node::TSIntersectionType(_) => {
        let node_start_line = node.start_line_fast(context.program);
        let has_node_leading_block = node
          .leading_comments_fast(context.program)
          .any(|c| c.is_block() && c.start_line_fast(context.program) == node_start_line);
        let has_previous_token_trailing_block = context.token_finder.get_previous_token(&node.range()).is_some_and(|token| {
          token
            .end()
            .trailing_comments_fast(context.program)
            .any(|c| c.is_block() && c.start_line_fast(context.program) == node_start_line)
        });
        has_node_leading_block || has_previous_token_trailing_block
      }
      _ => false,
    }
  }

  #[cfg(debug_assertions)]
  fn assert_generated_in_order(node: Node, context: &mut Context) {
    let node_pos = node.start();
    if context.last_generated_node_pos > node_pos {
      // When this panic happens it means that a node with a start further
      // along in the file has been generated before this current node. When
      // that occurs, comments that this node "owns" might have been shifted
      // over to the further along node since "forgotten" comments get
      // prepended when a node is being generated.
      //
      // Do the following steps to solve:
      //
      // 1. Uncomment the lines in `gen_node_with_inner_gen` in order to
      //    display the node kinds.
      // 2. Add a test that reproduces the issue then run the tests and see
      //    where it panics and how that node looks. Ensure the node widths
      //    are correct. If not, that's a bug in swc, so go fix it in swc.
      // 3. If it's not a bug in swc, then check the parsing code to ensure
      //    the nodes are being generated in order.
      panic!("Debug panic! Node comments retrieved out of order!");
    }
    context.last_generated_node_pos = node_pos;
  }
}

fn get_has_ignore_comment<'a>(leading_comments: &CommentsIterator<'a>, node: Node<'a>, context: &mut Context<'a>) -> bool {
  let comments = match context.parent() {
    Node::JSXElement(jsx_element) => get_comments_for_jsx_children(&jsx_element.children, &node.start(), context),
    Node::JSXFragment(jsx_fragment) => get_comments_for_jsx_children(&jsx_fragment.children, &node.start(), context),
    _ => leading_comments.clone(),
  };

  for comment in comments {
    if ir_helpers::text_has_dprint_ignore(comment.text_fast(context.program), &context.config.ignore_node_comment_text) {
      return true;
    }
  }

  return false;

  fn get_comments_for_jsx_children<'a>(children: &'a [JSXChild<'a>], node_lo: &SourcePos, context: &mut Context<'a>) -> CommentsIterator<'a> {
    let mut iterator = CommentsIterator::empty();
    let index = if let Ok(index) = children.binary_search_by_key(node_lo, |child| child.start()) {
      index
    } else {
      return iterator;
    };

    for i in (0..index).rev() {
      match children.get(i).unwrap() {
        JSXChild::ExpressionContainer(expr_container) => {
          match &expr_container.expression {
            JSXExpression::EmptyExpression(_) => {
              iterator.extend(get_jsx_expr_container_empty_comments(expr_container, context));
            }
            _ => break,
          };
        }
        JSXChild::Text(jsx_text) => {
          if !jsx_text.text_fast(context.program).trim().is_empty() {
            break;
          }
        }
        _ => break,
      }
    }

    iterator
  }
}

fn is_ignore_jsx_expr_container(node: Node, context: &Context) -> bool {
  if let Node::JSXExpressionContainer(expr_container) = node {
    if let JSXExpression::EmptyExpression(_) = &expr_container.expression {
      for comment in get_jsx_expr_container_empty_comments(expr_container, context) {
        if ir_helpers::text_has_dprint_ignore(comment.text_fast(context.program), &context.config.ignore_node_comment_text) {
          return true;
        }
      }
    }
  }

  false
}

/* class */

// oxc unifies SWC's ClassMethod/PrivateMethod/Constructor into a single
// `MethodDefinition` (distinguished by `.kind`), wrapping a `Function`.
fn gen_method_definition<'a>(node: &'a MethodDefinition<'a>, context: &mut Context<'a>) -> PrintItems {
  let func = &node.value;
  gen_class_or_object_method(
    ClassOrObjectMethod {
      node: Node::MethodDefinition(node),
      parameters_range: func.get_parameters_range(context),
      decorators: Some(&node.decorators),
      accessibility: node.accessibility,
      is_static: node.r#static,
      is_async: func.r#async,
      is_abstract: matches!(node.r#type, MethodDefinitionType::TSAbstractMethodDefinition),
      kind: node.kind.into(),
      is_generator: func.generator,
      is_optional: node.optional,
      is_override: node.r#override,
      key: prop_key_to_node(&node.key),
      key_computed: node.computed,
      type_params: func.type_parameters.as_deref().map(Node::TSTypeParameterDeclaration),
      params: formal_params_to_nodes(&func.params),
      return_type: func.return_type.as_deref().map(Node::TSTypeAnnotation),
      body: func.body.as_deref().map(Node::FunctionBody),
    },
    context,
  )
}

fn gen_auto_accessor<'a>(node: &'a AccessorProperty<'a>, context: &mut Context<'a>) -> PrintItems {
  gen_class_prop_common(
    GenClassPropCommon {
      original: Node::AccessorProperty(node),
      key: prop_key_to_node(&node.key),
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

fn gen_class_prop<'a>(node: &'a PropertyDefinition<'a>, context: &mut Context<'a>) -> PrintItems {
  gen_class_prop_common(
    GenClassPropCommon {
      original: Node::PropertyDefinition(node),
      key: prop_key_to_node(&node.key),
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

fn gen_decorator<'a>(node: &'a Decorator<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.push_sc(sc!("@"));
  items.extend(gen_node(expr_to_node(&node.expression), context));
  items
}

fn gen_private_name<'a>(node: &PrivateIdentifier<'a>, _context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.push_sc(sc!("#"));
  items.push_string(node.name.as_str().to_string());
  items
}

struct GenClassPropCommon<'a> {
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

fn gen_class_prop_common<'a>(node: GenClassPropCommon<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.extend(gen_decorators(node.decorators, false, context));
  if node.is_declare {
    items.push_sc(sc!("declare "));
  }
  if let Some(accessibility) = node.accessibility {
    items.push_string(format!("{} ", accessibility_to_str(accessibility)));
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
    // oxc has no ComputedPropName wrapper - the key node is the inner expression.
    let inner_key_node = node.key;
    gen_computed_prop_like(
      |context| gen_node(inner_key_node, context),
      GenComputedPropLikeOptions {
        inner_node_range: inner_key_node.range(),
      },
      context,
    )
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
    items.extend(gen_assignment(expr_to_node(value), sc!("="), context));
  }

  let should_semi = context.config.semi_colons.is_true()
    || matches!(
      node.original.next_token_fast(context.program).map(|t| t.kind()),
      Some(Kind::LBrack | Kind::Star)
    );
  if should_semi {
    items.push_sc(sc!(";"));
  }

  items
}

fn gen_static_block<'a>(node: &'a StaticBlock<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  let start_header_lsil = LineStartIndentLevel::new("staticBlockStart");
  items.push_info(start_header_lsil);
  items.push_sc(sc!("static"));

  let open_brace_token = context.token_finder.get_first_open_brace_token_within(node);
  items.extend(gen_brace_separator(
    GenBraceSeparatorOptions {
      brace_position: context.config.static_block_brace_position,
      open_brace_token,
      start_header_lsil: Some(start_header_lsil),
    },
    context,
  ));

  // oxc's StaticBlock has no inner BlockStatement; emit the `{ ... }` ourselves.
  let block_range = open_brace_token.map(|t| SourceRange::new(t.start(), node.end()));
  items.extend(gen_block(
    |stmts, context| {
      let inner_range = block_range.map(|r| SourceRange::new(r.start + 1, r.end - 1)).unwrap_or_else(|| node.range());
      gen_statements(inner_range, stmts, context)
    },
    GenBlockOptions {
      range: block_range,
      children: node.body.iter().map(stmt_to_node).collect(),
    },
    context,
  ));

  items
}

/* clauses */

fn gen_catch_clause<'a>(node: &'a CatchClause<'a>, context: &mut Context<'a>) -> PrintItems {
  // a bit overkill since the param will currently always just be an identifer
  let start_header_ln = LineNumber::new("catchClauseHeaderStart");
  let start_header_lsil = LineStartIndentLevel::new("catchClauseHeaderStart");
  let end_header_ln = LineNumber::new("catchClauseHeaderEnd");
  let mut items = PrintItems::new();

  items.push_info(start_header_ln);
  items.push_info(start_header_lsil);
  items.push_sc(sc!("catch"));

  if let Some(param) = &node.param {
    items.push_sc(sc!(" ("));
    if context.config.catch_clause_space_around {
      items.push_space();
    }
    items.extend(gen_node(binding_pattern_to_node(&param.pattern), context));
    items.extend(gen_type_ann_with_colon_if_exists(param.type_annotation.as_deref(), context));
    if context.config.catch_clause_space_around {
      items.push_space();
    }
    items.push_sc(sc!(")"));
  }
  items.push_info(end_header_ln);

  let single_body_position = if matches!(context.parent(), Node::TryStatement(try_stmt) if try_stmt.finalizer.is_some()) {
    Some(SameOrNextLinePosition::NextLine)
  } else {
    None
  };

  // not conditional... required
  items.extend(
    gen_conditional_brace_body(
      GenConditionalBraceBodyOptions {
        body_node: Node::BlockStatement(&node.body),
        use_braces: UseBraces::Always,
        brace_position: context.config.try_statement_brace_position,
        single_body_position,
        requires_braces_condition_ref: None,
        start_header_info: Some((start_header_ln, start_header_lsil)),
        end_header_info: Some(end_header_ln),
      },
      context,
    )
    .generated_node,
  );

  items
}

/* common */

fn gen_identifier<'a>(node: &IdentifierReference<'a>, _: &mut Context<'a>) -> PrintItems {
  // Note: in oxc, optional (`?`) is a flag on the binding/parameter rather than
  // on the identifier reference, so it is emitted by those callers.
  node.name.as_str().to_string().into()
}

fn gen_ident_name<'a>(node: &IdentifierName<'a>, _: &mut Context<'a>) -> PrintItems {
  node.name.as_str().to_string().into()
}

fn gen_binding_identifier<'a>(node: &BindingIdentifier<'a>, _context: &mut Context<'a>) -> PrintItems {
  // In oxc the type annotation, optional (`?`) and definite (`!`) markers live
  // on the enclosing `BindingPattern` / `VariableDeclarator` / `FormalParameter`
  // rather than the identifier, so they are emitted by those callers.
  node.name.as_str().to_string().into()
}

/* declarations */

// oxc unifies SWC's ClassDecl + ClassExpr into a single `Class` node; whether it
// is a declaration or expression is recorded in `Class.r#type` (an `export default
// class C` counts as a declaration, matching SWC's old special case).
fn gen_class<'a>(node: &'a Class<'a>, context: &mut Context<'a>) -> PrintItems {
  let is_anonymous_default_export = node.id.is_none() && matches!(context.parent(), Node::ExportDefaultDeclaration(_));
  let is_class_decl = matches!(node.r#type, ClassType::ClassDeclaration) && !is_anonymous_default_export;
  gen_class_decl_or_expr(
    ClassDeclOrExpr {
      member_node: Node::ClassBody(&node.body),
      decorators: &node.decorators,
      is_class_decl,
      is_declare: node.declare,
      is_abstract: node.r#abstract,
      ident: node.id.as_ref().map(Node::BindingIdentifier),
      type_params: node.type_parameters.as_deref().map(Node::TSTypeParameterDeclaration),
      super_class: node.super_class.as_ref().map(expr_to_node),
      super_type_params: node.super_type_arguments.as_deref().map(Node::TSTypeParameterInstantiation),
      implements: node.implements.iter().map(Node::TSClassImplements).collect(),
      members: node.body.body.iter().map(class_element_to_node).collect(),
      brace_position: if is_class_decl {
        context.config.class_declaration_brace_position
      } else {
        context.config.class_expression_brace_position
      },
    },
    context,
  )
}

struct ClassDeclOrExpr<'a> {
  member_node: Node<'a>,
  decorators: &'a [Decorator<'a>],
  is_class_decl: bool,
  is_declare: bool,
  is_abstract: bool,
  ident: Option<Node<'a>>,
  type_params: Option<Node<'a>>,
  super_class: Option<Node<'a>>,
  super_type_params: Option<Node<'a>>,
  implements: Vec<Node<'a>>,
  members: Vec<Node<'a>>,
  brace_position: BracePosition,
}

fn gen_class_decl_or_expr<'a>(node: ClassDeclOrExpr<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  let (start_before_owned_comments_ln, start_before_owned_comments_isol) = context.get_or_create_current_before_comments_start_info();

  // generate decorators (export wrappers emit the class's decorators themselves
  // since in those cases the decorators start before their parent)
  let parent_handles_decorators = matches!(context.parent(), Node::ExportNamedDeclaration(_) | Node::ExportDefaultDeclaration(_));
  if !parent_handles_decorators {
    let is_inline = !node.is_class_decl;
    items.extend(gen_decorators(node.decorators, is_inline, context));
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
      conditions::NewLineIfHangingSpaceOtherwiseOptions {
        start_lsil: start_header_lsil,
        end_lsil: None,
        space_char: Some(conditions::if_above_width_or(context.config.indent_width, Signal::SpaceOrNewLine.into(), " ".into()).into()),
      },
    ));
    items.push_condition(conditions::indent_if_start_of_line({
      let mut items = PrintItems::new();
      items.push_sc(sc!("extends "));
      items.extend(new_line_group({
        let mut items = PrintItems::new();
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
          should_use_blank_line: move |previous, next, context| node_helpers::has_separating_blank_line(&previous, &next, context.program),
          separator: Separator::none(),
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
      Rc::new(move |context| {
        if context.resolved_is_start_of_line(start_before_owned_comments_isol)? {
          Some(false)
        } else {
          condition_helpers::is_multiple_lines(context, start_before_owned_comments_ln, start_header_ln)
        }
      }),
      with_indent(items.into()),
      items.into(),
    )
    .into()
  }
}

// The class body's members are normally generated inline by `gen_class`; this
// standalone handler exists for completeness when a `ClassBody` is visited directly.
fn gen_class_body<'a>(node: &'a ClassBody<'a>, context: &mut Context<'a>) -> PrintItems {
  let members: Vec<Node<'a>> = node.body.iter().map(class_element_to_node).collect();
  context.with_maybe_consistent_props(
    members,
    |members| use_consistent_quotes_for_members(members.iter().copied()),
    |context, members| {
      gen_membered_body(
        GenMemberedBodyOptions {
          node: Node::ClassBody(node),
          members,
          start_header_lsil: None,
          brace_position: context.config.class_declaration_brace_position,
          should_use_blank_line: move |previous, next, context| node_helpers::has_separating_blank_line(&previous, &next, context.program),
          separator: Separator::none(),
        },
        context,
      )
    },
  )
}

fn gen_export_default_decl<'a>(node: &'a ExportDefaultDeclaration<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  // decorators are handled in gen_node because their starts come before the export
  items.push_sc(sc!("export default "));
  items.extend(gen_node(export_default_decl_kind_to_node(&node.declaration), context));
  // `export default <expr>` needs a trailing semicolon; function/class/interface declarations do not
  let is_expr = !matches!(
    node.declaration,
    ExportDefaultDeclarationKind::FunctionDeclaration(_)
      | ExportDefaultDeclarationKind::ClassDeclaration(_)
      | ExportDefaultDeclarationKind::TSInterfaceDeclaration(_)
  );
  if is_expr && context.config.semi_colons.is_true() {
    items.push_sc(sc!(";"));
  }
  items
}

fn gen_enum_decl<'a>(node: &'a TSEnumDeclaration<'a>, context: &mut Context<'a>) -> PrintItems {
  let start_header_lsil = LineStartIndentLevel::new("startHeader");
  let mut items = PrintItems::new();

  // header
  items.push_info(start_header_lsil);

  if node.declare {
    items.push_sc(sc!("declare "));
  }
  if node.r#const {
    items.push_sc(sc!("const "));
  }
  items.push_sc(sc!("enum "));
  items.extend(gen_node(Node::BindingIdentifier(&node.id), context));

  // body
  let member_spacing = context.config.enum_declaration_member_spacing;
  items.extend(gen_membered_body(
    GenMemberedBodyOptions {
      node: Node::TSEnumDeclaration(node),
      members: node.body.members.iter().map(Node::TSEnumMember).collect(),
      start_header_lsil: Some(start_header_lsil),
      brace_position: context.config.enum_declaration_brace_position,
      should_use_blank_line: move |previous, next, context| match member_spacing {
        MemberSpacing::BlankLine => true,
        MemberSpacing::NewLine => false,
        MemberSpacing::Maintain => node_helpers::has_separating_blank_line(&previous, &next, context.program),
      },
      separator: context.config.enum_declaration_trailing_commas.into(),
    },
    context,
  ));

  items
}

fn gen_enum_member<'a>(node: &'a TSEnumMember<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.extend(gen_node(ts_enum_member_name_to_node(&node.id), context));

  if let Some(init) = &node.initializer {
    items.extend(gen_assignment(expr_to_node(init), sc!("="), context));
  }

  items
}

fn gen_export_named_decl<'a>(node: &'a ExportNamedDeclaration<'a>, context: &mut Context<'a>) -> PrintItems {
  // oxc folds `export <declaration>` (e.g. `export const x = 1`, `export function`/`class`)
  // into ExportNamedDeclaration.declaration.
  if let Some(declaration) = &node.declaration {
    let mut items = PrintItems::new();
    // decorators of an exported class are emitted in gen_node (they start before `export`)
    items.push_sc(sc!("export "));
    items.extend(gen_node(decl_to_node(declaration), context));
    return items;
  }

  // `export { ... } [from '...']` form (specifiers are all named in oxc; default exports are
  // ExportDefaultDeclaration and `export * as ns` is ExportAllDeclaration)
  let named_exports: Vec<&ExportSpecifier> = node.specifiers.iter().collect();

  let force_single_line =
    context.config.export_declaration_force_single_line && !contains_line_or_multiline_comment(Node::ExportNamedDeclaration(node), context.program);

  let force_multi_line = !force_single_line
    && ((context.config.export_declaration_force_multi_line == ForceMultiLine::Always)
      || (named_exports.len() > 1 && context.config.export_declaration_force_multi_line == ForceMultiLine::WhenMultiple));

  let should_single_line = force_single_line
    || (!force_multi_line
      && (named_exports.len() <= 1 && context.config.export_declaration_force_multi_line == ForceMultiLine::Never)
      && node.start_line_fast(context.program) == node.end_line_fast(context.program));

  // generate
  let mut items = PrintItems::new();

  items.push_sc(sc!("export "));
  if node.export_kind.is_type() {
    items.push_sc(sc!("type "));
  }

  if !named_exports.is_empty() {
    items.extend(gen_named_import_or_export_specifiers(
      GenNamedImportOrExportSpecifierOptions {
        parent: Node::ExportNamedDeclaration(node),
        specifiers: named_exports.into_iter().map(Node::ExportSpecifier).collect(),
        force_single_line,
        force_multi_line_specifiers: force_multi_line,
      },
      context,
    ));
  } else {
    items.push_sc(sc!("{}"));
  }

  if let Some(src) = &node.source {
    items.push_sc(sc!(" from "));
    items.extend(gen_node(Node::StringLiteral(src), context));
  }

  if let Some(with_clause) = &node.with_clause {
    items.extend(gen_with_clause(with_clause, context));
  }

  if context.config.semi_colons.is_true() {
    items.push_sc(sc!(";"));
  }

  if should_single_line { with_no_new_lines(items) } else { items }
}

// oxc unifies SWC's FnDecl + FnExpr into a single `Function` (decl-vs-expr via `.r#type`).
fn gen_function<'a>(node: &'a Function<'a>, context: &mut Context<'a>) -> PrintItems {
  let is_anonymous_default_export = node.id.is_none() && matches!(context.parent(), Node::ExportDefaultDeclaration(_));
  let is_func_decl = !is_anonymous_default_export && matches!(node.r#type, FunctionType::FunctionDeclaration | FunctionType::TSDeclareFunction);

  if !is_func_decl && context.config.function_expression_flat_iife && is_iife_fn_expr(node, context) {
    context.skip_iife_body_indent = true;
  }

  let items = gen_function_decl_or_expr(
    FunctionDeclOrExprNode {
      node: Node::Function(node),
      is_func_decl,
      ident: node.id.as_ref(),
      declare: node.declare,
      func: node,
    },
    context,
  );

  if !is_func_decl && should_add_parens_around_expr(Node::Function(node), context) {
    if context.config.paren_expression_space_around {
      surround_with_parens(surround_with_spaces(items))
    } else {
      surround_with_parens(items)
    }
  } else {
    items
  }
}

struct FunctionDeclOrExprNode<'a> {
  node: Node<'a>,
  is_func_decl: bool,
  ident: Option<&'a BindingIdentifier<'a>>,
  declare: bool,
  func: &'a Function<'a>,
}

fn gen_function_decl_or_expr<'a>(node: FunctionDeclOrExprNode<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
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
    items.extend(gen_node(Node::BindingIdentifier(ident), context));
  }
  if let Some(type_params) = &func.type_parameters {
    items.extend(gen_node(Node::TSTypeParameterDeclaration(type_params), context));
  }
  #[allow(clippy::collapsible_if)]
  if get_use_space_before_parens(node.is_func_decl, context) {
    if node.ident.is_some() || func.type_parameters.is_some() || !space_after_function_keyword {
      items.push_space();
    }
  }

  let param_nodes = formal_params_to_nodes(&func.params);
  let param_count = param_nodes.len();
  items.extend(gen_parameters_or_arguments(
    GenParametersOrArgumentsOptions {
      node: node.node,
      nodes: param_nodes,
      range: func.get_parameters_range(context),
      custom_close_paren: |context| {
        Some(gen_close_paren_with_type(
          GenCloseParenWithTypeOptions {
            start_lsil: start_header_lsil,
            type_node: func.return_type.as_deref().map(Node::TSTypeAnnotation),
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
    let open_brace_token = context.token_finder.get_first_open_brace_token_within(body.as_ref());

    items.extend(gen_brace_separator(
      GenBraceSeparatorOptions {
        brace_position,
        open_brace_token,
        start_header_lsil: Some(start_header_lsil),
      },
      context,
    ));

    items.extend(gen_node(Node::FunctionBody(body), context));
  } else if context.config.semi_colons.is_true() {
    items.push_sc(sc!(";"));
  }

  return items;

  fn get_use_space_before_parens(is_func_decl: bool, context: &mut Context) -> bool {
    if is_func_decl {
      context.config.function_declaration_space_before_parentheses
    } else {
      context.config.function_expression_space_before_parentheses
    }
  }
}

// oxc stores a function's rest parameter in a dedicated `FormalParameters.rest` field
// (a `FormalParameterRest`) rather than as the last item, so callers building the node
// list for parameter generation must append it.
fn formal_params_to_nodes<'a>(params: &'a FormalParameters<'a>) -> Vec<Node<'a>> {
  let mut nodes: Vec<Node<'a>> = params.items.iter().map(Node::FormalParameter).collect();
  if let Some(rest) = &params.rest {
    nodes.push(Node::FormalParameterRest(rest));
  }
  nodes
}

fn gen_formal_parameter_rest<'a>(node: &'a FormalParameterRest<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.extend(gen_decorators(&node.decorators, true, context));
  items.push_sc(sc!("..."));
  items.extend(gen_node(binding_pattern_to_node(&node.rest.argument), context));
  items.extend(gen_type_ann_with_colon_if_exists(node.type_annotation.as_deref(), context));
  items
}

fn gen_param<'a>(node: &'a FormalParameter<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.extend(gen_decorators(&node.decorators, true, context));
  // parameter-property modifiers (SWC's TsParamProp is folded into FormalParameter here)
  if let Some(accessibility) = node.accessibility {
    items.push_string(format!("{} ", accessibility_to_str(accessibility)));
  }
  if node.r#override {
    items.push_sc(sc!("override "));
  }
  if node.readonly {
    items.push_sc(sc!("readonly "));
  }
  if let BindingPattern::AssignmentPattern(assign) = &node.pattern {
    items.extend(gen_node(binding_pattern_to_node(&assign.left), context));
    if node.optional {
      items.push_sc(sc!("?"));
    }
    items.extend(gen_type_ann_with_colon_if_exists(node.type_annotation.as_deref(), context));
    items.extend(gen_assignment(expr_to_node(&assign.right), sc!("="), context));
  } else {
    items.extend(gen_node(binding_pattern_to_node(&node.pattern), context));
    if node.optional {
      items.push_sc(sc!("?"));
    }
    items.extend(gen_type_ann_with_colon_if_exists(node.type_annotation.as_deref(), context));
    if let Some(initializer) = &node.initializer {
      items.extend(gen_assignment(expr_to_node(initializer), sc!("="), context));
    }
  }
  items
}

fn gen_import_decl<'a>(node: &'a ImportDeclaration<'a>, context: &mut Context<'a>) -> PrintItems {
  // fill specifiers (oxc keeps them in a single Option<Vec<ImportDeclarationSpecifier>>)
  let mut default_import: Option<&ImportDefaultSpecifier> = None;
  let mut namespace_import: Option<&ImportNamespaceSpecifier> = None;
  let mut named_imports: Vec<&ImportSpecifier> = Vec::new();

  for specifier in node.specifiers.iter().flatten() {
    match specifier {
      ImportDeclarationSpecifier::ImportDefaultSpecifier(node) => default_import = Some(node),
      ImportDeclarationSpecifier::ImportNamespaceSpecifier(node) => namespace_import = Some(node),
      ImportDeclarationSpecifier::ImportSpecifier(node) => named_imports.push(node),
    }
  }

  let force_single_line =
    context.config.import_declaration_force_single_line && !contains_line_or_multiline_comment(Node::ImportDeclaration(node), context.program);

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
  let mut items = PrintItems::new();

  items.push_sc(sc!("import "));
  if node.import_kind.is_type() {
    items.push_sc(sc!("type "));
  }

  match node.phase {
    Some(ImportPhase::Source) => items.push_sc(sc!("source ")),
    Some(ImportPhase::Defer) => items.push_sc(sc!("defer ")),
    None => {}
  }

  if let Some(default_import) = default_import {
    items.extend(gen_node(Node::ImportDefaultSpecifier(default_import), context));
    if namespace_import.is_some() || has_named_imports {
      items.push_sc(sc!(", "));
    }
  }
  if let Some(namespace_import) = namespace_import {
    items.extend(gen_node(Node::ImportNamespaceSpecifier(namespace_import), context));
  }

  if has_named_imports {
    items.extend(gen_named_import_or_export_specifiers(
      GenNamedImportOrExportSpecifierOptions {
        parent: Node::ImportDeclaration(node),
        specifiers: named_imports.into_iter().map(Node::ImportSpecifier).collect(),
        force_single_line,
        force_multi_line_specifiers: force_multi_line,
      },
      context,
    ));
  }

  if has_from {
    items.push_sc(sc!(" from "));
  }

  items.extend(gen_node(Node::StringLiteral(&node.source), context));

  if let Some(with_clause) = &node.with_clause {
    items.extend(gen_with_clause(with_clause, context));
  }

  if context.config.semi_colons.is_true() {
    items.push_sc(sc!(";"));
  }

  if should_single_line { with_no_new_lines(items) } else { items }
}

fn gen_import_equals_decl<'a>(node: &'a TSImportEqualsDeclaration<'a>, context: &mut Context<'a>) -> PrintItems {
  // note: a leading `export` (for `export import X = ...`) is emitted by the wrapping
  // export declaration, not here.
  let mut items = PrintItems::new();
  items.push_sc(sc!("import "));
  if node.import_kind.is_type() {
    items.push_sc(sc!("type "));
  }
  items.extend(gen_node(Node::BindingIdentifier(&node.id), context));
  items.push_sc(sc!(" = ")); // keep on one line
  items.extend(gen_node(ts_module_reference_to_node(&node.module_reference), context));

  if context.config.semi_colons.is_true() {
    items.push_sc(sc!(";"));
  }

  items
}

fn gen_interface_decl<'a>(node: &'a TSInterfaceDeclaration<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  let start_header_lsil = LineStartIndentLevel::new("startHeader");
  items.push_info(start_header_lsil);
  context.store_lsil_for_node(node, start_header_lsil);

  if node.declare {
    items.push_sc(sc!("declare "));
  }
  items.push_sc(sc!("interface "));
  items.extend(gen_node(Node::BindingIdentifier(&node.id), context));
  if let Some(type_params) = &node.type_parameters {
    items.extend(gen_node(Node::TSTypeParameterDeclaration(type_params), context));
  }
  items.extend(gen_extends_or_implements(
    GenExtendsOrImplementsOptions {
      text: sc!("extends"),
      type_items: node.extends.iter().map(Node::TSInterfaceHeritage).collect(),
      start_header_lsil,
      prefer_hanging: context.config.extends_clause_prefer_hanging,
    },
    context,
  ));
  items.extend(gen_node(Node::TSInterfaceBody(&node.body), context));

  items
}

// oxc unifies SWC's TsModuleDecl + TsNamespaceDecl into a single TSModuleDeclaration
// (kind distinguishes `module`/`namespace`; nested `a.b {}` recurses via the body).
fn gen_module_decl<'a>(node: &'a TSModuleDeclaration<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();

  let start_header_lsil = LineStartIndentLevel::new("startHeader");
  items.push_info(start_header_lsil);

  if node.declare {
    items.push_sc(sc!("declare "));
  }
  items.push_sc(match node.kind {
    TSModuleDeclarationKind::Namespace => sc!("namespace "),
    TSModuleDeclarationKind::Module => sc!("module "),
  });

  items.extend(gen_node(ts_module_declaration_name_to_node(&node.id), context));
  items.extend(gen_body(node.body.as_ref(), start_header_lsil, context));

  return items;

  fn gen_body<'a>(body: Option<&'a TSModuleDeclarationBody<'a>>, start_header_lsil: LineStartIndentLevel, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    match body {
      Some(TSModuleDeclarationBody::TSModuleBlock(block)) => {
        let mut members: Vec<Node<'a>> = block.directives.iter().map(Node::Directive).collect();
        members.extend(block.body.iter().map(stmt_to_node));
        items.extend(gen_membered_body(
          GenMemberedBodyOptions {
            node: Node::TSModuleBlock(block),
            members,
            start_header_lsil: Some(start_header_lsil),
            brace_position: context.config.module_declaration_brace_position,
            should_use_blank_line: move |previous, next, context| node_helpers::has_separating_blank_line(&previous, &next, context.program),
            separator: Separator::none(),
          },
          context,
        ));
      }
      Some(TSModuleDeclarationBody::TSModuleDeclaration(decl)) => {
        items.push_sc(sc!("."));
        items.extend(gen_node(ts_module_declaration_name_to_node(&decl.id), context));
        items.extend(gen_body(decl.body.as_ref(), start_header_lsil, context));
      }
      None => {
        if context.config.semi_colons.is_true() {
          items.push_sc(sc!(";"));
        }
      }
    }

    items
  }
}

fn gen_global_decl<'a>(node: &'a TSGlobalDeclaration<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();

  let start_header_lsil = LineStartIndentLevel::new("startHeader");
  items.push_info(start_header_lsil);

  if node.declare {
    items.push_sc(sc!("declare "));
  }
  items.push_sc(sc!("global"));
  items.extend(gen_ts_module_block(&node.body, start_header_lsil, context));

  items
}

fn gen_ts_module_block<'a>(block: &'a TSModuleBlock<'a>, start_header_lsil: LineStartIndentLevel, context: &mut Context<'a>) -> PrintItems {
  let mut members: Vec<Node<'a>> = block.directives.iter().map(Node::Directive).collect();
  members.extend(block.body.iter().map(stmt_to_node));
  gen_membered_body(
    GenMemberedBodyOptions {
      node: Node::TSModuleBlock(block),
      members,
      start_header_lsil: Some(start_header_lsil),
      brace_position: context.config.module_declaration_brace_position,
      should_use_blank_line: move |previous, next, context| node_helpers::has_separating_blank_line(&previous, &next, context.program),
      separator: Separator::none(),
    },
    context,
  )
}

fn gen_type_alias<'a>(node: &'a TSTypeAliasDeclaration<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  if node.declare {
    items.push_sc(sc!("declare "));
  }
  items.push_sc(sc!("type "));
  items.extend(gen_node(Node::BindingIdentifier(&node.id), context));
  if let Some(type_params) = &node.type_parameters {
    items.extend(gen_node(Node::TSTypeParameterDeclaration(type_params), context));
  }

  items.extend(gen_assignment(ts_type_to_node(&node.type_annotation), sc!("="), context));

  if context.config.semi_colons.is_true() {
    items.push_sc(sc!(";"));
  }

  items
}

// oxc has no separate UsingDecl - `using` / `await using` are VariableDeclarationKind
// variants handled directly in gen_var_decl.

/* exports */

struct GenNamedImportOrExportSpecifierOptions<'a> {
  parent: Node<'a>,
  specifiers: Vec<Node<'a>>,
  force_single_line: bool,
  force_multi_line_specifiers: bool,
}

fn gen_named_import_or_export_specifiers<'a>(opts: GenNamedImportOrExportSpecifierOptions<'a>, context: &mut Context<'a>) -> PrintItems {
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
      surround_single_line_with_space_at_end: get_use_space(opts.parent, context),
      allow_blank_lines: false,
      node_sorter: get_node_sorter(opts.parent, context),
    },
    context,
  );

  fn get_trailing_commas(parent_decl: Node, context: &Context) -> TrailingCommas {
    match parent_decl {
      Node::ExportNamedDeclaration(_) => context.config.export_declaration_trailing_commas,
      Node::ImportDeclaration(_) => context.config.import_declaration_trailing_commas,
      _ => unreachable!(),
    }
  }

  fn get_use_space(parent_decl: Node, context: &Context) -> bool {
    match parent_decl {
      Node::ExportNamedDeclaration(_) => context.config.export_declaration_space_surrounding_named_exports,
      Node::ImportDeclaration(_) => context.config.import_declaration_space_surrounding_named_imports,
      _ => unreachable!(),
    }
  }

  fn get_prefer_hanging(parent_decl: Node, context: &Context) -> bool {
    match parent_decl {
      Node::ExportNamedDeclaration(_) => context.config.export_declaration_prefer_hanging,
      Node::ImportDeclaration(_) => context.config.import_declaration_prefer_hanging,
      _ => unreachable!(),
    }
  }

  fn get_prefer_single_line(parent_decl: Node, context: &Context) -> bool {
    match parent_decl {
      Node::ExportNamedDeclaration(_) => context.config.export_declaration_prefer_single_line,
      Node::ImportDeclaration(_) => context.config.import_declaration_prefer_single_line,
      _ => unreachable!(),
    }
  }

  fn get_node_sorter<'a>(
    parent_decl: Node,
    context: &Context<'a>,
  ) -> Option<Box<dyn Fn((usize, Option<Node<'a>>), (usize, Option<Node<'a>>), ProgramInfo<'a>) -> std::cmp::Ordering>> {
    match parent_decl {
      Node::ExportNamedDeclaration(_) => get_node_sorter_from_order(
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

/* expressions */

fn gen_array_expr<'a>(node: &'a ArrayExpression<'a>, context: &mut Context<'a>) -> PrintItems {
  let prefer_hanging = match context.config.array_expression_prefer_hanging {
    PreferHanging::Never => false,
    PreferHanging::OnlySingleItem => node.elements.len() == 1,
    PreferHanging::Always => true,
  };
  gen_array_like_nodes(
    GenArrayLikeNodesOptions {
      node: Node::ArrayExpression(node),
      nodes: node.elements.iter().map(array_element_to_node).collect(),
      prefer_hanging,
      prefer_single_line: context.config.array_expression_prefer_single_line,
      trailing_commas: context.config.array_expression_trailing_commas,
      space_around: context.config.array_expression_space_around,
    },
    context,
  )
}

fn gen_arrow_func_expr<'a>(node: &'a ArrowFunctionExpression<'a>, context: &mut Context<'a>) -> PrintItems {
  let items = gen_inner(node, context);
  return if should_add_parens_around_expr(Node::ArrowFunctionExpression(node), context) {
    surround_with_parens(items)
  } else {
    items
  };

  fn gen_inner<'a>(node: &'a ArrowFunctionExpression<'a>, context: &mut Context<'a>) -> PrintItems {
    if let Some(node) = get_curried_arrow_expr(node) {
      // handle arrow functions that are curried differently
      gen_curried_arrow(node, context)
    } else {
      gen_inner_arrow(node, context)
    }
  }

  fn gen_curried_arrow<'a>(node: CurriedArrowExpr<'a>, context: &mut Context<'a>) -> PrintItems {
    let mut last_header_start_lsil = None;
    let mut force_use_new_lines = false;
    let last_arrow = node.last_arrow();
    let mut lines = Vec::new();
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

    let mut items = PrintItems::new();
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

      items.extend(actions::if_column_number_changes(move |context| {
        context.clear_info(end_ln);
      }));
      let lines = lines.into_iter().map(|l| l.into_rc_path()).collect::<Vec<_>>();
      items.push_condition(if_true_or(
        "multipleLinesCurrying",
        Rc::new(move |context| condition_helpers::is_multiple_lines(context, start_ln, end_ln)),
        {
          let mut items = PrintItems::new();
          for (i, line) in lines.clone().into_iter().enumerate() {
            if i > 0 {
              items.push_signal(Signal::NewLine);
            }
            items.push_optional_path(line);
          }
          items
        },
        {
          let mut items = PrintItems::new();
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

    let mut items = ir_helpers::new_line_group(items);
    items.extend(gen_arrow_body(last_arrow, last_header_start_lsil.unwrap(), context));
    items
  }

  fn gen_inner_arrow<'a>(node: &'a ArrowFunctionExpression<'a>, context: &mut Context<'a>) -> PrintItems {
    let (header_start_lsil, header_items) = gen_arrow_signature(&ArrowSignature::new(node), context);

    // in oxc an arrow is a direct argument of its call (no ExprOrSpread wrapper)
    let is_arrow_in_test_call_expr = matches!(context.parent(), Node::CallExpression(c) if node_helpers::is_test_library_call_expr(c, context.program));
    let mut items = if is_arrow_in_test_call_expr {
      ir_helpers::with_no_new_lines(header_items)
    } else {
      header_items
    };

    items.extend(gen_arrow_body(node, header_start_lsil, context));

    items
  }

  fn gen_arrow_body<'a>(node: &'a ArrowFunctionExpression<'a>, header_start_lsil: LineStartIndentLevel, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    // oxc arrow body is always a FunctionBody; `.expression` true means it wraps a
    // single bare expression (which `get_expression` extracts).
    let body_node = match node.get_expression() {
      Some(expr) => expr_to_node(expr),
      None => Node::FunctionBody(&node.body),
    };
    let generated_body = gen_node(body_node, context);
    let generated_body = if use_new_line_group_for_arrow_body(node, context) {
      new_line_group(generated_body)
    } else {
      generated_body
    }
    .into_rc_path();
    let open_brace_token = if node.expression {
      None
    } else {
      context.token_finder.get_first_open_brace_token_within(node.body.as_ref())
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

      items.extend(generated_body.into());
    } else {
      let start_body_ln = LineNumber::new("startBody");
      let end_body_ln = LineNumber::new("endBody");
      items.push_info(start_body_ln);

      if should_not_newline_after_arrow(node, context) {
        items.push_space();
      } else {
        // todo: uncomment this? I was making a lot of changes so didn't want to do it yet
        // items.extend(actions::if_column_number_changes(move |context| {
        //   context.clear_info(end_body_ln);
        // }));
        items.push_condition(conditions::if_above_width_or(
          context.config.indent_width,
          if_true_or(
            "newlineOrSpace",
            Rc::new(move |context| condition_helpers::is_multiple_lines(context, start_body_ln, end_body_ln)),
            Signal::NewLine.into(),
            Signal::SpaceOrNewLine.into(),
          )
          .into(),
          " ".into(),
        ));
      }

      items.push_condition(conditions::indent_if_start_of_line(generated_body.into()));
      items.push_info(end_body_ln);
    }
    items
  }

  fn gen_arrow_signature<'a>(node: &ArrowSignature<'a>, context: &mut Context<'a>) -> (LineStartIndentLevel, PrintItems) {
    let header_start_lsil = LineStartIndentLevel::new("arrowFunctionExpressionHeaderStart");
    let mut items = PrintItems::new();
    let should_use_parens = get_should_use_parens(node, context);

    items.push_info(header_start_lsil);
    if node.is_async() {
      items.push_sc(sc!("async "));
    }
    if let Some(type_params) = node.type_params() {
      items.extend(gen_node(Node::TSTypeParameterDeclaration(type_params), context));
    }

    if should_use_parens {
      // need to check if there are parens because gen_parameters_or_arguments depends on the parens existing
      if has_source_param_parens(node, context.program) {
        let param_nodes = formal_params_to_nodes(&node.inner.params);
        let param_count = param_nodes.len();
        items.extend(gen_parameters_or_arguments(
          GenParametersOrArgumentsOptions {
            node: Node::ArrowFunctionExpression(node.inner),
            range: node.inner.get_parameters_range(context),
            nodes: param_nodes,
            custom_close_paren: |context| {
              Some(gen_close_paren_with_type(
                GenCloseParenWithTypeOptions {
                  start_lsil: header_start_lsil,
                  type_node: node.return_type().map(Node::TSTypeAnnotation),
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
        items.extend(surround_with_parens(gen_node(Node::FormalParameter(&node.params()[0]), context)));
      }
    } else {
      items.extend(gen_node(Node::FormalParameter(&node.params()[0]), context));
    }

    if node.return_type().is_none() && has_source_param_parens(node, context.program) {
      items.extend(gen_recovered_arrow_return_type(node, context));
    }

    items.push_sc(sc!(" =>"));
    (header_start_lsil, items)
  }

  fn gen_recovered_arrow_return_type<'a>(node: &ArrowSignature<'a>, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    let Some(arrow_token) = node.inner.body.previous_token_fast(context.program) else {
      return items;
    };
    if arrow_token.kind() != Kind::Arrow {
      return items;
    }
    let arrow_token_index = context.program.tokens().partition_point(|token| token.end() <= arrow_token.start());
    let Some(close_paren) = context.program.tokens()[..arrow_token_index]
      .iter()
      .rev()
      .find(|token| token.kind() == Kind::RParen)
    else {
      return items;
    };
    let text = &context.program.text()[close_paren.end() as usize..arrow_token.start() as usize];
    let text = text.trim();
    if text.starts_with(':') {
      items.extend(gen_from_raw_string(text));
    }
    items
  }

  fn should_not_newline_after_arrow<'a>(node: &'a ArrowFunctionExpression<'a>, context: &Context<'a>) -> bool {
    match node.get_expression() {
      None => true, // block body
      Some(expr) => match expr {
        Expression::ParenthesizedExpression(_) | Expression::ArrayExpression(_) => true,
        Expression::TemplateLiteral(tpl) => tpl.quasis[0].value.raw.as_str().starts_with(['\n', '\r']),
        _ => is_jsx_paren_expr_handled_node(expr_to_node(expr), context),
      },
    }
  }

  fn get_should_use_parens<'a>(node: &ArrowSignature<'a>, context: &Context<'a>) -> bool {
    return match context.config.arrow_function_use_parentheses {
      UseParentheses::Force => true,
      UseParentheses::PreferNone => {
        node.params().len() != 1 || node.return_type().is_some() || is_first_param_not_identifier_or_has_type_annotation(node.params(), node, context.program)
      }
      UseParentheses::Maintain => {
        node.type_params().is_some()
          || node.return_type().is_some()
          || is_first_param_not_identifier_or_has_type_annotation(node.params(), node, context.program)
          || has_parens(node, context.program)
      }
    };

    fn is_first_param_not_identifier_or_has_type_annotation<'a>(
      params: &'a [FormalParameter<'a>],
      arrow: &ArrowSignature<'a>,
      program: ProgramInfo<'a>,
    ) -> bool {
      match params.first() {
        Some(param) if matches!(param.pattern, BindingPattern::BindingIdentifier(_)) => {
          param.type_annotation.is_some()
            || param.optional
            || param_has_surrounding_comments(Node::FormalParameter(param), program) && has_parens(arrow, program)
        }
        _ => true,
      }
    }

    fn param_has_surrounding_comments<'a>(param: Node<'a>, program: ProgramInfo<'a>) -> bool {
      if node_helpers::has_surrounding_comments(param, program) {
        true
      } else if matches!(param, Node::FormalParameter(param) if node_helpers::has_surrounding_comments(binding_pattern_to_node(&param.pattern), program)) {
        true
      } else if param
        .previous_token_fast(program)
        .is_some_and(|previous_token| !previous_token.trailing_comments_fast(program).is_empty())
      {
        true
      } else if let Some(comma_token) = param.next_token_fast(program).filter(|t| t.kind() == Kind::Comma) {
        !comma_token.trailing_comments_fast(program).is_empty()
      } else {
        false
      }
    }
  }

  // A single-param arrow has parens when the token immediately before `=>` is
  // `)`. Looking before the parameter confuses `call(a => {})` with `(a) => {}`.
  fn has_parens<'a>(node: &ArrowSignature<'a>, program: ProgramInfo<'a>) -> bool {
    if node.params().len() != 1 {
      true
    } else {
      let Some(arrow_token) = node.inner.body.previous_token_fast(program) else {
        return false;
      };
      debug_assert_eq!(arrow_token.kind(), Kind::Arrow);
      matches!(arrow_token.previous_token_fast(program).map(|t| t.kind()), Some(Kind::RParen))
    }
  }

  fn has_source_param_parens<'a>(node: &ArrowSignature<'a>, program: ProgramInfo<'a>) -> bool {
    if node.params().len() != 1 {
      true
    } else {
      node
        .params()
        .first()
        .and_then(|param| param.previous_token_fast(program))
        .is_some_and(|token| token.kind() == Kind::LParen && token.start() >= node.inner.start())
    }
  }
}

fn gen_as_expr<'a>(node: &'a TSAsExpression<'a>, context: &mut Context<'a>) -> PrintItems {
  let items = gen_as_expr_like(
    AsExprLike {
      expr: expr_to_node(&node.expression),
      type_ann: ts_type_to_node(&node.type_annotation),
    },
    context,
  );
  if is_assignment_lhs(node, context) {
    surround_expr_with_configured_parens(items, context)
  } else {
    items
  }
}

struct AsExprLike<'a> {
  expr: Node<'a>,
  type_ann: Node<'a>,
}

fn gen_as_expr_like<'a>(node: AsExprLike<'a>, context: &mut Context<'a>) -> PrintItems {
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

fn gen_satisfies_expr<'a>(node: &'a TSSatisfiesExpression<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = gen_node(expr_to_node(&node.expression), context);
  items.push_sc(sc!(" satisfies"));
  items.push_signal(Signal::SpaceIfNotTrailing);
  items.push_condition(conditions::with_indent_if_start_of_line_indented(gen_node(
    ts_type_to_node(&node.type_annotation),
    context,
  )));
  items
}

// oxc has no TsConstAssertion - `x as const` is a TSAsExpression to a `const` type ref.

fn gen_assignment_expr<'a>(node: &'a AssignmentExpression<'a>, context: &mut Context<'a>) -> PrintItems {
  // check for a nested assignment (ex. `a = b = c`)
  if node.operator == AssignmentOperator::Assign {
    if let Expression::AssignmentExpression(right) = &node.right {
      if right.operator == AssignmentOperator::Assign {
        let flattened_items = get_flattened_assign_expr(node, context);
        // if the number of assignments chained is excessive, then it's probably minified
        // so force using newlines to optimize formatting
        let force_use_new_lines = flattened_items.len() > 4
          || node_helpers::get_use_new_lines_for_nodes(&flattened_items[0].expr, &flattened_items.last().unwrap().expr, context.program);
        let indent_width = context.config.indent_width;
        return ir_helpers::gen_separated_values(
          |_| {
            let mut generated_nodes = Vec::new();
            for item in flattened_items.into_iter() {
              let lines_span = Some(ir_helpers::LinesSpan {
                start_line: item.expr.range().start_line_fast(context.program),
                end_line: item.expr.range().end_line_fast(context.program),
              });
              let mut items = gen_node(item.expr, context);
              if let Some(op) = item.post_op {
                items.push_sc(sc!(" ="));
                items.extend(gen_trailing_comments(&op.range(), context));
              } else {
                items = indent_if_start_of_line(items).into();
              }

              generated_nodes.push(ir_helpers::GeneratedValue {
                items,
                lines_span,
                allow_inline_multi_line: true,
                allow_inline_single_line: true,
              });
            }

            generated_nodes
          },
          ir_helpers::GenSeparatedValuesOptions {
            prefer_hanging: false,
            force_use_new_lines,
            allow_blank_lines: false,
            indent_width,
            single_line_options: ir_helpers::SingleLineOptions::separated_same_line(Signal::SpaceOrNewLine.into()),
            multi_line_options: ir_helpers::MultiLineOptions::same_line_start_hanging_indent(),
            force_possible_newline_at_start: false,
          },
        )
        .items;
      }
    }
  }

  // parse a single assignment (ex. `a = b` and not `a = b = c`)
  let mut items = PrintItems::new();
  items.extend(gen_node(assign_target_to_node(&node.left), context));
  items.extend(gen_assignment(expr_to_node(&node.right), assign_op_sc(node.operator), context));
  items
}

fn gen_await_expr<'a>(node: &'a AwaitExpression<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.push_sc(sc!("await "));
  items.extend(gen_node(expr_to_node(&node.argument), context));
  items
}

fn gen_binary_expr<'a>(node: BinaryLikeExpr<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  let flattened_binary_expr = get_flattened_bin_expr(node, context);
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
  let indent_width = context.config.indent_width;
  let binary_expr_start_il = IndentLevel::new("binExprStart");
  let binary_expr_start_isol = IsStartOfLine::new("binExprStart");
  let allow_no_indent = get_allow_no_indent(node, context);
  let use_space_surrounding_operator = get_use_space_surrounding_operator(&node.op(), context);
  let is_parent_bin_expr = matches!(context.parent(), Node::BinaryExpression(_) | Node::LogicalExpression(_));
  let multi_line_options = {
    let mut options = if line_per_expression {
      ir_helpers::MultiLineOptions::same_line_no_indent()
    } else {
      ir_helpers::MultiLineOptions::maintain_line_breaks()
    };
    options.with_hanging_indent = if is_parent_bin_expr {
      BoolOrCondition::Bool(false) // let the parent handle the indent
    } else {
      BoolOrCondition::Condition(Rc::new(move |condition_context| {
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
        let mut generated_nodes = Vec::new();
        let mut bin_expr_items_iter = flattened_binary_expr.into_iter().peekable();
        while let Some(bin_expr_item) = bin_expr_items_iter.next() {
          let lines_span = Some(ir_helpers::LinesSpan {
            start_line: bin_expr_item.expr.start_line_fast(context.program),
            end_line: bin_expr_item.expr.end_line_fast(context.program),
          });
          let mut items = PrintItems::new();

          let pre_op = bin_expr_item.pre_op;
          let post_op = bin_expr_item.post_op;
          let (leading_pre_op_comments, trailing_pre_op_comments) = if let Some(op) = &pre_op {
            (
              gen_leading_comments_same_line(&op.token.range(), context),
              gen_trailing_comments_same_line(&op.token.range(), context),
            )
          } else {
            (PrintItems::new(), PrintItems::new())
          };
          let is_inner_binary_expression = matches!(bin_expr_item.expr, Node::BinaryExpression(_) | Node::LogicalExpression(_));
          items.extend(gen_node_with_inner_gen(bin_expr_item.expr, context, |node_items, context| {
            let mut items = PrintItems::new();
            if let Some(op) = pre_op {
              if !leading_pre_op_comments.is_empty() {
                items.extend(leading_pre_op_comments);
              }
              items.push_sc(bin_like_op_sc(op.op));
              if trailing_pre_op_comments.is_empty() {
                if use_space_surrounding_operator {
                  items.push_space();
                }
              } else {
                items.extend(trailing_pre_op_comments);
                items.push_space();
              }
            }

            items.extend(if is_inner_binary_expression {
              let node_items = node_items.into_rc_path();
              with_queued_indent(
                // indent again if it hasn't done the current binary expression's hanging indent
                if_true_or(
                  "indentIfNecessary",
                  Rc::new(move |context| {
                    if allow_no_indent && context.resolved_is_start_of_line(binary_expr_start_isol)? {
                      return Some(false);
                    }
                    let binary_expr_start_il = context.resolved_indent_level(binary_expr_start_il)?;
                    let is_hanging = binary_expr_start_il < context.writer_info.indent_level;
                    Some(!is_hanging)
                  }),
                  with_queued_indent(node_items.into()),
                  node_items.into(),
                )
                .into(),
              )
            } else {
              node_items
            });

            if let Some(op) = post_op {
              let leading_post_op_comments = {
                let comments_between_expr_and_op = context
                  .program
                  .comments()
                  .iter()
                  .filter(|comment| {
                    !context.has_handled_comment(comment)
                      && comment.start() >= bin_expr_item.expr.end()
                      && comment.end() <= op.token.start()
                      && comment.start_line_fast(context.program) == op.token.start_line_fast(context.program)
                  })
                  .collect::<Vec<_>>();
                let mut items = gen_comment_collection(comments_between_expr_and_op.into_iter(), None, None, context);
                items.extend(gen_leading_comments_same_line(&op.token.range(), context));
                items
              };
              let trailing_post_op_comments = gen_trailing_comments_same_line(&op.token.range(), context);
              if leading_post_op_comments.is_empty() {
                if use_space_surrounding_operator {
                  items.push_space();
                }
              } else {
                items.push_space();
                items.extend(leading_post_op_comments);
                items.push_space();
              }
              items.push_sc(bin_like_op_sc(op.op));
              if !trailing_post_op_comments.is_empty() {
                items.extend(trailing_post_op_comments);
              }
            } else if let Some(next_item) = bin_expr_items_iter.peek() {
              // put any line comments after the next pre-op on this line instead
              if let Some(pre_op) = &next_item.pre_op {
                items.extend(gen_trailing_comments_if_line_comment_same_line(&pre_op.token.range(), context));
              }
            }

            items
          }));

          let items = if should_newline_group_bin_item_expr(bin_expr_item.expr, context) {
            ir_helpers::new_line_group(items)
          } else {
            items
          };

          generated_nodes.push(ir_helpers::GeneratedValue {
            items,
            lines_span,
            allow_inline_multi_line: true,
            allow_inline_single_line: true,
          });
        }

        generated_nodes
      },
      ir_helpers::GenSeparatedValuesOptions {
        prefer_hanging: false,
        force_use_new_lines,
        allow_blank_lines: false,
        indent_width,
        single_line_options: ir_helpers::SingleLineOptions::separated_same_line(if use_space_surrounding_operator {
          Signal::SpaceOrNewLine.into()
        } else {
          PrintItems::new()
        }),
        multi_line_options,
        force_possible_newline_at_start: false,
      },
    )
    .items,
  );

  return if node.op().is_equality() { ir_helpers::new_line_group(items) } else { items };

  fn get_allow_no_indent(node: BinaryLikeExpr, context: &mut Context) -> bool {
    let op = node.op();
    if !op.is_add_sub()
      && !op.is_mul_div()
      && !op.is_logical()
      && !op.is_bit_logical()
      && !op.is_bit_shift()
      && !matches!(op, BinaryLikeOp::Binary(BinaryOperator::Remainder))
    {
      false
    } else {
      match context.parent() {
        Node::ExpressionStatement(_) | Node::BinaryExpression(_) | Node::LogicalExpression(_) => false,
        // a binary expression used directly as a call/new argument (oxc has no
        // ExprOrSpread wrapper); array elements still allow no-indent as before
        Node::CallExpression(_) | Node::NewExpression(_) => false,
        _ => true,
      }
    }
  }

  fn get_use_space_surrounding_operator(op: &BinaryLikeOp, context: &Context) -> bool {
    if op.is_bitwise_or_arithmetic() {
      context.config.binary_expression_space_surrounding_bitwise_and_arithmetic_operator
    } else {
      true
    }
  }

  fn should_newline_group_bin_item_expr<'a>(node: Node<'a>, context: &Context<'a>) -> bool {
    if let Node::ParenthesizedExpression(paren) = node {
      return should_newline_group_bin_item_expr(expr_to_node(&paren.expression), context);
    }

    if is_jsx_paren_expr_handled_node(node, context) {
      // prefer using the possible newline at the start of the element
      return false;
    }

    true
  }
}

fn gen_call_or_opt_expr<'a>(node: CallOrOptCallExpr<'a>, context: &mut Context<'a>) -> PrintItems {
  if node_helpers::is_test_library_call_expr(node.inner(), context.program) {
    return gen_test_library_call_expr(node.inner(), context);
  }

  // flatten the call expression and check if it should be generated as a flattened member like expression
  let flattened_call_expr = flatten_member_like_expr(Node::CallExpression(node.inner()), context.program);
  return if flattened_call_expr.nodes.len() > 1 {
    gen_for_flattened_member_like_expr(flattened_call_expr, context)
  } else {
    gen_call_expr_like(
      CallExprLike {
        original_call_expr: node,
        generated_callee: gen_node(expr_to_node(node.callee()), context),
      },
      context,
    )
  };

  fn gen_test_library_call_expr<'a>(node: &'a CallExpression<'a>, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    items.extend(gen_test_library_callee(&node.callee, context));
    items.extend(gen_test_library_arguments(&node.arguments, context));
    return items;

    fn gen_test_library_callee<'a>(callee: &'a Expression<'a>, context: &mut Context<'a>) -> PrintItems {
      match callee {
        Expression::StaticMemberExpression(member_expr) => {
          let mut items = gen_node(expr_to_node(&member_expr.object), context);
          items.push_sc(sc!("."));
          items.extend(gen_node(Node::IdentifierName(&member_expr.property), context));
          items
        }
        _ => gen_node(expr_to_node(callee), context),
      }
    }

    fn gen_test_library_arguments<'a>(args: &'a [Argument<'a>], context: &mut Context<'a>) -> PrintItems {
      let mut items = PrintItems::new();
      items.extend(gen_node_with_inner_gen(arg_to_node(&args[0]), context, |items, _| {
        let mut new_items = ir_helpers::with_no_new_lines(items);
        new_items.push_sc(sc!(","));
        new_items
      }));
      items.push_space();
      items.extend(gen_node(arg_to_node(&args[1]), context));

      surround_with_parens(items)
    }
  }
}

struct CallExprLike<'a> {
  original_call_expr: CallOrOptCallExpr<'a>,
  generated_callee: PrintItems,
}

fn gen_call_expr_like<'a>(node: CallExprLike<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  let call_expr = node.original_call_expr;

  items.extend(node.generated_callee);

  if let Some(type_args) = call_expr.type_args() {
    items.extend(gen_node(Node::TSTypeParameterInstantiation(type_args), context));
  }

  if call_expr.is_optional() {
    items.push_sc(sc!("?."));
  }

  let call_expr_node = Node::CallExpression(call_expr.inner());
  let pushed_call_expr = context.current_node.range() != call_expr_node.range();
  if pushed_call_expr {
    let past_current_node = std::mem::replace(&mut context.current_node, call_expr_node);
    context.parent_stack.push(past_current_node);
  }
  items.push_condition(conditions::with_indent_if_start_of_line_indented(gen_parameters_or_arguments(
    GenParametersOrArgumentsOptions {
      node: call_expr_node,
      range: call_expr.get_parameters_range(context),
      nodes: call_expr.args().iter().map(arg_to_node).collect(),
      custom_close_paren: |_| None,
      is_parameters: false,
    },
    context,
  )));
  if pushed_call_expr {
    context.current_node = context.parent_stack.pop();
  }

  items
}

fn gen_conditional_expr<'a>(node: &'a ConditionalExpression<'a>, context: &mut Context<'a>) -> PrintItems {
  let question_token = context.token_finder.get_first_operator_after(&node.test, "?").unwrap();
  let colon_token = context.token_finder.get_first_operator_after(&node.consequent, ":").unwrap();
  let line_per_expression = context.config.conditional_expression_line_per_expression;
  let has_newline_test_cons = node_helpers::get_use_new_lines_for_nodes(&node.test, &node.consequent, context.program);
  let has_newline_const_alt = node_helpers::get_use_new_lines_for_nodes(&node.consequent, &node.alternate, context.program);
  let mut force_test_cons_newline = !context.config.conditional_expression_prefer_single_line && has_newline_test_cons;
  let mut force_cons_alt_newline = !context.config.conditional_expression_prefer_single_line && has_newline_const_alt;
  if line_per_expression && (force_test_cons_newline || force_cons_alt_newline) {
    // for line per expression, if one is true then both should be true
    force_test_cons_newline = true;
    force_cons_alt_newline = true;
  }

  let (question_position, colon_position) = get_operator_position(node, question_token, colon_token, context);
  let top_most_data = get_top_most_data(node, context);
  let before_alternate_ln = LineNumber::new("beforeAlternate");
  let end_ln = LineNumber::new("endConditionalExpression");
  let question_comment_items = gen_cond_token_comments(question_token, context, top_most_data.il);
  let colon_comment_items = gen_cond_token_comments(colon_token, context, top_most_data.il);
  let mut items = PrintItems::new();

  if top_most_data.is_top_most {
    items.push_info(top_most_data.ln);
    items.push_info(top_most_data.il);
  }

  let top_most_il = top_most_data.il;

  items.extend(ir_helpers::new_line_group(with_queued_indent({
    let mut items = gen_node(expr_to_node(&node.test), context);
    if question_position == OperatorPosition::SameLine {
      items.push_sc(sc!(" ?"));
    }
    items.extend(question_comment_items.trailing_line);
    items
  })));

  items.extend(question_comment_items.previous_lines);

  items.push_anchor(LineNumberAnchor::new(end_ln));
  items.push_anchor(LineNumberAnchor::new(before_alternate_ln));

  let multi_line_reevaluation = if force_test_cons_newline {
    items.push_signal(Signal::NewLine);
    None
  } else if line_per_expression {
    let mut condition = conditions::new_line_if_multiple_lines_space_or_new_line_otherwise(top_most_data.ln, Some(before_alternate_ln));
    let reevaluation = condition.create_reevaluation();
    items.push_condition(condition);
    Some(reevaluation)
  } else {
    items.push_signal(Signal::SpaceOrNewLine);
    None
  };

  let cons_and_alt_items = {
    let mut items = PrintItems::new();

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
      let mut items = PrintItems::new();
      items.extend(question_comment_items.leading_line);
      if question_position == OperatorPosition::NextLine {
        items.push_sc(sc!("? "));
      }
      items.extend(gen_node(expr_to_node(&node.consequent), context));
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
        top_most_data.ln,
        Some(before_alternate_ln),
      ));
    } else {
      items.push_signal(Signal::SpaceOrNewLine);
    }

    items.push_condition({
      let mut items = PrintItems::new();
      items.extend(colon_comment_items.leading_line);
      if colon_position == OperatorPosition::NextLine {
        items.push_sc(sc!(": "));
      }
      items.push_info(before_alternate_ln);
      items.extend(gen_node(expr_to_node(&node.alternate), context));
      indent_if_sol_and_same_indent_as_top_most(ir_helpers::new_line_group(items), top_most_il)
    });
    items.push_info(end_ln);

    if let Some(reevaluation) = multi_line_reevaluation {
      items.push_reevaluation(reevaluation);
    }

    items
  };

  if top_most_data.is_top_most {
    items.push_condition(conditions::indent_if_start_of_line(cons_and_alt_items));
  } else {
    items.push_condition(indent_if_sol_and_same_indent_as_top_most(cons_and_alt_items, top_most_data.il));
  }

  return items;

  struct TopMostData {
    ln: LineNumber,
    il: IndentLevel,
    is_top_most: bool,
  }

  fn get_top_most_data<'a>(node: &'a ConditionalExpression<'a>, context: &mut Context<'a>) -> TopMostData {
    // The "top most" node in nested conditionals follows the ancestors up through
    // the alternate expressions.
    let mut top_most_node = node;

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

    fn get_or_set_top_most(top_most_expr_start: SourcePos, is_top_most: bool, context: &mut Context) -> (LineNumber, IndentLevel) {
      if is_top_most {
        let ln = LineNumber::new("conditionalExprStart");
        let il = IndentLevel::new("conditionalExprStart");
        context.store_ln_for_node(&top_most_expr_start.range(), ln);
        context.store_il_for_node(&top_most_expr_start.range(), il);
        (ln, il)
      } else {
        (
          context.get_ln_for_node(&top_most_expr_start.range()).unwrap(),
          context.get_il_for_node(&top_most_expr_start.range()).unwrap(),
        )
      }
    }
  }

  fn get_operator_position(
    node: &ConditionalExpression,
    question_token: &Token,
    colon_token: &Token,
    context: &mut Context,
  ) -> (OperatorPosition, OperatorPosition) {
    fn get_maintain_position(node: &ConditionalExpression, expr: &Expression, token: &Token, context: &mut Context) -> OperatorPosition {
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

struct ConditionalTokenComments {
  previous_lines: PrintItems,
  leading_line: PrintItems,
  trailing_line: PrintItems,
}

/// Generates the comments used for tokens in conditional expressions and types.
fn gen_cond_token_comments(token: &Token, context: &mut Context, top_most_il: IndentLevel) -> ConditionalTokenComments {
  let token_line = token.end_line_fast(context.program);
  let previous_token = token.previous_token_fast(context.program).unwrap();
  let next_token = token.next_token_fast(context.program).unwrap();
  let previous_token_line = previous_token.end_line_fast(context.program);
  let next_token_line = next_token.start_line_fast(context.program);
  if token_line > previous_token_line {
    ConditionalTokenComments {
      previous_lines: {
        let program = context.program;
        let leading_comments = token.leading_comments_fast(program).filter(|c| {
          let comment_line = c.start_line_fast(program);
          comment_line > previous_token_line && comment_line < next_token_line
        });
        let trailing_comments = token.trailing_comments_fast(program).filter(|c| c.start_line_fast(program) < next_token_line);
        let items = gen_comments_as_statements(leading_comments.chain(trailing_comments), None, context);
        if items.is_empty() {
          items
        } else {
          let mut new_items = PrintItems::new();
          new_items.push_signal(Signal::NewLine);
          new_items.push_condition(indent_if_sol_and_same_indent_as_top_most(items, top_most_il));
          new_items
        }
      },
      leading_line: if token_line == next_token_line {
        gen_leading_comments_same_line(&token.range(), context)
      } else {
        PrintItems::new()
      },
      trailing_line: PrintItems::new(),
    }
  } else if token_line < next_token_line {
    ConditionalTokenComments {
      previous_lines: PrintItems::new(),
      leading_line: PrintItems::new(),
      trailing_line: gen_trailing_comments_same_line(&token.range(), context),
    }
  } else {
    // do nothing
    ConditionalTokenComments {
      previous_lines: PrintItems::new(),
      leading_line: PrintItems::new(),
      trailing_line: PrintItems::new(),
    }
  }
}

fn indent_if_sol_and_same_indent_as_top_most(items: PrintItems, indent_level: IndentLevel) -> Condition {
  let items = items.into_rc_path();
  if_true_or(
    "indentIfSameIndentationLevelAndStartOfLine",
    Rc::new(move |context| {
      if context.writer_info.is_start_of_line() {
        let top_most_il = context.resolved_indent_level(indent_level)?;
        Some(context.writer_info.indent_level == top_most_il)
      } else {
        Some(false)
      }
    }),
    with_indent(items.into()),
    items.into(),
  )
}

// oxc has no ExprOrSpread wrapper: call/array arguments are either a bare expression or a
// dedicated SpreadElement node (both handled directly via the dispatch).

// `implements X<T>` on a class (expression is a type name)
fn gen_class_implements<'a>(node: &'a TSClassImplements<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.extend(gen_node(ts_type_name_to_node(&node.expression), context));
  if let Some(type_args) = &node.type_arguments {
    items.extend(gen_node(Node::TSTypeParameterInstantiation(type_args), context));
  }
  items
}

// `extends X<T>` on an interface (expression is a value expression)
fn gen_interface_heritage<'a>(node: &'a TSInterfaceHeritage<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.extend(gen_node(expr_to_node(&node.expression), context));
  if let Some(type_args) = &node.type_arguments {
    items.extend(gen_node(Node::TSTypeParameterInstantiation(type_args), context));
  }
  items
}

fn is_iife_fn_expr(node: &Function, context: &Context) -> bool {
  let mut current_range = node.range();
  for ancestor in context.parent_stack.iter() {
    match *ancestor {
      Node::ParenthesizedExpression(paren) => {
        current_range = paren.range();
        continue;
      }
      // oxc has no separate OptCall; optional calls are CallExpression with `.optional`
      Node::CallExpression(call) => return call.callee.range() == current_range,
      _ => return false,
    }
  }
  false
}

fn should_add_parens_around_expr<'a>(node: Node<'a>, context: &Context<'a>) -> bool {
  if context.suppress_synthetic_expr_parens {
    return false;
  }

  let original_node = node;
  for ancestor in context.parent_stack.iter() {
    match *ancestor {
      Node::ParenthesizedExpression(paren_expr) => {
        if matches!(paren_expr.expression, Expression::BinaryExpression(_) | Expression::LogicalExpression(_)) {
          continue;
        }
        if !should_skip_paren_expr(paren_expr, context) {
          return false;
        }
      }
      // oxc has no separate OptCall; CallExpression covers optional calls
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
      // static / private member access: keep searching up
      Node::StaticMemberExpression(_) | Node::PrivateFieldExpression(_) => {}
      Node::ComputedMemberExpression(expr) => {
        if expr.expression.range().contains(&original_node.range()) {
          return false;
        }
      }
      Node::ConditionalExpression(cond_expr) => {
        return cond_expr.test.range().contains(&original_node.range());
      }
      // we only care to add parens when it's the left most expr
      Node::BinaryExpression(bin_expr) => {
        if bin_expr.right.range().contains(&original_node.range()) {
          return false;
        }
      }
      Node::LogicalExpression(bin_expr) => {
        if bin_expr.right.range().contains(&original_node.range()) {
          return false;
        }
      }
      Node::ChainExpression(_) => {
        // continue searching
      }
      _ => {
        return false;
      }
    }
  }

  false
}

// oxc unifies SWC's KeyValueProp / GetterProp / SetterProp / MethodProp / shorthand
// into a single `ObjectProperty` (distinguished by `.kind`, `.method`, `.shorthand`).
fn gen_object_property<'a>(node: &'a ObjectProperty<'a>, context: &mut Context<'a>) -> PrintItems {
  let func = || match &node.value {
    Expression::FunctionExpression(f) => &**f,
    _ => unreachable!("object getter/setter/method value is always a function expression"),
  };
  match node.kind {
    PropertyKind::Get => return gen_object_method(node, func(), ClassOrObjectMethodKind::Getter, context),
    PropertyKind::Set => return gen_object_method(node, func(), ClassOrObjectMethodKind::Setter, context),
    PropertyKind::Init if node.method => return gen_object_method(node, func(), ClassOrObjectMethodKind::Method, context),
    PropertyKind::Init => {}
  }

  // shorthand: `{ a }`; oxc also uses shorthand plus an assignment expression
  // for parser-recovered object assignment properties like `{ a = 1 }`.
  if node.shorthand {
    if let Expression::AssignmentExpression(assign_expr) = &node.value {
      let mut items = gen_node(prop_key_to_node(&node.key), context);
      items.extend(gen_assignment(expr_to_node(&assign_expr.right), sc!(":"), context));
      return items;
    } else if let Some(eq_index) = node.text_fast(context.program).find('=') {
      let mut items = gen_node(prop_key_to_node(&node.key), context);
      items.push_sc(sc!(":"));
      items.push_space();
      items.extend(gen_from_raw_string(node.text_fast(context.program)[eq_index + 1..].trim()));
      return items;
    } else {
      return gen_node(prop_key_to_node(&node.key), context);
    }
  }

  let mut items = PrintItems::new();
  if node.computed {
    items.extend(gen_computed_prop_like(
      |context| gen_node(prop_key_to_node(&node.key), context),
      GenComputedPropLikeOptions {
        inner_node_range: node.key.range(),
      },
      context,
    ));
  } else {
    items.extend(gen_quotable_prop(prop_key_to_node(&node.key), context));
  }
  items.extend(gen_assignment(expr_to_node(&node.value), sc!(":"), context));
  items
}

fn gen_object_method<'a>(node: &'a ObjectProperty<'a>, func: &'a Function<'a>, kind: ClassOrObjectMethodKind, context: &mut Context<'a>) -> PrintItems {
  gen_class_or_object_method(
    ClassOrObjectMethod {
      node: Node::ObjectProperty(node),
      parameters_range: func.get_parameters_range(context),
      decorators: None,
      accessibility: None,
      is_static: false,
      is_async: func.r#async,
      is_abstract: false,
      kind,
      is_generator: func.generator,
      is_optional: false,
      is_override: false,
      key: prop_key_to_node(&node.key),
      key_computed: node.computed,
      type_params: func.type_parameters.as_deref().map(Node::TSTypeParameterDeclaration),
      params: formal_params_to_nodes(&func.params),
      return_type: func.return_type.as_deref().map(Node::TSTypeAnnotation),
      body: func.body.as_deref().map(Node::FunctionBody),
    },
    context,
  )
}

// oxc splits SWC's `MemberExpr` into three distinct nodes; this enum recovers a
// unified view for the dispatch and flattening logic.
enum MemberExpr<'a> {
  Static(&'a StaticMemberExpression<'a>),
  Computed(&'a ComputedMemberExpression<'a>),
  Private(&'a PrivateFieldExpression<'a>),
}

impl<'a> MemberExpr<'a> {
  fn as_node(&self) -> Node<'a> {
    match self {
      MemberExpr::Static(n) => Node::StaticMemberExpression(n),
      MemberExpr::Computed(n) => Node::ComputedMemberExpression(n),
      MemberExpr::Private(n) => Node::PrivateFieldExpression(n),
    }
  }
}

fn gen_member_expr<'a>(node: MemberExpr<'a>, context: &mut Context<'a>) -> PrintItems {
  let flattened_member_expr = flatten_member_like_expr(node.as_node(), context.program);
  gen_for_flattened_member_like_expr(flattened_member_expr, context)
}

fn gen_meta_prop_expr<'a>(node: &'a MetaProperty<'a>, context: &mut Context<'a>) -> PrintItems {
  let flattened_meta_prop_expr = flatten_member_like_expr(Node::MetaProperty(node), context.program);
  gen_for_flattened_member_like_expr(flattened_meta_prop_expr, context)
}

fn gen_new_expr<'a>(node: &'a NewExpression<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.push_sc(sc!("new "));
  items.extend(gen_node(expr_to_node(&node.callee), context));
  if let Some(type_args) = &node.type_arguments {
    items.extend(gen_node(Node::TSTypeParameterInstantiation(type_args), context));
  }
  let args = node.arguments.iter().map(arg_to_node).collect();
  items.extend(gen_parameters_or_arguments(
    GenParametersOrArgumentsOptions {
      node: Node::NewExpression(node),
      range: node.get_parameters_range(context),
      nodes: args,
      custom_close_paren: |_| None,
      is_parameters: false,
    },
    context,
  ));
  items
}

fn gen_non_null_expr<'a>(node: &'a TSNonNullExpression<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.extend(gen_node(expr_to_node(&node.expression), context));
  items.push_sc(sc!("!"));
  items
}

// dynamic `import(...)` (oxc models this as a dedicated ImportExpression rather than
// a call with an `import` callee).
fn gen_import_expr<'a>(node: &'a ImportExpression<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.push_sc(sc!("import"));
  match node.phase {
    Some(ImportPhase::Source) => items.push_sc(sc!(".source")),
    Some(ImportPhase::Defer) => items.push_sc(sc!(".defer")),
    None => {}
  }
  let mut args = vec![expr_to_node(&node.source)];
  if let Some(options) = &node.options {
    args.push(expr_to_node(options));
  }
  items.extend(gen_parameters_or_arguments(
    GenParametersOrArgumentsOptions {
      node: Node::ImportExpression(node),
      range: node.get_parameters_range(context),
      nodes: args,
      custom_close_paren: |_| None,
      is_parameters: false,
    },
    context,
  ));
  items
}

fn gen_private_in_expr<'a>(node: &'a PrivateInExpression<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = gen_node(Node::PrivateIdentifier(&node.left), context);
  items.push_sc(sc!(" in "));
  items.extend(gen_node(expr_to_node(&node.right), context));
  items
}

fn gen_object_lit<'a>(node: &'a ObjectExpression<'a>, context: &mut Context<'a>) -> PrintItems {
  let items = context.with_maybe_consistent_props(
    node,
    |node| use_consistent_quotes_for_members(node.properties.iter().map(obj_prop_kind_to_node)),
    |context, node| {
      gen_object_like_node(
        GenObjectLikeNodeOptions {
          node: Node::ObjectExpression(node),
          members: node.properties.iter().map(obj_prop_kind_to_node).collect(),
          separator: context.config.object_expression_trailing_commas.into(),
          prefer_hanging: context.config.object_expression_prefer_hanging,
          prefer_single_line: context.config.object_expression_prefer_single_line,
          force_single_line: false,
          force_multi_line: is_node_definitely_above_line_width(node.range(), context),
          surround_single_line_with_spaces: context.config.object_expression_space_surrounding_properties,
          surround_single_line_with_space_at_end: context.config.object_expression_space_surrounding_properties,
          allow_blank_lines: true,
          node_sorter: None,
        },
        context,
      )
    },
  );

  if should_add_parens_around_expr(Node::ObjectExpression(node), context) {
    surround_with_parens(if context.config.paren_expression_space_around {
      surround_with_spaces(items)
    } else {
      items
    })
  } else {
    items
  }
}

fn gen_paren_expr<'a>(node: &'a ParenthesizedExpression<'a>, context: &mut Context<'a>) -> PrintItems {
  if should_skip_paren_expr(node, context) {
    return gen_node(expr_to_node(&node.expression), context);
  }

  let generated_items = conditions::with_indent_if_start_of_line_indented(gen_node_in_parens(
    |context| {
      let previous_value = context.suppress_synthetic_expr_parens;
      context.suppress_synthetic_expr_parens = true;
      let items = gen_node(expr_to_node(&node.expression), context);
      context.suppress_synthetic_expr_parens = previous_value;
      items
    },
    GenNodeInParensOptions {
      inner_range: node.expression.range(),
      prefer_hanging: true,
      allow_open_paren_trailing_comments: true,
      single_line_space_around: context.config.paren_expression_space_around,
    },
    context,
  ))
  .into();

  return if get_use_new_line_group(node, context) {
    new_line_group(generated_items)
  } else {
    generated_items
  };

  fn get_use_new_line_group<'a>(node: &'a ParenthesizedExpression<'a>, context: &Context<'a>) -> bool {
    if let Node::ArrowFunctionExpression(arrow_expr) = context.parent() {
      debug_assert!(arrow_expr.body.start() == node.start());
      use_new_line_group_for_arrow_body(arrow_expr, context)
    } else {
      true
    }
  }
}

fn is_member_expr(expr: &Expression) -> bool {
  matches!(
    expr,
    Expression::StaticMemberExpression(_) | Expression::ComputedMemberExpression(_) | Expression::PrivateFieldExpression(_)
  )
}

fn should_skip_paren_expr<'a>(node: &'a ParenthesizedExpression<'a>, context: &Context<'a>) -> bool {
  let inner = &node.expression;
  if node_helpers::has_surrounding_different_line_comments(expr_to_node(inner), context.program) {
    return false;
  }
  if let Some(open_paren) = context.token_finder.get_previous_token_if_open_paren(&node.expression.range()) {
    let trailing_comments = open_paren.trailing_comments_fast(context.program);
    if trailing_comments
      .into_iter()
      .any(|c| c.is_line() && c.start_line_fast(context.program) == open_paren.end_line_fast(context.program))
    {
      return false;
    }
  }
  if has_line_comment_before_next_close_paren(node.expression.range(), context) {
    return false;
  }

  // keep parens around any destructuring assignments
  if let Expression::AssignmentExpression(assign_expr) = inner {
    if matches!(assign_expr.left, AssignmentTarget::ObjectAssignmentTarget(_)) {
      return false;
    }
  }

  if matches!(inner, Expression::SequenceExpression(_)) {
    // don't care about extra logic for sequence expressions
    return false;
  }

  // keep when there is a JSDoc because it could be a type assertion or satisfies
  for c in node.leading_comments_fast(context.program) {
    if c.is_block() && c.text_fast(context.program).starts_with("/**") {
      return false;
    }
  }
  if let Some(open_paren) = context.token_finder.get_previous_token_if_open_paren(&node.expression.range()) {
    for c in open_paren.leading_comments_fast(context.program) {
      if c.is_block() && c.text_fast(context.program).starts_with("/**") {
        return false;
      }
    }
    if let Some(previous_token) = open_paren.previous_token_fast(context.program) {
      for c in previous_token.trailing_comments_fast(context.program) {
        if c.is_block() && c.text_fast(context.program).starts_with("/**") {
          return false;
        }
      }
    }
  }

  // keep for `(val as number)++` or `(<number>val)++`
  let parent = context.parent();
  if matches!(parent, Node::UpdateExpression(_)) && matches!(inner, Expression::TSAsExpression(_) | Expression::TSTypeAssertion(_)) {
    return false;
  }

  if matches!(inner, Expression::ArrayExpression(_) | Expression::Identifier(_)) {
    return true;
  }

  if let Expression::ChainExpression(chain_expr) = inner {
    if matches!(
      chain_element_to_node(&chain_expr.expression),
      Node::CallExpression(_) | Node::StaticMemberExpression(_) | Node::ComputedMemberExpression(_) | Node::PrivateFieldExpression(_)
    ) {
      return true;
    }
  }

  if matches!(
    parent,
    Node::StaticMemberExpression(_) | Node::ComputedMemberExpression(_) | Node::PrivateFieldExpression(_)
  ) && is_member_expr(inner)
  {
    return true;
  }

  // skip over any paren exprs within paren exprs and needless paren exprs
  // (ComputedPropName / KeyValueProp are both ObjectProperty in oxc)
  if matches!(
    parent,
    Node::ParenthesizedExpression(_)
      | Node::ExpressionStatement(_)
      | Node::JSXElement(_)
      | Node::JSXFragment(_)
      | Node::JSXExpressionContainer(_)
      | Node::UpdateExpression(_)
      | Node::ObjectProperty(_)
  ) {
    return true;
  }

  // skip explicitly parsing this as a paren expr as that will be handled
  // in the JSX element/fragment and it might collapse back to not having a paren expr
  if matches!(inner, Expression::JSXElement(_) | Expression::JSXFragment(_)) {
    return is_jsx_paren_expr_handled_node(expr_to_node(inner), context);
  }

  if let Node::AssignmentExpression(assign_expr) = parent {
    if assign_expr.right.range().contains(&node.range()) {
      return true;
    }
  }

  if let Node::VariableDeclarator(var_decl) = parent {
    if !matches!(inner, Expression::AssignmentExpression(_)) {
      if let Some(init) = &var_decl.init {
        if init.range().contains(&node.range()) {
          return true;
        }
      }
    }
  }

  // a parenthesized (non-spread) call/new argument or array element: oxc has no
  // ExprOrSpread wrapper, so the call/array is the direct parent (spreads are a
  // separate SpreadElement node). Preserve parens when the node is the callee.
  match parent {
    Node::CallExpression(call_expr) => return !call_expr.callee.range().contains(&node.range()),
    Node::NewExpression(new_expr) => return !new_expr.callee.range().contains(&node.range()),
    Node::ArrayExpression(_) => return true,
    _ => {}
  }

  if let Node::ComputedMemberExpression(member_expr) = parent {
    if member_expr.expression.range().contains(&node.range()) {
      return true;
    }
  }

  false
}

fn gen_sequence_expr<'a>(node: &'a SequenceExpression<'a>, context: &mut Context<'a>) -> PrintItems {
  gen_separated_values(
    GenSeparatedValuesParams {
      nodes: node.expressions.iter().map(|x| NodeOrSeparator::Node(expr_to_node(x))).collect(),
      prefer_hanging: context.config.sequence_expression_prefer_hanging,
      force_use_new_lines: is_node_definitely_above_line_width(node.range(), context),
      allow_blank_lines: false,
      separator: TrailingCommas::Never.into(),
      single_line_options: ir_helpers::SingleLineOptions::same_line_maybe_space_separated(),
      multi_line_options: ir_helpers::MultiLineOptions::same_line_start_hanging_indent(),
      force_possible_newline_at_start: false,
      node_sorter: None,
    },
    context,
  )
}

fn gen_spread_element<'a>(node: &'a SpreadElement<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.push_sc(sc!("..."));
  items.extend(gen_node(expr_to_node(&node.argument), context));
  items
}

/// Formats the tagged template literal using an external formatter.
/// Detects the type of embedded language automatically.
fn maybe_gen_tagged_tpl_with_external_formatter<'a>(node: &'a TaggedTemplateExpression<'a>, context: &mut Context<'a>) -> Option<PrintItems> {
  let external_formatter = context.external_formatter.as_ref()?;
  let embedded_lang = normalize_embedded_language_type(node)?;

  let placeholder_css = "@dpr1nt_";
  let placeholder_other = "dpr1nt_";
  // First creates text with placeholders for the expressions.
  let placeholder_text = match embedded_lang {
    "css" => placeholder_css,
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

  // Then formats the text with the external formatter.
  let formatted_tpl = match external_formatter(embedded_lang, text.replace(r"\\", "\\"), context.config) {
    Ok(formatted_tpl) => formatted_tpl?.replace("\\", r"\\"),
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

  let mut items = PrintItems::new();
  items.push_sc(sc!("`"));
  items.push_signal(Signal::NewLine);
  items.push_signal(Signal::StartIndent);
  let mut index = 0;
  let mut current_indent_level = 0;
  let use_tabs = context.config.use_tabs;
  let indent_width = if use_tabs { 1 } else { context.config.indent_width };
  let indent_char = if use_tabs { '\t' } else { ' ' };
  for line in formatted_tpl.lines() {
    // count indent characters
    let mut pos = line.chars().take_while(|ch| *ch == indent_char).count();
    let indent_level = if indent_width == 0 { 0 } else { pos / indent_width as usize };
    while indent_level > current_indent_level {
      items.push_signal(Signal::StartIndent);
      current_indent_level += 1;
    }
    while indent_level < current_indent_level {
      items.push_signal(Signal::FinishIndent);
      current_indent_level -= 1;
    }
    let mut parts = line[pos..].split(placeholder_text).enumerate().peekable();
    while let Some((i, part)) = parts.next() {
      let end = pos + part.len();
      if i > 0 {
        pos += part.find("_d").unwrap() + 2;
      }
      let text = &line[pos..end];
      if !text.is_empty() {
        items.extend(gen_from_raw_string(text));
      }
      if parts.peek().is_some() {
        items.push_sc(sc!("${"));
        items.extend(gen_node(expr_to_node(&node.quasi.expressions[index]), context));
        items.push_sc(sc!("}"));
        pos = end + placeholder_text.len();
        index += 1;
      }
    }
    items.push_signal(Signal::NewLine);
  }
  while current_indent_level > 0 {
    items.push_signal(Signal::FinishIndent);
    current_indent_level -= 1;
  }
  items.push_signal(Signal::FinishIndent);
  items.push_sc(sc!("`"));
  Some(items)
}

/// Normalizes the type of embedded language in a tagged template literal.
fn normalize_embedded_language_type<'a>(node: &'a TaggedTemplateExpression<'a>) -> Option<&'a str> {
  match &node.tag {
    Expression::Identifier(ident) => Some(ident.name.as_str()),
    Expression::StaticMemberExpression(member_expr) => {
      if let Expression::Identifier(ident) = &member_expr.object {
        if ident.name.as_str() == "styled" {
          return Some("css"); // styled.foo`...`
        }
      }
      None
    }
    Expression::CallExpression(call_expr) => {
      if let Expression::Identifier(ident) = &call_expr.callee {
        if ident.name.as_str() == "styled" {
          return Some("css"); // styled(Button)`...`
        }
      }
      None
    }
    _ => None,
  }
}

fn has_line_comment_before_next_close_paren(inner_range: SourceRange, context: &Context) -> bool {
  let Some(close_paren) = context.token_finder.get_next_token_if_close_paren(&inner_range) else {
    return false;
  };
  close_paren.leading_comments_fast(context.program).any(|c| c.is_line())
    || context
      .program
      .comments()
      .iter()
      .any(|c| c.is_line() && c.start() >= inner_range.end() && c.end() <= close_paren.start())
}

fn gen_tagged_tpl<'a>(node: &'a TaggedTemplateExpression<'a>, context: &mut Context<'a>) -> PrintItems {
  let use_space = context.config.tagged_template_space_before_literal;
  let mut items = gen_node(expr_to_node(&node.tag), context);
  if let Some(type_params) = &node.type_arguments {
    items.extend(gen_node(Node::TSTypeParameterInstantiation(type_params), context));
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

  items.push_condition(conditions::indent_if_start_of_line(gen_node(Node::TemplateLiteral(&node.quasi), context)));
  items
}

fn gen_tpl<'a>(node: &'a TemplateLiteral<'a>, context: &mut Context<'a>) -> PrintItems {
  gen_template_literal(
    node.quasis.iter().map(Node::TemplateElement).collect(),
    node.expressions.iter().map(expr_to_node).collect(),
    context,
  )
}

fn gen_tpl_element<'a>(node: &TemplateElement<'a>, context: &mut Context<'a>) -> PrintItems {
  gen_from_raw_string(node.text_fast(context.program))
}

fn gen_template_literal<'a>(quasis: Vec<Node<'a>>, exprs: Vec<Node<'a>>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  let mut is_head = true;
  for node in get_nodes(quasis, exprs) {
    if let Node::TemplateElement(node_tpl) = node {
      items.extend(gen_node_with_inner_gen(node, context, |node_items, _| {
        let mut items = PrintItems::new();
        items.push_sc(if is_head { sc!("`") } else { sc!("}") });
        items.push_signal(Signal::StartIgnoringIndent);
        items.extend(node_items);
        items.push_sc(if node_tpl.tail { sc!("`") } else { sc!("${") });
        items.push_signal(Signal::FinishIgnoringIndent);
        items
      }));
    } else {
      let leading_comment_items = gen_leading_comments_on_previous_lines(&node.range(), context);
      let trailing_comment_items = gen_trailing_comments_as_statements(&node.range(), context);
      let close_brace_comments = get_template_substitution_close_brace_comments(node, context);
      for comment in &close_brace_comments {
        context.mark_comment_handled(comment);
      }
      let generated_expr = gen_node(node, context);
      if !leading_comment_items.is_empty() || !trailing_comment_items.is_empty() || !close_brace_comments.is_empty() {
        items.push_signal(Signal::StartIndent);
        if !leading_comment_items.is_empty() {
          items.extend(surround_with_new_lines(leading_comment_items));
        }
        items.extend(generated_expr);
        if !trailing_comment_items.is_empty() {
          items.extend(trailing_comment_items);
        }
        if !close_brace_comments.is_empty() {
          items.extend(gen_comments_as_statements_unchecked(
            close_brace_comments.into_iter(),
            Some(&node.range()),
            context,
          ));
        }
        items.push_signal(Signal::FinishIndent);
      } else {
        let keep_on_one_line = get_keep_on_one_line(node);
        let possible_surround_newlines = get_possible_surround_newlines(node);
        items.extend(if keep_on_one_line {
          with_no_new_lines(generated_expr)
        } else if possible_surround_newlines {
          ir_helpers::surround_with_newlines_indented_if_multi_line(new_line_group(generated_expr), context.config.indent_width)
        } else {
          generated_expr
        });
      }
    }
    is_head = false;
  }
  return items;

  fn get_template_substitution_close_brace_comments<'a>(node: Node<'a>, context: &Context<'a>) -> Vec<&'a Comment> {
    let Some(close_brace) = node
      .next_tokens_fast(context.program)
      .iter()
      .find(|token| token.text_fast(context.program).starts_with('}'))
    else {
      return Vec::new();
    };
    if close_brace.start_line_fast(context.program) == node.end_line_fast(context.program) {
      return Vec::new();
    }
    get_comments_between(node.end(), close_brace.start(), context)
  }

  fn get_nodes<'a>(quasis: Vec<Node<'a>>, exprs: Vec<Node<'a>>) -> Vec<Node<'a>> {
    let mut quasis = quasis;
    let mut exprs = exprs;
    let mut nodes = Vec::new();

    // reverse the vectors and iterate from the back
    quasis.reverse();
    exprs.reverse();

    while !quasis.is_empty() || !exprs.is_empty() {
      let current_quasis = quasis.last();
      let current_expr = exprs.last();

      let is_quasis = if let Some(current_quasis) = current_quasis {
        if let Some(current_expr) = current_expr {
          current_quasis.start() < current_expr.start()
        } else {
          true
        }
      } else {
        false
      };

      if is_quasis {
        nodes.push(quasis.pop().unwrap());
      } else {
        nodes.push(exprs.pop().unwrap());
      }
    }

    nodes
  }

  // handle this on a case by case basis for now
  fn get_keep_on_one_line(node: Node) -> bool {
    match node {
      Node::IdentifierReference(_)
      | Node::IdentifierName(_)
      | Node::Super(_)
      | Node::ThisExpression(_)
      | Node::MetaProperty(_)
      | Node::StringLiteral(_)
      | Node::PrivateIdentifier(_) => true,
      Node::ChainExpression(expr) => get_keep_on_one_line(chain_element_to_node(&expr.expression)),
      Node::StaticMemberExpression(_) | Node::ComputedMemberExpression(_) | Node::PrivateFieldExpression(_) => keep_member_expr_on_one_line(node),
      Node::CallExpression(expr) => keep_call_expr_on_one_line(CallOrOptCallExpr(expr)),
      _ => false,
    }
  }

  fn get_possible_surround_newlines(node: Node) -> bool {
    match node {
      Node::ChainExpression(expr) => get_possible_surround_newlines(chain_element_to_node(&expr.expression)),
      Node::ConditionalExpression(_) => true,
      Node::BinaryExpression(_) | Node::LogicalExpression(_) => true,
      Node::StaticMemberExpression(_) | Node::ComputedMemberExpression(_) | Node::PrivateFieldExpression(_) => !keep_member_expr_on_one_line(node),
      Node::CallExpression(expr) => !keep_call_expr_on_one_line(CallOrOptCallExpr(expr)),
      _ => false,
    }
  }

  fn keep_member_expr_on_one_line(node: Node) -> bool {
    match node {
      Node::StaticMemberExpression(expr) => get_keep_on_one_line(expr_to_node(&expr.object)) && get_keep_on_one_line(Node::IdentifierName(&expr.property)),
      Node::PrivateFieldExpression(expr) => get_keep_on_one_line(expr_to_node(&expr.object)) && get_keep_on_one_line(Node::PrivateIdentifier(&expr.field)),
      // computed member accesses (`a[b]`) are not kept on one line
      _ => false,
    }
  }

  fn keep_call_expr_on_one_line(expr: CallOrOptCallExpr) -> bool {
    expr.args().is_empty() && get_keep_on_one_line(expr_to_node(expr.callee()))
  }
}

fn gen_type_assertion<'a>(node: &'a TSTypeAssertion<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.push_sc(sc!("<"));
  items.extend(gen_node(ts_type_to_node(&node.type_annotation), context));
  items.push_sc(sc!(">"));
  if context.config.type_assertion_space_before_expression {
    items.push_space();
  }
  items.extend(gen_node(expr_to_node(&node.expression), context));
  if matches!(context.parent(), Node::UpdateExpression(_)) || is_assignment_lhs(node, context) {
    items = surround_expr_with_configured_parens(items, context);
  }
  items
}

fn surround_expr_with_configured_parens(items: PrintItems, context: &Context) -> PrintItems {
  if context.config.paren_expression_space_around {
    surround_with_parens(surround_with_spaces(items))
  } else {
    surround_with_parens(items)
  }
}

fn is_assignment_lhs<'a>(node: &impl SourceRanged, context: &Context<'a>) -> bool {
  if let Node::AssignmentExpression(assign_expr) = context.parent() {
    assign_expr.left.range().contains(&node.range())
  } else {
    false
  }
}

fn gen_unary_expr<'a>(node: &'a UnaryExpression<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.push_sc(get_operator_text(node.operator));
  items.extend(gen_node(expr_to_node(&node.argument), context));

  // wrap in parens when the parent is an `in` / `instanceof` binary expression
  let should_use_parens = matches!(
    context.parent(),
    Node::BinaryExpression(parent) if matches!(parent.operator, BinaryOperator::In | BinaryOperator::Instanceof)
  );
  if should_use_parens {
    items = surround_with_parens(items);
  }

  return items;

  fn get_operator_text(op: UnaryOperator) -> &'static StringContainer {
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
}

fn gen_update_expr<'a>(node: &'a UpdateExpression<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  let operator_text = get_operator_text(node.operator);
  if node.prefix {
    items.push_sc(operator_text);
  }
  items.extend(gen_node(simple_assignment_target_to_node(&node.argument), context));
  if matches!(node.argument, SimpleAssignmentTarget::TSAsExpression(_)) {
    items = surround_expr_with_configured_parens(items, context);
  }
  if !node.prefix {
    items.push_sc(operator_text);
  }
  return items;

  fn get_operator_text(operator: UpdateOperator) -> &'static StringContainer {
    match operator {
      UpdateOperator::Decrement => sc!("--"),
      UpdateOperator::Increment => sc!("++"),
    }
  }
}

fn gen_yield_expr<'a>(node: &'a YieldExpression<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.push_sc(sc!("yield"));
  if node.delegate {
    items.push_sc(sc!("*"));
  }
  if let Some(arg) = &node.argument {
    items.push_space();
    items.extend(gen_node(expr_to_node(arg), context));
  }
  items
}

/* exports */

fn gen_export_named_specifier<'a>(node: &'a ExportSpecifier<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();

  let parent_type_only = matches!(context.parent(), Node::ExportNamedDeclaration(p) if p.export_kind.is_type());
  if node.export_kind.is_type() && !parent_type_only {
    items.push_sc(sc!("type "));
  }

  items.extend(gen_node(module_export_name_to_node(&node.local), context));
  // oxc always populates `exported`; an alias is present when it differs from `local`.
  if node.local.range() != node.exported.range() {
    items.push_signal(Signal::SpaceOrNewLine);
    items.push_condition(conditions::indent_if_start_of_line({
      let mut items = PrintItems::new();
      items.push_sc(sc!("as "));
      items.extend(gen_node(module_export_name_to_node(&node.exported), context));
      items
    }));
  }

  items
}

/* imports */

fn gen_import_named_specifier<'a>(node: &'a ImportSpecifier<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();

  let parent_type_only = matches!(context.parent(), Node::ImportDeclaration(p) if p.import_kind.is_type());
  if node.import_kind.is_type() && !parent_type_only {
    items.push_sc(sc!("type "));
  }

  // oxc always populates `imported`; an alias is present when it differs from `local`.
  if node.imported.range() != node.local.range() {
    items.extend(gen_node(module_export_name_to_node(&node.imported), context));
    items.push_signal(Signal::SpaceOrNewLine);
    items.push_condition(conditions::indent_if_start_of_line({
      let mut items = PrintItems::new();
      items.push_sc(sc!("as "));
      items.extend(gen_node(Node::BindingIdentifier(&node.local), context));
      items
    }));
  } else {
    items.extend(gen_node(Node::BindingIdentifier(&node.local), context));
  }

  items
}

fn gen_import_namespace_specifier<'a>(node: &'a ImportNamespaceSpecifier<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.push_sc(sc!("* as "));
  items.extend(gen_node(Node::BindingIdentifier(&node.local), context));
  items
}

fn gen_external_module_ref<'a>(node: &'a TSExternalModuleReference<'a>, context: &mut Context<'a>) -> PrintItems {
  // force everything on a single line
  let mut items = PrintItems::new();
  items.push_sc(sc!("require("));
  items.extend(gen_node(Node::StringLiteral(&node.expression), context));
  items.push_sc(sc!(")"));
  items
}

/* interface / type element */

fn gen_call_signature_decl<'a>(node: &'a TSCallSignatureDeclaration<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  let start_lsil = LineStartIndentLevel::new("startCallSignature");

  items.push_info(start_lsil);
  if let Some(type_params) = &node.type_parameters {
    items.extend(gen_node(Node::TSTypeParameterDeclaration(type_params), context));
  }
  let param_nodes = formal_params_to_nodes(&node.params);
  let param_count = param_nodes.len();
  items.extend(gen_parameters_or_arguments(
    GenParametersOrArgumentsOptions {
      node: Node::TSCallSignatureDeclaration(node),
      range: node.get_parameters_range(context),
      nodes: param_nodes,
      custom_close_paren: |context| {
        Some(gen_close_paren_with_type(
          GenCloseParenWithTypeOptions {
            start_lsil,
            type_node: node.return_type.as_deref().map(Node::TSTypeAnnotation),
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

fn gen_construct_signature_decl<'a>(node: &'a TSConstructSignatureDeclaration<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  let start_lsil = LineStartIndentLevel::new("startConstructSignature");

  items.push_info(start_lsil);
  items.push_sc(sc!("new"));
  if context.config.construct_signature_space_after_new_keyword {
    items.push_space();
  }
  if let Some(type_params) = &node.type_parameters {
    items.extend(gen_node(Node::TSTypeParameterDeclaration(type_params), context));
  }
  let param_nodes = formal_params_to_nodes(&node.params);
  let param_count = param_nodes.len();
  items.extend(gen_parameters_or_arguments(
    GenParametersOrArgumentsOptions {
      node: Node::TSConstructSignatureDeclaration(node),
      range: node.get_parameters_range(context),
      nodes: param_nodes,
      custom_close_paren: |context| {
        Some(gen_close_paren_with_type(
          GenCloseParenWithTypeOptions {
            start_lsil,
            type_node: node.return_type.as_deref().map(Node::TSTypeAnnotation),
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

fn gen_index_signature<'a>(node: &'a TSIndexSignature<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();

  if node.r#static {
    items.push_sc(sc!("static "));
  }
  if node.readonly {
    items.push_sc(sc!("readonly "));
  }

  let param = Node::TSIndexSignatureName(node.parameters.first().expect("Expected the index signature to have one parameter."));
  items.extend(gen_computed_prop_like(
    |context| gen_node(param, context),
    GenComputedPropLikeOptions {
      inner_node_range: param.range(),
    },
    context,
  ));
  items.extend(gen_type_ann_with_colon_if_exists(Some(&node.type_annotation), context));

  if matches!(context.parent(), Node::Class(_) | Node::ClassBody(_)) && context.config.semi_colons.is_true() {
    items.push_sc(sc!(";"));
  }

  items
}

// `[key: string]` index-signature parameter (oxc surfaces this as TSIndexSignatureName)
fn gen_index_signature_name<'a>(node: &'a TSIndexSignatureName<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = gen_from_raw_string(node.name.as_str());
  items.extend(gen_type_ann_with_colon_if_exists(Some(&node.type_annotation), context));
  items
}

fn gen_method_signature<'a>(node: &'a TSMethodSignature<'a>, context: &mut Context<'a>) -> PrintItems {
  let method_kind = match node.kind {
    TSMethodSignatureKind::Method => MethodSignatureLikeKind::Method,
    TSMethodSignatureKind::Get => MethodSignatureLikeKind::Getter,
    TSMethodSignatureKind::Set => MethodSignatureLikeKind::Setter,
  };
  gen_method_signature_like(
    MethodSignatureLike {
      node: Node::TSMethodSignature(node),
      method_kind,
      computed: node.computed,
      optional: node.optional,
      key: prop_key_to_node(&node.key),
      parameters_range: node.get_parameters_range(context),
      type_params: node.type_parameters.as_deref().map(Node::TSTypeParameterDeclaration),
      params: formal_params_to_nodes(&node.params),
      type_ann: node.return_type.as_deref().map(Node::TSTypeAnnotation),
    },
    context,
  )
}

enum MethodSignatureLikeKind {
  Method,
  Getter,
  Setter,
}

struct MethodSignatureLike<'a> {
  node: Node<'a>,
  method_kind: MethodSignatureLikeKind,
  computed: bool,
  optional: bool,
  key: Node<'a>,
  type_params: Option<Node<'a>>,
  parameters_range: Option<SourceRange>,
  params: Vec<Node<'a>>,
  type_ann: Option<Node<'a>>,
}

fn gen_method_signature_like<'a>(node: MethodSignatureLike<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
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

fn gen_property_signature<'a>(node: &'a TSPropertySignature<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  if node.readonly {
    items.push_sc(sc!("readonly "));
  }

  items.extend(if node.computed {
    gen_computed_prop_like(
      |context| gen_node(prop_key_to_node(&node.key), context),
      GenComputedPropLikeOptions {
        inner_node_range: node.key.range(),
      },
      context,
    )
  } else {
    gen_quotable_prop(prop_key_to_node(&node.key), context)
  });

  if node.optional {
    items.push_sc(sc!("?"));
  }
  items.extend(gen_type_ann_with_colon_if_exists(node.type_annotation.as_deref(), context));

  items
}

fn gen_quotable_prop<'a>(node: Node<'a>, context: &mut Context<'a>) -> PrintItems {
  match context.config.quote_props {
    QuoteProps::AsNeeded => match node {
      Node::StringLiteral(str_lit) => {
        let text = str_lit.value.as_str();
        if is_text_valid_identifier(text) {
          gen_from_raw_string(text)
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
        Node::StringLiteral(str_lit) => gen_from_raw_string(str_lit.value.as_str()),
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

fn gen_interface_body<'a>(node: &'a TSInterfaceBody<'a>, context: &mut Context<'a>) -> PrintItems {
  let start_header_lsil = get_parent_lsil(context);

  return context.with_maybe_consistent_props(
    node,
    |node| use_consistent_quotes_for_members(node.body.iter().map(ts_signature_to_node)),
    |context, node| {
      gen_membered_body(
        GenMemberedBodyOptions {
          node: Node::TSInterfaceBody(node),
          members: node.body.iter().map(ts_signature_to_node).collect(),
          start_header_lsil,
          brace_position: context.config.interface_declaration_brace_position,
          should_use_blank_line: move |previous, next, context| node_helpers::has_separating_blank_line(&previous, &next, context.program),
          separator: context.config.semi_colons.into(),
        },
        context,
      )
    },
  );

  fn get_parent_lsil(context: &mut Context) -> Option<LineStartIndentLevel> {
    for ancestor in context.parent_stack.iter() {
      if let Node::TSInterfaceDeclaration(ancestor) = *ancestor {
        return context.get_lsil_for_node(ancestor).map(|x| x.to_owned());
      }
    }
    None
  }
}

fn use_consistent_quotes_for_members<'a>(mut members: impl Iterator<Item = Node<'a>>) -> bool {
  fn check_prop_key(key: &PropertyKey) -> bool {
    match key {
      PropertyKey::StaticIdentifier(ident) => !is_text_valid_identifier(ident.name.as_str()),
      PropertyKey::StringLiteral(str) => !is_text_valid_identifier(str.value.as_str()),
      _ => false,
    }
  }

  members.any(|m| match m {
    // class members
    // Do not match class properties (Node::PropertyDefinition)
    // With `--strictPropertyInitialization`, TS treats properties
    // with quoted names differently than unquoted ones.
    // See https://github.com/microsoft/TypeScript/pull/20075
    Node::MethodDefinition(method) => check_prop_key(&method.key),
    // interface members (oxc folds getter/setter signatures into TSMethodSignature)
    Node::TSPropertySignature(signature) => check_prop_key(&signature.key),
    Node::TSMethodSignature(signature) => check_prop_key(&signature.key),
    // object members
    Node::ObjectProperty(prop) => check_prop_key(&prop.key),
    _ => false,
  })
}

fn gen_type_lit<'a>(node: &'a TSTypeLiteral<'a>, context: &mut Context<'a>) -> PrintItems {
  return context.with_maybe_consistent_props(
    node,
    |node| use_consistent_quotes_for_members(node.members.iter().map(ts_signature_to_node)),
    |context, node| {
      gen_object_like_node(
        GenObjectLikeNodeOptions {
          node: Node::TSTypeLiteral(node),
          members: node.members.iter().map(ts_signature_to_node).collect(),
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
          surround_single_line_with_space_at_end: context.config.type_literal_space_surrounding_properties,
          allow_blank_lines: true,
          node_sorter: None,
        },
        context,
      )
    },
  );

  fn semi_colon_or_comma_to_separator_value(value: SemiColonOrComma, context: &mut Context) -> SeparatorValue {
    match value {
      SemiColonOrComma::Comma => SeparatorValue::Comma(context.config.type_literal_trailing_commas),
      SemiColonOrComma::SemiColon => SeparatorValue::SemiColon(context.config.semi_colons),
    }
  }
}

/* jsx */

fn gen_jsx_attribute<'a>(node: &'a JSXAttribute<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.extend(gen_node(jsx_attribute_name_to_node(&node.name), context));
  if let Some(value) = &node.value {
    items.push_sc(sc!("="));
    let surround_with_braces = context.token_finder.get_previous_token_if_open_brace(value).is_some();
    let inner_items = gen_node(jsx_attribute_value_to_node(value), context);
    items.extend(if surround_with_braces {
      gen_as_jsx_expr_container(Node::JSXAttribute(node), inner_items, None, context)
    } else {
      inner_items
    });
  }
  items
}

fn gen_jsx_closing_element<'a>(node: &'a JSXClosingElement<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.push_sc(sc!("</"));
  items.extend(gen_node(jsx_element_name_to_node(&node.name), context));
  items.push_sc(sc!(">"));
  items
}

fn gen_jsx_closing_fragment<'a>(_: &'a JSXClosingFragment, _: &mut Context<'a>) -> PrintItems {
  "</>".into()
}

// oxc has no parent pointers; reconstruct a node's ancestor chain (immediate parent
// first). If `node` isn't the node currently being generated, it is a direct child of
// the current node, so the current node is its immediate parent.
fn ancestors_of<'a>(node: Node<'a>, context: &Context<'a>) -> Vec<Node<'a>> {
  let mut result = Vec::new();
  if node.range() != context.current_node.range() {
    result.push(context.current_node);
  }
  result.extend(context.parent_stack.iter().copied());
  result
}

fn handle_jsx_surrounding_parens(inner_items: PrintItems, context: &mut Context<'_>) -> PrintItems {
  if !is_jsx_paren_expr_handled_node(context.current_node, context) {
    if should_jsx_surround_newlines(context.current_node, context) {
      return surround_with_newlines_indented_if_multi_line(inner_items, context.config.indent_width);
    } else {
      return inner_items;
    }
  }

  if matches!(context.parent(), Node::JSXExpressionContainer(_)) && context.config.jsx_multi_line_parens != JsxMultiLineParens::Always {
    return surround_with_newlines_indented_if_multi_line(inner_items, context.config.indent_width);
  }

  let start_ln = LineNumber::new("conditionalParenStart");
  let end_ln = LineNumber::new("conditionalParenEnd");
  let mut items = PrintItems::new();
  let inner_items_rc = inner_items.into_rc_path();

  items.push_info(start_ln);
  items.push_anchor(LineNumberAnchor::new(end_ln));
  items.extend(actions::if_column_number_changes(move |context| {
    context.clear_info(end_ln);
  }));
  items.push_condition(if_true_or(
    "parensOrNewlinesIfMultipleLines",
    Rc::new(move |context| condition_helpers::is_multiple_lines(context, start_ln, end_ln)),
    surround_with_parens(surround_with_new_lines(with_indent(inner_items_rc.into()))),
    {
      let mut items = PrintItems::new();
      items.push_signal(Signal::PossibleNewLine);
      items.extend(inner_items_rc.into());
      items
    },
  ));

  items.push_info(end_ln);
  return items;

  fn should_jsx_surround_newlines<'a>(node: Node<'a>, context: &Context<'a>) -> bool {
    let mut iter = ancestors_of(node, context).into_iter();
    let mut parent = match iter.next() {
      Some(p) => p,
      None => return false,
    };
    while let Node::ParenthesizedExpression(paren_expr) = parent {
      if node_helpers::has_surrounding_comments(expr_to_node(&paren_expr.expression), context.program) {
        return false;
      }
      parent = match iter.next() {
        Some(p) => p,
        None => return false,
      };
    }

    matches!(parent, Node::JSXExpressionContainer(_))
  }
}

fn is_jsx_paren_expr_handled_node<'a>(node: Node<'a>, context: &Context<'a>) -> bool {
  if context.config.jsx_multi_line_parens == JsxMultiLineParens::Never {
    return false;
  }

  if !matches!(node, Node::JSXElement(_) | Node::JSXFragment(_)) {
    return false;
  }

  let mut iter = ancestors_of(node, context).into_iter();
  let mut parent = match iter.next() {
    Some(p) => p,
    None => return false,
  };
  // Only wrap the top-level JSX element in parens
  if matches!(parent, Node::JSXElement(_) | Node::JSXFragment(_)) {
    return false;
  }

  if node_helpers::has_surrounding_comments(node, context.program) {
    return false;
  }

  while matches!(parent, Node::ParenthesizedExpression(_)) {
    if node_helpers::has_surrounding_comments(parent, context.program) {
      return false;
    }
    parent = match iter.next() {
      Some(p) => p,
      None => return false,
    };
  }

  if context.config.jsx_multi_line_parens == JsxMultiLineParens::Always {
    return true;
  }

  // do not allow in expr statement, argument (call/new/array - oxc has no ExprOrSpread),
  // jsx exprs, or member exprs
  !matches!(
    parent,
    Node::ExpressionStatement(_)
      | Node::CallExpression(_)
      | Node::NewExpression(_)
      | Node::ArrayExpression(_)
      | Node::JSXExpressionContainer(_)
      | Node::StaticMemberExpression(_)
      | Node::ComputedMemberExpression(_)
      | Node::PrivateFieldExpression(_)
  )
}

fn gen_jsx_element<'a>(node: &'a JSXElement<'a>, context: &mut Context<'a>) -> PrintItems {
  let items = if let Some(closing) = &node.closing_element {
    // pre element bodies should be formatted as-is
    if node.opening_element.name.text_fast(context.program) == "pre" {
      let mut items = gen_node(Node::JSXOpeningElement(&node.opening_element), context);
      let in_between_range = SourceRange::new(node.opening_element.end(), closing.start());
      items.extend(ir_helpers::gen_from_raw_string_trim_line_ends(in_between_range.text_fast(context.program)));
      items.extend(gen_node(Node::JSXClosingElement(closing), context));
      items
    } else {
      let result = gen_jsx_with_opening_and_closing(
        GenJsxWithOpeningAndClosingOptions {
          opening_element: Node::JSXOpeningElement(&node.opening_element),
          closing_element: Node::JSXClosingElement(closing),
          children: node.children.iter().map(jsx_child_to_node).collect(),
        },
        context,
      );
      context.store_info_range_for_node(node, (result.start_ln, result.end_ln));
      result.items
    }
  } else {
    let start_ln = LineNumber::new("jsxElementStart");
    let end_ln = LineNumber::new("jsxElementEnd");
    let mut items = PrintItems::new();

    context.store_info_range_for_node(node, (start_ln, end_ln));

    items.push_info(start_ln);
    items.extend(gen_node(Node::JSXOpeningElement(&node.opening_element), context));
    items.push_info(end_ln);
    items
  };

  handle_jsx_surrounding_parens(items, context)
}

fn gen_jsx_empty_expr<'a>(node: &'a JSXEmptyExpression, context: &mut Context<'a>) -> PrintItems {
  gen_comment_collection(get_jsx_empty_expr_comments(node, context), None, None, context)
}

fn gen_jsx_expr_container<'a>(node: &'a JSXExpressionContainer<'a>, context: &mut Context<'a>) -> PrintItems {
  // Don't send JSX empty expressions to gen_node because it will not handle comments
  // the way they should be specifically handled for empty expressions.
  let expr_node = jsx_expression_to_node(&node.expression);
  let gen_inner = match &node.expression {
    JSXExpression::EmptyExpression(_) => gen_comment_collection(get_jsx_expr_container_empty_comments(node, context), None, None, context),
    expr => gen_node(jsx_expression_to_node(expr), context),
  };
  let container_range = get_jsx_expression_container_brace_range(expr_node, node.range(), context);

  gen_as_jsx_expr_container(expr_node, gen_inner, Some(container_range), context)
}

fn gen_as_jsx_expr_container(expr: Node, inner_items: PrintItems, container_range: Option<SourceRange>, context: &mut Context) -> PrintItems {
  let surround_with_space = context.config.jsx_expression_container_space_surrounding_expression;
  let open_token_end = container_range.map(|range| range.start() + 1);
  let close_token_start = container_range.map(|range| range.end() - 1);
  let open_token_comments = open_token_end.map(|pos| get_comments_between(pos, expr.start(), context)).unwrap_or_default();
  let close_token_comments = close_token_start.map(|pos| get_comments_between(expr.end(), pos, context)).unwrap_or_default();
  let has_open_token_comments = open_token_comments
    .iter()
    .any(|c| c.is_line() || c.start_line_fast(context.program) < expr.start_line_fast(context.program));
  let has_close_token_comments = close_token_comments
    .iter()
    .any(|c| c.is_line() || c.end_line_fast(context.program) > expr.end_line_fast(context.program));
  let surround_with_new_lines = should_surround_with_newlines(expr, context.program) || has_open_token_comments || has_close_token_comments;
  let mut items = PrintItems::new();
  items.push_sc(sc!("{"));
  if surround_with_new_lines {
    items.push_signal(Signal::NewLine);
    items.push_signal(Signal::StartIndent);
    if !open_token_comments.is_empty() {
      let comments = gen_comments_as_statements(open_token_comments.into_iter(), None, context);
      if !comments.is_empty() {
        items.extend(comments);
        items.push_signal(Signal::NewLine);
      }
    }
  } else if surround_with_space {
    items.push_space();
  }
  items.extend(inner_items);
  if surround_with_new_lines {
    items.extend(gen_trailing_comments_as_statements(&expr.range(), context));
    if !close_token_comments.is_empty() {
      items.extend(gen_comments_as_statements(close_token_comments.into_iter(), Some(&expr.range()), context));
    }
    items.push_signal(Signal::NewLine);
    items.push_signal(Signal::FinishIndent);
  } else if surround_with_space {
    items.push_space();
  }
  items.push_sc(sc!("}"));

  return items;

  fn should_surround_with_newlines(expr: Node, program: ProgramInfo) -> bool {
    let expr_start_line = expr.start_line_fast(program);
    for comment in expr.leading_comments_fast(program) {
      if comment.kind == CommentKind::Line {
        return true;
      } else if comment.start_line_fast(program) < expr_start_line {
        return true;
      }
    }
    let expr_end_line = expr.start_line_fast(program);
    for comment in expr.trailing_comments_fast(program) {
      if comment.kind == CommentKind::Line || comment.end_line_fast(program) > expr_end_line {
        return true;
      }
    }

    false
  }
}

fn get_jsx_expression_container_brace_range<'a>(expr: Node<'a>, fallback: SourceRange, context: &Context<'a>) -> SourceRange {
  let Some(open_brace) = context.token_finder.get_previous_token_if_open_brace(&expr) else {
    return fallback;
  };
  let Some(close_brace) = expr.next_tokens_fast(context.program).iter().find(|token| token.kind() == Kind::RCurly) else {
    return fallback;
  };
  SourceRange::new(open_brace.start(), close_brace.end())
}

fn get_comments_between<'a>(start: SourcePos, end: SourcePos, context: &Context<'a>) -> Vec<&'a Comment> {
  if start >= end {
    return Vec::new();
  }
  context
    .program
    .comments()
    .iter()
    .filter(|comment| comment.start() >= start && comment.end() <= end)
    .collect()
}

fn gen_jsx_fragment<'a>(node: &'a JSXFragment<'a>, context: &mut Context<'a>) -> PrintItems {
  let result = gen_jsx_with_opening_and_closing(
    GenJsxWithOpeningAndClosingOptions {
      opening_element: Node::JSXOpeningFragment(&node.opening_fragment),
      closing_element: Node::JSXClosingFragment(&node.closing_fragment),
      children: node.children.iter().map(jsx_child_to_node).collect(),
    },
    context,
  );

  context.store_info_range_for_node(node, (result.start_ln, result.end_ln));

  handle_jsx_surrounding_parens(result.items, context)
}

fn gen_jsx_member_expr<'a>(node: &'a JSXMemberExpression<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.extend(gen_node(jsx_member_expr_object_to_node(&node.object), context));
  items.push_sc(sc!("."));
  items.extend(gen_node(Node::JSXIdentifier(&node.property), context));
  items
}

fn gen_jsx_namespaced_name<'a>(node: &'a JSXNamespacedName<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.extend(gen_node(Node::JSXIdentifier(&node.namespace), context));
  items.push_sc(sc!(":"));
  items.extend(gen_node(Node::JSXIdentifier(&node.name), context));
  items
}

fn gen_jsx_opening_element<'a>(node: &'a JSXOpeningElement<'a>, context: &mut Context<'a>) -> PrintItems {
  let space_before_self_closing_tag_slash = context.config.jsx_self_closing_element_space_before_slash;
  let is_self_closing = is_self_closing(node, context);
  let name_or_type_arg_end = node.type_arguments.as_ref().map(|t| t.end()).unwrap_or_else(|| node.name.end());
  let last_node_end = node.attributes.last().map(|n| n.end()).unwrap_or(name_or_type_arg_end);
  let close_token_start = if is_self_closing { node.end() - 2 } else { node.end() - 1 };
  let comments_before_close_token = get_comments_between(last_node_end, close_token_start, context);
  let comments_to_generate_before_close_token = comments_before_close_token
    .iter()
    .copied()
    .filter(|c| c.is_line() || c.start_line_fast(context.program) > last_node_end.end_line_fast(context.program))
    .collect::<Vec<_>>();
  let has_line_comments_before_close_token = comments_to_generate_before_close_token.iter().any(|c| c.is_line());
  let force_use_new_lines = get_force_is_multi_line(node, context) || has_line_comments_before_close_token;
  let prefer_newline_before_close_bracket =
    get_should_prefer_newline_before_close_bracket(node, is_self_closing, context) || has_line_comments_before_close_token;
  let start_lsil = LineStartIndentLevel::new("openingElementStart");
  let mut items = PrintItems::new();

  items.push_info(start_lsil);
  items.push_sc(sc!("<"));
  items.extend(gen_node(jsx_element_name_to_node(&node.name), context));
  if let Some(type_args) = &node.type_arguments {
    items.extend(gen_node(Node::TSTypeParameterInstantiation(type_args), context));
  }

  if node.attributes.len() == 1 && node.type_arguments.is_none() && is_jsx_attr_with_string(&node.attributes[0]) {
    items.push_space();
    items.extend(gen_node(jsx_attribute_item_to_node(&node.attributes[0]), context));
  } else if !node.attributes.is_empty() {
    let mut multi_line_options = ir_helpers::MultiLineOptions::surround_newlines_indented();
    multi_line_options.newline_at_end = prefer_newline_before_close_bracket;
    items.extend(gen_separated_values(
      GenSeparatedValuesParams {
        nodes: node.attributes.iter().map(|p| NodeOrSeparator::Node(jsx_attribute_item_to_node(p))).collect(),
        prefer_hanging: context.config.jsx_attributes_prefer_hanging,
        force_use_new_lines,
        allow_blank_lines: false,
        separator: Separator::none(),
        single_line_options: ir_helpers::SingleLineOptions::separated_line_starting_with_space(),
        multi_line_options,
        force_possible_newline_at_start: false,
        node_sorter: None,
      },
      context,
    ));
  }

  // generate trailing comments on different lines
  let last_node_end_range = last_node_end.range();
  let generated_comments = gen_comments_as_statements_advancing_handled(
    comments_to_generate_before_close_token.into_iter(),
    if node.attributes.is_empty() { Some(&last_node_end_range) } else { None },
    context,
  );
  if !generated_comments.is_empty() {
    items.extend(with_indent(generated_comments));
    items.push_signal(Signal::NewLine);
  }

  if is_self_closing {
    if space_before_self_closing_tag_slash {
      items.push_force_current_line_indentation();
      items.extend(space_if_not_start_line());
    }
    items.push_sc(sc!("/"));
  } else if context.config.jsx_attributes_prefer_hanging && prefer_newline_before_close_bracket {
    items.push_condition(conditions::new_line_if_hanging(start_lsil, None));
  }
  items.push_sc(sc!(">"));

  return items;

  // oxc's JSXOpeningElement has no self_closing flag; detect it from the source text.
  fn is_self_closing(node: &JSXOpeningElement, context: &Context) -> bool {
    node.text_fast(context.program).trim_end().ends_with("/>")
  }

  fn get_force_is_multi_line(node: &JSXOpeningElement, context: &mut Context) -> bool {
    if context.config.jsx_attributes_prefer_single_line {
      false
    } else if let Some(first_attrib) = node.attributes.first() {
      node_helpers::get_use_new_lines_for_nodes(&node.name, first_attrib, context.program)
    } else {
      false
    }
  }

  fn get_should_prefer_newline_before_close_bracket(node: &JSXOpeningElement, is_self_closing: bool, context: &mut Context) -> bool {
    let bracket_pos_config = match is_self_closing {
      true => context.config.jsx_self_closing_element_bracket_position,
      false => context.config.jsx_opening_element_bracket_position,
    };
    match bracket_pos_config {
      SameOrNextLinePosition::Maintain => {
        if let Some(last_attr) = node.attributes.last() {
          last_attr.end_line_fast(context.program) < node.end_line_fast(context.program)
        } else {
          false
        }
      }
      SameOrNextLinePosition::NextLine => true,
      SameOrNextLinePosition::SameLine => false,
    }
  }

  fn is_jsx_attr_with_string(node: &JSXAttributeItem) -> bool {
    if let JSXAttributeItem::Attribute(attrib) = node {
      if let Some(value) = &attrib.value {
        return matches!(value, JSXAttributeValue::StringLiteral(_));
      }
    }
    false
  }
}

fn gen_jsx_opening_fragment<'a>(_: &'a JSXOpeningFragment, _: &mut Context<'a>) -> PrintItems {
  "<>".into()
}

fn gen_jsx_spread_attribute<'a>(node: &'a JSXSpreadAttribute<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  let spread_token = node.tokens_fast(context.program).iter().find(|token| token.text_fast(context.program) == "...");
  let comments_before_spread = spread_token
    .map(|token| get_comments_between(node.start() + 1, token.start(), context))
    .unwrap_or_default();
  items.push_sc(sc!("{"));
  if !comments_before_spread.is_empty() {
    items.extend(gen_comment_collection(comments_before_spread.into_iter(), None, None, context));
    items.push_space();
  }
  items.push_sc(sc!("..."));
  items.extend(gen_node(expr_to_node(&node.argument), context));
  items.push_sc(sc!("}"));
  items
}

fn gen_jsx_spread_child<'a>(node: &'a JSXSpreadChild<'a>, context: &mut Context<'a>) -> PrintItems {
  gen_as_jsx_expr_container(
    Node::JSXSpreadChild(node),
    {
      let mut items = PrintItems::new();
      items.push_sc(sc!("..."));
      items.extend(gen_node(expr_to_node(&node.expression), context));
      items
    },
    Some(node.range()),
    context,
  )
}

fn gen_jsx_text<'a>(node: &'a JSXText<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();

  for (i, line) in get_lines(node.text_fast(context.program)).into_iter().enumerate() {
    if i > 0 {
      items.push_signal(Signal::NewLine);
      items.push_signal(Signal::NewLine);
    }

    let mut was_last_space_or_newline = true;
    for word in line.split(' ') {
      if !was_last_space_or_newline {
        items.push_signal(Signal::SpaceOrNewLine);
        was_last_space_or_newline = true;
      }
      if !word.is_empty() {
        items.push_string(word.to_string());
        was_last_space_or_newline = false;
      }
    }
  }

  return ir_helpers::new_line_group(items);

  fn get_lines(node_text: &str) -> Vec<String> {
    let mut past_line: Option<&str> = None;
    let lines = node_text.trim().lines().map(|line| line.trim());
    let mut result = Vec::new();
    let mut current_line = String::new();

    for line in lines {
      if let Some(past_line) = past_line {
        if !line.is_empty() && past_line.is_empty() && !current_line.is_empty() {
          result.push(current_line);
          current_line = String::new();
        }
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

/* literals */

fn gen_big_int_literal<'a>(node: &BigIntLiteral<'a>, context: &mut Context<'a>) -> PrintItems {
  node.text_fast(context.program).to_string().into()
}

fn gen_bool_literal(node: &BooleanLiteral) -> PrintItems {
  match node.value {
    true => "true",
    false => "false",
  }
  .into()
}

fn gen_num_literal<'a>(node: &NumericLiteral<'a>, context: &mut Context<'a>) -> PrintItems {
  node.text_fast(context.program).to_string().into()
}

fn gen_reg_exp_literal<'a>(node: &RegExpLiteral<'a>, context: &mut Context<'a>) -> PrintItems {
  // emit the raw regex literal text (pattern + flags)
  node.text_fast(context.program).to_string().into()
}

fn gen_string_literal<'a>(node: &StringLiteral<'a>, context: &mut Context<'a>) -> PrintItems {
  let string_value = string_literal::get_value(node, context);
  if matches!(context.parent(), Node::JSXAttribute(_)) {
    string_literal::gen_jsx_text(&string_value, context)
  } else {
    string_literal::gen_non_jsx_text(&string_value, context)
  }
}

mod string_literal {
  use super::*;

  pub fn gen_non_jsx_text(string_value: &str, context: &mut Context) -> PrintItems {
    match context.config.quote_style {
      QuoteStyle::AlwaysDouble => format_with_double(string_value),
      QuoteStyle::AlwaysSingle => format_with_single(string_value),
      QuoteStyle::PreferDouble => handle_prefer_double(string_value),
      QuoteStyle::PreferSingle => handle_prefer_single(string_value),
    }
  }

  pub fn gen_jsx_text(string_value: &str, context: &mut Context) -> PrintItems {
    // JSX attributes cannot contain escaped quotes so regardless of
    // configuration, allow changing the quote style to single or
    // double depending on if it contains the opposite quote
    match context.config.jsx_quote_style {
      JsxQuoteStyle::PreferDouble => handle_prefer_double(string_value),
      JsxQuoteStyle::PreferSingle => handle_prefer_single(string_value),
    }
  }

  pub fn get_value(node: &StringLiteral, context: &mut Context) -> String {
    let raw_string_text = node.text_fast(context.program);
    if raw_string_text.len() <= 2 {
      return String::new();
    }
    let string_value = &raw_string_text[1..raw_string_text.len() - 1];
    let is_double_quote = raw_string_text.starts_with('"');

    return match is_double_quote {
      true => remove_needless_quote_backslashes(string_value.replace("\\\"", "\"")),
      false => remove_needless_quote_backslashes(string_value.replace("\\'", "'")),
    };

    fn remove_needless_quote_backslashes(text: String) -> String {
      // People may write string literals that look like the following:
      // * "test \' test"
      // * 'test \" test'
      // ...if so, remove these backslashes
      let mut new_string = String::with_capacity(text.len());
      let mut was_last_backslash = false;
      for c in text.chars() {
        if c == '\\' && !was_last_backslash {
          was_last_backslash = true;
        } else {
          if was_last_backslash && c != '\'' && c != '"' {
            new_string.push('\\');
          }
          new_string.push(c);
          was_last_backslash = false;
        }
      }
      if was_last_backslash {
        // backslashes can be the last character in a jsx string literal
        new_string.push('\\');
      }
      new_string
    }
  }

  fn handle_prefer_double(string_value: &str) -> PrintItems {
    if double_to_single(string_value) <= 0 {
      format_with_double(string_value)
    } else {
      format_with_single(string_value)
    }
  }

  fn handle_prefer_single(string_value: &str) -> PrintItems {
    if double_to_single(string_value) >= 0 {
      format_with_single(string_value)
    } else {
      format_with_double(string_value)
    }
  }

  fn format_with_double(string_value: &str) -> PrintItems {
    const DOUBLE_QUOTE_SC: &StringContainer = sc!("\"");
    let mut items = PrintItems::new();
    items.push_sc(DOUBLE_QUOTE_SC);
    items.extend(gen_from_raw_string(&string_value.replace('"', "\\\"")));
    items.push_sc(DOUBLE_QUOTE_SC);
    items
  }

  fn format_with_single(string_value: &str) -> PrintItems {
    const SINGLE_QUOTE_SC: &StringContainer = sc!("'");
    let mut items = PrintItems::new();
    items.push_sc(SINGLE_QUOTE_SC);
    items.extend(gen_from_raw_string(&string_value.replace('\'', "\\'")));
    items.push_sc(SINGLE_QUOTE_SC);
    items
  }

  fn double_to_single(string_value: &str) -> i32 {
    let mut double_count = 0;
    let mut single_count = 0;
    for c in string_value.chars() {
      match c {
        '"' => double_count += 1,
        '\'' => single_count += 1,
        _ => {}
      }
    }

    double_count - single_count
  }
}

/* top level */

// oxc unifies SWC's Module + Script into a single `Program` (with `hashbang`/`directives`/`body`).
fn gen_program_node<'a>(node: &'a Program<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  let mut statements: Vec<Node<'a>> = node.directives.iter().map(Node::Directive).collect();
  statements.extend(node.body.iter().map(stmt_to_node));
  let program_range = node.range();

  if let Some(hashbang) = &node.hashbang {
    items.push_sc(sc!("#!"));
    items.push_string(hashbang.value.as_str().to_string());
    items.push_signal(Signal::ExpectNewLine);
    if let Some(first_statement) = statements.first() {
      if node_helpers::has_separating_blank_line(&program_range.start.range(), first_statement, context.program) {
        items.push_signal(Signal::NewLine);
        items.push_signal(Signal::NewLine);
      }
    } else {
      let shebang_end = hashbang.end();
      if context
        .program
        .comments()
        .iter()
        .find(|comment| comment.start() >= shebang_end)
        .is_some_and(|comment| comment.start_line_fast(context.program) > shebang_end.end_line_fast(context.program) + 1)
      {
        items.push_signal(Signal::NewLine);
        items.push_signal(Signal::NewLine);
      }
      items.extend(gen_trailing_comments_as_statements(&shebang_end.range(), context));
    }
  }

  items.extend(gen_statements(program_range, statements, context));

  items
}

/* patterns */

fn gen_array_pat<'a>(node: &'a ArrayPattern<'a>, context: &mut Context<'a>) -> PrintItems {
  // oxc keeps a destructuring rest element in a dedicated `rest` field rather than as
  // the last element, so append it to the element list here.
  let mut nodes: Vec<Option<Node<'a>>> = node.elements.iter().map(|x| x.as_ref().map(binding_pattern_to_node)).collect();
  if let Some(rest) = &node.rest {
    nodes.push(Some(Node::BindingRestElement(rest)));
  }
  gen_array_like_nodes(
    GenArrayLikeNodesOptions {
      node: Node::ArrayPattern(node),
      nodes,
      prefer_hanging: context.config.array_pattern_prefer_hanging,
      prefer_single_line: context.config.array_pattern_prefer_single_line,
      trailing_commas: if node.rest.is_some() {
        TrailingCommas::Never
      } else {
        context.config.array_pattern_trailing_commas
      },
      space_around: context.config.array_pattern_space_around,
    },
    context,
  )
}

fn gen_assign_pat<'a>(node: &'a AssignmentPattern<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.extend(gen_node(binding_pattern_to_node(&node.left), context));
  items.extend(gen_assignment(expr_to_node(&node.right), sc!("="), context));
  items
}

// oxc unifies SWC's KeyValuePatProp + AssignPatProp into a single `BindingProperty`
// distinguished by `.shorthand` (the value carries any default via an AssignmentPattern).
fn gen_key_value_pat_prop<'a>(node: &'a BindingProperty<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  if node.shorthand {
    // `{ a }` or `{ a = 1 }` - the value holds the binding (and any default)
    items.extend(gen_node(binding_pattern_to_node(&node.value), context));
  } else {
    if node.computed {
      items.extend(gen_computed_prop_like(
        |context| gen_node(prop_key_to_node(&node.key), context),
        GenComputedPropLikeOptions {
          inner_node_range: node.key.range(),
        },
        context,
      ));
    } else {
      items.extend(gen_node(prop_key_to_node(&node.key), context));
    }
    items.extend(gen_assignment(binding_pattern_to_node(&node.value), sc!(":"), context));
  }
  items
}

fn gen_rest_pat<'a>(node: &'a BindingRestElement<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.push_sc(sc!("..."));
  items.extend(gen_node(binding_pattern_to_node(&node.argument), context));
  items
}

fn gen_object_pat<'a>(node: &'a ObjectPattern<'a>, context: &mut Context<'a>) -> PrintItems {
  // oxc stores the destructuring rest separately rather than as the last property.
  let has_rest = node.rest.is_some();
  let mut members: Vec<Node<'a>> = node.properties.iter().map(Node::BindingProperty).collect();
  if let Some(rest) = &node.rest {
    members.push(Node::BindingRestElement(rest));
  }
  gen_object_like_node(
    GenObjectLikeNodeOptions {
      node: Node::ObjectPattern(node),
      members,
      separator: if has_rest {
        TrailingCommas::Never.into()
      } else {
        context.config.object_pattern_trailing_commas.into()
      },
      prefer_hanging: context.config.object_pattern_prefer_hanging,
      prefer_single_line: context.config.object_pattern_prefer_single_line,
      force_single_line: false,
      force_multi_line: is_node_definitely_above_line_width(node.range(), context),
      surround_single_line_with_spaces: context.config.object_pattern_space_surrounding_properties,
      surround_single_line_with_space_at_end: context.config.object_pattern_space_surrounding_properties,
      allow_blank_lines: true,
      node_sorter: None,
    },
    context,
  )
}

fn gen_array_assignment_target<'a>(node: &'a ArrayAssignmentTarget<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut nodes: Vec<Option<Node<'a>>> = node.elements.iter().map(|x| x.as_ref().map(assign_target_maybe_default_to_node)).collect();
  if let Some(rest) = &node.rest {
    nodes.push(Some(Node::AssignmentTargetRest(rest)));
  }
  gen_array_like_nodes(
    GenArrayLikeNodesOptions {
      node: Node::ArrayAssignmentTarget(node),
      nodes,
      prefer_hanging: context.config.array_pattern_prefer_hanging,
      prefer_single_line: context.config.array_pattern_prefer_single_line,
      trailing_commas: if node.rest.is_some() {
        TrailingCommas::Never
      } else {
        context.config.array_pattern_trailing_commas
      },
      space_around: context.config.array_pattern_space_around,
    },
    context,
  )
}

fn gen_object_assignment_target<'a>(node: &'a ObjectAssignmentTarget<'a>, context: &mut Context<'a>) -> PrintItems {
  let has_rest = node.rest.is_some();
  let mut members: Vec<Node<'a>> = node.properties.iter().map(assign_target_property_to_node).collect();
  if let Some(rest) = &node.rest {
    members.push(Node::AssignmentTargetRest(rest));
  }
  gen_object_like_node(
    GenObjectLikeNodeOptions {
      node: Node::ObjectAssignmentTarget(node),
      members,
      separator: if has_rest {
        TrailingCommas::Never.into()
      } else {
        context.config.object_pattern_trailing_commas.into()
      },
      prefer_hanging: context.config.object_pattern_prefer_hanging,
      prefer_single_line: context.config.object_pattern_prefer_single_line,
      force_single_line: false,
      force_multi_line: is_node_definitely_above_line_width(node.range(), context),
      surround_single_line_with_spaces: context.config.object_pattern_space_surrounding_properties,
      surround_single_line_with_space_at_end: context.config.object_pattern_space_surrounding_properties,
      allow_blank_lines: true,
      node_sorter: None,
    },
    context,
  )
}

fn gen_assignment_target_rest<'a>(node: &'a AssignmentTargetRest<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.push_sc(sc!("..."));
  items.extend(gen_node(assign_target_to_node(&node.target), context));
  items
}

fn gen_assignment_target_with_default<'a>(node: &'a AssignmentTargetWithDefault<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.extend(gen_node(assign_target_to_node(&node.binding), context));
  items.extend(gen_assignment(expr_to_node(&node.init), sc!("="), context));
  items
}

fn gen_assignment_target_property_identifier<'a>(node: &'a AssignmentTargetPropertyIdentifier<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.extend(gen_node(Node::IdentifierReference(&node.binding), context));
  if let Some(init) = &node.init {
    items.extend(gen_assignment(expr_to_node(init), sc!("="), context));
  }
  items
}

fn gen_assignment_target_property_property<'a>(node: &'a AssignmentTargetPropertyProperty<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  if node.computed {
    items.extend(gen_computed_prop_like(
      |context| gen_node(prop_key_to_node(&node.name), context),
      GenComputedPropLikeOptions {
        inner_node_range: node.name.range(),
      },
      context,
    ));
  } else {
    items.extend(gen_node(prop_key_to_node(&node.name), context));
  }
  items.extend(gen_assignment(assign_target_maybe_default_to_node(&node.binding), sc!(":"), context));
  items
}

/* properties */

struct ClassOrObjectMethod<'a> {
  node: Node<'a>,
  parameters_range: Option<SourceRange>,
  decorators: Option<&'a [Decorator<'a>]>,
  accessibility: Option<TSAccessibility>,
  is_static: bool,
  is_async: bool,
  is_abstract: bool,
  kind: ClassOrObjectMethodKind,
  is_generator: bool,
  is_optional: bool,
  is_override: bool,
  key: Node<'a>,
  // oxc records computed keys (`[x]() {}`) as a flag rather than a ComputedPropName wrapper.
  key_computed: bool,
  type_params: Option<Node<'a>>,
  params: Vec<Node<'a>>,
  return_type: Option<Node<'a>>,
  body: Option<Node<'a>>,
}

enum ClassOrObjectMethodKind {
  Getter,
  Setter,
  Method,
  Constructor,
}

impl From<MethodDefinitionKind> for ClassOrObjectMethodKind {
  fn from(kind: MethodDefinitionKind) -> ClassOrObjectMethodKind {
    match kind {
      MethodDefinitionKind::Get => ClassOrObjectMethodKind::Getter,
      MethodDefinitionKind::Set => ClassOrObjectMethodKind::Setter,
      MethodDefinitionKind::Method => ClassOrObjectMethodKind::Method,
      MethodDefinitionKind::Constructor => ClassOrObjectMethodKind::Constructor,
    }
  }
}

fn gen_class_or_object_method<'a>(node: ClassOrObjectMethod<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  if let Some(decorators) = node.decorators.as_ref() {
    items.extend(gen_decorators(decorators, false, context));
  }

  let start_header_lsil = LineStartIndentLevel::new("methodStartHeader");
  items.push_info(start_header_lsil);

  if let Some(accessibility) = node.accessibility {
    items.push_string(format!("{} ", accessibility_to_str(accessibility)));
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
  if node.is_async {
    items.push_sc(sc!("async "));
  }

  match node.kind {
    ClassOrObjectMethodKind::Getter => items.push_sc(sc!("get ")),
    ClassOrObjectMethodKind::Setter => items.push_sc(sc!("set ")),
    ClassOrObjectMethodKind::Method | ClassOrObjectMethodKind::Constructor => {}
  }

  if node.is_generator {
    items.push_sc(sc!("*"));
  }
  if node.key_computed {
    items.extend(gen_computed_prop_like(
      |context| gen_node(node.key, context),
      GenComputedPropLikeOptions {
        inner_node_range: node.key.range(),
      },
      context,
    ));
  } else {
    items.extend(gen_quotable_prop(node.key, context));
  }
  if node.is_optional {
    items.push_sc(sc!("?"));
  }
  if let Some(type_params) = node.type_params {
    items.extend(gen_node(type_params, context));
  }
  if get_use_space_before_parens(&node.kind, context) {
    items.push_space()
  }

  let param_count = node.params.len();
  items.extend(gen_parameters_or_arguments(
    GenParametersOrArgumentsOptions {
      node: node.node,
      range: node.parameters_range,
      nodes: node.params,
      custom_close_paren: {
        let return_type = node.return_type;
        move |context| {
          Some(gen_close_paren_with_type(
            GenCloseParenWithTypeOptions {
              start_lsil: start_header_lsil,
              type_node: return_type,
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

  if let Some(body) = node.body {
    let brace_position = get_brace_position(&node.kind, context);
    items.extend(gen_brace_separator(
      GenBraceSeparatorOptions {
        brace_position,
        open_brace_token: context.token_finder.get_first_open_brace_token_within(&body),
        start_header_lsil: Some(start_header_lsil),
      },
      context,
    ));
    items.extend(gen_node(body, context));
  } else if context.config.semi_colons.is_true() {
    items.push_sc(sc!(";"));
  }

  return items;

  fn get_use_space_before_parens(kind: &ClassOrObjectMethodKind, context: &mut Context) -> bool {
    match kind {
      ClassOrObjectMethodKind::Constructor => context.config.constructor_space_before_parentheses,
      ClassOrObjectMethodKind::Getter => context.config.get_accessor_space_before_parentheses,
      ClassOrObjectMethodKind::Setter => context.config.set_accessor_space_before_parentheses,
      ClassOrObjectMethodKind::Method => context.config.method_space_before_parentheses,
    }
  }

  fn get_brace_position(kind: &ClassOrObjectMethodKind, context: &mut Context) -> BracePosition {
    match kind {
      ClassOrObjectMethodKind::Constructor => context.config.constructor_brace_position,
      ClassOrObjectMethodKind::Getter => context.config.get_accessor_brace_position,
      ClassOrObjectMethodKind::Setter => context.config.set_accessor_brace_position,
      ClassOrObjectMethodKind::Method => context.config.method_brace_position,
    }
  }
}

fn accessibility_to_str(accessibility: TSAccessibility) -> &'static str {
  match accessibility {
    TSAccessibility::Private => "private",
    TSAccessibility::Protected => "protected",
    TSAccessibility::Public => "public",
  }
}

/* statements */

fn gen_block_stmt<'a>(node: &'a BlockStatement<'a>, context: &mut Context<'a>) -> PrintItems {
  gen_block(
    |stmts, context| gen_statements(node.get_inner_range(context), stmts, context),
    GenBlockOptions {
      range: Some(node.range()),
      children: node.body.iter().map(stmt_to_node).collect(),
    },
    context,
  )
}

// oxc functions/arrows/methods hold their `{ ... }` block in a FunctionBody
// (directives + statements) rather than a BlockStatement.
fn gen_function_body<'a>(node: &'a FunctionBody<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut children: Vec<Node<'a>> = node.directives.iter().map(Node::Directive).collect();
  children.extend(node.statements.iter().map(stmt_to_node));
  gen_block(
    |stmts, context| gen_statements(node.get_inner_range(context), stmts, context),
    GenBlockOptions {
      range: Some(node.range()),
      children,
    },
    context,
  )
}

fn gen_break_stmt<'a>(node: &'a BreakStatement<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();

  items.push_sc(sc!("break"));
  if let Some(label) = &node.label {
    items.push_space();
    items.extend(gen_node(Node::LabelIdentifier(label), context));
  }
  if context.config.semi_colons.is_true() {
    items.push_sc(sc!(";"));
  }

  items
}

fn gen_continue_stmt<'a>(node: &'a ContinueStatement<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();

  items.push_sc(sc!("continue"));
  if let Some(label) = &node.label {
    items.push_space();
    items.extend(gen_node(Node::LabelIdentifier(label), context));
  }
  if context.config.semi_colons.is_true() {
    items.push_sc(sc!(";"));
  }

  items
}

fn gen_debugger_stmt<'a>(_: &'a DebuggerStatement, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();

  items.push_sc(sc!("debugger"));
  if context.config.semi_colons.is_true() {
    items.push_sc(sc!(";"));
  }

  items
}

fn gen_do_while_stmt<'a>(node: &'a DoWhileStatement<'a>, context: &mut Context<'a>) -> PrintItems {
  // the braces are technically optional on do while statements
  let mut items = PrintItems::new();
  let body_is_block = matches!(node.body, Statement::BlockStatement(_));
  items.push_sc(sc!("do"));
  items.extend(gen_brace_separator(
    GenBraceSeparatorOptions {
      brace_position: context.config.do_while_statement_brace_position,
      open_brace_token: if body_is_block {
        context.token_finder.get_first_open_brace_token_within(node)
      } else {
        None
      },
      start_header_lsil: None,
    },
    context,
  ));
  items.extend(gen_node(stmt_to_node(&node.body), context));
  if context.config.semi_colons.is_true() || body_is_block {
    items.extend(gen_control_flow_separator(
      context.config.do_while_statement_next_control_flow_position,
      &node.body.range(),
      "while",
      None,
      None,
      context,
    ));
  } else {
    // if ASI and the body is not a block, then we just always
    // put this on the next line for simplicity for now
    items.push_signal(Signal::NewLine);
  }
  items.push_sc(sc!("while"));
  if context.config.do_while_statement_space_after_while_keyword {
    items.push_space();
  }
  items.extend(gen_node_in_parens(
    |context| gen_node(expr_to_node(&node.test), context),
    GenNodeInParensOptions {
      inner_range: node.test.range(),
      prefer_hanging: context.config.do_while_statement_prefer_hanging,
      allow_open_paren_trailing_comments: false,
      single_line_space_around: context.config.do_while_statement_space_around,
    },
    context,
  ));
  if context.config.semi_colons.is_true() {
    items.push_sc(sc!(";"));
  }
  items
}

fn gen_export_all<'a>(node: &'a ExportAllDeclaration<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  if node.export_kind.is_type() {
    items.push_sc(sc!("export type * "));
  } else {
    items.push_sc(sc!("export * "));
  }
  // oxc folds `export * as ns from` into ExportAllDeclaration.exported
  if let Some(exported) = &node.exported {
    items.push_sc(sc!("as "));
    items.extend(gen_node(module_export_name_to_node(exported), context));
    items.push_space();
  }
  items.push_sc(sc!("from "));
  items.extend(gen_node(Node::StringLiteral(&node.source), context));

  if let Some(with_clause) = &node.with_clause {
    items.extend(gen_with_clause(with_clause, context));
  }

  if context.config.semi_colons.is_true() {
    items.push_sc(sc!(";"));
  }

  items
}

fn gen_with_clause<'a>(node: &'a WithClause<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.push_sc(if node.keyword == WithClauseKeyword::Assert {
    sc!(" assert ")
  } else {
    sc!(" with ")
  });
  items.extend(gen_object_like_node(
    GenObjectLikeNodeOptions {
      node: Node::WithClause(node),
      members: node.with_entries.iter().map(Node::ImportAttribute).collect(),
      separator: context.config.object_expression_trailing_commas.into(),
      prefer_hanging: context.config.object_expression_prefer_hanging,
      prefer_single_line: context.config.object_expression_prefer_single_line,
      force_single_line: false,
      force_multi_line: false,
      surround_single_line_with_spaces: context.config.object_expression_space_surrounding_properties,
      surround_single_line_with_space_at_end: context.config.object_expression_space_surrounding_properties,
      allow_blank_lines: false,
      node_sorter: None,
    },
    context,
  ));
  items
}

fn gen_import_attribute<'a>(node: &'a ImportAttribute<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  let key = match &node.key {
    ImportAttributeKey::Identifier(ident) => Node::IdentifierName(ident),
    ImportAttributeKey::StringLiteral(s) => Node::StringLiteral(s),
  };
  items.extend(gen_node(key, context));
  items.extend(gen_assignment(Node::StringLiteral(&node.value), sc!(":"), context));
  items
}

fn gen_empty_stmt(_: &EmptyStatement, _: &mut Context) -> PrintItems {
  ";".into()
}

// oxc surfaces prologue directives (e.g. `"use strict";`) as a dedicated `Directive`
// node rather than a string-literal expression statement.
fn gen_directive<'a>(node: &'a Directive<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.extend(gen_string_literal(&node.expression, context));
  if context.config.semi_colons.is_true() {
    items.push_sc(sc!(";"));
  }
  items
}

fn gen_export_assignment<'a>(node: &'a TSExportAssignment<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();

  items.push_sc(sc!("export"));
  items.extend(gen_assignment(expr_to_node(&node.expression), sc!("="), context));
  if context.config.semi_colons.is_true() {
    items.push_sc(sc!(";"));
  }

  items
}

fn gen_namespace_export<'a>(node: &'a TSNamespaceExportDeclaration<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.push_sc(sc!("export as namespace "));
  items.extend(gen_node(Node::IdentifierName(&node.id), context));

  if context.config.semi_colons.is_true() {
    items.push_sc(sc!(";"));
  }

  items
}

fn gen_expr_stmt<'a>(stmt: &'a ExpressionStatement<'a>, context: &mut Context<'a>) -> PrintItems {
  if context.config.semi_colons.is_true() || matches!(context.parent(), Node::DoWhileStatement(_)) {
    return gen_inner(stmt, context);
  } else {
    return gen_for_prefix_semi_colon_insertion(stmt, context);
  }

  fn gen_inner<'a>(stmt: &'a ExpressionStatement<'a>, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    items.extend(gen_node(expr_to_node(&stmt.expression), context));
    if context.config.semi_colons.is_true() {
      items.push_sc(sc!(";"));
    }
    items
  }

  fn gen_for_prefix_semi_colon_insertion<'a>(stmt: &'a ExpressionStatement<'a>, context: &mut Context<'a>) -> PrintItems {
    let generated_node = gen_inner(stmt, context);
    let generated_node = generated_node.into_rc_path();
    let brace_condition_ref = context.take_expr_stmt_single_line_parent_brace_ref(); // always clear this
    return if should_add_semi_colon(&generated_node).unwrap_or(false) {
      let mut items = PrintItems::new();
      if let Some(brace_condition_ref) = brace_condition_ref {
        // Do not add a semi-colon when the semi-colon is within an if stmt or for-like stmt where
        // there are no braces on the parent (ex. `if (true) []`) as this would break the code.
        items.push_condition(if_true(
          "semiColonIfBrace",
          Rc::new(move |context| context.resolved_condition(&brace_condition_ref)),
          ";".into(),
        ));
      } else {
        items.push_sc(sc!(";"));
      }
      items.extend(generated_node.into());
      items
    } else {
      generated_node.into()
    };

    fn should_add_semi_colon(path: &Option<PrintItemPath>) -> Option<bool> {
      // todo: this needs to be improved
      if let Some(path) = path {
        for item in PrintItemsIterator::new(path) {
          match item {
            PrintItem::String(value) => {
              if value.text.starts_with("++") || value.text.starts_with("--") {
                return Some(false); // don't need to add for increment/decrement
              }
              if let Some(c) = value.text.chars().next() {
                return utils::is_prefix_semi_colon_insertion_char(c).into();
              }
            }
            PrintItem::Condition(condition) => {
              // It's an assumption here that the true and false paths of the
              // condition will both contain the same text to look for. This is probably not robust
              // and perhaps instead there should be a way to do something like "get the next character" in
              // the printer.
              if let Some(result) = should_add_semi_colon(condition.true_path()) {
                return Some(result);
              }
              if let Some(result) = should_add_semi_colon(condition.false_path()) {
                return Some(result);
              }
            }
            PrintItem::RcPath(items) => {
              if let Some(result) = should_add_semi_colon(&Some(items)) {
                return Some(result);
              }
            }
            _ => { /* do nothing */ }
          }
        }
      }

      None
    }
  }
}

fn gen_for_stmt<'a>(node: &'a ForStatement<'a>, context: &mut Context<'a>) -> PrintItems {
  let start_header_ln = LineNumber::new("startHeader");
  let start_header_lsil = LineStartIndentLevel::new("startHeader");
  let end_header_ln = LineNumber::new("endHeader");
  let first_inner_node = {
    if let Some(init) = &node.init {
      init.range()
    } else {
      node
        .tokens_fast(context.program)
        .iter()
        .find(|t| t.kind() == Kind::Semicolon)
        .expect("Expected to find a semi-colon in for stmt.")
        .range()
    }
  };
  let last_inner_node = {
    if let Some(update) = &node.update {
      update.range()
    } else if let Some(test) = &node.test {
      context
        .token_finder
        .get_first_semi_colon_after(&test.range())
        .expect("Expected to find second semi-colon in for stmt.")
        .range()
    } else if let Some(init) = &node.init {
      let first_semi_colon = context
        .token_finder
        .get_first_semi_colon_after(init)
        .expect("Expected to find a semi-colon in for stmt.");
      context
        .token_finder
        .get_first_semi_colon_after(&first_semi_colon.range())
        .expect("Expected to find second semi-colon in for stmt.")
        .range()
    } else {
      context
        .token_finder
        .get_first_semi_colon_after(&first_inner_node)
        .expect("Expected to find second semi-colon in for stmt.")
        .range()
    }
  };
  let force_use_new_lines = get_use_new_lines(&first_inner_node, context);
  let mut items = PrintItems::new();
  items.push_info(start_header_ln);
  items.push_info(start_header_lsil);
  items.push_sc(sc!("for"));
  if context.config.for_statement_space_after_for_keyword {
    items.push_space();
  }

  let separator_after_semi_colons = if context.config.for_statement_space_after_semi_colons {
    Signal::SpaceOrNewLine
  } else {
    Signal::PossibleNewLine
  };
  let generated_init = ir_helpers::new_line_group({
    let mut items = PrintItems::new();
    if let Some(init) = &node.init {
      items.extend(gen_node(for_stmt_init_to_node(init), context));
    }
    items.push_sc(sc!(";"));
    if node.test.is_none() {
      items.push_sc(sc!(";"));
    }
    items
  });
  let generated_test = node.test.as_ref().map(|test| {
    ir_helpers::new_line_group({
      let mut items = PrintItems::new();
      items.extend(gen_node(expr_to_node(test), context));
      items.push_sc(sc!(";"));
      items
    })
  });
  let generated_update = node
    .update
    .as_ref()
    .map(|update| ir_helpers::new_line_group(gen_node(expr_to_node(update), context)));

  items.extend(gen_node_in_parens(
    |context| {
      ir_helpers::gen_separated_values(
        move |_| {
          let mut generated_nodes = Vec::new();
          generated_nodes.push(ir_helpers::GeneratedValue::from_items(generated_init));
          if let Some(generated_test) = generated_test {
            generated_nodes.push(ir_helpers::GeneratedValue::from_items(generated_test));
          }
          if let Some(generated_update) = generated_update {
            generated_nodes.push(ir_helpers::GeneratedValue::from_items(generated_update));
          }
          generated_nodes
        },
        ir_helpers::GenSeparatedValuesOptions {
          prefer_hanging: context.config.for_statement_prefer_hanging,
          force_use_new_lines,
          allow_blank_lines: false,
          indent_width: context.config.indent_width,
          single_line_options: ir_helpers::SingleLineOptions::separated_same_line(separator_after_semi_colons.into()),
          multi_line_options: ir_helpers::MultiLineOptions::same_line_no_indent(),
          force_possible_newline_at_start: false,
        },
      )
      .items
    },
    GenNodeInParensOptions {
      inner_range: SourceRange::new(first_inner_node.start(), last_inner_node.end()),
      prefer_hanging: context.config.for_statement_prefer_hanging,
      allow_open_paren_trailing_comments: false,
      single_line_space_around: context.config.for_statement_space_around,
    },
    context,
  ));

  items.push_info(end_header_ln);

  items.extend(
    gen_conditional_brace_body(
      GenConditionalBraceBodyOptions {
        body_node: stmt_to_node(&node.body),
        use_braces: context.config.for_statement_use_braces,
        brace_position: context.config.for_statement_brace_position,
        single_body_position: Some(context.config.for_statement_single_body_position),
        requires_braces_condition_ref: None,
        start_header_info: Some((start_header_ln, start_header_lsil)),
        end_header_info: Some(end_header_ln),
      },
      context,
    )
    .generated_node,
  );

  return items;

  fn get_use_new_lines<'a>(node: &SourceRange, context: &mut Context<'a>) -> bool {
    if context.config.for_statement_prefer_single_line {
      return false;
    }

    let open_paren_token = context.token_finder.get_previous_token_if_open_paren(node);
    if let Some(open_paren_token) = open_paren_token {
      node_helpers::get_use_new_lines_for_nodes(&open_paren_token.range(), node, context.program)
    } else {
      false
    }
  }
}

fn gen_for_in_stmt<'a>(node: &'a ForInStatement<'a>, context: &mut Context<'a>) -> PrintItems {
  let start_header_ln = LineNumber::new("startHeader");
  let start_header_lsil = LineStartIndentLevel::new("startHeader");
  let end_header_ln = LineNumber::new("endHeader");
  let mut items = PrintItems::new();
  items.push_info(start_header_ln);
  items.push_info(start_header_lsil);
  items.push_sc(sc!("for"));
  if context.config.for_in_statement_space_after_for_keyword {
    items.push_space();
  }
  let inner_header_range = SourceRange::new(node.left.start(), node.right.end());
  items.extend(gen_node_in_parens(
    |context| {
      let mut items = PrintItems::new();
      items.extend(gen_node(for_stmt_left_to_node(&node.left), context));
      items.push_signal(Signal::SpaceOrNewLine);
      items.push_condition(conditions::indent_if_start_of_line({
        let mut items = PrintItems::new();
        items.push_sc(sc!("in "));
        items.extend(gen_node(expr_to_node(&node.right), context));
        items
      }));
      items
    },
    GenNodeInParensOptions {
      inner_range: inner_header_range,
      prefer_hanging: context.config.for_in_statement_prefer_hanging,
      allow_open_paren_trailing_comments: false,
      single_line_space_around: context.config.for_in_statement_space_around,
    },
    context,
  ));
  items.push_info(end_header_ln);

  items.extend(
    gen_conditional_brace_body(
      GenConditionalBraceBodyOptions {
        body_node: stmt_to_node(&node.body),
        use_braces: context.config.for_in_statement_use_braces,
        brace_position: context.config.for_in_statement_brace_position,
        single_body_position: Some(context.config.for_in_statement_single_body_position),
        requires_braces_condition_ref: None,
        start_header_info: Some((start_header_ln, start_header_lsil)),
        end_header_info: Some(end_header_ln),
      },
      context,
    )
    .generated_node,
  );

  items
}

fn gen_for_of_stmt<'a>(node: &'a ForOfStatement<'a>, context: &mut Context<'a>) -> PrintItems {
  let start_header_ln = LineNumber::new("startHeader");
  let start_header_lsil = LineStartIndentLevel::new("startHeader");
  let end_header_ln = LineNumber::new("endHeader");
  let mut items = PrintItems::new();
  items.push_info(start_header_ln);
  items.push_info(start_header_lsil);
  items.push_sc(sc!("for"));
  if context.config.for_of_statement_space_after_for_keyword {
    items.push_space();
  }
  if node.r#await {
    // todo: generate comments around await token range
    items.push_sc(sc!("await "));
  }
  let inner_header_range = SourceRange::new(node.left.start(), node.right.end());
  items.extend(gen_node_in_parens(
    |context| {
      let mut items = PrintItems::new();
      items.extend(gen_node(for_stmt_left_to_node(&node.left), context));
      items.push_signal(Signal::SpaceOrNewLine);
      items.push_condition(conditions::indent_if_start_of_line({
        let mut items = PrintItems::new();
        items.push_sc(sc!("of "));
        items.extend(gen_node(expr_to_node(&node.right), context));
        items
      }));
      items
    },
    GenNodeInParensOptions {
      inner_range: inner_header_range,
      prefer_hanging: context.config.for_of_statement_prefer_hanging,
      allow_open_paren_trailing_comments: false,
      single_line_space_around: context.config.for_of_statement_space_around,
    },
    context,
  ));
  items.push_info(end_header_ln);

  items.extend(
    gen_conditional_brace_body(
      GenConditionalBraceBodyOptions {
        body_node: stmt_to_node(&node.body),
        use_braces: context.config.for_of_statement_use_braces,
        brace_position: context.config.for_of_statement_brace_position,
        single_body_position: Some(context.config.for_of_statement_single_body_position),
        requires_braces_condition_ref: None,
        start_header_info: Some((start_header_ln, start_header_lsil)),
        end_header_info: Some(end_header_ln),
      },
      context,
    )
    .generated_node,
  );

  items
}

fn gen_if_stmt<'a>(node: &'a IfStatement<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  let cons_range = node.consequent.range();
  let result = gen_header_with_conditional_brace_body(
    GenHeaderWithConditionalBraceBodyOptions {
      body_node: stmt_to_node(&node.consequent),
      generated_header: {
        let mut items = PrintItems::new();
        items.push_sc(sc!("if"));
        if context.config.if_statement_space_after_if_keyword {
          items.push_space();
        }
        items.extend(gen_node_in_parens(
          |context| gen_node(expr_to_node(&node.test), context),
          GenNodeInParensOptions {
            inner_range: node.test.range(),
            prefer_hanging: context.config.if_statement_prefer_hanging,
            allow_open_paren_trailing_comments: false,
            single_line_space_around: context.config.if_statement_space_around,
          },
          context,
        ));
        items
      },
      use_braces: context.config.if_statement_use_braces,
      brace_position: context.config.if_statement_brace_position,
      single_body_position: Some(context.config.if_statement_single_body_position),
      requires_braces_condition_ref: context.take_if_stmt_last_brace_condition_ref(),
    },
    context,
  );
  let if_stmt_start_ln = LineNumber::new("ifStmtStart");

  items.push_info(if_stmt_start_ln);
  items.extend(result.generated_node);

  if let Some(alt) = &node.alternate {
    if let Statement::IfStatement(alt_alt) = alt {
      if alt_alt.alternate.is_none() {
        context.store_if_stmt_last_brace_condition_ref(result.open_brace_condition_ref);
      }
    }

    items.extend(gen_control_flow_separator(
      context.config.if_statement_next_control_flow_position,
      &cons_range,
      "else",
      Some(if_stmt_start_ln),
      Some(result.close_brace_condition_ref),
      context,
    ));

    // generate the leading comments before the else keyword (the first `else` token
    // after the consequent; oxc has no children-with-tokens API)
    let else_keyword = node
      .tokens_fast(context.program)
      .iter()
      .find(|t| t.start() >= cons_range.end() && t.text_fast(context.program) == "else")
      .expect("Expected to find an else keyword.");
    items.extend(gen_leading_comments(&else_keyword.range(), context));
    items.extend(gen_leading_comments(&alt.range(), context));

    let start_else_header_ln = LineNumber::new("startElseHeader");
    let start_else_header_lsil = LineStartIndentLevel::new("startElseHeader");
    items.push_info(start_else_header_ln);
    items.push_info(start_else_header_lsil);
    items.push_sc(sc!("else"));

    if let Statement::IfStatement(alt) = alt {
      items.push_space();
      items.extend(gen_node(Node::IfStatement(alt), context));
    } else {
      items.extend(
        gen_conditional_brace_body(
          GenConditionalBraceBodyOptions {
            body_node: stmt_to_node(alt),
            use_braces: context.config.if_statement_use_braces,
            brace_position: context.config.if_statement_brace_position,
            single_body_position: Some(context.config.if_statement_single_body_position),
            requires_braces_condition_ref: Some(result.open_brace_condition_ref),
            start_header_info: Some((start_else_header_ln, start_else_header_lsil)),
            end_header_info: None,
          },
          context,
        )
        .generated_node,
      );
    }
  }

  items
}

fn gen_labeled_stmt<'a>(node: &'a LabeledStatement<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.extend(gen_node(Node::LabelIdentifier(&node.label), context));
  items.push_sc(sc!(":"));

  // not bothering to make this configurable, because who uses labeled statements?
  // edit: ok, maybe I will make this configurable in the future
  let is_inner_stmt_same_line = node.start_line_fast(context.program) == node.body.start_line_fast(context.program);
  if is_inner_stmt_same_line {
    items.push_space();
  } else {
    items.push_signal(Signal::NewLine);
  }

  items.extend(gen_node(stmt_to_node(&node.body), context));

  items
}

fn gen_return_stmt<'a>(node: &'a ReturnStatement<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.push_sc(sc!("return"));
  if let Some(arg) = &node.argument {
    items.push_space();
    items.extend(gen_node(expr_to_node(arg), context));
  }
  if context.config.semi_colons.is_true() {
    items.push_sc(sc!(";"));
  }
  items
}

fn gen_switch_stmt<'a>(node: &'a SwitchStatement<'a>, context: &mut Context<'a>) -> PrintItems {
  let start_header_lsil = LineStartIndentLevel::new("startHeader");
  let mut items = PrintItems::new();
  items.push_info(start_header_lsil);
  items.push_sc(sc!("switch "));
  items.extend(gen_node_in_parens(
    |context| gen_node(expr_to_node(&node.discriminant), context),
    GenNodeInParensOptions {
      inner_range: node.discriminant.range(),
      prefer_hanging: context.config.switch_statement_prefer_hanging,
      allow_open_paren_trailing_comments: false,
      single_line_space_around: context.config.switch_statement_space_around,
    },
    context,
  ));
  items.extend(gen_membered_body(
    GenMemberedBodyOptions {
      node: Node::SwitchStatement(node),
      members: node.cases.iter().map(Node::SwitchCase).collect(),
      start_header_lsil: Some(start_header_lsil),
      brace_position: context.config.switch_statement_brace_position,
      should_use_blank_line: |previous, next, context| {
        // do not put a blank line when the previous case has no body
        if let Node::SwitchCase(previous) = previous {
          if previous.consequent.is_empty() {
            return false;
          }
        }

        // Switch cases have custom rules for where the comments end up (based on their indentation),
        // so for here just check that there is no blank line between either the previous comment
        // or previous node
        if let Some(leading_comment) = next.leading_comments_fast(context.program).next() {
          previous.end_line_fast(context.program) + 1 < leading_comment.start_line_fast(context.program)
        } else {
          previous.end_line_fast(context.program) + 1 < next.start_line_fast(context.program)
        }
      },
      separator: Separator::none(),
    },
    context,
  ));
  items
}

fn gen_switch_case<'a>(node: &'a SwitchCase<'a>, context: &mut Context<'a>) -> PrintItems {
  let block_stmt_body = get_block_stmt_body(node);
  let mut items = PrintItems::new();
  let colon_token = context
    .token_finder
    .get_first_colon_token_after(&(if let Some(test) = &node.test { test.end() } else { node.start() }).range())
    .expect("Expected to find a colon token.");

  if let Some(test) = &node.test {
    items.push_sc(sc!("case "));
    items.extend(gen_node(expr_to_node(test), context));
    items.push_sc(sc!(":"));
  } else {
    items.push_sc(sc!("default:"));
  }

  items.extend(gen_trailing_comments_same_line(&colon_token.range(), context));
  let generated_trailing_comments = gen_trailing_comments_for_case(node, &block_stmt_body, context);
  if !node.consequent.is_empty() {
    if let Some(block_stmt_body) = block_stmt_body {
      items.extend(gen_brace_separator(
        GenBraceSeparatorOptions {
          brace_position: context.config.switch_case_brace_position,
          open_brace_token: context.token_finder.get_first_open_brace_token_within(&block_stmt_body),
          start_header_lsil: None,
        },
        context,
      ));
      items.extend(gen_node(stmt_to_node(node.consequent.first().unwrap()), context));
    } else {
      items.push_signal(Signal::NewLine);
      items.extend(ir_helpers::with_indent(gen_statements(
        SourceRange::new(colon_token.end(), node.end()),
        node.consequent.iter().map(stmt_to_node).collect(),
        context,
      )));
    }
  }

  items.extend(generated_trailing_comments);

  return items;

  fn get_block_stmt_body(node: &SwitchCase) -> Option<SourceRange> {
    if node.consequent.len() == 1 {
      if let Some(Statement::BlockStatement(block_stmt)) = node.consequent.first() {
        return Some(block_stmt.range());
      }
    }
    None
  }

  fn gen_trailing_comments_for_case<'a>(node: &'a SwitchCase<'a>, block_stmt_body: &Option<SourceRange>, context: &mut Context<'a>) -> PrintItems {
    let node_range = node.range();
    let mut items = PrintItems::new();
    // generate the trailing comments as statements
    let trailing_comments = get_trailing_comments_as_statements(&node_range, context);
    if !trailing_comments.is_empty() {
      let last_case = match context.parent() {
        Node::SwitchStatement(sw) => sw.cases.last(),
        _ => None,
      };
      let is_last_case = match last_case {
        Some(last_case) => last_case.start() == node_range.start,
        _ => false,
      };
      let mut is_equal_indent = block_stmt_body.is_some();
      let mut last_range = node_range;
      let last_node_column = node_range.start_column_fast(context.program);

      for comment in trailing_comments {
        is_equal_indent = is_equal_indent || comment.start_column_fast(context.program) <= last_node_column;
        let generated_comment = gen_comment_based_on_last_node(
          comment,
          &Some(last_range),
          GenCommentBasedOnLastNodeOptions { separate_with_newlines: true },
          context,
        );

        items.extend(if !is_last_case && is_equal_indent {
          generated_comment
        } else {
          ir_helpers::with_indent(generated_comment)
        });
        last_range = comment.range();
      }
    }
    items
  }
}

fn gen_throw_stmt<'a>(node: &'a ThrowStatement<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.push_sc(sc!("throw "));
  items.extend(gen_node(expr_to_node(&node.argument), context));
  if context.config.semi_colons.is_true() {
    items.push_sc(sc!(";"));
  }
  items
}

fn gen_try_stmt<'a>(node: &'a TryStatement<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  let brace_position = context.config.try_statement_brace_position;
  let next_control_flow_position = context.config.try_statement_next_control_flow_position;
  let mut last_block_range = node.block.range();
  let mut last_block_start_ln = LineNumber::new("tryStart");

  items.push_info(last_block_start_ln);
  items.push_sc(sc!("try"));

  items.extend(
    gen_conditional_brace_body(
      GenConditionalBraceBodyOptions {
        body_node: Node::BlockStatement(&node.block),
        use_braces: UseBraces::Always, // braces required
        brace_position: context.config.try_statement_brace_position,
        single_body_position: Some(SameOrNextLinePosition::NextLine),
        requires_braces_condition_ref: None,
        start_header_info: None,
        end_header_info: None,
      },
      context,
    )
    .generated_node,
  );

  if let Some(handler) = &node.handler {
    let handler_start_ln = LineNumber::new("handlerStart");
    items.push_info(handler_start_ln);
    items.extend(gen_control_flow_separator(
      next_control_flow_position,
      &last_block_range,
      "catch",
      Some(last_block_start_ln),
      None,
      context,
    ));
    last_block_range = handler.range();
    items.extend(gen_node(Node::CatchClause(handler), context));

    // set the next block to check the handler start info
    last_block_start_ln = handler_start_ln;
  }

  if let Some(finalizer) = &node.finalizer {
    items.extend(gen_control_flow_separator(
      next_control_flow_position,
      &last_block_range,
      "finally",
      Some(last_block_start_ln),
      None,
      context,
    ));
    items.push_sc(sc!("finally"));
    items.extend(
      gen_conditional_brace_body(
        GenConditionalBraceBodyOptions {
          body_node: Node::BlockStatement(finalizer),
          use_braces: UseBraces::Always, // braces required
          brace_position,
          single_body_position: Some(SameOrNextLinePosition::NextLine),
          requires_braces_condition_ref: None,
          start_header_info: None,
          end_header_info: None,
        },
        context,
      )
      .generated_node,
    );
  }

  items
}

fn gen_var_decl<'a>(node: &'a VariableDeclaration<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  if node.declare {
    items.push_sc(sc!("declare "));
  }
  // oxc folds `using` / `await using` declarations into VariableDeclaration.
  items.push_sc(match node.kind {
    VariableDeclarationKind::Const => sc!("const "),
    VariableDeclarationKind::Let => sc!("let "),
    VariableDeclarationKind::Var => sc!("var "),
    VariableDeclarationKind::Using => sc!("using "),
    VariableDeclarationKind::AwaitUsing => sc!("await using "),
  });

  items.extend(gen_var_declarators(Node::VariableDeclaration(node), &node.declarations, context));

  if requires_semi_colon_for_var_or_using_decl(Node::VariableDeclaration(node), context) {
    items.push_sc(sc!(";"));
  }

  items
}

fn requires_semi_colon_for_var_or_using_decl(node: Node, context: &mut Context) -> bool {
  let use_semi_colons = context.config.semi_colons.is_true();
  use_semi_colons
    && match context.parent() {
      Node::ForInStatement(parent) => node.start() >= parent.body.start(),
      Node::ForOfStatement(parent) => node.start() >= parent.body.start(),
      Node::ForStatement(parent) => node.start() >= parent.body.start(),
      _ => use_semi_colons,
    }
}

fn gen_var_declarators<'a>(parent: Node<'a>, decls: &'a [VariableDeclarator<'a>], context: &mut Context<'a>) -> PrintItems {
  fn get_use_new_lines<'a>(parent: Node<'a>, decls: &'a [VariableDeclarator<'a>], context: &mut Context<'a>) -> bool {
    if get_use_new_lines_for_nodes(decls, context.config.variable_statement_prefer_single_line, context) {
      true
    } else {
      // probably minified code
      decls.len() >= 2 && is_node_definitely_above_line_width(parent.range(), context)
    }
  }

  let decls_len = decls.len();
  if decls_len == 1 {
    // be lightweight by default
    gen_node(Node::VariableDeclarator(&decls[0]), context)
  } else if decls_len > 1 {
    let force_use_new_lines = get_use_new_lines(parent, decls, context);
    gen_separated_values(
      GenSeparatedValuesParams {
        nodes: decls.iter().map(|p| NodeOrSeparator::Node(Node::VariableDeclarator(p))).collect(),
        prefer_hanging: context.config.variable_statement_prefer_hanging,
        force_use_new_lines,
        allow_blank_lines: false,
        separator: TrailingCommas::Never.into(),
        single_line_options: ir_helpers::SingleLineOptions::same_line_maybe_space_separated(),
        multi_line_options: ir_helpers::MultiLineOptions::same_line_start_hanging_indent(),
        force_possible_newline_at_start: false,
        node_sorter: None,
      },
      context,
    )
  } else {
    PrintItems::new()
  }
}

fn gen_var_declarator<'a>(node: &'a VariableDeclarator<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();

  // In oxc the binding's type annotation / definite marker live on the declarator
  // rather than the pattern, so emit them here.
  items.extend(gen_node(binding_pattern_to_node(&node.id), context));
  if node.definite {
    items.push_sc(sc!("!"));
  }
  items.extend(gen_type_ann_with_colon_if_exists(node.type_annotation.as_deref(), context));

  if let Some(init) = &node.init {
    items.extend(gen_assignment(expr_to_node(init), sc!("="), context));
  }

  // Indent the first variable declarator when there are multiple.
  // Not ideal, but doing this here because of the abstraction used in
  // `gen_var_decl`. In the future this should probably be moved away.

  let parent_decls = match context.parent() {
    Node::VariableDeclaration(parent) => Some(&parent.declarations),
    _ => None,
  };
  if let Some(var_decls) = parent_decls {
    if var_decls.len() > 1 && var_decls[0].range() == node.range() {
      let items = items.into_rc_path();
      if_true_or(
        "indentIfNotStartOfLine",
        condition_resolvers::is_not_start_of_line(),
        with_indent(items.into()),
        items.into(),
      )
      .into()
    } else {
      items
    }
  } else {
    items
  }
}

fn gen_while_stmt<'a>(node: &'a WhileStatement<'a>, context: &mut Context<'a>) -> PrintItems {
  let start_header_ln = LineNumber::new("startHeader");
  let start_header_lsil = LineStartIndentLevel::new("startHeader");
  let end_header_ln = LineNumber::new("endHeader");
  let mut items = PrintItems::new();
  items.push_info(start_header_ln);
  items.push_info(start_header_lsil);
  items.push_sc(sc!("while"));
  if context.config.while_statement_space_after_while_keyword {
    items.push_space();
  }
  items.extend(gen_node_in_parens(
    |context| gen_node(expr_to_node(&node.test), context),
    GenNodeInParensOptions {
      inner_range: node.test.range(),
      prefer_hanging: context.config.while_statement_prefer_hanging,
      allow_open_paren_trailing_comments: false,
      single_line_space_around: context.config.while_statement_space_around,
    },
    context,
  ));
  items.push_info(end_header_ln);
  items.extend(
    gen_conditional_brace_body(
      GenConditionalBraceBodyOptions {
        body_node: stmt_to_node(&node.body),
        use_braces: context.config.while_statement_use_braces,
        brace_position: context.config.while_statement_brace_position,
        single_body_position: Some(context.config.while_statement_single_body_position),
        requires_braces_condition_ref: None,
        start_header_info: Some((start_header_ln, start_header_lsil)),
        end_header_info: Some(end_header_ln),
      },
      context,
    )
    .generated_node,
  );
  items
}

/* types */

fn gen_array_type<'a>(node: &'a TSArrayType<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.extend(gen_node(ts_type_to_node(&node.element_type), context));
  items.push_sc(sc!("[]"));
  items
}

fn gen_conditional_type<'a>(node: &'a TSConditionalType<'a>, context: &mut Context<'a>) -> PrintItems {
  let use_new_lines =
    !context.config.conditional_type_prefer_single_line && node_helpers::get_use_new_lines_for_nodes(&node.true_type, &node.false_type, context.program);
  let top_most_data = get_top_most_data(node, context);
  let is_parent_conditional_type = matches!(context.parent(), Node::TSConditionalType(_));
  let mut items = PrintItems::new();
  let before_false_ln = LineNumber::new("beforeFalse");
  let question_token = context.token_finder.get_first_operator_after(&node.extends_type, "?").unwrap();
  let colon_token = context.token_finder.get_first_operator_after(&node.true_type, ":").unwrap();
  let (question_position, colon_position) = get_operator_position(node, question_token, colon_token, context);
  let question_comment_items = gen_cond_token_comments(question_token, context, top_most_data.il);
  let colon_comment_items = gen_cond_token_comments(colon_token, context, top_most_data.il);

  // main area
  items.extend(ir_helpers::new_line_group(gen_node(ts_type_to_node(&node.check_type), context)));
  items.push_sc(sc!(" extends")); // do not newline before because it's a parsing error
  items.push_signal(Signal::SpaceOrNewLine);

  if top_most_data.is_top_most {
    items.push_info(top_most_data.ln);
    items.push_info(top_most_data.il);
  }

  items.push_condition(conditions::indent_if_start_of_line(ir_helpers::new_line_group({
    let mut items = gen_node(ts_type_to_node(&node.extends_type), context);
    if question_position == OperatorPosition::SameLine {
      items.push_sc(sc!(" ?"));
    }
    items.extend(question_comment_items.trailing_line);
    items
  })));

  if question_comment_items.previous_lines.is_empty() {
    items.push_signal(Signal::SpaceOrNewLine);
  } else {
    items.extend(question_comment_items.previous_lines);
  }

  items.push_condition({
    let inner_items = {
      let mut items = PrintItems::new();
      items.extend(question_comment_items.leading_line);
      if question_position == OperatorPosition::NextLine {
        items.push_sc(sc!("?"));
        items.push_signal(Signal::SpaceIfNotTrailing);
      }
      items.extend(ir_helpers::new_line_group(gen_node(ts_type_to_node(&node.true_type), context)));
      if colon_position == OperatorPosition::SameLine {
        items.push_sc(sc!(" :"));
      }
      items.extend(colon_comment_items.trailing_line);
      items
    }
    .into_rc_path();
    if_true_or(
      "isStartOfLineIndentElseQueue",
      condition_resolvers::is_start_of_line(),
      with_indent(inner_items.into()),
      with_queued_indent(inner_items.into()),
    )
  });

  items.extend(colon_comment_items.previous_lines);

  // false type
  if use_new_lines {
    items.push_signal(Signal::NewLine);
  } else {
    items.push_condition(conditions::new_line_if_multiple_lines_space_or_new_line_otherwise(
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
    let mut items = PrintItems::new();
    items.push_info(before_false_ln);
    items.extend(colon_comment_items.leading_line);
    if colon_position == OperatorPosition::NextLine {
      items.push_sc(sc!(":"));
      items.push_signal(Signal::SpaceIfNotTrailing);
    }
    items.extend(ir_helpers::new_line_group(gen_node(ts_type_to_node(&node.false_type), context)));
    items
  };

  if is_parent_conditional_type {
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

    fn get_or_set_top_most_ln(top_most_expr_start: SourcePos, is_top_most: bool, context: &mut Context) -> (LineNumber, IndentLevel) {
      if is_top_most {
        let ln = LineNumber::new("conditionalTypeStart");
        let il = IndentLevel::new("conditionalTypeStart");
        context.store_ln_for_node(&top_most_expr_start.range(), ln);
        context.store_il_for_node(&top_most_expr_start.range(), il);
        (ln, il)
      } else {
        (
          context.get_ln_for_node(&top_most_expr_start.range()).unwrap(),
          context.get_il_for_node(&top_most_expr_start.range()).unwrap(),
        )
      }
    }
  }

  fn get_operator_position(
    node: &TSConditionalType,
    question_token: &Token,
    colon_token: &Token,
    context: &mut Context,
  ) -> (OperatorPosition, OperatorPosition) {
    fn get_maintain_position(node: &TSConditionalType, ts_type: &TSType, token: &Token, context: &mut Context) -> OperatorPosition {
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

fn gen_constructor_type<'a>(node: &'a TSConstructorType<'a>, context: &mut Context<'a>) -> PrintItems {
  let start_lsil = LineStartIndentLevel::new("startConstructorType");
  let mut items = PrintItems::new();
  items.push_info(start_lsil);
  if node.r#abstract {
    items.push_sc(sc!("abstract "));
  }
  items.push_sc(sc!("new"));
  if context.config.constructor_type_space_after_new_keyword {
    items.push_space();
  }
  if let Some(type_params) = &node.type_parameters {
    items.extend(gen_node(Node::TSTypeParameterDeclaration(type_params), context));
  }

  let param_nodes = formal_params_to_nodes(&node.params);
  let param_count = param_nodes.len();
  items.extend(gen_parameters_or_arguments(
    GenParametersOrArgumentsOptions {
      node: Node::TSConstructorType(node),
      range: node.get_parameters_range(context),
      nodes: param_nodes,
      custom_close_paren: |context| {
        Some(gen_close_paren_with_type(
          GenCloseParenWithTypeOptions {
            start_lsil,
            type_node: Some(Node::TSTypeAnnotation(&node.return_type)),
            type_node_separator: Some({
              let mut items = PrintItems::new();
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

fn gen_function_type<'a>(node: &'a TSFunctionType<'a>, context: &mut Context<'a>) -> PrintItems {
  let start_lsil = LineStartIndentLevel::new("startFunctionType");
  let mut items = PrintItems::new();
  let mut indent_after_arrow_condition = if_true(
    "indentIfIsStartOfLineAfterArrow",
    condition_resolvers::is_start_of_line(),
    Signal::StartIndent.into(),
  );
  let indent_after_arrow_condition_ref = indent_after_arrow_condition.create_reference();

  items.push_info(start_lsil);
  if let Some(type_params) = &node.type_parameters {
    items.extend(gen_node(Node::TSTypeParameterDeclaration(type_params), context));
  }
  let param_nodes = formal_params_to_nodes(&node.params);
  let param_count = param_nodes.len();
  items.extend(gen_parameters_or_arguments(
    GenParametersOrArgumentsOptions {
      node: Node::TSFunctionType(node),
      range: node.get_parameters_range(context),
      nodes: param_nodes,
      custom_close_paren: |context| {
        Some(gen_close_paren_with_type(
          GenCloseParenWithTypeOptions {
            start_lsil,
            type_node: Some(Node::TSTypeAnnotation(&node.return_type)),
            type_node_separator: {
              let mut items = PrintItems::new();
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
    Rc::new(move |context| context.resolved_condition(&indent_after_arrow_condition_ref)),
    Signal::FinishIndent.into(),
  ));

  items
}

// oxc folds getter/setter interface signatures into TSMethodSignature (kind Get/Set),
// handled by gen_method_signature.

fn gen_keyword_type<'a>(node: Node<'a>, context: &mut Context<'a>) -> PrintItems {
  // oxc has a distinct node per keyword type (TSAnyKeyword, TSNumberKeyword, ...); just
  // emit its source text ("any", "unknown", "number", etc.).
  node.text_fast(context.program).to_string().into()
}

fn import_type_qualifier_to_node<'a>(q: &'a TSImportTypeQualifier<'a>) -> Node<'a> {
  match q {
    TSImportTypeQualifier::Identifier(i) => Node::IdentifierName(i),
    TSImportTypeQualifier::QualifiedName(q) => Node::TSImportTypeQualifiedName(q),
  }
}

fn gen_import_type<'a>(node: &'a TSImportType<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.push_sc(sc!("import("));
  items.extend(gen_node(Node::StringLiteral(&node.source), context));
  if let Some(options) = &node.options {
    items.push_sc(sc!(", "));
    items.extend(gen_object_like_node(
      GenObjectLikeNodeOptions {
        node: Node::ObjectExpression(options),
        members: options.properties.iter().map(obj_prop_kind_to_node).collect(),
        separator: context.config.object_expression_trailing_commas.into(),
        prefer_hanging: context.config.object_expression_prefer_hanging,
        prefer_single_line: context.config.object_expression_prefer_single_line,
        force_single_line: false,
        force_multi_line: is_node_definitely_above_line_width(options.range(), context),
        surround_single_line_with_spaces: context.config.object_expression_space_surrounding_properties,
        surround_single_line_with_space_at_end: false,
        allow_blank_lines: true,
        node_sorter: None,
      },
      context,
    ));
  }
  items.push_sc(sc!(")"));

  if let Some(qualifier) = &node.qualifier {
    items.push_sc(sc!("."));
    items.extend(gen_node(import_type_qualifier_to_node(qualifier), context));
  }

  if let Some(type_args) = &node.type_arguments {
    items.extend(gen_node(Node::TSTypeParameterInstantiation(type_args), context));
  }
  items
}

// `import("x").Foo.Bar` qualified name within an import type
fn gen_import_type_qualified_name<'a>(node: &'a TSImportTypeQualifiedName<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = gen_node(import_type_qualifier_to_node(&node.left), context);
  items.push_sc(sc!("."));
  items.extend(gen_node(Node::IdentifierName(&node.right), context));
  items
}

fn gen_indexed_access_type<'a>(node: &'a TSIndexedAccessType<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.extend(gen_node(ts_type_to_node(&node.object_type), context));
  items.extend(gen_computed_prop_like(
    |context| gen_node(ts_type_to_node(&node.index_type), context),
    GenComputedPropLikeOptions {
      inner_node_range: node.index_type.range(),
    },
    context,
  ));
  items
}

fn gen_infer_type<'a>(node: &'a TSInferType<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.push_sc(sc!("infer "));
  items.extend(gen_node(Node::TSTypeParameter(&node.type_parameter), context));
  items
}

fn gen_ts_instantiation<'a>(node: &'a TSInstantiationExpression<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = gen_node(expr_to_node(&node.expression), context);
  items.extend(gen_node(Node::TSTypeParameterInstantiation(&node.type_arguments), context));
  items
}

fn gen_intersection_type<'a>(node: &'a TSIntersectionType<'a>, context: &mut Context<'a>) -> PrintItems {
  gen_union_or_intersection_type(
    UnionOrIntersectionType {
      node: Node::TSIntersectionType(node),
      types: &node.types,
      is_union: false,
    },
    context,
  )
}

fn gen_lit_type<'a>(node: &'a TSLiteralType<'a>, context: &mut Context<'a>) -> PrintItems {
  match &node.literal {
    // need to do this in order to support negative numbers
    TSLiteral::NumericLiteral(_) | TSLiteral::BigIntLiteral(_) => node.text_fast(context.program).to_string().into(),
    _ => gen_node(ts_literal_to_node(&node.literal), context),
  }
}

fn gen_mapped_type<'a>(node: &'a TSMappedType<'a>, context: &mut Context<'a>) -> PrintItems {
  // oxc splits SWC's mapped-type `type_param` into separate `key` (P) + `constraint` (K),
  // and exposes optional/readonly as Option<TSMappedTypeModifierOperator>.
  let type_param_range = SourceRange::new(node.key.start(), node.constraint.end());
  let force_use_new_lines =
    !context.config.mapped_type_prefer_single_line && node_helpers::get_use_new_lines_for_nodes(&node.start().range(), &node.key, context.program);

  let range = node.range();
  let mut items = PrintItems::new();
  let start_ln = LineNumber::new("startMappedType");
  items.push_info(start_ln);
  items.extend(gen_surrounded_by_tokens(
    |context| {
      let is_different_line_than_start: ConditionResolver = if force_use_new_lines {
        condition_resolvers::true_resolver()
      } else {
        Rc::new(move |context| condition_helpers::is_on_different_line(context, start_ln))
      };
      let inner_items = {
        let mut items = PrintItems::new();

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

        let computed_inner_range = SourceRange::new(
          type_param_range.start,
          node.name_type.as_ref().map(|t| t.end()).unwrap_or_else(|| type_param_range.end),
        );
        items.extend(gen_computed_prop_like(
          |context| {
            // `P in K` (and optionally `as NameType`)
            let mut items = gen_node(Node::BindingIdentifier(&node.key), context);
            items.push_signal(Signal::SpaceOrNewLine);
            items.push_condition(conditions::indent_if_start_of_line({
              let mut items = PrintItems::new();
              items.push_sc(sc!("in"));
              items.push_signal(Signal::SpaceIfNotTrailing);
              items.extend(gen_node(ts_type_to_node(&node.constraint), context));
              items
            }));
            if let Some(name_type) = &node.name_type {
              items.push_sc(sc!(" as"));
              items.push_signal(Signal::SpaceIfNotTrailing);
              items.extend(gen_node(ts_type_to_node(name_type), context));
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

        items.extend(gen_type_ann_with_colon_if_exists_for_type(
          node.type_annotation.as_ref().map(ts_type_to_node),
          context,
        ));
        items.extend(get_generated_semi_colon(context.config.semi_colons, true, &is_different_line_than_start));
        items.extend(generated_semi_colon_comments);

        let inner_items = items.into_rc_path();
        if_true_or("noSpacesWhenMultiLine", is_different_line_than_start, inner_items.into(), {
          let mut items = PrintItems::new();
          items.push_signal(Signal::SpaceOrNewLine);
          items.push_optional_path(inner_items);
          items.push_signal(Signal::SpaceOrNewLine);
          items
        })
        .into()
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
    },
    context,
  ));
  items
}

fn gen_optional_type<'a>(node: &'a TSOptionalType<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.extend(gen_node(ts_type_to_node(&node.type_annotation), context));
  items.push_sc(sc!("?"));
  items
}

fn gen_qualified_name<'a>(node: &'a TSQualifiedName<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.extend(gen_node(ts_type_name_to_node(&node.left), context));
  items.push_sc(sc!("."));
  items.extend(gen_node(Node::IdentifierName(&node.right), context));
  items
}

fn gen_parenthesized_type<'a>(node: &'a TSParenthesizedType<'a>, context: &mut Context<'a>) -> PrintItems {
  if should_skip_parenthesized_type(node, context) {
    return gen_node(ts_type_to_node(&node.type_annotation), context);
  }

  let generated_type = conditions::with_indent_if_start_of_line_indented(gen_node_in_parens(
    |context| gen_node(ts_type_to_node(&node.type_annotation), context),
    GenNodeInParensOptions {
      inner_range: node.type_annotation.range(),
      prefer_hanging: true,
      allow_open_paren_trailing_comments: true,
      single_line_space_around: false,
    },
    context,
  ))
  .into();

  return if use_new_line_group(context) {
    new_line_group(generated_type)
  } else {
    generated_type
  };

  fn use_new_line_group(context: &Context) -> bool {
    !matches!(context.parent(), Node::TSTypeAliasDeclaration(_))
  }
}

fn should_skip_parenthesized_type<'a>(node: &'a TSParenthesizedType<'a>, context: &mut Context<'a>) -> bool {
  if node_helpers::has_surrounding_different_line_comments(ts_type_to_node(&node.type_annotation), context.program) {
    return false;
  }
  if let Some(open_paren) = context.token_finder.get_previous_token_if_open_paren(&node.type_annotation.range()) {
    let trailing_comments = open_paren.trailing_comments_fast(context.program);
    if trailing_comments
      .into_iter()
      .any(|c| c.is_line() && c.start_line_fast(context.program) == open_paren.end_line_fast(context.program))
    {
      return false;
    }
  }

  if context.token_finder.get_previous_token_if_colon(node).is_some() {
    return true;
  }

  matches!(context.parent(), Node::TSTypeAnnotation(_) | Node::TSTypeAliasDeclaration(_))
}

fn gen_rest_type<'a>(node: &'a TSRestType<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.push_sc(sc!("..."));
  items.extend(gen_node(ts_type_to_node(&node.type_annotation), context));
  items
}

fn gen_tpl_lit_type<'a>(node: &'a TSTemplateLiteralType<'a>, context: &mut Context<'a>) -> PrintItems {
  gen_template_literal(
    node.quasis.iter().map(Node::TemplateElement).collect(),
    node.types.iter().map(ts_type_to_node).collect(),
    context,
  )
}

fn gen_tuple_type<'a>(node: &'a TSTupleType<'a>, context: &mut Context<'a>) -> PrintItems {
  let prefer_hanging = match context.config.tuple_type_prefer_hanging {
    PreferHanging::Never => false,
    PreferHanging::OnlySingleItem => node.element_types.len() == 1,
    PreferHanging::Always => true,
  };
  gen_array_like_nodes(
    GenArrayLikeNodesOptions {
      node: Node::TSTupleType(node),
      nodes: node.element_types.iter().map(|x| Some(ts_tuple_element_to_node(x))).collect(),
      prefer_hanging,
      prefer_single_line: context.config.tuple_type_prefer_single_line,
      trailing_commas: context.config.tuple_type_trailing_commas,
      space_around: context.config.tuple_type_space_around,
    },
    context,
  )
}

fn gen_tuple_element<'a>(node: &'a TSNamedTupleMember<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.extend(gen_node(Node::IdentifierName(&node.label), context));
  if node.optional {
    items.push_sc(sc!("?"));
  }
  items.extend(gen_type_ann_with_colon_for_type(ts_tuple_element_to_node(&node.element_type), context));
  items
}

fn gen_type_ann<'a>(node: &'a TSTypeAnnotation<'a>, context: &mut Context<'a>) -> PrintItems {
  gen_node(ts_type_to_node(&node.type_annotation), context)
}

fn gen_type_param<'a>(node: &'a TSTypeParameter<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();

  if node.r#const {
    items.push_sc(sc!("const "));
  }
  if node.r#in {
    items.push_sc(sc!("in "));
  }
  if node.out {
    items.push_sc(sc!("out "));
  }

  items.extend(gen_node(Node::BindingIdentifier(&node.name), context));

  if let Some(constraint) = &node.constraint {
    items.push_signal(Signal::SpaceOrNewLine);
    items.push_condition(conditions::indent_if_start_of_line({
      let mut items = PrintItems::new();
      items.push_sc(if matches!(context.parent(), Node::TSMappedType(_)) {
        sc!("in")
      } else {
        sc!("extends")
      });
      items.push_signal(Signal::SpaceIfNotTrailing);
      items.extend(gen_node(ts_type_to_node(constraint), context));
      items
    }));
  }

  if let Some(default) = &node.default {
    items.extend(gen_assignment(ts_type_to_node(default), sc!("="), context));
  }

  items
}

fn gen_type_parameters<'a>(node: TypeParamNode<'a>, context: &mut Context<'a>) -> PrintItems {
  let params = node.params();
  let force_use_new_lines = get_use_new_lines(&node, &params, context);
  let mut items = PrintItems::new();
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
      allow_blank_lines: false,
      separator: get_trailing_commas(&node, context).into(),
      single_line_options: ir_helpers::SingleLineOptions::same_line_maybe_space_separated(),
      multi_line_options: ir_helpers::MultiLineOptions::surround_newlines_indented(),
      force_possible_newline_at_start: false,
      node_sorter: None,
    },
    context,
  ));
  items.push_sc(sc!(">"));

  return items;

  fn get_trailing_commas<'a>(node: &TypeParamNode<'a>, context: &mut Context<'a>) -> TrailingCommas {
    let trailing_commas = context.config.type_parameters_trailing_commas;
    if trailing_commas == TrailingCommas::Never {
      return trailing_commas;
    }

    // trailing commas are allowed in type parameter *declarations* only, not type arguments
    let type_params = match node {
      TypeParamNode::Decl(decl) => *decl,
      TypeParamNode::Instantiation(_) => return TrailingCommas::Never,
    };

    // Use trailing commas for arrow/function expressions in a JSX (or .cts/.mts) file
    // if the absence of one would lead to a parsing ambiguity (`<T,>() => ...`).
    let parent = node.parent(context);
    // the node containing the function/arrow (to exclude default exports)
    let grandparent = context.parent_stack.iter().nth(1).copied();
    let is_ambiguous_jsx_fn_expr = context.is_jsx()
      && matches!(
        parent,
        Node::ArrowFunctionExpression(_)
          | Node::Function(Function {
            r#type: FunctionType::FunctionExpression,
            ..
          })
      )
      // not ambiguous in a default export
      && !matches!(grandparent, Some(Node::ExportDefaultDeclaration(_)));
    // Prevent "This syntax is reserved in files with the .mts or .cts extension." diagnostic.
    let is_cts_mts_arrow_fn = matches!(context.media_type, MediaType::Cts | MediaType::Mts) && matches!(parent, Node::ArrowFunctionExpression(_));
    if is_ambiguous_jsx_fn_expr || is_cts_mts_arrow_fn {
      // ambiguous iff there's a single type parameter that is just an identifier
      // (no constraint or default)
      if type_params.params.len() == 1 {
        let type_param = &type_params.params[0];
        if type_param.constraint.is_none() && type_param.default.is_none() {
          return TrailingCommas::Always;
        }
      }
    }

    trailing_commas
  }

  fn get_use_new_lines(node: &TypeParamNode, params: &[Node], context: &mut Context) -> bool {
    if context.config.type_parameters_prefer_single_line || params.is_empty() {
      false
    } else {
      let first_param = &params[0];
      let angle_bracket_pos = node.start();
      node_helpers::get_use_new_lines_for_nodes(&angle_bracket_pos.range(), first_param, context.program)
    }
  }
}

fn gen_type_operator<'a>(node: &'a TSTypeOperator<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.push_sc(match node.operator {
    TSTypeOperatorOperator::Keyof => sc!("keyof"),
    TSTypeOperatorOperator::Unique => sc!("unique"),
    TSTypeOperatorOperator::Readonly => sc!("readonly"),
  });
  items.push_signal(Signal::SpaceIfNotTrailing);
  items.extend(gen_node(ts_type_to_node(&node.type_annotation), context));
  items
}

fn gen_type_predicate<'a>(node: &'a TSTypePredicate<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  if node.asserts {
    items.push_sc(sc!("asserts "));
  }
  items.extend(gen_node(ts_type_predicate_name_to_node(&node.parameter_name), context));
  if let Some(type_ann) = &node.type_annotation {
    items.push_sc(sc!(" is"));
    items.push_signal(Signal::SpaceIfNotTrailing);
    items.extend(gen_node(ts_type_to_node(&type_ann.type_annotation), context));
  }
  items
}

fn gen_type_query<'a>(node: &'a TSTypeQuery<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.push_sc(sc!("typeof"));
  items.push_signal(Signal::SpaceIfNotTrailing);
  items.extend(gen_node(ts_type_query_expr_name_to_node(&node.expr_name), context));
  if let Some(type_args) = &node.type_arguments {
    items.extend(gen_node(Node::TSTypeParameterInstantiation(type_args), context));
  }
  items
}

fn gen_type_reference<'a>(node: &'a TSTypeReference<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.extend(gen_node(ts_type_name_to_node(&node.type_name), context));
  if let Some(type_args) = &node.type_arguments {
    items.extend(gen_node(Node::TSTypeParameterInstantiation(type_args), context));
  }
  items
}

fn gen_union_type<'a>(node: &'a TSUnionType<'a>, context: &mut Context<'a>) -> PrintItems {
  gen_union_or_intersection_type(
    UnionOrIntersectionType {
      node: Node::TSUnionType(node),
      types: &node.types,
      is_union: true,
    },
    context,
  )
}

struct UnionOrIntersectionType<'a> {
  pub node: Node<'a>,
  pub types: &'a [TSType<'a>],
  pub is_union: bool,
}

fn gen_union_or_intersection_type<'a>(node: UnionOrIntersectionType<'a>, context: &mut Context<'a>) -> PrintItems {
  // todo: configuration for operator position
  let mut items = PrintItems::new();
  let force_use_new_lines = get_use_new_lines_for_nodes(node.types, context.config.union_and_intersection_type_prefer_single_line, context);
  let separator = if node.is_union { sc!("|") } else { sc!("&") };

  let indent_width = context.config.indent_width;
  let prefer_hanging = context.config.union_and_intersection_type_prefer_hanging;
  let is_parent_union_or_intersection = matches!(context.parent(), Node::TSUnionType(_) | Node::TSIntersectionType(_));
  let should_skip_list_indent = has_first_separator_leading_comment_on_previous_line(&node, separator, context);
  let multi_line_options = if !is_parent_union_or_intersection {
    if use_surround_newlines(node.node, context) {
      ir_helpers::MultiLineOptions::surround_newlines_indented()
    } else if should_skip_list_indent {
      ir_helpers::MultiLineOptions {
        newline_at_start: true,
        newline_at_end: false,
        with_indent: false,
        with_hanging_indent: BoolOrCondition::Bool(false),
        maintain_line_breaks: false,
      }
    } else {
      ir_helpers::MultiLineOptions::new_line_start()
    }
  } else {
    ir_helpers::MultiLineOptions::same_line_start_hanging_indent()
  };
  let gen_result = ir_helpers::gen_separated_values(
    |is_multi_line_or_hanging_ref| {
      let is_multi_line_or_hanging = is_multi_line_or_hanging_ref.create_resolver();
      let types_count = node.types.len();
      let mut generated_nodes = Vec::new();
      for (i, type_node) in node.types.iter().enumerate() {
        let (allow_inline_multi_line, allow_inline_single_line) = {
          let is_last_value = i + 1 == types_count; // allow the last type to be single line
          (allows_inline_multi_line(ts_type_to_node(type_node), context, types_count > 1), is_last_value)
        };
        let separator_token = context.token_finder.get_previous_token_if_operator(&type_node.range(), separator.text);
        let start_lc = LineAndColumn::new("start");
        let after_separator_ln = LineNumber::new("afterSeparator");
        let leading_comments_before_type = if i == 0 {
          let previous_token_end = separator_token
            .map(|token| token.end())
            .unwrap_or_else(|| context.token_finder.get_previous_token_end_before(&type_node.range()));
          get_comments_between(previous_token_end, type_node.start(), context)
            .into_iter()
            .filter(|comment| !context.has_handled_comment(comment))
            .collect::<Vec<_>>()
        } else {
          Vec::new()
        };
        let mut items = PrintItems::new();
        items.push_line_and_column(start_lc);
        if let Some(separator_token) = separator_token {
          items.extend(gen_leading_comments(&separator_token.range(), context));
        }
        if i == 0 && !is_parent_union_or_intersection {
          items.push_condition(if_true("separatorIfMultiLine", is_multi_line_or_hanging.clone(), {
            // todo: .into() implementation for StringContainer
            let mut items = PrintItems::new();
            items.push_sc(separator);
            items
          }));
        } else if i > 0 {
          items.push_sc(separator);
        }

        if let Some(separator_token) = separator_token {
          items.extend(gen_trailing_comments(&separator_token.range(), context));
        }
        items.push_info(after_separator_ln);

        items.push_condition(if_true(
          "afterSeparatorSpace",
          Rc::new(move |condition_context| {
            let is_on_same_line = condition_helpers::is_on_same_line(condition_context, after_separator_ln)?;
            let is_at_same_position = condition_helpers::is_at_same_position(condition_context, start_lc)?;
            Some(is_on_same_line && !is_at_same_position)
          }),
          Signal::SpaceIfNotTrailing.into(),
        ));
        let indent_type_after_leading_comments = leading_comments_before_type
          .last()
          .is_some_and(|comment| has_newline_between_positions(comment.end(), type_node.start(), context.program));
        if !leading_comments_before_type.is_empty() {
          items.extend(gen_comments_as_leading(
            &type_node.range(),
            CommentsIterator::new(leading_comments_before_type),
            context,
          ));
        }
        let generated_type = gen_node(ts_type_to_node(type_node), context);
        if indent_type_after_leading_comments {
          items.extend(with_indent(generated_type));
        } else {
          items.extend(generated_type);
        }

        generated_nodes.push(ir_helpers::GeneratedValue {
          items,
          lines_span: None,
          allow_inline_multi_line,
          allow_inline_single_line,
        });
      }

      generated_nodes
    },
    ir_helpers::GenSeparatedValuesOptions {
      prefer_hanging,
      force_use_new_lines,
      allow_blank_lines: false,
      indent_width,
      single_line_options: ir_helpers::SingleLineOptions::separated_same_line(Signal::SpaceOrNewLine.into()),
      multi_line_options,
      force_possible_newline_at_start: false,
    },
  );

  items.extend(gen_result.items);

  return items;

  fn use_surround_newlines<'a>(_node: Node<'a>, context: &mut Context<'a>) -> bool {
    match context.parent() {
      Node::TSTypeAssertion(_) => true,
      Node::TSParenthesizedType(paren_type) => !should_skip_parenthesized_type_for_nested_type(paren_type, context),
      _ => false,
    }
  }

  fn should_skip_parenthesized_type_for_nested_type<'a>(node: &'a TSParenthesizedType<'a>, context: &mut Context<'a>) -> bool {
    if node_helpers::has_surrounding_different_line_comments(ts_type_to_node(&node.type_annotation), context.program) {
      return false;
    }
    if let Some(open_paren) = context.token_finder.get_previous_token_if_open_paren(&node.type_annotation.range()) {
      let trailing_comments = open_paren.trailing_comments_fast(context.program);
      if trailing_comments
        .into_iter()
        .any(|c| c.is_line() && c.start_line_fast(context.program) == open_paren.end_line_fast(context.program))
      {
        return false;
      }
    }

    if context.token_finder.get_previous_token_if_colon(node).is_some() {
      return true;
    }

    matches!(
      context.parent_stack.iter().nth(1).copied(),
      Some(Node::TSTypeAnnotation(_) | Node::TSTypeAliasDeclaration(_))
    )
  }

  fn has_first_separator_leading_comment_on_previous_line(node: &UnionOrIntersectionType, separator: &'static StringContainer, context: &mut Context) -> bool {
    let Some(first_type) = node.types.first() else {
      return false;
    };
    let Some(separator_token) = context.token_finder.get_previous_token_if_operator(&first_type.range(), separator.text) else {
      return false;
    };
    let separator_start_line = separator_token.start_line_fast(context.program);
    separator_token
      .start()
      .leading_comments_fast(context.program)
      .any(|comment| comment.start_line_fast(context.program) < separator_start_line || comment.is_line())
  }
}

fn has_newline_between_positions(start: SourcePos, end: SourcePos, program: ProgramInfo) -> bool {
  let text = program.text();
  text.get(start as usize..end as usize).is_some_and(|text| text.as_bytes().contains(&b'\n'))
}

/* comments */

fn gen_leading_comments<'a>(node: &SourceRange, context: &mut Context<'a>) -> PrintItems {
  let leading_comments = node.leading_comments_fast(context.program);
  gen_comments_as_leading(node, leading_comments, context)
}

fn gen_comments_as_leading<'a>(node: &SourceRange, comments: CommentsIterator<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  if let Some(last_comment) = comments.peek_last_comment() {
    let last_comment_previously_handled = context.has_handled_comment(last_comment);

    items.extend(gen_comment_collection(comments, None, Some(node), context));

    // todo: this doesn't seem exactly right...
    if !last_comment_previously_handled {
      let node_start_line = node.start_line_fast(context.program);
      let last_comment_end_line = last_comment.end_line_fast(context.program);
      if node_start_line > last_comment_end_line {
        items.push_condition(if_true_or(
          "spaceIfForcingNoNewlines",
          condition_resolvers::is_forcing_no_newlines(),
          Signal::SpaceIfNotTrailing.into(),
          {
            let mut items = PrintItems::new();
            items.push_signal(Signal::NewLine);

            if node_start_line - 1 > last_comment_end_line {
              items.push_signal(Signal::NewLine);
            }
            items
          },
        ));
      } else if last_comment.is_block() {
        if node_start_line == last_comment_end_line {
          items.push_signal(Signal::SpaceIfNotTrailing);
        } else {
          // use a space if no newlines are currently being forced
          items.push_condition(if_true(
            "conditionalCommentBlockSpace",
            condition_resolvers::is_forcing_no_newlines(),
            Signal::SpaceIfNotTrailing.into(),
          ));
        }
      }
    }
  }

  items
}

fn gen_trailing_comments_as_statements<'a>(node: &SourceRange, context: &mut Context<'a>) -> PrintItems {
  let comments = get_trailing_comments_as_statements(node, context);
  gen_comments_as_statements(comments.into_iter(), Some(node), context)
}

fn gen_leading_comments_on_previous_lines<'a>(node: &SourceRange, context: &mut Context<'a>) -> PrintItems {
  let unhandled_comments = get_leading_comments_on_previous_lines(node, context);
  gen_comments_as_statements(unhandled_comments.into_iter(), None, context)
}

fn get_leading_comments_on_previous_lines<'a>(node: &SourceRange, context: &mut Context<'a>) -> Vec<&'a Comment> {
  let leading_comments = node.leading_comments_fast(context.program);
  if leading_comments.is_empty() {
    return Vec::new(); // avoid extra calculations
  }
  let node_start_line = node.start_line_fast(context.program);
  let previous_token_line = node.previous_token_fast(context.program).map(|t| t.end_line_fast(context.program));
  leading_comments
    .take_while(|c| {
      let line = c.start_line_fast(context.program);
      line < node_start_line && previous_token_line.map(|line| line < node_start_line).unwrap_or(true)
    })
    .collect::<Vec<_>>()
}

fn get_leading_comments_on_same_line<'a>(node: &SourceRange, context: &mut Context<'a>) -> Vec<&'a Comment> {
  let leading_comments = node.leading_comments_fast(context.program);
  if leading_comments.is_empty() {
    Vec::new()
  } else {
    let node_start_line = node.start_line_fast(context.program);
    leading_comments
      .filter(|c| c.start_line_fast(context.program) == node_start_line)
      .collect::<Vec<_>>()
  }
}

fn get_trailing_comments_on_same_line<'a>(node: &SourceRange, context: &mut Context<'a>) -> Vec<&'a Comment> {
  let trailing_comments = node.trailing_comments_fast(context.program);
  if trailing_comments.is_empty() {
    Vec::new()
  } else {
    let node_start_line = node.start_line_fast(context.program);
    trailing_comments
      .take_while(|c| c.start_line_fast(context.program) == node_start_line)
      .collect::<Vec<_>>()
  }
}

fn get_trailing_comments_as_statements<'a>(node: &SourceRange, context: &mut Context<'a>) -> Vec<&'a Comment> {
  let mut comments = Vec::new();
  let node_end_line = node.end_line_fast(context.program);
  for comment in node.trailing_comments_fast(context.program) {
    if !context.has_handled_comment(comment) && node_end_line < comment.end_line_fast(context.program) {
      comments.push(comment);
    }
  }
  comments
}

fn gen_comments_as_statements<'a>(comments: impl Iterator<Item = &'a Comment>, last_node: Option<&SourceRange>, context: &mut Context<'a>) -> PrintItems {
  let mut last_node = last_node.map(|l| l.range());
  let mut items = PrintItems::new();
  let mut was_last_block_comment = false;
  for comment in comments {
    if !context.has_handled_comment(comment) {
      items.extend(gen_comment_based_on_last_node(
        comment,
        &last_node,
        GenCommentBasedOnLastNodeOptions { separate_with_newlines: true },
        context,
      ));
      last_node = Some(comment.range());
      was_last_block_comment = comment.is_block();
    }
  }
  if was_last_block_comment {
    items.push_signal(Signal::ExpectNewLine);
  }
  items
}

fn gen_comments_as_statements_advancing_handled<'a>(
  comments: impl Iterator<Item = &'a Comment>,
  last_node: Option<&SourceRange>,
  context: &mut Context<'a>,
) -> PrintItems {
  let mut last_node = last_node.map(|l| l.range());
  let mut items = PrintItems::new();
  let mut was_last_block_comment = false;
  for comment in comments {
    if context.has_handled_comment(comment) {
      if last_node.is_some() {
        last_node = Some(comment.range());
        was_last_block_comment = comment.is_block();
      }
      continue;
    }

    items.extend(gen_comment_based_on_last_node(
      comment,
      &last_node,
      GenCommentBasedOnLastNodeOptions { separate_with_newlines: true },
      context,
    ));
    last_node = Some(comment.range());
    was_last_block_comment = comment.is_block();
  }
  if was_last_block_comment {
    items.push_signal(Signal::ExpectNewLine);
  }
  items
}

fn gen_comments_as_statements_unchecked<'a>(
  comments: impl Iterator<Item = &'a Comment>,
  last_node: Option<&SourceRange>,
  context: &mut Context<'a>,
) -> PrintItems {
  let mut last_node = last_node.map(|l| l.range());
  let mut items = PrintItems::new();
  let mut was_last_block_comment = false;
  for comment in comments {
    items.extend(gen_comment_based_on_last_node_unchecked(
      comment,
      &last_node,
      GenCommentBasedOnLastNodeOptions { separate_with_newlines: true },
      context,
    ));
    last_node = Some(comment.range());
    was_last_block_comment = comment.is_block();
  }
  if was_last_block_comment {
    items.push_signal(Signal::ExpectNewLine);
  }
  items
}

fn gen_comments_between_lines_indented(start_between_pos: SourcePos, context: &mut Context) -> PrintItems {
  let trailing_comments = get_comments_between_lines(start_between_pos, context);
  let mut items = PrintItems::new();

  if !trailing_comments.is_empty() {
    items.extend(with_indent({
      let mut items = PrintItems::new();
      if let Some(first_comment) = trailing_comments.first() {
        if first_comment.is_block() {
          items.push_signal(Signal::SpaceIfNotTrailing);
        }
      }
      items.extend(gen_comment_collection(
        trailing_comments.into_iter(),
        Some(&start_between_pos.range()),
        None,
        context,
      ));
      items
    }));
    items.push_signal(Signal::NewLine);
  }

  return items;

  fn get_comments_between_lines<'a>(previous_token_end: SourcePos, context: &mut Context<'a>) -> Vec<&'a Comment> {
    let mut comments = Vec::new();
    let trailing_comments = previous_token_end.trailing_comments_fast(context.program);
    if !trailing_comments.is_empty() {
      let next_token_pos = context.token_finder.get_next_token_pos_after(&previous_token_end.range());
      let next_token_start_line = next_token_pos.start_line_fast(context.program);

      for comment in trailing_comments {
        if !context.has_handled_comment(comment) && comment.start_line_fast(context.program) < next_token_start_line {
          comments.push(comment);
        }
      }
    }
    comments
  }
}

fn gen_comment_collection<'a>(
  comments: impl Iterator<Item = &'a Comment>,
  last_node: Option<&SourceRange>,
  next_node: Option<&SourceRange>,
  context: &mut Context<'a>,
) -> PrintItems {
  let mut last_node = last_node.map(|l| l.range());
  let mut items = PrintItems::new();
  let next_node_start_line = next_node.map(|n| n.start_line_fast(context.program));
  for comment in comments {
    if !context.has_handled_comment(comment) {
      items.extend(gen_comment_based_on_last_node(
        comment,
        &last_node,
        GenCommentBasedOnLastNodeOptions {
          separate_with_newlines: if let Some(next_node_start_line) = next_node_start_line {
            comment.start_line_fast(context.program) != next_node_start_line
          } else {
            false
          },
        },
        context,
      ));
      last_node = Some(comment.range());
    }
  }
  items
}

fn gen_leading_comments_same_line(node: &SourceRange, context: &mut Context) -> PrintItems {
  let comments = get_leading_comments_on_same_line(node, context);
  if comments.is_empty() {
    PrintItems::new()
  } else {
    let mut items = gen_comments_same_line(comments, context);
    if !items.is_empty() {
      items.push_space();
    }
    items
  }
}

fn gen_trailing_comments_same_line(node: &SourceRange, context: &mut Context) -> PrintItems {
  let comments = get_trailing_comments_on_same_line(node, context);
  if comments.is_empty() {
    PrintItems::new()
  } else {
    let mut items = PrintItems::new();
    let comment_items = gen_comments_same_line(comments, context);
    if !comment_items.is_empty() {
      items.push_space();
      items.extend(comment_items);
    }
    items
  }
}

fn gen_trailing_comments_if_line_comment_same_line(node: &SourceRange, context: &mut Context) -> PrintItems {
  let comments = get_trailing_comments_on_same_line(node, context);
  if !comments.iter().any(|c| c.kind == CommentKind::Line) {
    PrintItems::new()
  } else {
    let mut items = PrintItems::new();
    let comment_items = gen_comments_same_line(comments, context);
    if !comment_items.is_empty() {
      items.push_space();
      items.extend(comment_items);
    }
    items
  }
}

fn gen_comments_same_line(comments: Vec<&Comment>, context: &mut Context) -> PrintItems {
  let mut items = PrintItems::new();
  let mut had_comment_last = false;
  for comment in comments {
    if had_comment_last {
      items.push_space();
    }
    if let Some(comment) = gen_comment(comment, context) {
      items.extend(comment);
      had_comment_last = true;
    } else {
      had_comment_last = false;
    }
  }
  items
}

struct GenCommentBasedOnLastNodeOptions {
  separate_with_newlines: bool,
}

fn gen_comment_based_on_last_node(
  comment: &Comment,
  last_node: &Option<SourceRange>,
  opts: GenCommentBasedOnLastNodeOptions,
  context: &mut Context,
) -> PrintItems {
  let mut items = PrintItems::new();
  let mut pushed_ignore_new_lines = false;

  if let Some(last_node) = last_node {
    let comment_start_line = comment.start_line_fast(context.program);
    let last_node_end_line = last_node.end_line_fast(context.program);

    if opts.separate_with_newlines || comment_start_line > last_node_end_line {
      items.push_signal(Signal::NewLine);

      if comment_start_line > last_node_end_line + 1 {
        items.push_signal(Signal::NewLine);
      }
    } else if comment.kind == CommentKind::Line {
      items.push_signal(Signal::StartForceNoNewLines);
      items.push_space();
      pushed_ignore_new_lines = true;
    } else if last_node.text_fast(context.program).starts_with("/*") {
      items.push_space();
    }
  }

  if let Some(generated_comment) = gen_comment(comment, context) {
    items.extend(generated_comment);
  }

  if pushed_ignore_new_lines {
    items.push_signal(Signal::FinishForceNoNewLines);
  }

  items
}

fn gen_comment_based_on_last_node_unchecked(
  comment: &Comment,
  last_node: &Option<SourceRange>,
  opts: GenCommentBasedOnLastNodeOptions,
  context: &mut Context,
) -> PrintItems {
  let mut items = PrintItems::new();
  let mut pushed_ignore_new_lines = false;

  if let Some(last_node) = last_node {
    let comment_start_line = comment.start_line_fast(context.program);
    let last_node_end_line = last_node.end_line_fast(context.program);

    if opts.separate_with_newlines || comment_start_line > last_node_end_line {
      items.push_signal(Signal::NewLine);

      if comment_start_line > last_node_end_line + 1 {
        items.push_signal(Signal::NewLine);
      }
    } else if comment.kind == CommentKind::Line {
      items.push_signal(Signal::StartForceNoNewLines);
      items.push_space();
      pushed_ignore_new_lines = true;
    } else if last_node.text_fast(context.program).starts_with("/*") {
      items.push_space();
    }
  }

  items.extend(gen_comment_inner(comment, context));

  if pushed_ignore_new_lines {
    items.push_signal(Signal::FinishForceNoNewLines);
  }

  items
}

fn gen_comment(comment: &Comment, context: &mut Context) -> Option<PrintItems> {
  // only generate if handled
  if context.has_handled_comment(comment) {
    return None;
  }

  // mark handled and generate
  context.mark_comment_handled(comment);
  Some(gen_comment_inner(comment, context))
}

fn gen_comment_inner(comment: &Comment, context: &mut Context) -> PrintItems {
  return if comment.is_block() {
    if has_leading_astrisk_each_line(comment.content_fast(context.program)) {
      gen_js_doc_or_multiline_block(comment, context)
    } else {
      // Single-line comment block
      ir_helpers::gen_js_like_comment_block(comment.content_fast(context.program))
    }
  } else {
    ir_helpers::gen_js_like_comment_line(comment.content_fast(context.program), context.config.comment_line_force_space_after_slashes)
  };

  fn has_leading_astrisk_each_line(text: &str) -> bool {
    if !text.contains('\n') {
      return false;
    }

    for line in text.trim().split('\n') {
      let first_non_whitespace = line.trim_start().chars().next();
      if !matches!(first_non_whitespace, Some('*')) {
        return false;
      }
    }

    true
  }
}

fn gen_js_doc_or_multiline_block<'a>(comment: &Comment, context: &mut Context<'a>) -> PrintItems {
  debug_assert!(comment.is_block());
  let text = comment.content_fast(context.program);
  let is_js_doc = text.starts_with('*');
  return lines_to_print_items(is_js_doc, build_lines(text));

  fn build_lines(text: &str) -> Vec<&str> {
    let mut lines: Vec<&str> = Vec::new();

    for line in utils::split_lines(text) {
      let text = if line.line_index == 0 && !line.text.starts_with('*') {
        line.text
      } else {
        &line.text[get_line_start_index(line.text)..]
      };
      let text = if line.is_last && !text.trim().is_empty() { text } else { text.trim_end() };

      if !text.is_empty() || !lines.last().map(|l| l.is_empty()).unwrap_or(false) {
        lines.push(text);
      }
    }

    lines
  }

  fn get_line_start_index(text: &str) -> usize {
    for (byte_index, c) in text.char_indices() {
      if c == '*' {
        return byte_index + 1;
      } else if !c.is_whitespace() {
        return byte_index;
      }
    }

    0
  }

  fn lines_to_print_items(is_js_doc: bool, lines: Vec<&str>) -> PrintItems {
    let mut items = PrintItems::new();

    items.push_sc(sc!("/*"));

    for (i, line) in lines.iter().enumerate() {
      if i > 0 {
        items.push_signal(Signal::NewLine);
      }
      let mut text = String::new();
      // leading asterisk on the first line for jsdoc only
      if is_js_doc && i == 0 {
        text.push('*');
      } else if i > 0 {
        text.push_str(" *");
      }

      // line start space
      let is_space_or_asterisk = matches!(line.chars().next(), Some('*' | ' '));
      if !line.is_empty() && !is_space_or_asterisk {
        text.push(' ');
      }

      // line text
      items.push_string(text);
      if !line.is_empty() {
        items.extend(gen_from_raw_string(line));
      }
    }

    items.push_sc(if lines.len() > 1 && lines.last().map(|l| l.is_empty()).unwrap_or(false) {
      sc!("/")
    } else if lines.len() > 1 && lines.last().map(|l| l.ends_with('*') || l.ends_with(' ')).unwrap_or(false) {
      sc!("*/")
    } else {
      sc!(" */")
    });

    items
  }
}

fn gen_trailing_comments<'a>(node: &SourceRange, context: &mut Context<'a>) -> PrintItems {
  let trailing_comments = node.trailing_comments_fast(context.program);
  gen_comments_as_trailing(node, trailing_comments, context)
}

fn gen_comments_as_trailing<'a>(node: &SourceRange, trailing_comments: CommentsIterator<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();

  // don't do extra work
  if trailing_comments.is_empty() {
    return items;
  }

  let trailing_comments_on_same_line = get_trailing_comments_same_line(node, trailing_comments, context);

  // don't do extra work
  if trailing_comments_on_same_line.is_empty() {
    return items;
  }

  // now handle the comments
  let first_unhandled_comment = trailing_comments_on_same_line.iter().find(|c| !context.has_handled_comment(c));

  if let Some(first_unhandled_comment) = first_unhandled_comment {
    if first_unhandled_comment.is_block() {
      items.push_space();
    }
  }

  items.extend(gen_comment_collection(trailing_comments_on_same_line.into_iter(), Some(node), None, context));
  items
}

fn get_trailing_comments_same_line<'a>(node: &SourceRange, trailing_comments: CommentsIterator<'a>, context: &mut Context<'a>) -> Vec<&'a Comment> {
  // use the roslyn definition of trailing comments
  let node_end_line = node.end_line_fast(context.program);
  let trailing_comments_on_same_line = trailing_comments
    .into_iter()
    .filter(|c| c.start_line_fast(context.program) <= node_end_line) // less than or equal instead of just equal in order to include "forgotten" comments
    .collect::<Vec<_>>();

  // don't do extra work
  if trailing_comments_on_same_line.is_empty() {
    return trailing_comments_on_same_line;
  }

  // block comments after a comma on the same line as the next token are not considered a trailing comment of this node
  // ex. `a, /* 1 */ b`, the comment belongs to `b` and not `a`
  let comma_end = if node.text_fast(context.program) == "," {
    Some(node.end())
  } else {
    context.token_finder.get_next_token_if_comma(&node.range()).map(|t| t.end())
  };
  if let Some(comma_end) = comma_end {
    let next_token_pos = context.token_finder.get_next_token_pos_after(&comma_end.range());
    let next_token_pos_line_start = next_token_pos.start_line_fast(context.program);
    if next_token_pos_line_start == node_end_line {
      return trailing_comments_on_same_line.into_iter().filter(|c| c.start() < comma_end).collect::<Vec<_>>();
    }
  }
  trailing_comments_on_same_line
}

fn get_jsx_empty_expr_comments<'a>(node: &JSXEmptyExpression, context: &Context<'a>) -> CommentsIterator<'a> {
  node.end().leading_comments_fast(context.program)
}

fn get_jsx_expr_container_empty_comments<'a>(node: &JSXExpressionContainer<'a>, context: &Context<'a>) -> CommentsIterator<'a> {
  let start = node.start() + 1;
  let end = node.end().saturating_sub(1);
  CommentsIterator::new(
    context
      .program
      .comments()
      .iter()
      .filter(|comment| comment.start() >= start && comment.end() <= end)
      .collect(),
  )
}

/* helpers */

struct GenArrayLikeNodesOptions<'a> {
  node: Node<'a>,
  nodes: Vec<Option<Node<'a>>>,
  prefer_hanging: bool,
  prefer_single_line: bool,
  space_around: bool,
  trailing_commas: TrailingCommas,
}

fn gen_array_like_nodes<'a>(opts: GenArrayLikeNodesOptions<'a>, context: &mut Context<'a>) -> PrintItems {
  let node = opts.node;
  let nodes = opts.nodes;
  let nodes = if nodes.iter().any(|n| n.is_none()) {
    let commas = get_comma_tokens_from_children_with_tokens(opts.node, context.program);
    nodes
      .into_iter()
      .enumerate()
      .map(|(i, node)| match node {
        Some(node) => NodeOrSeparator::Node(node),
        None => NodeOrSeparator::Separator(commas[i]),
      })
      .collect::<Vec<_>>()
  } else {
    // faster
    nodes.into_iter().map(|n| NodeOrSeparator::Node(n.unwrap())).collect::<Vec<_>>()
  };

  let trailing_commas = if matches!(nodes.last(), Some(NodeOrSeparator::Separator(_))) {
    // maintain the trailing comma in order to maintain the length (ex. [,,,].length === 3)
    TrailingCommas::Always
  } else if allow_trailing_commas(&nodes) {
    opts.trailing_commas
  } else {
    TrailingCommas::Never
  };
  let prefer_hanging = opts.prefer_hanging;
  let space_around = opts.space_around;
  let force_use_new_lines = get_force_use_new_lines(&node.range(), &nodes, opts.prefer_single_line, context);
  let mut items = PrintItems::new();
  let first_member = nodes.first().map(|x| x.range());

  items.extend(gen_surrounded_by_tokens(
    |context| {
      gen_separated_values(
        GenSeparatedValuesParams {
          nodes,
          prefer_hanging,
          force_use_new_lines,
          allow_blank_lines: true,
          separator: trailing_commas.into(),
          single_line_options: SingleLineOptions {
            space_at_start: space_around,
            space_at_end: space_around,
            separator: Signal::SpaceOrNewLine.into(),
          },
          multi_line_options: ir_helpers::MultiLineOptions::surround_newlines_indented(),
          force_possible_newline_at_start: false,
          node_sorter: None,
        },
        context,
      )
    },
    |_| None,
    GenSurroundedByTokensOptions {
      open_token: sc!("["),
      close_token: sc!("]"),
      range: Some(node.range()),
      first_member,
      prefer_single_line_when_empty: true,
      allow_open_token_trailing_comments: true,
      single_line_space_around: false,
    },
    context,
  ));

  return items;

  fn get_force_use_new_lines(node: &SourceRange, nodes: &[NodeOrSeparator], prefer_single_line: bool, context: &mut Context) -> bool {
    if nodes.is_empty() {
      false
    } else if prefer_single_line {
      // if any comments exist on separate lines, then everything becomes multi-line
      has_any_node_comment_on_different_line(nodes, context)
    } else {
      let open_bracket_token = node
        .tokens_fast(context.program)
        .iter()
        .find(|t| t.kind() == Kind::LBrack)
        .expect("Expected to find an open bracket token.");

      // todo: tests for this (ex. [\n,] -> [\n    ,\n])
      node_helpers::get_use_new_lines_for_nodes(&open_bracket_token.range(), &nodes[0], context.program)
    }
  }

  fn allow_trailing_commas(nodes: &[NodeOrSeparator]) -> bool {
    if let Some(NodeOrSeparator::Node(last)) = nodes.last() {
      // this would be a syntax error
      if matches!(last, Node::BindingRestElement(_)) {
        return false;
      }
    }
    true
  }
}

struct GenMemberedBodyOptions<'a, FShouldUseBlankLine>
where
  FShouldUseBlankLine: Fn(Node, Node, &mut Context) -> bool,
{
  node: Node<'a>,
  members: Vec<Node<'a>>,
  start_header_lsil: Option<LineStartIndentLevel>,
  brace_position: BracePosition,
  should_use_blank_line: FShouldUseBlankLine,
  separator: Separator,
}

fn gen_membered_body<'a, FShouldUseBlankLine>(opts: GenMemberedBodyOptions<'a, FShouldUseBlankLine>, context: &mut Context<'a>) -> PrintItems
where
  FShouldUseBlankLine: Fn(Node, Node, &mut Context) -> bool,
{
  let mut items = PrintItems::new();
  let child_tokens = get_tokens_from_children_with_tokens(opts.node, context.program);
  let open_brace_token = child_tokens
    .iter()
    .find(|t| t.kind() == Kind::LCurly)
    .expect("Expected to find an open brace token.");
  let close_brace_token = child_tokens
    .iter()
    .rev()
    .find(|t| t.kind() == Kind::RCurly)
    .expect("Expected to find a close brace token.");

  items.extend(gen_brace_separator(
    GenBraceSeparatorOptions {
      brace_position: opts.brace_position,
      open_brace_token: Some(open_brace_token),
      start_header_lsil: opts.start_header_lsil,
    },
    context,
  ));

  let should_use_blank_line = opts.should_use_blank_line;
  let separator = opts.separator;

  items.extend(gen_block(
    |members, context| {
      gen_members(
        GenMembersOptions {
          inner_range: SourceRange::new(open_brace_token.end(), close_brace_token.start()),
          items: members.into_iter().map(|node| (node, None)).collect(),
          should_use_space: None,
          should_use_new_line: None,
          should_use_blank_line,
          separator,
          is_jsx_children: false,
        },
        context,
      )
    },
    GenBlockOptions {
      range: Some(SourceRange::new(open_brace_token.start(), close_brace_token.end())),
      children: opts.members,
    },
    context,
  ));

  items
}

fn gen_statements<'a>(inner_range: SourceRange, stmts: Vec<Node<'a>>, context: &mut Context<'a>) -> PrintItems {
  let stmt_groups = get_stmt_groups(stmts, context);
  let mut items = PrintItems::new();
  let mut last_node: Option<SourceRange> = None;
  let stmt_group_len = stmt_groups.len();

  for (stmt_group_index, stmt_group) in stmt_groups.into_iter().enumerate() {
    if stmt_group.kind == StmtGroupKind::Imports || stmt_group.kind == StmtGroupKind::Exports {
      // keep the leading comments of the stmt group on the same line
      let comments = get_leading_comments_on_previous_lines(&stmt_group.nodes.first().as_ref().unwrap().start().range(), context);
      let last_comment = comments.iter().filter(|c| !context.has_handled_comment(c)).last().map(|c| c.range());
      items.extend(gen_comments_as_statements(
        comments.into_iter(),
        last_node.as_ref().map(|x| x as &SourceRange),
        context,
      ));
      last_node = last_comment.or(last_node);
    }

    let nodes_len = stmt_group.nodes.len();
    let mut generated_nodes = Vec::with_capacity(nodes_len);
    let mut generated_line_separators = utils::VecMap::with_capacity(nodes_len);
    let sorter = get_node_sorter(stmt_group.kind, context);
    let sorted_indexes = match sorter {
      Some(sorter) => Some(get_sorted_indexes(stmt_group.nodes.iter().map(|n| Some(*n)), sorter, context)),
      None => None,
    };
    for (i, node) in stmt_group.nodes.into_iter().enumerate() {
      let is_empty_stmt = matches!(node, Node::EmptyStatement(_));
      if !is_empty_stmt {
        let mut separator_items = PrintItems::new();
        if let Some(last_node) = &last_node {
          separator_items.push_signal(Signal::NewLine);
          if node_helpers::has_separating_blank_line(last_node, &node, context.program) {
            separator_items.push_signal(Signal::NewLine);
          }
          generated_line_separators.insert(i, separator_items);
        }

        let mut items = PrintItems::new();
        let end_ln = LineNumber::new("endStatement");
        context.end_statement_or_member_lns.push(end_ln);
        items.extend(gen_node(node, context));
        items.push_info(end_ln);
        generated_nodes.push(items);
        context.end_statement_or_member_lns.pop();

        last_node = Some(node.range());
      } else {
        generated_nodes.push(gen_empty_stmt_comments(node, context));

        // ensure if this is last that it generates the trailing comment statements
        if stmt_group_index == stmt_group_len - 1 && i == nodes_len - 1 {
          last_node = Some(node.range());
        }
      }
    }

    // Get the generated statements/members sorted
    let generated_nodes = match sorted_indexes {
      Some(sorted_indexes) => sort_by_sorted_indexes(generated_nodes, sorted_indexes),
      None => generated_nodes,
    };

    // Now combine everything
    for (i, generated_node) in generated_nodes.into_iter().enumerate() {
      if let Some(generated_separator) = generated_line_separators.remove(i) {
        items.extend(generated_separator);
      }
      items.extend(generated_node);
    }
  }

  if let Some(last_node) = &last_node {
    let trailing_comment_range = context
      .token_finder
      .get_first_semi_colon_after(last_node)
      .map(|token| token.range())
      .unwrap_or_else(|| last_node.range());
    items.extend(gen_trailing_comments_as_statements(&trailing_comment_range, context));
    let close_token_leading_comments = get_comments_between(trailing_comment_range.end(), inner_range.end(), context);
    items.extend(gen_comments_as_statements(
      close_token_leading_comments.into_iter(),
      Some(&trailing_comment_range),
      context,
    ));
    if matches!(context.current_node, Node::Program(_)) && trailing_comment_range.next_token_fast(context.program).is_none() {
      let eof_comments = context
        .program
        .comments()
        .iter()
        .filter(|comment| comment.start() >= trailing_comment_range.end);
      items.extend(gen_comments_as_statements(eof_comments, Some(&trailing_comment_range), context));
    }
  }

  if stmt_group_len == 0 {
    items.extend(gen_comments_as_statements(
      inner_range.end.leading_comments_fast(context.program),
      None,
      context,
    ));
  }

  return items;

  fn get_node_sorter<'a>(
    group_kind: StmtGroupKind,
    context: &Context<'a>,
  ) -> Option<Box<dyn Fn((usize, Option<Node<'a>>), (usize, Option<Node<'a>>), ProgramInfo<'a>) -> std::cmp::Ordering>> {
    match group_kind {
      StmtGroupKind::Imports => get_node_sorter_from_order(context.config.module_sort_import_declarations, NamedTypeImportsExportsOrder::None),
      StmtGroupKind::Exports => get_node_sorter_from_order(context.config.module_sort_export_declarations, NamedTypeImportsExportsOrder::None),
      StmtGroupKind::Other => None,
    }
  }
}

fn gen_member_or_member_expr_stmt_comments(node: Node, context: &mut Context) -> PrintItems {
  if matches!(node, Node::EmptyStatement(_)) {
    return gen_empty_stmt_comments(node, context);
  }
  let mut items = PrintItems::new();
  let leading_comments = node.leading_comments_fast(context.program);
  items.extend(gen_comments_as_statements(leading_comments.clone(), None, context));
  let trailing_comments = get_trailing_comments_same_line(&node.range(), node.trailing_comments_fast(context.program), context);
  if !trailing_comments.is_empty() {
    if !leading_comments.is_empty() {
      items.push_signal(Signal::NewLine);
    }
    items.extend(gen_comment_collection(trailing_comments.into_iter(), None, None, context));
  }

  items
}

fn gen_switch_case_leading_comments<'a>(previous: Node<'a>, node: Node<'a>, context: &mut Context<'a>) -> PrintItems {
  let (Node::SwitchCase(previous_case), Node::SwitchCase(_)) = (previous, node) else {
    return PrintItems::new();
  };
  let Some(case_token) = node
    .tokens_fast(context.program)
    .iter()
    .find(|token| matches!(token.text_fast(context.program), "case" | "default"))
  else {
    return PrintItems::new();
  };
  let comments = get_comments_between(previous.end(), case_token.start(), context)
    .into_iter()
    .filter(|comment| !context.has_handled_comment(comment))
    .collect::<Vec<_>>();
  if comments.is_empty() {
    return PrintItems::new();
  }

  let case_column = case_token.start_column_fast(context.program);
  let case_start_line = case_token.start_line_fast(context.program);
  let previous_case_has_block_body = previous_case
    .consequent
    .first()
    .is_some_and(|stmt| matches!(stmt, Statement::BlockStatement(_)));
  let mut items = PrintItems::new();
  let mut last_range: Option<SourceRange> = None;
  let mut use_case_level_for_rest = false;

  for comment in &comments {
    let is_case_level_comment = previous_case_has_block_body || use_case_level_for_rest || comment.start_column_fast(context.program) <= case_column;
    use_case_level_for_rest |= is_case_level_comment;
    let generated_comment = gen_comment_based_on_last_node(comment, &last_range, GenCommentBasedOnLastNodeOptions { separate_with_newlines: true }, context);
    if is_case_level_comment {
      items.extend(generated_comment);
    } else {
      items.extend(ir_helpers::with_indent(generated_comment));
    }
    last_range = Some(comment.range());
  }

  if let Some(last_range) = last_range {
    if last_range.end_line_fast(context.program) + 1 < case_start_line {
      items.push_signal(Signal::NewLine);
      items.push_signal(Signal::NewLine);
    }
  }

  items
}

fn gen_empty_stmt_comments(node: Node, context: &mut Context) -> PrintItems {
  let leading_comments = node.leading_comments_fast(context.program);
  let trailing_comments = node.trailing_comments_fast(context.program);
  gen_comments_as_statements(leading_comments.chain(trailing_comments), None, context)
}

#[derive(PartialEq, Debug)]
enum StmtGroupKind {
  Imports,
  Exports,
  Other,
}

struct StmtGroup<'a> {
  kind: StmtGroupKind,
  nodes: Vec<Node<'a>>,
}

fn get_stmt_groups<'a>(stmts: Vec<Node<'a>>, context: &mut Context<'a>) -> Vec<StmtGroup<'a>> {
  let mut groups: Vec<StmtGroup<'a>> = Vec::new();
  let mut current_group: Option<StmtGroup> = None;
  let mut previous_last_end_line: Option<usize> = None;

  for stmt in stmts {
    let last_end_line = previous_last_end_line.take();
    let stmt_group_kind = match stmt {
      Node::ImportDeclaration(decl) if decl.specifiers.as_ref().is_some_and(|s| !s.is_empty()) => StmtGroupKind::Imports,
      Node::ExportAllDeclaration(_) => StmtGroupKind::Exports,
      Node::ExportNamedDeclaration(e) if e.source.is_some() => StmtGroupKind::Exports,
      _ => StmtGroupKind::Other,
    };
    previous_last_end_line = match stmt_group_kind {
      StmtGroupKind::Imports | StmtGroupKind::Exports => Some(stmt.end_line_fast(context.program)),
      StmtGroupKind::Other => None,
    };

    if let Some(group) = current_group.as_mut() {
      let is_same_group = group.kind == stmt_group_kind
        && (stmt_group_kind == StmtGroupKind::Other || last_end_line.is_none() || last_end_line.unwrap() + 1 >= stmt.start_line_fast(context.program));
      if is_same_group {
        group.nodes.push(stmt);
      } else {
        groups.push(current_group.take().unwrap());
        current_group = Some(StmtGroup {
          kind: stmt_group_kind,
          nodes: vec![stmt],
        })
      }
    } else {
      current_group = Some(StmtGroup {
        kind: stmt_group_kind,
        nodes: vec![stmt],
      });
    }
  }

  if let Some(current_group) = current_group {
    groups.push(current_group);
  }

  groups
}

struct GenMembersOptions<'a, FShouldUseBlankLine>
where
  FShouldUseBlankLine: Fn(Node<'a>, Node<'a>, &mut Context<'a>) -> bool,
{
  inner_range: SourceRange,
  items: Vec<(Node<'a>, Option<PrintItems>)>,
  should_use_space: Option<Box<dyn Fn(Node<'a>, Node<'a>, &mut Context<'a>) -> bool>>, // todo: Remove putting functions on heap by using type parameters?
  should_use_new_line: Option<Box<dyn Fn(Node<'a>, Node<'a>, &mut Context<'a>) -> bool>>,
  should_use_blank_line: FShouldUseBlankLine,
  separator: Separator,
  is_jsx_children: bool,
}

fn gen_members<'a, FShouldUseBlankLine>(opts: GenMembersOptions<'a, FShouldUseBlankLine>, context: &mut Context<'a>) -> PrintItems
where
  FShouldUseBlankLine: Fn(Node<'a>, Node<'a>, &mut Context<'a>) -> bool,
{
  let mut last_node: Option<Node> = None;
  let mut items = PrintItems::new();
  let children_len = opts.items.len();

  let mut member_items = opts.items.into_iter().enumerate().peekable();

  while let Some((i, (node, optional_print_items))) = member_items.next() {
    // class declarations may have empty statements
    let is_empty_stmt = matches!(node, Node::EmptyStatement(_));
    if !is_empty_stmt {
      if let Some(last_node) = last_node {
        if is_ignore_jsx_expr_container(last_node, context) && matches!(node, Node::JSXText(_)) {
          // ignore
        } else if should_use_new_line(&opts.should_use_new_line, last_node, node, context) {
          items.push_signal(Signal::NewLine);

          if (opts.should_use_blank_line)(last_node, node, context) {
            items.push_signal(Signal::NewLine);
          }
        } else if let Some(should_use_space) = &opts.should_use_space {
          if should_use_space(last_node, node, context) {
            if opts.is_jsx_children {
              items.extend(jsx_space_separator(last_node, node, context))
            } else {
              items.push_signal(Signal::SpaceOrNewLine);
            }
          }
        }
      }

      let next_node = member_items.peek().map(|(_, (n, _))| n);
      let end_ln = LineNumber::new("endMember");
      context.end_statement_or_member_lns.push(end_ln);
      let switch_case_leading_comments = if let Some(last_node) = last_node {
        gen_switch_case_leading_comments(last_node, node, context)
      } else {
        PrintItems::new()
      };
      items.extend(switch_case_leading_comments);
      items.extend(if let Some(print_items) = optional_print_items {
        print_items
      } else if opts.separator.is_none() || is_ignore_jsx_expr_container(node, context) && next_node.map(|n| matches!(n, Node::JSXText(_))).unwrap_or(false) {
        gen_node(node, context)
      } else {
        let generated_separator = get_generated_separator(&opts.separator, i == children_len - 1, &condition_resolvers::true_resolver());
        gen_node_with_separator(node, generated_separator, context)
      });
      items.push_info(end_ln);
      context.end_statement_or_member_lns.pop();

      last_node = Some(node);
    } else {
      items.extend(gen_member_or_member_expr_stmt_comments(node, context));

      // ensure if this is last that it generates the trailing comment statements
      if i == children_len - 1 {
        last_node = Some(node);
      }
    }
  }

  if let Some(last_node) = &last_node {
    let last_node_range = last_node.range();
    items.extend(gen_trailing_comments_as_statements(&last_node_range, context));
    let close_token_leading_comments = get_comments_between(last_node_range.end(), opts.inner_range.end(), context);
    let close_token_leading_comments = gen_comments_as_statements(close_token_leading_comments.into_iter(), Some(&last_node_range), context);
    if matches!(last_node, Node::SwitchCase(case) if !case.consequent.is_empty()) {
      items.extend(ir_helpers::with_indent(close_token_leading_comments));
    } else {
      items.extend(close_token_leading_comments);
    }
  }

  if children_len == 0 {
    items.extend(gen_comments_as_statements(
      opts.inner_range.end.leading_comments_fast(context.program),
      None,
      context,
    ));
  }

  return items;

  fn should_use_new_line<'a>(
    should_use_new_line: &Option<Box<dyn Fn(Node<'a>, Node<'a>, &mut Context<'a>) -> bool>>,
    last_node: Node<'a>,
    next_node: Node<'a>,
    context: &mut Context<'a>,
  ) -> bool {
    if let Some(should_use) = &should_use_new_line {
      (should_use)(last_node, next_node, context)
    } else {
      true
    }
  }
}

struct GenParametersOrArgumentsOptions<'a, F>
where
  F: FnOnce(&mut Context<'a>) -> Option<PrintItems>,
{
  node: Node<'a>,
  range: Option<SourceRange>,
  nodes: Vec<Node<'a>>,
  custom_close_paren: F,
  is_parameters: bool,
}

fn gen_parameters_or_arguments<'a, F>(opts: GenParametersOrArgumentsOptions<'a, F>, context: &mut Context<'a>) -> PrintItems
where
  F: FnOnce(&mut Context<'a>) -> Option<PrintItems>,
{
  let nodes = opts.nodes;
  let is_parameters = opts.is_parameters;
  let prefer_hanging_config = if is_parameters {
    context.config.parameters_prefer_hanging
  } else {
    context.config.arguments_prefer_hanging
  };
  let prefer_hanging = match prefer_hanging_config {
    PreferHanging::Never => false,
    PreferHanging::OnlySingleItem => only_single_item_and_no_comments(&nodes, context.program),
    PreferHanging::Always => true,
  };
  let prefer_single_item_hanging = prefer_hanging_config == PreferHanging::OnlySingleItem && prefer_hanging;
  let prefer_single_line = prefer_single_item_hanging
    || (is_parameters && context.config.parameters_prefer_single_line || !is_parameters && context.config.arguments_prefer_single_line);
  let range = opts.range;
  let force_use_new_lines =
    get_use_new_lines_for_nodes_with_preceeding_token("(", &nodes, prefer_single_line, context) || has_close_paren_leading_line_comment(&nodes, context);
  let custom_close_paren = opts.custom_close_paren;
  let first_member_range = nodes.iter().map(|n| n.range()).next();
  let nodes_length = nodes.len();
  let space_around = if nodes_length > 0 && is_parameters {
    context.config.parameters_space_around
  } else if nodes_length > 0 {
    context.config.arguments_space_around
  } else {
    false
  };
  let trailing_commas = get_trailing_commas(opts.node, &nodes, is_parameters, context);

  return gen_surrounded_by_tokens(
    |context| {
      let mut items = PrintItems::new();

      if !force_use_new_lines && nodes.len() == 1 && is_arrow_function_with_expr_body(nodes[0]) {
        let start_ln = LineNumber::new("startArrow");
        let start_lsil = LineStartIndentLevel::new("startArrow");
        let generated_node = gen_node(nodes.into_iter().next().unwrap(), context);

        items.push_info(start_ln);
        items.push_info(start_lsil);
        items.push_signal(Signal::PossibleNewLine);
        if space_around {
          items.push_signal(Signal::SpaceIfNotTrailing);
        }
        items.push_condition(conditions::indent_if_start_of_line(generated_node));
        items.push_condition(if_true_or(
          "isDifferentLineAndStartLineIndentation",
          Rc::new(move |context| {
            let start_ln = context.resolved_line_number(start_ln)?;
            let start_lsil = context.resolved_line_start_indent_level(start_lsil)?;
            let is_different_line = start_ln != context.writer_info.line_number;
            let is_different_start_line_indentation = start_lsil != context.writer_info.line_start_indent_level;
            Some(is_different_line && is_different_start_line_indentation)
          }),
          Signal::NewLine.into(),
          if space_around { Signal::SpaceIfNotTrailing.into() } else { PrintItems::new() },
        ));
      } else {
        let last_comma_token = nodes.last().and_then(|n| context.token_finder.get_next_token_if_comma(n));
        items.extend(gen_separated_values(
          GenSeparatedValuesParams {
            nodes: nodes.into_iter().map(NodeOrSeparator::Node).collect(),
            prefer_hanging,
            force_use_new_lines,
            allow_blank_lines: false,
            separator: trailing_commas.into(),
            single_line_options: SingleLineOptions {
              space_at_start: space_around,
              space_at_end: space_around,
              separator: Signal::SpaceOrNewLine.into(),
            },
            multi_line_options: if prefer_single_item_hanging {
              MultiLineOptions::maintain_line_breaks()
            } else {
              MultiLineOptions::surround_newlines_indented()
            },
            force_possible_newline_at_start: is_parameters,
            node_sorter: None,
          },
          context,
        ));
        // the comma may disappear, so generate any trailing comments on the same line
        if let Some(last_comma_token) = last_comma_token {
          items.extend(gen_trailing_comments_same_line(&last_comma_token.range(), context));
        }
      }

      items
    },
    custom_close_paren,
    GenSurroundedByTokensOptions {
      open_token: sc!("("),
      close_token: sc!(")"),
      range,
      first_member: first_member_range,
      prefer_single_line_when_empty: true,
      allow_open_token_trailing_comments: true,
      single_line_space_around: false,
    },
    context,
  );

  fn get_trailing_commas(node: Node, nodes: &[Node], is_parameters: bool, context: &mut Context) -> TrailingCommas {
    if let Some(last) = nodes.last() {
      // this would be a syntax error
      if is_param_rest_pat(*last) {
        return TrailingCommas::Never;
      }
    }

    return if is_dynamic_import(node) {
      TrailingCommas::Never // not allowed
    } else if is_parameters {
      context.config.parameters_trailing_commas
    } else {
      context.config.arguments_trailing_commas
    };

    fn is_dynamic_import(node: Node) -> bool {
      // oxc models dynamic `import(...)` as a dedicated ImportExpression
      matches!(node, Node::ImportExpression(_))
    }

    fn is_param_rest_pat(param: Node) -> bool {
      // oxc represents a function rest parameter as a FormalParameterRest and a
      // destructuring rest as a BindingRestElement.
      matches!(param, Node::FormalParameterRest(_) | Node::BindingRestElement(_))
    }
  }

  fn only_single_item_and_no_comments<'a>(nodes: &[Node<'a>], program: ProgramInfo<'a>) -> bool {
    if nodes.len() != 1 {
      return false;
    }
    // check for leading or trailing comments on the only child node
    let child = nodes[0];
    if !child.leading_comments_fast(program).is_empty() || !child.trailing_comments_fast(program).is_empty() {
      return false;
    }
    // search after the trailing comma if it exists
    match child.next_token_fast(program) {
      Some(t) if t.kind() == Kind::Comma => {
        if !t.trailing_comments_fast(program).is_empty() {
          return false;
        }
        let close_paren = child.next_tokens_fast(program).iter().find(|token| token.kind() == Kind::RParen);
        !close_paren.is_some_and(|token| {
          program
            .comments()
            .iter()
            .any(|comment| comment.is_line() && comment.start() > child.end() && comment.start() < token.start())
        })
      }
      _ => true,
    }
  }

  fn has_close_paren_leading_line_comment<'a>(nodes: &[Node<'a>], context: &mut Context<'a>) -> bool {
    let Some(last_node) = nodes.last() else {
      return false;
    };
    context
      .token_finder
      .get_next_token_if_comma(last_node)
      .is_some_and(|token| token.trailing_comments_fast(context.program).any(|c| c.is_line()))
      || context.token_finder.get_first_close_paren_after(last_node).is_some_and(|token| {
        token.leading_comments_fast(context.program).any(|c| c.is_line())
          || context
            .program
            .comments()
            .iter()
            .any(|c| c.is_line() && c.start() > last_node.end() && c.start() < token.start())
      })
  }
}

struct GenCloseParenWithTypeOptions<'a> {
  start_lsil: LineStartIndentLevel,
  type_node: Option<Node<'a>>,
  type_node_separator: Option<PrintItems>,
  param_count: usize,
}

fn gen_close_paren_with_type<'a>(opts: GenCloseParenWithTypeOptions<'a>, context: &mut Context<'a>) -> PrintItems {
  // todo: clean this up a bit
  let type_node_start_ln = LineNumber::new("typeNodeStart");
  let has_type_node = opts.type_node.is_some();
  let type_node_end_ln = LineNumber::new("typeNodeEnd");
  let start_lsil = opts.start_lsil;
  let generated_type_node = gen_type_node(
    opts.type_node,
    opts.type_node_separator,
    type_node_start_ln,
    type_node_end_ln,
    opts.param_count,
    context,
  );
  let mut items = PrintItems::new();

  items.push_condition(if_true(
    "newLineIfHeaderHangingAndTypeNodeMultipleLines",
    Rc::new(move |context| {
      if !has_type_node || context.writer_info.is_start_of_line() {
        return Some(false);
      }

      if let Some(is_hanging) = condition_helpers::is_hanging(context, start_lsil, None) {
        if let Some(is_multiple_lines) = condition_helpers::is_multiple_lines(context, type_node_start_ln, type_node_end_ln) {
          return Some(is_hanging && is_multiple_lines);
        }
      }
      None
    }),
    Signal::NewLine.into(),
  ));
  items.push_sc(sc!(")"));
  items.extend(generated_type_node);
  return items;

  fn gen_type_node<'a>(
    type_node: Option<Node<'a>>,
    type_node_separator: Option<PrintItems>,
    type_node_start_ln: LineNumber,
    type_node_end_ln: LineNumber,
    param_count: usize,
    context: &mut Context<'a>,
  ) -> PrintItems {
    let mut items = PrintItems::new();
    return if let Some(type_node) = type_node {
      let use_new_line_group = get_use_new_line_group(param_count, type_node, context);
      items.push_info(type_node_start_ln);
      if let Some(type_node_separator) = type_node_separator {
        items.extend(type_node_separator);
      } else {
        if context.config.type_annotation_space_before_colon {
          items.push_space();
        }
        items.push_sc(sc!(":"));
        items.push_signal(Signal::SpaceIfNotTrailing);
      }
      let generated_type_node = gen_node(type_node, context);
      items.extend(generated_type_node);
      items.push_info(type_node_end_ln);

      if use_new_line_group { new_line_group(items) } else { items }
    } else {
      items
    };

    fn get_use_new_line_group(param_count: usize, type_node: Node, context: &mut Context) -> bool {
      if param_count == 0 {
        false
      } else if context.config.parameters_prefer_hanging == PreferHanging::Always && param_count > 1 {
        // This was done to prevent the second argument becoming hanging, which doesn't
        // look good especially when the return type then becomes multi-line.
        match type_node {
          Node::TSUnionType(_) | Node::TSIntersectionType(_) => false,
          Node::TSTypeAnnotation(type_ann) => !matches!(type_ann.type_annotation, TSType::TSUnionType(_) | TSType::TSIntersectionType(_)),
          _ => true,
        }
      } else {
        true
      }
    }
  }
}

#[derive(PartialEq)]
enum SeparatorValue {
  SemiColon(SemiColons),
  Comma(TrailingCommas),
}

struct Separator {
  single_line: Option<SeparatorValue>,
  multi_line: Option<SeparatorValue>,
}

impl Separator {
  pub fn none() -> Self {
    Separator {
      single_line: None,
      multi_line: None,
    }
  }

  pub fn is_none(&self) -> bool {
    self.single_line.is_none() && self.multi_line.is_none()
  }
}

impl From<SemiColons> for Separator {
  fn from(value: SemiColons) -> Separator {
    Separator {
      single_line: Some(SeparatorValue::SemiColon(value)),
      multi_line: Some(SeparatorValue::SemiColon(value)),
    }
  }
}

impl From<TrailingCommas> for Separator {
  fn from(value: TrailingCommas) -> Separator {
    Separator {
      single_line: Some(SeparatorValue::Comma(value)),
      multi_line: Some(SeparatorValue::Comma(value)),
    }
  }
}

struct GenSeparatedValuesParams<'a> {
  nodes: Vec<NodeOrSeparator<'a>>,
  prefer_hanging: bool,
  force_use_new_lines: bool,
  allow_blank_lines: bool,
  separator: Separator,
  single_line_options: ir_helpers::SingleLineOptions,
  multi_line_options: ir_helpers::MultiLineOptions,
  force_possible_newline_at_start: bool,
  node_sorter: Option<Box<dyn Fn((usize, Option<Node<'a>>), (usize, Option<Node<'a>>), ProgramInfo<'a>) -> std::cmp::Ordering>>,
}

enum NodeOrSeparator<'a> {
  Node(Node<'a>),
  Separator(&'a Token),
}

impl<'a> SourceRanged for NodeOrSeparator<'a> {
  fn start(&self) -> SourcePos {
    match self {
      NodeOrSeparator::Node(node) => node.start(),
      NodeOrSeparator::Separator(token) => token.start(),
    }
  }

  fn end(&self) -> SourcePos {
    match self {
      NodeOrSeparator::Node(node) => node.end(),
      NodeOrSeparator::Separator(token) => token.end(),
    }
  }
}

impl<'a> NodeOrSeparator<'a> {
  pub fn as_node(&self) -> Option<Node<'a>> {
    match self {
      NodeOrSeparator::Node(node) => Some(*node),
      _ => None,
    }
  }
}

#[inline]
fn gen_separated_values<'a>(opts: GenSeparatedValuesParams<'a>, context: &mut Context<'a>) -> PrintItems {
  gen_separated_values_with_result(opts, context).items
}

fn gen_separated_values_with_result<'a>(opts: GenSeparatedValuesParams<'a>, context: &mut Context<'a>) -> GenSeparatedValuesResult {
  let nodes = opts.nodes;
  let separator = opts.separator;
  let indent_width = context.config.indent_width;
  let compute_lines_span = opts.allow_blank_lines; // save time otherwise
  let node_sorter = opts.node_sorter;

  // would need to make this take into account the new position of the nodes
  #[cfg(debug_assertions)]
  if node_sorter.is_some() && compute_lines_span {
    panic!("Not implemented scenario. Cannot computed lines span and allow blank lines");
  }
  let sorted_indexes = node_sorter.map(|sorter| get_sorted_indexes(nodes.iter().map(|d| d.as_node()), sorter, context));

  ir_helpers::gen_separated_values(
    |is_multi_line_or_hanging_ref| {
      let is_multi_line_or_hanging = is_multi_line_or_hanging_ref.create_resolver();
      let mut generated_nodes = Vec::new();
      let nodes_count = nodes.len();

      for (i, value) in nodes.into_iter().enumerate() {
        let node_index = match &sorted_indexes {
          Some(old_to_new_index) => *old_to_new_index.get(i).unwrap(),
          None => i,
        };
        let (allow_inline_multi_line, allow_inline_single_line) = if let NodeOrSeparator::Node(value) = value {
          let is_last_value = node_index + 1 == nodes_count; // allow the last node to be single line
          (allows_inline_multi_line(value, context, nodes_count > 1), is_last_value)
        } else {
          (false, false)
        };
        let lines_span = if compute_lines_span {
          value.as_node().map(|x| ir_helpers::LinesSpan {
            start_line: x.start_line_with_comments(context),
            end_line: x.end_line_with_comments(context),
          })
        } else {
          None
        };

        let items = if separator.is_none() {
          if let NodeOrSeparator::Node(value) = value {
            gen_node(value, context)
          } else {
            panic!("Unsupported scenario.")
          }
        } else {
          let generated_separator = get_generated_separator(&separator, node_index == nodes_count - 1, &is_multi_line_or_hanging);
          match value {
            NodeOrSeparator::Node(value) => gen_node_with_separator(value, generated_separator, context),
            NodeOrSeparator::Separator(separator_token) => {
              let mut items = PrintItems::new();
              // don't use gen_leading_comments here because we don't want a space between the block comment and separator (comma)
              let leading_comments = separator_token.leading_comments_fast(context.program);
              items.extend(gen_comment_collection(leading_comments, None, Some(&separator_token.range()), context));
              items.extend(generated_separator);
              items.extend(gen_trailing_comments_same_line(&separator_token.range(), context));
              items
            }
          }
        };

        let use_new_line_group = match value {
          // Prefer going inline multi-line for certain expressions in arguments
          // when initially single line.
          // Example: call({\n}) instead of call(\n  {\n  }\n)
          NodeOrSeparator::Node(n) => !matches!(n, Node::ObjectExpression(_) | Node::ArrayExpression(_)),
          _ => true,
        };

        generated_nodes.push(ir_helpers::GeneratedValue {
          items: if use_new_line_group { ir_helpers::new_line_group(items) } else { items },
          lines_span,
          allow_inline_multi_line,
          allow_inline_single_line,
        });
      }

      match sorted_indexes {
        Some(sorted_indexes) => sort_by_sorted_indexes(generated_nodes, sorted_indexes),
        None => generated_nodes,
      }
    },
    ir_helpers::GenSeparatedValuesOptions {
      prefer_hanging: opts.prefer_hanging,
      force_use_new_lines: opts.force_use_new_lines,
      allow_blank_lines: opts.allow_blank_lines,
      indent_width,
      single_line_options: opts.single_line_options,
      multi_line_options: opts.multi_line_options,
      force_possible_newline_at_start: opts.force_possible_newline_at_start,
    },
  )
}

fn get_sorted_indexes<'a: 'b, 'b>(
  nodes: impl Iterator<Item = Option<Node<'a>>>,
  sorter: Box<dyn Fn((usize, Option<Node<'a>>), (usize, Option<Node<'a>>), ProgramInfo<'a>) -> std::cmp::Ordering>,
  context: &mut Context<'a>,
) -> utils::VecMap<usize> {
  let mut nodes_with_indexes = nodes.enumerate().collect::<Vec<_>>();
  nodes_with_indexes.sort_unstable_by(|a, b| sorter((a.0, a.1), (b.0, b.1), context.program));
  let mut old_to_new_index = utils::VecMap::with_capacity(nodes_with_indexes.len());

  for (new_index, old_index) in nodes_with_indexes.into_iter().map(|(index, _)| index).enumerate() {
    old_to_new_index.insert(old_index, new_index);
  }

  old_to_new_index
}

fn sort_by_sorted_indexes<T>(items: Vec<T>, sorted_indexes: utils::VecMap<usize>) -> Vec<T> {
  let mut sorted_items = Vec::with_capacity(items.len());
  for _ in 0..items.len() {
    sorted_items.push(None);
  }

  for (i, generated_node) in items.into_iter().enumerate() {
    sorted_items[*sorted_indexes.get(i).unwrap_or(&i)] = Some(generated_node);
  }

  sorted_items.into_iter().map(|x| x.unwrap()).collect()
}

fn gen_node_with_separator<'a>(value: Node<'a>, generated_separator: PrintItems, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  let comma_token = get_comma_token(value, context);

  // get the trailing comments after the comma token (if the separator in the file is currently a comma)
  let generated_trailing_comments = if let Some(comma_token) = comma_token {
    gen_trailing_comments(&comma_token.range(), context)
  } else {
    PrintItems::new()
  };

  // if the current node is ignored and already has a semi-colon, then skip adding a separator
  let is_ignored_with_semi_colon =
    value.text_fast(context.program).ends_with(';') && get_has_ignore_comment(&value.leading_comments_fast(context.program), value, context);
  if is_ignored_with_semi_colon {
    items.extend(gen_node(value, context));
  } else {
    let generated_separator = generated_separator.into_rc_path();
    items.extend(gen_node_with_inner_gen(value, context, move |mut items, _| {
      // this Rc clone is necessary because we can't move the captured generated_separator out of this closure
      items.push_optional_path(generated_separator);
      items
    }));
  }

  items.extend(generated_trailing_comments);

  return items;

  fn get_comma_token<'a>(element: Node<'a>, context: &mut Context<'a>) -> Option<&'a Token> {
    match context.token_finder.get_next_token_if_comma(&element) {
      Some(comma) => Some(comma),
      None => context.token_finder.get_last_token_within_if_comma(&element), // may occur for type literals
    }
  }
}

/// Some nodes don't have a TsTypeAnn, but instead a Box<TsType>
fn gen_type_ann_with_colon_if_exists_for_type<'a>(type_ann: Option<Node<'a>>, context: &mut Context<'a>) -> PrintItems {
  if let Some(type_ann) = type_ann {
    gen_type_ann_with_colon_for_type(type_ann, context)
  } else {
    PrintItems::new()
  }
}

fn gen_type_ann_with_colon_for_type<'a>(type_ann: Node<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  if context.config.type_annotation_space_before_colon {
    items.push_space();
  }
  let colon_token = context.token_finder.get_previous_token_if_colon(&type_ann);
  #[cfg(debug_assertions)]
  assert_has_op(":", colon_token, context);
  items.extend(gen_type_ann_with_colon(type_ann, colon_token, context));
  items
}

fn gen_type_ann_with_colon_if_exists<'a>(type_ann: Option<&'a TSTypeAnnotation<'a>>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  if let Some(type_ann) = type_ann {
    if context.config.type_annotation_space_before_colon {
      items.push_space();
    }
    let colon_token = context.token_finder.get_first_colon_token_within(type_ann);
    #[cfg(debug_assertions)]
    assert_has_op(":", colon_token, context);
    items.extend(gen_type_ann_with_colon(ts_type_to_node(&type_ann.type_annotation), colon_token, context));
  }
  items
}

fn gen_type_ann_with_colon<'a>(type_ann: Node<'a>, colon_token: Option<&Token>, context: &mut Context<'a>) -> PrintItems {
  gen_assignment_like_with_token(type_ann, sc!(":"), colon_token, context)
}

struct GenBraceSeparatorOptions<'a> {
  brace_position: BracePosition,
  open_brace_token: Option<&'a Token>,
  start_header_lsil: Option<LineStartIndentLevel>,
}

fn gen_brace_separator<'a>(opts: GenBraceSeparatorOptions<'a>, context: &mut Context) -> PrintItems {
  match opts.brace_position {
    BracePosition::SameLineUnlessHanging => {
      if let Some(start_header_lsil) = opts.start_header_lsil {
        conditions::new_line_if_hanging_space_otherwise(conditions::NewLineIfHangingSpaceOtherwiseOptions {
          start_lsil: start_header_lsil,
          end_lsil: None,
          space_char: Some(space_if_not_start_line()),
        })
        .into()
      } else {
        space_if_not_start_line()
      }
    }
    BracePosition::SameLine => space_if_not_start_line(),
    BracePosition::NextLine => Signal::NewLine.into(),
    BracePosition::Maintain => {
      if let Some(open_brace_token) = opts.open_brace_token {
        if node_helpers::is_first_node_on_line(&open_brace_token.range(), context.program) {
          Signal::NewLine.into()
        } else {
          space_if_not_start_line()
        }
      } else {
        space_if_not_start_line()
      }
    }
  }
}

fn space_if_not_start_line() -> PrintItems {
  if_true("spaceIfNotStartLine", condition_resolvers::is_not_start_of_line(), " ".into()).into()
}

struct GenNodeInParensOptions {
  inner_range: SourceRange,
  prefer_hanging: bool,
  allow_open_paren_trailing_comments: bool,
  single_line_space_around: bool,
}

fn gen_node_in_parens<'a>(gen_node: impl FnOnce(&mut Context<'a>) -> PrintItems, opts: GenNodeInParensOptions, context: &mut Context<'a>) -> PrintItems {
  let inner_range = opts.inner_range;
  let paren_range = get_paren_range(&inner_range, context);
  let force_use_new_lines = get_force_use_new_lines(inner_range, &paren_range, context);

  return gen_surrounded_by_tokens(
    |context| {
      let generated_node = gen_node(context);
      if force_use_new_lines {
        surround_with_new_lines(with_indent(generated_node))
      } else if opts.prefer_hanging {
        generated_node
      } else {
        ir_helpers::surround_with_newlines_indented_if_multi_line(generated_node, context.config.indent_width)
      }
    },
    |_| None,
    GenSurroundedByTokensOptions {
      open_token: sc!("("),
      close_token: sc!(")"),
      range: paren_range,
      first_member: Some(inner_range),
      prefer_single_line_when_empty: true,
      allow_open_token_trailing_comments: opts.allow_open_paren_trailing_comments,
      single_line_space_around: opts.single_line_space_around,
    },
    context,
  );

  fn get_force_use_new_lines(inner_range: SourceRange, paren_range: &Option<SourceRange>, context: &mut Context) -> bool {
    if !context.config.parentheses_prefer_single_line {
      if let Some(paren_range) = &paren_range {
        if node_helpers::get_use_new_lines_for_nodes(&paren_range.start().range(), &inner_range, context.program) {
          return true;
        }
      }
    }

    has_any_node_comment_on_different_line(&[inner_range], context)
      || context
        .token_finder
        .get_next_token_if_close_paren(&inner_range)
        .is_some_and(|token| token.leading_comments_fast(context.program).any(|c| c.is_line()))
  }
}

fn get_paren_range<'a>(inner_range: &SourceRange, context: &mut Context<'a>) -> Option<SourceRange> {
  let open_paren = context.token_finder.get_previous_token_if_open_paren(inner_range);
  let close_paren = context.token_finder.get_next_token_if_close_paren(inner_range);

  if let Some(open_paren) = open_paren {
    if let Some(close_paren) = close_paren {
      return Some(SourceRange::new(open_paren.start(), close_paren.end()));
    }
  }

  None
}

struct GenExtendsOrImplementsOptions<'a> {
  text: &'static StringContainer,
  type_items: Vec<Node<'a>>,
  start_header_lsil: LineStartIndentLevel,
  prefer_hanging: bool,
}

fn gen_extends_or_implements<'a>(opts: GenExtendsOrImplementsOptions<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();

  if opts.type_items.is_empty() {
    return items;
  }

  items.push_condition(conditions::new_line_if_hanging_space_otherwise(
    conditions::NewLineIfHangingSpaceOtherwiseOptions {
      start_lsil: opts.start_header_lsil,
      end_lsil: None,
      space_char: Some(conditions::if_above_width_or(context.config.indent_width, Signal::SpaceOrNewLine.into(), " ".into()).into()),
    },
  ));
  // the newline group will force it to put the extends or implements on a new line
  items.push_condition(conditions::indent_if_start_of_line(ir_helpers::new_line_group({
    let mut items = PrintItems::new();
    items.push_sc(opts.text);
    items.extend(gen_separated_values(
      GenSeparatedValuesParams {
        nodes: opts.type_items.into_iter().map(NodeOrSeparator::Node).collect(),
        prefer_hanging: opts.prefer_hanging,
        force_use_new_lines: false,
        allow_blank_lines: false,
        separator: TrailingCommas::Never.into(),
        single_line_options: ir_helpers::SingleLineOptions::separated_line_starting_with_space(),
        multi_line_options: ir_helpers::MultiLineOptions::new_line_start(),
        force_possible_newline_at_start: false,
        node_sorter: None,
      },
      context,
    ));
    items
  })));

  items
}

struct GenObjectLikeNodeOptions<'a> {
  node: Node<'a>,
  members: Vec<Node<'a>>,
  separator: Separator,
  prefer_hanging: bool,
  prefer_single_line: bool,
  force_single_line: bool,
  force_multi_line: bool,
  surround_single_line_with_spaces: bool,
  surround_single_line_with_space_at_end: bool,
  allow_blank_lines: bool,
  node_sorter: Option<Box<dyn Fn((usize, Option<Node<'a>>), (usize, Option<Node<'a>>), ProgramInfo<'a>) -> std::cmp::Ordering>>,
}

fn gen_object_like_node<'a>(opts: GenObjectLikeNodeOptions<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();

  let child_tokens = get_tokens_from_children_with_tokens(opts.node, context.program);
  let open_brace_token = child_tokens.iter().find(|t| t.kind() == Kind::LCurly);
  let close_brace_token = child_tokens.iter().rev().find(|t| t.kind() == Kind::RCurly);
  let force_multi_line =
    opts.force_multi_line || !opts.force_single_line && get_use_new_lines_for_nodes_with_preceeding_token("{", &opts.members, opts.prefer_single_line, context);

  let first_member_range = opts.members.first().map(|x| x.range());
  let obj_range = if let (Some(open_brace_token), Some(close_brace_token)) = (open_brace_token, close_brace_token) {
    Some(SourceRange::new(open_brace_token.start(), close_brace_token.end()))
  } else {
    None
  };

  items.extend(gen_surrounded_by_tokens(
    |context| {
      if opts.members.is_empty() {
        PrintItems::new()
      } else {
        gen_separated_values(
          GenSeparatedValuesParams {
            nodes: opts.members.into_iter().map(NodeOrSeparator::Node).collect(),
            prefer_hanging: opts.prefer_hanging,
            force_use_new_lines: force_multi_line,
            allow_blank_lines: opts.allow_blank_lines,
            separator: opts.separator,
            single_line_options: ir_helpers::SingleLineOptions {
              space_at_start: opts.surround_single_line_with_spaces,
              space_at_end: opts.surround_single_line_with_space_at_end,
              separator: Signal::SpaceOrNewLine.into(),
            },
            multi_line_options: ir_helpers::MultiLineOptions::surround_newlines_indented(),
            force_possible_newline_at_start: false,
            node_sorter: opts.node_sorter,
          },
          context,
        )
      }
    },
    |_| None,
    GenSurroundedByTokensOptions {
      open_token: sc!("{"),
      close_token: sc!("}"),
      range: obj_range,
      first_member: first_member_range,
      prefer_single_line_when_empty: true,
      allow_open_token_trailing_comments: true,
      single_line_space_around: false,
    },
    context,
  ));

  items
}

fn gen_for_member_like_expr_item<'a>(item: &MemberLikeExprItem<'a>, context: &mut Context<'a>, index: usize, item_count: usize) -> PrintItems {
  let is_first = index == 0;
  let is_last = index == item_count - 1;
  match item {
    MemberLikeExprItem::Node { node, .. } => {
      let is_optional = item.is_optional();
      gen_node_with_inner_gen(*node, context, |node_items, context| {
        // Changing `2 .toString()` to `2.toString()` is a syntax error because `2.` is a numeric
        // literal, so we surround it in parenthesis `(2).toString()`
        let is_first_and_number = is_first && matches!(node, Node::NumericLiteral(_)) && !node.text_fast(context.program).contains('.');
        let mut items = PrintItems::new();
        if is_first_and_number {
          items.push_sc(sc!("("));
        }
        if !is_first {
          // computed member accesses are a separate MemberLikeExprItem::Computed variant,
          // so here a non-first member always uses `.` (or `?.`)
          if is_optional {
            items.push_sc(sc!("?."));
          } else {
            items.push_sc(sc!("."));
          }
        }
        items.extend(node_items);
        if is_first_and_number {
          items.push_sc(sc!(")"));
        }
        items
      })
    }
    MemberLikeExprItem::Computed { node, .. } => {
      // a computed member access `[expr]` (oxc keeps this as its own item variant)
      let is_optional = item.is_optional();
      let mut items = PrintItems::new();
      if is_optional {
        items.push_sc(sc!("?."));
      }
      items.extend(gen_computed_prop_like(
        |context| gen_node(*node, context),
        GenComputedPropLikeOptions {
          inner_node_range: node.range(),
        },
        context,
      ));
      if !is_last {
        items.extend(gen_trailing_comments(&item.range(), context));
      }
      items
    }
    MemberLikeExprItem::CallExpr(node) => {
      let mut items = PrintItems::new();

      items.extend(gen_call_expr_like(
        CallExprLike {
          original_call_expr: node.original_call_expr,
          generated_callee: gen_for_member_like_expr_item(&node.callee, context, index, item_count),
        },
        context,
      ));

      if !is_last {
        // Need to manually generate the trailing comments here because
        // this doesn't go through the gen_node method
        items.extend(gen_trailing_comments(&item.range(), context));
      }

      items
    }
  }
}

fn gen_for_flattened_member_like_expr<'a>(node: FlattenedMemberLikeExpr<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  let member_expr_start_ln = LineNumber::new("memberExprStart");
  let member_expr_last_item_start_ln = LineNumber::new("memberExprStartLastItem");
  let total_items_len = node.nodes.len();

  if total_items_len > 1 {
    items.push_info(member_expr_start_ln);
  }

  items.extend(gen_for_member_like_expr_item(&node.nodes[0], context, 0, total_items_len));

  for (i, item) in node.nodes.iter().enumerate().skip(1) {
    let force_use_new_line =
      !context.config.member_expression_prefer_single_line && node_helpers::get_use_new_lines_for_nodes(&node.nodes[i - 1], &node.nodes[i], context.program);
    if item.is_optional() || !item.is_computed() {
      if force_use_new_line {
        items.push_signal(Signal::NewLine);
      } else if !context.config.member_expression_line_per_expression {
        items.push_condition(conditions::if_above_width(context.config.indent_width, Signal::PossibleNewLine.into()));
      } else {
        items.push_condition(if_true_or(
          "isMultipleLines",
          Rc::new(move |context| condition_helpers::is_multiple_lines(context, member_expr_start_ln, member_expr_last_item_start_ln)),
          Signal::NewLine.into(),
          Signal::PossibleNewLine.into(),
        ));
      }
    }

    let is_last_item = i == total_items_len - 1;
    if is_last_item {
      // store this right before the last right expression
      items.push_info(member_expr_last_item_start_ln);
    }

    let generated_item = gen_for_member_like_expr_item(item, context, i, total_items_len);
    if item.is_computed() {
      items.push_condition(indent_if_start_of_line_or_start_of_line_indented(generated_item));
    } else {
      items.push_condition(conditions::indent_if_start_of_line(generated_item));
    }
  }

  items
}

struct GenComputedPropLikeOptions {
  inner_node_range: SourceRange,
}

fn gen_computed_prop_like<'a>(
  gen_inner: impl FnOnce(&mut Context<'a>) -> PrintItems,
  opts: GenComputedPropLikeOptions,
  context: &mut Context<'a>,
) -> PrintItems {
  let inner_node_range = opts.inner_node_range;
  let range = get_bracket_range(&inner_node_range, context);
  let force_use_new_lines = !context.config.computed_prefer_single_line
    && if let Some(range) = &range {
      node_helpers::get_use_new_lines_for_nodes(&range.start().range(), &inner_node_range.start().range(), context.program)
    } else {
      false
    };

  return new_line_group(gen_surrounded_by_tokens(
    |context| {
      if force_use_new_lines {
        surround_with_new_lines(with_indent(gen_inner(context)))
      } else {
        ir_helpers::surround_with_newlines_indented_if_multi_line(gen_inner(context), context.config.indent_width)
      }
    },
    |_| None,
    GenSurroundedByTokensOptions {
      open_token: sc!("["),
      close_token: sc!("]"),
      range,
      first_member: Some(inner_node_range),
      prefer_single_line_when_empty: false,
      allow_open_token_trailing_comments: true,
      single_line_space_around: false,
    },
    context,
  ));

  fn get_bracket_range(node: &SourceRange, context: &mut Context) -> Option<SourceRange> {
    let open_bracket = context.token_finder.get_previous_token_if_open_bracket(node);
    let close_bracket = context.token_finder.get_next_token_if_close_bracket(node);
    if let Some(open_bracket) = open_bracket {
      if let Some(close_bracket) = close_bracket {
        return Some(SourceRange::new(open_bracket.start(), close_bracket.end()));
      }
    }

    if cfg!(debug_assertions) {
      panic!("Debug panic! Could not find open and/or close bracket token.");
    } else {
      None
    }
  }
}

fn gen_decorators<'a>(decorators: &'a [Decorator<'a>], is_inline: bool, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  if decorators.is_empty() {
    return items;
  }

  let force_use_new_lines = !context.config.decorators_prefer_single_line
    && decorators.len() >= 2
    && node_helpers::get_use_new_lines_for_nodes(&decorators[0], &decorators[1], context.program);

  let separated_values_result = gen_separated_values_with_result(
    GenSeparatedValuesParams {
      nodes: decorators.iter().map(|p| NodeOrSeparator::Node(Node::Decorator(p))).collect(),
      prefer_hanging: false, // would need to think about the design because prefer_hanging causes a hanging indent
      force_use_new_lines,
      allow_blank_lines: false,
      separator: Separator::none(),
      single_line_options: ir_helpers::SingleLineOptions {
        space_at_start: false,
        space_at_end: is_inline,
        separator: Signal::SpaceOrNewLine.into(),
      },
      multi_line_options: ir_helpers::MultiLineOptions::same_line_no_indent(),
      force_possible_newline_at_start: false,
      node_sorter: None,
    },
    context,
  );

  items.extend(separated_values_result.items);

  if is_inline {
    let is_multi_line = separated_values_result.is_multi_line_condition_ref.create_resolver();
    items.push_condition(if_true("inlineMultiLineSpace", is_multi_line, Signal::NewLine.into()));
  } else {
    items.push_signal(Signal::NewLine);
  }

  // generate the comments between the last decorator and the next token
  if let Some(last_dec) = decorators.last() {
    let next_token_pos = context.token_finder.get_next_token_pos_after(last_dec);
    items.extend(gen_leading_comments(&next_token_pos.range(), context));
  }

  items
}

fn gen_control_flow_separator(
  next_control_flow_position: NextControlFlowPosition,
  previous_node_block: &SourceRange,
  token_text: &str,
  previous_start_ln: Option<LineNumber>,
  previous_close_brace_condition_ref: Option<ConditionReference>,
  context: &mut Context,
) -> PrintItems {
  let mut items = PrintItems::new();
  match next_control_flow_position {
    NextControlFlowPosition::SameLine => {
      items.push_condition(if_true_or(
        "newLineOrSpace",
        Rc::new(move |condition_context| {
          // newline if on the same line as the previous
          if let Some(previous_start_ln) = previous_start_ln {
            if condition_helpers::is_on_same_line(condition_context, previous_start_ln)? {
              return Some(true);
            }
          }

          // newline if the previous did not have a close brace
          if let Some(previous_close_brace_condition_ref) = previous_close_brace_condition_ref {
            if !condition_context.resolved_condition(&previous_close_brace_condition_ref)? {
              return Some(true);
            }
          }

          Some(false)
        }),
        Signal::NewLine.into(),
        " ".into(),
      ));
    }
    NextControlFlowPosition::NextLine => items.push_signal(Signal::NewLine),
    NextControlFlowPosition::Maintain => {
      let token = context.token_finder.get_first_keyword_after(previous_node_block, token_text);

      if token.is_some() && node_helpers::is_first_node_on_line(&token.unwrap().range(), context.program) {
        items.push_signal(Signal::NewLine);
      } else {
        items.push_space();
      }
    }
  }
  items
}

struct GenHeaderWithConditionalBraceBodyOptions<'a> {
  body_node: Node<'a>,
  generated_header: PrintItems,
  use_braces: UseBraces,
  brace_position: BracePosition,
  single_body_position: Option<SameOrNextLinePosition>,
  requires_braces_condition_ref: Option<ConditionReference>,
}

struct GenHeaderWithConditionalBraceBodyResult {
  generated_node: PrintItems,
  open_brace_condition_ref: ConditionReference,
  close_brace_condition_ref: ConditionReference,
}

fn gen_header_with_conditional_brace_body<'a>(
  opts: GenHeaderWithConditionalBraceBodyOptions<'a>,
  context: &mut Context<'a>,
) -> GenHeaderWithConditionalBraceBodyResult {
  let start_header_ln = LineNumber::new("startHeader");
  let start_header_lsil = LineStartIndentLevel::new("startHeader");
  let end_header_ln = LineNumber::new("endHeader");
  let mut items = PrintItems::new();

  items.push_info(start_header_ln);
  items.push_anchor(LineNumberAnchor::new(end_header_ln));
  items.push_info(start_header_lsil);
  items.extend(new_line_group(opts.generated_header));
  items.push_info(end_header_ln);
  let result = gen_conditional_brace_body(
    GenConditionalBraceBodyOptions {
      body_node: opts.body_node.into(),
      use_braces: if force_use_braces_for_stmt(opts.body_node) {
        UseBraces::Always
      } else {
        opts.use_braces
      },
      brace_position: opts.brace_position,
      single_body_position: opts.single_body_position,
      requires_braces_condition_ref: opts.requires_braces_condition_ref,
      start_header_info: Some((start_header_ln, start_header_lsil)),
      end_header_info: Some(end_header_ln),
    },
    context,
  );
  items.extend(result.generated_node);

  GenHeaderWithConditionalBraceBodyResult {
    open_brace_condition_ref: result.open_brace_condition_ref,
    close_brace_condition_ref: result.close_brace_condition_ref,
    generated_node: items,
  }
}

fn force_use_braces_for_stmt(stmt: Node) -> bool {
  match stmt {
    Node::BlockStatement(block) => {
      if block.body.len() != 1 {
        true
      } else {
        force_use_braces_for_stmt(stmt_to_node(&block.body[0]))
      }
    }
    // force braces for any children where no braces could be ambiguous
    Node::EmptyStatement(_)
    | Node::DoWhileStatement(_)
    | Node::ForStatement(_)
    | Node::ForInStatement(_)
    | Node::ForOfStatement(_)
    | Node::IfStatement(_) // especially force for this as it may cause a bug
    | Node::LabeledStatement(_)
    | Node::SwitchStatement(_)
    | Node::TryStatement(_)
    | Node::WhileStatement(_)
    | Node::WithStatement(_)
    // declarations (SWC's Stmt::Decl)
    | Node::VariableDeclaration(_)
    | Node::Function(_)
    | Node::Class(_)
    | Node::TSTypeAliasDeclaration(_)
    | Node::TSInterfaceDeclaration(_)
    | Node::TSEnumDeclaration(_)
    | Node::TSModuleDeclaration(_)
    | Node::TSImportEqualsDeclaration(_) => true,
    _ => false,
  }
}

struct GenConditionalBraceBodyOptions<'a> {
  body_node: Node<'a>,
  use_braces: UseBraces,
  brace_position: BracePosition,
  single_body_position: Option<SameOrNextLinePosition>,
  requires_braces_condition_ref: Option<ConditionReference>,
  start_header_info: Option<(LineNumber, LineStartIndentLevel)>,
  end_header_info: Option<LineNumber>,
}

struct GenConditionalBraceBodyResult {
  generated_node: PrintItems,
  open_brace_condition_ref: ConditionReference,
  close_brace_condition_ref: ConditionReference,
}

fn gen_conditional_brace_body<'a>(opts: GenConditionalBraceBodyOptions<'a>, context: &mut Context<'a>) -> GenConditionalBraceBodyResult {
  // todo: reorganize...
  let start_lc = LineAndColumn::new("start");
  let end_ln = LineNumber::new("end");
  let start_header_ln = opts.start_header_info.map(|v| v.0);
  let start_header_lsil = opts.start_header_info.map(|v| v.1);
  let end_header_ln = opts.end_header_info;
  let requires_braces_condition = opts.requires_braces_condition_ref;
  let start_inner_text_lc = LineAndColumn::new("startInnerText");
  let start_statements_lc = LineAndColumn::new("startStatements");
  let end_statements_lc = LineAndColumn::new("endStatements");
  let header_trailing_comments = get_header_trailing_comments(opts.body_node, context);
  let has_header_trailing_comments = !header_trailing_comments.is_empty();
  let body_should_be_multi_line = get_body_should_be_multi_line(opts.body_node, &header_trailing_comments, context);
  let should_use_new_line = get_should_use_new_line(opts.body_node, body_should_be_multi_line, &opts.single_body_position, context);
  let open_brace_token = get_open_brace_token(opts.body_node, context);
  let use_braces = opts.use_braces;
  let is_body_empty_stmt = matches!(opts.body_node, Node::EmptyStatement(_));
  let mut inner_brace_space_condition = if_true(
    "spaceCondition",
    Rc::new(move |condition_context| {
      if is_body_empty_stmt {
        return Some(false);
      }
      let (start_inner_line, start_inner_column) = condition_context.resolved_line_and_column(start_inner_text_lc)?;
      let (end_stmts_line, end_stmts_column) = condition_context.resolved_line_and_column(end_statements_lc)?;
      if start_inner_line < end_stmts_line {
        return Some(false);
      }
      Some(start_inner_column < end_stmts_column)
    }),
    Signal::SpaceOrNewLine.into(),
  );
  let inner_brace_space_condition_ref = inner_brace_space_condition.create_reference();
  let mut newline_condition = if_true(
    "newLineCondition",
    Rc::new(move |condition_context| {
      if is_body_empty_stmt {
        return Some(false);
      }

      if should_use_new_line {
        return Some(true);
      }
      let end_header_ln = condition_context.resolved_line_number(end_header_ln?)?;
      if end_header_ln < condition_context.writer_info.line_number {
        return Some(true);
      }
      let resolved_end_statements_ln = condition_context.resolved_line_number(end_statements_lc.line)?;
      Some(resolved_end_statements_ln > end_header_ln)
    }),
    Signal::NewLine.into(),
  );
  let newline_condition_ref = newline_condition.create_reference();
  let force_braces = get_force_braces(opts.body_node);
  let mut open_brace_condition = if_true(
    "openBrace",
    {
      let has_open_brace_token = open_brace_token.is_some();
      Rc::new(move |condition_context| {
        // never use braces for a single semi-colon on the end (ex. `for(;;);`)
        if is_body_empty_stmt {
          return Some(false);
        }

        match use_braces {
          UseBraces::WhenNotSingleLine => {
            if force_braces {
              Some(true)
            } else {
              let is_multiple_lines = condition_helpers::is_multiple_lines(condition_context, end_header_ln.unwrap_or(start_lc.line), end_ln)?;
              Some(is_multiple_lines)
            }
          }
          UseBraces::Maintain => Some(force_braces || has_open_brace_token),
          UseBraces::Always => Some(true),
          UseBraces::PreferNone => {
            if force_braces || body_should_be_multi_line {
              return Some(true);
            }
            if has_header_trailing_comments {
              return Some(false);
            }
            if let Some(start_header_ln) = start_header_ln {
              if let Some(end_header_ln) = end_header_ln {
                let is_header_multiple_lines = condition_helpers::is_multiple_lines(condition_context, start_header_ln, end_header_ln)?;
                if is_header_multiple_lines {
                  return Some(true);
                }
              }
            }
            let is_statements_multiple_lines = condition_helpers::is_multiple_lines(condition_context, start_statements_lc.line, end_statements_lc.line)?;
            if is_statements_multiple_lines {
              return Some(true);
            }

            if let Some(requires_braces_condition) = &requires_braces_condition {
              let requires_braces = condition_context.resolved_condition(requires_braces_condition)?;
              if requires_braces {
                return Some(true);
              }
            }

            Some(false)
          }
        }
      })
    },
    {
      let mut items = PrintItems::new();
      items.extend(gen_brace_separator(
        GenBraceSeparatorOptions {
          brace_position: opts.brace_position,
          open_brace_token,
          start_header_lsil,
        },
        context,
      ));
      items.push_sc(sc!("{"));
      items.push_condition(inner_brace_space_condition);
      items
    },
  );
  let open_brace_condition_reevaluation = open_brace_condition.create_reevaluation();
  let open_brace_condition_ref = open_brace_condition.create_reference();

  // store the brace condition if ASI and the body is an expression statement
  if context.config.semi_colons == SemiColons::Asi && node_helpers::is_expr_stmt_or_body_with_single_expr_stmt(opts.body_node) {
    context.store_expr_stmt_single_line_parent_brace_ref(open_brace_condition_ref);
  }

  // generate body
  let mut items = PrintItems::new();
  items.push_line_and_column(start_lc);
  items.push_anchor(LineNumberAnchor::new(end_ln));
  items.push_condition(open_brace_condition);
  items.push_line_and_column(start_inner_text_lc);
  let generated_comments = gen_comment_collection(header_trailing_comments.into_iter(), None, None, context);
  if !generated_comments.is_empty() {
    items.push_signal(Signal::StartForceNoNewLines);
    items.push_space();
    items.extend(generated_comments);
    items.push_signal(Signal::FinishForceNoNewLines);
  }
  items.push_condition(newline_condition);
  items.push_line_and_column(start_statements_lc);
  if !is_body_empty_stmt {
    items.push_condition(if_true(
      "spaceIfAtStart",
      Rc::new(move |context| condition_helpers::is_at_same_position(context, start_lc)),
      Signal::SpaceOrNewLine.into(),
    ));
  }

  if let Node::BlockStatement(body_node) = opts.body_node {
    items.extend(ir_helpers::with_indent({
      let mut items = PrintItems::new();
      // generate the remaining trailing comments inside because some of them are generated already
      // by parsing the header trailing comments
      items.extend(gen_leading_comments(&body_node.range(), context));
      let inner_range = body_node.get_inner_range(context);
      if body_node.body.is_empty() {
        let trailing_comments_same_line = get_trailing_comments_on_same_line(&inner_range.start().range(), context);
        items.extend(gen_comments_same_line(trailing_comments_same_line, context));
        // treat the remaining unhandled comments as statements
        items.extend(gen_comments_as_statements(
          inner_range
            .start()
            .trailing_comments_fast(context.program)
            .chain(inner_range.end().leading_comments_fast(context.program)),
          None,
          context,
        ));
      } else {
        items.extend(gen_statements(inner_range, body_node.body.iter().map(stmt_to_node).collect(), context));
      }
      items
    }));
  } else {
    items.extend(ir_helpers::with_indent({
      let mut items = PrintItems::new();
      let body_node_range = opts.body_node.range();
      items.extend(gen_node(opts.body_node, context));
      items.extend(gen_trailing_comments(&body_node_range, context));
      items
    }));
  }

  items.push_line_and_column(end_statements_lc);
  let mut close_brace_condition = if_true(
    "closeBrace",
    Rc::new(move |condition_context| condition_context.resolved_condition(&open_brace_condition_ref)),
    {
      let mut items = PrintItems::new();
      items.push_condition(if_true_or(
        "closeBraceNewLine",
        Rc::new(move |condition_context| {
          let is_new_line = condition_context.resolved_condition(&newline_condition_ref)?;
          if !is_new_line {
            return Some(false);
          }
          let has_statement_text = condition_helpers::are_line_and_columns_not_equal(condition_context, start_statements_lc, end_statements_lc)?;
          Some(has_statement_text)
        }),
        Signal::NewLine.into(),
        if_true(
          "closeBraceSpace",
          Rc::new(move |condition_context| {
            if condition_helpers::is_at_same_position(condition_context, start_inner_text_lc)? {
              return Some(false);
            }
            let had_space = condition_context.resolved_condition(&inner_brace_space_condition_ref)?;
            Some(had_space)
          }),
          " ".into(),
        )
        .into(),
      ));
      items.push_sc(sc!("}"));
      items
    },
  );
  let close_brace_condition_ref = close_brace_condition.create_reference();
  items.push_condition(close_brace_condition);
  items.push_info(end_ln);
  items.push_reevaluation(open_brace_condition_reevaluation);

  // return result
  return GenConditionalBraceBodyResult {
    generated_node: items,
    open_brace_condition_ref,
    close_brace_condition_ref,
  };

  fn get_should_use_new_line<'a>(
    body_node: Node,
    body_should_be_multi_line: bool,
    single_body_position: &Option<SameOrNextLinePosition>,
    context: &mut Context<'a>,
  ) -> bool {
    if body_should_be_multi_line {
      return true;
    }
    if let Some(single_body_position) = single_body_position {
      return match single_body_position {
        SameOrNextLinePosition::Maintain => {
          get_body_stmt_start_line(body_node, context) > body_node.previous_token_fast(context.program).unwrap().start_line_fast(context.program)
        }
        SameOrNextLinePosition::NextLine => true,
        SameOrNextLinePosition::SameLine => {
          if let Node::BlockStatement(block_stmt) = body_node {
            if block_stmt.body.len() != 1 {
              return true;
            }
            return get_body_stmt_start_line(body_node, context) > body_node.previous_token_fast(context.program).unwrap().start_line_fast(context.program);
          }
          return false;
        }
      };
    } else {
      if let Node::BlockStatement(block_stmt) = body_node {
        if block_stmt.body.is_empty() {
          // keep the block on the same line
          return block_stmt.start_line_fast(context.program) < block_stmt.end_line_fast(context.program);
        }
      }
      return true;
    }

    fn get_body_stmt_start_line(body_node: Node, context: &mut Context) -> usize {
      if let Node::BlockStatement(body_node) = body_node {
        if let Some(first_stmt) = body_node.body.first() {
          return first_stmt.start_line_fast(context.program);
        }
      }
      body_node.start_line_fast(context.program)
    }
  }

  fn get_body_should_be_multi_line<'a>(body_node: Node<'a>, header_trailing_comments: &[&'a Comment], context: &mut Context<'a>) -> bool {
    if let Node::BlockStatement(body_node) = body_node {
      if body_node.body.len() == 1 && !has_leading_comment_on_different_line(&body_node.body[0].range(), header_trailing_comments, context.program) {
        return false;
      }
      if body_node.body.is_empty() && body_node.start_line_fast(context.program) == body_node.end_line_fast(context.program) {
        return false;
      }
      return true;
    } else {
      return has_leading_comment_on_different_line(&body_node.range(), header_trailing_comments, context.program);
    }

    fn has_leading_comment_on_different_line<'a>(node: &SourceRange, header_trailing_comments: &[&'a Comment], program: ProgramInfo<'a>) -> bool {
      node_helpers::has_leading_comment_on_different_line(node, /* comments to ignore */ Some(header_trailing_comments), program)
    }
  }

  fn get_force_braces(body_node: Node) -> bool {
    if let Node::BlockStatement(body_node) = body_node {
      body_node.body.is_empty()
        || body_node.body.iter().all(|s| matches!(s, Statement::EmptyStatement(_)))
        || (body_node.body.len() == 1 && body_node.body[0].is_declaration())
    } else {
      false
    }
  }

  fn get_header_trailing_comments<'a>(body_node: Node<'a>, context: &mut Context<'a>) -> Vec<&'a Comment> {
    let mut comments = Vec::new();
    if let Node::BlockStatement(block_stmt) = body_node {
      let open_brace_token = context
        .token_finder
        .get_first_open_brace_token_within(block_stmt)
        .expect("Expected to find an open brace token.");
      let last_header_token_end = context.token_finder.get_previous_token_end_before(block_stmt);
      if let Some(comment) = get_comments_between(last_header_token_end, open_brace_token.start(), context)
        .into_iter()
        .find(|c| c.kind == CommentKind::Line)
      {
        comments.push(comment);
        return comments;
      }
      let comment_line = body_node.leading_comments_fast(context.program).find(|c| c.kind == CommentKind::Line);
      if let Some(comment) = comment_line {
        comments.push(comment);
        return comments;
      }
      let body_node_start_line = body_node.start_line_fast(context.program);
      comments.extend(
        open_brace_token
          .trailing_comments_fast(context.program)
          .take_while(|c| c.start_line_fast(context.program) == body_node_start_line && c.kind == CommentKind::Line),
      );
    } else {
      if let Some(previous_token) = body_node.previous_token_fast(context.program) {
        comments.extend(
          previous_token
            .trailing_comments_fast(context.program)
            .take_while(|c| c.start_line_fast(context.program) == previous_token.end_line_fast(context.program) && c.kind == CommentKind::Line),
        );
        if !comments.is_empty() {
          return comments;
        }
      }
      let leading_comments = body_node.leading_comments_fast(context.program);
      let last_header_token_end = context.token_finder.get_previous_token_end_before(&body_node);
      let last_header_token_end_line = last_header_token_end.end_line_fast(context.program);
      comments.extend(leading_comments.take_while(|c| c.start_line_fast(context.program) <= last_header_token_end_line && c.kind == CommentKind::Line));
    }

    comments
  }

  fn get_open_brace_token<'a>(body_node: Node<'a>, context: &mut Context<'a>) -> Option<&'a Token> {
    if let Node::BlockStatement(block_stmt) = body_node {
      context.token_finder.get_first_open_brace_token_within(block_stmt)
    } else {
      None
    }
  }
}

struct GenJsxWithOpeningAndClosingOptions<'a> {
  opening_element: Node<'a>,
  closing_element: Node<'a>,
  children: Vec<Node<'a>>,
}

struct GenJsxWithOpeningAndClosingResult {
  items: PrintItems,
  start_ln: LineNumber,
  end_ln: LineNumber,
}

fn gen_jsx_with_opening_and_closing<'a>(opts: GenJsxWithOpeningAndClosingOptions<'a>, context: &mut Context<'a>) -> GenJsxWithOpeningAndClosingResult {
  let force_use_multi_lines = get_force_use_multi_lines(opts.opening_element, &opts.children, context);
  let start_lc = LineAndColumn::new("start");
  let end_ln = LineNumber::new("end");
  let mut items = PrintItems::new();
  let inner_range = SourceRange::new(opts.opening_element.end(), opts.closing_element.start());

  items.extend(actions::action("clearEndIfPosChanges", move |context| {
    if let Some((line, column)) = context.resolved_line_and_column(start_lc) {
      if context.writer_info.line_number != line || context.writer_info.column_number != column {
        context.clear_info(end_ln);
      }
    }
  }));
  items.push_line_and_column(start_lc);
  items.extend(gen_node(opts.opening_element, context));
  items.extend(gen_jsx_children(
    GenJsxChildrenOptions {
      inner_range,
      children: opts.children,
      parent_start_ln: start_lc.line,
      parent_end_ln: end_ln,
      force_use_multi_lines,
    },
    context,
  ));
  items.extend(gen_node(opts.closing_element, context));
  items.push_info(end_ln);

  return GenJsxWithOpeningAndClosingResult {
    items,
    start_ln: start_lc.line,
    end_ln,
  };

  fn get_force_use_multi_lines(opening_element: Node, children: &[Node], context: &mut Context) -> bool {
    if context.config.jsx_force_new_lines_surrounding_content {
      return true;
    }

    // if any of the children are a jsx element or jsx fragment, then force multi-line
    for child in children {
      if matches!(child, Node::JSXElement(_) | Node::JSXFragment(_)) {
        return true;
      }
    }

    if context.config.jsx_element_prefer_single_line {
      false
    } else if let Some(first_child) = children.first() {
      if let Node::JSXText(first_child) = first_child {
        if first_child.text_fast(context.program).find('\n').is_some() {
          return true;
        }
      }

      node_helpers::get_use_new_lines_for_nodes(&opening_element, first_child, context.program)
    } else {
      false
    }
  }
}

struct GenJsxChildrenOptions<'a> {
  inner_range: SourceRange,
  children: Vec<Node<'a>>,
  parent_start_ln: LineNumber,
  parent_end_ln: LineNumber,
  force_use_multi_lines: bool,
}

fn gen_jsx_children<'a>(opts: GenJsxChildrenOptions<'a>, context: &mut Context<'a>) -> PrintItems {
  let filtered_children = get_filtered_jsx_children(opts.children, context);

  // Need to generate the children here so they only get generated once.
  // Nodes need to be only generated once so that their comments don't end up in
  // the handled comments collection and the second time they won't be generated out.
  let generated_children = filtered_children
    .into_iter()
    .map(|child| {
      (child, {
        let items = gen_node(child, context);
        match child {
          Node::JSXText(_) => items,
          _ => new_line_group(items),
        }
        .into_rc_path()
      })
    })
    .collect::<Vec<_>>();
  let parent_start_ln = opts.parent_start_ln;
  let parent_end_ln = opts.parent_end_ln;

  if opts.force_use_multi_lines {
    return gen_for_new_lines(generated_children, opts.inner_range, context);
  } else {
    // decide whether newlines should be used or not
    return if_true_or(
      "jsxChildrenNewLinesOrNot",
      Rc::new(move |condition_context| {
        // use newlines if the header is multiple lines
        if condition_context.resolved_line_number(parent_start_ln)? < condition_context.writer_info.line_number {
          return Some(true);
        }

        // use newlines if the entire jsx element is on multiple lines
        condition_helpers::is_multiple_lines(condition_context, parent_start_ln, parent_end_ln)
      }),
      gen_for_new_lines(generated_children.clone(), opts.inner_range, context),
      gen_for_single_line(generated_children, context),
    )
    .into();
  }

  /// JSX children includes JSXText whitespace nodes that overly complicates things.
  /// This function will filter out those nodes along with filtering out any jsx space expression
  /// nodes that may not appear in the final output.
  fn get_filtered_jsx_children<'a>(real_children: Vec<Node<'a>>, context: &mut Context<'a>) -> Vec<Node<'a>> {
    let real_children_len = real_children.len();
    let mut children: Vec<Node<'a>> = Vec::with_capacity(real_children_len);
    let mut current_jsx_space_exprs = Vec::new();
    let mut found_non_space_child = false; // include space expressions at the start

    for child in real_children.into_iter() {
      if found_non_space_child && node_helpers::has_jsx_space_expr_text(child, context.program) {
        current_jsx_space_exprs.push(child);
        continue;
      }
      let child_text = child.text_fast(context.program);
      if child_text.trim().is_empty() {
        continue;
      }

      children.push(child);
      current_jsx_space_exprs.clear();
      found_non_space_child = true;
    }

    // include any jsx space expressions that had no regular nodes following
    children.extend(current_jsx_space_exprs);

    children
  }

  fn gen_for_new_lines<'a>(children: Vec<(Node<'a>, Option<PrintItemPath>)>, inner_range: SourceRange, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    let has_children = !children.is_empty();
    items.push_signal(Signal::NewLine);
    items.extend(ir_helpers::with_indent(gen_members(
      GenMembersOptions {
        inner_range,
        items: children.into_iter().map(|(a, b)| (a, Some(b.into()))).collect(),
        should_use_space: Some(Box::new(|previous, next, context| {
          if has_jsx_space_between(previous, next, context.program) {
            true
          } else if let Node::JSXText(element) = previous {
            element.text_fast(context.program).ends_with(' ')
          } else if let Node::JSXText(element) = next {
            element.text_fast(context.program).starts_with(' ')
          } else {
            false
          }
        })),
        should_use_new_line: Some(Box::new(|previous, next, context| {
          if has_jsx_space_between(previous, next, context.program) {
            false // prefer collapsing
          } else if let Node::JSXText(next) = next {
            !utils::has_no_new_lines_in_leading_whitespace(next.text_fast(context.program))
          } else if let Node::JSXText(previous) = previous {
            !utils::has_no_new_lines_in_trailing_whitespace(previous.text_fast(context.program))
          } else {
            true
          }
        })),
        should_use_blank_line: |previous, next, context| {
          if has_jsx_space_between(previous, next, context.program) {
            false // prefer collapsing
          } else if let Node::JSXText(previous) = previous {
            utils::has_new_line_occurrences_in_trailing_whitespace(previous.text_fast(context.program), 2)
          } else if let Node::JSXText(next) = next {
            utils::has_new_line_occurrences_in_leading_whitespace(next.text_fast(context.program), 2)
          } else {
            node_helpers::has_separating_blank_line(&previous, &next, context.program)
          }
        },
        separator: Separator::none(),
        is_jsx_children: true,
      },
      context,
    )));

    if has_children {
      items.push_signal(Signal::NewLine);
    }

    items
  }

  fn gen_for_single_line<'a>(children: Vec<(Node<'a>, Option<PrintItemPath>)>, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    if children.is_empty() {
      items.push_signal(Signal::PossibleNewLine);
    } else {
      let mut previous_child = None;
      for (index, (child, generated_child)) in children.into_iter().enumerate() {
        if index > 0 && should_use_space(*previous_child.as_ref().unwrap(), child, context) {
          items.extend(jsx_space_separator(*previous_child.as_ref().unwrap(), child, context));
        } else {
          items.push_signal(Signal::PossibleNewLine);
        }

        items.extend(generated_child.into());

        previous_child = Some(child);
      }
      items.push_signal(Signal::PossibleNewLine);
    }
    items
  }

  fn should_use_space<'a>(previous_child: Node<'a>, current: Node<'a>, context: &mut Context<'a>) -> bool {
    if has_jsx_space_between(previous_child, current, context.program) {
      return true;
    }

    let past_token = context.token_finder.get_previous_token(&current);
    if let Some(token) = past_token {
      if token.kind() == Kind::JSXText {
        let text = token.text_fast(context.program);
        let had_line_break = text.contains('\n') || text.contains('\r');
        if !had_line_break && text.ends_with(' ') {
          return true;
        }
      }
    }
    if let Node::JSXText(child) = current {
      child.text_fast(context.program).starts_with(' ')
    } else {
      false
    }
  }

  /// If the node has a "JSX space expression" between or text that's only spaces between.
  fn has_jsx_space_between<'a>(previous_node: Node<'a>, next_node: Node<'a>, program: ProgramInfo<'a>) -> bool {
    node_helpers::count_spaces_between_jsx_children(previous_node, next_node, program) > 0
  }
}

fn jsx_space_separator<'a>(previous_node: Node<'a>, current_node: Node<'a>, context: &Context<'a>) -> PrintItems {
  return if is_ignore_jsx_expr_container(previous_node, context) && matches!(current_node, Node::JSXText(_)) {
    PrintItems::new()
  } else if node_should_force_newline_if_multi_line(previous_node) || node_should_force_newline_if_multi_line(current_node) {
    jsx_force_space_with_newline_if_either_node_multi_line(previous_node, current_node, context)
  } else {
    jsx_space_or_newline_or_expr_space(previous_node, current_node, context)
  };

  fn node_should_force_newline_if_multi_line(node: Node) -> bool {
    matches!(node, Node::JSXElement(_) | Node::JSXFragment(_))
  }

  fn get_node_ln_range<'a>(node: Node<'a>, context: &Context<'a>) -> Option<(LineNumber, LineNumber)> {
    if node_should_force_newline_if_multi_line(node) {
      context.get_ln_range_for_node(&node)
    } else {
      None
    }
  }

  fn jsx_force_space_with_newline_if_either_node_multi_line<'a>(previous_node: Node<'a>, current_node: Node<'a>, context: &Context<'a>) -> PrintItems {
    let previous_node_ln_range = get_node_ln_range(previous_node, context);
    let current_node_ln_range = get_node_ln_range(current_node, context);
    let spaces_between_count = node_helpers::count_spaces_between_jsx_children(previous_node, current_node, context.program);
    let jsx_space_expr_text = get_jsx_space_text(spaces_between_count, context);
    if_true_or(
      "jsxIsLastChildMultiLine",
      Rc::new(move |condition_context| {
        if let Some((start_ln, end_ln)) = previous_node_ln_range {
          let result = condition_helpers::is_multiple_lines(condition_context, start_ln, end_ln)?;
          if result {
            return Some(true);
          }
        }
        if let Some((start_ln, end_ln)) = current_node_ln_range {
          let result = condition_helpers::is_multiple_lines(condition_context, start_ln, end_ln)?;
          if result {
            return Some(true);
          }
        }

        Some(false)
      }),
      {
        let mut items = PrintItems::new();
        items.push_signal(Signal::PossibleNewLine);
        items.push_string(jsx_space_expr_text.clone());
        items.push_signal(Signal::NewLine);
        items
      },
      {
        let mut items = PrintItems::new();
        if spaces_between_count > 1 {
          items.push_signal(Signal::PossibleNewLine);
          items.push_string(jsx_space_expr_text);
          items.push_signal(Signal::PossibleNewLine);
        } else {
          items.extend(jsx_space_or_newline_or_expr_space(previous_node, current_node, context));
        }
        items
      },
    )
    .into()
  }

  fn jsx_space_or_newline_or_expr_space<'a>(previous_node: Node<'a>, current_node: Node<'a>, context: &Context<'a>) -> PrintItems {
    let spaces_between_count = node_helpers::count_spaces_between_jsx_children(previous_node, current_node, context.program);
    let mut items = PrintItems::new();

    if spaces_between_count > 1 {
      items.push_signal(Signal::PossibleNewLine);
      items.push_string(get_jsx_space_text(spaces_between_count, context));
      items.push_signal(Signal::PossibleNewLine);
      return items;
    }

    let start_line = LineNumber::new("jsxSpaceStartLine");
    let end_line = LineNumber::new("jsxSpaceEndLine");
    items.push_anchor(LineNumberAnchor::new(end_line));
    items.push_info(start_line);
    items.extend(actions::if_column_number_changes(move |context| {
      context.clear_info(end_line);
    }));

    let mut condition = if_true_or(
      "jsxSpaceOrNewLineIsMultipleLines",
      Rc::new(move |context| {
        let start_line = context.resolved_line_number(start_line)?;
        let end_line = context.resolved_line_number(end_line)?;
        Some(start_line < end_line)
      }),
      {
        let mut items = PrintItems::new();
        items.push_signal(Signal::PossibleNewLine);
        items.push_string(get_jsx_space_text(1, context));
        items.push_signal(Signal::NewLine);
        items
      },
      Signal::SpaceOrNewLine.into(),
    );
    let condition_reevaluation = condition.create_reevaluation();

    items.push_condition(condition);
    items.push_info(end_line);
    items.push_reevaluation(condition_reevaluation);
    items
  }
}

fn get_jsx_space_text(spaces_between_count: usize, context: &Context) -> String {
  format!("{{{}{}{}}}", get_quote_char(context), " ".repeat(spaces_between_count), get_quote_char(context))
}

fn get_quote_char(context: &Context) -> String {
  match context.config.quote_style {
    QuoteStyle::PreferDouble | QuoteStyle::AlwaysDouble => "\"".to_string(),
    QuoteStyle::PreferSingle | QuoteStyle::AlwaysSingle => "'".to_string(),
  }
}

#[inline]
fn gen_assignment<'a>(expr: Node<'a>, op: &'static StringContainer, context: &mut Context<'a>) -> PrintItems {
  gen_assignment_op_to(expr, op.text, op, context)
}

#[inline]
fn gen_assignment_op_to<'a>(expr: Node<'a>, _op: &'static str, op_to: &'static StringContainer, context: &mut Context<'a>) -> PrintItems {
  let op_token = context.token_finder.get_previous_token(&expr);
  #[cfg(debug_assertions)]
  assert_has_op(_op, op_token, context);

  gen_assignment_like_with_token(expr, op_to, op_token, context)
}

fn gen_assignment_like_with_token<'a>(expr: Node<'a>, op: &'static StringContainer, op_token: Option<&Token>, context: &mut Context<'a>) -> PrintItems {
  let use_new_line_group = get_use_new_line_group(expr);
  let mut items = PrintItems::new();

  if op.text != ":" {
    items.push_space();
  }
  items.push_sc(op);

  let op_end = op_token
    .map(|x| x.end())
    .unwrap_or_else(|| context.token_finder.get_previous_token_end_before(&expr));
  let defer_op_trailing_comments_to_expr = should_defer_op_trailing_comments_to_expr(expr, op_end, context);
  let op_trailing_comments = if defer_op_trailing_comments_to_expr {
    PrintItems::new()
  } else {
    gen_comments_between_lines_indented(op_end, context)
  };
  let has_expr_leading_comments_on_previous_line = expr
    .leading_comments_fast(context.program)
    .any(|comment| comment.start_line_fast(context.program) < expr.start_line_fast(context.program));
  let has_op_trailing_comments = !op_trailing_comments.is_empty();
  let had_op_trailing_comments = has_op_trailing_comments || (!defer_op_trailing_comments_to_expr && has_expr_leading_comments_on_previous_line);
  let defer_expr_leading_comments_on_previous_line = defer_op_trailing_comments_to_expr && has_expr_leading_comments_on_previous_line;
  items.extend(op_trailing_comments);
  if !has_op_trailing_comments && has_expr_leading_comments_on_previous_line {
    items.push_signal(Signal::NewLine);
  }

  let generated_assignment = {
    let mut items = PrintItems::new();
    if !had_op_trailing_comments && !defer_expr_leading_comments_on_previous_line {
      items.push_condition(conditions::if_above_width_or(
        context.config.indent_width,
        {
          let mut items = PrintItems::new();
          items.push_signal(Signal::SpaceIfNotTrailing);
          items.push_signal(Signal::PossibleNewLine);
          items
        },
        Signal::SpaceIfNotTrailing.into(),
      ));
    }
    let assignment = gen_node(expr, context);
    let assignment = if had_op_trailing_comments {
      assignment
    } else {
      conditions::indent_if_start_of_line(assignment).into()
    };
    let assignment = if use_new_line_group { new_line_group(assignment) } else { assignment };
    items.extend(assignment);
    items
  }
  .into_rc_path();

  items.push_condition(if_true_or(
    "indentIfStartOfLineIndentedOrTokenHadTrailingLineComment",
    if had_op_trailing_comments {
      condition_resolvers::true_resolver()
    } else {
      condition_resolvers::is_start_of_line_indented()
    },
    with_indent(generated_assignment.into()),
    generated_assignment.into(),
  ));

  return items;

  fn get_use_new_line_group(expr: Node) -> bool {
    matches!(
      expr,
      Node::StaticMemberExpression(_) | Node::ComputedMemberExpression(_) | Node::PrivateFieldExpression(_)
    )
  }

  fn should_defer_op_trailing_comments_to_expr<'a>(expr: Node<'a>, op_end: SourcePos, context: &mut Context<'a>) -> bool {
    match expr {
      Node::TSUnionType(node) => should_defer_union_or_intersection_op_trailing_comments(&node.types, sc!("|"), op_end, context),
      Node::TSIntersectionType(node) => should_defer_union_or_intersection_op_trailing_comments(&node.types, sc!("&"), op_end, context),
      Node::TSParenthesizedType(node) if should_skip_parenthesized_type(node, context) => match &node.type_annotation {
        TSType::TSUnionType(union_type) => should_defer_union_or_intersection_op_trailing_comments(&union_type.types, sc!("|"), op_end, context),
        TSType::TSIntersectionType(intersection_type) => {
          should_defer_union_or_intersection_op_trailing_comments(&intersection_type.types, sc!("&"), op_end, context)
        }
        _ => false,
      },
      _ => false,
    }
  }

  fn should_defer_union_or_intersection_op_trailing_comments<'a>(
    types: &'a [TSType<'a>],
    separator: &'static StringContainer,
    op_end: SourcePos,
    context: &mut Context<'a>,
  ) -> bool {
    let Some(first_type) = types.first() else {
      return true;
    };
    if context
      .token_finder
      .get_previous_token_if_operator(&first_type.range(), separator.text)
      .is_some()
    {
      return true;
    }
    !has_newline_between_positions(op_end, first_type.start(), context.program)
  }
}

struct GenBlockOptions<'a> {
  range: Option<SourceRange>,
  children: Vec<Node<'a>>,
}

fn gen_block<'a>(gen_inner: impl FnOnce(Vec<Node<'a>>, &mut Context<'a>) -> PrintItems, opts: GenBlockOptions<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  let skip_indent = context.skip_iife_body_indent;
  context.skip_iife_body_indent = false;
  let before_open_token_ln = LineNumber::new("after_open_token_info");
  let range = opts.range;
  let first_member_range = opts.children.first().map(|x| x.range()).or_else(|| {
    let range = range?;
    if range.start_line_fast(context.program) == range.end_line_fast(context.program) {
      return None;
    }
    get_comments_between(range.start() + 1, range.end() - 1, context)
      .into_iter()
      .find(|comment| comment.start_line_fast(context.program) != range.start_line_fast(context.program))
      .map(|comment| comment.range())
  });
  items.push_info(before_open_token_ln);
  items.extend(gen_surrounded_by_tokens(
    |context| {
      let mut items = PrintItems::new();
      let start_inner_lc = LineAndColumn::new("startStatements");
      let end_inner_lc = LineAndColumn::new("endStatements");
      let is_tokens_same_line_and_empty = if let Some(range) = &range {
        range.start_line_fast(context.program) == range.end_line_fast(context.program) && opts.children.is_empty()
      } else {
        true
      };
      if !is_tokens_same_line_and_empty {
        items.push_signal(Signal::NewLine);
      }
      items.push_line_and_column(start_inner_lc);
      let inner_comments = if opts.children.is_empty() && !is_tokens_same_line_and_empty {
        range
          .map(|range| {
            get_comments_between(range.start() + 1, range.end() - 1, context)
              .into_iter()
              .filter(|comment| comment.start_line_fast(context.program) != range.start_line_fast(context.program))
              .collect::<Vec<_>>()
          })
          .unwrap_or_default()
      } else {
        Vec::new()
      };
      let inner = if inner_comments.is_empty() {
        gen_inner(opts.children, context)
      } else {
        gen_comments_as_statements(inner_comments.into_iter(), None, context)
      };
      items.extend(if skip_indent { inner } else { ir_helpers::with_indent(inner) });
      items.push_line_and_column(end_inner_lc);

      if is_tokens_same_line_and_empty {
        items.push_condition(if_true(
          "newLineIfDifferentLine",
          Rc::new(move |context| condition_helpers::is_on_different_line(context, before_open_token_ln)),
          Signal::NewLine.into(),
        ));
      } else {
        items.push_condition(if_false(
          "endNewline",
          Rc::new(move |context| condition_helpers::are_line_and_columns_equal(context, start_inner_lc, end_inner_lc)),
          Signal::NewLine.into(),
        ));
      }
      items
    },
    |_| None,
    GenSurroundedByTokensOptions {
      open_token: sc!("{"),
      close_token: sc!("}"),
      range,
      first_member: first_member_range,
      prefer_single_line_when_empty: false,
      allow_open_token_trailing_comments: true,
      single_line_space_around: false,
    },
    context,
  ));
  items
}

struct GenSurroundedByTokensOptions {
  open_token: &'static StringContainer,
  close_token: &'static StringContainer,
  /// When `None`, means the tokens are missing
  range: Option<SourceRange>,
  first_member: Option<SourceRange>,
  prefer_single_line_when_empty: bool,
  allow_open_token_trailing_comments: bool,
  single_line_space_around: bool,
}

fn gen_surrounded_by_tokens<'a>(
  gen_inner: impl FnOnce(&mut Context<'a>) -> PrintItems,
  custom_close_token: impl FnOnce(&mut Context<'a>) -> Option<PrintItems>,
  opts: GenSurroundedByTokensOptions,
  context: &mut Context<'a>,
) -> PrintItems {
  let mut items = PrintItems::new();
  if let Some(range) = opts.range {
    let open_token_end = range.start() + opts.open_token.text.len() as u32;
    let close_token_start = range.end() - opts.close_token.text.len() as u32;

    // assert the tokens are in the place the caller says they are
    #[cfg(debug_assertions)]
    context.assert_text(SourceRange::new(range.start, open_token_end.start()), opts.open_token.text);
    #[cfg(debug_assertions)]
    context.assert_text(SourceRange::new(close_token_start.start(), range.end), opts.close_token.text);

    // generate
    let open_token_start_line = open_token_end.start_line_fast(context.program);
    let is_single_line;

    items.extend(gen_leading_comments(&range, context));
    items.push_sc(opts.open_token);
    if let Some(first_member) = opts.first_member {
      let first_member_start_line = first_member.start_line_fast(context.program);
      is_single_line = open_token_start_line == first_member_start_line;

      if is_single_line && opts.single_line_space_around {
        items.push_space();
      }

      if opts.allow_open_token_trailing_comments && !is_single_line {
        items.extend(gen_first_line_trailing_comment(
          open_token_start_line,
          open_token_end.trailing_comments_fast(context.program),
          context,
        ));
      }
      items.extend(gen_inner(context));

      let before_trailing_comments_lc = LineAndColumn::new("beforeTrailingComments");
      items.push_line_and_column(before_trailing_comments_lc);
      items.extend(with_indent(gen_trailing_comments_as_statements(&open_token_end.range(), context)));
      items.extend(with_indent(gen_comments_as_statements(
        close_token_start.leading_comments_fast(context.program),
        None,
        context,
      )));
      items.push_condition(if_true(
        "newLineIfHasCommentsAndNotStartOfNewLine",
        Rc::new(move |context| {
          let had_comments = !condition_helpers::is_at_same_position(context, before_trailing_comments_lc)?;
          Some(had_comments && !context.writer_info.is_start_of_line())
        }),
        Signal::NewLine.into(),
      ));
    } else {
      let comments = open_token_end.trailing_comments_fast(context.program);
      let close_token_leading_comments = close_token_start.leading_comments_fast(context.program);
      is_single_line = open_token_start_line == close_token_start.start_line_fast(context.program);
      if is_single_line && opts.single_line_space_around {
        items.push_space();
      }

      if !comments.is_empty() || !close_token_leading_comments.is_empty() {
        // generate the trailing comment on the first line only if multi-line and if a comment line
        if !is_single_line {
          items.extend(gen_first_line_trailing_comment(open_token_start_line, comments.clone(), context));
        }

        // generate the comments
        if comments
          .clone()
          .chain(close_token_leading_comments.clone())
          .any(|c| !context.has_handled_comment(c))
        {
          if is_single_line {
            let indent_width = context.config.indent_width;
            items.extend(
              ir_helpers::gen_separated_values(
                |_| {
                  let mut generated_comments = Vec::new();
                  for c in comments.chain(close_token_leading_comments) {
                    let start_line = c.start_line_fast(context.program);
                    let end_line = c.end_line_fast(context.program);
                    if let Some(items) = gen_comment(c, context) {
                      generated_comments.push(ir_helpers::GeneratedValue {
                        items,
                        lines_span: Some(ir_helpers::LinesSpan { start_line, end_line }),
                        allow_inline_multi_line: false,
                        allow_inline_single_line: false,
                      });
                    }
                  }
                  generated_comments
                },
                ir_helpers::GenSeparatedValuesOptions {
                  prefer_hanging: false,
                  force_use_new_lines: !is_single_line,
                  allow_blank_lines: true,
                  indent_width,
                  single_line_options: SingleLineOptions {
                    space_at_start: opts.single_line_space_around,
                    space_at_end: opts.single_line_space_around,
                    separator: Signal::SpaceOrNewLine.into(),
                  },
                  multi_line_options: ir_helpers::MultiLineOptions::surround_newlines_indented(),
                  force_possible_newline_at_start: false,
                },
              )
              .items,
            );
          } else {
            items.push_signal(Signal::NewLine);
            items.extend(with_indent(gen_comments_as_statements(
              comments.chain(close_token_leading_comments),
              None,
              context,
            )));
            items.push_signal(Signal::NewLine);
          }
        }
      } else if !is_single_line && !opts.prefer_single_line_when_empty {
        items.push_signal(Signal::NewLine);
      }
    }

    if is_single_line && opts.single_line_space_around {
      items.push_space();
    }
  } else {
    // todo: have a warning here when this happens
    items.push_sc(opts.open_token);
    items.extend(gen_inner(context));
  }

  if let Some(generated_close_token) = (custom_close_token)(context) {
    items.extend(generated_close_token);
  } else {
    items.push_sc(opts.close_token);
  }

  return items;

  fn gen_first_line_trailing_comment(open_token_start_line: usize, comments: CommentsIterator, context: &mut Context) -> PrintItems {
    let mut items = PrintItems::new();
    let first_comment = comments.into_iter().next();
    if let Some(first_comment) = first_comment {
      if first_comment.kind == CommentKind::Line && first_comment.start_line_fast(context.program) == open_token_start_line {
        if let Some(generated_comment) = gen_comment(first_comment, context) {
          items.push_signal(Signal::StartForceNoNewLines);
          items.push_space();
          items.extend(generated_comment);
          items.push_signal(Signal::FinishForceNoNewLines);
        }
      }
    }
    items
  }
}

#[cfg(debug_assertions)]
fn assert_has_op(op: &str, op_token: Option<&Token>, context: &mut Context) {
  if let Some(op_token) = op_token {
    context.assert_text(SourceRange::new(op_token.start(), op_token.end()), op);
  } else {
    panic!("Debug panic! Expected to have op token: {op}");
  }
}

fn use_new_line_group_for_arrow_body<'a>(arrow_expr: &'a ArrowFunctionExpression<'a>, context: &Context<'a>) -> bool {
  match arrow_expr.get_expression() {
    Some(expr) => match expr {
      Expression::ParenthesizedExpression(paren) => match &paren.expression {
        Expression::ObjectExpression(_) => false,
        inner => !is_jsx_paren_expr_handled_node(expr_to_node(inner), context),
      },
      _ => !is_jsx_paren_expr_handled_node(expr_to_node(expr), context),
    },
    None => true,
  }
}

fn surround_with_parens(items: PrintItems) -> PrintItems {
  let mut new_items = PrintItems::new();
  new_items.push_sc(sc!("("));
  new_items.extend(items);
  new_items.push_sc(sc!(")"));
  new_items
}

fn surround_with_spaces(items: PrintItems) -> PrintItems {
  let mut new_items = PrintItems::new();
  new_items.push_space();
  new_items.extend(items);
  new_items.push_space();
  new_items
}

fn assign_op_sc(assign_op: AssignmentOperator) -> &'static StringContainer {
  use AssignmentOperator::*;
  let sc = match assign_op {
    Assign => sc!("="),
    Addition => sc!("+="),
    Subtraction => sc!("-="),
    Multiplication => sc!("*="),
    Division => sc!("/="),
    Remainder => sc!("%="),
    ShiftLeft => sc!("<<="),
    ShiftRight => sc!(">>="),
    ShiftRightZeroFill => sc!(">>>="),
    BitwiseOR => sc!("|="),
    BitwiseXOR => sc!("^="),
    BitwiseAnd => sc!("&="),
    Exponential => sc!("**="),
    LogicalAnd => sc!("&&="),
    LogicalOr => sc!("||="),
    LogicalNullish => sc!("??="),
  };
  if cfg!(debug_assertions) {
    assert_eq!(sc.text, assign_op.as_str());
  }
  sc
}

fn binary_op_sc(binary_op: BinaryOperator) -> &'static StringContainer {
  use BinaryOperator::*;
  let sc = match binary_op {
    Equality => sc!("=="),
    Inequality => sc!("!="),
    StrictEquality => sc!("==="),
    StrictInequality => sc!("!=="),
    LessThan => sc!("<"),
    LessEqualThan => sc!("<="),
    GreaterThan => sc!(">"),
    GreaterEqualThan => sc!(">="),
    ShiftLeft => sc!("<<"),
    ShiftRight => sc!(">>"),
    ShiftRightZeroFill => sc!(">>>"),
    Addition => sc!("+"),
    Subtraction => sc!("-"),
    Multiplication => sc!("*"),
    Division => sc!("/"),
    Remainder => sc!("%"),
    BitwiseOR => sc!("|"),
    BitwiseXOR => sc!("^"),
    BitwiseAnd => sc!("&"),
    In => sc!("in"),
    Instanceof => sc!("instanceof"),
    Exponential => sc!("**"),
  };
  if cfg!(debug_assertions) {
    assert_eq!(sc.text, binary_op.as_str());
  }
  sc
}

fn logical_op_sc(logical_op: LogicalOperator) -> &'static StringContainer {
  use LogicalOperator::*;
  let sc = match logical_op {
    Or => sc!("||"),
    And => sc!("&&"),
    Coalesce => sc!("??"),
  };
  if cfg!(debug_assertions) {
    assert_eq!(sc.text, logical_op.as_str());
  }
  sc
}

fn bin_like_op_sc(op: BinaryLikeOp) -> &'static StringContainer {
  match op {
    BinaryLikeOp::Binary(op) => binary_op_sc(op),
    BinaryLikeOp::Logical(op) => logical_op_sc(op),
  }
}

/* is/has functions */

fn is_arrow_function_with_expr_body(node: Node) -> bool {
  matches!(node, Node::ArrowFunctionExpression(arrow) if arrow.expression)
}

fn allows_inline_multi_line<'a>(node: Node<'a>, context: &Context<'a>, has_siblings: bool) -> bool {
  return match node {
    Node::FormalParameter(param) => {
      allows_inline_multi_line(binding_pattern_to_node(&param.pattern), context, has_siblings)
        || param
          .type_annotation
          .as_ref()
          .is_some_and(|t| allows_inline_multi_line(ts_type_to_node(&t.type_annotation), context, has_siblings))
        || param
          .initializer
          .as_ref()
          .is_some_and(|expr| allows_inline_multi_line(expr_to_node(expr), context, has_siblings))
    }
    Node::TSAsExpression(as_expr) => {
      allows_inline_multi_line(expr_to_node(&as_expr.expression), context, has_siblings)
        && (matches!(as_expr.type_annotation, TSType::TSTypeReference(_))
          || as_expr.type_annotation.is_keyword()
          || allows_inline_multi_line(ts_type_to_node(&as_expr.type_annotation), context, has_siblings))
    }
    Node::Function(_)
    | Node::ArrowFunctionExpression(_)
    | Node::ObjectExpression(_)
    | Node::ArrayExpression(_)
    | Node::ObjectPattern(_)
    | Node::ArrayPattern(_)
    | Node::TSTypeLiteral(_)
    | Node::TSTupleType(_)
    | Node::TSArrayType(_) => true,
    Node::SpreadElement(node) => allows_inline_multi_line(expr_to_node(&node.argument), context, has_siblings),
    Node::ParenthesizedExpression(node) => {
      should_skip_paren_expr(node, context) && allows_inline_multi_line(expr_to_node(&node.expression), context, has_siblings)
    }
    Node::TaggedTemplateExpression(_) | Node::TemplateLiteral(_) => !has_siblings,
    Node::CallExpression(node) => !has_siblings && allow_inline_for_call_expr(CallOrOptCallExpr(node)),
    Node::AssignmentPattern(node) => {
      allows_inline_multi_line(binding_pattern_to_node(&node.left), context, has_siblings)
        || allows_inline_multi_line(expr_to_node(&node.right), context, has_siblings)
    }
    Node::TSTypeAnnotation(type_ann) => allows_inline_multi_line(ts_type_to_node(&type_ann.type_annotation), context, has_siblings),
    Node::TSNamedTupleMember(member) => allows_inline_multi_line(ts_tuple_element_to_node(&member.element_type), context, has_siblings),
    _ => false,
  };

  fn allow_inline_for_call_expr(node: CallOrOptCallExpr) -> bool {
    // do not allow call exprs with nested call exprs in the member expr to be inline
    return allow_for_expr(node.callee());

    fn allow_for_expr(expr: &Expression) -> bool {
      match expr {
        Expression::Super(_) => true,
        Expression::StaticMemberExpression(m) => allow_for_expr(&m.object),
        Expression::ComputedMemberExpression(m) => allow_for_expr(&m.object),
        Expression::PrivateFieldExpression(m) => allow_for_expr(&m.object),
        Expression::CallExpression(_) => false,
        Expression::ChainExpression(c) => !matches!(c.expression, ChainElement::CallExpression(_)),
        _ => true,
      }
    }
  }
}

fn get_use_new_lines_for_nodes_with_preceeding_token(
  open_token_text: &str,
  nodes: &[impl SourceRanged],
  prefer_single_line: bool,
  context: &mut Context,
) -> bool {
  if nodes.is_empty() {
    return false;
  }

  if prefer_single_line {
    // basic rule: if any comments exist on separate lines, then everything becomes multi-line
    has_any_node_comment_on_different_line(nodes, context)
  } else {
    let first_node = &nodes[0];
    let previous_token = context.token_finder.get_previous_token(first_node);

    if let Some(previous_token) = previous_token {
      if previous_token.text_fast(context.program) == open_token_text {
        return node_helpers::get_use_new_lines_for_nodes(&previous_token.range(), first_node, context.program);
      }
    }

    // arrow function expressions might not have an open paren (ex. `a => a + 5`)
    false
  }
}

fn get_use_new_lines_for_nodes(nodes: &[impl SourceRanged], prefer_single_line: bool, context: &mut Context) -> bool {
  if nodes.len() < 2 {
    return false;
  }

  if prefer_single_line {
    // basic rule: if any comments exist on separate lines, then everything becomes multi-line
    has_any_node_comment_on_different_line(nodes, context)
  } else {
    node_helpers::get_use_new_lines_for_nodes(&nodes[0], &nodes[1], context.program)
  }
}

/// Gets if any of the provided nodes have leading or trailing comments on a different line.
fn has_any_node_comment_on_different_line(nodes: &[impl SourceRanged], context: &mut Context) -> bool {
  for (i, node) in nodes.iter().enumerate() {
    if i == 0 {
      let first_node_start_line = node.start_line_fast(context.program);
      let mut comments_previous_lines = node
        .leading_comments_fast(context.program)
        .take_while(|c| c.kind == CommentKind::Line || c.start_line_fast(context.program) < first_node_start_line);
      if comments_previous_lines.next().is_some() {
        return true;
      }
    }

    let node_end = node.end();
    let next_node_pos = nodes.get(i + 1).map(|n| n.start());
    if check_pos_has_trailing_comments(node_end, next_node_pos, context) {
      return true;
    } else if let Some(comma) = context.token_finder.get_next_token_if_comma(&node_end.range()) {
      if check_pos_has_trailing_comments(comma.end(), next_node_pos, context) {
        return true;
      } else if next_node_pos.is_some_and(|next_node_pos| {
        get_comments_between(comma.end(), next_node_pos, context)
          .iter()
          .any(|c| c.kind == CommentKind::Line)
      }) {
        return true;
      }
    }
  }

  return false;

  fn check_pos_has_trailing_comments(end: SourcePos, next_node_pos: Option<SourcePos>, context: &mut Context) -> bool {
    let end_line = end.end_line_fast(context.program);
    let stop_line = next_node_pos.map(|p| p.start_line_fast(context.program));

    for c in end.trailing_comments_fast(context.program) {
      if c.kind == CommentKind::Line {
        return true;
      }
      if let Some(stop_line) = stop_line {
        if c.start_line_fast(context.program) >= stop_line {
          // do not look at comments that the next node owns
          return false;
        }
      }
      if c.end_line_fast(context.program) > end_line {
        return true;
      }
    }

    false
  }
}

fn is_node_definitely_above_line_width<'a>(range: SourceRange, context: &Context<'a>) -> bool {
  let text = range.text_fast(context.program);
  let max_width = context.config.line_width as usize * 2;
  if text.len() < max_width {
    return false;
  }
  let mut count = 0;
  for c in text.chars() {
    if !c.is_whitespace() {
      count += 1;
      if count > max_width {
        return true;
      }
    } else if c == '\n' {
      return false;
    }
  }
  false
}

/* config helpers */

fn get_generated_separator(separator: &Separator, is_trailing: bool, is_multi_line: &ConditionResolver) -> PrintItems {
  debug_assert!(!separator.is_none());
  // performance optimization
  return if separator.single_line == separator.multi_line {
    get_items(&separator.single_line, is_trailing, is_multi_line)
  } else {
    if_true_or(
      "is_multi_line",
      is_multi_line.clone(),
      get_items(&separator.multi_line, is_trailing, is_multi_line),
      get_items(&separator.single_line, is_trailing, is_multi_line),
    )
    .into()
  };

  fn get_items(value: &Option<SeparatorValue>, is_trailing: bool, is_multi_line: &ConditionResolver) -> PrintItems {
    match value {
      Some(SeparatorValue::Comma(trailing_comma)) => get_generated_trailing_comma(*trailing_comma, is_trailing, is_multi_line),
      Some(SeparatorValue::SemiColon(semi_colons)) => get_generated_semi_colon(*semi_colons, is_trailing, is_multi_line),
      None => PrintItems::new(),
    }
  }
}

fn get_generated_trailing_comma(option: TrailingCommas, is_trailing: bool, is_multi_line: &ConditionResolver) -> PrintItems {
  if !is_trailing {
    return ",".into();
  }

  match option {
    TrailingCommas::Always => ",".into(),
    TrailingCommas::OnlyMultiLine => if_true("trailingCommaIfMultiLine", is_multi_line.clone(), ",".into()).into(),
    TrailingCommas::Never => PrintItems::new(),
  }
}

fn get_generated_semi_colon(option: SemiColons, is_trailing: bool, is_multi_line: &ConditionResolver) -> PrintItems {
  match option {
    SemiColons::Always => ";".into(),
    SemiColons::Prefer => {
      if is_trailing {
        if_true("semiColonIfMultiLine", is_multi_line.clone(), ";".into()).into()
      } else {
        ";".into()
      }
    }
    SemiColons::Asi => {
      if is_trailing {
        PrintItems::new()
      } else {
        if_false("semiColonIfSingleLine", is_multi_line.clone(), ";".into()).into()
      }
    }
  }
}

// oxc has no children-with-tokens API; collect only the node's *top-level* commas
// (depth 1 within its own brackets) so nested commas don't throw off hole indexing.
fn get_comma_tokens_from_children_with_tokens<'a>(node: Node<'a>, program: ProgramInfo<'a>) -> Vec<&'a Token> {
  let mut result = Vec::new();
  let mut depth: i32 = 0;
  for token in node.tokens_fast(program) {
    match token.kind() {
      Kind::LBrack | Kind::LCurly | Kind::LParen => depth += 1,
      Kind::RBrack | Kind::RCurly | Kind::RParen => depth -= 1,
      Kind::Comma if depth == 1 => result.push(token),
      _ => {}
    }
  }
  result
}

fn get_tokens_from_children_with_tokens<'a>(node: Node<'a>, program: ProgramInfo<'a>) -> Vec<&'a Token> {
  // all tokens within the node; callers only need the first `{` / last `}`, which are the
  // outermost braces regardless of nesting.
  node.tokens_fast(program).iter().collect()
}
