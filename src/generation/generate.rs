use deno_ast::swc::common::comments::Comment;
use deno_ast::swc::common::comments::CommentKind;
use deno_ast::swc::parser::token::BinOpToken;
use deno_ast::swc::parser::token::Token;
use deno_ast::swc::parser::token::TokenAndSpan;
use deno_ast::view::*;
use deno_ast::CommentsIterator;
use deno_ast::MediaType;
use deno_ast::ParsedSource;
use deno_ast::SourcePos;
use deno_ast::SourceRange;
use deno_ast::SourceRanged;
use deno_ast::SourceRangedForSpanned;
use dprint_core::formatting::condition_resolvers;
use dprint_core::formatting::conditions::*;
use dprint_core::formatting::ir_helpers::*;
use dprint_core::formatting::*;
use dprint_core_macros::sc;
use std::rc::Rc;

use super::sorting::*;
use super::swc::get_flattened_bin_expr;
use super::swc::*;
use super::*;
use crate::configuration::*;
use crate::utils;

pub fn generate(parsed_source: &ParsedSource, config: &Configuration) -> PrintItems {
  // eprintln!("Leading: {:?}", parsed_source.comments().leading_map());
  // eprintln!("Trailing: {:?}", parsed_source.comments().trailing_map());

  parsed_source.with_view(|program| {
    let program_node = program.into();
    let mut context = Context::new(parsed_source.media_type(), parsed_source.tokens(), program_node, program, config);
    let mut items = gen_node(program_node, &mut context);
    items.push_condition(if_true(
      "endOfFileNewLine",
      Rc::new(|context| Some(context.writer_info.column_number > 0 || context.writer_info.line_number > 0)),
      Signal::NewLine.into(),
    ));

    #[cfg(debug_assertions)]
    context.assert_end_of_file_state();

    items
  })
}

fn gen_node<'a>(node: Node<'a>, context: &mut Context<'a>) -> PrintItems {
  gen_node_with_inner_gen(node, context, |items, _| items)
}

fn gen_node_with_inner_gen<'a>(node: Node<'a>, context: &mut Context<'a>, inner_gen: impl FnOnce(PrintItems, &mut Context<'a>) -> PrintItems) -> PrintItems {
  let node_kind = node.kind();
  // eprintln!("Node kind: {:?}", node_kind);
  // eprintln!("Text: {:?}", node.text());
  // eprintln!("Range: {:?}", node.range());

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
  if !matches!(node_kind, NodeKind::Module | NodeKind::Script) {
    // get the leading comments
    if does_first_child_own_leading_comments_on_same_line(node, context) {
      // Some block comments should belong to the first child rather than the
      // parent node because their first child may end up on the next line.
      let leading_comments = node_start.leading_comments_fast(context.program);
      has_ignore_comment = get_has_ignore_comment(&leading_comments, node, context);
      let program = context.program;
      let node_start_line = node.start_line_fast(program);
      let leading_comments_on_previous_lines = leading_comments.take_while(|c| c.kind == CommentKind::Line || c.start_line_fast(program) < node_start_line);
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
    items.extend(inner_gen(ir_helpers::gen_from_raw_string(node.text_fast(context.program)), context));

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
  if node_end != parent_end || matches!(context.parent().kind(), NodeKind::Module | NodeKind::Script) {
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
  return if node_kind == NodeKind::SpreadElement && node.parent().unwrap().kind() == NodeKind::JSXOpeningElement {
    gen_as_jsx_expr_container(node, items, context)
  } else {
    items
  };

  fn gen_node_inner<'a>(node: Node<'a>, context: &mut Context<'a>) -> PrintItems {
    match node {
      /* class */
      Node::ClassMethod(node) => gen_class_method(node, context),
      Node::ClassProp(node) => gen_class_prop(node, context),
      Node::Constructor(node) => gen_constructor(node, context),
      Node::Decorator(node) => gen_decorator(node, context),
      Node::TsParamProp(node) => gen_parameter_prop(node, context),
      Node::AutoAccessor(node) => gen_auto_accessor(node, context),
      Node::PrivateMethod(node) => gen_private_method(node, context),
      Node::PrivateName(node) => gen_private_name(node, context),
      Node::PrivateProp(node) => gen_private_prop(node, context),
      Node::StaticBlock(node) => gen_static_block(node, context),
      /* clauses */
      Node::CatchClause(node) => gen_catch_clause(node, context),
      /* common */
      Node::ComputedPropName(node) => gen_computed_prop_name(node, context),
      Node::Ident(node) => gen_identifier(node, context),
      Node::IdentName(node) => gen_ident_name(node, context),
      Node::BindingIdent(node) => gen_binding_identifier(node, context),
      /* declarations */
      Node::ClassDecl(node) => gen_class_decl(node, context),
      Node::ExportDecl(node) => gen_export_decl(node, context),
      Node::ExportDefaultDecl(node) => gen_export_default_decl(node, context),
      Node::ExportDefaultExpr(node) => gen_export_default_expr(node, context),
      Node::ExportDefaultSpecifier(node) => gen_export_default_specifier(node, context),
      Node::FnDecl(node) => gen_function_decl(node, context),
      Node::ImportDecl(node) => gen_import_decl(node, context),
      Node::NamedExport(node) => gen_export_named_decl(node, context),
      Node::Param(node) => gen_param(node, context),
      Node::TsEnumDecl(node) => gen_enum_decl(node, context),
      Node::TsEnumMember(node) => gen_enum_member(node, context),
      Node::TsImportEqualsDecl(node) => gen_import_equals_decl(node, context),
      Node::TsInterfaceDecl(node) => gen_interface_decl(node, context),
      Node::TsModuleDecl(node) => gen_module_decl(node, context),
      Node::TsNamespaceDecl(node) => gen_namespace_decl(node, context),
      Node::TsTypeAliasDecl(node) => gen_type_alias(node, context),
      Node::UsingDecl(node) => gen_using_decl(node, context),
      /* expressions */
      Node::ArrayLit(node) => gen_array_expr(node, context),
      Node::ArrowExpr(node) => gen_arrow_func_expr(node, context),
      Node::AssignExpr(node) => gen_assignment_expr(node, context),
      Node::AwaitExpr(node) => gen_await_expr(node, context),
      Node::BinExpr(node) => gen_binary_expr(node, context),
      Node::CallExpr(node) => gen_call_or_opt_expr(node.into(), context),
      Node::OptCall(node) => gen_call_or_opt_expr(node.into(), context),
      Node::Import(node) => gen_import_callee(node, context),
      Node::ClassExpr(node) => gen_class_expr(node, context),
      Node::CondExpr(node) => gen_conditional_expr(node, context),
      Node::ExprOrSpread(node) => gen_expr_or_spread(node, context),
      Node::FnExpr(node) => gen_fn_expr(node, context),
      Node::GetterProp(node) => gen_getter_prop(node, context),
      Node::KeyValueProp(node) => gen_key_value_prop(node, context),
      Node::AssignProp(node) => gen_assign_prop(node, context),
      Node::MemberExpr(node) => gen_member_expr(node, context),
      Node::MetaPropExpr(node) => gen_meta_prop_expr(node, context),
      Node::SuperPropExpr(node) => gen_super_prop_expr(node, context),
      Node::NewExpr(node) => gen_new_expr(node, context),
      Node::ObjectLit(node) => gen_object_lit(node, context),
      Node::OptChainExpr(node) => gen_node(node.base.into(), context),
      Node::ParenExpr(node) => gen_paren_expr(node, context),
      Node::SeqExpr(node) => gen_sequence_expr(node, context),
      Node::SetterProp(node) => gen_setter_prop(node, context),
      Node::SpreadElement(node) => gen_spread_element(node, context),
      Node::Super(_) => "super".into(),
      Node::TaggedTpl(node) => gen_tagged_tpl(node, context),
      Node::ThisExpr(_) => "this".into(),
      Node::Tpl(node) => gen_tpl(node, context),
      Node::TplElement(node) => gen_tpl_element(node, context),
      Node::TsAsExpr(node) => gen_as_expr(node, context),
      Node::TsSatisfiesExpr(node) => gen_satisfies_expr(node, context),
      Node::TsConstAssertion(node) => gen_const_assertion(node, context),
      Node::TsExprWithTypeArgs(node) => gen_expr_with_type_args(node, context),
      Node::TsNonNullExpr(node) => gen_non_null_expr(node, context),
      Node::TsTypeAssertion(node) => gen_type_assertion(node, context),
      Node::UnaryExpr(node) => gen_unary_expr(node, context),
      Node::UpdateExpr(node) => gen_update_expr(node, context),
      Node::YieldExpr(node) => gen_yield_expr(node, context),
      /* exports */
      Node::ExportNamedSpecifier(node) => gen_export_named_specifier(node, context),
      Node::ExportNamespaceSpecifier(node) => gen_namespace_export_specifier(node, context),
      /* imports */
      Node::ImportNamedSpecifier(node) => gen_import_named_specifier(node, context),
      Node::ImportStarAsSpecifier(node) => gen_import_namespace_specifier(node, context),
      Node::ImportDefaultSpecifier(node) => gen_node(node.local.into(), context),
      Node::TsExternalModuleRef(node) => gen_external_module_ref(node, context),
      /* interface / type element */
      Node::TsCallSignatureDecl(node) => gen_call_signature_decl(node, context),
      Node::TsConstructSignatureDecl(node) => gen_construct_signature_decl(node, context),
      Node::TsIndexSignature(node) => gen_index_signature(node, context),
      Node::TsInterfaceBody(node) => gen_interface_body(node, context),
      Node::TsMethodSignature(node) => gen_method_signature(node, context),
      Node::TsPropertySignature(node) => gen_property_signature(node, context),
      Node::TsTypeLit(node) => gen_type_lit(node, context),
      /* jsx */
      Node::JSXAttr(node) => gen_jsx_attribute(node, context),
      Node::JSXClosingElement(node) => gen_jsx_closing_element(node, context),
      Node::JSXClosingFragment(node) => gen_jsx_closing_fragment(node, context),
      Node::JSXElement(node) => gen_jsx_element(node, context),
      Node::JSXEmptyExpr(node) => gen_jsx_empty_expr(node, context),
      Node::JSXExprContainer(node) => gen_jsx_expr_container(node, context),
      Node::JSXFragment(node) => gen_jsx_fragment(node, context),
      Node::JSXMemberExpr(node) => gen_jsx_member_expr(node, context),
      Node::JSXNamespacedName(node) => gen_jsx_namespaced_name(node, context),
      Node::JSXOpeningElement(node) => gen_jsx_opening_element(node, context),
      Node::JSXOpeningFragment(node) => gen_jsx_opening_fragment(node, context),
      Node::JSXSpreadChild(node) => gen_jsx_spread_child(node, context),
      Node::JSXText(node) => gen_jsx_text(node, context),
      /* literals */
      Node::BigInt(node) => gen_big_int_literal(node, context),
      Node::Bool(node) => gen_bool_literal(node),
      Node::Null(_) => "null".into(),
      Node::Number(node) => gen_num_literal(node, context),
      Node::Regex(node) => gen_reg_exp_literal(node, context),
      Node::Str(node) => gen_string_literal(node, context),
      /* top level */
      Node::Module(node) => gen_module(node, context),
      Node::Script(node) => gen_script(node, context),
      /* patterns */
      Node::ArrayPat(node) => gen_array_pat(node, context),
      Node::AssignPat(node) => gen_assign_pat(node, context),
      Node::AssignPatProp(node) => gen_assign_pat_prop(node, context),
      Node::KeyValuePatProp(node) => gen_key_value_pat_prop(node, context),
      Node::RestPat(node) => gen_rest_pat(node, context),
      Node::ObjectPat(node) => gen_object_pat(node, context),
      /* properties */
      Node::MethodProp(node) => gen_method_prop(node, context),
      /* statements */
      Node::BlockStmt(node) => gen_block_stmt(node, context),
      Node::BreakStmt(node) => gen_break_stmt(node, context),
      Node::ContinueStmt(node) => gen_continue_stmt(node, context),
      Node::DebuggerStmt(node) => gen_debugger_stmt(node, context),
      Node::DoWhileStmt(node) => gen_do_while_stmt(node, context),
      Node::ExportAll(node) => gen_export_all(node, context),
      Node::ExprStmt(node) => gen_expr_stmt(node, context),
      Node::EmptyStmt(node) => gen_empty_stmt(node, context),
      Node::ForInStmt(node) => gen_for_in_stmt(node, context),
      Node::ForOfStmt(node) => gen_for_of_stmt(node, context),
      Node::ForStmt(node) => gen_for_stmt(node, context),
      Node::IfStmt(node) => gen_if_stmt(node, context),
      Node::LabeledStmt(node) => gen_labeled_stmt(node, context),
      Node::ReturnStmt(node) => gen_return_stmt(node, context),
      Node::SwitchStmt(node) => gen_switch_stmt(node, context),
      Node::SwitchCase(node) => gen_switch_case(node, context),
      Node::ThrowStmt(node) => gen_throw_stmt(node, context),
      Node::TryStmt(node) => gen_try_stmt(node, context),
      Node::TsExportAssignment(node) => gen_export_assignment(node, context),
      Node::TsNamespaceExportDecl(node) => gen_namespace_export(node, context),
      Node::VarDecl(node) => gen_var_decl(node, context),
      Node::VarDeclarator(node) => gen_var_declarator(node, context),
      Node::WhileStmt(node) => gen_while_stmt(node, context),
      /* types */
      Node::TsArrayType(node) => gen_array_type(node, context),
      Node::TsConditionalType(node) => gen_conditional_type(node, context),
      Node::TsConstructorType(node) => gen_constructor_type(node, context),
      Node::TsFnType(node) => gen_function_type(node, context),
      Node::TsGetterSignature(node) => gen_getter_signature(node, context),
      Node::TsSetterSignature(node) => gen_setter_signature(node, context),
      Node::TsKeywordType(node) => gen_keyword_type(node, context),
      Node::TsImportType(node) => gen_import_type(node, context),
      Node::TsIndexedAccessType(node) => gen_indexed_access_type(node, context),
      Node::TsInferType(node) => gen_infer_type(node, context),
      Node::TsInstantiation(node) => gen_ts_instantiation(node, context),
      Node::TsIntersectionType(node) => gen_intersection_type(node, context),
      Node::TsLitType(node) => gen_lit_type(node, context),
      Node::TsMappedType(node) => gen_mapped_type(node, context),
      Node::TsOptionalType(node) => gen_optional_type(node, context),
      Node::TsQualifiedName(node) => gen_qualified_name(node, context),
      Node::TsParenthesizedType(node) => gen_parenthesized_type(node, context),
      Node::TsRestType(node) => gen_rest_type(node, context),
      Node::TsThisType(_) => "this".into(),
      Node::TsTplLitType(node) => gen_tpl_lit_type(node, context),
      Node::TsTupleType(node) => gen_tuple_type(node, context),
      Node::TsTupleElement(node) => gen_tuple_element(node, context),
      Node::TsTypeAnn(node) => gen_type_ann(node, context),
      Node::TsTypeParam(node) => gen_type_param(node, context),
      Node::TsTypeParamDecl(node) => gen_type_parameters(TypeParamNode::Decl(node), context),
      Node::TsTypeParamInstantiation(node) => gen_type_parameters(TypeParamNode::Instantiation(node), context),
      Node::TsTypeOperator(node) => gen_type_operator(node, context),
      Node::TsTypePredicate(node) => gen_type_predicate(node, context),
      Node::TsTypeQuery(node) => gen_type_query(node, context),
      Node::TsTypeRef(node) => gen_type_reference(node, context),
      Node::TsUnionType(node) => gen_union_type(node, context),
      /* These should never be matched. Return its text if so */
      Node::Class(_) | Node::Function(_) | Node::Invalid(_) | Node::WithStmt(_) | Node::TsModuleBlock(_) => {
        if cfg!(debug_assertions) {
          panic!("Debug panic! Did not expect to generate IR for node of type {}.", node.kind());
        }

        gen_from_raw_string(node.text_fast(context.program))
      }
    }
  }

  #[inline]
  fn handle_decorators_if_necessary<'a>(node: Node<'a>, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();

    // decorators in these cases will have starts before their parent so they need to be handled specially
    if let Node::ExportDecl(decl) = node {
      if let Decl::Class(class_decl) = &decl.decl {
        items.extend(gen_decorators(class_decl.class.decorators, false, context));
      }
    } else if let Node::ExportDefaultDecl(decl) = node {
      if let DefaultDecl::Class(class_expr) = &decl.decl {
        items.extend(gen_decorators(class_expr.class.decorators, false, context));
      }
    }

    items
  }

  #[inline]
  fn does_first_child_own_leading_comments_on_same_line(node: Node, context: &mut Context) -> bool {
    match node {
      Node::TsUnionType(_) | Node::TsIntersectionType(_) => {
        let node_start_line = node.start_line_fast(context.program);
        node
          .leading_comments_fast(context.program)
          .any(|c| c.kind == CommentKind::Block && c.start_line_fast(context.program) == node_start_line)
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
  let comments = match node.parent() {
    Some(Node::JSXElement(jsx_element)) => get_comments_for_jsx_children(jsx_element.children, &node.start(), context),
    Some(Node::JSXFragment(jsx_fragment)) => get_comments_for_jsx_children(jsx_fragment.children, &node.start(), context),
    _ => leading_comments.clone(),
  };

  for comment in comments {
    if ir_helpers::text_has_dprint_ignore(&comment.text, &context.config.ignore_node_comment_text) {
      return true;
    }
  }

  return false;

  fn get_comments_for_jsx_children<'a>(children: &[JSXElementChild], node_lo: &SourcePos, context: &mut Context<'a>) -> CommentsIterator<'a> {
    let mut iterator = CommentsIterator::empty();
    let index = if let Ok(index) = children.binary_search_by_key(node_lo, |child| child.start()) {
      index
    } else {
      return iterator;
    };

    for i in (0..index).rev() {
      match children.get(i).unwrap() {
        JSXElementChild::JSXExprContainer(expr_container) => {
          match expr_container.expr {
            JSXExpr::JSXEmptyExpr(empty_expr) => {
              iterator.extend(get_jsx_empty_expr_comments(empty_expr, context));
            }
            _ => break,
          };
        }
        JSXElementChild::JSXText(jsx_text) => {
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

/* class */

fn gen_class_method<'a>(node: &'a ClassMethod<'a>, context: &mut Context<'a>) -> PrintItems {
  // todo: consolidate with private method
  gen_class_or_object_method(
    ClassOrObjectMethod {
      node: node.into(),
      parameters_range: node.get_parameters_range(context),
      decorators: Some(node.function.decorators),
      accessibility: node.accessibility(),
      is_static: node.is_static(),
      is_async: node.function.is_async(),
      is_abstract: node.is_abstract(),
      kind: node.method_kind().into(),
      is_generator: node.function.is_generator(),
      is_optional: node.is_optional(),
      is_override: node.is_override(),
      key: node.key.into(),
      type_params: node.function.type_params.map(|x| x.into()),
      params: node.function.params.iter().map(|&x| x.into()).collect(),
      return_type: node.function.return_type.map(|x| x.into()),
      body: node.function.body.map(|x| x.into()),
    },
    context,
  )
}

fn gen_auto_accessor<'a>(node: &AutoAccessor<'a>, context: &mut Context<'a>) -> PrintItems {
  gen_class_prop_common(
    GenClassPropCommon {
      original: node.into(),
      key: node.key.into(),
      value: node.value,
      type_ann: node.type_ann,
      is_static: node.is_static(),
      decorators: node.decorators,
      computed: false,
      is_auto_accessor: true,
      is_declare: false,
      accessibility: node.accessibility(),
      is_abstract: node.is_abstract(),
      is_optional: false,
      is_override: node.is_override(),
      readonly: false,
      definite: node.definite(),
    },
    context,
  )
}

fn gen_private_method<'a>(node: &PrivateMethod<'a>, context: &mut Context<'a>) -> PrintItems {
  gen_class_or_object_method(
    ClassOrObjectMethod {
      node: node.into(),
      parameters_range: node.get_parameters_range(context),
      decorators: Some(node.function.decorators),
      accessibility: node.accessibility(),
      is_static: node.is_static(),
      is_async: node.function.is_async(),
      is_abstract: node.is_abstract(),
      kind: node.method_kind().into(),
      is_generator: node.function.is_generator(),
      is_optional: node.is_optional(),
      is_override: node.is_override(),
      key: node.key.into(),
      type_params: node.function.type_params.map(|x| x.into()),
      params: node.function.params.iter().map(|&x| x.into()).collect(),
      return_type: node.function.return_type.map(|x| x.into()),
      body: node.function.body.map(|x| x.into()),
    },
    context,
  )
}

fn gen_class_prop<'a>(node: &ClassProp<'a>, context: &mut Context<'a>) -> PrintItems {
  gen_class_prop_common(
    GenClassPropCommon {
      original: node.into(),
      key: node.key.into(),
      value: node.value,
      type_ann: node.type_ann,
      is_static: node.is_static(),
      decorators: node.decorators,
      computed: matches!(node.key, PropName::Computed(_)),
      is_auto_accessor: false,
      is_declare: node.declare(),
      accessibility: node.accessibility(),
      is_abstract: node.is_abstract(),
      is_optional: node.is_optional(),
      is_override: node.is_override(),
      readonly: node.readonly(),
      definite: node.definite(),
    },
    context,
  )
}

fn gen_constructor<'a>(node: &Constructor<'a>, context: &mut Context<'a>) -> PrintItems {
  gen_class_or_object_method(
    ClassOrObjectMethod {
      node: node.into(),
      parameters_range: node.get_parameters_range(context),
      decorators: None,
      accessibility: node.accessibility(),
      is_static: false,
      is_async: false,
      is_abstract: false,
      kind: ClassOrObjectMethodKind::Constructor,
      is_generator: false,
      is_optional: node.is_optional(),
      is_override: false,
      key: node.key.into(),
      type_params: None,
      params: node.params.iter().map(|x| x.into()).collect(),
      return_type: None,
      body: node.body.map(|x| x.into()),
    },
    context,
  )
}

fn gen_decorator<'a>(node: &Decorator<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.push_sc(sc!("@"));
  items.extend(gen_node(node.expr.into(), context));
  items
}

fn gen_parameter_prop<'a>(node: &TsParamProp<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.extend(gen_decorators(node.decorators, true, context));
  if let Some(accessibility) = node.accessibility() {
    items.push_string(format!("{} ", accessibility_to_str(accessibility)));
  }
  if node.is_override() {
    items.push_sc(sc!("override "));
  }
  if node.readonly() {
    items.push_sc(sc!("readonly "));
  }
  items.extend(gen_node(node.param.into(), context));
  items
}

fn gen_private_name<'a>(node: &PrivateName<'a>, _context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.push_sc(sc!("#"));
  items.push_string(node.name().to_string());
  items
}

fn gen_private_prop<'a>(node: &PrivateProp<'a>, context: &mut Context<'a>) -> PrintItems {
  gen_class_prop_common(
    GenClassPropCommon {
      original: node.into(),
      key: node.key.into(),
      value: node.value,
      type_ann: node.type_ann,
      is_static: node.is_static(),
      decorators: node.decorators,
      computed: false,
      is_auto_accessor: false,
      is_declare: false,
      accessibility: node.accessibility(),
      is_abstract: false,
      is_optional: node.is_optional(),
      is_override: node.is_override(),
      readonly: node.readonly(),
      definite: node.definite(),
    },
    context,
  )
}

struct GenClassPropCommon<'a, 'b> {
  pub original: Node<'a>,
  pub key: Node<'a>,
  pub value: Option<Expr<'a>>,
  pub type_ann: Option<&'a TsTypeAnn<'a>>,
  pub is_static: bool,
  pub decorators: &'b [&'a Decorator<'a>],
  pub computed: bool,
  pub is_declare: bool,
  pub accessibility: Option<Accessibility>,
  pub is_auto_accessor: bool,
  pub is_abstract: bool,
  pub is_optional: bool,
  pub is_override: bool,
  pub readonly: bool,
  pub definite: bool,
}

fn gen_class_prop_common<'a, 'b>(node: GenClassPropCommon<'a, 'b>, context: &mut Context<'a>) -> PrintItems {
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
    let inner_key_node = match node.key {
      Node::ComputedPropName(prop) => prop.expr.as_node(),
      _ => node.key,
    };
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
    items.extend(gen_assignment(value.into(), sc!("="), context));
  }

  let should_semi = context.config.semi_colons.is_true()
    || matches!(
      node.original.next_token_fast(context.program),
      Some(TokenAndSpan {
        token: Token::LBracket | Token::BinOp(BinOpToken::Mul),
        ..
      })
    );
  if should_semi {
    items.push_sc(sc!(";"));
  }

  items
}

fn gen_static_block<'a>(node: &StaticBlock<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  let start_header_lsil = LineStartIndentLevel::new("staticBlockStart");
  items.push_info(start_header_lsil);
  items.push_sc(sc!("static"));

  items.extend(gen_brace_separator(
    GenBraceSeparatorOptions {
      brace_position: context.config.static_block_brace_position,
      open_brace_token: context.token_finder.get_first_open_brace_token_within(node.body),
      start_header_lsil: Some(start_header_lsil),
    },
    context,
  ));
  items.extend(gen_node(node.body.into(), context));

  items
}

/* clauses */

fn gen_catch_clause<'a>(node: &CatchClause<'a>, context: &mut Context<'a>) -> PrintItems {
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
    items.extend(gen_node(param.into(), context));
    if context.config.catch_clause_space_around {
      items.push_space();
    }
    items.push_sc(sc!(")"));
  }
  items.push_info(end_header_ln);

  let try_stmt = node.parent();
  let single_body_position = if try_stmt.finalizer.is_some() {
    Some(SameOrNextLinePosition::NextLine)
  } else {
    None
  };

  // not conditional... required
  items.extend(
    gen_conditional_brace_body(
      GenConditionalBraceBodyOptions {
        body_node: node.body.into(),
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

fn gen_computed_prop_name<'a>(node: &ComputedPropName<'a>, context: &mut Context<'a>) -> PrintItems {
  gen_computed_prop_like(
    |context| gen_node(node.expr.into(), context),
    GenComputedPropLikeOptions {
      inner_node_range: node.expr.range(),
    },
    context,
  )
}

fn gen_identifier<'a>(node: &Ident<'a>, _: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.push_string(node.sym().to_string());

  if node.optional() && !node.parent().is::<ClassProp>() && !node.parent().is::<ClassMethod>() {
    items.push_sc(sc!("?"));
  }

  items
}

fn gen_ident_name<'a>(node: &IdentName<'a>, _: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.push_string(node.sym().to_string());
  items
}

fn gen_binding_identifier<'a>(node: &BindingIdent<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.extend(gen_node(node.id.into(), context));

  if let Node::VarDeclarator(node) = node.parent() {
    if node.definite() {
      items.push_sc(sc!("!"));
    }
  }

  items.extend(gen_type_ann_with_colon_if_exists(node.type_ann, context));

  items
}

/* declarations */

fn gen_class_decl<'a>(node: &ClassDecl<'a>, context: &mut Context<'a>) -> PrintItems {
  gen_class_decl_or_expr(
    ClassDeclOrExpr {
      node: node.into(),
      member_node: node.class.into(),
      decorators: node.class.decorators,
      is_class_decl: true,
      is_declare: node.declare(),
      is_abstract: node.class.is_abstract(),
      ident: Some(node.ident.into()),
      type_params: node.class.type_params.map(|x| x.into()),
      super_class: node.class.super_class.map(|x| x.into()),
      super_type_params: node.class.super_type_params.map(|x| x.into()),
      implements: node.class.implements.iter().map(|&x| x.into()).collect(),
      members: node.class.body.iter().map(|x| x.into()).collect(),
      brace_position: context.config.class_declaration_brace_position,
    },
    context,
  )
}

struct ClassDeclOrExpr<'a> {
  node: Node<'a>,
  member_node: Node<'a>,
  decorators: &'a [&'a Decorator<'a>],
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

  // generate decorators
  let parent_kind = node.node.parent().unwrap().kind();
  if parent_kind != NodeKind::ExportDecl && parent_kind != NodeKind::ExportDefaultDecl {
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

fn gen_export_decl<'a>(node: &ExportDecl<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  // decorators are handled in gen_node because their starts come before the ExportDecl
  items.push_sc(sc!("export "));
  items.extend(gen_node(node.decl.into(), context));
  items
}

fn gen_export_default_decl<'a>(node: &ExportDefaultDecl<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  // decorators are handled in gen_node because their starts come before the ExportDefaultDecl
  items.push_sc(sc!("export default "));
  items.extend(gen_node(node.decl.into(), context));
  items
}

fn gen_export_default_expr<'a>(node: &ExportDefaultExpr<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.push_sc(sc!("export default "));
  items.extend(gen_node(node.expr.into(), context));
  if context.config.semi_colons.is_true() {
    items.push_sc(sc!(";"));
  }
  items
}

fn gen_export_default_specifier<'a>(node: &ExportDefaultSpecifier<'a>, context: &mut Context<'a>) -> PrintItems {
  gen_node(node.exported.into(), context)
}

fn gen_enum_decl<'a>(node: &TsEnumDecl<'a>, context: &mut Context<'a>) -> PrintItems {
  let start_header_lsil = LineStartIndentLevel::new("startHeader");
  let mut items = PrintItems::new();

  // header
  items.push_info(start_header_lsil);

  if node.declare() {
    items.push_sc(sc!("declare "));
  }
  if node.is_const() {
    items.push_sc(sc!("const "));
  }
  items.push_sc(sc!("enum "));
  items.extend(gen_node(node.id.into(), context));

  // body
  let member_spacing = context.config.enum_declaration_member_spacing;
  items.extend(gen_membered_body(
    GenMemberedBodyOptions {
      node: node.into(),
      members: node.members.iter().map(|&x| x.into()).collect(),
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

fn gen_enum_member<'a>(node: &TsEnumMember<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.extend(gen_node(node.id.into(), context));

  if let Some(init) = &node.init {
    items.extend(gen_assignment(init.into(), sc!("="), context));
  }

  items
}

fn gen_export_named_decl<'a>(node: &NamedExport<'a>, context: &mut Context<'a>) -> PrintItems {
  // fill specifiers
  let mut default_export: Option<&ExportDefaultSpecifier> = None;
  let mut namespace_export: Option<&ExportNamespaceSpecifier> = None;
  let mut named_exports: Vec<&ExportNamedSpecifier> = Vec::new();

  for specifier in node.specifiers {
    match specifier {
      ExportSpecifier::Default(node) => default_export = Some(node),
      ExportSpecifier::Namespace(node) => namespace_export = Some(node),
      ExportSpecifier::Named(node) => named_exports.push(node),
    }
  }

  let force_single_line = context.config.export_declaration_force_single_line && !contains_line_or_multiline_comment(node.into(), context.program);

  let force_multi_line = !force_single_line
    && ((context.config.export_declaration_force_multi_line == ForceMultiLine::Always)
      || (named_exports.len() > 1 && context.config.export_declaration_force_multi_line == ForceMultiLine::WhenMultiple));

  let should_single_line = force_single_line
    || (default_export.is_none()
      && namespace_export.is_none()
      && !force_multi_line
      && (named_exports.len() <= 1 && context.config.export_declaration_force_multi_line == ForceMultiLine::Never)
      && node.start_line_fast(context.program) == node.end_line_fast(context.program));

  // generate
  let mut items = PrintItems::new();

  items.push_sc(sc!("export "));
  if node.type_only() {
    items.push_sc(sc!("type "));
  }

  if let Some(default_export) = default_export {
    items.extend(gen_node(default_export.into(), context));
  } else if !named_exports.is_empty() {
    items.extend(gen_named_import_or_export_specifiers(
      GenNamedImportOrExportSpecifierOptions {
        parent: node.into(),
        specifiers: named_exports.into_iter().map(|x| x.into()).collect(),
        force_single_line,
        force_multi_line_specifiers: force_multi_line,
      },
      context,
    ));
  } else if let Some(namespace_export) = namespace_export {
    items.extend(gen_node(namespace_export.into(), context));
  } else {
    items.push_sc(sc!("{}"));
  }

  if let Some(src) = node.src {
    items.push_sc(sc!(" from "));
    items.extend(gen_node(src.into(), context));
  }

  if let Some(with_clause) = node.with {
    items.extend(gen_with_clause(with_clause, context));
  }

  if context.config.semi_colons.is_true() {
    items.push_sc(sc!(";"));
  }

  if should_single_line {
    with_no_new_lines(items)
  } else {
    items
  }
}

fn gen_function_decl<'a>(node: &FnDecl<'a>, context: &mut Context<'a>) -> PrintItems {
  gen_function_decl_or_expr(
    FunctionDeclOrExprNode {
      node: node.into(),
      is_func_decl: true,
      ident: Some(node.ident),
      declare: node.declare(),
      func: node.function,
    },
    context,
  )
}

struct FunctionDeclOrExprNode<'a> {
  node: Node<'a>,
  is_func_decl: bool,
  ident: Option<&'a Ident<'a>>,
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
  if func.is_async() {
    items.push_sc(sc!("async "));
  }
  items.push_sc(sc!("function"));
  if func.is_generator() {
    items.push_sc(sc!("*"));
  }
  if space_after_function_keyword {
    items.push_space()
  }
  if let Some(ident) = node.ident {
    if !space_after_function_keyword {
      items.push_space();
    }
    items.extend(gen_node(ident.into(), context));
  }
  if let Some(type_params) = func.type_params {
    items.extend(gen_node(type_params.into(), context));
  }
  #[allow(clippy::collapsible_if)]
  if get_use_space_before_parens(node.is_func_decl, context) {
    if node.ident.is_some() || func.type_params.is_some() || !space_after_function_keyword {
      items.push_space();
    }
  }

  items.extend(gen_parameters_or_arguments(
    GenParametersOrArgumentsOptions {
      node: node.node,
      nodes: func.params.iter().map(|&node| node.into()).collect(),
      range: func.get_parameters_range(context),
      custom_close_paren: |context| {
        Some(gen_close_paren_with_type(
          GenCloseParenWithTypeOptions {
            start_lsil: start_header_lsil,
            type_node: func.return_type.map(|x| x.into()),
            type_node_separator: None,
            param_count: func.params.len(),
          },
          context,
        ))
      },
      is_parameters: true,
    },
    context,
  ));

  if let Some(body) = func.body {
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

    items.extend(gen_node(body.into(), context));
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

fn gen_param<'a>(node: &Param<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.extend(gen_decorators(node.decorators, true, context));
  items.extend(gen_node(node.pat.into(), context));
  items
}

fn gen_import_decl<'a>(node: &ImportDecl<'a>, context: &mut Context<'a>) -> PrintItems {
  // fill specifiers
  let mut default_import: Option<&ImportDefaultSpecifier> = None;
  let mut namespace_import: Option<&ImportStarAsSpecifier> = None;
  let mut named_imports: Vec<&ImportNamedSpecifier> = Vec::new();

  for specifier in node.specifiers {
    match specifier {
      ImportSpecifier::Default(node) => default_import = Some(node),
      ImportSpecifier::Namespace(node) => namespace_import = Some(node),
      ImportSpecifier::Named(node) => named_imports.push(node),
    }
  }

  let force_single_line = context.config.import_declaration_force_single_line && !contains_line_or_multiline_comment(node.into(), context.program);

  let force_multi_line = context.config.import_declaration_force_multi_line == ForceMultiLine::Always
    || (named_imports.len() > 1 && context.config.import_declaration_force_multi_line == ForceMultiLine::WhenMultiple);

  let should_single_line = force_single_line
    || (default_import.is_none()
      && namespace_import.is_none()
      && !force_multi_line
      && (named_imports.len() <= 1 && context.config.import_declaration_force_multi_line == ForceMultiLine::Never)
      && node.start_line_fast(context.program) == node.end_line_fast(context.program));

  let has_named_imports = !named_imports.is_empty() || {
    let from_keyword = context.token_finder.get_previous_token_if_from_keyword(node.src);
    if let Some(from_keyword) = from_keyword {
      context.token_finder.get_previous_token_if_close_brace(&from_keyword.range()).is_some()
    } else {
      false
    }
  };
  let has_from = default_import.is_some() || namespace_import.is_some() || has_named_imports;
  let mut items = PrintItems::new();

  items.push_sc(sc!("import "));
  if node.type_only() {
    items.push_sc(sc!("type "));
  }

  match node.phase() {
    ImportPhase::Evaluation => {
      // ignore
    }
    ImportPhase::Source => {
      items.push_sc(sc!("source "));
    }
    ImportPhase::Defer => {
      items.push_sc(sc!("defer "));
    }
  }

  if let Some(default_import) = default_import {
    items.extend(gen_node(default_import.into(), context));
    if namespace_import.is_some() || has_named_imports {
      items.push_sc(sc!(", "));
    }
  }
  if let Some(namespace_import) = namespace_import {
    items.extend(gen_node(namespace_import.into(), context));
  }

  if has_named_imports {
    items.extend(gen_named_import_or_export_specifiers(
      GenNamedImportOrExportSpecifierOptions {
        parent: node.into(),
        specifiers: named_imports.into_iter().map(|x| x.into()).collect(),
        force_single_line,
        force_multi_line_specifiers: force_multi_line,
      },
      context,
    ));
  }

  if has_from {
    items.push_sc(sc!(" from "));
  }

  items.extend(gen_node(node.src.into(), context));

  if let Some(with_clause) = node.with {
    items.extend(gen_with_clause(with_clause, context));
  }

  if context.config.semi_colons.is_true() {
    items.push_sc(sc!(";"));
  }

  if should_single_line {
    with_no_new_lines(items)
  } else {
    items
  }
}

fn gen_import_equals_decl<'a>(node: &TsImportEqualsDecl<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  if node.is_export() {
    items.push_sc(sc!("export "));
  }

  items.push_sc(sc!("import "));
  if node.is_type_only() {
    items.push_sc(sc!("type "));
  }
  items.extend(gen_node(node.id.into(), context));
  items.push_sc(sc!(" = ")); // keep on one line
  items.extend(gen_node(node.module_ref.into(), context));

  if context.config.semi_colons.is_true() {
    items.push_sc(sc!(";"));
  }

  items
}

fn gen_interface_decl<'a>(node: &TsInterfaceDecl<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  let start_header_lsil = LineStartIndentLevel::new("startHeader");
  items.push_info(start_header_lsil);
  context.store_lsil_for_node(node, start_header_lsil);

  if node.declare() {
    items.push_sc(sc!("declare "));
  }
  items.push_sc(sc!("interface "));
  items.extend(gen_node(node.id.into(), context));
  if let Some(type_params) = node.type_params {
    items.extend(gen_node(type_params.into(), context));
  }
  items.extend(gen_extends_or_implements(
    GenExtendsOrImplementsOptions {
      text: sc!("extends"),
      type_items: node.extends.iter().map(|&x| x.into()).collect(),
      start_header_lsil,
      prefer_hanging: context.config.extends_clause_prefer_hanging,
    },
    context,
  ));
  items.extend(gen_node(node.body.into(), context));

  items
}

fn gen_module_decl<'a>(node: &TsModuleDecl<'a>, context: &mut Context<'a>) -> PrintItems {
  gen_module_or_namespace_decl(
    ModuleOrNamespaceDecl {
      declare: node.declare(),
      global: node.global(),
      id: node.id.into(),
      body: node.body.as_ref(),
    },
    context,
  )
}

fn gen_namespace_decl<'a>(node: &TsNamespaceDecl<'a>, context: &mut Context<'a>) -> PrintItems {
  gen_module_or_namespace_decl(
    ModuleOrNamespaceDecl {
      declare: node.declare(),
      global: node.global(),
      id: node.id.into(),
      body: Some(&node.body),
    },
    context,
  )
}

struct ModuleOrNamespaceDecl<'a, 'b> {
  pub declare: bool,
  pub global: bool,
  pub id: Node<'a>,
  pub body: Option<&'b TsNamespaceBody<'a>>,
}

fn gen_module_or_namespace_decl<'a, 'b>(node: ModuleOrNamespaceDecl<'a, 'b>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();

  let start_header_lsil = LineStartIndentLevel::new("startHeader");
  items.push_info(start_header_lsil);

  if node.declare {
    items.push_sc(sc!("declare "));
  }
  if !node.global {
    let module_or_namespace_keyword = node.id.previous_token_fast(context.program).unwrap();
    let has_namespace_keyword = module_or_namespace_keyword.text_fast(context.program).starts_with('n');
    items.push_sc(if has_namespace_keyword { sc!("namespace ") } else { sc!("module ") });
  }

  items.extend(gen_node(node.id, context));
  items.extend(gen_body(node.body, start_header_lsil, context));

  return items;

  fn gen_body<'a>(body: Option<&TsNamespaceBody<'a>>, start_header_lsil: LineStartIndentLevel, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    if let Some(body) = body {
      match body {
        TsNamespaceBody::TsModuleBlock(block) => {
          items.extend(gen_membered_body(
            GenMemberedBodyOptions {
              node: (*block).into(),
              members: block.body.iter().map(|x| x.into()).collect(),
              start_header_lsil: Some(start_header_lsil),
              brace_position: context.config.module_declaration_brace_position,
              should_use_blank_line: move |previous, next, context| node_helpers::has_separating_blank_line(&previous, &next, context.program),
              separator: Separator::none(),
            },
            context,
          ));
        }
        TsNamespaceBody::TsNamespaceDecl(decl) => {
          items.push_sc(sc!("."));
          items.extend(gen_node(decl.id.into(), context));
          items.extend(gen_body(Some(&decl.body), start_header_lsil, context));
        }
      }
    } else if context.config.semi_colons.is_true() {
      items.push_sc(sc!(";"));
    }

    items
  }
}

fn gen_type_alias<'a>(node: &TsTypeAliasDecl<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  if node.declare() {
    items.push_sc(sc!("declare "));
  }
  items.push_sc(sc!("type "));
  items.extend(gen_node(node.id.into(), context));
  if let Some(type_params) = node.type_params {
    items.extend(gen_node(type_params.into(), context));
  }

  items.extend(gen_assignment(node.type_ann.into(), sc!("="), context));

  if context.config.semi_colons.is_true() {
    items.push_sc(sc!(";"));
  }

  items
}

fn gen_using_decl<'a>(node: &UsingDecl<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  if node.is_await() {
    items.push_sc(sc!("await "));
  }
  items.push_sc(sc!("using "));

  items.extend(gen_var_declarators(node.into(), node.decls, context));

  if requires_semi_colon_for_var_or_using_decl(node.into(), context) {
    items.push_sc(sc!(";"));
  }

  items
}

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
      allow_blank_lines: false,
      node_sorter: get_node_sorter(opts.parent, context),
    },
    context,
  );

  fn get_trailing_commas(parent_decl: Node, context: &Context) -> TrailingCommas {
    match parent_decl {
      Node::NamedExport(_) => context.config.export_declaration_trailing_commas,
      Node::ImportDecl(_) => context.config.import_declaration_trailing_commas,
      _ => unreachable!(),
    }
  }

  fn get_use_space(parent_decl: Node, context: &Context) -> bool {
    match parent_decl {
      Node::NamedExport(_) => context.config.export_declaration_space_surrounding_named_exports,
      Node::ImportDecl(_) => context.config.import_declaration_space_surrounding_named_imports,
      _ => unreachable!(),
    }
  }

  fn get_prefer_hanging(parent_decl: Node, context: &Context) -> bool {
    match parent_decl {
      Node::NamedExport(_) => context.config.export_declaration_prefer_hanging,
      Node::ImportDecl(_) => context.config.import_declaration_prefer_hanging,
      _ => unreachable!(),
    }
  }

  fn get_prefer_single_line(parent_decl: Node, context: &Context) -> bool {
    match parent_decl {
      Node::NamedExport(_) => context.config.export_declaration_prefer_single_line,
      Node::ImportDecl(_) => context.config.import_declaration_prefer_single_line,
      _ => unreachable!(),
    }
  }

  fn get_node_sorter<'a>(
    parent_decl: Node,
    context: &Context<'a>,
  ) -> Option<Box<dyn Fn((usize, Option<Node<'a>>), (usize, Option<Node<'a>>), Program<'a>) -> std::cmp::Ordering>> {
    match parent_decl {
      Node::NamedExport(_) => get_node_sorter_from_order(
        context.config.export_declaration_sort_named_exports,
        context.config.export_declaration_sort_type_only_exports,
      ),
      Node::ImportDecl(_) => get_node_sorter_from_order(
        context.config.import_declaration_sort_named_imports,
        context.config.import_declaration_sort_type_only_imports,
      ),
      _ => unreachable!(),
    }
  }
}

/* expressions */

fn gen_array_expr<'a>(node: &ArrayLit<'a>, context: &mut Context<'a>) -> PrintItems {
  let prefer_hanging = match context.config.array_expression_prefer_hanging {
    PreferHanging::Never => false,
    PreferHanging::OnlySingleItem => node.elems.len() == 1,
    PreferHanging::Always => true,
  };
  gen_array_like_nodes(
    GenArrayLikeNodesOptions {
      node: node.into(),
      nodes: node.elems.iter().map(|&x| x.map(|elem| elem.into())).collect(),
      prefer_hanging,
      prefer_single_line: context.config.array_expression_prefer_single_line,
      trailing_commas: context.config.array_expression_trailing_commas,
      space_around: context.config.array_expression_space_around,
    },
    context,
  )
}

fn gen_arrow_func_expr<'a>(node: &'a ArrowExpr<'a>, context: &mut Context<'a>) -> PrintItems {
  let items = gen_inner(node, context);
  return if should_add_parens_around_expr(node.into(), context) {
    surround_with_parens(items)
  } else {
    items
  };

  fn gen_inner<'a>(node: &'a ArrowExpr<'a>, context: &mut Context<'a>) -> PrintItems {
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

  fn gen_inner_arrow<'a>(node: &'a ArrowExpr<'a>, context: &mut Context<'a>) -> PrintItems {
    let (header_start_lsil, header_items) = gen_arrow_signature(&ArrowSignature::new(node), context);

    let is_arrow_in_test_call_expr = node
      .parent()
      .parent()
      .unwrap()
      .to::<CallExpr>()
      .map(|c| node_helpers::is_test_library_call_expr(c, context.program))
      .unwrap_or(false);
    let mut items = if is_arrow_in_test_call_expr {
      ir_helpers::with_no_new_lines(header_items)
    } else {
      header_items
    };

    items.extend(gen_arrow_body(node, header_start_lsil, context));

    items
  }

  fn gen_arrow_body<'a>(node: &'a ArrowExpr<'a>, header_start_lsil: LineStartIndentLevel, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    let generated_body = gen_node(node.body.into(), context);
    let generated_body = if use_new_line_group_for_arrow_body(node, context) {
      new_line_group(generated_body)
    } else {
      generated_body
    }
    .into_rc_path();
    let open_brace_token = match &node.body {
      BlockStmtOrExpr::BlockStmt(stmt) => context.token_finder.get_first_open_brace_token_within(*stmt),
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

      items.extend(generated_body.into());
    } else {
      let start_body_ln = LineNumber::new("startBody");
      let end_body_ln = LineNumber::new("endBody");
      items.push_info(start_body_ln);

      if should_not_newline_after_arrow(&node.body, context) {
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
      items.extend(gen_node(type_params.into(), context));
    }

    if should_use_parens {
      // need to check if there are parens because gen_parameters_or_arguments depends on the parens existing
      if has_parens(node, context.program) {
        items.extend(gen_parameters_or_arguments(
          GenParametersOrArgumentsOptions {
            node: node.inner.into(),
            range: node.inner.get_parameters_range(context),
            nodes: node.params().iter().map(|node| node.into()).collect(),
            custom_close_paren: |context| {
              Some(gen_close_paren_with_type(
                GenCloseParenWithTypeOptions {
                  start_lsil: header_start_lsil,
                  type_node: node.return_type().map(|x| x.into()),
                  type_node_separator: None,
                  param_count: node.params().len(),
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
        items.extend(surround_with_parens(gen_node(node.params().first().unwrap().into(), context)));
      }
    } else {
      items.extend(gen_node(node.params().first().unwrap().into(), context));
    }

    items.push_sc(sc!(" =>"));
    (header_start_lsil, items)
  }

  fn should_not_newline_after_arrow<'a>(body: &BlockStmtOrExpr<'a>, context: &Context<'a>) -> bool {
    match body {
      BlockStmtOrExpr::BlockStmt(_) => true,
      BlockStmtOrExpr::Expr(expr) => match expr {
        Expr::Paren(_) | Expr::Array(_) => true,
        Expr::Tpl(tpl) => tpl.quasis[0].raw().starts_with(|c: char| c == '\n' || c == '\r'),
        _ => is_jsx_paren_expr_handled_node(expr.into(), context),
      },
    }
  }

  fn get_should_use_parens<'a>(node: &ArrowSignature<'a>, context: &Context<'a>) -> bool {
    return match context.config.arrow_function_use_parentheses {
      UseParentheses::Force => true,
      UseParentheses::PreferNone => {
        node.params().len() != 1 || node.return_type().is_some() || is_first_param_not_identifier_or_has_type_annotation(node.params(), node, context.program)
      }
      UseParentheses::Maintain => has_parens(node, context.program),
    };

    fn is_first_param_not_identifier_or_has_type_annotation<'a>(params: &[Pat<'a>], arrow: &ArrowSignature<'a>, program: Program<'a>) -> bool {
      match params.first() {
        Some(Pat::Ident(node)) => {
          node.type_ann.is_some() || node.id.optional() || param_has_surrounding_comments((*node).into(), program) && has_parens(arrow, program)
        }
        _ => true,
      }
    }

    fn param_has_surrounding_comments<'a>(param: Node<'a>, program: Program<'a>) -> bool {
      if node_helpers::has_surrounding_comments(param, program) {
        true
      } else if let Some(comma_token) = param.next_token_fast(program).filter(|t| t.token == Token::Comma) {
        !comma_token.trailing_comments_fast(program).is_empty()
      } else {
        false
      }
    }
  }

  fn has_parens<'a>(node: &ArrowSignature<'a>, program: Program<'a>) -> bool {
    if node.params().len() != 1 {
      true
    } else {
      for node_or_token in node.inner.children_with_tokens_fast(program) {
        match node_or_token {
          NodeOrToken::Node(_) => return false, // first param, so no parens
          NodeOrToken::Token(TokenAndSpan { token: Token::LParen, .. }) => return true,
          _ => {}
        }
      }
      false
    }
  }
}

fn gen_as_expr<'a>(node: &TsAsExpr<'a>, context: &mut Context<'a>) -> PrintItems {
  gen_as_expr_like(
    AsExprLike {
      expr: node.expr.into(),
      type_ann: node.type_ann.into(),
    },
    context,
  )
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

fn gen_satisfies_expr<'a>(node: &TsSatisfiesExpr<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = gen_node(node.expr.into(), context);
  items.push_sc(sc!(" satisfies"));
  items.push_signal(Signal::SpaceIfNotTrailing);
  items.push_condition(conditions::with_indent_if_start_of_line_indented(gen_node(node.type_ann.into(), context)));
  items
}

fn gen_const_assertion<'a>(node: &TsConstAssertion<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = gen_node(node.expr.into(), context);
  items.push_sc(sc!(" as const"));
  items
}

fn gen_assignment_expr<'a>(node: &AssignExpr<'a>, context: &mut Context<'a>) -> PrintItems {
  // check for a nested assignment (ex. `a = b = c`)
  if node.op() == AssignOp::Assign {
    if let Expr::Assign(right) = node.right {
      if right.op() == AssignOp::Assign {
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
  items.extend(gen_node(node.left.into(), context));
  items.extend(gen_assignment(node.right.into(), assign_op_sc(node.op()), context));
  items
}

fn gen_await_expr<'a>(node: &AwaitExpr<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.push_sc(sc!("await "));
  items.extend(gen_node(node.arg.into(), context));
  items
}

fn gen_binary_expr<'a>(node: &BinExpr<'a>, context: &mut Context<'a>) -> PrintItems {
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
  let allow_no_indent = get_allow_no_indent(node);
  let use_space_surrounding_operator = get_use_space_surrounding_operator(&node.op(), context);
  let is_parent_bin_expr = node.parent().kind() == NodeKind::BinExpr;
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
          let is_inner_binary_expression = bin_expr_item.expr.kind() == NodeKind::BinExpr;
          items.extend(gen_node_with_inner_gen(bin_expr_item.expr, context, |node_items, context| {
            let mut items = PrintItems::new();
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

  fn get_allow_no_indent(node: &BinExpr) -> bool {
    let parent = node.parent();
    let parent_kind = parent.kind();
    if !node.op().is_add_sub()
      && !node.op().is_mul_div()
      && !node.op().is_logical()
      && !node.op().is_bit_logical()
      && !node.op().is_bit_shift()
      && node.op() != BinaryOp::Mod
    {
      false
    } else if parent_kind == NodeKind::ExprStmt || parent_kind == NodeKind::BinExpr {
      false
    } else {
      // get if in an argument
      match parent {
        Node::ExprOrSpread(expr_or_spread) => !matches!(expr_or_spread.parent().kind(), NodeKind::CallExpr | NodeKind::OptCall | NodeKind::NewExpr),
        _ => true,
      }
    }
  }

  fn get_use_space_surrounding_operator(op: &BinaryOp, context: &Context) -> bool {
    if op.is_bitwise_or_arithmetic() {
      context.config.binary_expression_space_surrounding_bitwise_and_arithmetic_operator
    } else {
      true
    }
  }

  fn should_newline_group_bin_item_expr<'a>(node: Node<'a>, context: &Context<'a>) -> bool {
    if let Some(node) = node.to::<ParenExpr>() {
      return should_newline_group_bin_item_expr(node.expr.into(), context);
    }

    if is_jsx_paren_expr_handled_node(node, context) {
      // prefer using the possible newline at the start of the element
      return false;
    }

    true
  }
}

fn gen_call_or_opt_expr<'a>(node: CallOrOptCallExpr<'a>, context: &mut Context<'a>) -> PrintItems {
  if let CallOrOptCallExpr::CallExpr(node) = node {
    if node_helpers::is_test_library_call_expr(node, context.program) {
      return gen_test_library_call_expr(node, context);
    }
  }

  // flatten the call expression and check if it should be generated as a flattened member like expression
  let flattened_call_expr = flatten_member_like_expr(node.into(), context.program);
  return if flattened_call_expr.nodes.len() > 1 {
    gen_for_flattened_member_like_expr(flattened_call_expr, context)
  } else {
    gen_call_expr_like(
      CallExprLike {
        original_call_expr: node,
        generated_callee: gen_node(node.callee().into(), context),
      },
      context,
    )
  };

  fn gen_test_library_call_expr<'a>(node: &CallExpr<'a>, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    items.extend(gen_test_library_callee(&node.callee, context));
    items.extend(gen_test_library_arguments(node.args, context));
    return items;

    fn gen_test_library_callee<'a, 'b>(callee: &'b Callee<'a>, context: &mut Context<'a>) -> PrintItems {
      match callee {
        Callee::Expr(expr) => match expr {
          Expr::Member(member_expr) => {
            let mut items = PrintItems::new();
            items.extend(gen_node(member_expr.obj.into(), context));
            items.push_sc(sc!("."));
            items.extend(gen_node(member_expr.prop.into(), context));
            items
          }
          _ => gen_node(expr.into(), context),
        },
        _ => gen_node(callee.into(), context),
      }
    }

    fn gen_test_library_arguments<'a>(args: &[&'a ExprOrSpread<'a>], context: &mut Context<'a>) -> PrintItems {
      let mut items = PrintItems::new();
      items.extend(gen_node_with_inner_gen(args[0].into(), context, |items, _| {
        let mut new_items = ir_helpers::with_no_new_lines(items);
        new_items.push_sc(sc!(","));
        new_items
      }));
      items.push_space();
      items.extend(gen_node(args[1].into(), context));

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
    items.extend(gen_node(type_args.into(), context));
  }

  if call_expr.is_optional() {
    items.push_sc(sc!("?."));
  }

  items.push_condition(conditions::with_indent_if_start_of_line_indented(gen_parameters_or_arguments(
    GenParametersOrArgumentsOptions {
      node: call_expr.into(),
      range: call_expr.get_parameters_range(context),
      nodes: call_expr.args().iter().map(|&node| node.into()).collect(),
      custom_close_paren: |_| None,
      is_parameters: false,
    },
    context,
  )));

  items
}

fn gen_import_callee<'a>(node: &Import<'a>, _context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.push_sc(sc!("import"));
  // for now, just be simple and force this on one line
  match node.phase() {
    ImportPhase::Evaluation => {
      // nothing to do
    }
    ImportPhase::Source => {
      items.push_sc(sc!(".source"));
    }
    ImportPhase::Defer => {
      items.push_sc(sc!(".defer"));
    }
  }
  items
}

fn gen_class_expr<'a>(node: &ClassExpr<'a>, context: &mut Context<'a>) -> PrintItems {
  let is_class_decl = node.parent().kind() == NodeKind::ExportDefaultDecl && node.ident.is_some();
  gen_class_decl_or_expr(
    ClassDeclOrExpr {
      node: node.into(),
      member_node: node.class.into(),
      decorators: node.class.decorators,
      is_class_decl,
      is_declare: false,
      is_abstract: node.class.is_abstract(),
      ident: node.ident.map(|x| x.into()),
      type_params: node.class.type_params.map(|x| x.into()),
      super_class: node.class.super_class.map(|x| x.into()),
      super_type_params: node.class.super_type_params.map(|x| x.into()),
      implements: node.class.implements.iter().map(|&x| x.into()).collect(),
      members: node.class.body.iter().map(|x| x.into()).collect(),
      brace_position: if is_class_decl {
        context.config.class_declaration_brace_position
      } else {
        context.config.class_expression_brace_position
      },
    },
    context,
  )
}

fn gen_conditional_expr<'a>(node: &CondExpr<'a>, context: &mut Context<'a>) -> PrintItems {
  let question_token = context.token_finder.get_first_operator_after(&node.test, "?").unwrap();
  let colon_token = context.token_finder.get_first_operator_after(&node.cons, ":").unwrap();
  let line_per_expression = context.config.conditional_expression_line_per_expression;
  let has_newline_test_cons = node_helpers::get_use_new_lines_for_nodes(&node.test, &node.cons, context.program);
  let has_newline_const_alt = node_helpers::get_use_new_lines_for_nodes(&node.cons, &node.alt, context.program);
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
    let mut items = gen_node(node.test.into(), context);
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
      items.extend(gen_node(node.cons.into(), context));
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
      items.extend(gen_node(node.alt.into(), context));
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

  fn get_top_most_data<'a>(node: &CondExpr<'a>, context: &mut Context<'a>) -> TopMostData {
    // The "top most" node in nested conditionals follows the ancestors up through
    // the alternate expressions.
    let mut top_most_node = node;

    for ancestor in context.parent_stack.iter() {
      if let Node::CondExpr(parent) = ancestor {
        if parent.alt.start() == top_most_node.start() {
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

  fn get_operator_position(
    node: &CondExpr,
    question_token: &TokenAndSpan,
    colon_token: &TokenAndSpan,
    context: &mut Context,
  ) -> (OperatorPosition, OperatorPosition) {
    fn get_maintain_position(node: &CondExpr, expr: Expr, token: &TokenAndSpan, context: &mut Context) -> OperatorPosition {
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
        get_maintain_position(node, node.test, question_token, context),
        get_maintain_position(node, node.cons, colon_token, context),
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
fn gen_cond_token_comments(token: &TokenAndSpan, context: &mut Context, top_most_il: IndentLevel) -> ConditionalTokenComments {
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

fn gen_expr_or_spread<'a>(node: &ExprOrSpread<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  if node.spread().is_some() {
    items.push_sc(sc!("..."));
  }
  items.extend(gen_node(node.expr.into(), context));
  items
}

fn gen_expr_with_type_args<'a>(node: &TsExprWithTypeArgs<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.extend(gen_node(node.expr.into(), context));
  if let Some(type_args) = node.type_args {
    items.extend(gen_node(type_args.into(), context));
  }
  items
}

fn gen_fn_expr<'a>(node: &FnExpr<'a>, context: &mut Context<'a>) -> PrintItems {
  let items = gen_function_decl_or_expr(
    FunctionDeclOrExprNode {
      node: node.into(),
      is_func_decl: node.parent().kind() == NodeKind::ExportDefaultDecl && node.ident.is_some(),
      ident: node.ident,
      declare: false,
      func: node.function,
    },
    context,
  );

  if should_add_parens_around_expr(node.into(), context) {
    if context.config.paren_expression_space_around {
      surround_with_parens(surround_with_spaces(items))
    } else {
      surround_with_parens(items)
    }
  } else {
    items
  }
}

fn should_add_parens_around_expr<'a>(node: Node<'a>, context: &Context<'a>) -> bool {
  let original_node = node;
  for node in node.ancestors() {
    match node {
      Node::ParenExpr(paren_expr) => {
        if !should_skip_paren_expr(paren_expr, context) {
          return false;
        }
      }
      Node::CallExpr(call_expr) => {
        if !call_expr.callee.range().contains(&original_node.range()) {
          // it's in an argument, so don't add parens
          return false;
        }
      }
      Node::OptCall(call_expr) => {
        if !call_expr.callee.range().contains(&original_node.range()) {
          // it's in an argument, so don't add parens
          return false;
        }
      }
      Node::NewExpr(new_expr) => {
        if !new_expr.callee.range().contains(&original_node.range()) {
          // it's in an argument, so don't add parens
          return false;
        }
      }
      Node::ExprStmt(_) => return true,
      Node::MemberExpr(expr) => {
        if matches!(expr.prop, MemberProp::Computed(_)) && expr.prop.range().contains(&original_node.range()) {
          return false;
        }
      }
      Node::CondExpr(cond_expr) => {
        return cond_expr.test.range().contains(&original_node.range());
      }
      Node::BinExpr(bin_expr) => {
        // we only care to add parens when it's the left most expr
        if bin_expr.right.range().contains(&original_node.range()) {
          return false;
        }
      }
      Node::OptChainExpr(_) => {
        // continue searching
      }
      _ => {
        return false;
      }
    }
  }

  false
}

fn gen_getter_prop<'a>(node: &GetterProp<'a>, context: &mut Context<'a>) -> PrintItems {
  gen_class_or_object_method(
    ClassOrObjectMethod {
      node: node.into(),
      parameters_range: node.get_parameters_range(context),
      decorators: None,
      accessibility: None,
      is_static: false,
      is_async: false,
      is_abstract: false,
      kind: ClassOrObjectMethodKind::Getter,
      is_generator: false,
      is_optional: false,
      is_override: false,
      key: node.key.into(),
      type_params: None,
      params: Vec::new(),
      return_type: node.type_ann.map(|x| x.into()),
      body: node.body.map(|x| x.into()),
    },
    context,
  )
}

fn gen_key_value_prop<'a>(node: &KeyValueProp<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.extend(gen_quotable_prop(node.key.into(), context));
  items.extend(gen_assignment(node.value.into(), sc!(":"), context));
  items
}

fn gen_assign_prop<'a>(node: &AssignProp<'a>, context: &mut Context<'a>) -> PrintItems {
  // assignment properties are not valid, so turn this into a key value property
  let mut items = PrintItems::new();
  items.extend(gen_node(node.key.into(), context));
  items.extend(gen_assignment_op_to(node.value.into(), "=", sc!(":"), context)); // go from = to :
  items
}

fn gen_member_expr<'a>(node: &MemberExpr<'a>, context: &mut Context<'a>) -> PrintItems {
  let flattened_member_expr = flatten_member_like_expr(node.into(), context.program);
  gen_for_flattened_member_like_expr(flattened_member_expr, context)
}

fn gen_meta_prop_expr<'a>(node: &MetaPropExpr<'a>, context: &mut Context<'a>) -> PrintItems {
  let flattened_meta_prop_expr = flatten_member_like_expr(node.into(), context.program);
  gen_for_flattened_member_like_expr(flattened_meta_prop_expr, context)
}

fn gen_super_prop_expr<'a>(node: &SuperPropExpr<'a>, context: &mut Context<'a>) -> PrintItems {
  let flattened_member_expr = flatten_member_like_expr(node.into(), context.program);
  gen_for_flattened_member_like_expr(flattened_member_expr, context)
}

fn gen_new_expr<'a>(node: &NewExpr<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.push_sc(sc!("new "));
  items.extend(gen_node(node.callee.into(), context));
  if let Some(type_args) = node.type_args {
    items.extend(gen_node(type_args.into(), context));
  }
  let args = match node.args.as_ref() {
    Some(args) => args.iter().map(|&node| node.into()).collect(),
    None => Vec::new(),
  };
  items.extend(gen_parameters_or_arguments(
    GenParametersOrArgumentsOptions {
      node: node.into(),
      range: node.get_parameters_range(context),
      nodes: args,
      custom_close_paren: |_| None,
      is_parameters: false,
    },
    context,
  ));
  items
}

fn gen_non_null_expr<'a>(node: &TsNonNullExpr<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.extend(gen_node(node.expr.into(), context));
  items.push_sc(sc!("!"));
  items
}

fn gen_object_lit<'a>(node: &ObjectLit<'a>, context: &mut Context<'a>) -> PrintItems {
  let items = context.with_maybe_consistent_props(
    node,
    |node| use_consistent_quotes_for_members(node.props.iter().map(|p| p.into())),
    |context, node| {
      gen_object_like_node(
        GenObjectLikeNodeOptions {
          node: node.into(),
          members: node.props.iter().map(|x| x.into()).collect(),
          separator: context.config.object_expression_trailing_commas.into(),
          prefer_hanging: context.config.object_expression_prefer_hanging,
          prefer_single_line: context.config.object_expression_prefer_single_line,
          force_single_line: false,
          force_multi_line: is_node_definitely_above_line_width(node.range(), context),
          surround_single_line_with_spaces: context.config.object_expression_space_surrounding_properties,
          allow_blank_lines: true,
          node_sorter: None,
        },
        context,
      )
    },
  );

  if should_add_parens_around_expr(node.into(), context) {
    surround_with_parens(if context.config.paren_expression_space_around {
      surround_with_spaces(items)
    } else {
      items
    })
  } else {
    items
  }
}

fn gen_paren_expr<'a>(node: &'a ParenExpr<'a>, context: &mut Context<'a>) -> PrintItems {
  if should_skip_paren_expr(node, context) {
    return gen_node(node.expr.into(), context);
  }

  let generated_items = conditions::with_indent_if_start_of_line_indented(gen_node_in_parens(
    |context| gen_node(node.expr.into(), context),
    GenNodeInParensOptions {
      inner_range: node.expr.range(),
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

  fn get_use_new_line_group<'a>(node: &'a ParenExpr<'a>, context: &Context<'a>) -> bool {
    if let Node::ArrowExpr(arrow_expr) = node.parent() {
      debug_assert!(arrow_expr.body.start() == node.start());
      use_new_line_group_for_arrow_body(arrow_expr, context)
    } else {
      true
    }
  }
}

fn should_skip_paren_expr<'a>(node: &'a ParenExpr<'a>, context: &Context<'a>) -> bool {
  if node_helpers::has_surrounding_different_line_comments(node.expr.into(), context.program) {
    return false;
  }

  // keep parens around any destructuring assignments
  if let Node::AssignExpr(assign_expr) = node.expr.as_node() {
    let left_kind = assign_expr.left.kind();
    if matches!(left_kind, NodeKind::ObjectPat) {
      return false;
    }
  }

  if matches!(node.expr.kind(), NodeKind::SeqExpr) {
    // don't care about extra logic for sequence expressions
    return false;
  }

  // keep when there is a JSDoc type assertion
  for c in node.leading_comments_fast(context.program) {
    if c.kind == CommentKind::Block && c.text.starts_with('*') && c.text.contains("@type") {
      return false;
    }
  }

  // keep for `(val as number)++` or `(<number>val)++`
  let parent = node.parent();
  if parent.kind() == NodeKind::UpdateExpr && matches!(node.expr.kind(), NodeKind::TsAsExpr | NodeKind::TsTypeAssertion) {
    return false;
  }

  if matches!(node.expr.kind(), NodeKind::ArrayLit) || matches!(node.expr, Expr::Ident(_)) {
    return true;
  }

  if parent.kind() == NodeKind::MemberExpr && node.expr.kind() == NodeKind::MemberExpr {
    return true;
  }

  // skip over any paren exprs within paren exprs and needless paren exprs
  if matches!(
    parent.kind(),
    NodeKind::ParenExpr
      | NodeKind::ExprStmt
      | NodeKind::JSXElement
      | NodeKind::JSXFragment
      | NodeKind::JSXExprContainer
      | NodeKind::UpdateExpr
      | NodeKind::ComputedPropName
  ) {
    return true;
  }

  // skip explicitly parsing this as a paren expr as that will be handled
  // in the JSX element/fragment and it might collapse back to not having a paren expr
  if matches!(node.expr.kind(), NodeKind::JSXElement | NodeKind::JSXFragment) {
    return is_jsx_paren_expr_handled_node(node.expr.into(), context);
  }

  if let Node::AssignExpr(assign_expr) = parent {
    if assign_expr.right.range().contains(&node.range()) {
      return true;
    }
  }

  if let Node::VarDeclarator(var_decl) = parent {
    if node.expr.kind() != NodeKind::AssignExpr {
      if let Some(init) = var_decl.init {
        if init.range().contains(&node.range()) {
          return true;
        }
      }
    }
  }

  // skip over an expr or spread if not a spread
  if let Some(expr_or_spread) = parent.to::<ExprOrSpread>() {
    // these should only appear in these nodes
    let is_known_parent = matches!(
      expr_or_spread.parent().kind(),
      NodeKind::NewExpr | NodeKind::ArrayLit | NodeKind::CallExpr | NodeKind::OptCall
    );
    debug_assert!(is_known_parent);
    if is_known_parent && expr_or_spread.spread().is_none() {
      return true;
    }
  }

  if let Node::MemberExpr(member_expr) = parent {
    if matches!(member_expr.prop, MemberProp::Computed(_)) && member_expr.prop.range().contains(&node.range()) {
      return true;
    }
  }

  false
}

fn gen_sequence_expr<'a>(node: &SeqExpr<'a>, context: &mut Context<'a>) -> PrintItems {
  gen_separated_values(
    GenSeparatedValuesParams {
      nodes: node.exprs.iter().map(|x| NodeOrSeparator::Node(x.into())).collect(),
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

fn gen_setter_prop<'a>(node: &SetterProp<'a>, context: &mut Context<'a>) -> PrintItems {
  gen_class_or_object_method(
    ClassOrObjectMethod {
      node: node.into(),
      parameters_range: node.get_parameters_range(context),
      decorators: None,
      accessibility: None,
      is_static: false,
      is_async: false,
      is_abstract: false,
      kind: ClassOrObjectMethodKind::Setter,
      is_generator: false,
      is_optional: false,
      is_override: false,
      key: node.key.into(),
      type_params: None,
      params: vec![node.param.into()],
      return_type: None,
      body: node.body.map(|x| x.into()),
    },
    context,
  )
}

fn gen_spread_element<'a>(node: &SpreadElement<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.push_sc(sc!("..."));
  items.extend(gen_node(node.expr.into(), context));
  items
}

fn gen_tagged_tpl<'a>(node: &TaggedTpl<'a>, context: &mut Context<'a>) -> PrintItems {
  let use_space = context.config.tagged_template_space_before_literal;
  let mut items = gen_node(node.tag.into(), context);
  if let Some(type_params) = node.type_params {
    items.extend(gen_node(type_params.into(), context));
  }

  let generated_between_comments = gen_comments_between_lines_indented(node.tag.end(), context);
  if generated_between_comments.is_empty() {
    if use_space {
      items.push_signal(Signal::SpaceIfNotTrailing);
    }
  } else {
    items.extend(generated_between_comments);
  }

  items.push_condition(conditions::indent_if_start_of_line(gen_node(node.tpl.into(), context)));
  items
}

fn gen_tpl<'a>(node: &Tpl<'a>, context: &mut Context<'a>) -> PrintItems {
  gen_template_literal(
    node.quasis.iter().map(|&n| n.into()).collect(),
    node.exprs.iter().map(|x| x.into()).collect(),
    context,
  )
}

fn gen_tpl_element<'a>(node: &TplElement<'a>, context: &mut Context<'a>) -> PrintItems {
  gen_from_raw_string(node.text_fast(context.program))
}

fn gen_template_literal<'a>(quasis: Vec<Node<'a>>, exprs: Vec<Node<'a>>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  let mut is_head = true;
  for node in get_nodes(quasis, exprs) {
    if let Node::TplElement(node_tpl) = node {
      items.extend(gen_node_with_inner_gen(node, context, |node_items, _| {
        let mut items = PrintItems::new();
        items.push_sc(if is_head { sc!("`") } else { sc!("}") });
        items.push_signal(Signal::StartIgnoringIndent);
        items.extend(node_items);
        items.push_sc(if node_tpl.tail() { sc!("`") } else { sc!("${") });
        items.push_signal(Signal::FinishIgnoringIndent);
        items
      }));
    } else {
      let leading_comment_items = gen_leading_comments_on_previous_lines(&node.range(), context);
      let trailing_comment_items = gen_trailing_comments_as_statements(&node.range(), context);
      let generated_expr = gen_node(node, context);
      if !leading_comment_items.is_empty() || !trailing_comment_items.is_empty() {
        items.push_signal(Signal::StartIndent);
        if !leading_comment_items.is_empty() {
          items.extend(surround_with_new_lines(leading_comment_items));
        }
        items.extend(generated_expr);
        if !trailing_comment_items.is_empty() {
          items.extend(trailing_comment_items);
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
      Node::Ident(_) | Node::IdentName(_) | Node::ThisExpr(_) | Node::SuperPropExpr(_) | Node::MetaPropExpr(_) | Node::Str(_) | Node::PrivateName(_) => true,
      Node::OptChainExpr(expr) => get_keep_on_one_line(expr.base.as_node()),
      Node::MemberExpr(expr) => keep_member_expr_on_one_line(expr),
      Node::CallExpr(expr) => keep_call_expr_on_one_line(expr.into()),
      Node::OptCall(expr) => keep_call_expr_on_one_line(expr.into()),
      _ => false,
    }
  }

  fn get_possible_surround_newlines(node: Node) -> bool {
    match node {
      Node::OptChainExpr(expr) => get_possible_surround_newlines(expr.base.as_node()),
      Node::CondExpr(_) => true,
      Node::BinExpr(_) => true,
      Node::MemberExpr(expr) => !keep_member_expr_on_one_line(expr),
      Node::CallExpr(expr) => !keep_call_expr_on_one_line(expr.into()),
      Node::OptCall(expr) => !keep_call_expr_on_one_line(expr.into()),
      _ => false,
    }
  }

  fn keep_member_expr_on_one_line(expr: &MemberExpr) -> bool {
    get_keep_on_one_line(expr.obj.into()) && get_keep_on_one_line(expr.prop.into()) && !matches!(expr.prop, MemberProp::Computed(_))
  }

  fn keep_call_expr_on_one_line(expr: CallOrOptCallExpr) -> bool {
    expr.args().is_empty() && get_keep_on_one_line(expr.callee().into())
  }
}

fn gen_type_assertion<'a>(node: &TsTypeAssertion<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.push_sc(sc!("<"));
  items.extend(gen_node(node.type_ann.into(), context));
  items.push_sc(sc!(">"));
  if context.config.type_assertion_space_before_expression {
    items.push_space();
  }
  items.extend(gen_node(node.expr.into(), context));
  items
}

fn gen_unary_expr<'a>(node: &UnaryExpr<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.push_sc(get_operator_text(node.op()));
  items.extend(gen_node(node.arg.into(), context));

  if get_should_use_parens(node) {
    items = surround_with_parens(items);
  }

  return items;

  fn get_operator_text(op: UnaryOp) -> &'static StringContainer {
    match op {
      UnaryOp::Void => sc!("void "),
      UnaryOp::TypeOf => sc!("typeof "),
      UnaryOp::Delete => sc!("delete "),
      UnaryOp::Bang => sc!("!"),
      UnaryOp::Plus => sc!("+"),
      UnaryOp::Minus => sc!("-"),
      UnaryOp::Tilde => sc!("~"),
    }
  }

  fn get_should_use_parens(node: &UnaryExpr) -> bool {
    if let Node::BinExpr(parent) = node.parent() {
      return matches!(parent.op(), BinaryOp::In | BinaryOp::InstanceOf);
    }
    false
  }
}

fn gen_update_expr<'a>(node: &UpdateExpr<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  let operator_text = get_operator_text(node.op());
  if node.prefix() {
    items.push_sc(operator_text);
  }
  items.extend(gen_node(node.arg.into(), context));
  if !node.prefix() {
    items.push_sc(operator_text);
  }
  return items;

  fn get_operator_text(operator: UpdateOp) -> &'static StringContainer {
    match operator {
      UpdateOp::MinusMinus => sc!("--"),
      UpdateOp::PlusPlus => sc!("++"),
    }
  }
}

fn gen_yield_expr<'a>(node: &YieldExpr<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.push_sc(sc!("yield"));
  if node.delegate() {
    items.push_sc(sc!("*"));
  }
  if let Some(arg) = &node.arg {
    items.push_space();
    items.extend(gen_node(arg.into(), context));
  }
  items
}

/* exports */

fn gen_export_named_specifier<'a>(node: &ExportNamedSpecifier<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();

  if node.is_type_only() && !node.parent().type_only() {
    items.push_sc(sc!("type "));
  }

  items.extend(gen_node(node.orig.into(), context));
  if let Some(exported) = node.exported {
    items.push_signal(Signal::SpaceOrNewLine);
    items.push_condition(conditions::indent_if_start_of_line({
      let mut items = PrintItems::new();
      items.push_sc(sc!("as "));
      items.extend(gen_node(exported.into(), context));
      items
    }));
  }

  items
}

fn gen_namespace_export_specifier<'a>(node: &ExportNamespaceSpecifier<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.push_sc(sc!("* as "));
  items.extend(gen_node(node.name.into(), context));
  items
}

/* imports */

fn gen_import_named_specifier<'a>(node: &ImportNamedSpecifier<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();

  if node.is_type_only() && !node.parent().type_only() {
    items.push_sc(sc!("type "));
  }

  if let Some(imported) = node.imported {
    items.extend(gen_node(imported.into(), context));
    items.push_signal(Signal::SpaceOrNewLine);
    items.push_condition(conditions::indent_if_start_of_line({
      let mut items = PrintItems::new();
      items.push_sc(sc!("as "));
      items.extend(gen_node(node.local.into(), context));
      items
    }));
  } else {
    items.extend(gen_node(node.local.into(), context));
  }

  items
}

fn gen_import_namespace_specifier<'a>(node: &ImportStarAsSpecifier<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.push_sc(sc!("* as "));
  items.extend(gen_node(node.local.into(), context));
  items
}

fn gen_external_module_ref<'a>(node: &TsExternalModuleRef<'a>, context: &mut Context<'a>) -> PrintItems {
  // force everything on a single line
  let mut items = PrintItems::new();
  items.push_sc(sc!("require("));
  items.extend(gen_node(node.expr.into(), context));
  items.push_sc(sc!(")"));
  items
}

/* interface / type element */

fn gen_call_signature_decl<'a>(node: &TsCallSignatureDecl<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  let start_lsil = LineStartIndentLevel::new("startCallSignature");

  items.push_info(start_lsil);
  if let Some(type_params) = node.type_params {
    items.extend(gen_node(type_params.into(), context));
  }
  items.extend(gen_parameters_or_arguments(
    GenParametersOrArgumentsOptions {
      node: node.into(),
      range: node.get_parameters_range(context),
      nodes: node.params.iter().map(|node| node.into()).collect(),
      custom_close_paren: |context| {
        Some(gen_close_paren_with_type(
          GenCloseParenWithTypeOptions {
            start_lsil,
            type_node: node.type_ann.map(|x| x.into()),
            type_node_separator: None,
            param_count: node.params.len(),
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

fn gen_construct_signature_decl<'a>(node: &TsConstructSignatureDecl<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  let start_lsil = LineStartIndentLevel::new("startConstructSignature");

  items.push_info(start_lsil);
  items.push_sc(sc!("new"));
  if context.config.construct_signature_space_after_new_keyword {
    items.push_space();
  }
  if let Some(type_params) = node.type_params {
    items.extend(gen_node(type_params.into(), context));
  }
  items.extend(gen_parameters_or_arguments(
    GenParametersOrArgumentsOptions {
      node: node.into(),
      range: node.get_parameters_range(context),
      nodes: node.params.iter().map(|node| node.into()).collect(),
      custom_close_paren: |context| {
        Some(gen_close_paren_with_type(
          GenCloseParenWithTypeOptions {
            start_lsil,
            type_node: node.type_ann.map(|x| x.into()),
            type_node_separator: None,
            param_count: node.params.len(),
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

fn gen_index_signature<'a>(node: &TsIndexSignature<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();

  if node.is_static() {
    items.push_sc(sc!("static "));
  }
  if node.readonly() {
    items.push_sc(sc!("readonly "));
  }

  let param: Node<'a> = node.params.first().expect("Expected the index signature to have one parameter.").into();
  items.extend(gen_computed_prop_like(
    |context| gen_node(param, context),
    GenComputedPropLikeOptions {
      inner_node_range: param.range(),
    },
    context,
  ));
  items.extend(gen_type_ann_with_colon_if_exists(node.type_ann, context));

  if node.parent().is::<Class>() && context.config.semi_colons.is_true() {
    items.push_sc(sc!(";"));
  }

  items
}

fn gen_method_signature<'a>(node: &TsMethodSignature<'a>, context: &mut Context<'a>) -> PrintItems {
  gen_method_signature_like(
    MethodSignatureLike {
      node: node.into(),
      method_kind: MethodSignatureLikeKind::Method,
      computed: node.computed(),
      optional: node.optional(),
      key: node.key.into(),
      parameters_range: node.get_parameters_range(context),
      type_params: node.type_params.map(|p| p.into()),
      params: node.params.iter().map(|p| p.into()).collect(),
      type_ann: node.type_ann.map(|p| p.into()),
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

fn gen_property_signature<'a>(node: &TsPropertySignature<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  if node.readonly() {
    items.push_sc(sc!("readonly "));
  }

  items.extend(if node.computed() {
    gen_computed_prop_like(
      |context| gen_node(node.key.into(), context),
      GenComputedPropLikeOptions {
        inner_node_range: node.key.range(),
      },
      context,
    )
  } else {
    gen_quotable_prop(node.key.into(), context)
  });

  if node.optional() {
    items.push_sc(sc!("?"));
  }
  items.extend(gen_type_ann_with_colon_if_exists(node.type_ann, context));

  items
}

fn gen_quotable_prop<'a>(node: Node<'a>, context: &mut Context<'a>) -> PrintItems {
  match context.config.quote_props {
    QuoteProps::AsNeeded => match node {
      Node::Str(str_lit) if is_text_valid_identifier(str_lit.value()) => gen_from_raw_string(str_lit.value()),
      _ => gen_node(node, context),
    },
    QuoteProps::Consistent => match context.use_consistent_quote_props() {
      Some(true) => match node {
        // add quotes
        Node::Ident(ident) => string_literal::gen_non_jsx_text(ident.sym(), context),
        Node::IdentName(ident) => string_literal::gen_non_jsx_text(ident.sym(), context),
        _ => gen_node(node, context),
      },
      Some(false) => match node {
        // remove quotes
        Node::Str(str_lit) => gen_from_raw_string(str_lit.value()),
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

fn gen_interface_body<'a>(node: &TsInterfaceBody<'a>, context: &mut Context<'a>) -> PrintItems {
  let start_header_lsil = get_parent_lsil(node, context);

  return context.with_maybe_consistent_props(
    node,
    |node| use_consistent_quotes_for_members(node.body.iter().map(|n| n.into())),
    |context, node| {
      gen_membered_body(
        GenMemberedBodyOptions {
          node: node.into(),
          members: node.body.iter().map(|x| x.into()).collect(),
          start_header_lsil,
          brace_position: context.config.interface_declaration_brace_position,
          should_use_blank_line: move |previous, next, context| node_helpers::has_separating_blank_line(&previous, &next, context.program),
          separator: context.config.semi_colons.into(),
        },
        context,
      )
    },
  );

  fn get_parent_lsil(node: &TsInterfaceBody, context: &mut Context) -> Option<LineStartIndentLevel> {
    for ancestor in node.ancestors() {
      if let Node::TsInterfaceDecl(ancestor) = ancestor {
        return context.get_lsil_for_node(ancestor).map(|x| x.to_owned());
      }
    }
    None
  }
}

fn use_consistent_quotes_for_members<'a>(mut members: impl Iterator<Item = Node<'a>>) -> bool {
  fn check_expr(expr: Expr) -> bool {
    match expr {
      Expr::Ident(ident) => !is_text_valid_identifier(ident.sym()),
      Expr::Lit(Lit::Str(str)) => !is_text_valid_identifier(str.value()),
      _ => false,
    }
  }

  fn check_prop_name(name: PropName) -> bool {
    match name {
      PropName::Ident(ident) => !is_text_valid_identifier(ident.sym()),
      PropName::Str(str) => !is_text_valid_identifier(str.value()),
      _ => false,
    }
  }

  members.any(|m| match m {
    // class members
    // Do not match class properties (Node::ClassProp)
    // With `--strictPropertyInitialization`, TS treats properties
    // with quoted names differently than unquoted ones.
    // See https://github.com/microsoft/TypeScript/pull/20075
    Node::ClassMethod(method) => check_prop_name(method.key),
    // interface members
    Node::TsPropertySignature(signature) => check_expr(signature.key),
    Node::TsGetterSignature(signature) => check_expr(signature.key),
    Node::TsSetterSignature(signature) => check_expr(signature.key),
    // object members
    Node::KeyValueProp(prop) => check_prop_name(prop.key),
    Node::GetterProp(prop) => check_prop_name(prop.key),
    Node::SetterProp(prop) => check_prop_name(prop.key),
    Node::MethodProp(prop) => check_prop_name(prop.key),
    _ => false,
  })
}

fn gen_type_lit<'a>(node: &TsTypeLit<'a>, context: &mut Context<'a>) -> PrintItems {
  return context.with_maybe_consistent_props(
    node,
    |node| use_consistent_quotes_for_members(node.members.iter().map(|n| n.into())),
    |context, node| {
      gen_object_like_node(
        GenObjectLikeNodeOptions {
          node: node.into(),
          members: node.members.iter().map(|m| m.into()).collect(),
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

fn gen_jsx_attribute<'a>(node: &JSXAttr<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.extend(gen_node(node.name.into(), context));
  if let Some(value) = &node.value {
    items.push_sc(sc!("="));
    let surround_with_braces = context.token_finder.get_previous_token_if_open_brace(value).is_some();
    let inner_items = gen_node(value.into(), context);
    items.extend(if surround_with_braces {
      gen_as_jsx_expr_container(node.into(), inner_items, context)
    } else {
      inner_items
    });
  }
  items
}

fn gen_jsx_closing_element<'a>(node: &JSXClosingElement<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.push_sc(sc!("</"));
  items.extend(gen_node(node.name.into(), context));
  items.push_sc(sc!(">"));
  items
}

fn gen_jsx_closing_fragment<'a>(_: &'a JSXClosingFragment, _: &mut Context<'a>) -> PrintItems {
  "</>".into()
}

fn handle_jsx_surrounding_parens(inner_items: PrintItems, context: &mut Context<'_>) -> PrintItems {
  if !is_jsx_paren_expr_handled_node(context.current_node, context) {
    if should_jsx_surround_newlines(context.current_node, context) {
      return surround_with_newlines_indented_if_multi_line(inner_items, context.config.indent_width);
    } else {
      return inner_items;
    }
  }

  if context.parent().is::<JSXExprContainer>() && context.config.jsx_multi_line_parens != JsxMultiLineParens::Always {
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
    let mut parent = node.parent().unwrap();
    while let Some(paren_expr) = parent.to::<ParenExpr>() {
      if node_helpers::has_surrounding_comments(paren_expr.expr.into(), context.program) {
        return false;
      }
      parent = parent.parent().unwrap();
    }

    parent.is::<JSXExprContainer>()
  }
}

fn is_jsx_paren_expr_handled_node<'a>(node: Node<'a>, context: &Context<'a>) -> bool {
  if context.config.jsx_multi_line_parens == JsxMultiLineParens::Never {
    return false;
  }

  if !matches!(node.kind(), NodeKind::JSXElement | NodeKind::JSXFragment) {
    return false;
  }

  let mut parent = node.parent().unwrap();
  // Only wrap the top-level JSX element in parens
  if matches!(parent.kind(), NodeKind::JSXElement | NodeKind::JSXFragment) {
    return false;
  }

  if node_helpers::has_surrounding_comments(node, context.program) {
    return false;
  }

  while parent.is::<ParenExpr>() {
    if node_helpers::has_surrounding_comments(parent, context.program) {
      return false;
    }
    parent = parent.parent().unwrap();
  }

  if context.config.jsx_multi_line_parens == JsxMultiLineParens::Always {
    return true;
  }

  // do not allow in expr statement, argument, attributes, jsx exprs, or member exprs
  !matches!(
    parent.kind(),
    NodeKind::ExprStmt | NodeKind::ExprOrSpread | NodeKind::JSXExprContainer | NodeKind::MemberExpr
  )
}

fn gen_jsx_element<'a>(node: &JSXElement<'a>, context: &mut Context<'a>) -> PrintItems {
  let items = if let Some(closing) = node.closing {
    // pre element bodies should be formatted as-is
    if node.opening.name.text_fast(context.program) == "pre" {
      let mut items = gen_node(node.opening.into(), context);
      let in_between_range = SourceRange::new(node.opening.end(), closing.start());
      items.extend(ir_helpers::gen_from_raw_string_trim_line_ends(in_between_range.text_fast(context.program)));
      items.extend(gen_node(closing.into(), context));
      items
    } else {
      let result = gen_jsx_with_opening_and_closing(
        GenJsxWithOpeningAndClosingOptions {
          opening_element: node.opening.into(),
          closing_element: closing.into(),
          children: node.children.iter().map(|x| x.into()).collect(),
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
    items.extend(gen_node(node.opening.into(), context));
    items.push_info(end_ln);
    items
  };

  handle_jsx_surrounding_parens(items, context)
}

fn gen_jsx_empty_expr<'a>(node: &JSXEmptyExpr<'a>, context: &mut Context<'a>) -> PrintItems {
  gen_comment_collection(get_jsx_empty_expr_comments(node, context), None, None, context)
}

fn gen_jsx_expr_container<'a>(node: &JSXExprContainer<'a>, context: &mut Context<'a>) -> PrintItems {
  // Don't send JSX empty expressions to gen_node because it will not handle comments
  // the way they should be specifically handled for empty expressions.
  let gen_inner = match &node.expr {
    JSXExpr::JSXEmptyExpr(expr) => gen_jsx_empty_expr(expr, context),
    JSXExpr::Expr(expr) => gen_node(expr.into(), context),
  };

  gen_as_jsx_expr_container(node.expr.into(), gen_inner, context)
}

fn gen_as_jsx_expr_container(expr: Node, inner_items: PrintItems, context: &mut Context) -> PrintItems {
  let surround_with_space = context.config.jsx_expression_container_space_surrounding_expression;
  let surround_with_new_lines = should_surround_with_newlines(expr, context.program);
  let mut items = PrintItems::new();
  items.push_sc(sc!("{"));
  if surround_with_new_lines {
    items.push_signal(Signal::NewLine);
    items.push_signal(Signal::StartIndent);
  } else if surround_with_space {
    items.push_space();
  }
  items.extend(inner_items);
  if surround_with_new_lines {
    items.extend(gen_trailing_comments_as_statements(&expr.range(), context));
    items.push_signal(Signal::NewLine);
    items.push_signal(Signal::FinishIndent);
  } else if surround_with_space {
    items.push_space();
  }
  items.push_sc(sc!("}"));

  return items;

  fn should_surround_with_newlines(expr: Node, program: Program) -> bool {
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

fn gen_jsx_fragment<'a>(node: &JSXFragment<'a>, context: &mut Context<'a>) -> PrintItems {
  let result = gen_jsx_with_opening_and_closing(
    GenJsxWithOpeningAndClosingOptions {
      opening_element: node.opening.into(),
      closing_element: node.closing.into(),
      children: node.children.iter().map(|x| x.into()).collect(),
    },
    context,
  );

  context.store_info_range_for_node(node, (result.start_ln, result.end_ln));

  handle_jsx_surrounding_parens(result.items, context)
}

fn gen_jsx_member_expr<'a>(node: &JSXMemberExpr<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.extend(gen_node(node.obj.into(), context));
  items.push_sc(sc!("."));
  items.extend(gen_node(node.prop.into(), context));
  items
}

fn gen_jsx_namespaced_name<'a>(node: &JSXNamespacedName<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.extend(gen_node(node.ns.into(), context));
  items.push_sc(sc!(":"));
  items.extend(gen_node(node.name.into(), context));
  items
}

fn gen_jsx_opening_element<'a>(node: &JSXOpeningElement<'a>, context: &mut Context<'a>) -> PrintItems {
  let space_before_self_closing_tag_slash = context.config.jsx_self_closing_element_space_before_slash;
  let force_use_new_lines = get_force_is_multi_line(node, context);
  let prefer_newline_before_close_bracket = get_should_prefer_newline_before_close_bracket(node, context);
  let start_lsil = LineStartIndentLevel::new("openingElementStart");
  let mut items = PrintItems::new();

  items.push_info(start_lsil);
  items.push_sc(sc!("<"));
  items.extend(gen_node(node.name.into(), context));
  if let Some(type_args) = node.type_args {
    items.extend(gen_node(type_args.into(), context));
  }

  if node.attrs.len() == 1 && node.type_args.is_none() && is_jsx_attr_with_string(&node.attrs[0]) {
    items.push_space();
    items.extend(gen_node(node.attrs[0].into(), context));
  } else if !node.attrs.is_empty() {
    let mut multi_line_options = ir_helpers::MultiLineOptions::surround_newlines_indented();
    multi_line_options.newline_at_end = prefer_newline_before_close_bracket;
    items.extend(gen_separated_values(
      GenSeparatedValuesParams {
        nodes: node.attrs.iter().map(|p| NodeOrSeparator::Node(p.into())).collect(),
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
  let name_or_type_arg_end = node.type_args.map(|t| t.end()).unwrap_or_else(|| node.name.end());
  let last_node_end = node.attrs.last().map(|n| n.end()).unwrap_or(name_or_type_arg_end);

  let generated_comments = gen_comments_as_statements(last_node_end.trailing_comments_fast(context.program), None, context);
  if !generated_comments.is_empty() {
    if node.attrs.is_empty() {
      items.push_signal(Signal::NewLine);
    }
    items.extend(with_indent(generated_comments));
    items.push_signal(Signal::NewLine);
  }

  if node.self_closing() {
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

  fn get_force_is_multi_line(node: &JSXOpeningElement, context: &mut Context) -> bool {
    if context.config.jsx_attributes_prefer_single_line {
      false
    } else if let Some(first_attrib) = node.attrs.first() {
      node_helpers::get_use_new_lines_for_nodes(&node.name, first_attrib, context.program)
    } else {
      false
    }
  }

  fn get_should_prefer_newline_before_close_bracket(node: &JSXOpeningElement, context: &mut Context) -> bool {
    let bracket_pos_config = match node.self_closing() {
      true => context.config.jsx_self_closing_element_bracket_position,
      false => context.config.jsx_opening_element_bracket_position,
    };
    match bracket_pos_config {
      SameOrNextLinePosition::Maintain => {
        if let Some(last_attr) = node.attrs.last() {
          last_attr.end_line_fast(context.program) < node.end_line_fast(context.program)
        } else {
          false
        }
      }
      SameOrNextLinePosition::NextLine => true,
      SameOrNextLinePosition::SameLine => false,
    }
  }

  fn is_jsx_attr_with_string(node: &JSXAttrOrSpread) -> bool {
    if let JSXAttrOrSpread::JSXAttr(attrib) = node {
      if let Some(value) = attrib.value {
        return value.kind() == NodeKind::Str;
      }
    }
    false
  }
}

fn gen_jsx_opening_fragment<'a>(_: &'a JSXOpeningFragment, _: &mut Context<'a>) -> PrintItems {
  "<>".into()
}

fn gen_jsx_spread_child<'a>(node: &JSXSpreadChild<'a>, context: &mut Context<'a>) -> PrintItems {
  gen_as_jsx_expr_container(
    node.into(),
    {
      let mut items = PrintItems::new();
      items.push_sc(sc!("..."));
      items.extend(gen_node(node.expr.into(), context));
      items
    },
    context,
  )
}

fn gen_jsx_text<'a>(node: &JSXText<'a>, context: &mut Context<'a>) -> PrintItems {
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

fn gen_big_int_literal<'a>(node: &BigInt<'a>, context: &mut Context<'a>) -> PrintItems {
  node.text_fast(context.program).to_string().into()
}

fn gen_bool_literal(node: &Bool) -> PrintItems {
  match node.value() {
    true => "true",
    false => "false",
  }
  .into()
}

fn gen_num_literal<'a>(node: &Number<'a>, context: &mut Context<'a>) -> PrintItems {
  node.text_fast(context.program).to_string().into()
}

fn gen_reg_exp_literal(node: &Regex, _: &mut Context) -> PrintItems {
  // the exp and flags should not be nodes so just ignore that (swc issue #511)
  let mut items = PrintItems::new();
  items.push_sc(sc!("/"));
  items.push_string(node.exp().to_string());
  items.push_sc(sc!("/"));
  items.push_string(node.flags().to_string());
  items
}

fn gen_string_literal<'a>(node: &Str<'a>, context: &mut Context<'a>) -> PrintItems {
  let string_value = string_literal::get_value(node, context);
  if node.parent().is::<JSXAttr>() {
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

  pub fn get_value(node: &Str, context: &mut Context) -> String {
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

fn gen_module<'a>(node: &Module<'a>, context: &mut Context<'a>) -> PrintItems {
  gen_program(
    ProgramInfo {
      range: node.range(),
      shebang: node.shebang(),
      statements: node.body.iter().map(|x| x.into()).collect(),
    },
    context,
  )
}

fn gen_script<'a>(node: &Script<'a>, context: &mut Context<'a>) -> PrintItems {
  gen_program(
    ProgramInfo {
      range: node.range(),
      shebang: node.shebang(),
      statements: node.body.iter().map(|x| x.into()).collect(),
    },
    context,
  )
}

struct ProgramInfo<'a, 'b> {
  range: SourceRange,
  shebang: &'b Option<deno_ast::swc::atoms::Atom>,
  statements: Vec<Node<'a>>,
}

fn gen_program<'a, 'b>(node: ProgramInfo<'a, 'b>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  if let Some(shebang) = node.shebang {
    items.push_sc(sc!("#!"));
    items.push_string(shebang.to_string());
    items.push_signal(Signal::ExpectNewLine);
    if let Some(first_statement) = node.statements.first() {
      if node_helpers::has_separating_blank_line(&node.range.start, first_statement, context.program) {
        items.push_signal(Signal::NewLine);
        items.push_signal(Signal::NewLine);
      }
    } else {
      let shebang_end = node.range.start + "#!".len() + shebang.len();
      items.extend(gen_trailing_comments_as_statements(&shebang_end.range(), context));
    }
  }

  items.extend(gen_statements(node.range, node.statements, context));

  items
}

/* patterns */

fn gen_array_pat<'a>(node: &ArrayPat<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.extend(gen_array_like_nodes(
    GenArrayLikeNodesOptions {
      node: node.into(),
      nodes: node.elems.iter().map(|x| x.as_ref().map(|elem| elem.into())).collect(),
      prefer_hanging: context.config.array_pattern_prefer_hanging,
      prefer_single_line: context.config.array_pattern_prefer_single_line,
      trailing_commas: context.config.array_pattern_trailing_commas,
      space_around: context.config.array_pattern_space_around,
    },
    context,
  ));
  if node.optional() {
    items.push_sc(sc!("?"));
  }
  items.extend(gen_type_ann_with_colon_if_exists(node.type_ann, context));
  items
}

fn gen_assign_pat<'a>(node: &AssignPat<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.extend(gen_node(node.left.into(), context));
  items.extend(gen_assignment(node.right.into(), sc!("="), context));
  items
}

fn gen_assign_pat_prop<'a>(node: &AssignPatProp<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.extend(gen_node(node.key.into(), context));
  if let Some(value) = &node.value {
    items.extend(gen_assignment(value.into(), sc!("="), context));
  }
  items
}

fn gen_key_value_pat_prop<'a>(node: &KeyValuePatProp<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.extend(gen_node(node.key.into(), context));
  items.extend(gen_assignment(node.value.into(), sc!(":"), context));
  items
}

fn gen_rest_pat<'a>(node: &RestPat<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.push_sc(sc!("..."));
  items.extend(gen_node(node.arg.into(), context));
  items.extend(gen_type_ann_with_colon_if_exists(node.type_ann, context));
  items
}

fn gen_object_pat<'a>(node: &ObjectPat<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.extend(gen_object_like_node(
    GenObjectLikeNodeOptions {
      node: node.into(),
      members: node.props.iter().map(|x| x.into()).collect(),
      separator: get_trailing_commas(node, context).into(),
      prefer_hanging: context.config.object_pattern_prefer_hanging,
      prefer_single_line: context.config.object_pattern_prefer_single_line,
      force_single_line: false,
      force_multi_line: is_node_definitely_above_line_width(node.range(), context),
      surround_single_line_with_spaces: context.config.object_pattern_space_surrounding_properties,
      allow_blank_lines: true,
      node_sorter: None,
    },
    context,
  ));
  if node.optional() {
    items.push_sc(sc!("?"));
  }
  items.extend(gen_type_ann_with_colon_if_exists(node.type_ann, context));
  return items;

  fn get_trailing_commas(node: &ObjectPat, context: &Context) -> TrailingCommas {
    if let Some(last) = node.props.last() {
      if last.kind() == NodeKind::RestPat {
        return TrailingCommas::Never;
      }
    }
    context.config.object_pattern_trailing_commas
  }
}

/* properties */

fn gen_method_prop<'a>(node: &MethodProp<'a>, context: &mut Context<'a>) -> PrintItems {
  gen_class_or_object_method(
    ClassOrObjectMethod {
      node: node.into(),
      parameters_range: node.get_parameters_range(context),
      decorators: None,
      accessibility: None,
      is_static: false,
      is_async: node.function.is_async(),
      is_abstract: false,
      is_override: false,
      kind: ClassOrObjectMethodKind::Method,
      is_generator: node.function.is_generator(),
      is_optional: false,
      key: node.key.into(),
      type_params: node.function.type_params.map(|x| x.into()),
      params: node.function.params.iter().map(|&x| x.into()).collect(),
      return_type: node.function.return_type.map(|x| x.into()),
      body: node.function.body.map(|x| x.into()),
    },
    context,
  )
}

struct ClassOrObjectMethod<'a> {
  node: Node<'a>,
  parameters_range: Option<SourceRange>,
  decorators: Option<&'a [&'a Decorator<'a>]>,
  accessibility: Option<Accessibility>,
  is_static: bool,
  is_async: bool,
  is_abstract: bool,
  kind: ClassOrObjectMethodKind,
  is_generator: bool,
  is_optional: bool,
  is_override: bool,
  key: Node<'a>,
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

impl From<MethodKind> for ClassOrObjectMethodKind {
  fn from(kind: MethodKind) -> ClassOrObjectMethodKind {
    match kind {
      MethodKind::Getter => ClassOrObjectMethodKind::Getter,
      MethodKind::Setter => ClassOrObjectMethodKind::Setter,
      MethodKind::Method => ClassOrObjectMethodKind::Method,
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
  items.extend(gen_quotable_prop(node.key, context));
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

fn accessibility_to_str(accessibility: Accessibility) -> &'static str {
  match accessibility {
    Accessibility::Private => "private",
    Accessibility::Protected => "protected",
    Accessibility::Public => "public",
  }
}

/* statements */

fn gen_block_stmt<'a>(node: &BlockStmt<'a>, context: &mut Context<'a>) -> PrintItems {
  gen_block(
    |stmts, context| gen_statements(node.get_inner_range(context), stmts, context),
    GenBlockOptions {
      range: Some(node.range()),
      children: node.stmts.iter().map(|x| x.into()).collect(),
    },
    context,
  )
}

fn gen_break_stmt<'a>(node: &BreakStmt<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();

  items.push_sc(sc!("break"));
  if let Some(label) = node.label {
    items.push_space();
    items.extend(gen_node(label.into(), context));
  }
  if context.config.semi_colons.is_true() {
    items.push_sc(sc!(";"));
  }

  items
}

fn gen_continue_stmt<'a>(node: &ContinueStmt<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();

  items.push_sc(sc!("continue"));
  if let Some(label) = node.label {
    items.push_space();
    items.extend(gen_node(label.into(), context));
  }
  if context.config.semi_colons.is_true() {
    items.push_sc(sc!(";"));
  }

  items
}

fn gen_debugger_stmt<'a>(_: &'a DebuggerStmt, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();

  items.push_sc(sc!("debugger"));
  if context.config.semi_colons.is_true() {
    items.push_sc(sc!(";"));
  }

  items
}

fn gen_do_while_stmt<'a>(node: &DoWhileStmt<'a>, context: &mut Context<'a>) -> PrintItems {
  // the braces are technically optional on do while statements
  let mut items = PrintItems::new();
  items.push_sc(sc!("do"));
  items.extend(gen_brace_separator(
    GenBraceSeparatorOptions {
      brace_position: context.config.do_while_statement_brace_position,
      open_brace_token: if let Stmt::Block(_) = node.body {
        context.token_finder.get_first_open_brace_token_within(node)
      } else {
        None
      },
      start_header_lsil: None,
    },
    context,
  ));
  items.extend(gen_node(node.body.into(), context));
  if context.config.semi_colons.is_true() || matches!(node.body, Stmt::Block(_)) {
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
    |context| gen_node(node.test.into(), context),
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

fn gen_export_all<'a>(node: &ExportAll<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  if node.type_only() {
    items.push_sc(sc!("export type * from "));
  } else {
    items.push_sc(sc!("export * from "));
  }
  items.extend(gen_node(node.src.into(), context));

  if let Some(with_clause) = node.with {
    items.extend(gen_with_clause(with_clause, context));
  }

  if context.config.semi_colons.is_true() {
    items.push_sc(sc!(";"));
  }

  items
}

fn gen_with_clause<'a>(node: &ObjectLit<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  let previous_token_text = node.previous_token_fast(context.program).text_fast(context.program);
  items.push_sc(if previous_token_text == "assert" { sc!(" assert ") } else { sc!(" with ") });
  items.extend(gen_node(node.into(), context));
  items
}

fn gen_empty_stmt(_: &EmptyStmt, _: &mut Context) -> PrintItems {
  ";".into()
}

fn gen_export_assignment<'a>(node: &TsExportAssignment<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();

  items.push_sc(sc!("export"));
  items.extend(gen_assignment(node.expr.into(), sc!("="), context));
  if context.config.semi_colons.is_true() {
    items.push_sc(sc!(";"));
  }

  items
}

fn gen_namespace_export<'a>(node: &TsNamespaceExportDecl<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.push_sc(sc!("export as namespace "));
  items.extend(gen_node(node.id.into(), context));

  if context.config.semi_colons.is_true() {
    items.push_sc(sc!(";"));
  }

  items
}

fn gen_expr_stmt<'a>(stmt: &ExprStmt<'a>, context: &mut Context<'a>) -> PrintItems {
  if context.config.semi_colons.is_true() || stmt.parent().is::<DoWhileStmt>() {
    return gen_inner(stmt, context);
  } else {
    return gen_for_prefix_semi_colon_insertion(stmt, context);
  }

  fn gen_inner<'a>(stmt: &ExprStmt<'a>, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    items.extend(gen_node(stmt.expr.into(), context));
    if context.config.semi_colons.is_true() {
      items.push_sc(sc!(";"));
    }
    items
  }

  fn gen_for_prefix_semi_colon_insertion<'a>(stmt: &ExprStmt<'a>, context: &mut Context<'a>) -> PrintItems {
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

fn gen_for_stmt<'a>(node: &ForStmt<'a>, context: &mut Context<'a>) -> PrintItems {
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
        .find(|t| t.token == Token::Semi)
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
      items.extend(gen_node(init.into(), context));
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
      items.extend(gen_node(test.into(), context));
      items.push_sc(sc!(";"));
      items
    })
  });
  let generated_update = node.update.as_ref().map(|update| ir_helpers::new_line_group(gen_node(update.into(), context)));

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
        body_node: node.body.into(),
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

fn gen_for_in_stmt<'a>(node: &ForInStmt<'a>, context: &mut Context<'a>) -> PrintItems {
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
      items.extend(gen_node(node.left.into(), context));
      items.push_signal(Signal::SpaceOrNewLine);
      items.push_condition(conditions::indent_if_start_of_line({
        let mut items = PrintItems::new();
        items.push_sc(sc!("in "));
        items.extend(gen_node(node.right.into(), context));
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
        body_node: node.body.into(),
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

fn gen_for_of_stmt<'a>(node: &ForOfStmt<'a>, context: &mut Context<'a>) -> PrintItems {
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
  if node.is_await() {
    // todo: generate comments around await token range
    items.push_sc(sc!("await "));
  }
  let inner_header_range = SourceRange::new(node.left.start(), node.right.end());
  items.extend(gen_node_in_parens(
    |context| {
      let mut items = PrintItems::new();
      items.extend(gen_node(node.left.into(), context));
      items.push_signal(Signal::SpaceOrNewLine);
      items.push_condition(conditions::indent_if_start_of_line({
        let mut items = PrintItems::new();
        items.push_sc(sc!("of "));
        items.extend(gen_node(node.right.into(), context));
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
        body_node: node.body.into(),
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

fn gen_if_stmt<'a>(node: &IfStmt<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  let cons = node.cons;
  let cons_range = cons.range();
  let result = gen_header_with_conditional_brace_body(
    GenHeaderWithConditionalBraceBodyOptions {
      body_node: cons,
      generated_header: {
        let mut items = PrintItems::new();
        items.push_sc(sc!("if"));
        if context.config.if_statement_space_after_if_keyword {
          items.push_space();
        }
        let test = node.test;
        items.extend(gen_node_in_parens(
          |context| gen_node(test.into(), context),
          GenNodeInParensOptions {
            inner_range: test.range(),
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

  if let Some(alt) = node.alt {
    if let Stmt::If(alt_alt) = alt {
      if alt_alt.alt.is_none() {
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

    // generate the leading comments before the else keyword
    let else_keyword = node
      .children_with_tokens_fast(context.program)
      .iter()
      .find(|n| match n {
        NodeOrToken::Token(token) => token.text_fast(context.program) == "else",
        _ => false,
      })
      .expect("Expected to find an else keyword.")
      .unwrap_token();
    items.extend(gen_leading_comments(&else_keyword.range(), context));
    items.extend(gen_leading_comments(&alt.range(), context));

    let start_else_header_ln = LineNumber::new("startElseHeader");
    let start_else_header_lsil = LineStartIndentLevel::new("startElseHeader");
    items.push_info(start_else_header_ln);
    items.push_info(start_else_header_lsil);
    items.push_sc(sc!("else"));

    if let Stmt::If(alt) = alt {
      items.push_space();
      items.extend(gen_node(alt.into(), context));
    } else {
      items.extend(
        gen_conditional_brace_body(
          GenConditionalBraceBodyOptions {
            body_node: alt.into(),
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

fn gen_labeled_stmt<'a>(node: &LabeledStmt<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.extend(gen_node(node.label.into(), context));
  items.push_sc(sc!(":"));

  // not bothering to make this configurable, because who uses labeled statements?
  // edit: ok, maybe I will make this configurable in the future
  let is_inner_stmt_same_line = node.start_line_fast(context.program) == node.body.start_line_fast(context.program);
  if is_inner_stmt_same_line {
    items.push_space();
  } else {
    items.push_signal(Signal::NewLine);
  }

  items.extend(gen_node(node.body.into(), context));

  items
}

fn gen_return_stmt<'a>(node: &ReturnStmt<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.push_sc(sc!("return"));
  if let Some(arg) = &node.arg {
    items.push_space();
    items.extend(gen_node(arg.into(), context));
  }
  if context.config.semi_colons.is_true() {
    items.push_sc(sc!(";"));
  }
  items
}

fn gen_switch_stmt<'a>(node: &SwitchStmt<'a>, context: &mut Context<'a>) -> PrintItems {
  let start_header_lsil = LineStartIndentLevel::new("startHeader");
  let mut items = PrintItems::new();
  items.push_info(start_header_lsil);
  items.push_sc(sc!("switch "));
  items.extend(gen_node_in_parens(
    |context| gen_node(node.discriminant.into(), context),
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
      node: node.into(),
      members: node.cases.iter().map(|&x| x.into()).collect(),
      start_header_lsil: Some(start_header_lsil),
      brace_position: context.config.switch_statement_brace_position,
      should_use_blank_line: |previous, next, context| {
        // do not put a blank line when the previous case has no body
        if let Node::SwitchCase(previous) = previous {
          if previous.cons.is_empty() {
            return false;
          }
        }

        // Switch cases have custom rules for where the comments end up (based on their indentation),
        // so for here just check that there is no blank line between either the previous comment
        // or previous node
        let leading_comment = next.leading_comments_fast(context.program).last().map(|r| r.range());
        let previous_end_line = if let Some(leading_comment) = leading_comment {
          leading_comment.end_line_fast(context.program)
        } else {
          previous.end_line_fast(context.program)
        };
        previous_end_line + 1 < next.start_line_fast(context.program)
      },
      separator: Separator::none(),
    },
    context,
  ));
  items
}

fn gen_switch_case<'a>(node: &SwitchCase<'a>, context: &mut Context<'a>) -> PrintItems {
  let block_stmt_body = get_block_stmt_body(node);
  let mut items = PrintItems::new();
  let colon_token = context
    .token_finder
    .get_first_colon_token_after(&if let Some(test) = node.test { test.end() } else { node.start() })
    .expect("Expected to find a colon token.");

  if let Some(test) = &node.test {
    items.push_sc(sc!("case "));
    items.extend(gen_node(test.into(), context));
    items.push_sc(sc!(":"));
  } else {
    items.push_sc(sc!("default:"));
  }

  items.extend(gen_trailing_comments_same_line(&colon_token.range(), context));
  let generated_trailing_comments = gen_trailing_comments_for_case(node, &block_stmt_body, context);
  if !node.cons.is_empty() {
    if let Some(block_stmt_body) = block_stmt_body {
      items.extend(gen_brace_separator(
        GenBraceSeparatorOptions {
          brace_position: context.config.switch_case_brace_position,
          open_brace_token: context.token_finder.get_first_open_brace_token_within(&block_stmt_body),
          start_header_lsil: None,
        },
        context,
      ));
      items.extend(gen_node(node.cons.first().unwrap().into(), context));
    } else {
      items.push_signal(Signal::NewLine);
      items.extend(ir_helpers::with_indent(gen_statements(
        SourceRange::new(colon_token.end(), node.end()),
        node.cons.iter().map(|node| node.into()).collect(),
        context,
      )));
    }
  }

  items.extend(generated_trailing_comments);

  return items;

  fn get_block_stmt_body(node: &SwitchCase) -> Option<SourceRange> {
    if node.cons.len() == 1 {
      if let Some(Stmt::Block(block_stmt)) = node.cons.first() {
        return Some(block_stmt.range());
      }
    }
    None
  }

  fn gen_trailing_comments_for_case<'a>(node: &SwitchCase<'a>, block_stmt_body: &Option<SourceRange>, context: &mut Context<'a>) -> PrintItems {
    let node_range = node.range();
    let mut items = PrintItems::new();
    // generate the trailing comments as statements
    let trailing_comments = get_trailing_comments_as_statements(&node_range, context);
    if !trailing_comments.is_empty() {
      let last_case = node.parent().cases.last();
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

fn gen_throw_stmt<'a>(node: &ThrowStmt<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.push_sc(sc!("throw "));
  items.extend(gen_node(node.arg.into(), context));
  if context.config.semi_colons.is_true() {
    items.push_sc(sc!(";"));
  }
  items
}

fn gen_try_stmt<'a>(node: &TryStmt<'a>, context: &mut Context<'a>) -> PrintItems {
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
        body_node: node.block.into(),
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

  if let Some(handler) = node.handler {
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
    items.extend(gen_node(handler.into(), context));

    // set the next block to check the handler start info
    last_block_start_ln = handler_start_ln;
  }

  if let Some(finalizer) = node.finalizer {
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
          body_node: finalizer.into(),
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

fn gen_var_decl<'a>(node: &VarDecl<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  if node.declare() {
    items.push_sc(sc!("declare "));
  }
  items.push_sc(match node.decl_kind() {
    VarDeclKind::Const => sc!("const "),
    VarDeclKind::Let => sc!("let "),
    VarDeclKind::Var => sc!("var "),
  });

  items.extend(gen_var_declarators(node.into(), node.decls, context));

  if requires_semi_colon_for_var_or_using_decl(node.into(), context) {
    items.push_sc(sc!(";"));
  }

  items
}

fn requires_semi_colon_for_var_or_using_decl(node: Node, context: &mut Context) -> bool {
  let use_semi_colons = context.config.semi_colons.is_true();
  use_semi_colons
    && match node.parent() {
      Some(Node::ForInStmt(node)) => node.start() >= node.body.start(),
      Some(Node::ForOfStmt(node)) => node.start() >= node.body.start(),
      Some(Node::ForStmt(node)) => node.start() >= node.body.start(),
      _ => use_semi_colons,
    }
}

fn gen_var_declarators<'a>(parent: Node<'a>, decls: &[&'a VarDeclarator<'a>], context: &mut Context<'a>) -> PrintItems {
  fn get_use_new_lines<'a>(parent: Node<'a>, decls: &[&'a VarDeclarator<'a>], context: &mut Context<'a>) -> bool {
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
    gen_node(decls[0].into(), context)
  } else if decls_len > 1 {
    let force_use_new_lines = get_use_new_lines(parent, decls, context);
    gen_separated_values(
      GenSeparatedValuesParams {
        nodes: decls.iter().map(|&p| NodeOrSeparator::Node(p.into())).collect(),
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

fn gen_var_declarator<'a>(node: &VarDeclarator<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();

  items.extend(gen_node(node.name.into(), context));

  if let Some(init) = &node.init {
    items.extend(gen_assignment(init.into(), sc!("="), context));
  }

  // Indent the first variable declarator when there are multiple.
  // Not ideal, but doing this here because of the abstraction used in
  // `gen_var_decl`. In the future this should probably be moved away.

  let parent_decls = match node.parent() {
    Node::VarDecl(parent) => Some(&parent.decls),
    Node::UsingDecl(parent) => Some(&parent.decls),
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

fn gen_while_stmt<'a>(node: &WhileStmt<'a>, context: &mut Context<'a>) -> PrintItems {
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
    |context| gen_node(node.test.into(), context),
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
        body_node: node.body.into(),
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

fn gen_array_type<'a>(node: &TsArrayType<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.extend(gen_node(node.elem_type.into(), context));
  items.push_sc(sc!("[]"));
  items
}

fn gen_conditional_type<'a>(node: &TsConditionalType<'a>, context: &mut Context<'a>) -> PrintItems {
  let use_new_lines =
    !context.config.conditional_type_prefer_single_line && node_helpers::get_use_new_lines_for_nodes(&node.true_type, &node.false_type, context.program);
  let top_most_data = get_top_most_data(node, context);
  let is_parent_conditional_type = node.parent().kind() == NodeKind::TsConditionalType;
  let mut items = PrintItems::new();
  let before_false_ln = LineNumber::new("beforeFalse");
  let question_token = context.token_finder.get_first_operator_after(&node.extends_type, "?").unwrap();
  let colon_token = context.token_finder.get_first_operator_after(&node.true_type, ":").unwrap();
  let (question_position, colon_position) = get_operator_position(node, question_token, colon_token, context);
  let question_comment_items = gen_cond_token_comments(question_token, context, top_most_data.il);
  let colon_comment_items = gen_cond_token_comments(colon_token, context, top_most_data.il);

  // main area
  items.extend(ir_helpers::new_line_group(gen_node(node.check_type.into(), context)));
  items.push_sc(sc!(" extends")); // do not newline before because it's a parsing error
  items.push_signal(Signal::SpaceOrNewLine);

  if top_most_data.is_top_most {
    items.push_info(top_most_data.ln);
    items.push_info(top_most_data.il);
  }

  items.push_condition(conditions::indent_if_start_of_line(ir_helpers::new_line_group({
    let mut items = gen_node(node.extends_type.into(), context);
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
      items.extend(ir_helpers::new_line_group(gen_node(node.true_type.into(), context)));
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
    items.extend(ir_helpers::new_line_group(gen_node(node.false_type.into(), context)));
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

  fn get_top_most_data<'a>(node: &TsConditionalType<'a>, context: &mut Context<'a>) -> TopMostData {
    // todo: consolidate with conditional expression
    // The "top most" node in nested conditionals follows the ancestors up through
    // the false expressions.
    let mut top_most_node = node;

    for ancestor in context.parent_stack.iter() {
      if let Node::TsConditionalType(parent) = ancestor {
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

  fn get_operator_position(
    node: &TsConditionalType,
    question_token: &TokenAndSpan,
    colon_token: &TokenAndSpan,
    context: &mut Context,
  ) -> (OperatorPosition, OperatorPosition) {
    fn get_maintain_position(node: &TsConditionalType, ts_type: TsType, token: &TokenAndSpan, context: &mut Context) -> OperatorPosition {
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
        get_maintain_position(node, node.extends_type, question_token, context),
        get_maintain_position(node, node.true_type, colon_token, context),
      ),
    }
  }
}

fn gen_constructor_type<'a>(node: &TsConstructorType<'a>, context: &mut Context<'a>) -> PrintItems {
  let start_lsil = LineStartIndentLevel::new("startConstructorType");
  let mut items = PrintItems::new();
  items.push_info(start_lsil);
  if node.is_abstract() {
    items.push_sc(sc!("abstract "));
  }
  items.push_sc(sc!("new"));
  if context.config.constructor_type_space_after_new_keyword {
    items.push_space();
  }
  if let Some(type_params) = node.type_params {
    items.extend(gen_node(type_params.into(), context));
  }

  items.extend(gen_parameters_or_arguments(
    GenParametersOrArgumentsOptions {
      node: node.into(),
      range: node.get_parameters_range(context),
      nodes: node.params.iter().map(|node| node.into()).collect(),
      custom_close_paren: |context| {
        Some(gen_close_paren_with_type(
          GenCloseParenWithTypeOptions {
            start_lsil,
            type_node: Some(node.type_ann.into()),
            type_node_separator: Some({
              let mut items = PrintItems::new();
              items.push_sc(sc!(" =>"));
              items.push_signal(Signal::SpaceIfNotTrailing);
              items.push_signal(Signal::PossibleNewLine);
              items
            }),
            param_count: node.params.len(),
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

fn gen_function_type<'a>(node: &TsFnType<'a>, context: &mut Context<'a>) -> PrintItems {
  let start_lsil = LineStartIndentLevel::new("startFunctionType");
  let mut items = PrintItems::new();
  let mut indent_after_arrow_condition = if_true(
    "indentIfIsStartOfLineAfterArrow",
    condition_resolvers::is_start_of_line(),
    Signal::StartIndent.into(),
  );
  let indent_after_arrow_condition_ref = indent_after_arrow_condition.create_reference();

  items.push_info(start_lsil);
  if let Some(type_params) = node.type_params {
    items.extend(gen_node(type_params.into(), context));
  }
  items.extend(gen_parameters_or_arguments(
    GenParametersOrArgumentsOptions {
      node: node.into(),
      range: node.get_parameters_range(context),
      nodes: node.params.iter().map(|node| node.into()).collect(),
      custom_close_paren: |context| {
        Some(gen_close_paren_with_type(
          GenCloseParenWithTypeOptions {
            start_lsil,
            type_node: Some(node.type_ann.into()),
            type_node_separator: {
              let mut items = PrintItems::new();
              items.push_sc(sc!(" =>"));
              items.push_signal(Signal::SpaceIfNotTrailing);
              items.push_signal(Signal::PossibleNewLine);
              items.push_condition(indent_after_arrow_condition);
              Some(items)
            },
            param_count: node.params.len(),
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

fn gen_getter_signature<'a>(node: &TsGetterSignature<'a>, context: &mut Context<'a>) -> PrintItems {
  gen_method_signature_like(
    MethodSignatureLike {
      node: node.into(),
      method_kind: MethodSignatureLikeKind::Getter,
      computed: node.computed(),
      optional: false,
      key: node.key.into(),
      parameters_range: node.get_parameters_range(context),
      type_params: None,
      params: Vec::with_capacity(0),
      type_ann: node.type_ann.map(|p| p.into()),
    },
    context,
  )
}

fn gen_setter_signature<'a>(node: &TsSetterSignature<'a>, context: &mut Context<'a>) -> PrintItems {
  gen_method_signature_like(
    MethodSignatureLike {
      node: node.into(),
      method_kind: MethodSignatureLikeKind::Setter,
      computed: node.computed(),
      optional: false,
      key: node.key.into(),
      parameters_range: node.get_parameters_range(context),
      type_params: None,
      params: vec![node.param.into()],
      type_ann: None,
    },
    context,
  )
}

fn gen_keyword_type<'a>(node: &TsKeywordType<'a>, context: &mut Context<'a>) -> PrintItems {
  // will be a keyword like "any", "unknown", "number", etc...
  node.text_fast(context.program).to_string().into()
}

fn gen_import_type<'a>(node: &TsImportType<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.push_sc(sc!("import("));
  items.extend(gen_node(node.arg.into(), context));
  items.push_sc(sc!(")"));

  if let Some(qualifier) = &node.qualifier {
    items.push_sc(sc!("."));
    items.extend(gen_node(qualifier.into(), context));
  }

  if let Some(type_args) = node.type_args {
    items.extend(gen_node(type_args.into(), context));
  }
  items
}

fn gen_indexed_access_type<'a>(node: &TsIndexedAccessType<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.extend(gen_node(node.obj_type.into(), context));
  items.extend(gen_computed_prop_like(
    |context| gen_node(node.index_type.into(), context),
    GenComputedPropLikeOptions {
      inner_node_range: node.index_type.range(),
    },
    context,
  ));
  items
}

fn gen_infer_type<'a>(node: &TsInferType<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.push_sc(sc!("infer "));
  items.extend(gen_node(node.type_param.into(), context));
  items
}

fn gen_ts_instantiation<'a>(node: &TsInstantiation<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = gen_node(node.expr.into(), context);
  items.extend(gen_node(node.type_args.into(), context));
  items
}

fn gen_intersection_type<'a>(node: &TsIntersectionType<'a>, context: &mut Context<'a>) -> PrintItems {
  gen_union_or_intersection_type(
    UnionOrIntersectionType {
      node: node.into(),
      types: node.types,
      is_union: false,
    },
    context,
  )
}

fn gen_lit_type<'a>(node: &TsLitType<'a>, context: &mut Context<'a>) -> PrintItems {
  match &node.lit {
    // need to do this in order to support negative numbers
    TsLit::Number(_) | TsLit::BigInt(_) => node.text_fast(context.program).to_string().into(),
    _ => gen_node(node.lit.into(), context),
  }
}

fn gen_mapped_type<'a>(node: &TsMappedType<'a>, context: &mut Context<'a>) -> PrintItems {
  let force_use_new_lines =
    !context.config.mapped_type_prefer_single_line && node_helpers::get_use_new_lines_for_nodes(&node.start(), node.type_param, context.program);

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

        if let Some(readonly) = node.readonly() {
          items.push_sc(match readonly {
            TruePlusMinus::True => sc!("readonly "),
            TruePlusMinus::Plus => sc!("+readonly "),
            TruePlusMinus::Minus => sc!("-readonly "),
          });
        }

        let computed_inner_range = SourceRange::new(
          node.type_param.start(),
          node.name_type.map(|t| t.end()).unwrap_or_else(|| node.type_param.end()),
        );
        items.extend(gen_computed_prop_like(
          |context| {
            let mut items = PrintItems::new();
            if let Some(name_type) = node.name_type {
              items.extend(gen_as_expr_like(
                AsExprLike {
                  expr: node.type_param.into(),
                  type_ann: name_type.into(),
                },
                context,
              ));
            } else {
              items.extend(gen_node(node.type_param.into(), context));
            }
            items
          },
          GenComputedPropLikeOptions {
            inner_node_range: computed_inner_range,
          },
          context,
        ));

        if let Some(optional) = node.optional() {
          items.push_sc(match optional {
            TruePlusMinus::True => sc!("?"),
            TruePlusMinus::Plus => sc!("+?"),
            TruePlusMinus::Minus => sc!("-?"),
          });
        }

        items.extend(gen_type_ann_with_colon_if_exists_for_type(node.type_ann, context));
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
      first_member: Some(node.type_param.range()),
      prefer_single_line_when_empty: false,
      allow_open_token_trailing_comments: true,
      single_line_space_around: false,
    },
    context,
  ));
  items
}

fn gen_optional_type<'a>(node: &TsOptionalType<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.extend(gen_node(node.type_ann.into(), context));
  items.push_sc(sc!("?"));
  items
}

fn gen_qualified_name<'a>(node: &TsQualifiedName<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.extend(gen_node(node.left.into(), context));
  items.push_sc(sc!("."));
  items.extend(gen_node(node.right.into(), context));
  items
}

fn gen_parenthesized_type<'a>(node: &TsParenthesizedType<'a>, context: &mut Context<'a>) -> PrintItems {
  if should_skip_parenthesized_type(node, context) {
    return gen_node(node.type_ann.into(), context);
  }

  let generated_type = conditions::with_indent_if_start_of_line_indented(gen_node_in_parens(
    |context| gen_node(node.type_ann.into(), context),
    GenNodeInParensOptions {
      inner_range: node.type_ann.range(),
      prefer_hanging: true,
      allow_open_paren_trailing_comments: true,
      single_line_space_around: false,
    },
    context,
  ))
  .into();

  return if use_new_line_group(node) {
    new_line_group(generated_type)
  } else {
    generated_type
  };

  fn use_new_line_group(node: &TsParenthesizedType) -> bool {
    !matches!(node.parent(), Node::TsTypeAliasDecl(_))
  }
}

fn should_skip_parenthesized_type<'a>(node: &TsParenthesizedType<'a>, context: &mut Context<'a>) -> bool {
  if node_helpers::has_surrounding_different_line_comments(node.type_ann.into(), context.program) {
    return false;
  }

  matches!(node.parent().kind(), NodeKind::TsTypeAnn | NodeKind::TsTypeAliasDecl)
}

fn gen_rest_type<'a>(node: &TsRestType<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.push_sc(sc!("..."));
  items.extend(gen_node(node.type_ann.into(), context));
  items
}

fn gen_tpl_lit_type<'a>(node: &TsTplLitType<'a>, context: &mut Context<'a>) -> PrintItems {
  gen_template_literal(
    node.quasis.iter().map(|&x| x.into()).collect(),
    node.types.iter().map(|x| x.into()).collect(),
    context,
  )
}

fn gen_tuple_type<'a>(node: &TsTupleType<'a>, context: &mut Context<'a>) -> PrintItems {
  let prefer_hanging = match context.config.tuple_type_prefer_hanging {
    PreferHanging::Never => false,
    PreferHanging::OnlySingleItem => node.elem_types.len() == 1,
    PreferHanging::Always => true,
  };
  gen_array_like_nodes(
    GenArrayLikeNodesOptions {
      node: node.into(),
      nodes: node.elem_types.iter().map(|&x| Some(x.into())).collect(),
      prefer_hanging,
      prefer_single_line: context.config.tuple_type_prefer_single_line,
      trailing_commas: context.config.tuple_type_trailing_commas,
      space_around: context.config.tuple_type_space_around,
    },
    context,
  )
}

fn gen_tuple_element<'a>(node: &TsTupleElement<'a>, context: &mut Context<'a>) -> PrintItems {
  if let Some(label) = &node.label {
    let mut items = PrintItems::new();
    items.extend(gen_node(label.into(), context));
    items.extend(gen_type_ann_with_colon_for_type(node.ty, context));
    items
  } else {
    gen_node(node.ty.into(), context)
  }
}

fn gen_type_ann<'a>(node: &TsTypeAnn<'a>, context: &mut Context<'a>) -> PrintItems {
  gen_node(node.type_ann.into(), context)
}

fn gen_type_param<'a>(node: &TsTypeParam<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();

  if node.is_const() {
    items.push_sc(sc!("const "));
  }
  if node.is_in() {
    items.push_sc(sc!("in "));
  }
  if node.is_out() {
    items.push_sc(sc!("out "));
  }

  items.extend(gen_node(node.name.into(), context));

  if let Some(constraint) = &node.constraint {
    items.push_signal(Signal::SpaceOrNewLine);
    items.push_condition(conditions::indent_if_start_of_line({
      let mut items = PrintItems::new();
      items.push_sc(if node.parent().kind() == NodeKind::TsMappedType {
        sc!("in")
      } else {
        sc!("extends")
      });
      items.push_signal(Signal::SpaceIfNotTrailing);
      items.extend(gen_node(constraint.into(), context));
      items
    }));
  }

  if let Some(default) = &node.default {
    items.extend(gen_assignment(default.into(), sc!("="), context));
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
      separator: get_trailing_commas(node, context).into(),
      single_line_options: ir_helpers::SingleLineOptions::same_line_maybe_space_separated(),
      multi_line_options: ir_helpers::MultiLineOptions::surround_newlines_indented(),
      force_possible_newline_at_start: false,
      node_sorter: None,
    },
    context,
  ));
  items.push_sc(sc!(">"));

  return items;

  fn get_trailing_commas<'a>(node: TypeParamNode<'a>, context: &mut Context<'a>) -> TrailingCommas {
    let trailing_commas = context.config.type_parameters_trailing_commas;
    if trailing_commas == TrailingCommas::Never {
      return trailing_commas;
    }

    // trailing commas should be allowed in type parameters only—not arguments
    if let Some(type_params) = node.parent().get_type_parameters() {
      if type_params.start() == node.start() {
        // Use trailing commas for function expressions in a JSX file
        // if the absence of one would lead to a parsing ambiguity.
        let parent = node.parent();
        let is_ambiguous_jsx_fn_expr = context.is_jsx()
          && (parent.kind() == NodeKind::ArrowExpr || parent.parent().unwrap().kind() == NodeKind::FnExpr)
          // not ambiguous in a default export
          && !matches!(
            parent.parent().and_then(|p| p.parent()).map(|p| p.kind()),
            Some(NodeKind::ExportDefaultExpr | NodeKind::ExportDefaultDecl)
          );
        // Prevent "This syntax is reserved in files with the .mts or .cts extension." diagnostic.
        let is_cts_mts_arrow_fn = matches!(context.media_type, MediaType::Cts | MediaType::Mts) && parent.kind() == NodeKind::ArrowExpr;
        if is_ambiguous_jsx_fn_expr || is_cts_mts_arrow_fn {
          let children = type_params.children();
          // It is not ambiguous if there are multiple type parameters.
          if children.len() == 1 && children[0].kind() == NodeKind::TsTypeParam {
            let type_param = children[0];
            let children = type_param.children();
            // We have a possible ambiguity iff this type parameter is just an identifier.
            if children.len() == 1 && matches!(children[0].kind(), NodeKind::Ident | NodeKind::IdentName) {
              return TrailingCommas::Always;
            }
          }
        }
        return trailing_commas;
      }
    }

    TrailingCommas::Never
  }

  fn get_use_new_lines(node: &TypeParamNode, params: &[Node], context: &mut Context) -> bool {
    if context.config.type_parameters_prefer_single_line || params.is_empty() {
      false
    } else {
      let first_param = &params[0];
      let angle_bracket_pos = node.start();
      node_helpers::get_use_new_lines_for_nodes(&angle_bracket_pos, first_param, context.program)
    }
  }
}

fn gen_type_operator<'a>(node: &TsTypeOperator<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.push_sc(match node.op() {
    TsTypeOperatorOp::KeyOf => sc!("keyof"),
    TsTypeOperatorOp::Unique => sc!("unique"),
    TsTypeOperatorOp::ReadOnly => sc!("readonly"),
  });
  items.push_signal(Signal::SpaceIfNotTrailing);
  items.extend(gen_node(node.type_ann.into(), context));
  items
}

fn gen_type_predicate<'a>(node: &TsTypePredicate<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  if node.asserts() {
    items.push_sc(sc!("asserts "));
  }
  items.extend(gen_node(node.param_name.into(), context));
  if let Some(type_ann) = node.type_ann {
    items.push_sc(sc!(" is"));
    items.push_signal(Signal::SpaceIfNotTrailing);
    items.extend(gen_node(type_ann.into(), context));
  }
  items
}

fn gen_type_query<'a>(node: &TsTypeQuery<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.push_sc(sc!("typeof"));
  items.push_signal(Signal::SpaceIfNotTrailing);
  items.extend(gen_node(node.expr_name.into(), context));
  if let Some(type_args) = node.type_args {
    items.extend(gen_node(type_args.into(), context));
  }
  items
}

fn gen_type_reference<'a>(node: &TsTypeRef<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  items.extend(gen_node(node.type_name.into(), context));
  if let Some(type_params) = node.type_params {
    items.extend(gen_node(type_params.into(), context));
  }
  items
}

fn gen_union_type<'a>(node: &TsUnionType<'a>, context: &mut Context<'a>) -> PrintItems {
  gen_union_or_intersection_type(
    UnionOrIntersectionType {
      node: node.into(),
      types: node.types,
      is_union: true,
    },
    context,
  )
}

struct UnionOrIntersectionType<'a, 'b> {
  pub node: Node<'a>,
  pub types: &'b [TsType<'a>],
  pub is_union: bool,
}

fn gen_union_or_intersection_type<'a, 'b>(node: UnionOrIntersectionType<'a, 'b>, context: &mut Context<'a>) -> PrintItems {
  // todo: configuration for operator position
  let mut items = PrintItems::new();
  let force_use_new_lines = get_use_new_lines_for_nodes(node.types, context.config.union_and_intersection_type_prefer_single_line, context);
  let separator = if node.is_union { sc!("|") } else { sc!("&") };

  let indent_width = context.config.indent_width;
  let prefer_hanging = context.config.union_and_intersection_type_prefer_hanging;
  let is_parent_union_or_intersection = matches!(node.node.parent().unwrap().kind(), NodeKind::TsUnionType | NodeKind::TsIntersectionType);
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
      let is_multi_line_or_hanging = is_multi_line_or_hanging_ref.create_resolver();
      let types_count = node.types.len();
      let mut generated_nodes = Vec::new();
      for (i, type_node) in node.types.iter().enumerate() {
        let (allow_inline_multi_line, allow_inline_single_line) = {
          let is_last_value = i + 1 == types_count; // allow the last type to be single line
          (allows_inline_multi_line(type_node.into(), context, types_count > 1), is_last_value)
        };
        let separator_token = context.token_finder.get_previous_token_if_operator(&type_node.range(), separator.text);
        let start_lc = LineAndColumn::new("start");
        let after_separator_ln = LineNumber::new("afterSeparator");
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
        items.extend(gen_node(type_node.into(), context));

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

  fn use_surround_newlines<'a>(node: Node<'a>, context: &mut Context<'a>) -> bool {
    let parent = node.parent().unwrap();
    match parent {
      Node::TsTypeAssertion(_) => true,
      Node::TsParenthesizedType(paren_type) => !should_skip_parenthesized_type(paren_type, context),
      _ => false,
    }
  }
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
      } else if last_comment.kind == CommentKind::Block {
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
      was_last_block_comment = comment.kind == CommentKind::Block;
    }
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
        if first_comment.kind == CommentKind::Block {
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
      let next_token_pos = context.token_finder.get_next_token_pos_after(&previous_token_end);
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

fn gen_comment(comment: &Comment, context: &mut Context) -> Option<PrintItems> {
  // only generate if handled
  if context.has_handled_comment(comment) {
    return None;
  }

  // mark handled and generate
  context.mark_comment_handled(comment);

  return Some(match comment.kind {
    CommentKind::Block => {
      if has_leading_astrisk_each_line(&comment.text) {
        gen_js_doc_or_multiline_block(comment, context)
      } else {
        // Single-line comment block
        ir_helpers::gen_js_like_comment_block(&comment.text)
      }
    }
    CommentKind::Line => ir_helpers::gen_js_like_comment_line(&comment.text, context.config.comment_line_force_space_after_slashes),
  });

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

fn gen_js_doc_or_multiline_block(comment: &Comment, _context: &mut Context) -> PrintItems {
  debug_assert_eq!(comment.kind, CommentKind::Block);
  let is_js_doc = comment.text.starts_with('*');
  return lines_to_print_items(is_js_doc, build_lines(comment));

  fn build_lines(comment: &Comment) -> Vec<&str> {
    let mut lines: Vec<&str> = Vec::new();

    for line in utils::split_lines(&comment.text) {
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
    if first_unhandled_comment.kind == CommentKind::Block {
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
    let next_token_pos = context.token_finder.get_next_token_pos_after(&comma_end);
    let next_token_pos_line_start = next_token_pos.start_line_fast(context.program);
    if next_token_pos_line_start == node_end_line {
      return trailing_comments_on_same_line.into_iter().filter(|c| c.start() < comma_end).collect::<Vec<_>>();
    }
  }
  trailing_comments_on_same_line
}

fn get_jsx_empty_expr_comments<'a>(node: &JSXEmptyExpr, context: &mut Context<'a>) -> CommentsIterator<'a> {
  node.end().leading_comments_fast(context.program)
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
        .find(|t| t.token == Token::LBracket)
        .expect("Expected to find an open bracket token.");

      // todo: tests for this (ex. [\n,] -> [\n    ,\n])
      node_helpers::get_use_new_lines_for_nodes(&open_bracket_token.range(), &nodes[0], context.program)
    }
  }

  fn allow_trailing_commas(nodes: &[NodeOrSeparator]) -> bool {
    if let Some(NodeOrSeparator::Node(last)) = nodes.last() {
      // this would be a syntax error
      if last.kind() == NodeKind::RestPat {
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
    .find(|t| t.token == Token::LBrace)
    .expect("Expected to find an open brace token.");
  let close_brace_token = child_tokens
    .iter()
    .rev()
    .find(|t| t.token == Token::RBrace)
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
      let is_empty_stmt = node.is::<EmptyStmt>();
      if !is_empty_stmt {
        let mut separator_items = PrintItems::new();
        if let Some(last_node) = &last_node {
          separator_items.push_signal(Signal::NewLine);
          if node_helpers::has_separating_blank_line(&last_node, &node, context.program) {
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

        generated_nodes.push(items);

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
    items.extend(gen_trailing_comments_as_statements(last_node, context));
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
  ) -> Option<Box<dyn Fn((usize, Option<Node<'a>>), (usize, Option<Node<'a>>), Program<'a>) -> std::cmp::Ordering>> {
    match group_kind {
      StmtGroupKind::Imports => get_node_sorter_from_order(context.config.module_sort_import_declarations, NamedTypeImportsExportsOrder::None),
      StmtGroupKind::Exports => get_node_sorter_from_order(context.config.module_sort_export_declarations, NamedTypeImportsExportsOrder::None),
      StmtGroupKind::Other => None,
    }
  }
}

fn gen_member_or_member_expr_stmt_comments(node: Node, context: &mut Context) -> PrintItems {
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
      Node::ImportDecl(decl) if !decl.specifiers.is_empty() => StmtGroupKind::Imports,
      Node::ExportAll(_) => StmtGroupKind::Exports,
      Node::NamedExport(NamedExport { src: Some(_), .. }) => StmtGroupKind::Exports,
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

  for (i, (node, optional_print_items)) in opts.items.into_iter().enumerate() {
    // class declarations may have empty statements
    let is_empty_stmt = node.is::<EmptyStmt>();
    if !is_empty_stmt {
      if let Some(last_node) = last_node {
        if should_use_new_line(&opts.should_use_new_line, last_node, node, context) {
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

      let end_ln = LineNumber::new("endMember");
      context.end_statement_or_member_lns.push(end_ln);
      items.extend(if let Some(print_items) = optional_print_items {
        print_items
      } else if opts.separator.is_none() {
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
    items.extend(gen_trailing_comments_as_statements(&last_node.range(), context));
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
  let force_use_new_lines = get_use_new_lines_for_nodes_with_preceeding_token("(", &nodes, prefer_single_line, context);
  let range = opts.range;
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
      if let Node::CallExpr(call_expr) = &node {
        if let Callee::Import(_) = &call_expr.callee {
          return true;
        }
      }

      false
    }

    fn is_param_rest_pat(param: Node) -> bool {
      if let Node::Param(param) = param {
        param.pat.kind() == NodeKind::RestPat
      } else {
        // arrow functions will not be a Param
        param.kind() == NodeKind::RestPat
      }
    }
  }

  fn only_single_item_and_no_comments<'a>(nodes: &[Node<'a>], program: Program<'a>) -> bool {
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
      Some(TokenAndSpan { token: Token::Comma, span, .. }) => span.trailing_comments_fast(program).is_empty(),
      _ => true,
    }
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

      if use_new_line_group {
        new_line_group(items)
      } else {
        items
      }
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
          Node::TsUnionType(_) | Node::TsIntersectionType(_) => false,
          Node::TsTypeAnn(type_ann) => !matches!(type_ann.type_ann, TsType::TsUnionOrIntersectionType(_)),
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
  node_sorter: Option<Box<dyn Fn((usize, Option<Node<'a>>), (usize, Option<Node<'a>>), Program<'a>) -> std::cmp::Ordering>>,
}

enum NodeOrSeparator<'a> {
  Node(Node<'a>),
  Separator(&'a TokenAndSpan),
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

  ir_helpers::gen_separated_values(
    |is_multi_line_or_hanging_ref| {
      let is_multi_line_or_hanging = is_multi_line_or_hanging_ref.create_resolver();
      let mut generated_nodes = Vec::new();
      let nodes_count = nodes.len();
      let sorted_indexes = node_sorter.map(|sorter| get_sorted_indexes(nodes.iter().map(|d| d.as_node()), sorter, context));

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
          NodeOrSeparator::Node(Node::ExprOrSpread(expr_or_spread)) => !matches!(expr_or_spread.expr, Expr::Object(_) | Expr::Array(_)),
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
  sorter: Box<dyn Fn((usize, Option<Node<'a>>), (usize, Option<Node<'a>>), Program<'a>) -> std::cmp::Ordering>,
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

  fn get_comma_token<'a>(element: Node<'a>, context: &mut Context<'a>) -> Option<&'a TokenAndSpan> {
    match context.token_finder.get_next_token_if_comma(&element) {
      Some(comma) => Some(comma),
      None => context.token_finder.get_last_token_within_if_comma(&element), // may occur for type literals
    }
  }
}

/// Some nodes don't have a TsTypeAnn, but instead a Box<TsType>
fn gen_type_ann_with_colon_if_exists_for_type<'a>(type_ann: Option<TsType<'a>>, context: &mut Context<'a>) -> PrintItems {
  if let Some(type_ann) = type_ann {
    gen_type_ann_with_colon_for_type(type_ann, context)
  } else {
    PrintItems::new()
  }
}

fn gen_type_ann_with_colon_for_type<'a>(type_ann: TsType<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  if context.config.type_annotation_space_before_colon {
    items.push_space();
  }
  let colon_token = context.token_finder.get_previous_token_if_colon(&type_ann);
  #[cfg(debug_assertions)]
  assert_has_op(":", colon_token, context);
  items.extend(gen_type_ann_with_colon(type_ann.into(), colon_token, context));
  items
}

fn gen_type_ann_with_colon_if_exists<'a>(type_ann: Option<&TsTypeAnn<'a>>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  if let Some(type_ann) = type_ann {
    if context.config.type_annotation_space_before_colon {
      items.push_space();
    }
    let colon_token = context.token_finder.get_first_colon_token_within(type_ann);
    #[cfg(debug_assertions)]
    assert_has_op(":", colon_token, context);
    items.extend(gen_type_ann_with_colon(type_ann.into(), colon_token, context));
  }
  items
}

fn gen_type_ann_with_colon<'a>(type_ann: Node<'a>, colon_token: Option<&TokenAndSpan>, context: &mut Context<'a>) -> PrintItems {
  gen_assignment_like_with_token(type_ann, sc!(":"), colon_token, context)
}

struct GenBraceSeparatorOptions<'a> {
  brace_position: BracePosition,
  open_brace_token: Option<&'a TokenAndSpan>,
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
        if node_helpers::get_use_new_lines_for_nodes(&paren_range.start(), &inner_range, context.program) {
          return true;
        }
      }
    }

    has_any_node_comment_on_different_line(&[inner_range], context)
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
  allow_blank_lines: bool,
  node_sorter: Option<Box<dyn Fn((usize, Option<Node<'a>>), (usize, Option<Node<'a>>), Program<'a>) -> std::cmp::Ordering>>,
}

fn gen_object_like_node<'a>(opts: GenObjectLikeNodeOptions<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();

  let child_tokens = get_tokens_from_children_with_tokens(opts.node, context.program);
  let open_brace_token = child_tokens.iter().find(|t| t.token == Token::LBrace);
  let close_brace_token = child_tokens.iter().rev().find(|t| t.token == Token::RBrace);
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
              space_at_end: opts.surround_single_line_with_spaces,
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
    MemberLikeExprItem::Node(node) => {
      let is_optional = item.is_optional();
      gen_node_with_inner_gen(*node, context, |node_items, context| {
        // Changing `2 .toString()` to `2.toString()` is a syntax error because `2.` is a numeric
        // literal, so we surround it in parenthesis `(2).toString()`
        let is_first_and_number = is_first && node.kind() == NodeKind::Number && !node.text_fast(context.program).contains('.');
        let mut items = PrintItems::new();
        if is_first_and_number {
          items.push_sc(sc!("("));
        }
        if !is_first {
          if is_optional {
            items.push_sc(sc!("?."));
          } else if node.kind() != NodeKind::ComputedPropName {
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
    MemberLikeExprItem::Token(token) => {
      // don't bother with intertwined comments as its too much trouble
      let mut items = PrintItems::new();
      if !is_first {
        items.push_sc(sc!("."));
      }
      items.push_string(token.text_fast(context.program).to_string());
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
      node_helpers::get_use_new_lines_for_nodes(&range.start(), &inner_node_range.start(), context.program)
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

fn gen_decorators<'a>(decorators: &[&'a Decorator<'a>], is_inline: bool, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  if decorators.is_empty() {
    return items;
  }

  let force_use_new_lines = !context.config.decorators_prefer_single_line
    && decorators.len() >= 2
    && node_helpers::get_use_new_lines_for_nodes(decorators[0], decorators[1], context.program);

  let separated_values_result = gen_separated_values_with_result(
    GenSeparatedValuesParams {
      nodes: decorators.iter().map(|&p| NodeOrSeparator::Node(p.into())).collect(),
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
    let next_token_pos = context.token_finder.get_next_token_pos_after(*last_dec);
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
  body_node: Stmt<'a>,
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

fn force_use_braces_for_stmt(stmt: Stmt) -> bool {
  match stmt {
    Stmt::Block(block) => {
      if block.stmts.len() != 1 {
        true
      } else {
        force_use_braces_for_stmt(block.stmts[0])
      }
    }
    // force braces for any children where no braces could be ambiguous
    Stmt::Empty(_)
    | Stmt::DoWhile(_)
    | Stmt::For(_)
    | Stmt::ForIn(_)
    | Stmt::ForOf(_)
    | Stmt::Decl(_)
    | Stmt::If(_) // especially force for this as it may cause a bug
    | Stmt::Labeled(_)
    | Stmt::Switch(_)
    | Stmt::Try(_)
    | Stmt::While(_)
    | Stmt::With(_) => true,
    Stmt::Break(_) | Stmt::Continue(_) | Stmt::Debugger(_) | Stmt::Expr(_) | Stmt::Return(_) | Stmt::Throw(_) => false,
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
  let body_should_be_multi_line = get_body_should_be_multi_line(opts.body_node, &header_trailing_comments, context);
  let should_use_new_line = get_should_use_new_line(opts.body_node, body_should_be_multi_line, &opts.single_body_position, context);
  let open_brace_token = get_open_brace_token(opts.body_node, context);
  let use_braces = opts.use_braces;
  let is_body_empty_stmt = opts.body_node.kind() == NodeKind::EmptyStmt;
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

  if let Node::BlockStmt(body_node) = opts.body_node {
    items.extend(ir_helpers::with_indent({
      let mut items = PrintItems::new();
      // generate the remaining trailing comments inside because some of them are generated already
      // by parsing the header trailing comments
      items.extend(gen_leading_comments(&body_node.range(), context));
      let inner_range = body_node.get_inner_range(context);
      if body_node.stmts.is_empty() {
        let trailing_comments_same_line = get_trailing_comments_on_same_line(&inner_range.start().range(), context);
        items.extend(gen_comments_same_line(trailing_comments_same_line, context));
        // treat the remaining unhandled comments as statements
        items.extend(gen_comments_as_statements(
          inner_range.start().trailing_comments_fast(context.program),
          None,
          context,
        ));
      } else {
        items.extend(gen_statements(inner_range, body_node.stmts.iter().map(|x| x.into()).collect(), context));
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
          get_body_stmt_start_line(body_node, context) > body_node.previous_token_fast(context.program).start_line_fast(context.program)
        }
        SameOrNextLinePosition::NextLine => true,
        SameOrNextLinePosition::SameLine => {
          if let Node::BlockStmt(block_stmt) = body_node {
            if block_stmt.stmts.len() != 1 {
              return true;
            }
            return get_body_stmt_start_line(body_node, context) > body_node.previous_token_fast(context.program).start_line_fast(context.program);
          }
          return false;
        }
      };
    } else {
      if let Node::BlockStmt(block_stmt) = body_node {
        if block_stmt.stmts.is_empty() {
          // keep the block on the same line
          return block_stmt.start_line_fast(context.program) < block_stmt.end_line_fast(context.program);
        }
      }
      return true;
    }

    fn get_body_stmt_start_line(body_node: Node, context: &mut Context) -> usize {
      if let Node::BlockStmt(body_node) = body_node {
        if let Some(first_stmt) = body_node.stmts.first() {
          return first_stmt.start_line_fast(context.program);
        }
      }
      body_node.start_line_fast(context.program)
    }
  }

  fn get_body_should_be_multi_line<'a>(body_node: Node<'a>, header_trailing_comments: &[&'a Comment], context: &mut Context<'a>) -> bool {
    if let Node::BlockStmt(body_node) = body_node {
      if body_node.stmts.len() == 1 && !has_leading_comment_on_different_line(&body_node.stmts[0].range(), header_trailing_comments, context.program) {
        return false;
      }
      if body_node.stmts.is_empty() && body_node.start_line_fast(context.program) == body_node.end_line_fast(context.program) {
        return false;
      }
      return true;
    } else {
      return has_leading_comment_on_different_line(&body_node.range(), header_trailing_comments, context.program);
    }

    fn has_leading_comment_on_different_line<'a>(node: &SourceRange, header_trailing_comments: &[&'a Comment], program: Program<'a>) -> bool {
      node_helpers::has_leading_comment_on_different_line(node, /* comments to ignore */ Some(header_trailing_comments), program)
    }
  }

  fn get_force_braces(body_node: Node) -> bool {
    if let Node::BlockStmt(body_node) = body_node {
      body_node.stmts.is_empty()
        || body_node.stmts.iter().all(|s| s.kind() == NodeKind::EmptyStmt)
        || (body_node.stmts.len() == 1 && matches!(body_node.stmts[0], Stmt::Decl(_)))
    } else {
      false
    }
  }

  fn get_header_trailing_comments<'a>(body_node: Node<'a>, context: &mut Context<'a>) -> Vec<&'a Comment> {
    let mut comments = Vec::new();
    if let Node::BlockStmt(block_stmt) = body_node {
      let comment_line = body_node.leading_comments_fast(context.program).find(|c| c.kind == CommentKind::Line);
      if let Some(comment) = comment_line {
        comments.push(comment);
        return comments;
      }

      let open_brace_token = context
        .token_finder
        .get_first_open_brace_token_within(block_stmt)
        .expect("Expected to find an open brace token.");
      let body_node_start_line = body_node.start_line_fast(context.program);
      comments.extend(
        open_brace_token
          .trailing_comments_fast(context.program)
          .take_while(|c| c.start_line_fast(context.program) == body_node_start_line && c.kind == CommentKind::Line),
      );
    } else {
      let leading_comments = body_node.leading_comments_fast(context.program);
      let last_header_token_end = context.token_finder.get_previous_token_end_before(&body_node);
      let last_header_token_end_line = last_header_token_end.end_line_fast(context.program);
      comments.extend(leading_comments.take_while(|c| c.start_line_fast(context.program) <= last_header_token_end_line && c.kind == CommentKind::Line));
    }

    comments
  }

  fn get_open_brace_token<'a>(body_node: Node<'a>, context: &mut Context<'a>) -> Option<&'a TokenAndSpan> {
    if let Node::BlockStmt(block_stmt) = body_node {
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
      if found_non_space_child && node_helpers::has_jsx_space_expr_text(child) {
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
    if let Some(TokenAndSpan {
      token: deno_ast::swc::parser::token::Token::JSXText { .. },
      span,
      had_line_break,
    }) = past_token
    {
      let text = span.text_fast(context.program);
      if !had_line_break && text.ends_with(' ') {
        return true;
      }
    }
    if let Node::JSXText(child) = current {
      child.text_fast(context.program).starts_with(' ')
    } else {
      false
    }
  }

  /// If the node has a "JSX space expression" between or text that's only spaces between.
  fn has_jsx_space_between<'a>(previous_node: Node<'a>, next_node: Node<'a>, program: Program<'a>) -> bool {
    return node_helpers::nodes_have_only_spaces_between(previous_node, next_node, program) || has_jsx_space_expr_between(previous_node, next_node);

    fn has_jsx_space_expr_between(previous_node: Node, next_node: Node) -> bool {
      let nodes_between = node_helpers::get_siblings_between(previous_node, next_node);

      for node_between in nodes_between {
        if node_helpers::has_jsx_space_expr_text(node_between) {
          return true;
        }
      }

      false
    }
  }
}

fn jsx_space_separator<'a>(previous_node: Node<'a>, current_node: Node<'a>, context: &Context<'a>) -> PrintItems {
  return if node_should_force_newline_if_multi_line(previous_node) || node_should_force_newline_if_multi_line(current_node) {
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

fn gen_assignment_like_with_token<'a>(expr: Node<'a>, op: &'static StringContainer, op_token: Option<&TokenAndSpan>, context: &mut Context<'a>) -> PrintItems {
  let use_new_line_group = get_use_new_line_group(expr);
  let mut items = PrintItems::new();

  if op.text != ":" {
    items.push_space();
  }
  items.push_sc(op);

  let op_end = op_token
    .map(|x| x.end())
    .unwrap_or_else(|| context.token_finder.get_previous_token_end_before(&expr));
  let op_trailing_comments = gen_comments_between_lines_indented(op_end, context);
  let had_op_trailing_comments = !op_trailing_comments.is_empty();
  items.extend(op_trailing_comments);

  let generated_assignment = {
    let mut items = PrintItems::new();
    if !had_op_trailing_comments {
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
    matches!(expr, Node::MemberExpr(_))
  }
}

struct GenBlockOptions<'a> {
  range: Option<SourceRange>,
  children: Vec<Node<'a>>,
}

fn gen_block<'a>(gen_inner: impl FnOnce(Vec<Node<'a>>, &mut Context<'a>) -> PrintItems, opts: GenBlockOptions<'a>, context: &mut Context<'a>) -> PrintItems {
  let mut items = PrintItems::new();
  let before_open_token_ln = LineNumber::new("after_open_token_info");
  let first_member_range = opts.children.first().map(|x| x.range());
  let range = opts.range;
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
      items.extend(ir_helpers::with_indent(gen_inner(opts.children, context)));
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
    let open_token_end = range.start() + opts.open_token.text.len();
    let close_token_start = range.end() - opts.close_token.text.len();

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
      is_single_line = open_token_start_line == close_token_start.start_line_fast(context.program);

      if is_single_line && opts.single_line_space_around {
        items.push_space();
      }

      if !comments.is_empty() {
        // generate the trailing comment on the first line only if multi-line and if a comment line
        if !is_single_line {
          items.extend(gen_first_line_trailing_comment(open_token_start_line, comments.clone(), context));
        }

        // generate the comments
        if comments.clone().any(|c| !context.has_handled_comment(c)) {
          if is_single_line {
            let indent_width = context.config.indent_width;
            items.extend(
              ir_helpers::gen_separated_values(
                |_| {
                  let mut generated_comments = Vec::new();
                  for c in comments {
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
            items.extend(with_indent(gen_comments_as_statements(comments, None, context)));
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
fn assert_has_op(op: &str, op_token: Option<&TokenAndSpan>, context: &mut Context) {
  if let Some(op_token) = op_token {
    context.assert_text(SourceRange::new(op_token.start(), op_token.end()), op);
  } else {
    panic!("Debug panic! Expected to have op token: {op}");
  }
}

fn use_new_line_group_for_arrow_body<'a>(arrow_expr: &'a ArrowExpr<'a>, context: &Context<'a>) -> bool {
  match &arrow_expr.body {
    BlockStmtOrExpr::Expr(expr) => match expr {
      Expr::Paren(paren) => match paren.expr {
        Expr::Object(_) => false,
        _ => !is_jsx_paren_expr_handled_node(paren.expr.into(), context),
      },
      _ => !is_jsx_paren_expr_handled_node(expr.into(), context),
    },
    _ => true,
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

fn assign_op_sc(assign_op: AssignOp) -> &'static StringContainer {
  use AssignOp::*;
  let sc = match assign_op {
    Assign => sc!("="),
    AddAssign => sc!("+="),
    SubAssign => sc!("-="),
    MulAssign => sc!("*="),
    DivAssign => sc!("/="),
    ModAssign => sc!("%="),
    LShiftAssign => sc!("<<="),
    RShiftAssign => sc!(">>="),
    ZeroFillRShiftAssign => sc!(">>>="),
    BitOrAssign => sc!("|="),
    BitXorAssign => sc!("^="),
    BitAndAssign => sc!("&="),
    ExpAssign => sc!("**="),
    AndAssign => sc!("&&="),
    OrAssign => sc!("||="),
    NullishAssign => sc!("??="),
  };
  if cfg!(debug_assertions) {
    assert_eq!(sc.text, assign_op.as_str());
  }
  sc
}

fn binary_op_sc(binary_op: BinaryOp) -> &'static StringContainer {
  use BinaryOp::*;
  let sc = match binary_op {
    EqEq => sc!("=="),
    NotEq => sc!("!="),
    EqEqEq => sc!("==="),
    NotEqEq => sc!("!=="),
    Lt => sc!("<"),
    LtEq => sc!("<="),
    Gt => sc!(">"),
    GtEq => sc!(">="),
    LShift => sc!("<<"),
    RShift => sc!(">>"),
    ZeroFillRShift => sc!(">>>"),
    Add => sc!("+"),
    Sub => sc!("-"),
    Mul => sc!("*"),
    Div => sc!("/"),
    Mod => sc!("%"),
    BitOr => sc!("|"),
    BitXor => sc!("^"),
    BitAnd => sc!("&"),
    LogicalOr => sc!("||"),
    LogicalAnd => sc!("&&"),
    In => sc!("in"),
    InstanceOf => sc!("instanceof"),
    Exp => sc!("**"),
    NullishCoalescing => sc!("??"),
  };
  if cfg!(debug_assertions) {
    assert_eq!(sc.text, binary_op.as_str());
  }
  sc
}

/* is/has functions */

fn is_arrow_function_with_expr_body(node: Node) -> bool {
  match node {
    Node::ExprOrSpread(expr_or_spread) => match expr_or_spread.expr {
      Expr::Arrow(arrow) => matches!(&arrow.body, BlockStmtOrExpr::Expr(_)),
      _ => false,
    },
    _ => false,
  }
}

fn allows_inline_multi_line<'a>(node: Node<'a>, context: &Context<'a>, has_siblings: bool) -> bool {
  return match node {
    Node::Param(param) => allows_inline_multi_line(param.pat.into(), context, has_siblings),
    Node::TsAsExpr(as_expr) => {
      allows_inline_multi_line(as_expr.expr.into(), context, has_siblings)
        && match as_expr.type_ann {
          TsType::TsTypeRef(_) | TsType::TsKeywordType(_) => true,
          _ => allows_inline_multi_line(as_expr.type_ann.into(), context, has_siblings),
        }
    }
    Node::FnExpr(_)
    | Node::ArrowExpr(_)
    | Node::ObjectLit(_)
    | Node::ArrayLit(_)
    | Node::ObjectPat(_)
    | Node::ArrayPat(_)
    | Node::TsTypeLit(_)
    | Node::TsTupleType(_)
    | Node::TsArrayType(_) => true,
    Node::ExprOrSpread(node) => allows_inline_multi_line(node.expr.into(), context, has_siblings),
    Node::ParenExpr(node) => should_skip_paren_expr(node, context) && allows_inline_multi_line(node.expr.into(), context, has_siblings),
    Node::TaggedTpl(_) | Node::Tpl(_) => !has_siblings,
    Node::CallExpr(node) => !has_siblings && allow_inline_for_call_expr(node.into()),
    Node::OptCall(node) => !has_siblings && allow_inline_for_call_expr(node.into()),
    Node::BindingIdent(node) => match &node.type_ann {
      Some(type_ann) => allows_inline_multi_line(type_ann.type_ann.into(), context, has_siblings),
      None => false,
    },
    Node::AssignPat(node) => {
      allows_inline_multi_line(node.left.into(), context, has_siblings) || allows_inline_multi_line(node.right.into(), context, has_siblings)
    }
    Node::TsTypeAnn(type_ann) => allows_inline_multi_line(type_ann.type_ann.into(), context, has_siblings),
    Node::TsTupleElement(tuple_element) => allows_inline_multi_line(tuple_element.ty.into(), context, has_siblings),
    _ => false,
  };

  fn allow_inline_for_call_expr(node: CallOrOptCallExpr) -> bool {
    // do not allow call exprs with nested call exprs in the member expr to be inline
    return allow_for_callee(&node.callee());

    fn allow_for_callee(callee: &Callee) -> bool {
      match callee {
        Callee::Expr(expr) => allow_for_expr(expr),
        Callee::Import(_) => false,
        Callee::Super(_) => true,
      }
    }

    fn allow_for_expr(expr: &Expr) -> bool {
      match expr {
        Expr::Member(member_expr) => allow_for_expr(&member_expr.obj),
        Expr::Call(_)
        | Expr::OptChain(OptChainExpr {
          base: OptChainBase::Call(_), ..
        }) => false,
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
    } else if let Some(comma) = context.token_finder.get_next_token_if_comma(&node_end) {
      if check_pos_has_trailing_comments(comma.end(), next_node_pos, context) {
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

fn get_comma_tokens_from_children_with_tokens<'a>(node: Node<'a>, program: Program<'a>) -> Vec<&'a TokenAndSpan> {
  node
    .children_with_tokens_fast(program)
    .into_iter()
    .filter_map(|c| match c {
      NodeOrToken::Token(token) => {
        if token.token == Token::Comma {
          Some(token)
        } else {
          None
        }
      }
      _ => None,
    })
    .collect::<Vec<_>>()
}

fn get_tokens_from_children_with_tokens<'a>(node: Node<'a>, program: Program<'a>) -> Vec<&'a TokenAndSpan> {
  node
    .children_with_tokens_fast(program)
    .into_iter()
    .filter_map(|n| match n {
      NodeOrToken::Token(t) => Some(t),
      _ => None,
    })
    .collect::<Vec<_>>()
}
