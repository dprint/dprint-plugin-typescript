use dprint_core::formatting::bumpalo;
use dprint_core::formatting::bumpalo::collections::CollectIn;
use dprint_core::formatting::condition_resolvers;
use dprint_core::formatting::conditions::*;
use dprint_core::formatting::ir_helpers::*;
use dprint_core::formatting::*;
use dprint_core_macros::sc;
use std::borrow::Cow;
use std::cell::Cell;
use std::cell::RefCell;

use super::ast_helpers::get_flattened_bin_expr;
use super::ast_helpers::*;
use super::imports::classify::classify_import;
use super::imports::partition::partition_indices;
use super::sorting::*;
use super::*;
use crate::ast::*;
use crate::configuration::*;
use crate::tailwind;
use crate::utils;

mod class;
mod comments;
mod declarations;
mod expressions;
mod expressions2;
mod helpers;
mod helpers2;
mod interface_jsx;
mod statements;
mod types;

use class::*;
use comments::*;
use declarations::*;
use expressions::*;
use expressions2::*;
use helpers::*;
// the local one instead of the one from dprint_core
use helpers::gen_separated_values;
use helpers2::*;
use interface_jsx::*;
use statements::*;
use types::*;

pub(crate) use comments::detached_preamble_len;
pub(crate) use helpers::StmtGroupKind;
pub(crate) use helpers::get_semicolon_on_later_line;
pub(crate) use helpers::get_stmt_group_kind;
pub(crate) use helpers::get_stmt_range_with_decorators;
pub(crate) use statements::IfChainBranch;

pub fn generate<'a>(
  arena: &'a Arena,
  program: ProgramInfo<'a>,
  config: &'a Configuration,
  external_formatter: Option<&'a ExternalFormatter<'a>>,
) -> crate::Result<PrintItems<'a>> {
  let program_node = program.node();
  let mut context = Context::new(arena, program_node, program, config, external_formatter);
  let mut items = gen_node(program_node, &mut context);
  items.push_condition(if_true(
    "endOfFileNewLine",
    context
      .arena
      .alloc_resolver(|context| Some(context.writer_info.column_number > 0 || context.writer_info.line_number > 0)),
    Signal::NewLine.into_print_items(context.arena),
  ));

  #[cfg(debug_assertions)]
  context.assert_end_of_file_state();

  if let Some(diagnostic) = context.diagnostics.pop() {
    return Err(diagnostic.message.into());
  }

  if config.file_indent_level > 0 {
    Ok(with_indent_times(items, config.file_indent_level))
  } else {
    Ok(items)
  }
}

pub(super) fn gen_node<'a>(node: Node<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  gen_node_with_inner_gen(node, context, |items, _| items)
}

pub(super) fn gen_node_with_inner_gen<'a>(
  node: Node<'a>,
  context: &mut Context<'a>,
  inner_gen: impl FnOnce(PrintItems<'a>, &mut Context<'a>) -> PrintItems<'a>,
) -> PrintItems<'a> {
  let node_kind = node.kind();
  // eprintln!("Node kind: {:?}", node_kind);
  // eprintln!("Text: {:?}", node.text_fast(context.program));
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

  // do not get the comments for the program as this will be handled in gen_statements
  if node_kind != NodeKind::Program {
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
    let (node_text, node_text_end) = if node_kind == NodeKind::JSXText {
      // keep the leading text, but leave the trailing text to be formatted if on a separate line
      let node_text = node.text_fast(context.program);
      let end_trim = node_text.trim_end_matches(utils::is_jsx_whitespace_char);
      (if node_text[end_trim.len()..].contains('\n') { end_trim } else { node_text }, node_end)
    } else {
      (node.text_fast(context.program), node_end)
    };
    let node_items = gen_file_text_as_is(node_text, node_text_end, TrimLineEnds::No, context);
    items.extend(inner_gen(node_items, context));
  } else {
    items.extend(inner_gen(gen_node_inner(node, context), context));
  }

  // Get the trailing comments -- This needs to be done based on the parse
  // stack order because certain nodes like binary expressions are flattened
  if node_end != parent_end || context.parent().kind() == NodeKind::Program {
    let trailing_comments = context.comments.trailing_comments_with_previous(node_end);
    items.extend(gen_comments_as_trailing(&node_range, trailing_comments, context));
  }

  let items = if let Some((ln, isol)) = context.take_current_before_comments_start_info() {
    let mut new_items = PrintItems::new(context.arena);
    new_items.push_info(ln);
    new_items.push_info(isol);
    new_items.extend(items);
    new_items
  } else {
    items
  };

  // pop info
  context.current_node = context.parent_stack.pop();

  return items;

  fn gen_node_inner<'a>(node: Node<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
    match node {
      /* class */
      Node::MethodDefinition(node) => gen_class_method(node, context),
      Node::PropertyDefinition(node) => gen_class_prop(node, context),
      Node::AccessorProperty(node) => gen_auto_accessor(node, context),
      Node::Decorator(node) => gen_decorator(node, context),
      Node::PrivateIdentifier(node) => gen_private_name(node, context),
      Node::StaticBlock(node) => gen_static_block(node, context),
      /* clauses */
      Node::CatchClause(node) => gen_catch_clause(node, context),
      Node::CatchParameter(node) => gen_catch_parameter(node, context),
      /* common */
      Node::IdentifierReference(node) => gen_identifier(node, context),
      Node::IdentifierName(node) => gen_ident_name(node, context),
      Node::BindingIdentifier(node) => gen_binding_identifier(node, context),
      Node::LabelIdentifier(node) => gen_label_identifier(node, context),
      /* declarations */
      Node::Class(node) => {
        // swc parsed `export default class` as an expression, which is how it's formatted
        if node.is_declaration() && !is_export_default_decl(node.into_node(), context) {
          gen_class_decl(node, context)
        } else {
          gen_class_expr(node, context)
        }
      }
      Node::ExportDeclaration(node) => gen_export_decl(node, context),
      Node::ExportDefaultDeclaration(node) => gen_export_default_decl(node, context),
      Node::Function(node) => match node.r#type {
        // swc parsed `export default function` as an expression, which is how it's formatted
        FunctionType::FunctionDeclaration if is_export_default_decl(node.into_node(), context) => gen_fn_expr(node, context),
        FunctionType::FunctionDeclaration | FunctionType::TSDeclareFunction => gen_function_decl(node, context),
        FunctionType::FunctionExpression | FunctionType::TSEmptyBodyFunctionExpression => gen_fn_expr(node, context),
      },
      Node::ImportDeclaration(node) => gen_import_decl(node, context),
      Node::ExportNamedDeclaration(node) => gen_export_named_decl(node, context),
      Node::ExportFromDeclaration(node) => gen_export_from_decl(node, context),
      Node::FormalParameter(node) => gen_param(node, context),
      Node::FormalParameterRest(node) => gen_formal_parameter_rest(node, context),
      Node::TSEnumDeclaration(node) => gen_enum_decl(node, context),
      Node::TSEnumMember(node) => gen_enum_member(node, context),
      Node::TSImportEqualsDeclaration(node) => gen_import_equals_decl(node, context),
      Node::TSInterfaceDeclaration(node) => gen_interface_decl(node, context),
      Node::TSExternalModuleDeclaration(node) => gen_external_module_decl(node, context),
      Node::TSNamespaceDeclaration(node) => gen_namespace_decl(node, context),
      Node::TSGlobalDeclaration(node) => gen_global_decl(node, context),
      Node::TSTypeAliasDeclaration(node) => gen_type_alias(node, context),
      /* expressions */
      Node::ArrayExpression(node) => gen_array_expr(node, context),
      Node::ArrowFunctionExpression(node) => gen_arrow_func_expr(node, context),
      Node::AssignmentExpression(node) => gen_assignment_expr(node, context),
      Node::AwaitExpression(node) => gen_await_expr(node, context),
      Node::BinaryExpression(node) => gen_binary_expr(BinExpr::Binary(node), context),
      Node::LogicalExpression(node) => gen_binary_expr(BinExpr::Logical(node), context),
      Node::PrivateInExpression(node) => gen_binary_expr(BinExpr::PrivateIn(node), context),
      Node::CallExpression(node) => gen_call_or_opt_expr(node, context),
      Node::ImportExpression(node) => gen_import_expr(node, context),
      Node::ConditionalExpression(node) => gen_conditional_expr(node, context),
      Node::ObjectProperty(node) => gen_object_property(node, context),
      Node::StaticMemberExpression(_) | Node::ComputedMemberExpression(_) | Node::PrivateFieldExpression(_) => gen_member_expr(node, context),
      Node::ImportMeta(_) | Node::NewTarget(_) => gen_meta_prop_expr(node, context),
      Node::NewExpression(node) => gen_new_expr(node, context),
      Node::ObjectExpression(node) => gen_object_lit(node, context),
      Node::ChainExpression(node) => gen_node((&node.expression).into_node(), context),
      Node::ParenthesizedExpression(node) => gen_paren_expr(node, context),
      Node::SequenceExpression(node) => gen_sequence_expr(node, context),
      Node::SpreadElement(node) => gen_spread_element(node, context),
      Node::Super(_) => "super".into_print_items(context.arena),
      Node::TaggedTemplateExpression(node) => gen_tagged_tpl(node, context),
      Node::ThisExpression(_) => "this".into_print_items(context.arena),
      Node::TemplateLiteral(node) => gen_tpl(node, context),
      Node::TemplateElement(node) => gen_tpl_element(node, context),
      Node::TSAsExpression(node) => gen_as_expr(node, context),
      Node::TSSatisfiesExpression(node) => gen_satisfies_expr(node, context),
      Node::TSClassImplements(node) => gen_class_implements(node, context),
      Node::TSInterfaceHeritage(node) => gen_interface_heritage(node, context),
      Node::TSNonNullExpression(node) => gen_non_null_expr(node, context),
      Node::TSTypeAssertion(node) => gen_type_assertion(node, context),
      Node::TSInstantiationExpression(node) => gen_ts_instantiation(node, context),
      Node::UnaryExpression(node) => gen_unary_expr(node, context),
      Node::UpdateExpression(node) => gen_update_expr(node, context),
      Node::YieldExpression(node) => gen_yield_expr(node, context),
      Node::Elision(_) => PrintItems::new(context.arena),
      /* assignment targets */
      Node::ArrayAssignmentTarget(node) => gen_array_assignment_target(node, context),
      Node::ObjectAssignmentTarget(node) => gen_object_assignment_target(node, context),
      Node::AssignmentTargetWithDefault(node) => gen_assignment_target_with_default(node, context),
      Node::AssignmentTargetPropertyIdentifier(node) => gen_assignment_target_property_identifier(node, context),
      Node::AssignmentTargetPropertyProperty(node) => gen_assignment_target_property_property(node, context),
      Node::AssignmentTargetRest(node) => gen_assignment_target_rest(node, context),
      /* exports */
      Node::ExportSpecifier(node) => gen_export_named_specifier(node, context),
      /* imports */
      Node::ImportSpecifier(node) => gen_import_named_specifier(node, context),
      Node::ImportNamespaceSpecifier(node) => gen_import_namespace_specifier(node, context),
      Node::ImportDefaultSpecifier(node) => gen_node((&node.local).into_node(), context),
      Node::TSExternalModuleReference(node) => gen_external_module_ref(node, context),
      Node::WithClause(node) => gen_with_clause(node, context),
      Node::ImportAttribute(node) => gen_import_attribute(node, context),
      /* interface / type element */
      Node::TSCallSignatureDeclaration(node) => gen_call_signature_decl(node, context),
      Node::TSConstructSignatureDeclaration(node) => gen_construct_signature_decl(node, context),
      Node::TSIndexSignature(node) => gen_index_signature(node, context),
      Node::TSIndexSignatureName(node) => gen_index_signature_name(node, context),
      Node::TSInterfaceBody(node) => gen_interface_body(node, context),
      Node::TSMethodSignature(node) => gen_method_signature(node, context),
      Node::TSPropertySignature(node) => gen_property_signature(node, context),
      Node::TSTypeLiteral(node) => gen_type_lit(node, context),
      /* jsx */
      Node::JSXAttribute(node) => gen_jsx_attribute(node, context),
      Node::JSXSpreadAttribute(node) => gen_jsx_spread_attribute(node, context),
      Node::JSXClosingElement(node) => gen_jsx_closing_element(node, context),
      Node::JSXClosingFragment(node) => gen_jsx_closing_fragment(node, context),
      Node::JSXElement(node) => gen_jsx_element(node, context),
      Node::JSXEmptyExpression(node) => gen_jsx_empty_expr(node, context),
      Node::JSXExpressionContainer(node) => gen_jsx_expr_container(node, context),
      Node::JSXFragment(node) => gen_jsx_fragment(node, context),
      Node::JSXIdentifier(node) => gen_jsx_identifier(node, context),
      Node::JSXMemberExpression(node) => gen_jsx_member_expr(node, context),
      Node::JSXNamespacedName(node) => gen_jsx_namespaced_name(node, context),
      Node::JSXOpeningElement(node) => gen_jsx_opening_element(node, context),
      Node::JSXOpeningFragment(node) => gen_jsx_opening_fragment(node, context),
      Node::JSXSpreadChild(node) => gen_jsx_spread_child(node, context),
      Node::JSXText(node) => gen_jsx_text(node, context),
      /* literals */
      Node::BigIntLiteral(node) => gen_big_int_literal(node, context),
      Node::BooleanLiteral(node) => gen_bool_literal(context.arena, node),
      Node::NullLiteral(_) => "null".into_print_items(context.arena),
      Node::NumericLiteral(node) => gen_num_literal(node, context),
      Node::RegExpLiteral(node) => gen_reg_exp_literal(node, context),
      Node::StringLiteral(node) => gen_string_literal(node, context),
      Node::Directive(node) => gen_directive(node, context),
      /* top level */
      Node::Program(node) => gen_program(node, context),
      /* patterns */
      Node::ArrayPattern(node) => gen_array_pat(node, context),
      Node::AssignmentPattern(node) => gen_assign_pat(node, context),
      Node::BindingProperty(node) => gen_binding_property(node, context),
      Node::BindingRestElement(node) => gen_rest_pat(node, context),
      Node::ObjectPattern(node) => gen_object_pat(node, context),
      /* statements */
      Node::BlockStatement(node) => gen_block_stmt(node, context),
      Node::FunctionBody(node) => gen_function_body(node, context),
      Node::BreakStatement(node) => gen_break_stmt(node, context),
      Node::ContinueStatement(node) => gen_continue_stmt(node, context),
      Node::DebuggerStatement(node) => gen_debugger_stmt(node, context),
      Node::DoWhileStatement(node) => gen_do_while_stmt(node, context),
      Node::ExportAllDeclaration(node) => gen_export_all(node, context),
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
      Node::TSExportAssignment(node) => gen_export_assignment(node, context),
      Node::TSNamespaceExportDeclaration(node) => gen_namespace_export(node, context),
      Node::VariableDeclaration(node) => gen_var_decl(node, context),
      Node::VariableDeclarator(node) => gen_var_declarator(node, context),
      Node::WhileStatement(node) => gen_while_stmt(node, context),
      Node::WithStatement(node) => gen_with_stmt(node, context),
      /* types */
      Node::TSArrayType(node) => gen_array_type(node, context),
      Node::TSConditionalType(node) => gen_conditional_type(node, context),
      Node::TSConstructorType(node) => gen_constructor_type(node, context),
      Node::TSFunctionType(node) => gen_function_type(node, context),
      Node::TSAnyKeyword(_)
      | Node::TSStringKeyword(_)
      | Node::TSBooleanKeyword(_)
      | Node::TSNumberKeyword(_)
      | Node::TSNeverKeyword(_)
      | Node::TSIntrinsicKeyword(_)
      | Node::TSUnknownKeyword(_)
      | Node::TSNullKeyword(_)
      | Node::TSUndefinedKeyword(_)
      | Node::TSVoidKeyword(_)
      | Node::TSSymbolKeyword(_)
      | Node::TSObjectKeyword(_)
      | Node::TSBigIntKeyword(_) => gen_keyword_type(node, context),
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
      Node::TSThisType(_) => "this".into_print_items(context.arena),
      Node::TSThisParameter(node) => gen_ts_this_param(node, context),
      Node::TSTemplateLiteralType(node) => gen_tpl_lit_type(node, context),
      Node::TSTupleType(node) => gen_tuple_type(node, context),
      Node::TSNamedTupleMember(node) => gen_named_tuple_member(node, context),
      Node::TSTypeAnnotation(node) => gen_type_ann(node, context),
      Node::TSTypeParameter(node) => gen_type_param(node, context),
      Node::TSTypeParameterDeclaration(node) => gen_type_parameters(TypeParamNode::Decl(node), context),
      Node::TSTypeParameterInstantiation(node) => gen_type_parameters(TypeParamNode::Instantiation(node), context),
      Node::TSTypeOperator(node) => gen_type_operator(node, context),
      Node::TSTypePredicate(node) => gen_type_predicate(node, context),
      Node::TSTypeQuery(node) => gen_type_query(node, context),
      Node::TSTypeReference(node) => gen_type_reference(node, context),
      Node::TSUnionType(node) => gen_union_type(node, context),
      /* These should never be matched. Return its text if so */
      _ => {
        if cfg!(debug_assertions) {
          panic!("Debug panic! Did not expect to generate IR for node of type {:?}.", node.kind());
        }

        gen_file_text_as_is(node.text_fast(context.program), node.end(), TrimLineEnds::No, context)
      }
    }
  }

  #[inline]
  fn handle_decorators_if_necessary<'a>(node: Node<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
    let mut items = PrintItems::new(context.arena);

    // decorators before `export` (ex. `@dec export class A {}`) have starts before their parent
    // so they need to be handled specially, unlike ones after it (ex. `export @dec class A {}`)
    let decorators = match node {
      Node::ExportDeclaration(decl) => match &decl.declaration {
        Declaration::ClassDeclaration(class_decl) => Some(&class_decl.decorators),
        _ => None,
      },
      Node::ExportDefaultDeclaration(decl) => match &decl.declaration {
        ExportDefaultDeclarationKind::ClassDeclaration(class_decl) => Some(&class_decl.decorators),
        _ => None,
      },
      _ => None,
    };
    if let Some(decorators) = decorators
      && are_decorators_before(decorators, node.start())
    {
      items.extend(gen_decorators(decorators, DecoratorsSeparator::NewLine, context));
    }

    items
  }

  #[inline]
  fn does_first_child_own_leading_comments_on_same_line<'a>(node: Node, context: &mut Context<'a>) -> bool {
    match node {
      Node::TSUnionType(_) | Node::TSIntersectionType(_) => {
        let node_start_line = node.start_line_fast(context.program);
        node
          .leading_comments_fast(context.program)
          .any(|c| c.kind == CommentKind::Block && c.start_line_fast(context.program) == node_start_line)
      }
      _ => false,
    }
  }

  #[cfg(debug_assertions)]
  fn assert_generated_in_order<'a>(node: Node, context: &mut Context<'a>) {
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
      //    are correct. If not, that's a bug in the parser.
      // 3. If it's not a bug in the parser, then check the generation code to
      //    ensure the nodes are being generated in order.
      panic!("Debug panic! Node comments retrieved out of order!");
    }
    context.last_generated_node_pos = node_pos;
  }
}

/// Generates a jsx spread attribute (ex. `{...props}`), which keeps the comments
/// within its braces inside them.
pub(super) fn gen_jsx_spread_attribute<'a>(node: &'a JSXSpreadAttribute<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let dot_dot_dot = node
    .tokens_fast(context.program)
    .iter()
    .find(|t| t.kind == TokenKind::Dot3)
    .expect("Expected to find the spread token of the jsx spread attribute.");
  let inner_range = SourceRange::new(dot_dot_dot.start(), node.argument.end());
  let leading_comments = context.comments.leading_comments_with_previous(inner_range.start);
  if leading_comments
    .clone()
    .any(|c| ir_helpers::text_has_dprint_ignore(c.text, &context.config.ignore_node_comment_text))
  {
    let mut items = gen_comments_as_leading(&inner_range, leading_comments, context);
    items.push_force_current_line_indentation();
    items.extend(gen_file_text_as_is(
      inner_range.text_fast(context.program),
      inner_range.end,
      TrimLineEnds::No,
      context,
    ));
    return gen_as_jsx_expr_container(inner_range, items, context);
  }
  let mut items = gen_comments_as_leading(&inner_range, leading_comments, context);
  items.push_sc(sc!("..."));
  items.extend(gen_node((&node.argument).into_node(), context));
  let trailing_comments = context.comments.trailing_comments_with_previous(inner_range.end);
  items.extend(gen_comments_as_trailing(&inner_range, trailing_comments, context));
  gen_as_jsx_expr_container(inner_range, items, context)
}

fn is_export_default_decl<'a>(node: Node<'a>, context: &Context<'a>) -> bool {
  matches!(node.parent(context.program), Some(Node::ExportDefaultDeclaration(_)))
}

/// Gets the range of a jsx attribute.
pub(super) fn get_jsx_attr_range<'a>(attr: Node<'a>, _context: &Context<'a>) -> SourceRange {
  attr.range()
}

/// Generates text from the file as is. The comments before its end are marked as
/// handled because they're in the text, which would otherwise generate them again.
pub(super) fn gen_file_text_as_is<'a>(text: &'a str, end: SourcePos, trim_line_ends: TrimLineEnds, context: &mut Context<'a>) -> PrintItems<'a> {
  for comment in context.comments.trailing_comments_with_previous(end) {
    if comment.start() < end {
      context.mark_comment_handled(comment);
    }
  }
  match trim_line_ends {
    TrimLineEnds::Yes => ir_helpers::gen_from_raw_string(context.arena, context.alloc_cow_str(get_text_with_trimmed_line_ends(text))),
    TrimLineEnds::No => ir_helpers::gen_from_raw_string(context.arena, text),
  }
}

pub(super) enum TrimLineEnds {
  Yes,
  No,
}

/// Trims the whitespace at the end of each line, which JSX removes.
///
/// The end of the last line is beside the text after it, so it's kept
/// unless the last line is only the whitespace before that text.
fn get_text_with_trimmed_line_ends(text: &str) -> Cow<'_, str> {
  let needs_trimming = {
    let mut lines = text.split('\n').enumerate().peekable();
    let mut needs_trimming = false;
    while let Some((i, line)) = lines.next() {
      let is_trimmed = if lines.peek().is_some() {
        line.trim_end_matches(utils::is_jsx_whitespace_char).len() == line.len()
      } else {
        i == 0 || !utils::is_jsx_whitespace(line)
      };
      if !is_trimmed {
        needs_trimming = true;
        break;
      }
    }
    needs_trimming
  };
  if !needs_trimming {
    return Cow::Borrowed(text);
  }
  let mut result = String::with_capacity(text.len());
  let mut lines = text.split('\n').enumerate().peekable();
  while let Some((i, line)) = lines.next() {
    if lines.peek().is_some() {
      result.push_str(line.trim_end_matches(utils::is_jsx_whitespace_char));
      result.push('\n');
    } else if i == 0 || !utils::is_jsx_whitespace(line) {
      result.push_str(line);
    }
  }
  Cow::Owned(result)
}

pub(super) fn get_has_ignore_comment<'a>(leading_comments: &CommentsIterator<'a>, node: Node<'a>, context: &Context<'a>) -> bool {
  let comments = match node.parent(context.program) {
    Some(Node::JSXElement(jsx_element)) => get_comments_for_jsx_children(&jsx_element.children, &node.start(), context),
    Some(Node::JSXFragment(jsx_fragment)) => get_comments_for_jsx_children(&jsx_fragment.children, &node.start(), context),
    _ => leading_comments.clone(),
  };

  for comment in comments {
    if ir_helpers::text_has_dprint_ignore(comment.text, &context.config.ignore_node_comment_text) {
      return true;
    }
  }

  return false;

  fn get_comments_for_jsx_children<'a>(children: &'a [JSXChild<'a>], node_lo: &SourcePos, context: &Context<'a>) -> CommentsIterator<'a> {
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
            JSXExpression::EmptyExpression(empty_expr) => {
              iterator.extend(get_jsx_empty_expr_comments(empty_expr, context));
            }
            _ => break,
          };
        }
        JSXChild::Text(jsx_text) => {
          if !utils::is_jsx_whitespace(jsx_text.text_fast(context.program)) {
            break;
          }
        }
        _ => break,
      }
    }

    iterator
  }
}

pub(super) fn is_ignore_jsx_expr_container<'a>(node: Node<'a>, context: &Context<'a>) -> bool {
  if let Node::JSXExpressionContainer(expr_container) = node
    && let JSXExpression::EmptyExpression(empty_expr) = &expr_container.expression
  {
    for comment in get_jsx_empty_expr_comments(empty_expr, context) {
      if ir_helpers::text_has_dprint_ignore(comment.text, &context.config.ignore_node_comment_text) {
        return true;
      }
    }
  }

  false
}

/// Gets if the node is JSX text after an ignore comment, which is generated
/// as-is along with its leading whitespace and the spaces at its end.
pub(super) fn is_jsx_text_generated_as_is<'a>(node: Node<'a>, context: &Context<'a>) -> bool {
  node.kind() == NodeKind::JSXText
    && !utils::is_jsx_whitespace(node.text_fast(context.program))
    && get_has_ignore_comment(&CommentsIterator::empty(), node, context)
}

/// Generates an assignment target (ex. the left side of an assignment), which
/// keeps its parens when it's a type assertion (ex. `(x as T) = 1`) since the
/// code would otherwise be invalid. oxc doesn't keep these parens in the ast.
pub(super) fn gen_assignment_target<'a>(target: Node<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let is_type_assertion = matches!(target, Node::TSAsExpression(_) | Node::TSSatisfiesExpression(_) | Node::TSTypeAssertion(_));
  if !is_type_assertion || !has_surrounding_paren_tokens(target, context) {
    return gen_node(target, context);
  }
  let generated_items = conditions::with_indent_if_start_of_line_indented(gen_node_in_parens(
    |context| gen_node(target, context),
    GenNodeInParensOptions {
      inner_range: target.range(),
      prefer_hanging: true,
      allow_open_paren_trailing_comments: true,
      single_line_space_around: context.config.paren_expression_space_around,
    },
    context,
  ))
  .into_print_items(context.arena);
  new_line_group(generated_items)
}

fn has_surrounding_paren_tokens<'a>(node: Node<'a>, context: &Context<'a>) -> bool {
  context.token_finder.get_previous_token_if_open_paren(&node).is_some() && context.token_finder.get_next_token_if_close_paren(&node).is_some()
}

/// Gets the members of a class body along with an empty statement for each of
/// its semicolons that isn't part of a member (ex. `class A { ; }`), which swc
/// had as empty members, so that the comments around them are handled the same.
pub(super) fn get_class_members<'a>(body: &'a ClassBody<'a>, context: &Context<'a>) -> Vec<Node<'a>> {
  let mut members = body.body.iter().map(|m| m.into_node()).collect::<Vec<_>>();
  let tokens = body.tokens_fast(context.program);
  let mut empty_members = Vec::new();
  let mut member_index = 0;
  for token in tokens.iter().filter(|t| t.kind == TokenKind::Semicolon) {
    while member_index < members.len() && members[member_index].end() <= token.start() {
      member_index += 1;
    }
    let is_within_member = members.get(member_index).is_some_and(|m| m.start() <= token.start());
    if !is_within_member {
      // this isn't a node in the ast, so it has no parent
      let mut empty_stmt = <EmptyStatement as oxc_allocator::Dummy>::dummy(&oxc_allocator::Allocator::default());
      empty_stmt.node_id.set(oxc_syntax::node::NodeId::new(u32::MAX as usize - 1));
      empty_stmt.span = oxc_span::Span::new(token.start().as_u32(), token.end().as_u32());
      let empty_stmt = context.arena.bump().alloc(empty_stmt);
      empty_members.push((&*empty_stmt).into_node());
    }
  }
  if !empty_members.is_empty() {
    members.extend(empty_members);
    members.sort_by_key(|m| m.start());
  }
  members
}
