use std::rc::Rc;
use dprint_core::formatting::*;
use dprint_core::formatting::{parser_helpers::*,condition_resolvers, conditions::*};
use swc_ast_view::*;

use crate::configuration::*;
use crate::swc::ParsedSourceFile;
use crate::utils;
use super::sorting::*;
use super::swc::*;
use super::swc::{get_flattened_bin_expr};
use super::*;

pub fn parse<'a>(source_file: &'a ParsedSourceFile, config: &Configuration) -> PrintItems {
    let source_file_info = swc_ast_view::ModuleInfo {
        module: &source_file.module,
        source_file: Some(&source_file.info),
        tokens: Some(&source_file.tokens),
        comments: Some(&source_file.comments),
    };

    swc_ast_view::with_ast_view_for_module(source_file_info, |module| {
        let module_node = Node::Module(module);
        let mut context = Context::new(
            config,
            &source_file.tokens,
            module_node,
            &source_file.info,
            module,
        );
        let mut items = parse_node(module_node, &mut context);
        items.push_condition(if_true(
            "endOfFileNewLine",
            |context| Some(context.writer_info.column_number > 0 || context.writer_info.line_number > 0),
            Signal::NewLine.into()
        ));

        #[cfg(debug_assertions)]
        context.assert_end_of_file_state();

        items
    })
}

fn parse_node<'a>(node: Node<'a>, context: &mut Context<'a>) -> PrintItems {
    parse_node_with_inner_parse(node, context, |items, _| items)
}

fn parse_node_with_inner_parse<'a>(node: Node<'a>, context: &mut Context<'a>, inner_parse: impl FnOnce(PrintItems, &mut Context<'a>) -> PrintItems) -> PrintItems {
    let node_kind = node.kind();
    // println!("Node kind: {:?}", node_kind);
    // println!("Text: {:?}", node.text());

    // store info
    let past_current_node = std::mem::replace(&mut context.current_node, node);
    let parent_hi = past_current_node.hi();
    context.parent_stack.push(past_current_node);

    // handle decorators (since their starts can come before their parent)
    let mut items = handle_decorators_if_necessary(&node, context);

    // now that decorators might have been parsed, assert the node order to ensure comments are parsed correctly
    #[cfg(debug_assertions)]
    assert_parsed_in_order(&node, context);

    // parse item
    let node_span = node.span();
    let node_hi = node_span.hi;
    let node_lo = node_span.lo;
    let mut has_ignore_comment = false;

    // do not get the comments for modules as this will be handled in parse_statements
    if node_kind != NodeKind::Module {
        // get the leading comments
        if get_first_child_owns_leading_comments_on_same_line(&node, context) {
            // Some block comments should belong to the first child rather than the
            // parent node because their first child may end up on the next line.
            let leading_comments = node_lo.leading_comments_fast(context.module);
            has_ignore_comment = get_has_ignore_comment(&leading_comments, &node, context);
            let node_start_line = node.start_line_fast(context.module);
            let leading_comments_on_previous_lines = leading_comments
                .take_while(|c| c.kind == CommentKind::Line || c.start_line_fast(context.module) < node_start_line)
                .collect::<Vec<&'a Comment>>();
            items.extend(parse_comment_collection(leading_comments_on_previous_lines.into_iter(), None, None, context));
        } else {
            let leading_comments = context.comments.leading_comments_with_previous(node_lo);
            has_ignore_comment = get_has_ignore_comment(&leading_comments, &node, context);
            items.extend(parse_comments_as_leading(&node_span, leading_comments, context));
        }
    }

    // parse the node
    if has_ignore_comment {
        items.push_str(""); // force the current line indentation
        items.extend(inner_parse(parser_helpers::parse_raw_string(node.text_fast(context.module)), context));

        // mark any previous comments as handled
        for comment in context.comments.trailing_comments_with_previous(node_hi) {
            if comment.lo() < node_hi {
                context.mark_comment_handled(comment);
            }
        }
    } else {
        items.extend(inner_parse(parse_node_inner(node, context), context));
    }

    // Get the trailing comments -- This needs to be done based on the parse
    // stack order because certain nodes like binary expressions are flattened
    if node_hi != parent_hi || context.parent().kind() == NodeKind::Module {
        let trailing_comments = context.comments.trailing_comments_with_previous(node_hi);
        items.extend(parse_comments_as_trailing(&node_span, trailing_comments, context));
    }

    let items = if let Some(info) = context.take_current_before_comments_start_info() {
        let mut new_items = PrintItems::new();
        new_items.push_info(info);
        new_items.extend(items);
        new_items
    } else {
        items
    };

    // pop info
    context.current_node = context.parent_stack.pop();

    // need to ensure a jsx spread element's comments are parsed within the braces since swc
    // has no representation of a JSX spread attribute and goes straight to the spread element
    return if node_kind == NodeKind::SpreadElement && node.parent().unwrap().kind() == NodeKind::JSXOpeningElement {
        parse_as_jsx_expr_container(node, items, context)
    } else  {
        items
    };

    fn parse_node_inner<'a>(node: Node<'a>, context: &mut Context<'a>) -> PrintItems {
        match node {
            /* class */
            Node::ClassMethod(node) => parse_class_method(node, context),
            Node::ClassProp(node) => parse_class_prop(node, context),
            Node::Constructor(node) => parse_constructor(node, context),
            Node::Decorator(node) => parse_decorator(node, context),
            Node::TsParamProp(node) => parse_parameter_prop(node, context),
            Node::PrivateMethod(node) => parse_private_method(node, context),
            Node::PrivateName(node) => parse_private_name(node, context),
            Node::PrivateProp(node) => parse_private_prop(node, context),
            /* clauses */
            Node::CatchClause(node) => parse_catch_clause(node, context),
            /* common */
            Node::ComputedPropName(node) => parse_computed_prop_name(node, context),
            Node::Ident(node) => parse_identifier(node, context),
            Node::BindingIdent(node) => parse_binding_identifier(node, context),
            /* declarations */
            Node::ClassDecl(node) => parse_class_decl(node, context),
            Node::ExportDecl(node) => parse_export_decl(node, context),
            Node::ExportDefaultDecl(node) => parse_export_default_decl(node, context),
            Node::ExportDefaultExpr(node) => parse_export_default_expr(node, context),
            Node::ExportDefaultSpecifier(node) => parse_export_default_specifier(node, context),
            Node::FnDecl(node) => parse_function_decl(node, context),
            Node::ImportDecl(node) => parse_import_decl(node, context),
            Node::NamedExport(node) => parse_export_named_decl(node, context),
            Node::Param(node) => parse_param(node, context),
            Node::TsEnumDecl(node) => parse_enum_decl(node, context),
            Node::TsEnumMember(node) => parse_enum_member(node, context),
            Node::TsImportEqualsDecl(node) => parse_import_equals_decl(node, context),
            Node::TsInterfaceDecl(node) => parse_interface_decl(node, context),
            Node::TsModuleDecl(node) => parse_module_decl(node, context),
            Node::TsNamespaceDecl(node) => parse_namespace_decl(node, context),
            Node::TsTypeAliasDecl(node) => parse_type_alias(node, context),
            /* expressions */
            Node::ArrayLit(node) => parse_array_expr(node, context),
            Node::ArrowExpr(node) => parse_arrow_func_expr(node, context),
            Node::AssignExpr(node) => parse_assignment_expr(node, context),
            Node::AwaitExpr(node) => parse_await_expr(node, context),
            Node::BinExpr(node) => parse_binary_expr(node, context),
            Node::CallExpr(node) => parse_call_expr(node, context),
            Node::ClassExpr(node) => parse_class_expr(node, context),
            Node::CondExpr(node) => parse_conditional_expr(node, context),
            Node::ExprOrSpread(node) => parse_expr_or_spread(node, context),
            Node::FnExpr(node) => parse_fn_expr(node, context),
            Node::GetterProp(node) => parse_getter_prop(node, context),
            Node::KeyValueProp(node) => parse_key_value_prop(node, context),
            Node::AssignProp(node) => parse_assign_prop(node, context),
            Node::MemberExpr(node) => parse_member_expr(node, context),
            Node::MetaPropExpr(node) => parse_meta_prop_expr(node, context),
            Node::NewExpr(node) => parse_new_expr(node, context),
            Node::ObjectLit(node) => parse_object_lit(node, context),
            Node::OptChainExpr(node) => parse_node(node.expr.into(), context),
            Node::ParenExpr(node) => parse_paren_expr(node, context),
            Node::SeqExpr(node) => parse_sequence_expr(node, context),
            Node::SetterProp(node) => parse_setter_prop(node, context),
            Node::SpreadElement(node) => parse_spread_element(node, context),
            Node::Super(_) => "super".into(),
            Node::TaggedTpl(node) => parse_tagged_tpl(node, context),
            Node::ThisExpr(_) => "this".into(),
            Node::Tpl(node) => parse_tpl(node, context),
            Node::TplElement(node) => parse_tpl_element(node, context),
            Node::TsAsExpr(node) => parse_as_expr(node, context),
            Node::TsConstAssertion(node) => parse_const_assertion(node, context),
            Node::TsExprWithTypeArgs(node) => parse_expr_with_type_args(node, context),
            Node::TsNonNullExpr(node) => parse_non_null_expr(node, context),
            Node::TsTypeAssertion(node) => parse_type_assertion(node, context),
            Node::UnaryExpr(node) => parse_unary_expr(node, context),
            Node::UpdateExpr(node) => parse_update_expr(node, context),
            Node::YieldExpr(node) => parse_yield_expr(node, context),
            /* exports */
            Node::ExportNamedSpecifier(node) => parse_export_named_specifier(node, context),
            Node::ExportNamespaceSpecifier(node) => parse_namespace_export_specifier(node, context),
            /* imports */
            Node::ImportNamedSpecifier(node) => parse_import_named_specifier(node, context),
            Node::ImportStarAsSpecifier(node) => parse_import_namespace_specifier(node, context),
            Node::ImportDefaultSpecifier(node) => parse_node(node.local.into(), context),
            Node::TsExternalModuleRef(node) => parse_external_module_ref(node, context),
            /* interface / type element */
            Node::TsCallSignatureDecl(node) => parse_call_signature_decl(node, context),
            Node::TsConstructSignatureDecl(node) => parse_construct_signature_decl(node, context),
            Node::TsIndexSignature(node) => parse_index_signature(node, context),
            Node::TsInterfaceBody(node) => parse_interface_body(node, context),
            Node::TsMethodSignature(node) => parse_method_signature(node, context),
            Node::TsPropertySignature(node) => parse_property_signature(node, context),
            Node::TsTypeLit(node) => parse_type_lit(node, context),
            /* jsx */
            Node::JSXAttr(node) => parse_jsx_attribute(node, context),
            Node::JSXClosingElement(node) => parse_jsx_closing_element(node, context),
            Node::JSXClosingFragment(node) => parse_jsx_closing_fragment(node, context),
            Node::JSXElement(node) => parse_jsx_element(node, context),
            Node::JSXEmptyExpr(node) => parse_jsx_empty_expr(node, context),
            Node::JSXExprContainer(node) => parse_jsx_expr_container(node, context),
            Node::JSXFragment(node) => parse_jsx_fragment(node, context),
            Node::JSXMemberExpr(node) => parse_jsx_member_expr(node, context),
            Node::JSXNamespacedName(node) => parse_jsx_namespaced_name(node, context),
            Node::JSXOpeningElement(node) => parse_jsx_opening_element(node, context),
            Node::JSXOpeningFragment(node) => parse_jsx_opening_fragment(node, context),
            Node::JSXSpreadChild(node) => parse_jsx_spread_child(node, context),
            Node::JSXText(node) => parse_jsx_text(node, context),
            /* literals */
            Node::BigInt(node) => parse_big_int_literal(node, context),
            Node::Bool(node) => parse_bool_literal(node),
            Node::Null(_) => "null".into(),
            Node::Number(node) => parse_num_literal(node, context),
            Node::Regex(node) => parse_reg_exp_literal(node, context),
            Node::Str(node) => parse_string_literal(node, context),
            /* module */
            Node::Module(node) => parse_module(node, context),
            /* patterns */
            Node::ArrayPat(node) => parse_array_pat(node, context),
            Node::AssignPat(node) => parse_assign_pat(node, context),
            Node::AssignPatProp(node) => parse_assign_pat_prop(node, context),
            Node::KeyValuePatProp(node) => parse_key_value_pat_prop(node, context),
            Node::RestPat(node) => parse_rest_pat(node, context),
            Node::ObjectPat(node) => parse_object_pat(node, context),
            /* properties */
            Node::MethodProp(node) => parse_method_prop(node, context),
            /* statements */
            Node::BlockStmt(node) => parse_block_stmt(node, context),
            Node::BreakStmt(node) => parse_break_stmt(node, context),
            Node::ContinueStmt(node) => parse_continue_stmt(node, context),
            Node::DebuggerStmt(node) => parse_debugger_stmt(node, context),
            Node::DoWhileStmt(node) => parse_do_while_stmt(node, context),
            Node::ExportAll(node) => parse_export_all(node, context),
            Node::ExprStmt(node) => parse_expr_stmt(node, context),
            Node::EmptyStmt(node) => parse_empty_stmt(node, context),
            Node::ForInStmt(node) => parse_for_in_stmt(node, context),
            Node::ForOfStmt(node) => parse_for_of_stmt(node, context),
            Node::ForStmt(node) => parse_for_stmt(node, context),
            Node::IfStmt(node) => parse_if_stmt(node, context),
            Node::LabeledStmt(node) => parse_labeled_stmt(node, context),
            Node::ReturnStmt(node) => parse_return_stmt(node, context),
            Node::SwitchStmt(node) => parse_switch_stmt(node, context),
            Node::SwitchCase(node) => parse_switch_case(node, context),
            Node::ThrowStmt(node) => parse_throw_stmt(node, context),
            Node::TryStmt(node) => parse_try_stmt(node, context),
            Node::TsExportAssignment(node) => parse_export_assignment(node, context),
            Node::TsNamespaceExportDecl(node) => parse_namespace_export(node, context),
            Node::VarDecl(node) => parse_var_decl(node, context),
            Node::VarDeclarator(node) => parse_var_declarator(node, context),
            Node::WhileStmt(node) => parse_while_stmt(node, context),
            /* types */
            Node::TsArrayType(node) => parse_array_type(node, context),
            Node::TsConditionalType(node) => parse_conditional_type(node, context),
            Node::TsConstructorType(node) => parse_constructor_type(node, context),
            Node::TsFnType(node) => parse_function_type(node, context),
            Node::TsKeywordType(node) => parse_keyword_type(node, context),
            Node::TsImportType(node) => parse_import_type(node, context),
            Node::TsIndexedAccessType(node) => parse_indexed_access_type(node, context),
            Node::TsInferType(node) => parse_infer_type(node, context),
            Node::TsIntersectionType(node) => parse_intersection_type(node, context),
            Node::TsLitType(node) => parse_lit_type(node, context),
            Node::TsMappedType(node) => parse_mapped_type(node, context),
            Node::TsOptionalType(node) => parse_optional_type(node, context),
            Node::TsQualifiedName(node) => parse_qualified_name(node, context),
            Node::TsParenthesizedType(node) => parse_parenthesized_type(node, context),
            Node::TsRestType(node) => parse_rest_type(node, context),
            Node::TsThisType(_) => "this".into(),
            Node::TsTplLitType(node) => parse_tpl_lit_type(node, context),
            Node::TsTupleType(node) => parse_tuple_type(node, context),
            Node::TsTupleElement(node) => parse_tuple_element(node, context),
            Node::TsTypeAnn(node) => parse_type_ann(node, context),
            Node::TsTypeParam(node) => parse_type_param(node, context),
            Node::TsTypeParamDecl(node) => parse_type_parameters(TypeParamNode::Decl(node), context),
            Node::TsTypeParamInstantiation(node) => parse_type_parameters(TypeParamNode::Instantiation(node), context),
            Node::TsTypeOperator(node) => parse_type_operator(node, context),
            Node::TsTypePredicate(node) => parse_type_predicate(node, context),
            Node::TsTypeQuery(node) => parse_type_query(node, context),
            Node::TsTypeRef(node) => parse_type_reference(node, context),
            Node::TsUnionType(node) => parse_union_type(node, context),
            /* These should never be matched. Return its text if so */
            Node::Class(_) | Node::Function(_) | Node::Invalid(_) | Node::Script(_) | Node::WithStmt(_) | Node::TsModuleBlock(_) => {
                if cfg!(debug_assertions) {
                    panic!("Debug panic! Did not expect to parse node of type {}.", node.kind());
                }

                parse_raw_string(node.text_fast(context.module).into())
            },
        }
    }

    #[inline]
    fn handle_decorators_if_necessary<'a>(node: &Node<'a>, context: &mut Context<'a>) -> PrintItems {
        let mut items = PrintItems::new();

        // decorators in these cases will have starts before their parent so they need to be handled specially
        if let Node::ExportDecl(decl) = node {
            if let Decl::Class(class_decl) = &decl.decl {
                items.extend(parse_decorators(&class_decl.class.decorators, false, context));
            }
        } else if let Node::ExportDefaultDecl(decl) = node {
            if let DefaultDecl::Class(class_expr) = &decl.decl {
                items.extend(parse_decorators(&class_expr.class.decorators, false, context));
            }
        }

        return items;
    }

    #[inline]
    fn get_first_child_owns_leading_comments_on_same_line(node: &Node, context: &mut Context) -> bool {
        match node {
            Node::TsUnionType(_) | Node::TsIntersectionType(_) => {
                let node_start_line = node.start_line_fast(context.module);
                node.leading_comments_fast(context.module)
                    .filter(|c| c.kind == CommentKind::Block && c.start_line_fast(context.module) == node_start_line)
                    .next().is_some()
            },
            _ => false,
        }
    }

    #[cfg(debug_assertions)]
    fn assert_parsed_in_order(node: &Node, context: &mut Context) {
        let node_pos = node.lo().0;
        if context.last_parsed_node_pos > node_pos {
            // When this panic happens it means that a node with a start further
            // along in the file has been "parsed" before this current node. When
            // that occurs, comments that this node "owns" might have been shifted
            // over to the further along node since "forgotten" comments get
            // prepended when a node is being parsed.
            //
            // Do the following steps to solve:
            //
            // 1. Uncomment the lines in `parse_node_with_inner_parse` in order to
            //    display the node kinds.
            // 2. Add a test that reproduces the issue then run the tests and see
            //    where it panics and how that node looks. Ensure the node widths
            //    are correct. If not, that's a bug in swc, so go fix it in swc.
            // 3. If it's not a bug in swc, then check the parsing code to ensure
            //    the nodes are being parsed in order.
            panic!("Debug panic! Node comments retrieved out of order!");
        }
        context.last_parsed_node_pos = node_pos;
    }
}

fn get_has_ignore_comment<'a>(leading_comments: &CommentsIterator<'a>, node: &Node<'a>, context: &mut Context<'a>) -> bool {
    return if let Some(last_comment) = get_last_comment(leading_comments, node, context) {
        parser_helpers::text_has_dprint_ignore(&last_comment.text, &context.config.ignore_node_comment_text)
    } else {
        false
    };

    #[inline]
    fn get_last_comment<'a>(leading_comments: &CommentsIterator<'a>, node: &Node, context: &mut Context<'a>) -> Option<&'a Comment> {
        return match node.parent() {
            Some(Node::JSXElement(jsx_element)) => get_last_comment_for_jsx_children(&jsx_element.children, &node.lo(), context),
            Some(Node::JSXFragment(jsx_fragment)) => get_last_comment_for_jsx_children(&jsx_fragment.children, &node.lo(), context),
            _ => leading_comments.peek_last_comment(),
        };

        fn get_last_comment_for_jsx_children<'a>(children: &Vec<JSXElementChild>, node_lo: &BytePos, context: &mut Context<'a>) -> Option<&'a Comment> {
            let index = children.binary_search_by_key(node_lo, |child| child.lo()).ok()?;
            for i in (0..index).rev() {
                match children.get(i)? {
                    JSXElementChild::JSXExprContainer(expr_container) => {
                        return match expr_container.expr {
                            JSXExpr::JSXEmptyExpr(empty_expr) => {
                                get_jsx_empty_expr_comments(&empty_expr, context).last()
                            },
                            _ => None,
                        };
                    },
                    JSXElementChild::JSXText(jsx_text) => {
                        if !jsx_text.text_fast(context.module).trim().is_empty() { return None; }
                    }
                    _=> return None,
                }
            }

            None
        }
    }
}

/* class */

fn parse_class_method<'a>(node: &'a ClassMethod, context: &mut Context<'a>) -> PrintItems {
    // todo: consolidate with private method
    return parse_class_or_object_method(ClassOrObjectMethod {
        node: node.into(),
        parameters_span: node.get_parameters_span(context),
        decorators: Some(&node.function.decorators),
        accessibility: node.accessibility(),
        is_static: node.is_static(),
        is_async: node.function.is_async(),
        is_abstract: node.is_abstract(),
        kind: node.kind().into(),
        is_generator: node.function.is_generator(),
        is_optional: node.is_optional(),
        key: node.key.into(),
        type_params: node.function.type_params.map(|x| x.into()),
        params: node.function.params.iter().map(|&x| x.into()).collect(),
        return_type: node.function.return_type.map(|x| x.into()),
        body: node.function.body.map(|x| x.into()),
    }, context);
}

fn parse_private_method<'a>(node: &'a PrivateMethod, context: &mut Context<'a>) -> PrintItems {
    return parse_class_or_object_method(ClassOrObjectMethod {
        node: node.into(),
        parameters_span: node.get_parameters_span(context),
        decorators: Some(&node.function.decorators),
        accessibility: node.accessibility(),
        is_static: node.is_static(),
        is_async: node.function.is_async(),
        is_abstract: node.is_abstract(),
        kind: node.kind().into(),
        is_generator: node.function.is_generator(),
        is_optional: node.is_optional(),
        key: node.key.into(),
        type_params: node.function.type_params.map(|x| x.into()),
        params: node.function.params.iter().map(|&x| x.into()).collect(),
        return_type: node.function.return_type.map(|x| x.into()),
        body: node.function.body.map(|x| x.into()),
    }, context);
}

fn parse_class_prop<'a>(node: &'a ClassProp, context: &mut Context<'a>) -> PrintItems {
    parse_class_prop_common(ParseClassPropCommon {
        key: node.key.into(),
        value: &node.value,
        type_ann: &node.type_ann,
        is_static: node.is_static(),
        decorators: &node.decorators,
        computed: node.computed(),
        is_declare: node.declare(),
        accessibility: node.accessibility(),
        is_abstract: node.is_abstract(),
        is_optional: node.is_optional(),
        readonly: node.readonly(),
        definite: node.definite(),
    }, context)
}

fn parse_constructor<'a>(node: &'a Constructor, context: &mut Context<'a>) -> PrintItems {
    parse_class_or_object_method(ClassOrObjectMethod {
        node: node.into(),
        parameters_span: node.get_parameters_span(context),
        decorators: None,
        accessibility: node.accessibility(),
        is_static: false,
        is_async: false,
        is_abstract: false,
        kind: ClassOrObjectMethodKind::Constructor,
        is_generator: false,
        is_optional: node.is_optional(),
        key: node.key.into(),
        type_params: None,
        params: node.params.iter().map(|x| x.into()).collect(),
        return_type: None,
        body: node.body.map(|x| x.into()),
    }, context)
}

fn parse_decorator<'a>(node: &'a Decorator, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    items.push_str("@");
    items.extend(parse_node(node.expr.into(), context));
    return items;
}

fn parse_parameter_prop<'a>(node: &'a TsParamProp, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    items.extend(parse_decorators(&node.decorators, true, context));
    if let Some(accessibility) = node.accessibility() {
        items.push_str(&format!("{} ", accessibility_to_str(accessibility)));
    }
    if node.readonly() { items.push_str("readonly "); }
    items.extend(parse_node(node.param.into(), context));
    return items;
}

fn parse_private_name<'a>(node: &'a PrivateName, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    items.push_str("#");
    items.extend(parse_node(node.id.into(), context));
    items
}

fn parse_private_prop<'a>(node: &'a PrivateProp, context: &mut Context<'a>) -> PrintItems {
    parse_class_prop_common(ParseClassPropCommon {
        key: node.key.into(),
        value: &node.value,
        type_ann: &node.type_ann,
        is_static: node.is_static(),
        decorators: &node.decorators,
        computed: node.computed(),
        is_declare: false,
        accessibility: node.accessibility(),
        is_abstract: node.is_abstract(),
        is_optional: node.is_optional(),
        readonly: node.readonly(),
        definite: node.definite(),
    }, context)
}

struct ParseClassPropCommon<'a> {
    pub key: Node<'a>,
    pub value: &'a Option<Expr<'a>>,
    pub type_ann: &'a Option<&'a TsTypeAnn<'a>>,
    pub is_static: bool,
    pub decorators: &'a Vec<&'a Decorator<'a>>,
    pub computed: bool,
    pub is_declare: bool,
    pub accessibility: Option<Accessibility>,
    pub is_abstract: bool,
    pub is_optional: bool,
    pub readonly: bool,
    pub definite: bool,
}

fn parse_class_prop_common<'a>(node: ParseClassPropCommon<'a>, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    items.extend(parse_decorators(node.decorators, false, context));
    if node.is_declare { items.push_str("declare "); }
    if let Some(accessibility) = node.accessibility {
        items.push_str(&format!("{} ", accessibility_to_str(accessibility)));
    }
    if node.is_static { items.push_str("static "); }
    if node.is_abstract { items.push_str("abstract "); }
    if node.readonly { items.push_str("readonly "); }
    let key_span = node.key.span();
    items.extend(if node.computed {
        parse_computed_prop_like(|context| parse_node(node.key, context), ParseComputedPropLikeOptions {
            inner_node_span: key_span,
        }, context)
    } else {
        parse_node(node.key, context)
    });
    if node.is_optional { items.push_str("?"); }
    if node.definite { items.push_str("!"); }
    items.extend(parse_type_ann_with_colon_if_exists(node.type_ann, context));

    if let Some(value) = node.value {
        items.extend(parse_assignment(value.into(), "=", context));
    }

    if context.config.semi_colons.is_true() {
        items.push_str(";");
    }

    return items;
}

/* clauses */

fn parse_catch_clause<'a>(node: &'a CatchClause, context: &mut Context<'a>) -> PrintItems {
    // a bit overkill since the param will currently always just be an identifer
    let start_header_info = Info::new("catchClauseHeaderStart");
    let end_header_info = Info::new("catchClauseHeaderEnd");
    let mut items = PrintItems::new();

    items.push_info(start_header_info);
    items.push_str("catch");

    if let Some(param) = &node.param {
        items.push_str(" (");
        items.extend(parse_node(param.into(), context));
        items.push_str(")");
    }
    items.push_info(end_header_info);

    let single_body_position = if let Node::TryStmt(try_stmt) = node.parent().unwrap() {
        if try_stmt.finalizer.is_some() { Some(SingleBodyPosition::NextLine) } else { None }
    } else {
        None
    };

    // not conditional... required
    items.extend(parse_conditional_brace_body(ParseConditionalBraceBodyOptions {
        parent: node.span(),
        body_node: node.body.into(),
        use_braces: UseBraces::Always,
        brace_position: context.config.try_statement_brace_position,
        single_body_position,
        requires_braces_condition_ref: None,
        header_start_token: None,
        start_header_info: Some(start_header_info),
        end_header_info: Some(end_header_info),
    }, context).parsed_node);

    return items;
}

/* common */

fn parse_computed_prop_name<'a>(node: &'a ComputedPropName, context: &mut Context<'a>) -> PrintItems {
    parse_computed_prop_like(|context| parse_node(node.expr.into(), context), ParseComputedPropLikeOptions {
        inner_node_span: node.expr.span(),
    }, context)
}

fn parse_identifier<'a>(node: &'a Ident, _: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    items.push_str(node.sym() as &str);

    if node.optional() && !node.parent().unwrap().is::<ClassProp>() && !node.parent().unwrap().is::<ClassMethod>() {
        items.push_str("?");
    }

    return items;
}

fn parse_binding_identifier<'a>(node: &'a BindingIdent, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    items.extend(parse_node(node.id.into(), context));

    if let Node::VarDeclarator(node) = node.parent().unwrap() {
        if node.definite() {
            items.push_str("!");
        }
    }

    items.extend(parse_type_ann_with_colon_if_exists(&node.type_ann, context));

    return items;
}

/* declarations */

fn parse_class_decl<'a>(node: &'a ClassDecl, context: &mut Context<'a>) -> PrintItems {
    return parse_class_decl_or_expr(ClassDeclOrExpr {
        node: node.into(),
        member_node: node.class.into(),
        decorators: &node.class.decorators,
        is_class_expr: false,
        is_declare: node.declare(),
        is_abstract: node.class.is_abstract(),
        ident: Some(node.ident.into()),
        type_params: node.class.type_params.map(|x| x.into()),
        super_class: node.class.super_class.map(|x| x.into()),
        super_type_params: node.class.super_type_params.map(|x| x.into()),
        implements: node.class.implements.iter().map(|&x| x.into()).collect(),
        members: node.class.body.iter().map(|x| x.into()).collect(),
        brace_position: context.config.class_declaration_brace_position,
    }, context);
}

struct ClassDeclOrExpr<'a> {
    node: Node<'a>,
    member_node: Node<'a>,
    decorators: &'a Vec<&'a Decorator<'a>>,
    is_class_expr: bool,
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

fn parse_class_decl_or_expr<'a>(node: ClassDeclOrExpr<'a>, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    let start_before_owned_comments_info = context.get_or_create_current_before_comments_start_info();

    // parse decorators
    let parent_kind = node.node.parent().unwrap().kind();
    if parent_kind != NodeKind::ExportDecl && parent_kind != NodeKind::ExportDefaultDecl {
        items.extend(parse_decorators(node.decorators, node.is_class_expr, context));
    }

    // parse header and body
    let start_header_info = Info::new("startHeader");
    items.push_info(start_header_info);

    if node.is_declare { items.push_str("declare "); }
    if node.is_abstract { items.push_str("abstract "); }

    items.push_str("class");

    if let Some(ident) = node.ident {
        items.push_str(" ");
        items.extend(parse_node(ident, context));
    }
    if let Some(type_params) = node.type_params {
        items.extend(parse_node(type_params, context));
    }
    if let Some(super_class) = node.super_class {
        items.push_condition(conditions::new_line_if_hanging_space_otherwise(conditions::NewLineIfHangingSpaceOtherwiseOptions {
            start_info: start_header_info,
            end_info: None,
            space_char: Some(conditions::if_above_width_or(context.config.indent_width, Signal::SpaceOrNewLine.into(), " ".into()).into()),
        }));
        items.push_condition(conditions::indent_if_start_of_line({
            let mut items = PrintItems::new();
            items.push_str("extends ");
            items.extend(new_line_group({
                let mut items = PrintItems::new();
                items.extend(parse_node(super_class, context));
                if let Some(super_type_params) = node.super_type_params {
                    items.extend(parse_node(super_type_params, context));
                }
                items
            }));
            items
        }));
    }
    items.extend(parse_extends_or_implements(ParseExtendsOrImplementsOptions {
        text: "implements",
        type_items: node.implements,
        start_header_info,
        prefer_hanging: context.config.implements_clause_prefer_hanging,
    }, context));
    items.extend(parse_membered_body(ParseMemberedBodyOptions {
        node: node.member_node,
        members: node.members,
        start_header_info: Some(start_header_info),
        brace_position: node.brace_position,
        should_use_blank_line: move |previous, next, context| {
            node_helpers::has_separating_blank_line(previous, next, context.module)
        },
        separator: Separator::none(),
    }, context));

    if node.is_class_expr {
        let items = items.into_rc_path();
        if_true_or(
            "classExprConditionalIndent",
            move |context| {
                let resolved_start_before_comments_info = context.get_resolved_info(&start_before_owned_comments_info)?;
                if resolved_start_before_comments_info.is_start_of_line() {
                    Some(false)
                } else {
                    condition_resolvers::is_multiple_lines(context, &start_before_owned_comments_info, &start_header_info)
                }
            },
            with_indent(items.clone().into()),
            items.into(),
        ).into()
    } else {
        items
    }
}

fn parse_export_decl<'a>(node: &'a ExportDecl, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    // decorators are handled in parse_node because their starts come before the ExportDecl
    items.push_str("export ");
    items.extend(parse_node(node.decl.into(), context));
    items
}

fn parse_export_default_decl<'a>(node: &'a ExportDefaultDecl, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    // decorators are handled in parse_node because their starts come before the ExportDefaultDecl
    items.push_str("export default ");
    items.extend(parse_node(node.decl.into(), context));
    items
}

fn parse_export_default_expr<'a>(node: &'a ExportDefaultExpr, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    items.push_str("export default ");
    items.extend(parse_node(node.expr.into(), context));
    if context.config.semi_colons.is_true() { items.push_str(";"); }
    items
}

fn parse_export_default_specifier<'a>(node: &'a ExportDefaultSpecifier, context: &mut Context<'a>) -> PrintItems {
    parse_node(node.exported.into(), context)
}

fn parse_enum_decl<'a>(node: &'a TsEnumDecl, context: &mut Context<'a>) -> PrintItems {
    let start_header_info = Info::new("startHeader");
    let mut items = PrintItems::new();

    // header
    items.push_info(start_header_info);

    if node.declare() { items.push_str("declare "); }
    if node.is_const() { items.push_str("const "); }
    items.push_str("enum ");
    items.extend(parse_node(node.id.into(), context));

    // body
    let member_spacing = context.config.enum_declaration_member_spacing;
    items.extend(parse_membered_body(ParseMemberedBodyOptions {
        node: node.into(),
        members: node.members.iter().map(|&x| x.into()).collect(),
        start_header_info: Some(start_header_info),
        brace_position: context.config.enum_declaration_brace_position,
        should_use_blank_line: move |previous, next, context| {
            match member_spacing {
                MemberSpacing::BlankLine => true,
                MemberSpacing::NewLine => false,
                MemberSpacing::Maintain => node_helpers::has_separating_blank_line(previous, next, context.module),
            }
        },
        separator: context.config.enum_declaration_trailing_commas.into(),
    }, context));

    return items;
}

fn parse_enum_member<'a>(node: &'a TsEnumMember, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    items.extend(parse_node(node.id.into(), context));

    if let Some(init) = &node.init {
        items.extend(parse_assignment(init.into(), "=", context));
    }

    items
}

fn parse_export_named_decl<'a>(node: &'a NamedExport, context: &mut Context<'a>) -> PrintItems {
    // fill specifiers
    let mut default_export: Option<&ExportDefaultSpecifier> = None;
    let mut namespace_export: Option<&ExportNamespaceSpecifier> = None;
    let mut named_exports: Vec<&ExportNamedSpecifier> = Vec::new();

    for specifier in &node.specifiers {
        match specifier {
            ExportSpecifier::Default(node) => default_export = Some(node),
            ExportSpecifier::Namespace(node) => namespace_export = Some(node),
            ExportSpecifier::Named(node) => named_exports.push(node),
        }
    }

    let should_single_line = default_export.is_none() && namespace_export.is_none()
        && named_exports.len() <= 1
        && node.start_line_fast(context.module) == node.end_line_fast(context.module);

    // parse
    let mut items = PrintItems::new();

    items.push_str("export ");
    if node.type_only() { items.push_str("type "); }

    if let Some(default_export) = default_export {
        items.extend(parse_node(default_export.into(), context));
    } else if !named_exports.is_empty() {
        items.extend(parse_named_import_or_export_specifiers(
            node.into(),
            named_exports.into_iter().map(|x| x.into()).collect(),
            context
        ));
    } else if let Some(namespace_export) = namespace_export {
        items.extend(parse_node(namespace_export.into(), context));
    } else {
        items.push_str("{}");
    }

    if let Some(src) = node.src {
        items.push_str(" from ");
        items.extend(parse_node(src.into(), context));
    }

    if let Some(asserts) = node.asserts {
        items.push_str(" assert ");
        items.extend(parse_node(asserts.into(), context));
    }

    if context.config.semi_colons.is_true() {
        items.push_str(";");
    }

    if should_single_line {
        with_no_new_lines(items)
    } else {
        items
    }
}

fn parse_function_decl<'a>(node: &'a FnDecl, context: &mut Context<'a>) -> PrintItems {
    parse_function_decl_or_expr(FunctionDeclOrExprNode {
        node: node.into(),
        is_func_decl: true,
        ident: Some(node.ident),
        declare: node.declare(),
        func: node.function,
    }, context)
}

struct FunctionDeclOrExprNode<'a> {
    node: Node<'a>,
    is_func_decl: bool,
    ident: Option<&'a Ident<'a>>,
    declare: bool,
    func: &'a Function<'a>,
}

fn parse_function_decl_or_expr<'a>(node: FunctionDeclOrExprNode<'a>, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    let start_header_info = Info::new("functionHeaderStart");
    let func = node.func;
    let space_after_function_keyword = !node.is_func_decl && context.config.function_expression_space_after_function_keyword;

    items.push_info(start_header_info);
    if node.declare { items.push_str("declare "); }
    if func.is_async() { items.push_str("async "); }
    items.push_str("function");
    if func.is_generator() { items.push_str("*"); }
    if space_after_function_keyword {
        items.push_str(" ")
    }
    if let Some(ident) = node.ident {
        if !space_after_function_keyword {
            items.push_str(" ");
        }
        items.extend(parse_node(ident.into(), context));
    }
    if let Some(type_params) = func.type_params { items.extend(parse_node(type_params.into(), context)); }
    if get_use_space_before_parens(node.is_func_decl, context) {
        if node.ident.is_some() || func.type_params.is_some() || !space_after_function_keyword {
            items.push_str(" ");
        }
    }

    items.extend(parse_parameters_or_arguments(ParseParametersOrArgumentsOptions {
        node: node.node,
        nodes: func.params.iter().map(|&node| node.into()).collect(),
        span: func.get_parameters_span(context),
        custom_close_paren: |context| Some(parse_close_paren_with_type(ParseCloseParenWithTypeOptions {
            start_info: start_header_info,
            type_node: func.return_type.map(|x| x.into()),
            type_node_separator: None,
            param_count: func.params.len(),
        }, context)),
        is_parameters: true,
    }, context));

    if let Some(body) = func.body {
        let brace_position = if node.is_func_decl {
            context.config.function_declaration_brace_position
        } else {
            context.config.function_expression_brace_position
        };
        let open_brace_token = context.token_finder.get_first_open_brace_token_within(body);

        items.extend(parse_brace_separator(ParseBraceSeparatorOptions {
            brace_position: brace_position,
            open_brace_token: open_brace_token,
            start_header_info: Some(start_header_info),
        }, context));

        items.extend(parse_node(body.into(), context));
    } else {
        if context.config.semi_colons.is_true() {
            items.push_str(";");
        }
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

fn parse_param<'a>(node: &'a Param, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    items.extend(parse_decorators(&node.decorators, true, context));
    items.extend(parse_node(node.pat.into(), context));
    items
}

fn parse_import_decl<'a>(node: &'a ImportDecl, context: &mut Context<'a>) -> PrintItems {
    // fill specifiers
    let mut default_import: Option<&ImportDefaultSpecifier> = None;
    let mut namespace_import: Option<&ImportStarAsSpecifier> = None;
    let mut named_imports: Vec<&ImportNamedSpecifier> = Vec::new();

    for specifier in &node.specifiers {
        match specifier {
            ImportSpecifier::Default(node) => default_import = Some(node),
            ImportSpecifier::Namespace(node) => namespace_import = Some(node),
            ImportSpecifier::Named(node) => named_imports.push(node),
        }
    }

    let should_single_line = default_import.is_none() && namespace_import.is_none()
        && named_imports.len() <= 1
        && node.start_line_fast(context.module) == node.end_line_fast(context.module);
    let has_named_imports = !named_imports.is_empty() || {
        let from_keyword = context.token_finder.get_previous_token_if_from_keyword(&node.src);
        if let Some(from_keyword) = from_keyword {
            context.token_finder.get_previous_token_if_close_brace(from_keyword).is_some()
        } else {
            false
        }
    };
    let has_from = default_import.is_some() || namespace_import.is_some() || has_named_imports;
    let mut items = PrintItems::new();

    items.push_str("import ");
    if node.type_only() { items.push_str("type "); }

    if let Some(default_import) = default_import {
        items.extend(parse_node(default_import.into(), context));
        if namespace_import.is_some() || !named_imports.is_empty() {
            items.push_str(", ");
        }
    }
    if let Some(namespace_import) = namespace_import {
        items.extend(parse_node(namespace_import.into(), context));
    }

    if has_named_imports {
        items.extend(parse_named_import_or_export_specifiers(
            node.into(),
            named_imports.into_iter().map(|x| x.into()).collect(),
            context
        ));
    }

    if has_from { items.push_str(" from "); }

    items.extend(parse_node(node.src.into(), context));

    if let Some(asserts) = node.asserts {
        items.push_str(" assert ");
        items.extend(parse_node(asserts.into(), context));
    }

    if context.config.semi_colons.is_true() {
        items.push_str(";");
    }

    if should_single_line {
        with_no_new_lines(items)
    } else {
        items
    }
}

fn parse_import_equals_decl<'a>(node: &'a TsImportEqualsDecl, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    if node.is_export() {
        items.push_str("export ");
    }

    items.push_str("import ");
    items.extend(parse_node(node.id.into(), context));
    items.push_str(" = "); // keep on one line
    items.extend(parse_node(node.module_ref.into(), context));

    if context.config.semi_colons.is_true() { items.push_str(";"); }

    return items;
}

fn parse_interface_decl<'a>(node: &'a TsInterfaceDecl, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    let start_header_info = Info::new("startHeader");
    items.push_info(start_header_info);
    context.store_info_for_node(node, start_header_info);

    if node.declare() { items.push_str("declare "); }
    items.push_str("interface ");
    items.extend(parse_node(node.id.into(), context));
    if let Some(type_params) = node.type_params { items.extend(parse_node(type_params.into(), context)); }
    items.extend(parse_extends_or_implements(ParseExtendsOrImplementsOptions {
        text: "extends",
        type_items: node.extends.iter().map(|&x| x.into()).collect(),
        start_header_info,
        prefer_hanging: context.config.extends_clause_prefer_hanging,
    }, context));
    items.extend(parse_node(node.body.into(), context));

    return items;
}

fn parse_module_decl<'a>(node: &'a TsModuleDecl, context: &mut Context<'a>) -> PrintItems {
    parse_module_or_namespace_decl(ModuleOrNamespaceDecl {
        span: node.span(),
        declare: node.declare(),
        global: node.global(),
        id: node.id.into(),
        body: node.body.as_ref(),
    }, context)
}

fn parse_namespace_decl<'a>(node: &'a TsNamespaceDecl, context: &mut Context<'a>) -> PrintItems {
    parse_module_or_namespace_decl(ModuleOrNamespaceDecl {
        span: node.span(),
        declare: node.declare(),
        global: node.global(),
        id: node.id.into(),
        body: Some(&node.body)
    }, context)
}

struct ModuleOrNamespaceDecl<'a> {
    pub span: Span,
    pub declare: bool,
    pub global: bool,
    pub id: Node<'a>,
    pub body: Option<&'a TsNamespaceBody<'a>>,
}

fn parse_module_or_namespace_decl<'a>(node: ModuleOrNamespaceDecl<'a>, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();

    let start_header_info = Info::new("startHeader");
    items.push_info(start_header_info);

    if node.declare { items.push_str("declare "); }
    if !node.global {
        let module_or_namespace_keyword = node.id.previous_token_fast(context.module).unwrap();
        let has_namespace_keyword = module_or_namespace_keyword.text_fast(context.module).chars().next() == Some('n');
        items.push_str(if has_namespace_keyword { "namespace " } else { "module " });
    }

    items.extend(parse_node(node.id.into(), context));
    items.extend(parse_body(node.body, start_header_info, context));

    return items;

    fn parse_body<'a>(body: Option<&TsNamespaceBody<'a>>, start_header_info: Info, context: &mut Context<'a>) -> PrintItems {
        let mut items = PrintItems::new();
        if let Some(body) = body {
            match body {
                TsNamespaceBody::TsModuleBlock(block) => {
                    items.extend(parse_membered_body(ParseMemberedBodyOptions {
                        node: (*block).into(),
                        members: block.body.iter().map(|x| x.into()).collect(),
                        start_header_info: Some(start_header_info),
                        brace_position: context.config.module_declaration_brace_position,
                        should_use_blank_line: move |previous, next, context| {
                            node_helpers::has_separating_blank_line(previous, next, context.module)
                        },
                        separator: Separator::none(),
                    }, context));
                },
                TsNamespaceBody::TsNamespaceDecl(decl) => {
                    items.push_str(".");
                    items.extend(parse_node(decl.id.into(), context));
                    items.extend(parse_body(Some(&decl.body), start_header_info, context));
                }
            }
        }
        else if context.config.semi_colons.is_true() {
            items.push_str(";");
        }

        return items;
    }
}

fn parse_type_alias<'a>(node: &'a TsTypeAliasDecl, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    if node.declare() { items.push_str("declare "); }
    items.push_str("type ");
    items.extend(parse_node(node.id.into(), context));
    if let Some(type_params) = node.type_params {
        items.extend(parse_node(type_params.into(), context));
    }

    items.extend(parse_assignment(node.type_ann.into(), "=", context));

    if context.config.semi_colons.is_true() { items.push_str(";"); }

    return items;
}

/* exports */

fn parse_named_import_or_export_specifiers<'a>(parent: Node<'a>, specifiers: Vec<Node<'a>>, context: &mut Context<'a>) -> PrintItems {
    return parse_object_like_node(ParseObjectLikeNodeOptions {
        node: parent,
        members: specifiers,
        separator: get_trailing_commas(&parent, context).into(),
        prefer_hanging: get_prefer_hanging(&parent, context),
        prefer_single_line: get_prefer_single_line(&parent, context),
        surround_single_line_with_spaces: get_use_space(&parent, context),
        allow_blank_lines: false,
        node_sorter: get_node_sorter(&parent, context),
    }, context);

    fn get_trailing_commas(parent_decl: &Node, context: &Context) -> TrailingCommas {
        match parent_decl {
            Node::NamedExport(_) => context.config.export_declaration_trailing_commas,
            Node::ImportDecl(_) => context.config.import_declaration_trailing_commas,
            _ => unreachable!(),
        }
    }

    fn get_use_space(parent_decl: &Node, context: &Context) -> bool {
        match parent_decl {
            Node::NamedExport(_) => context.config.export_declaration_space_surrounding_named_exports,
            Node::ImportDecl(_) => context.config.import_declaration_space_surrounding_named_imports,
            _ => unreachable!(),
        }
    }

    fn get_prefer_hanging(parent_decl: &Node, context: &Context) -> bool {
        match parent_decl {
            Node::NamedExport(_) => context.config.export_declaration_prefer_hanging,
            Node::ImportDecl(_) => context.config.import_declaration_prefer_hanging,
            _ => unreachable!(),
        }
    }

    fn get_prefer_single_line(parent_decl: &Node, context: &Context) -> bool {
        match parent_decl {
            Node::NamedExport(_) => context.config.export_declaration_prefer_single_line,
            Node::ImportDecl(_) => context.config.import_declaration_prefer_single_line,
            _ => unreachable!(),
        }
    }

    fn get_node_sorter<'a>(
        parent_decl: &Node,
        context: &Context<'a>,
    ) -> Option<Box<dyn Fn((usize, Option<&Node<'a>>), (usize, Option<&Node<'a>>), &Module<'a>) -> std::cmp::Ordering>> {
        match parent_decl {
            Node::NamedExport(_) => get_node_sorter_from_order(context.config.export_declaration_sort_named_exports),
            Node::ImportDecl(_) => get_node_sorter_from_order(context.config.import_declaration_sort_named_imports),
            _ => unreachable!(),
        }
    }
}

/* expressions */

fn parse_array_expr<'a>(node: &'a ArrayLit, context: &mut Context<'a>) -> PrintItems {
    parse_array_like_nodes(ParseArrayLikeNodesOptions {
        node: node.into(),
        nodes: node.elems.iter().map(|&x| x.map(|elem| elem.into())).collect(),
        prefer_hanging: context.config.array_expression_prefer_hanging,
        prefer_single_line: context.config.array_expression_prefer_single_line,
        trailing_commas: context.config.array_expression_trailing_commas,
    }, context)
}

fn parse_arrow_func_expr<'a>(node: &'a ArrowExpr, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    let header_start_info = Info::new("arrowFunctionExpressionHeaderStart");
    let should_use_parens = get_should_use_parens(&node, context);

    items.push_info(header_start_info);
    if node.is_async() { items.push_str("async "); }
    if let Some(type_params) = node.type_params { items.extend(parse_node(type_params.into(), context)); }

    if should_use_parens {
        // need to check if there are parens because parse_parameters_or_arguments depends on the parens existing
        if has_parens(node, context) {
            items.extend(parse_parameters_or_arguments(ParseParametersOrArgumentsOptions {
                node: node.into(),
                span: node.get_parameters_span(context),
                nodes: node.params.iter().map(|node| node.into()).collect(),
                custom_close_paren: |context| Some(parse_close_paren_with_type(ParseCloseParenWithTypeOptions {
                    start_info: header_start_info,
                    type_node: node.return_type.map(|x| x.into()),
                    type_node_separator: None,
                    param_count: node.params.len(),
                }, context)),
                is_parameters: true,
            }, context));
        } else {
            // todo: this should probably use more of the same logic as in parse_parameters_or_arguments
            // there will only be one param in this case
            items.push_str("(");
            items.extend(parse_node(node.params.first().unwrap().into(), context));
            items.push_str(")");
        }
    } else {
        items.extend(parse_node(node.params.first().unwrap().into(), context));
    }

    items.push_str(" =>");

    let parsed_body = parse_node(node.body.into(), context);
    let parsed_body = if use_new_line_group_for_arrow_body(node) { new_line_group(parsed_body) } else { parsed_body }.into_rc_path();
    let open_brace_token = match &node.body {
        BlockStmtOrExpr::BlockStmt(stmt) => context.token_finder.get_first_open_brace_token_within(stmt),
        _ => None,
    };

    if open_brace_token.is_some() {
        items.extend(parse_brace_separator(ParseBraceSeparatorOptions {
            brace_position: context.config.arrow_function_brace_position,
            open_brace_token: open_brace_token,
            start_header_info: Some(header_start_info),
        }, context));

        items.extend(parsed_body.into());
    } else {
        let start_body_info = Info::new("startBody");
        let end_body_info = Info::new("endBody");
        items.push_info(start_body_info);

        if should_not_newline_after_arrow(&node.body) {
            items.push_str(" ");
        } else {
            items.push_condition(conditions::if_above_width_or(
                context.config.indent_width,
                if_true_or("newlineOrSpace", move |context| {
                    condition_resolvers::is_multiple_lines(context, &start_body_info, &end_body_info)
                }, Signal::NewLine.into(), Signal::SpaceOrNewLine.into()).into(),
                " ".into()
            ));
        }

        items.push_condition(conditions::indent_if_start_of_line(parsed_body.into()));
        items.push_info(end_body_info);
    }

    return items;

    fn should_not_newline_after_arrow(body: &BlockStmtOrExpr) -> bool {
        match body {
            BlockStmtOrExpr::BlockStmt(_) => true,
            BlockStmtOrExpr::Expr(expr) => {
                match expr {
                    Expr::Paren(_) | Expr::Array(_) => true,
                    _ => false,
                }
            }
        }
    }

    fn get_should_use_parens<'a>(node: &'a ArrowExpr, context: &mut Context<'a>) -> bool {
        let requires_parens = node.params.len() != 1 || node.return_type.is_some() || is_first_param_not_identifier_or_has_type_annotation(&node.params);

        return if requires_parens {
            true
        } else {
            match context.config.arrow_function_use_parentheses {
                UseParentheses::Force => true,
                UseParentheses::PreferNone => false,
                UseParentheses::Maintain => has_parens(&node, context),
            }
        };

        fn is_first_param_not_identifier_or_has_type_annotation(params: &Vec<Pat>) -> bool {
            let first_param = params.iter().next();
            match first_param {
                Some(Pat::Ident(node)) => node.type_ann.is_some(),
                _ => true
            }
        }
    }

    fn has_parens<'a>(node: &'a ArrowExpr, context: &mut Context<'a>) -> bool {
        if node.params.len() != 1 {
            true
        } else {
            for node_or_token in node.children_with_tokens_fast(context.module) {
                match node_or_token {
                    NodeOrToken::Node(_) => return false, // first param, so no parens
                    NodeOrToken::Token(TokenAndSpan {
                        token: Token::LParen,
                        ..
                    }) => return true,
                    _ => {},
                }
            }
            false
        }
    }
}

fn parse_as_expr<'a>(node: &'a TsAsExpr, context: &mut Context<'a>) -> PrintItems {
    parse_as_expr_like(AsExprLike {
        expr: node.expr.into(),
        type_ann: node.type_ann.into(),
    }, context)
}

struct AsExprLike<'a> {
    expr: Node<'a>,
    type_ann: Node<'a>,
}

fn parse_as_expr_like<'a>(node: AsExprLike<'a>, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    items.extend(parse_node(node.expr, context));
    items.push_str(" as");
    items.push_signal(Signal::SpaceIfNotTrailing);
    items.push_condition(conditions::with_indent_if_start_of_line_indented(parse_node(node.type_ann, context)));
    items
}

fn parse_const_assertion<'a>(node: &'a TsConstAssertion, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    items.extend(parse_node(node.expr.into(), context));
    items.push_str(" as const");
    items
}

fn parse_assignment_expr<'a>(node: &'a AssignExpr, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    items.extend(parse_node(node.left.into(), context));
    items.extend(parse_assignment(node.right.into(), node.op().as_str(), context));
    items
}

fn parse_await_expr<'a>(node: &'a AwaitExpr, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    items.push_str("await ");
    items.extend(parse_node(node.arg.into(), context));
    items
}

fn parse_binary_expr<'a>(node: &'a BinExpr, context: &mut Context<'a>) -> PrintItems {
    // todo: use a simplified version for nodes that don't need the complexity (for performance reasons)
    let mut items = PrintItems::new();
    let flattened_binary_expr = get_flattened_bin_expr(node, context);
    // println!("Bin expr: {:?}", flattened_binary_expr.iter().map(|x| x.expr.text(context)).collect::<Vec<_>>());
    let line_per_expression = context.config.binary_expression_line_per_expression;
    let force_use_new_lines = !context.config.binary_expression_prefer_single_line
        && node_helpers::get_use_new_lines_for_nodes(&flattened_binary_expr[0].expr, if line_per_expression {
            &flattened_binary_expr[1].expr
        } else {
            &flattened_binary_expr.last().unwrap().expr
        }, context.module);
    let indent_width = context.config.indent_width;
    let binary_expr_start_info = Info::new("binExprStartInfo");
    let allow_no_indent = get_allow_no_indent(node);
    let use_space_surrounding_operator = get_use_space_surrounding_operator(&node.op(), context);
    let is_parent_bin_expr = node.parent().unwrap().kind() == NodeKind::BinExpr;
    let multi_line_options = {
        let mut options = if line_per_expression {
            parser_helpers::MultiLineOptions::same_line_no_indent()
        } else {
            parser_helpers::MultiLineOptions::maintain_line_breaks()
        };
        options.with_hanging_indent = if is_parent_bin_expr {
            BoolOrCondition::Bool(false) // let the parent handle the indent
        } else {
            BoolOrCondition::Condition(Rc::new(Box::new(move |condition_context| {
                let binary_expr_start_info = condition_context.get_resolved_info(&binary_expr_start_info)?;
                if allow_no_indent && binary_expr_start_info.is_start_of_line() { return Some(false); }
                Some(condition_resolvers::is_start_of_line(condition_context))
            })))
        };
        options
    };

    items.push_info(binary_expr_start_info);

    items.extend(parser_helpers::parse_separated_values(|_| {
        let mut parsed_nodes = Vec::new();
        for bin_expr_item in flattened_binary_expr.into_iter() {
            let lines_span = Some(parser_helpers::LinesSpan{
                start_line: bin_expr_item.expr.span().start_line_fast(context.module),
                end_line: bin_expr_item.expr.span().end_line_fast(context.module)
            });
            let mut items = PrintItems::new();

            let pre_op = bin_expr_item.pre_op;
            let post_op = bin_expr_item.post_op;
            let (leading_pre_op_comments, trailing_pre_op_comments) = if let Some(op) = &pre_op {
                let op_line = op.token.start_line_fast(context.module);
                (parse_op_comments(
                    op.token.leading_comments_fast(context.module).filter(|x|
                        x.kind == CommentKind::Block && x.start_line_fast(context.module) == op_line
                    ).collect(),
                    context
                ), parse_op_comments(
                    op.token.trailing_comments_fast(context.module).filter(|x|
                        x.kind == CommentKind::Block && x.start_line_fast(context.module) == op_line
                    ).collect(),
                    context
                ))
            } else { (PrintItems::new(), PrintItems::new()) };
            let is_inner_binary_expression = bin_expr_item.expr.kind() == NodeKind::BinExpr;
            items.extend(parse_node_with_inner_parse(bin_expr_item.expr, context, |node_items, context| {
                let mut items = PrintItems::new();
                if let Some(op) = pre_op {
                    if !leading_pre_op_comments.is_empty() {
                        items.extend(leading_pre_op_comments);
                        items.push_str(" ");
                    }
                    items.push_str(op.op.as_str());
                    if trailing_pre_op_comments.is_empty() {
                        if use_space_surrounding_operator {
                            items.push_str(" ");
                        }
                    } else {
                        items.push_str(" ");
                        items.extend(trailing_pre_op_comments);
                        items.push_str(" ");
                    }
                }

                items.extend(if is_inner_binary_expression {
                    let node_items = node_items.into_rc_path();
                    with_queued_indent(
                        // indent again if it hasn't done the current binary expression's hanging indent
                        if_true_or(
                            "indentIfNecessary",
                            move |context| {
                                let binary_expr_start_info = context.get_resolved_info(&binary_expr_start_info)?;
                                if allow_no_indent && binary_expr_start_info.is_start_of_line() { return Some(false); }
                                let is_hanging = binary_expr_start_info.indent_level < context.writer_info.indent_level;
                                Some(!is_hanging)
                            },
                            with_queued_indent(node_items.clone().into()),
                            node_items.into(),
                        ).into()
                    )
                } else {
                    node_items
                });

                if let Some(op) = post_op {
                    let op_line = op.token.start_line_fast(context.module);
                    let leading_post_op_comments = parse_op_comments(
                        op.token.leading_comments_fast(context.module).filter(|x|
                            x.kind == CommentKind::Block && x.start_line_fast(context.module) == op_line
                        ).collect(),
                        context
                    );
                    let trailing_post_op_comments = parse_op_comments(
                        op.token.trailing_comments_fast(context.module).filter(|x|
                            x.start_line_fast(context.module) == op_line
                        ).collect(),
                        context
                    );
                    if leading_post_op_comments.is_empty() {
                        if use_space_surrounding_operator {
                            items.push_str(" ");
                        }
                    } else {
                        items.push_str(" ");
                        items.extend(leading_post_op_comments);
                        items.push_str(" ");
                    }
                    items.push_str(op.op.as_str());
                    if !trailing_post_op_comments.is_empty() {
                        items.push_str(" ");
                        items.extend(trailing_post_op_comments);
                    }
                }

                items
            }));

            parsed_nodes.push(parser_helpers::ParsedValue {
                items: parser_helpers::new_line_group(items),
                lines_span,
                allow_inline_multi_line: true,
                allow_inline_single_line: true,
            });
        }

        parsed_nodes
    }, parser_helpers::ParseSeparatedValuesOptions {
        prefer_hanging: false,
        force_use_new_lines,
        allow_blank_lines: false,
        single_line_space_at_start: false,
        single_line_space_at_end: false,
        single_line_separator: if use_space_surrounding_operator { Signal::SpaceOrNewLine.into() } else { PrintItems::new() },
        indent_width,
        multi_line_options,
        force_possible_newline_at_start: false,
    }).items);

    return if node.op().is_equality() { parser_helpers::new_line_group(items) } else { items };

    fn get_allow_no_indent(node: &BinExpr) -> bool {
        let parent = node.parent().unwrap();
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
                Node::ExprOrSpread(expr_or_spread) => {
                    match expr_or_spread.parent().unwrap().kind() {
                        NodeKind::CallExpr | NodeKind::NewExpr => false,
                        _ => true,
                    }
                },
                _ => true,
            }
        }
    }

    fn parse_op_comments(comments: Vec<&Comment>, context: &mut Context) -> PrintItems {
        let mut items = PrintItems::new();
        let mut had_comment_last = false;
        for comment in comments {
            if had_comment_last { items.push_str(" "); }
            if let Some(comment) = parse_comment(&comment, context) {
                items.extend(comment);
                had_comment_last = true;
            } else {
                had_comment_last = false;
            }
        }
        items
    }

    fn get_use_space_surrounding_operator(op: &BinaryOp, context: &mut Context) -> bool {
        if op.is_bitwise_or_arithmetic() {
            context.config.binary_expression_space_surrounding_bitwise_and_arithmetic_operator
        } else {
            true
        }
    }
}

fn parse_call_expr<'a>(node: &'a CallExpr, context: &mut Context<'a>) -> PrintItems {
    return if node_helpers::is_test_library_call_expr(&node, context.module) {
        parse_test_library_call_expr(node, context)
    } else {
        // flatten the call expression and check if it should be parsed as a flattened member like expression
        let flattened_call_expr = flatten_member_like_expr(node.into(), context.module);
        if flattened_call_expr.nodes.len() > 1 {
            parse_for_flattened_member_like_expr(flattened_call_expr, context)
        } else {
            parse_call_expr_like(CallExprLike {
                original_call_expr: node,
                parsed_callee: parse_node(node.callee.into(), context),
            }, context)
        }
    };

    fn parse_test_library_call_expr<'a>(node: &'a CallExpr, context: &mut Context<'a>) -> PrintItems {
        let mut items = PrintItems::new();
        items.extend(parse_test_library_callee(&node.callee, context));
        items.extend(parse_test_library_arguments(&node.args, context));
        return items;

        fn parse_test_library_callee<'a>(callee: &'a ExprOrSuper, context: &mut Context<'a>) -> PrintItems {
            match callee {
                ExprOrSuper::Expr(expr) => {
                    match expr {
                        Expr::Member(member_expr) => {
                            let mut items = PrintItems::new();
                            items.extend(parse_node(member_expr.obj.into(), context));
                            items.push_str(".");
                            items.extend(parse_node(member_expr.prop.into(), context));
                            items
                        },
                        _=> parse_node(expr.into(), context),
                    }
                },
                _ => parse_node(callee.into(), context),
            }
        }

        fn parse_test_library_arguments<'a>(args: &Vec<&'a ExprOrSpread>, context: &mut Context<'a>) -> PrintItems {
            let mut items = PrintItems::new();
            items.push_str("(");
            items.extend(parse_node_with_inner_parse(args[0].into(), context, |items, _| {
                let mut new_items = parser_helpers::with_no_new_lines(items);
                new_items.push_str(",");
                new_items
            }));
            items.push_str(" ");
            items.extend(parse_node(args[1].into(), context));
            items.push_str(")");

            return items;
        }
    }
}

struct CallExprLike<'a> {
    original_call_expr: &'a CallExpr<'a>,
    parsed_callee: PrintItems,
}

fn parse_call_expr_like<'a>(node: CallExprLike<'a>, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    let call_expr = node.original_call_expr;

    items.extend(node.parsed_callee);

    if let Some(type_args) = call_expr.type_args {
        items.extend(parse_node(type_args.into(), context));
    }

    if node_helpers::is_optional_call_expr(call_expr) {
        items.push_str("?.");
    }

    items.push_condition(conditions::with_indent_if_start_of_line_indented(parse_parameters_or_arguments(ParseParametersOrArgumentsOptions {
        node: call_expr.into(),
        span: call_expr.get_parameters_span(context),
        nodes: call_expr.args.iter().map(|&node| node.into()).collect(),
        custom_close_paren: |_| None,
        is_parameters: false,
    }, context)));

    items
}

fn parse_class_expr<'a>(node: &'a ClassExpr, context: &mut Context<'a>) -> PrintItems {
    parse_class_decl_or_expr(ClassDeclOrExpr {
        node: node.into(),
        member_node: node.class.into(),
        decorators: &node.class.decorators,
        is_class_expr: true,
        is_declare: false,
        is_abstract: node.class.is_abstract(),
        ident: node.ident.map(|x| x.into()),
        type_params: node.class.type_params.map(|x| x.into()),
        super_class: node.class.super_class.map(|x| x.into()),
        super_type_params: node.class.super_type_params.map(|x| x.into()),
        implements: node.class.implements.iter().map(|&x| x.into()).collect(),
        members: node.class.body.iter().map(|x| x.into()).collect(),
        brace_position: context.config.class_expression_brace_position,
    }, context)
}

fn parse_conditional_expr<'a>(node: &'a CondExpr, context: &mut Context<'a>) -> PrintItems {
    let operator_token = context.token_finder.get_first_operator_after(&node.test, "?").unwrap();
    let force_new_lines = !context.config.conditional_expression_prefer_single_line && (
        node_helpers::get_use_new_lines_for_nodes(&node.test, &node.cons, context.module)
        || node_helpers::get_use_new_lines_for_nodes(&node.cons, &node.alt, context.module)
    );
    let operator_position = get_operator_position(node, &operator_token, context);
    let top_most_data = get_top_most_data(node, context);
    let before_alternate_info = Info::new("beforeAlternateInfo");
    let end_info = Info::new("endConditionalExpression");
    let mut items = PrintItems::new();

    if top_most_data.is_top_most {
        items.push_info(top_most_data.top_most_info);
    }

    items.extend(parser_helpers::new_line_group(with_queued_indent(parse_node_with_inner_parse(node.test.into(), context, {
        move |mut items, _| {
            if operator_position == OperatorPosition::SameLine {
                items.push_str(" ?");
            }
            items
        }
    }))));

    // force re-evaluation of all the conditions below once the end info has been reached
    items.push_condition(conditions::force_reevaluation_once_resolved(context.end_statement_or_member_infos.peek().map(|x| x.clone()).unwrap_or(end_info)));

    if force_new_lines {
        items.push_signal(Signal::NewLine);
    } else {
        items.push_condition(conditions::new_line_if_multiple_lines_space_or_new_line_otherwise(top_most_data.top_most_info, Some(before_alternate_info)));
    }

    let cons_and_alt_items = {
        let mut items = PrintItems::new();
        if operator_position == OperatorPosition::NextLine {
            items.push_str("? ");
        }
        items.extend(parser_helpers::new_line_group(parse_node_with_inner_parse(node.cons.into(), context, {
            move |mut items, _| {
                if operator_position == OperatorPosition::SameLine {
                    items.push_str(" :");
                    items
                } else {
                    conditions::indent_if_start_of_line(items).into()
                }
            }
        })));

        if force_new_lines {
            items.push_signal(Signal::NewLine);
        } else {
            items.push_condition(conditions::new_line_if_multiple_lines_space_or_new_line_otherwise(top_most_data.top_most_info, Some(before_alternate_info)));
        }

        if operator_position == OperatorPosition::NextLine {
            items.push_str(": ");
        }
        items.push_info(before_alternate_info);
        items.extend(parser_helpers::new_line_group(parse_node_with_inner_parse(node.alt.into(), context, |items, _| {
            if operator_position == OperatorPosition::NextLine {
                conditions::indent_if_start_of_line(items).into()
            } else {
                items
            }
        })));
        items.push_info(end_info);

        items
    };

    if top_most_data.is_top_most {
        items.push_condition(conditions::indent_if_start_of_line(cons_and_alt_items));
    } else {
        let cons_and_alt_items = cons_and_alt_items.into_rc_path();
        let top_most_info = top_most_data.top_most_info;
        items.push_condition(if_true_or(
            "indentIfSameIndentationAsTopMostAndStartOfLine",
            move |context| {
                if context.writer_info.is_start_of_line() {
                    let top_most_info = context.get_resolved_info(&top_most_info)?;
                    Some(context.writer_info.indent_level == top_most_info.indent_level)
                } else {
                    Some(false)
                }
            },
            with_indent(cons_and_alt_items.clone().into()),
            cons_and_alt_items.into(),
        ));
    }

    return items;

    struct TopMostData {
        top_most_info: Info,
        is_top_most: bool,
    }

    fn get_top_most_data(node: &CondExpr, context: &mut Context) -> TopMostData {
        // The "top most" node in nested conditionals follows the ancestors up through
        // the alternate expressions.
        let mut top_most_node = node;

        for ancestor in context.parent_stack.iter() {
            if let Node::CondExpr(parent) = ancestor {
                if parent.alt.lo() == top_most_node.lo() {
                    top_most_node = parent;
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        let is_top_most = top_most_node.span() == node.span();
        let top_most_info = get_or_set_top_most_info(top_most_node.lo(), is_top_most, context);

        return TopMostData {
            is_top_most,
            top_most_info,
        };

        fn get_or_set_top_most_info(top_most_expr_start: BytePos, is_top_most: bool, context: &mut Context) -> Info {
            if is_top_most {
                let info = Info::new("conditionalExprStart");
                context.store_info_for_node(&top_most_expr_start, info);
                info
            } else {
                context.get_info_for_node(&top_most_expr_start).expect("Expected to have the top most expr info stored")
            }
        }
    }

    fn get_operator_position(node: &CondExpr, operator_token: &TokenAndSpan, context: &mut Context) -> OperatorPosition {
        match context.config.conditional_expression_operator_position {
            OperatorPosition::NextLine => OperatorPosition::NextLine,
            OperatorPosition::SameLine => OperatorPosition::SameLine,
            OperatorPosition::Maintain => {
                if node.test.end_line_fast(context.module) == operator_token.start_line_fast(context.module) {
                    OperatorPosition::SameLine
                } else {
                    OperatorPosition::NextLine
                }
            }
        }
    }
}

fn parse_expr_or_spread<'a>(node: &'a ExprOrSpread, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    if node.spread().is_some() { items.push_str("..."); }
    items.extend(parse_node(node.expr.into(), context));
    items
}

fn parse_expr_with_type_args<'a>(node: &'a TsExprWithTypeArgs, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    items.extend(parse_node(node.expr.into(), context));
    if let Some(type_args) = node.type_args {
        items.extend(parse_node(type_args.into(), context));
    }
    return items;
}

fn parse_fn_expr<'a>(node: &'a FnExpr, context: &mut Context<'a>) -> PrintItems {
    parse_function_decl_or_expr(FunctionDeclOrExprNode {
        node: node.into(),
        is_func_decl: false,
        ident: node.ident,
        declare: false,
        func: &node.function,
    }, context)
}

fn parse_getter_prop<'a>(node: &'a GetterProp, context: &mut Context<'a>) -> PrintItems {
    return parse_class_or_object_method(ClassOrObjectMethod {
        node: node.into(),
        parameters_span: node.get_parameters_span(context),
        decorators: None,
        accessibility: None,
        is_static: false,
        is_async: false,
        is_abstract: false,
        kind: ClassOrObjectMethodKind::Getter,
        is_generator: false,
        is_optional: false,
        key: node.key.into(),
        type_params: None,
        params: Vec::new(),
        return_type: node.type_ann.map(|x| x.into()),
        body: node.body.map(|x| x.into()),
    }, context);
}

fn parse_key_value_prop<'a>(node: &'a KeyValueProp, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    items.extend(parse_node(node.key.into(), context));
    items.extend(parse_assignment(node.value.into(), ":", context));
    items
}

fn parse_assign_prop<'a>(node: &'a AssignProp, context: &mut Context<'a>) -> PrintItems {
    // assignment properties are not valid, so turn this into a key value property
    let mut items = PrintItems::new();
    items.extend(parse_node(node.key.into(), context));
    items.extend(parse_assignment_op_to(node.value.into(), "=", ":", context)); // go from = to :
    items
}

fn parse_member_expr<'a>(node: &'a MemberExpr, context: &mut Context<'a>) -> PrintItems {
    let flattened_member_expr = flatten_member_like_expr(node.into(), context.module);
    parse_for_flattened_member_like_expr(flattened_member_expr, context)
}

fn parse_meta_prop_expr<'a>(node: &'a MetaPropExpr, context: &mut Context<'a>) -> PrintItems {
    let flattened_meta_prop_expr = flatten_member_like_expr(node.into(), context.module);
    parse_for_flattened_member_like_expr(flattened_meta_prop_expr, context)
}

fn parse_new_expr<'a>(node: &'a NewExpr, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    items.push_str("new ");
    items.extend(parse_node(node.callee.into(), context));
    if let Some(type_args) = node.type_args { items.extend(parse_node(type_args.into(), context)); }
    let args = match node.args.as_ref() {
        Some(args) => args.iter().map(|&node| node.into()).collect(),
        None => Vec::new(),
    };
    items.extend(parse_parameters_or_arguments(ParseParametersOrArgumentsOptions {
        node: node.into(),
        span: node.get_parameters_span(context),
        nodes: args,
        custom_close_paren: |_| None,
        is_parameters: false
    }, context));
    items
}

fn parse_non_null_expr<'a>(node: &'a TsNonNullExpr, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    items.extend(parse_node(node.expr.into(), context));
    items.push_str("!");
    items
}

fn parse_object_lit<'a>(node: &'a ObjectLit, context: &mut Context<'a>) -> PrintItems {
    parse_object_like_node(ParseObjectLikeNodeOptions {
        node: node.into(),
        members: node.props.iter().map(|x| x.into()).collect(),
        separator: context.config.object_expression_trailing_commas.into(),
        prefer_hanging: context.config.object_expression_prefer_hanging,
        prefer_single_line: context.config.object_expression_prefer_single_line,
        surround_single_line_with_spaces: context.config.object_expression_space_surrounding_properties,
        allow_blank_lines: true,
        node_sorter: None,
    }, context)
}

fn parse_paren_expr<'a>(node: &'a ParenExpr, context: &mut Context<'a>) -> PrintItems {
    let parsed_items = conditions::with_indent_if_start_of_line_indented(parse_node_in_parens(
        |context| parse_node(node.expr.into(), context),
        ParseNodeInParensOptions {
            inner_span: node.expr.span(),
            prefer_hanging: true,
            allow_open_paren_trailing_comments: true,
        },
        context
    )).into();

    return if get_use_new_line_group(node) {
        new_line_group(parsed_items)
    } else {
        parsed_items
    };

    fn get_use_new_line_group(node: &ParenExpr) -> bool {
        if let Some(Node::ArrowExpr(arrow_expr)) = node.parent() {
            debug_assert!(arrow_expr.body.lo() == node.lo());
            use_new_line_group_for_arrow_body(arrow_expr)
        } else {
            true
        }
    }
}

fn parse_sequence_expr<'a>(node: &'a SeqExpr, context: &mut Context<'a>) -> PrintItems {
    parse_separated_values(ParseSeparatedValuesParams {
        nodes: node.exprs.iter().map(|x| NodeOrSeparator::Node(x.into())).collect(),
        prefer_hanging: context.config.sequence_expression_prefer_hanging,
        force_use_new_lines: false,
        allow_blank_lines: false,
        separator: TrailingCommas::Never.into(),
        single_line_space_at_start: false,
        single_line_space_at_end: false,
        custom_single_line_separator: None,
        multi_line_options: parser_helpers::MultiLineOptions::same_line_start_hanging_indent(),
        force_possible_newline_at_start: false,
        node_sorter: None,
    }, context)
}

fn parse_setter_prop<'a>(node: &'a SetterProp, context: &mut Context<'a>) -> PrintItems {
    parse_class_or_object_method(ClassOrObjectMethod {
        node: node.into(),
        parameters_span: node.get_parameters_span(context),
        decorators: None,
        accessibility: None,
        is_static: false,
        is_async: false,
        is_abstract: false,
        kind: ClassOrObjectMethodKind::Setter,
        is_generator: false,
        is_optional: false,
        key: node.key.into(),
        type_params: None,
        params: vec![node.param.into()],
        return_type: None,
        body: node.body.map(|x| x.into()),
    }, context)
}

fn parse_spread_element<'a>(node: &'a SpreadElement, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    items.push_str("...");
    items.extend(parse_node(node.expr.into(), context));
    items
}

fn parse_tagged_tpl<'a>(node: &'a TaggedTpl, context: &mut Context<'a>) -> PrintItems {
    let use_space = context.config.tagged_template_space_before_literal;
    let mut items = parse_node(node.tag.into(), context);
    if let Some(type_params) = node.type_params { items.extend(parse_node(type_params.into(), context)); }

    items.push_condition(conditions::if_above_width_or(
        context.config.indent_width,
        if use_space { Signal::SpaceOrNewLine } else { Signal::PossibleNewLine }.into(),
        if use_space { " ".into() } else { PrintItems::new() }
    ));

    items.push_condition(conditions::indent_if_start_of_line(parse_template_literal(
        node.quasis.iter().map(|&n| n.into()).collect(),
        node.exprs.iter().map(|x| x.into()).collect(),
        context,
    )));
    items
}

fn parse_tpl<'a>(node: &'a Tpl, context: &mut Context<'a>) -> PrintItems {
    parse_template_literal(
        node.quasis.iter().map(|&n| n.into()).collect(),
        node.exprs.iter().map(|x| x.into()).collect(),
        context,
    )
}

fn parse_tpl_element<'a>(node: &'a TplElement, context: &mut Context<'a>) -> PrintItems {
    parse_raw_string(node.text_fast(context.module).into())
}

fn parse_template_literal<'a>(quasis: Vec<Node<'a>>, exprs: Vec<Node<'a>>, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    items.push_str("`");
    items.push_signal(Signal::StartIgnoringIndent);
    for node in get_nodes(quasis, exprs) {
        if node.kind() == NodeKind::TplElement {
            items.extend(parse_node(node, context));
        } else {
            items.push_str("${");
            items.push_signal(Signal::FinishIgnoringIndent);
            let keep_on_one_line = get_keep_on_one_line(&node);
            let possible_surround_newlines = get_possible_surround_newlines(&node);
            let parsed_expr = parse_node(node, context);
            items.extend(if keep_on_one_line {
                with_no_new_lines(parsed_expr)
            } else {
                if possible_surround_newlines {
                    parser_helpers::surround_with_newlines_indented_if_multi_line(new_line_group(parsed_expr), context.config.indent_width)
                } else {
                    parsed_expr
                }
            });
            items.push_str("}");
            items.push_signal(Signal::StartIgnoringIndent);
        }
    }
    items.push_str("`");
    items.push_signal(Signal::FinishIgnoringIndent);
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
                    if current_quasis.lo() < current_expr.lo() {
                        true
                    } else {
                        false
                    }
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

        return nodes;
    }

    // handle this on a case by case basis for now
    fn get_keep_on_one_line(node: &Node) -> bool {
        match node {
            Node::Ident(_) | Node::ThisExpr(_) | Node::Super(_) | Node::Str(_) | Node::PrivateName(_) => true,
            Node::MemberExpr(expr) => keep_member_expr_on_one_line(expr),
            Node::CallExpr(expr) => keep_call_expr_on_one_line(expr),
            _ => false,
        }
    }

    fn get_possible_surround_newlines(node: &Node) -> bool {
        match node {
            Node::CondExpr(_) => true,
            Node::MemberExpr(expr) => !keep_member_expr_on_one_line(expr),
            Node::CallExpr(expr) => !keep_call_expr_on_one_line(expr),
            _ => false,
        }
    }

    fn keep_member_expr_on_one_line(expr: &MemberExpr) -> bool {
        get_keep_on_one_line(&expr.obj.into()) && get_keep_on_one_line(&expr.prop.into()) && !expr.computed()
    }

    fn keep_call_expr_on_one_line(expr: &CallExpr) -> bool {
        expr.args.is_empty() && get_keep_on_one_line(&expr.callee.into())
    }
}

fn parse_type_assertion<'a>(node: &'a TsTypeAssertion, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    items.push_str("<");
    items.extend(parse_node(node.type_ann.into(), context));
    items.push_str(">");
    if context.config.type_assertion_space_before_expression { items.push_str(" "); }
    items.extend(parse_node(node.expr.into(), context));
    items
}

fn parse_unary_expr<'a>(node: &'a UnaryExpr, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    items.push_str(get_operator_text(node.op()));
    items.extend(parse_node(node.arg.into(), context));
    return items;

    fn get_operator_text<'a>(op: UnaryOp) -> &'a str {
        match op {
            UnaryOp::Void => "void ",
            UnaryOp::TypeOf => "typeof ",
            UnaryOp::Delete => "delete ",
            UnaryOp::Bang => "!",
            UnaryOp::Plus => "+",
            UnaryOp::Minus => "-",
            UnaryOp::Tilde => "~",
        }
    }
}

fn parse_update_expr<'a>(node: &'a UpdateExpr, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    let operator_text = get_operator_text(node.op());
    if node.prefix() {
        items.push_str(operator_text);
    }
    items.extend(parse_node(node.arg.into(), context));
    if !node.prefix() {
        items.push_str(operator_text);
    }
    return items;

    fn get_operator_text<'a>(operator: UpdateOp) -> &'a str {
        match operator {
            UpdateOp::MinusMinus => "--",
            UpdateOp::PlusPlus => "++",
        }
    }
}

fn parse_yield_expr<'a>(node: &'a YieldExpr, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    items.push_str("yield");
    if node.delegate() { items.push_str("*"); }
    if let Some(arg) = &node.arg {
        items.push_str(" ");
        items.extend(parse_node(arg.into(), context));
    }
    items
}

/* exports */

fn parse_export_named_specifier<'a>(node: &'a ExportNamedSpecifier, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();

    items.extend(parse_node(node.orig.into(), context));
    if let Some(exported) = node.exported {
        items.push_signal(Signal::SpaceOrNewLine);
        items.push_condition(conditions::indent_if_start_of_line({
            let mut items = PrintItems::new();
            items.push_str("as ");
            items.extend(parse_node(exported.into(), context));
            items
        }));
    }

    items
}

fn parse_namespace_export_specifier<'a>(node: &'a ExportNamespaceSpecifier, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    items.push_str("* as ");
    items.extend(parse_node(node.name.into(), context));
    items
}

/* imports */

fn parse_import_named_specifier<'a>(node: &'a ImportNamedSpecifier, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();

    if let Some(imported) = node.imported {
        items.extend(parse_node(imported.into(), context));
        items.push_signal(Signal::SpaceOrNewLine);
        items.push_condition(conditions::indent_if_start_of_line({
            let mut items = PrintItems::new();
            items.push_str("as ");
            items.extend(parse_node(node.local.into(), context));
            items
        }));
    } else {
        items.extend(parse_node(node.local.into(), context));
    }

    items
}

fn parse_import_namespace_specifier<'a>(node: &'a ImportStarAsSpecifier, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    items.push_str("* as ");
    items.extend(parse_node(node.local.into(), context));
    return items;
}

fn parse_external_module_ref<'a>(node: &'a TsExternalModuleRef, context: &mut Context<'a>) -> PrintItems {
    // force everything on a single line
    let mut items = PrintItems::new();
    items.push_str("require(");
    items.extend(parse_node(node.expr.into(), context));
    items.push_str(")");
    items
}

/* interface / type element */

fn parse_call_signature_decl<'a>(node: &'a TsCallSignatureDecl, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    let start_info = Info::new("startCallSignature");

    items.push_info(start_info);
    if let Some(type_params) = node.type_params { items.extend(parse_node(type_params.into(), context)); }
    items.extend(parse_parameters_or_arguments(ParseParametersOrArgumentsOptions {
        node: node.into(),
        span: node.get_parameters_span(context),
        nodes: node.params.iter().map(|node| node.into()).collect(),
        custom_close_paren: |context| Some(parse_close_paren_with_type(ParseCloseParenWithTypeOptions {
            start_info,
            type_node: node.type_ann.map(|x| x.into()),
            type_node_separator: None,
            param_count: node.params.len(),
        }, context)),
        is_parameters: true,
    }, context));

    return items;
}

fn parse_construct_signature_decl<'a>(node: &'a TsConstructSignatureDecl, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    let start_info = Info::new("startConstructSignature");

    items.push_info(start_info);
    items.push_str("new");
    if context.config.construct_signature_space_after_new_keyword { items.push_str(" "); }
    if let Some(type_params) = node.type_params { items.extend(parse_node(type_params.into(), context)); }
    items.extend(parse_parameters_or_arguments(ParseParametersOrArgumentsOptions {
        node: node.into(),
        span: node.get_parameters_span(context),
        nodes: node.params.iter().map(|node| node.into()).collect(),
        custom_close_paren: |context| Some(parse_close_paren_with_type(ParseCloseParenWithTypeOptions {
            start_info,
            type_node: node.type_ann.map(|x| x.into()),
            type_node_separator: None,
            param_count: node.params.len(),
        }, context)),
        is_parameters: true,
    }, context));

    return items;
}

fn parse_index_signature<'a>(node: &'a TsIndexSignature, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();

    if node.readonly() { items.push_str("readonly "); }

    let param: Node<'a> = node.params.iter().next().expect("Expected the index signature to have one parameter.").into();
    items.extend(parse_computed_prop_like(|context| parse_node(param, context), ParseComputedPropLikeOptions {
        inner_node_span: param.span(),
    }, context));
    items.extend(parse_type_ann_with_colon_if_exists(&node.type_ann, context));

    return items;
}

fn parse_method_signature<'a>(node: &'a TsMethodSignature, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    let start_info = Info::new("startMethodSignature");
    items.push_info(start_info);

    items.extend(if node.computed() {
        parse_computed_prop_like(|context| parse_node(node.key.into(), context), ParseComputedPropLikeOptions {
            inner_node_span: node.key.span(),
        }, context)
    } else {
        parse_node(node.key.into(), context)
    });

    if node.optional() { items.push_str("?"); }
    if let Some(type_params) = node.type_params { items.extend(parse_node(type_params.into(), context)); }

    items.extend(parse_parameters_or_arguments(ParseParametersOrArgumentsOptions {
        node: node.into(),
        span: node.get_parameters_span(context),
        nodes: node.params.iter().map(|node| node.into()).collect(),
        custom_close_paren: |context| Some(parse_close_paren_with_type(ParseCloseParenWithTypeOptions {
            start_info,
            type_node: node.type_ann.map(|x| x.into()),
            type_node_separator: None,
            param_count: node.params.len(),
        }, context)),
        is_parameters: true,
    }, context));

    return items;
}

fn parse_property_signature<'a>(node: &'a TsPropertySignature, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    if node.readonly() { items.push_str("readonly "); }

    items.extend(if node.computed() {
        parse_computed_prop_like(|context| parse_node(node.key.into(), context), ParseComputedPropLikeOptions {
            inner_node_span: node.key.span(),
        }, context)
    } else {
        parse_node(node.key.into(), context)
    });

    if node.optional() { items.push_str("?"); }
    items.extend(parse_type_ann_with_colon_if_exists(&node.type_ann, context));

    if let Some(init) = &node.init {
        items.extend(parse_assignment(init.into(), "=", context));
    }

    return items;
}

fn parse_interface_body<'a>(node: &'a TsInterfaceBody, context: &mut Context<'a>) -> PrintItems {
    let start_header_info = get_parent_info(node, context);

    return parse_membered_body(ParseMemberedBodyOptions {
        node: node.into(),
        members: node.body.iter().map(|x| x.into()).collect(),
        start_header_info: start_header_info,
        brace_position: context.config.interface_declaration_brace_position,
        should_use_blank_line: move |previous, next, context| {
            node_helpers::has_separating_blank_line(previous, next, context.module)
        },
        separator: context.config.semi_colons.into(),
    }, context);

    fn get_parent_info(node: &TsInterfaceBody, context: &mut Context) -> Option<Info> {
        for ancestor in node.ancestors() {
            if let Node::TsInterfaceDecl(ancestor) = ancestor {
                return context.get_info_for_node(ancestor).map(|x| x.to_owned());
            }
        }
        return None;
    }
}

fn parse_type_lit<'a>(node: &'a TsTypeLit, context: &mut Context<'a>) -> PrintItems {
    return parse_object_like_node(ParseObjectLikeNodeOptions {
        node: node.into(),
        members: node.members.iter().map(|m| m.into()).collect(),
        separator: Separator {
            single_line: Some(semi_colon_or_comma_to_separator_value(context.config.type_literal_separator_kind_single_line, context)),
            multi_line: Some(semi_colon_or_comma_to_separator_value(context.config.type_literal_separator_kind_multi_line, context)),
        },
        prefer_hanging: context.config.type_literal_prefer_hanging,
        prefer_single_line: context.config.type_literal_prefer_single_line,
        surround_single_line_with_spaces: context.config.type_literal_space_surrounding_properties,
        allow_blank_lines: true,
        node_sorter: None,
    }, context);

    fn semi_colon_or_comma_to_separator_value(value: SemiColonOrComma, context: &mut Context) -> SeparatorValue {
        match value {
            SemiColonOrComma::Comma => SeparatorValue::Comma(context.config.type_literal_trailing_commas),
            SemiColonOrComma::SemiColon => SeparatorValue::SemiColon(context.config.semi_colons),
        }
    }
}

/* jsx */

fn parse_jsx_attribute<'a>(node: &'a JSXAttr, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    items.extend(parse_node(node.name.into(), context));
    if let Some(value) = &node.value {
        items.push_str("=");
        let surround_with_braces = context.token_finder.get_previous_token_if_open_brace(value).is_some();
        let inner_items = parse_node(value.into(), context);
        items.extend(if surround_with_braces {
            parse_as_jsx_expr_container(node.into(), inner_items, context)
        } else {
            inner_items
        });
    }
    items
}

fn parse_jsx_closing_element<'a>(node: &'a JSXClosingElement, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    items.push_str("</");
    items.extend(parse_node(node.name.into(), context));
    items.push_str(">");
    items
}

fn parse_jsx_closing_fragment<'a>(_: &'a JSXClosingFragment, _: &mut Context<'a>) -> PrintItems {
    "</>".into()
}

fn parse_jsx_element<'a>(node: &'a JSXElement, context: &mut Context<'a>) -> PrintItems {
    if let Some(closing) = node.closing {
        let result = parse_jsx_with_opening_and_closing(ParseJsxWithOpeningAndClosingOptions {
            opening_element: node.opening.into(),
            closing_element: closing.into(),
            children: node.children.iter().map(|x| x.into()).collect(),
        }, context);
        context.store_info_range_for_node(node, (result.start_info, result.end_info));
        result.items
    } else {
        let start_info = Info::new("jsxElementStart");
        let end_info = Info::new("jsxElementEnd");
        let mut items = PrintItems::new();

        context.store_info_range_for_node(node, (start_info, end_info));

        items.push_info(start_info);
        items.extend(parse_node(node.opening.into(), context));
        items.push_info(end_info);
        items
    }
}

fn parse_jsx_empty_expr<'a>(node: &'a JSXEmptyExpr, context: &mut Context<'a>) -> PrintItems {
    parse_comment_collection(get_jsx_empty_expr_comments(node, context), None, None, context)
}

fn parse_jsx_expr_container<'a>(node: &'a JSXExprContainer, context: &mut Context<'a>) -> PrintItems {
    // Don't send JSX empty expressions to parse_node because it will not handle comments
    // the way they should be specifically handled for empty expressions.
    let parse_inner = match &node.expr {
        JSXExpr::JSXEmptyExpr(expr) => parse_jsx_empty_expr(expr, context),
        JSXExpr::Expr(expr) => parse_node(expr.into(), context),
    };

    parse_as_jsx_expr_container(node.expr.into(), parse_inner, context)
}

fn parse_as_jsx_expr_container(expr: Node, inner_items: PrintItems, context: &mut Context) -> PrintItems {
    let surround_with_space = context.config.jsx_expression_container_space_surrounding_expression;
    let surround_with_new_lines = should_surround_with_newlines(expr, &context.module);
    let mut items = PrintItems::new();
    items.push_str("{");
    if surround_with_new_lines {
        items.push_signal(Signal::NewLine);
        items.push_signal(Signal::StartIndent);
    } else if surround_with_space {
        items.push_str(" ");
    }
    items.extend(inner_items);
    if surround_with_new_lines {
        items.extend(parse_trailing_comments_as_statements(&expr, context));
        items.push_signal(Signal::NewLine);
        items.push_signal(Signal::FinishIndent);
    } else if surround_with_space {
        items.push_str(" ");
    }
    items.push_str("}");

    return items;

    fn should_surround_with_newlines(expr: Node, module: &Module) -> bool {
        let expr_start_line = expr.start_line_fast(module);
        for comment in expr.leading_comments_fast(module) {
            if comment.kind == CommentKind::Line {
                return true;
            } else if comment.start_line_fast(module) < expr_start_line {
                return true;
            }
        }
        let expr_end_line = expr.start_line_fast(module);
        for comment in expr.trailing_comments_fast(module) {
            if comment.kind == CommentKind::Line {
                return true;
            } else if comment.end_line_fast(module) > expr_end_line {
                return true;
            }
        }

        false
    }
}

fn parse_jsx_fragment<'a>(node: &'a JSXFragment, context: &mut Context<'a>) -> PrintItems {
    let result = parse_jsx_with_opening_and_closing(ParseJsxWithOpeningAndClosingOptions {
        opening_element: node.opening.into(),
        closing_element: node.closing.into(),
        children: node.children.iter().map(|x| x.into()).collect(),
    }, context);

    context.store_info_range_for_node(node, (result.start_info, result.end_info));

    result.items
}

fn parse_jsx_member_expr<'a>(node: &'a JSXMemberExpr, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    items.extend(parse_node(node.obj.into(), context));
    items.push_str(".");
    items.extend(parse_node(node.prop.into(), context));
    items
}

fn parse_jsx_namespaced_name<'a>(node: &'a JSXNamespacedName, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    items.extend(parse_node(node.ns.into(), context));
    items.push_str(":");
    items.extend(parse_node(node.name.into(), context));
    items
}

fn parse_jsx_opening_element<'a>(node: &'a JSXOpeningElement, context: &mut Context<'a>) -> PrintItems {
    let force_use_new_lines = get_force_is_multi_line(node, context);
    let start_info = Info::new("openingElementStartInfo");
    let mut items = PrintItems::new();

    items.push_info(start_info);
    items.push_str("<");
    items.extend(parse_node(node.name.into(), context));
    if let Some(type_args) = node.type_args {
        items.extend(parse_node(type_args.into(), context));
    }

    if !node.attrs.is_empty() {
        items.extend(parse_separated_values(ParseSeparatedValuesParams {
            nodes: node.attrs.iter().map(|p| NodeOrSeparator::Node(p.into())).collect(),
            prefer_hanging: context.config.jsx_attributes_prefer_hanging,
            force_use_new_lines,
            allow_blank_lines: false,
            separator: Separator::none(),
            single_line_space_at_start: true,
            single_line_space_at_end: node.self_closing(),
            custom_single_line_separator: None,
            multi_line_options: parser_helpers::MultiLineOptions::surround_newlines_indented(),
            force_possible_newline_at_start: false,
            node_sorter: None,
        }, context));
    }

    // parse trailing comments on different lines
    let name_or_type_arg_end = node.type_args.map(|t| t.hi()).unwrap_or(node.name.hi());
    let last_node_end = node.attrs.last().map(|n| n.hi()).unwrap_or(name_or_type_arg_end);

    let parsed_comments = parse_comments_as_statements(last_node_end.trailing_comments_fast(context.module), None, context);
    if !parsed_comments.is_empty() {
        if node.attrs.is_empty() {
            items.push_signal(Signal::NewLine);
        }
        items.extend(with_indent(parsed_comments));
        items.push_signal(Signal::NewLine);
    }

    if node.self_closing() {
        if node.attrs.is_empty() {
            items.push_str(""); // force current line indentation
            items.extend(space_if_not_start_line());
        }
        items.push_str("/");
    } else {
        if context.config.jsx_attributes_prefer_hanging {
            items.push_condition(conditions::new_line_if_hanging(start_info, None));
        }
    }
    items.push_str(">");

    return items;

    fn get_force_is_multi_line(node: &JSXOpeningElement, context: &mut Context) -> bool {
        if context.config.jsx_attributes_prefer_single_line {
            false
        } else if let Some(first_attrib) = node.attrs.first() {
            node_helpers::get_use_new_lines_for_nodes(&node.name, first_attrib, context.module)
        } else {
            false
        }
    }
}

fn parse_jsx_opening_fragment<'a>(_: &'a JSXOpeningFragment, _: &mut Context<'a>) -> PrintItems {
    "<>".into()
}

fn parse_jsx_spread_child<'a>(node: &'a JSXSpreadChild, context: &mut Context<'a>) -> PrintItems {
    parse_as_jsx_expr_container(node.into(), {
        let mut items = PrintItems::new();
        items.push_str("...");
        items.extend(parse_node(node.expr.into(), context));
        items
    }, context)
}

fn parse_jsx_text<'a>(node: &'a JSXText, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();

    for (i, line) in get_lines(node.text_fast(context.module)).into_iter().enumerate() {
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
                items.push_str(word);
                was_last_space_or_newline = false;
            }
        }
    }

    return parser_helpers::new_line_group(items);

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
                    current_line.push_str(" ");
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

fn parse_big_int_literal<'a>(node: &'a BigInt, context: &mut Context<'a>) -> PrintItems {
    node.text_fast(context.module).into()
}

fn parse_bool_literal(node: &Bool) -> PrintItems {
    match node.value() {
        true => "true",
        false => "false",
    }.into()
}

fn parse_num_literal<'a>(node: &'a Number, context: &mut Context<'a>) -> PrintItems {
    node.text_fast(context.module).into()
}

fn parse_reg_exp_literal(node: &Regex, _: &mut Context) -> PrintItems {
    // the exp and flags should not be nodes so just ignore that (swc issue #511)
    let mut items = PrintItems::new();
    items.push_str("/");
    items.push_str(node.exp() as &str);
    items.push_str("/");
    items.push_str(node.flags() as &str);
    items
}

fn parse_string_literal<'a>(node: &'a Str, context: &mut Context<'a>) -> PrintItems {
    return parse_raw_string(&get_string_literal_text(get_string_value(&node, context), context));

    fn get_string_literal_text(string_value: String, context: &mut Context) -> String {
        return match context.config.quote_style {
            QuoteStyle::AlwaysDouble => format_with_double(string_value),
            QuoteStyle::AlwaysSingle => format_with_single(string_value),
            QuoteStyle::PreferDouble => if double_to_single(&string_value) <= 0 {
                format_with_double(string_value)
            } else {
                format_with_single(string_value)
            },
            QuoteStyle::PreferSingle => if double_to_single(&string_value) >= 0 {
                format_with_single(string_value)
            } else {
                format_with_double(string_value)
            },
        };

        fn format_with_double(string_value: String) -> String {
            format!("\"{}\"", string_value.replace("\"", "\\\""))
        }

        fn format_with_single(string_value: String) -> String {
            format!("'{}'", string_value.replace("'", "\\'"))
        }

        fn double_to_single(string_value: &str) -> i32 {
            let mut double_count = 0;
            let mut single_count = 0;
            for c in string_value.chars() {
                match c {
                    '"' => double_count += 1,
                    '\'' => single_count += 1,
                    _ => {},
                }
            }

            return double_count - single_count;
        }
    }

    fn get_string_value(node: &Str, context: &mut Context) -> String {
        let raw_string_text = node.text_fast(context.module);
        let string_value = raw_string_text.chars().skip(1).take(raw_string_text.chars().count() - 2).collect::<String>();
        let is_double_quote = raw_string_text.chars().next().unwrap() == '"';

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
            new_string
        }
    }
}

/* module */

fn parse_module<'a>(node: &'a Module, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    if let Some(shebang) = node.shebang() {
        items.push_str("#!");
        items.push_str(&shebang as &str);
        items.push_signal(Signal::ExpectNewLine);
        if let Some(first_statement) = node.body.first() {
            if node_helpers::has_separating_blank_line(&node.lo(), first_statement, context.module) {
                items.push_signal(Signal::NewLine);
                items.push_signal(Signal::NewLine);
            }
        } else {
            let shebang_end = BytePos(("#!".len() + shebang.len()) as u32);
            items.extend(parse_trailing_comments_as_statements(&shebang_end, context));
        }
    }

    items.extend(parse_statements(node.span(), node.body.iter().map(|x| x.into()).collect(), context));

    return items;
}

/* patterns */

fn parse_array_pat<'a>(node: &'a ArrayPat, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    items.extend(parse_array_like_nodes(ParseArrayLikeNodesOptions {
        node: node.into(),
        nodes: node.elems.iter().map(|x| x.as_ref().map(|elem| elem.into())).collect(),
        prefer_hanging: context.config.array_pattern_prefer_hanging,
        prefer_single_line: context.config.array_pattern_prefer_single_line,
        trailing_commas: context.config.array_pattern_trailing_commas,
    }, context));
    if node.optional() { items.push_str("?"); }
    items.extend(parse_type_ann_with_colon_if_exists(&node.type_ann, context));
    items
}

fn parse_assign_pat<'a>(node: &'a AssignPat, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    items.extend(parse_node(node.left.into(), context));
    items.extend(parse_assignment(node.right.into(), "=", context));
    items
}

fn parse_assign_pat_prop<'a>(node: &'a AssignPatProp, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    items.extend(parse_node(node.key.into(), context));
    if let Some(value) = &node.value {
        items.extend(parse_assignment(value.into(), "=", context));
    }
    items
}

fn parse_key_value_pat_prop<'a>(node: &'a KeyValuePatProp, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    items.extend(parse_node(node.key.into(), context));
    items.extend(parse_assignment(node.value.into(), ":", context));
    items
}

fn parse_rest_pat<'a>(node: &'a RestPat, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    items.push_str("...");
    items.extend(parse_node(node.arg.into(), context));
    items.extend(parse_type_ann_with_colon_if_exists(&node.type_ann, context));
    items
}

fn parse_object_pat<'a>(node: &'a ObjectPat, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    items.extend(parse_object_like_node(ParseObjectLikeNodeOptions {
        node: node.into(),
        members: node.props.iter().map(|x| x.into()).collect(),
        separator: get_trailing_commas(node, context).into(),
        prefer_hanging: context.config.object_pattern_prefer_hanging,
        prefer_single_line: context.config.object_pattern_prefer_single_line,
        surround_single_line_with_spaces: context.config.object_pattern_space_surrounding_properties,
        allow_blank_lines: true,
        node_sorter: None,
    }, context));
    if node.optional() { items.push_str("?"); }
    items.extend(parse_type_ann_with_colon_if_exists(&node.type_ann, context));
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

fn parse_method_prop<'a>(node: &'a MethodProp, context: &mut Context<'a>) -> PrintItems {
    return parse_class_or_object_method(ClassOrObjectMethod {
        node: node.into(),
        parameters_span: node.get_parameters_span(context),
        decorators: None,
        accessibility: None,
        is_static: false,
        is_async: node.function.is_async(),
        is_abstract: false,
        kind: ClassOrObjectMethodKind::Method,
        is_generator: node.function.is_generator(),
        is_optional: false,
        key: node.key.into(),
        type_params: node.function.type_params.map(|x| x.into()),
        params: node.function.params.iter().map(|&x| x.into()).collect(),
        return_type: node.function.return_type.map(|x| x.into()),
        body: node.function.body.map(|x| x.into()),
    }, context);
}

struct ClassOrObjectMethod<'a> {
    node: Node<'a>,
    parameters_span: Option<Span>,
    decorators: Option<&'a Vec<&'a Decorator<'a>>>,
    accessibility: Option<Accessibility>,
    is_static: bool,
    is_async: bool,
    is_abstract: bool,
    kind: ClassOrObjectMethodKind,
    is_generator: bool,
    is_optional: bool,
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

fn parse_class_or_object_method<'a>(node: ClassOrObjectMethod<'a>, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    if let Some(decorators) = node.decorators.as_ref() {
        items.extend(parse_decorators(decorators, false, context));
    }

    let start_header_info = Info::new("methodStartHeaderInfo");
    items.push_info(start_header_info);

    if let Some(accessibility) = node.accessibility {
        items.push_str(&format!("{} ", accessibility_to_str(accessibility)));
    }
    if node.is_static { items.push_str("static "); }
    if node.is_abstract { items.push_str("abstract "); }
    if node.is_async { items.push_str("async "); }

    match node.kind {
        ClassOrObjectMethodKind::Getter => items.push_str("get "),
        ClassOrObjectMethodKind::Setter => items.push_str("set "),
        ClassOrObjectMethodKind::Method | ClassOrObjectMethodKind::Constructor => {},
    }

    if node.is_generator { items.push_str("*"); }
    items.extend(parse_node(node.key, context));
    if node.is_optional { items.push_str("?"); }
    if let Some(type_params) = node.type_params { items.extend(parse_node(type_params, context)); }
    if get_use_space_before_parens(&node.kind, context) { items.push_str(" ") }

    let param_count = node.params.len();
    items.extend(parse_parameters_or_arguments(ParseParametersOrArgumentsOptions {
        node: node.node,
        span: node.parameters_span,
        nodes: node.params.into_iter().map(|node| node.into()).collect(),
        custom_close_paren: {
            let return_type = node.return_type;
            move |context| Some(parse_close_paren_with_type(ParseCloseParenWithTypeOptions {
                start_info: start_header_info,
                type_node: return_type,
                type_node_separator: None,
                param_count,
            }, context))
        },
        is_parameters: true,
    }, context));

    if let Some(body) = node.body {
        let brace_position = get_brace_position(&node.kind, context);
        items.extend(parse_brace_separator(ParseBraceSeparatorOptions {
            brace_position: brace_position,
            open_brace_token: context.token_finder.get_first_open_brace_token_within(&body),
            start_header_info: Some(start_header_info),
        }, context));
        items.extend(parse_node(body, context));
    } else if context.config.semi_colons.is_true() {
        items.push_str(";");
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

fn parse_block_stmt<'a>(node: &'a BlockStmt, context: &mut Context<'a>) -> PrintItems {
    parse_block(|stmts, context| {
        parse_statements(
            node.get_inner_span(context),
            stmts,
            context
        )
    }, ParseBlockOptions {
        span: Some(node.span()),
        children: node.stmts.iter().map(|x| x.into()).collect(),
    }, context)
}

fn parse_break_stmt<'a>(node: &'a BreakStmt, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();

    items.push_str("break");
    if let Some(label) = node.label {
        items.push_str(" ");
        items.extend(parse_node(label.into(), context));
    }
    if context.config.semi_colons.is_true() {
        items.push_str(";");
    }

    items
}

fn parse_continue_stmt<'a>(node: &'a ContinueStmt, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();

    items.push_str("continue");
    if let Some(label) = node.label {
        items.push_str(" ");
        items.extend(parse_node(label.into(), context));
    }
    if context.config.semi_colons.is_true() {
        items.push_str(";");
    }

    items
}

fn parse_debugger_stmt<'a>(_: &'a DebuggerStmt, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();

    items.push_str("debugger");
    if context.config.semi_colons.is_true() {
        items.push_str(";");
    }

    items
}

fn parse_do_while_stmt<'a>(node: &'a DoWhileStmt, context: &mut Context<'a>) -> PrintItems {
    // the braces are technically optional on do while statements
    let mut items = PrintItems::new();
    items.push_str("do");
    items.extend(parse_brace_separator(ParseBraceSeparatorOptions {
        brace_position: context.config.do_while_statement_brace_position,
        open_brace_token: if let Stmt::Block(_) = node.body { context.token_finder.get_first_open_brace_token_within(node) } else { None },
        start_header_info: None,
    }, context));
    items.extend(parse_node(node.body.into(), context));
    items.push_str(" while");
    if context.config.do_while_statement_space_after_while_keyword {
        items.push_str(" ");
    }
    items.extend(parse_node_in_parens(
        |context| parse_node(node.test.into(), context),
        ParseNodeInParensOptions {
            inner_span: node.test.span(),
            prefer_hanging: context.config.do_while_statement_prefer_hanging,
            allow_open_paren_trailing_comments: false,
        },
        context
    ));
    if context.config.semi_colons.is_true() {
        items.push_str(";");
    }
    return items;
}

fn parse_export_all<'a>(node: &'a ExportAll, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    items.push_str("export * from ");
    items.extend(parse_node(node.src.into(), context));

    if let Some(asserts) = node.asserts {
        items.push_str(" assert ");
        items.extend(parse_node(asserts.into(), context));
    }

    if context.config.semi_colons.is_true() {
        items.push_str(";");
    }

    items
}

fn parse_empty_stmt(_: &EmptyStmt, _: &mut Context) -> PrintItems {
    ";".into()
}

fn parse_export_assignment<'a>(node: &'a TsExportAssignment, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();

    items.push_str("export");
    items.extend(parse_assignment(node.expr.into(), "=", context));
    if context.config.semi_colons.is_true() {
        items.push_str(";");
    }

    items
}

fn parse_namespace_export<'a>(node: &'a TsNamespaceExportDecl, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    items.push_str("export as namespace ");
    items.extend(parse_node(node.id.into(), context));

    if context.config.semi_colons.is_true() {
        items.push_str(";");
    }

    items
}

fn parse_expr_stmt<'a>(stmt: &'a ExprStmt, context: &mut Context<'a>) -> PrintItems {
    if context.config.semi_colons.is_true() {
        return parse_inner(&stmt, context);
    } else {
        return parse_for_prefix_semi_colon_insertion(&stmt, context);
    }

    fn parse_inner<'a>(stmt: &'a ExprStmt, context: &mut Context<'a>) -> PrintItems {
        let mut items = PrintItems::new();
        items.extend(parse_node(stmt.expr.into(), context));
        if context.config.semi_colons.is_true() {
            items.push_str(";");
        }
        return items;
    }

    fn parse_for_prefix_semi_colon_insertion<'a>(stmt: &'a ExprStmt, context: &mut Context<'a>) -> PrintItems {
        let parsed_node = parse_inner(&stmt, context);
        let parsed_node = parsed_node.into_rc_path();
        return if should_add_semi_colon(&parsed_node).unwrap_or(false) {
            let mut items = PrintItems::new();
            items.push_str(";");
            items.extend(parsed_node.into());
            items
        } else {
            parsed_node.into()
        };

        fn should_add_semi_colon(path: &Option<PrintItemPath>) -> Option<bool> {
            // todo: this needs to be improved
            if let Some(path) = path {
                for item in PrintItemsIterator::new(path.clone()) {
                    match item {
                        PrintItem::String(value) => {
                            if let Some(c) = value.text.chars().next() {
                                return utils::is_prefix_semi_colon_insertion_char(c).into();
                            }
                        },
                        PrintItem::Condition(condition) => {
                            // It's an assumption here that the true and false paths of the
                            // condition will both contain the same text to look for. This is probably not robust
                            // and perhaps instead there should be a way to do something like "get the next character" in
                            // the printer.
                            if let Some(result) = should_add_semi_colon(&condition.get_true_path()) {
                                return Some(result);
                            }
                            if let Some(result) = should_add_semi_colon(&condition.get_false_path()) {
                                return Some(result);
                            }
                        },
                        PrintItem::RcPath(items) => {
                            if let Some(result) = should_add_semi_colon(&Some(items)) {
                                return Some(result);
                            }
                        },
                        _ => { /* do nothing */ },
                    }
                }
            }

            None
        }
    }
}

fn parse_for_stmt<'a>(node: &'a ForStmt, context: &mut Context<'a>) -> PrintItems {
    let start_header_info = Info::new("startHeader");
    let end_header_info = Info::new("endHeader");
    let first_inner_node = {
        if let Some(init) = &node.init {
            init.span()
        } else {
            node.tokens_fast(context.module).iter().find(|t| t.token == Token::Semi).expect("Expected to find a semi-colon in for stmt.").span
        }
    };
    let last_inner_node = {
        if let Some(update) = &node.update {
            update.span()
        } else if let Some(test) = &node.test {
            context.token_finder.get_first_semi_colon_after(&test.span()).expect("Expected to find second semi-colon in for stmt.").span
        } else if let Some(init) = &node.init {
            let first_semi_colon = context.token_finder.get_first_semi_colon_after(init).expect("Expected to find a semi-colon in for stmt.");
            context.token_finder.get_first_semi_colon_after(&first_semi_colon.span).expect("Expected to find second semi-colon in for stmt.").span
        } else {
            context.token_finder.get_first_semi_colon_after(&first_inner_node).expect("Expected to find second semi-colon in for stmt.").span
        }
    };
    let force_use_new_lines = get_use_new_lines(&first_inner_node, context);
    let mut items = PrintItems::new();
    items.push_info(start_header_info);
    items.push_str("for");
    if context.config.for_statement_space_after_for_keyword {
        items.push_str(" ");
    }

    let separator_after_semi_colons = if context.config.for_statement_space_after_semi_colons { Signal::SpaceOrNewLine } else { Signal::PossibleNewLine };
    let parsed_init = parser_helpers::new_line_group({
        let mut items = PrintItems::new();
        if let Some(init) = &node.init {
            items.extend(parse_node(init.into(), context));
        }
        items.push_str(";");
        if node.test.is_none() { items.push_str(";"); }
        items
    });
    let parsed_test = if let Some(test) = &node.test {
        Some(parser_helpers::new_line_group({
            let mut items = PrintItems::new();
            items.extend(parse_node(test.into(), context));
            items.push_str(";");
            items
        }))
    } else {
        None
    };
    let parsed_update = if let Some(update) = &node.update {
        Some(parser_helpers::new_line_group(parse_node(update.into(), context)).into())
    } else {
        None
    };

    items.extend(parse_node_in_parens(
        |context| {
            parser_helpers::parse_separated_values(move |_| {
                let mut parsed_nodes = Vec::new();
                parsed_nodes.push(parser_helpers::ParsedValue::from_items(parsed_init));
                if let Some(parsed_test) = parsed_test { parsed_nodes.push(parser_helpers::ParsedValue::from_items(parsed_test)); }
                if let Some(parsed_update) = parsed_update { parsed_nodes.push(parser_helpers::ParsedValue::from_items(parsed_update)); }
                parsed_nodes
            }, parser_helpers::ParseSeparatedValuesOptions {
                prefer_hanging: context.config.for_statement_prefer_hanging,
                force_use_new_lines,
                allow_blank_lines: false,
                single_line_space_at_start: false,
                single_line_space_at_end: false,
                single_line_separator: separator_after_semi_colons.into(),
                indent_width: context.config.indent_width,
                multi_line_options: parser_helpers::MultiLineOptions::same_line_no_indent(),
                force_possible_newline_at_start: false,
            }).items
        },
        ParseNodeInParensOptions {
            inner_span: create_span(first_inner_node.lo(), last_inner_node.hi()),
            prefer_hanging: context.config.for_statement_prefer_hanging,
            allow_open_paren_trailing_comments: false,
        },
        context
    ));

    items.push_info(end_header_info);

    items.extend(parse_conditional_brace_body(ParseConditionalBraceBodyOptions {
        parent: node.span(),
        body_node: node.body.into(),
        use_braces: context.config.for_statement_use_braces,
        brace_position: context.config.for_statement_brace_position,
        single_body_position: Some(context.config.for_statement_single_body_position),
        requires_braces_condition_ref: None,
        header_start_token: None,
        start_header_info: Some(start_header_info),
        end_header_info: Some(end_header_info),
    }, context).parsed_node);

    return items;

    fn get_use_new_lines<'a>(node: &dyn Spanned, context: &mut Context<'a>) -> bool {
        if context.config.for_statement_prefer_single_line {
            return false;
        }

        let open_paren_token = context.token_finder.get_previous_token_if_open_paren(node);
        if let Some(open_paren_token) = open_paren_token {
            node_helpers::get_use_new_lines_for_nodes(open_paren_token, node, context.module)
        } else {
            false
        }
    }
}

fn parse_for_in_stmt<'a>(node: &'a ForInStmt, context: &mut Context<'a>) -> PrintItems {
    let start_header_info = Info::new("startHeader");
    let end_header_info = Info::new("endHeader");
    let mut items = PrintItems::new();
    items.push_info(start_header_info);
    items.push_str("for");
    if context.config.for_in_statement_space_after_for_keyword {
        items.push_str(" ");
    }
    let inner_header_span = create_span(node.left.lo(), node.right.hi());
    items.extend(parse_node_in_parens(
        |context| {
            let mut items = PrintItems::new();
            items.extend(parse_node(node.left.into(), context));
            items.push_signal(Signal::SpaceOrNewLine);
            items.push_condition(conditions::indent_if_start_of_line({
                let mut items = PrintItems::new();
                items.push_str("in ");
                items.extend(parse_node(node.right.into(), context));
                items
            }));
            items
        },
        ParseNodeInParensOptions {
            inner_span: inner_header_span,
            prefer_hanging: context.config.for_in_statement_prefer_hanging,
            allow_open_paren_trailing_comments: false,
        },
        context
    ));
    items.push_info(end_header_info);

    items.extend(parse_conditional_brace_body(ParseConditionalBraceBodyOptions {
        parent: node.span(),
        body_node: node.body.into(),
        use_braces: context.config.for_in_statement_use_braces,
        brace_position: context.config.for_in_statement_brace_position,
        single_body_position: Some(context.config.for_in_statement_single_body_position),
        requires_braces_condition_ref: None,
        header_start_token: None,
        start_header_info: Some(start_header_info),
        end_header_info: Some(end_header_info),
    }, context).parsed_node);

    return items;
}

fn parse_for_of_stmt<'a>(node: &'a ForOfStmt, context: &mut Context<'a>) -> PrintItems {
    let start_header_info = Info::new("startHeader");
    let end_header_info = Info::new("endHeader");
    let mut items = PrintItems::new();
    items.push_info(start_header_info);
    items.push_str("for");
    if context.config.for_of_statement_space_after_for_keyword {
        items.push_str(" ");
    }
    if let Some(_) = node.await_token() {
        // todo: parse comments around await token span
        items.push_str("await ");
    }
    let inner_header_span = create_span(node.left.lo(), node.right.hi());
    items.extend(parse_node_in_parens(
        |context| {
            let mut items = PrintItems::new();
            items.extend(parse_node(node.left.into(), context));
            items.push_signal(Signal::SpaceOrNewLine);
            items.push_condition(conditions::indent_if_start_of_line({
                let mut items = PrintItems::new();
                items.push_str("of ");
                items.extend(parse_node(node.right.into(), context));
                items
            }));
            items
        },
        ParseNodeInParensOptions {
            inner_span: inner_header_span,
            prefer_hanging: context.config.for_of_statement_prefer_hanging,
            allow_open_paren_trailing_comments: false,
        },
        context
    ));
    items.push_info(end_header_info);

    items.extend(parse_conditional_brace_body(ParseConditionalBraceBodyOptions {
        parent: node.span(),
        body_node: node.body.into(),
        use_braces: context.config.for_of_statement_use_braces,
        brace_position: context.config.for_of_statement_brace_position,
        single_body_position: Some(context.config.for_of_statement_single_body_position),
        requires_braces_condition_ref: None,
        header_start_token: None,
        start_header_info: Some(start_header_info),
        end_header_info: Some(end_header_info),
    }, context).parsed_node);

    return items;
}

fn parse_if_stmt<'a>(node: &'a IfStmt, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    let cons = node.cons;
    let cons_span = cons.span();
    let result = parse_header_with_conditional_brace_body(ParseHeaderWithConditionalBraceBodyOptions {
        parent: node.span(),
        body_node: cons.into(),
        parsed_header: {
            let mut items = PrintItems::new();
            items.push_str("if");
            if context.config.if_statement_space_after_if_keyword { items.push_str(" "); }
            let test = node.test;
            items.extend(parse_node_in_parens(
                |context| parse_node(test.into(), context),
                ParseNodeInParensOptions {
                    inner_span: test.span(),
                    prefer_hanging: context.config.if_statement_prefer_hanging,
                    allow_open_paren_trailing_comments: false,
                },
                context
            ));
            items
        },
        use_braces: context.config.if_statement_use_braces,
        brace_position: context.config.if_statement_brace_position,
        single_body_position: Some(context.config.if_statement_single_body_position),
        requires_braces_condition_ref: context.take_if_stmt_last_brace_condition_ref(),
    }, context);
    let if_stmt_start_info = Info::new("ifStmtStart");

    items.push_info(if_stmt_start_info);
    items.extend(result.parsed_node);

    if let Some(alt) = node.alt {
        if let Stmt::If(alt_alt) = alt {
            if alt_alt.alt.is_none() {
                context.store_if_stmt_last_brace_condition_ref(result.open_brace_condition_ref);
            }
        }

        items.extend(parse_control_flow_separator(
            context.config.if_statement_next_control_flow_position,
            &cons_span,
            "else",
            if_stmt_start_info,
            Some(result.close_brace_condition_ref),
            context
        ));

        // parse the leading comments before the else keyword
        let else_keyword = node.children_with_tokens_fast(context.module).iter().find(|n| match n {
            NodeOrToken::Token(token) => token.text_fast(context.module) == "else",
            _ => false
        }).expect("Expected to find an else keyword.").unwrap_token();
        items.extend(parse_leading_comments(else_keyword, context));
        items.extend(parse_leading_comments(&alt, context));

        let start_else_header_info = Info::new("startElseHeader");
        items.push_info(start_else_header_info);
        items.push_str("else");

        if let Stmt::If(alt) = alt {
            items.push_str(" ");
            items.extend(parse_node(alt.into(), context));
        } else {
            items.extend(parse_conditional_brace_body(ParseConditionalBraceBodyOptions {
                parent: node.span(),
                body_node: alt.into(),
                use_braces: context.config.if_statement_use_braces,
                brace_position: context.config.if_statement_brace_position,
                single_body_position: Some(context.config.if_statement_single_body_position),
                requires_braces_condition_ref: Some(result.open_brace_condition_ref),
                header_start_token: Some(else_keyword),
                start_header_info: Some(start_else_header_info),
                end_header_info: None,
            }, context).parsed_node);
        }
    }

    return items;
}

fn parse_labeled_stmt<'a>(node: &'a LabeledStmt, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    items.extend(parse_node(node.label.into(), context));
    items.push_str(":");

    // not bothering to make this configurable, because who uses labeled statements?
    if node.body.kind() == NodeKind::BlockStmt {
        items.push_str(" ");
    } else {
        items.push_signal(Signal::NewLine);
    }

    items.extend(parse_node(node.body.into(), context));

    return items;
}

fn parse_return_stmt<'a>(node: &'a ReturnStmt, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    items.push_str("return");
    if let Some(arg) = &node.arg {
        items.push_str(" ");
        items.extend(parse_node(arg.into(), context));
    }
    if context.config.semi_colons.is_true() { items.push_str(";"); }
    return items;
}

fn parse_switch_stmt<'a>(node: &'a SwitchStmt, context: &mut Context<'a>) -> PrintItems {
    let start_header_info = Info::new("startHeader");
    let mut items = PrintItems::new();
    items.push_info(start_header_info);
    items.push_str("switch ");
    items.extend(parse_node_in_parens(
        |context| parse_node(node.discriminant.into(), context),
        ParseNodeInParensOptions {
            inner_span: node.discriminant.span(),
            prefer_hanging: context.config.switch_statement_prefer_hanging,
            allow_open_paren_trailing_comments: false,
        },
        context
    ));
    items.extend(parse_membered_body(ParseMemberedBodyOptions {
        node: node.into(),
        members: node.cases.iter().map(|&x| x.into()).collect(),
        start_header_info: Some(start_header_info),
        brace_position: context.config.switch_statement_brace_position,
        should_use_blank_line: |previous, next, context| {
            // do not put a blank line when the previous case has no body
            if let Node::SwitchCase(previous) = previous {
                if previous.cons.is_empty() {
                    return false;
                }
            }
            node_helpers::has_separating_blank_line(previous, next, context.module)
        },
        separator: Separator::none(),
    }, context));
    return items;
}

fn parse_switch_case<'a>(node: &'a SwitchCase, context: &mut Context<'a>) -> PrintItems {
    let block_stmt_body = get_block_stmt_body(&node);
    let start_header_info = Info::new("switchCaseStartHeader");
    let mut items = PrintItems::new();
    let colon_token = context.token_finder.get_first_colon_token_after(&if let Some(test) = node.test {
        test.hi()
    } else {
        node.lo()
    }).expect("Expected to find a colon token.");

    items.push_info(start_header_info);

    if let Some(test) = &node.test {
        items.push_str("case ");
        items.extend(parse_node(test.into(), context));
        items.push_str(":");
    } else {
        items.push_str("default:");
    }

    items.extend(parse_first_line_trailing_comments(&node.span(), node.cons.get(0).map(|x| x.span()), context));
    let parsed_trailing_comments = parse_trailing_comments_for_case(node, &block_stmt_body, context);
    if !node.cons.is_empty() {
        if let Some(block_stmt_body) = block_stmt_body {
            items.extend(parse_brace_separator(ParseBraceSeparatorOptions {
                brace_position: context.config.switch_case_brace_position,
                open_brace_token: context.token_finder.get_first_open_brace_token_within(&block_stmt_body),
                start_header_info: None,
            }, context));
            items.extend(parse_node(node.cons.iter().next().unwrap().into(), context));
        } else {
            items.push_signal(Signal::NewLine);
            items.extend(parser_helpers::with_indent(parse_statements(
                create_span(colon_token.hi(), node.hi()),
                node.cons.iter().map(|node| node.into()).collect(),
                context,
            )));
        }
    }

    items.extend(parsed_trailing_comments);

    return items;

    fn get_block_stmt_body(node: &SwitchCase) -> Option<Span> {
        let first_cons = node.cons.get(0);
        if let Some(Stmt::Block(block_stmt)) = first_cons {
            if node.cons.len() == 1 {
                return Some(block_stmt.span());
            }
        }
        return None;
    }

    fn parse_trailing_comments_for_case<'a>(node: &'a SwitchCase, block_stmt_body: &Option<Span>, context: &mut Context<'a>) -> PrintItems {
        let node_span = node.span();
        let mut items = PrintItems::new();
        // parse the trailing comments as statements
        let trailing_comments = get_trailing_comments_as_statements(&node_span, context);
        if !trailing_comments.is_empty() {
            if let Some(Node::SwitchStmt(stmt)) = node.parent() {
                let last_case = stmt.cases.iter().last();
                let is_last_case = match last_case { Some(last_case) => last_case.lo() == node_span.lo, _=> false };
                let mut is_equal_indent = block_stmt_body.is_some();
                let mut last_node = node_span;

                for comment in trailing_comments {
                    is_equal_indent = is_equal_indent || comment.start_column_fast(context.module) <= last_node.start_column_fast(context.module);
                    let parsed_comment = parse_comment_based_on_last_node(&comment, &Some(&last_node), ParseCommentBasedOnLastNodeOptions {
                        separate_with_newlines: true
                    }, context);

                    items.extend(if !is_last_case && is_equal_indent {
                        parsed_comment
                    } else {
                        parser_helpers::with_indent(parsed_comment)
                    });
                    last_node = comment.span();
                }
            }
        }
        return items;
    }
}

fn parse_throw_stmt<'a>(node: &'a ThrowStmt, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    items.push_str("throw ");
    items.extend(parse_node(node.arg.into(), context));
    if context.config.semi_colons.is_true() { items.push_str(";"); }
    return items;
}

fn parse_try_stmt<'a>(node: &'a TryStmt, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    let brace_position = context.config.try_statement_brace_position;
    let next_control_flow_position = context.config.try_statement_next_control_flow_position;
    let mut last_block_span = node.block.span();
    let mut last_block_start_info = Info::new("tryStart");

    items.push_info(last_block_start_info);
    items.push_str("try");

    items.extend(parse_conditional_brace_body(ParseConditionalBraceBodyOptions {
        parent: node.span(),
        body_node: node.block.into(),
        use_braces: UseBraces::Always, // braces required
        brace_position: context.config.try_statement_brace_position,
        single_body_position: Some(SingleBodyPosition::NextLine),
        requires_braces_condition_ref: None,
        header_start_token: None,
        start_header_info: None,
        end_header_info: None,
    }, context).parsed_node);

    if let Some(handler) = node.handler {
        let handler_start_info = Info::new("handlerStart");
        items.push_info(handler_start_info);
        items.extend(parse_control_flow_separator(
            next_control_flow_position,
            &last_block_span,
            "catch",
            last_block_start_info,
            None,
            context
        ));
        last_block_span = handler.span();
        items.extend(parse_node(handler.into(), context));

        // set the next block to check the handler start info
        last_block_start_info = handler_start_info;
    }

    if let Some(finalizer) = node.finalizer {
        items.extend(parse_control_flow_separator(
            next_control_flow_position,
            &last_block_span,
            "finally",
            last_block_start_info,
            None,
            context
        ));
        items.push_str("finally");
        items.extend(parse_conditional_brace_body(ParseConditionalBraceBodyOptions {
            parent: node.span(),
            body_node: finalizer.into(),
            use_braces: UseBraces::Always, // braces required
            brace_position,
            single_body_position: Some(SingleBodyPosition::NextLine),
            requires_braces_condition_ref: None,
            header_start_token: None,
            start_header_info: None,
            end_header_info: None,
        }, context).parsed_node);
    }

    return items;
}

fn parse_var_decl<'a>(node: &'a VarDecl, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    let force_use_new_lines = get_use_new_lines(&node.decls, context);
    if node.declare() { items.push_str("declare "); }
    items.push_str(match node.kind() {
        VarDeclKind::Const => "const ",
        VarDeclKind::Let => "let ",
        VarDeclKind::Var => "var ",
    });

    let decls_len = node.decls.len();
    if decls_len == 1 {
        // be lightweight by default
        items.extend(parse_node(node.decls[0].into(), context));
    } else if decls_len > 1 {
        items.extend(parse_separated_values(ParseSeparatedValuesParams {
            nodes: node.decls.iter().map(|&p| NodeOrSeparator::Node(p.into())).collect(),
            prefer_hanging: context.config.variable_statement_prefer_hanging,
            force_use_new_lines,
            allow_blank_lines: false,
            separator: TrailingCommas::Never.into(),
            single_line_space_at_start: false,
            single_line_space_at_end: false,
            custom_single_line_separator: None,
            multi_line_options: parser_helpers::MultiLineOptions::same_line_start_hanging_indent(),
            force_possible_newline_at_start: false,
            node_sorter: None,
        }, context));
    }

    if requires_semi_colon(node, context) { items.push_str(";"); }

    return items;

    fn requires_semi_colon(node: &VarDecl, context: &mut Context) -> bool {
        let use_semi_colons = context.config.semi_colons.is_true();
        use_semi_colons && match node.parent().unwrap() {
            Node::ForInStmt(node) => node.lo() >= node.body.lo(),
            Node::ForOfStmt(node) => node.lo() >= node.body.lo(),
            Node::ForStmt(node) => node.lo() >= node.body.lo(),
            _ => use_semi_colons,
        }
    }

    fn get_use_new_lines<'a>(decls: &Vec<&'a VarDeclarator>, context: &mut Context) -> bool {
        get_use_new_lines_for_nodes(decls, context.config.variable_statement_prefer_single_line, context)
    }
}

fn parse_var_declarator<'a>(node: &'a VarDeclarator, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();

    items.extend(parse_node(node.name.into(), context));

    if let Some(init) = &node.init {
        items.extend(parse_assignment(init.into(), "=", context));
    }

    // Indent the first variable declarator when there are multiple.
    // Not ideal, but doing this here because of the abstraction used in
    // `parse_var_decl`. In the future this should probably be moved away.
    if let Node::VarDecl(var_dec) = node.parent().unwrap() {
        if var_dec.decls.len() > 1 && var_dec.decls[0].span() == node.span() {
            let items = items.into_rc_path();
            if_true_or(
                "indentIfNotStartOfLine",
                |context| Some(!condition_resolvers::is_start_of_line(context)),
                with_indent(items.clone().into()),
                items.into(),
            ).into()
        } else {
            items
        }
    } else {
        items
    }
}

fn parse_while_stmt<'a>(node: &'a WhileStmt, context: &mut Context<'a>) -> PrintItems {
    let start_header_info = Info::new("startHeader");
    let end_header_info = Info::new("endHeader");
    let mut items = PrintItems::new();
    items.push_info(start_header_info);
    items.push_str("while");
    if context.config.while_statement_space_after_while_keyword {
        items.push_str(" ");
    }
    items.extend(parse_node_in_parens(
        |context| parse_node(node.test.into(), context),
        ParseNodeInParensOptions {
            inner_span: node.test.span(),
            prefer_hanging: context.config.while_statement_prefer_hanging,
            allow_open_paren_trailing_comments: false,
        },
        context
    ));
    items.push_info(end_header_info);
    items.extend(parse_conditional_brace_body(ParseConditionalBraceBodyOptions {
        parent: node.span(),
        body_node: node.body.into(),
        use_braces: context.config.while_statement_use_braces,
        brace_position: context.config.while_statement_brace_position,
        single_body_position: Some(context.config.while_statement_single_body_position),
        requires_braces_condition_ref: None,
        header_start_token: None,
        start_header_info: Some(start_header_info),
        end_header_info: Some(end_header_info),
    }, context).parsed_node);
    return items;
}

/* types */

fn parse_array_type<'a>(node: &'a TsArrayType, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    items.extend(parse_node(node.elem_type.into(), context));
    items.push_str("[]");
    return items;
}

fn parse_conditional_type<'a>(node: &'a TsConditionalType, context: &mut Context<'a>) -> PrintItems {
    let use_new_lines = !context.config.conditional_type_prefer_single_line
        && node_helpers::get_use_new_lines_for_nodes(&node.true_type, &node.false_type, context.module);
    let top_most_data = get_top_most_data(node, context);
    let is_parent_conditional_type = node.parent().unwrap().kind() == NodeKind::TsConditionalType;
    let mut items = PrintItems::new();
    let before_false_info = Info::new("beforeFalse");

    // main area
    items.extend(parser_helpers::new_line_group(parse_node(node.check_type.into(), context)));
    items.push_str(" extends"); // do not newline before because it's a parsing error
    items.push_signal(Signal::SpaceOrNewLine);

    if top_most_data.is_top_most {
        items.push_info(top_most_data.top_most_info);
    }

    items.push_condition(conditions::indent_if_start_of_line(parser_helpers::new_line_group(parse_node(node.extends_type.into(), context))));
    items.push_signal(Signal::SpaceOrNewLine);
    items.push_condition(conditions::indent_if_start_of_line({
        let mut items = PrintItems::new();
        items.push_str("? ");
        items.extend(parser_helpers::new_line_group(parse_node(node.true_type.into(), context)));
        items
    }));

    // false type
    if use_new_lines {
        items.push_signal(Signal::NewLine);
    } else {
        items.push_condition(conditions::new_line_if_multiple_lines_space_or_new_line_otherwise(top_most_data.top_most_info, Some(before_false_info)));
    }

    let false_type_parsed = {
        let mut items = PrintItems::new();
        items.push_info(before_false_info);
        items.push_str(": ");
        items.extend(parser_helpers::new_line_group(parse_node(node.false_type.into(), context)));
        items
    };

    if is_parent_conditional_type {
        items.extend(false_type_parsed);
    } else {
        items.push_condition(conditions::indent_if_start_of_line(false_type_parsed));
    }

    return items;

    struct TopMostData {
        top_most_info: Info,
        is_top_most: bool,
    }

    fn get_top_most_data(node: &TsConditionalType, context: &mut Context) -> TopMostData {
        // todo: consolidate with conditional expression
        // The "top most" node in nested conditionals follows the ancestors up through
        // the false expressions.
        let mut top_most_node = node;

        for ancestor in context.parent_stack.iter() {
            if let Node::TsConditionalType(parent) = ancestor {
                if parent.false_type.lo() == top_most_node.lo() {
                    top_most_node = parent;
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        let is_top_most = top_most_node.span() == node.span();
        let top_most_info = get_or_set_top_most_info(top_most_node.lo(), is_top_most, context);

        return TopMostData {
            is_top_most,
            top_most_info,
        };

        fn get_or_set_top_most_info(top_most_expr_start: BytePos, is_top_most: bool, context: &mut Context) -> Info {
            if is_top_most {
                let info = Info::new("conditionalTypeStart");
                context.store_info_for_node(&top_most_expr_start, info);
                info
            } else {
                context.get_info_for_node(&top_most_expr_start).expect("Expected to have the top most expr info stored")
            }
        }
    }
}

fn parse_constructor_type<'a>(node: &'a TsConstructorType, context: &mut Context<'a>) -> PrintItems {
    let start_info = Info::new("startConstructorType");
    let mut items = PrintItems::new();
    items.push_info(start_info);
    if node.is_abstract() { items.push_str("abstract "); }
    items.push_str("new");
    if context.config.constructor_type_space_after_new_keyword { items.push_str(" "); }
    if let Some(type_params) = node.type_params {
        items.extend(parse_node(type_params.into(), context));
    }

    items.extend(parse_parameters_or_arguments(ParseParametersOrArgumentsOptions {
        node: node.into(),
        span: node.get_parameters_span(context),
        nodes: node.params.iter().map(|node| node.into()).collect(),
        custom_close_paren: |context| Some(parse_close_paren_with_type(ParseCloseParenWithTypeOptions {
            start_info,
            type_node: Some(node.type_ann.into()),
            type_node_separator: Some({
                let mut items = PrintItems::new();
                items.push_str(" =>");
                items.push_signal(Signal::SpaceIfNotTrailing);
                items.push_signal(Signal::PossibleNewLine);
                items
            }),
            param_count: node.params.len(),
        }, context)),
        is_parameters: true,
    }, context));

    items
}

fn parse_function_type<'a>(node: &'a TsFnType, context: &mut Context<'a>) -> PrintItems {
    let start_info = Info::new("startFunctionType");
    let mut items = PrintItems::new();
    let mut indent_after_arrow_condition = if_true(
        "indentIfIsStartOfLineAfterArrow",
        |context| Some(condition_resolvers::is_start_of_line(&context)),
        Signal::StartIndent.into()
    );
    let indent_after_arrow_condition_ref = indent_after_arrow_condition.get_reference();

    items.push_info(start_info);
    if let Some(type_params) = node.type_params {
        items.extend(parse_node(type_params.into(), context));
    }
    items.extend(parse_parameters_or_arguments(ParseParametersOrArgumentsOptions {
        node: node.into(),
        span: node.get_parameters_span(context),
        nodes: node.params.iter().map(|node| node.into()).collect(),
        custom_close_paren: |context| Some(parse_close_paren_with_type(ParseCloseParenWithTypeOptions {
            start_info,
            type_node: Some(node.type_ann.into()),
            type_node_separator: {
                let mut items = PrintItems::new();
                items.push_str(" =>");
                items.push_signal(Signal::SpaceIfNotTrailing);
                items.push_signal(Signal::PossibleNewLine);
                items.push_condition(indent_after_arrow_condition);
                Some(items)
            },
            param_count: node.params.len(),
        }, context)),
        is_parameters: true,
    }, context));

    items.push_condition(if_true(
        "shouldFinishIndent",
        move |context| context.get_resolved_condition(&indent_after_arrow_condition_ref),
        Signal::FinishIndent.into()
    ));

    return items;
}

fn parse_keyword_type<'a>(node: &'a TsKeywordType, context: &mut Context<'a>) -> PrintItems {
    // will be a keyword like "any", "unknown", "number", etc...
    node.text_fast(context.module).into()
}

fn parse_import_type<'a>(node: &'a TsImportType, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    items.push_str("import(");
    items.extend(parse_node(node.arg.into(), context));
    items.push_str(")");

    if let Some(qualifier) = &node.qualifier {
        items.push_str(".");
        items.extend(parse_node(qualifier.into(), context));
    }

    if let Some(type_args) = node.type_args {
        items.extend(parse_node(type_args.into(), context));
    }
    return items;
}

fn parse_indexed_access_type<'a>(node: &'a TsIndexedAccessType, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    items.extend(parse_node(node.obj_type.into(), context));
    items.extend(parse_computed_prop_like(|context| parse_node(node.index_type.into(), context), ParseComputedPropLikeOptions {
        inner_node_span: node.index_type.span(),
    }, context));
    return items;
}

fn parse_infer_type<'a>(node: &'a TsInferType, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    items.push_str("infer ");
    items.extend(parse_node(node.type_param.into(), context));
    return items;
}

fn parse_intersection_type<'a>(node: &'a TsIntersectionType, context: &mut Context<'a>) -> PrintItems {
    parse_union_or_intersection_type(UnionOrIntersectionType {
        node: node.into(),
        types: &node.types,
        is_union: false,
    }, context)
}

fn parse_lit_type<'a>(node: &'a TsLitType, context: &mut Context<'a>) -> PrintItems {
    match &node.lit {
        // need to do this in order to support negative numbers
        TsLit::Number(_) => node.text_fast(context.module).into(),
        _ => parse_node(node.lit.into(), context)
    }
}

fn parse_mapped_type<'a>(node: &'a TsMappedType, context: &mut Context<'a>) -> PrintItems {
    let force_use_new_lines = !context.config.mapped_type_prefer_single_line && node_helpers::get_use_new_lines_for_nodes(&node.lo(), &node.type_param, context.module);

    let span = node.span();
    let mut items = PrintItems::new();
    let start_info = Info::new("startMappedType");
    items.push_info(start_info);
    items.extend(parse_surrounded_by_tokens(|context| {
        let is_different_line_than_start = move |context: &mut ConditionResolverContext| {
            if force_use_new_lines {
                Some(true)
            } else {
                condition_resolvers::is_on_different_line(context, &start_info)
            }
        };
        let inner_items = {
            let mut items = PrintItems::new();

            // There could be, or could not be a semi-colon here. Parse the second
            // last token's trailing comments in order to get the comments that
            // should always appear after a semi-colon when it appears and potentially
            // steal the inner node's comments.
            let parsed_semi_colon_comments = {
                let node_tokens = node.tokens_fast(context.module);
                parse_trailing_comments(&node_tokens[node_tokens.len() - 2], context)
            };

            if let Some(readonly) = node.readonly() {
                items.push_str(match readonly {
                    TruePlusMinus::True => "readonly ",
                    TruePlusMinus::Plus => "+readonly ",
                    TruePlusMinus::Minus => "-readonly ",
                });
            }

            let computed_inner_span = Span::new(node.type_param.lo(), node.name_type.map(|t| t.hi()).unwrap_or(node.type_param.hi()), Default::default());
            items.extend(parse_computed_prop_like(|context| {
                let mut items = PrintItems::new();
                if let Some(name_type) = node.name_type {
                    items.extend(parse_as_expr_like(AsExprLike {
                        expr: node.type_param.into(),
                        type_ann: name_type.into(),
                    }, context));
                } else {
                    items.extend(parse_node(node.type_param.into(), context));
                }
                items
            }, ParseComputedPropLikeOptions {
                inner_node_span: computed_inner_span,
            }, context));

            if let Some(optional) = node.optional() {
                items.push_str(match optional {
                    TruePlusMinus::True => "?",
                    TruePlusMinus::Plus => "+?",
                    TruePlusMinus::Minus => "-?",
                });
            }

            items.extend(parse_type_ann_with_colon_if_exists_for_type(&node.type_ann, context));
            items.extend(get_parsed_semi_colon(context.config.semi_colons, true, &is_different_line_than_start));
            items.extend(parsed_semi_colon_comments);

            let inner_items = items.into_rc_path();
            if_true_or(
                "noSpacesWhenMultiLine",
                is_different_line_than_start,
                inner_items.clone().into(),
                {
                    let mut items = PrintItems::new();
                    items.push_signal(Signal::SpaceOrNewLine);
                    items.push_optional_path(inner_items);
                    items.push_signal(Signal::SpaceOrNewLine);
                    items
                }
            ).into()
        };

        if force_use_new_lines {
            surround_with_new_lines(with_indent(inner_items))
        } else {
            parser_helpers::surround_with_newlines_indented_if_multi_line(inner_items, context.config.indent_width)
        }
    }, |_| None, ParseSurroundedByTokensOptions {
        open_token: "{",
        close_token: "}",
        span: Some(span),
        first_member: Some(node.type_param.span()),
        prefer_single_line_when_empty: false,
        allow_open_token_trailing_comments: true,
    }, context));
    items
}

fn parse_optional_type<'a>(node: &'a TsOptionalType, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    items.extend(parse_node(node.type_ann.into(), context));
    items.push_str("?");
    return items;
}

fn parse_qualified_name<'a>(node: &'a TsQualifiedName, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    items.extend(parse_node(node.left.into(), context));
    items.push_str(".");
    items.extend(parse_node(node.right.into(), context));
    return items;
}

fn parse_parenthesized_type<'a>(node: &'a TsParenthesizedType, context: &mut Context<'a>) -> PrintItems {
    let parsed_type = conditions::with_indent_if_start_of_line_indented(parse_node_in_parens(
        |context| parse_node(node.type_ann.into(), context),
        ParseNodeInParensOptions {
            inner_span: node.type_ann.span(),
            prefer_hanging: true,
            allow_open_paren_trailing_comments: true,
        },
        context
    )).into();

    return if use_new_line_group(node) { new_line_group(parsed_type) } else { parsed_type };

    fn use_new_line_group(node: &TsParenthesizedType) -> bool {
        match node.parent().unwrap() {
            Node::TsTypeAliasDecl(_) => false,
            _ => true,
        }
    }
}

fn parse_rest_type<'a>(node: &'a TsRestType, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    items.push_str("...");
    items.extend(parse_node(node.type_ann.into(), context));
    return items;
}

fn parse_tpl_lit_type<'a>(node: &'a TsTplLitType, context: &mut Context<'a>) -> PrintItems {
    parse_template_literal(
        node.quasis.iter().map(|&x| x.into()).collect(),
        node.types.iter().map(|x| x.into()).collect(),
        context,
    )
}

fn parse_tuple_type<'a>(node: &'a TsTupleType, context: &mut Context<'a>) -> PrintItems {
    parse_array_like_nodes(ParseArrayLikeNodesOptions {
        node: node.into(),
        nodes: node.elem_types.iter().map(|&x| Some(x.into())).collect(),
        prefer_hanging: context.config.tuple_type_prefer_hanging,
        prefer_single_line: context.config.tuple_type_prefer_single_line,
        trailing_commas: context.config.tuple_type_trailing_commas,
    }, context)
}

fn parse_tuple_element<'a>(node: &'a TsTupleElement, context: &mut Context<'a>) -> PrintItems {
    if let Some(label) = &node.label {
        let mut items = PrintItems::new();
        items.extend(parse_node(label.into(), context));
        items.extend(parse_type_ann_with_colon_for_type(&node.ty, context));
        items
    } else {
        parse_node(node.ty.into(), context)
    }
}

fn parse_type_ann<'a>(node: &'a TsTypeAnn, context: &mut Context<'a>) -> PrintItems {
    parse_node(node.type_ann.into(), context)
}

fn parse_type_param<'a>(node: &'a TsTypeParam, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();

    items.extend(parse_node(node.name.into(), context));

    if let Some(constraint) = &node.constraint {
        items.push_signal(Signal::SpaceOrNewLine);
        items.push_condition(conditions::indent_if_start_of_line({
            let mut items = PrintItems::new();
            items.push_str(if node.parent().unwrap().kind() == NodeKind::TsMappedType {
                "in"
            } else {
                "extends"
            });
            items.push_signal(Signal::SpaceIfNotTrailing);
            items.extend(parse_node(constraint.into(), context));
            items
        }));
    }

    if let Some(default) = &node.default {
        items.extend(parse_assignment(default.into(), "=", context));
    }

    return items;
}

fn parse_type_parameters<'a>(node: TypeParamNode<'a>, context: &mut Context<'a>) -> PrintItems {
    let params = node.params();
    let force_use_new_lines = get_use_new_lines(&node, &params, context);
    let mut items = PrintItems::new();

    items.push_str("<");
    items.extend(parse_separated_values(ParseSeparatedValuesParams {
        nodes: params.into_iter().map(|p| NodeOrSeparator::Node(p)).collect(),
        prefer_hanging: context.config.type_parameters_prefer_hanging,
        force_use_new_lines,
        allow_blank_lines: false,
        separator: get_trailing_commas(&node, context).into(),
        single_line_space_at_start: false,
        single_line_space_at_end: false,
        custom_single_line_separator: None,
        multi_line_options: parser_helpers::MultiLineOptions::surround_newlines_indented(),
        force_possible_newline_at_start: false,
        node_sorter: None,
    }, context));
    items.push_str(">");

    return items;

    fn get_trailing_commas(node: &TypeParamNode, context: &mut Context) -> TrailingCommas {
        let trailing_commas = context.config.type_parameters_trailing_commas;
        if trailing_commas == TrailingCommas::Never { return trailing_commas; }

        // trailing commas should be allowed in type parameters only—not arguments
        if let Some(type_params) = node.parent().get_type_parameters() {
            if type_params.lo() == node.lo() {
                return trailing_commas;
            }
        }

        return TrailingCommas::Never;
    }

    fn get_use_new_lines(node: &TypeParamNode, params: &Vec<Node>, context: &mut Context) -> bool {
        if context.config.type_parameters_prefer_single_line || params.is_empty() {
            false
        } else {
            let first_param = &params[0];
            let angle_bracket_pos = node.lo();
            node_helpers::get_use_new_lines_for_nodes(&angle_bracket_pos, first_param, context.module)
        }
    }
}

fn parse_type_operator<'a>(node: &'a TsTypeOperator, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    items.push_str(match node.op() {
        TsTypeOperatorOp::KeyOf => "keyof",
        TsTypeOperatorOp::Unique => "unique",
        TsTypeOperatorOp::ReadOnly => "readonly",
    });
    items.push_signal(Signal::SpaceIfNotTrailing);
    items.extend(parse_node(node.type_ann.into(), context));
    return items;
}

fn parse_type_predicate<'a>(node: &'a TsTypePredicate, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    if node.asserts() { items.push_str("asserts "); }
    items.extend(parse_node(node.param_name.into(), context));
    if let Some(type_ann) = node.type_ann {
        items.push_str(" is");
        items.push_signal(Signal::SpaceIfNotTrailing);
        items.extend(parse_node(type_ann.into(), context));
    }
    return items;
}

fn parse_type_query<'a>(node: &'a TsTypeQuery, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    items.push_str("typeof");
    items.push_signal(Signal::SpaceIfNotTrailing);
    items.extend(parse_node(node.expr_name.into(), context));
    return items;
}

fn parse_type_reference<'a>(node: &'a TsTypeRef, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    items.extend(parse_node(node.type_name.into(), context));
    if let Some(type_params) = node.type_params {
        items.extend(parse_node(type_params.into(), context));
    }
    return items;
}

fn parse_union_type<'a>(node: &'a TsUnionType, context: &mut Context<'a>) -> PrintItems {
    parse_union_or_intersection_type(UnionOrIntersectionType {
        node: node.into(),
        types: &node.types,
        is_union: true,
    }, context)
}

struct UnionOrIntersectionType<'a> {
    pub node: Node<'a>,
    pub types: &'a Vec<TsType<'a>>,
    pub is_union: bool,
}

fn parse_union_or_intersection_type<'a>(node: UnionOrIntersectionType<'a>, context: &mut Context<'a>) -> PrintItems {
    // todo: configuration for operator position
    let mut items = PrintItems::new();
    let force_use_new_lines = get_use_new_lines_for_nodes(&node.types, context.config.union_and_intersection_type_prefer_single_line, context);
    let separator = if node.is_union { "|" } else { "&" };

    let leading_comments = node.node.span().leading_comments_fast(context.module);
    let has_leading_comments = !leading_comments.is_empty();

    let indent_width = context.config.indent_width;
    let prefer_hanging = context.config.union_and_intersection_type_prefer_hanging;
    let is_parent_union_or_intersection = match node.node.parent().unwrap().kind() {
        NodeKind::TsUnionType | NodeKind::TsIntersectionType => true,
        _ => false,
    };
    let multi_line_options = if !is_parent_union_or_intersection {
        if use_surround_newlines(&node.node) {
            parser_helpers::MultiLineOptions::surround_newlines_indented()
        } else if has_leading_comments {
            parser_helpers::MultiLineOptions::same_line_no_indent()
        } else {
            parser_helpers::MultiLineOptions::new_line_start()
        }
    } else if has_leading_comments {
        parser_helpers::MultiLineOptions::same_line_no_indent()
    } else {
        parser_helpers::MultiLineOptions::same_line_start_hanging_indent()
    };
    let parse_result = parser_helpers::parse_separated_values(|is_multi_line_or_hanging_ref| {
        let is_multi_line_or_hanging = is_multi_line_or_hanging_ref.create_resolver();
        let types_count = node.types.len();
        let mut parsed_nodes = Vec::new();
        for (i, type_node) in node.types.into_iter().enumerate() {
            let (allow_inline_multi_line, allow_inline_single_line) = {
                let is_last_value = i + 1 == types_count; // allow the last type to be single line
                (allows_inline_multi_line(&type_node.into(), types_count > 1), is_last_value)
            };
            let separator_token = context.token_finder.get_previous_token_if_operator(&type_node.span(), separator);
            let start_info = Info::new("startInfo");
            let after_separator_info = Info::new("afterSeparatorInfo");
            let mut items = PrintItems::new();
            items.push_info(start_info);
            if let Some(separator_token) = separator_token {
                items.extend(parse_leading_comments(separator_token, context));
            }
            if i == 0 && !is_parent_union_or_intersection {
                items.push_condition(if_true(
                    "separatorIfMultiLine",
                    is_multi_line_or_hanging.clone(),
                    separator.into(),
                ));
            } else if i > 0 {
                items.push_str(separator);
            }

            if let Some(separator_token) = separator_token {
                items.extend(parse_trailing_comments(separator_token, context));
            }
            items.push_info(after_separator_info);

            items.push_condition(if_true(
                "afterSeparatorSpace",
                move |condition_context| {
                    let is_on_same_line = condition_resolvers::is_on_same_line(condition_context, &after_separator_info)?;
                    let is_at_same_position = condition_resolvers::is_at_same_position(condition_context, &start_info)?;
                    return Some(is_on_same_line && !is_at_same_position);
                },
                Signal::SpaceIfNotTrailing.into(),
            ));
            items.extend(parse_node(type_node.into(), context));

            parsed_nodes.push(parser_helpers::ParsedValue {
                items,
                lines_span: None,
                allow_inline_multi_line,
                allow_inline_single_line,
            });
        }

        parsed_nodes
    }, parser_helpers::ParseSeparatedValuesOptions {
        prefer_hanging,
        force_use_new_lines,
        allow_blank_lines: false,
        single_line_space_at_start: false,
        single_line_space_at_end: false,
        single_line_separator: Signal::SpaceOrNewLine.into(),
        indent_width,
        multi_line_options,
        force_possible_newline_at_start: false,
    });

    items.extend(parse_result.items);

    return items;

    fn use_surround_newlines(node: &Node) -> bool {
        match node.parent().unwrap() {
            Node::TsTypeAssertion(_) | Node::TsParenthesizedType(_) => true,
            _ => false,
        }
    }
}

/* comments */

fn parse_leading_comments<'a>(node: &dyn Spanned, context: &mut Context<'a>) -> PrintItems {
    let leading_comments = node.leading_comments_fast(context.module);
    parse_comments_as_leading(node, leading_comments, context)
}

fn parse_comments_as_leading<'a>(node: &dyn Spanned, comments: CommentsIterator<'a>, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    if let Some(last_comment) = comments.peek_last_comment() {
        let last_comment_previously_handled = context.has_handled_comment(&last_comment);

        items.extend(parse_comment_collection(comments, None, Some(node), context));

        // todo: this doesn't seem exactly right...
        if !last_comment_previously_handled {
            let node_start_line = node.start_line_fast(context.module);
            let last_comment_end_line = last_comment.end_line_fast(context.module);
            if node_start_line > last_comment_end_line {
                items.push_signal(Signal::NewLine);

                if node_start_line - 1 > last_comment_end_line {
                    items.push_signal(Signal::NewLine);
                }
            }
            else if last_comment.kind == CommentKind::Block && node_start_line == last_comment_end_line {
                items.push_signal(Signal::SpaceIfNotTrailing);
            }
        }
    }

    items
}

fn parse_trailing_comments_as_statements<'a>(node: &dyn Spanned, context: &mut Context<'a>) -> PrintItems {
    let unhandled_comments = get_trailing_comments_as_statements(node, context);
    parse_comments_as_statements(unhandled_comments.into_iter(), Some(node), context)
}

fn get_leading_comments_on_previous_lines<'a>(node: &dyn Spanned, context: &mut Context<'a>) -> Vec<&'a Comment> {
    let node_start_line = node.start_line_fast(context.module);
    node.leading_comments_fast(context.module)
        .take_while(|c| c.kind == CommentKind::Line || c.start_line_fast(context.module) < node_start_line)
        .collect::<Vec<_>>()
}

fn get_trailing_comments_as_statements<'a>(node: &dyn Spanned, context: &mut Context<'a>) -> Vec<&'a Comment> {
    let mut comments = Vec::new();
    let node_end_line = node.end_line_fast(context.module);
    for comment in node.trailing_comments_fast(context.module) {
        if !context.has_handled_comment(&comment) && node_end_line < comment.end_line_fast(context.module) {
            comments.push(comment);
        }
    }
    comments
}

fn parse_comments_as_statements<'a>(comments: impl Iterator<Item=&'a Comment>, last_node: Option<&dyn Spanned>, context: &mut Context<'a>) -> PrintItems {
    let mut last_node = last_node;
    let mut items = PrintItems::new();
    for comment in comments {
        if !context.has_handled_comment(comment) {
            items.extend(parse_comment_based_on_last_node(comment, &last_node, ParseCommentBasedOnLastNodeOptions {
                separate_with_newlines: true
            }, context));
            last_node = Some(comment);
        }
    }
    items
}

fn parse_comment_collection<'a>(comments: impl Iterator<Item=&'a Comment>, last_node: Option<&dyn Spanned>, next_node: Option<&dyn Spanned>, context: &mut Context<'a>) -> PrintItems {
    let mut last_node = last_node;
    let mut items = PrintItems::new();
    let next_node_start_line = next_node.map(|n| n.start_line_fast(context.module));
    for comment in comments {
        if !context.has_handled_comment(comment) {
            items.extend(parse_comment_based_on_last_node(comment, &last_node, ParseCommentBasedOnLastNodeOptions {
                separate_with_newlines: if let Some(next_node_start_line) = next_node_start_line {
                    comment.start_line_fast(context.module) != next_node_start_line
                } else {
                    false
                }
            }, context));
            last_node = Some(comment);
        }
    }
    items
}

struct ParseCommentBasedOnLastNodeOptions {
    separate_with_newlines: bool,
}

fn parse_comment_based_on_last_node(comment: &Comment, last_node: &Option<&dyn Spanned>, opts: ParseCommentBasedOnLastNodeOptions, context: &mut Context) -> PrintItems {
    let mut items = PrintItems::new();
    let mut pushed_ignore_new_lines = false;

    if let Some(last_node) = last_node {
        let comment_start_line = comment.start_line_fast(context.module);
        let last_node_end_line = last_node.end_line_fast(context.module);

        if opts.separate_with_newlines || comment_start_line > last_node_end_line {
            items.push_signal(Signal::NewLine);

            if comment_start_line > last_node_end_line + 1 {
                items.push_signal(Signal::NewLine);
            }
        } else if comment.kind == CommentKind::Line {
            items.push_signal(Signal::StartForceNoNewLines);
            items.push_str(" ");
            pushed_ignore_new_lines = true;
        } else if last_node.text_fast(context.module).starts_with("/*") {
            items.push_str(" ");
        }
    }

    if let Some(parsed_comment) = parse_comment(&comment, context) {
        items.extend(parsed_comment);
    }

    if pushed_ignore_new_lines {
        items.push_signal(Signal::FinishForceNoNewLines);
    }

    return items;
}

fn parse_comment(comment: &Comment, context: &mut Context) -> Option<PrintItems> {
    // only parse if handled
    if context.has_handled_comment(comment) {
        return None;
    }

    // mark handled and parse
    context.mark_comment_handled(comment);
    return Some(match comment.kind {
        CommentKind::Block => parse_comment_block(comment),
        CommentKind::Line => parser_helpers::parse_js_like_comment_line(&comment.text, context.config.comment_line_force_space_after_slashes),
    });

    fn parse_comment_block(comment: &Comment) -> PrintItems {
        let mut items = PrintItems::new();
        items.push_str("/*");
        items.extend(parse_raw_string(&comment.text));
        items.push_str("*/");
        items
    }
}

fn parse_first_line_trailing_comments<'a>(node: &dyn Spanned, first_member: Option<Span>, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    let node_start_line = node.start_line_fast(context.module);

    for comment in get_comments(&node, &first_member, context) {
        if comment.start_line_fast(context.module) == node_start_line {
            if let Some(parsed_comment) = parse_comment(comment, context) {
                if comment.kind == CommentKind::Line {
                    items.push_str(" ");
                }
                items.extend(parsed_comment);
            }
        }
    }

    return items;

    fn get_comments<'a>(node: &dyn Spanned, first_member: &Option<Span>, context: &mut Context<'a>) -> Vec<&'a Comment> {
        let mut comments = Vec::new();
        if let Some(first_member) = first_member {
            comments.extend(first_member.leading_comments_fast(context.module));
        }
        comments.extend(node.trailing_comments_fast(context.module));
        return comments;
    }
}

fn parse_trailing_comments<'a>(node: &dyn Spanned, context: &mut Context<'a>) -> PrintItems {
    let trailing_comments = node.trailing_comments_fast(context.module);
    parse_comments_as_trailing(node, trailing_comments, context)
}

fn parse_comments_as_trailing<'a>(node: &dyn Spanned, trailing_comments: CommentsIterator<'a>, context: &mut Context<'a>) -> PrintItems {
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
    let first_unhandled_comment = trailing_comments_on_same_line.iter().filter(|c| !context.has_handled_comment(&c)).next();

    if let Some(first_unhandled_comment) = first_unhandled_comment {
        if first_unhandled_comment.kind == CommentKind::Block {
            items.push_str(" ");
        }
    }

    items.extend(parse_comment_collection(trailing_comments_on_same_line.into_iter(), Some(node), None, context));

    return items;

    fn get_trailing_comments_same_line<'a>(
        node: &dyn Spanned,
        trailing_comments: CommentsIterator<'a>,
        context: &mut Context<'a>,
    ) -> Vec<&'a Comment> {
        // use the roslyn definition of trailing comments
        let node_end_line = node.end_line_fast(context.module);
        let trailing_comments_on_same_line = trailing_comments.into_iter()
            .filter(|c|c.start_line_fast(context.module) <= node_end_line) // less than or equal instead of just equal in order to include "forgotten" comments
            .collect::<Vec<_>>();

        // don't do extra work
        if trailing_comments_on_same_line.is_empty() {
            return trailing_comments_on_same_line;
        }

        // block comments after a comma on the same line as the next token are not considered a trailing comment of this node
        // ex. `a, /* 1 */ b`, the comment belongs to `b` and not `a`
        let comma_end = if node.text_fast(context.module) == "," {
            Some(node.hi())
        } else {
            context.token_finder.get_next_token_if_comma(&node.span()).map(|t| t.hi())
        };
        if let Some(comma_end) = comma_end {
            let next_token_pos = context.token_finder.get_next_token_pos_after(&comma_end);
            let next_token_pos_line_start = next_token_pos.start_line_fast(context.module);
            if next_token_pos_line_start == node_end_line {
                return trailing_comments_on_same_line
                    .into_iter()
                    .filter(|c| c.lo() < comma_end)
                    .collect::<Vec<_>>()
            }
        }
        return trailing_comments_on_same_line;
    }
}

fn get_jsx_empty_expr_comments<'a>(node: &JSXEmptyExpr, context: &mut Context<'a>) -> CommentsIterator<'a> {
    node.hi().leading_comments_fast(context.module)
}

/* helpers */

struct ParseArrayLikeNodesOptions<'a> {
    node: Node<'a>,
    nodes: Vec<Option<Node<'a>>>,
    prefer_hanging: bool,
    prefer_single_line: bool,
    trailing_commas: TrailingCommas,
}

fn parse_array_like_nodes<'a>(opts: ParseArrayLikeNodesOptions<'a>, context: &mut Context<'a>) -> PrintItems {
    let node = opts.node;
    let nodes = opts.nodes;
    let nodes = if nodes.iter().any(|n| n.is_none()) {
        let commas = get_comma_tokens_from_children_with_tokens(&opts.node, context.module);
        nodes.into_iter().enumerate().map(|(i, node)| {
            match node {
                Some(node) => NodeOrSeparator::Node(node),
                None => NodeOrSeparator::Separator(&commas[i]),
            }
        }).collect::<Vec<_>>()
    } else {
        // faster
        nodes.into_iter().map(|n| NodeOrSeparator::Node(n.unwrap())).collect::<Vec<_>>()
    };

    let trailing_commas = if allow_trailing_commas(&nodes) { opts.trailing_commas } else { TrailingCommas::Never };
    let prefer_hanging = opts.prefer_hanging;
    let force_use_new_lines = get_force_use_new_lines(&node, &nodes, opts.prefer_single_line, context);
    let mut items = PrintItems::new();
    let first_member = nodes.get(0).map(|x| x.span());

    items.extend(parse_surrounded_by_tokens(|context| {
        parse_separated_values(ParseSeparatedValuesParams {
            nodes: nodes,
            prefer_hanging,
            force_use_new_lines,
            allow_blank_lines: true,
            separator: trailing_commas.into(),
            single_line_space_at_start: false,
            single_line_space_at_end: false,
            custom_single_line_separator: None,
            multi_line_options: parser_helpers::MultiLineOptions::surround_newlines_indented(),
            force_possible_newline_at_start: false,
            node_sorter: None,
        }, context)
    }, |_| None, ParseSurroundedByTokensOptions {
        open_token: "[",
        close_token: "]",
        span: Some(node.span()),
        first_member,
        prefer_single_line_when_empty: true,
        allow_open_token_trailing_comments: true,
    }, context));

    return items;

    fn get_force_use_new_lines(node: &dyn Spanned, nodes: &Vec<NodeOrSeparator>, prefer_single_line: bool, context: &mut Context) -> bool {
        if prefer_single_line || nodes.is_empty() {
            false
        } else {
            let open_bracket_token = node.tokens_fast(context.module)
                .iter()
                .find(|t| t.token == Token::LBracket)
                .expect("Expected to find an open bracket token.");

            // todo: tests for this (ex. [\n,] -> [\n    ,\n])
            node_helpers::get_use_new_lines_for_nodes(open_bracket_token, &nodes[0], context.module)
        }
    }

    fn allow_trailing_commas(nodes: &Vec<NodeOrSeparator>) -> bool {
        if let Some(NodeOrSeparator::Node(last)) = nodes.last() {
            // this would be a syntax error
            if last.kind() == NodeKind::RestPat {
                return false;
            }
        }
        true
    }
}

struct ParseMemberedBodyOptions<'a, FShouldUseBlankLine> where FShouldUseBlankLine : Fn(&Node, &Node, &mut Context) -> bool {
    node: Node<'a>,
    members: Vec<Node<'a>>,
    start_header_info: Option<Info>,
    brace_position: BracePosition,
    should_use_blank_line: FShouldUseBlankLine,
    separator: Separator,
}

fn parse_membered_body<'a, FShouldUseBlankLine>(
    opts: ParseMemberedBodyOptions<'a, FShouldUseBlankLine>,
    context: &mut Context<'a>
) -> PrintItems
    where FShouldUseBlankLine : Fn(&Node, &Node, &mut Context) -> bool
{
    let mut items = PrintItems::new();
    let child_tokens = get_tokens_from_children_with_tokens(&opts.node, context.module);
    let open_brace_token = child_tokens.iter().find(|t| t.token == Token::LBrace).expect("Expected to find an open brace token.");
    let close_brace_token = child_tokens.iter().rev().find(|t| t.token == Token::RBrace).expect("Expected to find a close brace token.");

    items.extend(parse_brace_separator(ParseBraceSeparatorOptions {
        brace_position: opts.brace_position,
        open_brace_token: Some(open_brace_token),
        start_header_info: opts.start_header_info,
    }, context));

    let should_use_blank_line = opts.should_use_blank_line;
    let separator = opts.separator;

    items.extend(parse_block(|members, context| {
        parse_members(ParseMembersOptions {
            inner_span: create_span(open_brace_token.hi(), close_brace_token.lo()),
            items: members.into_iter().map(|node| (node, None)).collect(),
            should_use_space: None,
            should_use_new_line: None,
            should_use_blank_line,
            separator,
            is_jsx_children: false,
        }, context)
    }, ParseBlockOptions {
        span: Some(create_span(open_brace_token.lo(), close_brace_token.hi())),
        children: opts.members,
    }, context));

    items
}

fn parse_statements<'a>(inner_span: Span, stmts: Vec<Node<'a>>, context: &mut Context<'a>) -> PrintItems {
    let stmt_groups = get_stmt_groups(stmts, context);
    let mut items = PrintItems::new();
    let mut last_node: Option<Span> = None;
    let stmt_group_len = stmt_groups.len();

    for (stmt_group_index, stmt_group) in stmt_groups.into_iter().enumerate() {
        if stmt_group.kind == StmtGroupKind::Imports || stmt_group.kind == StmtGroupKind::Exports {
            // keep the leading comments of the stmt group on the same line
            let comments = get_leading_comments_on_previous_lines(&stmt_group.nodes.first().as_ref().unwrap().lo(), context);
            let last_comment = comments.iter()
                .filter(|c| !context.has_handled_comment(c))
                .last()
                .map(|c| c.span);
            items.extend(parse_comments_as_statements(comments.into_iter(), if let Some(last_node) = &last_node {
                // todo: why can't I just use .as_ref() here?
                Some(last_node)
            } else {
                None
            }, context));
            last_node = last_comment.or(last_node);
        }

        let nodes_len = stmt_group.nodes.len();
        let mut parsed_nodes = Vec::with_capacity(nodes_len);
        let mut parsed_line_separators = utils::VecMap::with_capacity(nodes_len);
        let sorter = get_node_sorter(stmt_group.kind, context);
        let sorted_indexes = match sorter {
            Some(sorter) => Some(get_sorted_indexes(stmt_group.nodes.iter().map(|n| Some(n)), sorter, context)),
            None => None,
        };
        for (i, node) in stmt_group.nodes.into_iter().enumerate() {
            let is_empty_stmt = match node { Node::EmptyStmt(_) => true, _ => false };
            if !is_empty_stmt {
                let mut separator_items = PrintItems::new();
                if let Some(last_node) = &last_node {
                    separator_items.push_signal(Signal::NewLine);
                    if node_helpers::has_separating_blank_line(last_node, &node, context.module) {
                        separator_items.push_signal(Signal::NewLine);
                    }
                    parsed_line_separators.insert(i, separator_items);
                }

                let mut items = PrintItems::new();
                let end_info = Info::new("endStatementInfo");
                context.end_statement_or_member_infos.push(end_info);
                items.extend(parse_node(node.clone(), context));
                items.push_info(end_info);
                parsed_nodes.push(items);
                context.end_statement_or_member_infos.pop();

                last_node = Some(node.span());
            } else {
                let mut items = PrintItems::new();
                let mut comments = node.leading_comments_fast(context.module);
                comments.extend(node.trailing_comments_fast(context.module));
                items.extend(parse_comments_as_statements(comments, None, context));
                parsed_nodes.push(items);

                // ensure if this is last that it parses the trailing comment statements
                if stmt_group_index == stmt_group_len - 1 && i == nodes_len - 1 {
                    last_node = Some(node.span());
                }
            }
        }

        // Get the parsed statements/members sorted
        let parsed_nodes = match sorted_indexes {
            Some(sorted_indexes) => sort_by_sorted_indexes(parsed_nodes, sorted_indexes),
            None => parsed_nodes,
        };

        // Now combine everything
        for (i, parsed_node) in parsed_nodes.into_iter().enumerate() {
            if let Some(parsed_separator) = parsed_line_separators.remove(i) {
                items.extend(parsed_separator);
            }
            items.extend(parsed_node);
        }
    }

    if let Some(last_node) = &last_node {
        items.extend(parse_trailing_comments_as_statements(last_node, context));
    }

    if stmt_group_len == 0 {
        items.extend(parse_comments_as_statements(inner_span.hi.leading_comments_fast(context.module), None, context));
    }

    return items;

    fn get_node_sorter<'a>(
        group_kind: StmtGroupKind,
        context: &Context<'a>,
    ) -> Option<Box<dyn Fn((usize, Option<&Node<'a>>), (usize, Option<&Node<'a>>), &Module<'a>) -> std::cmp::Ordering>> {
        match group_kind {
            StmtGroupKind::Imports => get_node_sorter_from_order(context.config.module_sort_import_declarations),
            StmtGroupKind::Exports => get_node_sorter_from_order(context.config.module_sort_export_declarations),
            StmtGroupKind::Other => None,
        }
    }
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
            Node::ImportDecl(_) => StmtGroupKind::Imports,
            Node::ExportAll(_) => StmtGroupKind::Exports,
            Node::NamedExport(NamedExport {
                src: Some(_),
                ..
            }) => StmtGroupKind::Exports,
            _ => StmtGroupKind::Other,
        };
        previous_last_end_line = match stmt_group_kind {
            StmtGroupKind::Imports | StmtGroupKind::Exports => Some(stmt.end_line_fast(context.module)),
            StmtGroupKind::Other => None,
        };

        if let Some(group) = current_group.as_mut() {
            let is_same_group = group.kind == stmt_group_kind
                && (
                    stmt_group_kind == StmtGroupKind::Other
                    || last_end_line.is_none()
                    || last_end_line.unwrap() + 1 >= stmt.start_line_fast(context.module)
                );
            if is_same_group {
                group.nodes.push(stmt);
            } else {
                groups.push(current_group.take().unwrap());
                current_group = Some(StmtGroup {
                    kind: stmt_group_kind,
                    nodes: vec![stmt]
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

struct ParseMembersOptions<'a, FShouldUseBlankLine> where FShouldUseBlankLine : Fn(&Node, &Node, &mut Context) -> bool {
    inner_span: Span,
    items: Vec<(Node<'a>, Option<PrintItems>)>,
    should_use_space: Option<Box<dyn Fn(&Node, &Node, &mut Context) -> bool>>, // todo: Remove putting functions on heap by using type parameters?
    should_use_new_line: Option<Box<dyn Fn(&Node, &Node, &mut Context) -> bool>>,
    should_use_blank_line: FShouldUseBlankLine,
    separator: Separator,
    is_jsx_children: bool,
}

fn parse_members<'a, FShouldUseBlankLine>(
    opts: ParseMembersOptions<'a, FShouldUseBlankLine>,
    context: &mut Context<'a>
) -> PrintItems where FShouldUseBlankLine : Fn(&Node, &Node, &mut Context) -> bool
{
    let mut last_node: Option<Node> = None;
    let mut items = PrintItems::new();
    let children_len = opts.items.len();

    for (i, (node, optional_print_items)) in opts.items.into_iter().enumerate() {
        // class declarations may have empty statements
        let is_empty_stmt = match node { Node::EmptyStmt(_) => true, _ => false };
        if !is_empty_stmt {
            if let Some(last_node) = last_node {
                if should_use_new_line(&opts.should_use_new_line, &last_node, &node, context) {
                    items.push_signal(Signal::NewLine);

                    if (opts.should_use_blank_line)(&last_node, &node, context) {
                        items.push_signal(Signal::NewLine);
                    }
                }
                else if let Some(should_use_space) = &opts.should_use_space {
                    if should_use_space(&last_node, &node, context) {
                        if opts.is_jsx_children {
                            items.extend(jsx_space_separator(
                                &last_node,
                                &node,
                                context,
                            ))
                        } else {
                            items.push_signal(Signal::SpaceOrNewLine);
                        }
                    }
                }
            }

            let end_info = Info::new("endMemberInfo");
            context.end_statement_or_member_infos.push(end_info);
            items.extend(if let Some(print_items) = optional_print_items {
                print_items
            } else {
                if opts.separator.is_none() {
                    parse_node(node, context)
                } else {
                    let parsed_separator = get_parsed_separator(&opts.separator, i == children_len - 1, &|_| Some(true));
                    parse_node_with_separator(node, parsed_separator, context)
                }
            });
            items.push_info(end_info);
            context.end_statement_or_member_infos.pop();

            last_node = Some(node);
        } else {
            let mut comments = node.leading_comments_fast(context.module);
            comments.extend(node.trailing_comments_fast(context.module));
            items.extend(parse_comments_as_statements(comments, None, context));

            // ensure if this is last that it parses the trailing comment statements
            if i == children_len - 1 {
                last_node = Some(node);
            }
        }
    }

    if let Some(last_node) = &last_node {
        items.extend(parse_trailing_comments_as_statements(last_node, context));
    }

    if children_len == 0 {
        items.extend(parse_comments_as_statements(opts.inner_span.hi.leading_comments_fast(context.module), None, context));
    }

    return items;

    fn should_use_new_line(
        should_use_new_line: &Option<Box<dyn Fn(&Node, &Node, &mut Context) -> bool>>,
        last_node: &Node,
        next_node: &Node,
        context: &mut Context
    ) -> bool {
        if let Some(should_use) = &should_use_new_line {
            return (should_use)(last_node, next_node, context);
        }
        return true;
    }
}

struct ParseParametersOrArgumentsOptions<'a, F> where F : FnOnce(&mut Context<'a>) -> Option<PrintItems> {
    node: Node<'a>,
    span: Option<Span>,
    nodes: Vec<Node<'a>>,
    custom_close_paren: F,
    is_parameters: bool,
}

fn parse_parameters_or_arguments<'a, F>(opts: ParseParametersOrArgumentsOptions<'a, F>, context: &mut Context<'a>) -> PrintItems where F : FnOnce(&mut Context<'a>) -> Option<PrintItems> {
    let is_parameters = opts.is_parameters;
    let prefer_single_line = is_parameters && context.config.parameters_prefer_single_line || !is_parameters && context.config.arguments_prefer_single_line;
    let force_use_new_lines = get_use_new_lines_for_nodes_with_preceeding_token("(", &opts.nodes, prefer_single_line, context);
    let span = opts.span;
    let custom_close_paren = opts.custom_close_paren;
    let first_member_span = opts.nodes.iter().map(|n| n.span()).next();
    let nodes = opts.nodes;
    let prefer_hanging = if is_parameters { context.config.parameters_prefer_hanging } else { context.config.arguments_prefer_hanging };
    let trailing_commas = get_trailing_commas(&opts.node, &nodes, is_parameters, context);

    return parse_surrounded_by_tokens(|context| {
        let mut items = PrintItems::new();

        if !force_use_new_lines && nodes.len() == 1 && is_arrow_function_with_expr_body(&nodes[0]) {
            let start_info = Info::new("startArrow");
            let parsed_node = parse_node(nodes.into_iter().next().unwrap(), context);

            items.push_info(start_info);
            items.push_signal(Signal::PossibleNewLine);
            items.push_condition(conditions::indent_if_start_of_line(parsed_node));
            items.push_condition(if_true(
                "isDifferentLineAndStartLineIndentation",
                move |context| {
                    let start_info = context.get_resolved_info(&start_info)?;
                    let is_different_line = start_info.line_number != context.writer_info.line_number;
                    let is_different_start_line_indentation = start_info.line_start_indent_level != context.writer_info.line_start_indent_level;
                    Some(is_different_line && is_different_start_line_indentation)
                },
                Signal::NewLine.into()
            ));
        } else {
            items.extend(parse_separated_values(ParseSeparatedValuesParams {
                nodes: nodes.into_iter().map(|x| NodeOrSeparator::Node(x)).collect(),
                prefer_hanging,
                force_use_new_lines,
                allow_blank_lines: false,
                separator: trailing_commas.into(),
                single_line_space_at_start: false,
                single_line_space_at_end: false,
                custom_single_line_separator: None,
                multi_line_options: parser_helpers::MultiLineOptions::surround_newlines_indented(),
                force_possible_newline_at_start: is_parameters,
                node_sorter: None,
            }, context));
        }

        items
    }, custom_close_paren, ParseSurroundedByTokensOptions {
        open_token: "(",
        close_token: ")",
        span,
        first_member: first_member_span,
        prefer_single_line_when_empty: true,
        allow_open_token_trailing_comments: true,
    }, context);

    fn get_trailing_commas(node: &Node, nodes: &Vec<Node>, is_parameters: bool, context: &mut Context) -> TrailingCommas {
        if let Some(last) = nodes.last() {
            // this would be a syntax error
            if is_param_rest_pat(last) {
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

        fn is_dynamic_import(node: &Node) -> bool {
            if let Node::CallExpr(call_expr) = &node {
                if let ExprOrSuper::Expr(expr) = &call_expr.callee {
                    if let Expr::Ident(ident) = expr {
                        if (ident.sym() as &str) == "import" {
                            return true;
                        }
                    }
                }
            }

            false
        }

        fn is_param_rest_pat(param: &Node) -> bool {
            if let Node::Param(param) = param {
                param.pat.kind() == NodeKind::RestPat
            } else {
                // arrow functions will not be a Param
                param.kind() == NodeKind::RestPat
            }
        }
    }
}

struct ParseCloseParenWithTypeOptions<'a> {
    start_info: Info,
    type_node: Option<Node<'a>>,
    type_node_separator: Option<PrintItems>,
    param_count: usize,
}

fn parse_close_paren_with_type<'a>(opts: ParseCloseParenWithTypeOptions<'a>, context: &mut Context<'a>) -> PrintItems {
    // todo: clean this up a bit
    let type_node_start_info = Info::new("typeNodeStart");
    let has_type_node = opts.type_node.is_some();
    let type_node_end_info = Info::new("typeNodeEnd");
    let start_info = opts.start_info;
    let parsed_type_node = parse_type_node(opts.type_node, opts.type_node_separator, type_node_start_info, type_node_end_info, opts.param_count, context);
    let mut items = PrintItems::new();

    items.push_condition(if_true(
        "newLineIfHeaderHangingAndTypeNodeMultipleLines",
        move |context| {
            if !has_type_node { return Some(false); }

            if let Some(is_hanging) = condition_resolvers::is_hanging(context, &start_info, &None) {
                if let Some(is_multiple_lines) = condition_resolvers::is_multiple_lines(context, &type_node_start_info, &type_node_end_info) {
                    return Some(is_hanging && is_multiple_lines);
                }
            }
            return None;
        },
        Signal::NewLine.into(),
    ));
    items.push_str(")");
    items.extend(parsed_type_node);
    return items;

    fn parse_type_node<'a>(
        type_node: Option<Node<'a>>,
        type_node_separator: Option<PrintItems>,
        type_node_start_info: Info,
        type_node_end_info: Info,
        param_count: usize,
        context: &mut Context<'a>
    ) -> PrintItems {
        let mut items = PrintItems::new();
        return if let Some(type_node) = type_node {
            let use_new_line_group = get_use_new_line_group(param_count, &type_node, context);
            items.push_info(type_node_start_info);
            if let Some(type_node_separator) = type_node_separator {
                items.extend(type_node_separator);
            } else {
                if context.config.type_annotation_space_before_colon { items.push_str(" "); }
                items.push_str(":");
                items.push_signal(Signal::SpaceIfNotTrailing);
            }
            let parsed_type_node = parse_node(type_node.into(), context);
            items.extend(parsed_type_node);
            items.push_info(type_node_end_info);

            if use_new_line_group { new_line_group(items) } else { items }
        } else {
            items
        };

        fn get_use_new_line_group(param_count: usize, type_node: &Node, context: &mut Context) -> bool {
            if param_count == 0 {
                false
            } else {
                if context.config.parameters_prefer_hanging && param_count > 1 {
                    // This was done to prevent the second argument becoming hanging, which doesn't
                    // look good especially when the return type then becomes multi-line.
                    match type_node {
                        Node::TsUnionType(_) | Node::TsIntersectionType(_) => false,
                        Node::TsTypeAnn(type_ann) => match type_ann.type_ann {
                            TsType::TsUnionOrIntersectionType(_) => false,
                            _ => true,
                        },
                        _ => true,
                    }
                } else {
                    true
                }
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

struct ParseSeparatedValuesParams<'a> {
    nodes: Vec<NodeOrSeparator<'a>>,
    prefer_hanging: bool,
    force_use_new_lines: bool,
    allow_blank_lines: bool,
    separator: Separator,
    single_line_space_at_start: bool,
    single_line_space_at_end: bool,
    custom_single_line_separator: Option<PrintItems>,
    multi_line_options: parser_helpers::MultiLineOptions,
    force_possible_newline_at_start: bool,
    node_sorter: Option<Box<dyn Fn((usize, Option<&Node<'a>>), (usize, Option<&Node<'a>>), &Module<'a>) -> std::cmp::Ordering>>,
}

enum NodeOrSeparator<'a> {
    Node(Node<'a>),
    Separator(&'a TokenAndSpan)
}

impl<'a> Spanned for NodeOrSeparator<'a> {
    fn span(&self) -> Span {
        match self {
            NodeOrSeparator::Node(node) => node.span(),
            NodeOrSeparator::Separator(token) => token.span(),
        }
    }
}

impl<'a> NodeOrSeparator<'a> {
    pub fn as_node(&self) -> Option<&Node<'a>> {
        match self {
            NodeOrSeparator::Node(node) => Some(node),
            _ => None,
        }
    }
}

#[inline]
fn parse_separated_values<'a>(
    opts: ParseSeparatedValuesParams<'a>,
    context: &mut Context<'a>
) -> PrintItems {
    parse_separated_values_with_result(opts, context).items
}

fn parse_separated_values_with_result<'a>(
    opts: ParseSeparatedValuesParams<'a>,
    context: &mut Context<'a>
) -> ParseSeparatedValuesResult {
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

    parser_helpers::parse_separated_values(|is_multi_line_or_hanging_ref| {
        let is_multi_line_or_hanging = is_multi_line_or_hanging_ref.create_resolver();
        let mut parsed_nodes = Vec::new();
        let nodes_count = nodes.len();
        let sorted_indexes = match node_sorter {
            Some(sorter) => Some(get_sorted_indexes(nodes.iter().map(|d| d.as_node()), sorter, context)),
            None => None,
        };

        for (i, value) in nodes.into_iter().enumerate() {
            let node_index = match &sorted_indexes {
                Some(old_to_new_index) => *old_to_new_index.get(i).unwrap(),
                None => i,
            };
            let (allow_inline_multi_line, allow_inline_single_line) = if let NodeOrSeparator::Node(value) = &value {
                let is_last_value = node_index + 1 == nodes_count; // allow the last node to be single line
                (allows_inline_multi_line(value, nodes_count > 1), is_last_value)
            } else { (false, false) };
            let lines_span = if compute_lines_span {
                value.as_node().map(|x| parser_helpers::LinesSpan{
                    start_line: x.start_line_with_comments(context),
                    end_line: x.end_line_with_comments(context)
                })
            } else { None };

            let items = if separator.is_none() {
                if let NodeOrSeparator::Node(value) = value {
                    parse_node(value, context)
                } else {
                    panic!("Unsupported scenario.")
                }
            } else {
                let parsed_separator = get_parsed_separator(&separator, node_index == nodes_count - 1, &is_multi_line_or_hanging);
                match value {
                    NodeOrSeparator::Node(value) => {
                        parse_node_with_separator(value, parsed_separator, context)
                    },
                    NodeOrSeparator::Separator(separator_token) => {
                        let mut items = PrintItems::new();
                        // don't use parse_leading_comments here because we don't want a space between the block comment and separator (comma)
                        let leading_comments = separator_token.leading_comments_fast(context.module);
                        items.extend(parse_comment_collection(leading_comments, None, Some(separator_token), context));
                        items.extend(parsed_separator);
                        items.extend(parse_first_line_trailing_comments(separator_token, None, context));
                        items
                    }
                }
            };

            let use_new_line_group = match value {
                // Prefer going inline multi-line for certain expressions in arguments
                // when initially single line.
                // Example: call({\n}) instead of call(\n  {\n  }\n)
                NodeOrSeparator::Node(Node::ExprOrSpread(expr_or_spread)) => match expr_or_spread.expr {
                    Expr::Object(_) | Expr::Array(_) => false,
                    _ => true,
                },
                _ => true,
            };

            parsed_nodes.push(parser_helpers::ParsedValue {
                items: if use_new_line_group { parser_helpers::new_line_group(items) } else { items },
                lines_span,
                allow_inline_multi_line,
                allow_inline_single_line,
            });
        }

        match sorted_indexes {
            Some(sorted_indexes) => sort_by_sorted_indexes(parsed_nodes, sorted_indexes),
            None => parsed_nodes,
        }
    }, parser_helpers::ParseSeparatedValuesOptions {
        prefer_hanging: opts.prefer_hanging,
        force_use_new_lines: opts.force_use_new_lines,
        allow_blank_lines: opts.allow_blank_lines,
        single_line_space_at_start: opts.single_line_space_at_start,
        single_line_space_at_end: opts.single_line_space_at_end,
        single_line_separator: opts.custom_single_line_separator.unwrap_or(Signal::SpaceOrNewLine.into()),
        indent_width,
        multi_line_options: opts.multi_line_options,
        force_possible_newline_at_start: opts.force_possible_newline_at_start,
    })
}

fn get_sorted_indexes<'a: 'b, 'b>(
    nodes: impl Iterator<Item=Option<&'b Node<'a>>>,
    sorter: Box<dyn Fn((usize, Option<&Node<'a>>), (usize, Option<&Node<'a>>), &Module<'a>) -> std::cmp::Ordering>,
    context: &mut Context<'a>,
) -> utils::VecMap<usize> {
    let mut nodes_with_indexes = nodes.enumerate().collect::<Vec<_>>();
    nodes_with_indexes.sort_unstable_by(|a, b| sorter((a.0, a.1), (b.0, b.1), context.module));
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

    for (i, parsed_node) in items.into_iter().enumerate() {
        sorted_items[*sorted_indexes.get(i).unwrap_or(&i)] = Some(parsed_node);
    }

    sorted_items.into_iter().map(|x| x.unwrap()).collect()
}

fn parse_node_with_separator<'a>(value: Node<'a>, parsed_separator: PrintItems, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    let comma_token = get_comma_token(&value, context);

    // get the trailing comments after the comma token (if the separator in the file is currently a comma)
    let parsed_trailing_comments = if let Some(comma_token) = comma_token {
        parse_trailing_comments(comma_token, context)
    } else {
        PrintItems::new()
    };

    // if the current node is ignored and already has a semi-colon, then skip adding a separator
    let is_ignored_with_semi_colon = value.text_fast(context.module).ends_with(";")
        && get_has_ignore_comment(&value.leading_comments_fast(context.module), &value, context);
    if is_ignored_with_semi_colon {
        items.extend(parse_node(value, context));
    } else {
        let parsed_separator = parsed_separator.into_rc_path();
        items.extend(parse_node_with_inner_parse(value, context, move |mut items, _| {
            // this Rc clone is necessary because we can't move the captured parsed_comma out of this closure
            items.push_optional_path(parsed_separator.clone());
            items
        }));
    }

    items.extend(parsed_trailing_comments);

    return items;

    fn get_comma_token<'a>(element: &Node<'a>, context: &mut Context<'a>) -> Option<&'a TokenAndSpan> {
        match context.token_finder.get_next_token_if_comma(element) {
            Some(comma) => Some(comma),
            None => context.token_finder.get_last_token_within_if_comma(element), // may occur for type literals
        }
    }
}

/// Some nodes don't have a TsTypeAnn, but instead a Box<TsType>
fn parse_type_ann_with_colon_if_exists_for_type<'a>(type_ann: &'a Option<TsType>, context: &mut Context<'a>) -> PrintItems {
    if let Some(type_ann) = type_ann {
        parse_type_ann_with_colon_for_type(type_ann, context)
    } else {
        PrintItems::new()
    }
}

fn parse_type_ann_with_colon_for_type<'a>(type_ann: &'a TsType, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    if context.config.type_annotation_space_before_colon {
        items.push_str(" ");
    }
    let colon_token = context.token_finder.get_previous_token_if_colon(type_ann);
    #[cfg(debug_assertions)]
    assert_has_op(":", colon_token, context);
    items.extend(parse_type_ann_with_colon(type_ann.into(), colon_token, context));
    items
}

fn parse_type_ann_with_colon_if_exists<'a>(type_ann: &Option<&TsTypeAnn<'a>>, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    if let Some(type_ann) = type_ann {
        if context.config.type_annotation_space_before_colon {
            items.push_str(" ");
        }
        let colon_token = context.token_finder.get_first_colon_token_within(type_ann);
        #[cfg(debug_assertions)]
        assert_has_op(":", colon_token, context);
        items.extend(parse_type_ann_with_colon((*type_ann).into(), colon_token, context));
    }
    items
}

fn parse_type_ann_with_colon<'a>(type_ann: Node<'a>, colon_token: Option<&TokenAndSpan>, context: &mut Context<'a>) -> PrintItems {
    parse_assignment_like_with_token(type_ann, ":", colon_token, context)
}

struct ParseBraceSeparatorOptions<'a> {
    brace_position: BracePosition,
    open_brace_token: Option<&'a TokenAndSpan>,
    start_header_info: Option<Info>,
}

fn parse_brace_separator<'a>(opts: ParseBraceSeparatorOptions<'a>, context: &mut Context) -> PrintItems {
    return match opts.brace_position {
        BracePosition::SameLineUnlessHanging => {
            if let Some(start_header_info) = opts.start_header_info {
                conditions::new_line_if_hanging_space_otherwise(conditions::NewLineIfHangingSpaceOtherwiseOptions {
                    start_info: start_header_info,
                    end_info: None,
                    space_char: Some(space_if_not_start_line()),
                }).into()
            } else {
                space_if_not_start_line()
            }
        },
        BracePosition::SameLine => {
            space_if_not_start_line()
        },
        BracePosition::NextLine => {
            Signal::NewLine.into()
        },
        BracePosition::Maintain => {
            if let Some(open_brace_token) = opts.open_brace_token {
                if node_helpers::is_first_node_on_line(open_brace_token, context.module) {
                    Signal::NewLine.into()
                } else {
                    space_if_not_start_line()
                }
            } else {
                space_if_not_start_line()
            }
        },
    };
}

fn space_if_not_start_line() -> PrintItems {
    if_true(
        "spaceIfNotStartLine",
        |context| Some(!context.writer_info.is_start_of_line()),
        " ".into()
    ).into()
}

struct ParseNodeInParensOptions {
    inner_span: Span,
    prefer_hanging: bool,
    allow_open_paren_trailing_comments: bool,
}

fn parse_node_in_parens<'a>(
    parse_node: impl FnOnce(&mut Context<'a>) -> PrintItems,
    opts: ParseNodeInParensOptions,
    context: &mut Context<'a>
) -> PrintItems {
    let inner_span = opts.inner_span;
    let paren_span = get_paren_span(&inner_span, context);
    let force_use_new_lines = get_force_use_new_lines(inner_span, &paren_span, context);

    return parse_surrounded_by_tokens(|context| {
        let parsed_node = parse_node(context);
        if force_use_new_lines {
            surround_with_new_lines(with_indent(parsed_node))
        } else if opts.prefer_hanging {
            parsed_node
        } else {
            parser_helpers::surround_with_newlines_indented_if_multi_line(parsed_node, context.config.indent_width)
        }
    }, |_| None, ParseSurroundedByTokensOptions {
        open_token: "(",
        close_token: ")",
        span: paren_span,
        first_member: Some(inner_span),
        prefer_single_line_when_empty: true,
        allow_open_token_trailing_comments: opts.allow_open_paren_trailing_comments,
    }, context);

    fn get_force_use_new_lines(inner_span: Span, paren_span: &Option<Span>, context: &mut Context) -> bool {
        if !context.config.parentheses_prefer_single_line {
            if let Some(paren_span) = &paren_span {
                if node_helpers::get_use_new_lines_for_nodes(&paren_span.lo(), &inner_span, context.module) {
                    return true;
                }
            }
        }

        has_any_node_comment_on_different_line(&vec![inner_span], context)
    }
}

fn get_paren_span<'a>(inner_span: &Span, context: &mut Context<'a>) -> Option<Span> {
    let open_paren = context.token_finder.get_previous_token_if_open_paren(inner_span);
    let close_paren = context.token_finder.get_next_token_if_close_paren(inner_span);

    if let Some(open_paren) = open_paren {
        if let Some(close_paren) = close_paren {
            return Some(create_span(open_paren.lo(), close_paren.hi()));
        }
    }

    None
}

struct ParseExtendsOrImplementsOptions<'a> {
    text: &'a str,
    type_items: Vec<Node<'a>>,
    start_header_info: Info,
    prefer_hanging: bool,
}

fn parse_extends_or_implements<'a>(opts: ParseExtendsOrImplementsOptions<'a>, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();

    if opts.type_items.is_empty() {
        return items;
    }

    items.push_condition(conditions::new_line_if_hanging_space_otherwise(conditions::NewLineIfHangingSpaceOtherwiseOptions {
        start_info: opts.start_header_info,
        end_info: None,
        space_char: Some(conditions::if_above_width_or(context.config.indent_width, Signal::SpaceOrNewLine.into(), " ".into()).into()),
    }));
    // the newline group will force it to put the extends or implements on a new line
    items.push_condition(conditions::indent_if_start_of_line(parser_helpers::new_line_group({
        let mut items = PrintItems::new();
        items.push_str(opts.text);
        items.extend(parse_separated_values(ParseSeparatedValuesParams {
            nodes: opts.type_items.into_iter().map(|x| NodeOrSeparator::Node(x)).collect(),
            prefer_hanging: opts.prefer_hanging,
            force_use_new_lines: false,
            allow_blank_lines: false,
            separator: TrailingCommas::Never.into(),
            single_line_space_at_start: true,
            single_line_space_at_end: false,
            custom_single_line_separator: None,
            multi_line_options: parser_helpers::MultiLineOptions::new_line_start(),
            force_possible_newline_at_start: false,
            node_sorter: None,
        }, context));
        items
    })));

    return items;
}

struct ParseObjectLikeNodeOptions<'a> {
    node: Node<'a>,
    members: Vec<Node<'a>>,
    separator: Separator,
    prefer_hanging: bool,
    prefer_single_line: bool,
    surround_single_line_with_spaces: bool,
    allow_blank_lines: bool,
    node_sorter: Option<Box<dyn Fn((usize, Option<&Node<'a>>), (usize, Option<&Node<'a>>), &Module<'a>) -> std::cmp::Ordering>>,
}

fn parse_object_like_node<'a>(opts: ParseObjectLikeNodeOptions<'a>, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();

    let child_tokens = get_tokens_from_children_with_tokens(&opts.node, context.module);
    let open_brace_token = child_tokens.iter().find(|t| t.token == Token::LBrace);
    let close_brace_token = child_tokens.iter().rev().find(|t| t.token == Token::RBrace);
    let force_multi_line = get_use_new_lines_for_nodes_with_preceeding_token("{", &opts.members, opts.prefer_single_line, context);

    let first_member_span = opts.members.get(0).map(|x| x.span());
    let obj_span = if let (Some(open_brace_token), Some(close_brace_token)) = (open_brace_token, close_brace_token) {
        Some(create_span(open_brace_token.lo(), close_brace_token.hi()))
    } else {
        None
    };

    items.extend(parse_surrounded_by_tokens(|context| {
        if opts.members.is_empty() {
            PrintItems::new()
        } else {
            parse_separated_values(ParseSeparatedValuesParams {
                nodes: opts.members.into_iter().map(|x| NodeOrSeparator::Node(x)).collect(),
                prefer_hanging: opts.prefer_hanging,
                force_use_new_lines: force_multi_line,
                allow_blank_lines: opts.allow_blank_lines,
                separator: opts.separator,
                single_line_space_at_start: opts.surround_single_line_with_spaces,
                single_line_space_at_end: opts.surround_single_line_with_spaces,
                custom_single_line_separator: None,
                multi_line_options: parser_helpers::MultiLineOptions::surround_newlines_indented(),
                force_possible_newline_at_start: false,
                node_sorter: opts.node_sorter,
            }, context)
        }
    }, |_| None, ParseSurroundedByTokensOptions {
        open_token: "{",
        close_token: "}",
        span: obj_span,
        first_member: first_member_span,
        prefer_single_line_when_empty: true,
        allow_open_token_trailing_comments: true,
    }, context));

    items
}

fn parse_for_member_like_expr_item<'a>(
    item: &MemberLikeExprItem<'a>,
    context: &mut Context<'a>,
    index: usize,
    item_count: usize,
) -> PrintItems {
    let is_first = index == 0;
    let is_last = index == item_count - 1;
    match item {
        MemberLikeExprItem::Node(node) => {
            let is_optional = item.is_optional();
            parse_node_with_inner_parse(*node, context, |node_items, _| {
                let mut items = PrintItems::new();
                if !is_first {
                    if is_optional {
                        items.push_str("?.");
                    } else {
                        items.push_str(".");
                    }
                }
                items.extend(node_items);
                items
            })
        }
        MemberLikeExprItem::Computed(node) => {
            let is_optional = item.is_optional();
            let mut items = PrintItems::new();

            if is_optional {
                items.push_str("?.");
            }
            items.extend(parse_computed_prop_like(|context| parse_node(node.inner_node, context), ParseComputedPropLikeOptions {
                inner_node_span: node.inner_node.span(),
            }, context));

            if !is_last {
                // Manually parse the trailing comments of the close bracket token
                // because it doesn't go through the parse_node method
                items.extend(parse_trailing_comments(item, context));
            }

            items
        }
        MemberLikeExprItem::CallExpr(node) => {
            let mut items = PrintItems::new();

            items.extend(parse_call_expr_like(CallExprLike {
                original_call_expr: node.original_call_expr,
                parsed_callee: parse_for_member_like_expr_item(&node.callee, context, index, item_count),
            }, context));

            if !is_last {
                // Need to manually parse the trailing comments here because
                // this doesn't go through the parse_node method
                items.extend(parse_trailing_comments(item, context));
            }

            items
        }
    }
}

fn parse_for_flattened_member_like_expr<'a>(node: FlattenedMemberLikeExpr<'a>, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    let member_expr_start_info = Info::new("member_expr_start");
    let member_expr_end_info = Info::new("member_expr_start_last_item");
    let total_items_len = node.nodes.len();

    if total_items_len > 1 {
        items.push_info(member_expr_start_info);
    }

    items.extend(parse_for_member_like_expr_item(&node.nodes[0], context, 0, total_items_len));

    for (i, item) in node.nodes.iter().enumerate().skip(1) {
        let force_use_new_line = !context.config.member_expression_prefer_single_line
            && node_helpers::get_use_new_lines_for_nodes(&node.nodes[i - 1], &node.nodes[i], context.module);
        let is_optional = item.is_optional();
        if is_optional || !item.is_computed() {
            if force_use_new_line {
                items.push_signal(Signal::NewLine);
            } else if !context.config.member_expression_line_per_expression {
                items.push_condition(conditions::if_above_width(
                    context.config.indent_width,
                    Signal::PossibleNewLine.into()
                ));
            } else {
                items.push_condition(if_true_or(
                    "isMultipleLines",
                    move |context| condition_resolvers::is_multiple_lines(context, &member_expr_start_info, &member_expr_end_info),
                    Signal::NewLine.into(),
                    Signal::PossibleNewLine.into(),
                ));
            }
        }

        let is_last_item = i == total_items_len - 1;
        if is_last_item {
            // store this right before the last right expression
            items.push_info(member_expr_end_info);
        }

        let parsed_item = parse_for_member_like_expr_item(item, context, i, total_items_len);
        if item.is_computed() {
            items.push_condition(indent_if_start_of_line_or_start_of_line_indented(parsed_item));
        } else {
            items.push_condition(conditions::indent_if_start_of_line({
                parsed_item
            }));
        }
    }

    items
}

struct ParseComputedPropLikeOptions {
    inner_node_span: Span,
}

fn parse_computed_prop_like<'a>(
    parse_inner: impl FnOnce(&mut Context<'a>) -> PrintItems,
    opts: ParseComputedPropLikeOptions,
    context: &mut Context<'a>,
) -> PrintItems {
    let inner_node_span = opts.inner_node_span;
    let span = get_bracket_span(&inner_node_span, context);
    let force_use_new_lines = !context.config.computed_prefer_single_line
        && if let Some(span) = &span {
            node_helpers::get_use_new_lines_for_nodes(&span.lo(), &inner_node_span.lo(), context.module)
        } else {
            false
        };

    return new_line_group(parse_surrounded_by_tokens(|context| {
        if force_use_new_lines {
            surround_with_new_lines(with_indent(parse_inner(context)))
        } else {
            parser_helpers::surround_with_newlines_indented_if_multi_line(parse_inner(context), context.config.indent_width)
        }
    }, |_| None, ParseSurroundedByTokensOptions {
        open_token: "[",
        close_token: "]",
        span,
        first_member: Some(inner_node_span),
        prefer_single_line_when_empty: false,
        allow_open_token_trailing_comments: true,
    }, context));

    fn get_bracket_span(node: &dyn Spanned, context: &mut Context) -> Option<Span> {
        let open_bracket = context.token_finder.get_previous_token_if_open_bracket(node);
        let close_bracket = context.token_finder.get_next_token_if_close_bracket(node);
        if let Some(open_bracket) = open_bracket {
            if let Some(close_bracket) = close_bracket {
                return Some(create_span(open_bracket.lo(), close_bracket.hi()));
            }
        }

        if cfg!(debug_assertions) {
            panic!("Debug panic! Could not find open and/or close bracket token.");
        } else {
            None
        }
    }
}

fn parse_decorators<'a>(decorators: &Vec<&'a Decorator<'a>>, is_inline: bool, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    if decorators.is_empty() {
        return items;
    }

    let force_use_new_lines = !context.config.decorators_prefer_single_line
        && decorators.len() >= 2
        && node_helpers::get_use_new_lines_for_nodes(&decorators[0], &decorators[1], context.module);

    let separated_values_result = parse_separated_values_with_result(ParseSeparatedValuesParams {
        nodes: decorators.iter().map(|&p| NodeOrSeparator::Node(p.into())).collect(),
        prefer_hanging: false, // would need to think about the design because prefer_hanging causes a hanging indent
        force_use_new_lines,
        allow_blank_lines: false,
        separator: Separator::none(),
        single_line_space_at_start: false,
        single_line_space_at_end: is_inline,
        custom_single_line_separator: None,
        multi_line_options: parser_helpers::MultiLineOptions::same_line_no_indent(),
        force_possible_newline_at_start: false,
        node_sorter: None,
    }, context);

    items.extend(separated_values_result.items);

    if is_inline {
        let is_multi_line = separated_values_result.is_multi_line_condition_ref.create_resolver();
        items.push_condition(if_true("inlineMultiLineSpace", is_multi_line, Signal::NewLine.into()));
    } else {
        items.push_signal(Signal::NewLine);
    }

    // parse the comments between the last decorator and the next token
    if let Some(last_dec) = decorators.last() {
        let next_token_pos = context.token_finder.get_next_token_pos_after(last_dec);
        items.extend(parse_leading_comments(&next_token_pos, context));
    }

    return items;
}

fn parse_control_flow_separator(
    next_control_flow_position: NextControlFlowPosition,
    previous_node_block: &Span,
    token_text: &str,
    previous_start_info: Info,
    previous_close_brace_condition_ref: Option<ConditionReference>,
    context: &mut Context
) -> PrintItems {
    let mut items = PrintItems::new();
    match next_control_flow_position {
        NextControlFlowPosition::SameLine => {
            items.push_condition(if_true_or(
                "newLineOrSpace",
                move |condition_context| {
                    // newline if on the same line as the previous
                    if condition_resolvers::is_on_same_line(condition_context, &previous_start_info)? {
                        return Some(true);
                    }

                    // newline if the previous did not have a close brace
                    if let Some(previous_close_brace_condition_ref) = previous_close_brace_condition_ref {
                        if !condition_context.get_resolved_condition(&previous_close_brace_condition_ref)? {
                            return Some(true);
                        }
                    }

                    Some(false)
                },
                Signal::NewLine.into(),
                " ".into(),
            ));
        },
        NextControlFlowPosition::NextLine => items.push_signal(Signal::NewLine),
        NextControlFlowPosition::Maintain => {
            let token = context.token_finder.get_first_keyword_after(previous_node_block, token_text);

            if token.is_some() && node_helpers::is_first_node_on_line(token.unwrap(), context.module) {
                items.push_signal(Signal::NewLine);
            } else {
                items.push_str(" ");
            }
        }
    }
    return items;
}

struct ParseHeaderWithConditionalBraceBodyOptions<'a> {
    parent: Span,
    body_node: Node<'a>,
    parsed_header: PrintItems,
    use_braces: UseBraces,
    brace_position: BracePosition,
    single_body_position: Option<SingleBodyPosition>,
    requires_braces_condition_ref: Option<ConditionReference>,
}

struct ParseHeaderWithConditionalBraceBodyResult {
    parsed_node: PrintItems,
    open_brace_condition_ref: ConditionReference,
    close_brace_condition_ref: ConditionReference,
}

fn parse_header_with_conditional_brace_body<'a>(opts: ParseHeaderWithConditionalBraceBodyOptions<'a>, context: &mut Context<'a>) -> ParseHeaderWithConditionalBraceBodyResult {
    let start_header_info = Info::new("startHeader");
    let end_header_info = Info::new("endHeader");
    let mut items = PrintItems::new();

    items.push_info(start_header_info);
    items.extend(new_line_group(opts.parsed_header));
    items.push_info(end_header_info);
    let result = parse_conditional_brace_body(ParseConditionalBraceBodyOptions {
        parent: opts.parent,
        body_node: opts.body_node,
        use_braces: opts.use_braces,
        brace_position: opts.brace_position,
        single_body_position: opts.single_body_position,
        requires_braces_condition_ref: opts.requires_braces_condition_ref,
        header_start_token: None,
        start_header_info: Some(start_header_info),
        end_header_info: Some(end_header_info),
    }, context);
    items.extend(result.parsed_node);

    return ParseHeaderWithConditionalBraceBodyResult {
        open_brace_condition_ref: result.open_brace_condition_ref,
        close_brace_condition_ref: result.close_brace_condition_ref,
        parsed_node: items,
    };
}

struct ParseConditionalBraceBodyOptions<'a> {
    parent: Span,
    body_node: Node<'a>,
    use_braces: UseBraces,
    brace_position: BracePosition,
    single_body_position: Option<SingleBodyPosition>,
    requires_braces_condition_ref: Option<ConditionReference>,
    header_start_token: Option<&'a TokenAndSpan>,
    start_header_info: Option<Info>,
    end_header_info: Option<Info>,
}

struct ParseConditionalBraceBodyResult {
    parsed_node: PrintItems,
    open_brace_condition_ref: ConditionReference,
    close_brace_condition_ref: ConditionReference,
}

fn parse_conditional_brace_body<'a>(opts: ParseConditionalBraceBodyOptions<'a>, context: &mut Context<'a>) -> ParseConditionalBraceBodyResult {
    // todo: reorganize...
    let start_info = Info::new("startInfo");
    let end_info = Info::new("endInfo");
    let start_header_info = opts.start_header_info;
    let end_header_info = opts.end_header_info;
    let requires_braces_condition = opts.requires_braces_condition_ref;
    let start_inner_text_info = Info::new("startInnerText");
    let start_statements_info = Info::new("startStatements");
    let end_statements_info = Info::new("endStatements");
    let header_trailing_comments = get_header_trailing_comments(&opts.body_node, context);
    let body_should_be_multi_line = get_body_should_be_multi_line(&opts.body_node, &header_trailing_comments, context);
    let should_use_new_line = get_should_use_new_line(
        &opts.body_node,
        body_should_be_multi_line,
        &opts.single_body_position,
        &opts.header_start_token,
        &opts.parent,
        context
    );
    let open_brace_token = get_open_brace_token(&opts.body_node, context);
    let use_braces = opts.use_braces;
    let is_body_empty_stmt = opts.body_node.kind() == NodeKind::EmptyStmt;
    let mut inner_brace_space_condition = if_true(
        "spaceCondition",
        move |condition_context| {
            if is_body_empty_stmt { return Some(false); }
            let start_inner_text_info = condition_context.get_resolved_info(&start_inner_text_info)?;
            let end_statements_info = condition_context.get_resolved_info(&end_statements_info)?;
            if start_inner_text_info.line_number < end_statements_info.line_number {
                return Some(false);
            }
            return Some(start_inner_text_info.column_number < end_statements_info.column_number);
        },
        Signal::SpaceOrNewLine.into(),
    );
    let inner_brace_space_condition_ref = inner_brace_space_condition.get_reference();
    let mut newline_condition = if_true(
        "newLineCondition",
        move |condition_context| {
            if is_body_empty_stmt { return Some(false); }

            if should_use_new_line {
                return Some(true);
            }
            let start_header_info = start_header_info.as_ref()?;
            let resolved_start_info = condition_context.get_resolved_info(start_header_info)?;
            if resolved_start_info.line_number < condition_context.writer_info.line_number {
                return Some(true);
            }
            let resolved_end_statements_info = condition_context.get_resolved_info(&end_statements_info)?;
            return Some(resolved_end_statements_info.line_number > resolved_start_info.line_number);
        },
        Signal::NewLine.into(),
    );
    let newline_condition_ref = newline_condition.get_reference();
    let force_braces = get_force_braces(&opts.body_node);
    let mut open_brace_condition = Condition::new_with_dependent_infos("openBrace", ConditionProperties {
        condition: {
            let has_open_brace_token = open_brace_token.is_some();
            Rc::new(Box::new(move |condition_context| {
                // never use braces for a single semi-colon on the end (ex. `for(;;);`)
                if is_body_empty_stmt { return Some(false); }

                match use_braces {
                    UseBraces::WhenNotSingleLine => {
                        if force_braces {
                            Some(true)
                        } else {
                            let is_multiple_lines = condition_resolvers::is_multiple_lines(
                                condition_context,
                                &start_header_info.unwrap_or(start_info),
                                &end_info
                            )?;
                            Some(is_multiple_lines)
                        }
                    },
                    UseBraces::Maintain => Some(force_braces || has_open_brace_token),
                    UseBraces::Always => Some(true),
                    UseBraces::PreferNone => {
                        if force_braces || body_should_be_multi_line {
                            return Some(true)
                        }
                        if let Some(start_header_info) = &start_header_info {
                            if let Some(end_header_info) = &end_header_info {
                                let is_header_multiple_lines = condition_resolvers::is_multiple_lines(condition_context, start_header_info, end_header_info)?;
                                if is_header_multiple_lines {
                                    return Some(true);
                                }
                            }
                        }
                        let is_statements_multiple_lines = condition_resolvers::is_multiple_lines(condition_context, &start_statements_info, &end_statements_info)?;
                        if is_statements_multiple_lines {
                            return Some(true);
                        }

                        if let Some(requires_braces_condition) = &requires_braces_condition {
                            let requires_braces = condition_context.get_resolved_condition(requires_braces_condition)?;
                            if requires_braces {
                                return Some(true);
                            }
                        }

                        return Some(false);
                    }
                }
            }))
        },
        true_path: {
            let mut items = PrintItems::new();
            items.extend(parse_brace_separator(ParseBraceSeparatorOptions {
                brace_position: opts.brace_position,
                open_brace_token: open_brace_token,
                start_header_info,
            }, context));
            items.push_str("{");
            items.push_condition(inner_brace_space_condition);
            Some(items)
        },
        false_path: None,
    }, vec![end_info]);
    let open_brace_condition_ref = open_brace_condition.get_reference();

    // parse body
    let mut items = PrintItems::new();
    items.push_info(start_info);
    items.push_condition(open_brace_condition);
    items.push_info(start_inner_text_info);
    let parsed_comments = parse_comment_collection(header_trailing_comments.into_iter(), None, None, context);
    if !parsed_comments.is_empty() {
        items.push_signal(Signal::StartForceNoNewLines);
        items.push_str(" ");
        items.extend(parsed_comments);
        items.push_signal(Signal::FinishForceNoNewLines);
    }
    items.push_condition(newline_condition);
    items.push_info(start_statements_info);

    if let Node::BlockStmt(body_node) = opts.body_node {
        items.extend(parser_helpers::with_indent({
            let mut items = PrintItems::new();
            // parse the remaining trailing comments inside because some of them are parsed already
            // by parsing the header trailing comments
            items.extend(parse_leading_comments(body_node, context));
            items.extend(parse_statements(body_node.get_inner_span(context), body_node.stmts.iter().map(|x| x.into()).collect(), context));
            items
        }));
    } else {
        items.extend(parser_helpers::with_indent({
            let mut items = PrintItems::new();
            let body_node_span = opts.body_node.span();
            if !is_body_empty_stmt {
                items.push_condition(if_true(
                    "spaceIfAtStart",
                    move |context| condition_resolvers::is_at_same_position(context, &start_info),
                    Signal::SpaceOrNewLine.into(),
                ));
            }
            items.extend(parse_node(opts.body_node, context));
            items.extend(parse_trailing_comments(&body_node_span, context));
            items
        }));
    }

    items.push_info(end_statements_info);
    let mut close_brace_condition = if_true(
        "closeBrace",
        move |condition_context| condition_context.get_resolved_condition(&open_brace_condition_ref),
        {
            let mut items = PrintItems::new();
            items.push_condition(if_true_or(
                "closeBraceNewLine",
                move |condition_context| {
                    let is_new_line = condition_context.get_resolved_condition(&newline_condition_ref)?;
                    if !is_new_line { return Some(false); }
                    let has_statement_text = condition_resolvers::are_infos_not_equal(condition_context, &start_statements_info, &end_statements_info)?;
                    return Some(has_statement_text);
                },
                Signal::NewLine.into(),
                if_true(
                    "closeBraceSpace",
                    move |condition_context| {
                        if condition_resolvers::is_at_same_position(condition_context, &start_inner_text_info)? {
                            return Some(false);
                        }
                        let had_space = condition_context.get_resolved_condition(&inner_brace_space_condition_ref)?;
                        return Some(had_space);
                    },
                    " ".into(),
                ).into()
            ));
            items.push_str("}");
            items
        },
    );
    let close_brace_condition_ref = close_brace_condition.get_reference();
    items.push_condition(close_brace_condition);
    items.push_info(end_info);

    // return result
    return ParseConditionalBraceBodyResult {
        parsed_node: items,
        open_brace_condition_ref,
        close_brace_condition_ref,
    };

    fn get_should_use_new_line<'a>(
        body_node: &Node,
        body_should_be_multi_line: bool,
        single_body_position: &Option<SingleBodyPosition>,
        header_start_token: &Option<&'a TokenAndSpan>,
        parent: &Span,
        context: &mut Context<'a>
    ) -> bool {
        if body_should_be_multi_line {
            return true;
        }
        if let Some(single_body_position) = single_body_position {
            return match single_body_position {
                SingleBodyPosition::Maintain => get_body_stmt_start_line(body_node, context) > get_header_start_line(header_start_token, parent, context),
                SingleBodyPosition::NextLine => true,
                SingleBodyPosition::SameLine => {
                    if let Node::BlockStmt(block_stmt) = body_node {
                        if block_stmt.stmts.len() != 1 {
                            return true;
                        }
                        return get_body_stmt_start_line(body_node, context) > get_header_start_line(header_start_token, parent, context);
                    }
                    return false;
                },
            }
        } else {
            if let Node::BlockStmt(block_stmt) = body_node {
                if block_stmt.stmts.len() == 0 {
                    // keep the block on the same line
                    return block_stmt.start_line_fast(context.module) < block_stmt.end_line_fast(context.module);
                }
            }
            return true;
        }

        fn get_body_stmt_start_line(body_node: &Node, context: &mut Context) -> usize {
            if let Node::BlockStmt(body_node) = body_node {
                if let Some(first_stmt) = body_node.stmts.get(0) {
                    return first_stmt.start_line_fast(context.module);
                }
            }
            return body_node.start_line_fast(context.module);
        }

        fn get_header_start_line<'a>(header_start_token: &Option<&'a TokenAndSpan>, parent: &Span, context: &mut Context<'a>) -> usize {
            if let Some(header_start_token) = header_start_token {
                return header_start_token.start_line_fast(context.module);
            }
            return parent.start_line_fast(context.module);
        }
    }

    fn get_body_should_be_multi_line<'a>(body_node: &Node<'a>, header_trailing_comments: &Vec<&'a Comment>, context: &mut Context<'a>) -> bool {
        if let Node::BlockStmt(body_node) = body_node {
            if body_node.stmts.len() == 1 && !has_leading_comment_on_different_line(&body_node.stmts[0], header_trailing_comments, context.module) {
                return false;
            }
            if body_node.stmts.len() == 0 && body_node.start_line_fast(context.module) == body_node.end_line_fast(context.module) {
                return false;
            }
            return true;
        } else {
            return has_leading_comment_on_different_line(body_node, header_trailing_comments, context.module);
        }

        fn has_leading_comment_on_different_line(node: &dyn Spanned, header_trailing_comments: &Vec<&Comment>, module: &Module) -> bool {
            node_helpers::has_leading_comment_on_different_line(
                node,
                /* comments to ignore */ Some(header_trailing_comments),
                module
            )
        }
    }

    fn get_force_braces<'a>(body_node: &Node) -> bool {
        if let Node::BlockStmt(body_node) = body_node {
            return body_node.stmts.len() == 0;
        } else {
            return false;
        }
    }

    fn get_header_trailing_comments<'a>(body_node: &Node<'a>, context: &mut Context<'a>) -> Vec<&'a Comment> {
        let mut comments = Vec::new();
        if let Node::BlockStmt(block_stmt) = body_node {
            let comment_line = body_node.leading_comments_fast(context.module).filter(|c| c.kind == CommentKind::Line).next();
            if let Some(comment) = comment_line {
                comments.push(comment);
                return comments;
            }

            let open_brace_token = context.token_finder.get_first_open_brace_token_within(block_stmt).expect("Expected to find an open brace token.");
            let body_node_start_line = body_node.start_line_fast(context.module);
            comments.extend(open_brace_token.trailing_comments_fast(context.module).take_while(|c| c.start_line_fast(context.module) == body_node_start_line && c.kind == CommentKind::Line));
        } else {
            let leading_comments = body_node.leading_comments_fast(context.module);
            let last_header_token_end = context.token_finder.get_previous_token_end_before(body_node);
            let last_header_token_end_line = last_header_token_end.end_line_fast(context.module);
            comments.extend(leading_comments.take_while(|c| c.start_line_fast(context.module) <= last_header_token_end_line && c.kind == CommentKind::Line));
        }

        return comments;
    }

    fn get_open_brace_token<'a>(body_node: &Node<'a>, context: &mut Context<'a>) -> Option<&'a TokenAndSpan> {
        if let Node::BlockStmt(block_stmt) = body_node {
            context.token_finder.get_first_open_brace_token_within(block_stmt)
        } else {
            None
        }
    }
}

struct ParseJsxWithOpeningAndClosingOptions<'a> {
    opening_element: Node<'a>,
    closing_element: Node<'a>,
    children: Vec<Node<'a>>,
}

struct ParseJsxWithOpeningAndClosingResult {
    items: PrintItems,
    start_info: Info,
    end_info: Info,
}

fn parse_jsx_with_opening_and_closing<'a>(
    opts: ParseJsxWithOpeningAndClosingOptions<'a>,
    context: &mut Context<'a>,
) -> ParseJsxWithOpeningAndClosingResult {
    let force_use_multi_lines = get_force_use_multi_lines(&opts.opening_element, &opts.children, context);
    let start_info = Info::new("startInfo");
    let end_info = Info::new("endInfo");
    let mut items = PrintItems::new();
    let inner_span = create_span(opts.opening_element.hi(), opts.closing_element.lo());

    items.push_info(start_info);
    items.extend(parse_node(opts.opening_element, context));
    items.extend(parse_jsx_children(ParseJsxChildrenOptions {
        inner_span,
        children: opts.children,
        parent_start_info: start_info,
        parent_end_info: end_info,
        force_use_multi_lines,
    }, context));
    items.extend(parse_node(opts.closing_element, context));
    items.push_info(end_info);

    return ParseJsxWithOpeningAndClosingResult {
        items,
        start_info,
        end_info,
    };

    fn get_force_use_multi_lines(opening_element: &Node, children: &Vec<Node>, context: &mut Context) -> bool {
        // if any of the children are a jsx element or jsx fragment, then force multi-line
        for child in children {
            if matches!(child, Node::JSXElement(_) | Node::JSXFragment(_)) {
                return true;
            }
        }

        if context.config.jsx_element_prefer_single_line {
            false
        } else if let Some(first_child) = children.get(0) {
            if let Node::JSXText(first_child) = first_child {
                if first_child.text_fast(context.module).find("\n").is_some() {
                    return true;
                }
            }

            node_helpers::get_use_new_lines_for_nodes(opening_element, first_child, context.module)
        } else {
            false
        }
    }
}

struct ParseJsxChildrenOptions<'a> {
    inner_span: Span,
    children: Vec<Node<'a>>,
    parent_start_info: Info,
    parent_end_info: Info,
    force_use_multi_lines: bool,
}

fn parse_jsx_children<'a>(opts: ParseJsxChildrenOptions<'a>, context: &mut Context<'a>) -> PrintItems {
    let filtered_children = get_filtered_jsx_children(opts.children, context);

    // Need to parse the children here so they only get parsed once.
    // Nodes need to be only parsed once so that their comments don't end up in
    // the handled comments collection and the second time they won't be parsed out.
    let parsed_children = filtered_children
        .into_iter()
        .map(|child| (child, {
            let items = parse_node(child, context);
            match child {
                Node::JSXText(_) => items,
                _ => new_line_group(items),
            }.into_rc_path()
        }))
        .collect::<Vec<_>>();
    let parent_start_info = opts.parent_start_info;
    let parent_end_info = opts.parent_end_info;

    if opts.force_use_multi_lines {
        return parse_for_new_lines(parsed_children, opts.inner_span, context);
    }
    else {
        // decide whether newlines should be used or not
        return if_true_or(
            "jsxChildrenNewLinesOrNot",
            move |condition_context| {
                // use newlines if the header is multiple lines
                let resolved_parent_start_info = condition_context.get_resolved_info(&parent_start_info)?;
                if resolved_parent_start_info.line_number < condition_context.writer_info.line_number {
                    return Some(true);
                }

                // use newlines if the entire jsx element is on multiple lines
                return condition_resolvers::is_multiple_lines(condition_context, &parent_start_info, &parent_end_info);
            },
            parse_for_new_lines(parsed_children.clone(), opts.inner_span, context),
            parse_for_single_line(parsed_children, context),
        ).into();
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
            if found_non_space_child && node_helpers::has_jsx_space_expr_text(&child) {
                current_jsx_space_exprs.push(child);
                continue;
            }
            let child_text = child.text_fast(context.module);
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

    fn parse_for_new_lines<'a>(children: Vec<(Node<'a>, Option<PrintItemPath>)>, inner_span: Span, context: &mut Context<'a>) -> PrintItems {
        let mut items = PrintItems::new();
        let has_children = !children.is_empty();
        items.push_signal(Signal::NewLine);
        items.extend(parser_helpers::with_indent(parse_members(ParseMembersOptions {
            inner_span,
            items: children.into_iter().map(|(a, b)| (a, Some(b.into()))).collect(),
            should_use_space: Some(Box::new(|previous, next, context| {
                if has_jsx_space_between(previous, next, &context.module) {
                    true
                } else if let Node::JSXText(element) = previous {
                    element.text_fast(context.module).ends_with(" ")
                } else if let Node::JSXText(element) = next {
                    element.text_fast(context.module).starts_with(" ")
                } else {
                    false
                }
            })),
            should_use_new_line: Some(Box::new(|previous, next, context| {
                if has_jsx_space_between(previous, next, &context.module) {
                    false // prefer collapsing
                } else if let Node::JSXText(next) = next {
                    !utils::has_no_new_lines_in_leading_whitespace(next.text_fast(context.module))
                } else if let Node::JSXText(previous) = previous {
                    !utils::has_no_new_lines_in_trailing_whitespace(previous.text_fast(context.module))
                } else {
                    true
                }
            })),
            should_use_blank_line: |previous, next, context| {
                if has_jsx_space_between(previous, next, &context.module) {
                    false // prefer collapsing
                } else if let Node::JSXText(previous) = previous {
                    utils::has_new_line_occurrences_in_trailing_whitespace(previous.text_fast(context.module), 2)
                } else if let Node::JSXText(next) = next {
                    utils::has_new_line_occurrences_in_leading_whitespace(next.text_fast(context.module), 2)
                } else {
                    node_helpers::has_separating_blank_line(previous, next, context.module)
                }
            },
            separator: Separator::none(),
            is_jsx_children: true,
        }, context)));

        if has_children {
            items.push_signal(Signal::NewLine);
        }

        items
    }

    fn parse_for_single_line<'a>(children: Vec<(Node<'a>, Option<PrintItemPath>)>, context: &mut Context<'a>) -> PrintItems {
        let mut items = PrintItems::new();
        if children.is_empty() {
            items.push_signal(Signal::PossibleNewLine);
        } else {
            let mut previous_child = None;
            for (index, (child, parsed_child)) in children.into_iter().enumerate() {
                if index > 0 && should_use_space(previous_child.as_ref().unwrap(), &child, context) {
                    items.extend(jsx_space_separator(
                        previous_child.as_ref().unwrap(),
                        &child,
                        context,
                    ));
                } else {
                    items.push_signal(Signal::PossibleNewLine);
                }

                items.extend(parsed_child.into());

                previous_child = Some(child);
            }
            items.push_signal(Signal::PossibleNewLine);
        }
        items
    }

    fn should_use_space(previous_child: &Node, current: &Node, context: &mut Context) -> bool {
        if has_jsx_space_between(previous_child, current, &context.module) {
            return true;
        }

        let past_token = context.token_finder.get_previous_token(current);
        if let Some(TokenAndSpan { token: swc_ecmascript::parser::token::Token::JSXText { .. }, span, had_line_break }) = past_token {
            let text = span.text_fast(context.module);
            if !had_line_break && text.ends_with(" ") {
                return true;
            }
        }
        if let Node::JSXText(child) = current {
            child.text_fast(context.module).starts_with(" ")
        } else {
            false
        }
    }

    /// If the node has a "JSX space expression" between or text that's only spaces between.
    fn has_jsx_space_between(previous_node: &Node, next_node: &Node, module: &Module) -> bool {
        return node_helpers::nodes_have_only_spaces_between(previous_node, next_node, module)
            || has_jsx_space_expr_between(previous_node, next_node);

        fn has_jsx_space_expr_between(previous_node: &Node, next_node: &Node) -> bool {
            let nodes_between = node_helpers::get_siblings_between(previous_node, next_node);

            for node_between in nodes_between {
                if node_helpers::has_jsx_space_expr_text(&node_between) {
                    return true;
                }
            }

            false
        }
    }
}

fn jsx_space_separator(previous_node: &Node, current_node: &Node, context: &Context) -> PrintItems {
    return if node_should_force_newline_if_multi_line(previous_node) || node_should_force_newline_if_multi_line(current_node) {
        jsx_force_space_with_newline_if_either_node_multi_line(
            previous_node,
            current_node,
            context,
        )
    } else {
        jsx_space_or_newline_or_expr_space(
            previous_node,
            current_node,
            context,
        )
    };

    fn node_should_force_newline_if_multi_line(node: &Node) -> bool {
        matches!(node, Node::JSXElement(_) | Node::JSXFragment(_))
    }

    fn get_node_info_range(node: &Node, context: &Context) -> Option<(Info, Info)> {
        if node_should_force_newline_if_multi_line(node) {
            context.get_info_range_for_node(node)
        } else {
            None
        }
    }

    fn jsx_force_space_with_newline_if_either_node_multi_line(
        previous_node: &Node,
        current_node: &Node,
        context: &Context
    ) -> PrintItems {
        let previous_node_info_range = get_node_info_range(previous_node, context);
        let current_node_info_range = get_node_info_range(current_node, context);
        let spaces_between_count = node_helpers::count_spaces_between_jsx_children(
            previous_node,
            current_node,
            &context.module
        );
        let jsx_space_expr_text = format!("{{\"{}\"}}", " ".repeat(spaces_between_count));
        if_true_or(
            "jsxIsLastChildMultiLine",
            move |condition_context| {
                if let Some((start_info, end_info)) = previous_node_info_range {
                    let result = condition_resolvers::is_multiple_lines(condition_context, &start_info, &end_info)?;
                    if result {
                        return Some(true);
                    }
                }
                if let Some((start_info, end_info)) = current_node_info_range {
                    let result = condition_resolvers::is_multiple_lines(condition_context, &start_info, &end_info)?;
                    if result {
                        return Some(true);
                    }
                }

                return Some(false);
            },
            {
                let mut items = PrintItems::new();
                items.push_signal(Signal::PossibleNewLine);
                items.push_str(&jsx_space_expr_text);
                items.push_signal(Signal::NewLine);
                items
            },
            {
                let mut items = PrintItems::new();
                if spaces_between_count > 1 {
                    items.push_signal(Signal::PossibleNewLine);
                    items.push_str(&jsx_space_expr_text);
                    items.push_signal(Signal::PossibleNewLine);
                } else {
                    items.extend(jsx_space_or_newline_or_expr_space(
                        previous_node,
                        current_node,
                        context
                    ));
                }
                items
            },
        ).into()
    }

    fn jsx_space_or_newline_or_expr_space(previous_node: &Node, current_node: &Node, context: &Context) -> PrintItems {
        let spaces_between_count = node_helpers::count_spaces_between_jsx_children(previous_node, current_node, &context.module);
        let mut items = PrintItems::new();

        if spaces_between_count > 1 {
            items.push_signal(Signal::PossibleNewLine);
            items.push_str(&format!("{{\"{}\"}}", " ".repeat(spaces_between_count)));
            items.push_signal(Signal::PossibleNewLine);
            return items;
        }

        let start_info = Info::new("jsxSpaceStartInfo");
        let end_info = Info::new("jsxSpaceEndInfo");

        // Force resolving the end_info again when the start_info changes its position
        // Note: This actually might not be required, but probably good to do just in case
        // todo: This probably could be pushed down into dprint_core with better design.
        // The true and false path being blank implies that probably a new kind of print item
        // should be introduced
        items.push_condition(Condition::new("resetEndInfoOnStartInfoLineNumberChange", ConditionProperties {
            condition: Rc::new(Box::new(move |condition_context| {
                let resolved_start_info = condition_context.get_resolved_info(&start_info)?;
                if resolved_start_info.line_number != condition_context.writer_info.line_number {
                    condition_context.clear_info(&end_info);
                }
                Some(false)
            })),
            true_path: None,
            false_path: None,
        }));
        items.push_info(start_info);

        items.push_condition(Condition::new_with_dependent_infos("jsxSpaceOrNewLineIsMultipleLines", ConditionProperties {
            condition: Rc::new(Box::new(move |context| {
                let resolved_start_info = context.get_resolved_info(&start_info)?;
                let resolved_end_info = context.get_resolved_info(&end_info)?;
                Some(resolved_start_info.line_number < resolved_end_info.line_number)
            })),
            true_path: {
                let mut items = PrintItems::new();
                items.push_signal(Signal::PossibleNewLine);
                items.push_str("{\" \"}");
                items.push_signal(Signal::NewLine);
                Some(items)
            },
            false_path: Some(Signal::SpaceOrNewLine.into()),
        }, vec![end_info]));
        items.push_info(end_info);
        items
    }
}

#[inline]
fn parse_assignment<'a>(expr: Node<'a>, op: &str, context: &mut Context<'a>) -> PrintItems {
    parse_assignment_op_to(expr, op, op, context)
}

#[inline]
fn parse_assignment_op_to<'a>(expr: Node<'a>, op: &str, op_to: &str, context: &mut Context<'a>) -> PrintItems {
    let op_token = context.token_finder.get_previous_token(&expr);
    #[cfg(debug_assertions)]
    assert_has_op(op, op_token, context);

    parse_assignment_like_with_token(expr, op_to, op_token, context)
}

fn parse_assignment_like_with_token<'a>(expr: Node<'a>, op: &str, op_token: Option<&TokenAndSpan>, context: &mut Context<'a>) -> PrintItems {
    let use_new_line_group = get_use_new_line_group(&expr);
    let mut items = PrintItems::new();

    if op == ":" { items.push_str(op) } else { items.push_str(&format!(" {}", op)) }; // good enough for now...

    let op_end = op_token.map(|x| x.hi()).unwrap_or_else(|| context.token_finder.get_previous_token_end_before(&expr));
    let op_trailing_comments = get_op_trailing_comments(op_end, context);
    let had_op_trailing_comments = !op_trailing_comments.is_empty();
    if !op_trailing_comments.is_empty() {
        items.extend(with_indent({
            let mut items = PrintItems::new();
            if let Some(first_comment) = op_trailing_comments.iter().next() {
                if first_comment.kind == CommentKind::Block {
                    items.push_signal(Signal::SpaceIfNotTrailing);
                }
            }
            items.extend(parse_comment_collection(op_trailing_comments.into_iter(), Some(&op_end), None, context));
            items
        }));
        items.push_signal(Signal::NewLine);
    }

    let parsed_assignment = {
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
                Signal::SpaceIfNotTrailing.into()
            ).into());
        }
        let assignment = parse_node(expr, context);
        let assignment = if had_op_trailing_comments { assignment } else { conditions::indent_if_start_of_line(assignment).into() };
        let assignment = if use_new_line_group { new_line_group(assignment) } else { assignment };
        items.extend(assignment);
        items
    }.into_rc_path();

    items.push_condition(if_true_or(
        "indentIfStartOfLineIndentedOrTokenHadTrailingLineComment",
        move |context| Some(had_op_trailing_comments || condition_resolvers::is_start_of_line_indented(context)),
        with_indent(parsed_assignment.clone().into()),
        parsed_assignment.into()
    ));

    return items;

    fn get_use_new_line_group(expr: &Node) -> bool {
        match expr {
            Node::MemberExpr(_) => true,
            _ => false,
        }
    }

    fn get_op_trailing_comments<'a>(previous_token_end: BytePos, context: &mut Context<'a>) -> Vec<&'a Comment> {
        let mut comments = Vec::new();
        let trailing_comments = previous_token_end.trailing_comments_fast(context.module);
        if !trailing_comments.is_empty() {
            let next_token_pos = context.token_finder.get_next_token_pos_after(&previous_token_end);
            let next_token_start_line = next_token_pos.start_line_fast(context.module);

            for comment in trailing_comments {
                if !context.has_handled_comment(comment) && comment.start_line_fast(context.module) < next_token_start_line {
                    comments.push(comment);
                }
            }
        }
        comments
    }
}

struct ParseBlockOptions<'a> {
    span: Option<Span>,
    children: Vec<Node<'a>>,
}

fn parse_block<'a>(
    parse_inner: impl FnOnce(Vec<Node<'a>>, &mut Context<'a>) -> PrintItems,
    opts: ParseBlockOptions<'a>,
    context: &mut Context<'a>
) -> PrintItems {
    let mut items = PrintItems::new();
    let before_open_token_info = Info::new("after_open_token_info");
    let first_member_span = opts.children.get(0).map(|x| x.span());
    let span = opts.span;
    items.push_info(before_open_token_info);
    items.extend(parse_surrounded_by_tokens(|context| {
        let mut items = PrintItems::new();
        let start_inner_info = Info::new("startStatementsInfo");
        let end_inner_info = Info::new("endStatementsInfo");
        let is_tokens_same_line_and_empty = if let Some(span) = &span {
             span.start_line_fast(context.module) == span.end_line_fast(context.module) && opts.children.is_empty()
        } else { true };
        if !is_tokens_same_line_and_empty {
            items.push_signal(Signal::NewLine);
        }
        items.push_info(start_inner_info);
        items.extend(parser_helpers::with_indent(parse_inner(opts.children, context)));
        items.push_info(end_inner_info);

        if is_tokens_same_line_and_empty {
            items.push_condition(if_true(
                "newLineIfDifferentLine",
                move |context| condition_resolvers::is_on_different_line(context, &before_open_token_info),
                Signal::NewLine.into()
            ));
        } else {
            items.push_condition(if_false(
                "endNewline",
                move |context| condition_resolvers::are_infos_equal(context, &start_inner_info, &end_inner_info),
                Signal::NewLine.into(),
            ));
        }
        items
    }, |_| None, ParseSurroundedByTokensOptions {
        open_token: "{",
        close_token: "}",
        span,
        first_member: first_member_span,
        prefer_single_line_when_empty: false,
        allow_open_token_trailing_comments: true,
    }, context));
    items
}

struct ParseSurroundedByTokensOptions {
    open_token: &'static str,
    close_token: &'static str,
    /// When `None`, means the tokens are missing
    span: Option<Span>,
    first_member: Option<Span>,
    prefer_single_line_when_empty: bool,
    allow_open_token_trailing_comments: bool,
}

fn parse_surrounded_by_tokens<'a>(
    parse_inner: impl FnOnce(&mut Context<'a>) -> PrintItems,
    custom_close_token: impl FnOnce(&mut Context<'a>) -> Option<PrintItems>,
    opts: ParseSurroundedByTokensOptions,
    context: &mut Context<'a>
) -> PrintItems {
    let mut items = PrintItems::new();
    if let Some(span) = opts.span {
        let open_token_end = BytePos(span.lo.0 + (opts.open_token.len() as u32));
        let close_token_start = BytePos(span.hi.0 - (opts.close_token.len() as u32));

        // assert the tokens are in the place the caller says they are
        #[cfg(debug_assertions)]
        context.assert_text(create_span(span.lo, open_token_end.lo()), opts.open_token);
        #[cfg(debug_assertions)]
        context.assert_text(create_span(close_token_start.lo(), span.hi), opts.close_token);

        // parse
        let open_token_start_line = open_token_end.start_line_fast(context.module);

        items.extend(parse_leading_comments(&span, context));
        items.push_str(opts.open_token);
        if let Some(first_member) = opts.first_member {
            let first_member_start_line = first_member.start_line_fast(context.module);
            if opts.allow_open_token_trailing_comments && open_token_start_line < first_member_start_line {
                items.extend(parse_first_line_trailing_comment(open_token_start_line, open_token_end.trailing_comments_fast(context.module), context));
            }
            items.extend(parse_inner(context));

            let before_trailing_comments_info = Info::new("beforeTrailingComments");
            items.push_info(before_trailing_comments_info);
            items.extend(with_indent(parse_trailing_comments_as_statements(&open_token_end, context)));
            items.extend(with_indent(parse_comments_as_statements(close_token_start.leading_comments_fast(context.module), None, context)));
            items.push_condition(if_true(
                "newLineIfHasCommentsAndNotStartOfNewLine",
                move |context| {
                    let had_comments = !condition_resolvers::is_at_same_position(context, &before_trailing_comments_info)?;
                    return Some(had_comments && !context.writer_info.is_start_of_line())
                },
                Signal::NewLine.into()
            ));
        } else {
            let comments = open_token_end.trailing_comments_fast(context.module);
            let is_single_line = open_token_start_line == close_token_start.start_line_fast(context.module);
            if !comments.is_empty() {
                // parse the trailing comment on the first line only if multi-line and if a comment line
                if !is_single_line {
                    items.extend(parse_first_line_trailing_comment(open_token_start_line, comments.clone(), context));
                }

                // parse the comments
                if comments.clone().any(|c| !context.has_handled_comment(c)) {
                    if is_single_line {
                        let indent_width = context.config.indent_width;
                        items.extend(parser_helpers::parse_separated_values(|_| {
                            let mut parsed_comments = Vec::new();
                            for c in comments {
                                let start_line = c.start_line_fast(context.module);
                                let end_line = c.end_line_fast(context.module);
                                if let Some(items) = parse_comment(c, context) {
                                    parsed_comments.push(parser_helpers::ParsedValue {
                                        items,
                                        lines_span: Some(parser_helpers::LinesSpan { start_line, end_line }),
                                        allow_inline_multi_line: false,
                                        allow_inline_single_line: false,
                                    });
                                }
                            }
                            parsed_comments
                        }, parser_helpers::ParseSeparatedValuesOptions {
                            prefer_hanging: false,
                            force_use_new_lines: !is_single_line,
                            allow_blank_lines: true,
                            single_line_space_at_start: false,
                            single_line_space_at_end: false,
                            single_line_separator: Signal::SpaceOrNewLine.into(),
                            indent_width,
                            multi_line_options: parser_helpers::MultiLineOptions::surround_newlines_indented(),
                            force_possible_newline_at_start: false,
                        }).items);
                    } else {
                        items.push_signal(Signal::NewLine);
                        items.extend(with_indent(parse_comments_as_statements(comments, None, context)));
                        items.push_signal(Signal::NewLine);
                    }
                }
            } else {
                if !is_single_line && !opts.prefer_single_line_when_empty {
                    items.push_signal(Signal::NewLine);
                }
            }
        }
    } else {
        // todo: have a warning here when this happens
        items.push_str(opts.open_token);
        items.extend(parse_inner(context));
    }

    if let Some(parsed_close_token) = (custom_close_token)(context) {
        items.extend(parsed_close_token);
    } else {
        items.push_str(opts.close_token);
    }

    return items;

    fn parse_first_line_trailing_comment(open_token_start_line: usize, comments: CommentsIterator, context: &mut Context) -> PrintItems {
        let mut items = PrintItems::new();
        let first_comment = comments.into_iter().next();
        if let Some(first_comment) = first_comment {
            if first_comment.kind == CommentKind::Line && first_comment.start_line_fast(context.module) == open_token_start_line {
                if let Some(parsed_comment) = parse_comment(&first_comment, context) {
                    items.push_signal(Signal::StartForceNoNewLines);
                    items.push_str(" ");
                    items.extend(parsed_comment);
                    items.push_signal(Signal::FinishForceNoNewLines);
                }
            }
        }
        items
    }
}

#[cfg(debug_assertions)]
fn assert_has_op<'a>(op: &str, op_token: Option<&TokenAndSpan>, context: &mut Context<'a>) {
    if let Some(op_token) = op_token {
        context.assert_text(create_span(op_token.lo(), op_token.hi()), op);
    } else {
        panic!("Debug panic! Expected to have op token: {}", op);
    }
}

fn use_new_line_group_for_arrow_body(arrow_expr: &ArrowExpr) -> bool {
    match &arrow_expr.body {
        BlockStmtOrExpr::Expr(expr) => match expr {
            Expr::Paren(paren) => match paren.expr {
                Expr::Object(_) => false,
                _ => true,
            },
            _ => true,
        },
        _ => true,
    }
}

/* is/has functions */

fn is_arrow_function_with_expr_body(node: &Node) -> bool {
    match node {
        Node::ExprOrSpread(expr_or_spread) => {
            match expr_or_spread.expr {
                Expr::Arrow(arrow) => {
                    match &arrow.body {
                        BlockStmtOrExpr::Expr(_) => true,
                        _ => false,
                    }
                },
                _ => false,
            }
        },
        _ => false,
    }
}

fn allows_inline_multi_line(node: &Node, has_siblings: bool) -> bool {
    return match node {
        Node::Param(param) => allows_inline_multi_line(&param.pat.into(), has_siblings),
        Node::TsAsExpr(as_expr) => allows_inline_multi_line(&as_expr.expr.into(), has_siblings)
            && match as_expr.type_ann {
                TsType::TsTypeRef(_) | TsType::TsKeywordType(_) => true,
                _ => allows_inline_multi_line(&as_expr.type_ann.into(), has_siblings)
            },
        Node::FnExpr(_) | Node::ArrowExpr(_) | Node::ObjectLit(_) | Node::ArrayLit(_)
            | Node::ObjectPat(_) | Node::ArrayPat(_)
            | Node::TsTypeLit(_) | Node::TsTupleType(_)
            | Node::TsArrayType(_) => true,
        Node::ExprOrSpread(node) => allows_inline_multi_line(&node.expr.into(), has_siblings),
        Node::TaggedTpl(_) | Node::Tpl(_) => !has_siblings,
        Node::CallExpr(node) => !has_siblings && allow_inline_for_call_expr(node),
        Node::BindingIdent(node) => match &node.type_ann {
            Some(type_ann) => allows_inline_multi_line(&type_ann.type_ann.into(), has_siblings),
            None => false,
        },
        Node::AssignPat(node) => allows_inline_multi_line(&node.left.into(), has_siblings)
            || allows_inline_multi_line(&node.right.into(), has_siblings),
        Node::TsTypeAnn(type_ann) => allows_inline_multi_line(&type_ann.type_ann.into(), has_siblings),
        Node::TsTupleElement(tuple_element) => allows_inline_multi_line(&tuple_element.ty.into(), has_siblings),
        _ => false,
    };

    fn allow_inline_for_call_expr(node: &CallExpr) -> bool {
        // do not allow call exprs with nested call exprs in the member expr to be inline
        return allow_for_expr_or_super(&node.callee);

        fn allow_for_expr_or_super(expr_or_super: &ExprOrSuper) -> bool {
            match expr_or_super {
                ExprOrSuper::Expr(expr) => {
                    match expr {
                        Expr::Member(member_expr) => allow_for_expr_or_super(&member_expr.obj),
                        Expr::Call(_) => false,
                        _=> true,
                    }
                },
                ExprOrSuper::Super(_) => true,
            }
        }
    }
}

fn get_use_new_lines_for_nodes_with_preceeding_token(open_token_text: &str, nodes: &Vec<impl Spanned>, prefer_single_line: bool, context: &mut Context) -> bool {
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
            if previous_token.text_fast(context.module) == open_token_text {
                return node_helpers::get_use_new_lines_for_nodes(previous_token, first_node, context.module);
            }
        }

        // arrow function expressions might not have an open paren (ex. `a => a + 5`)
        false
    }
}

fn get_use_new_lines_for_nodes(nodes: &Vec<impl Spanned>, prefer_single_line: bool, context: &mut Context) -> bool {
    if nodes.len() < 2 {
        return false;
    }

    if prefer_single_line {
        // basic rule: if any comments exist on separate lines, then everything becomes multi-line
        has_any_node_comment_on_different_line(nodes, context)
    } else {
        node_helpers::get_use_new_lines_for_nodes(&nodes[0], &nodes[1], context.module)
    }
}

/// Gets if any of the provided nodes have leading or trailing comments on a different line.
fn has_any_node_comment_on_different_line(nodes: &Vec<impl Spanned>, context: &mut Context) -> bool {
    for (i, node) in nodes.iter().enumerate() {
        if i == 0 {
            let first_node_start_line = node.start_line_fast(context.module);
            let mut comments_previous_lines = node.leading_comments_fast(context.module)
                .take_while(|c| c.kind == CommentKind::Line || c.start_line_fast(context.module) < first_node_start_line);
            if comments_previous_lines.next().is_some() {
                return true;
            }
        }

        let node_end = node.hi();
        let next_node_pos = nodes.get(i + 1).map(|n| n.lo());
        if check_pos_has_trailing_comments(node_end, next_node_pos, context) {
            return true;
        } else if let Some(comma) = context.token_finder.get_next_token_if_comma(&node_end) {
            if check_pos_has_trailing_comments(comma.hi(), next_node_pos, context) {
                return true;
            }
        }
    }


    return false;

    fn check_pos_has_trailing_comments(end: BytePos, next_node_pos: Option<BytePos>, context: &mut Context) -> bool {
        let end_line = end.end_line_fast(context.module);
        let stop_line = next_node_pos.map(|p| p.start_line_fast(context.module));

        for c in end.trailing_comments_fast(context.module) {
            if c.kind == CommentKind::Line {
                return true;
            }
            if let Some(stop_line) = stop_line {
                if c.start_line_fast(context.module) >= stop_line {
                    // do not look at comments that the next node owns
                    return false;
                }
            }
            if c.end_line_fast(context.module) > end_line {
                return true;
            }
        }

        false
    }
}

/* config helpers */

fn get_parsed_separator(separator: &Separator, is_trailing: bool, is_multi_line: &(impl Fn(&mut ConditionResolverContext) -> Option<bool> + Clone + 'static)) -> PrintItems {
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
        ).into()
    };

    fn get_items(value: &Option<SeparatorValue>, is_trailing: bool, is_multi_line: &(impl Fn(&mut ConditionResolverContext) -> Option<bool> + Clone + 'static)) -> PrintItems {
        match value {
            Some(SeparatorValue::Comma(trailing_comma)) => get_parsed_trailing_comma(*trailing_comma, is_trailing, is_multi_line),
            Some(SeparatorValue::SemiColon(semi_colons)) => get_parsed_semi_colon(*semi_colons, is_trailing, is_multi_line),
            None => PrintItems::new(),
        }
    }
}

fn get_parsed_trailing_comma(option: TrailingCommas, is_trailing: bool, is_multi_line: &(impl Fn(&mut ConditionResolverContext) -> Option<bool> + Clone + 'static)) -> PrintItems {
    if !is_trailing { return ",".into(); }

    match option {
        TrailingCommas::Always => ",".into(),
        TrailingCommas::OnlyMultiLine => {
            if_true("trailingCommaIfMultiLine", is_multi_line.clone(), ",".into()).into()
        },
        TrailingCommas::Never => {
            PrintItems::new()
        },
    }
}

fn get_parsed_semi_colon(option: SemiColons, is_trailing: bool, is_multi_line: &(impl Fn(&mut ConditionResolverContext) -> Option<bool> + Clone + 'static)) -> PrintItems {
    match option {
        SemiColons::Always => ";".into(),
        SemiColons::Prefer => {
            if is_trailing {
                if_true("semiColonIfMultiLine", is_multi_line.clone(), ";".into()).into()
            } else {
                ";".into()
            }
        },
        SemiColons::Asi => {
            if is_trailing {
                PrintItems::new()
            } else {
                if_false("semiColonIfSingleLine", is_multi_line.clone(), ";".into()).into()
            }
        },
    }
}

fn get_comma_tokens_from_children_with_tokens<'a>(node: &Node<'a>, module: &Module<'a>) -> Vec<&'a TokenAndSpan> {
    node.children_with_tokens_fast(module).into_iter().filter_map(|c| match c {
        NodeOrToken::Token(token) => if token.token == Token::Comma {
            Some(token)
        } else {
            None
        },
        _ => None,
    }).collect::<Vec<_>>()
}

fn get_tokens_from_children_with_tokens<'a>(node: &Node<'a>, module: &Module<'a>) -> Vec<&'a TokenAndSpan> {
    node.children_with_tokens_fast(module).into_iter().filter_map(|n| match n {
        NodeOrToken::Token(t) => Some(t),
        _ => None,
    }).collect::<Vec<_>>()
}

fn create_span(lo: BytePos, hi: BytePos) -> Span {
    Span { lo, hi, ctxt: Default::default() }
}
