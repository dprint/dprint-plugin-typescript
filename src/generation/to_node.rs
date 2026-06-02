// oxc-port: converters from oxc's recursive enums to `Node` (`AstKind`).
//
// The SWC view layer provided `From`/`.into()` to turn any child node into the
// unified `Node` enum. oxc only ships `AstKind::from_expression`; this module
// adds the equivalents for the other enums the formatter recurses through.
//
// Each arm relies on `Deref` coercion from `&'a Box<'a, T>` (oxc's arena box)
// to `&'a T`, the same trick oxc's own `from_expression` uses.

use deno_ast::oxc::ast::ast::*;
use deno_ast::oxc::ast::AstKind;

use super::oxc_helpers::Node;

#[inline]
pub fn expr_to_node<'a>(e: &'a Expression<'a>) -> Node<'a> {
  AstKind::from_expression(e)
}

pub fn stmt_to_node<'a>(s: &'a Statement<'a>) -> Node<'a> {
  match s {
    Statement::BlockStatement(n) => AstKind::BlockStatement(n),
    Statement::BreakStatement(n) => AstKind::BreakStatement(n),
    Statement::ContinueStatement(n) => AstKind::ContinueStatement(n),
    Statement::DebuggerStatement(n) => AstKind::DebuggerStatement(n),
    Statement::DoWhileStatement(n) => AstKind::DoWhileStatement(n),
    Statement::EmptyStatement(n) => AstKind::EmptyStatement(n),
    Statement::ExpressionStatement(n) => AstKind::ExpressionStatement(n),
    Statement::ForInStatement(n) => AstKind::ForInStatement(n),
    Statement::ForOfStatement(n) => AstKind::ForOfStatement(n),
    Statement::ForStatement(n) => AstKind::ForStatement(n),
    Statement::IfStatement(n) => AstKind::IfStatement(n),
    Statement::LabeledStatement(n) => AstKind::LabeledStatement(n),
    Statement::ReturnStatement(n) => AstKind::ReturnStatement(n),
    Statement::SwitchStatement(n) => AstKind::SwitchStatement(n),
    Statement::ThrowStatement(n) => AstKind::ThrowStatement(n),
    Statement::TryStatement(n) => AstKind::TryStatement(n),
    Statement::WhileStatement(n) => AstKind::WhileStatement(n),
    Statement::WithStatement(n) => AstKind::WithStatement(n),
    // Declaration (inherited variants)
    Statement::VariableDeclaration(n) => AstKind::VariableDeclaration(n),
    Statement::FunctionDeclaration(n) => AstKind::Function(n),
    Statement::ClassDeclaration(n) => AstKind::Class(n),
    Statement::TSTypeAliasDeclaration(n) => AstKind::TSTypeAliasDeclaration(n),
    Statement::TSInterfaceDeclaration(n) => AstKind::TSInterfaceDeclaration(n),
    Statement::TSEnumDeclaration(n) => AstKind::TSEnumDeclaration(n),
    Statement::TSModuleDeclaration(n) => AstKind::TSModuleDeclaration(n),
    Statement::TSGlobalDeclaration(n) => AstKind::TSGlobalDeclaration(n),
    Statement::TSImportEqualsDeclaration(n) => AstKind::TSImportEqualsDeclaration(n),
    // ModuleDeclaration (inherited variants)
    Statement::ImportDeclaration(n) => AstKind::ImportDeclaration(n),
    Statement::ExportAllDeclaration(n) => AstKind::ExportAllDeclaration(n),
    Statement::ExportDefaultDeclaration(n) => AstKind::ExportDefaultDeclaration(n),
    Statement::ExportNamedDeclaration(n) => AstKind::ExportNamedDeclaration(n),
    Statement::TSExportAssignment(n) => AstKind::TSExportAssignment(n),
    Statement::TSNamespaceExportDeclaration(n) => AstKind::TSNamespaceExportDeclaration(n),
  }
}

/// Array literal elements; `Elision` (a sparse hole) maps to `None`.
pub fn array_element_to_node<'a>(e: &'a ArrayExpressionElement<'a>) -> Option<Node<'a>> {
  match e {
    ArrayExpressionElement::SpreadElement(n) => Some(AstKind::SpreadElement(n)),
    ArrayExpressionElement::Elision(_) => None,
    it @ match_expression!(ArrayExpressionElement) => Some(expr_to_node(it.to_expression())),
  }
}

pub fn for_stmt_init_to_node<'a>(init: &'a ForStatementInit<'a>) -> Node<'a> {
  match init {
    ForStatementInit::VariableDeclaration(n) => AstKind::VariableDeclaration(n),
    it @ match_expression!(ForStatementInit) => expr_to_node(it.to_expression()),
  }
}

pub fn for_stmt_left_to_node<'a>(left: &'a ForStatementLeft<'a>) -> Node<'a> {
  match left {
    ForStatementLeft::VariableDeclaration(n) => AstKind::VariableDeclaration(n),
    ForStatementLeft::AssignmentTargetIdentifier(n) => AstKind::IdentifierReference(n),
    ForStatementLeft::TSAsExpression(n) => AstKind::TSAsExpression(n),
    ForStatementLeft::TSSatisfiesExpression(n) => AstKind::TSSatisfiesExpression(n),
    ForStatementLeft::TSNonNullExpression(n) => AstKind::TSNonNullExpression(n),
    ForStatementLeft::TSTypeAssertion(n) => AstKind::TSTypeAssertion(n),
    ForStatementLeft::ComputedMemberExpression(n) => AstKind::ComputedMemberExpression(n),
    ForStatementLeft::StaticMemberExpression(n) => AstKind::StaticMemberExpression(n),
    ForStatementLeft::PrivateFieldExpression(n) => AstKind::PrivateFieldExpression(n),
    ForStatementLeft::ArrayAssignmentTarget(n) => AstKind::ArrayAssignmentTarget(n),
    ForStatementLeft::ObjectAssignmentTarget(n) => AstKind::ObjectAssignmentTarget(n),
  }
}

pub fn arg_to_node<'a>(a: &'a Argument<'a>) -> Node<'a> {
  match a {
    Argument::SpreadElement(n) => AstKind::SpreadElement(n),
    it @ match_expression!(Argument) => expr_to_node(it.to_expression()),
  }
}

pub fn simple_assignment_target_to_node<'a>(t: &'a SimpleAssignmentTarget<'a>) -> Node<'a> {
  match t {
    SimpleAssignmentTarget::AssignmentTargetIdentifier(n) => AstKind::IdentifierReference(n),
    SimpleAssignmentTarget::TSAsExpression(n) => AstKind::TSAsExpression(n),
    SimpleAssignmentTarget::TSSatisfiesExpression(n) => AstKind::TSSatisfiesExpression(n),
    SimpleAssignmentTarget::TSNonNullExpression(n) => AstKind::TSNonNullExpression(n),
    SimpleAssignmentTarget::TSTypeAssertion(n) => AstKind::TSTypeAssertion(n),
    SimpleAssignmentTarget::ComputedMemberExpression(n) => AstKind::ComputedMemberExpression(n),
    SimpleAssignmentTarget::StaticMemberExpression(n) => AstKind::StaticMemberExpression(n),
    SimpleAssignmentTarget::PrivateFieldExpression(n) => AstKind::PrivateFieldExpression(n),
  }
}

pub fn obj_prop_kind_to_node<'a>(p: &'a ObjectPropertyKind<'a>) -> Node<'a> {
  match p {
    ObjectPropertyKind::ObjectProperty(n) => AstKind::ObjectProperty(n),
    ObjectPropertyKind::SpreadProperty(n) => AstKind::SpreadElement(n),
  }
}

pub fn class_element_to_node<'a>(e: &'a ClassElement<'a>) -> Node<'a> {
  match e {
    ClassElement::StaticBlock(n) => AstKind::StaticBlock(n),
    ClassElement::MethodDefinition(n) => AstKind::MethodDefinition(n),
    ClassElement::PropertyDefinition(n) => AstKind::PropertyDefinition(n),
    ClassElement::AccessorProperty(n) => AstKind::AccessorProperty(n),
    ClassElement::TSIndexSignature(n) => AstKind::TSIndexSignature(n),
  }
}

pub fn binding_pattern_to_node<'a>(p: &'a BindingPattern<'a>) -> Node<'a> {
  match p {
    BindingPattern::BindingIdentifier(n) => AstKind::BindingIdentifier(n),
    BindingPattern::ObjectPattern(n) => AstKind::ObjectPattern(n),
    BindingPattern::ArrayPattern(n) => AstKind::ArrayPattern(n),
    BindingPattern::AssignmentPattern(n) => AstKind::AssignmentPattern(n),
  }
}

pub fn prop_key_to_node<'a>(key: &'a PropertyKey<'a>) -> Node<'a> {
  match key {
    PropertyKey::StaticIdentifier(n) => AstKind::IdentifierName(n),
    PropertyKey::PrivateIdentifier(n) => AstKind::PrivateIdentifier(n),
    it @ match_expression!(PropertyKey) => expr_to_node(it.to_expression()),
  }
}

pub fn assign_target_to_node<'a>(t: &'a AssignmentTarget<'a>) -> Node<'a> {
  match t {
    AssignmentTarget::AssignmentTargetIdentifier(n) => AstKind::IdentifierReference(n),
    AssignmentTarget::TSAsExpression(n) => AstKind::TSAsExpression(n),
    AssignmentTarget::TSSatisfiesExpression(n) => AstKind::TSSatisfiesExpression(n),
    AssignmentTarget::TSNonNullExpression(n) => AstKind::TSNonNullExpression(n),
    AssignmentTarget::TSTypeAssertion(n) => AstKind::TSTypeAssertion(n),
    AssignmentTarget::ComputedMemberExpression(n) => AstKind::ComputedMemberExpression(n),
    AssignmentTarget::StaticMemberExpression(n) => AstKind::StaticMemberExpression(n),
    AssignmentTarget::PrivateFieldExpression(n) => AstKind::PrivateFieldExpression(n),
    AssignmentTarget::ArrayAssignmentTarget(n) => AstKind::ArrayAssignmentTarget(n),
    AssignmentTarget::ObjectAssignmentTarget(n) => AstKind::ObjectAssignmentTarget(n),
  }
}

pub fn ts_type_to_node<'a>(t: &'a TSType<'a>) -> Node<'a> {
  match t {
    TSType::TSAnyKeyword(n) => AstKind::TSAnyKeyword(n),
    TSType::TSBigIntKeyword(n) => AstKind::TSBigIntKeyword(n),
    TSType::TSBooleanKeyword(n) => AstKind::TSBooleanKeyword(n),
    TSType::TSIntrinsicKeyword(n) => AstKind::TSIntrinsicKeyword(n),
    TSType::TSNeverKeyword(n) => AstKind::TSNeverKeyword(n),
    TSType::TSNullKeyword(n) => AstKind::TSNullKeyword(n),
    TSType::TSNumberKeyword(n) => AstKind::TSNumberKeyword(n),
    TSType::TSObjectKeyword(n) => AstKind::TSObjectKeyword(n),
    TSType::TSStringKeyword(n) => AstKind::TSStringKeyword(n),
    TSType::TSSymbolKeyword(n) => AstKind::TSSymbolKeyword(n),
    TSType::TSUndefinedKeyword(n) => AstKind::TSUndefinedKeyword(n),
    TSType::TSUnknownKeyword(n) => AstKind::TSUnknownKeyword(n),
    TSType::TSVoidKeyword(n) => AstKind::TSVoidKeyword(n),
    TSType::TSArrayType(n) => AstKind::TSArrayType(n),
    TSType::TSConditionalType(n) => AstKind::TSConditionalType(n),
    TSType::TSConstructorType(n) => AstKind::TSConstructorType(n),
    TSType::TSFunctionType(n) => AstKind::TSFunctionType(n),
    TSType::TSImportType(n) => AstKind::TSImportType(n),
    TSType::TSIndexedAccessType(n) => AstKind::TSIndexedAccessType(n),
    TSType::TSInferType(n) => AstKind::TSInferType(n),
    TSType::TSIntersectionType(n) => AstKind::TSIntersectionType(n),
    TSType::TSLiteralType(n) => AstKind::TSLiteralType(n),
    TSType::TSMappedType(n) => AstKind::TSMappedType(n),
    TSType::TSNamedTupleMember(n) => AstKind::TSNamedTupleMember(n),
    TSType::TSTemplateLiteralType(n) => AstKind::TSTemplateLiteralType(n),
    TSType::TSThisType(n) => AstKind::TSThisType(n),
    TSType::TSTupleType(n) => AstKind::TSTupleType(n),
    TSType::TSTypeLiteral(n) => AstKind::TSTypeLiteral(n),
    TSType::TSTypeOperatorType(n) => AstKind::TSTypeOperator(n),
    TSType::TSTypePredicate(n) => AstKind::TSTypePredicate(n),
    TSType::TSTypeQuery(n) => AstKind::TSTypeQuery(n),
    TSType::TSTypeReference(n) => AstKind::TSTypeReference(n),
    TSType::TSUnionType(n) => AstKind::TSUnionType(n),
    TSType::TSParenthesizedType(n) => AstKind::TSParenthesizedType(n),
    TSType::JSDocNullableType(n) => AstKind::JSDocNullableType(n),
    TSType::JSDocNonNullableType(n) => AstKind::JSDocNonNullableType(n),
    TSType::JSDocUnknownType(n) => AstKind::JSDocUnknownType(n),
  }
}
