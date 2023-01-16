use dprint_core::configuration::*;
use dprint_core::generate_str_to_from;
use serde::Deserialize;
use serde::Serialize;

/// Semi colon possibilities.
#[derive(Clone, PartialEq, Copy, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum SemiColons {
  /// Always uses semi-colons where applicable.
  Always,
  /// Prefers to use semi-colons, but doesn't add one in certain scenarios
  /// such as for the last member of a single-line type literal.
  Prefer,
  /// Uses automatic semi-colon insertion. Only adds a semi-colon at the start
  /// of some expression statements when necessary.
  Asi,
}

impl SemiColons {
  /// Gets if this option is "Always" or "Prefer".
  pub(crate) fn is_true(&self) -> bool {
    match self {
      SemiColons::Always | SemiColons::Prefer => true,
      SemiColons::Asi => false,
    }
  }
}

generate_str_to_from![SemiColons, [Always, "always"], [Prefer, "prefer"], [Asi, "asi"]];

/// Trailing comma possibilities.
#[derive(Clone, PartialEq, Copy, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum TrailingCommas {
  /// Trailing commas should not be used.
  Never,
  /// Trailing commas should always be used.
  Always,
  /// Trailing commas should only be used in multi-line scenarios.
  OnlyMultiLine,
}

generate_str_to_from![TrailingCommas, [Always, "always"], [Never, "never"], [OnlyMultiLine, "onlyMultiLine"]];

/// Where to place the opening brace.
#[derive(Clone, PartialEq, Copy, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum BracePosition {
  /// Maintains the brace being on the next line or the same line.
  Maintain,
  /// Forces the brace to be on the same line.
  SameLine,
  /// Forces the brace to be on the next line.
  NextLine,
  /// Forces the brace to be on the next line if the same line is hanging, but otherwise uses the same line.
  SameLineUnlessHanging,
}

generate_str_to_from![
  BracePosition,
  [Maintain, "maintain"],
  [SameLine, "sameLine"],
  [NextLine, "nextLine"],
  [SameLineUnlessHanging, "sameLineUnlessHanging"]
];

/// How to space members.
#[derive(Clone, PartialEq, Copy, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum MemberSpacing {
  /// Maintains whether a newline or blankline is used.
  Maintain,
  /// Forces a new line between members.
  #[serde(rename = "newLine")]
  NewLine,
  /// Forces a blank line between members.
  #[serde(rename = "blankLine")]
  BlankLine,
}

generate_str_to_from![MemberSpacing, [Maintain, "maintain"], [BlankLine, "blankLine"], [NewLine, "newLine"]];

/// Where to place the next control flow within a control flow statement.
#[derive(Clone, PartialEq, Copy, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum NextControlFlowPosition {
  /// Maintains the next control flow being on the next line or the same line.
  Maintain,
  /// Forces the next control flow to be on the same line.
  SameLine,
  /// Forces the next control flow to be on the next line.
  NextLine,
}

generate_str_to_from![NextControlFlowPosition, [Maintain, "maintain"], [SameLine, "sameLine"], [NextLine, "nextLine"]];

/// Where to place the operator for expressions that span multiple lines.
#[derive(Debug, Clone, PartialEq, Copy, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum OperatorPosition {
  /// Maintains the operator being on the next line or the same line.
  Maintain,
  /// Forces the operator to be on the same line.
  SameLine,
  /// Forces the operator to be on the next line.
  NextLine,
}

generate_str_to_from![OperatorPosition, [Maintain, "maintain"], [SameLine, "sameLine"], [NextLine, "nextLine"]];

/// Where to place a node that could be on the same line or next line.
#[derive(Clone, PartialEq, Copy, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum SameOrNextLinePosition {
  /// Maintains the position of the expression.
  Maintain,
  /// Forces the whole statement to be on one line.
  SameLine,
  /// Forces the expression to be on the next line.
  NextLine,
}

generate_str_to_from![SameOrNextLinePosition, [Maintain, "maintain"], [SameLine, "sameLine"], [NextLine, "nextLine"]];

/// If braces should be used or not in certain scenarios.
#[derive(Clone, PartialEq, Copy, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum UseBraces {
  /// Uses braces if they're used. Doesn't use braces if they're not used.
  Maintain,
  /// Uses braces when the body is on a different line.
  WhenNotSingleLine,
  /// Forces the use of braces. Will add them if they aren't used.
  Always,
  /// Forces no braces when the header is one line and body is one line. Otherwise forces braces.
  PreferNone,
}

generate_str_to_from![
  UseBraces,
  [Maintain, "maintain"],
  [WhenNotSingleLine, "whenNotSingleLine"],
  [Always, "always"],
  [PreferNone, "preferNone"]
];

/// Whether to use parentheses around a single parameter in an arrow function.
#[derive(Clone, PartialEq, Copy, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum UseParentheses {
  /// Maintains the current state of the parentheses.
  Maintain,
  /// Forces parentheses.
  Force,
  /// Prefers not using parentheses when possible.
  PreferNone,
}

generate_str_to_from![UseParentheses, [Maintain, "maintain"], [Force, "force"], [PreferNone, "preferNone"]];

/// How to decide to use single or double quotes.
#[derive(Clone, PartialEq, Copy, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum QuoteStyle {
  /// Always use double quotes.
  AlwaysDouble,
  /// Always use single quotes.
  AlwaysSingle,
  /// Prefer using double quotes except in scenarios where the string
  /// contains more double quotes than single quotes.
  PreferDouble,
  /// Prefer using single quotes except in scenarios where the string
  /// contains more single quotes than double quotes.
  PreferSingle,
}

impl QuoteStyle {
  /// Gets the associated JSX quote style.
  pub fn to_jsx_quote_style(&self) -> JsxQuoteStyle {
    match self {
      QuoteStyle::AlwaysDouble | QuoteStyle::PreferDouble => JsxQuoteStyle::PreferDouble,
      QuoteStyle::AlwaysSingle | QuoteStyle::PreferSingle => JsxQuoteStyle::PreferSingle,
    }
  }
}

generate_str_to_from![
  QuoteStyle,
  [AlwaysDouble, "alwaysDouble"],
  [AlwaysSingle, "alwaysSingle"],
  [PreferDouble, "preferDouble"],
  [PreferSingle, "preferSingle"]
];

/// Whether to use single or double quotes for JSX attributes.
#[derive(Clone, PartialEq, Copy, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum JsxQuoteStyle {
  /// Prefer using double quotes except in scenarios where the string
  /// contains more double quotes than single quotes.
  PreferDouble,
  /// Prefer using single quotes except in scenarios where the string
  /// contains more single quotes than double quotes.
  PreferSingle,
}

generate_str_to_from![JsxQuoteStyle, [PreferDouble, "preferDouble"], [PreferSingle, "preferSingle"]];

/// Behaviour to use for quotes on property names.
#[derive(Clone, PartialEq, Copy, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum QuoteProps {
  /// Preserve quotes around property names.
  Preserve,
  /// Remove unnecessary quotes around property names.
  AsNeeded,
}

generate_str_to_from![QuoteProps, [Preserve, "preserve"], [AsNeeded, "asNeeded"]];

/// Whether to surround a JSX element or fragment with parentheses
/// when it's the top JSX node and it spans multiple lines.
#[derive(Clone, PartialEq, Copy, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum JsxMultiLineParens {
  /// Never wrap JSX with parentheses.
  Never,
  /// Prefer wrapping with parentheses in most scenarios, except in function
  /// arguments and JSX attributes.
  Prefer,
  /// Always wrap JSX with parentheses if it spans multiple lines.
  Always,
}

generate_str_to_from![JsxMultiLineParens, [Never, "never"], [Prefer, "prefer"], [Always, "always"]];

/// Whether to use semi-colons or commas.
#[derive(Clone, PartialEq, Copy, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum SemiColonOrComma {
  /// Use semi colons (default).
  SemiColon,
  /// Use commas.
  Comma,
}

generate_str_to_from![SemiColonOrComma, [SemiColon, "semiColon"], [Comma, "comma"]];

/// Whether to use semi-colons, commas or new lines.
#[derive(Clone, PartialEq, Copy, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum SemiColonOrCommaOrNewLine {
  /// Use semi colons (default).
  SemiColon,
  /// Use commas.
  Comma,
  /// Use new lines.
  NewLine,
}

generate_str_to_from![SemiColonOrCommaOrNewLine, [SemiColon, "semiColon"], [Comma, "comma"], [NewLine, "newLine"]];

/// The kind of sort ordering to use.
#[derive(Clone, PartialEq, Copy, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum SortOrder {
  /// Maintains the current ordering.
  Maintain,
  /// Alphabetically and case sensitive.
  CaseSensitive,
  /// Alphabetically and case insensitive.
  CaseInsensitive,
}

generate_str_to_from![
  SortOrder,
  [Maintain, "maintain"],
  [CaseSensitive, "caseSensitive"],
  [CaseInsensitive, "caseInsensitive"]
];

#[derive(Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Configuration {
  pub indent_width: u8,
  pub line_width: u32,
  pub use_tabs: bool,
  pub new_line_kind: NewLineKind,
  pub quote_style: QuoteStyle,
  pub quote_props: QuoteProps,
  pub semi_colons: SemiColons,
  /* situational */
  #[serde(rename = "arrowFunction.useParentheses")]
  pub arrow_function_use_parentheses: UseParentheses,
  #[serde(rename = "binaryExpression.linePerExpression")]
  pub binary_expression_line_per_expression: bool,
  #[serde(rename = "conditionalExpression.linePerExpression")]
  pub conditional_expression_line_per_expression: bool,
  #[serde(rename = "jsx.quoteStyle")]
  pub jsx_quote_style: JsxQuoteStyle,
  #[serde(rename = "jsx.multiLineParens")]
  pub jsx_multi_line_parens: JsxMultiLineParens,
  #[serde(rename = "jsx.forceNewLinesSurroundingContent")]
  pub jsx_force_new_lines_surrounding_content: bool,
  #[serde(rename = "jsxOpeningElement.bracketPosition")]
  pub jsx_opening_element_bracket_position: SameOrNextLinePosition,
  #[serde(rename = "jsxSelfClosingElement.bracketPosition")]
  pub jsx_self_closing_element_bracket_position: SameOrNextLinePosition,
  #[serde(rename = "memberExpression.linePerExpression")]
  pub member_expression_line_per_expression: bool,
  #[serde(rename = "typeLiteral.separatorKind.singleLine")]
  pub type_literal_separator_kind_single_line: SemiColonOrComma,
  #[serde(rename = "typeLiteral.separatorKind.multiLine")]
  pub type_literal_separator_kind_multi_line: SemiColonOrCommaOrNewLine,
  /* sorting */
  #[serde(rename = "module.sortImportDeclarations")]
  pub module_sort_import_declarations: SortOrder,
  #[serde(rename = "module.sortExportDeclarations")]
  pub module_sort_export_declarations: SortOrder,
  #[serde(rename = "importDeclaration.sortNamedImports")]
  pub import_declaration_sort_named_imports: SortOrder,
  #[serde(rename = "exportDeclaration.sortNamedExports")]
  pub export_declaration_sort_named_exports: SortOrder,
  /* ignore comments */
  pub ignore_node_comment_text: String,
  pub ignore_file_comment_text: String,
  /* brace position */
  #[serde(rename = "arrowFunction.bracePosition")]
  pub arrow_function_brace_position: BracePosition,
  #[serde(rename = "classDeclaration.bracePosition")]
  pub class_declaration_brace_position: BracePosition,
  #[serde(rename = "classExpression.bracePosition")]
  pub class_expression_brace_position: BracePosition,
  #[serde(rename = "constructor.bracePosition")]
  pub constructor_brace_position: BracePosition,
  #[serde(rename = "doWhileStatement.bracePosition")]
  pub do_while_statement_brace_position: BracePosition,
  #[serde(rename = "enumDeclaration.bracePosition")]
  pub enum_declaration_brace_position: BracePosition,
  #[serde(rename = "getAccessor.bracePosition")]
  pub get_accessor_brace_position: BracePosition,
  #[serde(rename = "ifStatement.bracePosition")]
  pub if_statement_brace_position: BracePosition,
  #[serde(rename = "interfaceDeclaration.bracePosition")]
  pub interface_declaration_brace_position: BracePosition,
  #[serde(rename = "forStatement.bracePosition")]
  pub for_statement_brace_position: BracePosition,
  #[serde(rename = "forInStatement.bracePosition")]
  pub for_in_statement_brace_position: BracePosition,
  #[serde(rename = "forOfStatement.bracePosition")]
  pub for_of_statement_brace_position: BracePosition,
  #[serde(rename = "functionDeclaration.bracePosition")]
  pub function_declaration_brace_position: BracePosition,
  #[serde(rename = "functionExpression.bracePosition")]
  pub function_expression_brace_position: BracePosition,
  #[serde(rename = "method.bracePosition")]
  pub method_brace_position: BracePosition,
  #[serde(rename = "moduleDeclaration.bracePosition")]
  pub module_declaration_brace_position: BracePosition,
  #[serde(rename = "setAccessor.bracePosition")]
  pub set_accessor_brace_position: BracePosition,
  #[serde(rename = "staticBlock.bracePosition")]
  pub static_block_brace_position: BracePosition,
  #[serde(rename = "switchCase.bracePosition")]
  pub switch_case_brace_position: BracePosition,
  #[serde(rename = "switchStatement.bracePosition")]
  pub switch_statement_brace_position: BracePosition,
  #[serde(rename = "tryStatement.bracePosition")]
  pub try_statement_brace_position: BracePosition,
  #[serde(rename = "whileStatement.bracePosition")]
  pub while_statement_brace_position: BracePosition,
  /* prefer hanging */
  #[serde(rename = "arguments.preferHanging")]
  pub arguments_prefer_hanging: bool,
  #[serde(rename = "arrayExpression.preferHanging")]
  pub array_expression_prefer_hanging: bool,
  #[serde(rename = "arrayPattern.preferHanging")]
  pub array_pattern_prefer_hanging: bool,
  #[serde(rename = "doWhileStatement.preferHanging")]
  pub do_while_statement_prefer_hanging: bool,
  #[serde(rename = "exportDeclaration.preferHanging")]
  pub export_declaration_prefer_hanging: bool,
  #[serde(rename = "extendsClause.preferHanging")]
  pub extends_clause_prefer_hanging: bool,
  #[serde(rename = "forStatement.preferHanging")]
  pub for_statement_prefer_hanging: bool,
  #[serde(rename = "forInStatement.preferHanging")]
  pub for_in_statement_prefer_hanging: bool,
  #[serde(rename = "forOfStatement.preferHanging")]
  pub for_of_statement_prefer_hanging: bool,
  #[serde(rename = "ifStatement.preferHanging")]
  pub if_statement_prefer_hanging: bool,
  #[serde(rename = "implementsClause.preferHanging")]
  pub implements_clause_prefer_hanging: bool,
  #[serde(rename = "importDeclaration.preferHanging")]
  pub import_declaration_prefer_hanging: bool,
  #[serde(rename = "jsxAttributes.preferHanging")]
  pub jsx_attributes_prefer_hanging: bool,
  #[serde(rename = "objectExpression.preferHanging")]
  pub object_expression_prefer_hanging: bool,
  #[serde(rename = "objectPattern.preferHanging")]
  pub object_pattern_prefer_hanging: bool,
  #[serde(rename = "parameters.preferHanging")]
  pub parameters_prefer_hanging: bool,
  #[serde(rename = "sequenceExpression.preferHanging")]
  pub sequence_expression_prefer_hanging: bool,
  #[serde(rename = "switchStatement.preferHanging")]
  pub switch_statement_prefer_hanging: bool,
  #[serde(rename = "tupleType.preferHanging")]
  pub tuple_type_prefer_hanging: bool,
  #[serde(rename = "typeLiteral.preferHanging")]
  pub type_literal_prefer_hanging: bool,
  #[serde(rename = "typeParameters.preferHanging")]
  pub type_parameters_prefer_hanging: bool,
  #[serde(rename = "unionAndIntersectionType.preferHanging")]
  pub union_and_intersection_type_prefer_hanging: bool,
  #[serde(rename = "variableStatement.preferHanging")]
  pub variable_statement_prefer_hanging: bool,
  #[serde(rename = "whileStatement.preferHanging")]
  pub while_statement_prefer_hanging: bool,
  /* member spacing */
  #[serde(rename = "enumDeclaration.memberSpacing")]
  pub enum_declaration_member_spacing: MemberSpacing,
  /* next control flow position */
  #[serde(rename = "ifStatement.nextControlFlowPosition")]
  pub if_statement_next_control_flow_position: NextControlFlowPosition,
  #[serde(rename = "tryStatement.nextControlFlowPosition")]
  pub try_statement_next_control_flow_position: NextControlFlowPosition,
  #[serde(rename = "doWhileStatement.nextControlFlowPosition")]
  pub do_while_statement_next_control_flow_position: NextControlFlowPosition,
  /* operator position */
  #[serde(rename = "binaryExpression.operatorPosition")]
  pub binary_expression_operator_position: OperatorPosition,
  #[serde(rename = "conditionalExpression.operatorPosition")]
  pub conditional_expression_operator_position: OperatorPosition,
  #[serde(rename = "conditionalType.operatorPosition")]
  pub conditional_type_operator_position: OperatorPosition,
  /* single body position */
  #[serde(rename = "ifStatement.singleBodyPosition")]
  pub if_statement_single_body_position: SameOrNextLinePosition,
  #[serde(rename = "forStatement.singleBodyPosition")]
  pub for_statement_single_body_position: SameOrNextLinePosition,
  #[serde(rename = "forInStatement.singleBodyPosition")]
  pub for_in_statement_single_body_position: SameOrNextLinePosition,
  #[serde(rename = "forOfStatement.singleBodyPosition")]
  pub for_of_statement_single_body_position: SameOrNextLinePosition,
  #[serde(rename = "whileStatement.singleBodyPosition")]
  pub while_statement_single_body_position: SameOrNextLinePosition,
  /* trailing commas */
  #[serde(rename = "arguments.trailingCommas")]
  pub arguments_trailing_commas: TrailingCommas,
  #[serde(rename = "parameters.trailingCommas")]
  pub parameters_trailing_commas: TrailingCommas,
  #[serde(rename = "arrayExpression.trailingCommas")]
  pub array_expression_trailing_commas: TrailingCommas,
  #[serde(rename = "arrayPattern.trailingCommas")]
  pub array_pattern_trailing_commas: TrailingCommas,
  #[serde(rename = "enumDeclaration.trailingCommas")]
  pub enum_declaration_trailing_commas: TrailingCommas,
  #[serde(rename = "exportDeclaration.trailingCommas")]
  pub export_declaration_trailing_commas: TrailingCommas,
  #[serde(rename = "importDeclaration.trailingCommas")]
  pub import_declaration_trailing_commas: TrailingCommas,
  #[serde(rename = "objectPattern.trailingCommas")]
  pub object_pattern_trailing_commas: TrailingCommas,
  #[serde(rename = "objectExpression.trailingCommas")]
  pub object_expression_trailing_commas: TrailingCommas,
  #[serde(rename = "tupleType.trailingCommas")]
  pub tuple_type_trailing_commas: TrailingCommas,
  #[serde(rename = "typeLiteral.trailingCommas")]
  pub type_literal_trailing_commas: TrailingCommas,
  #[serde(rename = "typeParameters.trailingCommas")]
  pub type_parameters_trailing_commas: TrailingCommas,
  /* use braces */
  #[serde(rename = "ifStatement.useBraces")]
  pub if_statement_use_braces: UseBraces,
  #[serde(rename = "forStatement.useBraces")]
  pub for_statement_use_braces: UseBraces,
  #[serde(rename = "forOfStatement.useBraces")]
  pub for_of_statement_use_braces: UseBraces,
  #[serde(rename = "forInStatement.useBraces")]
  pub for_in_statement_use_braces: UseBraces,
  #[serde(rename = "whileStatement.useBraces")]
  pub while_statement_use_braces: UseBraces,
  /* prefer single line */
  #[serde(rename = "arrayExpression.preferSingleLine")]
  pub array_expression_prefer_single_line: bool,
  #[serde(rename = "arrayPattern.preferSingleLine")]
  pub array_pattern_prefer_single_line: bool,
  #[serde(rename = "arguments.preferSingleLine")]
  pub arguments_prefer_single_line: bool,
  #[serde(rename = "binaryExpression.preferSingleLine")]
  pub binary_expression_prefer_single_line: bool,
  #[serde(rename = "computed.preferSingleLine")]
  pub computed_prefer_single_line: bool,
  #[serde(rename = "conditionalExpression.preferSingleLine")]
  pub conditional_expression_prefer_single_line: bool,
  #[serde(rename = "conditionalType.preferSingleLine")]
  pub conditional_type_prefer_single_line: bool,
  #[serde(rename = "decorators.preferSingleLine")]
  pub decorators_prefer_single_line: bool,
  #[serde(rename = "exportDeclaration.preferSingleLine")]
  pub export_declaration_prefer_single_line: bool,
  #[serde(rename = "forStatement.preferSingleLine")]
  pub for_statement_prefer_single_line: bool,
  #[serde(rename = "importDeclaration.preferSingleLine")]
  pub import_declaration_prefer_single_line: bool,
  #[serde(rename = "jsxAttributes.preferSingleLine")]
  pub jsx_attributes_prefer_single_line: bool,
  #[serde(rename = "jsxElement.preferSingleLine")]
  pub jsx_element_prefer_single_line: bool,
  #[serde(rename = "mappedType.preferSingleLine")]
  pub mapped_type_prefer_single_line: bool,
  #[serde(rename = "memberExpression.preferSingleLine")]
  pub member_expression_prefer_single_line: bool,
  #[serde(rename = "objectExpression.preferSingleLine")]
  pub object_expression_prefer_single_line: bool,
  #[serde(rename = "objectPattern.preferSingleLine")]
  pub object_pattern_prefer_single_line: bool,
  #[serde(rename = "parameters.preferSingleLine")]
  pub parameters_prefer_single_line: bool,
  #[serde(rename = "parentheses.preferSingleLine")]
  pub parentheses_prefer_single_line: bool,
  #[serde(rename = "tupleType.preferSingleLine")]
  pub tuple_type_prefer_single_line: bool,
  #[serde(rename = "typeLiteral.preferSingleLine")]
  pub type_literal_prefer_single_line: bool,
  #[serde(rename = "typeParameters.preferSingleLine")]
  pub type_parameters_prefer_single_line: bool,
  #[serde(rename = "unionAndIntersectionType.preferSingleLine")]
  pub union_and_intersection_type_prefer_single_line: bool,
  #[serde(rename = "variableStatement.preferSingleLine")]
  pub variable_statement_prefer_single_line: bool,
  /* force single line */
  #[serde(rename = "importDeclaration.forceSingleLine")]
  pub import_declaration_force_single_line: bool,
  #[serde(rename = "exportDeclaration.forceSingleLine")]
  pub export_declaration_force_single_line: bool,

  /* use space separator */
  #[serde(rename = "binaryExpression.spaceSurroundingBitwiseAndArithmeticOperator")]
  pub binary_expression_space_surrounding_bitwise_and_arithmetic_operator: bool,
  #[serde(rename = "commentLine.forceSpaceAfterSlashes")]
  pub comment_line_force_space_after_slashes: bool,
  #[serde(rename = "constructSignature.spaceAfterNewKeyword")]
  pub construct_signature_space_after_new_keyword: bool,
  #[serde(rename = "constructor.spaceBeforeParentheses")]
  pub constructor_space_before_parentheses: bool,
  #[serde(rename = "constructorType.spaceAfterNewKeyword")]
  pub constructor_type_space_after_new_keyword: bool,
  #[serde(rename = "doWhileStatement.spaceAfterWhileKeyword")]
  pub do_while_statement_space_after_while_keyword: bool,
  #[serde(rename = "exportDeclaration.spaceSurroundingNamedExports")]
  pub export_declaration_space_surrounding_named_exports: bool,
  #[serde(rename = "forStatement.spaceAfterForKeyword")]
  pub for_statement_space_after_for_keyword: bool,
  #[serde(rename = "forStatement.spaceAfterSemiColons")]
  pub for_statement_space_after_semi_colons: bool,
  #[serde(rename = "forInStatement.spaceAfterForKeyword")]
  pub for_in_statement_space_after_for_keyword: bool,
  #[serde(rename = "forOfStatement.spaceAfterForKeyword")]
  pub for_of_statement_space_after_for_keyword: bool,
  #[serde(rename = "functionDeclaration.spaceBeforeParentheses")]
  pub function_declaration_space_before_parentheses: bool,
  #[serde(rename = "functionExpression.spaceBeforeParentheses")]
  pub function_expression_space_before_parentheses: bool,
  #[serde(rename = "functionExpression.spaceAfterFunctionKeyword")]
  pub function_expression_space_after_function_keyword: bool,
  #[serde(rename = "getAccessor.spaceBeforeParentheses")]
  pub get_accessor_space_before_parentheses: bool,
  #[serde(rename = "ifStatement.spaceAfterIfKeyword")]
  pub if_statement_space_after_if_keyword: bool,
  #[serde(rename = "importDeclaration.spaceSurroundingNamedImports")]
  pub import_declaration_space_surrounding_named_imports: bool,
  #[serde(rename = "jsxExpressionContainer.spaceSurroundingExpression")]
  pub jsx_expression_container_space_surrounding_expression: bool,
  #[serde(rename = "jsxSelfClosingElement.spaceBeforeSlash")]
  pub jsx_self_closing_element_space_before_slash: bool,
  #[serde(rename = "method.spaceBeforeParentheses")]
  pub method_space_before_parentheses: bool,
  #[serde(rename = "objectExpression.spaceSurroundingProperties")]
  pub object_expression_space_surrounding_properties: bool,
  #[serde(rename = "objectPattern.spaceSurroundingProperties")]
  pub object_pattern_space_surrounding_properties: bool,
  #[serde(rename = "setAccessor.spaceBeforeParentheses")]
  pub set_accessor_space_before_parentheses: bool,
  #[serde(rename = "spaceSurroundingProperties")]
  pub space_surrounding_properties: bool,
  #[serde(rename = "taggedTemplate.spaceBeforeLiteral")]
  pub tagged_template_space_before_literal: bool,
  #[serde(rename = "typeAnnotation.spaceBeforeColon")]
  pub type_annotation_space_before_colon: bool,
  #[serde(rename = "typeAssertion.spaceBeforeExpression")]
  pub type_assertion_space_before_expression: bool,
  #[serde(rename = "typeLiteral.spaceSurroundingProperties")]
  pub type_literal_space_surrounding_properties: bool,
  #[serde(rename = "whileStatement.spaceAfterWhileKeyword")]
  pub while_statement_space_after_while_keyword: bool,
  #[serde(rename = "arguments.spaceAround")]
  pub arguments_space_around: bool,
  #[serde(rename = "arrayExpression.spaceAround")]
  pub array_expression_space_around: bool,
  #[serde(rename = "arrayPattern.spaceAround")]
  pub array_pattern_space_around: bool,
  #[serde(rename = "doWhileStatement.spaceAround")]
  pub do_while_statement_space_around: bool,
  #[serde(rename = "forInStatement.spaceAround")]
  pub for_in_statement_space_around: bool,
  #[serde(rename = "forOfStatement.spaceAround")]
  pub for_of_statement_space_around: bool,
  #[serde(rename = "forStatement.spaceAround")]
  pub for_statement_space_around: bool,
  #[serde(rename = "ifStatement.spaceAround")]
  pub if_statement_space_around: bool,
  #[serde(rename = "parameters.spaceAround")]
  pub parameters_space_around: bool,
  #[serde(rename = "switchStatement.spaceAround")]
  pub switch_statement_space_around: bool,
  #[serde(rename = "tupleType.spaceAround")]
  pub tuple_type_space_around: bool,
  #[serde(rename = "whileStatement.spaceAround")]
  pub while_statement_space_around: bool,
}
