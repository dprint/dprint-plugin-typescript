use super::resolve_config::resolve_config;
use super::types::*;
use dprint_core::configuration::*;

/// TypeScript formatting configuration builder.
///
/// # Example
///
/// ```
/// use dprint_plugin_typescript::configuration::*;
///
/// let config = ConfigurationBuilder::new()
///     .line_width(80)
///     .prefer_hanging(true)
///     .prefer_single_line(false)
///     .quote_style(QuoteStyle::PreferSingle)
///     .next_control_flow_position(NextControlFlowPosition::SameLine)
///     .build();
/// ```
#[derive(Default)]
pub struct ConfigurationBuilder {
  pub(super) config: ConfigKeyMap,
  global_config: Option<GlobalConfiguration>,
}

impl ConfigurationBuilder {
  /// Constructs a new configuration builder.
  pub fn new() -> ConfigurationBuilder {
    ConfigurationBuilder::default()
  }

  /// Gets the final configuration that can be used to format a file.
  pub fn build(&self) -> Configuration {
    if let Some(global_config) = &self.global_config {
      resolve_config(self.config.clone(), global_config).config
    } else {
      let global_config = resolve_global_config(ConfigKeyMap::new(), &Default::default()).config;
      resolve_config(self.config.clone(), &global_config).config
    }
  }

  /// Set the global configuration.
  pub fn global_config(&mut self, global_config: GlobalConfiguration) -> &mut Self {
    self.global_config = Some(global_config);
    self
  }

  /// Helper method to set the configuration to what's used for Deno.
  pub fn deno(&mut self) -> &mut Self {
    self
      .line_width(80)
      .indent_width(2)
      .next_control_flow_position(NextControlFlowPosition::SameLine)
      .binary_expression_operator_position(OperatorPosition::SameLine)
      .conditional_expression_operator_position(OperatorPosition::NextLine)
      .conditional_type_operator_position(OperatorPosition::NextLine)
      .brace_position(BracePosition::SameLine)
      .comment_line_force_space_after_slashes(false)
      .construct_signature_space_after_new_keyword(true)
      .constructor_type_space_after_new_keyword(true)
      .arrow_function_use_parentheses(UseParentheses::Force)
      .new_line_kind(NewLineKind::LineFeed)
      .function_expression_space_after_function_keyword(true)
      .tagged_template_space_before_literal(false)
      .conditional_expression_prefer_single_line(true)
      .quote_style(QuoteStyle::PreferDouble)
      .jsx_multi_line_parens(JsxMultiLineParens::Prefer)
      .ignore_node_comment_text("deno-fmt-ignore")
      .ignore_file_comment_text("deno-fmt-ignore-file")
      .module_sort_import_declarations(SortOrder::Maintain)
      .module_sort_export_declarations(SortOrder::Maintain)
  }

  /// The width of a line the printer will try to stay under. Note that the printer may exceed this width in certain cases.
  ///
  /// Default: `120`
  pub fn line_width(&mut self, value: u32) -> &mut Self {
    self.insert("lineWidth", (value as i32).into())
  }

  /// Whether to use tabs (true) or spaces (false).
  ///
  /// Default: `false`
  pub fn use_tabs(&mut self, value: bool) -> &mut Self {
    self.insert("useTabs", value.into())
  }

  /// The number of columns for an indent.
  ///
  /// Default: `4`
  pub fn indent_width(&mut self, value: u8) -> &mut Self {
    self.insert("indentWidth", (value as i32).into())
  }

  /// The kind of newline to use.
  ///
  /// Default: `NewLineKind::LineFeed`
  pub fn new_line_kind(&mut self, value: NewLineKind) -> &mut Self {
    self.insert("newLineKind", value.to_string().into())
  }

  /// The quote style to use.
  ///
  /// Default: `QuoteStyle::AlwaysDouble`
  pub fn quote_style(&mut self, value: QuoteStyle) -> &mut Self {
    self.insert("quoteStyle", value.to_string().into())
  }

  /// The JSX quote style to use for string literals in JSX attributes.
  ///
  /// Default: `JsxQuoteStyle::PreferDouble`
  pub fn jsx_quote_style(&mut self, value: JsxQuoteStyle) -> &mut Self {
    self.insert("jsx.quoteStyle", value.to_string().into())
  }

  /// Whether to surround a JSX element or fragment with parentheses
  /// when it's the top JSX node and it spans multiple lines.
  ///
  /// Default: `JsxMultiLineParens::Prefer`
  pub fn jsx_multi_line_parens(&mut self, value: JsxMultiLineParens) -> &mut Self {
    self.insert("jsx.multiLineParens", value.to_string().into())
  }

  /// Forces newlines surrounding the content of JSX elements.
  ///
  /// Default: `false`
  pub fn jsx_force_new_lines_surrounding_content(&mut self, value: bool) -> &mut Self {
    self.insert("jsx.forceNewLinesSurroundingContent", value.into())
  }

  /// If the end angle bracket of a jsx opening element or self closing element
  /// should be on the same or next line when the attributes span multiple lines.
  ///
  /// Default: `nextLine`
  pub fn jsx_bracket_position(&mut self, value: SameOrNextLinePosition) -> &mut Self {
    self.insert("jsx.bracketPosition", value.to_string().into())
  }

  /// If the end angle bracket of a jsx opening element should be on the same
  /// or next line when the attributes span multiple lines.
  ///
  /// Default: `nextLine`
  pub fn jsx_opening_element_bracket_position(&mut self, value: SameOrNextLinePosition) -> &mut Self {
    self.insert("jsxOpeningElement.bracketPosition", value.to_string().into())
  }

  /// If the end angle bracket of a jsx self closing element should be on the same
  /// or next line when the attributes span multiple lines.
  ///
  /// Default: `nextLine`
  pub fn jsx_self_closing_element_bracket_position(&mut self, value: SameOrNextLinePosition) -> &mut Self {
    self.insert("jsxSelfClosingElement.bracketPosition", value.to_string().into())
  }

  /// Whether statements should end in a semi-colon.
  ///
  /// Default: `SemiColons::Prefer`
  pub fn semi_colons(&mut self, value: SemiColons) -> &mut Self {
    self.insert("semiColons", value.to_string().into())
  }

  /// Set to prefer hanging indentation when exceeding the line width.
  ///
  /// Default: `false`
  pub fn prefer_hanging(&mut self, value: bool) -> &mut Self {
    self.insert("preferHanging", value.into())
  }

  /// Behaviour to use for quotes on property names.
  ///
  /// Default: `preserve`
  pub fn quote_props(&mut self, value: QuoteProps) -> &mut Self {
    self.insert("quoteProps", value.to_string().into())
  }

  /// Where to place the opening brace.
  ///
  /// Default: `BracePosition::SameLineUnlessHanging`
  pub fn brace_position(&mut self, value: BracePosition) -> &mut Self {
    self.insert("bracePosition", value.to_string().into())
  }

  /// Where to place the next control flow within a control flow statement.
  ///
  /// Default: `NextControlFlowPosition::NextLine`
  pub fn next_control_flow_position(&mut self, value: NextControlFlowPosition) -> &mut Self {
    self.insert("nextControlFlowPosition", value.to_string().into())
  }

  /// Where to place the operator for expressions that span multiple lines.
  ///
  /// Default: `OperatorPosition::NextLine`
  pub fn operator_position(&mut self, value: OperatorPosition) -> &mut Self {
    self.insert("operatorPosition", value.to_string().into())
  }

  /// Where to place the expression of a statement that could possibly be on one line (ex. `if (true) console.log(5);`).
  ///
  /// Default: SingleBodyPosition::Maintain
  pub fn single_body_position(&mut self, value: SameOrNextLinePosition) -> &mut Self {
    self.insert("singleBodyPosition", value.to_string().into())
  }

  /// If trailing commas should be used.
  ///
  /// Default: `TrailingCommas::OnlyMultiLine`
  pub fn trailing_commas(&mut self, value: TrailingCommas) -> &mut Self {
    self.insert("trailingCommas", value.to_string().into())
  }

  /// If braces should be used or not.
  ///
  /// Default: `UseBraces::WhenNotSingleLine`
  pub fn use_braces(&mut self, value: UseBraces) -> &mut Self {
    self.insert("useBraces", value.to_string().into())
  }

  /// If code should revert back from being on multiple lines to
  /// being on a single line when able.
  ///
  /// Default: `false`
  pub fn prefer_single_line(&mut self, value: bool) -> &mut Self {
    self.insert("preferSingleLine", value.into())
  }

  /* space settings */

  /// Whether to surround bitwise and arithmetic operators in a binary expression with spaces.
  ///
  /// * `true` (default) - Ex. `1 + 2`
  /// * `false` - Ex. `1+2`
  pub fn binary_expression_space_surrounding_bitwise_and_arithmetic_operator(&mut self, value: bool) -> &mut Self {
    self.insert("binaryExpression.spaceSurroundingBitwiseAndArithmeticOperator", value.into())
  }

  /// Forces a space after the double slash in a comment line.
  ///
  /// `true` (default) - Ex. `//test` -> `// test`
  /// `false` - Ex. `//test` -> `//test`
  pub fn comment_line_force_space_after_slashes(&mut self, value: bool) -> &mut Self {
    self.insert("commentLine.forceSpaceAfterSlashes", value.into())
  }

  /// Whether to add a space after the `new` keyword in a construct signature.
  ///
  /// `true` - Ex. `new (): MyClass;`
  /// `false` (default) - Ex. `new(): MyClass;`
  pub fn construct_signature_space_after_new_keyword(&mut self, value: bool) -> &mut Self {
    self.insert("constructSignature.spaceAfterNewKeyword", value.into())
  }

  /// Whether to add a space before the parentheses of a constructor.
  ///
  /// `true` - Ex. `constructor ()`
  /// `false` (false) - Ex. `constructor()`
  pub fn constructor_space_before_parentheses(&mut self, value: bool) -> &mut Self {
    self.insert("constructor.spaceBeforeParentheses", value.into())
  }

  /// Whether to add a space after the `new` keyword in a constructor type.
  ///
  /// `true` - Ex. `type MyClassCtor = new () => MyClass;`
  /// `false` (default) - Ex. `type MyClassCtor = new() => MyClass;`
  pub fn constructor_type_space_after_new_keyword(&mut self, value: bool) -> &mut Self {
    self.insert("constructorType.spaceAfterNewKeyword", value.into())
  }

  /// Whether to add a space after the `while` keyword in a do while statement.
  ///
  /// `true` (true) - Ex. `do {\n} while (condition);`
  /// `false` - Ex. `do {\n} while(condition);`
  pub fn do_while_statement_space_after_while_keyword(&mut self, value: bool) -> &mut Self {
    self.insert("doWhileStatement.spaceAfterWhileKeyword", value.into())
  }

  /// Whether to add spaces around named exports in an export declaration.
  ///
  /// * `true` (default) - Ex. `export { SomeExport, OtherExport };`
  /// * `false` - Ex. `export {SomeExport, OtherExport};`
  pub fn export_declaration_space_surrounding_named_exports(&mut self, value: bool) -> &mut Self {
    self.insert("exportDeclaration.spaceSurroundingNamedExports", value.into())
  }

  /// Whether to add a space after the `for` keyword in a "for" statement.
  ///
  /// * `true` (default) - Ex. `for (let i = 0; i < 5; i++)`
  /// * `false` - Ex. `for(let i = 0; i < 5; i++)`
  pub fn for_statement_space_after_for_keyword(&mut self, value: bool) -> &mut Self {
    self.insert("forStatement.spaceAfterForKeyword", value.into())
  }

  /// Whether to add a space after the semi-colons in a "for" statement.
  ///
  /// * `true` (default) - Ex. `for (let i = 0; i < 5; i++)`
  /// * `false` - Ex. `for (let i = 0;i < 5;i++)`
  pub fn for_statement_space_after_semi_colons(&mut self, value: bool) -> &mut Self {
    self.insert("forStatement.spaceAfterSemiColons", value.into())
  }

  /// Whether to add a space after the `for` keyword in a "for in" statement.
  ///
  /// * `true` (default) - Ex. `for (const prop in obj)`
  /// * `false` - Ex. `for(const prop in obj)`
  pub fn for_in_statement_space_after_for_keyword(&mut self, value: bool) -> &mut Self {
    self.insert("forInStatement.spaceAfterForKeyword", value.into())
  }

  /// Whether to add a space after the `for` keyword in a "for of" statement.
  ///
  /// * `true` (default) - Ex. `for (const value of myArray)`
  /// * `false` - Ex. `for(const value of myArray)`
  pub fn for_of_statement_space_after_for_keyword(&mut self, value: bool) -> &mut Self {
    self.insert("forOfStatement.spaceAfterForKeyword", value.into())
  }

  /// Whether to add a space before the parentheses of a function declaration.
  ///
  /// * `true` - Ex. `function myFunction ()`
  /// * `false` (default) - Ex. `function myFunction()`
  pub fn function_declaration_space_before_parentheses(&mut self, value: bool) -> &mut Self {
    self.insert("functionDeclaration.spaceBeforeParentheses", value.into())
  }

  /// Whether to add a space before the parentheses of a function expression.
  ///
  /// `true` - Ex. `function<T> ()`
  /// `false` (default) - Ex. `function<T> ()`
  pub fn function_expression_space_before_parentheses(&mut self, value: bool) -> &mut Self {
    self.insert("functionExpression.spaceBeforeParentheses", value.into())
  }

  /// Whether to add a space after the function keyword of a function expression.
  ///
  /// `true` - Ex. `function <T>()`.
  /// `false` (default) - Ex. `function<T>()`
  pub fn function_expression_space_after_function_keyword(&mut self, value: bool) -> &mut Self {
    self.insert("functionExpression.spaceAfterFunctionKeyword", value.into())
  }

  /// Whether to add a space before the parentheses of a get accessor.
  ///
  /// `true` - Ex. `get myProp ()`
  /// `false` (false) - Ex. `get myProp()`
  pub fn get_accessor_space_before_parentheses(&mut self, value: bool) -> &mut Self {
    self.insert("getAccessor.spaceBeforeParentheses", value.into())
  }

  /// Whether to add a space after the `if` keyword in an "if" statement.
  ///
  /// `true` (default) - Ex. `if (true)`
  /// `false` - Ex. `if(true)`
  pub fn if_statement_space_after_if_keyword(&mut self, value: bool) -> &mut Self {
    self.insert("ifStatement.spaceAfterIfKeyword", value.into())
  }

  /// Whether to add spaces around named imports in an import declaration.
  ///
  /// * `true` (default) - Ex. `import { SomeExport, OtherExport } from "my-module";`
  /// * `false` - Ex. `import {SomeExport, OtherExport} from "my-module";`
  pub fn import_declaration_space_surrounding_named_imports(&mut self, value: bool) -> &mut Self {
    self.insert("importDeclaration.spaceSurroundingNamedImports", value.into())
  }

  /// Whether to add a space surrounding the expression of a JSX container.
  ///
  /// * `true` - Ex. `{ myValue }`
  /// * `false` (default) - Ex. `{myValue}`
  pub fn jsx_expression_container_space_surrounding_expression(&mut self, value: bool) -> &mut Self {
    self.insert("jsxExpressionContainer.spaceSurroundingExpression", value.into())
  }

  /// Whether to add a space before the slash in a self closing tag for a JSX element.
  ///
  /// * `true` (default) - Ex. `<Test />`
  /// * `false` - Ex. `<Test/>`
  pub fn jsx_self_closing_element_space_before_slash(&mut self, value: bool) -> &mut Self {
    self.insert("jsxSelfClosingElement.spaceBeforeSlash", value.into())
  }

  /// Whether to add a space surrounding the properties of an object expression.
  ///
  /// * `true` (default) - Ex. `{ key: value }`
  /// * `false` - Ex. `{key: value}`
  pub fn object_expression_space_surrounding_properties(&mut self, value: bool) -> &mut Self {
    self.insert("objectExpression.spaceSurroundingProperties", value.into())
  }

  /// Whether to add a space surrounding the properties of an object pattern.
  ///
  /// * `true` (default) - Ex. `{ key: value } = obj`
  /// * `false` - Ex. `{key: value} = obj`
  pub fn object_pattern_space_surrounding_properties(&mut self, value: bool) -> &mut Self {
    self.insert("objectPattern.spaceSurroundingProperties", value.into())
  }

  /// Whether to add a space before the parentheses of a method.
  ///
  /// `true` - Ex. `myMethod ()`
  /// `false` - Ex. `myMethod()`
  pub fn method_space_before_parentheses(&mut self, value: bool) -> &mut Self {
    self.insert("method.spaceBeforeParentheses", value.into())
  }

  /// Whether to add a space before the parentheses of a set accessor.
  ///
  /// `true` - Ex. `set myProp (value: string)`
  /// `false` (default) - Ex. `set myProp(value: string)`
  pub fn set_accessor_space_before_parentheses(&mut self, value: bool) -> &mut Self {
    self.insert("setAccessor.spaceBeforeParentheses", value.into())
  }

  /// Whether to add a space surrounding the properties of object-like nodes.
  ///
  /// * `true` (default) - Ex. `{ key: value }`
  /// * `false` - Ex. `{key: value}`
  pub fn space_surrounding_properties(&mut self, value: bool) -> &mut Self {
    self.insert("spaceSurroundingProperties", value.into())
  }

  /// Whether to add a space before the literal in a tagged template.
  ///
  /// * `true` (default) - Ex. `html \`<element />\``
  /// * `false` - Ex. `html\`<element />\``
  pub fn tagged_template_space_before_literal(&mut self, value: bool) -> &mut Self {
    self.insert("taggedTemplate.spaceBeforeLiteral", value.into())
  }

  /// Whether to add a space before the colon of a type annotation.
  ///
  /// * `true` - Ex. `function myFunction() : string`
  /// * `false` (default) - Ex. `function myFunction(): string`
  pub fn type_annotation_space_before_colon(&mut self, value: bool) -> &mut Self {
    self.insert("typeAnnotation.spaceBeforeColon", value.into())
  }

  /// Whether to add a space before the expression in a type assertion.
  ///
  /// * `true` (default) - Ex. `<string> myValue`
  /// * `false` - Ex. `<string>myValue`
  pub fn type_assertion_space_before_expression(&mut self, value: bool) -> &mut Self {
    self.insert("typeAssertion.spaceBeforeExpression", value.into())
  }

  /// Whether to add a space surrounding the properties of a type literal.
  ///
  /// * `true` (default) - Ex. `value: { key: Type }`
  /// * `false` - Ex. `value: {key: Type}`
  pub fn type_literal_space_surrounding_properties(&mut self, value: bool) -> &mut Self {
    self.insert("typeLiteral.spaceSurroundingProperties", value.into())
  }

  /// Whether to add a space after the `while` keyword in a while statement.
  ///
  /// * `true` (default) - Ex. `while (true)`
  /// * `false` - Ex. `while(true)`
  pub fn while_statement_space_after_while_keyword(&mut self, value: bool) -> &mut Self {
    self.insert("whileStatement.spaceAfterWhileKeyword", value.into())
  }

  /// Whether to place spaces around enclosed expressions.
  ///
  /// * `true` - Ex. `myFunction( true )`
  /// * `false` (default) - Ex. `myFunction(true)`
  pub fn space_around(&mut self, value: bool) -> &mut Self {
    self.insert("spaceAround", value.into())
  }

  /* situational */

  /// Whether to use parentheses for arrow functions.
  ///
  /// Default: `UseParentheses::Maintain`
  pub fn arrow_function_use_parentheses(&mut self, value: UseParentheses) -> &mut Self {
    self.insert("arrowFunction.useParentheses", value.to_string().into())
  }

  /// Whether to force a line per expression when spanning multiple lines.
  ///
  /// * `true` - Formats with each part on a new line.
  /// * `false` (default) - Maintains the line breaks as written by the programmer.
  pub fn binary_expression_line_per_expression(&mut self, value: bool) -> &mut Self {
    self.insert("binaryExpression.linePerExpression", value.into())
  }

  /// Whether to force a line per expression when spanning multiple lines.
  ///
  /// * `true` - Formats with each part on a new line.
  /// * `false` (default) - Maintains the line breaks as written by the programmer.
  pub fn conditional_expression_line_per_expression(&mut self, value: bool) -> &mut Self {
    self.insert("conditionalExpression.linePerExpression", value.into())
  }

  /// Whether to force a line per expression when spanning multiple lines.
  ///
  /// * `true` - Formats with each part on a new line.
  /// * `false` (default) - Maintains the line breaks as written by the programmer.
  pub fn member_expression_line_per_expression(&mut self, value: bool) -> &mut Self {
    self.insert("memberExpression.linePerExpression", value.into())
  }

  /// The kind of separator to use in type literals.
  pub fn type_literal_separator_kind(&mut self, value: SemiColonOrComma) -> &mut Self {
    self.insert("typeLiteral.separatorKind", value.to_string().into())
  }

  /// The kind of separator to use in type literals when single line.
  pub fn type_literal_separator_kind_single_line(&mut self, value: SemiColonOrComma) -> &mut Self {
    self.insert("typeLiteral.separatorKind.singleLine", value.to_string().into())
  }

  /// The kind of separator to use in type literals when multi-line.
  pub fn type_literal_separator_kind_multi_line(&mut self, value: SemiColonOrCommaOrNewLine) -> &mut Self {
    self.insert("typeLiteral.separatorKind.multiLine", value.to_string().into())
  }

  /* sorting */

  /// Alphabetically sorts the import declarations based on their module specifiers.
  ///
  /// Default: Case insensitive
  pub fn module_sort_import_declarations(&mut self, value: SortOrder) -> &mut Self {
    self.insert("module.sortImportDeclarations", value.to_string().into())
  }

  /// Alphabetically sorts the export declarations based on their module specifiers.
  ///
  /// Default: Case insensitive
  pub fn module_sort_export_declarations(&mut self, value: SortOrder) -> &mut Self {
    self.insert("module.sortExportDeclarations", value.to_string().into())
  }

  /// Alphabetically sorts the import declaration's named imports.
  ///
  /// Default: Case insensitive
  pub fn import_declaration_sort_named_imports(&mut self, value: SortOrder) -> &mut Self {
    self.insert("importDeclaration.sortNamedImports", value.to_string().into())
  }

  /// Alphabetically sorts the export declaration's named exports.
  ///
  /// Default: Case insensitive
  pub fn export_declaration_sort_named_exports(&mut self, value: SortOrder) -> &mut Self {
    self.insert("exportDeclaration.sortNamedExports", value.to_string().into())
  }

  /* ignore comments */

  /// The text to use for an ignore comment (ex. `// dprint-ignore`).
  ///
  /// Default: `"dprint-ignore"`
  pub fn ignore_node_comment_text(&mut self, value: &str) -> &mut Self {
    self.insert("ignoreNodeCommentText", value.into())
  }

  /// The text to use for a file ignore comment (ex. `// dprint-ignore-file`).
  ///
  /// Default: `"dprint-ignore-file"`
  pub fn ignore_file_comment_text(&mut self, value: &str) -> &mut Self {
    self.insert("ignoreFileCommentText", value.into())
  }

  /* brace position */

  pub fn arrow_function_brace_position(&mut self, value: BracePosition) -> &mut Self {
    self.insert("arrowFunction.bracePosition", value.to_string().into())
  }

  pub fn class_declaration_brace_position(&mut self, value: BracePosition) -> &mut Self {
    self.insert("classDeclaration.bracePosition", value.to_string().into())
  }

  pub fn class_expression_brace_position(&mut self, value: BracePosition) -> &mut Self {
    self.insert("classExpression.bracePosition", value.to_string().into())
  }

  pub fn constructor_brace_position(&mut self, value: BracePosition) -> &mut Self {
    self.insert("constructor.bracePosition", value.to_string().into())
  }

  pub fn do_while_statement_brace_position(&mut self, value: BracePosition) -> &mut Self {
    self.insert("doWhileStatement.bracePosition", value.to_string().into())
  }

  pub fn enum_declaration_brace_position(&mut self, value: BracePosition) -> &mut Self {
    self.insert("enumDeclaration.bracePosition", value.to_string().into())
  }

  pub fn for_statement_brace_position(&mut self, value: BracePosition) -> &mut Self {
    self.insert("forStatement.bracePosition", value.to_string().into())
  }

  pub fn for_in_statement_brace_position(&mut self, value: BracePosition) -> &mut Self {
    self.insert("forInStatement.bracePosition", value.to_string().into())
  }

  pub fn for_of_statement_brace_position(&mut self, value: BracePosition) -> &mut Self {
    self.insert("forOfStatement.bracePosition", value.to_string().into())
  }

  pub fn get_accessor_brace_position(&mut self, value: BracePosition) -> &mut Self {
    self.insert("getAccessor.bracePosition", value.to_string().into())
  }

  pub fn if_statement_brace_position(&mut self, value: BracePosition) -> &mut Self {
    self.insert("ifStatement.bracePosition", value.to_string().into())
  }

  pub fn interface_declaration_brace_position(&mut self, value: BracePosition) -> &mut Self {
    self.insert("interfaceDeclaration.bracePosition", value.to_string().into())
  }

  pub fn function_declaration_brace_position(&mut self, value: BracePosition) -> &mut Self {
    self.insert("functionDeclaration.bracePosition", value.to_string().into())
  }

  pub fn function_expression_brace_position(&mut self, value: BracePosition) -> &mut Self {
    self.insert("functionExpression.bracePosition", value.to_string().into())
  }

  pub fn method_brace_position(&mut self, value: BracePosition) -> &mut Self {
    self.insert("method.bracePosition", value.to_string().into())
  }

  pub fn module_declaration_brace_position(&mut self, value: BracePosition) -> &mut Self {
    self.insert("moduleDeclaration.bracePosition", value.to_string().into())
  }

  pub fn set_accessor_brace_position(&mut self, value: BracePosition) -> &mut Self {
    self.insert("setAccessor.bracePosition", value.to_string().into())
  }

  pub fn static_block_brace_position(&mut self, value: BracePosition) -> &mut Self {
    self.insert("staticBlock.bracePosition", value.to_string().into())
  }

  pub fn switch_case_brace_position(&mut self, value: BracePosition) -> &mut Self {
    self.insert("switchCase.bracePosition", value.to_string().into())
  }

  pub fn switch_statement_brace_position(&mut self, value: BracePosition) -> &mut Self {
    self.insert("switchStatement.bracePosition", value.to_string().into())
  }

  pub fn try_statement_brace_position(&mut self, value: BracePosition) -> &mut Self {
    self.insert("tryStatement.bracePosition", value.to_string().into())
  }

  pub fn while_statement_brace_position(&mut self, value: BracePosition) -> &mut Self {
    self.insert("whileStatement.bracePosition", value.to_string().into())
  }

  /* prefer hanging */

  pub fn arguments_prefer_hanging(&mut self, value: bool) -> &mut Self {
    self.insert("arguments.preferHanging", value.into())
  }

  pub fn array_expression_prefer_hanging(&mut self, value: bool) -> &mut Self {
    self.insert("arrayExpression.preferHanging", value.into())
  }

  pub fn array_pattern_prefer_hanging(&mut self, value: bool) -> &mut Self {
    self.insert("arrayPattern.preferHanging", value.into())
  }

  pub fn do_while_statement_prefer_hanging(&mut self, value: bool) -> &mut Self {
    self.insert("doWhileStatement.preferHanging", value.into())
  }

  pub fn export_declaration_prefer_hanging(&mut self, value: bool) -> &mut Self {
    self.insert("exportDeclaration.preferHanging", value.into())
  }

  pub fn extends_clause_prefer_hanging(&mut self, value: bool) -> &mut Self {
    self.insert("extendsClause.preferHanging", value.into())
  }

  pub fn for_in_statement_prefer_hanging(&mut self, value: bool) -> &mut Self {
    self.insert("forInStatement.preferHanging", value.into())
  }

  pub fn for_of_statement_prefer_hanging(&mut self, value: bool) -> &mut Self {
    self.insert("forOfStatement.preferHanging", value.into())
  }

  pub fn for_statement_prefer_hanging(&mut self, value: bool) -> &mut Self {
    self.insert("forStatement.preferHanging", value.into())
  }

  pub fn if_statement_prefer_hanging(&mut self, value: bool) -> &mut Self {
    self.insert("ifStatement.preferHanging", value.into())
  }

  pub fn implements_clause_prefer_hanging(&mut self, value: bool) -> &mut Self {
    self.insert("implementsClause.preferHanging", value.into())
  }

  pub fn import_declaration_prefer_hanging(&mut self, value: bool) -> &mut Self {
    self.insert("importDeclaration.preferHanging", value.into())
  }

  pub fn jsx_attributes_prefer_hanging(&mut self, value: bool) -> &mut Self {
    self.insert("jsxAttributes.preferHanging", value.into())
  }

  pub fn object_expression_prefer_hanging(&mut self, value: bool) -> &mut Self {
    self.insert("objectExpression.preferHanging", value.into())
  }

  pub fn object_pattern_prefer_hanging(&mut self, value: bool) -> &mut Self {
    self.insert("objectPattern.preferHanging", value.into())
  }

  pub fn parameters_prefer_hanging(&mut self, value: bool) -> &mut Self {
    self.insert("parameters.preferHanging", value.into())
  }

  pub fn sequence_expression_prefer_hanging(&mut self, value: bool) -> &mut Self {
    self.insert("sequenceExpression.preferHanging", value.into())
  }

  pub fn switch_statement_prefer_hanging(&mut self, value: bool) -> &mut Self {
    self.insert("switchStatement.preferHanging", value.into())
  }

  pub fn tuple_type_prefer_hanging(&mut self, value: bool) -> &mut Self {
    self.insert("tupleType.preferHanging", value.into())
  }

  pub fn type_literal_prefer_hanging(&mut self, value: bool) -> &mut Self {
    self.insert("typeLiteral.preferHanging", value.into())
  }

  pub fn type_parameters_prefer_hanging(&mut self, value: bool) -> &mut Self {
    self.insert("typeParameters.preferHanging", value.into())
  }

  pub fn union_and_intersection_type_prefer_hanging(&mut self, value: bool) -> &mut Self {
    self.insert("unionAndIntersectionType.preferHanging", value.into())
  }

  pub fn variable_statement_prefer_hanging(&mut self, value: bool) -> &mut Self {
    self.insert("variableStatement.preferHanging", value.into())
  }

  pub fn while_statement_prefer_hanging(&mut self, value: bool) -> &mut Self {
    self.insert("whileStatement.preferHanging", value.into())
  }

  /* force single line */

  pub fn export_declaration_force_single_line(&mut self, value: bool) -> &mut Self {
    self.insert("exportDeclaration.forceSingleLine", value.into())
  }

  pub fn import_declaration_force_single_line(&mut self, value: bool) -> &mut Self {
    self.insert("importDeclaration.forceSingleLine", value.into())
  }

  /* member spacing */

  pub fn enum_declaration_member_spacing(&mut self, value: MemberSpacing) -> &mut Self {
    self.insert("enumDeclaration.memberSpacing", value.to_string().into())
  }

  /* next control flow position */

  pub fn if_statement_next_control_flow_position(&mut self, value: NextControlFlowPosition) -> &mut Self {
    self.insert("ifStatement.nextControlFlowPosition", value.to_string().into())
  }

  pub fn try_statement_next_control_flow_position(&mut self, value: NextControlFlowPosition) -> &mut Self {
    self.insert("tryStatement.nextControlFlowPosition", value.to_string().into())
  }

  pub fn do_while_statement_next_control_flow_position(&mut self, value: NextControlFlowPosition) -> &mut Self {
    self.insert("doWhileStatement.nextControlFlowPosition", value.to_string().into())
  }

  /* operator position */

  pub fn binary_expression_operator_position(&mut self, value: OperatorPosition) -> &mut Self {
    self.insert("binaryExpression.operatorPosition", value.to_string().into())
  }

  pub fn conditional_expression_operator_position(&mut self, value: OperatorPosition) -> &mut Self {
    self.insert("conditionalExpression.operatorPosition", value.to_string().into())
  }

  pub fn conditional_type_operator_position(&mut self, value: OperatorPosition) -> &mut Self {
    self.insert("conditionalType.operatorPosition", value.to_string().into())
  }

  /* single body position */

  pub fn if_statement_single_body_position(&mut self, value: SameOrNextLinePosition) -> &mut Self {
    self.insert("ifStatement.singleBodyPosition", value.to_string().into())
  }

  pub fn for_statement_single_body_position(&mut self, value: SameOrNextLinePosition) -> &mut Self {
    self.insert("forStatement.singleBodyPosition", value.to_string().into())
  }

  pub fn for_in_statement_single_body_position(&mut self, value: SameOrNextLinePosition) -> &mut Self {
    self.insert("forInStatement.singleBodyPosition", value.to_string().into())
  }

  pub fn for_of_statement_single_body_position(&mut self, value: SameOrNextLinePosition) -> &mut Self {
    self.insert("forOfStatement.singleBodyPosition", value.to_string().into())
  }

  pub fn while_statement_single_body_position(&mut self, value: SameOrNextLinePosition) -> &mut Self {
    self.insert("whileStatement.singleBodyPosition", value.to_string().into())
  }

  /* trailing commas */

  pub fn arguments_trailing_commas(&mut self, value: TrailingCommas) -> &mut Self {
    self.insert("arguments.trailingCommas", value.to_string().into())
  }

  pub fn parameters_trailing_commas(&mut self, value: TrailingCommas) -> &mut Self {
    self.insert("parameters.trailingCommas", value.to_string().into())
  }

  pub fn array_expression_trailing_commas(&mut self, value: TrailingCommas) -> &mut Self {
    self.insert("arrayExpression.trailingCommas", value.to_string().into())
  }

  pub fn array_pattern_trailing_commas(&mut self, value: TrailingCommas) -> &mut Self {
    self.insert("arrayPattern.trailingCommas", value.to_string().into())
  }

  pub fn enum_declaration_trailing_commas(&mut self, value: TrailingCommas) -> &mut Self {
    self.insert("enumDeclaration.trailingCommas", value.to_string().into())
  }

  pub fn export_declaration_trailing_commas(&mut self, value: TrailingCommas) -> &mut Self {
    self.insert("exportDeclaration.trailingCommas", value.to_string().into())
  }

  pub fn import_declaration_trailing_commas(&mut self, value: TrailingCommas) -> &mut Self {
    self.insert("importDeclaration.trailingCommas", value.to_string().into())
  }

  pub fn object_expression_trailing_commas(&mut self, value: TrailingCommas) -> &mut Self {
    self.insert("objectExpression.trailingCommas", value.to_string().into())
  }

  pub fn object_pattern_trailing_commas(&mut self, value: TrailingCommas) -> &mut Self {
    self.insert("objectPattern.trailingCommas", value.to_string().into())
  }

  pub fn tuple_type_trailing_commas(&mut self, value: TrailingCommas) -> &mut Self {
    self.insert("tupleType.trailingCommas", value.to_string().into())
  }

  /// Only applies when using commas on type literals.
  pub fn type_literal_trailing_commas(&mut self, value: TrailingCommas) -> &mut Self {
    self.insert("typeLiteral.trailingCommas", value.to_string().into())
  }

  pub fn type_parameters_trailing_commas(&mut self, value: TrailingCommas) -> &mut Self {
    self.insert("typeParameters.trailingCommas", value.to_string().into())
  }

  /* use braces */

  pub fn if_statement_use_braces(&mut self, value: UseBraces) -> &mut Self {
    self.insert("ifStatement.useBraces", value.to_string().into())
  }

  pub fn for_statement_use_braces(&mut self, value: UseBraces) -> &mut Self {
    self.insert("forStatement.useBraces", value.to_string().into())
  }

  pub fn for_in_statement_use_braces(&mut self, value: UseBraces) -> &mut Self {
    self.insert("forInStatement.useBraces", value.to_string().into())
  }

  pub fn for_of_statement_use_braces(&mut self, value: UseBraces) -> &mut Self {
    self.insert("forOfStatement.useBraces", value.to_string().into())
  }

  pub fn while_statement_use_braces(&mut self, value: UseBraces) -> &mut Self {
    self.insert("whileStatement.useBraces", value.to_string().into())
  }

  /* prefer single line */

  pub fn array_expression_prefer_single_line(&mut self, value: bool) -> &mut Self {
    self.insert("arrayExpression.preferSingleLine", value.into())
  }

  pub fn array_pattern_prefer_single_line(&mut self, value: bool) -> &mut Self {
    self.insert("arrayPattern.preferSingleLine", value.into())
  }

  pub fn arguments_prefer_single_line(&mut self, value: bool) -> &mut Self {
    self.insert("arguments.preferSingleLine", value.into())
  }

  pub fn binary_expression_prefer_single_line(&mut self, value: bool) -> &mut Self {
    self.insert("binaryExpression.preferSingleLine", value.into())
  }

  pub fn computed_prefer_single_line(&mut self, value: bool) -> &mut Self {
    self.insert("computed.preferSingleLine", value.into())
  }

  pub fn conditional_expression_prefer_single_line(&mut self, value: bool) -> &mut Self {
    self.insert("conditionalExpression.preferSingleLine", value.into())
  }

  pub fn conditional_type_prefer_single_line(&mut self, value: bool) -> &mut Self {
    self.insert("conditionalType.preferSingleLine", value.into())
  }

  pub fn decorators_prefer_single_line(&mut self, value: bool) -> &mut Self {
    self.insert("decorators.preferSingleLine", value.into())
  }

  pub fn export_declaration_prefer_single_line(&mut self, value: bool) -> &mut Self {
    self.insert("exportDeclaration.preferSingleLine", value.into())
  }

  pub fn for_statement_prefer_single_line(&mut self, value: bool) -> &mut Self {
    self.insert("forStatement.preferSingleLine", value.into())
  }

  pub fn import_declaration_prefer_single_line(&mut self, value: bool) -> &mut Self {
    self.insert("importDeclaration.preferSingleLine", value.into())
  }

  pub fn jsx_attributes_prefer_single_line(&mut self, value: bool) -> &mut Self {
    self.insert("jsxAttributes.preferSingleLine", value.into())
  }

  pub fn jsx_element_prefer_single_line(&mut self, value: bool) -> &mut Self {
    self.insert("jsxElement.preferSingleLine", value.into())
  }

  pub fn mapped_type_prefer_single_line(&mut self, value: bool) -> &mut Self {
    self.insert("mappedType.preferSingleLine", value.into())
  }

  pub fn member_expression_prefer_single_line(&mut self, value: bool) -> &mut Self {
    self.insert("memberExpression.preferSingleLine", value.into())
  }

  pub fn object_expression_prefer_single_line(&mut self, value: bool) -> &mut Self {
    self.insert("objectExpression.preferSingleLine", value.into())
  }

  pub fn object_pattern_prefer_single_line(&mut self, value: bool) -> &mut Self {
    self.insert("objectPattern.preferSingleLine", value.into())
  }

  pub fn parameters_prefer_single_line(&mut self, value: bool) -> &mut Self {
    self.insert("parameters.preferSingleLine", value.into())
  }

  pub fn parentheses_prefer_single_line(&mut self, value: bool) -> &mut Self {
    self.insert("parentheses.preferSingleLine", value.into())
  }

  pub fn tuple_type_prefer_single_line(&mut self, value: bool) -> &mut Self {
    self.insert("tupleType.preferSingleLine", value.into())
  }

  pub fn type_literal_prefer_single_line(&mut self, value: bool) -> &mut Self {
    self.insert("typeLiteral.preferSingleLine", value.into())
  }

  pub fn type_parameters_prefer_single_line(&mut self, value: bool) -> &mut Self {
    self.insert("typeParameters.preferSingleLine", value.into())
  }

  pub fn union_and_intersection_type_prefer_single_line(&mut self, value: bool) -> &mut Self {
    self.insert("unionAndIntersectionType.preferSingleLine", value.into())
  }

  pub fn variable_statement_prefer_single_line(&mut self, value: bool) -> &mut Self {
    self.insert("variableStatement.preferSingleLine", value.into())
  }

  /* space around */

  pub fn arguments_space_around(&mut self, value: bool) -> &mut Self {
    self.insert("arguments.spaceAround", value.into())
  }

  pub fn array_expression_space_around(&mut self, value: bool) -> &mut Self {
    self.insert("arrayExpression.spaceAround", value.into())
  }

  pub fn array_pattern_space_around(&mut self, value: bool) -> &mut Self {
    self.insert("arrayPattern.spaceAround", value.into())
  }

  pub fn do_while_statement_space_around(&mut self, value: bool) -> &mut Self {
    self.insert("doWhileStatement.spaceAround", value.into())
  }

  pub fn for_in_statement_space_around(&mut self, value: bool) -> &mut Self {
    self.insert("forInStatement.spaceAround", value.into())
  }

  pub fn for_of_statement_space_around(&mut self, value: bool) -> &mut Self {
    self.insert("forOfStatement.spaceAround", value.into())
  }

  pub fn for_statement_space_around(&mut self, value: bool) -> &mut Self {
    self.insert("forStatement.spaceAround", value.into())
  }

  pub fn if_statement_space_around(&mut self, value: bool) -> &mut Self {
    self.insert("ifStatement.spaceAround", value.into())
  }

  pub fn parameters_space_around(&mut self, value: bool) -> &mut Self {
    self.insert("parameters.spaceAround", value.into())
  }

  pub fn switch_statement_space_around(&mut self, value: bool) -> &mut Self {
    self.insert("switchStatement.spaceAround", value.into())
  }

  pub fn tuple_type_space_around(&mut self, value: bool) -> &mut Self {
    self.insert("tupleType.spaceAround", value.into())
  }

  pub fn while_statement_space_around(&mut self, value: bool) -> &mut Self {
    self.insert("whileStatement.spaceAround", value.into())
  }

  #[cfg(test)]
  pub(super) fn get_inner_config(&self) -> ConfigKeyMap {
    self.config.clone()
  }

  fn insert(&mut self, name: &str, value: ConfigKeyValue) -> &mut Self {
    self.config.insert(String::from(name), value);
    self
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn check_all_values_set() {
    let mut config = ConfigurationBuilder::new();
    config
      .new_line_kind(NewLineKind::Auto)
      .line_width(80)
      .use_tabs(false)
      .indent_width(4)
      /* common */
      .quote_style(QuoteStyle::AlwaysDouble)
      .jsx_quote_style(JsxQuoteStyle::PreferSingle)
      .jsx_multi_line_parens(JsxMultiLineParens::Never)
      .jsx_force_new_lines_surrounding_content(true)
      .jsx_bracket_position(SameOrNextLinePosition::Maintain)
      .jsx_opening_element_bracket_position(SameOrNextLinePosition::Maintain)
      .jsx_self_closing_element_bracket_position(SameOrNextLinePosition::Maintain)
      .semi_colons(SemiColons::Prefer)
      .brace_position(BracePosition::NextLine)
      .next_control_flow_position(NextControlFlowPosition::SameLine)
      .operator_position(OperatorPosition::SameLine)
      .single_body_position(SameOrNextLinePosition::SameLine)
      .trailing_commas(TrailingCommas::Never)
      .use_braces(UseBraces::WhenNotSingleLine)
      .quote_props(QuoteProps::AsNeeded)
      .prefer_hanging(false)
      /* situational */
      .arrow_function_use_parentheses(UseParentheses::Maintain)
      .binary_expression_line_per_expression(false)
      .conditional_expression_line_per_expression(true)
      .member_expression_line_per_expression(false)
      .type_literal_separator_kind(SemiColonOrComma::Comma)
      .type_literal_separator_kind_single_line(SemiColonOrComma::Comma)
      .type_literal_separator_kind_multi_line(SemiColonOrCommaOrNewLine::Comma)
      /* sorting */
      .module_sort_import_declarations(SortOrder::Maintain)
      .module_sort_export_declarations(SortOrder::Maintain)
      .import_declaration_sort_named_imports(SortOrder::Maintain)
      .export_declaration_sort_named_exports(SortOrder::Maintain)
      /* ignore comments */
      .ignore_node_comment_text("ignore")
      .ignore_file_comment_text("ignore-file")
      /* brace position*/
      .arrow_function_brace_position(BracePosition::NextLine)
      .class_declaration_brace_position(BracePosition::NextLine)
      .class_expression_brace_position(BracePosition::NextLine)
      .constructor_brace_position(BracePosition::NextLine)
      .do_while_statement_brace_position(BracePosition::NextLine)
      .enum_declaration_brace_position(BracePosition::NextLine)
      .for_statement_brace_position(BracePosition::NextLine)
      .for_in_statement_brace_position(BracePosition::NextLine)
      .for_of_statement_brace_position(BracePosition::NextLine)
      .get_accessor_brace_position(BracePosition::NextLine)
      .if_statement_brace_position(BracePosition::NextLine)
      .interface_declaration_brace_position(BracePosition::NextLine)
      .function_declaration_brace_position(BracePosition::NextLine)
      .function_expression_brace_position(BracePosition::NextLine)
      .method_brace_position(BracePosition::NextLine)
      .module_declaration_brace_position(BracePosition::NextLine)
      .set_accessor_brace_position(BracePosition::NextLine)
      .static_block_brace_position(BracePosition::NextLine)
      .switch_case_brace_position(BracePosition::NextLine)
      .switch_statement_brace_position(BracePosition::NextLine)
      .try_statement_brace_position(BracePosition::NextLine)
      .while_statement_brace_position(BracePosition::NextLine)
      /* prefer hanging */
      .arguments_prefer_hanging(true)
      .array_expression_prefer_hanging(true)
      .array_pattern_prefer_hanging(true)
      .do_while_statement_prefer_hanging(true)
      .export_declaration_prefer_hanging(true)
      .extends_clause_prefer_hanging(true)
      .for_in_statement_prefer_hanging(true)
      .for_of_statement_prefer_hanging(true)
      .for_statement_prefer_hanging(true)
      .if_statement_prefer_hanging(true)
      .implements_clause_prefer_hanging(true)
      .import_declaration_prefer_hanging(true)
      .jsx_attributes_prefer_hanging(true)
      .object_expression_prefer_hanging(true)
      .object_pattern_prefer_hanging(true)
      .parameters_prefer_hanging(true)
      .sequence_expression_prefer_hanging(true)
      .switch_statement_prefer_hanging(true)
      .tuple_type_prefer_hanging(true)
      .type_literal_prefer_hanging(true)
      .type_parameters_prefer_hanging(true)
      .union_and_intersection_type_prefer_hanging(true)
      .variable_statement_prefer_hanging(true)
      .while_statement_prefer_hanging(true)
      /* member spacing */
      .enum_declaration_member_spacing(MemberSpacing::Maintain)
      /* next control flow position */
      .if_statement_next_control_flow_position(NextControlFlowPosition::SameLine)
      .try_statement_next_control_flow_position(NextControlFlowPosition::SameLine)
      .do_while_statement_next_control_flow_position(NextControlFlowPosition::SameLine)
      /* operator position */
      .binary_expression_operator_position(OperatorPosition::SameLine)
      .conditional_expression_operator_position(OperatorPosition::SameLine)
      .conditional_type_operator_position(OperatorPosition::SameLine)
      /* single body position */
      .if_statement_single_body_position(SameOrNextLinePosition::SameLine)
      .for_statement_single_body_position(SameOrNextLinePosition::SameLine)
      .for_in_statement_single_body_position(SameOrNextLinePosition::SameLine)
      .for_of_statement_single_body_position(SameOrNextLinePosition::SameLine)
      .while_statement_single_body_position(SameOrNextLinePosition::SameLine)
      /* trailing commas */
      .arguments_trailing_commas(TrailingCommas::Never)
      .parameters_trailing_commas(TrailingCommas::Never)
      .array_expression_trailing_commas(TrailingCommas::Never)
      .array_pattern_trailing_commas(TrailingCommas::Never)
      .enum_declaration_trailing_commas(TrailingCommas::Never)
      .import_declaration_trailing_commas(TrailingCommas::Never)
      .export_declaration_trailing_commas(TrailingCommas::Never)
      .object_expression_trailing_commas(TrailingCommas::Never)
      .object_pattern_trailing_commas(TrailingCommas::Never)
      .type_parameters_trailing_commas(TrailingCommas::Never)
      .tuple_type_trailing_commas(TrailingCommas::Never)
      .type_literal_trailing_commas(TrailingCommas::Never)
      /* use braces */
      .if_statement_use_braces(UseBraces::Always)
      .for_statement_use_braces(UseBraces::Always)
      .for_in_statement_use_braces(UseBraces::Always)
      .for_of_statement_use_braces(UseBraces::Always)
      .while_statement_use_braces(UseBraces::Always)
      /* prefer single line */
      .array_expression_prefer_single_line(false)
      .array_pattern_prefer_single_line(false)
      .arguments_prefer_single_line(false)
      .binary_expression_prefer_single_line(false)
      .computed_prefer_single_line(false)
      .conditional_expression_prefer_single_line(false)
      .conditional_type_prefer_single_line(false)
      .decorators_prefer_single_line(false)
      .export_declaration_prefer_single_line(false)
      .for_statement_prefer_single_line(false)
      .import_declaration_prefer_single_line(false)
      .jsx_attributes_prefer_single_line(false)
      .jsx_element_prefer_single_line(false)
      .mapped_type_prefer_single_line(false)
      .member_expression_prefer_single_line(false)
      .object_expression_prefer_single_line(false)
      .object_pattern_prefer_single_line(false)
      .parameters_prefer_single_line(false)
      .parentheses_prefer_single_line(false)
      .tuple_type_prefer_single_line(false)
      .type_literal_prefer_single_line(false)
      .type_parameters_prefer_single_line(false)
      .union_and_intersection_type_prefer_single_line(false)
      .variable_statement_prefer_single_line(false)
      /* force single line */
      .export_declaration_force_single_line(true)
      .import_declaration_force_single_line(true)
      /* space settings */
      .binary_expression_space_surrounding_bitwise_and_arithmetic_operator(true)
      .comment_line_force_space_after_slashes(false)
      .construct_signature_space_after_new_keyword(true)
      .constructor_space_before_parentheses(true)
      .constructor_type_space_after_new_keyword(true)
      .do_while_statement_space_after_while_keyword(true)
      .export_declaration_space_surrounding_named_exports(true)
      .for_statement_space_after_for_keyword(true)
      .for_statement_space_after_semi_colons(true)
      .for_in_statement_space_after_for_keyword(true)
      .for_of_statement_space_after_for_keyword(true)
      .function_declaration_space_before_parentheses(true)
      .function_expression_space_before_parentheses(true)
      .function_expression_space_after_function_keyword(true)
      .get_accessor_space_before_parentheses(true)
      .if_statement_space_after_if_keyword(true)
      .import_declaration_space_surrounding_named_imports(true)
      .jsx_expression_container_space_surrounding_expression(true)
      .jsx_self_closing_element_space_before_slash(true)
      .method_space_before_parentheses(true)
      .object_expression_space_surrounding_properties(false)
      .object_pattern_space_surrounding_properties(false)
      .set_accessor_space_before_parentheses(true)
      .space_surrounding_properties(false)
      .tagged_template_space_before_literal(false)
      .type_annotation_space_before_colon(true)
      .type_assertion_space_before_expression(true)
      .type_literal_space_surrounding_properties(false)
      .while_statement_space_after_while_keyword(true)
      /* space around */
      .arguments_space_around(true)
      .array_expression_space_around(true)
      .array_pattern_space_around(true)
      .do_while_statement_space_around(true)
      .for_in_statement_space_around(true)
      .for_of_statement_space_around(true)
      .for_statement_space_around(true)
      .if_statement_space_around(true)
      .parameters_space_around(true)
      .switch_statement_space_around(true)
      .tuple_type_space_around(true)
      .while_statement_space_around(true);

    let inner_config = config.get_inner_config();
    assert_eq!(inner_config.len(), 175);
    let diagnostics = resolve_config(inner_config, &resolve_global_config(ConfigKeyMap::new(), &Default::default()).config).diagnostics;
    assert_eq!(diagnostics.len(), 0);
  }
}
