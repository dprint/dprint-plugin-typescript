use super::*;

/* literals */

pub(super) fn gen_big_int_literal<'a>(node: &'a BigIntLiteral<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  node.text_fast(context.program).to_string().into_print_items(context.arena)
}

pub(super) fn gen_bool_literal<'a>(arena: &'a Arena, node: &BooleanLiteral) -> PrintItems<'a> {
  match node.value {
    true => "true",
    false => "false",
  }
  .into_print_items(arena)
}

pub(super) fn gen_num_literal<'a>(node: &'a NumericLiteral<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  node.text_fast(context.program).into_print_items(context.arena)
}

pub(super) fn gen_reg_exp_literal<'a>(node: &'a RegExpLiteral<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  // the exp and flags should not be nodes so just ignore that (swc issue #511)
  // the flags can't contain a slash, so the last one ends the pattern
  let text = node.text_fast(context.program);
  let last_slash_index = text.rfind('/').unwrap_or(text.len());
  let exp = text.get(1..last_slash_index).unwrap_or("");
  let flags = text.get(last_slash_index + 1..).unwrap_or("");
  let mut items = PrintItems::new(context.arena);
  items.push_sc(sc!("/"));
  items.push_str_runtime_width_computed(exp);
  items.push_sc(sc!("/"));
  items.push_str_runtime_width_computed(flags);
  items
}

pub(super) fn gen_string_literal<'a>(node: &'a StringLiteral<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut string_value = string_literal::get_value(node, context);
  if let Some(options) = get_class_names_sort_options(node.into_node(), context) {
    let sorted_value = tailwind::sort_class_names(string_value, &options, &get_tailwind_project(context));
    string_value = context.alloc_cow_str(sorted_value);
  }
  if matches!(node.into_node().parent(context.program), Some(Node::JSXAttribute(_))) {
    string_literal::gen_jsx_text(string_value, context)
  } else {
    string_literal::gen_non_jsx_text(string_value, context)
  }
}

/// Generates a directive (ex. `"use strict";`), which swc had as an expression
/// statement of a string literal.
pub(super) fn gen_directive<'a>(node: &'a Directive<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  gen_expr_stmt_like((&node.expression).into_node(), false, context)
}

pub(super) fn get_tpl_element_class_names_sort_options<'a>(node: &'a TemplateElement<'a>, context: &Context<'a>) -> Option<tailwind::SortOptions> {
  let Some(Node::TemplateLiteral(tpl)) = node.into_node().parent(context.program) else {
    return None;
  };
  // these are the template literal's, which `gen_tpl` gets once for all its elements
  let (tpl_start, mut options) = context.tpl_class_names_sort_options?;
  if tpl_start != tpl.start() {
    return None;
  }
  let index = tpl.quasis.binary_search_by_key(&node.start(), |quasi| quasi.start()).ok()?;
  let is_last = index >= tpl.expressions.len();
  let text = node.text_fast(context.program);
  // the class at an end is only part of a class name when the text
  // isn't separated from the neighbouring expression by whitespace
  options.ignore_first = if index > 0 {
    !text.starts_with(tailwind::is_class_separator)
  } else {
    options.ignore_first
  };
  options.ignore_last = if is_last {
    options.ignore_last
  } else {
    !text.ends_with(tailwind::is_class_separator)
  };
  options.collapse_start &= index == 0;
  options.collapse_end &= is_last;
  Some(options)
}

/// Gets how to sort the class names in the provided string or template literal, or
/// `None` when it's not somewhere that's known to have class names. This looks for
/// the same places as prettier-plugin-tailwindcss, but only where the text may be
/// part of the class names (ex. not in `styles["p-4 flex"]` or `x === "p-4 flex"`).
pub(super) fn get_class_names_sort_options<'a>(node: Node<'a>, context: &Context<'a>) -> Option<tailwind::SortOptions> {
  if context.config.jsx_sort_class_names.kind != JsxClassNamesSortOrder::Tailwind {
    return None;
  }
  let options = find_class_names_sort_options(node, context)?;
  // moving the classes around an escaped whitespace character (ex. a line continuation)
  // would change the value or escape the closing quote
  (!has_escaped_class_separator(get_class_names_text(node, context))).then_some(options)
}

pub(super) fn find_class_names_sort_options<'a>(node: Node<'a>, context: &Context<'a>) -> Option<tailwind::SortOptions> {
  let mut options = tailwind::SortOptions {
    preserve_whitespace: context.config.jsx_sort_class_names.preserve_whitespace,
    preserve_duplicates: context.config.jsx_sort_class_names.preserve_duplicates,
    ..Default::default()
  };
  let text = get_class_names_text(node, context);
  let starts_with_separator = text.starts_with(tailwind::is_class_separator);
  let ends_with_separator = text.ends_with(tailwind::is_class_separator);
  // if the text is at the start and the end of the string the child evaluates to, which is when what's
  // beside the child is beside the text and so needs whitespace to not be part of the class at that end
  let mut is_at_start = true;
  let mut is_at_end = true;
  let mut child = node;
  while let Some(parent) = child.parent(context.program) {
    let mut keeps_ends = false;
    // this stops at the first parent where the child isn't what has the class names, which
    // also stops the search right away for most strings and template literals in a file
    match parent {
      Node::JSXAttribute(attr) => return is_class_names_jsx_attr(attr, context).then_some(options),
      // optional calls are also call expressions
      Node::CallExpression(call_expr) => {
        if child.start() >= call_expr.callee.end() && is_class_names_function(&call_expr.callee, context) {
          return Some(options);
        }
      }
      // swc had `import(...)` as a call expression whose callee isn't an expression, which continued the search
      Node::ImportExpression(_) => {}
      Node::TaggedTemplateExpression(tagged_tpl) => {
        // the text of a template whose tag isn't configured isn't class names (ex. emotion's `css`)
        if child.start() < tagged_tpl.quasi.start() || !is_class_names_function(&tagged_tpl.tag, context) {
          return None;
        }
        // only the text of the template has class names, not the strings in its expressions
        if child.kind() == node.kind() && child.range() == node.range() {
          return Some(options);
        }
      }
      Node::BinaryExpression(bin_expr) if bin_expr.operator == BinaryOperator::Addition => {
        // the whitespace between concatenated strings separates their class names
        keeps_ends = true;
        if child.start() == bin_expr.left.start() {
          options.collapse_end = false;
          let is_separated = ends_with_separator || get_class_names_text((&bin_expr.right).into_node(), context).starts_with(tailwind::is_class_separator);
          options.ignore_last |= is_at_end && !is_separated;
          is_at_end = false;
        } else {
          options.collapse_start = false;
          let is_separated = starts_with_separator || get_class_names_text((&bin_expr.left).into_node(), context).ends_with(tailwind::is_class_separator);
          options.ignore_first |= is_at_start && !is_separated;
          is_at_start = false;
        }
      }
      Node::TemplateLiteral(tpl) => {
        // this is an expression in a template literal, so the whitespace at an end may
        // only be removed when the text next to it has whitespace to separate the classes
        if let Some(index) = tpl.expressions.iter().position(|expr| expr.start() == child.start()) {
          keeps_ends = true;
          let text_before = tpl.quasis[index].text_fast(context.program);
          let text_after = tpl.quasis[index + 1].text_fast(context.program);
          let is_separated_before = text_before.ends_with(tailwind::is_class_separator);
          let is_separated_after = text_after.starts_with(tailwind::is_class_separator);
          options.collapse_start &= is_separated_before;
          options.collapse_end &= is_separated_after;
          // nothing is before the first expression or after the last one when there's no text there
          let is_first = index == 0 && text_before.is_empty();
          let is_last = index + 1 == tpl.expressions.len() && text_after.is_empty();
          options.ignore_first |= is_at_start && !is_first && !is_separated_before && !starts_with_separator;
          options.ignore_last |= is_at_end && !is_last && !is_separated_after && !ends_with_separator;
          is_at_start &= is_first;
          is_at_end &= is_last;
        }
      }
      // these evaluate to one of their operands (`&&`, `||` and `??`)
      Node::LogicalExpression(_) => keeps_ends = true,
      // a string in the type of one of these is under a type node, which stops the search
      Node::ParenthesizedExpression(_)
      | Node::ConditionalExpression(_)
      | Node::TSAsExpression(_)
      | Node::TSSatisfiesExpression(_)
      | Node::TSNonNullExpression(_) => keeps_ends = true,
      // the object may have class names (ex. `["p-4", "flex"].join(" ")`), but not the
      // property or an object that's indexed into (ex. `styles["p-4 flex"]`)
      Node::StaticMemberExpression(member_expr) => {
        if child.start() != member_expr.object.start() {
          return None;
        }
      }
      Node::PrivateFieldExpression(member_expr) => {
        if child.start() != member_expr.object.start() {
          return None;
        }
      }
      Node::ComputedMemberExpression(_) => return None,
      // only a key value property is one of these, not a method, getter, setter or computed key
      Node::ObjectProperty(prop) => {
        if prop.method || prop.kind != PropertyKind::Init || (prop.computed && child.start() < prop.value.start()) {
          return None;
        }
      }
      // these hold class names that get combined (ex. `cn(["p-4", { flex: active }])`)
      Node::JSXExpressionContainer(_) | Node::SpreadElement(_) | Node::ArrayExpression(_) | Node::ObjectExpression(_) | Node::ChainExpression(_) => {}
      // anything else, like a member expression's property or a comparison, doesn't
      _ => return None,
    }
    if !keeps_ends {
      is_at_start = false;
      is_at_end = false;
    }
    child = parent;
  }
  None
}

/// Gets the text within the quotes of a string or template literal and otherwise an empty string.
pub(super) fn get_class_names_text<'a>(node: Node<'a>, context: &Context<'a>) -> &'a str {
  match node {
    Node::StringLiteral(_) | Node::TemplateLiteral(_) => {
      let text = node.text_fast(context.program);
      &text[1..text.len() - 1]
    }
    _ => "",
  }
}

/// Gets if the text has a backslash that escapes whitespace (ex. `\` then a newline).
pub(super) fn has_escaped_class_separator(text: &str) -> bool {
  let mut is_escaped = false;
  for c in text.chars() {
    if is_escaped && tailwind::is_class_separator(c) {
      return true;
    }
    is_escaped = !is_escaped && c == '\\';
  }
  false
}

pub(super) fn is_class_names_jsx_attr(attr: &JSXAttribute, context: &Context) -> bool {
  let name = attr.name.text_fast(context.program);
  matches!(name, "class" | "className")
    || context
      .config
      .jsx_sort_class_names
      .attributes
      .iter()
      .any(|attribute_name| attribute_name == name)
}

pub(super) fn get_tailwind_project<'a>(context: &Context<'a>) -> tailwind::Project<'a> {
  let config = &context.config.jsx_sort_class_names;
  tailwind::Project {
    prefix: config.prefix.as_deref(),
    theme: &config.theme,
    variants: &config.variants,
    utilities: &config.utilities,
  }
}

/// Gets if the expression starts with one of the configured function names (ex. `cn` in `cn.foo("")`).
pub(super) fn is_class_names_function<'a>(expr: &Expression<'a>, context: &Context<'a>) -> bool {
  if context.config.jsx_sort_class_names.functions.is_empty() {
    return false;
  }

  let mut expr = expr;
  loop {
    expr = match expr {
      Expression::Identifier(ident) => {
        let name = ident.text_fast(context.program);
        return context.config.jsx_sort_class_names.functions.iter().any(|function_name| function_name == name);
      }
      Expression::StaticMemberExpression(member_expr) => &member_expr.object,
      Expression::ComputedMemberExpression(member_expr) => &member_expr.object,
      Expression::PrivateFieldExpression(member_expr) => &member_expr.object,
      Expression::CallExpression(call_expr) => &call_expr.callee,
      Expression::ChainExpression(chain_expr) => match &chain_expr.expression {
        ChainElement::StaticMemberExpression(member_expr) => &member_expr.object,
        ChainElement::ComputedMemberExpression(member_expr) => &member_expr.object,
        ChainElement::PrivateFieldExpression(member_expr) => &member_expr.object,
        ChainElement::CallExpression(call_expr) => &call_expr.callee,
        ChainElement::TSNonNullExpression(_) => return false,
      },
      _ => return false,
    };
  }
}

pub(super) mod string_literal {
  use super::*;

  pub fn gen_non_jsx_text<'a>(string_value: &'a str, context: &mut Context<'a>) -> PrintItems<'a> {
    match context.config.quote_style {
      QuoteStyle::AlwaysDouble => format_with_double(string_value, context),
      QuoteStyle::AlwaysSingle => format_with_single(string_value, context),
      QuoteStyle::PreferDouble => handle_prefer_double(string_value, context),
      QuoteStyle::PreferSingle => handle_prefer_single(string_value, context),
    }
  }

  pub fn gen_jsx_text<'a>(string_value: &'a str, context: &mut Context<'a>) -> PrintItems<'a> {
    // JSX attributes cannot contain escaped quotes so regardless of
    // configuration, allow changing the quote style to single or
    // double depending on if it contains the opposite quote
    match context.config.jsx_quote_style {
      JsxQuoteStyle::PreferDouble => handle_prefer_double(string_value, context),
      JsxQuoteStyle::PreferSingle => handle_prefer_single(string_value, context),
    }
  }

  /// Gets the value of the string literal, which is a slice of the file's text when
  /// it doesn't have any escaped quotes.
  pub fn get_value<'a>(node: &StringLiteral<'a>, context: &mut Context<'a>) -> &'a str {
    let raw_string_text = node.text_fast(context.program);
    if raw_string_text.len() <= 2 {
      return "";
    }
    let string_value = &raw_string_text[1..raw_string_text.len() - 1];
    if !string_value.contains('\\') {
      return string_value;
    }
    let is_double_quote = raw_string_text.starts_with('"');

    let value = match is_double_quote {
      true => remove_needless_quote_backslashes(string_value.replace("\\\"", "\"")),
      false => remove_needless_quote_backslashes(string_value.replace("\\'", "'")),
    };
    return context.alloc_str(&value);

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

  fn handle_prefer_double<'a>(string_value: &'a str, context: &mut Context<'a>) -> PrintItems<'a> {
    if double_to_single(string_value) <= 0 {
      format_with_double(string_value, context)
    } else {
      format_with_single(string_value, context)
    }
  }

  fn handle_prefer_single<'a>(string_value: &'a str, context: &mut Context<'a>) -> PrintItems<'a> {
    if double_to_single(string_value) >= 0 {
      format_with_single(string_value, context)
    } else {
      format_with_double(string_value, context)
    }
  }

  fn format_with_double<'a>(string_value: &'a str, context: &mut Context<'a>) -> PrintItems<'a> {
    const DOUBLE_QUOTE_SC: &StringContainer = sc!("\"");
    let string_value = if string_value.contains('"') {
      context.alloc_str(&string_value.replace('"', "\\\""))
    } else {
      string_value
    };
    let mut items = PrintItems::new(context.arena);
    items.push_sc(DOUBLE_QUOTE_SC);
    items.extend(gen_from_raw_string(context.arena, string_value));
    items.push_sc(DOUBLE_QUOTE_SC);
    items
  }

  fn format_with_single<'a>(string_value: &'a str, context: &mut Context<'a>) -> PrintItems<'a> {
    const SINGLE_QUOTE_SC: &StringContainer = sc!("'");
    let string_value = if string_value.contains('\'') {
      context.alloc_str(&string_value.replace('\'', "\\'"))
    } else {
      string_value
    };
    let mut items = PrintItems::new(context.arena);
    items.push_sc(SINGLE_QUOTE_SC);
    items.extend(gen_from_raw_string(context.arena, string_value));
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

pub(super) fn gen_program<'a>(node: &'a Program<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  // swc's program range was empty when it had no tokens, which is where the
  // comments of a file with only comments are found
  let range = if context.program.tokens().is_empty() {
    SourceRange::new(node.start(), node.start())
  } else {
    node.range()
  };
  // the directives always come before the other statements
  let statements: Vec<Node<'a>> = node
    .directives
    .iter()
    .map(|x| x.into_node())
    .chain(node.body.iter().map(|x| x.into_node()))
    .collect();
  let mut items = PrintItems::new(context.arena);
  if let Some(hashbang) = &node.hashbang {
    // the hashbang's range includes the `#!`
    let hashbang_text = hashbang.text_fast(context.program);
    items.push_sc(sc!("#!"));
    items.push_str(hashbang_text.strip_prefix("#!").unwrap_or(hashbang_text));
    if let Some(first_statement) = statements.first() {
      // write the newline now instead of expecting one, so the infos at the start of
      // the first statement are on its line
      items.push_signal(Signal::NewLine);
      let blank_line_count = get_maintained_blank_line_count(&range.start, first_statement, context);
      if blank_line_count > 0 {
        push_blank_lines(&mut items, blank_line_count);
      }
    } else {
      items.push_signal(Signal::ExpectNewLine);
      let shebang_end = hashbang.end();
      items.extend(gen_trailing_comments_as_statements(&shebang_end.range(), context));
    }
  }

  items.extend(gen_statements(range, statements, context));

  items
}

/* patterns */

pub(super) fn gen_array_pat<'a>(node: &'a ArrayPattern<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  // the optional `?` and type annotation are generated by the parent (ex. a parameter) in oxc,
  // so the pattern's range is the range of the brackets
  gen_array_like_nodes(
    GenArrayLikeNodesOptions {
      node: node.into_node(),
      range: node.range(),
      nodes: node
        .elements
        .iter()
        .map(|x| x.as_ref().map(|elem| elem.into_node()))
        .chain(node.rest.iter().map(|rest| Some(rest.into_node())))
        .collect(),
      prefer_hanging: context.config.array_pattern_prefer_hanging,
      prefer_single_line: context.config.array_pattern_prefer_single_line,
      trailing_commas: context.config.array_pattern_trailing_commas,
      space_around: context.config.array_pattern_space_around,
    },
    context,
  )
}

pub(super) fn gen_assign_pat<'a>(node: &'a AssignmentPattern<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  items.extend(gen_node((&node.left).into_node(), context));
  items.extend(gen_assignment((&node.right).into_node(), sc!("="), context));
  items
}

/// Generates a property of an object pattern, which is either shorthand (ex. `a` or `a = 1`)
/// or a key and a value (ex. `a: b` or `[a]: b`).
pub(super) fn gen_binding_property<'a>(node: &'a BindingProperty<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  if node.shorthand {
    // the value has the key, along with the default value when it has one (ex. `a = 1`)
    items.extend(gen_node((&node.value).into_node(), context));
  } else {
    items.extend(gen_pat_prop_key((&node.key).into_node(), node.computed, context));
    items.extend(gen_assignment((&node.value).into_node(), sc!(":"), context));
  }
  items
}

pub(super) fn gen_rest_pat<'a>(node: &'a BindingRestElement<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  // the type annotation is generated by the parent (ex. a rest parameter) in oxc
  let mut items = PrintItems::new(context.arena);
  items.push_sc(sc!("..."));
  items.extend(gen_node((&node.argument).into_node(), context));
  items
}

pub(super) fn gen_object_pat<'a>(node: &'a ObjectPattern<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  // the optional `?` and type annotation are generated by the parent (ex. a parameter) in oxc
  return gen_object_like_node(
    GenObjectLikeNodeOptions {
      node: node.into_node(),
      members: node
        .properties
        .iter()
        .map(|x| x.into_node())
        .chain(node.rest.iter().map(|rest| rest.into_node()))
        .collect(),
      separator: get_trailing_commas(node, context).into(),
      prefer_hanging: context.config.object_pattern_prefer_hanging,
      prefer_single_line: context.config.object_pattern_prefer_single_line,
      force_single_line: false,
      force_multi_line: is_node_definitely_above_line_width(node.range(), context),
      surround_single_line_with_spaces: context.config.object_pattern_space_surrounding_properties,
      blank_lines: ListBlankLines::Maintain,
      node_sorter: None,
    },
    context,
  );

  fn get_trailing_commas<'a>(node: &ObjectPattern, context: &Context<'a>) -> TrailingCommas {
    if node.rest.is_some() {
      return TrailingCommas::Never;
    }
    context.config.object_pattern_trailing_commas
  }
}

/* assignment targets (destructuring assignments, which are generated like patterns) */

pub(super) fn gen_array_assignment_target<'a>(node: &'a ArrayAssignmentTarget<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  gen_array_like_nodes(
    GenArrayLikeNodesOptions {
      node: node.into_node(),
      range: node.range(),
      nodes: node
        .elements
        .iter()
        .map(|x| x.as_ref().map(|elem| elem.into_node()))
        .chain(node.rest.iter().map(|rest| Some(rest.into_node())))
        .collect(),
      prefer_hanging: context.config.array_pattern_prefer_hanging,
      prefer_single_line: context.config.array_pattern_prefer_single_line,
      trailing_commas: context.config.array_pattern_trailing_commas,
      space_around: context.config.array_pattern_space_around,
    },
    context,
  )
}

pub(super) fn gen_object_assignment_target<'a>(node: &'a ObjectAssignmentTarget<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  gen_object_like_node(
    GenObjectLikeNodeOptions {
      node: node.into_node(),
      members: node
        .properties
        .iter()
        .map(|x| x.into_node())
        .chain(node.rest.iter().map(|rest| rest.into_node()))
        .collect(),
      separator: if node.rest.is_some() {
        TrailingCommas::Never
      } else {
        context.config.object_pattern_trailing_commas
      }
      .into(),
      prefer_hanging: context.config.object_pattern_prefer_hanging,
      prefer_single_line: context.config.object_pattern_prefer_single_line,
      force_single_line: false,
      force_multi_line: is_node_definitely_above_line_width(node.range(), context),
      surround_single_line_with_spaces: context.config.object_pattern_space_surrounding_properties,
      blank_lines: ListBlankLines::Maintain,
      node_sorter: None,
    },
    context,
  )
}

pub(super) fn gen_assignment_target_with_default<'a>(node: &'a AssignmentTargetWithDefault<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  items.extend(gen_node((&node.binding).into_node(), context));
  items.extend(gen_assignment((&node.init).into_node(), sc!("="), context));
  items
}

/// Generates a shorthand property of an object assignment target (ex. `a` or `a = 1`).
pub(super) fn gen_assignment_target_property_identifier<'a>(node: &'a AssignmentTargetPropertyIdentifier<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  items.extend(gen_node((&node.binding).into_node(), context));
  if let Some(init) = &node.init {
    items.extend(gen_assignment(init.into_node(), sc!("="), context));
  }
  items
}

/// Generates a key and value property of an object assignment target (ex. `a: b` or `[a]: b = 1`).
pub(super) fn gen_assignment_target_property_property<'a>(node: &'a AssignmentTargetPropertyProperty<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  items.extend(gen_pat_prop_key((&node.name).into_node(), node.computed, context));
  items.extend(gen_assignment((&node.binding).into_node(), sc!(":"), context));
  items
}

pub(super) fn gen_assignment_target_rest<'a>(node: &'a AssignmentTargetRest<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  items.push_sc(sc!("..."));
  items.extend(gen_node((&node.target).into_node(), context));
  items
}

/// Generates the key of a property in a pattern, which is surrounded by brackets when computed.
pub(super) fn gen_pat_prop_key<'a>(key: Node<'a>, computed: bool, context: &mut Context<'a>) -> PrintItems<'a> {
  if computed {
    // swc had a node for the computed key that included the brackets
    gen_computed_prop_name(key, context)
  } else {
    gen_node(key, context)
  }
}

/* properties */

/// Generates a method of an object literal (ex. `a() {}`).
pub(super) fn gen_method_prop<'a>(node: &'a ObjectProperty<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let Expression::FunctionExpression(function) = &node.value else {
    unreachable!("expected the value of a method property to be a function");
  };
  let function: &'a Function<'a> = function;
  gen_class_or_object_method(
    ClassOrObjectMethod {
      node: node.into_node(),
      parameters_range: function.get_parameters_range(context),
      decorators: None,
      accessibility: None,
      is_static: false,
      is_async: function.r#async,
      is_abstract: false,
      is_override: false,
      kind: ClassOrObjectMethodKind::Method,
      is_generator: function.generator,
      is_optional: false,
      key: (&node.key).into_node(),
      computed: node.computed,
      type_params: function.type_parameters.as_ref().map(|x| x.into_node()),
      params: get_function_params(function, context),
      return_type: function.return_type.as_ref().map(|x| x.into_node()),
      body: function.body.as_ref().map(|x| x.into_node()),
    },
    context,
  )
}

pub(super) struct ClassOrObjectMethod<'a> {
  pub(super) node: Node<'a>,
  pub(super) parameters_range: Option<SourceRange>,
  pub(super) decorators: Option<&'a [Decorator<'a>]>,
  pub(super) accessibility: Option<TSAccessibility>,
  pub(super) is_static: bool,
  pub(super) is_async: bool,
  pub(super) is_abstract: bool,
  pub(super) kind: ClassOrObjectMethodKind,
  pub(super) is_generator: bool,
  pub(super) is_optional: bool,
  pub(super) is_override: bool,
  pub(super) key: Node<'a>,
  /// Whether the key is computed (ex. `[a]() {}`), which needs brackets around it.
  pub(super) computed: bool,
  pub(super) type_params: Option<Node<'a>>,
  pub(super) params: BumpVec<'a, Node<'a>>,
  pub(super) return_type: Option<Node<'a>>,
  pub(super) body: Option<Node<'a>>,
}

pub(super) enum ClassOrObjectMethodKind {
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

impl From<PropertyKind> for ClassOrObjectMethodKind {
  fn from(kind: PropertyKind) -> ClassOrObjectMethodKind {
    match kind {
      PropertyKind::Get => ClassOrObjectMethodKind::Getter,
      PropertyKind::Set => ClassOrObjectMethodKind::Setter,
      PropertyKind::Init => ClassOrObjectMethodKind::Method,
    }
  }
}

pub(super) fn gen_class_or_object_method<'a>(node: ClassOrObjectMethod<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  if let Some(decorators) = node.decorators {
    items.extend(gen_decl_decorators(decorators, context));
  }

  let start_header_lsil = LineStartIndentLevel::new("methodStartHeader");
  items.push_info(start_header_lsil);

  if let Some(accessibility) = node.accessibility {
    items.push_str_runtime_width_computed(accessibility_to_str(accessibility));
    items.push_space();
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
  let key = node.key;
  items.extend(if node.computed {
    // swc had a node for the computed key that included the brackets
    gen_computed_prop_name(key, context)
  } else {
    gen_quotable_prop(key, context)
  });
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
  } else if context.config.semicolons.is_true() {
    items.push_sc(sc!(";"));
  }

  return items;

  fn get_use_space_before_parens<'a>(kind: &ClassOrObjectMethodKind, context: &mut Context<'a>) -> bool {
    match kind {
      ClassOrObjectMethodKind::Constructor => context.config.constructor_space_before_parentheses,
      ClassOrObjectMethodKind::Getter => context.config.get_accessor_space_before_parentheses,
      ClassOrObjectMethodKind::Setter => context.config.set_accessor_space_before_parentheses,
      ClassOrObjectMethodKind::Method => context.config.method_space_before_parentheses,
    }
  }

  fn get_brace_position<'a>(kind: &ClassOrObjectMethodKind, context: &mut Context<'a>) -> BracePosition {
    match kind {
      ClassOrObjectMethodKind::Constructor => context.config.constructor_brace_position,
      ClassOrObjectMethodKind::Getter => context.config.get_accessor_brace_position,
      ClassOrObjectMethodKind::Setter => context.config.set_accessor_brace_position,
      ClassOrObjectMethodKind::Method => context.config.method_brace_position,
    }
  }
}

pub(super) fn accessibility_to_str(accessibility: TSAccessibility) -> &'static str {
  match accessibility {
    TSAccessibility::Private => "private",
    TSAccessibility::Protected => "protected",
    TSAccessibility::Public => "public",
  }
}

/* statements */

pub(super) fn gen_block_stmt<'a>(node: &'a BlockStatement<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  gen_block(
    |stmts, context| gen_statements(node.get_inner_range(context), stmts, context),
    GenBlockOptions {
      range: Some(node.range()),
      children: node.body.iter().map(|x| x.into_node()).collect(),
      block_padding: get_block_stmt_block_padding(node, context),
      keep_blank_lines_before_close_token_comments: true,
    },
    context,
  )
}

pub(super) fn get_block_stmt_block_padding<'a>(node: &'a BlockStatement<'a>, context: &Context<'a>) -> BlockPadding {
  let config = &context.config;
  // a static block has its statements directly in oxc, so a block in it is a nested block
  // and the static block generates its own body with `staticBlock.blockPadding`
  match node.into_node().parent(context.program) {
    Some(Node::DoWhileStatement(_)) => config.do_while_statement_block_padding,
    Some(Node::SwitchCase(_)) => config.switch_case_block_padding,
    _ => config.block_padding,
  }
}

pub(super) fn gen_function_body<'a>(node: &'a FunctionBody<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  gen_block(
    |stmts, context| gen_statements(node.get_inner_range(context), stmts, context),
    GenBlockOptions {
      range: Some(node.range()),
      // the directives always come before the other statements
      children: node
        .directives
        .iter()
        .map(|x| x.into_node())
        .chain(node.statements.iter().map(|x| x.into_node()))
        .collect(),
      block_padding: get_function_body_block_padding(node, context),
      keep_blank_lines_before_close_token_comments: true,
    },
    context,
  )
}

pub(super) fn get_function_body_block_padding<'a>(node: &'a FunctionBody<'a>, context: &Context<'a>) -> BlockPadding {
  let config = &context.config;
  let program = context.program;
  match node.into_node().parent(program) {
    Some(Node::Function(function)) => {
      let function_parent = function.into_node().parent(program);
      match function_parent {
        // the function is the value of these, which swc had as separate nodes
        Some(Node::MethodDefinition(method)) => match method.kind {
          MethodDefinitionKind::Constructor => config.constructor_block_padding,
          MethodDefinitionKind::Method => config.method_block_padding,
          MethodDefinitionKind::Get => config.get_accessor_block_padding,
          MethodDefinitionKind::Set => config.set_accessor_block_padding,
        },
        Some(Node::ObjectProperty(prop)) if prop.method || prop.kind != PropertyKind::Init => match prop.kind {
          PropertyKind::Init => config.method_block_padding,
          PropertyKind::Get => config.get_accessor_block_padding,
          PropertyKind::Set => config.set_accessor_block_padding,
        },
        _ => match function.r#type {
          FunctionType::FunctionDeclaration | FunctionType::TSDeclareFunction => {
            // swc had `export default function() {}` as a function expression, which is only
            // generated like a declaration when it has a name
            if matches!(function_parent, Some(Node::ExportDefaultDeclaration(_))) && function.id.is_none() {
              config.function_expression_block_padding
            } else {
              config.function_declaration_block_padding
            }
          }
          FunctionType::FunctionExpression | FunctionType::TSEmptyBodyFunctionExpression => config.function_expression_block_padding,
        },
      }
    }
    Some(Node::ArrowFunctionExpression(_)) => config.arrow_function_block_padding,
    _ => config.block_padding,
  }
}

pub(super) fn gen_break_stmt<'a>(node: &'a BreakStatement<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);

  items.push_sc(sc!("break"));
  if let Some(label) = &node.label {
    items.push_space();
    items.extend(gen_node(label.into_node(), context));
  }
  if context.config.semicolons.is_true() {
    items.push_sc(sc!(";"));
  }

  items
}

pub(super) fn gen_continue_stmt<'a>(node: &'a ContinueStatement<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);

  items.push_sc(sc!("continue"));
  if let Some(label) = &node.label {
    items.push_space();
    items.extend(gen_node(label.into_node(), context));
  }
  if context.config.semicolons.is_true() {
    items.push_sc(sc!(";"));
  }

  items
}

pub(super) fn gen_debugger_stmt<'a>(_: &'a DebuggerStatement, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);

  items.push_sc(sc!("debugger"));
  if context.config.semicolons.is_true() {
    items.push_sc(sc!(";"));
  }

  items
}

pub(super) fn gen_do_while_stmt<'a>(node: &'a DoWhileStatement<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  // the braces are technically optional on do while statements
  let mut items = PrintItems::new(context.arena);
  items.push_sc(sc!("do"));
  items.extend(gen_brace_separator(
    GenBraceSeparatorOptions {
      brace_position: context.config.do_while_statement_brace_position,
      open_brace_token: if let Statement::BlockStatement(_) = &node.body {
        context.token_finder.get_first_open_brace_token_within(node)
      } else {
        None
      },
      start_header_lsil: None,
    },
    context,
  ));
  items.extend(gen_node((&node.body).into_node(), context));
  if context.config.semicolons.is_true() || matches!(node.body, Statement::BlockStatement(_)) {
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
  let open_paren_token = context.token_finder.get_previous_token_if_open_paren(&node.test.range());
  if use_space_after_keyword(context.config.do_while_statement_space_after_while_keyword, open_paren_token, context) {
    items.push_space();
  }
  items.extend(gen_node_in_parens(
    |context| gen_node((&node.test).into_node(), context),
    GenNodeInParensOptions {
      inner_range: node.test.range(),
      prefer_hanging: context.config.do_while_statement_prefer_hanging,
      allow_open_paren_trailing_comments: false,
      single_line_space_around: context.config.do_while_statement_space_around,
    },
    context,
  ));
  if context.config.semicolons.is_true() {
    items.push_sc(sc!(";"));
  }
  items
}

pub(super) fn gen_export_all<'a>(node: &'a ExportAllDeclaration<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  if let Some(exported) = &node.exported {
    return gen_export_all_as_namespace(node, exported, context);
  }

  let mut items = PrintItems::new(context.arena);
  if node.export_kind.is_type() {
    items.push_sc(sc!("export type * from "));
  } else {
    items.push_sc(sc!("export * from "));
  }
  items.extend(gen_node((&node.source).into_node(), context));

  if let Some(with_clause) = &node.with_clause {
    items.extend(gen_with_clause_with_keyword(with_clause, context));
  }

  if context.config.semicolons.is_true() {
    items.push_sc(sc!(";"));
  }

  return items;

  /// Generates `export * as ns from "x"`, which swc had as a named export with a
  /// namespace specifier, so this generates it the same way.
  fn gen_export_all_as_namespace<'a>(node: &'a ExportAllDeclaration<'a>, exported: &'a ModuleExportName<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
    let force_single_line = context.config.export_declaration_force_single_line && !contains_line_or_multiline_comment(node.into_node(), context.program);

    let mut items = PrintItems::new(context.arena);

    items.push_sc(sc!("export "));
    if node.export_kind.is_type() {
      items.push_sc(sc!("type "));
    }

    items.push_sc(sc!("* as "));
    items.extend(gen_node(exported.into_node(), context));

    items.push_sc(sc!(" from "));
    items.extend(gen_node((&node.source).into_node(), context));

    if let Some(with_clause) = &node.with_clause {
      items.extend(gen_with_clause_with_keyword(with_clause, context));
    }

    if context.config.semicolons.is_true() {
      items.push_sc(sc!(";"));
    }

    if force_single_line { with_no_new_lines(items) } else { items }
  }
}

pub(super) fn gen_empty_stmt<'a>(_: &'a EmptyStatement, context: &mut Context<'a>) -> PrintItems<'a> {
  sc!(";").into_print_items(context.arena)
}

pub(super) fn gen_export_assignment<'a>(node: &'a TSExportAssignment<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);

  items.push_sc(sc!("export"));
  items.extend(gen_assignment((&node.expression).into_node(), sc!("="), context));
  if context.config.semicolons.is_true() {
    items.push_sc(sc!(";"));
  }

  items
}

pub(super) fn gen_namespace_export<'a>(node: &'a TSNamespaceExportDeclaration<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  items.push_sc(sc!("export as namespace "));
  items.extend(gen_node((&node.id).into_node(), context));

  if context.config.semicolons.is_true() {
    items.push_sc(sc!(";"));
  }

  items
}

pub(super) fn gen_expr_stmt<'a>(stmt: &'a ExpressionStatement<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let is_do_while_body = matches!(stmt.into_node().parent(context.program), Some(Node::DoWhileStatement(_)));
  gen_expr_stmt_like((&stmt.expression).into_node(), is_do_while_body, context)
}

/// Generates the expression of an expression statement or directive followed by its semi-colon.
pub(super) fn gen_expr_stmt_like<'a>(expr: Node<'a>, is_do_while_body: bool, context: &mut Context<'a>) -> PrintItems<'a> {
  if context.config.semicolons.is_true() || is_do_while_body {
    return gen_inner(expr, context);
  } else {
    return gen_for_prefix_semi_colon_insertion(expr, context);
  }

  fn gen_inner<'a>(expr: Node<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
    let mut items = PrintItems::new(context.arena);
    items.extend(gen_node(expr, context));
    if context.config.semicolons.is_true() {
      items.push_sc(sc!(";"));
    }
    items
  }

  fn gen_for_prefix_semi_colon_insertion<'a>(expr: Node<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
    let generated_node = gen_inner(expr, context);
    let generated_node = generated_node.into_rc_path();
    let brace_condition_ref = context.take_expr_stmt_single_line_parent_brace_ref(); // always clear this
    return if should_add_semi_colon(&generated_node).unwrap_or(false) {
      let mut items = PrintItems::new(context.arena);
      if let Some(brace_condition_ref) = brace_condition_ref {
        // Do not add a semi-colon when the semi-colon is within an if stmt or for-like stmt where
        // there are no braces on the parent (ex. `if (true) []`) as this would break the code.
        items.push_condition(if_true(
          "semiColonIfBrace",
          context.arena.alloc_resolver(move |context| context.resolved_condition(&brace_condition_ref)),
          sc!(";").into_print_items(context.arena),
        ));
      } else {
        items.push_sc(sc!(";"));
      }
      items.extend(generated_node.into_print_items(context.arena));
      items
    } else {
      generated_node.into_print_items(context.arena)
    };

    fn should_add_semi_colon<'a>(path: &Option<PrintItemPath<'a>>) -> Option<bool> {
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

pub(super) fn gen_for_stmt<'a>(node: &'a ForStatement<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
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
        .find(|t| t.kind == TokenKind::Semicolon)
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
  let mut items = PrintItems::new(context.arena);
  items.push_info(start_header_ln);
  items.push_info(start_header_lsil);
  items.push_sc(sc!("for"));
  let open_paren_token = node.tokens_fast(context.program).get(1).filter(|token| token.kind == TokenKind::LParen);
  if use_space_after_keyword(context.config.for_statement_space_after_for_keyword, open_paren_token, context) {
    items.push_space();
  }
  // these are generated before the init is, which would otherwise take them as its leading comments
  if let Some(open_paren_token) = open_paren_token {
    for comment in get_comments_before_token(open_paren_token, context) {
      if let Some(generated_comment) = gen_comment(comment, context) {
        items.extend(generated_comment);
        items.push_space();
      }
    }
  }

  let separator_after_semicolons = if context.config.for_statement_space_after_semicolons {
    Signal::SpaceOrNewLine
  } else {
    Signal::PossibleNewLine
  };
  let generated_init = ir_helpers::new_line_group({
    let mut items = PrintItems::new(context.arena);
    if let Some(init) = &node.init {
      items.extend(gen_node(init.into_node(), context));
    }
    items.push_sc(sc!(";"));
    if node.test.is_none() {
      items.push_sc(sc!(";"));
    }
    items
  });
  let generated_test = node.test.as_ref().map(|test| {
    ir_helpers::new_line_group({
      let mut items = PrintItems::new(context.arena);
      items.extend(gen_node(test.into_node(), context));
      items.push_sc(sc!(";"));
      items
    })
  });
  let generated_update = node
    .update
    .as_ref()
    .map(|update| ir_helpers::new_line_group(gen_node(update.into_node(), context)));

  items.extend(gen_node_in_parens(
    |context| {
      let arena = context.arena;
      ir_helpers::gen_separated_values(
        move |_| {
          let mut generated_nodes = BumpVec::with_capacity_in(3, arena.bump());
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
          max_blank_lines: 0,
          indent_width: context.config.indent_width,
          single_line_options: ir_helpers::SingleLineOptions::separated_same_line(separator_after_semicolons.into_print_items(context.arena)),
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
        body_node: (&node.body).into_node(),
        use_braces: context.config.for_statement_use_braces,
        brace_position: context.config.for_statement_brace_position,
        block_padding: context.config.for_statement_block_padding,
        single_body_position: Some(context.config.for_statement_single_body_position),
        if_chain_branch: None,
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

pub(super) fn gen_for_in_stmt<'a>(node: &'a ForInStatement<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let start_header_ln = LineNumber::new("startHeader");
  let start_header_lsil = LineStartIndentLevel::new("startHeader");
  let end_header_ln = LineNumber::new("endHeader");
  let mut items = PrintItems::new(context.arena);
  items.push_info(start_header_ln);
  items.push_info(start_header_lsil);
  items.push_sc(sc!("for"));
  let inner_header_range = SourceRange::new(node.left.start(), node.right.end());
  let open_paren_token = context.token_finder.get_previous_token_if_open_paren(&inner_header_range);
  if use_space_after_keyword(context.config.for_in_statement_space_after_for_keyword, open_paren_token, context) {
    items.push_space();
  }
  items.extend(gen_node_in_parens(
    |context| {
      let mut items = PrintItems::new(context.arena);
      items.extend(gen_assignment_target((&node.left).into_node(), context));
      items.push_signal(Signal::SpaceOrNewLine);
      items.push_condition(conditions::indent_if_start_of_line({
        let mut items = PrintItems::new(context.arena);
        items.push_sc(sc!("in "));
        items.extend(gen_node((&node.right).into_node(), context));
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
        body_node: (&node.body).into_node(),
        use_braces: context.config.for_in_statement_use_braces,
        brace_position: context.config.for_in_statement_brace_position,
        block_padding: context.config.for_in_statement_block_padding,
        single_body_position: Some(context.config.for_in_statement_single_body_position),
        if_chain_branch: None,
        start_header_info: Some((start_header_ln, start_header_lsil)),
        end_header_info: Some(end_header_ln),
      },
      context,
    )
    .generated_node,
  );

  items
}

pub(super) fn gen_for_of_stmt<'a>(node: &'a ForOfStatement<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let start_header_ln = LineNumber::new("startHeader");
  let start_header_lsil = LineStartIndentLevel::new("startHeader");
  let end_header_ln = LineNumber::new("endHeader");
  let mut items = PrintItems::new(context.arena);
  items.push_info(start_header_ln);
  items.push_info(start_header_lsil);
  items.push_sc(sc!("for"));
  if node.r#await {
    if let Some(await_token) = node.tokens_fast(context.program).get(1) {
      for comment in get_comments_before_token(await_token, context) {
        if let Some(generated_comment) = gen_comment(comment, context) {
          items.push_space();
          items.extend(generated_comment);
        }
      }
    }
    items.push_space();
    items.push_sc(sc!("await"));
  }
  let inner_header_range = SourceRange::new(node.left.start(), node.right.end());
  let open_paren_token = context.token_finder.get_previous_token_if_open_paren(&inner_header_range);
  if use_space_after_keyword(context.config.for_of_statement_space_after_for_keyword, open_paren_token, context) {
    items.push_space();
  }
  items.extend(gen_node_in_parens(
    |context| {
      let mut items = PrintItems::new(context.arena);
      items.extend(gen_assignment_target((&node.left).into_node(), context));
      items.push_signal(Signal::SpaceOrNewLine);
      items.push_condition(conditions::indent_if_start_of_line({
        let mut items = PrintItems::new(context.arena);
        items.push_sc(sc!("of "));
        items.extend(gen_node((&node.right).into_node(), context));
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
        body_node: (&node.body).into_node(),
        use_braces: context.config.for_of_statement_use_braces,
        brace_position: context.config.for_of_statement_brace_position,
        block_padding: context.config.for_of_statement_block_padding,
        single_body_position: Some(context.config.for_of_statement_single_body_position),
        if_chain_branch: None,
        start_header_info: Some((start_header_ln, start_header_lsil)),
        end_header_info: Some(end_header_ln),
      },
      context,
    )
    .generated_node,
  );

  items
}

pub(super) fn gen_if_stmt<'a>(node: &'a IfStatement<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  // an `else if` continues the chain of the if statement it's in, while any other if statement may start one
  let mut chain_end_condition = None;
  let if_chain_branch = context.take_if_chain_branch().or_else(|| {
    let (branch, condition) = IfChainBranch::start_chain(node, context)?;
    chain_end_condition = Some(condition);
    Some(branch)
  });
  let cons = &node.consequent;
  let cons_range = cons.range();
  let result = gen_header_with_conditional_brace_body(
    GenHeaderWithConditionalBraceBodyOptions {
      body_node: cons,
      generated_header: {
        let mut items = PrintItems::new(context.arena);
        items.push_sc(sc!("if"));
        let open_paren_token = context.token_finder.get_previous_token_if_open_paren(&node.test.range());
        if use_space_after_keyword(context.config.if_statement_space_after_if_keyword, open_paren_token, context) {
          items.push_space();
        }
        let test = &node.test;
        items.extend(gen_node_in_parens(
          |context| gen_node(test.into_node(), context),
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
      block_padding: context.config.if_statement_block_padding,
      single_body_position: Some(context.config.if_statement_single_body_position),
      if_chain_branch,
    },
    context,
  );
  let if_stmt_start_ln = LineNumber::new("ifStmtStart");

  items.push_info(if_stmt_start_ln);
  items.extend(result.generated_node);

  if let Some(alt) = &node.alternate {
    let alt_if_chain_branch = if_chain_branch.map(|branch| branch.next());

    items.extend(gen_control_flow_separator(
      context.config.if_statement_next_control_flow_position,
      &cons_range,
      "else",
      Some(if_stmt_start_ln),
      Some(result.close_brace_condition_ref),
      context,
    ));

    // generate the leading comments before the else keyword
    let else_keyword = context
      .token_finder
      .get_first_keyword_after(&cons_range, "else")
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
      if let Some(branch) = alt_if_chain_branch {
        context.store_if_chain_branch(branch);
      }
      items.extend(gen_node(alt.into_node(), context));
      // clear it for when the alternate wasn't generated as an if statement (ex. it's ignored)
      context.take_if_chain_branch();
    } else {
      items.extend(
        gen_conditional_brace_body(
          GenConditionalBraceBodyOptions {
            body_node: alt.into_node(),
            use_braces: context.config.if_statement_use_braces,
            brace_position: context.config.if_statement_brace_position,
            block_padding: context.config.if_statement_block_padding,
            single_body_position: Some(context.config.if_statement_single_body_position),
            if_chain_branch: alt_if_chain_branch,
            start_header_info: Some((start_else_header_ln, start_else_header_lsil)),
            end_header_info: None,
          },
          context,
        )
        .generated_node,
      );
    }
  }

  // resolved after every branch, so all the branches are known when it's looked ahead at
  if let Some(condition) = chain_end_condition {
    items.push_condition(condition);
  }

  items
}

/// A branch of an if-else chain with `useBraces: preferNone`, where every branch gets braces
/// when any of them needs them.
#[derive(Clone, Copy)]
pub(crate) struct IfChainBranch<'a> {
  /// What decides whether each branch needs braces, which is set when the branch is generated.
  brace_requirements: &'a [Cell<Option<BraceRequirement>>],
  /// Whether any branch needs braces, which is resolved after the last branch.
  any_requires_braces_ref: ConditionReference,
  index: usize,
}

impl<'a> IfChainBranch<'a> {
  /// Gets the first branch of the chain the if statement starts, along with the condition
  /// to put after the chain's last branch.
  pub(super) fn start_chain(node: &IfStatement<'a>, context: &mut Context<'a>) -> Option<(Self, Condition<'a>)> {
    if context.config.if_statement_use_braces != UseBraces::PreferNone || node.alternate.is_none() {
      return None;
    }
    let brace_requirements: &'a [Cell<Option<BraceRequirement>>] = context.arena.bump().alloc_slice_fill_with(count_branches(node), |_| Cell::new(None));
    let mut condition = if_true(
      "anyIfChainBranchRequiresBraces",
      context.arena.alloc_resolver(move |condition_context| {
        for requirement in brace_requirements.iter().filter_map(|requirement| requirement.get()) {
          if requirement.is_required(condition_context)? {
            return Some(true);
          }
        }
        Some(false)
      }),
      PrintItems::new(context.arena),
    );
    let branch = Self {
      brace_requirements,
      any_requires_braces_ref: condition.create_reference(),
      index: 0,
    };
    return Some((branch, condition));

    fn count_branches(node: &IfStatement) -> usize {
      let mut count = 1;
      let mut alt = node.alternate.as_ref();
      while let Some(stmt) = alt {
        count += 1;
        alt = match stmt {
          Statement::IfStatement(if_stmt) => if_stmt.alternate.as_ref(),
          _ => None,
        };
      }
      count
    }
  }

  pub(super) fn next(self) -> Self {
    Self { index: self.index + 1, ..self }
  }

  pub(super) fn set_brace_requirement(&self, requirement: BraceRequirement) {
    self.brace_requirements[self.index].set(Some(requirement));
  }

  pub(super) fn any_branch_requires_braces(&self, condition_context: &mut ConditionResolverContext) -> Option<bool> {
    condition_context.resolved_condition(&self.any_requires_braces_ref)
  }
}

pub(super) fn gen_labeled_stmt<'a>(node: &'a LabeledStatement<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  items.extend(gen_node((&node.label).into_node(), context));
  items.push_sc(sc!(":"));

  // not bothering to make this configurable, because who uses labeled statements?
  // edit: ok, maybe I will make this configurable in the future
  let is_inner_stmt_same_line = node.start_line_fast(context.program) == node.body.start_line_fast(context.program);
  if is_inner_stmt_same_line {
    items.push_space();
  } else {
    items.push_signal(Signal::NewLine);
  }

  items.extend(gen_node((&node.body).into_node(), context));

  items
}

pub(super) fn gen_return_stmt<'a>(node: &'a ReturnStatement<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  items.push_sc(sc!("return"));
  if let Some(arg) = &node.argument {
    items.push_space();
    items.extend(gen_node(arg.into_node(), context));
  }
  if context.config.semicolons.is_true() {
    items.push_sc(sc!(";"));
  }
  items
}

pub(super) fn gen_switch_stmt<'a>(node: &'a SwitchStatement<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let start_header_lsil = LineStartIndentLevel::new("startHeader");
  let mut items = PrintItems::new(context.arena);
  items.push_info(start_header_lsil);
  items.push_sc(sc!("switch"));
  let open_paren_token = context.token_finder.get_previous_token_if_open_paren(&node.discriminant.range());
  if use_space_after_keyword(context.config.switch_statement_space_after_switch_keyword, open_paren_token, context) {
    items.push_space();
  }
  items.extend(gen_node_in_parens(
    |context| gen_node((&node.discriminant).into_node(), context),
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
      node: node.into_node(),
      members: node.cases.iter().map(|x| x.into_node()).collect(),
      start_header_lsil: Some(start_header_lsil),
      brace_position: context.config.switch_statement_brace_position,
      block_padding: context.config.switch_statement_block_padding,
      get_forced_blank_line_count: |previous, next, context| {
        // do not put a blank line when the previous case has no body
        if let Node::SwitchCase(previous) = previous
          && previous.consequent.is_empty()
        {
          return Some(0);
        }

        // Switch cases have custom rules for where the comments end up (based on their indentation),
        // so for here just check that there is no blank line between either the previous comment
        // or previous node
        let leading_comment = next.leading_comments_fast(context.program).next_back().map(|r| r.range());
        let previous_end_line = if let Some(leading_comment) = leading_comment {
          leading_comment.end_line_fast(context.program)
        } else {
          previous.end_line_fast(context.program)
        };
        Some(cap_blank_line_count(
          next.start_line_fast(context.program).saturating_sub(previous_end_line + 1),
          context,
        ))
      },
      separator: Separator::none(),
      keep_blank_lines_before_close_token_comments: true,
    },
    context,
  ));
  items
}

pub(super) fn gen_switch_case<'a>(node: &'a SwitchCase<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let block_stmt_body = get_block_stmt_body(node);
  let mut items = PrintItems::new(context.arena);
  let colon_token = context
    .token_finder
    .get_first_colon_token_after(&if let Some(test) = &node.test { test.end() } else { node.start() })
    .expect("Expected to find a colon token.");

  if let Some(test) = &node.test {
    items.push_sc(sc!("case "));
    items.extend(gen_node(test.into_node(), context));
    items.push_sc(sc!(":"));
  } else {
    items.push_sc(sc!("default:"));
  }

  items.extend(gen_trailing_comments_same_line(&colon_token.range(), context));
  // get the comments on the same line as the end of the case first so they're not
  // generated after the comments on the following lines (ex. `b(); // comment`)
  let generated_same_line_trailing_comments = gen_trailing_comments(&node.range(), context);
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
      items.extend(gen_node(node.consequent.first().unwrap().into_node(), context));
    } else {
      items.push_signal(Signal::NewLine);
      items.extend(ir_helpers::with_indent(gen_statements(
        SourceRange::new(colon_token.end(), node.end()),
        node.consequent.iter().map(|node| node.into_node()).collect(),
        context,
      )));
    }
  }

  items.extend(generated_same_line_trailing_comments);
  items.extend(generated_trailing_comments);

  return items;

  fn get_block_stmt_body(node: &SwitchCase) -> Option<SourceRange> {
    if node.consequent.len() == 1
      && let Some(Statement::BlockStatement(block_stmt)) = node.consequent.first()
    {
      return Some(block_stmt.range());
    }
    None
  }

  fn gen_trailing_comments_for_case<'a>(node: &'a SwitchCase<'a>, block_stmt_body: &Option<SourceRange>, context: &mut Context<'a>) -> PrintItems<'a> {
    let node_range = node.range();
    let mut items = PrintItems::new(context.arena);
    // generate the trailing comments as statements
    let trailing_comments = get_trailing_comments_as_statements(&node_range, context);
    if !trailing_comments.is_empty() {
      let last_case = match node.into_node().parent(context.program) {
        Some(Node::SwitchStatement(switch_stmt)) => switch_stmt.cases.last(),
        _ => None,
      };
      let is_last_case = match last_case {
        Some(last_case) => last_case.start() == node_range.start,
        _ => false,
      };
      let mut is_equal_indent = block_stmt_body.is_some();
      // start after the comments on the case's last line since one of them may be a
      // block comment that spans multiple lines (ex. `b(); /* a\n b */`)
      let node_end_line = node_range.end_line_fast(context.program);
      let mut last_range = node_range
        .trailing_comments_fast(context.program)
        .take_while(|c| c.start_line_fast(context.program) == node_end_line)
        .last()
        .map(|c| c.range())
        .unwrap_or(node_range);
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

pub(super) fn gen_throw_stmt<'a>(node: &'a ThrowStatement<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  items.push_sc(sc!("throw "));
  items.extend(gen_node((&node.argument).into_node(), context));
  if context.config.semicolons.is_true() {
    items.push_sc(sc!(";"));
  }
  items
}

pub(super) fn gen_try_stmt<'a>(node: &'a TryStatement<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  let brace_position = context.config.try_statement_brace_position;
  let next_control_flow_position = context.config.try_statement_next_control_flow_position;
  let mut last_block_range = node.block.range();
  let mut last_block_start_ln = LineNumber::new("tryStart");

  items.push_info(last_block_start_ln);
  items.push_sc(sc!("try"));

  items.extend(
    gen_conditional_brace_body(
      GenConditionalBraceBodyOptions {
        body_node: (&node.block).into_node(),
        use_braces: UseBraces::Always, // braces required
        brace_position: context.config.try_statement_brace_position,
        block_padding: context.config.try_statement_block_padding,
        single_body_position: Some(SameOrNextLinePosition::NextLine),
        if_chain_branch: None,
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
    items.extend(gen_node(handler.into_node(), context));

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
          body_node: finalizer.into_node(),
          use_braces: UseBraces::Always, // braces required
          brace_position,
          block_padding: context.config.try_statement_block_padding,
          single_body_position: Some(SameOrNextLinePosition::NextLine),
          if_chain_branch: None,
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

/// Generates a variable declaration, which includes `using` and `await using`
/// declarations (swc had those as a separate node).
pub(super) fn gen_var_decl<'a>(node: &'a VariableDeclaration<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  if node.declare {
    items.push_sc(sc!("declare "));
  }
  match node.kind {
    VariableDeclarationKind::Const => items.push_sc(sc!("const ")),
    VariableDeclarationKind::Let => items.push_sc(sc!("let ")),
    VariableDeclarationKind::Var => items.push_sc(sc!("var ")),
    VariableDeclarationKind::Using => items.push_sc(sc!("using ")),
    VariableDeclarationKind::AwaitUsing => {
      items.push_sc(sc!("await "));
      items.push_sc(sc!("using "));
    }
  }

  items.extend(gen_var_declarators(node.into_node(), &node.declarations, context));

  if requires_semi_colon_for_var_or_using_decl(node.into_node(), context) {
    items.push_sc(sc!(";"));
  }

  items
}

pub(super) fn requires_semi_colon_for_var_or_using_decl<'a>(node: Node<'a>, context: &mut Context<'a>) -> bool {
  let use_semicolons = context.config.semicolons.is_true();
  use_semicolons
    && match node.parent(context.program) {
      Some(Node::ForInStatement(node)) => node.start() >= node.body.start(),
      Some(Node::ForOfStatement(node)) => node.start() >= node.body.start(),
      Some(Node::ForStatement(node)) => node.start() >= node.body.start(),
      _ => use_semicolons,
    }
}

pub(super) fn gen_var_declarators<'a>(parent: Node<'a>, decls: &'a [VariableDeclarator<'a>], context: &mut Context<'a>) -> PrintItems<'a> {
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
    gen_node((&decls[0]).into_node(), context)
  } else if decls_len > 1 {
    let force_use_new_lines = get_use_new_lines(parent, decls, context);
    gen_separated_values(
      GenSeparatedValuesParams {
        nodes: decls.iter().map(|p| NodeOrSeparator::Node(p.into_node())).collect(),
        prefer_hanging: context.config.variable_statement_prefer_hanging,
        force_use_new_lines,
        blank_lines: ListBlankLines::Remove,
        separator: TrailingCommas::Never.into(),
        single_line_options: ir_helpers::SingleLineOptions::same_line_maybe_space_separated(context.arena),
        multi_line_options: ir_helpers::MultiLineOptions::same_line_start_hanging_indent(),
        force_possible_newline_at_start: false,
        node_sorter: None,
      },
      context,
    )
  } else {
    PrintItems::new(context.arena)
  }
}

pub(super) fn gen_var_declarator<'a>(node: &'a VariableDeclarator<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);

  items.extend(gen_node((&node.id).into_node(), context));

  // the definite assignment assertion and type annotation are on the declarator in oxc (ex. `a!: string`)
  if node.definite {
    items.push_sc(sc!("!"));
  }
  items.extend(gen_type_ann_with_colon_if_exists(node.type_annotation.as_deref(), context));

  if let Some(init) = &node.init {
    items.extend(gen_assignment(init.into_node(), sc!("="), context));
  }

  // Indent the first variable declarator when there are multiple.
  // Not ideal, but doing this here because of the abstraction used in
  // `gen_var_decl`. In the future this should probably be moved away.

  let parent_decls = match node.into_node().parent(context.program) {
    Some(Node::VariableDeclaration(parent)) => Some(&parent.declarations),
    _ => None,
  };
  if let Some(var_decls) = parent_decls {
    if var_decls.len() > 1 && var_decls[0].range() == node.range() {
      let items = items.into_rc_path();
      if_true_or(
        "indentIfNotStartOfLine",
        condition_resolvers::is_not_start_of_line(),
        with_indent(items.into_print_items(context.arena)),
        items.into_print_items(context.arena),
      )
      .into_print_items(context.arena)
    } else {
      items
    }
  } else {
    items
  }
}

pub(super) fn gen_while_stmt<'a>(node: &'a WhileStatement<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let start_header_ln = LineNumber::new("startHeader");
  let start_header_lsil = LineStartIndentLevel::new("startHeader");
  let end_header_ln = LineNumber::new("endHeader");
  let mut items = PrintItems::new(context.arena);
  items.push_info(start_header_ln);
  items.push_info(start_header_lsil);
  items.push_sc(sc!("while"));
  let open_paren_token = context.token_finder.get_previous_token_if_open_paren(&node.test.range());
  if use_space_after_keyword(context.config.while_statement_space_after_while_keyword, open_paren_token, context) {
    items.push_space();
  }
  items.extend(gen_node_in_parens(
    |context| gen_node((&node.test).into_node(), context),
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
        body_node: (&node.body).into_node(),
        use_braces: context.config.while_statement_use_braces,
        brace_position: context.config.while_statement_brace_position,
        block_padding: context.config.while_statement_block_padding,
        single_body_position: Some(context.config.while_statement_single_body_position),
        if_chain_branch: None,
        start_header_info: Some((start_header_ln, start_header_lsil)),
        end_header_info: Some(end_header_ln),
      },
      context,
    )
    .generated_node,
  );
  items
}

pub(super) fn gen_with_stmt<'a>(node: &'a WithStatement<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  // `with` statements are deprecated and forbidden in strict mode, but still valid syntax
  let start_header_ln = LineNumber::new("startHeader");
  let start_header_lsil = LineStartIndentLevel::new("startHeader");
  let end_header_ln = LineNumber::new("endHeader");
  let mut items = PrintItems::new(context.arena);
  items.push_info(start_header_ln);
  items.push_info(start_header_lsil);
  items.push_sc(sc!("with"));
  let open_paren_token = context.token_finder.get_previous_token_if_open_paren(&node.object.range());
  if use_space_after_keyword(context.config.with_statement_space_after_with_keyword, open_paren_token, context) {
    items.push_space();
  }
  items.extend(gen_node_in_parens(
    |context| gen_node((&node.object).into_node(), context),
    GenNodeInParensOptions {
      inner_range: node.object.range(),
      prefer_hanging: context.config.with_statement_prefer_hanging,
      allow_open_paren_trailing_comments: false,
      single_line_space_around: context.config.with_statement_space_around,
    },
    context,
  ));
  items.push_info(end_header_ln);
  items.extend(
    gen_conditional_brace_body(
      GenConditionalBraceBodyOptions {
        body_node: (&node.body).into_node(),
        use_braces: context.config.with_statement_use_braces,
        brace_position: context.config.with_statement_brace_position,
        block_padding: context.config.with_statement_block_padding,
        single_body_position: Some(context.config.with_statement_single_body_position),
        if_chain_branch: None,
        start_header_info: Some((start_header_ln, start_header_lsil)),
        end_header_info: Some(end_header_ln),
      },
      context,
    )
    .generated_node,
  );
  items
}
