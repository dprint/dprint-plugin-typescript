use super::*;

pub(super) fn gen_for_member_like_expr_item<'a>(item: &MemberLikeExprItem<'a>, context: &mut Context<'a>, index: usize, item_count: usize) -> PrintItems<'a> {
  let is_first = index == 0;
  let is_last = index == item_count - 1;
  match item {
    MemberLikeExprItem::Node(node) => {
      let is_optional = item.is_optional(context.program);
      gen_node_with_inner_gen(*node, context, |node_items, context| {
        // Changing `2 .toString()` to `2.toString()` is a syntax error because `2.` is a numeric
        // literal, so we surround it in parenthesis `(2).toString()`
        let is_first_and_number = is_first && node.kind() == NodeKind::NumericLiteral && !node.text_fast(context.program).contains('.');
        let mut items = PrintItems::new(context.arena);
        if is_first_and_number {
          items.push_sc(sc!("("));
        }
        if !is_first {
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
    MemberLikeExprItem::Computed(computed) => {
      // the brackets aren't a node, so generate their comments the same way gen_node does for a node
      let member: &'a ComputedMemberExpression<'a> = computed.member;
      let range = computed.range;
      let is_optional = item.is_optional(context.program);
      let leading_comments = context.comments.leading_comments_with_previous(range.start);
      let has_ignore_comment = leading_comments
        .clone()
        .any(|c| ir_helpers::text_has_dprint_ignore(c.text, &context.config.ignore_node_comment_text));
      let mut items = gen_comments_as_leading(&range, leading_comments, context);
      if has_ignore_comment {
        items.push_force_current_line_indentation();
      }
      if is_optional {
        items.push_sc(sc!("?."));
      }
      if has_ignore_comment {
        items.extend(gen_file_text_as_is(range.text_fast(context.program), range.end, TrimLineEnds::No, context));
      } else {
        items.extend(gen_computed_prop_like(
          |context| gen_node((&member.expression).into_node(), context),
          GenComputedPropLikeOptions {
            inner_node_range: member.expression.range(),
          },
          context,
        ));
      }
      // the trailing comments are left to the parent when it ends at the same position
      if range.end != context.current_node.end() {
        let trailing_comments = context.comments.trailing_comments_with_previous(range.end);
        items.extend(gen_comments_as_trailing(&range, trailing_comments, context));
      }
      items
    }
    MemberLikeExprItem::Token(token) => {
      // don't bother with intertwined comments as its too much trouble
      let mut items = PrintItems::new(context.arena);
      if !is_first {
        items.push_sc(sc!("."));
      }
      items.push_str_runtime_width_computed(token.text_fast(context.program));
      items
    }
    MemberLikeExprItem::CallExpr(node) => {
      let mut items = PrintItems::new(context.arena);

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

pub(super) fn gen_for_flattened_member_like_expr<'a>(node: FlattenedMemberLikeExpr<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  let member_expr_start_ln = LineNumber::new("memberExprStart");
  let member_expr_start_cn = ColumnNumber::new("memberExprStartColumn");
  let member_expr_last_item_start_ln = LineNumber::new("memberExprStartLastItem");
  let total_items_len = node.nodes.len();
  let last_item_line = if context.config.member_expression_line_per_expression && total_items_len > 1 {
    Some(LastItemLine::new(member_expr_last_item_start_ln, context.arena))
  } else {
    None
  };

  if total_items_len > 1 {
    items.push_info(member_expr_start_ln);
    if let Some(last_item_line) = last_item_line {
      items.push_info(member_expr_start_cn);
      items.extend(last_item_line.gen_start(context.arena));
    }
  }

  items.extend(gen_for_member_like_expr_item(&node.nodes[0], context, 0, total_items_len));

  // the first condition might not be printed when the first item is short, so check the first two
  let mut multi_line_reevaluations = Vec::with_capacity(2);
  for (i, item) in node.nodes.iter().enumerate().skip(1) {
    let force_use_new_line =
      !context.config.member_expression_prefer_single_line && node_helpers::get_use_new_lines_for_nodes(&node.nodes[i - 1], &node.nodes[i], context.program);
    if item.is_optional(context.program) || !item.is_computed() {
      if force_use_new_line {
        items.push_signal(Signal::NewLine);
      } else if !context.config.member_expression_line_per_expression {
        items.push_condition(conditions::if_above_width(
          context.config.indent_width,
          Signal::PossibleNewLine.into_print_items(context.arena),
        ));
      } else {
        let mut condition = if_true_or(
          "isMultipleLines",
          context
            .arena
            .alloc_resolver(move |context| condition_helpers::is_multiple_lines(context, member_expr_start_ln, member_expr_last_item_start_ln)),
          Signal::NewLine.into_print_items(context.arena),
          Signal::PossibleNewLine.into_print_items(context.arena),
        );
        if multi_line_reevaluations.len() < 2 {
          multi_line_reevaluations.push(condition.create_reevaluation());
        }
        if i == 1 {
          // keep the second item on the first line when the first item isn't wider than
          // the indent, because breaking after it wouldn't make the lines any shorter
          let indent_width = context.config.indent_width as u32;
          items.push_condition(if_false(
            "isFirstItemNarrowerThanIndent",
            context.arena.alloc_resolver(move |context| {
              let start_line = context.resolved_line_number(member_expr_start_ln)?;
              let start_column = context.resolved_column_number(member_expr_start_cn)?;
              let writer_info = &context.writer_info;
              Some(writer_info.line_number == start_line && writer_info.column_number - start_column <= indent_width)
            }),
            condition.into_print_items(context.arena),
          ));
        } else {
          items.push_condition(condition);
        }
      }
    }

    let is_last_item = i == total_items_len - 1;
    if is_last_item {
      // store this right before the last right expression
      items.push_info(member_expr_last_item_start_ln);
      if let Some(last_item_line) = last_item_line {
        items.extend(last_item_line.gen_resolved(context.arena));
      }
      // when only the last item exceeds the line width, it moves to the next line after the
      // conditions were resolved, so check again to put every item on its own line
      for reevaluation in multi_line_reevaluations.drain(..) {
        items.push_reevaluation(reevaluation);
      }
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

pub(super) struct GenComputedPropLikeOptions {
  pub(super) inner_node_range: SourceRange,
}

pub(super) fn gen_computed_prop_like<'a>(
  gen_inner: impl FnOnce(&mut Context<'a>) -> PrintItems<'a>,
  opts: GenComputedPropLikeOptions,
  context: &mut Context<'a>,
) -> PrintItems<'a> {
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
      comments_only_padding: BlockPaddingBlankLines::default(),
      keep_blank_lines_before_close_token_comments: false,
    },
    context,
  ));

  fn get_bracket_range<'a>(node: &SourceRange, context: &mut Context<'a>) -> Option<SourceRange> {
    let open_bracket = context.token_finder.get_previous_token_if_open_bracket(node);
    let close_bracket = context.token_finder.get_next_token_if_close_bracket(node);
    if let Some(open_bracket) = open_bracket
      && let Some(close_bracket) = close_bracket
    {
      return Some(SourceRange::new(open_bracket.start(), close_bracket.end()));
    }

    if cfg!(debug_assertions) {
      panic!("Debug panic! Could not find open and/or close bracket token.");
    } else {
      None
    }
  }
}

/// Generates the decorators of a class or class member, which is on the same or next line
/// depending on `decorators.position`.
pub(super) fn gen_decl_decorators<'a>(decorators: &'a [Decorator<'a>], context: &mut Context<'a>) -> PrintItems<'a> {
  let Some(last_decorator) = decorators.last() else {
    return PrintItems::new(context.arena);
  };
  let program = context.program;
  // a line comment after the decorators ends the line
  let has_line_comment = last_decorator.end().trailing_comments_fast(program).any(|c| c.kind == CommentKind::Line);
  let is_same_line = !has_line_comment
    && match context.config.decorators_position {
      SameOrNextLinePosition::SameLine => true,
      SameOrNextLinePosition::NextLine => false,
      SameOrNextLinePosition::Maintain => {
        let next_token_pos = context.token_finder.get_next_token_pos_after(last_decorator);
        last_decorator.end_line_fast(program) == next_token_pos.start_line_fast(program)
      }
    };
  let separator = if is_same_line {
    DecoratorsSeparator::SpaceOrNewLine
  } else {
    DecoratorsSeparator::NewLine
  };
  gen_decorators(decorators, separator, context)
}

/// Gets if the decorators start before the position (ex. before `export` in `@dec export class A {}`).
pub(super) fn are_decorators_before(decorators: &[Decorator], pos: SourcePos) -> bool {
  decorators.first().is_some_and(|decorator| decorator.start() < pos)
}

/// What goes between decorators and what they decorate.
#[derive(Clone, Copy, PartialEq)]
pub(super) enum DecoratorsSeparator {
  Space,
  SpaceOrNewLine,
  NewLine,
}

pub(super) fn gen_decorators<'a>(decorators: &'a [Decorator<'a>], separator: DecoratorsSeparator, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  if decorators.is_empty() {
    return items;
  }

  let force_use_new_lines = !context.config.decorators_prefer_single_line
    && decorators.len() >= 2
    && node_helpers::get_use_new_lines_for_nodes(&decorators[0], &decorators[1], context.program);

  let separated_values_result = gen_separated_values_with_result(
    GenSeparatedValuesParams {
      nodes: decorators.iter().map(|p| NodeOrSeparator::Node(p.into_node())).collect(),
      prefer_hanging: false, // would need to think about the design because prefer_hanging causes a hanging indent
      force_use_new_lines,
      blank_lines: ListBlankLines::Remove,
      separator: Separator::none(),
      single_line_options: ir_helpers::SingleLineOptions {
        space_at_start: false,
        space_at_end: separator == DecoratorsSeparator::Space,
        separator: Signal::SpaceOrNewLine.into_print_items(context.arena),
      },
      multi_line_options: ir_helpers::MultiLineOptions::same_line_no_indent(),
      force_possible_newline_at_start: false,
      node_sorter: None,
    },
    context,
  );

  items.extend(separated_values_result.items);

  match separator {
    DecoratorsSeparator::Space => {
      let is_multi_line = separated_values_result.is_multi_line_condition_ref.create_resolver(context.arena);
      items.push_condition(if_true("inlineMultiLineSpace", is_multi_line, Signal::NewLine.into_print_items(context.arena)));
    }
    DecoratorsSeparator::SpaceOrNewLine => {
      let is_multi_line = separated_values_result.is_multi_line_condition_ref.create_resolver(context.arena);
      items.push_condition(if_true_or(
        "inlineMultiLineSpaceOrNewLine",
        is_multi_line,
        Signal::NewLine.into_print_items(context.arena),
        Signal::SpaceOrNewLine.into_print_items(context.arena),
      ));
    }
    DecoratorsSeparator::NewLine => items.push_signal(Signal::NewLine),
  }

  // generate the comments between the last decorator and the next token
  if let Some(last_dec) = decorators.last() {
    let next_token_pos = context.token_finder.get_next_token_pos_after(last_dec);
    items.extend(gen_leading_comments(&next_token_pos.range(), context));
  }

  items
}

pub(super) fn gen_control_flow_separator<'a>(
  next_control_flow_position: NextControlFlowPosition,
  previous_node_block: &SourceRange,
  token_text: &str,
  previous_start_ln: Option<LineNumber>,
  previous_close_brace_condition_ref: Option<ConditionReference>,
  context: &mut Context<'a>,
) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  match next_control_flow_position {
    NextControlFlowPosition::SameLine => {
      items.push_condition(if_true_or(
        "newLineOrSpace",
        context.arena.alloc_resolver(move |condition_context| {
          // newline if on the same line as the previous
          if let Some(previous_start_ln) = previous_start_ln
            && condition_helpers::is_on_same_line(condition_context, previous_start_ln)?
          {
            return Some(true);
          }

          // newline if the previous did not have a close brace
          if let Some(previous_close_brace_condition_ref) = previous_close_brace_condition_ref
            && !condition_context.resolved_condition(&previous_close_brace_condition_ref)?
          {
            return Some(true);
          }

          Some(false)
        }),
        Signal::NewLine.into_print_items(context.arena),
        " ".into_print_items(context.arena),
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

pub(super) struct GenHeaderWithConditionalBraceBodyOptions<'a> {
  pub(super) body_node: &'a Statement<'a>,
  pub(super) generated_header: PrintItems<'a>,
  pub(super) use_braces: UseBraces,
  pub(super) brace_position: BracePosition,
  pub(super) block_padding: BlockPadding,
  pub(super) single_body_position: Option<SameOrNextLinePosition>,
  pub(super) if_chain_branch: Option<IfChainBranch<'a>>,
}

pub(super) struct GenHeaderWithConditionalBraceBodyResult<'a> {
  pub(super) generated_node: PrintItems<'a>,
  pub(super) close_brace_condition_ref: ConditionReference,
}

pub(super) fn gen_header_with_conditional_brace_body<'a>(
  opts: GenHeaderWithConditionalBraceBodyOptions<'a>,
  context: &mut Context<'a>,
) -> GenHeaderWithConditionalBraceBodyResult<'a> {
  let start_header_ln = LineNumber::new("startHeader");
  let start_header_lsil = LineStartIndentLevel::new("startHeader");
  let end_header_ln = LineNumber::new("endHeader");
  let mut items = PrintItems::new(context.arena);

  items.push_info(start_header_ln);
  items.extend(gen_line_numbers_region_start(context.arena, &[end_header_ln]));
  items.push_info(start_header_lsil);
  items.extend(new_line_group(opts.generated_header));
  items.push_info(end_header_ln);
  let result = gen_conditional_brace_body(
    GenConditionalBraceBodyOptions {
      body_node: opts.body_node.into_node(),
      use_braces: if force_use_braces_for_stmt(opts.body_node) {
        UseBraces::Always
      } else {
        opts.use_braces
      },
      brace_position: opts.brace_position,
      block_padding: opts.block_padding,
      single_body_position: opts.single_body_position,
      if_chain_branch: opts.if_chain_branch,
      start_header_info: Some((start_header_ln, start_header_lsil)),
      end_header_info: Some(end_header_ln),
    },
    context,
  );
  items.extend(result.generated_node);

  GenHeaderWithConditionalBraceBodyResult {
    close_brace_condition_ref: result.close_brace_condition_ref,
    generated_node: items,
  }
}

pub(super) fn force_use_braces_for_stmt(stmt: &Statement) -> bool {
  match stmt {
    Statement::BlockStatement(block) => {
      if block.body.len() != 1 {
        true
      } else {
        force_use_braces_for_stmt(&block.body[0])
      }
    }
    // force braces for any children where no braces could be ambiguous
    Statement::EmptyStatement(_)
    | Statement::DoWhileStatement(_)
    | Statement::ForStatement(_)
    | Statement::ForInStatement(_)
    | Statement::ForOfStatement(_)
    | Statement::IfStatement(_) // especially force for this as it may cause a bug
    | Statement::LabeledStatement(_)
    | Statement::SwitchStatement(_)
    | Statement::TryStatement(_)
    | Statement::WhileStatement(_)
    | Statement::WithStatement(_) => true,
    oxc_ast::match_declaration!(Statement) => true,
    // module declarations can't be a body, but are statements in oxc
    oxc_ast::match_module_declaration!(Statement) => true,
    Statement::BreakStatement(_)
    | Statement::ContinueStatement(_)
    | Statement::DebuggerStatement(_)
    | Statement::ExpressionStatement(_)
    | Statement::ReturnStatement(_)
    | Statement::ThrowStatement(_) => false,
  }
}

pub(super) struct GenConditionalBraceBodyOptions<'a> {
  pub(super) body_node: Node<'a>,
  pub(super) use_braces: UseBraces,
  pub(super) brace_position: BracePosition,
  pub(super) block_padding: BlockPadding,
  pub(super) single_body_position: Option<SameOrNextLinePosition>,
  pub(super) if_chain_branch: Option<IfChainBranch<'a>>,
  pub(super) start_header_info: Option<(LineNumber, LineStartIndentLevel)>,
  pub(super) end_header_info: Option<LineNumber>,
}

pub(super) struct GenConditionalBraceBodyResult<'a> {
  pub(super) generated_node: PrintItems<'a>,
  pub(super) close_brace_condition_ref: ConditionReference,
}

/// What decides whether a body needs braces with `useBraces: preferNone`.
#[derive(Clone, Copy)]
pub(super) struct BraceRequirement {
  /// Whether the body needs braces no matter how it's printed.
  pub(super) is_forced: bool,
  /// The start and end of the header, where braces are needed when it's on multiple lines.
  pub(super) header_lns: Option<(LineNumber, LineNumber)>,
  /// The start and end of the statements, where braces are needed when they're on multiple lines.
  pub(super) statements_lns: (LineNumber, LineNumber),
}

impl BraceRequirement {
  pub(super) fn is_required(&self, condition_context: &mut ConditionResolverContext) -> Option<bool> {
    if self.is_forced {
      return Some(true);
    }
    if let Some((start_header_ln, end_header_ln)) = self.header_lns
      && condition_helpers::is_multiple_lines(condition_context, start_header_ln, end_header_ln)?
    {
      return Some(true);
    }
    condition_helpers::is_multiple_lines(condition_context, self.statements_lns.0, self.statements_lns.1)
  }
}

pub(super) fn gen_conditional_brace_body<'a>(opts: GenConditionalBraceBodyOptions<'a>, context: &mut Context<'a>) -> GenConditionalBraceBodyResult<'a> {
  // todo: reorganize...
  let start_lc = LineAndColumn::new("start");
  let end_ln = LineNumber::new("end");
  let start_header_ln = opts.start_header_info.map(|v| v.0);
  let start_header_lsil = opts.start_header_info.map(|v| v.1);
  let end_header_ln = opts.end_header_info;
  let if_chain_branch = opts.if_chain_branch;
  let start_inner_text_lc = LineAndColumn::new("startInnerText");
  let start_statements_lc = LineAndColumn::new("startStatements");
  let end_statements_lc = LineAndColumn::new("endStatements");
  let header_trailing_comments = get_header_trailing_comments(opts.body_node, context);
  let body_should_be_multi_line = get_body_should_be_multi_line(opts.body_node, &header_trailing_comments, context);
  let should_use_new_line = get_should_use_new_line(opts.body_node, body_should_be_multi_line, &opts.single_body_position, context);
  let open_brace_token = get_open_brace_token(opts.body_node, context);
  let use_braces = opts.use_braces;
  let is_body_empty_stmt = opts.body_node.kind() == NodeKind::EmptyStatement;
  let mut inner_brace_space_condition = if_true(
    "spaceCondition",
    context.arena.alloc_resolver(move |condition_context| {
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
    Signal::SpaceOrNewLine.into_print_items(context.arena),
  );
  let inner_brace_space_condition_ref = inner_brace_space_condition.create_reference();
  let force_braces = get_force_braces(opts.body_node);
  let brace_requirement = BraceRequirement {
    is_forced: use_braces == UseBraces::Always || force_braces || body_should_be_multi_line,
    header_lns: start_header_ln.zip(end_header_ln),
    statements_lns: (start_statements_lc.line, end_statements_lc.line),
  };
  if let Some(if_chain_branch) = if_chain_branch
    && !is_body_empty_stmt
  {
    if_chain_branch.set_brace_requirement(brace_requirement);
  }
  let mut open_brace_condition = if_true(
    "openBrace",
    {
      let has_open_brace_token = open_brace_token.is_some();
      context.arena.alloc_resolver(move |condition_context| {
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
            if brace_requirement.is_required(condition_context)? {
              return Some(true);
            }
            match if_chain_branch {
              Some(if_chain_branch) => if_chain_branch.any_branch_requires_braces(condition_context),
              None => Some(false),
            }
          }
        }
      })
    },
    {
      let mut items = PrintItems::new(context.arena);
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
  // whether the body prints anything, where a block with only empty statements doesn't
  let body_has_content = match opts.body_node {
    Node::BlockStatement(block) => {
      block.body.iter().any(|stmt| !matches!(stmt, Statement::EmptyStatement(_)))
        || block.get_inner_range(context).start().trailing_comments_fast(context.program).next().is_some()
    }
    _ => !is_body_empty_stmt,
  };
  let mut newline_condition = if_true(
    "newLineCondition",
    context.arena.alloc_resolver(move |condition_context| {
      if is_body_empty_stmt {
        return Some(false);
      }

      if should_use_new_line {
        return Some(true);
      }
      // A body with braces needs a space or newline after the open brace. The space was resolved
      // earlier, so when it isn't there the statements go on the next line. Otherwise, after going
      // back to an earlier point, this could be resolved from where the statements ended before and
      // the statements would be right after the open brace (ex. `{if (a) {`).
      if body_has_content
        && condition_context.resolved_condition(&open_brace_condition_ref) == Some(true)
        && condition_context.resolved_condition(&inner_brace_space_condition_ref) == Some(false)
      {
        return Some(true);
      }
      let end_header_ln = condition_context.resolved_line_number(end_header_ln?)?;
      if end_header_ln < condition_context.writer_info.line_number {
        return Some(true);
      }
      let resolved_end_statements_ln = condition_context.resolved_line_number(end_statements_lc.line)?;
      Some(resolved_end_statements_ln > end_header_ln)
    }),
    Signal::NewLine.into_print_items(context.arena),
  );
  let newline_condition_ref = newline_condition.create_reference();

  // store the brace condition if ASI and the body is an expression statement
  if context.config.semicolons == Semicolons::Asi && node_helpers::is_expr_stmt_or_body_with_single_expr_stmt(opts.body_node) {
    context.store_expr_stmt_single_line_parent_brace_ref(open_brace_condition_ref);
  }

  // generate body
  let mut items = PrintItems::new(context.arena);
  items.push_line_and_column(start_lc);
  items.extend(gen_line_numbers_region_start(context.arena, &[end_ln]));
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
  // The padding is outside the statements so it doesn't affect whether they're multi-line.
  // It's only added when the braces can't be removed, as removing them would otherwise move
  // the statements to a different line after they're printed.
  let can_remove_braces = use_braces == UseBraces::PreferNone && !force_braces && !body_should_be_multi_line;
  let padding = match opts.body_node {
    _ if can_remove_braces => BlockPaddingBlankLines::default(),
    Node::BlockStatement(body_node) => get_block_padding_blank_lines(
      SourceRange::new(body_node.start(), body_node.start() + 1),
      SourceRange::new(body_node.end() - 1, body_node.end()),
      opts.block_padding,
      context,
    ),
    // braces may be added to the body, so pad it the same as when it has them
    _ if opts.block_padding == BlockPadding::Always && !is_body_empty_stmt => BlockPaddingBlankLines { start: 1, end: 1 },
    _ => BlockPaddingBlankLines::default(),
  };
  let gen_padding = |count| {
    gen_conditional_body_padding(
      context.arena,
      count,
      GenConditionalBodyPaddingConditions {
        open_brace_condition_ref,
        newline_condition_ref,
        start_statements_lc,
        end_statements_lc,
      },
    )
  };
  items.extend(gen_padding(padding.start));
  items.push_line_and_column(start_statements_lc);
  if !is_body_empty_stmt {
    items.push_condition(if_true(
      "spaceIfAtStart",
      context
        .arena
        .alloc_resolver(move |context| condition_helpers::is_at_same_position(context, start_lc)),
      Signal::SpaceOrNewLine.into_print_items(context.arena),
    ));
  }

  if let Node::BlockStatement(body_node) = opts.body_node {
    items.extend(ir_helpers::with_indent({
      let mut items = PrintItems::new(context.arena);
      // generate the remaining trailing comments inside because some of them are generated already
      // by parsing the header trailing comments
      items.extend(gen_leading_comments(&body_node.range(), context));
      let inner_range = body_node.get_inner_range(context);
      if body_node.body.is_empty() {
        let trailing_comments_same_line = get_trailing_comments_on_same_line(&inner_range.start().range(), context);
        items.extend(gen_comments_same_line(trailing_comments_same_line, context));
        // treat the remaining unhandled comments as statements
        items.extend(gen_comments_as_statements(
          inner_range.start().trailing_comments_fast(context.program),
          None,
          context,
        ));
      } else {
        items.extend(gen_statements(inner_range, body_node.body.iter().map(|x| x.into_node()).collect(), context));
      }
      items
    }));
  } else {
    let mut body_items = PrintItems::new(context.arena);
    let body_node_range = opts.body_node.range();
    body_items.extend(gen_node(opts.body_node, context));
    body_items.extend(gen_trailing_comments(&body_node_range, context));
    // When braces might be added, the body is first printed after the header and then again
    // on the next line. A new line group prefers putting the body on the next line instead of
    // breaking it after the header, which would otherwise stay broken when printed again.
    let can_be_on_next_line = opts.single_body_position != Some(SameOrNextLinePosition::SameLine);
    if use_braces != UseBraces::Always && can_be_on_next_line {
      body_items = new_line_group(body_items);
    }
    items.extend(ir_helpers::with_indent(body_items));
  }

  items.push_line_and_column(end_statements_lc);
  items.extend(gen_padding(padding.end));
  let mut close_brace_condition = if_true(
    "closeBrace",
    context
      .arena
      .alloc_resolver(move |condition_context| condition_context.resolved_condition(&open_brace_condition_ref)),
    {
      let mut items = PrintItems::new(context.arena);
      items.push_condition(if_true_or(
        "closeBraceNewLine",
        context.arena.alloc_resolver(move |condition_context| {
          let is_new_line = condition_context.resolved_condition(&newline_condition_ref)?;
          if !is_new_line {
            return Some(false);
          }
          let has_statement_text = condition_helpers::are_line_and_columns_not_equal(condition_context, start_statements_lc, end_statements_lc)?;
          Some(has_statement_text)
        }),
        Signal::NewLine.into_print_items(context.arena),
        if_true(
          "closeBraceSpace",
          context.arena.alloc_resolver(move |condition_context| {
            if condition_helpers::is_at_same_position(condition_context, start_inner_text_lc)? {
              return Some(false);
            }
            let had_space = condition_context.resolved_condition(&inner_brace_space_condition_ref)?;
            Some(had_space)
          }),
          " ".into_print_items(context.arena),
        )
        .into_print_items(context.arena),
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
        SameOrNextLinePosition::Maintain => get_body_stmt_start_line(body_node, context) > get_previous_token_start_line(body_node, context),
        SameOrNextLinePosition::NextLine => true,
        SameOrNextLinePosition::SameLine => {
          if let Node::BlockStatement(block_stmt) = body_node {
            if block_stmt.body.len() != 1 {
              return true;
            }
            return get_body_stmt_start_line(body_node, context) > get_previous_token_start_line(body_node, context);
          }
          return false;
        }
      };
    } else {
      if let Node::BlockStatement(block_stmt) = body_node
        && block_stmt.body.is_empty()
      {
        // keep the block on the same line
        return block_stmt.start_line_fast(context.program) < block_stmt.end_line_fast(context.program);
      }
      return true;
    }

    fn get_body_stmt_start_line<'a>(body_node: Node, context: &mut Context<'a>) -> usize {
      if let Node::BlockStatement(body_node) = body_node
        && let Some(first_stmt) = body_node.body.first()
      {
        return first_stmt.start_line_fast(context.program);
      }
      body_node.start_line_fast(context.program)
    }

    fn get_previous_token_start_line<'a>(body_node: Node, context: &mut Context<'a>) -> usize {
      let program = context.program;
      // the body always has a token before it in its header
      body_node.previous_token_fast(program).map(|token| token.start_line_fast(program)).unwrap_or(0)
    }
  }

  fn get_body_should_be_multi_line<'a>(body_node: Node<'a>, header_trailing_comments: &[&'a Comment<'a>], context: &mut Context<'a>) -> bool {
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

    fn has_leading_comment_on_different_line<'a>(node: &SourceRange, header_trailing_comments: &[&'a Comment<'a>], program: ProgramInfo<'a>) -> bool {
      node_helpers::has_leading_comment_on_different_line(node, /* comments to ignore */ Some(header_trailing_comments), program)
    }
  }

  fn get_force_braces(body_node: Node) -> bool {
    if let Node::BlockStatement(body_node) = body_node {
      body_node.body.is_empty()
        || body_node.body.iter().all(|s| matches!(s, Statement::EmptyStatement(_)))
        || (body_node.body.len() == 1 && is_declaration(&body_node.body[0]))
    } else {
      false
    }
  }

  fn is_declaration(stmt: &Statement) -> bool {
    matches!(stmt, oxc_ast::match_declaration!(Statement))
  }

  fn get_header_trailing_comments<'a>(body_node: Node<'a>, context: &mut Context<'a>) -> Vec<&'a Comment<'a>> {
    let mut comments = Vec::new();
    if let Node::BlockStatement(block_stmt) = body_node {
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
    if let Node::BlockStatement(block_stmt) = body_node {
      context.token_finder.get_first_open_brace_token_within(block_stmt)
    } else {
      None
    }
  }
}

pub(super) struct GenJsxWithOpeningAndClosingOptions<'a> {
  pub(super) opening_element: Node<'a>,
  pub(super) closing_element: Node<'a>,
  pub(super) children: Vec<Node<'a>>,
}

pub(super) struct GenJsxWithOpeningAndClosingResult<'a> {
  pub(super) items: PrintItems<'a>,
  pub(super) start_ln: LineNumber,
  pub(super) end_ln: LineNumber,
}

pub(super) fn gen_jsx_with_opening_and_closing<'a>(
  opts: GenJsxWithOpeningAndClosingOptions<'a>,
  context: &mut Context<'a>,
) -> GenJsxWithOpeningAndClosingResult<'a> {
  let force_use_multi_lines = get_force_use_multi_lines(opts.opening_element, &opts.children, context);
  let start_lc = LineAndColumn::new("start");
  let end_ln = LineNumber::new("end");
  let mut items = PrintItems::new(context.arena);
  let inner_range = SourceRange::new(opts.opening_element.end(), opts.closing_element.start());

  items.extend(gen_line_numbers_region_start(context.arena, &[end_ln]));
  items.push_line_and_column(start_lc);
  items.extend(gen_node(opts.opening_element, context));
  let children_result = gen_jsx_children(
    GenJsxChildrenOptions {
      inner_range,
      children: opts.children,
      parent_start_ln: start_lc.line,
      parent_end_ln: end_ln,
      force_use_multi_lines,
    },
    context,
  );
  items.extend(children_result.items);
  items.extend(gen_node(opts.closing_element, context));
  items.push_info(end_ln);
  if let Some(reevaluation) = children_result.new_lines_reevaluation {
    // the end line changes when the line breaks after the element, which
    // happens after the condition was evaluated with the end line on the first line
    items.push_reevaluation(reevaluation);
  }

  return GenJsxWithOpeningAndClosingResult {
    items,
    start_ln: start_lc.line,
    end_ln,
  };

  fn get_force_use_multi_lines<'a>(opening_element: Node, children: &[Node], context: &mut Context<'a>) -> bool {
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
      if let Node::JSXText(first_child) = first_child
        && first_child.text_fast(context.program).find('\n').is_some()
      {
        return true;
      }

      node_helpers::get_use_new_lines_for_nodes(&opening_element, first_child, context.program)
    } else {
      false
    }
  }
}

pub(super) struct GenJsxChildrenOptions<'a> {
  pub(super) inner_range: SourceRange,
  pub(super) children: Vec<Node<'a>>,
  pub(super) parent_start_ln: LineNumber,
  pub(super) parent_end_ln: LineNumber,
  pub(super) force_use_multi_lines: bool,
}

pub(super) struct GenJsxChildrenResult<'a> {
  pub(super) items: PrintItems<'a>,
  pub(super) new_lines_reevaluation: Option<ConditionReevaluation>,
}

/// The spaces JSX keeps between the children and the opening and closing tags.
#[derive(Clone, Copy)]
pub(super) struct JsxEdgeSpaces {
  pub(super) leading: bool,
  pub(super) trailing: bool,
}

pub(super) fn gen_jsx_children<'a>(opts: GenJsxChildrenOptions<'a>, context: &mut Context<'a>) -> GenJsxChildrenResult<'a> {
  let edge_spaces = get_jsx_edge_spaces(&opts.children, context);
  let filtered_children = get_filtered_jsx_children(opts.children, context);
  // the closing tag stays beside the opening tag when there aren't any children since a line
  // break between them is text that type checking sees as children (ex. `<C\n  a\n>\n</C>`)
  if filtered_children.is_empty() && !edge_spaces.leading {
    return GenJsxChildrenResult {
      items: PrintItems::new(context.arena),
      new_lines_reevaluation: None,
    };
  }

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
    return GenJsxChildrenResult {
      items: gen_for_new_lines(generated_children, edge_spaces, opts.inner_range, context),
      new_lines_reevaluation: None,
    };
  } else {
    // decide whether newlines should be used or not
    let mut condition = if_true_or(
      "jsxChildrenNewLinesOrNot",
      context.arena.alloc_resolver(move |condition_context| {
        // use newlines if the header is multiple lines
        if condition_context.resolved_line_number(parent_start_ln)? < condition_context.writer_info.line_number {
          return Some(true);
        }

        // use newlines if the entire jsx element is on multiple lines
        condition_helpers::is_multiple_lines(condition_context, parent_start_ln, parent_end_ln)
      }),
      gen_for_new_lines(generated_children.clone(), edge_spaces, opts.inner_range, context),
      gen_for_single_line(generated_children, edge_spaces, context),
    );
    let new_lines_reevaluation = condition.create_reevaluation();
    return GenJsxChildrenResult {
      items: condition.into_print_items(context.arena),
      new_lines_reevaluation: Some(new_lines_reevaluation),
    };
  }

  /// JSX keeps whitespace without a line break, so the whitespace between a tag
  /// and the text beside it is a space when it's on the same line.
  fn get_jsx_edge_spaces<'a>(children: &[Node<'a>], context: &Context<'a>) -> JsxEdgeSpaces {
    let leading = match children.first() {
      Some(Node::JSXText(text)) => utils::has_jsx_leading_space(text.text_fast(context.program)),
      _ => false,
    };
    let trailing = match children.last() {
      // text generated as-is already has its spaces
      Some(&child @ Node::JSXText(text)) => !is_jsx_text_generated_as_is(child, context) && utils::has_jsx_trailing_space(text.text_fast(context.program)),
      _ => false,
    };
    JsxEdgeSpaces { leading, trailing }
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
      if utils::is_jsx_whitespace(child.text_fast(context.program)) {
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

  fn gen_for_new_lines<'a>(
    children: Vec<(Node<'a>, Option<PrintItemPath<'a>>)>,
    edge_spaces: JsxEdgeSpaces,
    inner_range: SourceRange,
    context: &mut Context<'a>,
  ) -> PrintItems<'a> {
    let mut items = PrintItems::new(context.arena);
    let has_children = !children.is_empty();
    let is_last_child_text = matches!(children.last(), Some((Node::JSXText(_), _)));
    // a newline after ignored text would remove the spaces at its end, so keep the closing tag beside it
    let is_closing_beside_last_child = children
      .last()
      .is_some_and(|(child, _)| is_jsx_text_generated_as_is(*child, context) && utils::has_jsx_trailing_space(child.text_fast(context.program)));
    // a space beside a newline would be removed, so the edge spaces become jsx space expressions
    let mut inner_items = PrintItems::new(context.arena);
    if edge_spaces.leading {
      inner_items.push_str_runtime_width_computed(get_jsx_space_text(1, context));
      if has_children {
        inner_items.push_signal(Signal::NewLine);
      }
    }
    inner_items.extend(gen_members(
      GenMembersOptions {
        inner_range,
        items: children.into_iter().map(|(a, b)| (a, Some(b.into_print_items(context.arena)))).collect(),
        should_use_space: Some(Box::new(|previous, next, context| has_jsx_space_between(previous, next, context.program))),
        should_use_new_line: Some(Box::new(|previous, next, context| {
          if has_jsx_space_between(previous, next, context.program) {
            false // prefer collapsing
          } else if let Node::JSXText(next) = next {
            utils::get_jsx_leading_whitespace(next.text_fast(context.program)).contains('\n')
          } else if let Node::JSXText(previous) = previous {
            utils::get_jsx_trailing_whitespace(previous.text_fast(context.program)).contains('\n')
          } else {
            true
          }
        })),
        get_forced_blank_line_count: |previous, next, context| {
          let has_blank_line = if has_jsx_space_between(previous, next, context.program) {
            false // prefer collapsing
          } else if let Node::JSXText(previous) = previous {
            utils::get_jsx_trailing_whitespace(previous.text_fast(context.program)).matches('\n').count() >= 2
          } else if let Node::JSXText(next) = next {
            utils::get_jsx_leading_whitespace(next.text_fast(context.program)).matches('\n').count() >= 2
          } else {
            node_helpers::has_separating_blank_line(&previous, &next, context.program)
          };
          // keep at most one blank line between jsx children
          Some(cap_blank_line_count(usize::from(has_blank_line), context))
        },
        separator: Separator::none(),
        is_jsx_children: true,
      },
      context,
    ));
    if edge_spaces.trailing && has_children {
      // same as a jsx space expression after the last child
      if is_last_child_text {
        inner_items.push_signal(Signal::PossibleNewLine);
      } else {
        inner_items.push_signal(Signal::NewLine);
      }
      inner_items.push_str_runtime_width_computed(get_jsx_space_text(1, context));
    }

    items.push_signal(Signal::NewLine);
    items.extend(ir_helpers::with_indent(inner_items));

    if (has_children || edge_spaces.leading) && !is_closing_beside_last_child {
      items.push_signal(Signal::NewLine);
    }

    items
  }

  fn gen_for_single_line<'a>(children: Vec<(Node<'a>, Option<PrintItemPath<'a>>)>, edge_spaces: JsxEdgeSpaces, context: &mut Context<'a>) -> PrintItems<'a> {
    let mut items = PrintItems::new(context.arena);
    // the possible newlines are outside the edge spaces so that going multi-line
    // happens at the edges, which then uses jsx space expressions for the spaces
    items.push_signal(Signal::PossibleNewLine);
    // when there are no children, the leading and trailing spaces are from the same whitespace text
    if edge_spaces.leading {
      items.push_space();
    }
    if !children.is_empty() {
      let mut previous_child = None;
      for (child, generated_child) in children.into_iter() {
        if let Some(previous_child) = previous_child {
          if has_jsx_space_between(previous_child, child, context.program) {
            items.extend(jsx_space_separator(previous_child, child, context));
          } else {
            items.push_signal(Signal::PossibleNewLine);
          }
        }

        items.extend(generated_child.into_print_items(context.arena));

        previous_child = Some(child);
      }
      if edge_spaces.trailing {
        items.push_space();
      }
      items.push_signal(Signal::PossibleNewLine);
    }
    items
  }

  /// If the node has a "JSX space expression" between or text that's only spaces between.
  fn has_jsx_space_between<'a>(previous_node: Node<'a>, next_node: Node<'a>, program: ProgramInfo<'a>) -> bool {
    fn has_jsx_space_expr_between<'a>(previous_node: Node<'a>, next_node: Node<'a>, program: ProgramInfo<'a>) -> bool {
      let nodes_between = node_helpers::get_siblings_between(previous_node, next_node, program);

      for node_between in nodes_between {
        if node_helpers::has_jsx_space_expr_text(node_between, program) {
          return true;
        }
      }

      false
    }

    node_helpers::nodes_have_only_spaces_between(previous_node, next_node, program) || has_jsx_space_expr_between(previous_node, next_node, program)
  }
}

pub(super) fn jsx_space_separator<'a>(previous_node: Node<'a>, current_node: Node<'a>, context: &Context<'a>) -> PrintItems<'a> {
  return if is_jsx_text_generated_as_is(previous_node, context) || is_jsx_text_generated_as_is(current_node, context) {
    // the spaces are in the text
    PrintItems::new(context.arena)
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

  fn jsx_force_space_with_newline_if_either_node_multi_line<'a>(previous_node: Node<'a>, current_node: Node<'a>, context: &Context<'a>) -> PrintItems<'a> {
    let previous_node_ln_range = get_node_ln_range(previous_node, context);
    let current_node_ln_range = get_node_ln_range(current_node, context);
    let spaces_between_count = node_helpers::count_spaces_between_jsx_children(previous_node, current_node, context.program);
    let jsx_space_expr_text = get_jsx_space_text(spaces_between_count, context);
    if_true_or(
      "jsxIsLastChildMultiLine",
      context.arena.alloc_resolver(move |condition_context| {
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
        let mut items = PrintItems::new(context.arena);
        items.push_signal(Signal::PossibleNewLine);
        items.push_str_runtime_width_computed(jsx_space_expr_text);
        items.push_signal(Signal::NewLine);
        items
      },
      {
        let mut items = PrintItems::new(context.arena);
        if spaces_between_count > 1 {
          items.push_signal(Signal::PossibleNewLine);
          items.push_str_runtime_width_computed(jsx_space_expr_text);
          items.push_signal(Signal::PossibleNewLine);
        } else {
          items.extend(jsx_space_or_newline_or_expr_space(previous_node, current_node, context));
        }
        items
      },
    )
    .into_print_items(context.arena)
  }

  fn jsx_space_or_newline_or_expr_space<'a>(previous_node: Node<'a>, current_node: Node<'a>, context: &Context<'a>) -> PrintItems<'a> {
    let spaces_between_count = node_helpers::count_spaces_between_jsx_children(previous_node, current_node, context.program);
    let mut items = PrintItems::new(context.arena);

    if spaces_between_count > 1 {
      items.push_signal(Signal::PossibleNewLine);
      items.push_str_runtime_width_computed(get_jsx_space_text(spaces_between_count, context));
      items.push_signal(Signal::PossibleNewLine);
      return items;
    }

    let start_line = LineNumber::new("jsxSpaceStartLine");
    let end_line = LineNumber::new("jsxSpaceEndLine");
    items.push_info(start_line);
    items.extend(gen_line_numbers_region_start(context.arena, &[end_line]));

    let mut condition = if_true_or(
      "jsxSpaceOrNewLineIsMultipleLines",
      context.arena.alloc_resolver(move |context| {
        let start_line = context.resolved_line_number(start_line)?;
        let end_line = context.resolved_line_number(end_line)?;
        Some(start_line < end_line)
      }),
      {
        let mut items = PrintItems::new(context.arena);
        items.push_signal(Signal::PossibleNewLine);
        items.push_str_runtime_width_computed(get_jsx_space_text(1, context));
        items.push_signal(Signal::NewLine);
        items
      },
      Signal::SpaceOrNewLine.into_print_items(context.arena),
    );
    let condition_reevaluation = condition.create_reevaluation();

    items.push_condition(condition);
    items.push_info(end_line);
    items.push_reevaluation(condition_reevaluation);
    items
  }
}

pub(super) fn get_jsx_space_text<'a>(spaces_between_count: usize, context: &Context<'a>) -> &'a str {
  let is_double_quote = matches!(context.config.quote_style, QuoteStyle::PreferDouble | QuoteStyle::AlwaysDouble);
  match (spaces_between_count, is_double_quote) {
    (1, true) => "{\" \"}",
    (1, false) => "{' '}",
    _ => context.alloc_str(&format!(
      "{{{}{}{}}}",
      get_quote_char(context),
      " ".repeat(spaces_between_count),
      get_quote_char(context)
    )),
  }
}

pub(super) fn get_quote_char<'a>(context: &Context<'a>) -> String {
  match context.config.quote_style {
    QuoteStyle::PreferDouble | QuoteStyle::AlwaysDouble => "\"".to_string(),
    QuoteStyle::PreferSingle | QuoteStyle::AlwaysSingle => "'".to_string(),
  }
}

#[inline]
pub(super) fn gen_assignment<'a>(expr: Node<'a>, op: &'static StringContainer<'static>, context: &mut Context<'a>) -> PrintItems<'a> {
  gen_assignment_op_to(expr, op.text, op, context)
}

#[inline]
pub(super) fn gen_assignment_op_to<'a>(
  expr: Node<'a>,
  _op: &'static str,
  op_to: &'static StringContainer<'static>,
  context: &mut Context<'a>,
) -> PrintItems<'a> {
  let op_token = context.token_finder.get_previous_token(&expr);
  #[cfg(debug_assertions)]
  assert_has_op(_op, op_token, context);

  gen_assignment_like_with_token(expr, op_to, op_token, context)
}

pub(super) fn gen_assignment_like_with_token<'a>(
  expr: Node<'a>,
  op: &'static StringContainer<'static>,
  op_token: Option<&TokenAndSpan>,
  context: &mut Context<'a>,
) -> PrintItems<'a> {
  let use_new_line_group = get_use_new_line_group(expr);
  let mut items = PrintItems::new(context.arena);

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
    let mut items = PrintItems::new(context.arena);
    let mut reevaluation = None;
    if !had_op_trailing_comments {
      let start_ln = LineNumber::new("assignmentStart");
      let paren_end_ln = get_paren_with_kept_line_breaks(expr, context.config).map(|paren_start| {
        let end_ln = LineNumber::new("assignmentParenEnd");
        context.assignment_paren_end_ln = Some((paren_start, end_ln));
        end_ln
      });
      let curried_arrow_end_ln = get_curried_arrow_start(expr).map(|arrow_start| {
        let end_ln = LineNumber::new("assignmentCurriedArrowEnd");
        context.assignment_curried_arrow_end_ln = Some((arrow_start, end_ln));
        end_ln
      });
      let indent_width = context.config.indent_width as u32;
      let mut condition = if_true_or(
        "possibleNewLineIfAboveWidth",
        context.arena.alloc_resolver(move |condition_context| {
          let writer_info = &condition_context.writer_info;
          if writer_info.column_number <= writer_info.line_start_column_number() + indent_width {
            return Some(false);
          }
          // The line breaks in the parenthesized expression are kept when formatting again, so
          // don't move it to the next line when it doesn't fit on one line there anyway.
          if let Some(paren_end_ln) = paren_end_ln {
            let start_line = condition_context.resolved_line_number(start_ln);
            let paren_end_line = condition_context.resolved_line_number(paren_end_ln);
            if let (Some(start_line), Some(paren_end_line)) = (start_line, paren_end_line)
              && paren_end_line > start_line
            {
              return Some(false);
            }
          }
          Some(true)
        }),
        {
          let mut items = PrintItems::new(context.arena);
          items.push_signal(Signal::SpaceIfNotTrailing);
          items.push_signal(Signal::PossibleNewLine);
          items
        },
        Signal::SpaceIfNotTrailing.into_print_items(context.arena),
      );
      if paren_end_ln.is_some() {
        reevaluation = Some(condition.create_reevaluation());
      }
      if let Some(curried_arrow_end_ln) = curried_arrow_end_ln {
        // keep the arrows of a multi-line curried arrow function indented on the lines after the operator
        items.push_condition(if_true_or(
          "newLineIfMultiLineCurriedArrow",
          context
            .arena
            .alloc_resolver(move |condition_context| condition_helpers::is_multiple_lines(condition_context, start_ln, curried_arrow_end_ln)),
          Signal::NewLine.into_print_items(context.arena),
          {
            let mut items = PrintItems::new(context.arena);
            items.push_condition(condition);
            items
          },
        ));
      } else {
        items.push_condition(condition);
      }
      items.push_info(start_ln);
    }
    let assignment = gen_node(expr, context);
    let assignment = if had_op_trailing_comments {
      assignment
    } else {
      conditions::indent_if_start_of_line(assignment).into_print_items(context.arena)
    };
    let assignment = if use_new_line_group { new_line_group(assignment) } else { assignment };
    items.extend(assignment);
    if let Some(reevaluation) = reevaluation {
      items.push_reevaluation(reevaluation);
    }
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
    with_indent(generated_assignment.into_print_items(context.arena)),
    generated_assignment.into_print_items(context.arena),
  ));

  return items;

  fn get_use_new_line_group(expr: Node) -> bool {
    // swc had a separate node for `super.a`, which didn't use a new line group
    match expr {
      Node::StaticMemberExpression(member) => !matches!(member.object, Expression::Super(_)),
      Node::ComputedMemberExpression(member) => !matches!(member.object, Expression::Super(_)),
      Node::PrivateFieldExpression(_) => true,
      _ => false,
    }
  }

  fn get_curried_arrow_start(expr: Node) -> Option<SourcePos> {
    match expr {
      Node::ArrowFunctionExpression(arrow) if matches!(arrow.body, ArrowFunctionBody::ArrowFunctionExpression(_)) => Some(arrow.start()),
      _ => None,
    }
  }

  /// Gets the start of the parenthesized expression or type at the start of the right side when it has
  /// one that keeps its line breaks (ex. `(a ? b : c) as T`, `!!(a && b)`, or `<(A | B)>value`).
  fn get_paren_with_kept_line_breaks<'a>(expr: Node<'a>, config: &Configuration) -> Option<SourcePos> {
    let mut node = expr;
    loop {
      node = match node {
        Node::TSAsExpression(expr) => (&expr.expression).into_node(),
        Node::TSSatisfiesExpression(expr) => (&expr.expression).into_node(),
        Node::TSNonNullExpression(expr) => (&expr.expression).into_node(),
        Node::StaticMemberExpression(expr) => (&expr.object).into_node(),
        Node::ComputedMemberExpression(expr) => (&expr.object).into_node(),
        Node::PrivateFieldExpression(expr) => (&expr.object).into_node(),
        Node::UnaryExpression(expr) => (&expr.argument).into_node(),
        Node::AwaitExpression(expr) => (&expr.argument).into_node(),
        Node::ParenthesizedExpression(paren) => {
          let keeps_line_breaks = match paren.expression {
            Expression::ConditionalExpression(_) => !config.conditional_expression_prefer_single_line,
            Expression::BinaryExpression(_) | Expression::LogicalExpression(_) => !config.binary_expression_prefer_single_line,
            _ => false,
          };
          return keeps_line_breaks.then_some(paren.start());
        }
        // ex. `<(A | B)>value`
        Node::TSTypeAssertion(expr) => {
          let TSType::TSParenthesizedType(paren) = &expr.type_annotation else {
            return None;
          };
          let keeps_line_breaks =
            matches!(paren.type_annotation, TSType::TSUnionType(_) | TSType::TSIntersectionType(_)) && !config.union_and_intersection_type_prefer_single_line;
          return keeps_line_breaks.then_some(paren.start());
        }
        _ => return None,
      };
    }
  }
}

pub(super) struct GenBlockOptions<'a> {
  pub(super) range: Option<SourceRange>,
  pub(super) children: Vec<Node<'a>>,
  pub(super) block_padding: BlockPadding,
  pub(super) keep_blank_lines_before_close_token_comments: bool,
}

pub(super) fn gen_block<'a>(
  gen_inner: impl FnOnce(Vec<Node<'a>>, &mut Context<'a>) -> PrintItems<'a>,
  opts: GenBlockOptions<'a>,
  context: &mut Context<'a>,
) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  let skip_indent = context.skip_iife_body_indent;
  context.skip_iife_body_indent = false;
  let before_open_token_ln = LineNumber::new("after_open_token_info");
  let first_member_range = opts.children.first().map(|x| x.range());
  let range = opts.range;
  let padding = match range {
    Some(range) => get_block_padding_blank_lines(
      SourceRange::new(range.start, range.start + 1),
      SourceRange::new(range.end - 1, range.end),
      opts.block_padding,
      context,
    ),
    None => BlockPaddingBlankLines::default(),
  };
  items.push_info(before_open_token_ln);
  items.extend(gen_surrounded_by_tokens(
    |context| {
      let mut items = PrintItems::new(context.arena);
      let start_inner_lc = LineAndColumn::new("startStatements");
      let end_inner_lc = LineAndColumn::new("endStatements");
      let is_tokens_same_line_and_empty = if let Some(range) = &range {
        range.start_line_fast(context.program) == range.end_line_fast(context.program) && opts.children.is_empty()
      } else {
        true
      };
      if !is_tokens_same_line_and_empty {
        items.push_signal(Signal::NewLine);
        items.extend(gen_blank_lines_if_has_content(context.arena, padding.start, start_inner_lc, end_inner_lc));
      }
      items.push_line_and_column(start_inner_lc);
      let inner = gen_inner(opts.children, context);
      items.extend(if skip_indent { inner } else { ir_helpers::with_indent(inner) });
      items.push_line_and_column(end_inner_lc);

      if is_tokens_same_line_and_empty {
        items.push_condition(if_true(
          "newLineIfDifferentLine",
          context
            .arena
            .alloc_resolver(move |context| condition_helpers::is_on_different_line(context, before_open_token_ln)),
          Signal::NewLine.into_print_items(context.arena),
        ));
      } else {
        items.push_condition(if_false(
          "endNewline",
          context
            .arena
            .alloc_resolver(move |context| condition_helpers::are_line_and_columns_equal(context, start_inner_lc, end_inner_lc)),
          Signal::NewLine.into_print_items(context.arena),
        ));
        items.extend(gen_blank_lines_if_has_content(context.arena, padding.end, start_inner_lc, end_inner_lc));
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
      comments_only_padding: padding,
      keep_blank_lines_before_close_token_comments: opts.keep_blank_lines_before_close_token_comments,
    },
    context,
  ));
  items
}

pub(super) struct GenSurroundedByTokensOptions {
  pub(super) open_token: &'static StringContainer<'static>,
  pub(super) close_token: &'static StringContainer<'static>,
  /// When `None`, means the tokens are missing
  pub(super) range: Option<SourceRange>,
  pub(super) first_member: Option<SourceRange>,
  pub(super) prefer_single_line_when_empty: bool,
  pub(super) allow_open_token_trailing_comments: bool,
  pub(super) single_line_space_around: bool,
  /// The blank lines to keep around the comments when there are only comments between the tokens.
  pub(super) comments_only_padding: BlockPaddingBlankLines,
  /// Whether to keep the blank lines before the comments on their own lines before the close
  /// token, which is for the lists (ex. the comments after the last value's comma).
  pub(super) keep_blank_lines_before_close_token_comments: bool,
}

pub(super) fn gen_surrounded_by_tokens<'a>(
  gen_inner: impl FnOnce(&mut Context<'a>) -> PrintItems<'a>,
  custom_close_token: impl FnOnce(&mut Context<'a>) -> Option<PrintItems<'a>>,
  opts: GenSurroundedByTokensOptions,
  context: &mut Context<'a>,
) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
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
        // no space when the inner text starts on the next line
        items.push_signal(Signal::SpaceIfNotTrailing);
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
      if opts.keep_blank_lines_before_close_token_comments {
        items.extend(gen_blank_lines_before_close_token_comments(close_token_start, context));
      }
      items.extend(with_indent(gen_comments_as_statements(
        close_token_start.leading_comments_fast(context.program),
        None,
        context,
      )));
      items.push_condition(if_true(
        "newLineIfHasCommentsAndNotStartOfNewLine",
        context.arena.alloc_resolver(move |context| {
          let had_comments = !condition_helpers::is_at_same_position(context, before_trailing_comments_lc)?;
          Some(had_comments && !context.writer_info.is_start_of_line())
        }),
        Signal::NewLine.into_print_items(context.arena),
      ));
    } else {
      let comments = open_token_end.trailing_comments_fast(context.program);
      // the padding goes on its own lines, so a block with only comments is multi-line when padded
      is_single_line = open_token_start_line == close_token_start.start_line_fast(context.program) && opts.comments_only_padding.is_empty();

      if is_single_line && opts.single_line_space_around {
        // no space when the inner text starts on the next line
        items.push_signal(Signal::SpaceIfNotTrailing);
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
            let arena = context.arena;
            let max_blank_lines = context.config.max_blank_lines;
            items.extend(
              ir_helpers::gen_separated_values(
                |_| {
                  let mut generated_comments = BumpVec::with_capacity_in(comments.size_hint().0, arena.bump());
                  for c in comments {
                    let start_line = c.start_line_fast(context.program);
                    let end_line = c.end_line_fast(context.program);
                    if let Some(items) = gen_comment(c, context) {
                      generated_comments.push(ir_helpers::GeneratedValue {
                        items,
                        lines_span: Some(ir_helpers::LinesSpan { start_line, end_line }),
                        allow_inline_multi_line: false,
                        allow_inline_single_line: false,
                        is_known_multi_line: false,
                      });
                    }
                  }
                  generated_comments
                },
                ir_helpers::GenSeparatedValuesOptions {
                  prefer_hanging: false,
                  force_use_new_lines: false,
                  max_blank_lines,
                  indent_width,
                  single_line_options: SingleLineOptions {
                    space_at_start: opts.single_line_space_around,
                    space_at_end: opts.single_line_space_around,
                    separator: Signal::SpaceOrNewLine.into_print_items(arena),
                  },
                  multi_line_options: ir_helpers::MultiLineOptions::surround_newlines_indented(),
                  force_possible_newline_at_start: false,
                },
              )
              .items,
            );
          } else {
            items.push_signal(Signal::NewLine);
            push_blank_lines(&mut items, opts.comments_only_padding.start);
            items.extend(with_indent(gen_comments_as_statements(comments, None, context)));
            items.push_signal(Signal::NewLine);
            push_blank_lines(&mut items, opts.comments_only_padding.end);
          }
        }
      } else if !is_single_line && !opts.prefer_single_line_when_empty {
        items.push_signal(Signal::NewLine);
      }
    }

    if is_single_line && opts.single_line_space_around {
      // no space when the close token starts the line
      items.push_condition(if_false(
        "spaceIfNotStartOfLine",
        condition_resolvers::is_start_of_line(),
        Signal::SpaceIfNotTrailing.into_print_items(context.arena),
      ));
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

  fn gen_first_line_trailing_comment<'a>(open_token_start_line: usize, comments: CommentsIterator<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
    let mut items = PrintItems::new(context.arena);
    let first_comment = comments.into_iter().next();
    if let Some(first_comment) = first_comment
      && first_comment.kind == CommentKind::Line
      && first_comment.start_line_fast(context.program) == open_token_start_line
      && let Some(generated_comment) = gen_comment(first_comment, context)
    {
      items.push_signal(Signal::StartForceNoNewLines);
      items.push_space();
      items.extend(generated_comment);
      items.push_signal(Signal::FinishForceNoNewLines);
    }
    items
  }

  /// Generates the blank lines to keep before the comments on their own lines before the close
  /// token (ex. `// c` in `{\n  a,\n\n  // c\n}`), the same as after the last member of a block.
  fn gen_blank_lines_before_close_token_comments<'a>(close_token_start: SourcePos, context: &mut Context<'a>) -> PrintItems<'a> {
    let program = context.program;
    let mut items = PrintItems::new(context.arena);
    let first_comment = close_token_start.leading_comments_fast(program).find(|c| !context.has_handled_comment(c));
    let (Some(first_comment), Some(previous_token)) = (first_comment, close_token_start.range().previous_token_fast(program)) else {
      return items;
    };
    // the comments before the first comment were already generated after the previous token
    let previous_end_line = previous_token
      .end()
      .trailing_comments_fast(program)
      .chain(close_token_start.leading_comments_fast(program))
      .filter(|c| c.end() <= first_comment.start())
      .map(|c| c.end_line_fast(program))
      .fold(previous_token.end_line_fast(program), usize::max);
    let blank_line_count = cap_blank_line_count(first_comment.start_line_fast(program).saturating_sub(previous_end_line + 1), context);
    if blank_line_count > 0 {
      let mut blank_lines = PrintItems::new(context.arena);
      push_blank_lines(&mut blank_lines, blank_line_count);
      // only when multi-line, since the comments are otherwise on the line of the last member
      items.push_condition(if_true(
        "blankLinesBeforeCloseTokenComments",
        condition_resolvers::is_start_of_line(),
        blank_lines,
      ));
    }
    items
  }
}

#[cfg(debug_assertions)]
pub(super) fn assert_has_op<'a>(op: &str, op_token: Option<&TokenAndSpan>, context: &mut Context<'a>) {
  if let Some(op_token) = op_token {
    context.assert_text(SourceRange::new(op_token.start(), op_token.end()), op);
  } else {
    panic!("Debug panic! Expected to have op token: {op}");
  }
}

pub(super) fn use_new_line_group_for_arrow_body<'a>(arrow_expr: &'a ArrowFunctionExpression<'a>, context: &Context<'a>) -> bool {
  match &arrow_expr.body {
    ArrowFunctionBody::FunctionBody(_) => true,
    ArrowFunctionBody::ParenthesizedExpression(paren) => match paren.expression {
      Expression::ObjectExpression(_) => false,
      _ => !is_jsx_paren_expr_handled_node((&paren.expression).into_node(), context),
    },
    expr => !is_jsx_paren_expr_handled_node(expr.into_node(), context),
  }
}

/// Gets if the arrow function is the last of more than one call argument and its body breaks inside
/// its parens, which keeps the arguments on the call's line (ex. `call(a, () => (\n  <div />\n))`).
pub(super) fn is_hugged_last_arg_arrow<'a>(arrow_expr: &'a ArrowFunctionExpression<'a>, context: &Context<'a>) -> bool {
  // the parent of an argument is the call (a spread argument is in a spread element instead)
  let args: &'a [Argument<'a>] = match arrow_expr.into_node().parent(context.program) {
    Some(Node::CallExpression(call_expr)) => &call_expr.arguments,
    Some(Node::NewExpression(new_expr)) => &new_expr.arguments,
    _ => return false,
  };
  // hanging arguments keep going on the next line instead
  context.config.arguments_prefer_hanging != PreferHanging::Always
    && args.len() > 1
    && args.last().is_some_and(|last_arg| last_arg.start() == arrow_expr.start())
    // functions side by side read better on separate lines (ex. `call(\n  (a) => b,\n  (c) => ({ d }),\n)`)
    && !matches!(args[args.len() - 2], Argument::ArrowFunctionExpression(_) | Argument::FunctionExpression(_))
    && !use_new_line_group_for_arrow_body(arrow_expr, context)
}

/// Prevents a condition from reading a line number that's resolved later in the output with
/// the value from an earlier printing at a different position. The printer doesn't clear
/// resolved infos when it goes back and prints something again, so a stale end line could
/// make a condition think its node is multi-line, which then makes it multi-line.
///
/// Keeps the line numbers resolved after this correct when the printer comes back here
/// at a different position (see `InfoRegion`).
pub(super) fn gen_line_numbers_region_start<'a>(arena: &'a Arena, line_numbers: &[LineNumber]) -> PrintItems<'a> {
  let region = ir_helpers::InfoRegion::new(arena);
  for line_number in line_numbers {
    region.add(*line_number);
  }
  region.gen_start(arena)
}

/// The line of the last item of a member expression, which `isMultipleLines` conditions
/// compare with the line the member expression starts on.
///
/// The printer keeps resolved infos when it goes back to an earlier point, so when the
/// member expression is printed again at a different position, this line is from where it
/// was printed before. If it was on a later line, the stale value would keep the member
/// expression broken, which then keeps it on a later line, so it needs to be worked out
/// again. If it was on the same line, the member expression fit there and the line width
/// breaks it if it no longer does, so it can be moved instead, which avoids printing
/// member expressions nested in it again each time.
#[derive(Clone, Copy)]
pub(super) struct LastItemLine<'a> {
  pub(super) line_number: LineNumber,
  pub(super) start: &'a Cell<Option<(u32, u32)>>,
  /// The column numbers where it started on the same line as its last item.
  pub(super) single_line_columns: &'a RefCell<BumpVec<'a, u32>>,
  /// The resolved line number, without looking ahead to it.
  pub(super) resolved: &'a Cell<Option<u32>>,
}

impl<'a> LastItemLine<'a> {
  fn new(line_number: LineNumber, arena: &'a Arena) -> Self {
    Self {
      line_number,
      start: arena.bump().alloc(Cell::new(None)),
      single_line_columns: arena.bump().alloc(RefCell::new(BumpVec::new_in(arena.bump()))),
      resolved: arena.bump().alloc(Cell::new(None)),
    }
  }

  /// Generates the items for the start of the member expression.
  fn gen_start(self, arena: &'a Arena) -> PrintItems<'a> {
    actions::action(arena, "memberExprLastItemLineStart", move |context| {
      let start = (context.writer_info.line_number, context.writer_info.column_number);
      let last_start = self.start.replace(Some(start));
      if last_start.is_none() || last_start == Some(start) {
        return;
      }
      let Some(resolved) = self.resolved.get() else {
        return;
      };
      if self.single_line_columns.borrow().contains(&start.1) {
        context.shift_line_number(self.line_number, start.0 as isize - resolved as isize);
        self.resolved.set(Some(start.0));
      } else {
        context.clear_info(self.line_number);
        self.resolved.set(None);
      }
    })
  }

  /// Generates the items for right after the line number is resolved.
  fn gen_resolved(self, arena: &'a Arena) -> PrintItems<'a> {
    actions::action(arena, "memberExprLastItemLineResolved", move |context| {
      let Some((start_line, start_column)) = self.start.get() else {
        return;
      };
      let line_number = context.writer_info.line_number;
      self.resolved.set(Some(line_number));
      let mut single_line_columns = self.single_line_columns.borrow_mut();
      let index = single_line_columns.iter().position(|column| *column == start_column);
      match (line_number == start_line, index) {
        (true, None) => single_line_columns.push(start_column),
        (false, Some(index)) => {
          single_line_columns.swap_remove(index);
        }
        _ => {}
      }
    })
  }
}

/// Gets the parameters of a function, which starts with its `this` parameter.
pub(super) fn get_function_params<'a>(function: &'a Function<'a>, context: &Context<'a>) -> BumpVec<'a, Node<'a>> {
  get_formal_params(function.this_param.as_deref(), &function.params, context)
}

/// Gets the nodes of the parameters, which are the `this` parameter, the parameters,
/// and then the rest parameter.
pub(super) fn get_formal_params<'a>(
  this_param: Option<&'a TSThisParameter<'a>>,
  params: &'a FormalParameters<'a>,
  context: &Context<'a>,
) -> BumpVec<'a, Node<'a>> {
  let mut nodes = BumpVec::with_capacity_in(params.items.len() + 2, context.arena.bump());
  if let Some(this_param) = this_param {
    nodes.push(this_param.into_node());
  }
  nodes.extend(params.items.iter().map(|param| param.into_node()));
  if let Some(rest) = &params.rest {
    nodes.push(rest.into_node());
  }
  nodes
}

pub(super) fn surround_with_parens<'a>(items: PrintItems<'a>) -> PrintItems<'a> {
  let mut new_items = PrintItems::new(items.arena());
  new_items.push_sc(sc!("("));
  new_items.extend(items);
  new_items.push_sc(sc!(")"));
  new_items
}

pub(super) fn surround_with_spaces<'a>(items: PrintItems<'a>) -> PrintItems<'a> {
  let mut new_items = PrintItems::new(items.arena());
  new_items.push_space();
  new_items.extend(items);
  new_items.push_space();
  new_items
}

pub(super) fn assign_op_sc(assign_op: AssignmentOperator) -> &'static StringContainer<'static> {
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

pub(super) fn binary_op_sc(binary_op: BinaryOp) -> &'static StringContainer<'static> {
  let sc = match binary_op {
    BinaryOp::Binary(binary_op) => {
      use BinaryOperator::*;
      match binary_op {
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
      }
    }
    BinaryOp::Logical(logical_op) => match logical_op {
      LogicalOperator::Or => sc!("||"),
      LogicalOperator::And => sc!("&&"),
      LogicalOperator::Coalesce => sc!("??"),
    },
  };
  if cfg!(debug_assertions) {
    assert_eq!(sc.text, binary_op.as_str());
  }
  sc
}

/* is/has functions */

/// Gets if the node is a call argument that's an arrow function with an expression body.
pub(super) fn is_arrow_function_with_expr_body(node: Node) -> bool {
  match node {
    // the arguments of a call are the expressions (swc wrapped them in a node)
    Node::ArrowFunctionExpression(arrow) => !matches!(arrow.body, ArrowFunctionBody::FunctionBody(_)),
    _ => false,
  }
}

pub(super) fn allows_inline_multi_line<'a>(node: Node<'a>, context: &Context<'a>, has_siblings: bool) -> bool {
  return match node {
    // swc had the type annotation on the binding identifier and the initializer in an assignment pattern
    Node::FormalParameter(param) => {
      let allows_binding = match &param.pattern {
        BindingPattern::BindingIdentifier(_) => match &param.type_annotation {
          Some(type_ann) => allows_inline_multi_line((&type_ann.type_annotation).into_node(), context, has_siblings),
          None => false,
        },
        pattern => allows_inline_multi_line(pattern.into_node(), context, has_siblings),
      };
      match &param.initializer {
        Some(initializer) => allows_binding || allows_inline_multi_line(initializer.into_node(), context, has_siblings),
        None => allows_binding,
      }
    }
    // swc had the `this` parameter as a binding identifier with a type annotation
    Node::TSThisParameter(param) => match &param.type_annotation {
      Some(type_ann) => allows_inline_multi_line((&type_ann.type_annotation).into_node(), context, has_siblings),
      None => false,
    },
    // swc had a separate node for `as const`
    Node::TSAsExpression(as_expr) if is_const_assertion(as_expr) => false,
    Node::TSAsExpression(as_expr) => {
      allows_inline_multi_line((&as_expr.expression).into_node(), context, has_siblings)
        && match &as_expr.type_annotation {
          TSType::TSTypeReference(_)
          | TSType::TSAnyKeyword(_)
          | TSType::TSBigIntKeyword(_)
          | TSType::TSBooleanKeyword(_)
          | TSType::TSIntrinsicKeyword(_)
          | TSType::TSNeverKeyword(_)
          | TSType::TSNullKeyword(_)
          | TSType::TSNumberKeyword(_)
          | TSType::TSObjectKeyword(_)
          | TSType::TSStringKeyword(_)
          | TSType::TSSymbolKeyword(_)
          | TSType::TSUndefinedKeyword(_)
          | TSType::TSUnknownKeyword(_)
          | TSType::TSVoidKeyword(_) => true,
          type_ann => allows_inline_multi_line(type_ann.into_node(), context, has_siblings),
        }
    }
    Node::Function(func) => func.r#type == FunctionType::FunctionExpression,
    Node::ArrowFunctionExpression(_)
    | Node::ObjectExpression(_)
    | Node::ArrayExpression(_)
    | Node::ObjectPattern(_)
    | Node::ArrayPattern(_)
    | Node::ObjectAssignmentTarget(_)
    | Node::ArrayAssignmentTarget(_)
    | Node::TSTypeLiteral(_)
    | Node::TSTupleType(_)
    | Node::TSArrayType(_) => true,
    // swc wrapped the spread arguments and array elements in a node that was checked
    // by its expression, but not the spread properties of an object
    Node::SpreadElement(node) => {
      !matches!(node.into_node().parent(context.program), Some(Node::ObjectExpression(_)))
        && allows_inline_multi_line((&node.argument).into_node(), context, has_siblings)
    }
    Node::ParenthesizedExpression(node) => {
      should_skip_paren_expr(node, context) && allows_inline_multi_line((&node.expression).into_node(), context, has_siblings)
    }
    Node::TaggedTemplateExpression(_) | Node::TemplateLiteral(_) => !has_siblings,
    Node::CallExpression(node) => !has_siblings && allow_inline_for_call_expr(node),
    Node::NewExpression(node) => !has_siblings && allow_inline_for_callee_expr(&node.callee),
    Node::AssignmentPattern(node) => {
      allows_inline_multi_line((&node.left).into_node(), context, has_siblings) || allows_inline_multi_line((&node.right).into_node(), context, has_siblings)
    }
    Node::AssignmentTargetWithDefault(node) => {
      allows_inline_multi_line((&node.binding).into_node(), context, has_siblings) || allows_inline_multi_line((&node.init).into_node(), context, has_siblings)
    }
    Node::TSTypeAnnotation(type_ann) => allows_inline_multi_line((&type_ann.type_annotation).into_node(), context, has_siblings),
    Node::TSNamedTupleMember(tuple_element) => allows_inline_multi_line((&tuple_element.element_type).into_node(), context, has_siblings),
    _ => false,
  };

  fn allow_inline_for_call_expr(node: &CallExpression) -> bool {
    // `import(...)` is a separate node and `super(...)` has a `Super` callee, which is allowed
    allow_inline_for_callee_expr(&node.callee)
  }

  /// Do not allow call or new exprs with nested call exprs in the member expr to be inline.
  fn allow_inline_for_callee_expr(expr: &Expression) -> bool {
    match expr {
      Expression::StaticMemberExpression(member_expr) => allow_inline_for_callee_expr(&member_expr.object),
      Expression::ComputedMemberExpression(member_expr) => allow_inline_for_callee_expr(&member_expr.object),
      Expression::PrivateFieldExpression(member_expr) => allow_inline_for_callee_expr(&member_expr.object),
      Expression::CallExpression(_) => false,
      Expression::ChainExpression(chain_expr) => !matches!(chain_expr.expression, ChainElement::CallExpression(_)),
      _ => true,
    }
  }

  fn is_const_assertion(as_expr: &TSAsExpression) -> bool {
    match &as_expr.type_annotation {
      TSType::TSTypeReference(type_ref) => {
        type_ref.type_arguments.is_none() && matches!(&type_ref.type_name, TSTypeName::IdentifierReference(ident) if ident.name.as_str() == "const")
      }
      _ => false,
    }
  }
}

pub(super) fn get_use_new_lines_for_nodes_with_preceeding_token<'a>(
  open_token_text: &str,
  nodes: &[impl SourceRanged],
  prefer_single_line: bool,
  context: &mut Context<'a>,
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

    if let Some(previous_token) = previous_token
      && previous_token.text_fast(context.program) == open_token_text
    {
      return node_helpers::get_use_new_lines_for_nodes(&previous_token.range(), first_node, context.program);
    }

    // arrow function expressions might not have an open paren (ex. `a => a + 5`)
    false
  }
}

pub(super) fn get_use_new_lines_for_nodes<'a>(nodes: &[impl SourceRanged], prefer_single_line: bool, context: &mut Context<'a>) -> bool {
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
pub(super) fn has_any_node_comment_on_different_line<'a>(nodes: &[impl SourceRanged], context: &mut Context<'a>) -> bool {
  match nodes.first() {
    Some(first_node) => has_leading_comment_on_previous_line(first_node, context) || has_any_node_trailing_comment_on_different_line(nodes, context),
    None => false,
  }
}

/// Gets if the node has a line comment before it or a comment on a previous line.
pub(super) fn has_leading_comment_on_previous_line<'a>(node: &impl SourceRanged, context: &mut Context<'a>) -> bool {
  let node_start_line = node.start_line_fast(context.program);
  node
    .leading_comments_fast(context.program)
    .take_while(|c| c.kind == CommentKind::Line || c.start_line_fast(context.program) < node_start_line)
    .next()
    .is_some()
}

/// Gets if any of the provided nodes have trailing comments on a different line.
pub(super) fn has_any_node_trailing_comment_on_different_line<'a>(nodes: &[impl SourceRanged], context: &mut Context<'a>) -> bool {
  for (i, node) in nodes.iter().enumerate() {
    let node_end = node.end();
    let next_node_pos = nodes.get(i + 1).map(|n| n.start());
    if check_pos_has_trailing_comments(node_end, next_node_pos, context) {
      return true;
    } else if let Some(comma) = context.token_finder.get_next_token_if_comma(&node_end)
      && check_pos_has_trailing_comments(comma.end(), next_node_pos, context)
    {
      return true;
    }
  }

  return false;

  fn check_pos_has_trailing_comments<'a>(end: SourcePos, next_node_pos: Option<SourcePos>, context: &mut Context<'a>) -> bool {
    let end_line = end.end_line_fast(context.program);
    let stop_line = next_node_pos.map(|p| p.start_line_fast(context.program));

    for c in end.trailing_comments_fast(context.program) {
      if c.kind == CommentKind::Line {
        return true;
      }
      if let Some(stop_line) = stop_line
        && c.start_line_fast(context.program) >= stop_line
      {
        // do not look at comments that the next node owns
        return false;
      }
      if c.end_line_fast(context.program) > end_line {
        return true;
      }
    }

    false
  }
}

pub(super) fn is_node_definitely_above_line_width<'a>(range: SourceRange, context: &Context<'a>) -> bool {
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

pub(super) fn get_generated_separator<'a>(arena: &'a Arena, separator: &Separator, is_trailing: bool, is_multi_line: &ConditionResolver<'a>) -> PrintItems<'a> {
  debug_assert!(!separator.is_none());
  // performance optimization
  return if separator.single_line == separator.multi_line {
    get_items(arena, &separator.single_line, is_trailing, is_multi_line)
  } else {
    if_true_or(
      "is_multi_line",
      *is_multi_line,
      get_items(arena, &separator.multi_line, is_trailing, is_multi_line),
      get_items(arena, &separator.single_line, is_trailing, is_multi_line),
    )
    .into_print_items(arena)
  };

  fn get_items<'a>(arena: &'a Arena, value: &Option<SeparatorValue>, is_trailing: bool, is_multi_line: &ConditionResolver<'a>) -> PrintItems<'a> {
    match value {
      Some(SeparatorValue::Comma(trailing_comma)) => get_generated_trailing_comma(arena, *trailing_comma, is_trailing, is_multi_line),
      Some(SeparatorValue::Semicolon(semicolons)) => get_generated_semi_colon(arena, *semicolons, is_trailing, is_multi_line),
      None => PrintItems::new(arena),
    }
  }
}

pub(super) fn get_generated_trailing_comma<'a>(
  arena: &'a Arena,
  option: TrailingCommas,
  is_trailing: bool,
  is_multi_line: &ConditionResolver<'a>,
) -> PrintItems<'a> {
  if !is_trailing {
    return sc!(",").into_print_items(arena);
  }

  match option {
    TrailingCommas::Always => sc!(",").into_print_items(arena),
    TrailingCommas::OnlyMultiLine => if_true("trailingCommaIfMultiLine", *is_multi_line, sc!(",").into_print_items(arena)).into_print_items(arena),
    TrailingCommas::Never => PrintItems::new(arena),
  }
}

pub(super) fn get_generated_semi_colon<'a>(arena: &'a Arena, option: Semicolons, is_trailing: bool, is_multi_line: &ConditionResolver<'a>) -> PrintItems<'a> {
  match option {
    Semicolons::Always => sc!(";").into_print_items(arena),
    Semicolons::Prefer => {
      if is_trailing {
        if_true("semiColonIfMultiLine", *is_multi_line, sc!(";").into_print_items(arena)).into_print_items(arena)
      } else {
        sc!(";").into_print_items(arena)
      }
    }
    Semicolons::Asi => {
      if is_trailing {
        PrintItems::new(arena)
      } else {
        if_false("semiColonIfSingleLine", *is_multi_line, sc!(";").into_print_items(arena)).into_print_items(arena)
      }
    }
  }
}
