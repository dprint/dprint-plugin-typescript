use super::*;

/* helpers */

pub(super) struct GenArrayLikeNodesOptions<'a> {
  pub(super) node: Node<'a>,
  /// The range from the open bracket to the close bracket.
  pub(super) range: SourceRange,
  pub(super) nodes: Vec<Option<Node<'a>>>,
  pub(super) prefer_hanging: bool,
  pub(super) prefer_single_line: bool,
  pub(super) space_around: bool,
  pub(super) trailing_commas: TrailingCommas,
}

pub(super) fn gen_array_like_nodes<'a>(opts: GenArrayLikeNodesOptions<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let prefer_single_line = opts.prefer_single_line || context.take_prefer_single_line_value(&opts.node);
  let nodes = get_array_like_values(opts.node, opts.nodes, context);

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
  let range = opts.range;
  let force_use_new_lines = get_array_like_force_use_new_lines(range, &nodes, prefer_single_line, context);
  let mut items = PrintItems::new(context.arena);
  let first_member = nodes.first().map(|x| x.range());

  items.extend(gen_surrounded_by_tokens(
    |context| {
      gen_separated_values(
        GenSeparatedValuesParams {
          nodes,
          prefer_hanging,
          force_use_new_lines,
          blank_lines: ListBlankLines::Maintain,
          separator: trailing_commas.into(),
          single_line_options: SingleLineOptions {
            space_at_start: space_around,
            space_at_end: space_around,
            separator: Signal::SpaceOrNewLine.into_print_items(context.arena),
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
      range: Some(range),
      first_member,
      prefer_single_line_when_empty: true,
      allow_open_token_trailing_comments: true,
      single_line_space_around: false,
      comments_only_padding: BlockPaddingBlankLines::default(),
      keep_blank_lines_before_close_token_comments: true,
    },
    context,
  ));

  return items;

  fn allow_trailing_commas(nodes: &[NodeOrSeparator]) -> bool {
    if let Some(NodeOrSeparator::Node(last)) = nodes.last() {
      // this would be a syntax error
      if matches!(last.kind(), NodeKind::BindingRestElement | NodeKind::AssignmentTargetRest) {
        return false;
      }
    }
    true
  }
}

/// Gets the values of an array-like node with a separator in place of each hole (ex. `[a, , b]`).
pub(super) fn get_array_like_values<'a>(node: Node<'a>, nodes: Vec<Option<Node<'a>>>, context: &Context<'a>) -> Vec<NodeOrSeparator<'a>> {
  // the holes of an array expression are elisions
  let is_hole = |node: &Option<Node<'a>>| matches!(node, None | Some(Node::Elision(_)));
  if nodes.iter().any(is_hole) {
    let program = context.program;
    // the comma of a hole is the next token after the open bracket or the comma before it
    let mut previous_end = node.start() + 1;
    nodes
      .into_iter()
      .map(|value| match value {
        Some(value) if !matches!(value, Node::Elision(_)) => {
          // skip the comma that separates the value from what's after it
          previous_end = match value.next_token_fast(program) {
            Some(token) if token.kind == TokenKind::Comma => token.end(),
            _ => value.end(),
          };
          NodeOrSeparator::Node(value)
        }
        _ => {
          let comma = previous_end
            .next_token_fast(program)
            .filter(|token| token.kind == TokenKind::Comma)
            .expect("Expected to find the comma of the array hole.");
          previous_end = comma.end();
          NodeOrSeparator::Separator(comma)
        }
      })
      .collect::<Vec<_>>()
  } else {
    // faster
    nodes.into_iter().map(|n| NodeOrSeparator::Node(n.unwrap())).collect::<Vec<_>>()
  }
}

/// Gets if an array-like node is multi-line because of how it's written (ex. a newline after the open bracket).
pub(super) fn get_array_like_force_use_new_lines<'a>(
  range: SourceRange,
  nodes: &[NodeOrSeparator],
  prefer_single_line: bool,
  context: &mut Context<'a>,
) -> bool {
  if nodes.is_empty() {
    false
  } else if prefer_single_line {
    // if any comments exist on separate lines, then everything becomes multi-line
    has_any_node_comment_on_different_line(nodes, context)
  } else {
    // the range starts with the open bracket
    let open_bracket_range = SourceRange::new(range.start, range.start + 1);

    // todo: tests for this (ex. [\n,] -> [\n    ,\n])
    node_helpers::get_use_new_lines_for_nodes(&open_bracket_range, &nodes[0], context.program)
  }
}

pub(super) struct GenMemberedBodyOptions<'a, FGetForcedBlankLineCount>
where
  FGetForcedBlankLineCount: Fn(Node, Node, &mut Context<'a>) -> Option<usize>,
{
  pub(super) node: Node<'a>,
  pub(super) members: Vec<Node<'a>>,
  pub(super) start_header_lsil: Option<LineStartIndentLevel>,
  pub(super) brace_position: BracePosition,
  pub(super) block_padding: BlockPadding,
  /// Gets the number of blank lines to force between two members, or `None` to maintain them.
  pub(super) get_forced_blank_line_count: FGetForcedBlankLineCount,
  pub(super) separator: Separator,
  /// Whether to keep the blank lines before the comments after the last member's separator.
  pub(super) keep_blank_lines_before_close_token_comments: bool,
}

pub(super) fn gen_membered_body<'a, FGetForcedBlankLineCount>(
  opts: GenMemberedBodyOptions<'a, FGetForcedBlankLineCount>,
  context: &mut Context<'a>,
) -> PrintItems<'a>
where
  FGetForcedBlankLineCount: Fn(Node, Node, &mut Context<'a>) -> Option<usize>,
{
  let mut items = PrintItems::new(context.arena);
  let tokens = opts.node.tokens_fast(context.program);
  // the body is at the end of the node
  let close_brace_index = tokens.len().checked_sub(1).filter(|index| tokens[*index].kind == TokenKind::RCurly);
  let close_brace_token = close_brace_index.map(|index| &tokens[index]);
  // use the open brace that matches the close brace because the header can have the braces
  // of a type literal (ex. `class A extends B<{}> {`)
  let open_brace_token = close_brace_index
    .and_then(|index| get_matching_open_brace_index(tokens, index))
    .map(|index| &tokens[index]);
  // the parser may have recovered from a missing brace without reporting an error
  let (Some(open_brace_token), Some(close_brace_token)) = (open_brace_token, close_brace_token) else {
    context.diagnostics.push(GenerateDiagnostic {
      message: format!(
        "Expected to find the open and close brace tokens of the body at line {}",
        opts.node.start_line_fast(context.program) + 1
      ),
    });
    return items;
  };

  items.extend(gen_brace_separator(
    GenBraceSeparatorOptions {
      brace_position: opts.brace_position,
      open_brace_token: Some(open_brace_token),
      start_header_lsil: opts.start_header_lsil,
    },
    context,
  ));

  let get_forced_blank_line_count = opts.get_forced_blank_line_count;
  let separator = opts.separator;

  items.extend(gen_block(
    |members, context| {
      gen_members(
        GenMembersOptions {
          inner_range: SourceRange::new(open_brace_token.end(), close_brace_token.start()),
          items: members.into_iter().map(|node| (node, None)).collect(),
          should_use_space: None,
          should_use_new_line: None,
          get_forced_blank_line_count,
          separator,
          is_jsx_children: false,
        },
        context,
      )
    },
    GenBlockOptions {
      range: Some(SourceRange::new(open_brace_token.start(), close_brace_token.end())),
      children: opts.members,
      block_padding: opts.block_padding,
      keep_blank_lines_before_close_token_comments: opts.keep_blank_lines_before_close_token_comments,
    },
    context,
  ));

  items
}

pub(super) fn get_member_spacing_forced_blank_line_count(member_spacing: MemberSpacing) -> Option<usize> {
  match member_spacing {
    MemberSpacing::BlankLine => Some(1),
    MemberSpacing::NewLine => Some(0),
    MemberSpacing::Maintain => None,
  }
}

/// Gets the range of a statement including the decorators before `export` (ex. `@dec export class A {}`),
/// which come before the statement's start.
pub(crate) fn get_stmt_range_with_decorators(node: Node) -> SourceRange {
  let decorators = match node {
    Node::ExportDeclaration(decl) => match &decl.declaration {
      Declaration::ClassDeclaration(class_decl) => Some(&class_decl.decorators),
      _ => None,
    },
    Node::ExportDefaultDeclaration(decl) => match &decl.declaration {
      ExportDefaultDeclarationKind::ClassDeclaration(class_decl) | ExportDefaultDeclarationKind::ClassExpression(class_decl) => Some(&class_decl.decorators),
      _ => None,
    },
    _ => None,
  };
  let start = match decorators.and_then(|decorators| decorators.first()) {
    Some(decorator) if decorator.start() < node.start() => decorator.start(),
    _ => node.start(),
  };
  SourceRange::new(start, node.end())
}

/// Gets the number of blank lines to keep between two nodes, which is the
/// number in the source capped at the `maxBlankLines` configuration.
pub(super) fn get_maintained_blank_line_count<'a>(first: &impl SourceRanged, second: &impl SourceRanged, context: &Context<'a>) -> usize {
  cap_blank_line_count(node_helpers::get_separating_blank_line_count(first, second, context.program), context)
}

pub(super) fn cap_blank_line_count<'a>(count: usize, context: &Context<'a>) -> usize {
  count.min(context.config.max_blank_lines as usize)
}

pub(super) fn push_blank_lines<'a>(items: &mut PrintItems<'a>, count: usize) {
  for _ in 0..count {
    items.push_signal(Signal::NewLine);
  }
}

/// The number of blank lines to keep after the open brace and before the close brace of a block.
#[derive(Default, Clone, Copy)]
pub(super) struct BlockPaddingBlankLines {
  pub(super) start: usize,
  pub(super) end: usize,
}

impl BlockPaddingBlankLines {
  pub(super) fn is_empty(&self) -> bool {
    self.start == 0 && self.end == 0
  }
}

/// Generates blank lines that only apply when something was printed between the
/// line and columns (ex. a block that only has empty statements prints nothing).
pub(super) fn gen_blank_lines_if_has_content<'a>(arena: &'a Arena, count: usize, start_lc: LineAndColumn, end_lc: LineAndColumn) -> PrintItems<'a> {
  if count == 0 {
    return PrintItems::new(arena);
  }
  let mut blank_lines = PrintItems::new(arena);
  push_blank_lines(&mut blank_lines, count);
  if_true(
    "blankLinesIfHasContent",
    arena.alloc_resolver(move |context| condition_helpers::are_line_and_columns_not_equal(context, start_lc, end_lc)),
    blank_lines,
  )
  .into_print_items(arena)
}

pub(super) fn get_block_padding_blank_lines<'a>(
  open_brace: SourceRange,
  close_brace: SourceRange,
  block_padding: BlockPadding,
  context: &Context<'a>,
) -> BlockPaddingBlankLines {
  if block_padding == BlockPadding::Remove {
    return BlockPaddingBlankLines::default();
  }

  let program = context.program;
  let open_brace_line = open_brace.end_line_fast(program);
  // a line comment on the open brace's line stays on that line, but other comments move to the next line
  let first_comment_start_line = open_brace
    .end
    .trailing_comments_fast(program)
    .find(|c| c.kind == CommentKind::Block || c.start_line_fast(program) > open_brace_line)
    .map(|c| c.start_line_fast(program));
  let content_start_line = match first_comment_start_line {
    Some(line) => line,
    None => {
      let next_token_pos = context.token_finder.get_next_token_pos_after(&open_brace);
      if next_token_pos >= close_brace.start {
        return BlockPaddingBlankLines::default(); // nothing between the braces
      }
      next_token_pos.start_line_fast(program)
    }
  };
  let previous_token_end_line = context.token_finder.get_previous_token_end_before(&close_brace).end_line_fast(program);
  let content_end_line = match close_brace.start.leading_comments_fast(program).peek_last_comment() {
    Some(comment) => comment.end_line_fast(program).max(previous_token_end_line),
    None => previous_token_end_line,
  };
  let close_brace_start_line = close_brace.start_line_fast(program);

  match block_padding {
    BlockPadding::Remove => BlockPaddingBlankLines::default(),
    BlockPadding::Maintain => BlockPaddingBlankLines {
      start: cap_blank_line_count(content_start_line.saturating_sub(open_brace_line + 1), context),
      end: cap_blank_line_count(close_brace_start_line.saturating_sub(content_end_line + 1), context),
    },
    BlockPadding::Always => BlockPaddingBlankLines { start: 1, end: 1 },
  }
}

#[derive(Clone, Copy)]
pub(super) struct GenConditionalBodyPaddingConditions {
  pub(super) open_brace_condition_ref: ConditionReference,
  pub(super) newline_condition_ref: ConditionReference,
  pub(super) start_statements_lc: LineAndColumn,
  pub(super) end_statements_lc: LineAndColumn,
}

/// Generates the blank lines of a conditional body's padding, which only apply
/// when the body has braces, starts on a new line, and has statements or comments.
pub(super) fn gen_conditional_body_padding<'a>(arena: &'a Arena, count: usize, conditions: GenConditionalBodyPaddingConditions) -> PrintItems<'a> {
  if count == 0 {
    return PrintItems::new(arena);
  }
  let mut blank_lines = PrintItems::new(arena);
  push_blank_lines(&mut blank_lines, count);
  if_true(
    "conditionalBodyPadding",
    arena.alloc_resolver(move |context| {
      if !context.resolved_condition(&conditions.open_brace_condition_ref)? || !context.resolved_condition(&conditions.newline_condition_ref)? {
        return Some(false);
      }
      condition_helpers::are_line_and_columns_not_equal(context, conditions.start_statements_lc, conditions.end_statements_lc)
    }),
    blank_lines,
  )
  .into_print_items(arena)
}

/// Generates a list of statements, which starts with the directives (ex. `"use strict";`).
pub(super) fn gen_statements<'a>(inner_range: SourceRange, stmts: Vec<Node<'a>>, context: &mut Context<'a>) -> PrintItems<'a> {
  let blank_lines_after_imports = context.config.module_blank_lines_after_imports.map(|count| count as usize);
  let is_import_stmts = if blank_lines_after_imports.is_some() {
    stmts
      .iter()
      .filter(|stmt| !matches!(stmt, Node::EmptyStatement(_)))
      .map(|stmt| is_import_stmt(*stmt))
      .collect::<Vec<_>>()
  } else {
    Vec::new()
  };
  let stmt_groups = get_stmt_groups(stmts, context);
  let mut items = PrintItems::new(context.arena);
  let mut last_node: Option<SourceRange> = None;
  let stmt_group_len = stmt_groups.len();
  let mut is_after_import = false;
  // the index of the statement within the statements that aren't empty
  let mut stmt_index = 0;
  let mut previous_stmt_group_kind = None;

  for (stmt_group_index, stmt_group) in stmt_groups.into_iter().enumerate() {
    let mut has_stmt_group_comments = false;
    if stmt_group.kind == StmtGroupKind::Imports || stmt_group.kind == StmtGroupKind::Exports {
      // keep the leading comments of the stmt group on the same line
      let first_node = *stmt_group.nodes.first().unwrap();
      let mut comments = get_leading_comments_on_previous_lines(&first_node.start().range(), context);
      if stmt_group.subgroup_boundaries.is_some() {
        // Imports get reordered, so only the detached portion of the preamble
        // (e.g. a file header separated by a blank line) stays pinned here.
        // The attached portion is left for `gen_node` to emit inside the
        // import's own items so that it travels with it.
        comments.truncate(detached_preamble_len(&comments, first_node, context.program));
      }
      let last_comment = comments.iter().rfind(|c| !context.has_handled_comment(c)).map(|c| c.range());
      let forced_blank_line_count = blank_lines_after_imports.filter(|_| is_after_import && stmt_group.kind == StmtGroupKind::Exports);
      match forced_blank_line_count {
        Some(count) if last_comment.is_some() && last_node.is_some() => {
          items.push_signal(Signal::NewLine);
          push_blank_lines(&mut items, count);
          items.extend(gen_comments_as_statements(comments.into_iter(), None, context));
        }
        _ => {
          items.extend(gen_comments_as_statements(
            comments.into_iter(),
            last_node.as_ref().map(|x| x as &SourceRange),
            context,
          ));
        }
      }
      if last_comment.is_some() {
        is_after_import = false;
        has_stmt_group_comments = true;
      }
      last_node = last_comment.or(last_node);
    }
    // the blank line between groups of imports or exports separates them when sorting, so keep it
    let keep_blank_line_before_group = !has_stmt_group_comments
      && matches!(stmt_group.kind, StmtGroupKind::Imports | StmtGroupKind::Exports)
      && previous_stmt_group_kind == Some(stmt_group.kind);
    previous_stmt_group_kind = Some(stmt_group.kind);

    let nodes_len = stmt_group.nodes.len();
    let mut generated_nodes = BumpVec::with_capacity_in(nodes_len, context.arena.bump());
    let mut generated_line_separators = utils::VecMap::with_capacity(nodes_len);
    let has_subgroup_boundaries = stmt_group.subgroup_boundaries.is_some();
    let sorted_indexes = match stmt_group.sorted_indexes {
      Some(indexes) => Some(indexes),
      None => get_node_sorter(stmt_group.kind, context).map(|sorter| sorter.get_sorted_indexes(stmt_group.nodes.iter().map(|n| Some(*n)), context.program)),
    };
    let subgroup_boundaries = stmt_group.subgroup_boundaries.unwrap_or_default();
    // the separator computed at index i ends up before the node at output position i
    // once `sort_by_sorted_indexes` runs, so the modes that keep the blank lines in
    // the source look them up between the nodes that are adjacent in the output
    let source_blank_line_counts_by_output_index: Vec<usize> = if has_subgroup_boundaries
      && matches!(
        context.config.module_import_groups_newlines_between,
        NewlinesBetween::AlwaysAndInsideGroups | NewlinesBetween::Ignore
      ) {
      let indexes = sorted_indexes.as_ref().unwrap();
      let mut source_index_by_output_index = vec![0; nodes_len];
      for source_index in 0..nodes_len {
        source_index_by_output_index[*indexes.get(source_index).unwrap_or(&source_index)] = source_index;
      }
      (0..nodes_len)
        .map(|output_index| {
          let previous_source_index = output_index.checked_sub(1).map(|index| source_index_by_output_index[index]);
          let source_index = source_index_by_output_index[output_index];
          // only keep a blank line when the nodes around it are still next to each other
          match previous_source_index {
            Some(previous_source_index) if previous_source_index + 1 == source_index => get_maintained_blank_line_count(
              &stmt_group.nodes[previous_source_index],
              &get_stmt_range_with_decorators(stmt_group.nodes[source_index]),
              context,
            ),
            _ => 0,
          }
        })
        .collect()
    } else {
      Vec::new()
    };
    for (i, node) in stmt_group.nodes.into_iter().enumerate() {
      let is_empty_stmt = matches!(node, Node::EmptyStatement(_));
      if !is_empty_stmt {
        let is_import = is_import_stmt(node);
        let mut separator_items = PrintItems::new(context.arena);
        if let Some(last_node) = &last_node {
          separator_items.push_signal(Signal::NewLine);
          let mut blank_line_count = if has_subgroup_boundaries {
            get_import_group_blank_line_count(i, last_node, node, &subgroup_boundaries, &source_blank_line_counts_by_output_index, context)
          } else {
            match blank_lines_after_imports {
              Some(count) if is_after_import && !is_import => count,
              _ => get_maintained_blank_line_count(last_node, &get_stmt_range_with_decorators(node), context),
            }
          };
          if i == 0 && keep_blank_line_before_group {
            blank_line_count = blank_line_count.max(1);
          }
          push_blank_lines(&mut separator_items, blank_line_count);
          generated_line_separators.insert(i, separator_items);
        }
        // the forced blank lines after the imports go before the comments that are before the
        // semicolon on a later line, since those comments are with the next statement when formatted again
        let is_next_stmt_import = is_import_stmts.get(stmt_index + 1).copied();
        let blank_lines_before_comments = blank_lines_after_imports.filter(|_| is_import && is_next_stmt_import == Some(false));
        stmt_index += 1;

        let mut items = PrintItems::new(context.arena);
        let end_ln = LineNumber::new("endStatement");
        context.end_statement_or_member_lns.push(end_ln);
        let generated =
          gen_node_with_comments_before_semicolon_on_later_line(node, |node, context| gen_node(node, context), blank_lines_before_comments, context);
        // the blank lines were already added before the comments
        is_after_import = is_import && !(generated.comment_range.is_some() && blank_lines_before_comments.is_some());
        items.extend(generated.node_items);
        items.push_info(end_ln);
        items.extend(generated.comment_items);
        generated_nodes.push(items);
        context.end_statement_or_member_lns.pop();

        last_node = Some(generated.last_range);
      } else {
        let generated = gen_empty_stmt_or_member_comments(node, last_node.as_ref(), context);
        generated_nodes.push(generated.items);
        if generated.has_leading_comments {
          last_node = Some(node.range());
        }

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

  fn get_node_sorter(group_kind: StmtGroupKind, context: &Context) -> Option<NodeSorter> {
    match group_kind {
      StmtGroupKind::Imports => get_node_sorter_from_order(context.config.module_sort_import_declarations, NamedTypeImportsExportsOrder::Inline),
      StmtGroupKind::Exports => get_node_sorter_from_order(context.config.module_sort_export_declarations, NamedTypeImportsExportsOrder::Inline),
      StmtGroupKind::Other => None,
    }
  }
}

pub(super) struct GeneratedNodeWithCommentsAfter<'a> {
  pub(super) node_items: PrintItems<'a>,
  pub(super) comment_items: PrintItems<'a>,
  /// The range of the last comment after the node, if any.
  pub(super) comment_range: Option<SourceRange>,
  /// The range of what was generated last, which is used for the blank line checks.
  pub(super) last_range: SourceRange,
}

impl<'a> GeneratedNodeWithCommentsAfter<'a> {
  pub(super) fn new(node_items: PrintItems<'a>, node: Node) -> Self {
    Self {
      comment_items: PrintItems::new(node_items.arena()),
      node_items,
      comment_range: None,
      last_range: node.range(),
    }
  }
}

/// Generates a statement or member that may end with a semicolon written at the start of the
/// next line (ex. `;(a)()`). The comments before that semicolon are on the lines after the rest
/// of the node, so they're generated after it instead of within it.
pub(super) fn gen_node_with_comments_before_semicolon_on_later_line<'a>(
  node: Node<'a>,
  gen_inner: impl FnOnce(Node<'a>, &mut Context<'a>) -> PrintItems<'a>,
  // the number of blank lines to force before the comments instead of maintaining them
  blank_lines_before_comments: Option<usize>,
  context: &mut Context<'a>,
) -> GeneratedNodeWithCommentsAfter<'a> {
  let program = context.program;
  let Some((previous_token, semicolon)) = get_semicolon_on_later_line(node, program) else {
    let node_items = gen_inner(node, context);
    return GeneratedNodeWithCommentsAfter::new(node_items, node);
  };
  let previous_token_end_line = previous_token.end_line_fast(program);
  let comments = semicolon
    .start()
    .leading_comments_fast(program)
    .filter(|c| c.start_line_fast(program) > previous_token_end_line && !context.has_handled_comment(c))
    .collect::<Vec<_>>();
  for comment in &comments {
    context.mark_comment_handled(comment);
  }
  let node_items = gen_inner(node, context);
  for comment in &comments {
    context.unmark_comment_handled(comment);
  }
  let node_range = SourceRange::new(node.start(), previous_token.end());
  let comment_range = comments.last().map(|c| c.range());
  let comment_items = match blank_lines_before_comments {
    Some(count) if !comments.is_empty() => {
      let mut items = PrintItems::new(context.arena);
      items.push_signal(Signal::NewLine);
      push_blank_lines(&mut items, count);
      items.extend(gen_comments_as_statements(comments.into_iter(), None, context));
      items
    }
    _ => gen_comments_as_statements(comments.into_iter(), Some(&node_range), context),
  };
  // the semicolon is the start of the next statement's line (ex. `;(a)()`), unless nothing
  // comes after it on its line, which is then the last line of this node
  let is_semicolon_on_own_line = semicolon
    .next_token_fast(program)
    .is_none_or(|token| token.start_line_fast(program) > semicolon.end_line_fast(program));
  GeneratedNodeWithCommentsAfter {
    node_items,
    comment_items,
    comment_range,
    last_range: if is_semicolon_on_own_line {
      SourceRange::new(node.start(), semicolon.end())
    } else {
      comment_range.unwrap_or(node_range)
    },
  }
}

/// Gets the semicolon that ends the node when it's on a later line than the rest of
/// the node, along with the token before it.
pub(crate) fn get_semicolon_on_later_line<'a>(node: Node<'a>, program: ProgramInfo<'a>) -> Option<(&'a TokenAndSpan, &'a TokenAndSpan)> {
  let [.., previous_token, semicolon] = node.tokens_fast(program) else {
    return None;
  };
  (semicolon.kind == TokenKind::Semicolon && previous_token.end_line_fast(program) < semicolon.start_line_fast(program)).then_some((previous_token, semicolon))
}

pub(super) struct GeneratedEmptyStmtOrMemberComments<'a> {
  pub(super) items: PrintItems<'a>,
  /// Whether it has leading comments, which means what comes next is separated from the empty
  /// statement rather than from what's before it.
  pub(super) has_leading_comments: bool,
}

/// Generates the comments of an empty statement or class member, which itself isn't generated. The
/// leading comments go on their own lines after what's before it (ex. the comment in
/// `a();\n// c\n;b();`) rather than being joined to it.
pub(super) fn gen_empty_stmt_or_member_comments<'a>(
  node: Node,
  previous_range: Option<&SourceRange>,
  context: &mut Context<'a>,
) -> GeneratedEmptyStmtOrMemberComments<'a> {
  let mut items = PrintItems::new(context.arena);
  let leading_comments = node.leading_comments_fast(context.program);
  let has_leading_comments = leading_comments.clone().any(|comment| !context.has_handled_comment(comment));
  items.extend(gen_comments_as_statements(leading_comments.clone(), previous_range, context));
  let trailing_comments = get_trailing_comments_same_line(&node.range(), node.trailing_comments_fast(context.program), context);
  if !trailing_comments.is_empty() {
    if !leading_comments.is_empty() {
      items.push_signal(Signal::NewLine);
    }
    items.extend(gen_comment_collection(trailing_comments.into_iter(), None, None, context));
  }

  GeneratedEmptyStmtOrMemberComments { items, has_leading_comments }
}

/// Gets the number of blank lines before the import at the index of a run of
/// imports that's reordered by `module.importGroups`.
pub(super) fn get_import_group_blank_line_count<'a>(
  index: usize,
  last_node: &SourceRange,
  node: Node<'a>,
  subgroup_boundaries: &[usize],
  source_blank_line_counts_by_output_index: &[usize],
  context: &Context<'a>,
) -> usize {
  let newlines_between = context.config.module_import_groups_newlines_between;
  if index == 0 {
    // this separates the run from whatever is before it (ex. a statement), which
    // the modes don't govern except for forcing a blank line in the "always" modes
    let count = get_maintained_blank_line_count(last_node, &node, context);
    return match newlines_between {
      NewlinesBetween::Always | NewlinesBetween::AlwaysAndInsideGroups => count.max(1),
      NewlinesBetween::Never | NewlinesBetween::Ignore => count,
    };
  }
  let is_at_boundary = subgroup_boundaries.binary_search(&index).is_ok();
  let source_count = source_blank_line_counts_by_output_index.get(index).copied().unwrap_or(0);
  match newlines_between {
    NewlinesBetween::Always => usize::from(is_at_boundary),
    NewlinesBetween::AlwaysAndInsideGroups if is_at_boundary => source_count.max(1),
    NewlinesBetween::AlwaysAndInsideGroups | NewlinesBetween::Ignore => source_count,
    NewlinesBetween::Never => 0,
  }
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub(crate) enum StmtGroupKind {
  Imports,
  Exports,
  Other,
}

pub(super) struct StmtGroup<'a> {
  pub(super) kind: StmtGroupKind,
  pub(super) nodes: BumpVec<'a, Node<'a>>,
  /// Output positions marking the start of each subgroup, which is where a
  /// blank line gets forced. Only Some for `StmtGroupKind::Imports` when
  /// `module.importGroups` is non-empty.
  pub(super) subgroup_boundaries: Option<Vec<usize>>,
  /// Source index -> output index for the import-group reordering. Takes the
  /// place of the sorter that `gen_statements` would otherwise resolve.
  pub(super) sorted_indexes: Option<utils::VecMap<usize>>,
}

pub(super) fn is_import_stmt(node: Node) -> bool {
  match node {
    Node::ImportDeclaration(_) | Node::TSImportEqualsDeclaration(_) => true,
    // `export import a = b.c;`, which swc represented as an import equals declaration
    Node::ExportDeclaration(decl) => matches!(decl.declaration, Declaration::TSImportEqualsDeclaration(_)),
    _ => false,
  }
}

/// Gets the kind of group a statement is sorted in, which is `Other` for a statement formatting
/// doesn't sort.
pub(crate) fn get_stmt_group_kind(stmt: Node, program: ProgramInfo, config: &Configuration) -> StmtGroupKind {
  let stmt_group_kind = match stmt {
    Node::ImportDeclaration(decl) if decl.specifiers.as_ref().is_some_and(|specifiers| !specifiers.is_empty()) => StmtGroupKind::Imports,
    Node::ExportAllDeclaration(_) => StmtGroupKind::Exports,
    Node::ExportFromDeclaration(_) => StmtGroupKind::Exports,
    _ => StmtGroupKind::Other,
  };
  // an ignored declaration is a barrier: it stays where it is and ends the
  // run around it, the same way a side-effect import does
  let is_ignored = || {
    stmt
      .leading_comments_fast(program)
      .any(|comment| ir_helpers::text_has_dprint_ignore(comment.text, &config.ignore_node_comment_text))
  };
  if stmt_group_kind != StmtGroupKind::Other && is_ignored() {
    StmtGroupKind::Other
  } else {
    stmt_group_kind
  }
}

pub(super) fn get_stmt_groups<'a>(stmts: Vec<Node<'a>>, context: &mut Context<'a>) -> BumpVec<'a, StmtGroup<'a>> {
  let mut groups = BumpVec::new_in(context.arena.bump());
  let mut current_group: Option<StmtGroup> = None;
  let mut previous_last_end_line: Option<usize> = None;

  for stmt in stmts {
    let last_end_line = previous_last_end_line.take();
    let stmt_group_kind = get_stmt_group_kind(stmt, context.program, context.config);
    previous_last_end_line = match stmt_group_kind {
      StmtGroupKind::Imports | StmtGroupKind::Exports => Some(stmt.end_line_fast(context.program)),
      StmtGroupKind::Other => None,
    };

    if let Some(group) = current_group.as_mut() {
      let is_same_group = group.kind == stmt_group_kind
        && match last_end_line {
          _ if stmt_group_kind == StmtGroupKind::Other => true,
          None => true,
          Some(last_end_line) if stmt_group_kind == StmtGroupKind::Imports && context.resolved_import_groups.is_some() => {
            // when grouping, blank lines between imports are style rather than
            // structure, so the run only ends at a comment on its own line that's
            // detached from the import by a blank line (ex. a section header),
            // which stays where it is, while attached comments travel with the import
            let comments = stmt
              .leading_comments_fast(context.program)
              .filter(|comment| comment.start_line_fast(context.program) > last_end_line)
              .collect::<Vec<_>>();
            comments.is_empty() || detached_preamble_len(&comments, stmt, context.program) == 0
          }
          Some(last_end_line) => last_end_line + 1 >= stmt.start_line_fast(context.program),
        };
      if is_same_group {
        group.nodes.push(stmt);
      } else {
        groups.push(current_group.take().unwrap());
        current_group = Some(StmtGroup {
          kind: stmt_group_kind,
          nodes: bumpalo::vec![in context.arena.bump(); stmt],
          subgroup_boundaries: None,
          sorted_indexes: None,
        })
      }
    } else {
      current_group = Some(StmtGroup {
        kind: stmt_group_kind,
        nodes: bumpalo::vec![in context.arena.bump(); stmt],
        subgroup_boundaries: None,
        sorted_indexes: None,
      });
    }
  }

  if let Some(current_group) = current_group {
    groups.push(current_group);
  }

  if let Some(resolved) = context.resolved_import_groups {
    for g in groups.iter_mut() {
      if g.kind != StmtGroupKind::Imports {
        continue;
      }
      let classified: Vec<(usize, usize)> = g
        .nodes
        .iter()
        .enumerate()
        .map(|(i, node)| {
          // an Imports group only ever holds import declarations
          let Node::ImportDeclaration(d) = node else { unreachable!() };
          let idx = classify_import(
            d.source.value.as_str().unwrap_or(""),
            d.import_kind.is_type(),
            context.config.module_type_imports,
            resolved,
          );
          (idx, i)
        })
        .collect();
      let sorter = get_node_sorter_from_order(context.config.module_sort_import_declarations, NamedTypeImportsExportsOrder::Inline);
      let sort_keys = sorter.map(|sorter| {
        (
          sorter,
          g.nodes.iter().map(|node| sorter.get_node_sort_key(*node, context.program)).collect::<Vec<_>>(),
        )
      });
      let (ordered, boundaries) = partition_indices(&classified, resolved.groups.len(), |a_orig: usize, b_orig: usize| -> std::cmp::Ordering {
        if let Some((sorter, sort_keys)) = &sort_keys {
          sorter.cmp_node_sort_keys(&sort_keys[a_orig], &sort_keys[b_orig])
        } else {
          a_orig.cmp(&b_orig)
        }
      });

      // Hand the reordering to the same mechanism the plain sorter uses: it
      // permutes the *generated* items, so each import keeps the comments it
      // owns and nodes are still generated in source order.
      let mut sorted_indexes = utils::VecMap::with_capacity(ordered.len());
      for (new_index, old_index) in ordered.into_iter().enumerate() {
        sorted_indexes.insert(old_index, new_index);
      }
      g.subgroup_boundaries = Some(boundaries);
      g.sorted_indexes = Some(sorted_indexes);
    }
  }
  groups
}

pub(super) struct GenMembersOptions<'a, FGetForcedBlankLineCount>
where
  FGetForcedBlankLineCount: Fn(Node<'a>, Node<'a>, &mut Context<'a>) -> Option<usize>,
{
  pub(super) inner_range: SourceRange,
  pub(super) items: Vec<(Node<'a>, Option<PrintItems<'a>>)>,
  pub(super) should_use_space: Option<Box<dyn Fn(Node<'a>, Node<'a>, &mut Context<'a>) -> bool>>, // todo: Remove putting functions on heap by using type parameters?
  pub(super) should_use_new_line: Option<Box<dyn Fn(Node<'a>, Node<'a>, &mut Context<'a>) -> bool>>,
  /// Gets the number of blank lines to force between two members, or `None` to maintain them.
  pub(super) get_forced_blank_line_count: FGetForcedBlankLineCount,
  pub(super) separator: Separator,
  pub(super) is_jsx_children: bool,
}

pub(super) fn gen_members<'a, FGetForcedBlankLineCount>(opts: GenMembersOptions<'a, FGetForcedBlankLineCount>, context: &mut Context<'a>) -> PrintItems<'a>
where
  FGetForcedBlankLineCount: Fn(Node<'a>, Node<'a>, &mut Context<'a>) -> Option<usize>,
{
  let mut last_node: Option<Node> = None;
  // where the comments that were before a semicolon on a later line end, which are generated after
  // the member (it's the semicolon when it's on its own line)
  let mut last_comment_range: Option<SourceRange> = None;
  // whether the forced blank lines to the next member were added before those comments
  let mut forced_blank_lines_before_comments = false;
  let mut items = PrintItems::new(context.arena);
  let children_len = opts.items.len();

  let mut member_items = opts.items.into_iter().enumerate().peekable();

  while let Some((i, (node, optional_print_items))) = member_items.next() {
    // class declarations may have empty statements
    let is_empty_stmt = matches!(node, Node::EmptyStatement(_));
    if !is_empty_stmt {
      if let Some(last_node) = last_node {
        if is_jsx_text_generated_as_is(node, context) {
          // ignore
        } else if should_use_new_line(&opts.should_use_new_line, last_node, node, context) {
          items.push_signal(Signal::NewLine);

          let blank_line_count = if forced_blank_lines_before_comments {
            0 // already added before the comments after the last member
          } else {
            match (opts.get_forced_blank_line_count)(last_node, node, context) {
              Some(count) => count,
              None => match &last_comment_range {
                Some(last_comment_range) => get_maintained_blank_line_count(last_comment_range, &node, context),
                None => get_maintained_blank_line_count(&last_node, &node, context),
              },
            }
          };
          push_blank_lines(&mut items, blank_line_count);
        } else if let Some(should_use_space) = &opts.should_use_space
          && should_use_space(last_node, node, context)
        {
          if opts.is_jsx_children {
            items.extend(jsx_space_separator(last_node, node, context))
          } else {
            items.push_signal(Signal::SpaceOrNewLine);
          }
        }
      }

      let next_node = member_items.peek().map(|(_, (n, _))| *n);
      // the forced blank lines go before the comments that are before the semicolon on a
      // later line, since those comments are with the next member when formatted again
      let blank_lines_before_comments = match next_node {
        Some(next_node) if !matches!(next_node, Node::EmptyStatement(_)) => (opts.get_forced_blank_line_count)(node, next_node, context),
        _ => None,
      };
      let end_ln = LineNumber::new("endMember");
      context.end_statement_or_member_lns.push(end_ln);
      // the same goes for the comments that are before the comma on a later line
      let previous_list_blank_lines = blank_lines_before_comments.map(|count| {
        (
          std::mem::replace(&mut context.allow_blank_lines_in_list, false),
          std::mem::replace(&mut context.force_blank_line_in_list, count > 0),
        )
      });
      let generated = match optional_print_items {
        Some(print_items) => GeneratedNodeWithCommentsAfter::new(print_items, node),
        None => gen_node_with_comments_before_semicolon_on_later_line(
          node,
          |node, context| {
            if opts.separator.is_none() || is_ignore_jsx_expr_container(node, context) && next_node.map(|n| n.kind() == NodeKind::JSXText).unwrap_or(false) {
              gen_node(node, context)
            } else {
              let generated_separator = get_generated_separator(context.arena, &opts.separator, i == children_len - 1, &condition_resolvers::true_resolver());
              gen_node_with_separator(node, generated_separator, context)
            }
          },
          blank_lines_before_comments,
          context,
        ),
      };
      if let Some((allow_blank_lines, force_blank_line)) = previous_list_blank_lines {
        context.allow_blank_lines_in_list = allow_blank_lines;
        context.force_blank_line_in_list = force_blank_line;
      }
      items.extend(generated.node_items);
      items.push_info(end_ln);
      items.extend(generated.comment_items);
      context.end_statement_or_member_lns.pop();

      forced_blank_lines_before_comments =
        blank_lines_before_comments.is_some() && (generated.comment_range.is_some() || has_comment_on_later_line_before_comma(node, context.program));
      last_comment_range = generated.comment_range.map(|_| generated.last_range);
      last_node = Some(node);
    } else {
      let previous_range = last_comment_range.or_else(|| last_node.map(|node| node.range()));
      let generated = gen_empty_stmt_or_member_comments(node, previous_range.as_ref(), context);
      items.extend(generated.items);
      if generated.has_leading_comments {
        last_comment_range = Some(node.range());
      }

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

pub(super) struct GenParametersOrArgumentsOptions<'a, F>
where
  F: FnOnce(&mut Context<'a>) -> Option<PrintItems<'a>>,
{
  pub(super) node: Node<'a>,
  pub(super) range: Option<SourceRange>,
  pub(super) nodes: BumpVec<'a, Node<'a>>,
  pub(super) custom_close_paren: F,
  pub(super) is_parameters: bool,
}

pub(super) fn gen_parameters_or_arguments<'a, F>(opts: GenParametersOrArgumentsOptions<'a, F>, context: &mut Context<'a>) -> PrintItems<'a>
where
  F: FnOnce(&mut Context<'a>) -> Option<PrintItems<'a>>,
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
      let mut items = PrintItems::new(context.arena);

      if !force_use_new_lines && nodes.len() == 1 && is_arrow_function_with_expr_body(nodes[0]) {
        let start_ln = LineNumber::new("startArrow");
        let start_lsil = LineStartIndentLevel::new("startArrow");
        let node = nodes.into_iter().next().unwrap();
        let comma_token = context.token_finder.get_next_token_if_comma(&node);
        // like the other arguments, the comma is used whether or not the arrow function's body goes
        // on the next line, and it goes before the comments after it
        let generated_node = if trailing_commas == TrailingCommas::Always {
          gen_node_with_separator(node, sc!(",").into_print_items(context.arena), context)
        } else {
          gen_node(node, context)
        };

        items.push_info(start_ln);
        items.push_info(start_lsil);
        items.push_signal(Signal::PossibleNewLine);
        if space_around {
          items.push_signal(Signal::SpaceIfNotTrailing);
        }
        items.push_condition(conditions::indent_if_start_of_line(generated_node));
        // the comma may disappear, so generate any trailing comments on the same line
        if let Some(comma_token) = comma_token {
          items.extend(gen_trailing_comments_same_line(&comma_token.range(), context));
        }
        items.push_condition(if_true_or(
          "isDifferentLineAndStartLineIndentation",
          context.arena.alloc_resolver(move |context| {
            let start_ln = context.resolved_line_number(start_ln)?;
            let start_lsil = context.resolved_line_start_indent_level(start_lsil)?;
            let is_different_line = start_ln != context.writer_info.line_number;
            let is_different_start_line_indentation = start_lsil != context.writer_info.line_start_indent_level;
            Some(is_different_line && is_different_start_line_indentation)
          }),
          Signal::NewLine.into_print_items(context.arena),
          if space_around {
            Signal::SpaceIfNotTrailing.into_print_items(context.arena)
          } else {
            PrintItems::new(context.arena)
          },
        ));
      } else {
        let last_comma_token = nodes.last().and_then(|n| context.token_finder.get_next_token_if_comma(n));
        items.extend(gen_separated_values(
          GenSeparatedValuesParams {
            nodes: nodes.into_iter().map(NodeOrSeparator::Node).collect(),
            prefer_hanging,
            force_use_new_lines,
            blank_lines: ListBlankLines::Remove,
            separator: trailing_commas.into(),
            single_line_options: SingleLineOptions {
              space_at_start: space_around,
              space_at_end: space_around,
              separator: Signal::SpaceOrNewLine.into_print_items(context.arena),
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
      comments_only_padding: BlockPaddingBlankLines::default(),
      // the blank lines between the values are removed
      keep_blank_lines_before_close_token_comments: false,
    },
    context,
  );

  fn get_trailing_commas<'a>(node: Node, nodes: &[Node], is_parameters: bool, context: &mut Context<'a>) -> TrailingCommas {
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
      matches!(node, Node::ImportExpression(_))
    }

    fn is_param_rest_pat(param: Node) -> bool {
      // the rest parameter of the formal parameters (ex. `...a: string[]`) or a rest pattern
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
      Some(token) if token.kind == TokenKind::Comma => token.trailing_comments_fast(program).is_empty(),
      _ => true,
    }
  }
}

pub(super) struct GenCloseParenWithTypeOptions<'a> {
  pub(super) start_lsil: LineStartIndentLevel,
  pub(super) type_node: Option<Node<'a>>,
  pub(super) type_node_separator: Option<PrintItems<'a>>,
  pub(super) param_count: usize,
}

pub(super) fn gen_close_paren_with_type<'a>(opts: GenCloseParenWithTypeOptions<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
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
  let mut items = PrintItems::new(context.arena);

  items.push_condition(if_true(
    "newLineIfHeaderHangingAndTypeNodeMultipleLines",
    context.arena.alloc_resolver(move |context| {
      if !has_type_node || context.writer_info.is_start_of_line() {
        return Some(false);
      }

      if let Some(is_hanging) = condition_helpers::is_hanging(context, start_lsil, None)
        && let Some(is_multiple_lines) = condition_helpers::is_multiple_lines(context, type_node_start_ln, type_node_end_ln)
      {
        return Some(is_hanging && is_multiple_lines);
      }
      None
    }),
    Signal::NewLine.into_print_items(context.arena),
  ));
  items.push_sc(sc!(")"));
  items.extend(generated_type_node);
  return items;

  fn gen_type_node<'a>(
    type_node: Option<Node<'a>>,
    type_node_separator: Option<PrintItems<'a>>,
    type_node_start_ln: LineNumber,
    type_node_end_ln: LineNumber,
    param_count: usize,
    context: &mut Context<'a>,
  ) -> PrintItems<'a> {
    let mut items = PrintItems::new(context.arena);
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

    fn get_use_new_line_group<'a>(param_count: usize, type_node: Node, context: &mut Context<'a>) -> bool {
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
pub(super) enum SeparatorValue {
  Semicolon(Semicolons),
  Comma(TrailingCommas),
}

pub(super) struct Separator {
  pub(super) single_line: Option<SeparatorValue>,
  pub(super) multi_line: Option<SeparatorValue>,
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

impl From<Semicolons> for Separator {
  fn from(value: Semicolons) -> Separator {
    Separator {
      single_line: Some(SeparatorValue::Semicolon(value)),
      multi_line: Some(SeparatorValue::Semicolon(value)),
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

pub(super) struct GenSeparatedValuesParams<'a> {
  pub(super) nodes: Vec<NodeOrSeparator<'a>>,
  pub(super) prefer_hanging: bool,
  pub(super) force_use_new_lines: bool,
  pub(super) blank_lines: ListBlankLines,
  pub(super) separator: Separator,
  pub(super) single_line_options: ir_helpers::SingleLineOptions<'a>,
  pub(super) multi_line_options: ir_helpers::MultiLineOptions<'a>,
  pub(super) force_possible_newline_at_start: bool,
  pub(super) node_sorter: Option<NodeSorter>,
}

/// How to handle the blank lines between the values of a list.
#[derive(Clone, Copy, PartialEq)]
pub(super) enum ListBlankLines {
  /// Removes the blank lines.
  Remove,
  /// Maintains the blank lines up to the `maxBlankLines` configuration.
  Maintain,
  /// Forces a blank line between each value when the list is multi-line.
  Force,
}

impl From<MemberSpacing> for ListBlankLines {
  fn from(member_spacing: MemberSpacing) -> Self {
    match member_spacing {
      MemberSpacing::Maintain => ListBlankLines::Maintain,
      MemberSpacing::NewLine => ListBlankLines::Remove,
      MemberSpacing::BlankLine => ListBlankLines::Force,
    }
  }
}

pub(super) enum NodeOrSeparator<'a> {
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
pub(super) fn gen_separated_values<'a>(opts: GenSeparatedValuesParams<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  gen_separated_values_with_result(opts, context).items
}

pub(super) fn gen_separated_values_with_result<'a>(opts: GenSeparatedValuesParams<'a>, context: &mut Context<'a>) -> GenSeparatedValuesResult<'a> {
  let nodes = opts.nodes;
  let separator = opts.separator;
  let indent_width = context.config.indent_width;
  let blank_lines = opts.blank_lines;
  let max_blank_lines = match blank_lines {
    ListBlankLines::Remove => 0,
    ListBlankLines::Maintain => context.config.max_blank_lines,
    ListBlankLines::Force => 1,
  };
  let node_sorter = opts.node_sorter;

  // would need to make this take into account the new position of the nodes
  #[cfg(debug_assertions)]
  if node_sorter.is_some() && blank_lines == ListBlankLines::Maintain {
    panic!("Not implemented scenario. Cannot computed lines span and allow blank lines");
  }
  let sorted_indexes = node_sorter.map(|sorter| sorter.get_sorted_indexes(nodes.iter().map(|d| d.as_node()), context.program));

  return ir_helpers::gen_separated_values(
    |is_multi_line_or_hanging_ref| {
      let is_multi_line_or_hanging = is_multi_line_or_hanging_ref.create_resolver(context.arena);
      let nodes_count = nodes.len();
      let disallow_inline_multi_line = get_disallow_inline_multi_line_for_neighbors(&nodes, context);
      // when this breaks a list that was hugged, the values that caused it go on one line when they fit
      // instead of keeping the newline after their open token from the hugging (ex. `[{ a }, {\n  b,\n}]`)
      let prefer_single_line_for_neighbors = disallow_inline_multi_line.contains(&true) && !is_first_value_on_new_line(&nodes, context);
      let mut generated_nodes = BumpVec::with_capacity_in(nodes_count, context.arena.bump());
      let mut forced_lines_span_line = 0;

      for (i, value) in nodes.into_iter().enumerate() {
        let node_index = match &sorted_indexes {
          Some(old_to_new_index) => *old_to_new_index.get(i).unwrap(),
          None => i,
        };
        let (allow_inline_multi_line, allow_inline_single_line) = if let NodeOrSeparator::Node(value) = value {
          let is_last_value = node_index + 1 == nodes_count; // allow the last node to be single line
          (
            !disallow_inline_multi_line[i] && allows_inline_multi_line(value, context, nodes_count > 1),
            is_last_value,
          )
        } else {
          (false, false)
        };
        let lines_span = match blank_lines {
          ListBlankLines::Remove => None,
          ListBlankLines::Maintain => value.as_node().map(|x| ir_helpers::LinesSpan {
            start_line: x.start_line_with_comments(context),
            end_line: x.end_line_with_comments(context),
          }),
          // a span that's one blank line after the previous value's, unless the blank line
          // was already added before the comments after the previous value's comma
          ListBlankLines::Force => value.as_node().map(|node| {
            let start_line = forced_lines_span_line;
            forced_lines_span_line += if has_comment_on_later_line_before_comma(node, context.program) {
              1
            } else {
              2
            };
            ir_helpers::LinesSpan {
              start_line,
              end_line: start_line,
            }
          }),
        };

        let previous_prefer_single_line_value_start = match value {
          NodeOrSeparator::Node(node) if prefer_single_line_for_neighbors && disallow_inline_multi_line[i] => {
            context.prefer_single_line_value_start.replace(get_inner_value(node).start())
          }
          _ => context.prefer_single_line_value_start,
        };
        let items = if separator.is_none() {
          if let NodeOrSeparator::Node(value) = value {
            gen_node(value, context)
          } else {
            panic!("Unsupported scenario.")
          }
        } else {
          let generated_separator = get_generated_separator(context.arena, &separator, node_index == nodes_count - 1, &is_multi_line_or_hanging);
          match value {
            NodeOrSeparator::Node(value) => {
              let previous_allow_blank_lines = std::mem::replace(&mut context.allow_blank_lines_in_list, blank_lines != ListBlankLines::Remove);
              let previous_force_blank_line = std::mem::replace(&mut context.force_blank_line_in_list, blank_lines == ListBlankLines::Force);
              let items = gen_node_with_separator(value, generated_separator, context);
              context.allow_blank_lines_in_list = previous_allow_blank_lines;
              context.force_blank_line_in_list = previous_force_blank_line;
              items
            }
            NodeOrSeparator::Separator(separator_token) => {
              let mut items = PrintItems::new(context.arena);
              // don't use gen_leading_comments here because we don't want a space between the block comment and separator (comma)
              let leading_comments = separator_token.leading_comments_fast(context.program);
              items.extend(gen_comment_collection(leading_comments, None, Some(&separator_token.range()), context));
              items.extend(generated_separator);
              items.extend(gen_trailing_comments_same_line(&separator_token.range(), context));
              items
            }
          }
        };
        context.prefer_single_line_value_start = previous_prefer_single_line_value_start;

        let use_new_line_group = match value {
          // Prefer going inline multi-line for certain expressions in arguments
          // when initially single line.
          // Example: call({\n}) instead of call(\n  {\n  }\n)
          NodeOrSeparator::Node(node) if is_arg_or_array_element(node, context.program) => match get_spread_argument_or_node(node) {
            Node::ObjectExpression(_) | Node::ArrayExpression(_) => false,
            // the same goes for a last argument that's an arrow function whose body breaks inside its parens
            // ex. call(a, () => (\n  <div />\n)) instead of call(\n  a,\n  () => (\n    <div />\n  ),\n)
            Node::ArrowFunctionExpression(arrow_expr) => !is_hugged_last_arg_arrow(arrow_expr, context),
            _ => true,
          },
          _ => true,
        };

        generated_nodes.push(ir_helpers::GeneratedValue {
          items: if use_new_line_group { ir_helpers::new_line_group(items) } else { items },
          lines_span,
          allow_inline_multi_line,
          allow_inline_single_line,
          is_known_multi_line: false,
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
      max_blank_lines,
      indent_width,
      single_line_options: opts.single_line_options,
      multi_line_options: opts.multi_line_options,
      force_possible_newline_at_start: opts.force_possible_newline_at_start,
    },
  );

  /// Gets if the value is a call argument or array element, which swc wrapped in an `ExprOrSpread`.
  fn is_arg_or_array_element<'a>(node: Node<'a>, program: ProgramInfo<'a>) -> bool {
    matches!(
      node.parent(program),
      Some(Node::CallExpression(_) | Node::NewExpression(_) | Node::ImportExpression(_) | Node::ArrayExpression(_))
    )
  }

  /// Gets the argument of a spread element (ex. `a` in `...a`) or otherwise the node.
  fn get_spread_argument_or_node(node: Node) -> Node {
    match node {
      Node::SpreadElement(spread) => (&spread.argument).into_node(),
      node => node,
    }
  }

  /// Gets if each value isn't allowed to be inline multi-line because it has a value beside it of
  /// the same kind, so the container breaks instead (ex. `[{ a }, { b }]` instead of expanding the
  /// last object). When every value with a same-kind neighbor is already multi-line because of how
  /// it's written, they're all allowed to be inline multi-line so they stay hugged
  /// (ex. `[{\n  a,\n}, {\n  b,\n}]`).
  fn get_disallow_inline_multi_line_for_neighbors<'a>(nodes: &[NodeOrSeparator<'a>], context: &mut Context<'a>) -> Vec<bool> {
    let has_same_kind_neighbor = get_has_same_kind_neighbor(nodes);
    let are_all_multi_line = nodes
      .iter()
      .zip(&has_same_kind_neighbor)
      .filter(|(_, has_neighbor)| **has_neighbor)
      .all(|(value, _)| value.as_node().is_some_and(|node| is_multi_line_as_written(node, context)));
    if are_all_multi_line {
      vec![false; nodes.len()]
    } else {
      has_same_kind_neighbor
    }
  }

  /// Gets if the first value starts on a later line than the token before it (ex. the open bracket).
  fn is_first_value_on_new_line<'a>(nodes: &[NodeOrSeparator<'a>], context: &Context<'a>) -> bool {
    let Some(first_value) = nodes.first() else {
      return false;
    };
    match context.token_finder.get_previous_token(first_value) {
      Some(previous_token) => node_helpers::get_use_new_lines_for_nodes(&previous_token.range(), first_value, context.program),
      None => false,
    }
  }

  /// Gets if each value has a value beside it of the same kind.
  fn get_has_same_kind_neighbor(nodes: &[NodeOrSeparator]) -> Vec<bool> {
    let mut result = vec![false; nodes.len()];
    let mut previous: Option<(usize, Option<NodeKind>)> = None;
    for (i, value) in nodes.iter().enumerate() {
      let NodeOrSeparator::Node(node) = value else {
        continue;
      };
      let kind = get_kind(*node);
      if let Some((previous_index, previous_kind)) = previous
        && kind.is_some()
        && kind == previous_kind
      {
        result[previous_index] = true;
        result[i] = true;
      }
      previous = Some((i, kind));
    }
    result
  }

  fn get_kind(node: Node) -> Option<NodeKind> {
    match get_inner_value(node) {
      Node::ObjectExpression(_) | Node::ObjectPattern(_) | Node::ObjectAssignmentTarget(_) | Node::TSTypeLiteral(_) => Some(NodeKind::ObjectExpression),
      Node::ArrayExpression(_) | Node::ArrayPattern(_) | Node::ArrayAssignmentTarget(_) | Node::TSTupleType(_) => Some(NodeKind::ArrayExpression),
      _ => None,
    }
  }

  /// Gets if the object or array is multi-line regardless of the line width, using the
  /// same rule as when generating it (ex. a newline after the open brace).
  fn is_multi_line_as_written<'a>(node: Node<'a>, context: &mut Context<'a>) -> bool {
    match get_inner_value(node) {
      Node::ObjectExpression(node) => {
        get_use_new_lines_for_nodes_with_preceeding_token("{", &node.properties[..], context.config.object_expression_prefer_single_line, context)
      }
      Node::ObjectPattern(node) => {
        let props = node
          .properties
          .iter()
          .map(|p| p.into_node())
          .chain(node.rest.as_ref().map(|r| r.into_node()))
          .collect::<Vec<_>>();
        get_use_new_lines_for_nodes_with_preceeding_token("{", &props, context.config.object_pattern_prefer_single_line, context)
      }
      Node::ObjectAssignmentTarget(node) => {
        let props = node
          .properties
          .iter()
          .map(|p| p.into_node())
          .chain(node.rest.as_ref().map(|r| r.into_node()))
          .collect::<Vec<_>>();
        get_use_new_lines_for_nodes_with_preceeding_token("{", &props, context.config.object_pattern_prefer_single_line, context)
      }
      Node::TSTypeLiteral(node) => {
        get_use_new_lines_for_nodes_with_preceeding_token("{", &node.members[..], context.config.type_literal_prefer_single_line, context)
      }
      Node::ArrayExpression(node) => {
        let values = get_array_like_values(
          node.into_node(),
          node
            .elements
            .iter()
            .map(|elem| match elem {
              ArrayExpressionElement::Elision(_) => None,
              elem => Some(elem.into_node()),
            })
            .collect(),
          context,
        );
        get_array_like_force_use_new_lines(node.range(), &values, context.config.array_expression_prefer_single_line, context)
      }
      Node::ArrayPattern(node) => {
        // the pattern starts with its open bracket, which is all that's used from the range
        let elements = node
          .elements
          .iter()
          .map(|x| x.as_ref().map(|elem| elem.into_node()))
          .chain(node.rest.as_ref().map(|r| Some(r.into_node())))
          .collect();
        let values = get_array_like_values(node.into_node(), elements, context);
        get_array_like_force_use_new_lines(node.range(), &values, context.config.array_pattern_prefer_single_line, context)
      }
      Node::ArrayAssignmentTarget(node) => {
        // the target starts with its open bracket, which is all that's used from the range
        let elements = node
          .elements
          .iter()
          .map(|x| x.as_ref().map(|elem| elem.into_node()))
          .chain(node.rest.as_ref().map(|r| Some(r.into_node())))
          .collect();
        let values = get_array_like_values(node.into_node(), elements, context);
        get_array_like_force_use_new_lines(node.range(), &values, context.config.array_pattern_prefer_single_line, context)
      }
      Node::TSTupleType(node) => {
        let values = get_array_like_values(node.into_node(), node.element_types.iter().map(|x| Some(x.into_node())).collect(), context);
        get_array_like_force_use_new_lines(node.range(), &values, context.config.tuple_type_prefer_single_line, context)
      }
      _ => false,
    }
  }

  /// Gets the value within a labeled tuple element (ex. `{}` in `[a: {}]`), which
  /// swc represented as part of every tuple element.
  fn get_inner_value(node: Node) -> Node {
    match node {
      Node::TSNamedTupleMember(member) => (&member.element_type).into_node(),
      node => node,
    }
  }
}

pub(super) fn sort_by_sorted_indexes<'b, T>(items: BumpVec<'b, T>, sorted_indexes: utils::VecMap<usize>) -> BumpVec<'b, T> {
  let bump = items.bump();
  let mut sorted_items = BumpVec::with_capacity_in(items.len(), bump);
  for _ in 0..items.len() {
    sorted_items.push(None);
  }

  for (i, generated_node) in items.into_iter().enumerate() {
    sorted_items[*sorted_indexes.get(i).unwrap_or(&i)] = Some(generated_node);
  }

  sorted_items.into_iter().map(|x| x.unwrap()).collect_in(bump)
}

pub(super) fn gen_node_with_separator<'a>(value: Node<'a>, generated_separator: PrintItems<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  let comma_token = get_comma_token(value, context);

  // get the trailing comments after the comma token (if the separator in the file is currently a comma)
  let generated_trailing_comments = if let Some(comma_token) = comma_token {
    gen_comma_trailing_comments(comma_token, context)
  } else {
    PrintItems::new(context.arena)
  };

  // if the current node is ignored and already has a semi-colon, then skip adding a separator
  let is_ignored_with_semi_colon =
    value.text_fast(context.program).ends_with(';') && get_has_ignore_comment(&value.leading_comments_fast(context.program), value, context);
  if is_ignored_with_semi_colon {
    items.extend(gen_node(value, context));
  } else {
    // the comments before a comma that's within the node (ex. in a type literal member) are generated
    // before the node so that they go after the separator instead of where the comma was
    let generated_comments_before_comma = match comma_token {
      Some(comma_token) if comma_token.end() == value.end() => gen_comments_before_comma_within(comma_token, context),
      _ => PrintItems::new(context.arena),
    };
    let generated_separator = generated_separator.into_rc_path();
    items.extend(gen_node_with_inner_gen(value, context, move |mut items, _| {
      // this Rc clone is necessary because we can't move the captured generated_separator out of this closure
      items.push_optional_path(generated_separator);
      items
    }));
    items.extend(generated_comments_before_comma);
  }

  items.extend(generated_trailing_comments);

  return items;

  fn get_comma_token<'a>(element: Node<'a>, context: &mut Context<'a>) -> Option<&'a TokenAndSpan> {
    match context.token_finder.get_next_token_if_comma(&element) {
      Some(comma) => Some(comma),
      None => context.token_finder.get_last_token_within_if_comma(&element), // may occur for type literals
    }
  }

  fn gen_comma_trailing_comments<'a>(comma_token: &'a TokenAndSpan, context: &mut Context<'a>) -> PrintItems<'a> {
    let comma_range = comma_token.range();
    let trailing_comments = comma_range.trailing_comments_fast(context.program);
    if trailing_comments.is_empty() {
      return PrintItems::new(context.arena);
    }
    // the comments before the comma go after it, so these go on their own line after
    // those when they're on a different line (ex. `// d` in `a\n// c\n, // d`)
    match get_comments_before_token(comma_token, context).last() {
      Some(last_comment) => {
        let trailing_comments = get_trailing_comments_same_line(&comma_range, trailing_comments, context);
        gen_comments_after_comma(&last_comment.range(), trailing_comments, comma_token, context)
      }
      None => gen_comments_as_trailing(&comma_range, trailing_comments, context),
    }
  }

  fn gen_comments_before_comma_within<'a>(comma_token: &'a TokenAndSpan, context: &mut Context<'a>) -> PrintItems<'a> {
    let comments = get_comments_before_token(comma_token, context);
    match comma_token.previous_token_fast(context.program) {
      Some(previous_token) if !comments.is_empty() => gen_comments_after_comma(&previous_token.range(), comments, comma_token, context),
      _ => PrintItems::new(context.arena),
    }
  }
}

/// Some nodes don't have a TSTypeAnnotation, but instead a TSType
pub(super) fn gen_type_ann_with_colon_if_exists_for_type<'a>(type_ann: Option<&'a TSType<'a>>, context: &mut Context<'a>) -> PrintItems<'a> {
  if let Some(type_ann) = type_ann {
    gen_type_ann_with_colon_for_type(type_ann, context)
  } else {
    PrintItems::new(context.arena)
  }
}

pub(super) fn gen_type_ann_with_colon_for_type<'a>(type_ann: &'a TSType<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  if context.config.type_annotation_space_before_colon {
    items.push_space();
  }
  let colon_token = context.token_finder.get_previous_token_if_colon(type_ann);
  #[cfg(debug_assertions)]
  assert_has_op(":", colon_token, context);
  items.extend(gen_type_ann_with_colon(type_ann.into_node(), colon_token, context));
  items
}

pub(super) fn gen_type_ann_with_colon_if_exists<'a>(type_ann: Option<&'a TSTypeAnnotation<'a>>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);
  if let Some(type_ann) = type_ann {
    if context.config.type_annotation_space_before_colon {
      items.push_space();
    }
    let colon_token = context.token_finder.get_first_colon_token_within(type_ann);
    #[cfg(debug_assertions)]
    assert_has_op(":", colon_token, context);
    items.extend(gen_type_ann_with_colon(type_ann.into_node(), colon_token, context));
  }
  items
}

pub(super) fn gen_type_ann_with_colon<'a>(type_ann: Node<'a>, colon_token: Option<&TokenAndSpan>, context: &mut Context<'a>) -> PrintItems<'a> {
  gen_assignment_like_with_token(type_ann, sc!(":"), colon_token, context)
}

pub(super) struct GenBraceSeparatorOptions<'a> {
  pub(super) brace_position: BracePosition,
  pub(super) open_brace_token: Option<&'a TokenAndSpan>,
  pub(super) start_header_lsil: Option<LineStartIndentLevel>,
}

pub(super) fn gen_brace_separator<'a>(opts: GenBraceSeparatorOptions<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  match opts.brace_position {
    BracePosition::SameLineUnlessHanging => {
      if let Some(start_header_lsil) = opts.start_header_lsil {
        conditions::new_line_if_hanging_space_otherwise(
          context.arena,
          conditions::NewLineIfHangingSpaceOtherwiseOptions {
            start_lsil: start_header_lsil,
            end_lsil: None,
            space_char: Some(space_if_not_start_line(context.arena)),
          },
        )
        .into_print_items(context.arena)
      } else {
        space_if_not_start_line(context.arena)
      }
    }
    BracePosition::SameLine => space_if_not_start_line(context.arena),
    BracePosition::NextLine => Signal::NewLine.into_print_items(context.arena),
    BracePosition::Maintain => {
      if let Some(open_brace_token) = opts.open_brace_token {
        if node_helpers::is_first_node_on_line(&open_brace_token.range(), context.program) {
          Signal::NewLine.into_print_items(context.arena)
        } else {
          space_if_not_start_line(context.arena)
        }
      } else {
        space_if_not_start_line(context.arena)
      }
    }
  }
}

pub(super) fn space_if_not_start_line<'a>(arena: &'a Arena) -> PrintItems<'a> {
  if_true("spaceIfNotStartLine", condition_resolvers::is_not_start_of_line(), " ".into_print_items(arena)).into_print_items(arena)
}

pub(super) struct GenNodeInParensOptions {
  pub(super) inner_range: SourceRange,
  pub(super) prefer_hanging: bool,
  pub(super) allow_open_paren_trailing_comments: bool,
  pub(super) single_line_space_around: bool,
}

pub(super) fn gen_node_in_parens<'a>(
  gen_node: impl FnOnce(&mut Context<'a>) -> PrintItems<'a>,
  opts: GenNodeInParensOptions,
  context: &mut Context<'a>,
) -> PrintItems<'a> {
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
      comments_only_padding: BlockPaddingBlankLines::default(),
      keep_blank_lines_before_close_token_comments: false,
    },
    context,
  );

  fn get_force_use_new_lines<'a>(inner_range: SourceRange, paren_range: &Option<SourceRange>, context: &mut Context<'a>) -> bool {
    if !context.config.parentheses_prefer_single_line
      && let Some(paren_range) = &paren_range
      && node_helpers::get_use_new_lines_for_nodes(&paren_range.start(), &inner_range, context.program)
    {
      return true;
    }

    has_any_node_comment_on_different_line(&[inner_range], context)
  }
}

pub(super) fn get_paren_range<'a>(inner_range: &SourceRange, context: &mut Context<'a>) -> Option<SourceRange> {
  let open_paren = context.token_finder.get_previous_token_if_open_paren(inner_range);
  let close_paren = context.token_finder.get_next_token_if_close_paren(inner_range);

  if let Some(open_paren) = open_paren
    && let Some(close_paren) = close_paren
  {
    return Some(SourceRange::new(open_paren.start(), close_paren.end()));
  }

  None
}

pub(super) struct GenExtendsOrImplementsOptions<'a> {
  pub(super) text: &'static StringContainer<'static>,
  pub(super) type_items: Vec<Node<'a>>,
  pub(super) start_header_lsil: LineStartIndentLevel,
  pub(super) prefer_hanging: bool,
}

pub(super) fn gen_extends_or_implements<'a>(opts: GenExtendsOrImplementsOptions<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);

  if opts.type_items.is_empty() {
    return items;
  }

  items.push_condition(conditions::new_line_if_hanging_space_otherwise(
    context.arena,
    conditions::NewLineIfHangingSpaceOtherwiseOptions {
      start_lsil: opts.start_header_lsil,
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
  // the newline group will force it to put the extends or implements on a new line
  items.push_condition(conditions::indent_if_start_of_line(ir_helpers::new_line_group({
    let mut items = PrintItems::new(context.arena);
    items.push_sc(opts.text);
    items.extend(gen_separated_values(
      GenSeparatedValuesParams {
        nodes: opts.type_items.into_iter().map(NodeOrSeparator::Node).collect(),
        prefer_hanging: opts.prefer_hanging,
        force_use_new_lines: false,
        blank_lines: ListBlankLines::Remove,
        separator: TrailingCommas::Never.into(),
        single_line_options: ir_helpers::SingleLineOptions::separated_line_starting_with_space(context.arena),
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

pub(super) struct GenObjectLikeNodeOptions<'a> {
  pub(super) node: Node<'a>,
  pub(super) members: Vec<Node<'a>>,
  pub(super) separator: Separator,
  pub(super) prefer_hanging: bool,
  pub(super) prefer_single_line: bool,
  pub(super) force_single_line: bool,
  pub(super) force_multi_line: bool,
  pub(super) surround_single_line_with_spaces: bool,
  pub(super) blank_lines: ListBlankLines,
  pub(super) node_sorter: Option<NodeSorter>,
}

pub(super) fn gen_object_like_node<'a>(opts: GenObjectLikeNodeOptions<'a>, context: &mut Context<'a>) -> PrintItems<'a> {
  let mut items = PrintItems::new(context.arena);

  // the braces are the first open brace of the node and the close brace matching it, which is before
  // the braces of anything after them (ex. the import attributes in `import { a } from "b" with { ... }`)
  let tokens = opts.node.tokens_fast(context.program);
  let open_brace_index = tokens.iter().position(|t| t.kind == TokenKind::LCurly);
  let open_brace_token = open_brace_index.map(|index| &tokens[index]);
  let close_brace_token = open_brace_index
    .and_then(|index| get_matching_close_brace_index(tokens, index))
    .map(|index| &tokens[index]);
  let prefer_single_line = opts.prefer_single_line || context.take_prefer_single_line_value(&opts.node);
  let force_multi_line =
    opts.force_multi_line || !opts.force_single_line && get_use_new_lines_for_nodes_with_preceeding_token("{", &opts.members, prefer_single_line, context);

  let first_member_range = opts.members.first().map(|x| x.range());
  let keep_blank_lines_before_close_token_comments = opts.blank_lines != ListBlankLines::Remove;
  let obj_range = if let (Some(open_brace_token), Some(close_brace_token)) = (open_brace_token, close_brace_token) {
    Some(SourceRange::new(open_brace_token.start(), close_brace_token.end()))
  } else {
    None
  };

  let space_at_start = opts.surround_single_line_with_spaces;
  // a generated trailing separator means the closing braces aren't consecutive,
  // even when the last member ends in a closing brace in the source
  let has_single_line_trailing_separator = matches!(
    opts.separator.single_line,
    Some(SeparatorValue::Semicolon(Semicolons::Always) | SeparatorValue::Comma(TrailingCommas::Always))
  );
  let space_at_end = space_at_start
    && (context.config.space_between_object_closing_braces
      || has_single_line_trailing_separator
      || !opts
        .members
        .last()
        .is_some_and(|last| get_ends_with_object_like_node(*last, close_brace_token, context)));

  items.extend(gen_surrounded_by_tokens(
    |context| {
      if opts.members.is_empty() {
        PrintItems::new(context.arena)
      } else {
        gen_separated_values(
          GenSeparatedValuesParams {
            nodes: opts.members.into_iter().map(NodeOrSeparator::Node).collect(),
            prefer_hanging: opts.prefer_hanging,
            force_use_new_lines: force_multi_line,
            blank_lines: opts.blank_lines,
            separator: opts.separator,
            single_line_options: ir_helpers::SingleLineOptions {
              space_at_start,
              space_at_end,
              separator: Signal::SpaceOrNewLine.into_print_items(context.arena),
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
      comments_only_padding: BlockPaddingBlankLines::default(),
      keep_blank_lines_before_close_token_comments,
    },
    context,
  ));

  items
}

/// Gets if the last member ends with the closing brace of an object-like node
/// that only has separators and whitespace before the parent's closing brace.
pub(super) fn get_ends_with_object_like_node<'a>(last_member: Node<'a>, parent_close_brace: Option<&TokenAndSpan>, context: &Context<'a>) -> bool {
  use oxc_ast_visit::Visit;

  let Some(parent_close_brace) = parent_close_brace else {
    return false;
  };
  // source separators may be removed, so skip them
  let Some(close_brace) = last_member
    .tokens_fast(context.program)
    .iter()
    .rev()
    .find(|token| !matches!(token.kind, TokenKind::Semicolon | TokenKind::Comma))
  else {
    return false;
  };
  if close_brace.kind != TokenKind::RCurly {
    return false;
  }
  // comments between the braces need to stay separated from the closing brace
  let text_between = SourceRange::new(close_brace.end(), parent_close_brace.start()).text_fast(context.program);
  if !text_between.chars().all(|c| c.is_whitespace() || c == ',' || c == ';') {
    return false;
  }

  // ensure the brace belongs to an object-like node and not something like a function body
  let mut finder = ObjectLikeNodeAtEndFinder {
    end: close_brace.end(),
    is_at_end_stack: Vec::new(),
    found: false,
  };
  match last_member {
    Node::ObjectProperty(node) => finder.visit_object_property(node),
    Node::SpreadElement(node) => finder.visit_spread_element(node),
    Node::BindingProperty(node) => finder.visit_binding_property(node),
    Node::BindingRestElement(node) => finder.visit_binding_rest_element(node),
    Node::AssignmentTargetPropertyIdentifier(node) => finder.visit_assignment_target_property_identifier(node),
    Node::AssignmentTargetPropertyProperty(node) => finder.visit_assignment_target_property_property(node),
    Node::AssignmentTargetRest(node) => finder.visit_assignment_target_rest(node),
    Node::TSPropertySignature(node) => finder.visit_ts_property_signature(node),
    Node::TSMethodSignature(node) => finder.visit_ts_method_signature(node),
    Node::TSIndexSignature(node) => finder.visit_ts_index_signature(node),
    Node::TSCallSignatureDeclaration(node) => finder.visit_ts_call_signature_declaration(node),
    Node::TSConstructSignatureDeclaration(node) => finder.visit_ts_construct_signature_declaration(node),
    // the other members can't contain an object-like node at their end (ex. import specifiers)
    _ => return is_object_like_node(last_member),
  }
  return finder.found;

  /// Finds an object-like node at the end of the visited node, where the nodes between it and
  /// the visited node all end at the same position (ex. `{ b }` in `a: { b }`).
  struct ObjectLikeNodeAtEndFinder {
    end: SourcePos,
    /// Whether each node being visited is at the end, along with its ancestors below the visited node.
    is_at_end_stack: Vec<bool>,
    found: bool,
  }

  impl<'a> Visit<'a> for ObjectLikeNodeAtEndFinder {
    fn enter_node(&mut self, kind: AstKind<'a>) {
      let is_at_end = match self.is_at_end_stack.last() {
        Some(is_parent_at_end) => *is_parent_at_end && kind.end() == self.end,
        // the visited node, which may end with a separator
        None => true,
      };
      if is_at_end && is_object_like_node(kind) {
        self.found = true;
      }
      self.is_at_end_stack.push(is_at_end);
    }

    fn leave_node(&mut self, _kind: AstKind<'a>) {
      self.is_at_end_stack.pop();
    }
  }

  fn is_object_like_node(node: Node) -> bool {
    matches!(
      node.kind(),
      NodeKind::ObjectExpression | NodeKind::ObjectPattern | NodeKind::ObjectAssignmentTarget | NodeKind::TSTypeLiteral | NodeKind::TSMappedType
    )
  }
}

/// Gets the index of the close brace token that matches the open brace token at the index.
pub(super) fn get_matching_close_brace_index(tokens: &[TokenAndSpan], open_brace_index: usize) -> Option<usize> {
  let mut depth = 0;
  for (index, token) in tokens.iter().enumerate().skip(open_brace_index) {
    match token.kind {
      TokenKind::LCurly => depth += 1,
      TokenKind::RCurly => {
        depth -= 1;
        if depth == 0 {
          return Some(index);
        }
      }
      _ => {}
    }
  }
  None
}

/// Gets the index of the open brace token that matches the close brace token at the index.
pub(super) fn get_matching_open_brace_index(tokens: &[TokenAndSpan], close_brace_index: usize) -> Option<usize> {
  let mut depth = 0;
  for (index, token) in tokens[..=close_brace_index].iter().enumerate().rev() {
    match token.kind {
      TokenKind::RCurly => depth += 1,
      TokenKind::LCurly => {
        depth -= 1;
        if depth == 0 {
          return Some(index);
        }
      }
      _ => {}
    }
  }
  None
}
