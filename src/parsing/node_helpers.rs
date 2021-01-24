use swc_ast_view::*;

pub fn is_first_node_on_line(node: &dyn Spanned, module: &Module) -> bool {
    let start = node.lo().0 as usize;
    let module_text = module.text().as_bytes();

    for i in (0..start).rev() {
        let c = module_text[i];
        if c != ' ' as u8 && c != '\t' as u8 {
            return c == '\n' as u8;
        }
    }

    return true;
}

pub fn has_separating_blank_line(first_node: &dyn Spanned, second_node: &dyn Spanned, module: &Module) -> bool {
    return get_second_start_line(first_node, second_node, module) > first_node.end_line_fast(module) + 1;

    fn get_second_start_line(first_node: &dyn Spanned, second_node: &dyn Spanned, module: &Module) -> usize {
        let leading_comments = second_node.leading_comments_fast(module);

        for comment in leading_comments {
            let comment_start_line = comment.start_line_fast(module);
            if comment_start_line > first_node.end_line_fast(module) {
                return comment_start_line;
            }
        }

        second_node.start_line_fast(module)
    }
}

pub fn get_use_new_lines_for_nodes(first_node: &dyn Spanned, second_node: &dyn Spanned, module: &Module) -> bool {
    return first_node.end_line_fast(module) != second_node.start_line_fast(module);
}

pub fn has_leading_comment_on_different_line<'a>(node: &dyn Spanned, comments_to_ignore: Option<&Vec<&'a Comment>>, module: &Module<'a>) -> bool {
    get_leading_comment_on_different_line(node, comments_to_ignore, module).is_some()
}

pub fn get_leading_comment_on_different_line<'a>(node: &dyn Spanned, comments_to_ignore: Option<&Vec<&'a Comment>>, module: &Module<'a>) -> Option<&'a Comment> {
    let comments_to_ignore: Option<Vec<BytePos>> = comments_to_ignore.map(|x| x.iter().map(|c| c.lo()).collect());
    let node_start_line = node.start_line_fast(module);
    for comment in node.leading_comments_fast(module) {
        if let Some(comments_to_ignore) = &comments_to_ignore {
            if comments_to_ignore.contains(&comment.lo()) {
                continue;
            }
        }

        if comment.start_line_fast(module) < node_start_line {
            return Some(comment);
        }
    }

    return None;
}

pub fn nodes_have_only_spaces_between(previous_node: &Node, next_node: &Node, module: &Module) -> bool {
    if let Node::JSXText(previous_node) = previous_node {
        let previous_node_text = previous_node.text_fast(module);
        crate::utils::has_no_new_lines_in_trailing_whitespace(previous_node_text)
            && previous_node_text.chars().last() == Some(' ')
    } else if let Node::JSXText(next_node) = next_node {
        let next_node_text = next_node.text_fast(module);
        crate::utils::has_no_new_lines_in_leading_whitespace(next_node_text)
            && next_node_text.chars().next() == Some(' ')
    } else {
        crate::utils::is_not_empty_and_only_spaces(&module.text()[previous_node.hi().0 as usize..next_node.lo().0 as usize])
    }
}

pub fn get_siblings_between<'a>(node_a: &Node<'a>, node_b: &Node<'a>) -> Vec<Node<'a>> {
    let mut parent_children = node_a.parent().unwrap().children();
    parent_children.drain(node_a.child_index() + 1..node_b.child_index()).collect()
}
