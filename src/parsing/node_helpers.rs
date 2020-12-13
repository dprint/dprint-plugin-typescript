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
