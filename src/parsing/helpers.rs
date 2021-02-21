// todo: move these down to dprint_core eventually
use dprint_core::formatting::*;

pub fn indent_if_start_of_line_or_start_of_line_indented(items: PrintItems) -> Condition {
    let rc_path = items.into_rc_path();
    conditions::if_true_or(
        "withIndentIfStartOfLineOrStartOfLineIndented",
        |context| Some(condition_resolvers::is_start_of_line_indented(context) || condition_resolvers::is_start_of_line(context)),
        parser_helpers::with_indent(rc_path.clone().into()),
        rc_path.into(),
    )
}