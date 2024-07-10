pub fn is_prefix_semi_colon_insertion_char(value: char) -> bool {
  // from: https://standardjs.com/rules.html#semicolons
  matches!(value, '[' | '(' | '`' | '+' | '*' | '/' | '-' | ',' | '.')
}
pub fn start_with_increment_decrement(text: &str) -> bool {
  // Check if the text contains ++ or --
  text.starts_with("++") || text.starts_with("--")
}
