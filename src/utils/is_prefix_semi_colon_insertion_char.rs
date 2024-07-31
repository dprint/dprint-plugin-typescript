pub fn is_prefix_semi_colon_insertion_char(value: char) -> bool {
  // from: https://standardjs.com/rules.html#semicolons
  matches!(value, '[' | '(' | '`' | '+' | '*' | '/' | '-' | ',' | '.')
}
