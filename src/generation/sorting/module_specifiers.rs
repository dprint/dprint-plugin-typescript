use std::cmp::Ordering;
use ModuleSpecifierInfo::*;

pub fn cmp_module_specifiers(a: &str, b: &str, cmp_text: impl Fn(&str, &str) -> Ordering) -> Ordering {
  let a_info = get_module_specifier_info(a);
  let b_info = get_module_specifier_info(b);
  cmp_module_spec_info(&a_info, &b_info, cmp_text)
}

fn cmp_module_spec_info(a: &ModuleSpecifierInfo, b: &ModuleSpecifierInfo, cmp_text: impl Fn(&str, &str) -> Ordering) -> Ordering {
  match (a, b) {
    (Node_ { text: lhs }, Node_ { text: rhs }) => cmp_text(lhs, rhs),
    (Node_ { .. }, _) => Ordering::Less,
    (_, Node_ { .. }) => Ordering::Greater,

    (Npm_ { text: lhs }, Npm_ { text: rhs }) => cmp_text(lhs, rhs),
    (Npm_ { .. }, _) => Ordering::Less,
    (_, Npm_ { .. }) => Ordering::Greater,

    (Absolute { text: lhs }, Absolute { text: rhs }) => cmp_text(lhs, rhs),
    (Absolute { .. }, _) => Ordering::Less,
    (_, Absolute { .. }) => Ordering::Greater,

    (Relative { count: a_count, text: lhs }, Relative { count: b_count, text: rhs }) => b_count.cmp(a_count).then(cmp_text(lhs, rhs)),
  }
}

#[derive(Debug, PartialEq)]
enum ModuleSpecifierInfo<'a> {
  Absolute { text: &'a str },
  Relative { count: usize, text: &'a str },
  Npm_ { text: &'a str },
  Node_ { text: &'a str },
}

fn get_module_specifier_info(text: &str) -> ModuleSpecifierInfo<'_> {
  let no_quotes_text = &text[1..text.len() - 1];
  if let Some(text) = no_quotes_text.strip_prefix("npm:") {
    return Npm_ { text };
  } else if let Some(text) = no_quotes_text.strip_prefix("node:") {
    return Node_ { text };
  }
  let parts = no_quotes_text.split('/').collect::<Vec<_>>();
  if parts[0] == "." || parts[0] == ".." {
    let mut relative_count = 0;
    let mut start_index = 0;
    let parts_len = parts.len();
    for (i, part) in parts.into_iter().enumerate() {
      if part == ".." {
        relative_count += 1;
      } else if part != "." {
        break;
      }

      // use 0 for last part since it does not have a slash after it
      let next_slash_width = if i == parts_len - 1 { 0 } else { 1 };
      start_index += part.len() + next_slash_width;
    }

    Relative {
      count: relative_count,
      text: &no_quotes_text[start_index..],
    }
  } else {
    Absolute { text: no_quotes_text }
  }
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn it_should_compare_module_specifiers() {
    assert_eq!(cmp_module_specifiers("''", "'test'", |a, b| a.cmp(b)), Ordering::Less);
    assert_eq!(cmp_module_specifiers("'a'", "'b'", |a, b| a.cmp(b)), Ordering::Less);
    assert_eq!(cmp_module_specifiers("'b'", "'a'", |a, b| a.cmp(b)), Ordering::Greater);
    assert_eq!(cmp_module_specifiers("'a'", "'a'", |a, b| a.cmp(b)), Ordering::Equal);
    assert_eq!(cmp_module_specifiers("'a'", "'./a'", |a, b| a.cmp(b)), Ordering::Less);

    assert_eq!(cmp_module_specifiers("'./a'", "'a'", |a, b| a.cmp(b)), Ordering::Greater);
    assert_eq!(cmp_module_specifiers("'./a'", "'./a'", |a, b| a.cmp(b)), Ordering::Equal);
    assert_eq!(cmp_module_specifiers("'../a'", "'./a'", |a, b| a.cmp(b)), Ordering::Less);
    assert_eq!(cmp_module_specifiers("'./a'", "'../a'", |a, b| a.cmp(b)), Ordering::Greater);
    assert_eq!(cmp_module_specifiers("'../../a'", "'../a'", |a, b| a.cmp(b)), Ordering::Less);
    assert_eq!(cmp_module_specifiers("'../a'", "'../../a'", |a, b| a.cmp(b)), Ordering::Greater);
    assert_eq!(cmp_module_specifiers("'..'", "'test'", |a, b| a.cmp(b)), Ordering::Greater);

    assert_eq!(cmp_module_specifiers("'npm:a'", "''", |a, b| a.cmp(b)), Ordering::Less);
    assert_eq!(cmp_module_specifiers("'npm:a'", "'a'", |a, b| a.cmp(b)), Ordering::Less);
    assert_eq!(cmp_module_specifiers("'npm:a'", "'b'", |a, b| a.cmp(b)), Ordering::Less);
    assert_eq!(cmp_module_specifiers("'npm:b'", "'a'", |a, b| a.cmp(b)), Ordering::Less);
    assert_eq!(cmp_module_specifiers("'npm:a'", "'./a'", |a, b| a.cmp(b)), Ordering::Less);
    assert_eq!(cmp_module_specifiers("'npm:a'", "'./b'", |a, b| a.cmp(b)), Ordering::Less);
    assert_eq!(cmp_module_specifiers("'npm:b'", "'./a'", |a, b| a.cmp(b)), Ordering::Less);
    assert_eq!(cmp_module_specifiers("'npm:a'", "'npm:b'", |a, b| a.cmp(b)), Ordering::Less);
    assert_eq!(cmp_module_specifiers("'npm:a'", "'npm:a'", |a, b| a.cmp(b)), Ordering::Equal);
    assert_eq!(cmp_module_specifiers("'npm:b'", "'npm:a'", |a, b| a.cmp(b)), Ordering::Greater);
    assert_eq!(cmp_module_specifiers("'npm:a'", "'node:a'", |a, b| a.cmp(b)), Ordering::Greater);
    assert_eq!(cmp_module_specifiers("'npm:a'", "'node:b'", |a, b| a.cmp(b)), Ordering::Greater);
    assert_eq!(cmp_module_specifiers("'npm:b'", "'node:a'", |a, b| a.cmp(b)), Ordering::Greater);

    assert_eq!(cmp_module_specifiers("'node:a'", "''", |a, b| a.cmp(b)), Ordering::Less);
    assert_eq!(cmp_module_specifiers("'node:a'", "'a'", |a, b| a.cmp(b)), Ordering::Less);
    assert_eq!(cmp_module_specifiers("'node:a'", "'b'", |a, b| a.cmp(b)), Ordering::Less);
    assert_eq!(cmp_module_specifiers("'node:b'", "'a'", |a, b| a.cmp(b)), Ordering::Less);
    assert_eq!(cmp_module_specifiers("'node:a'", "'./a'", |a, b| a.cmp(b)), Ordering::Less);
    assert_eq!(cmp_module_specifiers("'node:a'", "'./b'", |a, b| a.cmp(b)), Ordering::Less);
    assert_eq!(cmp_module_specifiers("'node:b'", "'./a'", |a, b| a.cmp(b)), Ordering::Less);
    assert_eq!(cmp_module_specifiers("'node:a'", "'npm:b'", |a, b| a.cmp(b)), Ordering::Less);
    assert_eq!(cmp_module_specifiers("'node:a'", "'npm:a'", |a, b| a.cmp(b)), Ordering::Less);
    assert_eq!(cmp_module_specifiers("'node:b'", "'npm:a'", |a, b| a.cmp(b)), Ordering::Less);
    assert_eq!(cmp_module_specifiers("'node:a'", "'node:a'", |a, b| a.cmp(b)), Ordering::Equal);
    assert_eq!(cmp_module_specifiers("'node:a'", "'node:b'", |a, b| a.cmp(b)), Ordering::Less);
    assert_eq!(cmp_module_specifiers("'node:b'", "'node:a'", |a, b| a.cmp(b)), Ordering::Greater);
  }

  #[test]
  fn it_should_get_module_specifier_info_when_empty() {
    assert_eq!(get_module_specifier_info("''"), Absolute { text: "" });
    assert_eq!(get_module_specifier_info("\"\""), Absolute { text: "" });
  }

  #[test]
  fn it_should_get_module_specifier_info_for_absolute() {
    assert_eq!(get_module_specifier_info("'testing'"), Absolute { text: "testing" });
    assert_eq!(get_module_specifier_info("\"testing/asdf\""), Absolute { text: "testing/asdf" });
  }

  #[test]
  fn it_should_get_module_specifier_info_for_relative() {
    assert_eq!(get_module_specifier_info("'./a'"), Relative { count: 0, text: "a" });
    assert_eq!(get_module_specifier_info("'./../ab-cd/t'"), Relative { count: 1, text: "ab-cd/t" });
    assert_eq!(get_module_specifier_info("'../asdf'"), Relative { count: 1, text: "asdf" });
    assert_eq!(get_module_specifier_info("'../../../test'"), Relative { count: 3, text: "test" });
  }

  #[test]
  fn it_should_get_module_specifier_info_for_npm() {
    assert_eq!(get_module_specifier_info("'npm:foo'"), Npm_ { text: "foo" });
    assert_eq!(get_module_specifier_info("\"npm:foo/bar\""), Npm_ { text: "foo/bar" });
  }

  #[test]
  fn it_should_get_module_specifier_info_for_node() {
    assert_eq!(get_module_specifier_info("'node:foo'"), Node_ { text: "foo" });
    assert_eq!(get_module_specifier_info("\"node:foo/bar\""), Node_ { text: "foo/bar" });
  }
}
