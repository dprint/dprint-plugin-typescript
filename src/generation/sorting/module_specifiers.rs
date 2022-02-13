use std::cmp::Ordering;

pub fn cmp_module_specifiers(a: &str, b: &str, cmp_text: impl Fn(&str, &str) -> Ordering) -> Ordering {
  let a_info = get_module_specifier_info(a);
  let b_info = get_module_specifier_info(b);

  match &a_info {
    ModuleSpecifierInfo::Absolute { text: a_text } => match b_info {
      ModuleSpecifierInfo::Absolute { text: b_text } => cmp_text(a_text, b_text),
      ModuleSpecifierInfo::Relative { .. } => Ordering::Less,
    },
    ModuleSpecifierInfo::Relative {
      relative_count: a_relative_count,
      folder_text: a_folder_text,
    } => match &b_info {
      ModuleSpecifierInfo::Absolute { .. } => Ordering::Greater,
      ModuleSpecifierInfo::Relative {
        relative_count: b_relative_count,
        folder_text: b_folder_text,
      } => match a_relative_count.cmp(b_relative_count) {
        Ordering::Greater => Ordering::Less,
        Ordering::Less => Ordering::Greater,
        Ordering::Equal => cmp_text(a_folder_text, b_folder_text),
      },
    },
  }
}

#[derive(Debug, PartialEq)]
enum ModuleSpecifierInfo<'a> {
  Absolute { text: &'a str },
  Relative { relative_count: usize, folder_text: &'a str },
}

fn get_module_specifier_info(text: &str) -> ModuleSpecifierInfo<'_> {
  let no_quotes_text = &text[1..text.len() - 1];
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

    ModuleSpecifierInfo::Relative {
      relative_count,
      folder_text: &no_quotes_text[start_index..],
    }
  } else {
    ModuleSpecifierInfo::Absolute { text: no_quotes_text }
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
  }

  #[test]
  fn it_should_get_module_specifier_info_when_empty() {
    let result = get_module_specifier_info("''");
    assert_eq!(result, ModuleSpecifierInfo::Absolute { text: "" });
  }

  #[test]
  fn it_should_get_module_specifier_info_for_absolute() {
    // double quotes
    let result = get_module_specifier_info(r#""testing/asdf""#);
    assert_eq!(result, ModuleSpecifierInfo::Absolute { text: "testing/asdf" });

    // single quotes
    let result = get_module_specifier_info("'testing'");
    assert_eq!(result, ModuleSpecifierInfo::Absolute { text: "testing" });
  }

  #[test]
  fn it_should_get_module_specifier_info_for_relative() {
    let result = get_module_specifier_info("'./a'");
    assert_eq!(
      result,
      ModuleSpecifierInfo::Relative {
        relative_count: 0,
        folder_text: "a",
      }
    );

    let result = get_module_specifier_info("'./../testing-test/t'");
    assert_eq!(
      result,
      ModuleSpecifierInfo::Relative {
        relative_count: 1,
        folder_text: "testing-test/t",
      }
    );

    let result = get_module_specifier_info("'../asdf'");
    assert_eq!(
      result,
      ModuleSpecifierInfo::Relative {
        relative_count: 1,
        folder_text: "asdf",
      }
    );

    let result = get_module_specifier_info("'../../../test'");
    assert_eq!(
      result,
      ModuleSpecifierInfo::Relative {
        relative_count: 3,
        folder_text: "test",
      }
    );
  }
}
