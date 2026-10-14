use std::cmp::Ordering;

#[cfg(test)]
pub fn cmp_module_specifiers(a: &str, b: &str, cmp_text: impl Fn(&str, &str) -> Ordering) -> Ordering {
  let a_info = get_module_specifier_info(a);
  let b_info = get_module_specifier_info(b);

  cmp_module_specifier_infos(&a_info, &b_info, cmp_text)
}

pub(super) fn cmp_module_specifier_infos(
  a_info: &ModuleSpecifierInfo,
  b_info: &ModuleSpecifierInfo,
  cmp_text: impl Fn(&str, &str) -> Ordering,
) -> Ordering {
  match a_info {
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
pub(super) enum ModuleSpecifierInfo<'a> {
  Absolute { text: &'a str },
  Relative { relative_count: usize, folder_text: &'a str },
}

pub(super) fn get_module_specifier_info(text: &str) -> ModuleSpecifierInfo<'_> {
  let no_quotes_text = &text[1..text.len() - 1];
  let mut relative_count = 0;
  let mut start_index = 0;
  loop {
    let remaining_text = &no_quotes_text[start_index..];
    match remaining_text {
      "." => {
        start_index += 1;
        break;
      }
      ".." => {
        relative_count += 1;
        start_index += 2;
        break;
      }
      _ if remaining_text.starts_with("./") => {
        start_index += 2;
      }
      _ if remaining_text.starts_with("../") => {
        relative_count += 1;
        start_index += 3;
      }
      _ => break,
    }
  }

  if start_index > 0 {
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
