use std::cmp::Ordering;

#[cfg(test)]
pub fn cmp_module_specifiers(a: &str, b: &str, cmp_text: impl Fn(&str, &str) -> Ordering) -> Ordering {
  let a_info = get_module_specifier_info(a);
  let b_info = get_module_specifier_info(b);

  cmp_module_specifier_infos(&a_info, &b_info, cmp_text)
}

/// Compares module specifiers, which are schemed (ex. `node:fs`), then bare (ex. `react`),
/// then relative from the furthest folder up to the closest (ex. `../a` before `./a`).
pub(super) fn cmp_module_specifier_infos(a_info: &ModuleSpecifierInfo, b_info: &ModuleSpecifierInfo, cmp_text: impl Fn(&str, &str) -> Ordering) -> Ordering {
  match (a_info, b_info) {
    (ModuleSpecifierInfo::Schemed { text: a_text }, ModuleSpecifierInfo::Schemed { text: b_text })
    | (ModuleSpecifierInfo::Bare { text: a_text }, ModuleSpecifierInfo::Bare { text: b_text }) => cmp_text(a_text, b_text),
    (
      ModuleSpecifierInfo::Relative {
        relative_count: a_relative_count,
        folder_text: a_folder_text,
      },
      ModuleSpecifierInfo::Relative {
        relative_count: b_relative_count,
        folder_text: b_folder_text,
      },
    ) => b_relative_count.cmp(a_relative_count).then_with(|| cmp_text(a_folder_text, b_folder_text)),
    _ => a_info.kind_order().cmp(&b_info.kind_order()),
  }
}

#[derive(Debug, PartialEq)]
pub(super) enum ModuleSpecifierInfo<'a> {
  /// A specifier that starts with a scheme (ex. `node:fs`, `npm:react`, or `https://example.com/mod.ts`).
  Schemed {
    text: &'a str,
  },
  /// A specifier that's neither schemed nor relative (ex. `react` or `@scope/package`).
  Bare {
    text: &'a str,
  },
  Relative {
    relative_count: usize,
    folder_text: &'a str,
  },
}

impl ModuleSpecifierInfo<'_> {
  fn kind_order(&self) -> u8 {
    match self {
      ModuleSpecifierInfo::Schemed { .. } => 0,
      ModuleSpecifierInfo::Bare { .. } => 1,
      ModuleSpecifierInfo::Relative { .. } => 2,
    }
  }
}

pub(super) fn get_module_specifier_info(text: &str) -> ModuleSpecifierInfo<'_> {
  let no_quotes_text = &text[1..text.len() - 1];
  if has_scheme(no_quotes_text) {
    return ModuleSpecifierInfo::Schemed { text: no_quotes_text };
  }
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
    ModuleSpecifierInfo::Bare { text: no_quotes_text }
  }
}

/// Gets if the specifier starts with letters followed by a colon (ex. `node:` or `https:`).
fn has_scheme(text: &str) -> bool {
  match text.find(':') {
    Some(colon_index) => colon_index > 0 && text[..colon_index].bytes().all(|b| b.is_ascii_alphabetic()),
    None => false,
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
  fn it_should_compare_schemed_module_specifiers_first() {
    assert_eq!(cmp_module_specifiers("'npm:b'", "''", |a, b| a.cmp(b)), Ordering::Less);
    assert_eq!(cmp_module_specifiers("'npm:b'", "'a'", |a, b| a.cmp(b)), Ordering::Less);
    assert_eq!(cmp_module_specifiers("'a'", "'npm:b'", |a, b| a.cmp(b)), Ordering::Greater);
    assert_eq!(cmp_module_specifiers("'npm:b'", "'./a'", |a, b| a.cmp(b)), Ordering::Less);
    assert_eq!(cmp_module_specifiers("'npm:b'", "'../a'", |a, b| a.cmp(b)), Ordering::Less);
    assert_eq!(cmp_module_specifiers("'./a'", "'npm:b'", |a, b| a.cmp(b)), Ordering::Greater);
    assert_eq!(cmp_module_specifiers("'https://a.com/b.ts'", "'a'", |a, b| a.cmp(b)), Ordering::Less);
    // they're compared by their text, including the scheme
    assert_eq!(cmp_module_specifiers("'npm:a'", "'npm:b'", |a, b| a.cmp(b)), Ordering::Less);
    assert_eq!(cmp_module_specifiers("'npm:a'", "'npm:a'", |a, b| a.cmp(b)), Ordering::Equal);
    assert_eq!(cmp_module_specifiers("'node:b'", "'npm:a'", |a, b| a.cmp(b)), Ordering::Less);
    assert_eq!(cmp_module_specifiers("'npm:a'", "'node:b'", |a, b| a.cmp(b)), Ordering::Greater);
    assert_eq!(cmp_module_specifiers("'jsr:@std/path'", "'node:fs'", |a, b| a.cmp(b)), Ordering::Less);
  }

  #[test]
  fn it_should_get_module_specifier_info_when_empty() {
    let result = get_module_specifier_info("''");
    assert_eq!(result, ModuleSpecifierInfo::Bare { text: "" });
  }

  #[test]
  fn it_should_get_module_specifier_info_for_bare() {
    // double quotes
    let result = get_module_specifier_info(r#""testing/asdf""#);
    assert_eq!(result, ModuleSpecifierInfo::Bare { text: "testing/asdf" });

    // single quotes
    let result = get_module_specifier_info("'testing'");
    assert_eq!(result, ModuleSpecifierInfo::Bare { text: "testing" });

    // a colon that isn't after only letters isn't a scheme
    assert_eq!(get_module_specifier_info("'@scope/a:b'"), ModuleSpecifierInfo::Bare { text: "@scope/a:b" });
    assert_eq!(get_module_specifier_info("'a1:b'"), ModuleSpecifierInfo::Bare { text: "a1:b" });
    assert_eq!(get_module_specifier_info("':a'"), ModuleSpecifierInfo::Bare { text: ":a" });
  }

  #[test]
  fn it_should_get_module_specifier_info_for_schemed() {
    assert_eq!(get_module_specifier_info("'node:fs'"), ModuleSpecifierInfo::Schemed { text: "node:fs" });
    assert_eq!(
      get_module_specifier_info(r#""npm:foo/bar""#),
      ModuleSpecifierInfo::Schemed { text: "npm:foo/bar" }
    );
    assert_eq!(
      get_module_specifier_info("'jsr:@std/path'"),
      ModuleSpecifierInfo::Schemed { text: "jsr:@std/path" }
    );
    assert_eq!(
      get_module_specifier_info("'https://example.com/mod.ts'"),
      ModuleSpecifierInfo::Schemed {
        text: "https://example.com/mod.ts"
      }
    );
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
