use std::cmp::Ordering;

pub fn cmp_module_specifiers(a: &str, b: &str, cmp_text: impl Fn(&str, &str) -> Ordering) -> Ordering {
    let a_info = get_module_specifier_info(a);
    let b_info = get_module_specifier_info(b);

    match a_info.kind {
        ModuleSpecifierKind::Absolute => {
            match b_info.kind {
                ModuleSpecifierKind::Absolute => cmp_text(a_info.comparison_text, b_info.comparison_text),
                ModuleSpecifierKind::Relative(_) => Ordering::Less,
            }
        },
        ModuleSpecifierKind::Relative(a_relative_count) => {
            match b_info.kind {
                ModuleSpecifierKind::Absolute => Ordering::Greater,
                ModuleSpecifierKind::Relative(b_relative_count) => {
                    if a_relative_count > b_relative_count {
                        Ordering::Less
                    } else if a_relative_count < b_relative_count {
                        Ordering::Greater
                    } else {
                        cmp_text(a_info.comparison_text, b_info.comparison_text)
                    }
                },
            }
        }
    }
}

struct ModuleSpecifierInfo<'a> {
    comparison_text: &'a str,
    kind: ModuleSpecifierKind,
}

#[derive(Debug, PartialEq)]
enum ModuleSpecifierKind {
    Absolute,
    Relative(usize),
}

fn get_module_specifier_info<'a>(text: &'a str) -> ModuleSpecifierInfo<'a> {
    let no_quotes_text = &text[1..text.len() - 1];
    let parts = no_quotes_text.split("/").collect::<Vec<_>>();
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

        ModuleSpecifierInfo {
            comparison_text: &no_quotes_text[start_index..],
            kind: ModuleSpecifierKind::Relative(relative_count),
        }
    } else {
        ModuleSpecifierInfo {
            comparison_text: no_quotes_text,
            kind: ModuleSpecifierKind::Absolute,
        }
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
        assert_eq!(result.comparison_text, "");
        assert_eq!(result.kind, ModuleSpecifierKind::Absolute);
    }

    #[test]
    fn it_should_get_module_specifier_info_for_absolute() {
        // double quotes
        let result = get_module_specifier_info(r#""testing/asdf""#);
        assert_eq!(result.comparison_text, "testing/asdf");
        assert_eq!(result.kind, ModuleSpecifierKind::Absolute);

        // single quotes
        let result = get_module_specifier_info("'testing'");
        assert_eq!(result.comparison_text, "testing");
        assert_eq!(result.kind, ModuleSpecifierKind::Absolute);
    }

    #[test]
    fn it_should_get_module_specifier_info_for_relative() {
        let result = get_module_specifier_info("'./a'");
        assert_eq!(result.comparison_text, "a");
        assert_eq!(result.kind, ModuleSpecifierKind::Relative(0));

        let result = get_module_specifier_info("'./../testing-test/t'");
        assert_eq!(result.comparison_text, "testing-test/t");
        assert_eq!(result.kind, ModuleSpecifierKind::Relative(1));

        let result = get_module_specifier_info("'../asdf'");
        assert_eq!(result.comparison_text, "asdf");
        assert_eq!(result.kind, ModuleSpecifierKind::Relative(1));

        let result = get_module_specifier_info("'../../../test'");
        assert_eq!(result.comparison_text, "test");
        assert_eq!(result.kind, ModuleSpecifierKind::Relative(3));
    }
}
