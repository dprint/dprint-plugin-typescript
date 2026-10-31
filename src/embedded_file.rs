use std::path::PathBuf;

use crate::configuration::Configuration;

/// The text of a tagged template prepared for formatting it as a file of its
/// embedded language (ex. by the dprint host with another plugin).
pub(crate) struct EmbeddedFile {
  /// Path whose extension decides what formats the text.
  pub file_path: PathBuf,
  pub text: String,
  /// Whether the text is wrapped in a rule, which makes the declarations of
  /// a css tagged template (ex. `color: red;`) valid in a stylesheet.
  is_wrapped_in_rule: bool,
}

impl EmbeddedFile {
  pub fn new(extension: &str, text: &str) -> Self {
    // the template's text is put on its own lines at the indentation of the code
    // when it's formatted, so remove that to make formatting it again give the same text
    let text = dedent(trim_blank_lines(text));
    let is_wrapped_in_rule = matches!(extension, "css" | "less" | "scss");
    EmbeddedFile {
      file_path: PathBuf::from(format!("file.{}", extension)),
      text: if is_wrapped_in_rule { format!("a{{\n{}\n;}}\n", text) } else { text },
      is_wrapped_in_rule,
    }
  }

  /// Gets the text for the tagged template from the formatted file text or `None`
  /// when it's not in the expected shape, which leaves the template as is.
  pub fn into_template_text(self, formatted: &str, config: &Configuration) -> Option<String> {
    if self.is_wrapped_in_rule {
      unwrap_rule(formatted, config)
    } else {
      Some(trim_blank_lines(formatted).to_string())
    }
  }
}

/// Gets the declarations of the rule the text was wrapped in, dedented by one level.
fn unwrap_rule(text: &str, config: &Configuration) -> Option<String> {
  let indent_text = if config.use_tabs {
    "\t".to_string()
  } else {
    " ".repeat(config.indent_width as usize)
  };
  let text = trim_blank_lines(text);
  let mut lines = text.lines();
  let first_line = lines.next()?.trim();
  let last_line = lines.next_back()?;
  if !first_line.starts_with('a') || !first_line.ends_with('{') || last_line.trim_end() != "}" {
    return None;
  }
  let text = lines
    // the semi-colon added after the declarations, which a formatter might keep on its own line
    .filter(|line| line.trim() != ";")
    .map(|line| line.strip_prefix(&indent_text).unwrap_or(line))
    .collect::<Vec<_>>()
    .join("\n");
  Some(trim_blank_lines(&text).to_string())
}

/// Removes the lines at the start that only have whitespace and the whitespace at the end.
fn trim_blank_lines(text: &str) -> &str {
  let start = match text.find(|c: char| !c.is_whitespace()) {
    Some(index) => text[..index].rfind('\n').map(|index| index + 1).unwrap_or(0),
    None => text.len(),
  };
  text[start..].trim_end()
}

/// Removes the leading whitespace that all the lines with text have in common.
fn dedent(text: &str) -> String {
  let mut common_indent: Option<&str> = None;
  for line in text.lines().filter(|line| !line.trim().is_empty()) {
    let indent = &line[..line.len() - line.trim_start().len()];
    common_indent = Some(match common_indent {
      Some(common_indent) => {
        let len = common_indent.bytes().zip(indent.bytes()).take_while(|(a, b)| a == b).count();
        &common_indent[..len]
      }
      None => indent,
    });
  }
  match common_indent {
    Some(common_indent) if !common_indent.is_empty() => text
      .lines()
      .map(|line| line.strip_prefix(common_indent).unwrap_or(line.trim_start()))
      .collect::<Vec<_>>()
      .join("\n"),
    _ => text.to_string(),
  }
}

#[cfg(test)]
mod test {
  use super::*;
  use crate::configuration::ConfigurationBuilder;

  #[test]
  fn dedents_and_trims_text() {
    let file = EmbeddedFile::new("html", "\n  \n    <div>\n      <b>a</b>\n    </div>\n  ");
    assert_eq!(file.file_path, PathBuf::from("file.html"));
    assert_eq!(file.text, "<div>\n  <b>a</b>\n</div>");
  }

  #[test]
  fn keeps_text_without_common_indent() {
    let file = EmbeddedFile::new("html", "<div>\n  <b>a</b>\n</div>");
    assert_eq!(file.text, "<div>\n  <b>a</b>\n</div>");
  }

  #[test]
  fn trims_formatted_text() {
    let config = ConfigurationBuilder::new().build();
    let file = EmbeddedFile::new("html", "<div></div>");
    assert_eq!(file.into_template_text("\n<div></div>\n\n", &config).unwrap(), "<div></div>");
  }

  #[test]
  fn wraps_css_in_rule() {
    let config = ConfigurationBuilder::new().indent_width(2).build();
    let file = EmbeddedFile::new("less", "\n    color: red;\n    .a { b: c; }\n  ");
    assert_eq!(file.file_path, PathBuf::from("file.less"));
    assert_eq!(file.text, "a{\ncolor: red;\n.a { b: c; }\n;}\n");
    assert_eq!(
      file
        .into_template_text("a {\n  color: red;\n\n  .a {\n    b: c;\n  }\n  ;\n}\n", &config)
        .unwrap(),
      "color: red;\n\n.a {\n  b: c;\n}"
    );
  }

  #[test]
  fn unwraps_css_rule_with_tabs() {
    let config = ConfigurationBuilder::new().use_tabs(true).build();
    let file = EmbeddedFile::new("css", "color: red;");
    assert_eq!(file.into_template_text("a {\n\tcolor: red;\n}\n", &config).unwrap(), "color: red;");
  }

  #[test]
  fn leaves_css_in_unexpected_shape() {
    let config = ConfigurationBuilder::new().build();
    let file = EmbeddedFile::new("less", "color: red;");
    assert!(file.into_template_text("color: red;\n", &config).is_none());
  }

  #[test]
  fn does_not_wrap_sass() {
    let file = EmbeddedFile::new("sass", "color: red");
    assert_eq!(file.text, "color: red");
  }
}
