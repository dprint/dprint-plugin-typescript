use std::path::{Path, PathBuf};
use std::sync::Arc;

use dprint_core::configuration::*;
use dprint_development::*;
use dprint_plugin_typescript::configuration::*;
use dprint_plugin_typescript::*;

type Result<T> = std::result::Result<T, Box<dyn std::error::Error + Send + Sync + 'static>>;

fn external_formatter(lang: &str, text: String, config: &Configuration) -> Result<Option<String>> {
  match lang {
    "css" => format_embedded_css(&text, config),
    "html" => format_html(&text, config),
    "sql" => format_sql(&text, config),
    "graphql" => format_graphql_stub(&text, config),
    _ => Ok(None),
  }
}

/// Stub formatter for graphql — collapses whitespace runs to single spaces on each
/// line. Just enough to verify that the `graphql` language key is reached from
/// `gql`, `graphql`, and `graphql.experimental` tag patterns. Emits at 0-level
/// indent; the host re-indents to match the surrounding code.
fn format_graphql_stub(text: &str, _config: &Configuration) -> Result<Option<String>> {
  // collect non-empty collapsed lines so the second formatting pass is idempotent
  let lines: Vec<String> = text
    .lines()
    .map(|l| l.split_whitespace().collect::<Vec<_>>().join(" "))
    .filter(|l| !l.is_empty())
    .collect();
  if lines.is_empty() {
    return Ok(Some(String::new()));
  }
  let mut out = lines.join("\n");
  out.push('\n');
  Ok(Some(out))
}

fn format_embedded_css(text: &str, config: &Configuration) -> Result<Option<String>> {
  use malva::config;
  let options = config::FormatOptions {
    layout: config::LayoutOptions {
      indent_width: config.indent_width as usize,
      ..Default::default()
    },
    ..Default::default()
  };
  // Wraps the text in a css block of `a { ... }`
  // to make it valid css (less)
  // We choose LESS syntax because it allows us to use `@variable` as both value and mixin.
  // The latter works as placeholder for key-value pair.
  let text = malva::format_text(&format!("a{{\n{}\n;}}", text), malva::Syntax::Less, &options)?;
  let mut buf = vec![];
  for (i, l) in text.lines().enumerate() {
    // skip the first line (a {)
    if i == 0 {
      continue;
    }
    // skip the last line (})
    if l.starts_with("}") {
      continue;
    }
    let mut chars = l.chars();
    // drop the indentation
    for _ in 0..config.indent_width {
      chars.next();
    }
    buf.push(chars.as_str());
  }
  Ok(Some(buf.join("\n").to_string()))
}

fn format_html(text: &str, config: &Configuration) -> Result<Option<String>> {
  use markup_fmt::config;
  let options = config::FormatOptions {
    layout: config::LayoutOptions {
      indent_width: config.indent_width as usize,
      ..Default::default()
    },
    ..Default::default()
  };
  let css_options = malva::config::FormatOptions {
    layout: malva::config::LayoutOptions {
      indent_width: config.indent_width as usize,
      ..Default::default()
    },
    ..Default::default()
  };
  // formats the css in style elements like deno does, which errors when the css can't be parsed
  let text = markup_fmt::format_text(text, markup_fmt::Language::Html, &options, |code, hints| match hints.ext {
    "css" => malva::format_text(code, malva::Syntax::Css, &css_options).map(Into::into),
    _ => Ok(code.into()),
  })?;
  Ok(Some(text.to_string()))
}

fn format_sql(text: &str, config: &Configuration) -> Result<Option<String>> {
  let options = dprint_plugin_sql::configuration::ConfigurationBuilder::new()
    .indent_width(config.indent_width)
    .build();
  dprint_plugin_sql::format_text(Path::new("_path.sql"), text, &options).map_err(Into::into)
}

fn main() {
  //debug_here!();
  let global_config = GlobalConfiguration {
    // for the tests only because a higher indent width increases the likelihood of problems
    indent_width: Some(4),
    ..Default::default()
  };

  run_specs(
    &PathBuf::from("./tests/specs"),
    &ParseSpecOptions { default_file_name: "file.ts" },
    &RunSpecsOptions {
      fix_failures: false,
      format_twice: true,
    },
    {
      let global_config = global_config.clone();
      Arc::new(move |file_name, file_text, range, spec_config| {
        assert!(range.is_none(), "range formatting is not supported");
        let spec_config: ConfigKeyMap = serde_json::from_value(spec_config.clone().into()).unwrap();
        let config_result = resolve_config(spec_config, &global_config);
        ensure_no_diagnostics(&config_result.diagnostics);

        let result = format_text(FormatTextOptions {
          path: file_name,
          extension: None,
          text: file_text.into(),
          config: &config_result.config,
          external_formatter: Some(&external_formatter),
        })?;
        Ok(result)
      })
    },
    Arc::new(move |_file_name, _file_text, _spec_config| {
      #[cfg(feature = "tracing")]
      {
        let spec_config: ConfigKeyMap = serde_json::from_value(_spec_config.clone().into()).unwrap();
        let config_result = resolve_config(spec_config, &global_config);
        ensure_no_diagnostics(&config_result.diagnostics);
        return serde_json::to_string(&trace_file(_file_name, _file_text, &config_result.config)).unwrap();
      }

      #[cfg(not(feature = "tracing"))]
      panic!("\n====\nPlease run with `cargo test --features tracing` to get trace output\n====\n")
    }),
  )
}
