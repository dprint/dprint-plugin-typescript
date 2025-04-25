use std::path::{Path, PathBuf};
use std::sync::Arc;

use deno_ast::MediaType;
use dprint_core::configuration::*;
use dprint_development::*;
use dprint_plugin_typescript::configuration::*;
use dprint_plugin_typescript::*;

fn external_formatter(media_type: MediaType, text: String, config: &Configuration) -> Option<String> {
  match media_type {
    MediaType::Css => format_embedded_css(&text, config),
    MediaType::Html => format_html(&text, config),
    MediaType::Sql => format_sql(&text, config),
    _ => unreachable!(),
  }
}

fn format_embedded_css(text: &str, config: &Configuration) -> Option<String> {
  use malva::config;
  let options = config::FormatOptions {
    layout: config::LayoutOptions {
      indent_width: config.indent_width as usize,
      ..Default::default()
    },
    ..Default::default()
  };
  // Wraps the text in a css block of `a { ... }`
  // to make it valid css (scss)
  let Ok(text) = malva::format_text(&format!("a{{\n{}\n}}", text), malva::Syntax::Scss, &options) else {
    return None;
  };
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
  Some(buf.join("\n").to_string())
}

fn format_html(text: &str, config: &Configuration) -> Option<String> {
  use markup_fmt::config;
  let options = config::FormatOptions {
    layout: config::LayoutOptions {
      indent_width: config.indent_width as usize,
      ..Default::default()
    },
    ..Default::default()
  };
  let Ok(text) = markup_fmt::format_text(text, markup_fmt::Language::Html, &options, |code, _| {
    Ok::<_, std::convert::Infallible>(code.into())
  }) else {
    return None;
  };
  Some(text.to_string())
}

fn format_sql(text: &str, config: &Configuration) -> Option<String> {
  let options = dprint_plugin_sql::configuration::ConfigurationBuilder::new()
    .indent_width(config.indent_width)
    .build();
  dprint_plugin_sql::format_text(Path::new("_path.sql"), text, &options).ok().flatten()
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
      Arc::new(move |file_name, file_text, spec_config| {
        let spec_config: ConfigKeyMap = serde_json::from_value(spec_config.clone().into()).unwrap();
        let config_result = resolve_config(spec_config, &global_config);
        ensure_no_diagnostics(&config_result.diagnostics);

        format_text(FormatTextOptions {
          path: file_name,
          extension: None,
          text: file_text.into(),
          config: &config_result.config,
          external_formatter: Some(&external_formatter),
        })
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
