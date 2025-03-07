use std::path::{Path, PathBuf};
use std::sync::Arc;

use deno_ast::MediaType;
use dprint_core::configuration::*;
use dprint_development::*;
use dprint_plugin_typescript::configuration::*;
use dprint_plugin_typescript::*;

fn external_formatter(media_type: MediaType, text: String) -> Option<String> {
  match media_type {
    MediaType::Css => format_embedded_css(&text),
    MediaType::Html => format_html(&text),
    MediaType::Sql => format_sql(&text),
    _ => unreachable!(),
  }
}
fn format_embedded_css(text: &str) -> Option<String> {
  use malva::config;
  let options = config::FormatOptions {
    layout: config::LayoutOptions {
      indent_width: 4,
      ..Default::default()
    },
    ..Default::default()
  };
  let Ok(text) = malva::format_text(&format!("a{{\n{}\n}}", text), malva::Syntax::Scss, &options) else {
    return None;
  };
  let mut buf = vec![];
  for (i, l) in text.lines().enumerate() {
    if i == 0 {
      continue;
    }
    if l.starts_with("}") {
      continue;
    }
    let mut chars = l.chars();
    // drop the first 4 chars
    chars.next();
    chars.next();
    chars.next();
    chars.next();
    buf.push(chars.as_str());
  }
  Some(buf.join("\n").to_string())
}

fn format_html(text: &str) -> Option<String> {
  use markup_fmt::config;
  let options = config::FormatOptions {
    layout: config::LayoutOptions {
      indent_width: 4,
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

fn format_sql(text: &str) -> Option<String> {
  let options = dprint_plugin_sql::configuration::ConfigurationBuilder::new().indent_width(4).build();
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

        format_text_with_external_formatter(file_name, None, file_text.into(), &config_result.config, Box::new(external_formatter))
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
