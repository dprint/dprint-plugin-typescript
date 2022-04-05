extern crate dprint_development;
extern crate dprint_plugin_typescript;

//#[macro_use] extern crate debug_here;

use std::fs;
use std::path::PathBuf;
use std::time::Instant;

use dprint_core::configuration::*;
use dprint_development::*;
use dprint_plugin_typescript::configuration::*;
use dprint_plugin_typescript::*;

#[allow(dead_code)]
fn test_performance() {
  // run this with `cargo test --release -- --nocapture`

  // This file was not written with an 80 line width in mind so overall
  // it's not too bad, but there are a few small issues to fix here and there.
  let config = ConfigurationBuilder::new().line_width(80).quote_style(QuoteStyle::PreferSingle).build();
  let file_text = fs::read_to_string("tests/performance/checker.txt").expect("Expected to read.");

  //debug_here!();

  for i in 0..10 {
    let start = Instant::now();
    let result = format_text(&PathBuf::from("checker.ts"), &file_text, &config).expect("Could not parse...");

    println!("{}ms", start.elapsed().as_millis());
    println!("---");

    if i == 0 {
      fs::write("tests/performance/checker_output.txt", result.as_deref().unwrap_or(&file_text)).expect("Expected to write to the file.");
    }
  }
}

#[test]
fn test_specs() {
  //debug_here!();
  let global_config = GlobalConfiguration::default();

  run_specs(
    &PathBuf::from("./tests/specs"),
    &ParseSpecOptions { default_file_name: "file.ts" },
    &RunSpecsOptions {
      fix_failures: false,
      format_twice: true,
    },
    {
      let global_config = global_config.clone();
      move |file_name, file_text, spec_config| {
        let config_result = resolve_config(parse_config_key_map(spec_config), &global_config);
        ensure_no_diagnostics(&config_result.diagnostics);

        format_text(file_name, file_text, &config_result.config)
      }
    },
    move |_file_name, _file_text, _spec_config| {
      #[cfg(feature = "tracing")]
      {
        let config_result = resolve_config(parse_config_key_map(_spec_config), &global_config);
        ensure_no_diagnostics(&config_result.diagnostics);
        return serde_json::to_string(&trace_file(_file_name, _file_text, &config_result.config)).unwrap();
      }

      #[cfg(not(feature = "tracing"))]
      panic!("\n====\nPlease run with `cargo test --features tracing` to get trace output\n====\n")
    },
  )
}
