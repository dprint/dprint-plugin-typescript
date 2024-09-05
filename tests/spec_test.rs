use std::path::PathBuf;
use std::sync::Arc;

use dprint_core::configuration::*;
use dprint_development::*;
use dprint_plugin_typescript::configuration::*;
use dprint_plugin_typescript::*;

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

        format_text(file_name, None, file_text.into(), &config_result.config)
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
