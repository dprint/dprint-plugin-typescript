use dprint_core::configuration::ConfigKeyMap;
use dprint_core::configuration::GlobalConfiguration;
use dprint_core::configuration::ResolveConfigurationResult;
use dprint_core::generate_plugin_code;
use dprint_core::plugins::FormatResult;
use dprint_core::plugins::PluginInfo;
use dprint_core::plugins::SyncPluginHandler;
use std::path::Path;

use super::configuration::resolve_config;
use super::configuration::Configuration;

struct TypeScriptPluginHandler;

impl SyncPluginHandler<Configuration> for TypeScriptPluginHandler {
  fn resolve_config(&mut self, config: ConfigKeyMap, global_config: &GlobalConfiguration) -> ResolveConfigurationResult<Configuration> {
    resolve_config(config, global_config)
  }

  fn plugin_info(&mut self) -> PluginInfo {
    let version = env!("CARGO_PKG_VERSION").to_string();
    PluginInfo {
      name: env!("CARGO_PKG_NAME").to_string(),
      version: version.clone(),
      config_key: "typescript".to_string(),
      file_extensions: vec![
        String::from("ts"),
        String::from("tsx"),
        String::from("js"),
        String::from("jsx"),
        String::from("mjs"),
        String::from("cjs"),
        String::from("mts"),
        String::from("cts"),
      ],
      file_names: vec![],
      help_url: "https://dprint.dev/plugins/typescript".to_string(),
      config_schema_url: format!("https://plugins.dprint.dev/dprint/dprint-plugin-typescript/{}/schema.json", version),
      update_url: Some("https://plugins.dprint.dev/dprint/dprint-plugin-typescript/latest.json".to_string()),
    }
  }

  fn license_text(&mut self) -> String {
    std::str::from_utf8(include_bytes!("../LICENSE")).unwrap().into()
  }

  fn format(
    &mut self,
    file_path: &Path,
    file_text: &str,
    config: &Configuration,
    _format_with_host: impl FnMut(&Path, String, &ConfigKeyMap) -> FormatResult,
  ) -> FormatResult {
    super::format_text(file_path, file_text, config)
  }
}

generate_plugin_code!(TypeScriptPluginHandler, TypeScriptPluginHandler);
