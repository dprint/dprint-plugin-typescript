use dprint_core::configuration::ConfigKeyMap;
use dprint_core::configuration::GlobalConfiguration;
use dprint_core::generate_plugin_code;
use dprint_core::plugins::CheckConfigUpdatesMessage;
use dprint_core::plugins::ConfigChange;
use dprint_core::plugins::FileMatchingInfo;
use dprint_core::plugins::FormatResult;
use dprint_core::plugins::PluginInfo;
use dprint_core::plugins::PluginResolveConfigurationResult;
use dprint_core::plugins::SyncFormatRequest;
use dprint_core::plugins::SyncHostFormatRequest;
use dprint_core::plugins::SyncPluginHandler;

use super::configuration::resolve_config;
use super::configuration::Configuration;

struct TypeScriptPluginHandler;

impl SyncPluginHandler<Configuration> for TypeScriptPluginHandler {
  fn resolve_config(&mut self, config: ConfigKeyMap, global_config: &GlobalConfiguration) -> PluginResolveConfigurationResult<Configuration> {
    let result = resolve_config(config, global_config);
    PluginResolveConfigurationResult {
      config: result.config,
      diagnostics: result.diagnostics,
      file_matching: FileMatchingInfo {
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
      },
    }
  }

  fn plugin_info(&mut self) -> PluginInfo {
    let version = env!("CARGO_PKG_VERSION");
    PluginInfo {
      name: env!("CARGO_PKG_NAME").to_string(),
      version: version.to_string(),
      config_key: "typescript".to_string(),
      help_url: "https://dprint.dev/plugins/typescript".to_string(),
      config_schema_url: format!("https://plugins.dprint.dev/dprint/dprint-plugin-typescript/{}/schema.json", version),
      update_url: Some("https://plugins.dprint.dev/dprint/dprint-plugin-typescript/latest.json".to_string()),
    }
  }

  fn check_config_updates(&self, _message: CheckConfigUpdatesMessage) -> anyhow::Result<Vec<ConfigChange>> {
    Ok(Vec::new())
  }

  fn license_text(&mut self) -> String {
    std::str::from_utf8(include_bytes!("../LICENSE")).unwrap().into()
  }

  fn format(&mut self, request: SyncFormatRequest<Configuration>, _format_with_host: impl FnMut(SyncHostFormatRequest) -> FormatResult) -> FormatResult {
    if request.range.is_some() {
      return Ok(None); // not implemented
    }

    let file_text = String::from_utf8(request.file_bytes)?;
    super::format_text(&request.file_path, file_text, request.config).map(|maybe_text| maybe_text.map(|t| t.into_bytes()))
  }
}

generate_plugin_code!(TypeScriptPluginHandler, TypeScriptPluginHandler);
