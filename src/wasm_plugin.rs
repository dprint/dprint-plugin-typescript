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
    let config = resolve_config(config, global_config);
    PluginResolveConfigurationResult {
      config: config.config,
      diagnostics: config.diagnostics,
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

  fn check_config_updates(&self, _message: CheckConfigUpdatesMessage) -> Result<Vec<ConfigChange>, anyhow::Error> {
    Ok(Vec::new())
  }

  fn plugin_info(&mut self) -> PluginInfo {
    let version = env!("CARGO_PKG_VERSION").to_string();
    PluginInfo {
      name: env!("CARGO_PKG_NAME").to_string(),
      version: version.clone(),
      config_key: "typescript".to_string(),
      help_url: env!("CARGO_PKG_HOMEPAGE").to_string(),
      config_schema_url: env!("DPRINT_SCHEMA_URL_TEMPLATE").replace("{VERSION}", &version),
      update_url: Some(env!("DPRINT_UPDATE_URL").to_string()),
    }
  }

  fn license_text(&mut self) -> String {
    std::str::from_utf8(include_bytes!("../LICENSE")).unwrap().into()
  }

  fn format(&mut self, request: SyncFormatRequest<Configuration>, _format_with_host: impl FnMut(SyncHostFormatRequest) -> FormatResult) -> FormatResult {
    let file_text = String::from_utf8(request.file_bytes)?;
    super::format_text(super::FormatTextOptions {
      path: request.file_path,
      extension: None,
      text: file_text,
      config: request.config,
      // todo: support this in Wasm
      external_formatter: None,
    })
    .map(|maybe_text| maybe_text.map(|t| t.into_bytes()))
  }
}

generate_plugin_code!(TypeScriptPluginHandler, TypeScriptPluginHandler);
