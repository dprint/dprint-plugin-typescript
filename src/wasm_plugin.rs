use std::cell::RefCell;

use dprint_core::configuration::ConfigKeyMap;
use dprint_core::configuration::GlobalConfiguration;
use dprint_core::generate_plugin_code;
use dprint_core::plugins::CheckConfigUpdatesMessage;
use dprint_core::plugins::ConfigChange;
use dprint_core::plugins::CriticalFormatError;
use dprint_core::plugins::FileMatchingInfo;
use dprint_core::plugins::FormatError;
use dprint_core::plugins::FormatResult;
use dprint_core::plugins::PluginInfo;
use dprint_core::plugins::PluginResolveConfigurationResult;
use dprint_core::plugins::SyncFormatRequest;
use dprint_core::plugins::SyncHostFormatRequest;
use dprint_core::plugins::SyncPluginHandler;

use super::configuration::resolve_config;
use super::configuration::Configuration;
use super::embedded_file::EmbeddedFile;

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
        additive: false,
      },
    }
  }

  fn check_config_updates(&self, _message: CheckConfigUpdatesMessage) -> Result<Vec<ConfigChange>, FormatError> {
    Ok(Vec::new())
  }

  fn plugin_info(&mut self) -> PluginInfo {
    let version = env!("CARGO_PKG_VERSION").to_string();
    PluginInfo {
      name: env!("CARGO_PKG_NAME").to_string(),
      version: version.clone(),
      config_key: "typescript".to_string(),
      help_url: "https://dprint.dev/plugins/typescript".to_string(),
      config_schema_url: format!("https://plugins.dprint.dev/dprint/dprint-plugin-typescript/{}/schema.json", version),
      update_url: Some("https://plugins.dprint.dev/dprint/dprint-plugin-typescript/latest.json".to_string()),
    }
  }

  fn license_text(&mut self) -> String {
    std::str::from_utf8(include_bytes!("../LICENSE")).unwrap().into()
  }

  fn format(&mut self, request: SyncFormatRequest<Configuration>, format_with_host: impl FnMut(SyncHostFormatRequest) -> FormatResult) -> FormatResult {
    // `String::from_utf8` reuses the buffer, but going to an `Arc<str>` has to
    // copy because the allocation needs room for the reference count
    let file_text = String::from_utf8(request.file_bytes)?;
    // the external formatter can only be called by reference
    let format_with_host = RefCell::new(format_with_host);
    // a critical error (ex. the host is shutting down) only reaches the formatter
    // as a message, so it's kept to return it instead of the formatter's error
    let critical_error = RefCell::new(None);
    let external_formatter = |extension: &str, text: String, config: &Configuration| -> Result<Option<String>, Box<dyn std::error::Error + Send + Sync>> {
      match format_embedded_with_host(extension, &text, config, &mut *format_with_host.borrow_mut()) {
        Ok(text) => Ok(text),
        Err(err) => {
          let message = dprint_core::plugins::error_to_string(&err);
          if err.downcast_ref::<CriticalFormatError>().is_some() {
            critical_error.borrow_mut().get_or_insert(err);
          }
          Err(message.into())
        }
      }
    };
    let result = super::format_text(super::FormatTextOptions {
      path: request.file_path,
      extension: None,
      text: file_text.into(),
      config: request.config,
      external_formatter: Some(&external_formatter),
      arena_pool: Some(request.arena_pool),
    });
    if let Some(err) = critical_error.into_inner() {
      return Err(err);
    }
    let maybe_text = result.map_err(FormatError::new)?;
    Ok(maybe_text.map(|t| t.into_bytes()))
  }
}

/// Formats the text of a tagged template with the plugin the host uses for the extension.
///
/// Returns `None` when the host says the text didn't change, which is also what it says
/// when no plugin formats the extension, so the template is left as is.
fn format_embedded_with_host(
  extension: &str,
  text: &str,
  config: &Configuration,
  format_with_host: &mut dyn FnMut(SyncHostFormatRequest) -> FormatResult,
) -> Result<Option<String>, FormatError> {
  let file = EmbeddedFile::new(extension, text);
  // the template's text is reindented with this configuration, so it's formatted with it too
  let mut override_config = ConfigKeyMap::new();
  override_config.insert("lineWidth".into(), (config.line_width as i32).into());
  override_config.insert("indentWidth".into(), (config.indent_width as i32).into());
  override_config.insert("useTabs".into(), config.use_tabs.into());
  override_config.insert("newLineKind".into(), "lf".into());
  let maybe_bytes = format_with_host(SyncHostFormatRequest {
    file_path: &file.file_path,
    file_bytes: file.text.as_bytes(),
    range: None,
    override_config: &override_config,
  })?;
  let Some(bytes) = maybe_bytes else {
    return Ok(None);
  };
  let formatted = String::from_utf8(bytes)?;
  Ok(file.into_template_text(&formatted, config))
}

generate_plugin_code!(TypeScriptPluginHandler, TypeScriptPluginHandler);
