use std::path::PathBuf;
use dprint_core::generate_plugin_code;
use super::configuration::{Configuration, resolve_config};

fn get_plugin_config_key() -> String {
    String::from("typescript")
}

fn get_plugin_file_extensions() -> Vec<String> {
    vec![String::from("ts"), String::from("tsx"), String::from("js"), String::from("jsx"), String::from("mjs"), String::from("cjs")]
}

fn get_plugin_help_url() -> String {
    String::from("https://dprint.dev/plugins/typescript")
}

fn get_plugin_config_schema_url() -> String {
    String::new() // none until https://github.com/microsoft/vscode/issues/98443 is resolved
}

fn format_text(file_path: &PathBuf, file_text: &str, config: &Configuration) -> Result<String, String> {
    super::format_text(file_path, file_text, config)
}

fn get_plugin_license_text() -> String {
    std::str::from_utf8(include_bytes!("../LICENSE")).unwrap().into()
}

// for clearing the configuration in the playground
#[no_mangle]
pub fn reset_config() {
    unsafe {
        RESOLVE_CONFIGURATION_RESULT.take();
    }
}

generate_plugin_code!();
