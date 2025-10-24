fn main() {
    // Read Cargo.toml and extract custom metadata
    let manifest = std::env::var("CARGO_MANIFEST_DIR").unwrap();
    let cargo_toml = std::fs::read_to_string(format!("{}/Cargo.toml", manifest))
        .expect("Failed to read Cargo.toml");

    // Parse TOML to get metadata
    let toml: toml::Table = cargo_toml.parse().expect("Failed to parse Cargo.toml");

    if let Some(package) = toml.get("package") {
        if let Some(metadata) = package.get("metadata") {
            if let Some(update_url) = metadata.get("dprint_update_url") {
                if let Some(url_str) = update_url.as_str() {
                    println!("cargo:rustc-env=DPRINT_UPDATE_URL={}", url_str);
                }
            }
            if let Some(schema_url_template) = metadata.get("dprint_schema_url_template") {
                if let Some(url_str) = schema_url_template.as_str() {
                    println!("cargo:rustc-env=DPRINT_SCHEMA_URL_TEMPLATE={}", url_str);
                }
            }
        }
    }
}
