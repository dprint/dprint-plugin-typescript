// @ts-check
const fs = require("fs");
const path = require("path");
const args = process.argv.slice(2);
const wasmPath = path.join(__dirname, "../../target/wasm32-unknown-unknown/release/dprint_plugin_typescript.wasm");
fs.copyFileSync(wasmPath, path.join(__dirname, "plugin.wasm"));

if (args.length > 0) {
  // update the version based on the first argument
  const packageJsonPath = path.join(__dirname, "package.json");
  const packageJsonText = fs.readFileSync(packageJsonPath, "utf8");
  const packageJson = JSON.parse(packageJsonText);
  if (args[0] === "sync-version") {
    const cargoTomlPath = path.join(__dirname, "../../Cargo.toml");
    const cargoTomlText = fs.readFileSync(cargoTomlPath, "utf8");
    const versionMatch = cargoTomlText.match(/^version\s*=\s*"([^"]+)"/m);
    if (!versionMatch) {
      throw new Error("Could not find version in Cargo.toml");
    }
    packageJson.version = versionMatch[1];
  } else {
    packageJson.version = args[0];
  }
  fs.writeFileSync(packageJsonPath, JSON.stringify(packageJson, undefined, 2) + "\n");
}
