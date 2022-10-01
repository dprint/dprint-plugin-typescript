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
  packageJson.version = args[0];
  fs.writeFileSync(packageJsonPath, JSON.stringify(packageJson, undefined, 2) + "\n");
}
