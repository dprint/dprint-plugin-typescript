// @ts-check
const fs = require("fs");
const path = require("path");
const args = process.argv.slice(2);
const wasmPath = path.join(__dirname, "../../target/wasm32-unknown-unknown/release/dprint_plugin_typescript.wasm");
const wasmBytes = fs.readFileSync(wasmPath);

let output = "module.exports.encodedBuffer = \"";
output += wasmBytes.toString("base64");
output += "\";\n";

fs.writeFileSync(path.join(__dirname, "buffer.generated.js"), output);

if (args.length > 0) {
    // update the version based on the first argument
    const packageJsonPath = path.join(__dirname, "package.json");
    const packageJsonText = fs.readFileSync(packageJsonPath, "utf8");
    const packageJson = JSON.parse(packageJsonText);
    packageJson.version = args[0];
    fs.writeFileSync(packageJsonPath, JSON.stringify(packageJson, undefined, 2) + "\n");
}
