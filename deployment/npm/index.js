/**
 * Gets the path to the Wasm module.
 * @returns {string}
 */
function getPath() {
  return require("path").join(__dirname, "plugin.wasm");
}

module.exports = {
  getPath,
};
