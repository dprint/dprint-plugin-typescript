/**
 * Gets a buffer representing the WASM module.
 * @returns {ArrayBuffer}
 */
function getBuffer() {
  const encodedBuffer = require("./buffer.generated").encodedBuffer;
  return decodeEncodedBuffer(encodedBuffer);
}

/**
 * @param {string} encodedBuffer
 * @returns {ArrayBuffer}
 */
function decodeEncodedBuffer(encodedBuffer) {
  // https://stackoverflow.com/a/51473757/188246
  const binaryString = toBinaryString();
  const bytes = new Uint8Array(binaryString.length);
  for (let i = 0; i < binaryString.length; i++) {
    bytes[i] = binaryString.charCodeAt(i);
  }
  return bytes.buffer;

  function toBinaryString() {
    if (typeof atob === "function") {
      return atob(encodedBuffer);
    } else {
      return Buffer.from(encodedBuffer, "base64").toString("binary");
    }
  }
}

module.exports = {
  getBuffer,
};
