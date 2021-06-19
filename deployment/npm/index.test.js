// @ts-check
const assert = require("assert");
const createFromBuffer = require("@dprint/formatter").createFromBuffer;
const getBuffer = require("./index").getBuffer;

const formatter = createFromBuffer(getBuffer());
const result = formatter.formatText("file.ts", "const   t   = 5");

assert.strictEqual(result, "const t = 5;\n");
