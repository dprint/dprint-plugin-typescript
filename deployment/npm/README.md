# @dprint/typescript

npm distribution of [dprint-plugin-typescript](https://github.com/dprint/dprint-plugin-typescript).

Use this with [@dprint/formatter](https://github.com/dprint/js-formatter) or just use @dprint/formatter and download the [dprint-plugin-typescript Wasm file](https://github.com/dprint/dprint-plugin-typescript/releases).

## Example

```ts
import { createFromBuffer } from "@dprint/formatter";
import { getPath } from "@dprint/typescript";
import * as fs from "fs";

const buffer = fs.readFileSync(getPath());
const formatter = createFromBuffer(buffer);

console.log(formatter.formatText("test.ts", "const   t    =    5;"));
```
