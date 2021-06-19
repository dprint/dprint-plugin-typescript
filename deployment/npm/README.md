# @dprint/typescript

npm distribution of [dprint-plugin-typescript](https://github.com/dprint/dprint-plugin-typescript).

Use this with [@dprint/formatter](https://github.com/dprint/js-formatter) or just use @dprint/formatter and download the [dprint-plugin-typescript WASM file](https://github.com/dprint/dprint-plugin-typescript/releases).

## Example

```ts
import { createFromBuffer } from "@dprint/formatter";
import { getBuffer } from "@dprint/typescript";

const formatter = createFromBuffer(getBuffer());

console.log(formatter.formatText("test.ts", "const   t    =    5;"));
```
