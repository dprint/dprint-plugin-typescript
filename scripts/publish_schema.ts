// Generates the outputs of the code examples in the config schema with the
// built plugin and checks that the plugin accepts every property in the schema.
//
// Usage: deno run -A ./scripts/publish_schema.ts <plugin.wasm> <schema.json>
//
// This overwrites the schema file with the outputs added, so it's run on CI
// before the schema is uploaded to a release. An example is declared with an
// `x-example` on a property, or on the definition named after the property:
//
// "x-example": {
//   "config": { "lineWidth": 40 }, // optional
//   "extension": "tsx", // optional, defaults to "ts"
//   "input": ["line 1", "line 2"]
// }
//
// The output of formatting the input with each of the property's values is
// added as `"outputs": { "<value>": "<formatted text>" }`.
import { createFromBuffer } from "npm:@dprint/formatter@0.4.1";

interface SchemaDefinition {
  $ref?: string;
  default?: unknown;
  type?: string | string[];
  const?: unknown;
  oneOf?: SchemaDefinition[];
  enum?: unknown[];
  deprecationMessage?: string;
  "x-example"?: Example;
}

interface Example {
  config?: Record<string, unknown>;
  extension?: string;
  input: string[];
  outputs?: Record<string, string>;
}

interface Schema {
  definitions: Record<string, SchemaDefinition>;
  properties: Record<string, SchemaDefinition>;
}

const [wasmPath, schemaPath] = Deno.args;
if (wasmPath == null || schemaPath == null) {
  throw new Error("Usage: publish_schema.ts <plugin.wasm> <schema.json>");
}

const schema: Schema = JSON.parse(Deno.readTextFileSync(schemaPath));
const formatter = createFromBuffer(Deno.readFileSync(wasmPath));

checkPropertiesAreAccepted();
const errors: string[] = [];
for (const [name, definition] of [...Object.entries(schema.definitions), ...Object.entries(schema.properties)]) {
  const example = definition["x-example"];
  if (example != null) {
    try {
      example.outputs = generateOutputs(name, example);
    } catch (err) {
      errors.push((err as Error).message);
    }
  }
}
if (errors.length > 0) {
  throw new Error(errors.join("\n"));
}

Deno.writeTextFileSync(schemaPath, JSON.stringify(schema, undefined, 2) + "\n");

// ensures the schema doesn't have properties the plugin doesn't know about
function checkPropertiesAreAccepted() {
  const config = Object.fromEntries(
    Object.entries(schema.properties)
      .map(([name, property]) => [name, resolveRef(property).default])
      .filter(([_name, value]) => value !== undefined),
  );
  setConfig(config, "the property defaults");
}

function generateOutputs(propertyName: string, example: Example) {
  const property = schema.properties[propertyName];
  if (property == null) {
    throw new Error(`An example is on "${propertyName}", which isn't a property.`);
  }
  const values = getValues(resolveRef(property));
  if (values.length < 2) {
    throw new Error(`The example of "${propertyName}" is on a property without multiple values.`);
  }

  const outputs: Record<string, string> = {};
  for (const value of values) {
    setConfig({ ...example.config, [propertyName]: value }, `the example of "${propertyName}"`);
    outputs[typeof value === "string" ? value : JSON.stringify(value)] = formatter.formatText({
      filePath: `example.${example.extension ?? "ts"}`,
      fileText: example.input.join("\n") + "\n",
    });
  }
  if (new Set(Object.values(outputs)).size === 1) {
    throw new Error(`The example of "${propertyName}" formats the same with every value.`);
  }
  return outputs;
}

function getValues(definition: SchemaDefinition): unknown[] {
  if (definition.oneOf != null) {
    // deprecated values are aliases of other values, and an option that refers
    // to a definition (ex. a kind of sorting) has the values of that definition
    return definition.oneOf.flatMap((option) => {
      if (option.$ref != null) {
        return getValues(resolveRef(option));
      }
      return option.deprecationMessage == null && option.const !== undefined ? [option.const] : [];
    });
  } else if (definition.enum != null) {
    return definition.enum;
  } else if (definition.type === "boolean") {
    return [true, false];
  } else {
    return [];
  }
}

function resolveRef(definition: SchemaDefinition): SchemaDefinition {
  if (definition.$ref == null) {
    return definition;
  }
  const { $ref, ...rest } = definition;
  const referenced = schema.definitions[$ref.replace("#/definitions/", "")];
  if (referenced == null) {
    throw new Error(`Could not find ${$ref}.`);
  }
  // a property may set its own values alongside the reference (ex. an example)
  return { ...referenced, ...rest };
}

function setConfig(config: Record<string, unknown>, description: string) {
  formatter.setConfig({}, config);
  const diagnostics = formatter.getConfigDiagnostics();
  if (diagnostics.length > 0) {
    throw new Error(`The plugin had diagnostics for ${description}:\n${JSON.stringify(diagnostics, undefined, 2)}`);
  }
}
