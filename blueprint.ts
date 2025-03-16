import type { Json } from "./mod.ts";

type Blueprint = {
  preamble: {
    title: string;
    description: string;
    version: string;
    plutusVersion: string;
    license: string;
  };
  validators: {
    title: string;
    datum?: {
      title: string;
      schema: {
        $ref: string;
      };
    };
    redeemer: {
      title: string;
      schema: {
        $ref: string;
      };
    };
    parameters?: {
      title: string;
      schema: {
        $ref: string;
      };
    }[];
    compiledCode: string;
    hash: string;
  }[];
  definitions: Record<string, {
    title: string;
    schema: {
      $ref: string;
    };
  }>;
};

const plutusJson: Blueprint = JSON.parse(
  Deno.readTextFileSync("plutus.json").replaceAll("~1", "/"),
);

const plutusVersion = "Plutus" +
  plutusJson.preamble.plutusVersion.toUpperCase();

const definitions = plutusJson.definitions;

const validators = plutusJson.validators.filter((validator) =>
  !validator.title.includes(".else")
).map((validator) => {
  const title = validator.title;
  const name = title
    .split(".")
    .map((w) => upperFirst(snakeToCamel(w)))
    .join("");
  const datum = validator.datum;
  const datumTitle = datum ? snakeToCamel(datum.title) : null;
  const datumSchema = datum
    ? { shape: resolveSchema(datum.schema), definitions }
    : null;

  const redeemer = validator.redeemer;
  const redeemerTitle = snakeToCamel(redeemer.title);
  const redeemerSchema = {
    shape: resolveSchema(redeemer.schema),
    definitions,
  };

  const params = validator.parameters || [];
  const paramsSchema = {
    shape: {
      dataType: "list",
      items: params.map((param) => resolveSchema(param.schema)),
    },
    definitions,
  };
  const paramsArgs = params.map((
    param,
    index,
  ) => [
    snakeToCamel(param.title),
    schemaToType(paramsSchema.shape.items[index]),
  ]);

  const script = validator.compiledCode;

  return `export interface ${name} {
    new (${paramsArgs.map((param) => param.join(":")).join(",")}): Script;${
    datum ? `\n${datumTitle}: ${schemaToType(datumSchema?.shape)};` : ""
  }
    ${redeemerTitle}: ${schemaToType(redeemerSchema.shape)};
  };

  export const ${name} = Object.assign(
    function (${paramsArgs.map((param) => param.join(":")).join(",")}) {${
    paramsArgs.length > 0
      ? `return { type: "${plutusVersion}", script: applyParamsToScript([${
        paramsArgs.map((param) => param[0]).join(",")
      }], "${script}", { "shape":${
        JSON.stringify(paramsSchema.shape)
      }, definitions } as any) };`
      : `return {type: "${plutusVersion}", script: "${script}"};`
  }},
    ${
    datum
      ? `{${datumTitle}: { "shape": ${
        JSON.stringify(datumSchema?.shape)
      }, definitions }},`
      : ""
  }
    {${redeemerTitle}: { "shape": ${
    JSON.stringify(redeemerSchema.shape)
  }, definitions }},
  ) as unknown as ${name};`;
});

function definitionsToTypes(definitions: Blueprint["definitions"]): string {
  return Object.entries(definitions).map(([name, schema]) =>
    `export type ${upperFirst(snakeToCamel(name))} = ${schemaToType(schema)};`
  ).join("\n");
}

function resolveSchema(schema: Json): Json {
  if (schema.items) {
    if (schema.items instanceof Array) {
      return {
        ...schema,
        items: schema.items.map((item: Json) => resolveSchema(item)),
      };
    } else {
      return {
        ...schema,
        items: resolveSchema(schema.items),
      };
    }
  } else if (schema.anyOf) {
    return {
      ...schema,
      anyOf: schema.anyOf.map((a: Json) => ({
        ...a,
        fields: a.fields.map((field: Json) => ({
          ...resolveSchema(field),
          title: field.title ? snakeToCamel(field.title) : undefined,
        })),
      })),
    };
  } else if (schema.keys && schema.values) {
    return {
      ...schema,
      keys: resolveSchema(schema.keys),
      values: resolveSchema(schema.values),
    };
  } else {
    return schema;
  }
}

function resolveDefinitions(
  definitions: Blueprint["definitions"],
): Blueprint["definitions"] {
  return Object.fromEntries(
    Object.entries(definitions).map(([name, schema]) => {
      return [name, resolveSchema(schema)];
    }),
  );
}

function schemaToType(schema: Json): string {
  if (!schema) throw new Error("Could not generate type.");
  const shapeType = (schema.anyOf ? "enum" : schema["$ref"] ? "$ref" : "") ||
    schema.dataType;

  switch (shapeType) {
    case "integer": {
      return "bigint";
    }
    case "bytes": {
      return "string";
    }
    case "constructor": {
      if (isVoid(schema)) {
        return "undefined";
      } else {
        return `{${
          schema.fields.map((field: Json) =>
            `${snakeToCamel(field.title || "wrapper")}:${schemaToType(field)}`
          ).join(";")
        }}`;
      }
    }
    case "enum": {
      // When enum has only one entry it's a single constructor/record object
      if (schema.anyOf.length === 1) {
        return schemaToType(schema.anyOf[0]);
      }
      if (isBoolean(schema)) {
        return "boolean";
      }
      if (isNullable(schema)) {
        return `${schemaToType(schema.anyOf[0].fields[0])} | null`;
      }
      return schema.anyOf.map((entry: Json) =>
        entry.fields.length === 0
          ? `"${snakeToCamel(entry.title)}"`
          : `{${snakeToCamel(entry.title)}: ${
            entry.fields[0].title
              ? `{${
                entry.fields.map((field: Json) =>
                  [snakeToCamel(field.title), schemaToType(field)].join(":")
                ).join(",")
              }}}`
              : `[${
                entry.fields.map((field: Json) => schemaToType(field)).join(",")
              }]}`
          }`
      ).join(" | ");
    }
    case "list": {
      if (schema.items instanceof Array) {
        return `[${
          schema.items.map((item: Json) => schemaToType(item)).join(",")
        }]`;
      } else {
        return `Array<${schemaToType(schema.items)}>`;
      }
    }
    case "map": {
      return `Map<${schemaToType(schema.keys)}, ${
        schemaToType(schema.values)
      }>`;
    }
    case "$ref": {
      const definition: string = schema["$ref"].split("#/definitions/")[1];
      return upperFirst(snakeToCamel(definition));
    }
    case undefined: {
      return "Data";
    }
  }
  throw new Error("Could not type cast data.");
}

function isBoolean(shape: Json): boolean {
  return shape.anyOf && shape.anyOf[0]?.title === "False" &&
    shape.anyOf[1]?.title === "True";
}

function isVoid(shape: Json): boolean {
  return shape.index === 0 && shape.fields.length === 0;
}

function isNullable(shape: Json): boolean {
  return shape.anyOf && shape.anyOf[0]?.title === "Some" &&
    shape.anyOf[1]?.title === "None";
}

function snakeToCamel(s: string): string {
  const withUnderscore = s.charAt(0) === "_" ? s.charAt(0) : "";
  return withUnderscore +
    (withUnderscore ? s.slice(1) : s).replace(
      /([-_$/][a-zA-Z])/g,
      (group) =>
        group
          .toUpperCase()
          .replace("-", "")
          .replace("_", "")
          .replace("$", "")
          .replace("/", ""),
    );
}

function upperFirst(s: string): string {
  const withUnderscore = s.charAt(0) === "_" ? s.charAt(0) : "";
  return withUnderscore +
    s.charAt(withUnderscore ? 1 : 0).toUpperCase() +
    s.slice((withUnderscore ? 1 : 0) + 1);
}

function replaceBlueprintImport(url: string): string {
  const jsrSubstring = "https://jsr.io/@spacebudz/lucid/";
  if (url.startsWith(jsrSubstring)) {
    const version = url.split(jsrSubstring)[1].replace(
      "/blueprint.ts",
      "",
    );
    return "jsr:@spacebudz/lucid@" + version;
  }
  return url.replace("/blueprint.ts", "/mod.ts");
}

const imports = `// deno-lint-ignore-file
import { applyParamsToScript, Data, Script } from "${
  replaceBlueprintImport(import.meta.url)
}"`;

const plutus = imports +
  "\n\n" +
  `${definitionsToTypes(definitions)}` +
  "\n\n" +
  `const definitions = ${JSON.stringify(resolveDefinitions(definitions))};` +
  "\n\n" +
  validators.join("\n\n");

await Deno.writeTextFile("plutus.ts", plutus);
await new Deno.Command(Deno.execPath(), {
  args: ["fmt", "plutus.ts"],
  stderr: "piped",
}).output();
console.log(
  "%cGenerated %cplutus.ts",
  "color: green; font-weight: bold",
  "font-weight: bold",
);
