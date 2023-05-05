import packageJson from "./package.json" assert { type: "json" };
import { parse } from "https://deno.land/std@0.185.0/flags/mod.ts";

const flags = parse(Deno.args, {
  boolean: ["npm"],
});

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
  await Deno.readTextFile("plutus.json"),
);

const plutusVersion = "Plutus" +
  plutusJson.preamble.plutusVersion.toUpperCase();

const definitions = plutusJson.definitions;

const imports = `// deno-lint-ignore-file
import { applyParamsToScript, Data, Validator } from "${
  flags.npm
    ? "lucid-cardano"
    : `https://deno.land/x/lucid@${packageJson.version}/mod.ts`
}"`;

const validators = plutusJson.validators.map((validator) => {
  const title = validator.title;
  const name = (() => {
    const [a, b] = title.split(".");
    return a.slice(0, 1).toUpperCase() +
      snakeToCamel(a.slice(1)) +
      b.slice(0, 1).toUpperCase() +
      b.slice(1);
  })();
  const datum = validator.datum
    ? resolveSchema(validator.datum.schema, definitions)
    : null;
  const redeemer = resolveSchema(validator.redeemer.schema, definitions);
  const params = validator.parameters || [];
  const paramsSchema = {
    dataType: "list",
    items: params.map((param) => resolveSchema(param.schema, definitions)),
  };
  const paramsArgs = params && params.length > 0
    ? params.map((
      param,
      index,
    ) => [snakeToCamel(param.title), schemaToType(paramsSchema.items[index])])
    : null;
  const script = validator.compiledCode;

  return `export namespace ${name} {
    // ${title}
    ${
    datum
      ? `\n// Datum\nexport const ${datum.title || "Datum"} = ${
        JSON.stringify(datum)
      } as unknown as ${datum.title || "Datum"};
    export type ${datum.title || "Datum"} = ${schemaToType(datum)};`
      : ""
  }
    // Redeemer
    export const ${redeemer.title || "Redeemer"} = ${
    JSON.stringify(redeemer)
  } as unknown as ${redeemer.title || "Redeemer"};
    export type ${redeemer.title || "Redeemer"} = ${schemaToType(redeemer)};
    // Validator
    ${
    paramsArgs
      ? `export function validator(${
        paramsArgs.map((param) => param.join(":")).join(",")
      }): Validator { return { type: "${plutusVersion}", script: applyParamsToScript("${script}", [${
        paramsArgs.map((param) => param[0]).join(",")
      }], ${JSON.stringify(paramsSchema)}) }; }`
      : `export function validator(): Validator {return {type: "${plutusVersion}", script: "${script}"};}`
  };
  }`;
});

const plutus = imports + "\n\n" + validators.join("\n\n");

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

function resolveSchema(schema: any, definitions: any): any {
  if (schema.items) {
    if (schema.items instanceof Array) {
      return {
        ...schema,
        items: schema.items.map((item: any) =>
          resolveSchema(item, definitions)
        ),
      };
    } else {
      return {
        ...schema,
        items: resolveSchema(schema.items, definitions),
      };
    }
  } else if (schema.anyOf) {
    return {
      ...schema,
      anyOf: schema.anyOf.map((a: any) => ({
        ...a,
        fields: a.fields.map((field: any) => ({
          ...resolveSchema(field, definitions),
          title: field.title ? snakeToCamel(field.title) : undefined,
        })),
      })),
    };
  } else if (schema.keys && schema.values) {
    return {
      ...schema,
      keys: resolveSchema(schema.keys, definitions),
      values: resolveSchema(schema.values, definitions),
    };
  } else {
    if (schema["$ref"]) {
      const refKey =
        schema["$ref"].replaceAll("~1", "/").split("#/definitions/")[1];
      return resolveSchema(definitions[refKey], definitions);
    } else {
      return schema;
    }
  }
}

function schemaToType(schema: any): string {
  if (!schema) throw new Error("Could not generate type.");
  const shapeType = (schema.anyOf ? "enum" : "") || schema.dataType;

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
          schema.fields.map((field: any) =>
            `${field.title || "wrapper"}:${schemaToType(field)}`
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
      return schema.anyOf.map((entry: any) =>
        entry.fields.length === 0
          ? `"${entry.title}"`
          : `{${entry.title}: ${
            entry.fields[0].title
              ? `{${
                entry.fields.map((field: any) =>
                  [field.title, schemaToType(field)].join(":")
                ).join(",")
              }}}`
              : `[${
                entry.fields.map((field: any) => schemaToType(field)).join(",")
              }]}`
          }`
      ).join(" | ");
    }
    case "list": {
      if (schema.items instanceof Array) {
        return `[${
          schema.items.map((item: any) => schemaToType(item)).join(",")
        }]`;
      } else {
        return `${schemaToType(schema.items)}[]`;
      }
    }
    case "map": {
      return `Map<${schemaToType(schema.keys)}, ${
        schemaToType(schema.values)
      }>`;
    }
    case undefined: {
      return "Data";
    }
  }
  throw new Error("Could not type cast data.");
}

function isBoolean(shape: any): boolean {
  return shape.anyOf && shape.anyOf[0]?.title === "False" &&
    shape.anyOf[1]?.title === "True";
}

function isVoid(shape: any): boolean {
  return shape.index === 0 && shape.fields.length === 0;
}

function isNullable(shape: any): boolean {
  return shape.anyOf && shape.anyOf[0]?.title === "Some" &&
    shape.anyOf[1]?.title === "None";
}

function snakeToCamel(s: string): string {
  return s.toLowerCase().replace(/([-_][a-z])/g, (group) =>
    group
      .toUpperCase()
      .replace("-", "")
      .replace("_", ""));
}
