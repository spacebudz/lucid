import { copySync, moveSync } from "https://deno.land/std@0.224.0/fs/mod.ts";
import denoJson from "./deno.json" with { type: "json" };
import * as dnt from "jsr:@deno/dnt";

await dnt.emptyDir("./dist");

try {
  moveSync("./src/core/core.backup.ts", "./src/core/core.ts", {
    overwrite: true,
  });
} catch (_) {}
copySync("./src/core/core.ts", "./src/core/core.backup.ts");
const coreFile = await Deno.readTextFile("./src/core/core.ts");
Deno.writeTextFileSync(
  "./src/core/core.ts",
  coreFile.replace(
    `import * as Core from "./libs/lucid_core/pkg/lucid_core.js";`,
    "const Core = {};",
  ).replace(
    `import * as MessageSigningInstance from "./libs/message_signing/pkg/message_signing.js";`,
    "const MessageSigningInstance = {};",
  ),
);
try {
  await dnt.build({
    entryPoints: ["./mod.ts"],
    skipNpmInstall: true,
    outDir: "./dist",
    test: false,
    scriptModule: false,
    esModule: true,
    typeCheck: false,
    skipSourceOutput: true,
    shims: {},
    package: {
      name: "lucid-cardano",
      version: denoJson.version,
      license: denoJson.license,
      author: denoJson.author,
      description: denoJson.description,
      repository: denoJson.repository,
      type: "module",
      main: "./esm/mod.js",
      engines: {
        node: ">=20",
      },
    },
    postBuild: async () => {
      const coreFileEsm = await Deno.readTextFile(
        "./dist/esm/src/core/core.js",
      );
      Deno.writeTextFileSync(
        "./dist/esm/src/core/core.js",
        coreFileEsm.replace(
          "const Core = {};",
          `import * as Core from "./libs/lucid_core/pkg/lucid_core.js";`,
        ).replace(
          "const MessageSigningInstance = {};",
          `import * as MessageSigningInstance from "./libs/message_signing/pkg/message_signing.js";`,
        ),
      );

      copySync(
        "./src/core/libs/message_signing/pkg/",
        "./dist/esm/src/core/libs/message_signing/pkg",
        {
          overwrite: true,
        },
      );
      copySync(
        "./src/core/libs/lucid_core/pkg/",
        "./dist/esm/src/core/libs/lucid_core/pkg",
        {
          overwrite: true,
        },
      );
    },
  });
} catch (e) {
  console.log(e);
}
moveSync("./src/core/core.backup.ts", "./src/core/core.ts", {
  overwrite: true,
});
