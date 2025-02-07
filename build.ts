import { copySync, moveSync } from "https://deno.land/std@0.224.0/fs/mod.ts";
import denoJson from "./deno.json" with { type: "json" };
import * as dnt from "jsr:@deno/dnt";

await dnt.emptyDir("./dist");

try {
  moveSync("./lib/core/core.backup.ts", "./lib/core/core.ts", {
    overwrite: true,
  });
} catch (_) {}
copySync("./lib/core/core.ts", "./lib/core/core.backup.ts");
const coreFile = await Deno.readTextFile("./lib/core/core.ts");
Deno.writeTextFileSync(
  "./lib/core/core.ts",
  coreFile.replace(
    `import * as Core from "../../rs_lib/pkg/lucid_core.js";`,
    "const Core = {};",
  ).replace(
    `import * as MessageSigningInstance from "../../rs_lib/message_signing/pkg/message_signing.js";`,
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
        "./dist/esm/lib/core/core.js",
      );
      Deno.writeTextFileSync(
        "./dist/esm/lib/core/core.js",
        coreFileEsm
          .replace(
            "const Core = {};",
            `import * as Core from "./libs/lucid_core/pkg/lucid_core.js";`,
          )
          .replace(
            "const MessageSigningInstance = {};",
            `import * as MessageSigningInstance from "./libs/message_signing/pkg/message_signing.js";`,
          ),
      );

      const coreTypesFileEsm = await Deno.readTextFile(
        "./dist/esm/lib/core/core.d.ts",
      );
      Deno.writeTextFileSync(
        "./dist/esm/lib/core/core.d.ts",
        coreTypesFileEsm
          .replaceAll(
            "../../rs_lib/pkg/lucid_core",
            "./libs/lucid_core/pkg/lucid_core",
          )
          .replace(
            "../../rs_lib/message_signing/pkg/message_signing",
            "./libs/message_signing/pkg/message_signing",
          ),
      );

      copySync(
        "./rs_lib/message_signing/pkg/",
        "./dist/esm/lib/core/libs/message_signing/pkg",
      );
      copySync(
        "./rs_lib/pkg/",
        "./dist/esm/lib/core/libs/lucid_core/pkg",
      );
    },
  });
} catch (e) {
  console.log(e);
}
moveSync("./lib/core/core.backup.ts", "./lib/core/core.ts", {
  overwrite: true,
});
