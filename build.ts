import { copySync } from "https://deno.land/std@0.224.0/fs/mod.ts";
import * as esbuild from "https://deno.land/x/esbuild@v0.24.2/mod.js";
import { denoPlugins } from "jsr:@luca/esbuild-deno-loader@^0.11.1";
import partialPackage from "./package.json" with { type: "json" };

const importWasm: esbuild.Plugin = {
  name: "import-wasm",
  setup(build: any) {
    build.onResolve({
      filter: /^\.\/libs\/lucid_core\/pkg\/lucid_core.js$/,
    }, () => {
      return {
        path: "./pkg/lucid_core.js",
        external: true,
      };
    });
    build.onResolve({
      filter: /^\.\/libs\/message_signing\/pkg\/message_signing.js$/,
    }, () => {
      return {
        path: "./pkg/message_signing.js",
        external: true,
      };
    });
  },
};

await esbuild.build({
  bundle: true,
  format: "esm",
  entryPoints: ["./mod.ts"],
  outfile: "./dist/mod.js",
  plugins: [
    importWasm,
    ...denoPlugins(),
  ],
});
esbuild.stop();

Deno.writeTextFileSync(
  "./dist/package.json",
  JSON.stringify(
    {
      ...partialPackage,
      dependencies: {
        "node-fetch": "^3.2.3",
        "@peculiar/webcrypto": "^1.4.0",
        "ws": "^8.10.0",
      },
      types: "./mod.d.ts",
      main: "./mod.js",
      module: "./mod.js",
      exports: {
        ".": {
          import: "./mod.js",
        },
      },
      type: "module",
      engines: {
        node: ">=22",
      },
    },
    null,
    2,
  ),
);

copySync("./src/core/libs/message_signing/pkg/", "./dist/pkg", {
  overwrite: true,
});
copySync("./src/core/libs/lucid_core/pkg/", "./dist/pkg", {
  overwrite: true,
});
