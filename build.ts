import { build, emptyDir } from "https://deno.land/x/dnt@0.30.0/mod.ts";
import * as esbuild from "https://deno.land/x/esbuild@v0.14.45/mod.js";
import packageInfo from "./package.json" assert { type: "json" };

await emptyDir("./dist");

//** NPM ES Module for Node.js and Browser */

await build({
  entryPoints: ["./mod.ts"],
  outDir: "./dist",
  test: false,
  scriptModule: false,
  typeCheck: false,
  shims: {},
  package: {
    ...packageInfo,
    engines: {
      node: ">=14",
    },
    dependencies: {
      "node-fetch": "^3.2.3",
      "@peculiar/webcrypto": "^1.4.0",
      "ws": "^8.10.0",
    },
    main: "./esm/mod.js",
    type: "module",
  },
});

Deno.copyFileSync("LICENSE", "dist/LICENSE");
Deno.copyFileSync("README.md", "dist/README.md");

// copy wasm files
// Core
Deno.copyFileSync(
  "src/core/wasm_modules/cardano_multiplatform_lib_web/cardano_multiplatform_lib_bg.wasm",
  "dist/esm/src/core/wasm_modules/cardano_multiplatform_lib_web/cardano_multiplatform_lib_bg.wasm",
);
// Deno.writeTextFileSync(
//   "dist/esm/src/core/wasm_modules/cardano_multiplatform_lib_web/package.json",
//   JSON.stringify({ type: "commonjs" }),
// );
// Message
Deno.copyFileSync(
  "src/core/wasm_modules/cardano_message_signing_web/cardano_message_signing_bg.wasm",
  "dist/esm/src/core/wasm_modules/cardano_message_signing_web/cardano_message_signing_bg.wasm",
);
// Deno.writeTextFileSync(
//   "dist/esm/src/core/wasm_modules/cardano_message_signing_web/package.json",
//   JSON.stringify({ type: "commonjs" }),
// );

//** Web ES Module */

await esbuild.build({
  bundle: true,
  format: "esm",
  entryPoints: ["./dist/esm/mod.js"],
  outfile: "./dist/web/mod.js",
  minify: true,
  external: [
    "node-fetch",
    "@peculiar/webcrypto",
    "ws",
    "module",
  ],
});
esbuild.stop();
