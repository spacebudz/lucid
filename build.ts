import * as dnt from "https://deno.land/x/dnt@0.39.0/mod.ts";
import * as esbuild from "https://deno.land/x/esbuild@v0.19.7/mod.js";
import packageInfo from "./package.json" assert { type: "json" };

await dnt.emptyDir("./dist");

await dnt.build({
  entryPoints: ["./mod.ts"],
  outDir: "./dist",
  test: false,
  esModule: true,
  declaration: "inline",
  skipNpmInstall: true,
  compilerOptions: {
    target: "Latest",
    skipLibCheck: true,
  },
  scriptModule: false,
  typeCheck: false,
  skipSourceOutput: true,
  shims: {},
  package: {
    ...packageInfo
  },
});

Deno.copyFileSync("LICENSE", "dist/LICENSE");
Deno.copyFileSync("README.md", "dist/README.md");

// Copy WebAssembly

Deno.copyFileSync(
  "src/core/libs/cardano_multiplatform_lib/cardano_multiplatform_lib_bg.wasm",
  "dist/esm/src/core/libs/cardano_multiplatform_lib/cardano_multiplatform_lib_bg.wasm",
);
Deno.copyFileSync(
  "src/core/libs/cardano_message_signing/cardano_message_signing_bg.wasm",
  "dist/esm/src/core/libs/cardano_message_signing/cardano_message_signing_bg.wasm",
);

//** Web ES Module */

const importPathPlugin = {
  name: "core-import-path",
  setup(build: any) {
    build.onResolve({
      filter:
        /^\.\/libs\/cardano_multiplatform_lib\/cardano_multiplatform_lib.generated.js$/,
    }, (args: any) => {
      return {
        path:
          "../esm/src/core/libs/cardano_multiplatform_lib/cardano_multiplatform_lib.generated.js",
        external: true,
      };
    });
    build.onResolve({
      filter:
        /^\.\/libs\/cardano_message_signing\/cardano_message_signing.generated.js$/,
    }, (args: any) => {
      return {
        path:
          "../esm/src/core/libs/cardano_message_signing/cardano_message_signing.generated.js",
        external: true,
      };
    });
  },
};

// await esbuild.build({
//   bundle: true,
//   format: "esm",
//   entryPoints: ["./dist/esm/mod.js"],
//   outfile: "./dist/web/mod.js",
//   minify: true,
//   plugins: [
//     importPathPlugin,
//   ],
// });
// esbuild.stop();

// /** Add necessary global import statements to NPM ES Module. */
const coreFile = `
const C = await (async () => {
  try {
    return await import(
      /* webpackIgnore: true */ "./libs/cardano_multiplatform_lib/nodejs/cardano_multiplatform_lib.generated.js"
    );
  } catch (_e) {
    // This only ever happens during SSR rendering
    return null;
  }
})();
const M = await (async () => {
  try {
    return await import(
      /* webpackIgnore: true */ "./libs/cardano_message_signing/nodejs/cardano_message_signing.generated.js"
    );
  } catch (_e) {
    // This only ever happens during SSR rendering
    return null;
  }
})();
export { C, M };
`;
Deno.writeTextFileSync("dist/esm/src/core/core.js", coreFile);

Deno.mkdirSync("dist/esm/src/core/libs/cardano_message_signing/nodejs");
Deno.mkdirSync("dist/esm/src/core/libs/cardano_multiplatform_lib/nodejs");

Deno.copyFileSync(
  "src/core/libs/cardano_message_signing/nodejs/cardano_message_signing.generated.js",
  "dist/esm/src/core/libs/cardano_message_signing/nodejs/cardano_message_signing.generated.js",
);

Deno.copyFileSync(
  "src/core/libs/cardano_multiplatform_lib/nodejs/cardano_multiplatform_lib.generated.js",
  "dist/esm/src/core/libs/cardano_multiplatform_lib/nodejs/cardano_multiplatform_lib.generated.js",
);

Deno.writeTextFile(
  "dist/esm/src/core/libs/cardano_message_signing/nodejs/package.json",
  JSON.stringify({ type: "commonjs" }),
);
Deno.writeTextFile(
  "dist/esm/src/core/libs/cardano_multiplatform_lib/nodejs/package.json",
  JSON.stringify({ type: "commonjs" }),
);
