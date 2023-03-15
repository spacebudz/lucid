import * as dnt from "https://deno.land/x/dnt@0.30.0/mod.ts";
import * as esbuild from "https://deno.land/x/esbuild@v0.17.11/mod.js";
import packageInfo from "./package.json" assert { type: "json" };

await dnt.emptyDir("./dist");

//** NPM ES Module for Node.js and Browser */

await dnt.build({
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

await esbuild.build({
  bundle: true,
  format: "esm",
  entryPoints: ["./dist/esm/mod.js"],
  outfile: "./dist/web/mod.js",
  minify: true,
  plugins: [
    importPathPlugin,
  ],
});
esbuild.stop();

/** Add necessary global import statements to NPM ES Module. */
const nodeImports = `const isNode = globalThis?.process?.versions?.node;
if (isNode) {
  const fetch = /* #__PURE__ */ await import(/* webpackIgnore: true */ "node-fetch");
  const { Crypto } = /* #__PURE__ */ await import(/* webpackIgnore: true */ "@peculiar/webcrypto");
  const { WebSocket } = /* #__PURE__ */ await import(/* webpackIgnore: true */ "ws");
  if (!globalThis.WebSocket) globalThis.WebSocket = WebSocket;
  if (!globalThis.crypto) globalThis.crypto = new Crypto();
  if (!globalThis.fetch) globalThis.fetch = fetch.default;
  if (!globalThis.Headers) globalThis.Headers = fetch.Headers;
  if (!globalThis.Request) globalThis.Request = fetch.Request;
  if (!globalThis.Response) globalThis.Response = fetch.Response;
}
`;
Deno.writeTextFileSync("./dist/esm/mod.js", "import './node_imports.js'", {
  append: true,
});
Deno.writeTextFileSync("./dist/esm/node_imports.js", nodeImports);
