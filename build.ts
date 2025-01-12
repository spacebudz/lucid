import * as esbuild from "https://deno.land/x/esbuild@v0.24.2/mod.js";
import { wasmLoader } from "npm:esbuild-plugin-wasm";
import denoResolve from "https://deno.land/x/esbuild_plugin_deno_resolve@v0.0.8/index.ts";

await esbuild.build({
  bundle: true,
  format: "esm",
  entryPoints: ["./mod.ts"],
  outfile: "./dist/mod.js",
  minify: true,
  plugins: [
    wasmLoader({ mode: "deferred" }), // Node.js with embedded?
    denoResolve(),
  ],
});
esbuild.stop();
