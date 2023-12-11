import * as dnt from "https://deno.land/x/dnt@0.39.0/mod.ts";
import { copySync } from "https://deno.land/std@0.208.0/fs/copy.ts";
import { ensureDirSync } from "https://deno.land/std@0.208.0/fs/ensure_dir.ts";
// import { build } from "https://deno.land/x/esbuild@v0.19.7/mod.js";
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
  shims: {
    webSocket: true,
    undici: false,
    crypto: true,
  },
  package: {
    ...packageInfo
  },
});

Deno.copyFileSync("LICENSE", "dist/LICENSE");
Deno.copyFileSync("README.md", "dist/README.md");

// Copy WASM Pack files

for (const pkg of ["cardano_message_signing", "cardano_multiplatform_lib"]) {
  const files = Deno.readDirSync(`src/core/libs/${pkg}/pkg`);

  for (const file of files) {
    if (file.isFile && !file.name.endsWith(".js")) {
      copySync(
      `src/core/libs/${pkg}/pkg/${file.name}`,
      `dist/esm/src/core/libs/${pkg}/pkg/${file.name}`,
        { overwrite: true }
      );
    }
  }

  const packageJson = JSON.parse(Deno.readTextFileSync(`src/core/libs/${pkg}/pkg/package.json`));
  packageJson.type = "commonjs";

  Deno.writeTextFile(
    `dist/esm/src/core/libs/${pkg}/pkg/package.json`,
    JSON.stringify(packageJson, null, 2),
  );
}


