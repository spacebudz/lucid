import type * as Core from "./wasm_modules/cardano-multiplatform-lib-web/cardano_multiplatform_lib.js";
export { Core };

// dnt-shim-ignore
const isNode = typeof window === "undefined";

if (isNode) {
  const fetch = await import(/* webpackIgnore: true */ "node-fetch" as string);
  const { Crypto } = await import(
    /* webpackIgnore: true */ "@peculiar/webcrypto" as string
  );
  // @ts-ignore : global
  global.crypto = new Crypto();
  // @ts-ignore : global
  global.fetch = fetch.default;
  // @ts-ignore : global
  global.Headers = fetch.Headers;
  // @ts-ignore : global
  global.Request = fetch.Request;
  // @ts-ignore : global
  global.Response = fetch.Response;
}

const importForEnvironment = async (): Promise<typeof Core | null> => {
  try {
    if (isNode) {
      return (await import(
        /* webpackIgnore: true */
        "./wasm_modules/cardano-multiplatform-lib-nodejs/cardano_multiplatform_lib.js"
      )) as unknown as typeof Core;
    }

    const pkg = await import(
      "./wasm_modules/cardano-multiplatform-lib-web/cardano_multiplatform_lib.js"
    );

    await pkg.default(
      await fetch(
        new URL(
          "./wasm_modules/cardano-multiplatform-lib-web/cardano_multiplatform_lib_bg.wasm",
          import.meta.url,
        ),
      ),
    );
    return pkg as unknown as typeof Core;
  } catch (_e) {
    // This only ever happens during SSR rendering
    return null;
  }
};

export const C: typeof Core = (await importForEnvironment())!;
