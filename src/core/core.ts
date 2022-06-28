/* eslint-disable */
import type * as Core from "../../custom_modules/cardano-multiplatform-lib-web/cardano_multiplatform_lib.js";
export { Core };
/* eslint-enable */

if (typeof window === 'undefined') {
  const fetch = await import(/* webpackIgnore: true */ 'node-fetch');
  // @ts-ignore
  global.fetch = fetch.default;
  // @ts-ignore
  global.Headers = fetch.Headers;
  // @ts-ignore
  global.Request = fetch.Request;
  // @ts-ignore
  global.Response = fetch.Response;
}

const importForEnvironment = async (): Promise<typeof Core | null> => {
  try {
    if (typeof window === 'undefined') {
      return (await import(
        /* webpackIgnore: true */
        '../../custom_modules/cardano-multiplatform-lib-nodejs/cardano_multiplatform_lib.js'
      )) as any;
    }

    const pkg = await import(
      '../../custom_modules/cardano-multiplatform-lib-web/cardano_multiplatform_lib.js'
    );

    await pkg.default(
      await fetch(
        new URL(
          '../custom_modules/cardano-multiplatform-lib-web/cardano_multiplatform_lib_bg.wasm',
          import.meta.url
        ) as any
      )
    );
    return pkg;
  } catch (_e) {
    // This only ever happens during SSR rendering
    return null;
  }
};

export const C: typeof Core = (await importForEnvironment())!;
