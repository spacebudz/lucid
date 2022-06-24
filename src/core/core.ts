/* eslint-disable */
import type Core from "../../custom_modules/cardano-multiplatform-lib-browser/cardano_multiplatform_lib.js";
export { Core };
/* eslint-enable */

/** Check if environment is Node.js or SSR like Gatsby or Next.js */
const importNodeOrSSR = async () => {
  try {
    return await import(
      /* webpackIgnore: true */
      '../../custom_modules/cardano-multiplatform-lib-nodejs/cardano_multiplatform_lib.js'
    );
  } catch (e) {
    return null;
  }
};

const importDeno = async () => {
  const pkg = await import(
    /* webpackIgnore: true */
    '../../custom_modules/cardano-multiplatform-lib-web/cardano_multiplatform_lib.js'
  );
  await pkg.default(
    (window as any).Deno.readFile(
      // Deno.readFile reads from the root file
      './custom_modules/cardano-multiplatform-lib-web/cardano_multiplatform_lib_bg.wasm'
    )
  );
  return pkg;
};

export const C = (typeof window !== 'undefined'
  ? typeof (window as any).Deno !== 'undefined'
    ? await importDeno()
    : await import(
        '../../custom_modules/cardano-multiplatform-lib-browser/cardano_multiplatform_lib.js'
      )
  : await importNodeOrSSR())!;
