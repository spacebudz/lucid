/** @internal */
export const C =
  typeof window !== 'undefined'
    ? await import(
        '../../custom_modules/cardano-multiplatform-lib-browser/cardano_multiplatform_lib'
      )
    : await import(
        '../../custom_modules/cardano-multiplatform-lib-nodejs/cardano_multiplatform_lib'
      );
