/** @internal */
export const C =
  typeof window !== 'undefined'
    ? await import('../../custom_modules/cardano-multiplatform-lib-browser')
    : await import(
        /* webpackIgnore: true */ '../../custom_modules/cardano-multiplatform-lib-nodejs'
      );
