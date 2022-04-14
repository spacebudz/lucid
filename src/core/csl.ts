export const S =
  typeof window !== 'undefined'
    ? await import('../../custom_modules/cardano-multiplatform-lib-browser')
    : await import('../../custom_modules/cardano-multiplatform-lib-nodejs');
