/** Check if environment is Node.js or SSR like Gatsby or Next.js */
const importNodeOrSSR = async () => {
  try {
    return await import(
      /* webpackIgnore: true */ '../../custom_modules/cardano-multiplatform-lib-nodejs'
    );
  } catch (e) {
    return null;
  }
};

/** @internal */
export const C =
  typeof window !== 'undefined'
    ? await import('../../custom_modules/cardano-multiplatform-lib-browser')
    : await importNodeOrSSR();
