
const C = await (async () => {
  try {
    return await import(
      /* webpackIgnore: true */ "./libs/cardano_multiplatform_lib/nodejs/cardano_multiplatform_lib.generated.js"
    );
  } catch (_e) {
    // This only ever happens during SSR rendering
    return null;
  }
})();
const M = await (async () => {
  try {
    return await import(
      /* webpackIgnore: true */ "./libs/cardano_message_signing/nodejs/cardano_message_signing.generated.js"
    );
  } catch (_e) {
    // This only ever happens during SSR rendering
    return null;
  }
})();
export { C, M };
