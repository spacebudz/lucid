const isNode = globalThis?.process?.versions?.node;

const C = await (async () => {
  try {
    if (isNode) {
      return await import(
        /* webpackIgnore: true */ "./libs/cardano_multiplatform_lib/nodejs/cardano_multiplatform_lib.generated.js"
      );
    }
    return await import(
      "./libs/cardano_multiplatform_lib/cardano_multiplatform_lib.generated.js"
    );
  } catch (_e) {
    // This only ever happens during SSR rendering
    return null;
  }
})();
const M = await (async () => {
  try {
    if (isNode) {
      return await import(
        /* webpackIgnore: true */ "./libs/cardano_message_signing/nodejs/cardano_message_signing.generated.js"
      );
    }
    return await import(
      "./libs/cardano_message_signing/cardano_message_signing.generated.js"
    );
  } catch (_e) {
    // This only ever happens during SSR rendering
    return null;
  }
})();
if (!isNode) {
  async function unsafeInstantiate(module) {
    try {
      await module.instantiate();
    } catch (_e) {
      // This only ever happens during SSR rendering
    }
  }
  await Promise.all([
    unsafeInstantiate(C),
    unsafeInstantiate(M),
  ]);
}
export { C, M };
