/**
 * These types and classes are used to help with freeing memory.
 * Objects passed from the WASM to JS (Objects from Rust libraries, for example) are not freed automatically, or at least inconsistently.
 * This can lead to memory leaks.
 * In order to free these objects, we need to call the `free()` method on them. These types make it easier.
 */

/** This interface represents WASM objects that can and need to be freed. */
export interface Freeable {
  free(): void;
}

export type FreeableBucket = Array<Freeable | undefined | null>;

/** This class makes it easier to free large sets of memory. It can be used like this:
 * ```ts
 * const bucket: FreeableBucket = [];
 * try {
 *    const rustObject = C.some_rust_object();
 *    bucket.push(rustObject);
 *    ...
 * } finally {
 *    Freeables.free(...bucket);
 * }
 * ```
 */
export abstract class Freeables {
  static free(...bucket: FreeableBucket) {
    bucket.forEach((freeable) => {
      freeable?.free();
    });
  }
}
