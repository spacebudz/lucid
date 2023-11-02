export interface Freeable {
  free(): void;
}

export type FreeableBucket = Array<Freeable | undefined | null>;

export abstract class Freeables {
  static free(...bucket: FreeableBucket) {
    bucket.forEach((freeable) => {
      freeable?.free();
    });
  }
}
