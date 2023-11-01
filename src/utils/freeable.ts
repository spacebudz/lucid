export interface Freeable {
  free(): void;
}

export type FreeableBucket = Array<Freeable | undefined>;

export abstract class Freeables {
  static free(...bucket: (Freeable | undefined)[]) {
    bucket.forEach((freeable) => {
      freeable?.free();
    });
  }
}
