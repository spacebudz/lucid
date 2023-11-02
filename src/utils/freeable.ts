export interface Freeable {
  free(): void;
}

export type FreeableBucket = Array<Freeable | undefined | null>;

export abstract class Freeables {
  static free(...bucket: (Freeable | undefined | null)[]) {
    bucket.forEach((freeable) => {
      freeable?.free();
    });
  }
}
