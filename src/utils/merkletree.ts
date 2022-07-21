// Haskell implementation: https://github.com/input-output-hk/hydra-poc/blob/master/plutus-merkle-tree/src/Plutus/MerkleTree.hs
import { concat, equals } from "https://deno.land/std@0.148.0/bytes/mod.ts";
import { toHex } from "./utils.ts";

type MerkleNode = {
  node: Hash;
  left: MerkleNode | null;
  right: MerkleNode | null;
};

type Hash = Uint8Array;

type MerkleTreeProof = Array<{
  left?: Hash;
  right?: Hash;
}>;

export class MerkleTree {
  root!: MerkleNode | null;

  /** Construct Merkle tree from data, which get hashed with sha256 */
  static async new(data: Array<Uint8Array>) {
    const mt = new this();
    mt.root = await this.buildRecursively(data);
    return mt;
  }

  /** Construct Merkle tree from sha256 hashes */
  static async fromHashes(hashes: Array<Hash>) {
    const mt = new this();
    mt.root = await this.buildRecursively(hashes, true);
    return mt;
  }

  private static async buildRecursively(
    data: Array<Uint8Array>,
    isHash?: boolean,
  ): Promise<MerkleNode | null> {
    if (data.length <= 0) return null;
    if (data.length === 1) {
      return {
        node: isHash ? data[0] : await sha256(data[0]),
        left: null,
        right: null,
      };
    }

    const cutoff = Math.floor(data.length / 2);
    const [left, right] = [data.slice(0, cutoff), data.slice(cutoff)];
    const lnode = await this.buildRecursively(left);
    const rnode = await this.buildRecursively(right);
    if (lnode === null || rnode === null) return null;
    return {
      node: await combineHash(lnode.node, rnode.node),
      left: lnode,
      right: rnode,
    };
  }

  rootHash(): Hash {
    if (this.root === null) throw new Error("Merkle tree root hash not found.");
    return this.root.node;
  }

  async getProof(data: Uint8Array): Promise<MerkleTreeProof> {
    const hash = await sha256(data);
    const proof: MerkleTreeProof = [];
    const searchRecursively = (tree: MerkleNode | null) => {
      if (tree && equals(tree.node, hash)) return true;

      if (tree?.right) {
        if (searchRecursively(tree.left)) {
          proof.push({ right: tree.right.node });
          return true;
        }
      }

      if (tree?.left) {
        if (searchRecursively(tree.right)) {
          proof.push({ left: tree.left.node });
          return true;
        }
      }
    };
    searchRecursively(this.root);
    return proof;
  }

  size(): number {
    const searchRecursively = (tree: MerkleNode | null): number => {
      if (tree === null) return 0;
      return 1 + searchRecursively(tree.left) + searchRecursively(tree.right);
    };
    return searchRecursively(this.root);
  }

  static async verify(
    data: Uint8Array,
    rootHash: Hash,
    proof: MerkleTreeProof,
  ): Promise<boolean> {
    const hash = await sha256(data);
    const searchRecursively = async (
      rootHash2: Hash,
      proof: MerkleTreeProof,
    ): Promise<boolean> => {
      if (proof.length <= 0) return equals(rootHash, rootHash2);
      const [h, t] = [proof[0], proof.slice(1)];
      if (h.left) {
        return searchRecursively(await combineHash(h.left, rootHash2), t);
      }
      if (h.right) {
        return searchRecursively(await combineHash(rootHash2, h.right), t);
      }
      return false;
    };
    return searchRecursively(hash, proof);
  }

  toString(): string {
    // deno-lint-ignore no-explicit-any
    const searchRecursively = (tree: MerkleNode | null): any => {
      if (tree === null) return null;
      return {
        node: toHex(tree.node),
        left: searchRecursively(tree.left),
        right: searchRecursively(tree.right),
      };
    };
    return JSON.stringify(searchRecursively(this.root), null, 2);
  }
}

export { concat, equals };

export const sha256 = async (data: Uint8Array): Promise<Hash> =>
  new Uint8Array(await crypto.subtle.digest("SHA-256", data));

export const combineHash = (hash1: Hash, hash2: Hash): Promise<Hash> =>
  sha256(concat(hash1, hash2));
