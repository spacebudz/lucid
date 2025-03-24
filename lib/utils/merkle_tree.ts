// Haskell implementation: https://github.com/input-output-hk/hydra-poc/blob/master/plutus-merkle-tree/src/Plutus/MerkleTree.hs
import { concat, equals } from "jsr:@std/bytes@1.0.4";
import { crypto } from "jsr:@std/crypto@1.0.4";
import { toHex } from "./utils.ts";

type MerkleNode = {
  node: Hash;
  left: MerkleNode | null;
  right: MerkleNode | null;
};

type Hash = Uint8Array;

type MerkleProof = Array<{
  left?: Hash;
  right?: Hash;
}>;

export class MerkleTree {
  private root: MerkleNode | null;

  /** Construct Merkle tree from data, which get hashed with sha256 */
  constructor(data: Uint8Array[]) {
    this.root = this.build(data.map(sha256));
  }

  /** Construct Merkle tree from sha256 hashes */
  static fromHashes(hashes: Hash[]): MerkleTree {
    return new this(hashes);
  }

  private build(
    hashes: Hash[],
  ): MerkleNode | null {
    if (hashes.length <= 0) return null;
    if (hashes.length === 1) {
      return {
        node: hashes[0],
        left: null,
        right: null,
      };
    }

    const cutoff = Math.floor(hashes.length / 2);
    const [left, right] = [hashes.slice(0, cutoff), hashes.slice(cutoff)];
    const lnode = this.build(left);
    const rnode = this.build(right);

    if (lnode === null || rnode === null) return null;

    return {
      node: combineHash(lnode.node, rnode.node),
      left: lnode,
      right: rnode,
    };
  }

  rootHash(): Hash {
    if (this.root === null) throw new Error("Merkle tree root hash not found.");
    return this.root.node;
  }

  getProof(data: Uint8Array): MerkleProof {
    const proof: MerkleProof = [];

    function search(tree: MerkleNode | null): boolean {
      if (tree && equals(tree.node, hash)) return true;

      if (tree?.right) {
        if (search(tree.left)) {
          proof.push({ right: tree.right.node });
          return true;
        }
      }

      if (tree?.left) {
        if (search(tree.right)) {
          proof.push({ left: tree.left.node });
          return true;
        }
      }

      return false;
    }

    const hash = sha256(data);
    search(this.root);
    return proof;
  }

  size(): number {
    function search(tree: MerkleNode | null): number {
      if (tree === null) return 0;
      return 1 + search(tree.left) + search(tree.right);
    }
    return search(this.root);
  }

  static verify(
    data: Uint8Array,
    rootHash: Hash,
    proof: MerkleProof,
  ): boolean {
    function search(
      rootHash2: Hash,
      proof: MerkleProof,
    ): boolean {
      if (proof.length <= 0) return equals(rootHash, rootHash2);
      const [h, t] = [proof[0], proof.slice(1)];
      if (h.left) {
        return search(combineHash(h.left, rootHash2), t);
      }
      if (h.right) {
        return search(combineHash(rootHash2, h.right), t);
      }
      return false;
    }

    const hash = sha256(data);
    return search(hash, proof);
  }

  toString(): string {
    function search(tree: MerkleNode | null): unknown {
      if (tree === null) return null;
      return {
        node: toHex(tree.node),
        left: search(tree.left),
        right: search(tree.right),
      };
    }
    return JSON.stringify(search(this.root), null, 2);
  }
}

export { concat, equals };

export function sha256(data: Uint8Array): Hash {
  return new Uint8Array(crypto.subtle.digestSync("SHA-256", data));
}

export function combineHash(hash1: Hash, hash2: Hash): Hash {
  return sha256(concat([hash1, hash2]));
}
