// Haskell implementation: https://github.com/input-output-hk/hydra-poc/blob/master/plutus-merkle-tree/src/Plutus/MerkleTree.hs
import { concat, equals } from "../../deps/deno.land/std@0.148.0/bytes/mod.js";
import { Sha256 } from "../../deps/deno.land/std@0.153.0/hash/sha256.js";
import { toHex } from "./utils.js";
export class MerkleTree {
    /** Construct Merkle tree from data, which get hashed with sha256 */
    constructor(data) {
        Object.defineProperty(this, "root", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: void 0
        });
        this.root = MerkleTree.buildRecursively(data.map((d) => sha256(d)));
    }
    /** Construct Merkle tree from sha256 hashes */
    static fromHashes(hashes) {
        return new this(hashes);
    }
    static buildRecursively(hashes) {
        if (hashes.length <= 0)
            return null;
        if (hashes.length === 1) {
            return {
                node: hashes[0],
                left: null,
                right: null,
            };
        }
        const cutoff = Math.floor(hashes.length / 2);
        const [left, right] = [hashes.slice(0, cutoff), hashes.slice(cutoff)];
        const lnode = this.buildRecursively(left);
        const rnode = this.buildRecursively(right);
        if (lnode === null || rnode === null)
            return null;
        return {
            node: combineHash(lnode.node, rnode.node),
            left: lnode,
            right: rnode,
        };
    }
    rootHash() {
        if (this.root === null)
            throw new Error("Merkle tree root hash not found.");
        return this.root.node;
    }
    getProof(data) {
        const hash = sha256(data);
        const proof = [];
        const searchRecursively = (tree) => {
            if (tree && equals(tree.node, hash))
                return true;
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
    size() {
        const searchRecursively = (tree) => {
            if (tree === null)
                return 0;
            return 1 + searchRecursively(tree.left) + searchRecursively(tree.right);
        };
        return searchRecursively(this.root);
    }
    static verify(data, rootHash, proof) {
        const hash = sha256(data);
        const searchRecursively = (rootHash2, proof) => {
            if (proof.length <= 0)
                return equals(rootHash, rootHash2);
            const [h, t] = [proof[0], proof.slice(1)];
            if (h.left) {
                return searchRecursively(combineHash(h.left, rootHash2), t);
            }
            if (h.right) {
                return searchRecursively(combineHash(rootHash2, h.right), t);
            }
            return false;
        };
        return searchRecursively(hash, proof);
    }
    toString() {
        // deno-lint-ignore no-explicit-any
        const searchRecursively = (tree) => {
            if (tree === null)
                return null;
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
export function sha256(data) {
    return new Uint8Array(new Sha256().update(data).arrayBuffer());
}
export function combineHash(hash1, hash2) {
    return sha256(concat(hash1, hash2));
}
