import { concat, equals } from "../../deps/deno.land/std@0.148.0/bytes/mod.js";
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
export declare class MerkleTree {
    root: MerkleNode | null;
    /** Construct Merkle tree from data, which get hashed with sha256 */
    constructor(data: Array<Uint8Array>);
    /** Construct Merkle tree from sha256 hashes */
    static fromHashes(hashes: Array<Hash>): MerkleTree;
    private static buildRecursively;
    rootHash(): Hash;
    getProof(data: Uint8Array): MerkleTreeProof;
    size(): number;
    static verify(data: Uint8Array, rootHash: Hash, proof: MerkleTreeProof): boolean;
    toString(): string;
}
export { concat, equals };
export declare function sha256(data: Uint8Array): Hash;
export declare function combineHash(hash1: Hash, hash2: Hash): Hash;
