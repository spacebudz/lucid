export type Message = string | number[] | ArrayBuffer;
export declare class Sha256 {
    #private;
    constructor(is224?: boolean, sharedMemory?: boolean);
    protected init(is224: boolean, sharedMemory: boolean): void;
    /** Update hash
     *
     * @param message The message you want to hash.
     */
    update(message: Message): this;
    protected finalize(): void;
    protected hash(): void;
    /** Return hash in hex string. */
    hex(): string;
    /** Return hash in hex string. */
    toString(): string;
    /** Return hash in integer array. */
    digest(): number[];
    /** Return hash in integer array. */
    array(): number[];
    /** Return hash in ArrayBuffer. */
    arrayBuffer(): ArrayBuffer;
}
export declare class HmacSha256 extends Sha256 {
    #private;
    constructor(secretKey: Message, is224?: boolean, sharedMemory?: boolean);
    protected finalize(): void;
}
