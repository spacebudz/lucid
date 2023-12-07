export declare function mnemonicToEntropy(mnemonic: string, wordlist?: Array<string>): string;
export declare function generateMnemonic(strength: number, rng?: (size: number) => Uint8Array, wordlist?: Array<string>): string;
