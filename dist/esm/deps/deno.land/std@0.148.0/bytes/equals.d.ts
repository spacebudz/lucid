/** Check whether binary arrays are equal to each other using 8-bit comparisons.
 * @private
 * @param a first array to check equality
 * @param b second array to check equality
 */
export declare function equalsNaive(a: Uint8Array, b: Uint8Array): boolean;
/** Check whether binary arrays are equal to each other using 32-bit comparisons.
 * @private
 * @param a first array to check equality
 * @param b second array to check equality
 */
export declare function equals32Bit(a: Uint8Array, b: Uint8Array): boolean;
/** Check whether binary arrays are equal to each other.
 * @param a first array to check equality
 * @param b second array to check equality
 */
export declare function equals(a: Uint8Array, b: Uint8Array): boolean;
