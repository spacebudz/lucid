import { PlutusData, Datum, Redeemer } from '..';
export declare class Construct {
    index: number;
    args: PlutusData[];
    constructor(index: number, args: PlutusData[]);
}
export declare class Data {
    static to(data: PlutusData): Datum | Redeemer;
    static from(data: Datum | Redeemer): PlutusData;
    static empty(): Datum | Redeemer;
}
