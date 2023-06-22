declare namespace _default {
    const name: string;
    const version: string;
    const license: string;
    const description: string;
    const repository: string;
    const module: string;
    const main: string;
    const types: string;
    namespace engines {
        const node: string;
    }
    const dependencies: {
        "node-fetch": string;
        "@peculiar/webcrypto": string;
        ws: string;
    };
    const type: string;
    const exports: {
        ".": {
            import: string;
            types: string;
        };
    };
}
export default _default;
