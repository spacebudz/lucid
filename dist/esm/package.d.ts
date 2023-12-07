declare namespace _default {
    let name: string;
    let version: string;
    let license: string;
    let description: string;
    let repository: string;
    let module: string;
    let main: string;
    let types: string;
    namespace engines {
        let node: string;
    }
    let dependencies: {
        "node-fetch": string;
        "@peculiar/webcrypto": string;
        ws: string;
    };
    let type: string;
    let exports: {
        ".": {
            import: string;
            types: string;
        };
    };
    let packageManager: string;
}
export default _default;
