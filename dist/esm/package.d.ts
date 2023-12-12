declare namespace _default {
    let name: string;
    let version: string;
    let license: string;
    let description: string;
    let repository: string;
    let module: string;
    let main: string;
    let types: string;
    namespace scripts {
        let pack: string;
    }
    namespace engines {
        let node: string;
    }
    let dependencies: {
        "@deno/shim-crypto": string;
        ws: string;
    };
    let devDependencies: {
        "wasm-pack": string;
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
