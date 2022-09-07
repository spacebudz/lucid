# Contributing

## General

#### - Keep it simple

#### - Try to avoid external dependencies. The less dependencies the easier Lucid is interoperable between Browser, Node.js and Deno.

## Style Guide

### - TypeScript

#### - Use TypeScript instead of JavaScript.

#### - Follow the default TypeScript styling conventions unless mentioned differently here.

#### - Try to avoid `any` wherever you can.

#### - Do not use the filename index.ts/index.js.
Deno does not treat "index.js" or "index.ts" in a special way. By using these filenames, it suggests that they can be left out of the module specifier when they cannot. This is confusing.

If a directory of code needs a default entry point, use the filename `mod.ts`. The filename `mod.ts` follows Rust’s convention, is shorter than `index.ts`, and doesn’t come with any preconceived notions about how it might work.

#### - Use underscores in folders and filenames.
Example: Use `merkle_tree.ts` instead of `merkle-tree.ts` or `merkleTree.ts`.

#### - Exported functions: max 2-3 args, put the rest into an options object.

```ts
// BAD. If the second argument was not optional, it would be OKAY to do it like this.
export function fromSeed(
  address: string,
  addressType?: "Base" | "Enterprise",
  accountIndex?: number
): string {}
```

```ts
// GOOD.
export interface SeedOptions {
  addressType?: "Base" | "Enterprise",
  accountIndex?: number
}
export function resolve(
  hostname: string,
  options: SeedOptions = {},
): string {}
```


#### - Top-level functions should not use arrow syntax.

```ts
// BAD.
export const add = (a : number, b : number): number => a + b;
```

```ts
// GOOD.
export function add(a: number, b: number): number { return a + b; }
```

#### - Be explicit about types used in functions.

```ts
// BAD. Return type is only implicitly determined.
export function add(a: number, b: number) { return a + b; }
```

```ts
// GOOD.
export function add(a: number, b: number): number { return a + b; }
```

But if the return type is `void` then don't explicitly mention it:

```ts
// GOOD.
export function log(str: string) { console.log(str); }
```


#### - Structure comments properly


#### - Run `deno fmt` before you commit and push any code changes.



### Rust

#### - Follow Rust conventions and be consistent with existing code.
