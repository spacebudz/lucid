# Contributing

## General

- #### Keep it simple.

- #### Avoid external dependencies. The less dependencies the easier Lucid is interoperable between Browser, Node.js and Deno. But Deno dependencies are fine most of the time because they are mostly written in a browser compatible way.

- #### Avoid exposing functions and arguments directly from the `cardano-multiplatform-lib` or `message-signing` library. Rather wrap them and expose types from JavaScript like `string`, `number` or `object`.

- #### Add tests to the `tests` folder.

- #### Always run and test the code with the latest Deno version. Simply do `deno upgrade` to get the latest version.

## Feedback

- #### Contributions in the form of feedback and issue is very much welcome. Might it be a suggestion, a bug report or maybe some questions that you have. It helps improving Lucid in the long run and these are probably the best kind of contributions to start with. Do not hesitate to add thumbs up 👍 on open issues you support to show your interest.

## Style Guide

### TypeScript

- #### Use TypeScript instead of JavaScript.

- #### Follow the default TypeScript styling conventions unless mentioned differently here.

- #### Try to avoid `any` wherever you can.

- #### Do not use the filename index.ts/index.js.

Deno does not treat "index.js" or "index.ts" in a special way. By using these
filenames, it suggests that they can be left out of the module specifier when
they cannot. This is confusing.

If a directory of code needs a default entry point, use the filename `mod.ts`.
The filename `mod.ts` follows Rust’s convention, is shorter than `index.ts`, and
doesn’t come with any preconceived notions about how it might work.

- #### Use underscores in folders and filenames.

Example: Use `merkle_tree.ts` instead of `merkle-tree.ts` or `merkleTree.ts`.

- #### Exported functions: max 2-3 args, put the rest into an options object.

```ts
// BAD. If the second argument was not optional, it would be OKAY to do it like this.
export function fromSeed(
  address: string,
  addressType?: "Base" | "Enterprise",
  accountIndex?: number,
): string {}
```

```ts
// GOOD.
export interface SeedOptions {
  addressType?: "Base" | "Enterprise";
  accountIndex?: number;
}
export function fromSeed(
  address: string,
  options: SeedOptions = {},
): string {}
```

- #### Top-level functions should not use arrow syntax.

```ts
// BAD.
export const add = (a: number, b: number): number => a + b;
```

```ts
// GOOD.
export function add(a: number, b: number): number {
  return a + b;
}
```

- #### Be explicit about types used in functions.

```ts
// BAD. Return type is only implicitly determined.
export function add(a: number, b: number) {
  return a + b;
}
```

```ts
// GOOD.
export function add(a: number, b: number): number {
  return a + b;
}
```

But if the return type is `void` then don't explicitly mention it:

```ts
// GOOD.
export function log(str: string) {
  console.log(str);
}
```

- #### Structure comments properly

Single line comment:

```ts
// This is a comment.
```

Multiline comment:

```ts
/*
  This is a comment on line 1.
  This is a comment on line 2.
 */
```

Single line comment to describe a function/variable:

```ts
/** This functions adds two numbers together. */
export function add(a: number, b: number): number {
  return a + b;
}
```

Multiline comment to describe a function/variable:

```ts
/**
 * This functions adds two numbers together.
 * This is another random comment.
 */
export function add(a: number, b: number): number {
  return a + b;
}
```

Always end comments with a dot `.` (If they are just a few words then it's not
necessary).

Avoid JSDoc `@param`. If `@param` is used, it should not include the `type` as
TypeScript is already strongly-typed:

```ts
/**
 * Function with non-obvious param.
 * @param foo Description of non-obvious parameter.
 */
```

- #### Run `deno fmt` before you commit and push any code changes.

### Rust

- #### Follow Rust conventions and be consistent with existing code.
