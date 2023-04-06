---
title: Import
description: How to import Lucid.
order: 1
---

Lucid is written in [Deno](https://deno.land/), but can be used in many other
JavaScript runtimes too.

## Using Deno

Make sure you have
[Deno](https://deno.land/manual@v1.29.1/getting_started/installation) installed.

```js
import { Lucid } from "https://deno.land/x/lucid/mod.ts";

const lucid = await Lucid.new();
```

**Hint:** We always recommend importing libraries including the version tag:\
e.g. `https://deno.land/x/lucid@0.8.3/mod.ts`\
For simplicity we leave it out in the documentation.

### Visual Studio Code configuration

If you use Visual Studio Code, it's highly recommended to install the
[Deno extension](https://marketplace.visualstudio.com/items?itemName=denoland.vscode-deno).

## Using NPM/Node.js

Make sure you have [Node.js](https://nodejs.org/en/) installed.

Add Lucid to your project:

```sh
npm install lucid-cardano
```

then import it:

```js
import { Lucid } from "lucid-cardano";

const lucid = await Lucid.new();
```

**Note:** Lucid is an ES Module. You need to set `{"type" : "module"}` in your
`package.json`.\
To bundle your NPM project you may need to adjust your bundler. For instance
when using [Webpack](https://webpack.js.org/) you need to enbale in the
`webpack.config.json`:

```json
experiments: {
    "asyncWebAssembly": true,
    "topLevelAwait": true,
    "layers": true // optional, with some bundlers/frameworks it doesn't work without
  }
```

## Using Web

Create a html file and import Lucid in a `<script>` tag:

```html
<script type="module">
import { Lucid } from "https://unpkg.com/lucid-cardano/web/mod.js"
const lucid = await Lucid.new();
</script>
```

## Build locally

Clone the repository:

```sh
git clone https://github.com/spacebudz/lucid.git
```

With Deno you can simply import Lucid like this (at the root of the Lucid
folder):

```js
import { Lucid } from "./mod.ts";
```

For NPM and web bundle you need to build Lucid first. Go to the root of the
Lucid folder and execute:

```sh
deno task build
```

This will output a `dist` folder at the root containing the NPM and web bundle.
