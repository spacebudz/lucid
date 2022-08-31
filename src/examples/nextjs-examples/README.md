This is a simple integration of two examples (alwaysSucceeds and matchingNumbers) into a running next.js app. Aim is to have full end-to-end templates.

-----------

## Getting Started

First, install, then run the development server:

```bash
npm install

npm run dev # use this
# or
yarn dev # warning: untested
```

Open [http://localhost:3000](http://localhost:3000) with your browser to see the result.


[!] You need to have Nami* installed and enabled, and ideally set to "testnet" unless you desire unpredicable mayhem. [!]

* IMPORTANT: you need to use the vasil-ready version, which you'll get from here: [https://github.com/Berry-Pool/nami-wallet/tree/vasil](https://github.com/Berry-Pool/nami-wallet/tree/vasil) (mind the branch!), following the instructions in the readme there.

------------

You can start editing the page by modifying `pages/index.tsx`. The page auto-updates as you edit the file.

[API routes](https://nextjs.org/docs/api-routes/introduction) can be accessed on [http://localhost:3000/api/hello](http://localhost:3000/api/hello). This endpoint can be edited in `pages/api/hello.ts`.

The `pages/api` directory is mapped to `/api/*`. Files in this directory are treated as [API routes](https://nextjs.org/docs/api-routes/introduction) instead of React pages.

## Learn More

To learn more about Next.js, take a look at the following resources:

- [Next.js Documentation](https://nextjs.org/docs) - learn about Next.js features and API.
- [Learn Next.js](https://nextjs.org/learn) - an interactive Next.js tutorial.

You can check out [the Next.js GitHub repository](https://github.com/vercel/next.js/) - your feedback and contributions are welcome!

## Deploy on Vercel

The easiest way to deploy your Next.js app is to use the [Vercel Platform](https://vercel.com/new?utm_medium=default-template&filter=next.js&utm_source=create-next-app&utm_campaign=create-next-app-readme) from the creators of Next.js.

Check out our [Next.js deployment documentation](https://nextjs.org/docs/deployment) for more details.
