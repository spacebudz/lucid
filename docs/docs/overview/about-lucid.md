---
title: About Lucid
description: Welcome to Lucid, an off-chain framework for Cardano.
order: 0
---

**Lucid** (pronounced `/ˈluː.sɪd/`) means _expressed clearly_ or _easy to
understand_.

Lucid is a platform designed to simplify the development process on Cardano by
eliminating the challenges associated with building transactions and interacting
with smart contracts. It achieves this by providing an abstraction layer that
hides the complexities of Cardano's infrastructure.\
One of the benefits of using Lucid is that it is built using
JavaScript/TypeScript, which makes it easier for developers to access a broad
range of libraries from the JavaScript community. This lowers the barrier to
entry for developers looking to build on Cardano and reduces the amount of time
and effort required to get started.

## What Lucid is

To comprehend Lucid, it's essential to understand how Cardano operates. On
Cardano, the majority of computation occurs off-chain. Smart contracts, also
known as validators, are simply scripts that lock UTxOs. To spend these UTxOs,
the validator must be executed, resulting in either true or false. However, a
validator alone cannot modify the state of the chain; only the transaction can
do so. The validator ensures that the transaction is created with certain
necessary constraints. Lucid is a framework that simplifies the process of
building these transactions to meet the validator's constraints. In summary,
Lucid makes it easier to construct transactions that satisfy the requirements of
the validator.

## What Lucid is not

It's important to note that Lucid isn't a smart contract language or a tool to
create validators. Rather, it is an off-chain component that streamlines the
process of managing everything from building transactions to creating wallets.
With Lucid, developers can focus on building the logic for their dApps while the
platform handles the complexities of transaction and wallet management. This
simplifies the development process and allows developers to build Cardano-based
applications with greater ease and efficiency.
