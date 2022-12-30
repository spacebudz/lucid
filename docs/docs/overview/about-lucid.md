---
title: About Lucid
description: Welcome to Lucid, an off-chain framework for Cardano.
order: 0
---

**Lucid** (pronounced `/ˈluː.sɪd/`) means _expressed clearly_ or _easy to
understand_.

Developing on Cardano has always been very hard because of missing tooling and
infrastructure. The goal of Lucid is to abstract away all the complexity that
comes with building transactions and interacting with smart contracts.\
Addionally Lucid is written in JavaScript/TypeScript which lowers the barrier to
entry a lot and lets you access a wide range of libraries written by the
JavaScript community.

## What Lucid is

To understand what Lucid is we need to understand how Cardano works. On Cardano
most of the computation happens off-chain. Smart contracts also known as
validators are just scripts that lock UTxOs. In order to spend these UTxOs the
validators needs to be executed, which then either result in true or false. A
validator itself cannot change the state of the chain, only the transaction can.
A validator makes sure the transaction is created with certain necessary
constraints. So Lucid is a framework that helps you to easily build these
transactions to satisfy the constraints of the validator.

## What Lucid is not

Lucid is not a smart contract language or a tool to write validators. Lucid is
an off-chain component that manages everything from transaction building to
wallet creation.
