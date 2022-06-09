Kindelia
========

Kindelia is a peer-to-peer functional computer. It allows developers to host decentralized applications that stay up forever. It can be seen as a complete remake of Ethereum, greatly simplifying the protocol, increasing layer-1 performance and security. It is on development, with the genesis block scheduled for Jan 2023. For more information, check our [WHITEPAPER.md](WHITEPAPER.md). Notable differences include:

### Zero-cost reused SSTOREs

It uses a different state store, which replaces expensive Merkle trees by efficient reversible heaps, allowing for **zero-cost reused SSTOREs**. This makes Kindelia the only layer-1 computation network capable of hosting highly dynamic applications like games with realistic costs, without appealing to complex crypto like zero-knowledge proofs. In exchange, we lose the ability of making certain kinds of light clients.

### A functional runtime

It replaces the [EVM](https://ethereum.org/en/developers/docs/evm/) by the [HVM](https://github.com/kindelia/hvm), a functional runtime that decreases the cost of beta-reduction and pattern-matching 100-fold. This, among other things, makes it highly compatible with Haskell, Idris, Kind and the like, viabilizing **formal verification**, the act of mathematically proving that an app has zero bugs before deployment. This is not just a nice feature, but an essential part of applications that can't be patched or reversed. After all, what is the point of hosting an app in an eternal computer, if it will only last until a bug is found?

### No currency, abstracted accounts, several polishments

Everything is simplified and polished. It has no native token, thus, **is not a cryptocurrency**. It is just a pure p2p computer. Blocks are just files of code, transactions are just function definitions or statements. Users can either mine their own blocks, or pay miners in any on-chain asset, rather than a single favored asset. Also, accounts are fully abstracted, which allows users to pick whatever signature scheme they like, making the network **inherently secure against quantum computers**, which would irreversibly destroy Bitcoin and Ethereum.

Installation
------------

```bash
# in this directory:
cargo install --path .
```

Usage
-----

1. Testing a block offline:

```
kindelia run example/example.kindelia
```

2. Starting a node:

```
kindelia start
```

Note: node communication and consensus still in development.
