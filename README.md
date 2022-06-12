Kindelia
========

A peer-to-peer functional computer where developers can host decentralized apps that stay up forever. It is like a massively simplified Ethereum, with no currency, and greatly increased efficiency and security. It replaces the [EVM](https://ethereum.org/en/developers/docs/evm/) by the [HVM](https://github.com/kindelia/hvm), a functional virtual machine capable of running mathematically unbreakable programs from languages like [Idris](https://github.com/idris-lang/Idris2) and [Kind](https://github.com/kindelia/kind). It also features zero-cost SSTOREs, making dynamic apps massively cheaper.

For more information, check the [whitepaper](WHITEPAPER.md).

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
