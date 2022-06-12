Kindelia
========

A peer-to-peer functional computer capable of hosting decentralized apps that stay up forever. Essentially, it is a complete redesign of Ethereum's idea, built upon type theoretic foundations. Main differences include:

- There is no native coin. It is just cryptocomputer, not a cryptocurrency.

- The [EVM](https://ethereum.org/en/developers/docs/evm/) is replaced by the [HVM](https://github.com/kindelia/hvm), a blazingly fast functional runtime.

- It can run formally verified programs from languages like [Idris](https://github.com/idris-lang/Idris2) and [Kind](https://github.com/kindelia/kind) natively.

- Zero-cost SSTOREs, allowing dynamic games and virtual worlds to run on layer 1.

- Everything is massively simplified and optimized.

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
