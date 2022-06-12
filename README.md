Kindelia
========

A peer-to-peer functional computer capable of hosting decentralized apps that stay up forever. Essentially, it ss a complete redesign of Ethereum's idea made by a passionate type theorist. Main differences include:

- There is no native coin. It is just a p2p computer, not a cryptocurrency.

- The [EVM](https://ethereum.org/en/developers/docs/evm/) by the [HVM](https://github.com/kindelia/hvm), a parallel functional runtime.

- It can run formally verified programs from languages like [Idris](https://github.com/idris-lang/Idris2), [Kind](https://github.com/kindelia/kind) natively.

- Zero-cost SSTOREs, allowing highly dynamic apps (like MMORPGs) to run on layer 1!

- Everything is much simpler, well-thought and saner in general.

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
