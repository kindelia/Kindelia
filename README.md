Kindelia
========

A peer-to-peer functional computer capable of hosting decentralized apps that stay up forever. Essentially, it is a complete redesign of Ethereum's idea, built upon type theoretic foundations. Main differences include:

- There is no native coin. It is not a cryptocurrency, it is a cryptocomputer.

- The [EVM](https://ethereum.org/en/developers/docs/evm/) is replaced by the [HVM](https://github.com/kindelia/hvm), a blazingly fast functional runtime.

- It can run formally verified programs from languages like [Idris](https://github.com/idris-lang/Idris2) and [Kind](https://github.com/kindelia/kind) natively.

- Zero-cost SSTOREs, allowing dynamic games and virtual worlds to run on layer 1.

- Truly decentralized, not just in code. **Political** and **economical** decentralization were addressed.

- It is so minimalist and elegant you could call it a massively multiplayer λ-calculus REPL.

For more information, check the [whitepaper](WHITEPAPER.md).

Installation
------------

```bash
# in this directory:
cargo install --path .
```

Usage
-----

1. Starting a node:

```
kindelia start
```


2. Running a block (offline):

```
kindelia run example/example.kdl
```

3. Posting a transaction:

```
kindelia post example/post.kdl 127.0.0.1:42000
```
