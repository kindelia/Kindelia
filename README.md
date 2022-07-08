Kindelia: a minimal peer-to-peer computer that isn't a cryptocurrency
---------------------------------------------------------------------

Kindelia is a peer-to-peer functional computer capable of hosting decentralized apps that stay up forever. Essentially, it is a minimalist redesign of Ethereum's idea, built upon type theoretic foundations. Differences include:

- There is **no native coin**. It is not a cryptocurrency. It is a cryptocomputer.

- It can host **functional apps** cheaply, thanks to the [HVM](https://github.com/kindelia/hvm) and its functional opcodes.

- It can host **real-time apps** cheaply, thanks to reversible heap snapshots and zero-cost SSTOREs.

- It is **extremely minimalist**. The Rust client has about 10k LOC, vs 600k+ LOC for Go Ethereum.

- It is **maximally decentralized**, not just technologically. Political and economical decentralization are addressed.

Resources
---------

- For a quick summary, check the [whitepaper](WHITEPAPER.md).

- For an in-depth overview, check the [whitebook](WHITEBOOK.md).

- For a roadmap, check the [roadmap](ROADMAP.md).

Installation
------------

```bash
# in this directory:
cargo install --path .
```

For dev/debug purposes, with debug assertions etc, also useful for machines
without enough memory to allocate the full memory requirement:

```bash
cargo install --path=. --profile=dev_fast
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
