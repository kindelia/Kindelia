Kindelia: a minimal peer-to-peer computer that isn't a cryptocurrency
---------------------------------------------------------------------

Kindelia is a peer-to-peer functional computer capable of hosting decentralized apps that stay up forever. Essentially, it is a minimalist redesign of Ethereum's idea, built upon type theoretic foundations. Differences include:

- There is **no native coin**. It is not a cryptocurrency. It is a cryptocomputer.

- It can host **functional apps** cheaply, thanks to the [HVM](https://github.com/kindelia/hvm) and its functional opcodes.

- It can host **real-time apps** cheaply, thanks to 1s blocks, reversible heaps and zero-cost SSTOREs.

- It is **extremely minimalist**. The Rust client has about 10k LOC, vs 600k+ LOC used by Go Ethereum.

- It is **maximally decentralized**, not just tech-wise. Political and economical centralization are addressed.

- It is **PoW-based**, forever. In fact, PoS isn't even possible, since there is no built-in currency.

Resources
---------

- For a quick summary, check the [whitepaper](WHITEPAPER.md).

- For an in-depth overview, check the [whitebook](WHITEBOOK.md).

- For a roadmap, check the [roadmap](ROADMAP.md).

Installation
------------

Clone this repository, navigate to its root directory, and enter:

```sh
cargo install --path kindelia
```

Usage
-----

1. Starting a node:

```sh
kindelia node start
```

2. Running a block (offline):

```sh
kindelia test example/example.kdl
```

3. Posting a transaction:

```sh
kindelia post-udp --host 127.0.0.1:42000 example/post.kdl 
```
