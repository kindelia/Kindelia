# Kindelia: a minimal peer-to-peer computer that isn't a cryptocurrency

Kindelia is a peer-to-peer functional computer capable of hosting decentralized
apps that stay up forever. Essentially, it is a minimalist redesign of
Ethereum's idea, built upon type-theoretic foundations. Differences include:

- There is **no native coin**. It is not a cryptocurrency but a crypto-computer.

- It can host **functional apps** cheaply, thanks to the [HVM].

- It can host **real-time apps** cheaply, thanks to 1s blocks, reversible heaps,
  and zero-cost SSTOREs.

- It is **extremely minimalist**. The Rust client has about 10k LOC, vs 600k+
  LOC used by Go Ethereum.

- It is **maximally decentralized**, not just tech-wise. Political and economic
  centralization are addressed.

- It is **PoW-based**, forever. PoS isn't even possible since there is no
  built-in currency.

## Resources

- For a quick summary, check the [whitepaper](WHITEPAPER.md).

- For an in-depth overview, check the [whitebook](WHITEBOOK.md).

- For a roadmap, check the [roadmap](ROADMAP.md).

## Installation

- Get Rust. See [rustup.rs](https://rustup.rs/).

- Clone this repository, navigate to its root directory, and run:

    ```sh
    cargo install --path kindelia
    ```

## Usage

1. Start a node:

    ```sh
    kindelia node start [--mine]
    ```

2. Run transactions offline:

    ```sh
    kindelia test example/example.kdl
    ```

3. Dry run transactions in your local node:

    ```sh
    kindelia run-remote example/example.kdl
    ```

4. Posting transactions to your local node:

     ```sh
     kindelia publish example/example.kdl
     ```

---

## Completion

### Bash

Source the output of `kindelia completion bash`.

```sh
kindelia completion bash > kindelia_completion.sh
source kindelia_completion.sh
```

### Zsh

Put the output of `kindelia completion zsh` in a folder listed in your `$FPATH`
(e.g. `/usr/local/share/zsh/site-functions`).

```zsh
kindelia completion zsh > kindelia_completion.sh
```

[HVM]: https://github.com/kindelia/hvm
