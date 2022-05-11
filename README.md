Kindelia: a peer-to-peer functional computer
============================================

Turing-complete blockchains, such as Ethereum, allow users to create
decentralized applications (DApps), but continuous security exploits hinder
their adoption. Formal verification techniques can be used to ensure these DApps
have no exploits, but Ethereum's underlying processor, the EVM, make these too
expensive to be viable. By leveraging a functional virtual machine, the HVM,
Kindelia is able to run formally verified DApps cheaply and efficiently.
Moreover, extensive low-level optimizations let it sustain an extremelly high
layer-1 throughput, compared to alternatives. Finally, Kindelia has no built-in
token, and, thus, isn't a cryptocurrency; instead, it is merely a peer-to-peer
computer capable of hosting secure functional programs.

Introduction
============

### Efficiency

Its virtual machine, the HVM, can do up to _TODO_ more arithmetic operations per
second, and, notably, _TODO_ more pattern-matches per second. Moreover, Kindelia
transactions are up to _TODO_ smaller than Ethereum. These numbers mean that, at
baseline, Kindelia transactions should be 2 orders of magnitude cheaper than
Ethereum's, at the same usage level. That doesn't come from a layer-2 solution,
nor zero-knowledge proofs. This is exclusively due to sheer layer-1 efficiency,
which, even though not sufficient to achieve long-term scalability, does matter;
after all, layer-2 transactions must still use layer-1, and zero-knowledge
proofs add overwhelming complexity to the system.

TODO: Get the exact numbers by running OpenEthereum and comparing.

TODO: insert tables here
