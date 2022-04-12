Kindelia: a hack-proof decentralized computer
=============================================

Turing-complete blockchains, such as Ethereum, allow users to host applications
in a decentralized network (DApps), but continuous security exploits hinder
their adoption. Formal verification techniques can be used to ensure these
applications have no exploits before being deployed, but Ethereum's underlying
processor, the EVM, make these techniques too expensive to be viable. By
leveraging a pure functional virtual machine, the HVM, Kindelia is able to run
formally verified programs cheaply and efficiently, letting it host hack-proof
DApps. Moreover, due to extensive low-level optimizations, Kindelia is able to
sustain a considerably higher throughput, making its layer-1 transaction costs 2
order of magnitudes lower than Ethereum's, for the same usage level.

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
