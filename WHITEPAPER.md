---
# mainfont: DejaVuSerif.ttf
# sansfont: DejaVuSans.ttf
# monofont: DejaVuSansMono.ttf
monofont: /Users/kelvin/Library/Fonts/Fira Code Regular Nerd Font Complete Mono.otf
# mathfont: texgyredejavu-math.otf
---

Kindelia: a peer-to-peer functional computer
--------------------------------------------

Kindelia is a peer-to-peer functional computer capable of hosting applications
that stay up forever. It has no native coin, which means it isn't a
cryptocurrency, but a cryptocomputer. It uses the
[HVM](https://github.com/kindelia/hvm), a blazingly fast functional virtual
machine, to run formally verified apps natively, making it as secure as
mathematically possible. It replaces expensive Merkle tree insertions by
reversible heaps snapshots, greatly reducing the cost of SSTORE and SLOAD, which
makes real-time apps economically viable. Unlike similar projects, it addresses
political and economical centralization. Finally, it is extremelly minimal: a full
node requires drastically less lines of code than alternatives.

Introduction
------------

Kindelia is a pure decentralized computer that aggressively purges redundant
features and bloat, removes the built-in token, replaces the EVM, a slow stack
machine, by the HVM, a fast graph reduction machine, and replaces slow Merkle
trees by fast heap snapshots. That allows it to host 2 kinds of apps that,
previously, were not economically viable:

### 1. Formally Verified apps

These apps are developed in functional languages like Idris, Agda and Kind, and
accompanied by a "formal verification" of their correctness, which is like a
mathematical guarantee that they can not have any bug or exploit, at all.  This
is extremely valuable for DApps, because they hold money, yet can't be patched
or reversed, which is a recipe for disaster. Deploying formally verified apps
written on these languages to Ethereum or Cardano is not economically viable,
but, thanks to HVM's functional opcodes, including constant-cost beta-reduction
and pattern matching, Kindelia can handle these natively and cheaply.

### 2. Real-Time apps

Apps that involve huge amounts of state changes, such as games and exchanges, are
too expensive on Ethereum, due to the high cost of the SSTORE opcode. Moreover,
persisting state on Ethereum is complex and error-prone, due to the need of
serializing structures into a map of 256-bit integers. Kindelia replaces the
Merkle stores by reversible snapshots of HVM's heap, which makes the SSTORE
opcode essentially free. This not only makes statefulness much more convenient
and less error-prone (since apps can persist high-level structures), but also
allows Kindelia to host highly dynamic layer 1 apps, such as games, which are
simply not viable on Ethereum.

In short, for these familiar with functional languages, Kindelia can be seen as
a worldwide, permanent functional REPL - like Haskell's GHCI - where anyone can
input commands to deploy and run functions that stay up forever. And for these
familiar with decentralized computers, Kindelia can be seen as a polishment of
Ethereum, under the following recipe:

```
1. Start from Ethereum
2. Remove Ether (and the associated pre-mine)
3. Replace the slow EVM by the fast HVM
4. Replace slow Merkle trees by fast reversible heaps
5. Aggressively optimize and simplify everything
6. Actually build it all before raising any money
7. Deliver a working network with a fair release
```

Note we do not claim that Kindelia is *better* than Ethereum. Ethereum is a
monolithic protocol with tons of features, opcodes, precompiles, a [complex
block structure](https://i.stack.imgur.com/afWDt.jpg), and an ambitious roadmap
to add grandiose systems such as PoS and shards. Kindelia doesn't offer all
these features. Instead, it focuses on being the simplest p2p computer possible,
offering just the minimal set of features required to host secure apps in a
decentralized network. It follows the Unix philosophy of doing one thing, and
doing it well. For a comparison, Ethereum's Go client has 680k lines of code and
is growing, while Kindelia has about 10k and intends to keep that count low. In
short, Kindelia aims to be as simple, boring, robust and fair as Bitcoin, while
still retaining the Turing completeness that makes Ethereum so powerful.

How it works?
-------------

On conventional cryptocurrencies such as Bitcoin, network peers use a consensus
algorithm to agree on a canonical ordering of blocks. These blocks group
user-generated transactions, which have the effect of sending money from an
address to another. On Ethereum, these transactions can also execute
computations that change a contract's state. On Kindelia, blocks don't group
*transactions*, but *statements* with no monetary content; that is, they don't
have `.to` or `.amount` fields. Instead, they are just scripts that affect the
network state.

     Block #0                  Block #1                  Block #2
    .---------------.         .---------------.         .---------------.
    | - Prev_Hash   | <------ | - Prev_Hash   | <------ | - Prev_Hash   |
    | - Time        |         | - Time        |         | - Time        |
    | - Nonce       |         | - Nonce       |         | - Nonce       |
    | - Statement_0 |         | - Statement_0 |         | - Statement_0 |
    | - Statement_1 |         | - Statement_1 |         | - Statement_1 |
    | - ...         |         | - ...         |         | - ...         |
    | - Statement_N |         | - Statement_N |         | - Statement_N |
    '---------------'         '---------------'         '---------------'

Kindelia nodes use Proof-of-Work consensus to order blocks canonically.
Proof-of-Stake isn't intended, nor possible, since there is no native currency
to stake. Blocks merely group statements, of which there are 4 kinds:

- **CTR**: declares a new constructor

- **FUN**: installs a new global function

- **RUN**: runs a side-effective expression

- **REG**: registers a namespace

Kindelia functions are defined by equations, as in Haskell. For example, the
function below sums an immutable tree:

```c
fun (Sum tree) {
  (Sum {Leaf x})     = x
  (Sum {Branch a b}) = (+ (Sum a) (Sum b))
}
```

Once deployed and mined, this statement will cause `Sum` to be defined globally.
Kindelia functions can call each-other, and are pure, thus, side-effect free.
The `RUN` statement offers a escape hatch where side-effects can occur and alter
the network's state,in a way that works exactly like Haskell's IO.
For example, the statement below uses the `ask` syntax (similar to Haskell's
`do`) to increment the state of a global `Count` function, and then returns its
current state:

```c
run {
  ask (Call 'Count' {Inc});
  ask num = (Call 'Count' {Get});
  (Done state)
}
```

When that statement is included in a block, its effects are evaluated inside the
[HVM](https://github.com/kindelia/hvm), which offers all the features required
for a great decentralized runtime: instant compilation, no garbage accumulation,
great performance, and a well-defined cost table. Among alternatives, only
Kindelia is capable of running functional apps with an efficiency on par with
Haskell's GHC.

```
.-----------------------------------------------------.
| Opcode    | Effect                          | Mana  |
|-----------|---------------------------------|-------|
| APP-LAM   | applies a lambda                | 2     |
| APP-SUP   | applies a superposition         | 4     |
| OP2-NUM   | operates on a number            | 2     |
| OP2-SUP   | operates on a superposition     | 4     |
| FUN-CTR   | pattern-matches a constructor   | 2 + M |
| FUN-SUP   | pattern-matches a superposition | 2 + A |
| DUP-LAM   | clones a lambda                 | 4     |
| DUP-NUM   | clones a number                 | 2     |
| DUP-CTR   | clones a constructor            | 2 + A |
| DUP-SUP-0 | clones a superposition          | 4     |
| DUP-SUP-1 | undoes a superposition          | 2     |
| DUP-ERA   | clones an erasure               | 2     |
|-----------------------------------------------------|
| * A is the constructor or function arity            |
| * M is the alloc count of the right-hand side       |
'-----------------------------------------------------'
```

The elegant cost table above exhibit the 12 opcodes of HVM, including lambda
application, pattern-matching, integer operations and primitives for erasing and
duplicating data. These opcodes are expression-based, rather than stack-based.
For example, an `OP2-NUM` is computed on the `(+ 2 3) ~> 5` reduction. Kindelia
nodes impose a maximum "mana" limit per block, which combats spam, fulfilling a
role similar to Ethereum's gas.

Note that the table above has only computational opcodes; there is no equivalent
to "SSTORE" or "SLOAD". That's because space is treated separately from
computation. In order to persist data, functions can invoke the `save` and
`load` effects. For example, below we implement the `Count` function:

```c
fun (Count action) {

  // Increments the counter
  (Count {Inc}) =
    ask num = Load;
    ask (Save (+ num #1));
    (Done #0)

  // Returns the counter
  (Count {Get}) =
    ask num = Load;
    (Done num)

} with { #0 }
```

Internally, all that `!save` does is store a pointer to the saved expression,
preventing it from being garbage-collected on HVM's runtime heap, which causes
it to persist across blocks. In other words, Kindelia's `!save` is drastically
cheaper than Ethereum's `SSTORE`, which involves an expensive Merkle tree
insertion. It is also more convenient, since apps can save and load entire
structures such as lists, maps and JSONs at once, instead of having to serialize
and deserialize them into maps of u256 words.

Of course, since `!save` has no cost, this would expose the network to a trivial
spam attack, where a function persists an obscene amount of data. To prevent
that, nodes also impose a limit on how much the HVM's heap can grow on each
block. That limit is called `bits`, and is separated from the `mana` limit. 

Note the syntax used so far is a low-level representation of HVM's inner terms,
so, while it looks somewhat high-level, it is actually equivalent to our assembly,
and developers aren't meant to write on it directly, other than for debugging
purposes. Instead, they should use a higher level language such as Idris, Agda,
Coq, Lean or Kind2. Below is the same program, in Kind:

```c
Count : Contract {
  init: #0
  call: @action
    match action {
      // Increments the counter
      Inc => do {
        num = Load;
        Save (+ num #1);
        return #0;
      }
      // Returns the counter
      Get => do {
        ask num = Load;
        return num;
      }
    }
}
```

With Kind's dependent type system, devs can prove theorems about their contracts,
ensuring that they're mathematically unbreakable, which, while not an absolute
guarantee (after all, someone could forget to prove an important invariant!), is
as good as it gets in terms of security, and is on an entire new league compared
to former alternatives. Developers could also use Idris, Coq, Lean, Agda and even
Haskell. Compiling functional languages to Kindelia is viable, because of its
functional opcodes.

Statements can also be signed:

```c
run {
  ask (Call 'CatCoin' {Send 'Alice' 100});
  (Done #0)
} sign {
  00c0777281fe0a814d0f1826ad
  7f4228f7308df5c4365f8dc577
  ed64b3e32505a143d5566b8d38
  1f5b93988d19a82924fcef232e
  6ccc5a0e006e5b6f946cd15372
}
```

A signed statement works as usual, except that, when it calls a function, that
function can query the signer's address, allowing it to operate like a contract.
For example, a `CatCoin` function can store an `Address -> Balance` to operate
like a token.  Kindelia addresses consist of the first 15 bytes of the
respective Ethereum address. As such, both account systems are compatible, and
Ethereum users can use their existing accounts to sign Kindelia statements.

Kindelia also has a built-in namespace system based on a hierarchy of names.
Kindelia functions are addressed by 72-bit names, a limit imposed by the size of
HVM's pointers. These names contain up to 12 6-bit chars, including letters,
numbers, underscore and periods. Names without periods can be deployed by
anyone. Names with periods are namespaced, and can only be deployed by the
namespace owner. So, for example, `Foo.Bar.app` can only be deployed by the
owner of the `Foo.Bar` namespace. The owner of a namespace can grant
sub-namespaces to other users.

Finally, the lack of a native currency may cause one to wonder how block rewards
and transaction fees could possibly work. Initially, when the network isn't
fully used, miners will just include statements altruistically, sorted by their
hashes, i.e., using a portable Proof-of-Work in order to prevent spam. That
means that, on the beginning, anyone will be able to deploy and use functions
for free.  When blocks reach any of the 3 imposed limits (space, state growth or
computation), a fee market will naturally emerge, and users will pay miners to
prioritize their statements:

```c
// Statement signed by Bob to send 1000 CAT to Alice
run {
  ask miner = (call 'BlockMiner' {Get});    // gets the miner name
  ask (call 'CatCoin' {Send 'Alice' 1000}); // Alice gets: 1000 CAT
  ask (call 'CatCoin' {Send miner     50}); // Miner fees:   50 CAT
  (Done #0)
} sign {
  ...signature...
}
```

As for block rewards, the same principle holds. Tokens and applications can leave
rewards that only the block miner can collect. For example, Kindelia's Genesis Token,
a no-premine currency which will be deployed by the Kindelia Foundation on the first
block, will include a method that mints coins once per block, following Bitcoin's
emission curve. This serves as an incentive for miners that keep the network secure.
In other words, Kindelia doesn't need a built-in token to have block rewards and miner
fees. Instead, it flexibly allows users to pay fees in whatever tokens they want, and
miners to collect block rewards from a constellation of user-deployed tokens, rather
than a single official one.

Benchmarks
----------

Ethereum and Kindelia are architecturally different in many aspects,
so it is not always straightforward to draw direct comparisons between them,
but we can make approximations. The table below compares network parameters:

```
Parameter          |       Ethereum |       Kindelia
------------------ | -------------- | --------------
Avg. Block Time    |     13 seconds |       1 second
Avg. Block Size    |   95,441 bytes |    1,280 bytes
State Growth Limit |      ??? bytes |      256 bytes
Computation Limit  | 21,000,000 gas | 4,000,000 mana
```

Kindelia blocks are shorter, which allows them to fit in a single UDP packet,
enabling its ultra-fast 1 second block time. Despite that, Kindelia can sustain
the same throughput as Ethereum, since its space is used more efficiently. For
example, the table below compares common transaction sizes:

```
Transaction             | Ethereum  | Kindelia | ratio
----------------------- | --------- | -------- | -----
Deploy a small contract | 550 bytes | 66 bytes |  12 %
Call a simple method    | 113 bytes | 32 bytes |  28 %
```

These numbers were obtained by deploying and calling a simple [counter
contract](https://gist.github.com/VictorTaelin/bb0f8fb30b61bf0c216675791b72500c)
on both networks. A contract deployment on Kindelia can use up to 8x less space
than on Ethereum. In average, both networks should sustain the same number of
transactions per second, but a single Kindelia transaction can do much more.
The table below compares both networks in actual operations per second:

```
Operation              |      Ethereum |       Kindelia |    ratio
---------------------- | ------------- | -------------- | --------
Numeric addition       |  769,230 op/s | 2,000,000 op/s |    260 %
Numeric multiplication |  461,538 op/s | 2,000,000 op/s |    432 %
Lambda application     |   11,538 op/s | 2,000,000 op/s | 17,332 %
Pattern match          |   11,538 op/s | 2,000,000 op/s | 17,332 %
Uint load              |   23,076 op/s | 2,000,000 op/s |  1,733 %
Uint store             |      461 op/s | 2,000,000 op/s | 86,760 %
```

On Kindelia, all the opcodes above cost exactly 2 mana. On Ethereum, numeric
opcodes are cheap (3-5 gas), but functional operations are expensive (about
[~200 gas](https://medium.com/@maiavictor/compiling-formality-to-the-evm-99aec75677dd)
per beta-reduction, due to emulation overhead), and storage opcodes are
prohibitive (100-20000 gas, due to costly Merkle tree insertions). Kindelia can
sustain from 2x to 867x more operations per second than Ethereum, which is what
makes it able to host functional and real-time applications without complex
layer 2 indirections.

Conclusion
----------

With pure functions, effect expressions and signed transactions, Kindelia can
host any conceivable application, from tokens, to exchanges, to complete virtual
worlds. Thanks to HVM's native functional opcodes, applications hosted on it can
be made as secure as mathematically possible, by programming them on well-typed
languages, or even proof assistants. Moreover, thanks to reversible heap
snapshots, Kindelia is able to host dynamic applications such as games and
exchanges natively, which wasn't economically possible without layer 2
solutions. 
