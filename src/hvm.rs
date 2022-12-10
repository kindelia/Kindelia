// Welcome to Kindelia's High-order Virtual Machine!
// =================================================
//
// This file is a modification of the project hosted on github.com/kindelia/hvm, and it makes a
// series of changes with the goal of serving the requirements of a peer-to-peer computer.
//
// Kindelia-HVM's memory model
// ---------------------------
//
// The runtime memory consists of just a vector of u128 pointers. That is:
//
//   Mem ::= Vec<Ptr>
//
// A pointer has 3 parts:
//
//   Ptr ::= TT AAAAAAAAAAAAAAAAAA BBBBBBBBBBBB
//
// Where:
//
//   T : u8  is the pointer tag 
//   A : u72 is the 1st value
//   B : u48 is the 2nd value
//
// There are 12 possible tags:
//
//   Tag | Val | Meaning
//   ----| --- | -------------------------------
//   DP0 |   0 | a variable, bound to the 1st argument of a duplication
//   DP1 |   1 | a variable, bound to the 2nd argument of a duplication
//   VAR |   2 | a variable, bound to the one argument of a lambda
//   ARG |   3 | an used argument of a lambda or duplication
//   ERA |   4 | an erased argument of a lambda or duplication
//   LAM |   5 | a lambda
//   APP |   6 | an application
//   SUP |   7 | a superposition
//   CTR |   8 | a constructor
//   FUN |   9 | a function
//   OP2 |  10 | a numeric operation
//   NUM |  11 | a 120-bit number
//
// The semantics of the 1st and 2nd values depend on the pointer tag. 
//
//   Tag | 1st ptr value                | 2nd ptr value
//   --- | ---------------------------- | ---------------------------------
//   DP0 | the duplication label        | points to the duplication node
//   DP1 | the duplication label        | points to the duplication node
//   VAR | not used                     | points to the lambda node
//   ARG | not used                     | points to the variable occurrence
//   ERA | not used                     | not used
//   LAM | not used                     | points to the lambda node
//   APP | not used                     | points to the application node
//   SUP | the duplication label        | points to the superposition node
//   CTR | the constructor name         | points to the constructor node
//   FUN | the function name            | points to the function node
//   OP2 | the operation name           | points to the operation node
//   NUM | the most significant 72 bits | the least significant 48 bits
//
// Notes:
//
//   1. The duplication label is an internal value used on the DUP-SUP rule.
//   2. The operation name only uses 4 of the 72 bits, as there are only 16 ops.
//   3. NUM pointers don't point anywhere, they just store the number directly.
//
// A node is a tuple of N pointers stored on sequential memory indices.
// The meaning of each index depends on the node. There are 7 types:
//
//   Duplication Node:
//   - [0] => either an ERA or an ARG pointing to the 1st variable location
//   - [1] => either an ERA or an ARG pointing to the 2nd variable location.
//   - [2] => pointer to the duplicated expression
//
//   Lambda Node:
//   - [0] => either and ERA or an ERA pointing to the variable location
//   - [1] => pointer to the lambda's body
//
//   Application Node:
//   - [0] => pointer to the lambda
//   - [1] => pointer to the argument
//
//   Superposition Node:
//   - [0] => pointer to the 1st superposed value
//   - [1] => pointer to the 2sd superposed value
//
//   Constructor Node:
//   - [0] => pointer to the 1st field
//   - [1] => pointer to the 2nd field
//   - ... => ...
//   - [N] => pointer to the Nth field
//
//   Function Node:
//   - [0] => pointer to the 1st argument
//   - [1] => pointer to the 2nd argument
//   - ... => ...
//   - [N] => pointer to the Nth argument
//
//   Operation Node:
//   - [0] => pointer to the 1st operand
//   - [1] => pointer to the 2nd operand
//
// Notes:
//
//   1. Duplication nodes DON'T have a body. They "float" on the global scope.
//   2. Lambdas and Duplications point to their variables, and vice-versa.
//   3. ARG pointers can only show up inside Lambdas and Duplications.
//   4. Nums and vars don't require a node type, because they're unboxed.
//   5. Function and Constructor arities depends on the user-provided definition.
//
// Example 0:
//
//   Term:
//
//    {T2 #7 #8}
//
//   Memory:
//
//     Root : Ptr(CTR, 0x0000000007b9d30a43, 0x000000000000)
//     0x00 | Ptr(NUM, 0x000000000000000000, 0x000000000007) // the tuple's 1st field
//     0x01 | Ptr(NUM, 0x000000000000000000, 0x000000000008) // the tuple's 2nd field
//
//   Notes:
//
//     1. This is just a pair with two numbers.
//     2. The root pointer is not stored on memory.
//     3. The '0x0000000007b9d30a43' constant encodes the 'T2' name.
//     4. Since nums are unboxed, a 2-tuple uses 2 memory slots, or 32 bytes.
//
// Example 1:
//
//   Term:
//
//     λ~ λb b
//
//   Memory:
//
//     Root : Ptr(LAM, 0x000000000000000000, 0x000000000000)
//     0x00 | Ptr(ERA, 0x000000000000000000, 0x000000000000) // 1st lambda's argument
//     0x01 | Ptr(LAM, 0x000000000000000000, 0x000000000002) // 1st lambda's body
//     0x02 | Ptr(ARG, 0x000000000000000000, 0x000000000003) // 2nd lambda's argument
//     0x03 | Ptr(VAR, 0x000000000000000000, 0x000000000002) // 2nd lambda's body
//
//   Notes:
//
//     1. This is a λ-term that discards the 1st argument and returns the 2nd.
//     2. The 1st lambda's argument not used, thus, an ERA pointer.
//     3. The 2nd lambda's argument points to its variable, and vice-versa.
//     4. Each lambda uses 2 memory slots. This term uses 64 bytes in total.
//
// Example 2:
//
//   Term:
//
//     λx dup x0 x1 = x; (* x0 x1)
//
//   Memory:
//
//     Root : Ptr(LAM, 0x000000000000000000, 0x000000000000)
//     0x00 | Ptr(ARG, 0x000000000000000000, 0x000000000004) // the lambda's argument
//     0x01 | Ptr(OP2, 0x000000000000000002, 0x000000000005) // the lambda's body
//     0x02 | Ptr(ARG, 0x000000000000000000, 0x000000000005) // the duplication's 1st argument
//     0x03 | Ptr(ARG, 0x000000000000000000, 0x000000000006) // the duplication's 2nd argument
//     0x04 | Ptr(VAR, 0x000000000000000000, 0x000000000000) // the duplicated expression
//     0x05 | Ptr(DP0, 0x7b93e8d2b9ba31fb21, 0x000000000002) // the operator's 1st operand
//     0x06 | Ptr(DP1, 0x7b93e8d2b9ba31fb21, 0x000000000002) // the operator's 2st operand
//
//   Notes:
//
//     1. This is a lambda function that squares a number.
//     2. Notice how every ARGs point to a VAR/DP0/DP1, that points back its source node.
//     3. DP1 does not point to its ARG. It points to the duplication node, which is at 0x02.
//     4. The lambda's body does not point to the dup node, but to the operator. Dup nodes float.
//     5. 0x7b93e8d2b9ba31fb21 is a globally unique random label assigned to the duplication node.
//     6. That duplication label is stored on the DP0/DP1 that point to the node, not on the node.
//     7. A lambda uses 2 memory slots, a duplication uses 3, an operator uses 2. Total: 112 bytes.
//     8. In-memory size is different to, and larger than, serialization size.
//
// How is Kindelia's HVM different from the conventional HVM?
// ----------------------------------------------------------
//
// First, it is a 128-bit, rather than a 64-bit architecture. It can store 120-bit unboxed
// integers, up from 32-bit unboxed uints stored by the conventional HVM. It allows addressing up
// to 2^72 function names, up from 2^30 allowed by the conventional HVM, which isn't enough for
// Kindelia. This change comes with a cost of about ~30% reduced performance, which is acceptable.
//
// Second, it implements a reversible heap machinery, which allows saving periodic snapshots of
// past heap states, and jump back to them. This is necessary because of decentralized consensus.
// If we couldn't revert to past states, we'd have to recompute the entire history anytime there is
// a block reorg, which isn't practical. On Ethereum, this is achieved by storing the state as a
// Map<U256> using Merkle Trees, which, being an immutable structure, allows non-destructive
// insertions and rollbacks. We could do the same, but we decided to further leverage the HVM by
// saving its whole heap as the network state. In other words, applications are allowed to persist
// arbitrary HVM structures on disk by using the io_save operation. For example:
//
//   (io_save {Cons #1 {Cons #2 {Cons #3 {Nil}}}} ...)
//
// The operation above would persist the [1,2,3] list as the app's state, with no need for
// serialization. As such, when the app stops running, that list will not be freed from memory.
// Instead, the heap will persist between blocks, so the app just needs to store a pointer to
// the list's head, allowing it to retrieve its state later on. This is only possible because the
// HVM is garbage-collection free, otherwise, leaks would overwhelm the memory.
//
// How are reversible heaps stored?
// --------------------------------
//
// Kindelia's heap is set to grow exactly 8 GB per year. In other words, 10 years after the genesis
// block, the heap size will be of exactly 80 GB. But that doesn't mean a full node will be able
// to operate with even that much ram, because Kindelia must also save snapshots. Right now, it
// stores at most 10 snapshots, trying to keep them distributed with exponentially decreasing ages.
// For example, if we're on block 1000, it might store a snapshot of blocks 998, 996, 992, 984,
// 968, 872, 744 and 488, which is compatible with the fact that longer-term rollbacks are
// increasingly unlikely. If there is a rollback to block 990, we just go back to the earliest
// snapshot, 984, and reprocess blocks 985-1000, which is much faster than recomputing the entire
// history.
//
// In order to keep a good set of snapshots, we must be able to create and discard these heaps.
// Obviously, if this operation required copying the entire heap buffer every block, it would
// completely destroy the network's performance. As such, instead, heaps only actually store
// data that changed. So, using the example above, if a list was allocated and persisted on block 980,
// it will actually be stored on the snapshot 984, which is the earliest snapshot after 980. If the
// runtime, now on block 1000, attempts to read the memory where the list is allocated, it will
// actually receive a signal that it is stored on a past heap, and look for it on 996 and 992,
// until it is found on block 984.
//
// To achieve that, hashmaps are used to store defined functions and persistent state pointers. If
// a key isn't present, Kindelia will look for it on past snapshots. As for the runtime's memory,
// where HVM constructors and lambdas are stored, it doesn't use a hashmap. Instead, it uses a
// Nodes type, which stores data in a big pre-allocated u128 buffer, and keeps track of used memory
// slots in a separate buffer. We then reserve a constant, U128_NONE, to signal that an index isn't
// present, and must be found ina  past heap. This is different from 0, which means that this index
// is empty, and can be allocated. This allows for fast write, read, disposal and merging of heaps,
// but comes at the cost of wasting a lot of memory. Because of that, Kindelia's current
// implementation demands up to ~10x more available memory than the current heap size, but that
// could be reduced ten-fold by replacing vectors by a hashmap, or by just saving fewer past heaps.
//
// Other than a 128-bit architecture and reversible heaps, Kindelia's HVM is similar to the
// conventional HVM. This file will be extensively commented, with in-depth explanations of every
// little aspect, from the HVM's memory model to interaction net rewrite rules.

#![allow(dead_code)]
#![allow(non_snake_case)]
#![allow(unused_variables)]
#![allow(clippy::style)]
#![allow(clippy::identity_op)]

use std::collections::{hash_map, HashMap, HashSet};
use std::fmt::{self, Write};
use std::hash::{BuildHasherDefault, Hash, Hasher};
use std::path::PathBuf;
use std::fs::File;
use std::sync::Arc;
use std::time::Instant;

use serde::{Serialize, Deserialize};
use serde_with::{serde_as, DisplayFromStr};

use crate::bits::ProtoSerialize;
use crate::constants;
use crate::crypto;
use crate::util::{self, U128_SIZE, mask};
use crate::util::{LocMap, NameMap, U128Map, U120Map};
use crate::NoHashHasher::NoHashHasher;

use crate::common::{Name, U120};
use crate::persistence::DiskSer;

/// This is the HVM's term type. It is used to represent an expression. It is not used in rewrite
/// rules. Instead, it is stored on HVM's heap using its memory model, which will be elaborated
/// later on. Below is a description of each variant:
/// - Var: variable. It stores up to 12 6-bit letters.
/// - Dup: a lazy duplication of any other term. Written as: `dup a b = term; body`
/// - Lam: an affine lambda. Written as: `@var body`.
/// - App: a lambda application. Written as: `(!f x)`.
/// - Ctr: a constructor. Written as: `{Ctr val0 val1 ...}`
/// - Fun: a function call. Written as: `(Fun arg0 arg1 ...)`
/// - Num: an unsigned integer.
/// - Op2: a numeric operation.

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum Term {
  Var { name: Name },
  Dup { nam0: Name, nam1: Name, expr: Box<Term>, body: Box<Term> },
  Lam { name: Name, body: Box<Term> },
  App { func: Box<Term>, argm: Box<Term> },
  Ctr { name: Name, args: Vec<Term> },
  Fun { name: Name, args: Vec<Term> },
  Num { numb: U120 },
  Op2 { oper: Oper, val0: Box<Term>, val1: Box<Term> },  // FIXME: refactor `oper` u128 to enum
}

/// A native HVM 120-bit machine integer operation.
/// - Add: addition
/// - Sub: subtraction
/// - Mul: multiplication
/// - Div: division
/// - Mod: modulo
/// - And: bitwise and
/// - Or : bitwise or
/// - Xor: bitwise xor
/// - Shl: shift left
/// - Shr: shift right
/// - Ltn: less than
/// - Lte: less than or equal
/// - Eql: equal
/// - Gte: greater than or equal
/// - Gtn: greater than
/// - Neq: not equal

#[derive(Clone, Copy, Debug, PartialEq, Serialize, Deserialize)]
pub enum Oper {
  Add, Sub, Mul, Div,
  Mod, And, Or,  Xor,
  Shl, Shr, Ltn, Lte,
  Eql, Gte, Gtn, Neq,
}

/// A rewrite rule, or equation, in the shape of `left_hand_side = right_hand_side`.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Rule {
  pub lhs: Term,
  pub rhs: Term,
}

/// A function, which is just a vector of rewrite rules.
#[derive(Clone, Debug, PartialEq, Default, Serialize, Deserialize)]
pub struct Func {
  pub rules: Vec<Rule>,
}

// The types below are used by the runtime to evaluate rewrite rules. They store the same data as
// the type aboves, except in a semi-compiled, digested form, allowing faster computation.

// Compiled information about a left-hand side variable.
#[derive(Clone, Debug, PartialEq)]
pub struct Var {
  pub name : Name,         // this variable's name
  pub param: u64,         // in what parameter is this variable located?
  pub field: Option<u64>, // in what field is this variable located? (if any)
  pub erase: bool,         // should this variable be collected (because it is unused)?
}

// Compiled information about a rewrite rule.
#[derive(Clone, Debug, PartialEq)]
pub struct CompRule {
  pub cond: Vec<RawCell>,          // left-hand side matching conditions
  pub vars: Vec<Var>,          // left-hand side variable locations
  pub eras: Vec<(u64, u64)>, // must-clear locations (argument number and arity)
  pub body: Term,              // right-hand side body of rule
}

// Compiled information about a function.
#[derive(Clone, Debug, PartialEq, Default)]
pub struct CompFunc {
  pub func: Func,           // the original function
  pub arity: u64,          // number of arguments
  pub redux: Vec<u64>,     // index of strict arguments
  pub rules: Vec<CompRule>, // vector of rules
}

// TODO: refactor all these maps to use `Name` newtype

// A file, which is just a map of `FuncID -> CompFunc`
// It is used to find a function when it is called, in order to apply its rewrite rules.
#[derive(Clone, Debug, PartialEq)]
pub struct Funcs {
  pub funcs: NameMap<Arc<CompFunc>>,
}

// TODO: arity with no u128

// A map of `FuncID -> Arity`
// It is used in many places to find the arity (argument count) of functions and constructors.
#[derive(Clone, Debug, PartialEq)]
pub struct Arits {
  pub arits: NameMap<u64>,
}

// A map of `FuncID -> FuncID
// Stores the owner of the 'FuncID' a namespace.
#[derive(Clone, Debug, PartialEq)]
pub struct Ownrs {
  pub ownrs: NameMap<U120>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Indxs {
  pub indxs: NameMap<u128>
}


// A map of `FuncID -> RawCell`
// It links a function id to its state on the runtime memory.
#[derive(Clone, Debug, PartialEq)]
pub struct Store {
  pub links: U120Map<RawCell>,
}

/// A global statement that alters the state of the blockchain
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum Statement {
  Fun { name: Name, args: Vec<Name>, func: Func, init: Option<Term>, sign: Option<crypto::Signature> },
  Ctr { name: Name, args: Vec<Name>, sign: Option<crypto::Signature> },
  Run { expr: Term, sign: Option<crypto::Signature> },
  Reg { name: Name, ownr: U120, sign: Option<crypto::Signature> },
}

/// RawCell
/// =======

/// An HVM memory cell/word.
/// It can point to an HVM node, a variable ocurrence, or store an unboxed U120.
#[derive(Debug, Eq, PartialEq, Clone, Hash, Copy)]
#[repr(transparent)]
pub struct RawCell(u128);

impl std::ops::Deref for RawCell {
  type Target = u128;
  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

impl RawCell {
  pub const fn new(value: u128) -> Option<Self> {
    // TODO: CellTag
    // let tag = num >> (EXT_SIZE + VAL_SIZE);
    // if matches!(tag, CellTag) {
    //   Some(RawCell(num))
    // } else {
    //   None
    // }
    Some(RawCell(value))
  }
  /// For testing purposes only. TODO: remove.
  pub const fn new_unchecked(value: u128) -> Self {
    RawCell(value)
  }
}

// Loc
// ===

/// A HVM memory location, or "pointer".

#[derive(Debug, Eq, PartialEq, Clone, Hash, Copy)]
#[repr(transparent)]
pub struct Loc(u64);

impl crate::NoHashHasher::IsEnabled for crate::hvm::Loc {}

impl Loc {
  pub const _MAX: u64 = (1 << VAL_SIZE) - 1;
  pub const MAX: Loc = Loc(Loc::_MAX);

  pub fn new(num: u64) -> Option<Self> {
    if num >> VAL_SIZE == 0 {
      Some(Loc(num))
    } else {
      None
    }
  }
}

impl std::ops::Deref for Loc {
  type Target = u64;
  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

impl std::ops::Add<u64> for Loc {
  type Output = Self;
  fn add(self, other: u64) -> Self::Output {
    Loc(self.0 + other)
  }
}

impl std::ops::Add for Loc {
  type Output = Self;
  fn add(self, other: Self) -> Self::Output {
    Loc(self.0 + other.0)
  }
}


// A mergeable vector of RawCells
#[derive(Debug, Clone, PartialEq)]
pub struct Nodes {
  pub nodes: LocMap<RawCell>,
}

#[derive(Debug, Clone, PartialEq)] 
pub struct Hashs {
  pub stmt_hashes: U128Map<crypto::Hash>,
}

// HVM's memory state (nodes, functions, metadata, statistics)
#[derive(Debug, Clone, PartialEq)]
pub struct Heap {
  pub uuid: u128,  // unique identifier
  pub memo: Nodes, // memory block holding HVM nodes
  pub disk: Store, // points to stored function states
  pub file: Funcs, // function codes
  pub arit: Arits, // function arities
  pub indx: Indxs, // function name to position in heap
  pub hash: Hashs,
  pub ownr: Ownrs, // namespace owners
  pub tick: u64,  // tick counter
  pub time: u128,  // block timestamp
  pub meta: u128,  // block metadata
  pub hax0: u128,  // block hash, part 0
  pub hax1: u128,  // block hash, part 1
  pub funs: u64,  // total function count
  pub dups: u64,  // total dups count
  pub rwts: u64,  // total graph rewrites
  pub mana: u64,  // total mana cost
  pub size: u64,  // total used memory (in 64-bit words)
  pub mcap: u64,  // memory capacity (in 64-bit words)
  pub next: u64,  // memory index that *may* be empty
  // TODO: store run results (Num). (block_idx, stmt_idx) [as u128] -> U120
}

// A list of past heap states, for block-reorg rollback
// FIXME: this should be replaced by a much simpler index array
#[derive(Debug, Clone)]
pub enum Rollback {
  Cons {
    keep: u64,
    life: u64,
    head: u64,
    tail: Arc<Rollback>,
  },
  Nil,
}

// The current and past states
pub struct Runtime {
  heap: Vec<Heap>,      // heap objects
  draw: u64,            // drawing heap index
  curr: u64,            // current heap index
  nuls: Vec<u64>,       // reuse heap indices
  back: Arc<Rollback>,  // past states
  path: PathBuf,        // where to save runtime state
}

#[derive(Debug, Clone)]
pub enum RuntimeError {
  NotEnoughMana,
  NotEnoughSpace,
  DivisionByZero,
  TermIsInvalidNumber { term: RawCell},
  CtrOrFunNotDefined { name: Name },
  StmtDoesntExist { stmt_index: u128},
  ArityMismatch { name: Name, expected: usize, got: usize },
  UnboundVar { name: Name },
  NameTooBig { numb: u128 },
  TermIsNotLinear { term: Term, var: Name },
  TermExceedsMaxDepth,
  EffectFailure(EffectFailure),
  DefinitionError(DefinitionError)
}
#[derive(Debug, Clone)]
pub enum DefinitionError {
  FunctionHasNoRules,
  LHSIsNotAFunction, // TODO: check at compile time
  LHSArityMismatch { rule_index: usize, expected: usize, got: usize }, // TODO: check at compile time
  LHSNotConstructor { rule_index: usize }, // TODO: check at compile time
  VarIsUsedTwiceInDefinition { name : Name, rule_index: usize },
  VarIsNotLinearInBody { name : Name, rule_index: usize },
  VarIsNotUsed { name : Name, rule_index: usize },
  NestedMatch { rule_index: usize },
  UnsupportedMatch { rule_index: usize },
  
}

#[derive(Debug, Clone)]
pub enum EffectFailure {
  NoSuchState { state: U120 },
  InvalidCallArg {caller: U120, callee: U120, arg: RawCell},
  InvalidIOCtr { name: Name },
  InvalidIONonCtr { ptr: RawCell },
}

//pub fn heaps_invariant(rt: &Runtime) -> (bool, Vec<u8>, Vec<u64>) {
  //let mut seen = vec![0u8; 10];
  //let mut heaps = vec![0u64; 0];
  //let mut push = |id: u64| {
    //let idx = id as usize;
    //seen[idx] += 1;
    //heaps.push(id);
  //};
  //push(rt.draw);
  //push(rt.curr);
  //for nul in &rt.nuls {
    //push(*nul);
  //}
  //{
    //let mut back = &*rt.back;
    //while let Rollback::Cons { keep, life, head, tail } = back {
      //push(*head);
      //back = &*tail;
    //}
  //}
  //let failed = seen.iter().all(|c| *c == 1);
  //(failed, seen, heaps)
//}

pub type StatementResult = Result<StatementInfo, StatementErr>;

// TODO: refactor (de)serialization out or simplify
#[serde_as]
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum StatementInfo {
  Ctr { name: Name, args: Vec<Name> },
  Fun { name: Name, args: Vec<Name> },
  Run {
    done_term: Term,
    #[serde_as(as = "DisplayFromStr")]
    used_mana: u64,
    #[serde_as(as = "DisplayFromStr")]
    size_diff: i64,
    #[serde_as(as = "DisplayFromStr")]
    end_size: u64,
  },
  Reg { name: Name, ownr: U120 },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StatementErr {
  pub err: String,
}

pub type ParseResult<'a, A> = Result<(&'a str, A), ParseErr>;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ParseErr {
  pub code: String,
  pub erro: String,
}

// Constants
// ---------

const U128_PER_KB: u128 = (1024 / U128_SIZE) as u128;
const U128_PER_MB: u128 = U128_PER_KB << 10;
const U128_PER_GB: u128 = U128_PER_MB << 10;

// With the constants below, we pre-alloc 6 heaps, which is enough for
// 4 snapshots: 16 seconds old, 4 minutes old, 1 hour old and 1 day old, on
// average.

/// Number of heaps (2 are used for draw/curr, the rest for rollbacks)
const MAX_HEAPS: u64 = 6;
// Number of heaps for snapshots
const MAX_ROLLBACK: u64 = MAX_HEAPS - 2;

pub const MAX_TERM_DEPTH: u128 = 256; // maximum depth of a LHS or RHS term

// Size of each RawCell field in bits
pub const VAL_SIZE: usize = 48;
pub const EXT_SIZE: usize = 72;
pub const TAG_SIZE: usize = 8;
pub const NUM_SIZE: usize = EXT_SIZE + VAL_SIZE;

// Position of each RawCell field
pub const VAL_POS: usize = 0;
pub const EXT_POS: usize = VAL_POS + VAL_SIZE;
pub const TAG_POS: usize = EXT_POS + EXT_SIZE;
pub const NUM_POS: usize = 0;

// First bit of each field
pub const VAL_SHL: u128 = 1 << VAL_POS;
pub const EXT_SHL: u128 = 1 << EXT_POS;
pub const TAG_SHL: u128 = 1 << TAG_POS;
pub const NUM_SHL: u128 = 1 << NUM_POS;

// Bit mask for each field
pub const VAL_MASK: u128 = mask(VAL_SIZE, VAL_POS);
pub const EXT_MASK: u128 = mask(EXT_SIZE, EXT_POS);
pub const TAG_MASK: u128 = mask(TAG_SIZE, TAG_POS);
pub const NUM_MASK: u128 = mask(NUM_SIZE, NUM_POS);

// TODO: refactor to enums with u8 / u128 repr

pub const DP0: u128 = 0x0;
pub const DP1: u128 = 0x1;
pub const VAR: u128 = 0x2;
pub const ARG: u128 = 0x3;
pub const ERA: u128 = 0x4;
pub const LAM: u128 = 0x5;
pub const APP: u128 = 0x6;
pub const SUP: u128 = 0x7;
pub const CTR: u128 = 0x8;
pub const FUN: u128 = 0x9;
pub const OP2: u128 = 0xA;
pub const NUM: u128 = 0xB;
pub const NIL: u128 = 0xF;

pub const ADD : u128 = 0x0;
pub const SUB : u128 = 0x1;
pub const MUL : u128 = 0x2;
pub const DIV : u128 = 0x3;
pub const MOD : u128 = 0x4;
pub const AND : u128 = 0x5;
pub const OR  : u128 = 0x6;
pub const XOR : u128 = 0x7;
pub const SHL : u128 = 0x8;
pub const SHR : u128 = 0x9;
pub const LTN : u128 = 0xA;
pub const LTE : u128 = 0xB;
pub const EQL : u128 = 0xC;
pub const GTE : u128 = 0xD;
pub const GTN : u128 = 0xE;
pub const NEQ : u128 = 0xF;

pub const U128_NONE : u128 = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF;
pub const I128_NONE : i128 = -0x7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF;
pub const U64_NONE: u64 = u64::MAX; //TODO: rewrite as FFF's?if think it is easier to read like this.

// TODO: r -> U120
// (IO r:Type) : Type
//   (DONE expr)           : (IO r)
//   (TAKE           then) : (IO r)
//   (SAVE expr      then) : (IO r)
//   (CALL name argm then) : (IO r)
//   (SUBJ           then) : (IO r)
//   (FROM           then) : (IO r)
//   (TICK           then) : (IO r)
//   (TIME           then) : (IO r)
const IO_DONE : u128 = 0x39960f; // name_to_u128("DONE")
const IO_TAKE : u128 = 0x78b54f; // name_to_u128("TAKE")
const IO_SAVE : u128 = 0x74b80f; // name_to_u128("SAVE")
const IO_CALL : u128 = 0x34b596; // name_to_u128("CALL")
const IO_SUBJ : u128 = 0x75f314; // name_to_u128("SUBJ")
const IO_FROM : u128 = 0x41c657; // name_to_u128("FROM")
const IO_LOAD : u128 = 0x5992ce; // name_to_u128("LOAD")
const IO_TICK : u128 = 0x793355; // name_to_u128("TICK")
const IO_TIME : u128 = 0x7935cf; // name_to_u128("TIME")
const IO_META : u128 = 0x5cf78b; // name_to_u128("META")
const IO_HAX0 : u128 = 0x48b881; // name_to_u128("HAX0")
const IO_HAX1 : u128 = 0x48b882; // name_to_u128("HAX1")
const IO_GIDX : u128 = 0x4533a2; // name_to_u128("GIDX")
const IO_STH0 : u128 = 0x75e481; // name_to_u128("STH0")
const IO_STH1 : u128 = 0x75e482; // name_to_u128("STH1")
// TODO: STH0 & STH1 -> get hash of statement (by (block_idx, stmt_idx))
// TODO: GRUN -> get run result

// Maximum mana that can be spent in a block
pub const BLOCK_MANA_LIMIT : u64 = 4_000_000;

// Maximum state growth per block, in bits
pub const BLOCK_BITS_LIMIT : u64 = 2048; // 1024 bits per sec = about 8 GB per year

// Mana Table
// ----------

// |-----------|---------------------------------|-------|
// | Opcode    | Effect                          | Mana  |
// |-----------|---------------------------------|-------|
// | APP-LAM   | applies a lambda                | 2     |
// | APP-SUP   | applies a superposition         | 4     |
// | OP2-NUM   | operates on a number            | 2     |
// | OP2-SUP   | operates on a superposition     | 4     |
// | FUN-CTR   | pattern-matches a constructor   | 2 + M |
// | FUN-SUP   | pattern-matches a superposition | 2 + A |
// | DUP-LAM   | clones a lambda                 | 4     |
// | DUP-NUM   | clones a number                 | 2     |
// | DUP-CTR   | clones a constructor            | 2 + A |
// | DUP-SUP-D | clones a superposition          | 4     |
// | DUP-SUP-E | undoes a superposition          | 2     |
// | DUP-ERA   | clones an erasure               | 2     |
// |-----------------------------------------------------|
// | * A is the constructor or function arity            |
// | * M is the alloc count of the right-hand side       |
// |-----------------------------------------------------|


fn AppLamMana() -> u64 {
  return 2;
}

fn AppSupMana() -> u64 {
  return 4;
}

fn Op2NumMana() -> u64 {
  return 2;
}

fn Op2SupMana() -> u64 {
  return 4;
}

fn FunCtrMana(body: &Term) -> u64 {
  return 2 + count_allocs(body);
}

fn FunSupMana(arity: u64) -> u64 {
  return 2 + arity;
}

fn DupLamMana() -> u64 {
  return 4;
}

fn DupNumMana() -> u64 {
  return 2;
}

fn DupCtrMana(arity: u64) -> u64 {
  return 2 + arity;
}

fn DupDupMana() -> u64 {
  return 4;
}

fn DupSupMana() -> u64 {
  return 2;
}

fn DupEraMana() -> u64 {
  return 2;
}

fn count_allocs(body: &Term) -> u64 {
  match body {
    Term::Var { name } => {
      0
    }
    Term::Dup { nam0, nam1, expr, body } => {
      let expr = count_allocs(expr);
      let body = count_allocs(body);
      3 + expr + body
    }
    Term::Lam { name, body } => {
      let body = count_allocs(body);
      2 + body
    }
    Term::App { func, argm } => {
      let func = count_allocs(func);
      let argm = count_allocs(argm);
      2 + func + argm
    }
    Term::Fun { name, args } => {
      let size = args.len() as u64;
      let mut count = 0;
      for (i, arg) in args.iter().enumerate() {
        count += count_allocs(arg);
      }
      size + count
    }
    Term::Ctr { name, args } => {
      let size = args.len() as u64;
      let mut count = 0;
      for (i, arg) in args.iter().enumerate() {
        count += count_allocs(arg);
      }
      size + count
    }
    Term::Num { numb } => {
      0
    }
    Term::Op2 { oper, val0, val1 } => {
      let val0 = count_allocs(val0);
      let val1 = count_allocs(val1);
      2 + val0 + val1
    }
  }
}

// Utils
// -----

// TODO: is this necessary? could it be genetic at least..?

pub fn init_name_map<A>() -> NameMap<A> {
  HashMap::with_hasher(BuildHasherDefault::default())
}

pub fn init_u128_map<A>() -> U128Map<A> {
  HashMap::with_hasher(BuildHasherDefault::default())
}

pub fn init_u120_map<A>() -> U120Map<A> {
  HashMap::with_hasher(BuildHasherDefault::default())
}

pub fn init_loc_map<A>() -> LocMap<A> {
  HashMap::with_hasher(BuildHasherDefault::default())
}

// Address
// -------

fn show_addr(addr: U120) -> String {
  let name = Name::try_from(addr);
  if let Ok(name) = name {
    if !name.is_empty() {
      return name.to_string();
    }
  }
  addr.to_hex_literal()
}


// Oper
// ====

impl TryFrom<u128> for Oper {
  type Error = String;
  fn try_from(value: u128) -> Result<Self, Self::Error> {
      match value {
        ADD => Ok(Oper::Add),
        SUB => Ok(Oper::Sub),
        MUL => Ok(Oper::Mul),
        DIV => Ok(Oper::Div),
        MOD => Ok(Oper::Mod),
        AND => Ok(Oper::And),
        OR  => Ok(Oper::Or),
        XOR => Ok(Oper::Xor),
        SHL => Ok(Oper::Shl),
        SHR => Ok(Oper::Shr),
        LTN => Ok(Oper::Ltn),
        LTE => Ok(Oper::Lte),
        EQL => Ok(Oper::Eql),
        GTE => Ok(Oper::Gte),
        GTN => Ok(Oper::Gtn),
        NEQ => Ok(Oper::Neq),
        _ => Err(format!("Invalid value for operation: {}", value))
      }
  }
}

// Term
// ====

impl Term {
  pub fn num(numb: U120) -> Self {
    Term::Num { numb }
  }

  pub fn var(name: Name) -> Self {
    Term::Var { name }
  }

  pub fn lam(name: Name, body: Box<Term>) -> Self {
    Term::Lam { name, body }
  }

  pub fn dup(nam0: Name, nam1: Name, expr: Box<Term>, body: Box<Term>) -> Self {
    Term::Dup { nam0, nam1, expr, body }
  }

  pub fn op2(oper: Oper, val0: Box<Term>, val1: Box<Term>) -> Self {
    Term::Op2 { oper, val0, val1 }
  }

  pub fn app(func: Box<Term>, argm: Box<Term>) -> Self {
    Term::App { func, argm }
  }

  pub fn fun(name: Name, args: Vec<Term>) -> Self {
    Term::Fun { name, args }
  }

  pub fn ctr(name: Name, args: Vec<Term>) -> Self {
    Term::Ctr { name, args }
  }
}

// Parser
// ======

impl ParseErr {
  fn new<C, E>(code: C, erro: E) -> Self
  where 
    C: Into<String>,
    E: Into<String>
  {
    ParseErr { code: code.into(), erro: erro.into() }
  }
}

// TODO: this should not use strings
pub fn split_names(name: Name) -> Vec<String> {
  name.to_string().split('.').map(|x| x.to_string()).collect()
}

pub fn get_namespace(name: Name) -> Option<Name> {
  let names = split_names(name);
  // TODO: pattern match
  // TODO: operate on number instead of string
  if names.len() > 1 {
    let name = Name::from_str_unsafe(&names[0 .. names.len() - 1].join("."));
    return Some(name);
  } else {
    return None;
  }
}

// Statements
// ----------

// Removes the signature from a statement
pub fn remove_sign(statement: &Statement) -> Statement {
  match statement {
    Statement::Fun { name, args, func, init, sign } => {
      Statement::Fun {
        name: *name,
        args: args.clone(),
        func: func.clone(),
        init: init.clone(),
        sign: None,
      }
    }
    Statement::Ctr { name, args, sign } => {
      Statement::Ctr {
        name: *name,
        args: args.clone(),
        sign: None,
      }
    }
    Statement::Run { expr, sign } => {
      Statement::Run {
        expr: expr.clone(),
        sign: None,
      }
    }
    Statement::Reg { name, ownr, sign } => {
      Statement::Reg {
        name: *name,
        ownr: *ownr,
        sign: None,
      }
    }
  }
}

pub fn set_sign(statement: &Statement, new_sign: crypto::Signature) -> Statement {
  match statement {
    Statement::Fun { name, args, func, init, sign } => {
      Statement::Fun {
        name: *name,
        args: args.clone(),
        func: func.clone(),
        init: init.clone(),
        sign: Some(new_sign),
      }
    }
    Statement::Ctr { name, args, sign } => {
      Statement::Ctr {
        name: *name,
        args: args.clone(),
        sign: Some(new_sign),
      }
    }
    Statement::Run { expr, sign } => {
      Statement::Run {
        expr: expr.clone(),
        sign: Some(new_sign),
      }
    }
    Statement::Reg { name, ownr, sign } => {
      Statement::Reg {
        name: *name,
        ownr: *ownr,
        sign: Some(new_sign),
      }
    }
  }
}

// StatementInfo
// =============

impl fmt::Display for StatementInfo {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      StatementInfo::Ctr { name, args } => write!(f, "[ctr] {}", name),
      StatementInfo::Fun { name, args } => write!(f, "[fun] {}", name),
      StatementInfo::Reg { name, .. } => write!(f, "[reg] {}", name),
      StatementInfo::Run { done_term, used_mana, size_diff, .. } =>
        write!(f, "[run] {} \x1b[2m[{} mana | {} size]\x1b[0m", view_term(&done_term), used_mana, size_diff)
    }
  }
}

// Rollback
// --------

fn absorb_u128(a: u128, b: u128, overwrite: bool) -> u128 {
  if b == U128_NONE { a } else if overwrite || a == U128_NONE { b } else { a }
}

fn absorb_u64(a: u64, b: u64, overwrite: bool) -> u64 {
  if b == U64_NONE { a } else if overwrite || a == U64_NONE { b } else { a }
}

impl Heap {
  fn write(&mut self, idx: Loc, val: RawCell) {
    return self.memo.write(idx, val);
  }
  fn read(&self, idx: Loc) -> RawCell {
    return self.memo.read(idx);
  }
  fn write_disk(&mut self, name: U120, val: RawCell) {
    return self.disk.write(name, val);
  }
  fn read_disk(&self, name: U120) -> Option<RawCell> {
    return self.disk.read(name);
  }
  fn write_file(&mut self, name: Name, fun: Arc<CompFunc>) {
    return self.file.write(name, fun);
  }
  fn read_file(&self, name: &Name) -> Option<Arc<CompFunc>> {
    return self.file.read(name);
  }
  fn write_arit(&mut self, name: Name, val: u64) {
    return self.arit.write(name, val);
  }
  fn read_arit(&self, name: &Name) -> Option<u64> {
    return self.arit.read(name);
  }
  fn write_ownr(&mut self, name: Name, val: U120) {
    return self.ownr.write(name, val);
  }
  fn read_ownr(&self, name: &Name) -> Option<U120> {
    return self.ownr.read(name);
  }
  fn write_indx(&mut self, name: Name, pos: u128) {
    return self.indx.write(name, pos);
  }
  fn read_indx(&self, name: &Name) -> Option<u128> {
    return self.indx.read(name);
  }
  fn write_stmt_hash(&mut self, pos: u128, hash: crypto::Hash) {
    return self.hash.write(pos, hash);
  }
  fn read_stmt_hash(&self, pos: &u128) -> Option<&crypto::Hash> {
    return self.hash.read(pos);
  }
  fn set_tick(&mut self, tick: u64) {
    self.tick = tick;
  }
  fn get_tick(&self) -> u64 {
    return self.tick;
  }
  fn set_time(&mut self, time: u128) {
    self.time = time;
  }
  fn get_time(&self) -> u128 {
    return self.time;
  }
  fn set_meta(&mut self, meta: u128) {
    self.meta = meta;
  }
  fn get_meta(&self) -> u128 {
    return self.meta;
  }
  fn set_hax0(&mut self, meta: u128) {
    self.hax0 = meta;
  }
  fn get_hax0(&self) -> u128 {
    return self.hax0;
  }
  fn set_hax1(&mut self, meta: u128) {
    self.hax1 = meta;
  }
  fn get_hax1(&self) -> u128 {
    return self.hax1;
  }
  fn set_funs(&mut self, funs: u64) {
    self.funs = funs;
  }
  fn get_funs(&self) -> u64 {
    return self.funs;
  }
  fn set_dups(&mut self, dups: u64) {
    self.dups = dups;
  }
  fn get_dups(&self) -> u64 {
    return self.dups;
  }
  fn set_rwts(&mut self, rwts: u64) {
    self.rwts = rwts;
  }
  fn get_rwts(&self) -> u64 {
    return self.rwts;
  }
  // NOTE: u64 for mana suffices
  fn set_mana(&mut self, mana: u64) { 
    self.mana = mana;
  }
  fn get_mana(&self) -> u64 {
    return self.mana;
  }
  fn set_size(&mut self, size: u64) {
    self.size = size;
  }
  fn get_size(&self) -> u64 {
    return self.size;
  }
  fn set_mcap(&mut self, mcap: u64) {
    self.mcap = mcap;
  }
  fn get_mcap(&self) -> u64 {
    return self.mcap;
  }
  fn set_next(&mut self, next: u64) {
    self.next = next;
  }
  fn get_next(&self) -> u64 {
    return self.next;
  }
  fn absorb(&mut self, other: &mut Self, overwrite: bool) {
    self.memo.absorb(&mut other.memo, overwrite);
    self.disk.absorb(&mut other.disk, overwrite);
    self.file.absorb(&mut other.file, overwrite);
    self.arit.absorb(&mut other.arit, overwrite);
    self.tick = absorb_u64(self.tick, other.tick, overwrite);
    self.time = absorb_u128(self.time, other.time, overwrite);
    self.meta = absorb_u128(self.meta, other.meta, overwrite);
    self.hax0 = absorb_u128(self.hax0, other.hax0, overwrite);
    self.hax1 = absorb_u128(self.hax1, other.hax1, overwrite);
    self.funs = absorb_u64(self.funs, other.funs, overwrite);
    self.dups = absorb_u64(self.dups, other.dups, overwrite);
    self.rwts = absorb_u64(self.rwts, other.rwts, overwrite);
    self.mana = absorb_u64(self.mana, other.mana, overwrite);
    self.size = absorb_u64(self.size, other.size, overwrite);
    self.mcap = absorb_u64(self.mcap, other.mcap, overwrite);
    self.next = absorb_u64(self.next, other.next, overwrite);
  }
  fn clear(&mut self) {
    self.uuid = fastrand::u128(..);
    self.memo.clear();
    self.disk.clear();
    self.file.clear();
    self.arit.clear();
    self.tick = U64_NONE;
    self.time = U128_NONE;
    self.meta = U128_NONE;
    self.hax0 = U128_NONE;
    self.hax1 = U128_NONE;
    self.funs = U64_NONE;
    self.dups = U64_NONE;
    self.rwts = U64_NONE;
    self.mana = U64_NONE;
    self.size = U64_NONE;
    self.mcap = U64_NONE;
    self.next = U64_NONE;
  }
  pub fn serialize(self: &Heap, path: &PathBuf, append: bool) -> std::io::Result<()> {
    fn open_writer(heap: &Heap, path: &PathBuf, buffer_name: &str, append: bool) -> std::io::Result<File> {
      let file_path = Heap::buffer_file_path(heap.uuid, buffer_name, path);
      std::fs::OpenOptions::new()
        .write(true)
        .append(append)
        .create(true)
        .open(file_path)
    }
    self.memo.nodes.disk_serialize(&mut open_writer(self, path, "memo", append)?)?;
    self.disk.links.disk_serialize(&mut open_writer(self, path, "disk", append)?)?;
    self.file.funcs.disk_serialize(&mut open_writer(self, path, "file", append)?)?;
    self.arit.arits.disk_serialize(&mut open_writer(self, path, "arit", append)?)?;
    self.indx.indxs.disk_serialize(&mut open_writer(self, path, "indx", append)?)?;
    self.hash.stmt_hashes.disk_serialize(&mut open_writer(self, path, "stmt_hashes", append)?)?;
    self.ownr.ownrs.disk_serialize(&mut open_writer(self, path, "ownr", append)?)?;
    let mut stat = open_writer(self, path, "stat", false)?;
    self.tick.disk_serialize(&mut stat)?;
    self.time.disk_serialize(&mut stat)?;
    self.meta.disk_serialize(&mut stat)?;
    self.hax0.disk_serialize(&mut stat)?;
    self.hax1.disk_serialize(&mut stat)?;
    self.funs.disk_serialize(&mut stat)?;
    self.dups.disk_serialize(&mut stat)?;
    self.rwts.disk_serialize(&mut stat)?;
    self.mana.disk_serialize(&mut stat)?;
    self.size.disk_serialize(&mut stat)?;
    self.mcap.disk_serialize(&mut stat)?;
    self.next.disk_serialize(&mut stat)?;
    Ok(())
  }
  pub fn deserialize(uuid: u128, path: &PathBuf) -> std::io::Result<Heap> {
    fn open_reader(uuid: u128, path: &PathBuf, buffer_name: &str) -> std::io::Result<File> {
      let file_path = Heap::buffer_file_path(uuid, buffer_name, path);
      std::fs::OpenOptions::new()
        .read(true)
        .open(file_path)
    }
    fn read_hash_map_from_file<K: DiskSer + Eq + std::hash::Hash + crate::NoHashHasher::IsEnabled, V: DiskSer>
      (uuid: u128, path: &PathBuf, buffer_name: &str) -> std::io::Result<HashMap<K, V, std::hash::BuildHasherDefault<NoHashHasher<K>>>> {
      HashMap::disk_deserialize(&mut open_reader(uuid, path, buffer_name)?)?
        .ok_or_else(|| std::io::Error::from(std::io::ErrorKind::UnexpectedEof))
    }
    fn read_num<T: DiskSer>(file: &mut File) -> std::io::Result<T>{
      T::disk_deserialize(file)?.ok_or_else(|| std::io::Error::from(std::io::ErrorKind::UnexpectedEof))
    }
    let memo = Nodes { nodes: read_hash_map_from_file(uuid, path, "memo")? };
    let disk = Store { links: read_hash_map_from_file(uuid, path, "disk")? };
    let file = Funcs { funcs: read_hash_map_from_file(uuid, path, "file")? };
    let arit = Arits { arits: read_hash_map_from_file(uuid, path, "arit")? };
    let indx = Indxs { indxs: read_hash_map_from_file(uuid, path, "indx")? };
    let hash = Hashs { stmt_hashes: read_hash_map_from_file(uuid, path, "stmt_hashes")? };    
    let ownr = Ownrs { ownrs: read_hash_map_from_file(uuid, path, "ownr")? };
    let mut stat = open_reader(uuid, path, "stat")?;
    let tick = read_num(&mut stat)?;
    let time = read_num(&mut stat)?;
    let meta = read_num(&mut stat)?;
    let hax0 = read_num(&mut stat)?;
    let hax1 = read_num(&mut stat)?;
    let funs = read_num(&mut stat)?;
    let dups = read_num(&mut stat)?;
    let rwts = read_num(&mut stat)?;
    let mana = read_num(&mut stat)?;
    let size = read_num(&mut stat)?;
    let mcap = read_num(&mut stat)?;
    let next = read_num(&mut stat)?;
    Ok( Heap { uuid, memo, disk, file, arit, indx, hash, ownr, tick, time, meta, hax0, hax1, funs, dups, rwts,  mana, size, mcap, next })
  }

  fn buffer_file_path(uuid: u128, buffer_name: &str, path: &PathBuf) -> PathBuf {
    path.join(format!("{:0>32x}.{}.bin", uuid, buffer_name))
  }
  fn delete_buffer(&self, uuid: u128, buffer_name: &str, path: &PathBuf) -> std::io::Result<()> {
    std::fs::remove_file(Heap::buffer_file_path(uuid, buffer_name, path))
  }
  fn delete_buffers(&mut self, path: &PathBuf) -> std::io::Result<()> {
    self.delete_buffer(self.uuid, "memo", path)?;
    self.delete_buffer(self.uuid, "disk", path)?;
    self.delete_buffer(self.uuid, "file", path)?;
    self.delete_buffer(self.uuid, "arit", path)?;
    self.delete_buffer(self.uuid, "indx", path)?;
    self.delete_buffer(self.uuid, "ownr", path)?;
    self.delete_buffer(self.uuid, "stat", path)?;
    return Ok(());
  }
  pub fn get_fn_count(&self) -> u64 {
    return self.file.funcs.len() as u64
  }
  pub fn get_ns_count(&self) -> u64 {
    return self.ownr.ownrs.len() as u64
  }
  pub fn get_ct_count(&self) -> u64 {
    return self.arit.arits.len() as u64 - self.get_fn_count()
  }
}

pub fn init_heap() -> Heap {
  Heap {
    uuid: fastrand::u128(..),
    memo: Nodes { nodes: init_loc_map() },
    disk: Store { links: init_u120_map() },
    file: Funcs { funcs: init_name_map() },
    arit: Arits { arits: init_name_map() },
    ownr: Ownrs { ownrs: init_name_map() },
    indx: Indxs { indxs: init_name_map() },
    hash: Hashs { stmt_hashes: init_u128_map() },
    tick: U64_NONE,
    time: U128_NONE,
    meta: U128_NONE,
    hax0: U128_NONE,
    hax1: U128_NONE,
    funs: U64_NONE,
    dups: U64_NONE,
    rwts: U64_NONE,
    mana: U64_NONE,
    size: U64_NONE,
    mcap: U64_NONE,
    next: U64_NONE,
  }
}

impl Nodes {
  fn write(&mut self, idx: Loc, val: RawCell) {
    self.nodes.insert(idx, val);
  }
  fn read(&self, idx: Loc) -> RawCell {
    return self.nodes.get(&idx).map(|x| *x).unwrap_or(RawCell(U128_NONE));
  }
  fn clear(&mut self) {
    self.nodes.clear();
  }
  fn absorb(&mut self, other: &mut Self, overwrite: bool) {
    for (idx, ownr) in other.nodes.drain() {
      if overwrite || !self.nodes.contains_key(&idx) {
        self.nodes.insert(idx, ownr);
      }
    }
    other.clear();
  }
}

impl Store {
  fn write(&mut self, fid: U120, val: RawCell) {
    self.links.insert(fid, val);
  }
  fn read(&self, fid: U120) -> Option<RawCell> {
    self.links.get(&fid).map(|x| *x)
  }
  fn clear(&mut self) {
    self.links.clear();
  }
  fn absorb(&mut self, other: &mut Self, overwrite: bool) {
    for (fid, func) in other.links.drain() {
      if overwrite || !self.links.contains_key(&fid) {
        self.write(fid, func);
      }
    }
  }
}

impl Funcs {
  fn write(&mut self, name: Name, val: Arc<CompFunc>) {
    self.funcs.entry(name).or_insert(val);
  }
  fn read(&self, name: &Name) -> Option<Arc<CompFunc>> {
    return self.funcs.get(name).map(|x| x.clone());
  }
  fn clear(&mut self) {
    self.funcs.clear();
  }
  fn absorb(&mut self, other: &mut Self, overwrite: bool) {
    for (fid, func) in other.funcs.drain() {
      if overwrite || !self.funcs.contains_key(&fid) {
        self.write(fid, func.clone());
      }
    }
  }
}

impl Arits {
  fn write(&mut self, name: Name, val: u64) {
    self.arits.entry(name).or_insert(val);
  }
  fn read(&self, name: &Name) -> Option<u64> {
    return self.arits.get(name).map(|x| *x);
  }
  fn clear(&mut self) {
    self.arits.clear();
  }
  fn absorb(&mut self, other: &mut Self, overwrite: bool) {
    for (fid, arit) in other.arits.drain() {
      if overwrite || !self.arits.contains_key(&fid) {
        self.arits.insert(fid, arit);
      }
    }
  }
}

impl Ownrs {
  fn write(&mut self, name: Name, val: U120) {
    self.ownrs.entry(name).or_insert(val);
  }
  fn read(&self, name: &Name) -> Option<U120> {
    return self.ownrs.get(name).map(|x| *x);
  }
  fn clear(&mut self) {
    self.ownrs.clear();
  }
  fn absorb(&mut self, other: &mut Self, overwrite: bool) {
    for (fid, ownr) in other.ownrs.drain() {
      if overwrite || !self.ownrs.contains_key(&fid) {
        self.ownrs.insert(fid, ownr);
      }
    }
  }
}

impl Indxs {
  fn write(&mut self, name: Name, pos: u128) {
    self.indxs.insert(name, pos);
  }
  fn read(&self, name: &Name) -> Option<u128> {
    return self.indxs.get(name).map(|x| *x);
  }
  fn clear(&mut self) {
    self.indxs.clear();
  }
  fn absorb(&mut self, other: &mut Self, overwrite: bool) {
    for (name, pos) in other.indxs.drain() {
      if overwrite || !self.indxs.contains_key(&name) {
        self.indxs.insert(name, pos);
      }
    }
  }
}

impl Hashs {
  fn write(&mut self, pos: u128, hash: crypto::Hash) {
    self.stmt_hashes.insert(pos, hash);
  }
  fn read(&self, pos: &u128) -> Option<&crypto::Hash> {
    return self.stmt_hashes.get(pos);
  }
  fn clear(&mut self) {
    self.stmt_hashes.clear();
  }
  fn absorb(&mut self, other: &mut Self, overwrite: bool) {
    for (indx, hash) in other.stmt_hashes.drain() {
      if overwrite || !self.stmt_hashes.contains_key(&indx) {
        self.stmt_hashes.insert(indx, hash);
      }
    }
  }
}


pub fn init_runtime(heaps_path: PathBuf, init_stmts: &[Statement]) -> Runtime {
  // Default runtime store path
  std::fs::create_dir_all(&heaps_path).unwrap(); // TODO: handle unwrap
  let mut heap = Vec::new();
  for i in 0 .. MAX_HEAPS {
    heap.push(init_heap());
  }
  let mut rt = Runtime {
    heap,
    draw: 0,
    curr: 1,
    nuls: (2 .. MAX_HEAPS).collect(),
    back: Arc::new(Rollback::Nil),
    path: heaps_path,
  };

  rt.run_statements(init_stmts, true, false);
  rt.commit();

  rt
}

impl Runtime {

  // API
  // ---

  pub fn clear(&mut self) {
    self.clear_heap(self.draw);
  }

  pub fn define_function(&mut self, name: Name, func: CompFunc, stmt_index: Option<usize>, stmt_hash: crypto::Hash) {
    self.get_heap_mut(self.draw).write_arit(name, func.arity);
    self.get_heap_mut(self.draw).write_file(name, Arc::new(func));
    self.save_stmt_name(name, stmt_index, stmt_hash);
  }

  pub fn define_constructor(&mut self, name: Name, arity: u64, stmt_index: Option<usize>, stmt_hash: crypto::Hash) {
    self.get_heap_mut(self.draw).write_arit(name, arity);
    self.save_stmt_name(name, stmt_index, stmt_hash);
  }

 
  pub fn define_register(&mut self, name: Name, stmt_index: Option<usize>, stmt_hash: crypto::Hash) {
    self.save_stmt_name(name, stmt_index, stmt_hash);
  }

  pub fn save_stmt_name(&mut self, name: Name, stmt_index: Option<usize>, stmt_hash: crypto::Hash) {
    if let Some(idx) = stmt_index {
      let tick = self.get_tick() as u128;
      let pos = tick.wrapping_shl(60) | (idx as u128); //TODO: refactor to use less bits
      self.get_heap_mut(self.draw).write_indx(name, pos);
      self.get_heap_mut(self.draw).write_stmt_hash(pos, stmt_hash);
    }
  }

  pub fn create_term(&mut self, term: &Term, loc: Loc, vars_data: &mut NameMap<Vec<RawCell>>) -> Result<RawCell, RuntimeError> {
    return create_term(self, term, loc, vars_data);
  }

  pub fn alloc_term(&mut self, term: &Term) -> Result<Loc, RuntimeError> {
    let loc = alloc(self, 1);
    let ptr = create_term(self, term, loc, &mut init_name_map())?;
    self.write(loc, ptr);
    Ok(loc)
  }

  pub fn collect(&mut self, term: RawCell) {
    collect(self, term)
  }

  pub fn collect_at(&mut self, loc: Loc) {
    collect(self, self.read(loc))
  }

  //fn run_io_term(&mut self, subject: u128, caller: u128, term: &Term) -> Option<RawCell> {
    //let main = self.alloc_term(term);
    //let done = self.run_io(subject, caller, main);
    //return done;
  //}

  //fn run_io_from_code(&mut self, code: &str) -> Option<RawCell> {
    //return self.run_io_term(0, 0, &read_term(code).1);
  //}

  pub fn run_statements(&mut self, statements: &[Statement], silent: bool, debug: bool) -> Vec<StatementResult> {
    statements.iter().enumerate().map(
      |(i, s)| {
        let res = self.run_statement(s, silent, debug, Some(i));
        if let Ok(..) = res {
          self.draw();
        }
        res
      }
    ).collect()
  }

  pub fn run_statements_from_code(&mut self, code: &str, silent: bool, debug: bool) -> Vec<StatementResult> {
    let stataments = read_statements(code);
    match stataments {
      Ok((.., statements)) => self.run_statements(&statements, silent, debug),
      Err(ParseErr { erro , .. }) => {
        return vec![Err(StatementErr { err: erro })];
      }
    }
  }

  pub fn test_statements(&mut self, statements: &[Statement]) -> Vec<StatementResult> {
    let mut results = vec![];
    for (idx, statement) in statements.iter().enumerate() {
      let res = self.run_statement(statement, true, false, Some(idx));
      match res {
        Ok(..) => {
          results.push(res);
        }
        Err(..) => {
          results.push(res);
          break
        }
      }
    }
    self.undo();
    results
  }

  pub fn test_statements_from_code(&mut self, code: &str) -> Vec<StatementResult> {
    let stataments = read_statements(code);
    match stataments {
      Ok((.., statements)) => self.test_statements(&statements),
      Err(ParseErr { erro , .. }) => {
        return vec![Err(StatementErr { err: erro })];
      }
    }
  }

  pub fn compute_at(&mut self, loc: Loc, mana: u64) -> Result<RawCell, RuntimeError> {
    compute_at(self, loc, mana)
  }

  pub fn compute(&mut self, lnk: RawCell, mana: u64) -> Result<RawCell, RuntimeError> {
    let host = alloc_lnk(self, lnk);
    let done = self.compute_at(host, mana)?;
    clear(self, host, 1);
    return Ok(done);
  }

  pub fn show_term(&self, lnk: RawCell) -> String {
    return show_term(self, lnk, None);
  }

  pub fn show_term_at(&self, loc: Loc) -> String {
    return show_term(self, self.read(loc), None);
  }

  // Heaps
  // -----

  pub fn get_heap(&self, index: u64) -> &Heap {
    return &self.heap[index as usize];
  }

  pub fn get_heap_mut(&mut self, index: u64) -> &mut Heap {
    return &mut self.heap[index as usize];
  }

  // Copies the contents of the absorbed heap into the absorber heap
  fn absorb_heap(&mut self, absorber: u64, absorbed: u64, overwrite: bool) {
    // FIXME: can we satisfy the borrow checker without using unsafe pointers?
    unsafe {
      let a_arr = &mut self.heap as *mut Vec<Heap>;
      let a_ref = &mut *(&mut (*a_arr)[absorber as usize] as *mut Heap);
      let b_ref = &mut *(&mut (*a_arr)[absorbed as usize] as *mut Heap);
      a_ref.absorb(b_ref, overwrite);
    }
  }

  fn clear_heap(&mut self, index: u64) {
    self.heap[index as usize].clear();
  }

  fn undo(&mut self) {
    self.clear_heap(self.draw);
  }

  fn draw(&mut self) {
    self.absorb_heap(self.curr, self.draw, true);
    self.clear_heap(self.draw);
  }

  // IO
  // --

  pub fn run_io(&mut self, subject: U120, caller: U120, host: Loc, mana: u64) -> Result<RawCell, RuntimeError> {
    // eprintln!("-- {}", show_term(self, host, None));
    let term = reduce(self, host, mana)?;
    // eprintln!("-- {}", show_term(self, term, None));
    match get_tag(term) {
      CTR => {
        let ext = get_ext(term);
        match ext {
          IO_DONE => {
            let retr = ask_arg(self, term, 0);
            clear(self, host, 1);
            clear(self, get_loc(term, 0), 1);
            return Ok(retr);
          }
          IO_TAKE => {
            //println!("- IO_TAKE subject is {} {}", u128_to_name(subject), subject);
            let cont = ask_arg(self, term, 0);
            if let Some(state) = self.read_disk(subject) {
              if state != RawCell(U128_NONE) {
                self.write_disk(subject, RawCell(U128_NONE));
                let cont = alloc_app(self, cont, state);
                let done = self.run_io(subject, subject, cont, mana);
                clear(self, host, 1);
                clear(self, get_loc(term, 0), 1);
                return done;
              } else {
                return Err(RuntimeError::EffectFailure(
                  EffectFailure::NoSuchState { state: subject },
                ));
              }
            }
            return Err(RuntimeError::EffectFailure(
              EffectFailure::NoSuchState { state: subject },
            ));
          }
          IO_SAVE => {
            //println!("- IO_SAVE subject is {} {}", u128_to_name(subject), subject);
            let expr = ask_arg(self, term, 0);
            let save = self.compute(expr, mana)?;
            self.write_disk(subject, save);
            let cont = ask_arg(self, term, 1);
            let cont = alloc_app(self, cont, Num(0));
            let done = self.run_io(subject, subject, cont, mana);
            clear(self, host, 1);
            clear(self, get_loc(term, 0), 2);
            return done;
          }
          IO_CALL => {
            let fnid = ask_arg(self, term, 0);
            let argm = ask_arg(self, term, 1);
            let cont = ask_arg(self, term, 2);
            let fnid = self.check_num(fnid, mana)?;
            
            let arg_name = Name::new(get_ext(argm)).ok_or_else(|| RuntimeError::NameTooBig { numb: *argm })?;
            let arg_arit = self
              .get_arity(&arg_name)
              .ok_or_else(|| RuntimeError::CtrOrFunNotDefined { name: arg_name })?;
            // Checks if the argument is a constructor with numeric fields. This is needed since
            // Kindelia's language is untyped, yet contracts can call each other freely. That would
            // allow a contract to pass an argument with an unexpected type to another, corrupting
            // its state. To avoid that, we only allow contracts to communicate by passing flat
            // constructors of numbers, like `{Send 'Alice' #123}` or `{Inc}`.
            for i in 0 .. arg_arit {
              let argm = reduce(self, get_loc(argm, 0), mana)?;
              if get_tag(argm) != NUM {
                let f = EffectFailure::InvalidCallArg { caller: subject, callee: fnid, arg: argm };
                return Err(RuntimeError::EffectFailure(f));
              }
            }
            // Calls called function IO, changing the subject
            // TODO: this should not alloc a Fun as it's limited to 72-bit names
            let name = Name::new(*fnid).ok_or_else(|| RuntimeError::NameTooBig { numb: *fnid })?;
            let ioxp = alloc_fun(self, name, &[argm]);
            let retr = self.run_io(fnid, subject, ioxp, mana)?;
            // Calls the continuation with the value returned
            let cont = alloc_app(self, cont, retr);
            let done = self.run_io(subject, caller, cont, mana);
            // Clears memory
            clear(self, host, 1);
            //clear(self, get_loc(argm, 0), arit);
            clear(self, get_loc(term, 0), 3);
            return done;
          }
          IO_GIDX => {
            let fnid = ask_arg(self, term, 0);
            let cont = ask_arg(self, term, 1);
            let name = self.check_name(fnid, mana)?;
            let indx = self.get_index(&name).ok_or_else(|| RuntimeError::CtrOrFunNotDefined { name })?;
            let cont = alloc_app(self, cont, Num(indx));
            let done = self.run_io(subject, caller, cont, mana);
            clear(self, host, 1);
            clear(self, get_loc(term, 0), 2);
            return done;
          }
          IO_STH0 => {
            let indx = ask_arg(self, term, 0);
            let cont = ask_arg(self, term, 1);
            let indx = self.check_num(indx, mana)?;
            let stmt_hash = self.get_sth0(*indx).ok_or_else(|| RuntimeError::StmtDoesntExist { stmt_index: *indx })?;
            let cont = alloc_app(self, cont, Num(stmt_hash));
            let done = self.run_io(subject, caller, cont, mana);
            clear(self, host, 1);
            clear(self, get_loc(term, 0), 2);
            return done;
          }
          IO_STH1 => {
            let indx = ask_arg(self, term, 0);
            let cont = ask_arg(self, term, 1);
            let indx = self.check_num(indx, mana)?;
            let stmt_hash = self.get_sth1(*indx).ok_or_else(|| RuntimeError::StmtDoesntExist { stmt_index: *indx })?;
            let cont = alloc_app(self, cont, Num(stmt_hash));
            let done = self.run_io(subject, caller, cont, mana);
            clear(self, host, 1);
            clear(self, get_loc(term, 0), 2);
            return done;
          }
          IO_SUBJ => {
            let cont = ask_arg(self, term, 0);
            let cont = alloc_app(self, cont, Num(*subject));
            let done = self.run_io(subject, caller, cont, mana);
            clear(self, host, 1);
            clear(self, get_loc(term, 0), 1);
            return done;
          }
          IO_FROM => {
            let cont = ask_arg(self, term, 0);
            let cont = alloc_app(self, cont, Num(*caller));
            let done = self.run_io(subject, caller, cont, mana);
            clear(self, host, 1);
            clear(self, get_loc(term, 0), 1);
            return done;
          }
          IO_TICK => {
            let cont = ask_arg(self, term, 0);
            let cont = alloc_app(self, cont, Num(self.get_tick() as u128));
            let done = self.run_io(subject, subject, cont, mana);
            clear(self, host, 1);
            clear(self, get_loc(term, 0), 1);
            return done;
          }
          IO_TIME => {
            let cont = ask_arg(self, term, 0);
            let cont = alloc_app(self, cont, Num(self.get_time()));
            let done = self.run_io(subject, subject, cont, mana);
            clear(self, host, 1);
            clear(self, get_loc(term, 0), 1);
            return done;
          }
          IO_META => {
            let cont = ask_arg(self, term, 0);
            let cont = alloc_app(self, cont, Num(self.get_meta()));
            let done = self.run_io(subject, subject, cont, mana);
            clear(self, host, 1);
            clear(self, get_loc(term, 0), 1);
            return done;
          }
          IO_HAX0 => {
            let cont = ask_arg(self, term, 0);
            let cont = alloc_app(self, cont, Num(self.get_hax0()));
            let done = self.run_io(subject, subject, cont, mana);
            clear(self, host, 1);
            clear(self, get_loc(term, 0), 1);
            return done;
          }
          IO_HAX1 => {
            let cont = ask_arg(self, term, 0);
            let cont = alloc_app(self, cont, Num(self.get_hax1()));
            let done = self.run_io(subject, subject, cont, mana);
            clear(self, host, 1);
            clear(self, get_loc(term, 0), 1);
            return done;
          }
          _ => {
            let name = Name::new_unsafe(ext);
            return Err(RuntimeError::EffectFailure(
              EffectFailure::InvalidIOCtr { name },
            ));
          }
        }
      }
      _ => {
        return Err(RuntimeError::EffectFailure(
          EffectFailure::InvalidIONonCtr { ptr: term },
        ));
      }
    }
  }

  /// Gets the subject of a signature.
  ///
  /// - If there is no signature, returns `0`.
  /// - If there is a signature, but subject cannot be retrieved correctly,
  ///   returns `1`.
  /// - Else, returns the subject.
  pub fn get_subject(&mut self, sign: &Option<crypto::Signature>, hash: &crypto::Hash) -> U120 {
    match sign {
      None       => U120::from_u128_unchecked(0),
      Some(sign) => sign.signer_name(hash).map(|x| U120::from_u128_unchecked(*x)).unwrap_or_else(|| U120::from_u128_unchecked(1)),
    }
  }

  pub fn check_num(&mut self, ptr: RawCell, mana: u64) -> Result<U120, RuntimeError> {
    let num = self.compute(ptr, mana)?;
    match get_tag(num) {
      NUM => Ok(get_num(num)),
      _ => Err(RuntimeError::TermIsInvalidNumber { term: num })
    }
  }

  pub fn check_name(&mut self, ptr: RawCell, mana: u64) -> Result<Name, RuntimeError> {
    let num = self.check_num(ptr, mana)?;
    match Name::new(*num) {
      None => Err(RuntimeError::NameTooBig { numb: *num }),
      Some(name) => Ok(name),
    }
  }

  // Can this subject deploy this name?
  pub fn can_deploy(&mut self, subj: U120, name: &Name) -> bool {
    if name.is_empty() {
      // No one can deploy the empty name
      false
    } else {
      match get_namespace(*name) {
        None => {
          // Anyone can deploy a namespace-less name
          true
        }
        Some(namespace) => {
          // Only owner can deploy on its namespace
          Some(subj) == self.get_owner(&namespace)
        }
      }
    }
  }

  // Can this subject register this namespace?
  pub fn can_register(&mut self, subj: U120, name: &Name) -> bool {
    if name.is_empty() {
      // Anyone can register the empty namespace (should happen on Genesis Block)
      true
    } else {
      // Only namespace owner can register a sub-namespace
      let namespace = get_namespace(*name).unwrap_or(Name::new_unsafe(0));
      Some(subj) == self.get_owner(&namespace)
    }
  }

  /// Run statement in the `draw` heap.
  ///
  /// It doesn't alter `curr` heap.
  #[allow(clippy::useless_format)]
  pub fn run_statement(&mut self, statement: &Statement, silent: bool, sudo: bool, stmt_index: Option<usize>) -> StatementResult {
    fn error(rt: &mut Runtime, tag: &str, err: String) -> StatementResult {
      rt.undo(); // TODO: don't undo inside here. too much coupling
      println!("{:03$} [{}] ERROR: {}", rt.get_tick(), tag, err, 10);
      return Err(StatementErr { err });
    }
    fn handle_runtime_err<T>(rt: &mut Runtime, tag: &str, val: Result<T, RuntimeError>) -> Result<T, StatementErr> {
      val.map_err(|err| {
        let err = show_runtime_error(err);
        rt.undo(); // TODO: don't undo inside here. too much coupling
        println!("{:03$} [{}] ERROR: {}", rt.get_tick(), tag, err, 10);
        StatementErr { err }
      })
    }
    let hash = hash_statement(statement);
    let res = match statement {
      Statement::Fun { name, args, func, init, sign } => {
        if self.exists(name) {
          return error(self, "fun", format!("Can't redefine '{}'.", name));
        }
        let subj = self.get_subject(&sign, &hash);
        if !(self.can_deploy(subj, name) || sudo) {
          return error(self, "fun", format!("Subject '#x{:0>30x}' not allowed to deploy '{}'.", *subj, name));
        }
        handle_runtime_err(self, "fun", check_func(&func))?;
        let func = compile_func(func, true);
        let func = handle_runtime_err(self, "fun", func)?;
        let name = *name;
        self.set_arity(name, args.len() as u64);
        self.define_function(name, func, stmt_index, hash);
        if let Some(state) = init {
          let state = self.create_term(state, Loc(0), &mut init_name_map());
          let state = handle_runtime_err(self, "fun", state)?;
          let state = self.compute(state, self.get_mana_limit());
          let state = handle_runtime_err(self, "fun", state)?;
          self.write_disk(U120::from(name), state);
        }
        let args = args.iter().map(|x| *x).collect::<Vec<_>>();
        StatementInfo::Fun { name, args }
      }
      Statement::Ctr { name, args, sign } => {
        if self.exists(name) {
          return error(self, "ctr", format!("Can't redefine '{}'.", name));
        }
        let subj = self.get_subject(&sign, &hash);
        if !(self.can_deploy(subj, name) || sudo) {
          return error(self, "ctr", format!("Subject '#x{:0>30x}' not allowed to deploy '{}'.", *subj, name));
        }
        if args.len() > 16 {
          return error(self, "ctr", format!("Can't define contructor with arity larger than 16."));
        }
        let name = *name;
        self.define_constructor(name, args.len() as u64, stmt_index, hash);
        let args = args.iter().map(|x| *x).collect::<Vec<_>>();
        StatementInfo::Ctr { name, args }
      }
      Statement::Run { expr, sign } => {
        let mana_ini = self.get_mana();
        let mana_lim = if !sudo { self.get_mana_limit() } else { u64::MAX }; // ugly
        let size_ini = self.get_size();
        let size_lim = self.get_size_limit();
        handle_runtime_err(self, "run", check_term(&expr))?; 
        let subj = self.get_subject(&sign, &hash);
        let host = self.alloc_term(expr);
        let host = handle_runtime_err(self, "run", host)?;
        let done = self.run_io(subj, U120::from_u128_unchecked(0), host, mana_lim);
        if let Err(err) = done {
          return error(self, "run", show_runtime_error(err));
        }
        let done = done.unwrap();
        let done = self.compute(done, mana_lim);
        if let Err(err) = done {
          return error(self, "run", show_runtime_error(err));
        }
        let done = done.unwrap();
        // TODO:
        // The term return by Done is only read and stored in debug mode for
        // testing purpouses. In the future, the Done return value will be
        // limited to `Term::Num`s and the U120s will be stored as part of the
        // protocol. Also, a `Log` primitive should be added.
        let done_term =
          // if debug {
          if let Some(term) = readback_term(self, done, Some(1 << 16)) {
            term
           } else {
            Term::num(U120::ZERO)
          };
        self.collect(done);
        let size_end = self.get_size() as u64;
        let mana_dif = self.get_mana() - mana_ini;
        let size_dif = (size_end as i64) - (size_ini as i64);
        if size_end > size_lim && !sudo {
          return error(self, "run", format!("Not enough space."));
        }
        StatementInfo::Run {
          done_term,
          used_mana: mana_dif,
          size_diff: size_dif,
          end_size: size_end, // TODO: rename to done_size for consistency?
        }
        // TODO: save run to statement array?
      }
      Statement::Reg { name, ownr, sign } => {
        let ownr = *ownr;

        if self.exists(name) {
          return error(self, "run", format!("Can't redefine '{}'.", name));
        }
        let subj = self.get_subject(sign, &hash);
        if !(self.can_register(subj, name) || sudo) {
          return error(self, "run", format!("Subject '{}' not allowed to register '{}'.", subj, name));
        }
        let name = *name;
        self.define_register(name, stmt_index, hash);
        self.set_owner(name, ownr);
        StatementInfo::Reg { name, ownr }
      }
    };
    if !silent {
      println!("{:02$} {}", self.get_tick(), res, 10);
    }
    Ok(res)
  }

  // Maximum mana = 42m * block_number
  pub fn get_mana_limit(&self) -> u64 {
    (self.get_tick() + 1) * BLOCK_MANA_LIMIT
  }

  // Maximum size = 2048 * block_number
  pub fn get_size_limit(&self) -> u64 {
    (self.get_tick() as u64 + 1) * (BLOCK_BITS_LIMIT / 128)
  }

  // Rollback
  // --------

  // Returns a clone of a reference to the current rollback state.
  pub fn get_back(&self) -> Arc<Rollback> {
    return self.back.clone();
  }

  /// Advances the heap time counter.
  pub fn open(&mut self) {
    self.set_tick(self.get_tick() + 1);
  }

  /// Saves past states for rollback.
  pub fn commit(&mut self) {
    self.draw();
    self.snapshot();
  }

  pub fn snapshot(&mut self) {
    //println!("tick self.curr={}", self.curr);
    let (included, absorber, deleted, rollback) = rollback_push(self.curr, self.back.clone(), 0);
    // println!("- tick={} self.curr={}, included={:?} absorber={:?} deleted={:?} rollback={}", self.get_tick(), self.curr, included, absorber, deleted, view_rollback(&self.back));
    self.back = rollback;
    // println!(" - back {}", view_rollback(&self.back));
    if included {
      self.save_state_metadata().expect("Error saving state metadata.");
      let path = &self.get_dir_path();
      // let _ = &self.heap[self.curr as usize].serialize(path, true).expect("Error saving buffers."); // heap persistence disabled
      if let Some(deleted) = deleted {
        if let Some(absorber) = absorber {
          self.absorb_heap(absorber, deleted, false);
          // let _ = self.heap[absorber as usize].serialize(path, false).expect("Couldn't append buffers."); // heap persistence disabled
        }
        // self.heap[deleted as usize].delete_buffers(path).expect("Couldn't delete buffers."); // heap persistence disabled
        self.clear_heap(deleted);
        self.curr = deleted;
      } else if let Some(empty) = self.nuls.pop() {
        self.curr = empty;
      } else {
        //println!("- {} {} {:?} {}", self.draw, self.curr, self.nuls, view_rollback(&self.back));
        panic!("Not enough heaps.");
      }
    }
  }

  // Rolls back to the earliest state before or equal `tick`
  pub fn rollback(&mut self, tick: u64) {
    // If target tick is older than current tick
    if tick < self.get_tick() {
      eprintln!("- rolling back from {} to {}", self.get_tick(), tick);
      self.clear_heap(self.curr);
      self.nuls.push(self.curr);
      let mut cuts = 0;
      let path = self.get_dir_path();
      // Removes heaps until the runtime's tick is larger than, or equal to, the target tick
      while tick < self.get_tick() {
        if let Rollback::Cons { keep, life, head, tail } = &*self.back.clone() {
          // self.heap[*head as usize].delete_buffers(&path).expect("Couldn't delete buffers."); // heap persistence disabled
          self.clear_heap(*head);
          self.nuls.push(*head);
          self.back = tail.clone();
          cuts += 1 + life;
        }
      }
      if let Rollback::Cons { keep, life, head, tail } = &*self.back {
        self.back = Arc::new(Rollback::Cons { keep: 0, life: *life + cuts, head: *head, tail: tail.clone() });
      }
      self.curr = self.nuls.pop().expect("No heap available!");
    }
    // println!("- rolled back to {}", self.get_tick());
  }

  // Persistence
  // -----------

  pub fn get_dir_path(&self) -> PathBuf {
    return self.path.clone();
  }

  // Persists the current state. Since heaps are automatically saved to disk, function only saves
  // their uuids. Note that this will NOT save the current heap, nor anything after the last heap
  // included on the Rollback list. In other words, it forgets up to ~16 recent blocks. This
  // function is used to avoid re-processing the entire block history on node startup.
  pub fn save_state_metadata(&self) -> std::io::Result<()> {
    fn build_persistence_buffers(rt: &Runtime, rollback: &Rollback, keeps: &mut Vec<u128>, lifes: &mut Vec<u128>, uuids: &mut Vec<u128>) {
      match rollback {
        Rollback::Cons { keep, life, head, tail } => {
          keeps.push(*keep as u128);
          lifes.push(*life as u128);
          uuids.push(rt.heap[*head as usize].uuid);
          build_persistence_buffers(rt, tail, keeps, lifes, uuids);
        }
        Rollback::Nil => {}
      }
    }
    let mut keeps : Vec<u128> = vec![];
    let mut lifes : Vec<u128> = vec![];
    let mut uuids : Vec<u128> = vec![];
    build_persistence_buffers(self, &self.back,  &mut keeps, &mut lifes, &mut uuids);
    std::fs::write(self.path.join("_keeps_"), &util::u128s_to_u8s(&keeps))?;
    std::fs::write(self.path.join("_lifes_"), &util::u128s_to_u8s(&lifes))?;
    std::fs::write(self.path.join("_uuids_"), &util::u128s_to_u8s(&uuids))?;
    return Ok(());
  }

  // Restores the saved state. This loads the persisted Rollback list and its heaps.
  pub fn restore_state(&mut self) -> std::io::Result<()> {
    for i in 0 .. MAX_HEAPS {
      self.heap[i as usize].clear();
    }
    self.nuls = (2 .. MAX_HEAPS).collect();
    // for i in 0 .. std::cmp::max(uuids.len(), 8) {
    //   self.heap[i + 2].load_buffers(uuids[i])?;
    // }
    let mut keeps = util::u8s_to_u128s(&std::fs::read(self.path.join("_keeps_"))?);
    let mut lifes = util::u8s_to_u128s(&std::fs::read(self.path.join("_lifes_"))?);
    let mut uuids = util::u8s_to_u128s(&std::fs::read(self.path.join("_uuids_"))?);
    fn load_heaps(rt: &mut Runtime, keeps: &mut Vec<u128>, lifes: &mut Vec<u128>, uuids: &mut Vec<u128>, index: u64, back: Arc<Rollback>) -> std::io::Result<Arc<Rollback>> {
      let keep = keeps.pop();
      let life = lifes.pop();
      let uuid = uuids.pop();
      match (keep, life, uuid) {
        (Some(keep), Some(life), Some(uuid)) => {
          let next = rt.nuls.pop();
          match next {
            Some(next) => {
              let path = rt.get_dir_path();
              rt.heap[index as usize] = Heap::deserialize(uuid, &path)?;
              rt.curr = index;
              return load_heaps(rt, keeps, lifes, uuids, next, Arc::new(Rollback::Cons { keep: keep as u64, life: life as u64, head: index, tail: back }));
            }
            None => {
              panic!("Not enough heaps.");
            }
          }
        }
        (None, None, None) => {
          return Ok(back);
        }
        _ => {
          panic!("Corrupted persistence files.");
        }
      }
    }
    self.draw = 0;
    self.curr = 1;
    self.back = load_heaps(self, &mut keeps, &mut lifes, &mut uuids, self.curr, Arc::new(Rollback::Nil))?;
    self.curr = self.nuls.pop().expect("No heap available!");
    return Ok(());
  }

  // Reverts until the last 
  pub fn clear_current_heap(&mut self) {
    self.heap[self.curr as usize].clear();
  }

  // Heap writers and readers
  // ------------------------

  // Attempts to read data from the latest heap.
  // If not present, looks for it on past states.
  pub fn get_with<A: std::cmp::PartialEq>(&self, zero: A, none: A, get: impl Fn(&Heap) -> A) -> A {
    let got = get(&self.get_heap(self.draw));
    if none != got {
      return got;
    }
    let got = get(&self.get_heap(self.curr));
    if none != got {
      return got;
    }
    let mut back = &self.back;
    loop {
      match &**back {
        Rollback::Cons { keep, life, head, tail } => {
          let val = get(self.get_heap(*head));
          if val != none {
            return val;
          }
          back = tail;
        }
        Rollback::Nil => {
          return zero;
        }
      }
    }
  }

  // Same as get_with, but gets a function
  // FIXME: can get_with be generalized for this case too?
  pub fn get_func(&self, name: &Name) -> Option<Arc<CompFunc>> {
    let got = self.get_heap(self.draw).read_file(name);
    if let Some(func) = got {
      return Some(func);
    }
    let got = self.get_heap(self.curr).read_file(name);
    if let Some(func) = got {
      return Some(func);
    }
    let mut back = &self.back;
    loop {
      match &**back {
        Rollback::Cons { keep, life, head, tail } => {
          let got = self.get_heap(*head).file.read(name);
          if let Some(func) = got {
            return Some(func);
          }
          back = tail;
        }
        Rollback::Nil => {
          return None;
        }
      }
    }
  }

  pub fn reduce_with<A>(&self, acc: &mut A, reduce: impl Fn(&mut A, &Heap)) {
    reduce(acc, &self.get_heap(self.draw));
    reduce(acc, &self.get_heap(self.curr));
    let mut back = &self.back;
    while let Rollback::Cons { keep: _, life, head, tail } = &**back {
      reduce(acc, self.get_heap(*head));
      back = tail;
    }
  }

  pub fn write(&mut self, idx: Loc, val: RawCell) {
    return self.get_heap_mut(self.draw).write(idx, val);
  }

  pub fn read(&self, idx: Loc) -> RawCell {
    return self.get_with(RawCell(0), RawCell(U128_NONE), |heap| heap.read(idx));
  }

  pub fn write_disk(&mut self, name: U120, val: RawCell) {
    return self.get_heap_mut(self.draw).write_disk(name, val);
  }

  pub fn read_disk(&self, name: U120) -> Option<RawCell> {
    return self.get_with(None, None, |heap| heap.read_disk(name));
  }

  pub fn read_disk_as_term(&mut self, name: U120, limit: Option<usize>) -> Option<Term> {
    let host = self.read_disk(name)?;
    readback_term(self, host, limit)
  }

  pub fn read_file(&self, name: &Name) -> Option<CompFunc> {
    self.get_with(None, None, |heap| heap.read_file(name)).map(|func| (*func).clone())
  }

  pub fn get_arity(&self, name: &Name) -> Option<u64> {
    self.get_with(None, None, |heap| heap.read_arit(name))
  }

  pub fn set_arity(&mut self, name: Name, arity: u64) {
    self.get_heap_mut(self.draw).write_arit(name, arity);
  }

  pub fn get_owner(&self, name: &Name) -> Option<U120> {
    self.get_with(None, None, |heap| heap.read_ownr(name))
  }

  pub fn set_owner(&mut self, name: Name, owner: U120) {
    self.get_heap_mut(self.draw).write_ownr(name, owner);
  }

  pub fn get_index(&mut self, name: &Name) -> Option<u128> {
    self.get_with(None, None, |heap| heap.read_indx(name))
  }


  pub fn get_sth0(&mut self, pos: u128) -> Option<u128> {
    let stmt_hash = self.get_with(None, None, |heap| heap.read_stmt_hash(&pos).map(|h| h.clone()));
    if let Some(stmt_hash) = stmt_hash { // is cloning here really necessary?
      let mut bytes: [u8; 16] = [0; 16];
      bytes.copy_from_slice(&stmt_hash.0[0..16]);
      Some(u128::from_le_bytes(bytes) & *U120::MAX)
    }
    else {
      None
    }
  }

  pub fn get_sth1(&mut self, pos: u128) -> Option<u128> {
    let stmt_hash = self.get_with(None, None, |heap| heap.read_stmt_hash(&pos).map(|h| h.clone()));
    if let Some(stmt_hash) = stmt_hash {
      let mut bytes: [u8; 16] = [0; 16];
      bytes.copy_from_slice(&stmt_hash.0[15..31]); //read from 15 to 31st byte and throw the last one away.
      Some(u128::from_le_bytes(bytes) & *U120::MAX)
    }
    else {
      None
    }
  }

  
  pub fn exists(&self, name: &Name) -> bool {
    // there is a function or a constructor with this name
    if let Some(_) = self.get_with(None, None, |heap| heap.read_arit(name)) {
      return true;
    }
    // there is a namespace with this name
    else if let Some(_) = self.get_with(None, None, |heap| heap.read_ownr(name)) {
      return true;
    } else {
      return false;
    }
  }

  pub fn get_dups(&self) -> u64 {
    return self.get_with(0, U64_NONE, |heap| heap.get_dups());
  }

  pub fn set_rwts(&mut self, rwts: u64) {
    self.get_heap_mut(self.draw).set_rwts(rwts);
  }

  pub fn get_rwts(&self) -> u64 {
    return self.get_with(0, U64_NONE, |heap| heap.rwts);
  }

  pub fn set_mana(&mut self, mana: u64) {
    self.get_heap_mut(self.draw).set_mana(mana);
  }

  pub fn get_mana(&self) -> u64 {
    return self.get_with(0, U64_NONE, |heap| heap.mana);
  }

  pub fn set_tick(&mut self, tick: u64) {
    self.get_heap_mut(self.draw).set_tick(tick);
  }

  pub fn get_tick(&self) -> u64 {
    return self.get_with(0, U64_NONE, |heap| heap.tick);
  }

  pub fn set_time(&mut self, time: u128) {
    self.get_heap_mut(self.draw).set_time(time);
  }

  pub fn get_time(&self) -> u128 {
    return self.get_with(0, U128_NONE, |heap| heap.time);
  }

  pub fn set_meta(&mut self, meta: u128) {
    self.get_heap_mut(self.draw).set_meta(meta);
  }

  pub fn get_meta(&self) -> u128 {
    return self.get_with(0, U128_NONE, |heap| heap.meta);
  }

  pub fn set_hax0(&mut self, hax0: u128) {
    self.get_heap_mut(self.draw).set_hax0(hax0);
  }

  pub fn get_hax0(&self) -> u128 {
    return self.get_with(0, U128_NONE, |heap| heap.hax0);
  }

  pub fn set_hax1(&mut self, hax1: u128) {
    self.get_heap_mut(self.draw).set_hax1(hax1);
  }

  pub fn get_hax1(&self) -> u128 {
    return self.get_with(0, U128_NONE, |heap| heap.hax1);
  }

  pub fn set_size(&mut self, size: u64) {
    self.get_heap_mut(self.draw).size = size;
  }

  pub fn get_size(&self) -> u64 {
    return self.get_with(0, U64_NONE, |heap| heap.size);
  }

  pub fn set_mcap(&mut self, mcap: u64) {
    self.get_heap_mut(self.draw).mcap = mcap;
  }

  pub fn get_mcap(&self) -> u64 {
    return self.get_with(32, U64_NONE, |heap| heap.mcap);
  }

  pub fn set_next(&mut self, next: u64) {
    self.get_heap_mut(self.draw).next = next;
  }

  pub fn get_next(&self) -> u64 {
    return self.get_with(0, U64_NONE, |heap| heap.next);
  }

  pub fn fresh_dups(&mut self) -> u64 {
    let dups = self.get_dups();
    self.get_heap_mut(self.draw).set_dups(dups + 1);
    return dups & 0x3FFFFFFF;
  }

  pub fn get_all_funs(&self) -> Vec<Name> {
    let mut funcs: Vec<Name> = Vec::new();
    self.reduce_with(&mut funcs, |acc, heap| {
      let mut heap_funcs: Vec<Name> = 
        heap.file.funcs
          .keys()
          .map(|f| *f)
          .collect();
      acc.append(&mut heap_funcs);
    });
    funcs
  }

  pub fn get_all_ctr(&self) -> Vec<Name> {
    let mut ctrs: Vec<Name> = Vec::new();
    self.reduce_with(&mut ctrs, |acc, heap| {
      let heap_funs: Vec<_> = 
        heap.file.funcs
          .keys()
          .collect();
      let mut heap_ctrs = 
        heap.arit.arits
          .keys()
          .filter(|s| !heap_funs.contains(s))
          .map(|c| *c)
          .collect();
      acc.append(&mut heap_ctrs);
    });
    ctrs
  }

  pub fn get_all_ns(&self) -> Vec<Name> {
    let mut ns: Vec<Name> = Vec::new();
    self.reduce_with(&mut ns, |acc, heap| {
      let mut heap_ns: Vec<Name> = 
        heap.ownr.ownrs
          .keys()
          .map(|n| *n)
          .collect();
      acc.append(&mut heap_ns);
    });
    ns
  }
}

// Attempts to include a heap state on the list of past heap states. It only keeps at most
// `log_16(tick)` heaps in memory, rejecting heaps that it doesn't need to store. It returns:
// - included : Bool             = true if the heap was included, false if it was rejected
// - absorber : Option<Box<u64>> = the index of the dropped heap absorber (if any)
// - deleted  : Option<Box<u64>> = the index of the dropped heap (if any)
// - rollback : Rollback         = the updated rollback object
pub fn rollback_push(elem: u64, back: Arc<Rollback>, depth: u64) -> (bool, Option<u64>, Option<u64>, Arc<Rollback>) {
  if depth >= MAX_ROLLBACK {
    return (false, None, Some(elem), Arc::new(Rollback::Nil));
  } else {
    match &*back {
      Rollback::Nil => {
        let rollback = Arc::new(Rollback::Cons { keep: 0, life: 0, head: elem, tail: Arc::new(Rollback::Nil) });
        return (true, None, None, rollback);
      }
      Rollback::Cons { keep, life, head, tail } => {
        if *keep == 0xF {
          if *life > 0 {
            let tail = Arc::new(Rollback::Cons { keep: 0, life: life - 1, head: *head, tail: tail.clone() });
            let back = Arc::new(Rollback::Cons { keep: 0, life: 0, head: elem, tail });
            return (true, None, None, back);
          } else {
            let (included, absorber, deleted, tail) = rollback_push(*head, tail.clone(), depth + 1);
            let absorber = if !included { Some(elem) } else { absorber };
            let rollback = Arc::new(Rollback::Cons { keep: 0, life: *life, head: elem, tail });
            return (true, absorber, deleted, rollback);
          }
        } else {
          let rollback = Arc::new(Rollback::Cons { keep: keep + 1, life: *life, head: *head, tail: tail.clone() });
          return (false, None, Some(elem), rollback);
        }
      }
    }
  }
}

pub fn view_rollback(back: &Arc<Rollback>) -> String {
  match &**back {
    Rollback::Nil => {
      return String::new();
    }
    Rollback::Cons { keep, life, head, tail } => {
      return format!("[{:x} {}] {}", keep, head, view_rollback(tail));
    }
  }
}


// Constructors
// ------------

pub fn Var(pos: Loc) -> RawCell {
  RawCell((VAR * TAG_SHL) | *pos as u128)
}

pub fn Dp0(col: u128, pos: Loc) -> RawCell {
  RawCell((DP0 * TAG_SHL) | (col * EXT_SHL) | *pos as u128)
}

pub fn Dp1(col: u128, pos: Loc) -> RawCell {
  RawCell((DP1 * TAG_SHL) | (col * EXT_SHL) | *pos as u128)
}

pub fn Arg(pos: Loc) -> RawCell {
  RawCell((ARG * TAG_SHL) | *pos as u128)
}

pub fn Era() -> RawCell {
  RawCell(ERA * TAG_SHL)
}

pub fn Lam(pos: Loc) -> RawCell {
  RawCell((LAM * TAG_SHL) | *pos as u128)
}

pub fn App(pos: Loc) -> RawCell {
  RawCell((APP * TAG_SHL) | *pos as u128)
}

pub fn Par(col: u128, pos: Loc) -> RawCell {
  RawCell((SUP * TAG_SHL) | (col * EXT_SHL) | *pos as u128)
}

pub fn Op2(ope: u128, pos: Loc) -> RawCell {
  RawCell((OP2 * TAG_SHL) | (ope * EXT_SHL) | *pos as u128)
}

pub fn Num(val: u128) -> RawCell {
  debug_assert!((!NUM_MASK & val) == 0, "Num overflow: `{}`.", val);
  RawCell((NUM * TAG_SHL) | (val & NUM_MASK))
}

pub fn Ctr(fun: Name, pos: Loc) -> RawCell {
  RawCell((CTR * TAG_SHL) | (*fun * EXT_SHL) | *pos as u128)
}

pub fn Fun(fun: Name, pos: Loc) -> RawCell {
  RawCell((FUN * TAG_SHL) | (*fun * EXT_SHL) | *pos as u128)
}

// Getters
// -------

pub fn get_tag(lnk: RawCell) -> u128 {
  *lnk / TAG_SHL
}

pub fn get_ext(lnk: RawCell) -> u128 {
  (*lnk / EXT_SHL) & 0xFF_FFFF_FFFF_FFFF_FFFF
}

pub fn get_val(lnk: RawCell) -> u64 {
  (*lnk & 0xFFFF_FFFF_FFFF) as u64
}

pub fn get_num(lnk: RawCell) -> U120 {
  U120::from_u128_unchecked(*lnk & NUM_MASK)
}

//pub fn get_ari(lnk: RawCell) -> u128 {
  //(lnk / ARI) & 0xF
//}

pub fn get_loc(lnk: RawCell, arg: u64) -> Loc {
  Loc(get_val(lnk) + arg)
}

// Memory
// ------

pub fn ask_lnk(rt: &Runtime, loc: Loc) -> RawCell {
  rt.read(loc)
  //unsafe { *rt.heap.get_unchecked(loc as usize) }
}

pub fn ask_arg(rt: &Runtime, term: RawCell, arg: u64) -> RawCell {
  ask_lnk(rt, get_loc(term, arg))
}

pub fn link(rt: &mut Runtime, loc: Loc, lnk: RawCell) -> RawCell {
  rt.write(loc, lnk);
  if get_tag(lnk) <= VAR {
    let pos = get_loc(lnk, (get_tag(lnk) & 0x01) as u64);
    rt.write(pos, Arg(loc));
  }
  lnk
}

pub fn alloc(rt: &mut Runtime, arity: u64) -> Loc {
  if arity == 0 {
    return Loc(0);
  } else {
    loop {
      // Attempts to allocate enough space, starting from the last index
      // where we previously found free space, and moving rightwards
      let mcap = rt.get_mcap();
      let index = rt.get_next();
      if index <= mcap - arity {
        let index = Loc(index);
        let mut has_space = true;
        for i in 0 .. arity {
          if *rt.read(index + i) != 0 {
            has_space = false;
            break;
          }
        }
        // If we managed to find enough free space somewhere, return that index
        if has_space {
          rt.set_next(rt.get_next() + arity);
          rt.set_size(rt.get_size() + arity);
          //println!("{}", show_memo(rt));
          for i in 0 .. arity {
            rt.write(index + i, RawCell(NIL * TAG_SHL)); // millions perished for forgetting this line
          }
          return index;
        }
      }
      // If we couldn't allocate space...
      // - If less than 50% of the memory is used, jump to a random index and try again
      // - If more than 50% of the memory is used, double the maximum cap and try again
      if rt.get_size() * 2 < mcap {
        rt.set_next(fastrand::u64(..) % mcap as u64);
      } else {
        rt.set_mcap(mcap * 2);
      }
    }
  }
}

pub fn clear(rt: &mut Runtime, loc: Loc, size: u64) {
  for i in 0 .. size {
    if rt.read(loc + i) == RawCell(0) {
      panic!("Cleared twice: {}", *loc);
    }
    rt.write(loc + i, RawCell(0));
  }
  rt.set_size(rt.get_size() - size);
  //rt.free[size as usize].push(loc);
}

pub fn collect(rt: &mut Runtime, term: RawCell) {
  let mut stack : Vec<RawCell> = Vec::new();
  let mut next = term;
  let mut dups : Vec<RawCell> = Vec::new();
  loop {
    let term = next;
    match get_tag(term) {
      DP0 => {
        link(rt, get_loc(term, 0), Era());
        dups.push(term);
      }
      DP1 => {
        link(rt, get_loc(term, 1), Era());
        dups.push(term);
      }
      VAR => {
        link(rt, get_loc(term, 0), Era());
      }
      LAM => {
        if get_tag(ask_arg(rt, term, 0)) != ERA {
          link(rt, get_loc(ask_arg(rt, term, 0), 0), Era());
        }
        next = ask_arg(rt, term, 1);
        clear(rt, get_loc(term, 0), 2);
        continue;
      }
      APP => {
        stack.push(ask_arg(rt, term, 0));
        next = ask_arg(rt, term, 1);
        clear(rt, get_loc(term, 0), 2);
        continue;
      }
      SUP => {
        stack.push(ask_arg(rt, term, 0));
        next = ask_arg(rt, term, 1);
        clear(rt, get_loc(term, 0), 2);
        continue;
      }
      OP2 => {
        stack.push(ask_arg(rt, term, 0));
        next = ask_arg(rt, term, 1);
        clear(rt, get_loc(term, 0), 2);
        continue;
      }
      NUM => {}
      CTR | FUN => {
        let arity = rt.get_arity(&Name::new_unsafe(get_ext(term))).unwrap();
        // NOTE: should never be none, should panic
        // TODO: remove unwrap?
        for i in 0 .. arity {
          if i < arity - 1 {
            stack.push(ask_arg(rt, term, i));
          } else {
            next = ask_arg(rt, term, i);
          }
        }
        clear(rt, get_loc(term, 0), arity);
        if arity > 0 {
          continue;
        }
      }
      _ => {}
    }
    if let Some(got) = stack.pop() {
      next = got;
    } else {
      break;
    }
  }
  for dup in dups {
    let fst = ask_arg(rt, dup, 0);
    let snd = ask_arg(rt, dup, 1);
    if get_tag(fst) == ERA && get_tag(snd) == ERA {
      collect(rt, ask_arg(rt, dup, 2));
      clear(rt, get_loc(dup, 0), 3);
    }
  }
}

// Term
// ----

pub fn check_term(term: &Term) -> Result<(), RuntimeError> {
  check_linear(term)?;
  check_term_depth(term, 0)?;
  Ok(())
}


pub fn check_func(func: &Func) -> Result<(), RuntimeError> {
  for rule in &func.rules {
    check_term(&rule.lhs)?;
    check_term(&rule.rhs)?;
  }
  Ok(())
}


// Counts how many times the free variable 'name' appears inside Term
fn count_uses(term: &Term, name: Name) -> u128 {
  match term {
    Term::Var { name: var_name } => {
      return if name == *var_name { 1 } else { 0 };
    }
    Term::Dup { nam0, nam1, expr, body } => {
      let expr_uses = count_uses(expr, name);
      let body_uses = if name == *nam0 || name == *nam1 { 0 } else { count_uses(body, name) };
      return expr_uses + body_uses;
    }
    Term::Lam { name: lam_name, body } => {
      return if name == *lam_name { 0 } else { count_uses(body, name) };
    }
    Term::App { func, argm } => {
      let func_uses = count_uses(func, name);
      let argm_uses = count_uses(argm, name);
      return func_uses + argm_uses;
    }
    Term::Ctr { name: ctr_name, args } => {
      let mut uses = 0;
      for arg in args {
        uses += count_uses(arg, name);
      }
      return uses;
    }
    Term::Fun { name: fun_name, args } => {
      let mut uses = 0;
      for arg in args {
        uses += count_uses(arg, name);
      }
      return uses;
    }
    Term::Num { numb } => {
      return 0;
    }
    Term::Op2 { oper, val0, val1 } => {
      let val0_uses = count_uses(val0, name);
      let val1_uses = count_uses(val1, name);
      return val0_uses + val1_uses;
    }
  }
}

// Checks if:
// - Every non-erased variable is used exactly once
// - Every erased variable is never used
pub fn check_linear(term: &Term) -> Result<(), RuntimeError> {
  // println!("{}", view_term(term));
  let res = match term {
    Term::Var { name: var_name } => {
      // TODO: check unbound variables
      Ok(())
    }
    Term::Dup { nam0, nam1, expr, body } => {
      check_linear(expr)?;
      check_linear(body)?;
      if !(*nam0 == Name::NONE || count_uses(body, *nam0) == 1) {
        return Err(RuntimeError::TermIsNotLinear { term: term.clone(), var: *nam0 });
      }
      if !(*nam1 == Name::NONE || count_uses(body, *nam1) == 1) {
        return Err(RuntimeError::TermIsNotLinear {term : term.clone(), var: *nam0 });
      }
      Ok(())
    }
    Term::Lam { name, body } => {
      check_linear(body)?;
      if !(*name == Name::NONE || count_uses(body, *name) == 1) {
        return Err(RuntimeError::TermIsNotLinear {term : term.clone(), var: *name });
      }
      Ok(())
    }
    Term::App { func, argm } => {
      check_linear(func)?;
      check_linear(argm)?;
      Ok(())
    }
    Term::Ctr { name: ctr_name, args } => {
      for arg in args {
        check_linear(arg)?;
      }
      Ok(())
    }
    Term::Fun { name: fun_name, args } => {
      for arg in args {
        check_linear(arg)?;
      }
      Ok(())
    }
    Term::Num { numb } => {
      Ok(())
    }
    Term::Op2 { oper, val0, val1 } => {
      check_linear(val0)?;
      check_linear(val1)?;
      Ok(())
    }
  };

  // println!("{}: {}", view_term(term), res);
  res
}

pub fn check_term_depth(term: &Term, depth: u128) -> Result<(), RuntimeError> {
  if depth > MAX_TERM_DEPTH {
    return Err(RuntimeError::TermExceedsMaxDepth);
    // this is the stupidest clone of all time, it is a huge waste
    // but receivin a borrow in an enum is boring
  } else {
    match term {
      Term::Var { name } => {
        return Ok(());
      },
      Term::Dup { nam0, nam1, expr, body } => {
        check_term_depth(expr, depth + 1)?;
        check_term_depth(body, depth + 1)?;
        return Ok(());
      }
      Term::Lam { name, body } => {
        check_term_depth(body, depth + 1)?;
        return Ok(());
      }
      Term::App { func, argm } => {
        check_term_depth(func, depth + 1)?;
        check_term_depth(argm, depth + 1)?;
        return Ok(());
      }
      Term::Ctr { name, args } => {
        for arg in args {
          check_term_depth(arg, depth + 1)?;
        }
        return Ok(());
      }
      Term::Fun { name, args } => {
        for arg in args {
          check_term_depth(arg, depth + 1)?;
        }
        return Ok(());
      }
      Term::Num { numb } => {
        return Ok(());
      }
      Term::Op2 { oper, val0, val1 } => {
        check_term_depth(val0, depth + 1)?;
        check_term_depth(val1, depth + 1)?;
        return Ok(());
      }
    }
  }
}

// Writes a Term represented as a Rust enum on the Runtime's rt.
pub fn create_term(rt: &mut Runtime, term: &Term, loc: Loc, vars_data: &mut NameMap<Vec<RawCell>>) -> Result<RawCell, RuntimeError> {
  fn consume(rt: &mut Runtime, loc: Loc, name: Name, vars_data: &mut NameMap<Vec<RawCell>>) -> Option<RawCell> {
    let got = vars_data.get_mut(&name)?;
    let got = got.pop()?;
    Some(got)
  }

  fn bind(rt: &mut Runtime, loc: Loc, name: Name, lnk: RawCell, vars_data: &mut NameMap<Vec<RawCell>>) {
    // println!("~~ bind {} {}", u128_to_name(name), show_ptr(lnk));
    if name == Name::NONE {
      link(rt, loc, Era());
    } else {
      let got = vars_data.entry(name).or_insert(Vec::new());
      got.push(lnk);
      link(rt, loc, Era());
    }
  }

  match term {
    Term::Var { name } => {
      //println!("~~ var {} {}", name, vars_data.len());
      consume(rt, loc, *name, vars_data).ok_or_else(||
        RuntimeError::UnboundVar { name: *name })
    }
    Term::Dup { nam0, nam1, expr, body } => {
      let node = alloc(rt, 3);
      let dupk = rt.fresh_dups();
      // TODO: Review: expr create_term was moved above the 2 below binds (Dp0
      // and Dp1) to so it consumes variable names so they can be re-binded,
      // allowing: `dup x y = x`
      let expr = create_term(rt, expr, node + 2, vars_data)?;
      link(rt, node + 2, expr);
      bind(rt, node + 0, *nam0, Dp0(dupk as u128, node), vars_data);
      bind(rt, node + 1, *nam1, Dp1(dupk as u128, node), vars_data); // TODO: shouldnt these be labels? why are they u64?
      let body = create_term(rt, body, loc, vars_data);
      body
    }
    Term::Lam { name, body } => {
      let node = alloc(rt, 2);
      bind(rt, node + 0, *name, Var(node), vars_data);
      let body = create_term(rt, body, node + 1, vars_data)?;
      link(rt, node + 1, body);
      Ok(Lam(node))
    }
    Term::App { func, argm } => {
      let node = alloc(rt, 2);
      let func = create_term(rt, func, node + 0, vars_data)?;
      link(rt, node + 0, func);
      let argm = create_term(rt, argm, node + 1, vars_data)?;
      link(rt, node + 1, argm);
      Ok(App(node))
    }
    Term::Fun { name, args } => {
      let expected = rt.get_arity(name)
        .ok_or_else(|| RuntimeError::CtrOrFunNotDefined { name: *name })?
        as usize;
      if args.len() != expected {
        Err(RuntimeError::ArityMismatch { name: *name, expected, got: args.len() })
      } else {
        let size = args.len() as u64;
        let node = alloc(rt, size);
        for (i, arg) in args.iter().enumerate() {
          let arg_lnk = create_term(rt, arg, node + i as u64, vars_data)?;
          link(rt, node + i as u64, arg_lnk);
        }
        Ok(Fun(*name, node))
      }
    }
    Term::Ctr { name, args } => {
      let expected = rt.get_arity(name)
        .ok_or_else(|| RuntimeError::CtrOrFunNotDefined { name: *name })?
        as usize;
      if args.len() != expected {
        Err(RuntimeError::ArityMismatch { name: *name, expected, got: args.len() })
      } else {
        let size = args.len() as u64;
        let node = alloc(rt, size);
        for (i, arg) in args.iter().enumerate() {
          let arg_lnk = create_term(rt, arg, node + i as u64, vars_data)?;
          link(rt, node + i as u64, arg_lnk);
        }
        Ok(Ctr(*name, node))
      }
    }
    Term::Num { numb } => {
      Ok(Num(**numb))
    }
    Term::Op2 { oper, val0, val1 } => {
      let node = alloc(rt, 2);
      let val0 = create_term(rt, val0, node + 0, vars_data)?;
      link(rt, node + 0, val0);
      let val1 = create_term(rt, val1, node + 1, vars_data)?;
      link(rt, node + 1, val1);
      Ok(Op2(*oper as u128, node))
    }
  }
}

/// Given a Func (a vector of rules, lhs/rhs pairs), builds the CompFunc object
pub fn compile_func(func: &Func, debug: bool) -> Result<CompFunc, RuntimeError> {
  let rules = &func.rules;

  // If there are no rules, return none
  if rules.len() == 0 {
    return Err(RuntimeError::DefinitionError(DefinitionError::FunctionHasNoRules));
  }

  // Find the function arity
  let arity;
  if let Term::Fun { args, .. } = &rules[0].lhs {
    arity = args.len() as u64;
  } else {
    return Err(RuntimeError::DefinitionError(DefinitionError::LHSIsNotAFunction));
    // TODO: remove this error, should be checked at compile time
  }

  // The resulting vector
  let mut comp_rules = Vec::new();

  // A vector with the indices that are strict
  let mut strict = vec![false; arity as usize];

  // For each rule (lhs/rhs pair)
  for rule_index in 0 .. rules.len() {
    let rule = &func.rules[rule_index];

    // Validates that:
    // - the same lhs variable names aren't defined twice or more
    // - lhs variables are used linearly on the rhs
    let mut seen : HashSet<Name> = HashSet::new();
    fn check_var(name: Name, body: &Term, seen: &mut HashSet<Name>, rule_index: usize) -> Result<(), RuntimeError> {
      if seen.contains(&name) {
        return Err(RuntimeError::DefinitionError(DefinitionError::VarIsUsedTwiceInDefinition { name, rule_index}));
      } else if name == Name::NONE {
        return Ok(());
      } else {
        seen.insert(name);
        let uses = count_uses(body, name);
        match uses {
          0 => Err(RuntimeError::DefinitionError(DefinitionError::VarIsNotUsed { name, rule_index })),
          1 => Ok(()),
          _ => Err(RuntimeError::DefinitionError(DefinitionError::VarIsNotLinearInBody { name, rule_index }))
        }
      }
    }

    let mut cond = Vec::new();
    let mut vars = Vec::new();
    let mut eras = Vec::new();

    // If the lhs is a Fun
    if let Term::Fun { ref name, ref args } = rule.lhs {

      // If there is an arity mismatch, return None
      if args.len() as u64 != arity {
        return Err(RuntimeError::DefinitionError(DefinitionError::LHSArityMismatch { rule_index, expected: arity as usize, got: args.len() }));
        // TODO: should check at compile time, remove this error
      }

      // For each lhs argument
      for i in 0 .. args.len() as u64 {

        match &args[i as usize] {
          // If it is a constructor...
          Term::Ctr { name: arg_name, args: arg_args } => {
            strict[i as usize] = true;
            cond.push(Ctr(*arg_name, Loc(0))); // adds its matching condition
            eras.push((i, arg_args.len() as u64)); // marks its index and arity for freeing
            // For each of its fields...
            for j in 0 .. arg_args.len() as u64 {
              // If it is a variable...
              if let Term::Var { name } = arg_args[j as usize] {
                check_var(name, &rule.rhs, &mut seen, rule_index)?;
                vars.push(Var { name, param: i, field: Some(j), erase: name == Name::NONE }); // add its location
              // Otherwise..
              } else {
                return Err(RuntimeError::DefinitionError(DefinitionError::NestedMatch { rule_index })); // return none, because we don't allow nested matches
              }
            }
          }
          // If it is a number...
          Term::Num { numb: arg_numb } => {
            strict[i as usize] = true;
            cond.push(Num(**arg_numb)); // adds its matching condition
          }
          // If it is a variable...
          Term::Var { name: arg_name } => {
            check_var(*arg_name, &rule.rhs, &mut seen, rule_index)?;
            vars.push(Var { name: *arg_name, param: i, field: None, erase: *arg_name == Name::NONE }); // add its location
            cond.push(Var(Loc(0))); // it has no matching condition
          }
          _ => {
            return Err(RuntimeError::DefinitionError(DefinitionError::UnsupportedMatch { rule_index } ));
          }
        }
      }

    // If lhs isn't a Ctr, return None
    } else {
      return Err(RuntimeError::DefinitionError(DefinitionError::LHSNotConstructor { rule_index }))
    }

    // Creates the rhs body
    let body = rule.rhs.clone();

    // Adds the rule to the result vector
    comp_rules.push(CompRule { cond, vars, eras, body });
  }

  // Builds the redux object, with the index of strict arguments
  let mut redux = Vec::new();
  for i in 0 .. strict.len() {
    if strict[i] {
      redux.push(i as u64);
    }
  }

  return Ok(CompFunc {
    func: func.clone(),
    arity,
    redux,
    rules: comp_rules,
  });
}

pub fn create_app(rt: &mut Runtime, func: RawCell, argm: RawCell) -> RawCell {
  let node = alloc(rt, 2);
  link(rt, node + 0, func);
  link(rt, node + 1, argm);
  App(node)
}

pub fn create_fun(rt: &mut Runtime, fun: Name, args: &[RawCell]) -> RawCell {
  let node = alloc(rt, args.len() as u64);
  for i in 0 .. args.len() {
    link(rt, node + (i as u64), args[i]);
  }
  Fun(fun, node)
}

pub fn alloc_lnk(rt: &mut Runtime, term: RawCell) -> Loc {
  let loc = alloc(rt, 1);
  link(rt, loc, term);
  return loc;
}

pub fn alloc_app(rt: &mut Runtime, func: RawCell, argm: RawCell) -> Loc {
  let app = create_app(rt, func, argm);
  return alloc_lnk(rt, app);
}

pub fn alloc_fun(rt: &mut Runtime, fun: Name, args: &[RawCell]) -> Loc {
  let fun = create_fun(rt, fun, args);
  return alloc_lnk(rt, fun);
}

// Reduction
// ---------

pub fn subst(rt: &mut Runtime, lnk: RawCell, val: RawCell) {
  if get_tag(lnk) != ERA {
    link(rt, get_loc(lnk, 0), val);
  } else {
    collect(rt, val);
  }
}

// TODO: document
pub fn reduce(rt: &mut Runtime, root: Loc, mana: u64) -> Result<RawCell, RuntimeError> {
  let mut vars_data: NameMap<Vec<RawCell>> = init_name_map();

  let mut stack: Vec<Loc> = Vec::new();

  // TODO: document `init` / refactor to tuple/struct if no performance impact
  let mut init = 1;
  let mut host = root;

  let mut func_val : Option<CompFunc>;
  let mut func_ref : Option<&mut CompFunc>;

  loop {
    let term = ask_lnk(rt, host);

    if rt.get_mana() > mana {
      return Err(RuntimeError::NotEnoughMana);
    }

    // if true {
    //   println!("----------------------");
    //   println!("{}", show_term(rt, ask_lnk(rt, root), Some(term)));
    // }

    if init == 1 {
      match get_tag(term) {
        APP => {
          stack.push(host);
          init = 1;
          host = get_loc(term, 0);
          continue;
        }
        DP0 | DP1 => {
          stack.push(host);
          host = get_loc(term, 2);
          continue;
        }
        OP2 => {
          stack.push(host);
          stack.push(Loc(*(get_loc(term, 0) + 1) | 0x1_0000_0000_0000)); //this is so ugly
          host = get_loc(term, 0);
          continue;
        }
        FUN => {
          let name = Name::new_unsafe(get_ext(term));
          let ari = rt.get_arity(&name).ok_or_else(|| RuntimeError::CtrOrFunNotDefined { name })?;
          if let Some(func) = &rt.get_func(&name) {
            if ari == func.arity {
              if func.redux.len() == 0 {
                init = 0;
              } else {
                stack.push(host);
                for (i, redux) in func.redux.iter().enumerate() {
                  if i < func.redux.len() - 1 {
                    let loc = get_loc(term, *redux);
                    stack.push(Loc(*loc | 0x1_0000_0000_0000));
                  } else {
                    host = get_loc(term, *redux);
                  }
                }
              }
              continue;
            }
          }
        }
        // We don't need to reduce further
        CTR | NUM | LAM | VAR | SUP | ERA | ARG => {}
        _ => panic!("Unexpected term tag `{}` on reduce", get_tag(term)),
      }
    } else {
      match get_tag(term) {
        APP => {
          let arg0 = ask_arg(rt, term, 0);
          // (@x(body) a)
          // ------------ APP-LAM
          // x <- a
          // body
          if get_tag(arg0) == LAM {
            //println!("app-lam");
            rt.set_mana(rt.get_mana() + AppLamMana());
            rt.set_rwts(rt.get_rwts() + 1);
            subst(rt, ask_arg(rt, arg0, 0), ask_arg(rt, term, 1));
            let _done = link(rt, host, ask_arg(rt, arg0, 1));
            clear(rt, get_loc(term, 0), 2);
            clear(rt, get_loc(arg0, 0), 2);
            init = 1;
            continue;
          // ({a b} c)
          // ----------------- APP-SUP
          // dup x0 x1 = c
          // {(a x0) (b x1)}
          } else if get_tag(arg0) == SUP {
            //println!("app-sup");
            rt.set_mana(rt.get_mana() + AppSupMana());
            rt.set_rwts(rt.get_rwts() + 1);
            let app0 = get_loc(term, 0);
            let app1 = get_loc(arg0, 0);
            let let0 = alloc(rt, 3);
            let par0 = alloc(rt, 2);
            link(rt, let0 + 2, ask_arg(rt, term, 1));
            link(rt, app0 + 1, Dp0(get_ext(arg0), let0));
            link(rt, app0 + 0, ask_arg(rt, arg0, 0));
            link(rt, app1 + 0, ask_arg(rt, arg0, 1));
            link(rt, app1 + 1, Dp1(get_ext(arg0), let0));
            link(rt, par0 + 0, App(app0));
            link(rt, par0 + 1, App(app1));
            let done = Par(get_ext(arg0), par0);
            link(rt, host, done);
          }
        }
        DP0 | DP1 => {
          let arg0 = ask_arg(rt, term, 2);
          // dup r s = @x(f)
          // --------------- DUP-LAM
          // dup f0 f1 = f
          // r <- @x0(f0)
          // s <- @x1(f1)
          // x <- {x0 x1}
          if get_tag(arg0) == LAM {
            //println!("dup-lam");
            rt.set_mana(rt.get_mana() + DupLamMana());
            rt.set_rwts(rt.get_rwts() + 1);
            let let0 = get_loc(term, 0);
            let par0 = get_loc(arg0, 0);
            let lam0 = alloc(rt, 2);
            let lam1 = alloc(rt, 2);
            link(rt, let0 + 2, ask_arg(rt, arg0, 1));
            link(rt, par0 + 1, Var(lam1));
            let arg0_arg_0 = ask_arg(rt, arg0, 0);
            link(rt, par0 + 0, Var(lam0));
            subst(rt, arg0_arg_0, Par(get_ext(term), par0));
            let term_arg_0 = ask_arg(rt, term, 0);
            link(rt, lam0 + 1, Dp0(get_ext(term), let0));
            subst(rt, term_arg_0, Lam(lam0));
            let term_arg_1 = ask_arg(rt, term, 1);
            link(rt, lam1 + 1, Dp1(get_ext(term), let0));
            subst(rt, term_arg_1, Lam(lam1));
            let done = Lam(if get_tag(term) == DP0 { lam0 } else { lam1 });
            link(rt, host, done);
            init = 1;
            continue;
          // dup x y = {a b}
          // --------------- DUP-SUP-E
          // x <- a
          // y <- b
          } else if get_tag(arg0) == SUP {
            if get_ext(term) == get_ext(arg0) {
              //println!("dup-sup-e");
              rt.set_mana(rt.get_mana() + DupSupMana());
              rt.set_rwts(rt.get_rwts() + 1);
              subst(rt, ask_arg(rt, term, 0), ask_arg(rt, arg0, 0));
              subst(rt, ask_arg(rt, term, 1), ask_arg(rt, arg0, 1));
              let _done = link(rt, host, ask_arg(rt, arg0, if get_tag(term) == DP0 { 0 } else { 1 }));
              clear(rt, get_loc(term, 0), 3);
              clear(rt, get_loc(arg0, 0), 2);
              init = 1;
              continue;
            // dup x y = {a b}
            // ----------------- DUP-SUP-D
            // x <- {xA xB}
            // y <- {yA yB}
            // dup xA yA = a
            // dup xB yB = b
            } else {
              //println!("dup-sup-d");
              rt.set_mana(rt.get_mana() + DupDupMana());
              rt.set_rwts(rt.get_rwts() + 1);
              let par0 = alloc(rt, 2);
              let let0 = get_loc(term, 0);
              let par1 = get_loc(arg0, 0);
              let let1 = alloc(rt, 3);
              link(rt, let0 + 2, ask_arg(rt, arg0, 0));
              link(rt, let1 + 2, ask_arg(rt, arg0, 1));
              let term_arg_0 = ask_arg(rt, term, 0);
              let term_arg_1 = ask_arg(rt, term, 1);
              link(rt, par1 + 0, Dp1(get_ext(term), let0));
              link(rt, par1 + 1, Dp1(get_ext(term), let1));
              link(rt, par0 + 0, Dp0(get_ext(term), let0));
              link(rt, par0 + 1, Dp0(get_ext(term), let1));
              subst(rt, term_arg_0, Par(get_ext(arg0), par0));
              subst(rt, term_arg_1, Par(get_ext(arg0), par1));
              let done = Par(get_ext(arg0), if get_tag(term) == DP0 { par0 } else { par1 });
              link(rt, host, done);
            }
          // dup x y = N
          // ----------- DUP-NUM
          // x <- N
          // y <- N
          // ~
          } else if get_tag(arg0) == NUM {
            //println!("dup-num");
            rt.set_mana(rt.get_mana() + DupNumMana());
            rt.set_rwts(rt.get_rwts() + 1);
            subst(rt, ask_arg(rt, term, 0), arg0);
            subst(rt, ask_arg(rt, term, 1), arg0);
            clear(rt, get_loc(term, 0), 3);
            let _done = arg0;
            link(rt, host, arg0);
          // dup x y = (K a b c ...)
          // ----------------------- DUP-CTR
          // dup a0 a1 = a
          // dup b0 b1 = b
          // dup c0 c1 = c
          // ...
          // x <- (K a0 b0 c0 ...)
          // y <- (K a1 b1 c1 ...)
          } else if get_tag(arg0) == CTR {
            //println!("dup-ctr");
            let func = get_ext(arg0);
            let name = Name::new_unsafe(func);
            let arit = rt.get_arity(&name).ok_or_else(|| RuntimeError::CtrOrFunNotDefined { name })?;
            rt.set_mana(rt.get_mana() + DupCtrMana(arit));
            rt.set_rwts(rt.get_rwts() + 1);
            if arit == 0 {
              subst(rt, ask_arg(rt, term, 0), Ctr(name, Loc(0)));
              subst(rt, ask_arg(rt, term, 1), Ctr(name, Loc(0)));
              clear(rt, get_loc(term, 0), 3);
              let _done = link(rt, host, Ctr(name, Loc(0)));
            } else {
              let ctr0 = get_loc(arg0, 0);
              let ctr1 = alloc(rt, arit);
              for i in 0..arit - 1 {
                let leti = alloc(rt, 3);
                link(rt, leti + 2, ask_arg(rt, arg0, i));
                link(rt, ctr0 + i, Dp0(get_ext(term), leti));
                link(rt, ctr1 + i, Dp1(get_ext(term), leti));
              }
              let leti = get_loc(term, 0);
              link(rt, leti + 2, ask_arg(rt, arg0, arit - 1));
              let term_arg_0 = ask_arg(rt, term, 0);
              link(rt, ctr0 + (arit - 1), Dp0(get_ext(term), leti));
              subst(rt, term_arg_0, Ctr(name, ctr0));
              let term_arg_1 = ask_arg(rt, term, 1);
              link(rt, ctr1 + (arit - 1), Dp1(get_ext(term), leti));
              subst(rt, term_arg_1, Ctr(name, ctr1));
              let done = Ctr(name, if get_tag(term) == DP0 { ctr0 } else { ctr1 });
              link(rt, host, done);
            }
          // dup x y = *
          // ----------- DUP-ERA
          // x <- *
          // y <- *
          } else if get_tag(arg0) == ERA {
            //println!("dup-era");
            rt.set_mana(rt.get_mana() + DupEraMana());
            rt.set_rwts(rt.get_rwts() + 1);
            subst(rt, ask_arg(rt, term, 0), Era());
            subst(rt, ask_arg(rt, term, 1), Era());
            link(rt, host, Era());
            clear(rt, get_loc(term, 0), 3);
            init = 1;
            continue;
          }
        }
        OP2 => {
          let arg0 = ask_arg(rt, term, 0);
          let arg1 = ask_arg(rt, term, 1);
          // (+ a b)
          // --------- OP2-NUM
          // add(a, b)
          if get_tag(arg0) == NUM && get_tag(arg1) == NUM {
            //eprintln!("op2-num");
            let op  = get_ext(term).try_into().expect("Invalid operation coming from HVM");
            let a_u = get_num(arg0);
            let b_u = get_num(arg1);
            if op == Oper::Div && *b_u == 0 {
              return Err(RuntimeError::DivisionByZero)
            }
            rt.set_mana(rt.get_mana() + Op2NumMana());
            let res = match op {
              Oper::Add => *a_u.wrapping_add(b_u),
              Oper::Sub => *a_u.wrapping_sub(b_u),
              Oper::Mul => *a_u.wrapping_mul(b_u),
              Oper::Div => *a_u.wrapping_div(b_u),
              Oper::Mod => *a_u.wrapping_rem(b_u),
              Oper::Shl => *a_u.wrapping_shl(b_u),
              Oper::Shr => *a_u.wrapping_shr(b_u),
              Oper::And => *a_u & *b_u,
              Oper::Or  => *a_u | *b_u,
              Oper::Xor => *a_u ^ *b_u,
              Oper::Ltn => u128::from(*a_u <  *b_u),
              Oper::Lte => u128::from(*a_u <= *b_u),
              Oper::Eql => u128::from(*a_u == *b_u),
              Oper::Gte => u128::from(*a_u >= *b_u),
              Oper::Gtn => u128::from(*a_u >  *b_u),
              Oper::Neq => u128::from(*a_u != *b_u),
            };
            let done = Num(res);
            clear(rt, get_loc(term, 0), 2);
            link(rt, host, done);
          // (+ {a0 a1} b)
          // --------------------- OP2-SUP-0
          // let b0 b1 = b
          // {(+ a0 b0) (+ a1 b1)}
          } else if get_tag(arg0) == SUP {
            //println!("op2-sup-0");
            rt.set_mana(rt.get_mana() + Op2SupMana());
            rt.set_rwts(rt.get_rwts() + 1);
            let op20 = get_loc(term, 0);
            let op21 = get_loc(arg0, 0);
            let let0 = alloc(rt, 3);
            let par0 = alloc(rt, 2);
            link(rt, let0 + 2, arg1);
            link(rt, op20 + 1, Dp0(get_ext(arg0), let0));
            link(rt, op20 + 0, ask_arg(rt, arg0, 0));
            link(rt, op21 + 0, ask_arg(rt, arg0, 1));
            link(rt, op21 + 1, Dp1(get_ext(arg0), let0));
            link(rt, par0 + 0, Op2(get_ext(term), op20));
            link(rt, par0 + 1, Op2(get_ext(term), op21));
            let done = Par(get_ext(arg0), par0);
            link(rt, host, done);
          // (+ a {b0 b1})
          // --------------- OP2-SUP-1
          // dup a0 a1 = a
          // {(+ a0 b0) (+ a1 b1)}
          } else if get_tag(arg1) == SUP {
            //println!("op2-sup-1");
            rt.set_mana(rt.get_mana() + Op2SupMana());
            rt.set_rwts(rt.get_rwts() + 1);
            let op20 = get_loc(term, 0);
            let op21 = get_loc(arg1, 0);
            let let0 = alloc(rt, 3);
            let par0 = alloc(rt, 2);
            link(rt, let0 + 2, arg0);
            link(rt, op20 + 0, Dp0(get_ext(arg1), let0));
            link(rt, op20 + 1, ask_arg(rt, arg1, 0));
            link(rt, op21 + 1, ask_arg(rt, arg1, 1));
            link(rt, op21 + 0, Dp1(get_ext(arg1), let0));
            link(rt, par0 + 0, Op2(get_ext(term), op20));
            link(rt, par0 + 1, Op2(get_ext(term), op21));
            let done = Par(get_ext(arg1), par0);
            link(rt, host, done);
          }
        }
        FUN => {

          fn call_function(rt: &mut Runtime, func: Arc<CompFunc>, host: Loc, term: RawCell, mana: u64, vars_data: &mut NameMap<Vec<RawCell>>) -> Result<bool, RuntimeError> {
            // For each argument, if it is a redex and a SUP, apply the cal_par rule
            for idx in &func.redux {
              // (F {a0 a1} b c ...)
              // ------------------- FUN-SUP
              // dup b0 b1 = b
              // dup c0 c1 = c
              // ...
              // {(F a0 b0 c0 ...) (F a1 b1 c1 ...)}
              if get_tag(ask_arg(rt, term, *idx)) == SUP {
                //println!("fun-sup");
                let funx = get_ext(term);
                let name = Name::new_unsafe(funx);
                let arit = rt.get_arity(&name).ok_or_else(|| RuntimeError::CtrOrFunNotDefined { name })?;
                rt.set_mana(rt.get_mana() + FunSupMana(arit));
                rt.set_rwts(rt.get_rwts() + 1);
                let argn = ask_arg(rt, term, *idx);
                let fun0 = get_loc(term, 0);
                let fun1 = alloc(rt, arit);
                let par0 = get_loc(argn, 0);
                for i in 0..arit {
                  if i != *idx {
                    let leti = alloc(rt, 3);
                    let argi = ask_arg(rt, term, i);
                    link(rt, fun0 + i, Dp0(get_ext(argn), leti));
                    link(rt, fun1 + i, Dp1(get_ext(argn), leti));
                    link(rt, leti + 2, argi);
                  } else {
                    link(rt, fun0 + i, ask_arg(rt, argn, 0));
                    link(rt, fun1 + i, ask_arg(rt, argn, 1));
                  }
                }
                link(rt, par0 + 0, Fun(name, fun0));
                link(rt, par0 + 1, Fun(name, fun1));
                let done = Par(get_ext(argn), par0);
                link(rt, host, done);
                return Ok(true);
              }
            }
            // For each rule condition vector
            for rule in &func.rules {
              // Check if the rule matches
              let mut matched = true;
              //println!("- matching rule");
              // Tests each rule condition (ex: `get_tag(args[0]) == SUCC`)
              for i in 0 .. rule.cond.len() as u64 {
                let cond = rule.cond[i as usize];
                match get_tag(cond) {
                  NUM => {
                    //println!("Didn't match because of NUM. i={} {} {}", i, get_val(ask_arg(rt, term, i)), get_val(cond));
                    let same_tag = get_tag(ask_arg(rt, term, i)) == NUM;
                    let same_val = get_val(ask_arg(rt, term, i)) == get_val(cond);
                    matched = matched && same_tag && same_val;
                  }
                  CTR => {
                    //println!("Didn't match because of CTR. i={} {} {}", i, get_tag(ask_arg(rt, term, i)), get_val(cond));
                    let same_tag = get_tag(ask_arg(rt, term, i)) == CTR;
                    let same_ext = get_ext(ask_arg(rt, term, i)) == get_ext(cond);
                    matched = matched && same_tag && same_ext;
                  }
                  VAR => {
                    if func.redux.contains(&i) {
                      let not_var = get_tag(ask_arg(rt, term, i)) > VAR;
                      matched = matched && not_var;
                    }
                  }
                  _ => {}
                }
              }
              // (user-defined)
              // -------------- FUN-CTR
              // (user-defined)
              // If all conditions are satisfied, the rule matched, so we must apply it
              if matched {
                //println!("fun-ctr");
                //println!("- matched");
                // Increments the gas count
                rt.set_mana(rt.get_mana() + FunCtrMana(&rule.body));
                rt.set_rwts(rt.get_rwts() + 1);
                // Gathers matched variables
                //let mut vars = vec![None; 16]; // FIXME: pre-alloc statically
                for (i, rule_var) in rule.vars.iter().enumerate() {
                  let mut var = term;
                  var = ask_arg(rt, var, rule_var.param);
                  if let Some(field) = rule_var.field {
                    var = ask_arg(rt, var, field);
                  }
                  //eprintln!("~~ set {} {}", u128_to_name(rule_var.name), show_ptr(var));
                  if !rule_var.erase {
                    let arr = vars_data.entry(rule_var.name).or_insert(Vec::new());
                    arr.push(var);
                  } else {
                    // Collects unused argument
                    collect(rt, var);
                  }
                }
                // Builds the right-hand side term (ex: `(Succ (Add a b))`)
                //println!("-- vars: {:?}", vars);
                let done = create_term(rt, &rule.body, host, vars_data)?;
                // Links the host location to it
                link(rt, host, done);
                // Clears the matched ctrs (the `(Succ ...)` and the `(Add ...)` ctrs)
                for (eras_index, eras_arity) in &rule.eras {
                  clear(rt, get_loc(ask_arg(rt, term, *eras_index), 0), *eras_arity);
                }
                clear(rt, get_loc(term, 0), func.arity);
                // // Collects unused variables (none in this example)
                // for i in 0 .. rule.vars.len() {
                //   if rule.vars[i].erase {
                //     if let Some(var) = vars_data.get(&(i as u64)) {
                //       collect(rt, *var, mana)?;
                //     }
                //   }
                // }
                return Ok(true);
              }
            }
            return Ok(false);
          }

          let fid = get_ext(term);
          if let Some(func) = rt.get_func(&Name::new_unsafe(fid)) {
            if call_function(rt, func, host, term, mana, &mut vars_data)? {
              init = 1;
              continue;
            }
          }

        }
        // We don't need to reduce further
        CTR | NUM | LAM | VAR | SUP | ERA | ARG => {}
        _ => panic!("Unexpected term tag `{}` on reduce", get_tag(term)),
      }
    }

    // When we don't need to reduce the head
    if let Some(item) = stack.pop() {
      init = *item >> 48;
      host = Loc(*item & 0x0_FFFF_FFFF_FFFF);
      continue;
    }

    break;
  }

  // FIXME: remove this when Runtime is split (see above)
  //rt.get_heap_mut(self.curr).file = file;

  return Ok(ask_lnk(rt, root));
}

/// Evaluates redexes iteratively. This is used to save space before storing a term, since,
/// otherwise, chunks would grow indefinitely due to lazy evaluation. It does not reduce the term to
/// normal form, though, since it stops on WHNFs. If it did, then storing a state wouldn't be O(1),
/// since it would require passing over the entire state.
pub fn compute_at(rt: &mut Runtime, host: Loc, mana: u64) -> Result<RawCell, RuntimeError> {
  enum StackItem {
    LinkResolver(Loc),
    Host(Loc, u64)
  }
  let mut stack = vec![StackItem::Host(host, mana)];
  let mut output = vec![];
  while !stack.is_empty() {
    let item = stack.pop().unwrap();
    match item {
      StackItem::Host(host, mana) => {
        let term = ask_lnk(rt, host);
        let norm = reduce(rt, host, mana)?;

        if term == norm {
          output.push(Some(term));
        } else {
          match get_tag(norm) {
            LAM => {
              let loc_1 = get_loc(norm, 1);
              stack.push(StackItem::LinkResolver(loc_1));
              stack.push(StackItem::Host(loc_1, mana));
            }
            APP => {
              let loc_0 = get_loc(norm, 0);
              let loc_1 = get_loc(norm, 1);
              stack.push(StackItem::LinkResolver(loc_1));
              stack.push(StackItem::Host(loc_1, mana));
              stack.push(StackItem::LinkResolver(loc_0));
              stack.push(StackItem::Host(loc_0, mana));
            }
            SUP => {
              let loc_0 = get_loc(norm, 0);
              let loc_1 = get_loc(norm, 1);
              stack.push(StackItem::LinkResolver(loc_1));
              stack.push(StackItem::Host(loc_1, mana));
              stack.push(StackItem::LinkResolver(loc_0));
              stack.push(StackItem::Host(loc_0, mana));
            }
            DP0 => {
              let loc_2 = get_loc(norm, 2);
              stack.push(StackItem::LinkResolver(loc_2));
              stack.push(StackItem::Host(loc_2, mana));
            }
            DP1 => {
              let loc_2 = get_loc(norm, 2);
              stack.push(StackItem::LinkResolver(loc_2));
              stack.push(StackItem::Host(loc_2, mana));
            }
            CTR | FUN => {
              let name = Name::new_unsafe(get_ext(norm));
              let arity = rt.get_arity(&name).ok_or_else(|| RuntimeError::CtrOrFunNotDefined { name })?;
              for i in (0..arity).rev() {
                let loc_i = get_loc(norm, i);
                stack.push(StackItem::LinkResolver(loc_i));
                stack.push(StackItem::Host(loc_i, mana));
              }
            }
            _ => {}
          };
          output.push(Some(norm));
        }
      },
      StackItem::LinkResolver(loc) => {
        match output.pop() {
          Some(lnk) => {
            if let Some(lnk) = lnk {
              link(rt, loc, lnk);
            }
          }
          None => panic!("No term to resolve link"),
        }
      }
    }
  }
  // FIXME: is this always safe? if no, create a runtime error for what could go wrong
  Ok(output.pop().unwrap().unwrap())
}

// Debug
// -----

pub fn show_ptr(x: RawCell) -> String {
  if x == RawCell(0) {
    String::from("~")
  } else {
    let tag = get_tag(x);
    let ext = get_ext(x);
    let val = get_val(x);
    let tgs = match tag {
      DP0 => "DP0",
      DP1 => "DP1",
      VAR => "VAR",
      ARG => "ARG",
      ERA => "ERA",
      LAM => "LAM",
      APP => "APP",
      SUP => "SUP",
      CTR => "CTR",
      FUN => "FUN",
      OP2 => "OP2",
      NUM => "NUM",
      _   => "?",
    };
    let name = Name::new_unsafe(ext);
    format!("{}:{}:{:x}", tgs, name, val)
  }
}

pub fn show_rt(rt: &Runtime) -> String {
  let mut s: String = String::new();
  for i in 0..32 {
    // pushes to the string
    write!(s, "{:x} | ", i).unwrap();
    s.push_str(&show_ptr(rt.read(Loc(i))));
    s.push('\n');
  }
  s
}

fn show_memo(rt: &Runtime) -> String {
  let mut txt = String::new();
  for i in 0 .. rt.get_mcap() {
    txt.push(if rt.read(Loc(i)) == RawCell(0) { '_' } else { 'X' });
  }
  return txt;
}

// TODO: this should be replaced by readback + view_term
pub fn show_term(rt: &Runtime, term: RawCell, focus: Option<RawCell>) -> String {
  enum StackItem {
    Term(RawCell),
    Str(String),
  }
  let mut names: HashMap<Loc, String> = HashMap::new();
  fn find_lets(
    rt: &Runtime,
    term: RawCell,
    names: &mut HashMap<Loc, String>,
    focus: Option<RawCell>
  ) -> String {
    let mut lets: HashMap<Loc, Loc> = HashMap::new();
    let mut kinds: HashMap<Loc, u128> = HashMap::new();
    let mut count: u128 = 0;
    let mut stack = vec![term];
    let mut text = String::new();
    while !stack.is_empty() { 
      let term = stack.pop().unwrap();
      match get_tag(term) {
        LAM => {
          names.insert(get_loc(term, 0), format!("{}", count));
          count += 1;
          stack.push(ask_arg(rt, term, 1));
        }
        APP => {
          stack.push(ask_arg(rt, term, 1));
          stack.push(ask_arg(rt, term, 0));
        }
        SUP => {
          stack.push(ask_arg(rt, term, 1));
          stack.push(ask_arg(rt, term, 0));
        }
        DP0 => {
          if let hash_map::Entry::Vacant(e) = lets.entry(get_loc(term, 0)) {
            names.insert(get_loc(term, 0), format!("{}", count));
            count += 1;
            kinds.insert(get_loc(term, 0), get_ext(term));
            e.insert(get_loc(term, 0));
            stack.push(ask_arg(rt, term, 2));
          }
        }
        DP1 => {
          if let hash_map::Entry::Vacant(e) = lets.entry(get_loc(term, 0)) {
            names.insert(get_loc(term, 0), format!("{}", count));
            count += 1;
            kinds.insert(get_loc(term, 0), get_ext(term));
            e.insert(get_loc(term, 0));
            stack.push(ask_arg(rt, term, 2));
          }
        }
        OP2 => {
          stack.push(ask_arg(rt, term, 1));
          stack.push(ask_arg(rt, term, 0));
        }
        CTR | FUN => {
          let name = Name::new_unsafe(get_ext(term));
          let arity = rt.get_arity(&name).unwrap();
          // NOTE: arity should never be None (read from memory), should panic
          // TODO: remove unwrap?
          for i in (0..arity).rev() {
            stack.push(ask_arg(rt, term, i));
          }
        }
        _ => {}
      }
    }

    for (_key, pos) in lets {
      // todo: reverse
      let what = String::from("?h");
      //let kind = kinds.get(&key).unwrap_or(&0);
      let name = names.get(&pos).unwrap_or(&what);
      let nam0 = if ask_lnk(rt, pos + 0) == Era() { String::from("*") } else { format!("a{}", name) };
      let nam1 = if ask_lnk(rt, pos + 1) == Era() { String::from("*") } else { format!("b{}", name) };
      writeln!(text, "dup {} {} = {};\n", nam0, nam1, go(rt, ask_lnk(rt, pos + 2), &names, focus)).unwrap();
    }
    text
  }

  fn go(rt: &Runtime, term: RawCell, names: &HashMap<Loc, String>, focus: Option<RawCell>) -> String {
    let mut stack = vec![StackItem::Term(term)];
    let mut output = Vec::new();
    while !stack.is_empty() {
      let item = stack.pop().unwrap();
      match item {
        StackItem::Str(txt) => {
          output.push(txt);
        },
        StackItem::Term(term) => {
          if let Some(focus) = focus {
            if focus == term {
              output.push("$".to_string());
            }
          }
          match get_tag(term) {
            DP0 => {
              output.push(format!("a{}", names.get(&get_loc(term, 0)).unwrap_or(&String::from("?a"))));
            }
            DP1 => {
              output.push(format!("b{}", names.get(&get_loc(term, 0)).unwrap_or(&String::from("?b"))));
            }
            VAR => {
              output.push(format!("x{}", names.get(&get_loc(term, 0)).unwrap_or(&String::from("?c"))));
            }
            LAM => {
              let name = format!("x{}", names.get(&get_loc(term, 0)).unwrap_or(&String::from("?")));
              output.push(format!("@{}", name));
              stack.push(StackItem::Term(ask_arg(rt, term, 1)));
            }
            APP => {
              output.push("(".to_string());
              stack.push(StackItem::Str(")".to_string()));
              stack.push(StackItem::Term(ask_arg(rt, term, 1)));
              stack.push(StackItem::Str(" ".to_string()));
              stack.push(StackItem::Term(ask_arg(rt, term, 0)));
            }
            SUP => {
              output.push("{".to_string());
              stack.push(StackItem::Str("}".to_string()));
              //let kind = get_ext(term);
              stack.push(StackItem::Term(ask_arg(rt, term, 1)));
              stack.push(StackItem::Str(" ".to_string()));
              stack.push(StackItem::Term(ask_arg(rt, term, 0)));
            }
            OP2 => {
              let oper = get_ext(term).try_into().unwrap();
              let symb = match oper {
                Oper::Add => "+",
                Oper::Sub => "-",
                Oper::Mul => "*",
                Oper::Div => "/",
                Oper::Mod => "%",
                Oper::And => "&",
                Oper::Or  => "|",
                Oper::Xor => "^",
                Oper::Shl => "<<",
                Oper::Shr => ">>",
                Oper::Ltn => "<",
                Oper::Lte => "<=",
                Oper::Eql => "=",
                Oper::Gte => ">=",
                Oper::Gtn => ">",
                Oper::Neq => "!=",
              };
              output.push(format!("({} ", symb));
              stack.push(StackItem::Str(")".to_string()));
              stack.push(StackItem::Term(ask_arg(rt, term, 1)));
              stack.push(StackItem::Str(" ".to_string()));
              stack.push(StackItem::Term(ask_arg(rt, term, 0)));
            }
            NUM => {
              let numb = get_num(term);
              output.push(format!("#{}", numb));
            }
            CTR => {
              let name = Name::new_unsafe(get_ext(term));
              let mut arit = rt.get_arity(&name).unwrap();
              // NOTE: arity should never be zero (read from memory)
              // TODO: remove unwrap
              let mut name = view_name(name);
              // Pretty print names
              if name == "Name" && arit == 1 {
                let arg = ask_arg(rt, term, 0);
                if get_tag(arg) == NUM {
                  let sugar: Name = get_num(arg).into();
                  name = format!("Name '{}'", sugar);
                  arit = 0; // erase arit to avoid for
                }
              }
              output.push(format!("{{{}", name));
              stack.push(StackItem::Str("}".to_string()));

              for i in (0..arit).rev() {
                stack.push(StackItem::Term(ask_arg(rt, term, i)));
                stack.push(StackItem::Str(" ".to_string()));

              }
            }
            FUN => {
              let name = Name::new_unsafe(get_ext(term));
              output.push(format!("({}", name));
              stack.push(StackItem::Str(")".to_string()));
              let arit = rt.get_arity(&name).unwrap();
              for i in (0..arit).rev() {
                stack.push(StackItem::Term(ask_arg(rt, term, i)));
                stack.push(StackItem::Str(" ".to_string()));
              }
            }
            ERA => {
              output.push(String::from("*"));
            }
            _ => {
              // println!("{}", show_ptr(term));
              // println!("{}", show_term(rt,  ask_lnk(rt, term), None));
              output.push(format!("?g({})", get_tag(term)))
            },
          }
        }
      }
    }

    let res = output.join("");
    return res;

  }

  let mut text = find_lets(rt, term, &mut names, focus);
  text.push_str( &go(rt, term, &names, focus));
  text
}


fn show_runtime_error(err: RuntimeError) -> String {
  match err {
    RuntimeError::NotEnoughMana => "Not enough mana".to_string(),
    RuntimeError::NotEnoughSpace => "Not enough space".to_string(),
    RuntimeError::DivisionByZero => "Tried to divide by zero".to_string(),
    RuntimeError::TermExceedsMaxDepth => "Term exceeds maximum depth.".to_string(),
    RuntimeError::UnboundVar { name } => format!("Unbound variable '{}'.", name),
    RuntimeError::TermIsInvalidNumber { term } => format!("'{}' is not a number.", show_ptr(term)),
    RuntimeError::CtrOrFunNotDefined { name } => format!("'{}' is not defined.", name),
    RuntimeError::StmtDoesntExist { stmt_index } => format!("Statement with index '{}' does not exist.", stmt_index),
    RuntimeError::ArityMismatch { name, expected, got } => format!("Arity mismatch for '{}': expected {} args, got {}.", name, expected, got),
    RuntimeError::NameTooBig { numb } => format!("Cannot fit '{}' into a function name.", numb),
    RuntimeError::TermIsNotLinear { term, var } => format!("'{}' is not linear: '{}' is used more than once.", view_term(&term), var),
    RuntimeError::EffectFailure(effect_failure) =>
      match effect_failure {
        EffectFailure::NoSuchState { state: addr } => format!("Tried to read state of '{}' but did not exist.", show_addr(addr)),
        EffectFailure::InvalidCallArg { caller, callee, arg } => {
          let pos = get_val(arg);
          format!("'{}' tried to call '{}' with invalid argument '{}'.", show_addr(caller), show_addr(callee), show_ptr(arg))
        },
        EffectFailure::InvalidIOCtr { name } => format!("'{}' is not an IO constructor.", name),
        EffectFailure::InvalidIONonCtr { ptr } => format!("'{}' is not an IO term.", show_ptr(ptr)),
    }
  RuntimeError::DefinitionError(def_error) =>
      match def_error {
        DefinitionError::FunctionHasNoRules => "Function has no rules.".to_string(),
        DefinitionError::LHSIsNotAFunction => "Left hand side of function definition is not a function".to_string(),
        DefinitionError::LHSArityMismatch { rule_index, expected, got } => format!("Arity mismatch at left-hand side of rule {}: expected {} arguments but got {}.", rule_index, expected, got),
        DefinitionError::LHSNotConstructor { rule_index } => format!("Left-hand side of rule {} is not a constructor.", rule_index),
        DefinitionError::VarIsUsedTwiceInDefinition { name, rule_index } => format!("'{}' is used twice in left-hand side of rule.", name),
        DefinitionError::VarIsNotLinearInBody { name, rule_index } => format!("'{}' is not used linearly in body, in rule '{}'", name, rule_index),
        DefinitionError::VarIsNotUsed { name, rule_index } => format!("'{}' is not used in rule {}.", name, rule_index),
        DefinitionError::NestedMatch { rule_index } => format!("Nested pattern matching is not supported (at rule {}).", rule_index),
        DefinitionError::UnsupportedMatch { rule_index } => format!("Unsupported match in rule {}. Only constructor, variable and number pattern matching are supported.", rule_index),
      }
  }
}

pub fn readback_term(rt: &Runtime, term: RawCell, limit:Option<usize>) -> Option<Term> {
  fn find_names(rt: &Runtime, term: RawCell, names: &mut LocMap<String>) {
    let mut stack = vec![term];
    while !stack.is_empty() {
      let term = stack.pop().unwrap();
      match get_tag(term) {
        LAM => {
          let param = ask_arg(rt, term, 0);
          let body = ask_arg(rt, term, 1);
          // TODO ask
          names.insert(get_loc(term, 0), format!("{}", names.len()));
          stack.push(body);
        }
        APP => {
          let lam = ask_arg(rt, term, 0);
          let arg = ask_arg(rt, term, 1);
          stack.push(arg);
          stack.push(lam);
        }
        SUP => {
          let arg0 = ask_arg(rt, term, 0);
          let arg1 = ask_arg(rt, term, 1);
          stack.push(arg1);
          stack.push(arg0);
        }
        DP0 | DP1 => {
          if let hash_map::Entry::Vacant(e) = names.entry(get_loc(term, 0)) {
            names.insert(get_loc(term, 0), format!("{}", names.len()));
            stack.push(ask_arg(rt, term, 2));
          }
        }
        OP2 => {
          let arg0 = ask_arg(rt, term, 0);
          let arg1 = ask_arg(rt, term, 1);
          stack.push(arg1);
          stack.push(arg0);
        }
        NUM => {}
        CTR | FUN => {
          let name = Name::new_unsafe(get_ext(term));
          let arity = rt.get_arity(&name).unwrap();
          // NOTE: should never be None, should panic.
          // TODO: remove unwrap?
          for i in (0..arity).rev() {
            let arg = ask_arg(rt, term, i);
            stack.push(arg);
          }
        }
        _ => {}
      }
    }
 }

  struct DupStore {
    stacks: HashMap<u128, Vec<bool>>, // ext -> bool
  }

  impl DupStore {
    fn new() -> DupStore {
      DupStore { stacks: HashMap::new() }
    }
    fn get(&self, col: u128) -> Option<&Vec<bool>> {
      self.stacks.get(&col)
    }
    fn pop(&mut self, col: u128) -> bool {
      let stack = self.stacks.entry(col).or_insert_with(Vec::new);
      stack.pop().unwrap_or(false)
    }
    fn push(&mut self, col: u128, val: bool) {
      let stack = self.stacks.entry(col).or_insert_with(Vec::new);
      stack.push(val);
    }
  }

  fn readback(
    rt: &Runtime,
    term: RawCell,
    names: &mut LocMap<String>,
    seen: &mut HashSet<RawCell>,
    dup_store: &mut DupStore,
    limit: Option<usize>
  ) -> Option<Term> {
    enum StackItem {
      Term(RawCell),
      Resolver(RawCell),
      SUPResolverSome(RawCell, bool), // auxiliar case when in SUP does not have a DUP to evaluate
      SUPResolverNone(RawCell), // auxiliar case when in SUP does not have a DUP to evaluate
    }

    let mut output = Vec::new();
    let mut stack = vec![StackItem::Term(term)];

    let print_stack = |stack: &Vec<StackItem>| {
      println!("Stack: ");
      for (i, item) in stack.iter().rev().enumerate() {
        let (prefix, term) = match item {
          StackItem::Term(term) => ("term", term),
          StackItem::Resolver(term) => ("resolver", term),
          StackItem::SUPResolverSome(term, ..) => ("sup some", term),
          StackItem::SUPResolverNone(term) => ("sup none", term),
        };
        if i == 0 {
          println!("{} {}", prefix, show_term(rt, *term, None));
        } else {
          println!("{} {}", prefix, **term);
        }
      }
    };

    let print_output = |output: &Vec<Term>| {
      println!("Output: ");
      for item in output.iter().rev() {
        println!("{}", item);
      }
    };

    let mut count = 0;
    while let Some(item) = stack.pop()  {
      if let Some(limit) = limit {
        if count == limit {
          return None;
        }
        else {
          count += 1;
        }
      }
      match item {
        StackItem::Term(term) => {
          debug_assert!(term != RawCell(0));
          match get_tag(term) {
            DP0 | DP1 => {
              if !seen.contains(&term) { // this avoids looping when term doesnt exist
                seen.insert(term);
                let col = get_ext(term);
                let val = ask_arg(rt, term, 2);
                if get_tag(term) == DP0 {
                  dup_store.push(col, false);
                } else {
                  dup_store.push(col, true);
                }
                stack.push(StackItem::Resolver(term));
                stack.push(StackItem::Term(val));
              }
            }
            SUP => {
              let col = get_ext(term);
              let empty = &Vec::new();
              let dup_stack = dup_store.get(col).unwrap_or(empty);
              if let Some(val) = dup_stack.last() {
                let arg_idx = *val as u64;
                let val = ask_arg(rt, term, arg_idx);
                let old = dup_store.pop(col);
                stack.push(StackItem::SUPResolverSome(term, old));
                stack.push(StackItem::Term(val));
                // let got = readback(rt, val, names, dup_store);
              } else {
                let val0 = ask_arg(rt, term, 0);
                let val1 = ask_arg(rt, term, 1);
                stack.push(StackItem::SUPResolverNone(term));
                stack.push(StackItem::Term(val1));
                stack.push(StackItem::Term(val0));
                // let val0 = readback(rt, val0, names, dup_store);
                // let val1 = readback(rt, val1, names, dup_store);
              }
            }
            VAR => {
              let name = &format!("x{}", names.get(&get_loc(term, 0)).unwrap_or(&String::from("_")));
              let name: Name = (name as &str).try_into().unwrap(); 
              output.push(Term::var(name));
            }
            NUM => {
              let numb = get_num(term);
              output.push(Term::num(numb));
            }
            OP2 => {
              stack.push(StackItem::Resolver(term));
              stack.push(StackItem::Term(ask_arg(rt, term, 1)));
              stack.push(StackItem::Term(ask_arg(rt, term, 0)));
            }
            CTR | FUN => {
              let name = Name::new_unsafe(get_ext(term));
              let arit = rt.get_arity(&name).unwrap();
              // NOTE: arity cant be None, should panic
              // TODO: remove unwrap?
              stack.push(StackItem::Resolver(term));
              for i in 0..arit {
                stack.push(StackItem::Term(ask_arg(rt, term, i)));
              }
            }
            LAM => {
              stack.push(StackItem::Resolver(term));
              stack.push(StackItem::Term(ask_arg(rt, term, 1)));
            }
            APP => {
              stack.push(StackItem::Resolver(term));
              stack.push(StackItem::Term(ask_arg(rt, term, 1)));
              stack.push(StackItem::Term(ask_arg(rt, term, 0)));
            }
            _ => {}
          }
        }
        StackItem::SUPResolverSome(term, old) => {
          let col = get_ext(term); 
          dup_store.push(col, old);
        }
        StackItem::SUPResolverNone(term) => {
                    // TODO: check if this should really be here. is it necessary?
          let name = "HVM.sup"; // lang::Term doesn't have a Sup variant
          let name = name.try_into().unwrap();
          let val0 = output.pop().unwrap();
          let val1 = output.pop().unwrap();
          let args = vec![val0, val1];
          return Some(Term::ctr(name, args));
        }
        StackItem::Resolver(term) => {
          match get_tag(term) {
            DP0 | DP1 => {
              let col = get_ext(term);
              dup_store.pop(col);
            }
            CTR | FUN => {
              let name = Name::new_unsafe(get_ext(term));
              let arit = rt.get_arity(&name).unwrap();
              let mut args = Vec::new();
              for i in 0..arit {
                args.push(output.pop().unwrap());
              }
              if get_tag(term) == CTR {
                output.push(Term::ctr(name, args));
              } else {
                output.push(Term::fun(name, args));
              }
            },
            LAM => {
              let name = format!("x{}", names.get(&get_loc(term, 0)).unwrap_or(&String::from("_")));
              let name = Name::from_str(&name).unwrap();
              let body = Box::new(output.pop().unwrap());
              output.push(Term::lam(name, body));
            }
            APP => {
              let argm = Box::new(output.pop().unwrap());
              let func = Box::new(output.pop().unwrap());
              output.push(Term::app(func, argm));
            }
            OP2 => {
              let oper = get_ext(term);
              let oper = oper.try_into().unwrap();
              let val1 = Box::new(output.pop().unwrap());
              let val0 = Box::new(output.pop().unwrap());
              output.push(Term::op2(oper, val0, val1))
            }
            _ => panic!("Term not valid in readback"),
          }
        }
      }
    }
    if let Some(item) = output.pop() {
          Some(item)
    }
    else {
            panic!("Readback output is empty")
    }
  }

  let mut names: LocMap<String> = init_loc_map();
  let mut seen: HashSet<RawCell> = HashSet::new();
  let mut dup_store = DupStore::new();
  find_names(rt, term, &mut names);
  readback(rt, term, &mut names, &mut seen, &mut dup_store, limit)
}

// Parsing
// -------

fn head(code: &str) -> char {
  return code.chars().take(1).last().unwrap_or('\0');
}

fn tail(code: &str) -> &str {
  if code.len() > 0 {
    return &code[head(code).len_utf8()..];
  } else {
    return "";
  }
}

fn drop(code: &str, amount: u128) -> &str {
  let mut code = code;
  for _ in 0 .. amount {
    code = tail(code);
  }
  return code;
}

fn nth(code: &str, index: u128) -> char {
  return head(drop(code, index));
}

fn skip(code: &str) -> &str {
  let mut code = code;
  loop {
    if " \n\r\t".contains(head(code)) {
      while " \n\r\t".contains(head(code)) {
        code = tail(code);
      }
      continue;
    }
    if head(code) == '/' && nth(code,1) == '/' {
      while head(code) != '\n' && head(code) != '\0' {
        code = tail(code);
      }
      continue;
    }
    break;
  }
  return code;
}

fn hash(name: &str) -> u128 {
  let mut hasher = hash_map::DefaultHasher::new();
  name.hash(&mut hasher);
  hasher.finish() as u128
}

fn is_name_char(chr: char) -> bool {
  return chr == '_' || chr == '.'
      || chr >= 'a' && chr <= 'z'
      || chr >= 'A' && chr <= 'Z'
      || chr >= '0' && chr <= '9';
}

pub fn read_char(code: &str, chr: char) -> ParseResult<()> {
  let code = skip(code);
  if head(code) == chr {
    Ok((tail(code), ()))
  } else {
    Err(ParseErr {
      erro: format!("Expected '{}', found '{}'. Context:\n\x1b[2m{}\x1b[0m", chr, head(code), code.chars().take(64).collect::<String>()),
      code: code.to_string(),
    })
  }
}

pub fn read_numb<T>(code: &str) -> ParseResult<T>
where
  T: TryFrom<u128, Error = String>,
{
  let mut code = skip(code);
  let (code, numb) = {
    if head(code) == 'x' {
      code = tail(code);
      let mut numb = 0;
      let mut code = code;
      let mut digits = 0; // this will be used to prevent number overflow panicking
      loop {
        if head(code) >= '0' && head(code) <= '9' {
          numb = numb * 16 + head(code) as u128 - 0x30;
          code = tail(code);
        } else if head(code) >= 'a' && head(code) <= 'f' {
          numb = numb * 16 + head(code) as u128 - 0x61 + 10;
          code = tail(code);
        } else if head(code) >= 'A' && head(code) <= 'F' {
          numb = numb * 16 + head(code) as u128 - 0x41 + 10;
          code = tail(code);
        } else {
          break;
        }
        digits += 1;
        if digits > 30 {
          return Err(ParseErr::new(code, "Hexadecimal number with more than 30 digits"))
        }
      }
      (code, numb)
    } else {
      let mut numb = 0;
      while head(code) >= '0' && head(code) <= '9' {
        numb = numb * 10 + head(code) as u128 - 0x30;
        code = tail(code);
      }
      (code, numb)
    }
  };
  let numb: T = numb.try_into().map_err(|err| ParseErr::new(code, err))?;
  Ok((code, numb))
}

pub fn read_name(code: &str) -> ParseResult<Name> {
  let code = skip(code);
  let mut name = String::new();
  if head(code) == '~' {
    return Ok((tail(code), Name::NONE));
  } else {
    let mut code = code;
    while is_name_char(head(code)) {
      name.push(head(code));
      code = tail(code);
    }
    if name.is_empty() {
      return Err(ParseErr {
        code: code.to_string(),
        erro: format!("Expected identifier, found `{}`.", head(code))
      });
    }
    if name == "ask" || name == "dup" || name == "let" {
      return Err(ParseErr {
        code: code.to_string(),
        erro: format!("Use of the keyword {} as a name for a term in `{}`.", name, code)
      });
    }
    if ('0'..='9').contains(&name.chars().nth(0).unwrap_or(' ')) {
      // In most cases, an user writing 0-9 probably wants to make a number, not a name, so, to
      // avoid mistakes, we disable this syntax by default. But since names CAN start with 0-9 on
      // Kindelia, we must create an alternative, explicit way to parse "numeric names".
      return Err(ParseErr {
        code: code.to_string(),
        erro: format!("Number must start with #, but '{}' doesn't.", name),
      });
    }
    let name = Name::from_str(&name);
    let name =
      match name {
        Ok(name) => name,
        Err(msg) => {
          return Err(ParseErr {
            code: code.to_string(),
            erro: format!("Identifier too long: {}", msg),
          });
        }
      };
    return Ok((code, name));
  }
}

pub fn read_hex(code: &str) -> ParseResult<Vec<u8>> {
  let mut data : Vec<u8> = Vec::new();
  let mut code = skip(code);
  while nth(code,0).is_ascii_hexdigit() && nth(code,1).is_ascii_hexdigit() {
    data.append(&mut hex::decode(&String::from_iter([nth(code,0),nth(code,1)])).unwrap());
    code = drop(code, 2);
    code = skip(code);
  }
  return Ok((code, data));
}

pub fn read_until<A>(code: &str, stop: char, read: fn(&str) -> ParseResult<A>) -> ParseResult<Vec<A>> {
  let mut elems = Vec::new();
  let mut code = code;
  while code.len() > 0 && head(skip(code)) != stop {
    let (new_code, elem) = read(code)?;
    code = new_code;
    elems.push(elem);
  }
  code = tail(skip(code));
  return Ok((code, elems));
}

pub fn read_term(code: &str) -> ParseResult<Term> {
  let code = skip(code);
  match head(code) {
    '@' => {
      let code         = tail(code);
      let (code, name) = read_name(code)?;
      let (code, body) = read_term(code)?;
      let term = Term::lam(name, Box::new(body));
      return Ok((code, term));
    },
    '(' => {
      let code = skip(tail(code));
      let (code, oper) = read_oper(code);
      if let Some(oper) = oper {
        let (code, val0) = read_term(code)?;
        let (code, val1) = read_term(code)?;
        let (code, unit) = read_char(code, ')')?;
        let term = Term::op2(oper, Box::new(val0), Box::new(val1));
        return Ok((code, term));
      } else if head(code) == '!' {
        let code = tail(code);
        let (code, func) = read_term(code)?;
        let (code, argm) = read_term(code)?;
        let (code, unit) = read_char(code, ')')?;
        let term = Term::app(Box::new(func), Box::new(argm));
        return Ok((code, term));
      } else if ('A'..='Z').contains(&head(code)) {
        let (code, name) = read_name(code)?;
        let (code, args) = read_until(code, ')', read_term)?;
        let term = Term::fun(name, args);
        return Ok((code, term));
      } else {
        let (code, func) = read_term(code)?;
        let (code, argm) = read_term(code)?;
        let (code, unit) = read_char(code, ')')?;
        let term = Term::app(Box::new(func), Box::new(argm));
        return Ok((code, term));
      }
    },
    '{' => {
      let code = tail(code);
      let (code, name) = read_name(code)?;
      let (code, args) = read_until(code, '}', read_term)?;
      let term = Term::ctr(name, args);
      return Ok((code, term));
    },
    '[' => {
      let code = tail(code);
      let (code, vals) = read_until(code, ']', read_term)?;
      let num_vals = vals.len();
      if num_vals <= 12 {
        let name = Name::from_str(&format!("T{}", num_vals)).unwrap();
        let term = Term::ctr(name, vals);
        return Ok((code, term));
      } else {
        return Err(ParseErr { code: code.to_string(), erro: "Tuple too long".to_string() });
      }
    },
    '#' => {
      let code = tail(code);
      let (code, numb) = read_numb(code)?;
      let term = Term::num(numb);
      return Ok((code, term));
    },
    '\'' => {
      let code = tail(code);
      let (code, name) = read_name(code)?;
      let (code, unit) = read_char(code, '\'')?;
      let numb = *name;
      let numb: U120 = numb.try_into().map_err(|erro| ParseErr::new(code, erro))?;
      let term = Term::num(numb);
      return Ok((code, term));
    },
    _ => {
      if let ('d','u','p',' ') = (nth(code,0), nth(code,1), nth(code,2), nth(code,3)) {
        let code = drop(code,3);
        let (code, nam0) = read_name(code)?;
        let (code, nam1) = read_name(code)?;
        let (code, unit) = read_char(code, '=')?;
        let (code, expr) = read_term(code)?;
        let (code, unit) = read_char(code, ';')?;
        let (code, body) = read_term(code)?;
        let term = Term::dup(nam0, nam1, Box::new(expr), Box::new(body));
        return Ok((code, term));
      // let x = y; z
      // ------------
      // (@x z y)
      } else if let ('l','e','t',' ') = (nth(code,0), nth(code,1), nth(code,2), nth(code,3)) {
        let code = drop(code,3);
        let (code, name) = read_name(code)?;
        let (code, unit) = read_char(code, '=')?;
        let (code, expr) = read_term(code)?;
        let (code, unit) = read_char(code, ';')?;
        let (code, body) = read_term(code)?;
        let lam = Term::lam(name, Box::new(body));
        let app = Term::app(Box::new(lam), Box::new(expr));
        return Ok((code, app));
      // ask x = y; z
      // ------------
      // (y @x z)
      } else if let ('a','s','k',' ') = (nth(code,0), nth(code,1), nth(code,2), nth(code,3)) {
        let code = skip(drop(code,3));
        if nth(code,0) == '(' {
          let (code, expr) = read_term(code)?;
          let (code, unit) = read_char(code, ';')?;
          let (code, body) = read_term(code)?;
          let argm = Term::lam(Name::NONE, Box::new(body));
          let term = Term::app(Box::new(expr), Box::new(argm));
          return Ok((code, term));
        } else {
          let (code, name) = read_name(code)?;
          let (code, unit) = read_char(code, '=')?;
          let (code, expr) = read_term(code)?;
          let (code, unit) = read_char(code, ';')?;
          let (code, body) = read_term(code)?;
          let argm = Term::lam(name, Box::new(body));
          let term = Term::app(Box::new(expr), Box::new(argm));
          return Ok((code, term));
        }
      } else {
        let (code, name) = read_name(code)?;
        let term = Term::var(name);
        return Ok((code, term));
      }
    }
  }
}

pub fn read_oper(in_code: &str) -> (&str, Option<Oper>) {
  let code = skip(in_code);
  match head(code) {
    // Should not match with `~`
    '+' => (tail(code), Some(Oper::Add)),
    '-' => (tail(code), Some(Oper::Sub)),
    '*' => (tail(code), Some(Oper::Mul)),
    '/' => (tail(code), Some(Oper::Div)),
    '%' => (tail(code), Some(Oper::Mod)),
    '&' => (tail(code), Some(Oper::And)),
    '|' => (tail(code), Some(Oper::Or)),
    '^' => (tail(code), Some(Oper::Xor)),
    '<' => match head(tail(code)) {
      '=' => (tail(tail(code)), Some(Oper::Lte)),
      '<' => (tail(tail(code)), Some(Oper::Shl)),
      _   => (tail(code), Some(Oper::Ltn)),
    },
    '>' => match head(tail(code)) {
      '=' => (tail(tail(code)), Some(Oper::Gte)),
      '>' => (tail(tail(code)), Some(Oper::Shr)),
      _   => (tail(code), Some(Oper::Gtn)),
    },
    '=' => match head(tail(code)) {
      '=' => (tail(tail(code)), Some(Oper::Eql)),
      _   => (code, None),
    },
    '!' => match head(tail(code)) {
      '=' => (tail(tail(code)), Some(Oper::Neq)),
      _   => (code, None),
    },
    _ => (code, None),
  }
}

pub fn read_rule(code: &str) -> ParseResult<Rule> {
  // TODO: custom parser for lhs
  let (code, lhs) = read_term(code)?;
  let (code, ())  = read_char(code, '=')?;
  let (code, rhs) = read_term(code)?;
  return Ok((code, Rule{lhs, rhs}));
}

pub fn read_rules(code: &str) -> ParseResult<Vec<Rule>> {
  let (code, rules) = read_until(code, '\0', read_rule)?;
  return Ok((code, rules));
}

pub fn read_func(code: &str) -> ParseResult<CompFunc> {
  let (code, rules) = read_until(code, '\0', read_rule)?;
  let func = Func { rules };
  let comp_func = compile_func(&func, false);
  match comp_func {
    Ok(func) => Ok((code, func)),
    Err(def_err) => Err(ParseErr {
      code: code.to_string(),
      erro: show_runtime_error(def_err)
    })
  }
}

pub fn read_sign(code: &str) -> ParseResult<Option<crypto::Signature>> {
  let code = skip(code);
  if let ('s','i','g','n') = (nth(code,0), nth(code,1), nth(code,2), nth(code,3)) {
    let code = drop(code,4);
    let (code, unit) = read_char(code, '{')?;
    let (code, sign) = read_hex(code)?;
    let (code, unit) = read_char(code, '}')?;
    if sign.len() == 65 {
      return Ok((code, Some(crypto::Signature(sign.as_slice().try_into().unwrap())))); // TODO: remove unwrap
    } else {
      return Err(ParseErr { 
        code: code.to_string(), 
        erro: "Wrong signature size".to_string()
      });
    }
  }
  return Ok((code, None));
}

pub fn read_statement(code: &str) -> ParseResult<Statement> {
  let code = skip(code);
  match (nth(code,0), nth(code,1), nth(code,2)) {
    ('f','u','n') => {
      let code = drop(code,3);
      let (code, unit) = read_char(code, '(')?;
      let (code, name) = read_name(code)?;
      let (code, args) = read_until(code, ')', read_name)?;
      let (code, unit) = read_char(code, '{')?;
      let (code, ruls) = read_until(code, '}', read_rule)?;
      let code = skip(code);
      let (code, init) = if let ('w','i','t','h') = (nth(code,0), nth(code,1), nth(code,2), nth(code,3)) {
        let code = drop(code,4);
        let (code, unit) = read_char(code, '{')?;
        let (code, init) = read_term(code)?;
        let (code, unit) = read_char(code, '}')?;
        (code, Some(init))
      } else {
        (code, None)
      };
      let (code, sign) = read_sign(code)?;
      let func = Func { rules: ruls };
      return Ok((code, Statement::Fun { name, args, func, init, sign }));
    }
    ('c','t','r') => {
      let code = drop(code,3);
      let (code, unit) = read_char(code, '{')?;
      let (code, name) = read_name(code)?;
      let (code, args) = read_until(code, '}', read_name)?;
      let (code, sign) = read_sign(code)?;
      return Ok((code, Statement::Ctr { name, args, sign }));
    }
    ('r','u','n') => {
      let code = drop(code,3);
      let (code, unit) = read_char(code, '{')?;
      let (code, expr) = read_term(code)?;
      let (code, unit) = read_char(code, '}')?;
      let (code, sign) = read_sign(code)?;
      return Ok((code, Statement::Run { expr, sign  }));
    }
    // reg Foo.Bar { #x123456 } sign { signature }
    ('r','e','g') => {
      let code = skip(drop(code, 3));
      let (code, name) =
        if nth(code, 0) == '{' {
          (code, Name::EMPTY)
        } else {
          read_name(code)?
        };
      let (code, unit) = read_char(code, '{')?;
      let code = skip(code);
      let (code, ownr) = match head(code) {
        '#' => {
          let code = tail(code);
          read_numb(code)?
        },
        '\'' => {
          let code = tail(code);
          let (code, name) = read_name(code)?;
          let (code, unit) = read_char(code, '\'')?;
          let numb: U120 = name.into();
          (code, numb)
        },
        _ => return Err(ParseErr::new(code, "Expected a number representation"))
      };
      let (code, unit) = read_char(code, '}')?;
      let (code, sign) = read_sign(code)?;
      return Ok((code, Statement::Reg { name, ownr, sign }));
    }
    _ => {
      return Err(ParseErr { code: code.to_string(),  erro: "Expected statement.".to_string() });
    }
  }
}

pub fn read_statements(code: &str) -> ParseResult<Vec<Statement>> {
  read_until(code, '\0', read_statement)
}

pub fn parse_code(code: &str) -> Result<Vec<Statement>, String> {
  let statements = read_statements(code);
  match statements {
    Ok((code, statements)) => {
      if code.is_empty() {
        Ok(statements)
      } else {
        Err(format!("Your code was not parsed entirely: {}", code))
      }
    }
    Err(ParseErr { erro, .. }) => Err(erro),
  }
}

// View
// ----

// TODO: move to Display trait
pub fn view_name(name: Name) -> String {
  if name.is_none() {
    return "~".to_string();
  } else {
    return name.to_string();
  }
}

pub fn view_term(term: &Term) -> String {
  enum StackItem<'a> {
    Term(&'a Term),
    Str(String),
  }

  let mut stack = vec![StackItem::Term(term)];
  let mut output = Vec::new();

  while !stack.is_empty() {
    let item = stack.pop().unwrap();

    match item {
      StackItem::Str(str) => {
        output.push(str);
      }
      StackItem::Term(term) => {  
        match term {
          Term::Var { name } => {
            output.push(view_name(*name));
          }
          Term::Dup { nam0, nam1, expr, body } => {
            output.push("dup ".to_string());
            output.push(view_name(*nam0));
            output.push(" ".to_string());
            output.push(view_name(*nam1));
            output.push(" = ".to_string());
            stack.push(StackItem::Term(body));
            stack.push(StackItem::Str("; ".to_string()));
            stack.push(StackItem::Term(expr));
          }
          Term::Lam { name, body } => {
            output.push(format!("@{} ", view_name(*name)));
            stack.push(StackItem::Term(body));
          }
          Term::App { func, argm } => {
            output.push("(".to_string());
            stack.push(StackItem::Str(")".to_string()));
            stack.push(StackItem::Term(argm));
            stack.push(StackItem::Str(" ".to_string()));
            stack.push(StackItem::Term(func));
          }
          Term::Ctr { name, args } => {
            let name = view_name(*name);
            // Pretty print names
            if name == "Name" && args.len() == 1 {
              if let Term::Num { numb } = args[0] {
                output.push(format!("{{Name '{}'}}", view_name(numb.into())));
              }
            } else {
              output.push("{".to_string());
              output.push(name);
              stack.push(StackItem::Str("}".to_string()));
              for arg in args.iter().rev() {
                stack.push(StackItem::Term(arg));
                stack.push(StackItem::Str(" ".to_string()));
              }
            }
          }
          Term::Fun { name, args } => {
            let name = view_name(*name);
            output.push("(".to_string());
            output.push(name);
            stack.push(StackItem::Str(")".to_string()));
            for arg in args.iter().rev() {
              stack.push(StackItem::Term(arg));
              stack.push(StackItem::Str(" ".to_string()));
            }
          }
          Term::Num { numb } => {
            // If it has 26-30 bits, pretty-print as a name
            //if *numb > 0x3FFFFFF && *numb <= 0x3FFFFFFF {
              //return format!("@{}", view_name(*numb));
            //} else {
              output.push(format!("#{}", numb));
            //}
          }
          Term::Op2 { oper, val0, val1 } => {
            let oper = view_oper(oper);
            output.push(format!("({} ", oper));
            stack.push(StackItem::Str(")".to_string()));
            stack.push(StackItem::Term(val1));
            stack.push(StackItem::Str(" ".to_string()));
            stack.push(StackItem::Term(val0));
          }
        }
      }
    }
  }
  let res = output.join("");
  res
}

pub fn view_oper(oper: &Oper) -> String {
  match oper {
    Oper::Add => "+",
    Oper::Sub => "-",
    Oper::Mul => "*",
    Oper::Div => "/",
    Oper::Mod => "%",
    Oper::And => "&",
    Oper::Or  => "|",
    Oper::Xor => "^",
    Oper::Shl => "<<",
    Oper::Shr => ">>",
    Oper::Ltn => "<",
    Oper::Lte => "<=",
    Oper::Eql => "==",
    Oper::Gte => ">=",
    Oper::Gtn => ">",
    Oper::Neq => "!=",
  }.to_string()
}

pub fn view_statement(statement: &Statement) -> String {
  fn view_sign(sign: &Option<crypto::Signature>) -> String {
    fn format_sign(sign: &crypto::Signature) -> String {
      let hex = sign.to_hex();
      let mut text = String::new();
      for i in 0 .. 5 {
        text.push_str("  ");
        text.push_str(&hex[i * 26 .. (i+1) * 26]);
        text.push_str("\n");
      }
      return text;
    }
    match sign {
      None       => String::new(),
      Some(sign) => format!(" sign {{\n{}}}", format_sign(sign)),
    }
  }
  match statement {
    Statement::Fun { name, args, func, init, sign } => {
      let func = func.rules.iter().map(|x| format!("\n  {} = {}", view_term(&x.lhs), view_term(&x.rhs)));
      let func = func.collect::<Vec<String>>().join("");
      let args = args.iter().map(|x| x.to_string()).collect::<Vec<String>>().join(" ");
      let init = if let Some(init) = init {
        let init = view_term(init);
        format!(" with {{\n  {}\n}}", init)
      } else {
        "\n".to_string()
      };
      let sign = view_sign(sign);
      return format!("fun ({} {}) {{{}\n}}{}{}", name, args, func, init, sign);
    }
    Statement::Ctr { name, args, sign } => {
      // correct:
      let name = name;
      let args = args.iter().map(|x| format!(" {}", x)).collect::<Vec<String>>().join("");
      let sign = view_sign(sign);
      return format!("ctr {{{}{}}}{}", name, args, sign);
    }
    Statement::Run { expr, sign } => {
      let expr = view_term(expr);
      let sign = view_sign(sign);
      return format!("run {{\n  {}\n}}{}", expr, sign);
    }
    Statement::Reg { name, ownr, sign } => {
      let name = name;
      let ownr = format!("#x{:0>30x}", **ownr);
      let sign = view_sign(sign);
      return format!("reg {} {{ {} }}{}", name, ownr, sign);
    }
  }
}

pub fn view_statements(statements: &[Statement]) -> String {
  let mut result = String::new();
  for statement in statements {
    result.push_str(&view_statement(statement));
    result.push_str("\n");
  }
  return result;
}

impl fmt::Display for Term {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
      write!(f, "{}", view_term(self))
  }
}

impl fmt::Display for Statement {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
      write!(f, "{}", view_statement(self))
  }
}

// Hashing
// -------

pub fn hash_term(term: &Term) -> crypto::Hash {
  crypto::Hash::keccak256_from_bytes(&util::bitvec_to_bytes(&term.proto_serialized()))
}

pub fn hash_statement(statement: &Statement) -> crypto::Hash {
  crypto::Hash::keccak256_from_bytes(&util::bitvec_to_bytes(&remove_sign(&statement).proto_serialized()))
}

// Tests
// -----

// FIXME: since we don't have a proper macro, we're using this temporarily
pub fn print_io_consts() {
  let names = ["done", "take", "save", "call", "subj", "from", "load"];
  for name in names {
    let name = name.to_uppercase();
    let name = Name::from_str(&format!("IO_{}", name)).unwrap();
    let numb = *name;
    println!("const IO_{} : u128 = 0x{:x}; // name_to_u128(\"IO_{}\")", name, numb, name);
  }
  for name in names {
    let name = Name::from_str(&name).unwrap();
    let numb = *name;
    println!("const MC_{} : u128 = 0x{:x}; // name_to_u128(\"{}\")", name.to_string().to_uppercase(), numb, name);
  }
}

// Serializes, deserializes and evaluates statements
pub fn test_statements(statements: &Vec<Statement>, debug: bool) {
  let str_0 = view_statements(statements);
  let str_1 = view_statements(&Vec::proto_deserialized(&statements.proto_serialized()).unwrap());

  println!("Block {}", if str_0 == str_1 { "" } else { "(note: serialization error, please report)" });
  println!("=====");
  println!();

  // TODO: code below does not need heaps_path at all. extract heap persistence out of Runtime.
  let heaps_path = dirs::home_dir().unwrap().join(".kindelia").join("state").join("heaps");
  let genesis_smts = parse_code(constants::GENESIS_CODE).expect("Genesis code parses");
  let mut rt = init_runtime(heaps_path, &genesis_smts);
  let init = Instant::now();
  rt.run_statements(&statements, false, debug);
  println!();

  println!("Stats");
  println!("=====");
  println!();

  println!("[size] {}", rt.get_size());
  println!("[mana] {}", rt.get_mana());
  println!("[rwts] {}", rt.get_rwts());
  println!("[time] {} ms", init.elapsed().as_millis());
}

pub fn test_statements_from_code(code: &str, debug: bool) {
  let statments = read_statements(code);
  match statments {
    Ok((.., statements)) => test_statements(&statements, debug),
    Err(ParseErr { code, erro }) => println!("{}", erro),
  }
}

pub fn test_statements_from_file(file: &str, debug: bool) {
  test_statements_from_code(&std::fs::read_to_string(file).expect("file not found"), debug);
}

// Term Drop implementation
// ========================

// This implementation is necessary as Rust is unable do dealloc deeply nested
// structures without it.
//
// References:
// - https://rust-unofficial.github.io/too-many-lists/first-drop.html
// - https://doc.rust-lang.org/nomicon/destructors.html

// impl Drop for Term {
//     fn drop(&mut self) {
//         todo!()
//     }
// }
