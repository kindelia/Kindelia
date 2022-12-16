//! Welcome to Kindelia's High-order Virtual Machine!
//! =================================================
//!
//! This module is a modification of the project hosted on
//! github.com/Kindelia/HVM, and it makes a series of changes with the goal of
//! serving the requirements of a peer-to-peer computer.
//!
//! How is Kindelia's HVM different from the conventional HVM?
//! ----------------------------------------------------------
//!
//! First, it is a 128-bit, rather than a 64-bit architecture. It can store
//! 120-bit unboxed integers, up from 32-bit unboxed uints stored by the
//! conventional HVM. It allows addressing up to 2^72 function names, up from
//! 2^30 allowed by the conventional HVM, which isn't enough for Kindelia. This
//! change comes with a cost of about ~30% reduced performance, which is
//! acceptable.
//!
//! Second, it implements a reversible heap machinery, which allows saving
//! periodic snapshots of past heap states, and jump back to them. This is
//! necessary because of decentralized consensus. If we couldn't revert to past
//! states, we'd have to recompute the entire history anytime there is a block
//! reorg, which isn't practical. On Ethereum, this is achieved by storing the
//! state as a Map<U256> using Merkle Trees, which, being an immutable
//! structure, allows non-destructive insertions and rollbacks. We could do the
//! same, but we decided to further leverage the HVM by saving its whole heap as
//! the network state. In other words, applications are allowed to persist
//! arbitrary HVM structures on disk by using the io_save operation. For
//! example:
//!
//! ```text
//! (io_save {Cons #1 {Cons #2 {Cons #3 {Nil}}}} ...)
//! ```
//! 
//! The operation above would persist the `[1,2,3]` list as the app's state,
//! with no need for serialization. As such, when the app stops running, that
//! list will not be freed from memory. Instead, the heap will persist between
//! blocks, so the app just needs to store a pointer to the list's head,
//! allowing it to retrieve its state later on. This is only possible because
//! the HVM is garbage-collection free, otherwise, leaks would overwhelm the
//! memory.
//!
//! How are reversible heaps stored?
//! --------------------------------
//!
//! Kindelia's heap is set to grow exactly 8 GB per year. In other words, 10
//! years after the genesis block, the heap size will be of exactly 80 GB. But
//! that doesn't mean a full node will be able to operate with even that much
//! ram, because Kindelia must also save snapshots. Right now, it stores at most
//! 10 snapshots, trying to keep them distributed with exponentially decreasing
//! ages. For example, if we're on block 1000, it might store a snapshot of
//! blocks 998, 996, 992, 984, 968, 872, 744 and 488, which is compatible with
//! the fact that longer-term rollbacks are increasingly unlikely. If there is a
//! rollback to block 990, we just go back to the earliest snapshot, 984, and
//! reprocess blocks 985-1000, which is much faster than recomputing the entire
//! history.
//!
//! In order to keep a good set of snapshots, we must be able to create and
//! discard these heaps. Obviously, if this operation required copying the
//! entire heap buffer every block, it would completely destroy the network's
//! performance. As such, instead, heaps only actually store data that changed.
//! So, using the example above, if a list was allocated and persisted on block
//! 980, it will actually be stored on the snapshot 984, which is the earliest
//! snapshot after 980. If the runtime, now on block 1000, attempts to read the
//! memory where the list is allocated, it will actually receive a signal that
//! it is stored on a past heap, and look for it on 996 and 992, until it is
//! found on block 984.
//!
//! To achieve that, hashmaps are used to store defined functions and persistent
//! state pointers. If a key isn't present, Kindelia will look for it on past
//! snapshots. As for the runtime's memory, where HVM constructors and lambdas
//! are stored, it doesn't use a hashmap. Instead, it uses a Nodes type, which
//! stores data in a big pre-allocated u128 buffer, and keeps track of used
//! memory slots in a separate buffer. We then reserve a constant, U128_NONE, to
//! signal that an index isn't present, and must be found ina  past heap. This
//! is different from 0, which means that this index is empty, and can be
//! allocated. This allows for fast write, read, disposal and merging of heaps,
//! but comes at the cost of wasting a lot of memory. Because of that,
//! Kindelia's current implementation demands up to ~10x more available memory
//! than the current heap size, but that could be reduced ten-fold by replacing
//! vectors by a hashmap, or by just saving fewer past heaps.
//!
//! Other than a 128-bit architecture and reversible heaps, Kindelia's HVM is
//! similar to the conventional HVM.

#![allow(dead_code)]
#![allow(non_snake_case)]
#![allow(unused_variables)]
#![allow(clippy::style)]
#![allow(clippy::identity_op)]

pub mod debug;
pub mod memory;
pub mod costs;
pub mod functions;
pub mod checks;

use std::collections::{hash_map, HashMap, HashSet};
use std::fmt;
use std::fs::File;
use std::hash::BuildHasherDefault;
use std::path::PathBuf;
use std::str::FromStr;
use std::sync::Arc;
use std::time::Instant;

use serde::{Deserialize, Serialize};

use kindelia_common::nohash_hasher::NoHashHasher;
use kindelia_common::{crypto, nohash_hasher, Name, U120};
use kindelia_lang::ast;
use kindelia_lang::ast::{Oper, Statement, Term};
use kindelia_lang::parser::{parse_code, parse_statements, ParseErr};

use crate::bits::ProtoSerialize;
use crate::constants;
use crate::persistence::DiskSer;
use crate::runtime::functions::compile_func;
use crate::util::{self, U128_SIZE};
use crate::util::{LocMap, NameMap, U120Map, U128Map};

pub use memory::{CellTag, RawCell, Loc};

use self::functions::CompFunc;

// Runtime data
// ============

// TODO: join maps below.

/// Stores function code, mapping `Name -> CompFunc`. It is used to find a
/// function when it is called, in order to apply its rewrite rules.
#[derive(Clone, Debug, PartialEq)]
pub struct Funcs {
  pub funcs: NameMap<Arc<CompFunc>>,
}

/// Stores constructor and function arities, mapping `Name -> arity`. It is used
/// in many places to find the arity (argument count) of functions and
/// constructors.
#[derive(Clone, Debug, PartialEq)]
pub struct Arits {
  pub arits: NameMap<u64>,
}

/// Stores the owner of the 'FuncID' a namespace, mapping `Name -> Name`.
#[derive(Clone, Debug, PartialEq)]
pub struct Ownrs {
  pub ownrs: NameMap<U120>,
}

/// Stores the block/statement number.
#[derive(Clone, Debug, PartialEq)]
pub struct Indxs {
  pub indxs: NameMap<u128>
}

/// Links a function id to its state on the runtime memory.
#[derive(Clone, Debug, PartialEq)]
pub struct Store {
  pub links: U120Map<RawCell>,
}

/// A mergeable vector of RawCells.
#[derive(Debug, Clone, PartialEq)]
pub struct Nodes {
  pub nodes: LocMap<RawCell>,
}

/// Stores a hash of each statement.
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
  pub tick: u64,   // tick counter
  pub time: u128,  // block timestamp
  pub meta: u128,  // block metadata
  pub hax0: u128,  // block hash, part 0
  pub hax1: u128,  // block hash, part 1
  pub funs: u64,   // total function count
  pub dups: u64,   // total dups count
  pub rwts: u64,   // total graph rewrites
  pub mana: u64,   // total mana cost
  pub size: u64,   // total used memory (in 64-bit words)
  pub mcap: u64,   // memory capacity (in 64-bit words)
  pub next: u64,   // memory index that *may* be empty
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
  heap: Vec<Heap>,     // heap objects
  draw: u64,           // drawing heap index
  curr: u64,           // current heap index
  nuls: Vec<u64>,      // reuse heap indices
  back: Arc<Rollback>, // past states
  path: PathBuf,       // where to save runtime state
}

#[derive(Debug, Clone)]
pub enum RuntimeError {
  NotEnoughMana,
  NotEnoughSpace,
  DivisionByZero,
  TermIsInvalidNumber { term: RawCell },
  CtrOrFunNotDefined { name: Name },
  StmtDoesntExist { stmt_index: u128 },
  ArityMismatch { name: Name, expected: usize, got: usize },
  UnboundVar { name: Name },
  NameTooBig { numb: u128 },
  TermIsNotLinear { term: Term, var: Name },
  TermExceedsMaxDepth,
  EffectFailure(EffectFailure),
  DefinitionError(DefinitionError),
}

#[derive(Debug, Clone)]
pub enum DefinitionError {
  FunctionHasNoRules,
  LHSIsNotAFunction, // TODO: check at compile time
  LHSArityMismatch { rule_index: usize, expected: usize, got: usize }, // TODO: check at compile time
  LHSNotConstructor { rule_index: usize }, // TODO: check at compile time
  VarIsUsedTwiceInDefinition { name: Name, rule_index: usize },
  VarIsNotLinearInBody { name: Name, rule_index: usize },
  VarIsNotUsed { name: Name, rule_index: usize },
  NestedMatch { rule_index: usize },
  UnsupportedMatch { rule_index: usize },
}

#[derive(Debug, Clone)]
pub enum EffectFailure {
  NoSuchState { state: U120 },
  InvalidCallArg { caller: U120, callee: U120, arg: RawCell },
  InvalidIOCtr { name: Name },
  InvalidIONonCtr { ptr: RawCell },
  IoFail { err: RawCell },
}

pub type StatementResult = Result<StatementInfo, StatementErr>;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum StatementInfo {
  Ctr { name: Name, args: Vec<Name> },
  Fun { name: Name, args: Vec<Name> },
  Run {
    done_term: Term,
    used_mana: u64,
    size_diff: i64,
    end_size: u64,
  },
  Reg { name: Name, ownr: U120 },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StatementErr {
  pub err: String,
}

pub fn hash_statement(statement: &Statement) -> crypto::Hash {
  crypto::Hash::keccak256_from_bytes(&util::bitvec_to_bytes(&ast::remove_sign(&statement).proto_serialized()))
}

pub fn hash_term(term: &Term) -> crypto::Hash {
  crypto::Hash::keccak256_from_bytes(&util::bitvec_to_bytes(&term.proto_serialized()))
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

pub const U128_NONE: u128 = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF;
pub const I128_NONE: i128 = -0x7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF;
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
const IO_FAIL : u128 = 0x40b4d6; // name_to_u128("FAIL")
const IO_NORM : u128 = 0x619717; // name_to_u128("NORM")
// TODO: GRUN -> get run result

// Maximum mana that can be spent in a block
pub const BLOCK_MANA_LIMIT: u64 = 4_000_000;

// Maximum state growth per block, in bits
pub const BLOCK_BITS_LIMIT: u64 = 2048; // 1024 bits per sec = about 8 GB per year

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

// Parser
// ======

// TODO: this should not use strings
pub fn split_names(name: Name) -> Vec<String> {
  name.to_string().split('.').map(|x| x.to_string()).collect()
}

pub fn get_namespace(name: Name) -> Option<Name> {
  let names = split_names(name);
  // TODO: pattern match
  // TODO: operate on numbers instead of string
  if names.len() > 1 {
    let name = Name::from_str_unsafe(&names[0 .. names.len() - 1].join("."));
    return Some(name);
  } else {
    return None;
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
        write!(f, "[run] {} \x1b[2m[{} mana | {} size]\x1b[0m", done_term, used_mana, size_diff)
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
    fn read_hash_map_from_file<K: DiskSer + Eq + std::hash::Hash + nohash_hasher::IsEnabled, V: DiskSer>
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
    return self.nodes.get(&idx).map(|x| *x).unwrap_or(RawCell::NONE);
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
  for i in 0..MAX_HEAPS {
    heap.push(init_heap());
  }
  let mut rt = Runtime {
    heap,
    draw: 0,
    curr: 1,
    nuls: (2..MAX_HEAPS).collect(),
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
    let stataments = parse_statements(code);
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
          break;
        }
      }
    }
    self.undo();
    results
  }

  pub fn test_statements_from_code(&mut self, code: &str) -> Vec<StatementResult> {
    let stataments = parse_statements(code);
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

  // Full-reduce a term.
  pub fn normalize(&mut self, host: Loc, mana:u64, seen: &mut HashSet<RawCell>) -> Result<RawCell, RuntimeError> {
    enum StackItem {
      Host(Loc),
      Linker(Loc),
    }
    let mut stack = vec![StackItem::Host(host)];
    let mut output = vec![];
    while !stack.is_empty() {
      let item = stack.pop().unwrap();
      match item {
        StackItem::Host(host) => {
          let term = ask_lnk(self, host);
          if seen.contains(&term) {
            output.push(term);
          }
          else {
            let term = reduce(self, host, mana)?;
            seen.insert(term);
            let mut recursive_locs = vec![];
            match term.get_tag() {
              CellTag::DP0 => {
                recursive_locs.push(term.get_loc(2));
              },
              CellTag::DP1 => {
                recursive_locs.push(term.get_loc(2));
              },
              CellTag::LAM => {
                recursive_locs.push(term.get_loc(1));
              },
              CellTag::APP => {
                recursive_locs.push(term.get_loc(0));
                recursive_locs.push(term.get_loc(1));
              },
              CellTag::SUP => {
                recursive_locs.push(term.get_loc(0));
                recursive_locs.push(term.get_loc(1));
              },
              CellTag::CTR | CellTag::FUN => {
                let name = term.get_name_from_ext();
                let arity = self.get_arity(&name).ok_or_else(|| RuntimeError::CtrOrFunNotDefined { name })?;
                for i in 0..arity {
                  recursive_locs.push(term.get_loc(i));
                }
              },
              _ => {}
            };
            for loc in recursive_locs {
              stack.push(StackItem::Linker(loc));
              stack.push(StackItem::Host(loc));
              // let lnk = self.normalize(loc, mana, seen)?;
              // link(self, loc, lnk);
            }
            output.push(term);
          }
        },
        StackItem::Linker(loc) => {
          let lnk = output.pop().unwrap();
          link(self, loc, lnk);
        },
      }  
    }
    Ok(output.pop().unwrap())
  }

  pub fn show_term(&self, lnk: RawCell) -> String {
    return debug::show_term(self, lnk, None);
  }

  pub fn show_term_at(&self, loc: Loc) -> String {
    return debug::show_term(self, self.read(loc), None);
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
    match term.get_tag() {
      CellTag::CTR => {
        let ext = term.get_ext();
        match ext {
          IO_DONE => {
            let retr = ask_arg(self, term, 0);
            clear(self, host, 1);
            clear(self, term.get_loc(0), 1);
            return Ok(retr);
          }
          IO_TAKE => {
            //println!("- IO_TAKE subject is {} {}", u128_to_name(subject), subject);
            let cont = ask_arg(self, term, 0);
            if let Some(state) = self.read_disk(subject) {
              if state != RawCell::NONE {
                self.write_disk(subject, RawCell::NONE);
                let cont = alloc_app(self, cont, state);
                let done = self.run_io(subject, subject, cont, mana);
                clear(self, host, 1);
                clear(self, term.get_loc(0), 1);
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
            let cont = alloc_app(self, cont, RawCell::num(0));
            let done = self.run_io(subject, subject, cont, mana);
            clear(self, host, 1);
            clear(self, term.get_loc(0), 2);
            return done;
          }
          IO_CALL => {
            let fnid = ask_arg(self, term, 0);
            let argm = ask_arg(self, term, 1);
            let cont = ask_arg(self, term, 2);
            let fnid = self.check_num(fnid, mana)?;
            
            let arg_name = Name::new(argm.get_ext()).ok_or_else(|| RuntimeError::NameTooBig { numb: *argm })?;
            let arg_arit = self
              .get_arity(&arg_name)
              .ok_or_else(|| RuntimeError::CtrOrFunNotDefined { name: arg_name })?;
            // Checks if the argument is a constructor with numeric fields. This is needed since
            // Kindelia's language is untyped, yet contracts can call each other freely. That would
            // allow a contract to pass an argument with an unexpected type to another, corrupting
            // its state. To avoid that, we only allow contracts to communicate by passing flat
            // constructors of numbers, like `{Send 'Alice' #123}` or `{Inc}`.
            for i in 0 .. arg_arit {
              let argm = reduce(self, argm.get_loc(0), mana)?;
              if argm.get_tag() != CellTag::NUM {
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
            //clear(self, argm.get_loc(0), arit);
            clear(self, term.get_loc(0), 3);
            return done;
          }
          IO_NORM => {
            let unnormalized = term.get_loc(0);
            let cont = ask_arg(self, term, 1);
            let normalized = self.normalize(unnormalized, mana, &mut HashSet::new())?;
            let cont = alloc_app(self, cont, normalized);
            let done = self.run_io(subject, caller, cont, mana);
            clear(self, host, 1);
            clear(self, term.get_loc(0), 2);
            return done;
          }
          IO_GIDX => {
            let fnid = ask_arg(self, term, 0);
            let cont = ask_arg(self, term, 1);
            let name = self.check_name(fnid, mana)?;
            let indx = self.get_index(&name).ok_or_else(|| RuntimeError::CtrOrFunNotDefined { name })?;
            let cont = alloc_app(self, cont, RawCell::num(indx));
            let done = self.run_io(subject, caller, cont, mana);
            clear(self, host, 1);
            clear(self, term.get_loc(0), 2);
            return done;
          }          
          IO_STH0 => {
            let indx = ask_arg(self, term, 0);
            let cont = ask_arg(self, term, 1);
            let indx = self.check_num(indx, mana)?;
            let stmt_hash = self.get_sth0(*indx).ok_or_else(|| RuntimeError::StmtDoesntExist { stmt_index: *indx })?;
            let cont = alloc_app(self, cont, RawCell::num(stmt_hash));
            let done = self.run_io(subject, caller, cont, mana);
            clear(self, host, 1);
            clear(self, term.get_loc(0), 2);
            return done;
          }
          IO_STH1 => {
            let indx = ask_arg(self, term, 0);
            let cont = ask_arg(self, term, 1);
            let indx = self.check_num(indx, mana)?;
            let stmt_hash = self.get_sth1(*indx).ok_or_else(|| RuntimeError::StmtDoesntExist { stmt_index: *indx })?;
            let cont = alloc_app(self, cont, RawCell::num(stmt_hash));
            let done = self.run_io(subject, caller, cont, mana);
            clear(self, host, 1);
            clear(self, term.get_loc(0), 2);
            return done;
          }
          IO_SUBJ => {
            let cont = ask_arg(self, term, 0);
            let cont = alloc_app(self, cont, RawCell::num(*subject));
            let done = self.run_io(subject, caller, cont, mana);
            clear(self, host, 1);
            clear(self, term.get_loc(0), 1);
            return done;
          }
          IO_FROM => {
            let cont = ask_arg(self, term, 0);
            let cont = alloc_app(self, cont, RawCell::num(*caller));
            let done = self.run_io(subject, caller, cont, mana);
            clear(self, host, 1);
            clear(self, term.get_loc(0), 1);
            return done;
          }
          IO_TICK => {
            let cont = ask_arg(self, term, 0);
            let cont = alloc_app(self, cont, RawCell::num(self.get_tick() as u128));
            let done = self.run_io(subject, subject, cont, mana);
            clear(self, host, 1);
            clear(self, term.get_loc(0), 1);
            return done;
          }
          IO_TIME => {
            let cont = ask_arg(self, term, 0);
            let cont = alloc_app(self, cont, RawCell::num(self.get_time()));
            let done = self.run_io(subject, subject, cont, mana);
            clear(self, host, 1);
            clear(self, term.get_loc(0), 1);
            return done;
          }
          IO_META => {
            let cont = ask_arg(self, term, 0);
            let cont = alloc_app(self, cont, RawCell::num(self.get_meta()));
            let done = self.run_io(subject, subject, cont, mana);
            clear(self, host, 1);
            clear(self, term.get_loc(0), 1);
            return done;
          }
          IO_HAX0 => {
            let cont = ask_arg(self, term, 0);
            let cont = alloc_app(self, cont, RawCell::num(self.get_hax0()));
            let done = self.run_io(subject, subject, cont, mana);
            clear(self, host, 1);
            clear(self, term.get_loc(0), 1);
            return done;
          }
          IO_HAX1 => {
            let cont = ask_arg(self, term, 0);
            let cont = alloc_app(self, cont, RawCell::num(self.get_hax1()));
            let done = self.run_io(subject, subject, cont, mana);
            clear(self, host, 1);
            clear(self, term.get_loc(0), 1);
            return done;
          }
          IO_FAIL => {
            let err = ask_arg(self, term, 0);
            clear(self, host, 1);
            clear(self, term.get_loc(0), 1);
            // TODO: readback error?
            return Err(RuntimeError::EffectFailure(EffectFailure::IoFail { err }));
          }
          _ => {
            let name = term.get_name_from_ext();
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
    match num.get_tag() {
      CellTag::NUM => Ok(num.get_num()),
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
        handle_runtime_err(self, "fun", checks::check_func(&func))?;
        let func = compile_func(func, true);
        let func = handle_runtime_err(self, "fun", func)?;
        let name = *name;
        self.set_arity(name, args.len() as u64);
        self.define_function(name, func, stmt_index, hash);
        if let Some(state) = init {
          let state = self.create_term(state, Loc::ZERO, &mut init_name_map());
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
        handle_runtime_err(self, "run", checks::check_term(&expr))?;
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
    let mut keeps = util::u8s_to_u128s(&std::fs::read(self.path.join("_keeps_"))?).unwrap();
    let mut lifes = util::u8s_to_u128s(&std::fs::read(self.path.join("_lifes_"))?).unwrap();
    let mut uuids = util::u8s_to_u128s(&std::fs::read(self.path.join("_uuids_"))?).unwrap();
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
    return self.get_with(RawCell::ZERO, RawCell::NONE, |heap| heap.read(idx));
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

// Memory
// ------

pub fn ask_lnk(rt: &Runtime, loc: Loc) -> RawCell {
  rt.read(loc)
  //unsafe { *rt.heap.get_unchecked(loc as usize) }
}

pub fn ask_arg(rt: &Runtime, term: RawCell, arg: u64) -> RawCell {
  ask_lnk(rt, term.get_loc(arg))
}

pub fn link(rt: &mut Runtime, loc: Loc, lnk: RawCell) -> RawCell {
  rt.write(loc, lnk);
  if lnk.get_tag() as u8 <= CellTag::VAR as u8 {
    let pos = lnk.get_loc((lnk.get_tag() as u8 & 0x01) as u64);
    rt.write(pos, RawCell::arg(loc));
  }
  lnk
}

pub fn alloc(rt: &mut Runtime, arity: u64) -> Loc {
  if arity == 0 {
    return Loc::ZERO;  // TODO
  } else {
    loop {
      // Attempts to allocate enough space, starting from the last index
      // where we previously found free space, and moving rightwards
      let mcap = rt.get_mcap();
      let index = rt.get_next();
      if index <= mcap - arity {
        let index = Loc::from_u64_unchecked(index);
        let mut has_space = true;
        for i in 0..arity {
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
          for i in 0..arity {
            rt.write(index + i, RawCell::nil()); // millions perished for forgetting this line
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
  for i in 0..size {
    if rt.read(loc + i) == RawCell::ZERO {
      panic!("Cleared twice: {}", *loc);
    }
    rt.write(loc + i, RawCell::ZERO);
  }
  rt.set_size(rt.get_size() - size);
  //rt.free[size as usize].push(loc);
}

pub fn collect(rt: &mut Runtime, term: RawCell) {
  let mut stack: Vec<RawCell> = Vec::new();
  let mut next = term;

  /// Collects the DP and checks if the entire dup-node can be collected, clears
  /// it and returns the body to be collected, if so.
  fn collect_dp_and_check_dup(rt: &mut Runtime, dup_loc: Loc, side: bool) -> Option<RawCell> {
    // Arg cell inside the dup-node corresponding to the DP we are collecting
    let dp_arg_loc = dup_loc + (side as u64);
    link(rt, dp_arg_loc, RawCell::era());

    // Arg cell corresponding to the counterpart DP
    let co_dp_arg_loc = dup_loc + (!side as u64);
    let co_dp_arg = rt.read(co_dp_arg_loc);

    // If both DPs have been collected, collect the entire dup-node
    if co_dp_arg.get_tag() == CellTag::ERA {
      let dup_body = rt.read(dup_loc + 2);
      clear(rt, dup_loc, 3);
      // Returns the dup body to be collected
      Some(dup_body)
    } else {
      None
    }
  }

  loop {
    let term = next;
    let tag = term.get_tag();
    match tag {
      CellTag::DP0 | CellTag::DP1 => {
        let side = matches!(tag, CellTag::DP1);
        let dup_node_loc = term.get_ptr();
        let body = collect_dp_and_check_dup(rt, dup_node_loc, side);
        if let Some(body) = body {
          next = body;
          continue;
        }
      }
      CellTag::VAR => {
        link(rt, term.get_loc(0), RawCell::era());
      }
      CellTag::LAM => {
        let arg = ask_arg(rt, term, 0);
        if arg.get_tag() != CellTag::ERA {
          link(rt, arg.get_loc(0), RawCell::era());
        }
        next = ask_arg(rt, term, 1);
        clear(rt, term.get_loc(0), 2);
        continue;
      }
      CellTag::APP => {
        stack.push(ask_arg(rt, term, 0));
        next = ask_arg(rt, term, 1);
        clear(rt, term.get_loc(0), 2);
        continue;
      }
      CellTag::SUP => {
        stack.push(ask_arg(rt, term, 0));
        next = ask_arg(rt, term, 1);
        clear(rt, term.get_loc(0), 2);
        continue;
      }
      CellTag::OP2 => {
        stack.push(ask_arg(rt, term, 0));
        next = ask_arg(rt, term, 1);
        clear(rt, term.get_loc(0), 2);
        continue;
      }
      CellTag::NUM => {}
      CellTag::CTR | CellTag::FUN => {
        let arity = rt.get_arity(&term.get_name_from_ext()).unwrap();
        // NOTE: should never be none, should panic
        // TODO: remove unwrap?
        for i in 0..arity {
          if i < arity - 1 {
            stack.push(ask_arg(rt, term, i));
          } else {
            next = ask_arg(rt, term, i);
          }
        }
        clear(rt, term.get_loc(0), arity);
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
}

// Term
// ----

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
      link(rt, loc, RawCell::era());
    } else {
      let got = vars_data.entry(name).or_default();
      got.push(lnk);
      link(rt, loc, RawCell::era());
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
      bind(rt, node + 0, *nam0, RawCell::dp0(dupk as u128, node), vars_data);
      bind(rt, node + 1, *nam1, RawCell::dp1(dupk as u128, node), vars_data); // TODO: shouldnt these be labels? why are they u64?
      let body = create_term(rt, body, loc, vars_data);
      body
    }
    Term::Lam { name, body } => {
      let node = alloc(rt, 2);
      bind(rt, node + 0, *name, RawCell::var(node), vars_data);
      let body = create_term(rt, body, node + 1, vars_data)?;
      link(rt, node + 1, body);
      Ok(RawCell::lam(node))
    }
    Term::App { func, argm } => {
      let node = alloc(rt, 2);
      let func = create_term(rt, func, node + 0, vars_data)?;
      link(rt, node + 0, func);
      let argm = create_term(rt, argm, node + 1, vars_data)?;
      link(rt, node + 1, argm);
      Ok(RawCell::app(node))
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
        Ok(RawCell::fun(*name, node))
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
        Ok(RawCell::ctr(*name, node))
      }
    }
    Term::Num { numb } => {
      Ok(RawCell::num(**numb))
    }
    Term::Op2 { oper, val0, val1 } => {
      let node = alloc(rt, 2);
      let val0 = create_term(rt, val0, node + 0, vars_data)?;
      link(rt, node + 0, val0);
      let val1 = create_term(rt, val1, node + 1, vars_data)?;
      link(rt, node + 1, val1);
      Ok(RawCell::op2(*oper as u128, node))
    }
  }
}

pub fn create_app(rt: &mut Runtime, func: RawCell, argm: RawCell) -> RawCell {
  let node = alloc(rt, 2);
  link(rt, node + 0, func);
  link(rt, node + 1, argm);
  RawCell::app(node)
}

pub fn create_fun(rt: &mut Runtime, fun: Name, args: &[RawCell]) -> RawCell {
  let node = alloc(rt, args.len() as u64);
  for i in 0..args.len() {
    link(rt, node + (i as u64), args[i]);
  }
  RawCell::fun(fun, node)
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
  if lnk.get_tag() != CellTag::ERA {
    link(rt, lnk.get_loc(0), val);
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

  let mut func_val: Option<CompFunc>;
  let mut func_ref: Option<&mut CompFunc>;

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
      match term.get_tag() {
        CellTag::APP => {
          stack.push(host);
          init = 1;
          host = term.get_loc(0);
          continue;
        }
        CellTag::DP0 | CellTag::DP1 => {
          stack.push(host);
          host = term.get_loc(2);
          continue;
        }
        CellTag::OP2 => {
          stack.push(host);
          stack.push(Loc::from_u64_unchecked(*(term.get_loc(0) + 1) | 0x1_0000_0000_0000)); // this is so ugly; TODO
          host = term.get_loc(0);
          continue;
        }
        CellTag::FUN => {
          let name = term.get_name_from_ext();
          let ari = rt.get_arity(&name).ok_or_else(|| RuntimeError::CtrOrFunNotDefined { name })?;
          if let Some(func) = &rt.get_func(&name) {
            if ari == func.arity {
              if func.redux.len() == 0 {
                init = 0;
              } else {
                stack.push(host);
                for (i, redux) in func.redux.iter().enumerate() {
                  if i < func.redux.len() - 1 {
                    let loc = term.get_loc(*redux);
                    stack.push(Loc::from_u64_unchecked(*loc | 0x1_0000_0000_0000));
                  } else {
                    host = term.get_loc(*redux);
                  }
                }
              }
              continue;
            }
          }
        }
        // We don't need to reduce further
        _ => {}
      }
    } else {
      match term.get_tag() {
        CellTag::APP => {
          let arg0 = ask_arg(rt, term, 0);
          // (@x(body) a)
          // ------------ APP-LAM
          // x <- a
          // body
          if arg0.get_tag() == CellTag::LAM {
            //println!("app-lam");
            rt.set_mana(rt.get_mana() + costs::app_lam_mana());
            rt.set_rwts(rt.get_rwts() + 1);
            subst(rt, ask_arg(rt, arg0, 0), ask_arg(rt, term, 1));
            let _done = link(rt, host, ask_arg(rt, arg0, 1));
            clear(rt, term.get_loc(0), 2);
            clear(rt, arg0.get_loc(0), 2);
            init = 1;
            continue;
          // ({a b} c)
          // ----------------- APP-SUP
          // dup x0 x1 = c
          // {(a x0) (b x1)}
          } else if arg0.get_tag() == CellTag::SUP {
            //println!("app-sup");
            rt.set_mana(rt.get_mana() + costs::app_sup_mana());
            rt.set_rwts(rt.get_rwts() + 1);
            let app0 = term.get_loc(0);
            let app1 = arg0.get_loc(0);
            let let0 = alloc(rt, 3);
            let par0 = alloc(rt, 2);
            link(rt, let0 + 2, ask_arg(rt, term, 1));
            link(rt, app0 + 1, RawCell::dp0(arg0.get_ext(), let0));
            link(rt, app0 + 0, ask_arg(rt, arg0, 0));
            link(rt, app1 + 0, ask_arg(rt, arg0, 1));
            link(rt, app1 + 1, RawCell::dp1(arg0.get_ext(), let0));
            link(rt, par0 + 0, RawCell::app(app0));
            link(rt, par0 + 1, RawCell::app(app1));
            let done = RawCell::par(arg0.get_ext(), par0);
            link(rt, host, done);
          }
        }
        CellTag::DP0 | CellTag::DP1 => {
          let arg0 = ask_arg(rt, term, 2);
          // dup r s = @x(f)
          // --------------- DUP-LAM
          // dup f0 f1 = f
          // r <- @x0(f0)
          // s <- @x1(f1)
          // x <- {x0 x1}
          if arg0.get_tag() == CellTag::LAM {
            //println!("dup-lam");
            rt.set_mana(rt.get_mana() + costs::dup_lam_mana());
            rt.set_rwts(rt.get_rwts() + 1);
            let let0 = term.get_loc(0);
            let par0 = arg0.get_loc(0);
            let lam0 = alloc(rt, 2);
            let lam1 = alloc(rt, 2);
            link(rt, let0 + 2, ask_arg(rt, arg0, 1));
            link(rt, par0 + 1, RawCell::var(lam1));
            let arg0_arg_0 = ask_arg(rt, arg0, 0);
            link(rt, par0 + 0, RawCell::var(lam0));
            subst(rt, arg0_arg_0, RawCell::par(term.get_ext(), par0));
            let term_arg_0 = ask_arg(rt, term, 0);
            link(rt, lam0 + 1, RawCell::dp0(term.get_ext(), let0));
            subst(rt, term_arg_0, RawCell::lam(lam0));
            let term_arg_1 = ask_arg(rt, term, 1);
            link(rt, lam1 + 1, RawCell::dp1(term.get_ext(), let0));
            subst(rt, term_arg_1, RawCell::lam(lam1));
            let done = RawCell::lam(if term.get_tag() == CellTag::DP0 { lam0 } else { lam1 });
            link(rt, host, done);
            init = 1;
            continue;
          // dup x y = {a b}
          // --------------- DUP-SUP-E
          // x <- a
          // y <- b
          } else if arg0.get_tag() == CellTag::SUP {
            if term.get_ext() == arg0.get_ext() {
              //println!("dup-sup-e");
              rt.set_mana(rt.get_mana() + costs::dup_sup_mana());
              rt.set_rwts(rt.get_rwts() + 1);
              subst(rt, ask_arg(rt, term, 0), ask_arg(rt, arg0, 0));
              subst(rt, ask_arg(rt, term, 1), ask_arg(rt, arg0, 1));
              let _done = link(rt, host, ask_arg(rt, arg0, if term.get_tag() == CellTag::DP0 { 0 } else { 1 }));
              clear(rt, term.get_loc(0), 3);
              clear(rt, arg0.get_loc(0), 2);
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
              rt.set_mana(rt.get_mana() + costs::dup_dup_mana());
              rt.set_rwts(rt.get_rwts() + 1);
              let par0 = alloc(rt, 2);
              let let0 = term.get_loc(0);
              let par1 = arg0.get_loc(0);
              let let1 = alloc(rt, 3);
              link(rt, let0 + 2, ask_arg(rt, arg0, 0));
              link(rt, let1 + 2, ask_arg(rt, arg0, 1));
              let term_arg_0 = ask_arg(rt, term, 0);
              let term_arg_1 = ask_arg(rt, term, 1);
              link(rt, par1 + 0, RawCell::dp1(term.get_ext(), let0));
              link(rt, par1 + 1, RawCell::dp1(term.get_ext(), let1));
              link(rt, par0 + 0, RawCell::dp0(term.get_ext(), let0));
              link(rt, par0 + 1, RawCell::dp0(term.get_ext(), let1));
              subst(rt, term_arg_0, RawCell::par(arg0.get_ext(), par0));
              subst(rt, term_arg_1, RawCell::par(arg0.get_ext(), par1));
              let done = RawCell::par(arg0.get_ext(), if term.get_tag() == CellTag::DP0 { par0 } else { par1 });
              link(rt, host, done);
            }
          // dup x y = N
          // ----------- DUP-NUM
          // x <- N
          // y <- N
          // ~
          } else if arg0.get_tag() == CellTag::NUM {
            //println!("dup-num");
            rt.set_mana(rt.get_mana() + costs::dup_num_mana());
            rt.set_rwts(rt.get_rwts() + 1);
            subst(rt, ask_arg(rt, term, 0), arg0);
            subst(rt, ask_arg(rt, term, 1), arg0);
            clear(rt, term.get_loc(0), 3);
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
          } else if arg0.get_tag() == CellTag::CTR {
            //println!("dup-ctr");
            let name = arg0.get_name_from_ext();
            let arit = rt.get_arity(&name).ok_or_else(|| RuntimeError::CtrOrFunNotDefined { name })?;
            rt.set_mana(rt.get_mana() + costs::dup_ctr_mana(arit));
            rt.set_rwts(rt.get_rwts() + 1);
            if arit == 0 {
              subst(rt, ask_arg(rt, term, 0), RawCell::ctr(name, Loc::ZERO));
              subst(rt, ask_arg(rt, term, 1), RawCell::ctr(name, Loc::ZERO));
              clear(rt, term.get_loc(0), 3);
              let _done = link(rt, host, RawCell::ctr(name, Loc::ZERO));
            } else {
              let ctr0 = arg0.get_loc(0);
              let ctr1 = alloc(rt, arit);
              for i in 0..arit - 1 {
                let leti = alloc(rt, 3);
                link(rt, leti + 2, ask_arg(rt, arg0, i));
                link(rt, ctr0 + i, RawCell::dp0(term.get_ext(), leti));
                link(rt, ctr1 + i, RawCell::dp1(term.get_ext(), leti));
              }
              let leti = term.get_loc(0);
              link(rt, leti + 2, ask_arg(rt, arg0, arit - 1));
              let term_arg_0 = ask_arg(rt, term, 0);
              link(rt, ctr0 + (arit - 1), RawCell::dp0(term.get_ext(), leti));
              subst(rt, term_arg_0, RawCell::ctr(name, ctr0));
              let term_arg_1 = ask_arg(rt, term, 1);
              link(rt, ctr1 + (arit - 1), RawCell::dp1(term.get_ext(), leti));
              subst(rt, term_arg_1, RawCell::ctr(name, ctr1));
              let done = RawCell::ctr(name, if term.get_tag() == CellTag::DP0 { ctr0 } else { ctr1 });
              link(rt, host, done);
            }
          // dup x y = *
          // x <- *
          // y <- *
          } else if arg0.get_tag() == CellTag::ERA {
            //println!("dup-era");
            rt.set_mana(rt.get_mana() + costs::dup_era_mana());
            rt.set_rwts(rt.get_rwts() + 1);
            subst(rt, ask_arg(rt, term, 0), RawCell::era());
            subst(rt, ask_arg(rt, term, 1), RawCell::era());
            link(rt, host, RawCell::era());
            clear(rt, term.get_loc(0), 3);
            init = 1;
            continue;
          }
        }
        CellTag::OP2 => {
          let arg0 = ask_arg(rt, term, 0);
          let arg1 = ask_arg(rt, term, 1);
          // (+ a b)
          // --------- OP2-NUM
          // add(a, b)
          if arg0.get_tag() == CellTag::NUM && arg1.get_tag() == CellTag::NUM {
            //eprintln!("op2-num");
            let op  = term.get_ext().try_into().expect("Invalid operation coming from HVM");
            let a_u = arg0.get_num();
            let b_u = arg1.get_num();
            if op == Oper::Div && *b_u == 0 {
              return Err(RuntimeError::DivisionByZero)
            }
            rt.set_mana(rt.get_mana() + costs::op2_num_mana());
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
            let done = RawCell::num(res);
            clear(rt, term.get_loc(0), 2);
            link(rt, host, done);
          // (+ {a0 a1} b)
          // --------------------- OP2-SUP-0
          // let b0 b1 = b
          // {(+ a0 b0) (+ a1 b1)}
          } else if arg0.get_tag() == CellTag::SUP {
            //println!("op2-sup-0");
            rt.set_mana(rt.get_mana() + costs::op2_sup_mana());
            rt.set_rwts(rt.get_rwts() + 1);
            let op20 = term.get_loc(0);
            let op21 = arg0.get_loc(0);
            let let0 = alloc(rt, 3);
            let par0 = alloc(rt, 2);
            link(rt, let0 + 2, arg1);
            link(rt, op20 + 1, RawCell::dp0(arg0.get_ext(), let0));
            link(rt, op20 + 0, ask_arg(rt, arg0, 0));
            link(rt, op21 + 0, ask_arg(rt, arg0, 1));
            link(rt, op21 + 1, RawCell::dp1(arg0.get_ext(), let0));
            link(rt, par0 + 0, RawCell::op2(term.get_ext(), op20));
            link(rt, par0 + 1, RawCell::op2(term.get_ext(), op21));
            let done = RawCell::par(arg0.get_ext(), par0);
            link(rt, host, done);
          // (+ a {b0 b1})
          // --------------- OP2-SUP-1
          // dup a0 a1 = a
          // {(+ a0 b0) (+ a1 b1)}
          } else if arg1.get_tag() == CellTag::SUP {
            //println!("op2-sup-1");
            rt.set_mana(rt.get_mana() + costs::op2_sup_mana());
            rt.set_rwts(rt.get_rwts() + 1);
            let op20 = term.get_loc(0);
            let op21 = arg1.get_loc(0);
            let let0 = alloc(rt, 3);
            let par0 = alloc(rt, 2);
            link(rt, let0 + 2, arg0);
            link(rt, op20 + 0, RawCell::dp0(arg1.get_ext(), let0));
            link(rt, op20 + 1, ask_arg(rt, arg1, 0));
            link(rt, op21 + 1, ask_arg(rt, arg1, 1));
            link(rt, op21 + 0, RawCell::dp1(arg1.get_ext(), let0));
            link(rt, par0 + 0, RawCell::op2(term.get_ext(), op20));
            link(rt, par0 + 1, RawCell::op2(term.get_ext(), op21));
            let done = RawCell::par(arg1.get_ext(), par0);
            link(rt, host, done);
          }
        }
        CellTag::FUN => {

          fn call_function(rt: &mut Runtime, func: Arc<CompFunc>, host: Loc, term: RawCell, mana: u64, vars_data: &mut NameMap<Vec<RawCell>>) -> Result<bool, RuntimeError> {
            // For each argument, if it is a redex and a SUP, apply the cal_par rule
            for idx in &func.redux {
              // (F {a0 a1} b c ...)
              // ------------------- FUN-SUP
              // dup b0 b1 = b
              // dup c0 c1 = c
              // ...
              // {(F a0 b0 c0 ...) (F a1 b1 c1 ...)}
              if ask_arg(rt, term, *idx).get_tag() == CellTag::SUP {
                //println!("fun-sup");
                let name = term.get_name_from_ext();
                let arit = rt.get_arity(&name).ok_or_else(|| RuntimeError::CtrOrFunNotDefined { name })?;
                rt.set_mana(rt.get_mana() + costs::fun_sup_mana(arit));
                rt.set_rwts(rt.get_rwts() + 1);
                let argn = ask_arg(rt, term, *idx);
                let fun0 = term.get_loc(0);
                let fun1 = alloc(rt, arit);
                let par0 = argn.get_loc(0);
                for i in 0..arit {
                  if i != *idx {
                    let leti = alloc(rt, 3);
                    let argi = ask_arg(rt, term, i);
                    link(rt, fun0 + i, RawCell::dp0(argn.get_ext(), leti));
                    link(rt, fun1 + i, RawCell::dp1(argn.get_ext(), leti));
                    link(rt, leti + 2, argi);
                  } else {
                    link(rt, fun0 + i, ask_arg(rt, argn, 0));
                    link(rt, fun1 + i, ask_arg(rt, argn, 1));
                  }
                }
                link(rt, par0 + 0, RawCell::fun(name, fun0));
                link(rt, par0 + 1, RawCell::fun(name, fun1));
                let done = RawCell::par(argn.get_ext(), par0);
                link(rt, host, done);
                return Ok(true);
              }
            }
            // For each rule condition vector
            for rule in &func.rules {
              // Check if the rule matches
              let mut matched = true;
              //println!("- matching rule");
              // Tests each rule condition (ex: `args[0].get_tag() == SUCC`)
              for i in 0..rule.cond.len() as u64 {
                let argi = ask_arg(rt, term, i);
                let cond = rule.cond[i as usize];
                match cond.get_tag() {
                  CellTag::NUM => {
                    //println!("Didn't match because of NUM. i={} {} {}", i, get_val(ask_arg(rt, term, i)), cond.get_val());
                    let same_tag = argi.get_tag() == CellTag::NUM;
                    let same_num = argi.get_num() == cond.get_num();
                    matched = matched && same_tag && same_num;
                  }
                  CellTag::CTR => {
                    //println!("Didn't match because of CTR. i={} {} {}", i, get_tag(ask_arg(rt, term, i)), cond.get_val());
                    let same_tag = argi.get_tag() == CellTag::CTR;
                    let same_ext = argi.get_ext() == cond.get_ext();
                    matched = matched && same_tag && same_ext;
                  }
                  CellTag::VAR => {
                    if func.redux.contains(&i) {
                      let not_var = argi.get_tag() as u8 > CellTag::VAR as u8;
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
                rt.set_mana(rt.get_mana() + costs::fun_ctr_mana(&rule.body));
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
                    let arr = vars_data.entry(rule_var.name).or_default();
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
                  clear(rt, ask_arg(rt, term, *eras_index).get_loc(0), *eras_arity);
                }
                clear(rt, term.get_loc(0), func.arity);
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

          if let Some(func) = rt.get_func(&term.get_name_from_ext()) {
            if call_function(rt, func, host, term, mana, &mut vars_data)? {
              init = 1;
              continue;
            }
          }

        }
        // We don't need to reduce further
        _ => {}
      }
    }

    // When we don't need to reduce the head
    if let Some(item) = stack.pop() {
      init = *item >> 48;
      host = Loc::from_u64_unchecked(*item & 0x0_FFFF_FFFF_FFFF);
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
pub fn compute_at(rt: &mut Runtime, loc: Loc, mana: u64) -> Result<RawCell, RuntimeError> {
  enum StackItem {
    LinkResolver { loc: Loc },
    ComputeAt { loc: Loc },
  }
  fn compute_and_link_arg(stack: &mut Vec<StackItem>, cell: RawCell, arg: u64) {
    let loc = cell.get_loc(arg);
    stack.push(StackItem::LinkResolver { loc });
    stack.push(StackItem::ComputeAt { loc });
  }

  let mut stack = vec![StackItem::ComputeAt { loc }];
  let mut output = vec![];

  while let Some(item) = stack.pop() {
    match item {
      StackItem::ComputeAt { loc } => {
        let term = ask_lnk(rt, loc);
        let norm = reduce(rt, loc, mana)?;

        // if matches!(norm.get_tag(), APP) {
        //   stack.push(StackItem::LinkResolver{ loc: host });
        //   stack.push(StackItem::ComputeAt { loc: host });
        // } else {
        //   output.push(norm);
        // };

        output.push(norm);

        if term != norm {
          match norm.get_tag() {
            CellTag::LAM => {
              compute_and_link_arg(&mut stack, norm, 1);
            }
            CellTag::APP => {
              compute_and_link_arg(&mut stack, norm, 1);
              compute_and_link_arg(&mut stack, norm, 0);
            }
            CellTag::SUP => {
              compute_and_link_arg(&mut stack, norm, 1);
              compute_and_link_arg(&mut stack, norm, 0);
            }
            CellTag::DP0 => {
              compute_and_link_arg(&mut stack, norm, 2);
            }
            CellTag::DP1 => {
              compute_and_link_arg(&mut stack, norm, 2);
            }
            CellTag::CTR | CellTag::FUN => {
              let name = norm.get_name_from_ext();
              let arity = rt.get_arity(&name).ok_or_else(|| RuntimeError::CtrOrFunNotDefined { name })?;
              for i in (0..arity).rev() {
                compute_and_link_arg(&mut stack, norm, i);
              }
            }
            _ => {}
          };
        }
      }
      StackItem::LinkResolver { loc } => {
        let cell = output.pop().expect("No term to resolve link");
        link(rt, loc, cell);
      }
    }
  }
  debug_assert!(output.len() == 1);
  // FIXME: is this always safe? if no, create a runtime error for what could go wrong
  Ok(output.pop().expect("Output stack is empty"))
}

// Debug
// =====

pub fn show_runtime_error(err: RuntimeError) -> String {
  match err {
    RuntimeError::NotEnoughMana => "Not enough mana".to_string(),
    RuntimeError::NotEnoughSpace => "Not enough space".to_string(),
    RuntimeError::DivisionByZero => "Tried to divide by zero".to_string(),
    RuntimeError::TermExceedsMaxDepth => "Term exceeds maximum depth.".to_string(),
    RuntimeError::UnboundVar { name } => format!("Unbound variable '{}'.", name),
    RuntimeError::TermIsInvalidNumber { term } => format!("'{}' is not a number.", term),
    RuntimeError::CtrOrFunNotDefined { name } => format!("'{}' is not defined.", name),
    RuntimeError::StmtDoesntExist { stmt_index } => format!("Statement with index '{}' does not exist.", stmt_index),
    RuntimeError::ArityMismatch { name, expected, got } => format!("Arity mismatch for '{}': expected {} args, got {}.", name, expected, got),
    RuntimeError::NameTooBig { numb } => format!("Cannot fit '{}' into a function name.", numb),
    RuntimeError::TermIsNotLinear { term, var } => format!("'{}' is not linear: '{}' is used more than once.", term, var),
    RuntimeError::EffectFailure(effect_failure) =>
      match effect_failure {
        EffectFailure::NoSuchState { state: addr } => format!("Tried to read state of '{}' but did not exist.", show_addr(addr)),
        EffectFailure::InvalidCallArg { caller, callee, arg } => {
          let pos = arg.get_ptr();
          format!("'{}' tried to call '{}' with invalid argument '{}'.", show_addr(caller), show_addr(callee), arg)
        },
        EffectFailure::InvalidIOCtr { name } => format!("'{}' is not an IO constructor.", name),
        EffectFailure::InvalidIONonCtr { ptr } => format!("'{}' is not an IO term.", ptr),
        EffectFailure::IoFail { err } => format!("Failed: '{}'", err),
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
      match term.get_tag() {
        CellTag::LAM => {
          let param = ask_arg(rt, term, 0);
          let body = ask_arg(rt, term, 1);
          // TODO ask
          names.insert(term.get_loc(0), format!("{}", names.len()));
          stack.push(body);
        }
        CellTag::APP => {
          let lam = ask_arg(rt, term, 0);
          let arg = ask_arg(rt, term, 1);
          stack.push(arg);
          stack.push(lam);
        }
        CellTag::SUP => {
          let arg0 = ask_arg(rt, term, 0);
          let arg1 = ask_arg(rt, term, 1);
          stack.push(arg1);
          stack.push(arg0);
        }
        CellTag::DP0 | CellTag::DP1 => {
          if let hash_map::Entry::Vacant(e) = names.entry(term.get_loc(0)) {
            names.insert(term.get_loc(0), format!("{}", names.len()));
            stack.push(ask_arg(rt, term, 2));
          }
        }
        CellTag::OP2 => {
          let arg0 = ask_arg(rt, term, 0);
          let arg1 = ask_arg(rt, term, 1);
          stack.push(arg1);
          stack.push(arg0);
        }
        CellTag::NUM => {}
        CellTag::CTR | CellTag::FUN => {
          let name = term.get_name_from_ext();
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
    dup_store: &mut DupStore,
    limit: Option<usize>,
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
          println!("{} {}", prefix, debug::show_term(rt, *term, None));
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
          debug_assert!(term != RawCell::ZERO);
          match term.get_tag() {
            CellTag::DP0 | CellTag::DP1 => {
              let col = term.get_ext();
              let val = ask_arg(rt, term, 2);
              if term.get_tag() == CellTag::DP0 {
                dup_store.push(col, false);
              } else {
                dup_store.push(col, true);
              }
              stack.push(StackItem::Resolver(term));
              stack.push(StackItem::Term(val));
            }
            CellTag::SUP => {
              let col = term.get_ext();
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
            CellTag::VAR => {
              let name = &format!("x{}", names.get(&term.get_loc(0)).unwrap_or(&String::from("_")));
              let name: Name = (name as &str).try_into().unwrap(); 
              output.push(Term::var(name));
            }
            CellTag::NUM => {
              let numb = term.get_num();
              output.push(Term::num(numb));
            }
            CellTag::OP2 => {
              stack.push(StackItem::Resolver(term));
              stack.push(StackItem::Term(ask_arg(rt, term, 1)));
              stack.push(StackItem::Term(ask_arg(rt, term, 0)));
            }
            CellTag::CTR | CellTag::FUN => {
              let name = term.get_name_from_ext();
              let arit = rt.get_arity(&name).unwrap();
              stack.push(StackItem::Resolver(term));
              for i in 0..arit {
                stack.push(StackItem::Term(ask_arg(rt, term, i)));
              }
            }
            CellTag::LAM => {
              stack.push(StackItem::Resolver(term));
              stack.push(StackItem::Term(ask_arg(rt, term, 1)));
            }
            CellTag::APP => {
              stack.push(StackItem::Resolver(term));
              stack.push(StackItem::Term(ask_arg(rt, term, 1)));
              stack.push(StackItem::Term(ask_arg(rt, term, 0)));
            }
            _ => {}
          }
        }
        StackItem::SUPResolverSome(term, old) => {
          let col = term.get_ext();
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
          match term.get_tag() {
            CellTag::DP0 | CellTag::DP1 => {
              let col = term.get_ext();
              dup_store.pop(col);
            }
            CellTag::CTR | CellTag::FUN => {
              let name = term.get_name_from_ext();
              let arit = rt.get_arity(&name).unwrap();
              let mut args = Vec::new();
              for i in 0..arit {
                args.push(output.pop().unwrap());
              }
              if term.get_tag() == CellTag::CTR {
                output.push(Term::ctr(name, args));
              } else {
                output.push(Term::fun(name, args));
              }
            },
            CellTag::LAM => {
              let name = format!("x{}", names.get(&term.get_loc(0)).unwrap_or(&String::from("_")));
              let name = Name::from_str(&name).unwrap();
              let body = Box::new(output.pop().unwrap());
              output.push(Term::lam(name, body));
            }
            CellTag::APP => {
              let argm = Box::new(output.pop().unwrap());
              let func = Box::new(output.pop().unwrap());
              output.push(Term::app(func, argm));
            }
            CellTag::OP2 => {
              let oper = term.get_ext();
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
    } else {
      panic!("Readback output is empty")
    }
  }

  let mut names: LocMap<String> = init_loc_map();
  let mut dup_store = DupStore::new();
  find_names(rt, term, &mut names);
  readback(rt, term, &mut names, &mut dup_store, limit)
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
  let str_0 = ast::view_statements(statements);
  let statements = &Vec::proto_deserialized(&statements.proto_serialized()).unwrap();
  let str_1 = ast::view_statements(statements);

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
  let statments = parse_statements(code);
  match statments {
    Ok((.., statements)) => test_statements(&statements, debug),
    Err(ParseErr { code, erro }) => println!("{}", erro),
  }
}

pub fn test_statements_from_file(file: &str, debug: bool) {
  test_statements_from_code(&std::fs::read_to_string(file).expect("file not found"), debug);
}
