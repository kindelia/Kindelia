#![allow(clippy::identity_op)]
#![allow(dead_code)]
#![allow(non_snake_case)]

use nohash_hasher::NoHashHasher;
use rand::prelude::*;
use std::collections::hash_map::DefaultHasher;
use std::collections::{hash_map, HashMap};
use std::hash::{Hash, Hasher, BuildHasherDefault};
use std::time::Instant;

// Types
// -----

// A native HVM term
#[derive(Clone, Debug)]
pub enum Term {
  Var { name: u64 },
  Dup { nam0: u64, nam1: u64, expr: Box<Term>, body: Box<Term> },
  Lam { name: u64, body: Box<Term> },
  App { func: Box<Term>, argm: Box<Term> },
  Ctr { name: u64, args: Vec<Term> },
  Fun { name: u64, args: Vec<Term> },
  U60 { numb: u64 },
  Op2 { oper: u64, val0: Box<Term>, val1: Box<Term> },
}

// A native HVM machine integer operation
#[derive(Clone, Copy, Debug)]
pub enum Oper {
  Add, Sub, Mul, Div,
  Mod, And, Or,  Xor,
  Shl, Shr, Lte, Ltn,
  Eql, Gte, Gtn, Neq,
}

// A left-hand side variable in a rewrite rule (equation)
#[derive(Clone, Debug)]
pub struct Var {
  pub name : u64,         // this variable's name
  pub param: u64,         // in what parameter is this variable located?
  pub field: Option<u64>, // in what field is this variabled located? (if any)
  pub erase: bool,        // should this variable be collected (because it is unused)?
}

// A rewrite rule (equation)
#[derive(Clone, Debug)]
pub struct Rule {
  pub cond: Vec<Lnk>,        // left-hand side matching conditions
  pub vars: Vec<Var>,        // left-hand side variable locations
  pub eras: Vec<(u64, u64)>, // must-clear locations (argument number and arity)
  pub body: Term,            // right-hand side body of rule
}

// A function is a vector of rules
#[derive(Clone, Debug)]
pub struct Func {
  arity: u64,       // number of arguments
  redux: Vec<u64>,  // index of strict arguments
  rules: Vec<Rule>, // vector of rules
}

// A file is a map of `FuncID -> Function`
#[derive(Clone, Debug)]
pub struct File {
  pub funcs: HashMap<u64, Func, BuildHasherDefault<NoHashHasher<u64>>>,
}

// A map of `FundID -> Arity`
#[derive(Clone, Debug)]
pub struct Arit {
  pub arits: HashMap<u64, u64, BuildHasherDefault<NoHashHasher<u64>>>,
}

// Can point to a node, a variable, or hold an unboxed value
pub type Lnk = u64;

// A mergeable vector of u64 values
#[derive(Debug, Clone)]
pub struct Blob {
  data: Vec<u64>,
  used: Vec<usize>,
}

// HVM's memory state (nodes, functions, metadata, statistics)
#[derive(Debug)]
pub struct Heap {
  pub data: Blob, // memory block holding HVM nodes
  pub file: File, // functions
  pub arit: Arit, // function arities
  pub tick: u64,  // time counter
  pub funs: u64,  // total function count
  pub dups: u64,  // total function count
  pub cost: u64,  // total cost count, in gas
  pub size: i64,  // total used memory (in 64-bit words)
  pub next: u64,  // memory index that *may* be empty
}

// A list of past heap states, for block-reorg rollback
#[derive(Debug)]
pub enum Rollback {
  Cons {
    keep: u64,
    head: Box<Heap>,
    tail: Box<Rollback>,
  },
  Nil
}

// The current and past states
pub struct Runtime {
  heap: Box<Heap>,      // current state
  back: Box<Rollback>,  // past states
  nuls: Vec<Box<Heap>>, // empty heaps (for reuse)
}

// Constants
// ---------

const U64_PER_KB: u64 = 0x80;
const U64_PER_MB: u64 = 0x20000;
const U64_PER_GB: u64 = 0x8000000;

const HEAP_SIZE: u64 = 256 * U64_PER_MB;
//const HEAP_SIZE: u64 = 256;

pub const MAX_ARITY: u64 = 16;
pub const MAX_FUNCS: u64 = 16777216; // TODO: increase to 2^30 once arity is moved out

pub const SEEN_SIZE: usize = 4194304; // uses 32 MB, covers heaps up to 2 GB
pub const VARS_SIZE: usize = 262144; // maximum variables per rule

pub const VAL: u64 = 1;
pub const EXT: u64 = 0b1000000000000000000000000000000;
pub const ARI: u64 = 0b100000000000000000000000000000000000000000000000000000000;
pub const TAG: u64 = 0b1000000000000000000000000000000000000000000000000000000000000;

pub const DP0: u64 = 0x0;
pub const DP1: u64 = 0x1;
pub const VAR: u64 = 0x2;
pub const ARG: u64 = 0x3;
pub const ERA: u64 = 0x4;
pub const LAM: u64 = 0x5;
pub const APP: u64 = 0x6;
pub const PAR: u64 = 0x7;
pub const CTR: u64 = 0x8;
pub const FUN: u64 = 0x9;
pub const OP2: u64 = 0xA;
pub const U60: u64 = 0xB;

pub const ADD: u64 = 0x0;
pub const SUB: u64 = 0x1;
pub const MUL: u64 = 0x2;
pub const DIV: u64 = 0x3;
pub const MOD: u64 = 0x4;
pub const AND: u64 = 0x5;
pub const OR : u64 = 0x6;
pub const XOR: u64 = 0x7;
pub const SHL: u64 = 0x8;
pub const SHR: u64 = 0x9;
pub const LTN: u64 = 0xA;
pub const LTE: u64 = 0xB;
pub const EQL: u64 = 0xC;
pub const GTE: u64 = 0xD;
pub const GTN: u64 = 0xE;
pub const NEQ: u64 = 0xF;

pub const U64_NONE: u64 = 0xFFFFFFFFFFFFFFFF;
pub const I64_NONE: i64 = -0x7FFFFFFFFFFFFFFF;

// Rollback
// --------

impl Heap {
  fn write(&mut self, idx: usize, val: u64) {
    return self.data.write(idx, val);
  }
  fn read(&self, idx: usize) -> u64 {
    return self.data.read(idx);
  }
  fn merge(&mut self, mut other: Self) -> Self {
    other.data = self.data.merge(other.data);
    other.file = self.file.merge(other.file);
    other.arit = self.arit.merge(other.arit);
    self.tick = self.tick + other.tick;
    self.funs = self.funs + other.funs;
    self.dups = self.dups + other.dups;
    self.cost = self.cost + other.cost;
    self.size = self.size + other.size;
    self.next = self.next;
    return other;
  }
  fn clear(&mut self) {
    self.data.clear();
    self.file.clear();
    self.arit.clear();
    self.tick = U64_NONE;
    self.funs = U64_NONE;
    self.dups = U64_NONE;
    self.cost = U64_NONE;
    self.size = I64_NONE;
    self.next = U64_NONE;
  }
}

pub fn init_heap() -> Heap {
  Heap {
    data: init_heapdata(U64_NONE),
    file: File { funcs: HashMap::with_hasher(BuildHasherDefault::default()) },
    arit: Arit { arits: HashMap::with_hasher(BuildHasherDefault::default()) },
    tick: U64_NONE,
    funs: U64_NONE,
    dups: U64_NONE,
    cost: U64_NONE,
    size: I64_NONE,
    next: U64_NONE,
  }
}

pub fn init_heapdata(zero: u64) -> Blob {
  return Blob {
    data: vec![zero; HEAP_SIZE as usize],
    used: vec![],
  };
}

impl Blob {
  fn write(&mut self, idx: usize, val: u64) {
    unsafe {
      let got = self.data.get_unchecked_mut(idx);
      if *got == U64_NONE {
        self.used.push(idx);
      }
      *got = val;
    }
  }
  fn read(&self, idx: usize) -> u64 {
    unsafe {
      return *self.data.get_unchecked(idx);
    }
  }
  fn clear(&mut self) {
    for idx in &self.used {
      unsafe {
        let val = self.data.get_unchecked_mut(*idx);
        *val = U64_NONE;
      }
    }
    self.used.clear();
  }
  fn merge(&mut self, mut other: Self) -> Self {
    for idx in &other.used {
      unsafe {
        let other_val = other.data.get_unchecked_mut(*idx);
        let self_val = self.data.get_unchecked_mut(*idx);
        if *self_val == U64_NONE {
          self.write(*idx, *other_val);
        }
      }
    }
    other.clear();
    return other;
  }
}

impl File {
  fn write(&mut self, fid: u64, val: Func) {
    self.funcs.insert(fid, val);
  }
  fn read(&self, fid: u64) -> Option<&Func> {
    return self.funcs.get(&fid);
  }
  fn clear(&mut self) {
    self.funcs.clear();
  }
  fn merge(&mut self, mut other: Self) -> Self {
    for (fid, func) in other.funcs.drain() {
      if !self.funcs.contains_key(&fid) {
        self.write(fid, func);
      }
    }
    other.clear();
    return other;
  }
}

impl Arit {
  fn write(&mut self, fid: u64, val: u64) {
    self.arits.insert(fid, val);
  }
  fn read(&self, fid: u64) -> Option<u64> {
    if let Some(arit) = self.arits.get(&fid) {
      return Some(*arit);
    } else {
      return None;
    }
  }
  fn clear(&mut self) {
    self.arits.clear();
  }
  fn merge(&mut self, mut other: Self) -> Self {
    for (fid, func) in other.arits.drain() {
      if !self.arits.contains_key(&fid) {
        self.arits.insert(fid, func);
      }
    }
    return other;
  }
}

pub fn init_rollback() -> Rollback {
  return Rollback::Nil;
  //return rollback_push(Box::new(init_heap()), Box::new(Rollback::Nil)).2;
}

// Attempts to include a heap state on the list of past heap states. It only keeps at most
// `log_16(tick)` heaps in memory, rejecting heaps that it doesn't need to store. It returns:
// - included : Bool = true if the heap was included, false if it was rejected
// - new_heap : Option<Box<Heap>> = either `None` or `Some(drop)`, where `drop` is:
//   - if the `heap` was included: an empty heap (to be reused)
//   - if the `heap` was rejected: that heap itself
// - rollback : Rollback = the updated list of past heap states
pub fn rollback_push(mut elem: Box<Heap>, back: Box<Rollback>) -> (bool, Option<Box<Heap>>, Rollback) {
  match *back {
    Rollback::Nil => {
      return (true, None, Rollback::Cons {
        keep: 0,
        head: elem,
        tail: Box::new(Rollback::Nil),
      })
    }
    Rollback::Cons { keep, head, tail } => {
      if keep == 0xF {
        let (included, mut lost, tail) = rollback_push(head, tail);
        if !included {
          if let Some(lost_val) = lost {
            lost = Some(Box::new(elem.merge(*lost_val)));
          }
        }
        return (true, lost, Rollback::Cons {
          keep: 0,
          head: elem,
          tail: Box::new(tail),
        });
      } else {
        return (false, Some(elem), Rollback::Cons {
          keep: keep + 1,
          head: head,
          tail: tail,
        });
      }
    }
  }
}

pub fn init_runtime() -> Runtime {
  let mut nuls = Vec::new();
  for i in 0 .. 8 {
    nuls.push(Box::new(init_heap()));
  }
  return Runtime {
    heap: Box::new(init_heap()),
    back: Box::new(init_rollback()),
    nuls: nuls,
  };
}

impl Runtime {

  // API
  // ---

  fn define_function(&mut self, fid: u64, func: Func) {
    self.heap.arit.write(fid, func.arity);
    self.heap.file.write(fid, func);
  }

  fn define_constructor(&mut self, cid: u64, arity: u64) {
    self.heap.arit.write(cid, arity);
  }

  fn create_term(&mut self, term: &Term) -> u64 {
    let loc = alloc(self, 1);
    let lnk = alloc_term(self, term, loc);
    self.write(loc as usize, lnk);
    return loc;
  }

  fn normalize(&mut self, loc: u64) -> Lnk {
    normal(self, loc)
  }

  fn show_term_at(&mut self, loc: u64) -> String {
    return show_term(self, self.read(loc as usize));
  }

  fn get_arity(&self, fid: u64) -> u64 {
    if let Some(arity) = self.heap.arit.read(fid) {
      return arity;
    } else {
      return 0;
    }
  }

  // Rollback
  // --------

  // Advances the heap time counter, saving past states for rollback.
  fn tick(mut self) {
    self.heap.tick += 1;
    let (_, drop, back) = rollback_push(self.heap, self.back);
    self.back = Box::new(back);
    self.heap = match drop {
      Some(heap) => { heap }
      None => {
        match self.nuls.pop() {
          Some(heap) => { heap }
          None => {
            // Shouldn't happen because we pre-alloc 9 heaps,
            // which is enough to store past states for up to
            // 68719476736 blocks, which is more than 6000 years
            panic!("Impossible error.");
          }
        }
      }
    };
  }

  // Rolls back to the earliest state before or equal `tick`
  // FIXME: remove functions from file; actually not necessary, 
  fn rollback(mut self, tick: u64) {
    // If current heap is older than the target tick
    if self.heap.tick > tick {
      let init_funs = self.heap.funs;
      let mut done;
      let mut back = *self.back;
      // Removes all heaps that are older than the target tick
      loop {
        (done, back) = match back {
          Rollback::Cons { keep, mut head, tail } => {
            if head.tick > tick {
              head.clear();
              self.nuls.push(head);
              (false, *tail)
            } else {
              (true, Rollback::Cons { keep, head, tail })
            }
          }
          Rollback::Nil => {
            (true, Rollback::Nil)
          }
        };
        if done {
          break;
        }
      }
      // Moves the most recent valid heap to `self.heap`
      match back {
        Rollback::Cons { keep, head, tail } => {
          self.back = tail;
          self.heap = head;
        }
        Rollback::Nil => {
          self.back = Box::new(Rollback::Nil);
          self.heap = self.nuls.pop().expect("Impossible error.");
        }
      }
    }
  }

  // Heap writers and readers
  // ------------------------

  // Attempts to read data from the latest heap.
  // If not present, looks for it on past states.
  fn get_with<A: std::cmp::PartialEq>(&self, zero: A, none: A, get: impl Fn(&Heap) -> A) -> A {
    let got = get(&self.heap);
    if got != none {
      return got;
    } else {
      let mut back = &self.back;
      loop {
        match &**back {
          Rollback::Cons { keep, head, tail } => {
            let val = get(&head);
            if val != none {
              return val;
            }
            back = &*tail;
          }
          Rollback::Nil => {
            return zero;
          }
        }
      }
    }
  }

  fn write(&mut self, idx: usize, val: u64) {
    return self.heap.write(idx, val);
  }

  fn read(&self, idx: usize) -> u64 {
    return self.get_with(0, U64_NONE, |heap| heap.read(idx));
  }

  fn set_tick(&mut self, tick: u64) {
    self.heap.tick = tick;
  }

  fn get_tick(&self) -> u64 {
    return self.get_with(0, U64_NONE, |heap| heap.tick);
  }

  fn set_funs(&mut self, funs: u64) {
    self.heap.funs = funs;
  }

  fn get_funs(&self) -> u64 {
    return self.get_with(0, U64_NONE, |heap| heap.funs);
  }

  fn set_dups(&mut self, dups: u64) {
    self.heap.dups = dups;
  }

  fn get_dups(&self) -> u64 {
    return self.get_with(0, U64_NONE, |heap| heap.dups);
  }

  fn set_cost(&mut self, cost: u64) {
    self.heap.cost = cost;
  }

  fn get_cost(&self) -> u64 {
    return self.get_with(0, U64_NONE, |heap| heap.cost);
  }

  fn set_size(&mut self, size: i64) {
    self.heap.size = size;
  }

  fn get_size(&self) -> i64 {
    return self.get_with(0, I64_NONE, |heap| heap.size);
  }

  fn set_next(&mut self, next: u64) {
    self.heap.next = next;
  }

  fn get_next(&self) -> u64 {
    return self.get_with(0, U64_NONE, |heap| heap.next);
  }

  fn fresh_dups(&mut self) -> u64 {
    let dups = self.get_dups();
    self.set_dups(dups + 1);
    return dups & 0x3FFFFFFF;
  }

}

// Globals
// -------

static mut SEEN_DATA: [u64; SEEN_SIZE] = [0; SEEN_SIZE];
static mut VARS_DATA: [Option<u64>; VARS_SIZE] = [None; VARS_SIZE];
static mut CALL_COUNT: &'static mut [u64] = &mut [0; MAX_FUNCS as usize];

// Constructors
// ------------

pub fn Var(pos: u64) -> Lnk {
  (VAR * TAG) | pos
}

pub fn Dp0(col: u64, pos: u64) -> Lnk {
  (DP0 * TAG) | (col * EXT) | pos
}

pub fn Dp1(col: u64, pos: u64) -> Lnk {
  (DP1 * TAG) | (col * EXT) | pos
}

pub fn Arg(pos: u64) -> Lnk {
  (ARG * TAG) | pos
}

pub fn Era() -> Lnk {
  ERA * TAG
}

pub fn Lam(pos: u64) -> Lnk {
  (LAM * TAG) | pos
}

pub fn App(pos: u64) -> Lnk {
  (APP * TAG) | pos
}

pub fn Par(col: u64, pos: u64) -> Lnk {
  (PAR * TAG) | (col * EXT) | pos
}

pub fn Op2(ope: u64, pos: u64) -> Lnk {
  (OP2 * TAG) | (ope * EXT) | pos
}

pub fn U_32(val: u64) -> Lnk {
  (U60 * TAG) | val
}

pub fn Ctr(fun: u64, pos: u64) -> Lnk {
  (CTR * TAG) | (fun * EXT) | pos
}

pub fn Fun(fun: u64, pos: u64) -> Lnk {
  (FUN * TAG) | (fun * EXT) | pos
}

// Getters
// -------

pub fn get_tag(lnk: Lnk) -> u64 {
  lnk / TAG
}

pub fn get_ext(lnk: Lnk) -> u64 {
  (lnk / EXT) & 0x3FFFFFFF
}

pub fn get_val(lnk: Lnk) -> u64 {
  lnk & 0x3FFFFFFF
}

pub fn get_num(lnk: Lnk) -> u64 {
  lnk & 0xFFFFFFFFFFFFFFF
}

//pub fn get_ari(lnk: Lnk) -> u64 {
  //(lnk / ARI) & 0xF
//}

pub fn get_loc(lnk: Lnk, arg: u64) -> u64 {
  get_val(lnk) + arg
}

// Memory
// ------

pub fn ask_lnk(rt: &Runtime, loc: u64) -> Lnk {
  rt.read(loc as usize)
  //unsafe { *rt.data.get_unchecked(loc as usize) }
}

pub fn ask_arg(rt: &Runtime, term: Lnk, arg: u64) -> Lnk {
  ask_lnk(rt, get_loc(term, arg))
}

pub fn link(rt: &mut Runtime, loc: u64, lnk: Lnk) -> Lnk {
  rt.write(loc as usize, lnk);
  //*rt.data.get_unchecked_mut(loc as usize) = lnk;
  if get_tag(lnk) <= VAR {
    let pos = get_loc(lnk, get_tag(lnk) & 0x01);
    rt.write(pos as usize, Arg(loc));
    //*rt.data.get_unchecked_mut(pos as usize) = Arg(loc);
  }
  lnk
}

pub fn alloc(rt: &mut Runtime, size: u64) -> u64 {
  if size == 0 {
    return 0;
  } else {
    loop {
      let index = rt.get_next();
      if index <= HEAP_SIZE - size {
        let mut empty = true;
        for i in 0 .. size {
          if rt.read((index + i) as usize) != 0 {
            empty = false;
            break;
          }
        }
        if empty {
          rt.set_next(rt.get_next() + size);
          rt.set_size(rt.get_size() + size as i64);
          return index;
        }
      }
      rt.set_next(fastrand::u64(..) % HEAP_SIZE);
    }
  }
}

pub fn clear(rt: &mut Runtime, loc: u64, size: u64) {
  for i in 0 .. size {
    rt.write((loc + i) as usize, 0);
  }
  rt.set_size(rt.get_size() - size as i64);
  //rt.free[size as usize].push(loc);
}

pub fn collect(rt: &mut Runtime, term: Lnk) {
  let mut stack : Vec<Lnk> = Vec::new();
  let mut next = term;
  loop {
    let term = next;
    match get_tag(term) {
      DP0 => {
        link(rt, get_loc(term, 0), Era());
        reduce(rt, get_loc(ask_arg(rt,term,1),0));
      }
      DP1 => {
        link(rt, get_loc(term, 1), Era());
        reduce(rt, get_loc(ask_arg(rt,term,0),0));
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
      PAR => {
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
      U60 => {}
      CTR | FUN => {
        let arity = rt.get_arity(get_ext(term));
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
}

pub fn inc_cost(rt: &mut Runtime) {
  rt.set_cost(rt.get_cost() + 1);
}

// Term
// ----

// Writes a Term represented as a Rust enum on the Runtime's rt.
pub fn alloc_term(rt: &mut Runtime, term: &Term, loc: u64) -> Lnk {
  fn bind(rt: &mut Runtime, loc: u64, name: u64, lnk: Lnk) {
    //println!("~~ bind {} {}", u64_to_name(name), show_lnk(lnk));
    unsafe {
      match VARS_DATA[name as usize] {
        Some(got) => {
          VARS_DATA[name as usize] = None;
          link(rt, got, lnk);
        }
        None => {
          VARS_DATA[name as usize] = Some(lnk);
          link(rt, loc, Era());
        }
      }
    }
  }
  match term {
    Term::Var { name } => {
      unsafe {
        //println!("~~ var {} {}", u64_to_name(*name), VARS_DATA.len());
        if (*name as usize) < VARS_DATA.len() {
          match VARS_DATA[*name as usize] {
            Some(got) => {
              VARS_DATA[*name as usize] = None;
              return got;
            }
            None => {
              VARS_DATA[*name as usize] = Some(loc);
              return U_32(0);
            }
          }
        } else {
          return U_32(0);
        }
      }
    }
    Term::Dup { nam0, nam1, expr, body } => {
      let node = alloc(rt, 3);
      let dupk = rt.get_dups();
      bind(rt, node + 0, *nam0, Dp0(dupk, node));
      bind(rt, node + 1, *nam1, Dp1(dupk, node));
      let expr = alloc_term(rt, expr, node + 2);
      link(rt, node + 2, expr);
      let body = alloc_term(rt, body, loc);
      body
    }
    Term::Lam { name, body } => {
      let node = alloc(rt, 2);
      bind(rt, node + 0, *name, Var(node));
      let body = alloc_term(rt, body, node + 1);
      link(rt, node + 1, body);
      Lam(node)
    }
    Term::App { func, argm } => {
      let node = alloc(rt, 2);
      let func = alloc_term(rt, func, node + 0);
      link(rt, node + 0, func);
      let argm = alloc_term(rt, argm, node + 1);
      link(rt, node + 1, argm);
      App(node)
    }
    Term::Fun { name, args } => {
      let size = args.len() as u64;
      let node = alloc(rt, size);
      for (i, arg) in args.iter().enumerate() {
        let arg_lnk = alloc_term(rt, arg, node + i as u64);
        link(rt, node + i as u64, arg_lnk);
      }
      Fun(*name, node)
    }
    Term::Ctr { name, args } => {
      let size = args.len() as u64;
      let node = alloc(rt, size);
      for (i, arg) in args.iter().enumerate() {
        let arg_lnk = alloc_term(rt, arg, node + i as u64);
        link(rt, node + i as u64, arg_lnk);
      }
      Ctr(*name, node)
    }
    Term::U60 { numb } => {
      U_32(*numb as u64)
    }
    Term::Op2 { oper, val0, val1 } => {
      let node = alloc(rt, 2);
      let val0 = alloc_term(rt, val0, node + 0);
      link(rt, node + 0, val0);
      let val1 = alloc_term(rt, val1, node + 1);
      link(rt, node + 1, val1);
      Op2(*oper, node)
    }
  }
}

// Given a vector of rules (lhs/rhs pairs), builds the Func object
pub fn build_func(lines: &[(Term,Term)]) -> Option<Func> {
  // If there are no rules, return none
  if lines.len() == 0 {
    return None;
  }

  // Find the function arity
  let arity;
  if let Term::Fun { args, .. } = &lines[0].0 {
    arity = args.len() as u64;
  } else {
    return None;
  }

  // The resulting vector
  let mut rules = Vec::new();

  // A vector with the indices that are strict
  let mut strict = vec![false; arity as usize];

  // For each rule (lhs/rhs pair)
  for i in 0 .. lines.len() {
    let rule = &lines[i];

    let mut cond = Vec::new();
    let mut vars = Vec::new();
    let mut eras = Vec::new();

    // If the lhs is a Fun
    if let Term::Fun { ref name, ref args } = rule.0 {

      // If there is an arity mismatch, return None
      if args.len() as u64 != arity {
        return None;
      }

      // For each lhs argument
      for i in 0 .. args.len() as u64 {
        
        match &args[i as usize] {
          // If it is a constructor...
          Term::Ctr { name: arg_name, args: arg_args } => {
            strict[i as usize] = true;
            cond.push(Ctr(*arg_name, 0)); // adds its matching condition
            eras.push((i, arg_args.len() as u64)); // marks its index and arity for freeing
            // For each of its fields...
            for j in 0 .. arg_args.len() as u64 {
              // If it is a variable...
              if let Term::Var { name } = arg_args[j as usize] {
                vars.push(Var { name, param: i, field: Some(j), erase: name == U64_NONE }); // add its location
              // Otherwise..
              } else {
                return None; // return none, because we don't allow nested matches
              }
            }
          }
          // If it is a number...
          Term::U60 { numb: arg_numb } => {
            strict[i as usize] = true;
            cond.push(U_32(*arg_numb as u64)); // adds its matching condition
          }
          // If it is a variable...
          Term::Var { name: arg_name } => {
            vars.push(Var { name: *arg_name, param: i, field: None, erase: *arg_name == U64_NONE }); // add its location
            cond.push(0); // it has no matching condition
          }
          _ => {
            return None;
          }
        }
      }

    // If lhs isn't a Ctr, return None
    } else {
      return None;
    }

    // Creates the rhs body
    let body = rule.1.clone();

    // Adds the rule to the result vector
    rules.push(Rule { cond, vars, eras, body });
  }

  // Builds the redux object, with the index of strict arguments
  let mut redux = Vec::new();
  for i in 0 .. strict.len() {
    if strict[i] {
      redux.push(i as u64);
    }
  }

  return Some(Func { arity, redux, rules });
}


// Reduction
// ---------

pub fn subst(rt: &mut Runtime, lnk: Lnk, val: Lnk) {
  if get_tag(lnk) != ERA {
    link(rt, get_loc(lnk, 0), val);
  } else {
    collect(rt, val);
  }
}

pub fn reduce(rt: &mut Runtime, root: u64) -> Lnk {

  // Separates runtime from file to satisfy the borrow checker
  // FIXME: this isn't good code; should split Runtime instead
  let mut file = File { funcs: HashMap::with_hasher(BuildHasherDefault::default()) };
  std::mem::swap(&mut rt.heap.file, &mut file);

  let mut stack: Vec<u64> = Vec::new();

  let mut init = 1;
  let mut host = root;

  loop {
    let term = ask_lnk(rt, host);

    //if debug || true {
      //println!("------------------------");
      //println!("{}", show_term(rt, ask_lnk(rt, 0)));
    //}

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
          stack.push(get_loc(term, 1) | 0x80000000);
          host = get_loc(term, 0);
          continue;
        }
        FUN => {
          let fun = get_ext(term);
          let ari = rt.get_arity(fun);
          if let Some(func) = &file.funcs.get(&fun) {
            if ari == func.arity {
              if func.redux.len() == 0 {
                init = 0;
              } else {
                stack.push(host);
                for (i, redux) in func.redux.iter().enumerate() {
                  if i < func.redux.len() - 1 {
                    stack.push(get_loc(term, *redux) | 0x80000000);
                  } else {
                    host = get_loc(term, *redux);
                  }
                }
              }
              continue;
            }
          }
        }
        _ => {}
      }
    } else {
      match get_tag(term) {
        APP => {
          let arg0 = ask_arg(rt, term, 0);
          if get_tag(arg0) == LAM {
            //println!("app-lam");
            inc_cost(rt);
            subst(rt, ask_arg(rt, arg0, 0), ask_arg(rt, term, 1));
            let _done = link(rt, host, ask_arg(rt, arg0, 1));
            clear(rt, get_loc(term, 0), 2);
            clear(rt, get_loc(arg0, 0), 2);
            init = 1;
            continue;
          }
          if get_tag(arg0) == PAR {
            //println!("app-sup");
            inc_cost(rt);
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
          // let argK = ask_arg(rt, term, if get_tag(term) == DP0 { 1 } else { 0 });
          // if get_tag(argK) == ERA {
          //   let done = arg0;
          //   link(rt, host, done);
          //   init = 1;
          //   continue;
          // }
          if get_tag(arg0) == LAM {
            //println!("dup-lam");
            inc_cost(rt);
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
          } else if get_tag(arg0) == PAR {
            //println!("dup-sup");
            if get_ext(term) == get_ext(arg0) {
              inc_cost(rt);
              subst(rt, ask_arg(rt, term, 0), ask_arg(rt, arg0, 0));
              subst(rt, ask_arg(rt, term, 1), ask_arg(rt, arg0, 1));
              let _done = link(rt, host, ask_arg(rt, arg0, if get_tag(term) == DP0 { 0 } else { 1 }));
              clear(rt, get_loc(term, 0), 3);
              clear(rt, get_loc(arg0, 0), 2);
              init = 1;
              continue;
            } else {
              inc_cost(rt);
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
          } else if get_tag(arg0) == U60 {
            //println!("dup-u32");
            inc_cost(rt);
            subst(rt, ask_arg(rt, term, 0), arg0);
            subst(rt, ask_arg(rt, term, 1), arg0);
            clear(rt, get_loc(term, 0), 3);
            let _done = arg0;
            link(rt, host, arg0);
          } else if get_tag(arg0) == CTR {
            //println!("dup-ctr");
            inc_cost(rt);
            let func = get_ext(arg0);
            let arit = rt.get_arity(func);
            if arit == 0 {
              subst(rt, ask_arg(rt, term, 0), Ctr(func, 0));
              subst(rt, ask_arg(rt, term, 1), Ctr(func, 0));
              clear(rt, get_loc(term, 0), 3);
              let _done = link(rt, host, Ctr(func, 0));
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
              link(rt, ctr0 + arit - 1, Dp0(get_ext(term), leti));
              subst(rt, term_arg_0, Ctr(func, ctr0));
              let term_arg_1 = ask_arg(rt, term, 1);
              link(rt, ctr1 + arit - 1, Dp1(get_ext(term), leti));
              subst(rt, term_arg_1, Ctr(func, ctr1));
              let done = Ctr(func, if get_tag(term) == DP0 { ctr0 } else { ctr1 });
              link(rt, host, done);
            }
          } else if get_tag(arg0) == ERA {
            inc_cost(rt);
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
          if get_tag(arg0) == U60 && get_tag(arg1) == U60 {
            //println!("op2-u32");
            inc_cost(rt);
            let a = get_num(arg0);
            let b = get_num(arg1);
            let c = match get_ext(term) {
              ADD => (a + b)  & 0xFFFFFFFFFFFFFFF,
              SUB => (a - b)  & 0xFFFFFFFFFFFFFFF,
              MUL => (a * b)  & 0xFFFFFFFFFFFFFFF,
              DIV => (a / b)  & 0xFFFFFFFFFFFFFFF,
              MOD => (a % b)  & 0xFFFFFFFFFFFFFFF,
              AND => (a & b)  & 0xFFFFFFFFFFFFFFF,
              OR  => (a | b)  & 0xFFFFFFFFFFFFFFF,
              XOR => (a ^ b)  & 0xFFFFFFFFFFFFFFF,
              SHL => (a << b) & 0xFFFFFFFFFFFFFFF,
              SHR => (a >> b) & 0xFFFFFFFFFFFFFFF,
              LTN => u64::from(a <  b),
              LTE => u64::from(a <= b),
              EQL => u64::from(a == b),
              GTE => u64::from(a >= b),
              GTN => u64::from(a >  b),
              NEQ => u64::from(a != b),
              _   => 0,
            };
            let done = U_32(c);
            clear(rt, get_loc(term, 0), 2);
            link(rt, host, done);
          } else if get_tag(arg0) == PAR {
            //println!("op2-sup-0");
            inc_cost(rt);
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
          } else if get_tag(arg1) == PAR {
            //println!("op2-sup-1");
            inc_cost(rt);
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
          let fun = get_ext(term);
          let ari = rt.get_arity(fun);

          //println!("- on call {} | {}", get_loc(term, 0), show_lnk(term));

          if let Some(func) = &file.funcs.get(&fun) {

          //println!("- calling");

            let mut cont = false;

            // For each argument, if it is a redex and a PAR, apply the cal_par rule
            for idx in &func.redux {
              if get_tag(ask_arg(rt, term, *idx)) == PAR {
                //println!("cal-par");
                inc_cost(rt);
                let argn = ask_arg(rt, term, *idx);
                let func = get_ext(term);
                let arit = rt.get_arity(func);
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
                link(rt, par0 + 0, Fun(func, fun0));
                link(rt, par0 + 1, Fun(func, fun1));
                let done = Par(get_ext(argn), par0);
                link(rt, host, done);
                cont = true;
                break;
              }
            }

            // If applied the PAR rule, continue
            if cont {
              init = 1;
              continue;
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
                  U60 => {
                    //println!("Didn't match because of U60. i={} {} {}", i, get_val(ask_arg(rt, term, i)), get_val(cond));
                    let same_tag = get_tag(ask_arg(rt, term, i)) == U60;
                    let same_val = get_val(ask_arg(rt, term, i)) == get_val(cond);
                    matched = matched && same_tag && same_val;
                  }
                  CTR => {
                    //println!("Didn't match because of CTR. i={} {} {}", i, get_tag(ask_arg(rt, term, i)), get_val(cond));
                    let same_tag = get_tag(ask_arg(rt, term, i)) == CTR;
                    let same_ext = get_ext(ask_arg(rt, term, i)) == get_ext(cond);
                    matched = matched && same_tag && same_ext;
                  }
                  _ => {}
                }
              }

              // If all conditions are satisfied, the rule matched, so we must apply it
              if matched {
                //println!("cal-fun");
                //println!("- matched");

                // Increments the gas count
                inc_cost(rt);

                // Gathers matched variables
                //let mut vars = vec![None; 16]; // FIXME: pre-alloc statically
                for i in 0 .. rule.vars.len() {
                  let mut var = term;
                  var = ask_arg(rt, var, rule.vars[i].param);
                  if let Some(field) = rule.vars[i].field {
                    var = ask_arg(rt, var, field);
                  }
                  unsafe {
                    //println!("~~ set {} {}", u64_to_name(rule.vars[i].name), show_lnk(var));
                    VARS_DATA[rule.vars[i].name as usize] = Some(var);
                  }
                }

                // Builds the right-hand side term (ex: `(Succ (Add a b))`)
                //println!("-- alloc {:?}", rule.body);
                //println!("-- vars: {:?}", vars);
                let done = alloc_term(rt, &rule.body, host);

                // Links the host location to it
                link(rt, host, done);

                // Clears the matched ctrs (the `(Succ ...)` and the `(Add ...)` ctrs)
                for (eras_index, eras_arity) in &rule.eras {
                  clear(rt, get_loc(ask_arg(rt, term, *eras_index), 0), *eras_arity);
                }
                clear(rt, get_loc(term, 0), func.arity);

                // Collects unused variables (none in this example)
                for i in 0 .. rule.vars.len() {
                  if rule.vars[i].erase {
                    unsafe {
                      if let Some(var) = VARS_DATA[i] {
                        collect(rt, var);
                      }
                    }
                  }
                }

                cont = true;
                break;
              }
                      
            }

            // If applied a function, continue
            if cont {
              init = 1;
              continue;
            }
          }
        }
        _ => {}
      }
    }

    if let Some(item) = stack.pop() {
      init = item >> 31;
      host = item & 0x7FFFFFFF;
      continue;
    }

    break;
  }

  // FIXME: remove this when Runtime is split (see above)
  rt.heap.file = file;

  ask_lnk(rt, root)
}

pub fn set_bit(bits: &mut [u64], bit: u64) {
  bits[bit as usize >> 6] |= 1 << (bit & 0x3f);
}

pub fn get_bit(bits: &[u64], bit: u64) -> bool {
  (((bits[bit as usize >> 6] >> (bit & 0x3f)) as u8) & 1) == 1
}

pub fn normal_go(rt: &mut Runtime, host: u64, seen: &mut [u64]) -> Lnk {
  let term = ask_lnk(rt, host);
  if get_bit(seen, host) {
    term
  } else {
    let term = reduce(rt, host);
    set_bit(seen, host);
    let mut rec_locs = Vec::with_capacity(16);
    match get_tag(term) {
      LAM => {
        rec_locs.push(get_loc(term, 1));
      }
      APP => {
        rec_locs.push(get_loc(term, 0));
        rec_locs.push(get_loc(term, 1));
      }
      PAR => {
        rec_locs.push(get_loc(term, 0));
        rec_locs.push(get_loc(term, 1));
      }
      DP0 => {
        rec_locs.push(get_loc(term, 2));
      }
      DP1 => {
        rec_locs.push(get_loc(term, 2));
      }
      CTR | FUN => {
        let arity = rt.get_arity(get_ext(term));
        for i in 0..arity {
          rec_locs.push(get_loc(term, i));
        }
      }
      _ => {}
    }
    for loc in rec_locs {
      let lnk: Lnk = normal_go(rt, loc, seen);
      link(rt, loc, lnk);
    }
    term
  }
}

pub fn normal(rt: &mut Runtime, host: u64) -> Lnk {
  let mut done;
  let mut cost = rt.get_cost();
  loop {
    let mut seen = vec![0; 4194304];
    done = normal_go(rt, host, &mut seen);
    if rt.get_cost() != cost {
      cost = rt.get_cost();
    } else {
      break;
    }
  }
  done
}

// Debug
// -----

pub fn show_lnk(x: Lnk) -> String {
  if x == 0 {
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
      PAR => "PAR",
      CTR => "CTR",
      FUN => "FUN",
      OP2 => "OP2",
      U60 => "U60",
      _   => "?",
    };
    format!("{}:{:x}:{:x}", tgs, ext, val)
  }
}

pub fn show_rt(rt: &Runtime) -> String {
  let mut s: String = String::new();
  for i in 0..32 {
    // pushes to the string
    s.push_str(&format!("{:x} | ", i));
    s.push_str(&show_lnk(rt.read(i)));
    s.push('\n');
  }
  s
}

pub fn show_term(rt: &Runtime, term: Lnk) -> String {
  let mut lets: HashMap<u64, u64> = HashMap::new();
  let mut kinds: HashMap<u64, u64> = HashMap::new();
  let mut names: HashMap<u64, String> = HashMap::new();
  let mut count: u64 = 0;
  fn find_lets(
    rt: &Runtime,
    term: Lnk,
    lets: &mut HashMap<u64, u64>,
    kinds: &mut HashMap<u64, u64>,
    names: &mut HashMap<u64, String>,
    count: &mut u64,
  ) {
    match get_tag(term) {
      LAM => {
        names.insert(get_loc(term, 0), format!("{}", count));
        *count += 1;
        find_lets(rt, ask_arg(rt, term, 1), lets, kinds, names, count);
      }
      APP => {
        find_lets(rt, ask_arg(rt, term, 0), lets, kinds, names, count);
        find_lets(rt, ask_arg(rt, term, 1), lets, kinds, names, count);
      }
      PAR => {
        find_lets(rt, ask_arg(rt, term, 0), lets, kinds, names, count);
        find_lets(rt, ask_arg(rt, term, 1), lets, kinds, names, count);
      }
      DP0 => {
        if let hash_map::Entry::Vacant(e) = lets.entry(get_loc(term, 0)) {
          names.insert(get_loc(term, 0), format!("{}", count));
          *count += 1;
          kinds.insert(get_loc(term, 0), get_ext(term));
          e.insert(get_loc(term, 0));
          find_lets(rt, ask_arg(rt, term, 2), lets, kinds, names, count);
        }
      }
      DP1 => {
        if let hash_map::Entry::Vacant(e) = lets.entry(get_loc(term, 0)) {
          names.insert(get_loc(term, 0), format!("{}", count));
          *count += 1;
          kinds.insert(get_loc(term, 0), get_ext(term));
          e.insert(get_loc(term, 0));
          find_lets(rt, ask_arg(rt, term, 2), lets, kinds, names, count);
        }
      }
      OP2 => {
        find_lets(rt, ask_arg(rt, term, 0), lets, kinds, names, count);
        find_lets(rt, ask_arg(rt, term, 1), lets, kinds, names, count);
      }
      CTR | FUN => {
        let arity = rt.get_arity(get_ext(term));
        for i in 0 .. arity {
          find_lets(rt, ask_arg(rt, term, i), lets, kinds, names, count);
        }
      }
      _ => {}
    }
  }
  fn go(rt: &Runtime, term: Lnk, names: &HashMap<u64, String>) -> String {
    let done = match get_tag(term) {
      DP0 => {
        format!("a{}", names.get(&get_loc(term, 0)).unwrap_or(&String::from("?a")))
      }
      DP1 => {
        format!("b{}", names.get(&get_loc(term, 0)).unwrap_or(&String::from("?b")))
      }
      VAR => {
        format!("x{}", names.get(&get_loc(term, 0)).unwrap_or(&String::from("?c")))
      }
      LAM => {
        let name = format!("x{}", names.get(&get_loc(term, 0)).unwrap_or(&String::from("?d")));
        format!("λ{} {}", name, go(rt, ask_arg(rt, term, 1), names))
      }
      APP => {
        let func = go(rt, ask_arg(rt, term, 0), names);
        let argm = go(rt, ask_arg(rt, term, 1), names);
        format!("({} {})", func, argm)
      }
      PAR => {
        //let kind = get_ext(term);
        let func = go(rt, ask_arg(rt, term, 0), names);
        let argm = go(rt, ask_arg(rt, term, 1), names);
        format!("{{{} {}}}", func, argm)
      }
      OP2 => {
        let oper = get_ext(term);
        let val0 = go(rt, ask_arg(rt, term, 0), names);
        let val1 = go(rt, ask_arg(rt, term, 1), names);
        let symb = match oper {
          ADD => "+",
          SUB => "-",
          MUL => "*",
          DIV => "/",
          MOD => "%",
          AND => "&",
          OR  => "|",
          XOR => "^",
          SHL => "<<",
          SHR => ">>",
          LTN => "<",
          LTE => "<=",
          EQL => "=",
          GTE => ">=",
          GTN => ">",
          NEQ => "!=",
          _   => "?",
        };
        format!("({} {} {})", symb, val0, val1)
      }
      U60 => {
        format!("#{}", get_num(term))
      }
      CTR => {
        let func = get_ext(term);
        let arit = rt.get_arity(func);
        let args: Vec<String> = (0..arit).map(|i| go(rt, ask_arg(rt, term, i), names)).collect();
        format!("(#{}{})", u64_to_name(func), args.iter().map(|x| format!(" {}", x)).collect::<String>())
      }
      FUN => {
        let func = get_ext(term);
        let arit = rt.get_arity(func);
        let args: Vec<String> = (0..arit).map(|i| go(rt, ask_arg(rt, term, i), names)).collect();
        format!("(@{}{})", u64_to_name(func), args.iter().map(|x| format!(" {}", x)).collect::<String>())
      }
      ERA => {
        format!("*")
      }
      _ => format!("?g({})", get_tag(term)),
    };
    return done;
  }
  find_lets(rt, term, &mut lets, &mut kinds, &mut names, &mut count);
  let mut text = go(rt, term, &names);
  for (_key, pos) in lets {
    // todo: reverse
    let what = String::from("?h");
    //let kind = kinds.get(&key).unwrap_or(&0);
    let name = names.get(&pos).unwrap_or(&what);
    let nam0 = if ask_lnk(rt, pos + 0) == Era() { String::from("*") } else { format!("a{}", name) };
    let nam1 = if ask_lnk(rt, pos + 1) == Era() { String::from("*") } else { format!("b{}", name) };
    text.push_str(&format!("\n& {} {} = {};", nam0, nam1, go(rt, ask_lnk(rt, pos + 2), &names)));
  }
  text
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

fn skip(code: &str) -> &str {
  let mut code = code;
  while head(code) == ' ' || head(code) == '\n' {
    code = tail(code);
  }
  return code;
}

fn hash(name: &str) -> u64 {
  let mut hasher = DefaultHasher::new();
  name.hash(&mut hasher);
  hasher.finish()
}

fn is_name_char(chr: char) -> bool {
  return chr == '_' || chr == '.'
      || chr >= 'a' && chr <= 'z'
      || chr >= 'A' && chr <= 'Z'
      || chr >= '0' && chr <= '9';
}

fn read_char(code: &str, chr: char) -> (&str, ()) {
  let code = skip(code);
  if head(code) == chr {
    return (tail(code), ());
  } else {
    panic!("Expected {}, found {}.", chr, head(code));
  }
}

fn read_numb(code: &str) -> (&str, u64) {
  let code = skip(code);
  let mut numb = 0;
  let mut code = code;
  while head(code) >= '0' && head(code) <= '9' {
    numb = numb * 10 + head(code) as u64 - 0x30;
    code = tail(code);
  }
  return (code, numb);
}

fn read_name(code: &str) -> (&str, u64) {
  let code = skip(code);
  let mut name = String::new();
  let mut code = code;
  while is_name_char(head(code)) {
    name.push(head(code));
    code = tail(code);
  }
  return (code, name_to_u64(&name));
}

// Converts a name to a number, using the following table:
// '.'       => 0
// '0' - '9' => 1 - 10
// 'A' - 'Z' => 11 - 36
// 'a' - 'z' => 37 - 62
// '_'       => 63
fn name_to_u64(code: &str) -> u64 {
  let mut num = 0;
  for chr in code.chars() {
    if chr == '.' {
      num = num * 64 + 0;
    } else if chr >= '0' && chr <= '9' {
      num = num * 64 + 1 + chr as u64 - '0' as u64;
    } else if chr >= 'A' && chr <= 'Z' {
      num = num * 64 + 11 + chr as u64 - 'A' as u64;
    } else if chr >= 'a' && chr <= 'z' {
      num = num * 64 + 37 + chr as u64 - 'a' as u64;
    } else if chr == '_' {
      num = num * 64 + 63;
    }
  }
  return num;
}

fn u64_to_name(num: u64) -> String {
  let mut name = String::new();
  let mut num = num;
  while num > 0 {
    let chr = (num % 64) as u8;
    if chr == 0 {
      name.push('.');
    } else if chr < 10 {
      name.push((chr + 0 + '0' as u8) as char);
    } else if chr < 36 {
      name.push((chr - 11 + 'A' as u8) as char);
    } else if chr < 62 {
      name.push((chr - 37 + 'a' as u8) as char);
    } else if chr == 63 {
      name.push('_');
    }
    num = num / 64;
  }
  name.chars().rev().collect()
}

fn read_until<A>(code: &str, stop: char, read: fn(&str) -> (&str, A)) -> (&str, Vec<A>) {
  let mut elems = Vec::new();
  let mut code = code;
  while code.len() > 0 && head(skip(code)) != stop {
    let (new_code, elem) = read(code);
    code = new_code;
    elems.push(elem);
  }
  code = tail(skip(code));
  return (code, elems);
}

fn read_term(code: &str) -> (&str, Term) {
  let code = skip(code);
  match head(code) {
    'λ' => {
      let code         = tail(code);
      let (code, name) = read_name(code);
      let (code, body) = read_term(code);
      return (code, Term::Lam { name, body: Box::new(body) });
    },
    '&' => {
      let code         = tail(code);
      let (code, nam0) = read_name(code);
      let (code, nam1) = read_name(code);
      let (code, skip) = read_char(code, '=');
      let (code, expr) = read_term(code);
      let (code, skip) = read_char(code, ';');
      let (code, body) = read_term(code);
      return (code, Term::Dup { nam0, nam1, expr: Box::new(expr), body: Box::new(body) });
    },
    '(' => {
      let code = tail(code);
      if head(code) == '+' {
        let code = tail(code);
        let (code, val0) = read_term(code);
        let (code, val1) = read_term(code);
        let (code, skip) = read_char(code, ')');
        return (code, Term::Op2 { oper: ADD, val0: Box::new(val0), val1: Box::new(val1) });
      } else if head(code) == '-' {
        let code = tail(code);
        let (code, val0) = read_term(code);
        let (code, val1) = read_term(code);
        let (code, skip) = read_char(code, ')');
        return (code, Term::Op2 { oper: SUB, val0: Box::new(val0), val1: Box::new(val1) });
      } else if head(code) == '#' {
        let code = tail(code);
        let (code, name) = read_name(code);
        let (code, args) = read_until(code, ')', read_term);
        return (code, Term::Ctr { name, args });
      } else if head(code) == '@' {
        let code = tail(code);
        let (code, name) = read_name(code);
        let (code, args) = read_until(code, ')', read_term);
        return (code, Term::Fun { name, args });
      } else {
        let (code, func) = read_term(code);
        let (code, argm) = read_term(code);
        let (code, skip) = read_char(code, ')');
        return (code, Term::App { func: Box::new(func), argm: Box::new(argm) });
      }
    },
    '#' => {
      let code = tail(code);
      let (code, numb) = read_numb(code);
      return (code, Term::U60 { numb });
    },
    '~' => {
      let code = tail(code);
      return (code, Term::Var { name: 0xFFFFFFFFFFFFFFFF });
    },
    _ => {
      let (code, name) = read_name(code);
      return (code, Term::Var { name: name % 0x3FFFF });
    }
  }
}

fn read_rule(code: &str) -> (&str, (Term,Term)) {
  let (code, lhs) = read_term(code);
  let (code, ())  = read_char(code, '=');
  let (code, rhs) = read_term(code);
  return (code, (lhs, rhs));
}

fn read_func(code: &str) -> (&str, Func) {
  let (code, rules) = read_until(code, '\0', read_rule);
  if let Some(func) = build_func(rules.as_slice()) {
    return (code, func);
  } else {
    panic!("Couldn't parse function.");
  }
}

// Tests
// -----

pub fn test_0() {
  
  let mut rt = init_runtime();

  rt.define_constructor(name_to_u64("Leaf"), 1);
  rt.define_constructor(name_to_u64("Node"), 2);
  rt.define_function(name_to_u64("Gen"), read_func("
    (@Gen #0) = (#Leaf #1)
    (@Gen x) = & x0 x1 = x; (#Node (@Gen (- x0 #1)) (@Gen (- x1 #1)))
  ").1);
  rt.define_function(name_to_u64("Sum"), read_func("
    (@Sum (#Leaf x))   = x
    (@Sum (#Node a b)) = (+ (@Sum a) (@Sum b))
  ").1);

  // Main term
  let main = rt.create_term(&read_term("(@Sum (@Gen #21))").1);
  println!("term: {:?}", rt.show_term_at(main));

  // Normalizes and benchmarks
  let init = Instant::now();
  rt.normalize(main);
  println!("term: {:?}", rt.show_term_at(main));
  println!("cost: {}", rt.get_cost());
  println!("size: {}", rt.get_size());
  println!("time: {}", init.elapsed().as_millis());

  //println!("{}", show_rt(&rt));
}
