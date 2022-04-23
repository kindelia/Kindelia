#![allow(clippy::identity_op)]
#![allow(dead_code)]
#![allow(non_snake_case)]

use std::collections::{hash_map, HashMap};
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

use rand::prelude::*;
use std::time::Instant;

// Terms
// -----

#[derive(Clone, Debug)]
pub enum Term {
  Var { name: u64 },
  Dup { nam0: u64, nam1: u64, expr: Box<Term>, body: Box<Term> },
  Lam { name: u64, body: Box<Term> },
  App { func: Box<Term>, argm: Box<Term> },
  Ctr { name: u64, args: Vec<Term> },
  Fun { name: u64, args: Vec<Term> },
  U32 { numb: u32 },
  Op2 { oper: u64, val0: Box<Term>, val1: Box<Term> },
}

#[derive(Clone, Copy, Debug)]
pub enum Oper {
  Add,
  Sub,
  Mul,
  Div,
  Mod,
  And,
  Or,
  Xor,
  Shl,
  Shr,
  Lte,
  Ltn,
  Eql,
  Gte,
  Gtn,
  Neq,
}

// Functions
// ---------

#[derive(Clone, Debug)]
pub struct Var {
  pub param: u64,         // in what parameter is this variable located?
  pub field: Option<u64>, // in what field is this variabled located? (if any)
  pub erase: bool,        // should this variable be collected (because it is unused)?
}

#[derive(Clone, Debug)]
pub struct Rule {
  pub cond: Vec<Lnk>,        // left-hand side matching conditions
  pub vars: Vec<Var>,        // left-hand side variable locations
  pub eras: Vec<(u64, u64)>, // must-clear locations (argument number and arity)
  pub body: Term,            // right-hand side body of rule
}

#[derive(Clone, Debug)]
pub struct Func {
  arity: u64,       // number of arguments
  redux: Vec<u64>,  // index of strict arguments
  rules: Vec<Rule>, // vector of rules
}

#[derive(Clone, Debug)]
pub struct File {
  pub funcs: Vec<Option<Func>>,
}

// Constants
// ---------

const U64_PER_KB: u64 = 0x80;
const U64_PER_MB: u64 = 0x20000;
const U64_PER_GB: u64 = 0x8000000;
const FRAME_SIZE: u64 = 1024 * U64_PER_MB;

pub const MAX_ARITY: u64 = 16;
pub const MEM_SPACE: u64 = U64_PER_GB;
pub const MAX_DYNFUNS: u64 = 65536;

pub const SEEN_SIZE: usize = 4194304; // uses 32 MB, covers heaps up to 2 GB
pub const VARS_SIZE: usize = 65536; // maximum variables per rule

pub const VAL: u64 = 1;
pub const EXT: u64 = 0x100000000;
pub const ARI: u64 = 0x100000000000000;
pub const TAG: u64 = 0x1000000000000000;

pub const DP0: u64 = 0x0;
pub const DP1: u64 = 0x1;
pub const VAR: u64 = 0x2;
pub const ARG: u64 = 0x3;
pub const ERA: u64 = 0x4;
pub const LAM: u64 = 0x5;
pub const APP: u64 = 0x6;
pub const PAR: u64 = 0x7;
pub const CTR: u64 = 0x8;
pub const CAL: u64 = 0x9;
pub const OP2: u64 = 0xA;
pub const U32: u64 = 0xB;
pub const F32: u64 = 0xC;
pub const OUT: u64 = 0xE;
pub const NIL: u64 = 0xF;

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

// Types
// -----

pub type Lnk = u64;

// 256 bits + 512 MB
// -----------------
//  64 bits : size
//  64 bits : next
//  64 bits : cost
//  64 bits : ????
//  16 MB   : free[0]
//  16 MB   : free[1]
//  16 MB   : free[2]
//  16 MB   : free[3]
//  16 MB   : free[4]
//  16 MB   : free[5]
//  16 MB   : free[6]
//  16 MB   : free[7]
//  16 MB   : free[8]
//  16 MB   : free[9]
//  16 MB   : free[10]
//  16 MB   : free[11]
//  16 MB   : free[12]
//  16 MB   : free[13]
//  16 MB   : free[14]
//  16 MB   : free[15]
// 256 MB   : node

pub struct Frame {
  pub data: Vec<u64>,
  pub cost: u64,
  pub size: u64,
  pub next: u64,
  //pub node: Vec<Lnk>,
  //pub size: u64,
  //pub free: Vec<Vec<u64>>,
}

pub fn new_worker() -> Frame {
  Frame {
    data: vec![0; FRAME_SIZE as usize],
    cost: 0,
    size: 0,
    next: 0,
    //size: 0,
    //free: vec![vec![]; 16],
  }
}

// Globals
// -------

static mut SEEN_DATA: [u64; SEEN_SIZE] = [0; SEEN_SIZE];
static mut VARS_DATA: [Option<Lnk>; VARS_SIZE] = [None; VARS_SIZE];
static mut CALL_COUNT: &'static mut [u64] = &mut [0; MAX_DYNFUNS as usize];

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
  (U32 * TAG) | val
}

pub fn Nil() -> Lnk {
  NIL * TAG
}

pub fn Ctr(ari: u64, fun: u64, pos: u64) -> Lnk {
  (CTR * TAG) | (ari * ARI) | (fun * EXT) | pos
}

pub fn Fun(ari: u64, fun: u64, pos: u64) -> Lnk {
  (CAL * TAG) | (ari * ARI) | (fun * EXT) | pos
}

pub fn Out(arg: u64, fld: u64) -> Lnk {
  (OUT * TAG) | (arg << 8) | fld
}

// Getters
// -------

pub fn get_tag(lnk: Lnk) -> u64 {
  lnk / TAG
}

pub fn get_ext(lnk: Lnk) -> u64 {
  (lnk / EXT) & 0xFFFFFF
}

pub fn get_val(lnk: Lnk) -> u64 {
  lnk & 0xFFFFFFFF
}

pub fn get_ari(lnk: Lnk) -> u64 {
  (lnk / ARI) & 0xF
}

pub fn get_loc(lnk: Lnk, arg: u64) -> u64 {
  get_val(lnk) + arg
}

// Memory
// ------

pub fn ask_lnk(mem: &Frame, loc: u64) -> Lnk {
  unsafe { *mem.data.get_unchecked(loc as usize) }
}

pub fn ask_arg(mem: &Frame, term: Lnk, arg: u64) -> Lnk {
  ask_lnk(mem, get_loc(term, arg))
}

pub fn link(mem: &mut Frame, loc: u64, lnk: Lnk) -> Lnk {
  unsafe {
    *mem.data.get_unchecked_mut(loc as usize) = lnk;
    if get_tag(lnk) <= VAR {
      let pos = get_loc(lnk, get_tag(lnk) & 0x01);
      *mem.data.get_unchecked_mut(pos as usize) = Arg(loc);
    }
  }
  lnk
}

pub fn alloc(mem: &mut Frame, size: u64) -> u64 {
  if size == 0 {
    return 0;
  } else {
    loop {
      let index = mem.next;
      if index <= FRAME_SIZE - size {
        let mut empty = true;
        for i in 0 .. size {
          if mem.data[(index + i) as usize] != 0 {
            empty = false;
            break;
          }
        }
        if empty {
          mem.next += size;
          mem.size += size;
          return index;
        }
      }
      mem.next = fastrand::u64(..) % FRAME_SIZE;
    }
  }
}

pub fn clear(mem: &mut Frame, loc: u64, size: u64) {
  for i in 0 .. size {
    mem.data[(loc + i) as usize] = 0;
  }
  mem.size -= size;
  //mem.free[size as usize].push(loc);
}

pub fn collect(mem: &mut Frame, term: Lnk) {
  let mut stack : Vec<Lnk> = Vec::new();
  let mut next = term;
  loop {
    let term = next;
    match get_tag(term) {
      DP0 => {
        link(mem, get_loc(term, 0), Era());
        //r_educe(mem, get_loc(ask_arg(mem,term,1),0));
      }
      DP1 => {
        link(mem, get_loc(term, 1), Era());
        //r_educe(mem, get_loc(ask_arg(mem,term,0),0));
      }
      VAR => {
        link(mem, get_loc(term, 0), Era());
      }
      LAM => {
        if get_tag(ask_arg(mem, term, 0)) != ERA {
          link(mem, get_loc(ask_arg(mem, term, 0), 0), Era());
        }
        next = ask_arg(mem, term, 1);
        clear(mem, get_loc(term, 0), 2);
        continue;
      }
      APP => {
        stack.push(ask_arg(mem, term, 0));
        next = ask_arg(mem, term, 1);
        clear(mem, get_loc(term, 0), 2);
        continue;
      }
      PAR => {
        stack.push(ask_arg(mem, term, 0));
        next = ask_arg(mem, term, 1);
        clear(mem, get_loc(term, 0), 2);
        continue;
      }
      OP2 => {
        stack.push(ask_arg(mem, term, 0));
        next = ask_arg(mem, term, 1);
        clear(mem, get_loc(term, 0), 2);
        continue;
      }
      U32 => {}
      CTR | CAL => {
        let arity = get_ari(term);
        for i in 0..arity {
          if i < arity - 1 {
            stack.push(ask_arg(mem, term, i));
          } else {
            next = ask_arg(mem, term, i);
          }
        }
        clear(mem, get_loc(term, 0), arity);
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

pub fn inc_cost(mem: &mut Frame) {
  mem.cost += 1;
}

// Term
// ----

// Writes a Term represented as a Rust enum on the Runtime's memory.
pub fn alloc_term(mem: &mut Frame, term: &Term, loc: u64, dups: &mut u64) -> Lnk {
  fn bind(mem: &mut Frame, loc: u64, name: u64, lnk: Lnk) {
    unsafe {
      match VARS_DATA[name as usize] {
        Some(got) => {
          VARS_DATA[name as usize] = None;
          link(mem, got, lnk);
        }
        None => {
          VARS_DATA[name as usize] = Some(lnk);
          link(mem, loc, Era());
        }
      }
    }
  }
  match term {
    Term::Var { name } => {
      unsafe {
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
      let node = alloc(mem, 3);
      let dupk = *dups;
      *dups += 1;
      bind(mem, node + 0, *nam0, Dp0(dupk, node));
      bind(mem, node + 1, *nam1, Dp1(dupk, node));
      let expr = alloc_term(mem, expr, node + 2, dups);
      link(mem, node + 2, expr);
      let body = alloc_term(mem, body, loc, dups);
      body
    }
    Term::Lam { name, body } => {
      let node = alloc(mem, 2);
      bind(mem, node + 0, *name, Var(node));
      let body = alloc_term(mem, body, node + 1, dups);
      link(mem, node + 1, body);
      Lam(node)
    }
    Term::App { func, argm } => {
      let node = alloc(mem, 2);
      let func = alloc_term(mem, func, node + 0, dups);
      link(mem, node + 0, func);
      let argm = alloc_term(mem, argm, node + 1, dups);
      link(mem, node + 1, argm);
      App(node)
    }
    Term::Fun { name, args } => {
      let size = args.len() as u64;
      let node = alloc(mem, size);
      for (i, arg) in args.iter().enumerate() {
        let arg_lnk = alloc_term(mem, arg, node + i as u64, dups);
        link(mem, node + i as u64, arg_lnk);
      }
      Fun(size, *name, node)
    }
    Term::Ctr { name, args } => {
      let size = args.len() as u64;
      let node = alloc(mem, size);
      for (i, arg) in args.iter().enumerate() {
        let arg_lnk = alloc_term(mem, arg, node + i as u64, dups);
        link(mem, node + i as u64, arg_lnk);
      }
      Ctr(size, *name, node)
    }
    Term::U32 { numb } => {
      U_32(*numb as u64)
    }
    Term::Op2 { oper, val0, val1 } => {
      let node = alloc(mem, 2);
      let val0 = alloc_term(mem, val0, node + 0, dups);
      link(mem, node + 0, val0);
      let val1 = alloc_term(mem, val1, node + 1, dups);
      link(mem, node + 1, val1);
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
            cond.push(Ctr(args.len() as u64, *arg_name, 0)); // adds its matching condition
            eras.push((i, args.len() as u64)); // marks its index and arity for freeing
            // For each of its fields...
            for j in 0 .. arg_args.len() as u64 {
              // If it is a variable...
              if let Term::Var { name } = arg_args[j as usize] {
                vars.push(Var { param: i, field: Some(j), erase: name == 0xFFFFFFFFFFFFFFFF }); // add its location
              // Otherwise..
              } else {
                return None; // return none, because we don't allow nested matches
              }
            }
          }
          // If it is a number...
          Term::U32 { numb: arg_numb } => {
            strict[i as usize] = true;
            cond.push(U_32(*arg_numb as u64)); // adds its matching condition
          }
          // If it is a variable...
          Term::Var { name: arg_name } => {
            vars.push(Var { param: i, field: None, erase: *arg_name == 0xFFFFFFFFFFFFFFFF }); // add its location
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

pub fn subst(mem: &mut Frame, lnk: Lnk, val: Lnk) {
  if get_tag(lnk) != ERA {
    link(mem, get_loc(lnk, 0), val);
  } else {
    collect(mem, val);
  }
}

pub fn reduce(
  mem: &mut Frame,
  file: &File,
  dups: &mut u64,
  root: u64,
  _i2n: Option<&HashMap<u64, String>>,
  debug: bool,
) -> Lnk {

  let mut stack: Vec<u64> = Vec::new();

  let mut init = 1;
  let mut host = root;

  loop {
    let term = ask_lnk(mem, host);

    //if debug || true {
      //println!("------------------------");
      //println!("{}", show_term(mem, ask_lnk(mem, 0), _i2n, term));
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
        CAL => {
          let fun = get_ext(term);
          let ari = get_ari(term);
          if let Some(func) = &file.funcs[fun as usize] {
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
          let arg0 = ask_arg(mem, term, 0);
          if get_tag(arg0) == LAM {
            //println!("app-lam");
            inc_cost(mem);
            subst(mem, ask_arg(mem, arg0, 0), ask_arg(mem, term, 1));
            let _done = link(mem, host, ask_arg(mem, arg0, 1));
            clear(mem, get_loc(term, 0), 2);
            clear(mem, get_loc(arg0, 0), 2);
            init = 1;
            continue;
          }
          if get_tag(arg0) == PAR {
            //println!("app-sup");
            inc_cost(mem);
            let app0 = get_loc(term, 0);
            let app1 = get_loc(arg0, 0);
            let let0 = alloc(mem, 3);
            let par0 = alloc(mem, 2);
            link(mem, let0 + 2, ask_arg(mem, term, 1));
            link(mem, app0 + 1, Dp0(get_ext(arg0), let0));
            link(mem, app0 + 0, ask_arg(mem, arg0, 0));
            link(mem, app1 + 0, ask_arg(mem, arg0, 1));
            link(mem, app1 + 1, Dp1(get_ext(arg0), let0));
            link(mem, par0 + 0, App(app0));
            link(mem, par0 + 1, App(app1));
            let done = Par(get_ext(arg0), par0);
            link(mem, host, done);
          }
        }
        DP0 | DP1 => {
          let arg0 = ask_arg(mem, term, 2);
          // let argK = ask_arg(mem, term, if get_tag(term) == DP0 { 1 } else { 0 });
          // if get_tag(argK) == ERA {
          //   let done = arg0;
          //   link(mem, host, done);
          //   init = 1;
          //   continue;
          // }
          if get_tag(arg0) == LAM {
            //println!("dup-lam");
            inc_cost(mem);
            let let0 = get_loc(term, 0);
            let par0 = get_loc(arg0, 0);
            let lam0 = alloc(mem, 2);
            let lam1 = alloc(mem, 2);
            link(mem, let0 + 2, ask_arg(mem, arg0, 1));
            link(mem, par0 + 1, Var(lam1));
            let arg0_arg_0 = ask_arg(mem, arg0, 0);
            link(mem, par0 + 0, Var(lam0));
            subst(mem, arg0_arg_0, Par(get_ext(term), par0));
            let term_arg_0 = ask_arg(mem, term, 0);
            link(mem, lam0 + 1, Dp0(get_ext(term), let0));
            subst(mem, term_arg_0, Lam(lam0));
            let term_arg_1 = ask_arg(mem, term, 1);
            link(mem, lam1 + 1, Dp1(get_ext(term), let0));
            subst(mem, term_arg_1, Lam(lam1));
            let done = Lam(if get_tag(term) == DP0 { lam0 } else { lam1 });
            link(mem, host, done);
            init = 1;
            continue;
          } else if get_tag(arg0) == PAR {
            //println!("dup-sup");
            if get_ext(term) == get_ext(arg0) {
              inc_cost(mem);
              subst(mem, ask_arg(mem, term, 0), ask_arg(mem, arg0, 0));
              subst(mem, ask_arg(mem, term, 1), ask_arg(mem, arg0, 1));
              let _done = link(mem, host, ask_arg(mem, arg0, if get_tag(term) == DP0 { 0 } else { 1 }));
              clear(mem, get_loc(term, 0), 3);
              clear(mem, get_loc(arg0, 0), 2);
              init = 1;
              continue;
            } else {
              inc_cost(mem);
              let par0 = alloc(mem, 2);
              let let0 = get_loc(term, 0);
              let par1 = get_loc(arg0, 0);
              let let1 = alloc(mem, 3);
              link(mem, let0 + 2, ask_arg(mem, arg0, 0));
              link(mem, let1 + 2, ask_arg(mem, arg0, 1));
              let term_arg_0 = ask_arg(mem, term, 0);
              let term_arg_1 = ask_arg(mem, term, 1);
              link(mem, par1 + 0, Dp1(get_ext(term), let0));
              link(mem, par1 + 1, Dp1(get_ext(term), let1));
              link(mem, par0 + 0, Dp0(get_ext(term), let0));
              link(mem, par0 + 1, Dp0(get_ext(term), let1));
              subst(mem, term_arg_0, Par(get_ext(arg0), par0));
              subst(mem, term_arg_1, Par(get_ext(arg0), par1));
              let done = Par(get_ext(arg0), if get_tag(term) == DP0 { par0 } else { par1 });
              link(mem, host, done);
            }
          } else if get_tag(arg0) == U32 {
            //println!("dup-u32");
            inc_cost(mem);
            subst(mem, ask_arg(mem, term, 0), arg0);
            subst(mem, ask_arg(mem, term, 1), arg0);
            let _done = arg0;
            link(mem, host, arg0);
          } else if get_tag(arg0) == CTR {
            //println!("dup-ctr");
            inc_cost(mem);
            let func = get_ext(arg0);
            let arit = get_ari(arg0);
            if arit == 0 {
              subst(mem, ask_arg(mem, term, 0), Ctr(0, func, 0));
              subst(mem, ask_arg(mem, term, 1), Ctr(0, func, 0));
              clear(mem, get_loc(term, 0), 3);
              let _done = link(mem, host, Ctr(0, func, 0));
            } else {
              let ctr0 = get_loc(arg0, 0);
              let ctr1 = alloc(mem, arit);
              for i in 0..arit - 1 {
                let leti = alloc(mem, 3);
                link(mem, leti + 2, ask_arg(mem, arg0, i));
                link(mem, ctr0 + i, Dp0(get_ext(term), leti));
                link(mem, ctr1 + i, Dp1(get_ext(term), leti));
              }
              let leti = get_loc(term, 0);
              link(mem, leti + 2, ask_arg(mem, arg0, arit - 1));
              let term_arg_0 = ask_arg(mem, term, 0);
              link(mem, ctr0 + arit - 1, Dp0(get_ext(term), leti));
              subst(mem, term_arg_0, Ctr(arit, func, ctr0));
              let term_arg_1 = ask_arg(mem, term, 1);
              link(mem, ctr1 + arit - 1, Dp1(get_ext(term), leti));
              subst(mem, term_arg_1, Ctr(arit, func, ctr1));
              let done = Ctr(arit, func, if get_tag(term) == DP0 { ctr0 } else { ctr1 });
              link(mem, host, done);
            }
          } else if get_tag(arg0) == ERA {
            inc_cost(mem);
            subst(mem, ask_arg(mem, term, 0), Era());
            subst(mem, ask_arg(mem, term, 1), Era());
            link(mem, host, Era());
            clear(mem, get_loc(term, 0), 3);
            init = 1;
            continue;
          }
        }
        OP2 => {
          let arg0 = ask_arg(mem, term, 0);
          let arg1 = ask_arg(mem, term, 1);
          if get_tag(arg0) == U32 && get_tag(arg1) == U32 {
            //println!("op2-u32");
            inc_cost(mem);
            let a = get_val(arg0);
            let b = get_val(arg1);
            let c = match get_ext(term) {
              ADD => (a + b)  & 0xFFFFFFFF,
              SUB => (a - b)  & 0xFFFFFFFF,
              MUL => (a * b)  & 0xFFFFFFFF,
              DIV => (a / b)  & 0xFFFFFFFF,
              MOD => (a % b)  & 0xFFFFFFFF,
              AND => (a & b)  & 0xFFFFFFFF,
              OR  => (a | b)  & 0xFFFFFFFF,
              XOR => (a ^ b)  & 0xFFFFFFFF,
              SHL => (a << b) & 0xFFFFFFFF,
              SHR => (a >> b) & 0xFFFFFFFF,
              LTN => u64::from(a < b),
              LTE => u64::from(a <= b),
              EQL => u64::from(a == b),
              GTE => u64::from(a >= b),
              GTN => u64::from(a > b),
              NEQ => u64::from(a != b),
              _ => 0,
            };
            let done = U_32(c);
            clear(mem, get_loc(term, 0), 2);
            link(mem, host, done);
          } else if get_tag(arg0) == PAR {
            //println!("op2-sup-0");
            inc_cost(mem);
            let op20 = get_loc(term, 0);
            let op21 = get_loc(arg0, 0);
            let let0 = alloc(mem, 3);
            let par0 = alloc(mem, 2);
            link(mem, let0 + 2, arg1);
            link(mem, op20 + 1, Dp0(get_ext(arg0), let0));
            link(mem, op20 + 0, ask_arg(mem, arg0, 0));
            link(mem, op21 + 0, ask_arg(mem, arg0, 1));
            link(mem, op21 + 1, Dp1(get_ext(arg0), let0));
            link(mem, par0 + 0, Op2(get_ext(term), op20));
            link(mem, par0 + 1, Op2(get_ext(term), op21));
            let done = Par(get_ext(arg0), par0);
            link(mem, host, done);
          } else if get_tag(arg1) == PAR {
            //println!("op2-sup-1");
            inc_cost(mem);
            let op20 = get_loc(term, 0);
            let op21 = get_loc(arg1, 0);
            let let0 = alloc(mem, 3);
            let par0 = alloc(mem, 2);
            link(mem, let0 + 2, arg0);
            link(mem, op20 + 0, Dp0(get_ext(arg1), let0));
            link(mem, op20 + 1, ask_arg(mem, arg1, 0));
            link(mem, op21 + 1, ask_arg(mem, arg1, 1));
            link(mem, op21 + 0, Dp1(get_ext(arg1), let0));
            link(mem, par0 + 0, Op2(get_ext(term), op20));
            link(mem, par0 + 1, Op2(get_ext(term), op21));
            let done = Par(get_ext(arg1), par0);
            link(mem, host, done);
          }
        }
        CAL => {
          let fun = get_ext(term);
          let ari = get_ari(term);

          //println!("- on call {} | {}", get_loc(term, 0), show_lnk(term));

          if let Some(func) = &file.funcs[fun as usize] {

          //println!("- calling");

            let mut cont = false;

            // For each argument, if it is a redex and a PAR, apply the cal_par rule
            for idx in &func.redux {
              if get_tag(ask_arg(mem, term, *idx)) == PAR {
                inc_cost(mem);
                let argn = ask_arg(mem, term, *idx);
                let arit = get_ari(term);
                let func = get_ext(term);
                let fun0 = get_loc(term, 0);
                let fun1 = alloc(mem, arit);
                let par0 = get_loc(argn, 0);
                for i in 0..arit {
                  if i != *idx {
                    let leti = alloc(mem, 3);
                    let argi = ask_arg(mem, term, i);
                    link(mem, fun0 + i, Dp0(get_ext(argn), leti));
                    link(mem, fun1 + i, Dp1(get_ext(argn), leti));
                    link(mem, leti + 2, argi);
                  } else {
                    link(mem, fun0 + i, ask_arg(mem, argn, 0));
                    link(mem, fun1 + i, ask_arg(mem, argn, 1));
                  }
                }
                link(mem, par0 + 0, Fun(arit, func, fun0));
                link(mem, par0 + 1, Fun(arit, func, fun1));
                let done = Par(get_ext(argn), par0);
                link(mem, host, done);
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
                  U32 => {
                    //println!("Didn't match because of U32. i={} {} {}", i, get_val(ask_arg(mem, term, i)), get_val(cond));
                    let same_tag = get_tag(ask_arg(mem, term, i)) == U32;
                    let same_val = get_val(ask_arg(mem, term, i)) == get_val(cond);
                    matched = matched && same_tag && same_val;
                  }
                  CTR => {
                    //println!("Didn't match because of CTR. i={} {} {}", i, get_tag(ask_arg(mem, term, i)), get_val(cond));
                    let same_tag = get_tag(ask_arg(mem, term, i)) == CTR;
                    let same_ext = get_ext(ask_arg(mem, term, i)) == get_ext(cond);
                    matched = matched && same_tag && same_ext;
                  }
                  _ => {}
                }
              }

              // If all conditions are satisfied, the rule matched, so we must apply it
              if matched {
                
                //println!("- matched");

                // Increments the gas count
                inc_cost(mem);

                // Gathers matched variables
                //let mut vars = vec![None; 16]; // FIXME: pre-alloc statically
                for i in 0 .. rule.vars.len() {
                  let mut var = term;
                  var = ask_arg(mem, var, rule.vars[i].param);
                  if let Some(field) = rule.vars[i].field {
                    var = ask_arg(mem, var, field);
                  }
                  unsafe {
                    VARS_DATA[i] = Some(var);
                  }
                }

                // Builds the right-hand side term (ex: `(Succ (Add a b))`)
                //println!("-- alloc {:?}", rule.body);
                //println!("-- vars: {:?}", vars);
                let done = alloc_term(mem, &rule.body, host, dups);

                // Links the host location to it
                link(mem, host, done);

                // Clears the matched ctrs (the `(Succ ...)` and the `(Add ...)` ctrs)
                for (eras_index, eras_arity) in &rule.eras {
                  clear(mem, get_loc(ask_arg(mem, term, *eras_index), 0), *eras_arity);
                }
                clear(mem, get_loc(term, 0), func.arity);

                // Collects unused variables (none in this example)
                for i in 0 .. rule.vars.len() {
                  if rule.vars[i].erase {
                    unsafe {
                      if let Some(var) = VARS_DATA[i] {
                        collect(mem, var);
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

  ask_lnk(mem, root)
}

pub fn set_bit(bits: &mut [u64], bit: u64) {
  bits[bit as usize >> 6] |= 1 << (bit & 0x3f);
}

pub fn get_bit(bits: &[u64], bit: u64) -> bool {
  (((bits[bit as usize >> 6] >> (bit & 0x3f)) as u8) & 1) == 1
}

pub fn normal_go(
  mem: &mut Frame,
  file: &File,
  dups: &mut u64,
  host: u64,
  seen: &mut [u64],
  i2n: Option<&HashMap<u64, String>>,
  debug: bool,
) -> Lnk {
  let term = ask_lnk(mem, host);
  if get_bit(seen, host) {
    term
  } else {
    let term = reduce(mem, file, dups, host, i2n, debug);
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
      CTR | CAL => {
        let arity = get_ari(term);
        for i in 0..arity {
          rec_locs.push(get_loc(term, i));
        }
      }
      _ => {}
    }
    for loc in rec_locs {
      let lnk: Lnk = normal_go(mem, file, dups, loc, seen, i2n, debug);
      link(mem, loc, lnk);
    }
    term
  }
}

pub fn normal(
  mem: &mut Frame,
  file: &File,
  host: u64,
  i2n: Option<&HashMap<u64, String>>,
  debug: bool,
) -> Lnk {
  let mut done;
  let mut dups = 0;
  let mut cost = mem.cost;
  loop {
    let mut seen = vec![0; 4194304];
    done = normal_go(mem, file, &mut dups, host, &mut seen, i2n, debug);
    if mem.cost != cost {
      cost = mem.cost;
    } else {
      break;
    }
  }
  //print_call_counts(i2n); // TODO: uncomment
  done
}

// Debug
// -----

// Debug: prints call counts
fn print_call_counts(i2n: Option<&HashMap<u64, String>>) {
  unsafe {
    let mut counts : Vec<(String,u64)> = Vec::new();
    for fun in 0..MAX_DYNFUNS {
      if let Some(id_to_name) = i2n {
        match id_to_name.get(&fun) {
          None => {
            break;
          }
          Some(fun_name) => {
            counts.push((fun_name.clone(), CALL_COUNT[fun as usize]));
          }
        }
      }
    }
    counts.sort_by(|a, b| a.1.partial_cmp(&b.1).unwrap());
    for (name, count) in counts {
      println!("{} - {}", name, count);
    }
    println!("");
  }
}

pub fn show_lnk(x: Lnk) -> String {
  if x == 0 {
    String::from("~")
  } else {
    let tag = get_tag(x);
    let ext = get_ext(x);
    let val = get_val(x);
    let ari = match tag {
      CTR => format!("{}", get_ari(x)),
      CAL => format!("{}", get_ari(x)),
      _ => String::new(),
    };
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
      CAL => "CAL",
      OP2 => "OP2",
      U32 => "U32",
      F32 => "F32",
      OUT => "OUT",
      NIL => "NIL",
      _   => "?",
    };
    format!("{}{}:{:x}:{:x}", tgs, ari, ext, val)
  }
}

pub fn show_mem(worker: &Frame) -> String {
  let mut s: String = String::new();
  for i in 0..48 {
    // pushes to the string
    s.push_str(&format!("{:x} | ", i));
    s.push_str(&show_lnk(worker.data[i]));
    s.push('\n');
  }
  s
}

pub fn show_term(
  mem: &Frame,
  term: Lnk,
  i2n: Option<&HashMap<u64, String>>,
  focus: u64,
) -> String {
  let mut lets: HashMap<u64, u64> = HashMap::new();
  let mut kinds: HashMap<u64, u64> = HashMap::new();
  let mut names: HashMap<u64, String> = HashMap::new();
  let mut count: u64 = 0;
  fn find_lets(
    mem: &Frame,
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
        find_lets(mem, ask_arg(mem, term, 1), lets, kinds, names, count);
      }
      APP => {
        find_lets(mem, ask_arg(mem, term, 0), lets, kinds, names, count);
        find_lets(mem, ask_arg(mem, term, 1), lets, kinds, names, count);
      }
      PAR => {
        find_lets(mem, ask_arg(mem, term, 0), lets, kinds, names, count);
        find_lets(mem, ask_arg(mem, term, 1), lets, kinds, names, count);
      }
      DP0 => {
        if let hash_map::Entry::Vacant(e) = lets.entry(get_loc(term, 0)) {
          names.insert(get_loc(term, 0), format!("{}", count));
          *count += 1;
          kinds.insert(get_loc(term, 0), get_ext(term));
          e.insert(get_loc(term, 0));
          find_lets(mem, ask_arg(mem, term, 2), lets, kinds, names, count);
        }
      }
      DP1 => {
        if let hash_map::Entry::Vacant(e) = lets.entry(get_loc(term, 0)) {
          names.insert(get_loc(term, 0), format!("{}", count));
          *count += 1;
          kinds.insert(get_loc(term, 0), get_ext(term));
          e.insert(get_loc(term, 0));
          find_lets(mem, ask_arg(mem, term, 2), lets, kinds, names, count);
        }
      }
      OP2 => {
        find_lets(mem, ask_arg(mem, term, 0), lets, kinds, names, count);
        find_lets(mem, ask_arg(mem, term, 1), lets, kinds, names, count);
      }
      CTR | CAL => {
        let arity = get_ari(term);
        for i in 0..arity {
          find_lets(mem, ask_arg(mem, term, i), lets, kinds, names, count);
        }
      }
      _ => {}
    }
  }
  fn go(
    mem: &Frame,
    term: Lnk,
    names: &HashMap<u64, String>,
    i2n: Option<&HashMap<u64, String>>,
    focus: u64,
  ) -> String {
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
        format!("λ{} {}", name, go(mem, ask_arg(mem, term, 1), names, i2n, focus))
      }
      APP => {
        let func = go(mem, ask_arg(mem, term, 0), names, i2n, focus);
        let argm = go(mem, ask_arg(mem, term, 1), names, i2n, focus);
        format!("({} {})", func, argm)
      }
      PAR => {
        //let kind = get_ext(term);
        let func = go(mem, ask_arg(mem, term, 0), names, i2n, focus);
        let argm = go(mem, ask_arg(mem, term, 1), names, i2n, focus);
        format!("{{{} {}}}", func, argm)
      }
      OP2 => {
        let oper = get_ext(term);
        let val0 = go(mem, ask_arg(mem, term, 0), names, i2n, focus);
        let val1 = go(mem, ask_arg(mem, term, 1), names, i2n, focus);
        let symb = match oper {
          0x0 => "+",
          0x1 => "-",
          0x2 => "*",
          0x3 => "/",
          0x4 => "%",
          0x5 => "&",
          0x6 => "|",
          0x7 => "^",
          0x8 => "<<",
          0x9 => ">>",
          0xA => "<",
          0xB => "<=",
          0xC => "=",
          0xD => ">=",
          0xE => ">",
          0xF => "!=",
          _   => "?e",
        };
        format!("({} {} {})", symb, val0, val1)
      }
      U32 => {
        format!("#{}", get_val(term))
      }
      CTR => {
        let func = get_ext(term);
        let arit = get_ari(term);
        let args: Vec<String> = (0..arit).map(|i| go(mem, ask_arg(mem, term, i), names, i2n, focus)).collect();
        format!("(C{}{})", func, args.iter().map(|x| format!(" {}", x)).collect::<String>())
      }
      CAL => {
        let func = get_ext(term);
        let arit = get_ari(term);
        let args: Vec<String> = (0..arit).map(|i| go(mem, ask_arg(mem, term, i), names, i2n, focus)).collect();
        format!("(F{}{})", func, args.iter().map(|x| format!(" {}", x)).collect::<String>())
      }
      ERA => {
        format!("*")
      }
      _ => format!("?g({})", get_tag(term)),
    };
    if term == focus {
      format!("${}", done)
    } else {
      done
    }
  }
  find_lets(mem, term, &mut lets, &mut kinds, &mut names, &mut count);
  let mut text = go(mem, term, &names, i2n, focus);
  for (_key, pos) in lets {
    // todo: reverse
    let what = String::from("?h");
    //let kind = kinds.get(&key).unwrap_or(&0);
    let name = names.get(&pos).unwrap_or(&what);
    let nam0 = if ask_lnk(mem, pos + 0) == Era() { String::from("*") } else { format!("a{}", name) };
    let nam1 = if ask_lnk(mem, pos + 1) == Era() { String::from("*") } else { format!("b{}", name) };
    text.push_str(&format!(
      "\ndup {} {} = {};",
      //kind,
      nam0,
      nam1,
      go(mem, ask_lnk(mem, pos + 2), &names, i2n, focus)
    ));
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
  return chr >= 'a' && chr <= 'z'
      || chr >= 'A' && chr <= 'Z'
      || chr >= '0' && chr <= '9'
      || chr == '_' || chr == '.';
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

fn read_name(code: &str) -> (&str, String) {
  let code = skip(code);
  let mut text = String::new();
  let mut code = code;
  while is_name_char(head(code)) {
    text.push(head(code));
    code = tail(code);
  }
  return (code, text);
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
      let (code, name) = read_numb(code);
      let (code, body) = read_term(code);
      return (code, Term::Lam { name, body: Box::new(body) });
    },
    '!' => {
      let code         = tail(code);
      let (code, nam0) = read_numb(code);
      let (code, nam1) = read_numb(code);
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
      } else if head(code) == 'C' {
        let code = tail(code);
        let (code, name) = read_numb(code);
        let (code, args) = read_until(code, ')', read_term);
        return (code, Term::Ctr { name, args });
      } else if head(code) == 'F' {
        let code = tail(code);
        let (code, name) = read_numb(code);
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
      return (code, Term::U32 { numb: numb as u32 });
    },
    '~' => {
      let code = tail(code);
      return (code, Term::Var { name: 0xFFFFFFFFFFFFFFFF });
    },
    _ => {
      let (code, name) = read_numb(code);
      return (code, Term::Var { name });
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
  let mut mem = new_worker();

  //let func = read_func("
    //(F0 (C0))   = (C0)
    //(F0 (C1 0)) = (C1 (C1 (F0 0)))
  //").1;

  let gen = read_func("
    (F0 #0) = (C0 #1)
    (F1 0) = ! 1 2 = 0; (C1 (F0 (- 1 #1)) (F0 (- 2 #1)))
  ").1;

  let sum = read_func("
    (F1 (C0 0))   = 0
    (F1 (C1 0 1)) = (+ (F1 0) (F1 1))
  ").1;

  let file = File {
    funcs: vec![
      Some(gen.clone()),
      Some(sum.clone()),
    ]
  };

  let val = read_term("(F1 (F0 #23))").1;

  mem.next    = 1;
  mem.data[0] = alloc_term(&mut mem, &val, 1, &mut 0);

  println!("term: {:?}", show_term(&mem, mem.data[0], None, 0));

  let init = Instant::now();

  let term = normal(&mut mem, &file, 0, None, false);
  println!("term: {:?}", show_term(&mem, term, None, 0));

  println!("cost: {}", mem.cost);
  println!("time: {}", init.elapsed().as_millis());
}

