// ## Mana Table
//
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

use kindelia_lang::ast::Term;

pub fn app_lam_mana() -> u64 {
  return 2;
}

pub fn app_sup_mana() -> u64 {
  return 4;
}

pub fn op2_num_mana() -> u64 {
  return 2;
}

pub fn op2_sup_mana() -> u64 {
  return 4;
}

pub fn fun_ctr_mana(body: &Term) -> u64 {
  return 2 + count_allocs(body);
}

pub fn fun_sup_mana(arity: u64) -> u64 {
  return 2 + arity;
}

pub fn dup_lam_mana() -> u64 {
  return 4;
}

pub fn dup_num_mana() -> u64 {
  return 2;
}

pub fn dup_ctr_mana(arity: u64) -> u64 {
  return 2 + arity;
}

pub fn dup_dup_mana() -> u64 {
  return 4;
}

pub fn dup_sup_mana() -> u64 {
  return 2;
}

pub fn dup_era_mana() -> u64 {
  return 2;
}

pub fn count_allocs(body: &Term) -> u64 {
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
