use kindelia_common::Name;
use kindelia_lang::ast::{Term, Func};

use super::RuntimeError;

/// Maximum depth of a LHS or RHS term
pub const MAX_TERM_DEPTH: u128 = 256; 

pub fn check_func(func: &Func) -> Result<(), RuntimeError> {
  for rule in &func.rules {
    check_term(&rule.lhs)?;
    check_term(&rule.rhs)?;
  }
  Ok(())
}

pub fn check_term(term: &Term) -> Result<(), RuntimeError> {
  check_linear(term)?;
  check_term_depth(term, 0)?;
  Ok(())
}

/// Checks if:
/// - Every non-erased variable is used exactly once
/// - Every erased variable is never used
pub fn check_linear(term: &Term) -> Result<(), RuntimeError> {
  // println!("{}", term);
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

  // println!("{}: {}", term, res);
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
      }
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

/// Counts how many times the free variable 'name' appears inside Term
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
