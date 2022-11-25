// Functions
// =========

// The types below are used by the runtime to evaluate rewrite rules. They store
// the same data as the types on `ast`, except in a semi-compiled, digested
// form, allowing faster computation.

use std::collections::HashSet;

use kindelia_common::Name;
use kindelia_lang::ast::{Func, Var, Term};

use crate::runtime::{count_uses, Loc};

use super::{RawCell, RuntimeError, DefinitionError};

/// Compiled information about a rewrite rule.
#[derive(Clone, Debug, PartialEq)]
pub struct CompRule {
  /// Left-hand side matching conditions
  pub cond: Vec<RawCell>,
  /// Left-hand side variable locations
  pub vars: Vec<Var>,
  // Must-clear locations (argument number and arity)
  pub eras: Vec<(u64, u64)>, 
  // Right-hand side body of rule
  pub body: Term,
}

/// Compiled information about a function.
#[derive(Clone, Debug, PartialEq, Default)]
pub struct CompFunc {
  // The original function
  pub func: Func,
  // Number of arguments
  pub arity: u64,
  // Index of strict arguments
  pub redux: Vec<u64>,
  // List of rules
  pub rules: Vec<CompRule>, 
}

/// Given a Func (a vector of rules, lhs/rhs pairs), builds the CompFunc object.
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
  for rule_index in 0..rules.len() {
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
            cond.push(RawCell::ctr(*arg_name, Loc::ZERO)); // adds its matching condition
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
            cond.push(RawCell::num(**arg_numb)); // adds its matching condition
          }
          // If it is a variable...
          Term::Var { name: arg_name } => {
            check_var(*arg_name, &rule.rhs, &mut seen, rule_index)?;
            vars.push(Var { name: *arg_name, param: i, field: None, erase: *arg_name == Name::NONE }); // add its location
            cond.push(RawCell::var(Loc::ZERO)); // it has no matching condition
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
  for i in 0..strict.len() {
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
