use std::fmt;

use serde::{Deserialize, Serialize};

pub use kindelia_common::crypto::Signature;
pub use kindelia_common::{Name, U120};

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
  Op2 { oper: Oper, val0: Box<Term>, val1: Box<Term> }, // FIXME: refactor `oper` u128 to enum
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

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
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
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Var {
  pub name: Name,         // this variable's name
  pub param: u64,         // in what parameter is this variable located?
  pub field: Option<u64>, // in what field is this variable located? (if any)
  pub erase: bool,        // should this variable be collected (because it is unused)?
}

/// A global statement that alters the state of the blockchain
// TODO: It would probably good to have a separate Signature object as part of the AST,
// to decouple the AST from the crypto parts.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Statement {
  Fun { name: Name, args: Vec<Name>, func: Func, init: Option<Term>, sign: Option<Signature> },
  Ctr { name: Name, args: Vec<Name>, sign: Option<Signature> },
  Run { expr: Term, sign: Option<Signature> },
  Reg { name: Name, ownr: U120, sign: Option<Signature> },
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

impl fmt::Display for Term {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    fn view_term(term: &Term) -> String {
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
                output.push(name.to_string());
              }
              Term::Dup { nam0, nam1, expr, body } => {
                output.push("dup ".to_string());
                output.push(nam0.to_string());
                output.push(" ".to_string());
                output.push(nam1.to_string());
                output.push(" = ".to_string());
                stack.push(StackItem::Term(body));
                stack.push(StackItem::Str("; ".to_string()));
                stack.push(StackItem::Term(expr));
              }
              Term::Lam { name, body } => {
                output.push(format!("@{} ", name));
                stack.push(StackItem::Term(body));
              }
              Term::App { func, argm } => {
                output.push("(!".to_string());
                stack.push(StackItem::Str(")".to_string()));
                stack.push(StackItem::Term(argm));
                stack.push(StackItem::Str(" ".to_string()));
                stack.push(StackItem::Term(func));
              }
              Term::Ctr { name, args } => {
                let name = name.to_string();
                // Pretty print names
                if name == "Name" && args.len() == 1 {
                  if let Term::Num { numb } = args[0] {
                    let name: Name = numb.into();
                    output.push(format!("{{Name '{}'}}", name));
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
                let name = name.to_string();
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
                  //return numb.to_string();
                //} else {
                  output.push(format!("#{}", numb));
                //}
              }
              Term::Op2 { oper, val0, val1 } => {
                output.push("(".to_string());
                output.push(oper.to_string());
                output.push(" ".to_string());
                stack.push(StackItem::Str(")".to_string()));
                stack.push(StackItem::Term(val1));
                stack.push(StackItem::Str(" ".to_string()));
                stack.push(StackItem::Term(val0));
              }
            }
          }
        }
      }
      output.join("")
    }

    f.write_str(&view_term(self))
  }
}

impl Drop for Term {
  fn drop(&mut self) {
    /// Verify if `term` has recursive childs (any of its
    /// `Term`'s properties is not a var or a num)
    fn term_is_recursive(term: &Term) -> bool {
      fn term_is_num_or_var(term: &Term) -> bool {
        matches!(term, Term::Num { .. } | Term::Var { .. })
      }
      match term {
        Term::Var { .. } => false,
        Term::Dup { expr, body, .. } => !(term_is_num_or_var(expr) && term_is_num_or_var(body)),
        Term::Lam { body, .. } => !term_is_num_or_var(body),
        Term::App { func, argm } => !(term_is_num_or_var(func) && term_is_num_or_var(argm)),
        Term::Ctr { args, .. } => args.iter().any(|term| !term_is_num_or_var(term)),
        Term::Fun { args, .. } => args.iter().any(|term| !term_is_num_or_var(term)),
        Term::Num { .. } => false,
        Term::Op2 { val0, val1, .. } => !(term_is_num_or_var(val0) && term_is_num_or_var(val1)),
      }
    }

    // if term is not recursive it will not enter this if
    // and will be dropped normally
    if term_is_recursive(self) {
      // `Self::Num { numb: U120::ZERO }` is being used as a default `Term`.
      // It is being repeated to avoid create a Term variable that would be dropped
      // and would call this implementation (could generate a stack overflow,
      // or unecessary calls, depending where putted)
      let term = std::mem::replace(self, Self::Num { numb: U120::ZERO });
      let mut stack = vec![term]; // this will store the recursive terms
      while let Some(mut in_term) = stack.pop() {
        // if `in_term` is not recursive nothing will be done and, therefore,
        // it will be dropped. This will call this drop function from the start
        // with `in_term` as `self` and it will not pass the first
        // `if term_is_recursive`, dropping the term normally.
        if term_is_recursive(&in_term) {
          // if the `in_term` is recursive, its children will be erased, and added to stack.
          // The `in_term` will be dropped after this, but it will not be recursive anymore,
          // so the drop will occur normally. The while will repeat this for all `in_term` children
          match &mut in_term {
            Term::Var { .. } => {}
            Term::Num { .. } => {}
            Term::Dup { expr, body, .. } => {
              let expr = std::mem::replace(expr.as_mut(), Self::Num { numb: U120::ZERO });
              let body = std::mem::replace(body.as_mut(), Self::Num { numb: U120::ZERO });
              stack.push(expr);
              stack.push(body);
              
            },
            Term::Lam { body, .. } => {
              let body = std::mem::replace(body.as_mut(), Self::Num { numb: U120::ZERO });
              stack.push(body);
            },
            Term::App { func, argm } => {
              let func = std::mem::replace(func.as_mut(), Self::Num { numb: U120::ZERO });
              let argm = std::mem::replace(argm.as_mut(), Self::Num { numb: U120::ZERO });
              stack.push(func);
              stack.push(argm);
            },
            Term::Ctr { args, .. } => {
              for arg in args {
                let arg = std::mem::replace(arg, Self::Num { numb: U120::ZERO });
                stack.push(arg);
              }
            },
            Term::Fun { args, .. } => {
              for arg in args {
                let arg = std::mem::replace(arg, Self::Num { numb: U120::ZERO });
                stack.push(arg);
              }
            },
            Term::Op2 { val0, val1, .. } => {
              let val0 = std::mem::replace(val0.as_mut(), Self::Num { numb: U120::ZERO });
              let val1 = std::mem::replace(val1.as_mut(), Self::Num { numb: U120::ZERO });
              stack.push(val0);
              stack.push(val1);
            }
          }
        }
      }
    }
  }
}

// Rule
// ====

// Func
// ====

// Oper
// ====

// TODO: This is an HVM implementation detail and shouldn't be in this crate.
// However, because the trait below needs it, I had to put it here.
#[repr(u8)]
pub enum Op {
  ADD = 0x00,
  SUB = 0x01,
  MUL = 0x02,
  DIV = 0x03,
  MOD = 0x04,
  AND = 0x05,
  OR = 0x06,
  XOR = 0x07,
  SHL = 0x08,
  SHR = 0x09,
  LTN = 0x0A,
  LTE = 0x0B,
  EQL = 0x0C,
  GTE = 0x0D,
  GTN = 0x0E,
  NEQ = 0x0F,
}

impl TryFrom<u128> for Oper {
  type Error = String;
  fn try_from(value: u128) -> Result<Self, Self::Error> {
    match value {
      x if x == Op::ADD as u128 => Ok(Oper::Add),
      x if x == Op::SUB as u128 => Ok(Oper::Sub),
      x if x == Op::MUL as u128 => Ok(Oper::Mul),
      x if x == Op::DIV as u128 => Ok(Oper::Div),
      x if x == Op::MOD as u128 => Ok(Oper::Mod),
      x if x == Op::AND as u128 => Ok(Oper::And),
      x if x == Op::OR as u128 => Ok(Oper::Or),
      x if x == Op::XOR as u128 => Ok(Oper::Xor),
      x if x == Op::SHL as u128 => Ok(Oper::Shl),
      x if x == Op::SHR as u128 => Ok(Oper::Shr),
      x if x == Op::LTN as u128 => Ok(Oper::Ltn),
      x if x == Op::LTE as u128 => Ok(Oper::Lte),
      x if x == Op::EQL as u128 => Ok(Oper::Eql),
      x if x == Op::GTE as u128 => Ok(Oper::Gte),
      x if x == Op::GTN as u128 => Ok(Oper::Gtn),
      x if x == Op::NEQ as u128 => Ok(Oper::Neq),
      _ => Err(format!("Invalid value for operation: {}", value)),
    }
  }
}

impl fmt::Display for Oper {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let str_ = match self {
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
    };
    f.write_str(str_)
  }
}

// Statement
// =========

// TODO: move these functions to impl Statement?
pub fn view_statement_header(statement: &Statement) -> String {
  let statement = remove_sign(statement);
  if let Statement::Fun { name, args, .. } = statement {
    let args = args
      .iter()
      .map(|x| x.to_string())
      .collect::<Vec<String>>()
      .join(" ");
    format!("fun ({} {})", name, args)
  } else {
    statement.to_string()
  }
}

pub fn view_statement(statement: &Statement) -> String {
  fn view_sign(sign: &Option<Signature>) -> String {
    fn format_sign(sign: &Signature) -> String {
      let hex = sign.to_hex();
      let mut text = String::new();
      for i in 0..5 {
        text.push_str("  ");
        text.push_str(&hex[i * 26..(i + 1) * 26]);
        text.push('\n');
      }
      text
    }
    match sign {
      None       => String::new(),
      Some(sign) => format!(" sign {{\n{}}}", format_sign(sign)),
    }
  }
  match statement {
    Statement::Fun { name, args, func, init, sign } => {
      let func = func.rules.iter().map(|x| format!("\n  {} = {}", x.lhs, x.rhs));
      let func = func.collect::<Vec<String>>().join("");
      let args = args.iter().map(|x| x.to_string()).collect::<Vec<String>>().join(" ");
      let init = if let Some(init) = init {
        format!(" with {{\n  {}\n}}", init)
      } else {
        "\n".to_string()
      };
      let sign = view_sign(sign);
      format!("fun ({} {}) {{{}\n}}{}{}", name, args, func, init, sign)
    }
    Statement::Ctr { name, args, sign } => {
      // correct:
      let name = name;
      let args = args
        .iter()
        .map(|x| format!(" {}", x))
        .collect::<Vec<String>>()
        .join("");
      let sign = view_sign(sign);
      format!("ctr {{{}{}}}{}", name, args, sign)
    }
    Statement::Run { expr, sign } => {
      let sign = view_sign(sign);
      format!("run {{\n  {}\n}}{}", expr, sign)
    }
    Statement::Reg { name, ownr, sign } => {
      let name = name;
      let ownr = format!("#x{:0>30x}", **ownr);
      let sign = view_sign(sign);
      format!("reg {} {{ {} }}{}", name, ownr, sign)
    }
  }
}

pub fn view_statements(statements: &[Statement]) -> String {
  let mut result = String::new();
  for statement in statements {
    result.push_str(&statement.to_string());
    result.push('\n');
  }
  result
}

impl fmt::Display for Statement {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str(&view_statement(self))
  }
}

// Removes the signature from a statement
pub fn remove_sign(statement: &Statement) -> Statement {
  match statement {
    Statement::Fun { name, args, func, init, sign: _ } => {
      Statement::Fun {
        name: *name,
        args: args.clone(),
        func: func.clone(),
        init: init.clone(),
        sign: None,
      }
    }
    Statement::Ctr { name, args, sign: _ } => {
      Statement::Ctr {
        name: *name,
        args: args.clone(),
        sign: None,
      }
    }
    Statement::Run { expr, sign: _ } => {
      Statement::Run {
        expr: expr.clone(),
        sign: None,
      }
    }
    Statement::Reg { name, ownr, sign: _ } => {
      Statement::Reg {
        name: *name,
        ownr: *ownr,
        sign: None,
      }
    }
  }
}

pub fn set_sign(statement: &Statement, new_sign: Signature) -> Statement {
  match statement {
    Statement::Fun { name, args, func, init, sign: _ } => {
      Statement::Fun {
        name: *name,
        args: args.clone(),
        func: func.clone(),
        init: init.clone(),
        sign: Some(new_sign),
      }
    }
    Statement::Ctr { name, args, sign: _ } => {
      Statement::Ctr {
        name: *name,
        args: args.clone(),
        sign: Some(new_sign),
      }
    }
    Statement::Run { expr, sign: _ } => {
      Statement::Run {
        expr: expr.clone(),
        sign: Some(new_sign),
      }
    }
    Statement::Reg { name, ownr, sign: _ } => {
      Statement::Reg {
        name: *name,
        ownr: *ownr,
        sign: Some(new_sign),
      }
    }
  }
}
