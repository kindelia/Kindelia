#![allow(clippy::style)]

use serde::{Deserialize, Serialize};

use crate::ast::{Func, Oper, Rule, Statement, Term};
use kindelia_common::{crypto, Name, U120};
use thiserror::Error;

pub type ParseResult<'a, A> = Result<(&'a str, A), ParseErr>;

#[derive(Error, Debug, Clone, Serialize, Deserialize)]
#[error("Parser error")]
pub struct ParseErr {
  pub code: String,
  pub erro: String,
}

impl ParseErr {
  fn new<C, E>(code: C, erro: E) -> Self
  where
    C: Into<String>,
    E: Into<String>,
  {
    ParseErr { code: code.into(), erro: erro.into() }
  }
}

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
  for _ in 0..amount {
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
    if head(code) == '/' && nth(code, 1) == '/' {
      while head(code) != '\n' && head(code) != '\0' {
        code = tail(code);
      }
      continue;
    }
    break;
  }
  return code;
}

// fn hash(name: &str) -> u128 {
//   let mut hasher = hash_map::DefaultHasher::new();
//   name.hash(&mut hasher);
//   hasher.finish() as u128
// }

fn is_name_char(chr: char) -> bool {
  return chr == '_'
    || chr == '.'
    || chr >= 'a' && chr <= 'z'
    || chr >= 'A' && chr <= 'Z'
    || chr >= '0' && chr <= '9';
}

pub fn parse_char(code: &str, chr: char) -> ParseResult<()> {
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

pub fn parse_numb<T>(code: &str) -> ParseResult<T>
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

pub fn parse_name(code: &str) -> ParseResult<Name> {
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
        erro: format!("Expected identifier, found `{}`.", head(code)),
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

// Like parse_name, but we're sure it's an actual name and not something else
pub fn parse_strict_name(code: &str) -> ParseResult<Name> {
  let code = skip(code);
  let mut name = String::new();
  let mut code = code;
  while is_name_char(head(code)) {
    name.push(head(code));
    code = tail(code);
  }
  if name.is_empty() {
    return Err(ParseErr {
      code: code.to_string(),
      erro: format!("Expected identifier, found `{}`.", head(code)),
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

pub fn parse_hex(code: &str) -> ParseResult<Vec<u8>> {
  let mut data: Vec<u8> = Vec::new();
  let mut code = skip(code);
  while nth(code,0).is_ascii_hexdigit() && nth(code,1).is_ascii_hexdigit() {
    data.append(&mut hex::decode(&String::from_iter([nth(code,0),nth(code,1)])).unwrap());
    code = drop(code, 2);
    code = skip(code);
  }
  return Ok((code, data));
}

pub fn parse_until<A>(code: &str, stop: char, read: fn(&str) -> ParseResult<A>) -> ParseResult<Vec<A>> {
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

pub fn parse_term(code: &str) -> ParseResult<Term> {
  let code = skip(code);
  match head(code) {
    // Lambda definition
    '@' => {
      let code         = tail(code);
      let (code, name) = parse_name(code)?;
      let (code, body) = parse_term(code)?;
      let term = Term::lam(name, Box::new(body));
      return Ok((code, term));
    }
    // Redex
    '(' => {
      let code = skip(tail(code));
      let (code, oper) = parse_oper(code);
      // Native number operation
      if let Some(oper) = oper {
        let (code, val0) = parse_term(code)?;
        let (code, val1) = parse_term(code)?;
        let (code, _unit) = parse_char(code, ')')?;
        let term = Term::op2(oper, Box::new(val0), Box::new(val1));
        return Ok((code, term));
      }
      // Lambda application
      else if head(code) == '!' {
        let code = tail(code);
        let (code, func) = parse_term(code)?;
        let (code, argm) = parse_term(code)?;
        let (code, _unit) = parse_char(code, ')')?;
        let term = Term::app(Box::new(func), Box::new(argm));
        return Ok((code, term));
      }
      // Function application
      else {
        let (code, name) = parse_strict_name(code)?;
        let (code, args) = parse_until(code, ')', parse_term)?;
        let term = Term::fun(name, args);
        return Ok((code, term));
      }
    }
    // Constructor
    '{' => {
      let code = tail(code);
      let (code, name) = parse_strict_name(code)?;
      let (code, args) = parse_until(code, '}', parse_term)?;
      let term = Term::ctr(name, args);
      return Ok((code, term));
    }
    // Tuple sugar
    '[' => {
      let code = tail(code);
      let (code, vals) = parse_until(code, ']', parse_term)?;
      let num_vals = vals.len();
      if num_vals <= 12 {
        let name = Name::from_str(&format!("T{}", num_vals)).unwrap();
        let term = Term::ctr(name, vals);
        return Ok((code, term));
      } else {
        return Err(ParseErr { code: code.to_string(), erro: "Tuple too long".to_string() });
      }
    }
    // Number
    '#' => {
      let code = tail(code);
      let (code, numb) = parse_numb(code)?;
      let term = Term::num(numb);
      return Ok((code, term));
    }
    // Name to number sugar
    '\'' => {
      let code = tail(code);
      let (code, name) = parse_strict_name(code)?;
      let (code, _unit) = parse_char(code, '\'')?;
      let numb = name.into();
      let term = Term::num(numb);
      return Ok((code, term));
    }
    _ => {
      if let ('d','u','p',' ') = (nth(code,0), nth(code,1), nth(code,2), nth(code,3)) {
        let code = drop(code,3);
        let (code, nam0) = parse_name(code)?;
        let (code, nam1) = parse_name(code)?;
        let (code, _unit) = parse_char(code, '=')?;
        let (code, expr) = parse_term(code)?;
        let (code, _unit) = parse_char(code, ';')?;
        let (code, body) = parse_term(code)?;
        let term = Term::dup(nam0, nam1, Box::new(expr), Box::new(body));
        return Ok((code, term));
      // let x = y; z
      // ------------
      // (@x z y)
      } else if let ('l','e','t',' ') = (nth(code,0), nth(code,1), nth(code,2), nth(code,3)) {
        let code = drop(code,3);
        let (code, name) = parse_name(code)?;
        let (code, _unit) = parse_char(code, '=')?;
        let (code, expr) = parse_term(code)?;
        let (code, _unit) = parse_char(code, ';')?;
        let (code, body) = parse_term(code)?;
        let lam = Term::lam(name, Box::new(body));
        let app = Term::app(Box::new(lam), Box::new(expr));
        return Ok((code, app));
      // ask x = y; z
      // ------------
      // (y @x z)
      } else if let ('a','s','k',' ') = (nth(code,0), nth(code,1), nth(code,2), nth(code,3)) {
        let code = skip(drop(code,3));
        if nth(code,0) == '(' {
          let (code, expr) = parse_term(code)?;
          let (code, _unit) = parse_char(code, ';')?;
          let (code, body) = parse_term(code)?;
          let argm = Term::lam(Name::NONE, Box::new(body));
          let term = Term::app(Box::new(expr), Box::new(argm));
          return Ok((code, term));
        } else {
          let (code, name) = parse_name(code)?;
          let (code, _unit) = parse_char(code, '=')?;
          let (code, expr) = parse_term(code)?;
          let (code, _unit) = parse_char(code, ';')?;
          let (code, body) = parse_term(code)?;
          let argm = Term::lam(name, Box::new(body));
          let term = Term::app(Box::new(expr), Box::new(argm));
          return Ok((code, term));
        }
      } else {
        let (code, name) = parse_name(code)?;
        let term = Term::var(name);
        return Ok((code, term));
      }
    }
  }
}

pub fn parse_oper(in_code: &str) -> (&str, Option<Oper>) {
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

pub fn parse_rule(code: &str) -> ParseResult<Rule> {
  // TODO: custom parser for lhs
  let (code, lhs) = parse_term(code)?;
  let (code, ())  = parse_char(code, '=')?;
  let (code, rhs) = parse_term(code)?;
  return Ok((code, Rule { lhs, rhs }));
}

pub fn parse_rules(code: &str) -> ParseResult<Vec<Rule>> {
  let (code, rules) = parse_until(code, '\0', parse_rule)?;
  return Ok((code, rules));
}

pub fn parse_sign(code: &str) -> ParseResult<Option<crypto::Signature>> {
  let code = skip(code);
  if let ('s','i','g','n') = (nth(code,0), nth(code,1), nth(code,2), nth(code,3)) {
    let code = drop(code,4);
    let (code, _unit) = parse_char(code, '{')?;
    let (code, sign) = parse_hex(code)?;
    let (code, _unit) = parse_char(code, '}')?;
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

pub fn parse_statement(code: &str) -> ParseResult<Statement> {
  let code = skip(code);
  match (nth(code,0), nth(code,1), nth(code,2)) {
    ('f','u','n') => {
      let code = drop(code,3);
      let (code, _unit) = parse_char(code, '(')?;
      let (code, name) = parse_strict_name(code)?;
      let (code, args) = parse_until(code, ')', parse_name)?;
      let (code, _unit) = parse_char(code, '{')?;
      let (code, ruls) = parse_until(code, '}', parse_rule)?;
      let code = skip(code);
      let (code, init) = if let ('w','i','t','h') = (nth(code,0), nth(code,1), nth(code,2), nth(code,3)) {
        let code = drop(code,4);
        let (code, _unit) = parse_char(code, '{')?;
        let (code, init) = parse_term(code)?;
        let (code, _unit) = parse_char(code, '}')?;
        (code, Some(init))
      } else {
        (code, None)
      };
      let (code, sign) = parse_sign(code)?;
      let func = Func { rules: ruls };
      return Ok((code, Statement::Fun { name, args, func, init, sign }));
    }
    ('c','t','r') => {
      let code = drop(code,3);
      let (code, _unit) = parse_char(code, '{')?;
      let (code, name) = parse_strict_name(code)?;
      let (code, args) = parse_until(code, '}', parse_name)?;
      let (code, sign) = parse_sign(code)?;
      return Ok((code, Statement::Ctr { name, args, sign }));
    }
    ('r','u','n') => {
      let code = drop(code,3);
      let (code, _unit) = parse_char(code, '{')?;
      let (code, expr) = parse_term(code)?;
      let (code, _unit) = parse_char(code, '}')?;
      let (code, sign) = parse_sign(code)?;
      return Ok((code, Statement::Run { expr, sign  }));
    }
    // reg Foo.Bar { #x123456 } sign { signature }
    ('r','e','g') => {
      let code = skip(drop(code, 3));
      let (code, name) =
        if nth(code, 0) == '{' {
          (code, Name::EMPTY)
        } else {
          parse_strict_name(code)?
        };
      let (code, _unit) = parse_char(code, '{')?;
      let code = skip(code);
      let (code, ownr) = match head(code) {
        '#' => {
          let code = tail(code);
          parse_numb(code)?
        }
        '\'' => {
          let code = tail(code);
          let (code, name) = parse_strict_name(code)?;
          let (code, _unit) = parse_char(code, '\'')?;
          let numb: U120 = name.into();
          (code, numb)
        },
        _ => return Err(ParseErr::new(code, "Expected a number representation"))
      };
      let (code, _unit) = parse_char(code, '}')?;
      let (code, sign) = parse_sign(code)?;
      return Ok((code, Statement::Reg { name, ownr, sign }));
    }
    _ => {
      return Err(ParseErr { code: code.to_string(),  erro: "Expected statement.".to_string() });
    }
  }
}

pub fn parse_statements(code: &str) -> ParseResult<Vec<Statement>> {
  parse_until(code, '\0', parse_statement)
}

pub fn parse_code(code: &str) -> Result<Vec<Statement>, String> {
  let statements = parse_statements(code);
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
