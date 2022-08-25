use serde::ser::{SerializeStruct, SerializeStructVariant};

use crate::hvm::{Func, Rule, Statement, Term};
use crate::util::U256;

// Util
// ====

// U256 to hexadecimal string
pub fn u256_to_hex(value: &U256) -> String {
  let mut be_bytes = [0u8; 32];
  value.to_big_endian(&mut be_bytes);
  format!("0x{}", hex::encode(be_bytes))
}

// // Hexadecimal string to U256
// pub fn hex_to_u256(hex: &str) -> Result<U256, String> {
//   let bytes = hex::decode(hex);
//   let bytes = match bytes {
//     Ok(bytes) => bytes,
//     Err(_) => return Err(format!("Invalid hexadecimal string: '{}'", hex)),
//   };
//   if bytes.len() != 256 / 8 {
//     Err(format!("Invalid hexadecimal string: {}", hex))
//   } else {
//     let num = U256::from_big_endian(&bytes);
//     Ok(num)
//   }
// }

// Serde Implementations
// =====================

impl serde::Serialize for Statement {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: serde::Serializer,
  {
    match self {
      // TODO: serialize sign
      Statement::Fun { name, args, func, init, sign: _ } => {
        let mut s = serializer.serialize_struct_variant("Statement", 0, "Fun", 4)?;
        s.serialize_field("name", name)?;
        s.serialize_field("args", args)?;
        s.serialize_field("func", func)?;
        s.serialize_field("init", init)?;
        s.end()
      }
      // TODO: serialize sign
      Statement::Ctr { name, args, sign: _ } => {
        let mut s = serializer.serialize_struct_variant("Statement", 1, "Ctr", 2)?;
        s.serialize_field("name", name)?;
        s.serialize_field("args", args)?;
        s.end()
      }
      // TODO: serialize sign
      Statement::Run { expr, sign: _ } => {
        let mut s = serializer.serialize_struct_variant("Statement", 2, "Run", 1)?;
        s.serialize_field("body", expr)?;
        s.end()
      }
      // TODO: serialize
      Statement::Reg { .. } => {
        panic!("TODO");
      }
    }
  }
}

impl serde::Serialize for Func {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: serde::Serializer,
  {
    let mut s = serializer.serialize_struct("Func", 2)?;
    s.serialize_field("rules", &self.rules)?;
    s.end()
  }
}

impl serde::Serialize for Rule {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: serde::Serializer,
  {
    let mut s = serializer.serialize_struct("Rule", 2)?;
    s.serialize_field("lhs", &self.lhs)?;
    s.serialize_field("rhs", &self.rhs)?;
    s.end()
  }
}

impl serde::Serialize for Term {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: serde::Serializer,
  {
    match self {
      Term::Var { name } => {
        let mut s = serializer.serialize_struct_variant("Term", 0, "Var", 1)?;
        s.serialize_field("name", name)?;
        s.end()
      }
      Term::Dup { nam0, nam1, expr, body } => {
        let mut s = serializer.serialize_struct_variant("Term", 1, "Dup", 4)?;
        s.serialize_field("nam0", nam0)?;
        s.serialize_field("nam1", nam1)?;
        s.serialize_field("expr", &expr)?;
        s.serialize_field("body", &body)?;
        s.end()
      }
      Term::Lam { name, body } => {
        let mut s = serializer.serialize_struct_variant("Term", 2, "Lam", 2)?;
        s.serialize_field("name", name)?;
        s.serialize_field("body", &body)?;
        s.end()
      }
      Term::App { func, argm } => {
        let mut s = serializer.serialize_struct_variant("Term", 3, "App", 2)?;
        s.serialize_field("func", &func)?;
        s.serialize_field("argm", &argm)?;
        s.end()
      }
      Term::Ctr { name, args } => {
        let mut s = serializer.serialize_struct_variant("Term", 4, "Ctr", 2)?;
        s.serialize_field("name", name)?;
        s.serialize_field("args", args)?;
        s.end()
      }
      Term::Fun { name, args } => {
        let mut s = serializer.serialize_struct_variant("Term", 5, "Fun", 2)?;
        s.serialize_field("name", name)?;
        s.serialize_field("args", args)?;
        s.end()
      }
      Term::Num { numb } => {
        let mut s = serializer.serialize_struct_variant("Term", 6, "Num", 1)?;
        s.serialize_field("numb", &numb.to_string())?;
        s.end()
      }
      Term::Op2 { oper, val0, val1 } => {
        let mut s = serializer.serialize_struct_variant("Term", 7, "Op2", 3)?;
        s.serialize_field("oper", &oper.to_string())?;
        s.serialize_field("val0", &val0)?;
        s.serialize_field("val1", &val1)?;
        s.end()
      }
    }
  }
}
