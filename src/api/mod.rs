#![warn(dead_code)]
#![warn(unused_imports)]
#![warn(non_snake_case)]
#![warn(unused_variables)]
#![warn(clippy::style)]
#![allow(clippy::let_and_return)]

pub mod client;
pub mod server;

use std::collections::{HashMap, HashSet};
use std::fmt::{self, Display};

use primitive_types::U256;
use serde::{Deserialize, Serialize};
use serde_with::{serde_as, DisplayFromStr};
use tokio::sync::oneshot;

use crate::hvm;
use crate::node;
use crate::{bits, util};

pub use crate::common::Name;

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

// Basic
// =====

// Hash
// ----

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
#[serde(into = "String", try_from = "&str")]
pub struct Hash {
  value: U256,
}

impl From<U256> for Hash {
  fn from(value: U256) -> Self {
    Hash { value }
  }
}

impl From<Hash> for U256 {
  fn from(hash: Hash) -> Self {
    hash.value
  }
}

impl Display for Hash {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let mut be_bytes = [0u8; 32];
    self.value.to_big_endian(&mut be_bytes);
    f.write_fmt(format_args!("0x{}", hex::encode(&be_bytes)))
  }
}

impl TryFrom<&str> for Hash {
  type Error = String;
  fn try_from(value: &str) -> Result<Self, Self::Error> {
    let rest = value.strip_prefix("0x");
    let hex_str = rest.ok_or("Missing `0x` prefix from hash hex string.")?;
    let bytes = hex::decode(hex_str).map_err(|e| e.to_string())?;
    if bytes.len() != 32 {
      return Err("Hash hex string must be 64 hex digits long.".to_string());
    }
    let bytes = &bytes[0..32];
    let value = U256::from_big_endian(bytes);
    Ok(Hash { value })
  }
}

impl From<Hash> for String {
  fn from(hash: Hash) -> Self {
    u256_to_hex(&hash.value)
  }
}

// HexStatement decorator for Statement
// ------------------------------------

/// Decorator for Statement that serializes it as hexadecimal string of the
/// protocol's serialization format.
#[derive(Debug, Serialize, Deserialize)]
pub struct HexStatement(hvm::Statement);

impl std::ops::Deref for HexStatement {
  type Target = hvm::Statement;
  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

impl From<HexStatement> for hvm::Statement {
  fn from(hex_statement: HexStatement) -> Self {
    hex_statement.0 
  }
}

impl From<hvm::Statement> for HexStatement {
  fn from(statement: hvm::Statement) -> Self {
    HexStatement(statement)
  }
}

impl Display for HexStatement {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let bytes = util::bitvec_to_bytes(&bits::serialized_statement(self));
    let hex = hex::encode(bytes);
    write!(f, "{}", hex)
  }
}

impl TryFrom<&str> for HexStatement {
  type Error = String;
  fn try_from(value: &str) -> Result<Self, Self::Error> {
    let bytes = hex::decode(&value).map_err(|e| e.to_string())?;
    let bits = util::bytes_to_bitvec(&bytes);
    let stmt = bits::deserialize_statement(&bits, &mut 0, &mut HashMap::new())
      .ok_or_else(|| format!("invalid Statement serialization: {}", value))?;
    Ok(HexStatement(stmt))
  }
}

// mod statement_ser_hex {
//   use crate::hvm::Statement;
//   use serde::{Deserializer, Serializer};
//   type T = U256;
//   pub fn serialize<S>(v: &T, s: S) -> Result<S::Ok, S::Error> where S: Serializer {
//     todo!()
//   }
//   pub fn deserialize<'de, D>(d: D) -> Result<T, D::Error> where D: Deserializer<'de> {
//     todo!()
//   }
// }

// API
// ===

#[derive(Debug, Serialize, Deserialize)]
pub struct Stats {
  pub tick: u64,
  pub mana: u64,
  pub space: u64,
  pub fun_count: u64,
  pub ctr_count: u64,
  pub reg_count: u64,
}

impl From<&node::Transaction> for String {
  fn from(transaction: &node::Transaction) -> Self {
    hex::encode(&transaction.data)
  }
}

#[serde_as]
#[derive(Debug, Serialize, Deserialize)]
pub struct BlockRepr {
  #[serde(with = "u128_time_ser")]
  pub time: u128, // block timestamp
  #[serde_as(as = "DisplayFromStr")]
  // TODO: serialize as Hex / refactor to array
  pub meta: u128, // block metadata
  pub prev: Hash,        // previous block hash (32 bytes)
  pub body: Vec<String>, // block contents (list of statements)
}

mod u128_time_ser {
  use chrono::prelude::{DateTime, Utc};
  use serde::{de, ser};
  use serde::{Deserializer, Serializer};
  use std::{fmt, time};
  type T = u128;

  struct TimeVisitor;

  impl<'de> de::Visitor<'de> for TimeVisitor {
    type Value = T;

    fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
      f.write_str("a valid ISO 8601 timestamp string")
    }

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
      E: de::Error,
    {
      let dt = DateTime::parse_from_rfc3339(v);
      let dt = dt.map_err(|e| {
        de::Error::custom(format!("invalid timestamp '{}': {}", v, e))
      })?;
      let st: time::SystemTime = dt.into();
      let epoc = st.duration_since(time::UNIX_EPOCH);
      let epoc = epoc.map_err(|e| {
        de::Error::custom(format!("invalid epoch '{}': {}", dt, e))
      })?;
      Ok(epoc.as_millis())
    }
  }

  pub fn serialize<S>(v: &T, s: S) -> Result<S::Ok, S::Error>
  where
    S: Serializer,
  {
    let epoc = *v as u64;
    let st = time::SystemTime::UNIX_EPOCH
      .checked_add(time::Duration::from_micros(epoc));
    let st = st.ok_or_else(|| {
      ser::Error::custom(format!("invalid time value '{}': ", epoc,))
    })?;
    let dt: DateTime<Utc> = st.into();
    s.serialize_str(&dt.format("%+").to_string())
  }
  pub fn deserialize<'de, D>(d: D) -> Result<T, D::Error>
  where
    D: Deserializer<'de>,
  {
    d.deserialize_str(TimeVisitor)
  }
}

impl From<&node::Block> for BlockRepr {
  fn from(block: &node::Block) -> Self {
    let transactions = node::extract_transactions(&block.body);
    let hexes = transactions.iter().map(|t| t.into());
    BlockRepr {
      time: block.time,
      meta: block.meta,
      prev: block.prev.into(),
      body: hexes.collect(),
    }
  }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct BlockInfo {
  pub block: BlockRepr,
  pub hash: Hash,
  pub height: u64,
  pub results: Option<Vec<hvm::StatementResult>>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct FuncInfo {
  pub func: hvm::Func,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct CtrInfo {
  pub arit: u64
}

#[derive(Debug, Serialize, Deserialize)]
pub struct RegInfo {
  pub ownr: Name,
  pub stmt: Vec<Name>
}

type RequestAnswer<T> = oneshot::Sender<T>;

// Node Internal API
// =================

pub enum NodeRequest {
  GetStats {
    tx: RequestAnswer<Stats>,
  },
  GetBlockHash {
    index: u64,
    tx: RequestAnswer<Option<U256>>,
  },
  GetBlock {
    hash: U256,
    tx: RequestAnswer<Option<BlockInfo>>,
  },
  GetBlocks {
    range: (i64, i64),
    tx: RequestAnswer<Vec<BlockInfo>>,
  },
  GetFunctions {
    tx: RequestAnswer<HashSet<u128>>,
  },
  GetFunction {
    name: Name,
    tx: RequestAnswer<Option<FuncInfo>>,
  },
  GetState {
    name: Name,
    tx: RequestAnswer<Option<hvm::Term>>,
  },
  // TODO
  // GetPeers {
  //   all: bool,
  //   tx: RequestAnswer<Vec<node::Peer<node::Address>>>,
  // },
  GetConstructor {
    name: Name,
    tx: RequestAnswer<Option<CtrInfo>>,
  },
  GetReg {
    name: Name,
    tx: RequestAnswer<Option<RegInfo>>,
  },
  /// DEPRECATED
  TestCode {
    code: String,
    tx: RequestAnswer<Vec<hvm::StatementResult>>,
  },
  /// DEPRECATED
  PostCode {
    code: String,
    tx: RequestAnswer<Result<(), String>>,
  },
  Run {
    code: Vec<hvm::Statement>,
    tx: RequestAnswer<Vec<hvm::StatementResult>>,
  },
  Publish {
    code: Vec<hvm::Statement>,
    tx: RequestAnswer<Vec<Result<(),()>>>,
  },
}

// async fn ask<T>(
//   node_query_tx: SyncSender<NodeRequest>,
//   f: impl Fn(oneshot::Sender<T>) -> NodeRequest,
// ) -> T {
//   let (tx, rx) = oneshot::channel();
//   let request = f(tx);
//   node_query_tx.send(request).unwrap();
//   let result = rx.await.expect("Node query channel closed");
//   result
// }
