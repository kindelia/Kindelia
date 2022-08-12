pub mod http;
pub mod serialization;

use std::collections::{HashMap, HashSet};
use std::fmt::{self, Display};
use std::sync::mpsc::SyncSender;

use primitive_types::U256;
use tokio::sync::oneshot;
use serde::{Deserialize, Serialize};

use crate::node;
use crate::hvm;

use self::serialization::u256_to_hex;

type NodeRequester = SyncSender<NodeRequest>;

// Basic
// =====

// Name Type
// ---------

struct Name(u128);

impl Name {
  fn new(name: u128) -> Option<Name> {
    if name >> 120 != 0 {
      None
    } else {
      Some(Name(name))
    }
  }
}

impl fmt::Display for Name {
  fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
    write!(f, "{}", hvm::u128_to_name(self.0))
  }
}

// Hash
// ----

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
#[serde(into = "String", try_from = "String")]
pub struct Hash {
  value: U256,
}

impl From<U256> for Hash {
  fn from(value: U256) -> Self {
    Hash { value }
  }
}

impl Into<U256> for Hash {
  fn into(self) -> U256 {
    self.value
  }
}

impl Display for Hash {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let mut be_bytes = [0u8; 32];
    self.value.to_big_endian(&mut be_bytes);
    f.write_fmt(format_args!("0x{}", hex::encode(&be_bytes)))
  }
}

impl TryFrom<String> for Hash {
  type Error = String;
  fn try_from(value: String) -> Result<Self, Self::Error> {
    let rest = value.strip_prefix("0x");
    let hex_str = rest.ok_or("Missing `0x` prefix from hash hex string.")?;
    let bytes = hex::decode(hex_str).map_err(|e| e.to_string())?;
    if bytes.len() != 64 {
      return Err("Hash hex string must be 64 hex digits long.".to_string());
    }
    let bytes = &bytes[0..64];
    let value = U256::from_big_endian(bytes);
    Ok(Hash { value })
  }
}

impl Into<String> for Hash {
  fn into(self) -> String {
    u256_to_hex(&self.value)
  }
}

// mod U256_ser_hex {
//   use crate::util::U256;
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
  pub tick: u128,
}

impl Into<String> for &node::Transaction {
  fn into(self) -> String {
    hex::encode(&self.data)
  }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct BlockRepr {
  pub time: u128, // block timestamp
  pub meta: u128, // block metadata
  pub prev: Hash, // previous block (32 bytes)
  pub body: Vec<String>, // block contents (1280 bytes) 
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

#[derive(Debug, Serialize)] // TODO: Deserialize
pub struct BlockInfo {
  pub block: BlockRepr,
  pub hash: Hash,
  pub height: u64,
  pub content: Vec<hvm::Statement>,
  pub results: Option<Vec<hvm::StatementResult>>,
}

#[derive(Debug)]
pub struct FuncInfo {
  pub func: hvm::Func,
}

type RequestAnswer<T> = oneshot::Sender<T>;

// Node Internal API
// =================

pub enum NodeRequest {
  GetStats {
    tx: RequestAnswer<Stats>,
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
    name: u128,
    tx: RequestAnswer<Option<FuncInfo>>,
  },
  GetState {
    name: u128,
    tx: RequestAnswer<Option<hvm::Term>>,
  },
  /// deprecated
  TestCode {
    code: String,
    tx: RequestAnswer<Vec<hvm::StatementResult>>,
  },
  /// deprecated
  PostCode {
    code: String,
    tx: RequestAnswer<Result<(), String>>,
  },
  Run {
    hex: String,
    tx: RequestAnswer<hvm::StatementResult>,
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
