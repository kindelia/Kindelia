#![warn(dead_code)]
#![warn(unused_imports)]
#![warn(non_snake_case)]
#![warn(unused_variables)]
#![warn(clippy::style)]
#![allow(clippy::let_and_return)]

use std::collections::HashSet;
use tokio::sync::oneshot;

use kindelia_common::Name;
use kindelia_lang::ast;

use crate::net::ProtoComm;
use crate::node;
use crate::runtime;

// TODO: i think we should put this Hash structure in `node.rs`
// and transform the block/transaction to use it instead of pure U256.
pub use hash::Hash;
mod hash {
  use std::{
    fmt::{self, Display},
    ops::Deref,
  };

  use primitive_types::U256;
  use serde::{Deserialize, Serialize};

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

  impl Deref for Hash {
    type Target = U256;
    fn deref(&self) -> &Self::Target {
      &self.value
    }
  }

  impl Display for Hash {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
      let value = **self; // convert to U256
      let mut be_bytes = [0u8; 32];
      value.to_big_endian(&mut be_bytes);
      write!(f, "0x{}", hex::encode(be_bytes))
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

  // needed for serde::Deserialize
  impl From<Hash> for String {
    fn from(hash: Hash) -> Self {
      format!("{}", hash)
    }
  }
}

pub use hex_statement::HexStatement;
mod hex_statement {
  use std::collections::HashMap;
  use std::fmt::{self, Display};

  use kindelia_lang::ast::Statement;
  use serde::{Deserialize, Serialize};

  use crate::{bits::ProtoSerialize, util};

  /// Decorator for Statement that serializes it as hexadecimal string of the
  /// protocol's serialization format.
  #[derive(Debug, Clone, Serialize, Deserialize)]
  pub struct HexStatement(Statement);

  impl std::ops::Deref for HexStatement {
    type Target = Statement;
    fn deref(&self) -> &Self::Target {
      &self.0
    }
  }

  impl From<HexStatement> for Statement {
    fn from(hex_statement: HexStatement) -> Self {
      hex_statement.0
    }
  }

  impl From<Statement> for HexStatement {
    fn from(statement: Statement) -> Self {
      HexStatement(statement)
    }
  }

  impl Display for HexStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
      let bytes = util::bitvec_to_bytes(&self.proto_serialized());
      let hex = hex::encode(bytes);
      write!(f, "{}", hex)
    }
  }

  impl TryFrom<&str> for HexStatement {
    type Error = String;
    fn try_from(value: &str) -> Result<Self, Self::Error> {
      let bytes = hex::decode(value).map_err(|e| e.to_string())?;
      let bits = util::bytes_to_bitvec(&bytes);
      let stmt =
        Statement::proto_deserialize(&bits, &mut 0, &mut HashMap::new())
          .ok_or_else(|| {
            format!("invalid Statement serialization: {}", value)
          })?;
      Ok(HexStatement(stmt))
    }
  }
}

pub use answers::{
  BlockInfo, CtrInfo, FuncInfo, LimitStats, PublishError, PublishResults,
  RegInfo, Stats,
};
mod answers {
  use serde::{Deserialize, Serialize};
  use thiserror::Error;

  use kindelia_common::Name;
  use kindelia_lang::ast::Func;

  use crate::node::{PoolError, TransactionError};
  use crate::runtime::StatementResult;

  use super::{BlockRepr, Hash};

  #[derive(Debug, Serialize, Deserialize)]
  pub struct LimitStats {
    pub limit: u64,
    pub used: u64,
    pub available: u64,
  }

  #[derive(Debug, Serialize, Deserialize)]
  pub struct Stats {
    pub tick: u64,
    pub mana: LimitStats,
    pub space: LimitStats,
    pub fun_count: u64,
    pub ctr_count: u64,
    pub reg_count: u64,
  }

  #[derive(Debug, Serialize, Deserialize)]
  pub struct BlockInfo {
    pub block: BlockRepr,
    pub hash: Hash,
    pub height: u64,
    pub results: Option<Vec<StatementResult>>,
  }

  #[derive(Debug, Serialize, Deserialize)]
  pub struct FuncInfo {
    pub func: Func,
  }

  #[derive(Debug, Serialize, Deserialize)]
  pub struct CtrInfo {
    pub arit: u64,
  }

  #[derive(Debug, Serialize, Deserialize)]
  pub struct RegInfo {
    pub ownr: Name,
    pub stmt: Vec<Name>,
  }

  #[derive(Debug, Error, Serialize, Deserialize)]
  #[error(transparent)]
  pub enum PublishError {
    PoolError(#[from] PoolError),
    TransactionError(#[from] TransactionError),
  }

  pub type PublishResults = Vec<Result<(), PublishError>>;
}

pub use block_repr::BlockRepr;
// TODO: i dont know where to put this
mod block_repr {
  use serde::{Deserialize, Serialize};
  use serde_with::{serde_as, DisplayFromStr};

  use crate::node;

  use super::Hash;

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
      let transactions = block.body.extract_transactions();
      let hexes = transactions.iter().map(|t| t.into());
      BlockRepr {
        time: block.time,
        meta: block.meta,
        prev: block.prev.into(),
        body: hexes.collect(),
      }
    }
  }
}

// Node Internal API
// =================

pub type ReqAnsSend<T> = oneshot::Sender<T>;
pub type ReqAnsRecv<T> = oneshot::Receiver<T>;

/// Represents each possible request for the node api
pub enum NodeRequest<C: ProtoComm> {
  GetStats {
    tx: ReqAnsSend<Stats>,
  },
  GetBlockHash {
    index: u64,
    tx: ReqAnsSend<Option<Hash>>,
  },
  GetBlock {
    hash: Hash,
    tx: ReqAnsSend<Option<BlockInfo>>,
  },
  GetBlocks {
    range: (i64, i64),
    tx: ReqAnsSend<Vec<BlockInfo>>,
  },
  GetFunctions {
    tx: ReqAnsSend<HashSet<u128>>,
  },
  GetFunction {
    name: Name,
    tx: ReqAnsSend<Option<FuncInfo>>,
  },
  GetState {
    name: Name,
    tx: ReqAnsSend<Option<ast::Term>>,
  },
  GetPeers {
    all: bool,
    tx: ReqAnsSend<Vec<node::Peer<C::Address>>>,
  },
  GetConstructor {
    name: Name,
    tx: ReqAnsSend<Option<CtrInfo>>,
  },
  GetReg {
    name: Name,
    tx: ReqAnsSend<Option<RegInfo>>,
  },
  /// DEPRECATED
  RunCode {
    code: String,
    tx: ReqAnsSend<Vec<runtime::StatementResult>>,
  },
  /// DEPRECATED
  PublishCode {
    code: String,
    tx: ReqAnsSend<Result<PublishResults, String>>,
  },
  Run {
    code: Vec<ast::Statement>,
    tx: ReqAnsSend<Vec<runtime::StatementResult>>,
  },
  Publish {
    code: Vec<ast::Statement>,
    tx: ReqAnsSend<PublishResults>,
  },
}

impl<C: ProtoComm> NodeRequest<C> {
  pub fn get_stats() -> (Self, ReqAnsRecv<Stats>) {
    let (tx, rx) = oneshot::channel();
    (NodeRequest::GetStats { tx }, rx)
  }
  pub fn get_block_hash(index: u64) -> (Self, ReqAnsRecv<Option<Hash>>) {
    let (tx, rx) = oneshot::channel();
    (NodeRequest::GetBlockHash { index, tx }, rx)
  }
  pub fn get_block(hash: Hash) -> (Self, ReqAnsRecv<Option<BlockInfo>>) {
    let (tx, rx) = oneshot::channel();
    (NodeRequest::GetBlock { hash, tx }, rx)
  }
  pub fn get_blocks(range: (i64, i64)) -> (Self, ReqAnsRecv<Vec<BlockInfo>>) {
    let (tx, rx) = oneshot::channel();
    (NodeRequest::GetBlocks { range, tx }, rx)
  }
  pub fn get_functions() -> (Self, ReqAnsRecv<HashSet<u128>>) {
    let (tx, rx) = oneshot::channel();
    (NodeRequest::GetFunctions { tx }, rx)
  }
  pub fn get_function(name: Name) -> (Self, ReqAnsRecv<Option<FuncInfo>>) {
    let (tx, rx) = oneshot::channel();
    (NodeRequest::GetFunction { name, tx }, rx)
  }
  pub fn get_state(name: Name) -> (Self, ReqAnsRecv<Option<ast::Term>>) {
    let (tx, rx) = oneshot::channel();
    (NodeRequest::GetState { name, tx }, rx)
  }
  pub fn get_peers(
    all: bool,
  ) -> (Self, ReqAnsRecv<Vec<node::Peer<C::Address>>>) {
    let (tx, rx) = oneshot::channel();
    (NodeRequest::GetPeers { all, tx }, rx)
  }
  pub fn get_constructor(name: Name) -> (Self, ReqAnsRecv<Option<CtrInfo>>) {
    let (tx, rx) = oneshot::channel();
    (NodeRequest::GetConstructor { name, tx }, rx)
  }
  pub fn get_reg(name: Name) -> (Self, ReqAnsRecv<Option<RegInfo>>) {
    let (tx, rx) = oneshot::channel();
    (NodeRequest::GetReg { name, tx }, rx)
  }
  pub fn test_code(
    code: String,
  ) -> (Self, ReqAnsRecv<Vec<runtime::StatementResult>>) {
    let (tx, rx) = oneshot::channel();
    (NodeRequest::RunCode { code, tx }, rx)
  }
  pub fn post_code(
    code: String,
  ) -> (Self, ReqAnsRecv<Result<PublishResults, String>>) {
    let (tx, rx) = oneshot::channel();
    (NodeRequest::PublishCode { code, tx }, rx)
  }
  pub fn run(
    code: Vec<ast::Statement>,
  ) -> (Self, ReqAnsRecv<Vec<runtime::StatementResult>>) {
    let (tx, rx) = oneshot::channel();
    (NodeRequest::Run { code, tx }, rx)
  }
  pub fn publish(
    code: Vec<ast::Statement>,
  ) -> (Self, ReqAnsRecv<PublishResults>) {
    let (tx, rx) = oneshot::channel();
    (NodeRequest::Publish { code, tx }, rx)
  }
}
