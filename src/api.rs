#![warn(dead_code)]
#![warn(unused_imports)]
#![warn(non_snake_case)]
#![warn(unused_variables)]
#![warn(clippy::style)]
#![allow(clippy::let_and_return)]
use std::sync::mpsc::SyncSender;

use serde_json::json;
use tokio::net::TcpListener;
use tokio::sync::oneshot;
use tokio_stream::wrappers::TcpListenerStream;
use warp::hyper::StatusCode;
use warp::reject::{self, Rejection};
use warp::reply::{self, Reply};
use warp::{path, Filter};

use crate::hvm;
use crate::node::Request as NodeRequest;
use crate::util::U256;

// Util
// ====

// U256 to hexadecimal string
pub fn u256_to_hex(value: &U256) -> String {
  let mut be_bytes = [0u8; 32];
  value.to_big_endian(&mut be_bytes);
  format!("0x{}", hex::encode(be_bytes))
}

// Hexadecimal string to U256
pub fn hex_to_u256(hex: &str) -> Result<U256, String> {
  let bytes = hex::decode(hex);
  let bytes = match bytes {
    Ok(bytes) => bytes,
    Err(_) => return Err(format!("Invalid hexadecimal string: '{}'", hex)),
  };
  if bytes.len() != 256 / 8 {
    Err(format!("Invalid hexadecimal string: {}", hex))
  } else {
    let num = U256::from_big_endian(&bytes);
    Ok(num)
  }
}

fn ok_json<T>(data: T) -> warp::reply::Json
where
  T: serde::Serialize,
{
  let json_body = json!({ "status": "ok", "data": data });
  warp::reply::json(&json_body)
}

fn error_json<T>(error: T) -> warp::reply::Json
where
  T: serde::Serialize,
{
  let json_body = json!({ "status": "error", "error": error });
  warp::reply::json(&json_body)
}

// HVM
// ===

pub fn name_to_u128_safe(name: &str) -> Option<u128> {
  let mut num: u128 = 0;
  for (i, chr) in name.chars().enumerate() {
    debug_assert!(i < 20, "Name too big: `{}`.", name);
    num = (num << 6) + char_to_u128_safe(chr)?;
  }
  Some(num)
}

pub const fn char_to_u128_safe(chr: char) -> Option<u128> {
  match chr {
    '.' => Some(0),
    '0'..='9' => Some(1 + chr as u128 - '0' as u128),
    'A'..='Z' => Some(11 + chr as u128 - 'A' as u128),
    'a'..='z' => Some(37 + chr as u128 - 'a' as u128),
    '_' => Some(63),
    _ => None,
  }
}

fn u128_names_to_strings(names: &[u128]) -> Vec<String> {
  names.iter().copied().map(hvm::u128_to_name).collect::<Vec<_>>()
}

// Erros
// =====

#[derive(Debug)]
struct InvalidParameter {
  name: Option<String>,
  message: String,
}

impl From<String> for InvalidParameter {
  fn from(message: String) -> Self {
    InvalidParameter { name: None, message }
  }
}

impl reject::Reject for InvalidParameter {}

// API
// ===

async fn handle_rejection(err: Rejection) -> Result<impl Reply, std::convert::Infallible> {
  if err.is_not_found() {
    Ok(reply::with_status(error_json("NOT_FOUND"), StatusCode::NOT_FOUND))
  } else if let Some(e) = err.find::<InvalidParameter>() {
    let name = e.name.as_ref().map(|n| format!(" '{}'", n)).unwrap_or_default();
    let msg = format!("Parameter{} is invalid: {}", name, e.message);
    Ok(reply::with_status(error_json(msg), StatusCode::BAD_REQUEST))
  } else {
    eprintln!("unhandled rejection: {:?}", err);
    Ok(reply::with_status(error_json("INTERNAL_SERVER_ERROR"), StatusCode::INTERNAL_SERVER_ERROR))
  }
}

pub fn api_loop(node_query_sender: SyncSender<NodeRequest>) {
  let runtime = tokio::runtime::Runtime::new().unwrap();

  runtime.block_on(async move {
    api_serve(node_query_sender).await;
  });
}

async fn api_serve(node_query_sender: SyncSender<NodeRequest>) {
  async fn ask<T>(
    node_query_tx: SyncSender<NodeRequest>,
    f: impl Fn(oneshot::Sender<T>) -> NodeRequest,
  ) -> T {
    let (tx, rx) = oneshot::channel();
    let request = f(tx);
    node_query_tx.send(request).unwrap();
    let result = rx.await.expect("Node query channel closed");
    result
  }

  let root = warp::path::end().map(|| "UP");

  // TODO: macro to wrap those clones

  let query_tx = node_query_sender.clone();
  let get_tick = path!("tick").then(move || {
    let query_tx = query_tx.clone();
    async move {
      let tick = ask(query_tx, |tx| NodeRequest::GetTick { tx }).await;
      ok_json(format!("Tick: {}", tick))
    }
  });

  // == Blocks ==

  let query_tx = node_query_sender.clone();
  let get_blocks = path!("blocks").then(move || {
    let query_tx = query_tx.clone();
    async move {
      let range = (-10, -1);
      let blocks = ask(query_tx, |tx| NodeRequest::GetBlocks { range, tx }).await;
      ok_json(blocks)
    }
  });

  let get_block = || {
    let query_tx = node_query_sender.clone();
    path!("blocks" / String / ..).and_then(move |hash_hex: String| {
      let query_tx = query_tx.clone();
      async move {
        let hash_hex = hash_hex.strip_prefix("0x").unwrap_or(&hash_hex);
        match hex_to_u256(hash_hex) {
          Ok(hash) => {
            let block = ask(query_tx, |tx| NodeRequest::GetBlock { hash, tx }).await;
            Ok(block)
          }
          Err(err) => {
            Err(reject::custom(InvalidParameter::from(format!("Invalid block hash: '{}'", err))))
          }
        }
      }
    })
  };

  let get_block_go = get_block().and(path!()).map(ok_json);

  let blocks_router = get_blocks //
    .or(get_block_go);

  // == Functions ==

  let query_tx = node_query_sender.clone();
  let get_functions = path!("functions").then(move || {
    let query_tx = query_tx.clone();
    async move {
      let functions = ask(query_tx, |tx| NodeRequest::GetFunctions { tx }).await;
      let functions: Vec<u128> = functions.into_iter().map(|x| x as u128).collect();
      let functions = u128_names_to_strings(&functions);
      ok_json(functions)
    }
  });

  let get_function_base =
    path!("functions" / String / ..).and_then(move |name_txt: String| async move {
      if let Some(name) = name_to_u128_safe(&name_txt) {
        Ok(name)
      } else {
        let msg = format!("Invalid function name: '{}'", name_txt);
        Err(reject::custom(InvalidParameter::from(msg)))
      }
    });

  let query_tx = node_query_sender.clone();
  let get_function = get_function_base.and(path!()).and_then(move |name: u128| {
    let query_tx = query_tx.clone();
    async move {
      let function = ask(query_tx, |tx| NodeRequest::GetFunction { name, tx }).await;
      if let Some(function) = function {
        Ok(ok_json(function))
      } else {
        Err(reject::not_found())
      }
    }
  });

  let query_tx = node_query_sender.clone();
  let get_function_state = get_function_base.and(path!("state")).and_then(move |name: u128| {
    let query_tx = query_tx.clone();
    async move {
      let state = ask(query_tx, |tx| NodeRequest::GetState { name, tx }).await;
      if let Some(state) = state {
        Ok(ok_json(state))
      } else {
        Err(reject::not_found())
      }
    }
  });

  let functions_router = get_functions //
    .or(get_function) //
    .or(get_function_state);

  // ==

  let app = root.or(get_tick).or(blocks_router).or(functions_router);
  let app = app.recover(handle_rejection);
  let app = app.map(|reply| warp::reply::with_header(reply, "Access-Control-Allow-Origin", "*"));

  let listener_v4 = TcpListener::bind("0.0.0.0:8000").await.unwrap();
  // let listener_v6 = TcpListener::bind("[::]:8000").await.unwrap();
  let listener = TcpListenerStream::new(listener_v4)
    // .merge(TcpListenerStream::new(listener_v6))
    ;

  warp::serve(app).run_incoming(listener).await;
}

mod ser {
  use super::{u128_names_to_strings, u256_to_hex};
  use crate::hvm::{u128_to_name, Func, Rule, Statement, StatementErr, StatementInfo, Term};
  use crate::node::{Block, BlockInfo, FuncInfo};
  use serde::ser::{SerializeStruct, SerializeStructVariant};
  use serde::Serialize;

  impl Serialize for BlockInfo {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
      S: serde::Serializer,
    {
      let mut s = serializer.serialize_struct("BlockInfo", 5)?;
      s.serialize_field("block", &self.block)?;
      s.serialize_field("height", &self.height)?;
      s.serialize_field("hash", &u256_to_hex(&self.hash))?;
      s.serialize_field("content", &self.content)?;
      s.serialize_field("results", &self.results)?;

      s.end()
    }
  }

  impl Serialize for StatementInfo {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
      S: serde::Serializer,
    {
      match self {
        StatementInfo::Ctr { name, args } => {
          let code = 0;
          let mut s = serializer.serialize_struct_variant("StatementInfo", code, "Ctr", 2)?;
          s.serialize_field("name", &u128_to_name(*name))?;
          s.serialize_field("args", &u128_names_to_strings(args))?;
          s.end()
        }
        StatementInfo::Fun { name, args } => {
          let code = 1;
          let mut s = serializer.serialize_struct_variant("StatementInfo", code, "Fun", 2)?;
          s.serialize_field("name", &u128_to_name(*name))?;
          s.serialize_field("args", &u128_names_to_strings(args))?;
          s.end()
        }
        StatementInfo::Run { done_term, used_mana, size_diff, end_size } => {
          let code = 2;
          let mut s = serializer.serialize_struct_variant("StatementInfo", code, "Run", 4)?;
          s.serialize_field("done_term", &done_term)?;
          s.serialize_field("used_mana", &used_mana.to_string())?;
          s.serialize_field("size_diff", &size_diff.to_string())?;
          s.serialize_field("end_size", &end_size.to_string())?;
          s.end()
        }
        StatementInfo::Reg { .. } => {
          panic!("TODO");
        }
      }
    }
  }

  impl Serialize for StatementErr {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
      S: serde::Serializer,
    {
      let mut s = serializer.serialize_struct("StatementErr", 1)?;
      s.serialize_field("err", &self.err)?;
      s.end()
    }
  }

  impl Serialize for FuncInfo {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
      S: serde::Serializer,
    {
      let mut s = serializer.serialize_struct("FuncInfo", 1)?;
      s.serialize_field("func", &self.func)?;
      s.end()
    }
  }

  impl serde::Serialize for Block {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
      S: serde::Serializer,
    {
      let body = &self.body.data;
      let body_bytes = body.into_iter().collect::<Vec<_>>();
      let mut s = serializer.serialize_struct("Block", 4)?;
      s.serialize_field("time", &self.time.to_string())?;
      s.serialize_field("meta", &self.meta.to_string())?; // ?? hex?
      s.serialize_field("prev", &u256_to_hex(&self.prev))?;
      s.serialize_field("body", &body_bytes)?;
      s.end()
    }
  }

  impl serde::Serialize for Statement {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
      S: serde::Serializer,
    {
      match self {
        // TODO: serialize sign
        Statement::Fun { name, args, func, init, sign: _ } => {
          let mut s = serializer.serialize_struct_variant("Statement", 0, "Fun", 4)?;
          s.serialize_field("name", &u128_to_name(*name))?;
          s.serialize_field("args", &u128_names_to_strings(args))?;
          s.serialize_field("func", func)?;
          s.serialize_field("init", init)?;
          s.end()
        }
        // TODO: serialize sign
        Statement::Ctr { name, args, sign: _ } => {
          let mut s = serializer.serialize_struct_variant("Statement", 1, "Ctr", 2)?;
          s.serialize_field("name", &u128_to_name(*name))?;
          s.serialize_field("args", &u128_names_to_strings(args))?;
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
          s.serialize_field("name", &u128_to_name(*name))?;
          s.end()
        }
        Term::Dup { nam0, nam1, expr, body } => {
          let mut s = serializer.serialize_struct_variant("Term", 1, "Dup", 4)?;
          s.serialize_field("nam0", &u128_to_name(*nam0))?;
          s.serialize_field("nam1", &u128_to_name(*nam1))?;
          s.serialize_field("expr", &expr)?;
          s.serialize_field("body", &body)?;
          s.end()
        }
        Term::Lam { name, body } => {
          let mut s = serializer.serialize_struct_variant("Term", 2, "Lam", 2)?;
          s.serialize_field("name", &u128_to_name(*name))?;
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
          s.serialize_field("name", &u128_to_name(*name))?;
          s.serialize_field("args", args)?;
          s.end()
        }
        Term::Fun { name, args } => {
          let mut s = serializer.serialize_struct_variant("Term", 5, "Fun", 2)?;
          s.serialize_field("name", &u128_to_name(*name))?;
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
}
