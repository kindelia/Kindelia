// TODO: spam control system

use std::sync::mpsc::SyncSender;

use bit_vec::BitVec;
use serde::de::DeserializeOwned;
use serde::Deserialize;
use tokio::net::TcpListener;
use tokio::sync::oneshot;
use tokio_stream::wrappers::TcpListenerStream;
use warp::body;
use warp::hyper::StatusCode;
use warp::query::query;
use warp::reply::{self, Reply};
use warp::{path, post, Filter};
use warp::{reject, Rejection};

use super::u256_to_hex;
use super::NodeRequest;
use crate::api::HexStatement;
use crate::common::Name;
use crate::hvm::{self, StatementErr, StatementInfo};
use crate::bits::ProtoSerialize;
use crate::net::ProtoAddr;
use crate::util::U256;

// Util
// ====

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
  // let json_body = json!({ "status": "ok", "data": data });
  warp::reply::json(&data)
}

fn json_body<T>() -> impl Filter<Extract = (T,), Error = warp::Rejection> + Clone
where
  T: DeserializeOwned + Send,
{
  // When accepting a body, we want a JSON body
  warp::body::json()
}

// HVM
// ===

fn u128_names_to_strings(names: &[u128]) -> Vec<String> {
  names
    .iter()
    .copied()
    .map(Name::new_unsafe)
    .map(|n| n.to_string())
    .collect::<Vec<_>>()
}

// Protocol Serialization
// ======================

fn bitvec_to_hex(bits: &BitVec) -> String {
  let bytes = bits.to_bytes();
  let hex = hex::encode(bytes);
  hex
}

// Errors
// ======

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

#[derive(Debug)]
pub enum Error {
  NotFound,
}

impl warp::reject::Reject for Error {}

async fn handle_rejection(
  err: Rejection,
) -> Result<impl Reply, std::convert::Infallible> {
  if err.is_not_found() {
    Ok(reply::with_status("NOT_FOUND".into(), StatusCode::NOT_FOUND))
  } else if let Some(e) = err.find::<Error>() {
    match e {
      // On rejection we force this custom branch, not err.is_not_foun()
      Error::NotFound => {
        Ok(reply::with_status("NOT_FOUND".into(), StatusCode::NOT_FOUND))
      }
    }
  } else if let Some(e) = err.find::<InvalidParameter>() {
    let name = e.name.as_ref().map(|n| format!(" '{}'", n)).unwrap_or_default();
    let msg = format!("parameter {} is invalid: {}", name, e.message);
    Ok(reply::with_status(msg, StatusCode::BAD_REQUEST))
  } else {
    eprintln!("HTTP API: unhandled rejection: {:?}", err);
    let err = format!("INTERNAL_SERVER_ERROR: {:?}", err);
    Ok(reply::with_status(
      err,
      StatusCode::INTERNAL_SERVER_ERROR,
    ))
  }
}

pub fn http_api_loop<A: ProtoAddr>(node_query_sender: SyncSender<NodeRequest<A>>) {
  let runtime = tokio::runtime::Runtime::new().unwrap();

  runtime.block_on(async move {
    api_serve(node_query_sender).await;
  });
}

async fn api_serve<A: ProtoAddr>(node_query_sender: SyncSender<NodeRequest<A>>) {
  // async fn ask<T>(
  //   node_query_tx: SyncSender<NodeRequest<A>>,
  //   f: impl FnOnce(oneshot::Sender<T>) -> NodeRequest<A>,
  // ) -> T {
  //   let (tx, rx) = oneshot::channel();
  //   let request = f(tx);
  //   node_query_tx.send(request).unwrap();
  //   let result = rx.await.expect("Node query channel closed");
  //   result
  // }
  
  let ask = <T>|
    node_query_tx: SyncSender<NodeRequest<A>>,
    f: impl FnOnce(oneshot::Sender<T>) -> NodeRequest<A>,
  | -> T {
    let (tx, rx) = oneshot::channel();
    let request = f(tx);
    node_query_tx.send(request).unwrap();
    let result = rx.await.expect("Node query channel closed");
    result
  }

  let root = warp::path::end().map(|| "UP");

  let query_tx = node_query_sender.clone();
  let get_stats = path!("stats").then(move || {
    let query_tx = query_tx.clone();
    async move {
      let stats = ask(query_tx, |tx| NodeRequest::GetStats { tx }).await;
      ok_json(stats)
    }
  });

  // == Blocks ==

  let query_tx = node_query_sender.clone();
  let get_blocks = path!("blocks").then(move || {
    let query_tx = query_tx.clone();
    async move {
      let range = (-10, -1);
      let blocks =
        ask(query_tx, |tx| NodeRequest::GetBlocks { range, tx }).await;
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
            let block =
              ask(query_tx, |tx| NodeRequest::GetBlock { hash, tx }).await;
            Ok(block)
          }
          Err(err) => Err(reject::custom(InvalidParameter::from(format!(
            "Invalid block hash: '{}'",
            err
          )))),
        }
      }
    })
  };

  let query_tx = node_query_sender.clone();
  let get_block_hash = path!("block-hash" / u64).and_then(move |index: u64| {
    let query_tx = query_tx.clone();
    async move {
      let block_hash =
        ask(query_tx, |tx| NodeRequest::GetBlockHash { index, tx }).await;
      match block_hash {
        None => Err(Rejection::from(Error::NotFound)),
        Some(block_hash) => Ok(ok_json(u256_to_hex(&block_hash))),
      }
    }
  });

  let get_block_go = get_block().and(path!()).map(ok_json);

  let blocks_router = get_blocks //
    .or(get_block_go)
    .or(get_block_hash);

  // == Functions ==

  let query_tx = node_query_sender.clone();
  let get_functions = path!("functions").then(move || {
    let query_tx = query_tx.clone();
    async move {
      let functions =
        ask(query_tx, |tx| NodeRequest::GetFunctions { tx }).await;
      let functions: Vec<u128> =
        functions.into_iter().map(|x| x as u128).collect();
      let functions = u128_names_to_strings(&functions);
      ok_json(functions)
    }
  });

  let get_function_base = path!("functions" / String / ..).and_then(
    move |name_txt: String| async move {
      match Name::from_str(&name_txt) {
        Ok(name) => Ok(name),
        Err(err) => {
          let msg = format!("Invalid function name '{}': {}", name_txt, err);
          Err(reject::custom(InvalidParameter::from(msg)))
        }
      }
    },
  );

  let query_tx = node_query_sender.clone();
  let get_function =
    get_function_base.and(path!()).and_then(move |name: Name| {
      let query_tx = query_tx.clone();
      async move {
        let function =
          ask(query_tx, |tx| NodeRequest::GetFunction { name, tx }).await;
        if let Some(function) = function {
          Ok(ok_json(function))
        } else {
          Err(Rejection::from(Error::NotFound))
        }
      }
    });

  #[derive(Deserialize)]
  struct GetStateQuery {
    protocol: Option<bool>, // TODO: base64 ?
  }

  let query_tx = node_query_sender.clone();
  let get_function_state = get_function_base
    .and(path!("state"))
    .and(query::<GetStateQuery>())
    .and_then(move |name: Name, query: GetStateQuery| {
      let query_tx = query_tx.clone();
      async move {
        let state =
          ask(query_tx, |tx| NodeRequest::GetState { name, tx }).await;
        if let Some(state) = state {
          if let Some(true) = query.protocol {
            let encoded = state.proto_serialized();
            let hex = bitvec_to_hex(&encoded);
            Ok(ok_json(hex))
          } else {
            Ok(ok_json(state))
          }
        } else {
          Err(Rejection::from(Error::NotFound))
        }
      }
    });

  let functions_router = get_functions //
    .or(get_function) //
    .or(get_function_state);

  // == Constructors ==

  let get_constructor_base = path!("constructor" / String / ..).and_then(
    move |name_txt: String| async move {
      match Name::from_str(&name_txt) {
        Ok(name) => Ok(name),
        Err(err) => {
          let msg = format!("Invalid function name '{}': {}", name_txt, err);
          Err(reject::custom(InvalidParameter::from(msg)))
        }
      }
    },
  );

  let query_tx = node_query_sender.clone();
  let get_constructor =
    get_constructor_base.and(path!()).and_then(move |name: Name| {
      let query_tx = query_tx.clone();
      async move {
        let constructor =
          ask(query_tx, |tx| NodeRequest::GetConstructor { name, tx }).await;
        if let Some(constructor) = constructor {
          Ok(ok_json(constructor))
        } else {
          Err(Rejection::from(Error::NotFound))
        }
      }
    });

  let constructor_router = get_constructor;

  // == Interact ==

  let interact_base = path!("code" / ..);

  let query_tx = node_query_sender.clone();
  let interact_test =
    post().and(interact_base).and(path!("run")).and(body::bytes()).and_then(
      move |code: warp::hyper::body::Bytes| {
        let query_tx = query_tx.clone();
        async move {
          let code = String::from_utf8(code.to_vec());
          if let Ok(code) = code {
            let res = ask(query_tx, |tx| NodeRequest::TestCode {
              code: code.clone(),
              tx,
            })
            .await;
            Ok(ok_json(res))
          } else {
            Err(reject::custom(InvalidParameter::from(
              "Invalid code".to_string(),
            )))
          }
        }
      },
    );

  let query_tx = node_query_sender.clone();
  let interact_send =
    post().and(interact_base).and(path!("publish")).and(body::bytes()).and_then(
      move |code: warp::hyper::body::Bytes| {
        let query_tx = query_tx.clone();
        async move {
          let code = String::from_utf8(code.to_vec());
          if let Ok(code) = code {
            let res = ask(query_tx, |tx| NodeRequest::PostCode {
              code: code.clone(),
              tx,
            })
            .await;
            match res {
              Ok(res) => Ok(ok_json(res)),
              Err(err) => Err(reject::custom(InvalidParameter::from(err))), // TODO change this type?
            }
          } else {
            Err(reject::custom(InvalidParameter::from(
              "Invalid code".to_string(),
            )))
          }
        }
      },
    );

  let query_tx = node_query_sender.clone();
  let interact_run = post().and(path!("run")).and(json_body()).and_then(
    move |code: Vec<HexStatement>| {
      let query_tx = query_tx.clone();
      async move {
        let code: Vec<hvm::Statement> =
          code.into_iter().map(|x| x.into()).collect();
        let results = ask(query_tx, |tx| NodeRequest::Run { code, tx }).await;
        // TODO: resulte type will be different
        let result: Result<Vec<StatementInfo>, StatementErr> =
          results.into_iter().collect();
        match result {
          Ok(res) => Ok(ok_json(res)),
          Err(err) => Err(reject::custom(InvalidParameter::from(format!(
            "failed to run statement: {}",
            err.err
          )))), // TODO: create specific error
        }
      }
    },
  );

  let query_tx = node_query_sender.clone();
  let interact_publish = post().and(path!("publish")).and(json_body()).then(
    move |code: Vec<HexStatement>| {
      let query_tx = query_tx.clone();
      async move {
        let code: Vec<hvm::Statement> =
          code.into_iter().map(|x| x.into()).collect();
        let results =
          ask(query_tx, |tx| NodeRequest::Publish { code, tx }).await;
        let result: Vec<Result<(), ()>> = results.into_iter().collect();
        ok_json(result)
      }
    },
  );

  let interact_router =
    interact_test.or(interact_send).or(interact_run).or(interact_publish);

  // == Reg ==

  let get_reg_base =
    path!("reg" / String / ..).and_then(move |name_txt: String| async move {
      match Name::from_str(&name_txt) {
        Ok(name) => Ok(name),
        Err(err) => {
          let msg = format!("Invalid constructor name '{}': {}", name_txt, err);
          Err(reject::custom(InvalidParameter::from(msg)))
        }
      }
    });

  let query_tx = node_query_sender.clone();
  let get_reg = get_reg_base.and(path!()).and_then(move |name: Name| {
    let query_tx = query_tx.clone();
    async move {
      let reg = ask(query_tx, |tx| NodeRequest::GetReg { name, tx }).await;
      if let Some(reg) = reg {
        Ok(ok_json(reg))
      } else {
        Err(Rejection::from(Error::NotFound))
      }
    }
  });

  let reg_router = get_reg;

  // == Peers ==

  // TODO

  // let get_peers_base = path!("peers" / ..);

  // let query_tx = node_query_sender.clone();
  // let get_peers = get_peers_base.and(path!()).then(move || {
  //   let query_tx = query_tx.clone();
  //   async move {
  //     let peers_store =
  //       ask(query_tx, |tx| NodeRequest::GetPeers { tx, all: false }).await;
  //     ok_json(peers_store)
  //   }
  // });

  // let query_tx = node_query_sender.clone();
  // let get_all_peers = get_peers_base.and(path!("all")).then(move || {
  //   let query_tx = query_tx.clone();
  //   async move {
  //     let peers_store =
  //       ask(query_tx, |tx| NodeRequest::GetPeers { tx, all: true }).await;
  //     ok_json(peers_store)
  //   }
  // });

  // let peers_router = get_peers.or(get_all_peers);

  // ==

  let app = root
    .or(get_stats)
    .or(blocks_router)
    .or(functions_router)
    .or(interact_router)
    // .or(peers_router)
    .or(constructor_router)
    .or(reg_router);
  let app = app.recover(handle_rejection);
  let app = app.map(|reply| {
    warp::reply::with_header(reply, "Access-Control-Allow-Origin", "*")
  });

  let listener_v4 = TcpListener::bind("0.0.0.0:8000").await.unwrap();
  // let listener_v6 = TcpListener::bind("[::]:8000").await.unwrap();
  let listener = TcpListenerStream::new(listener_v4)
    // .merge(TcpListenerStream::new(listener_v6))
    ;

  warp::serve(app).run_incoming(listener).await;
}
