use std::str::FromStr;
use std::sync::mpsc::SyncSender;

use bit_vec::BitVec;
use serde::de::DeserializeOwned;
use serde::Deserialize;
use serde_json::json;
use tokio::net::TcpListener;
use tokio_stream::wrappers::TcpListenerStream;
use warp::body;
use warp::hyper::StatusCode;
use warp::query::query;
use warp::reply::{self, Reply};
use warp::{path, post, Filter};
use warp::{reject, Rejection};

use kindelia_common::{Name, U256};
use kindelia_core::api::{
  u256_to_hex, HexStatement, NodeRequest, PublishError, ReqAnsRecv,
};
use kindelia_core::bits::ProtoSerialize;
use kindelia_core::config::ApiConfig;
use kindelia_core::net::ProtoComm;
use kindelia_core::runtime::{StatementErr, StatementInfo};
use kindelia_lang::ast;

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

fn err_json(message: &str) -> warp::reply::Json {
  let json_body = json!({ "message": message });
  warp::reply::json(&json_body)
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
  hex::encode(bytes)
}

// Errors
// ======

#[derive(Debug)]
pub struct NotFound {
  message: String,
}

impl From<String> for NotFound {
  fn from(message: String) -> Self {
    Self { message }
  }
}

impl warp::reject::Reject for NotFound {}

#[derive(Debug)]
struct InvalidParameter {
  name: Option<String>,
  message: String,
}

impl From<String> for InvalidParameter {
  fn from(message: String) -> Self {
    Self { name: None, message }
  }
}

impl reject::Reject for InvalidParameter {}

#[derive(Debug)]
struct TermTooBig {
  message: String,
}

impl From<String> for TermTooBig {
  fn from(message: String) -> Self {
    Self { message }
  }
}

impl reject::Reject for TermTooBig {}

// API
// ===

async fn handle_rejection(
  err: Rejection,
) -> Result<impl Reply, std::convert::Infallible> {
  if err.is_not_found() {
    Ok(reply::with_status(err_json("NOT_FOUND"), StatusCode::NOT_FOUND))
  } else if let Some(e) = err.find::<NotFound>() {
    Ok(reply::with_status(err_json(&e.message), StatusCode::NOT_FOUND))
  } else if let Some(e) = err.find::<TermTooBig>() {
    Ok(reply::with_status(err_json(&e.message), StatusCode::IM_A_TEAPOT))
  } else if let Some(e) = err.find::<InvalidParameter>() {
    let name = e.name.as_ref().map(|n| format!(" '{}'", n)).unwrap_or_default();
    let msg = format!("Parameter{} is invalid: {}", name, e.message);
    Ok(reply::with_status(err_json(&msg), StatusCode::BAD_REQUEST))
  } else {
    eprintln!("HTTP API: unhandled rejection: {:?}", err);
    let msg = format!("INTERNAL_SERVER_ERROR: {:?}", err);
    Ok(reply::with_status(err_json(&msg), StatusCode::INTERNAL_SERVER_ERROR))
  }
}

pub async fn api_serve<'a, C: ProtoComm + 'static>(
  node_query_sender: SyncSender<NodeRequest<C>>,
  api_config: ApiConfig,
  ws_router: impl warp::Filter<Extract = (impl Reply,), Error = Rejection>
    + Clone
    + Send
    + Sync
    + 'static,
) {
  async fn ask<T, C: ProtoComm>(
    node_query_tx: SyncSender<NodeRequest<C>>,
    req: (NodeRequest<C>, ReqAnsRecv<T>),
  ) -> T {
    let (request, rx) = req;
    node_query_tx.send(request).unwrap();
    rx.await.expect("Node query channel closed")
  }

  let root = warp::path::end().map(|| "UP");

  let query_tx = node_query_sender.clone();
  let get_stats = path!("stats").then(move || {
    let query_tx = query_tx.clone();
    async move {
      let stats = ask(query_tx, NodeRequest::get_stats()).await;
      ok_json(stats)
    }
  });

  // == Blocks ==

  let query_tx = node_query_sender.clone();
  let get_blocks = path!("blocks").then(move || {
    let query_tx = query_tx.clone();
    async move {
      let range = (-10, -1);
      let blocks = ask(query_tx, NodeRequest::get_blocks(range)).await;
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
            let block = ask(query_tx, NodeRequest::get_block(hash)).await;
            match block {
              Some(block) => Ok(block),
              None => {
                let message = format!("Block '{}' not found", hash_hex);
                Err(warp::reject::custom(NotFound::from(message)))
              }
            }
          }
          Err(err) => {
            let msg = format!("Invalid block hash: {}", err);
            Err(reject::custom(InvalidParameter::from(msg)))
          }
        }
      }
    })
  };

  let query_tx = node_query_sender.clone();
  let get_block_hash = path!("block-hash" / u64).and_then(move |index: u64| {
    let query_tx = query_tx.clone();
    async move {
      let block_hash = ask(query_tx, NodeRequest::get_block_hash(index)).await;
      match block_hash {
        None => {
          let message = format!("Block with index {} not found", index);
          Err(Rejection::from(NotFound::from(message)))
        }
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
      let functions = ask(query_tx, NodeRequest::get_functions()).await;
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
        let function = ask(query_tx, NodeRequest::get_function(name)).await;
        if let Some(function) = function {
          Ok(ok_json(function))
        } else {
          let message = format!("Function '{}' not found", name);
          Err(Rejection::from(NotFound::from(message)))
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
        let state = ask(query_tx, NodeRequest::get_state(name)).await;
        if let Some(state) = state {
          if let Some(true) = query.protocol {
            let encoded = state.proto_serialized();
            let hex = bitvec_to_hex(&encoded);
            Ok(ok_json(hex))
          } else {
            Ok(ok_json(state))
          }
        } else {
          // TODO: better error handling when reading state
          let message =
            format!("State for function '{}' is too big or is missing.", name);
          Err(Rejection::from(TermTooBig::from(message)))
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
        let ctr = ask(query_tx, NodeRequest::get_constructor(name)).await;
        if let Some(ctr) = ctr {
          Ok(ok_json(ctr))
        } else {
          let msg = format!("Constructor '{}' not found", name);
          Err(Rejection::from(NotFound::from(msg)))
        }
      }
    });

  let constructor_router = get_constructor;

  // == Interact ==

  let interact_code_base = path!("code" / ..);

  let query_tx = node_query_sender.clone();
  let interact_code_run = post()
    .and(interact_code_base)
    .and(path!("run"))
    .and(body::bytes())
    .and_then(move |code: warp::hyper::body::Bytes| {
      let query_tx = query_tx.clone();
      async move {
        let code = String::from_utf8(code.to_vec());
        if let Ok(code) = code {
          let res = ask(query_tx, NodeRequest::test_code(code)).await;
          Ok(ok_json(res))
        } else {
          Err(reject::custom(InvalidParameter::from(
            "Invalid code".to_string(),
          )))
        }
      }
    });

  let query_tx = node_query_sender.clone();
  let interact_code_publish = post()
    .and(interact_code_base)
    .and(path!("publish"))
    .and(body::bytes())
    .and_then(move |code: warp::hyper::body::Bytes| {
      let query_tx = query_tx.clone();
      async move {
        let code = String::from_utf8(code.to_vec());
        if let Ok(code) = code {
          let res = ask(query_tx, NodeRequest::post_code(code)).await;
          match res {
            Ok(res) => {
              let res: Vec<_> = res
                .into_iter()
                .map(|r| r.map_err(|err| err.to_string()))
                .collect();
              Ok(ok_json(res))
            }
            Err(err) => Err(reject::custom(InvalidParameter::from(err))), // TODO change this type?
          }
        } else {
          Err(reject::custom(InvalidParameter::from(
            "Invalid code".to_string(),
          )))
        }
      }
    });

  let query_tx = node_query_sender.clone();
  let interact_run = post().and(path!("run")).and(json_body()).and_then(
    move |code: Vec<HexStatement>| {
      let query_tx = query_tx.clone();
      async move {
        let code: Vec<ast::Statement> =
          code.into_iter().map(|x| x.into()).collect();
        let results = ask(query_tx, NodeRequest::run(code)).await;
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
        let code: Vec<ast::Statement> =
          code.into_iter().map(|x| x.into()).collect();
        let results = ask(query_tx, NodeRequest::publish(code)).await;
        let result: Vec<Result<(), PublishError>> =
          results.into_iter().collect();
        ok_json(result)
      }
    },
  );

  let interact_router = interact_code_run
    .or(interact_code_publish)
    .or(interact_run)
    .or(interact_publish);

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
      let reg = ask(query_tx, NodeRequest::get_reg(name)).await;
      if let Some(reg) = reg {
        Ok(ok_json(reg))
      } else {
        let msg = format!("Register for name '{}' not found", name);
        Err(Rejection::from(NotFound::from(msg)))
      }
    }
  });

  let reg_router = get_reg;

  // == Peers ==

  let get_peers_base = path!("peers" / ..);

  let query_tx = node_query_sender.clone();
  let get_peers = get_peers_base.and(path!()).then(move || {
    let query_tx = query_tx.clone();
    async move {
      let peers = ask(query_tx, NodeRequest::get_peers(false)).await;
      ok_json(peers)
    }
  });

  let query_tx = node_query_sender.clone();
  let get_all_peers = get_peers_base.and(path!("all")).then(move || {
    let query_tx = query_tx.clone();
    async move {
      let peers = ask(query_tx, NodeRequest::get_peers(true)).await;
      ok_json(peers)
    }
  });

  let peers_router = get_peers.or(get_all_peers);

  // == Favicon ==

  let no_favicon = path!("favicon.ico").map(warp::reply).map(|reply| {
    warp::reply::with_status(reply, warp::http::StatusCode::NO_CONTENT)
  });

  // == Events ==

  let events_router = path!("events").and(ws_router);

  // == Server ==

  let app = root
    .or(no_favicon)
    .or(get_stats)
    .or(blocks_router)
    .or(functions_router)
    .or(interact_router)
    .or(peers_router)
    .or(constructor_router)
    .or(reg_router)
    .or(events_router);

  let app = app.recover(handle_rejection);
  let app = app.map(|reply| {
    warp::reply::with_header(reply, "Access-Control-Allow-Origin", "*")
  });

  let listener_v4 =
    TcpListener::bind(format!("0.0.0.0:{}", api_config.port)).await.unwrap();
  // let listener_v6 = TcpListener::bind("[::]:8000").await.unwrap();
  let listener = TcpListenerStream::new(listener_v4)
    // .merge(TcpListenerStream::new(listener_v6))
    ;

  warp::serve(app).run_incoming(listener).await;
}
