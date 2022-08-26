
use std::sync::mpsc::SyncSender;

use serde_json::json;
use tokio::net::TcpListener;
use tokio::sync::oneshot;
use tokio_stream::wrappers::TcpListenerStream;
use warp::hyper::StatusCode;
use warp::reply::{self, Reply};
use warp::{body, path, post, Filter};
use warp::{reject, Rejection};

use super::NodeRequest;
use crate::hvm::{self, Name, name_to_u128};
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

fn u128_names_to_strings(names: &[u128]) -> Vec<String> {
  names.iter().copied().map(hvm::u128_to_name).collect::<Vec<_>>()
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

pub fn http_api_loop(node_query_sender: SyncSender<NodeRequest>) {
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
      match name_to_u128(&name_txt) {
        Ok(name) => Ok(name),
        Err(err) => {
          let msg = format!("Invalid function name '{}': {}", name_txt, err);
          Err(reject::custom(InvalidParameter::from(msg)))
        }
      }
    });

  let query_tx = node_query_sender.clone();
  let get_function = get_function_base.and(path!()).and_then(move |name: Name| {
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
  let get_function_state = get_function_base.and(path!("state")).and_then(move |name: Name| {
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

  // == Interact ==
  let interact_base = path!("code" / ..);

  let query_tx = node_query_sender.clone();
  let interact_test = post().and(interact_base).and(path!("test")).and(body::bytes()).and_then(
    move |code: warp::hyper::body::Bytes| {
      let query_tx = query_tx.clone();
      async move {
        let code = String::from_utf8(code.to_vec());
        if let Ok(code) = code {
          let res = ask(query_tx, |tx| NodeRequest::TestCode { code: code.clone(), tx }).await;
          Ok(ok_json(res))
        } else {
          Err(reject::custom(InvalidParameter::from("Invalid code".to_string())))
        }
      }
    },
  );

  let query_tx = node_query_sender.clone();
  let interact_send = post().and(interact_base).and(path!("send")).and(body::bytes()).and_then(
    move |code: warp::hyper::body::Bytes| {
      let query_tx = query_tx.clone();
      async move {
        let code = String::from_utf8(code.to_vec());
        if let Ok(code) = code {
          let res = ask(query_tx, |tx| NodeRequest::PostCode { code: code.clone(), tx }).await;
          match res {
            Ok(res) => Ok(ok_json(res)),
            Err(err) => Err(reject::custom(InvalidParameter::from(err))), // TODO change this type?
          }
        } else {
          Err(reject::custom(InvalidParameter::from("Invalid code".to_string())))
        }
      }
    },
  );

  let query_tx = node_query_sender.clone();
  let interact_run = path!("run" / String).and_then(move |hex: String| {
    let query_tx = query_tx.clone();
    async move {
      let result = ask(query_tx, |tx| NodeRequest::Run { hex: hex.clone(), tx }).await;
      match result {
        Ok(res) => Ok(ok_json(format!("Result: {:?}", res))),
        Err(_err) => {
          Err(reject::custom(InvalidParameter::from("Failed to execute statement".to_string())))
        } // TODO: create specific error
      }
    }
  });

  let interact_router = interact_test.or(interact_send).or(interact_run);

  // ==

  let app = root.or(get_tick).or(blocks_router).or(functions_router).or(interact_router);
  let app = app.recover(handle_rejection);
  let app = app.map(|reply| warp::reply::with_header(reply, "Access-Control-Allow-Origin", "*"));

  let listener_v4 = TcpListener::bind("0.0.0.0:8000").await.unwrap();
  // let listener_v6 = TcpListener::bind("[::]:8000").await.unwrap();
  let listener = TcpListenerStream::new(listener_v4)
    // .merge(TcpListenerStream::new(listener_v6))
    ;

  warp::serve(app).run_incoming(listener).await;
}
