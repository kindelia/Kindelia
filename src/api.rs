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
use tokio_stream::{wrappers::TcpListenerStream, StreamExt};
use warp::reject;
use warp::{path, Filter};

use crate::hvm::{name_to_u128, u128_to_name};
use crate::node::Block;

use crate::node::Request as NodeRequest;

fn u128_names_to_strings(names: &[u128]) -> Vec<String> {
  names.iter().copied().map(u128_to_name).collect::<Vec<_>>()
}

pub fn api_loop(node_query_sender: SyncSender<NodeRequest>) {
  let runtime = tokio::runtime::Runtime::new().unwrap();

  runtime.block_on(async move {
    // // Custom rejection handler that maps rejections into responses.
    // // https://docs.rs/warp/latest/warp/reject/index.html
    // use warp::hyper::StatusCode;
    // use warp::reject::{self, Rejection};
    // use warp::reply::{self, Reply};
    // async fn handle_rejection(err: Rejection) -> Result<impl Reply, std::convert::Infallible> {
    //   if err.is_not_found() {
    //     Ok(reply::with_status("NOT_FOUND", StatusCode::NOT_FOUND))
    //   } else {
    //     eprintln!("unhandled rejection: {:?}", err);
    //     Ok(reply::with_status("INTERNAL_SERVER_ERROR", StatusCode::INTERNAL_SERVER_ERROR))
    //   }
    // }

    let root = warp::path::end().map(|| "UP");

    // TODO: macro to wrap those clones

    let node_query_tx = node_query_sender.clone();
    let get_tick = path!("tick").then(move || {
      let node_query_tx = node_query_tx.clone();
      async move {
        let (tx, rx) = oneshot::channel();
        node_query_tx.send(NodeRequest::GetTick { tx }).unwrap();
        let tick = rx.await.unwrap();
        ok_json(format!("Tick: {}", tick))
      }
    });

    // == Blocks ==

    let node_query_tx = node_query_sender.clone();
    let get_blocks = path!("blocks").then(move || {
      let node_query_tx = node_query_tx.clone();
      async move {
        let (tx, rx) = oneshot::channel();
        node_query_tx.send(NodeRequest::GetBlocks { range: (-10, -1), tx }).unwrap();
        let blocks = rx.await.unwrap();
        ok_json(blocks)
      }
    });

    let get_block = || {
      let node_query_tx = node_query_sender.clone();
      path!("blocks" / u128 / ..).then(move |block_height| {
        let node_query_tx = node_query_tx.clone();
        async move {
          let (tx, rx) = oneshot::channel();
          node_query_tx.send(NodeRequest::GetBlock { block_height, tx }).unwrap();
          let block = rx.await.unwrap();
          block
        }
      })
    };

    let get_block_go = get_block().and(path!()).map(ok_json);

    let get_block_content = get_block().and(path!("content")).map(move |block: Block| {
      let bits = crate::bits::BitVec::from_bytes(&block.body.value);
      let stmts = crate::bits::deserialize_statements(&bits, &mut 0);
      ok_json(stmts)
    });

    let blocks_router = get_blocks //
      .or(get_block_go) //
      .or(get_block_content);

    // == Functions ==

    let node_query_tx = node_query_sender.clone();
    let get_functions = path!("functions").then(move || {
      let node_query_tx = node_query_tx.clone();
      async move {
        let (tx, rx) = oneshot::channel();
        node_query_tx.send(NodeRequest::GetFunctions { tx }).unwrap();
        let functions = rx.await.unwrap();
        ok_json(u128_names_to_strings(&functions))
      }
    });

    let node_query_tx = node_query_sender.clone();
    let _get_function = path!("functions" / u128).then(move |id| {
      let node_query_tx = node_query_tx.clone();
      async move {
        let (tx, rx) = oneshot::channel();
        node_query_tx.send(NodeRequest::GetFunction { name: id, tx }).unwrap();
        let function = rx.await.unwrap();
        ok_json(function)
      }
    });

    // TODO merge code redundancy with above route
    let node_query_tx = node_query_sender.clone();
    let get_function_state = path!("functions" / String / "state").and_then(move |id: String| {
      let node_query_tx = node_query_tx.clone();
      async move {
        let (tx, rx) = oneshot::channel();
        let node_query_tx = node_query_tx.clone();
        let id = name_to_u128(&id);
        node_query_tx.send(NodeRequest::GetState { name: id, tx }).unwrap();
        let state = rx.await.unwrap();
        if let Some(state) = state {
          Ok(ok_json(state))
        } else {
          Err(reject::not_found())
        }
      }
    });

    let functions_router = get_functions //
      // .or(get_function) //
      .or(get_function_state);

    // ==

    let app = root.or(get_tick).or(blocks_router).or(functions_router);

    let listener_v4 = TcpListener::bind("127.0.0.1:8000").await.unwrap();
    let listener_v6 = TcpListener::bind("[::1]:8000").await.unwrap();
    let incoming_connections =
      TcpListenerStream::new(listener_v4).merge(TcpListenerStream::new(listener_v6));

    warp::serve(app).run_incoming(incoming_connections).await;
    // .recover(handle_rejection)
  });
}

fn ok_json<T>(data: T) -> warp::reply::Json
where
  T: serde::Serialize,
{
  let json_body = json!({ "status": "ok", "data": data });
  warp::reply::json(&json_body)
}

mod ser {
  use super::u128_names_to_strings;
  use crate::hvm::u128_to_name;
  use crate::hvm::{Rule, Statement, Term};
  use crate::node::Block;
  use serde::ser::{SerializeStruct, SerializeStructVariant};

  impl serde::Serialize for Block {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
      S: serde::Serializer,
    {
      let body = self.body.value;
      let body_bytes = body.into_iter().collect::<Vec<_>>();
      let mut s = serializer.serialize_struct("Block", 4)?;
      s.serialize_field("time", &self.time.to_string())?;
      s.serialize_field("rand", &self.rand.to_string())?;
      s.serialize_field("prev", &self.prev.to_string())?;
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
        Statement::Fun { name, args, func, init } => {
          let mut s = serializer.serialize_struct_variant("Statement", 0, "Fun", 4)?;
          s.serialize_field("name", &u128_to_name(*name))?;
          s.serialize_field("args", &u128_names_to_strings(args))?;
          s.serialize_field("args", func)?;
          s.serialize_field("init", init)?;
          s.end()
        }
        Statement::Ctr { name, args } => {
          let mut s = serializer.serialize_struct_variant("Statement", 1, "Ctr", 2)?;
          s.serialize_field("name", &u128_to_name(*name))?;
          s.serialize_field("args", &u128_names_to_strings(args))?;
          s.end()
        }
        // TODO: serialize 'with'
        Statement::Run { expr, sign: _ } => {
          let mut s = serializer.serialize_struct_variant("Statement", 2, "Run", 1)?;
          s.serialize_field("body", expr)?;
          s.end()
        }
      }
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
