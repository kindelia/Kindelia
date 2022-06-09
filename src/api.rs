#![warn(dead_code)]
#![warn(unused_imports)]
#![warn(non_snake_case)]
#![warn(unused_variables)]
#![warn(clippy::style)]
#![allow(clippy::let_and_return)]
use std::sync::mpsc::SyncSender;

use serde_json::json;
use tokio::sync::oneshot;

use crate::node::Request as NodeRequest;

pub fn api_loop(node_query_sender: SyncSender<NodeRequest>) {
  let runtime = tokio::runtime::Runtime::new().unwrap();

  runtime.block_on(async move {
    use warp::{path, Filter};
    use crate::node::Block;

    let root = warp::path::end().map(|| "UP");

    // TODO: macro to wrap those clones

    let node_query_tx = node_query_sender.clone();
    let get_tick = path!("tick").then(move || {
      let node_query_tx = node_query_tx.clone();
      async move {
        let (tx, rx) = oneshot::channel();
        node_query_tx.clone().send(NodeRequest::GetTick { answer: tx }).unwrap();
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
        node_query_tx.send(NodeRequest::GetBlocks { range: (-10, -1), answer: tx }).unwrap();
        let blocks = rx.await.unwrap();
        blocks
      }
    });

    let get_block = || {
      let node_query_tx = node_query_sender.clone();
      path!("blocks" / u128 / ..).then(move |block_height| {
        let node_query_tx = node_query_tx.clone();
        async move {
          let (tx, rx) = oneshot::channel();
          let node_query_tx = node_query_tx.clone();
          node_query_tx.send(NodeRequest::GetBlock { block_height, answer: tx }).unwrap();
          let block = rx.await.unwrap();
          block
        }
      })
    };

    let get_block_content = get_block().and(path!("content")).map(move |block: Block| {
      let bits = crate::bits::BitVec::from_bytes(&block.body.value);
      let stmts = crate::bits::deserialize_statements(&bits, &mut 0);
      stmts
    });

    // == Functions ==

    // ==

    let get_tick = get_tick;
    let get_blocks = get_blocks.map(ok_json);
    let get_block = get_block().and(path!()).map(ok_json);
    let get_block_content = get_block_content.map(ok_json);

    let app = root.or(get_tick).or(get_blocks).or(get_block).or(get_block_content);

    warp::serve(app).run(([127, 0, 0, 1], 8000)).await;
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
  // pub const TAG: &str = "$";
  use crate::hvm::u128_to_name;
  use crate::hvm::{Rule, Statement, Term};
  use crate::node::Block;

  use serde::ser::{SerializeStruct, SerializeStructVariant};

  fn u128_names_to_strings(names: &[u128]) -> Vec<String> {
    names.iter().copied().map(u128_to_name).collect::<Vec<_>>()
  }

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
