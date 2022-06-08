#![warn(dead_code)]
#![warn(unused_imports)]
#![warn(non_snake_case)]
#![warn(unused_variables)]
#![warn(clippy::style)]
#![allow(clippy::let_and_return)]
use std::sync::mpsc::SyncSender;

use futures::sync::oneshot;
use futures::Future;
use serde_json::json;

use crate::node::*;

pub fn api_loop(node_query_tx: SyncSender<Request>) {
  let runtime = tokio::runtime::Runtime::new().unwrap();

  runtime.block_on(async move {
    use warp::path;
    use warp::Filter;

    let root = warp::path::end().map(|| "UP");

    let node_query_tx_1 = node_query_tx.clone();
    let node_query_tx_2 = node_query_tx.clone();

    let get_tick = path!("tick").map(move || {
      let (rx, tx) = oneshot::channel();
      node_query_tx_1.send(Request::GetTick { answer: rx }).unwrap();

      // FIXME: this .wait() call blocks. Since the node may take some time to respond, I believe
      // this will greatly impact warp's performance, decreasing how many requests per second the
      // node can handle. If this is correct, we should use an async oneshot channel instead.

      let tick = tx.wait().unwrap();

      format!("Tick: {}", tick)
    });

    let get_block = || {
      let node_query_tx = node_query_tx_2.clone();
      path!("blocks" / u128 / ..).map(move |block_height| {
        let (tx, rx) = oneshot::channel();
        node_query_tx.send(Request::GetBlock { block_height, answer: tx }).unwrap();
        let block = rx.wait().unwrap();
        block
      })
    };

    let get_block_content = get_block().and(path!("content")).map(move |block: Block| {
      let bits = crate::bits::BitVec::from_bytes(&block.body.value);
      let stmts = crate::bits::deserialize_statements(&bits, &mut 0);
      // let json = json::JsonValue::from(stmts);
      // format!("{}", json)
      stmts
    });

    let get_block = get_block().and(path!()).map(ok_json);
    let get_block_content = get_block_content.map(ok_json);

    // println!("{:?}", get_block_content);

    let app = root.or(get_tick).or(get_block).or(get_block_content);

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
  pub const TAG: &str = "$";
  use crate::hvm::u128_to_name;
  use crate::hvm::{Statement, Term};
  use crate::node::{self, Block};
  use json::object;
  use json::JsonValue;

  use serde::ser::SerializeStruct;

  impl Into<JsonValue> for Statement {
    fn into(self) -> JsonValue {
      match self {
        Statement::Fun { name, args, func, init } => object! {
          TAG => "Fun",
          "name" => u128_to_name(name),
          "args" => names_to_json(args),
          "func" => rules_to_json(func),
          "init" => init,
        },
        Statement::Ctr { name, args } => object! {
          TAG => "Ctr",
          "name" => u128_to_name(name),
          "args" => names_to_json(args),
        },
        Statement::Run { expr } => object! {
          TAG => "Run",
          "body" => expr,
        },
      }
    }
  }

  fn names_to_json(names: Vec<u128>) -> JsonValue {
    names.iter().copied().map(u128_to_name).collect::<Vec<_>>().into()
  }

  fn rules_to_json(rules: Vec<(Term, Term)>) -> JsonValue {
    let rules = rules.into_iter();
    let mut rules_json = JsonValue::new_array();
    for (lhs, rhs) in rules {
      rules_json
        .push(object! {
          "lhs" => lhs,
          "rhs" => rhs,
        })
        .expect("Not an array");
    }
    rules_json
  }

  pub fn _block_to_json(block: &node::Block) -> JsonValue {
    let body = block.body.value;
    let bits = crate::bits::BitVec::from_bytes(&block.body.value);
    let stmts = crate::bits::deserialize_statements(&bits, &mut 0);
    let body_bytes = body.into_iter().collect::<Vec<_>>();
    object! {
      TAG => "Block",
      "time" => block.time.to_string(),
      "rand" => block.rand.to_string(),
      "prev" => block.prev.to_string(),
      "body" => body_bytes,
      "content" => stmts,
    }
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

  impl From<Term> for JsonValue {
    fn from(val: Term) -> Self {
      match val {
        Term::Var { name } => object! {
          TAG => "Var",
          "name" => u128_to_name(name),
        },
        Term::Dup { nam0, nam1, expr, body } => object! {
          TAG => "Dup",
          "nam0" => u128_to_name(nam0),
          "nam1" => u128_to_name(nam1),
          "expr" => *expr,
          "body" => *body,
        },
        Term::Lam { name, body } => object! {
          TAG => "Lam",
          "name" => u128_to_name(name),
          "body" => *body,
        },
        Term::App { func, argm } => object! {
          TAG => "App",
          "func" => *func,
          "argm" => *argm,
        },
        Term::Ctr { name, args } => object! {
          TAG => "Ctr",
          "name" => u128_to_name(name),
          "args" => args,
        },
        Term::Fun { name, args } => object! {
          TAG => "Fun",
          "name" => u128_to_name(name),
          "args" => args,
        },
        Term::Num { numb } => object! {
          TAG => "Num",
          "numb" => numb.to_string(),
        },
        Term::Op2 { oper, val0, val1 } => object! {
          TAG => "Op2",
          "oper" => u128_to_name(oper),
          "val0" => *val0,
          "val1" => *val1,
        },
      }
    }
  }

  // impl serde::Serialize for Term {
  //   fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  //   where
  //     S: serde::Serializer,
  //   {
  //     // let mut s = serializer.serialize_struct("Person", 3)?;
  //     let mut s = serializer.serialize_enum("Term");
  //     // s.serialize_field("name", &self.name)?;
  //     // s.serialize_field("age", &self.age)?;
  //     // s.serialize_field("phones", &self.phones)?;
  //     s.end()
  //   }
  // }

  // pub fn code_to_json(code: Vec<Statement>) -> JsonValue {
  //   let mut code_json = JsonValue::new_array();
  //   for stmt in code {
  //     let stmt: JsonValue = stmt.into();
  //     code_json.push(stmt).expect("Not an array");
  //   }
  //   code_json
  // }
}
