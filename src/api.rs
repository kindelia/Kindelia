use futures::Future;
use futures::sync::oneshot;
use std::sync::mpsc::{SyncSender, Receiver};
use std::thread;

use crate::node::*;

pub fn api_loop(node_query_sender: SyncSender<Request>) {
  let runtime = tokio::runtime::Runtime::new().unwrap();
  runtime.block_on(async move {
    use warp::Filter;
    let hello = warp::path!("get_tick").map(move || {
      let (send_answer, receive_answer) = oneshot::channel();
      node_query_sender.send(Request::GetTick { answer: send_answer }).unwrap();
      // FIXME: this .wait() call blocks. Since the node may take some time to respond, I believe
      // this will greatly impact warp's performance, decreasing how many requests per second the
      // node can handle. If this is correct, we should use an async oneshot channel instead.
      return format!("Tick: {}", receive_answer.wait().unwrap());
    });
    warp::serve(hello).run(([127, 0, 0, 1], 8000)).await;
  });
}
