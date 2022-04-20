use crate::api::{self, ClientCommEdge, NodeAns, NodeAsk, NodeInfo};

pub struct HeadlessFrontend {}

impl HeadlessFrontend {
  pub fn new() -> HeadlessFrontend {
    HeadlessFrontend {}
  }
}

impl api::Frontend for HeadlessFrontend {
  fn get_tasks(
    &self,
    comm_edge: ClientCommEdge,
  ) -> Vec<Box<dyn FnOnce() + Send + 'static>> {
    let io_task = move || {
      io_loop(comm_edge);
    };
    vec![Box::new(io_task)]
  }
}

fn io_loop(comm: ClientCommEdge) {
  let (tx, rx) = comm;

  loop {
    tx.send(NodeAsk::Info {
      max_last_blocks: Some(0),
    })
    .unwrap();
    match rx.recv() {
      Ok(msg) => match msg {
        NodeAns::NodeInfo(info) => {
          display(&info);
        }
      },
      Err(err) => panic!("Error: {}", err),
    }
    std::thread::sleep(std::time::Duration::from_millis(1000));
  }
}

fn display(info: &NodeInfo) {
  println!("{{");

  let time = crate::algorithms::get_time();

  println!(r#"  "time": "{}","#, time);
  println!(r#"  "num_peers: {},"#, info.num_peers);
  println!(r#"  "tip_hash": "{}","#, info.tip_hash);
  println!(r#"  "tip_height": {}"#, info.height);

  println!("}}");
}
