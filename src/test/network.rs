// use std::time::Duration;

// use turmoil::Builder;

// #[derive(Debug)]
// struct Echo(String);

// impl turmoil::Message for Echo {
//   fn write_json(&self, dst: &mut dyn std::io::Write) {
//     unimplemented!()
//   }
// }

// #[test]
// fn simulation() {
//   let mut sim = Builder::new()
//     .simulation_duration(Duration::from_secs(600))
//     // .tick_duration(Duration::from_secs(1))
//     .max_message_latency(Duration::from_secs(60))
//     .build();
//   // .build_with_rng(Box::new(rand::rngs::OsRng::default()));

//   // register a host
//   sim.register("vasco1", |server| async move {
//     loop {
//       let (msg, src) = server.recv().await;
//       let msg: Echo = msg;
//       println!("vasco");
//       server.send(src, msg);
//     }
//   });

//   let clients: Vec<_> =
//     (0..10).map(|i| sim.client(format!("vasco{}", i + 2))).collect();

//   // register a client (this is the test code)
//   sim.run_until(async {
//     for client in clients {
//       println!("rapaz");
//       client.send("vasco1", Echo("hello, server!".to_string()));
//       let (echo, _) = client.recv().await;
//       println!("{}", echo.0);
//       assert_eq!("hello, server!", echo.0);
//     }
//   });
// }

use std::{
  collections::HashMap,
  path::PathBuf,
  sync::mpsc::{self, Receiver},
  thread,
};
use tokio::sync::oneshot;

use std::sync::mpsc::Sender;

use crate::{
  bits::{self, deserialize_number, serialize_number},
  net,
  node::{self, Message, MinerCommunication},
  util::u256,
};

impl bits::ProtoSerialize for u32 {
  fn serialize(&self, bits: &mut bit_vec::BitVec, names: &mut bits::Names) {
    serialize_number(&u256(*self as u128), bits, names);
  }
  fn deserialize(
    bits: &bit_vec::BitVec,
    index: &mut u128,
    names: &mut bits::Names,
  ) -> Option<Self> {
    deserialize_number(bits, index, names).map(|n| n.low_u32())
  }
}

impl net::ProtoCommAddress for u32 {}

struct SocketMock {
  tx: Sender<RouterMessage>,
  rx: Receiver<RouterMessage>,
  addr: u32,
}

impl SocketMock {
  fn get_addr(&self) -> u32 {
    self.addr
  }
}

impl net::ProtoComm for SocketMock {
  type Address = u32;
  fn recv(&mut self) -> Vec<(Self::Address, node::Message<Self::Address>)> {
    let mut messages = Vec::new();
    while let Ok(RouterMessage(to_addr, from_addr, msg)) = self.rx.try_recv() {
      // println!(
      //   "socket {}: received message from router, from addr {}",
      //   self.addr, from_addr
      // );
      messages.push((from_addr, msg))
    }
    messages
  }

  fn send(
    &mut self,
    addresses: Vec<Self::Address>,
    message: &node::Message<Self::Address>,
  ) {
    for addr in addresses {
      // println!(
      //   "socket {}: sending message to router, to addr {}",
      //   self.addr, addr
      // );
      self.tx.send(RouterMessage(addr, self.addr, message.to_owned())).unwrap()
      // addr.tx.send(message).unwrap()
    }
  }
}

#[derive(Debug)]
struct RouterMessage(u32, u32, node::Message<u32>);

struct RouterMock {
  sockets: HashMap<u32, Sender<RouterMessage>>,
  rx: Receiver<RouterMessage>,
  tx: Sender<RouterMessage>,
}

impl RouterMock {
  fn new() -> RouterMock {
    let (tx, rx) = mpsc::channel();
    RouterMock { sockets: HashMap::new(), rx, tx }
  }

  fn ask_sock(&mut self) -> SocketMock {
    let addr = self.sockets.len() as u32;
    let (tx, rx) = mpsc::channel();

    let socket_mock = SocketMock { addr, rx, tx: self.tx.clone() };
    self.sockets.insert(addr, tx);
    socket_mock
  }

  fn send(&self, router_message: &RouterMessage) {
    let RouterMessage(to_addr, from_addr, msg) = router_message;
    // println!("router sending: from {} to {}", from_addr, to_addr);
    let tx = self.sockets.get(&to_addr).unwrap();
    tx.send(RouterMessage(*to_addr, *from_addr, msg.to_owned())).unwrap()
  }
}

#[test]
#[ignore]
fn network() {
  let mut router_mock = RouterMock::new();

  let socket_mock1 = router_mock.ask_sock();
  let socket_mock2 = router_mock.ask_sock();
  let socket_mock3 = router_mock.ask_sock();

  let mut threads = vec![];

  // Spawns the router thread
  let router_thread = thread::spawn(move || loop {
    while let Ok(router_message) = router_mock.rx.try_recv() {
      // println!("vasco");
      // println!("{:?}", router_message);
      // println!(
      //   "router receiving: repassing message from {} to {}",
      //   router_message.1, router_message.0
      // );
      router_mock.send(&router_message)
    }
  });
  threads.push(router_thread);

  // Spawns the nodes thread

  let socket_thread1 = thread::spawn(move || {
    start(&Some(vec![1_u32]), socket_mock1);
  });
  threads.push(socket_thread1);

  let socket_thread2 = thread::spawn(move || {
    start(&Some(vec![]), socket_mock2);
  });
  threads.push(socket_thread2);

  let socket_thread3 = thread::spawn(move || {
    start(&Some(vec![1_u32]), socket_mock3);
  });
  threads.push(socket_thread3);

  // Joins all threads
  for thread in threads {
    thread.join().unwrap();
  }
}

fn start(init_peers: &Option<Vec<u32>>, socket_mock: SocketMock) {
  eprintln!("Starting Kindelia node.");

  let state_path = dirs::home_dir().unwrap().join(".kindelia");
  let port = socket_mock.get_addr();

  // Node state object
  let (node_query_sender, node) =
    node::Node::new(state_path, init_peers, port as u64, port, socket_mock);

  // Node to Miner communication object
  let miner_comm_0 = MinerCommunication::new();
  let miner_comm_1 = miner_comm_0.clone();

  // Threads
  let mut threads = vec![];

  // Spawns the node thread
  let node_thread = thread::spawn(move || {
    node.main(miner_comm_0, true);
  });
  threads.push(node_thread);

  // Spawns the miner thread
  let miner_thread = thread::spawn(move || {
    node::miner_loop(miner_comm_1);
  });
  threads.push(miner_thread);

  // Joins all threads
  for thread in threads {
    thread.join().unwrap();
  }
}
