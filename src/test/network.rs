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
  net::{self, ProtoComm},
  node::{self, Message, MinerCommunication},
  util::u256,
};

use super::util::temp_dir;

#[test]
#[ignore = "network simulation"]
fn network() {
  // creates the router
  let mut router_mock = RouterMock::new();

  // create three sockets mock
  let socket_mock1 = router_mock.ask_sock();
  let socket_mock2 = router_mock.ask_sock();
  let socket_mock3 = router_mock.ask_sock();

  let sockets = vec![socket_mock1, socket_mock2, socket_mock3];

  let mut threads = vec![];

  // Spawns the router thread
  let router_thread = thread::spawn(move || loop {
    while let Ok(router_message) = router_mock.rx.try_recv() {
      router_mock.send(&router_message)
    }
  });
  threads.push(router_thread);

  // Spawns the nodes threads

  for socket in sockets {
    let socket_thread = thread::spawn(move || {
      let state_path = temp_dir()
        .path
        .join(".kindelia")
        .join(format!(".test-{}", socket.addr));
      node::start(
        state_path,
        socket.get_addr() as u64,
        socket,
        &Some(vec![1_u32]),
        true,
        false,
      );
    });
    threads.push(socket_thread);
  }

  // Joins all threads
  for thread in threads {
    thread.join().unwrap();
  }
}

// Simulation implementation
// ================================

// Simulation address

/// The address of the simulation will be a simple `u32` value.
impl net::ProtoCommAddress for u32 {}

/// `ProtoSerialize` implementation of `u32`, necessary to satisfy
/// the ProtoCommAddress trait.
impl bits::ProtoSerialize for u32 {
  fn proto_serialize(
    &self,
    bits: &mut bit_vec::BitVec,
    names: &mut bits::Names,
  ) {
    bits::serialize_number(&u256(*self as u128), bits, names);
  }
  fn proto_deserialize(
    bits: &bit_vec::BitVec,
    index: &mut u128,
    names: &mut bits::Names,
  ) -> Option<Self> {
    bits::deserialize_number(bits, index, names).map(|n| n.low_u32())
  }
}

// Simulation socket
/// This struct represents a Socket.
///
/// It has
/// - an `u32` address to identify itself
/// - a `tx` `Sender` that is capable of send `RouterMessage`'s to the `RouterMock`
/// - a `rx` `Receiver` that is capable of receive `RouterMessage`'s from the `RouterMock`
pub struct SocketMock {
  tx: Sender<RouterMessage>,
  rx: Receiver<RouterMessage>,
  addr: u32,
}

/// Implementation of the `ProtoComm` for the `SocketMock`
impl net::ProtoComm for SocketMock {
  type Address = u32;
  fn proto_recv(
    &mut self,
  ) -> Vec<(Self::Address, node::Message<Self::Address>)> {
    let mut messages = Vec::new();
    while let Ok(RouterMessage { to_addr, from_addr, msg }) = self.rx.try_recv()
    {
      messages.push((from_addr, msg))
    }
    messages
  }

  fn proto_send(
    &mut self,
    addresses: Vec<Self::Address>,
    message: &node::Message<Self::Address>,
  ) {
    for addr in addresses {
      self
        .tx
        .send(RouterMessage {
          to_addr: addr,
          from_addr: self.addr,
          msg: message.to_owned(),
        })
        .unwrap()
    }
  }

  fn get_addr(&self) -> Self::Address {
    self.addr
  }
}

// Simulation router

/// A representation of a message that the `RouterMock`
/// expects to receive.
#[derive(Debug)]
struct RouterMessage {
  to_addr: u32,
  from_addr: u32,
  msg: node::Message<u32>,
}

/// This struct represents a Router.
///
/// It has
/// - all the sockets `Sender`'s created by itself stored in `sockets`
/// - a `tx` `Sender` that is passed for the `sockets`
/// - a `rx` `Receiver` that is capable of receiving `RouterMessage`'s from the all the sockets
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
    let RouterMessage { to_addr, from_addr, msg } = router_message;
    let tx = self.sockets.get(&to_addr).unwrap();
    tx.send(RouterMessage {
      to_addr: *to_addr,
      from_addr: *from_addr,
      msg: msg.to_owned(),
    })
    .unwrap()
  }
}

fn start(init_peers: &Option<Vec<u32>>, socket_mock: SocketMock) {
  eprintln!("Starting Kindelia node.");

  let state_path = dirs::home_dir().unwrap().join(".kindelia");
  let port = socket_mock.get_addr();

  // Node state object
  let (node_query_sender, node) =
    node::Node::new(state_path, init_peers, port as u64, socket_mock);

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
