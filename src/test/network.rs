use petgraph::{
  algo::astar, graph::NodeIndices, prelude::UnGraph, visit::IntoNodeIdentifiers,
};
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
  util::u256, config,
};

#[cfg(feature = "events")]
use crate::events;

use super::util::temp_dir;

#[test]
#[ignore = "network simulation"]
fn network() {
  let mut g = UnGraph::new_undirected();
  let a = g.add_node(0_u32);
  let b = g.add_node(1);
  let c = g.add_node(2);
  g.extend_with_edges(&[
    (a, b, RouterMockConnection::new(2, 0.5)),
    (b, c, RouterMockConnection::new(5, 1.)),
  ]);

  // creates the router
  let (router_mock, sockets) = RouterMock::from_graph(g.clone());
  let mut threads = vec![];

  // Spawns the router thread
  let router_thread = thread::spawn(move || loop {
    while let Ok(router_message) = router_mock.rx.try_recv() {
      router_mock.send(&router_message)
    }
  });
  threads.push(router_thread);

  // Spawns the nodes threads
  for (socket, idx) in sockets {
    let initial_peers =
      g.neighbors(idx).map(|node| *g.node_weight(node).unwrap()).collect();
    let addr = socket.addr;
    let socket_thread = thread::spawn(move || {
      let data_path =
        temp_dir().path.join(".kindelia").join(format!(".test-{}", addr));

      #[cfg(feature = "events")]
      let ws_config =
        config::WsConfig { port: 30000 + (addr as u16), buffer_size: 1024 * 2 };

      let mine_cfg = config::MineConfigBuilder::default()
        .enabled(true)
        .slow_mining(100)
        .build()
        .unwrap();
      let ui_cfg = config::UiConfigBuilder::default().json(true).build().unwrap();
      let node_cfg = config::NodeConfigBuilder::default()
        .data_path(data_path)
        .build().unwrap();
      node::start(
        node_cfg,
        socket,
        initial_peers,
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
impl net::ProtoAddr for u32 {}

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
  network: UnGraph<u32, RouterMockConnection>,
  sockets: HashMap<u32, Sender<RouterMessage>>,
  rx: Receiver<RouterMessage>,
  tx: Sender<RouterMessage>,
}

#[derive(Default, Debug, PartialEq, Clone, Copy)]
struct RouterMockConnection {
  delay: u64,
  error: f32,
}

impl RouterMockConnection {
  fn new(delay: u64, error: f32) -> RouterMockConnection {
    if error < 0. || error > 1. {
      panic!("Only values between 0 and 1 are allowed for connection error");
    }
    RouterMockConnection { delay, error }
  }
}

impl std::ops::Add for RouterMockConnection {
  type Output = Self;
  fn add(self, rhs: Self) -> Self::Output {
    RouterMockConnection {
      delay: self.delay + rhs.delay,
      error: 1. - ((1. - self.error) * (1. - rhs.error)),
    }
  }
}

impl PartialOrd for RouterMockConnection {
  fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
    let error_cmp = self.error.partial_cmp(&other.error);
    if let Some(std::cmp::Ordering::Equal) = error_cmp {
      self.delay.partial_cmp(&other.delay)
    } else {
      error_cmp
    }
  }
}

impl RouterMock {
  fn from_graph(
    network: UnGraph<u32, RouterMockConnection>,
  ) -> (RouterMock, Vec<(SocketMock, petgraph::prelude::NodeIndex)>) {
    let (router_tx, router_rx) = mpsc::channel();

    let mut sockets = HashMap::new();
    let mut mocks = Vec::new();

    for idx in network.node_indices() {
      let addr = network.node_weight(idx).unwrap();
      let (tx, rx) = mpsc::channel();
      let socket_mock = SocketMock { addr: *addr, rx, tx: router_tx.clone() };
      sockets.insert(*addr, tx);
      mocks.push((socket_mock, idx))
    }

    (RouterMock { network, sockets, rx: router_rx, tx: router_tx }, mocks)
  }

  fn ask_sock(&mut self) -> SocketMock {
    let addr = self.sockets.len() as u32;
    let (tx, rx) = mpsc::channel();

    let socket_mock = SocketMock { addr, rx, tx: self.tx.clone() };
    self.sockets.insert(addr, tx);
    socket_mock
  }

  fn connection(&self, addr1: u32, addr2: u32) -> Option<RouterMockConnection> {
    astar(
      &self.network,
      addr1.into(),
      |finish| finish == addr2.into(),
      |e| *e.weight(),
      |_| RouterMockConnection::default(),
    )
    .map(|res| res.0)
  }

  fn send(&self, router_message: &RouterMessage) {
    let RouterMessage { to_addr, from_addr, msg } = router_message;
    let tx = self.sockets.get(&to_addr).unwrap();

    let connection = self.connection(*to_addr, *from_addr);
    if let Some(connection) = connection {
      let chance: f32 = rand::random();
      // println!(
      //   "Error chance of {} from {} to {}",
      //   connection.error, from_addr, to_addr
      // );
      if chance > connection.error {
        // println!(
        //   "Awaiting {} from {} to {}",
        //   connection.delay, from_addr, to_addr
        // );
        thread::sleep(std::time::Duration::from_millis(connection.delay));
        tx.send(RouterMessage {
          to_addr: *to_addr,
          from_addr: *from_addr,
          msg: msg.to_owned(),
        })
        .unwrap()
      } else {
        // println!("Not sending from {} to {}", from_addr, to_addr);
      }
    }
  }
}
