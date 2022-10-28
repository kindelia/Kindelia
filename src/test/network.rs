use futures_util::future::join_all;
use petgraph::algo::astar;
use petgraph::prelude::UnGraph;

use std::collections::HashMap;
use std::sync::mpsc::{self, Receiver, Sender};
use std::thread;

use crate::bits;
use crate::config;
#[cfg(feature = "events")]
use crate::events;
use crate::net;
use crate::node;

use super::util::temp_dir;

#[test]
#[ignore = "network simulation"]
fn network() {
  let simulation_time = std::env::var("SIMULATION_TIME").ok();

  let mut graph = UnGraph::new_undirected();
  let a = graph.add_node(0_u32);
  let b = graph.add_node(1);
  // let c = graph.add_node(2);
  // let d = graph.add_node(3);
  // let e = graph.add_node(4);
  graph.extend_with_edges(&[(a, b, RouterMockConnection::new(20, 0.5))]);
  // graph.extend_with_edges(&[(b, c, RouterMockConnection::new(20, 0.5))]);
  // graph.extend_with_edges(&[(c, d, RouterMockConnection::new(20, 0.5))]);
  // graph.extend_with_edges(&[(d, e, RouterMockConnection::new(20, 0.5))]);

  // creates the router
  let (router_mock, sockets) = RouterMock::from_graph(graph.clone());
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
    let initial_peers = graph
      .neighbors(idx)
      .map(|node| *graph.node_weight(node).unwrap())
      .collect();
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

      let node_cfg = config::NodeConfig {
        network_id: 0,
        data_path,
        mining: mine_cfg,
        ui: Some(config::UiConfig { json: true, tags: vec![] }),
        api: None,
        ws: Some(ws_config), // Some(ws_config),
      };
      node::start(node_cfg, socket, initial_peers);
    });
    threads.push(socket_thread);
  }

  if let Some(simulation_time) = simulation_time {
    if let Ok(simulation_time) = simulation_time.parse() {
      let killer = thread::spawn(move || {
        thread::sleep(std::time::Duration::from_secs(simulation_time));
        std::process::exit(0);
      });
      threads.push(killer);
    }
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
    bits::serialize_number(*self as u128, bits);
  }
  fn proto_deserialize(
    bits: &bit_vec::BitVec,
    index: &mut usize,
    names: &mut bits::Names,
  ) -> Option<Self> {
    bits::deserialize_number(bits, index).map(|n| n.low_u32())
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
    let to_addr = *to_addr;
    let from_addr = *from_addr;
    let msg = msg.clone();
    let tx = self.sockets.get(&to_addr).unwrap();

    let connection = self.connection(to_addr, from_addr);
    if let Some(connection) = connection {
      let chance: f32 = rand::random();
      // println!(
      //   "Error chance of {} from {} to {}",
      //   connection.error, from_addr, to_addr
      // );
      if chance > connection.error {
        let tx = tx.clone();
        thread::spawn(move || {
          // println!(
          //   "Awaiting {} from {} to {}",
          //   connection.delay, from_addr, to_addr
          // );
          thread::sleep(std::time::Duration::from_millis(connection.delay));
          // println!("Sending from {} to {}", from_addr, to_addr);
          tx.send(RouterMessage { to_addr, from_addr, msg }).unwrap()
        });
      } else {
        // println!("Not sending from {} to {}", from_addr, to_addr);
      }
    }
  }
}
