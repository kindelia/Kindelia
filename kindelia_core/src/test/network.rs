use petgraph::algo::astar;
use petgraph::prelude::UnGraph;

use std::collections::HashMap;
use std::sync::mpsc::{self, Receiver, Sender};
use std::thread;

use crate::config;
#[cfg(feature = "events")]
use crate::events;
use crate::net::{self, ProtoComm, ProtoCommError};
use crate::node;
use crate::{bits, persistence};
use kindelia_lang::parser;

use super::util::temp_dir;

#[test]
#[ignore = "network simulation"]
fn network() {
  let simulation_time = std::env::var("SIMULATION_TIME").ok();

  let mut graph = UnGraph::new_undirected();
  let a = graph.add_node(0_u32);
  let b = graph.add_node(1);
  let c = graph.add_node(2);
  let d = graph.add_node(3);
  let e = graph.add_node(4);
  graph.extend_with_edges(&[(a, b, RouterMockConnection::new(20, 0.5))]);
  graph.extend_with_edges(&[(b, c, RouterMockConnection::new(20, 0.5))]);
  graph.extend_with_edges(&[(c, d, RouterMockConnection::new(20, 0.5))]);
  graph.extend_with_edges(&[(d, e, RouterMockConnection::new(20, 0.5))]);

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
      let ws_config = config::WsConfig { buffer_size: 1024 * 2 };

      let mine_cfg = config::MineConfigBuilder::default()
        .enabled(true)
        .slow_mining(100)
        .build()
        .unwrap();

      let node_cfg = config::NodeConfig {
        network_id: 0,
        data_path,
        mining: mine_cfg,
        ui: None, // not needed to configure in simulation; only prints in json
        ws: Some(ws_config),
      };

      // whit this is possible to choose what events print
      let tags = vec![];
      start_simulation(node_cfg, socket, initial_peers, tags);
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
// =========================

/// A version of start node but for simulation
fn start_simulation<C: ProtoComm + 'static>(
  node_config: config::NodeConfig,
  comm: C,
  initial_peers: Vec<C::Address>,
  tags: Vec<events::NodeEventDiscriminant>,
) {
  let addr = comm.get_addr().unwrap();

  // Events
  #[cfg(feature = "events")]
  let (event_tx, event_thread) = spawn_event_handler(tags, addr);

  // Mining
  let (miner_comm, miner_thrds) =
    node::spawn_miner(node_config.mining, Some(event_tx.clone()));

  // Storage
  let storage = persistence::EmptyStorage;

  let GENESIS_CODE = include_str!("../../genesis-tests.kdl");
  let genesis_stmts =
    parser::parse_code(GENESIS_CODE).expect("Genesis code parses.");

  // Node
  let node_thread = {
    let (node_query_sender, node) = node::Node::new(
      node_config.data_path,
      node_config.network_id,
      addr,
      &genesis_stmts,
      initial_peers,
      comm,
      miner_comm,
      storage,
      #[cfg(feature = "events")]
      Some(event_tx),
    );

    // Spawns the node thread
    std::thread::spawn(move || {
      node.main();
    })
  };

  // Threads
  let mut threads = vec![node_thread];
  threads.insert(0, event_thread);
  threads.extend(miner_thrds.into_iter());

  // Joins all threads
  for thread in threads {
    thread.join().unwrap();
  }
}

/// A version of `kindelia::spawn_event_handlers` but for simulation.
///
/// It does not contain the ws server and only prints the events in JSON.
fn spawn_event_handler<A: net::ProtoAddr + 'static>(
  tags: Vec<events::NodeEventDiscriminant>,
  addr: A,
) -> (
  std::sync::mpsc::Sender<(events::NodeEventType, u128)>,
  std::thread::JoinHandle<()>,
) {
  let (event_tx, event_rx) =
    std::sync::mpsc::channel::<(events::NodeEventType, u128)>();

  let thread = std::thread::spawn(move || {
    while let Ok((event, time)) = event_rx.recv() {
      if tags.is_empty() || tags.contains(&(event.clone()).into()) {
        let event = events::NodeEvent { time, addr, event };
        println!("{}", serde_json::to_string(&event).unwrap());
      }
    }
  });

  (event_tx, thread)
}

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

  fn get_addr(&self) -> Result<Self::Address, ProtoCommError> {
    Ok(self.addr)
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
