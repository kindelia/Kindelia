use std::path::PathBuf;
use std::sync::mpsc;

use priority_queue::PriorityQueue;
use thiserror::Error;

use kindelia_common::crypto::Keccakable;
use kindelia_lang::parser;

use crate::api::NodeRequest;
use crate::net::ProtoComm;
use crate::node::{
  build_genesis_block, initial_target, BlockBodyError, MinerCommunication,
  Node, Peer, PeersStore,
};
use crate::persistence::BlockStorage;
use crate::runtime::*;
use crate::util::*;

use crate::events::NodeEventEmittedInfo;

/// Errors associated with NodeBuilder
#[derive(Error, Debug)]
pub enum NodeBuilderError {
  #[error("Parse error in block statements: {0}")]
  Parse(String),
  #[error(transparent)]
  Epoch(#[from] EpochError),
  #[error(transparent)]
  BlockBody(#[from] BlockBodyError),
  #[error("Missing required field {field}")]
  MissingRequired { field: String },
}

/// A Builder to simplify creation of a Node
///
/// Example:
///
///   let (node_query_sender, node) = NodeBuilder::default()
///    .network_id(network_id)
///    .comm(comm)
///    .miner_comm(miner_comm)
///    .addr(addr)
///    .storage(file_writer)
///    .genesis_code(genesis_code)
///    .data_path(data_path)
///    .build()?;
pub struct NodeBuilder<C: ProtoComm, S: BlockStorage> {
  network_id: u32,          // Network ID / magic number
  comm: Option<C>,          // UDP socket
  addr: Option<C::Address>, // UDP port
  storage: Option<S>,       // A `BlockStorage` implementation
  peers: Vec<C::Address>,   // peers store and state control

  #[cfg(feature = "events")]
  event_emitter: Option<mpsc::Sender<NodeEventEmittedInfo>>,
  miner_comm: Option<MinerCommunication>,

  // these do not exist in Node. they are "transient"
  data_path: PathBuf, // path to ~/.kindelia/state/<network_id>
  genesis_code: String, // statements in genesis block body
}

impl<C: ProtoComm, S: BlockStorage> NodeBuilder<C, S> {
  /// set network identifier
  pub fn network_id(mut self, network_id: u32) -> Self {
    self.network_id = network_id;
    self
  }

  /// set comm channel, eg std::net::UdpSocket + ProtoComm
  pub fn comm(mut self, comm: C) -> Self {
    self.comm = Some(comm);
    self
  }

  /// set comm addr, eg net::Address
  pub fn addr(mut self, addr: C::Address) -> Self {
    self.addr = Some(addr);
    self
  }

  /// set storage, eg persistence::SimpleFileStorage
  pub fn storage(mut self, storage: S) -> Self {
    self.storage = Some(storage);
    self
  }

  /// set source code statements for the genesis block
  pub fn genesis_code(mut self, genesis_code: String) -> Self {
    self.genesis_code = genesis_code;
    self
  }

  /// set data path, eg ~/.kindelia/state/<network_id>
  pub fn data_path(mut self, data_path: PathBuf) -> Self {
    self.data_path = data_path;
    self
  }

  /// set initial peers for this node to contact
  pub fn peers(mut self, peers: Vec<C::Address>) -> Self {
    self.peers = peers;
    self
  }

  /// set event emitter
  pub fn event_emitter(
    mut self,
    emitter: mpsc::Sender<NodeEventEmittedInfo>,
  ) -> Self {
    self.event_emitter = Some(emitter);
    self
  }

  /// set miner communication channel, or None if not mining.
  pub fn miner_comm(mut self, miner_comm: Option<MinerCommunication>) -> Self {
    self.miner_comm = miner_comm;
    self
  }

  /// validates builder inputs and builds a Node instance.
  //
  // note: I would like to define a type alias to get rid of this clippy
  //       warning however it appears to require "inherent associated types"
  //       due to the use of generics, which are unstable.
  //       see: https://github.com/rust-lang/rust/issues/8995
  #[allow(clippy::type_complexity)]
  pub fn build(
    self,
  ) -> Result<(mpsc::SyncSender<NodeRequest<C>>, Node<C, S>), NodeBuilderError>
  {
    let genesis_stmts = parser::parse_code(&self.genesis_code)
      .map_err(NodeBuilderError::Parse)?;
    let genesis_block = build_genesis_block(&genesis_stmts)?;
    let genesis_block = genesis_block.hashed();
    let genesis_hash = genesis_block.get_hash().into();

    let runtime = init_runtime(self.data_path.join("heaps"), &genesis_stmts);

    let tip = genesis_hash;
    let block = u256map_from([(genesis_hash, genesis_block)]);
    let pending = u256map_new();
    let ancestor = u256map_new();
    let wait_list = u256map_new();
    let children = u256map_from([(genesis_hash, vec![])]);
    let work = u256map_from([(genesis_hash, u256(0))]);
    let height = u256map_from([(genesis_hash, 0)]);
    let target = u256map_from([(genesis_hash, initial_target())]);
    let results = u256map_from([(genesis_hash, vec![])]);

    let (query_sender, query_recv) = mpsc::sync_channel(1);

    let pool = PriorityQueue::new();
    let peers = PeersStore::new();

    let now = try_get_time()?;

    let comm = self
      .comm
      .ok_or(NodeBuilderError::MissingRequired { field: "comm".to_string() })?;

    let addr = self
      .addr
      .ok_or(NodeBuilderError::MissingRequired { field: "addr".to_string() })?;

    let storage = self.storage.ok_or(NodeBuilderError::MissingRequired {
      field: "storage".to_string(),
    })?;

    // finally we can instantiate a Node!
    let mut node = Node {
      network_id: self.network_id,
      comm,
      addr,
      storage,
      runtime,
      query_recv,
      pool,
      peers,
      genesis_hash,
      tip,
      block,
      pending,
      ancestor,
      wait_list,
      children,
      work,
      target,
      height,
      results,
      miner_comm: self.miner_comm,

      #[cfg(feature = "events")]
      event_emitter: self.event_emitter,
    };

    // make node aware of each initial peer
    self.peers.iter().for_each(|address| {
      node.peers.see_peer(
        Peer { address: *address, seen_at: now },
        #[cfg(feature = "events")] // TODO: remove (implement on Node)
        node.event_emitter.clone(),
      )
    });

    Ok((query_sender, node))
  }
}

impl<C: ProtoComm, S: BlockStorage> Default for NodeBuilder<C, S> {
  fn default() -> Self {
    Self {
      network_id: Default::default(),
      comm: Default::default(),
      addr: Default::default(),
      storage: Default::default(),
      peers: Default::default(),
      data_path: Default::default(),
      genesis_code: Default::default(),

      #[cfg(feature = "events")]
      event_emitter: Default::default(),
      miner_comm: Default::default(),
    }
  }
}
