use std::sync::mpsc::{self, Receiver, Sender, SyncSender};

use crate::common::*;
use crate::types::{Block, Body, Node};

// Channels
// ========

pub type NodeRecv = Receiver<NodeAsk>;
pub type NodeSender = Sender<NodeAns>;
pub type ClientRecv = Receiver<NodeAns>;
pub type ClientSender = SyncSender<NodeAsk>;
pub type NodeCommEdge = (NodeSender, NodeRecv);
pub type ClientCommEdge = (ClientSender, ClientRecv);

pub fn make_node_channels(
  ask_buffer_size: usize,
) -> (NodeCommEdge, ClientCommEdge) {
  let (client_tx, node_rx) = mpsc::sync_channel(ask_buffer_size);
  let (node_tx, client_rx) = mpsc::channel();
  ((node_tx, node_rx), (client_tx, client_rx))
}

// Messages
// ========

pub enum NodeAsk {
  Info { max_last_blocks: Option<u64> },
  ToMine(Box<Body>),
}

pub enum NodeAns {
  NodeInfo(NodeInfo),
}

pub struct NodeInfo {
  pub time: u64,
  pub num_peers: u64,
  pub num_blocks: u64,
  pub num_pending: u64,
  pub height: u64,
  pub difficulty: U256,
  pub hash_rate: U256,
  pub tip_hash: U256,
  pub last_blocks: Vec<Block>,
}

pub trait Frontend {
  fn get_tasks(
    &self,
    comm_edge: ClientCommEdge,
  ) -> Vec<Box<dyn FnOnce() + Send + 'static>>;
}
