use bit_vec::BitVec;
use im::HashSet;
use primitive_types::U256;
use priority_queue::PriorityQueue;
use sha3::Digest;
use std::collections::HashMap;
use std::net::*;
use std::sync::{Arc, Mutex};
use std::thread;

use crate::constants::*;

pub type U256Map<T> = HashMap<U256, T>;

pub type Hash = U256;

#[derive(Debug, Clone, PartialEq)]
pub struct Body {
  pub value: [u8; BODY_SIZE],
}

#[derive(Debug, Clone)]
pub struct Block {
  pub time: u64,  // block timestamp
  pub rand: u64,  // block nonce
  pub prev: U256, // previous block (32 bytes)
  pub body: Body, // block contents (1280 bytes)
}

pub type Transaction = Vec<u8>;

pub struct Node {
  pub socket     : UdpSocket,                      // UDP socket
  pub port       : u16,                            // UDP port
  pub tip        : (U256, U256),                   // current tip work and block hash
  pub block      : U256Map<Block>,                 // block hash -> block information
  pub children   : U256Map<Vec<U256>>,             // block hash -> blocks that have this as its parent
  pub waiters    : U256Map<Vec<Block>>,            // block hash -> blocks that are waiting for this block info
  pub work       : U256Map<U256>,                  // block hash -> accumulated work
  pub target     : U256Map<U256>,                  // block hash -> this block's target
  pub height     : U256Map<u64>,                   // block hash -> cached height
  pub seen       : U256Map<()>,                    // block hash -> have we received it yet?
  pub was_mined  : U256Map<HashSet<Transaction>>,  // block hash -> set of transaction hashes that were already mined
  pub pool       : PriorityQueue<Transaction,u64>, // transactions to be mined
  pub peer_id    : HashMap<Address, u64>,          // peer address -> peer id
  pub peers      : HashMap<u64, Peer>,             // peer id -> peer
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Address {
  IPv4 {
    val0: u8,
    val1: u8,
    val2: u8,
    val3: u8,
    port: u16,
  }
}

#[derive(Debug, Copy, Clone)]
pub struct Peer {
  pub seen_at: u64,
  pub address: Address,
}

#[derive(Debug, Clone)]
pub enum MinerComm {
  Request {
    prev: U256,
    body: Body,
    targ: U256, 
  },
  Answer {
    block: Block
  },
  Stop,
}

pub type SharedMinerComm = Arc<Mutex<MinerComm>>;
pub type SharedInput = Arc<Mutex<String>>;

#[derive(Debug, Clone)]
pub enum Message {
  PutBlock {
    block: Block,
    peers: Vec<Peer>,
  },
  AskBlock {
    bhash: Hash
  },
}
