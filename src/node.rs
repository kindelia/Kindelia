use bit_vec::BitVec;
use im::HashSet;
use pad::{PadStr, Alignment};
use primitive_types::U256;
use priority_queue::PriorityQueue;
use sha3::Digest;
use rand::seq::IteratorRandom;

use std::collections::HashMap;
use std::net::*;
use std::sync::{Arc, Mutex};
use std::thread;

use std::io::{stdin, stdout, Write};
use termion::event::Key;
use termion::input::TermRead;
use termion::raw::IntoRawMode;
use termion::{color, style};

use crate::algorithms::*;
use crate::constants::*;
use crate::network::*;
use crate::serializer::*;
use crate::types::*;

use crate::api::{self, NodeCommEdge};

// initial target of 256 hashes per block
pub fn INITIAL_TARGET() -> U256 {
  return difficulty_to_target(u256(INITIAL_DIFFICULTY));
}

pub fn ZERO_HASH() -> U256 {
  return hash_u256(u256(0));
}

pub fn ROOT_BLOCK() -> Block {
  return Block {
    prev: ZERO_HASH(),
    time: 0,
    rand: 0,
    body: Body {
      value: [0; 1280]
    }
  }
}

pub fn new_node() -> Node {
  let try_ports = [UDP_PORT, UDP_PORT + 1, UDP_PORT + 2];
  let (socket, port) = udp_init(&try_ports).expect("Couldn't open UDP socket.");
  let mut node = Node {
    socket     : socket,
    port       : port,
    block      : HashMap::from([(ZERO_HASH(), ROOT_BLOCK())]),
    children   : HashMap::from([(ZERO_HASH(), vec![])]),
    waiters    : HashMap::new(),
    work       : HashMap::from([(ZERO_HASH(), u256(0))]),
    height     : HashMap::from([(ZERO_HASH(), 0)]),
    target     : HashMap::from([(ZERO_HASH(), INITIAL_TARGET())]),
    seen       : HashMap::from([(ZERO_HASH(), ())]),
    tip        : (u256(0), ZERO_HASH()),
    was_mined  : HashMap::new(),
    pool       : PriorityQueue::new(),
    peer_id    : HashMap::new(),
    peers      : HashMap::new(),
  };
  node_see_peer(&mut node, Peer {
    address: Address::IPv4 { val0: 127, val1: 0, val2: 0, val3: 1, port: UDP_PORT + 0 },
    seen_at: get_time(),
  });
  node_see_peer(&mut node, Peer {
    address: Address::IPv4 { val0: 127, val1: 0, val2: 0, val3: 1, port: UDP_PORT + 1 },
    seen_at: get_time(),
  });
  node_see_peer(&mut node, Peer {
    address: Address::IPv4 { val0: 127, val1: 0, val2: 0, val3: 1, port: UDP_PORT + 2 },
    seen_at: get_time(),
  });
  return node;
}

pub fn new_miner_comm() -> SharedMinerComm {
  Arc::new(Mutex::new(MinerComm::Stop))
}

pub fn node_see_peer(node: &mut Node, peer: Peer) {
  match node.peer_id.get(&peer.address) {
    None => {
      // FIXME: `index` can't be generated from length as `.peers` is a map and
      // peers can be deleted
      let index = node.peers.len() as u64;
      node.peers.insert(index, peer.clone());
      node.peer_id.insert(peer.address, index);
    }
    Some(index) => {
      let old_peer = node.peers.get_mut(&index).unwrap();
      old_peer.seen_at = peer.seen_at;
    }
  }
}

pub fn node_del_peer(node: &mut Node, addr: Address) {
  if let Some(index) = node.peer_id.get(&addr) {
    node.peers.remove(&index);
    node.peer_id.remove(&addr);
  }
}

pub fn get_random_peers(node: &mut Node, amount: u64) -> Vec<Peer> {
  let amount = amount as usize;
  let mut rng = rand::thread_rng();
  node.peers.values().cloned().choose_multiple(&mut rng, amount)
}

pub fn node_add_block(node: &mut Node, block: &Block) {
  // Adding a block might trigger the addition of other blocks
  // that were waiting for it. Because of that, we loop here.
  let mut must_add = vec![block.clone()]; // blocks to be added
  //println!("- add_block");
  // While there is a block to add...
  while let Some(block) = must_add.pop() {
    let btime = block.time; // the block timestamp
    // If block is too far into the future, ignore it
    if btime >= get_time() + DELAY_TOLERANCE {
      continue;
    }
    let bhash = hash_block(&block); // hash of the block
    // If we already registered this block, ignore it
    if node.block.get(&bhash).is_some() {
      continue;
    }
    let phash = block.prev; // hash of the previous block
    // If previous block is available, add the block to the chain
    if node.block.get(&phash).is_some() {
      let work = get_hash_work(bhash); // block work score
      node.block.insert(bhash, block.clone()); // inserts the block
      node.work.insert(bhash, u256(0)); // inits the work attr
      node.height.insert(bhash, 0); // inits the height attr
      node.target.insert(bhash, u256(0)); // inits the target attr
      node.children.insert(bhash, vec![]); // inits the children attrs
      // Checks if this block PoW hits the target
      let has_enough_work = bhash >= node.target[&phash];
      // Checks if this block's timestamp is larger than its parent's timestamp
      // Note: Bitcoin checks if it is larger than the median of the last 11 blocks; should we?
      let advances_time = btime > node.block[&phash].time;
      // If the PoW hits the target and the block's timestamp is valid...
      if has_enough_work && advances_time {
        node.work.insert(bhash, node.work[&phash] + work); // sets this block accumulated work
        node.height.insert(bhash, node.height[&phash] + 1); // sets this block accumulated height
        // If this block starts a new period, computes the new target
        if node.height[&bhash] > 0 && node.height[&bhash] > BLOCKS_PER_PERIOD && node.height[&bhash] % BLOCKS_PER_PERIOD == 1 {
          // Finds the checkpoint hash (hash of the first block of the last period)
          let mut checkpoint_hash = phash;
          for _ in 0 .. BLOCKS_PER_PERIOD - 1 {
            checkpoint_hash = node.block[&checkpoint_hash].prev;
          }
          // Computes how much time the last period took to complete
          let period_time = btime - node.block[&checkpoint_hash].time;
          // Computes the target of this period
          let last_target = node.target[&phash];
          let next_scaler = 2u64.pow(32) * TIME_PER_PERIOD / period_time;
          let next_target = compute_next_target(last_target, u256(next_scaler));
          // Sets the new target
          node.target.insert(bhash, next_target);
        // Otherwise, keep the old target
        } else {
          node.target.insert(bhash, node.target[&phash]);
        }
        // Updates the tip work and block hash
        if node.work[&bhash] > node.tip.0 {
          node.tip = (node.work[&bhash], bhash);
        }
      }
      // Registers this block as a child of its parent
      node.children.insert(phash, vec![bhash]);
      // If there were blocks waiting for this one, mark them for addition
      for waiter in node.waiters.get(&bhash).unwrap_or(&vec![]) {
        must_add.push(waiter.clone());
      }
      // Delete this block's waiters vector
      node.waiters.remove(&bhash);
    // Otherwise (if previous block is unavailable), and if it is the
    // first time we see this block, add it to its parent's waiters list
    } else if node.seen.get(&bhash).is_none() {
      node.waiters.insert(phash, vec![block.clone()]);
    }
    // Mark this block as seen
    node.seen.insert(bhash, ());
  }
}

pub fn get_longest_chain(node: &Node) -> Vec<Block> {
  let mut longest = Vec::new();
  let mut bhash = node.tip.1;
  while node.block.contains_key(&bhash) && bhash != ZERO_HASH() {
    let block = node.block.get(&bhash).unwrap();
    longest.push(block.clone());
    bhash = block.prev;
  }
  longest.reverse();
  return longest;
}

pub fn node_message_receive(node: &mut Node) {
  for (addr, msg) in udp_receive(&mut node.socket) {
    node_message_handle(node, addr, &msg);
  }
}

// Sends a block to a target address; also share some random peers
// FIXME: instead of sharing random peers, share recently active peers
pub fn node_send_block_to(node: &mut Node, addr: Address, block: Block) {
  let msg = Message::PutBlock {
    block: block,
    peers: get_random_peers(node, 3).clone(),
  };
  udp_send(&mut node.socket, addr, &msg);
}

pub fn node_message_handle(node: &mut Node, addr: Address, msg: &Message) {
  node_see_peer(node, Peer { address: addr, seen_at: get_time() });
  match msg {
    // Someone asked a block
    Message::AskBlock { bhash } => {
      if let Some(block) = node.block.get(&bhash) {
        let block = block.clone();
        node_send_block_to(node, addr, block);
      }
    }
    // Someone sent us a block
    Message::PutBlock { block, peers } => {
      node_add_block(node, &block);
      for peer in peers {
        //node_see_peer(node, *peer);
      }
    }
  }
}

pub fn gossip(node: &mut Node, peer_count: u64, message: &Message) {
  for peer in get_random_peers(node, peer_count) {
    udp_send(&mut node.socket, peer.address, message);
  }
}

pub fn get_blocks_dir() -> String {
  let mut dir = dirs::home_dir().unwrap();
  dir.push(".kindelia");
  dir.push("blocks");
  return dir.to_str().unwrap().to_string();
}

// Mining
// ======

// Get the current target
pub fn get_tip_target(node: &Node) -> U256 {
  node.target[&node.tip.1]
}

// Given a target, attempts to mine a block by changing its nonce up to `max_attempts` times
pub fn try_mine(prev: U256, body: Body, targ: U256, max_attempts: u64) -> Option<Block> {
  let rand = rand::random::<u64>();
  let time = get_time();
  let mut block = Block { time, rand, prev, body };
  for _i in 0 .. max_attempts {
    if hash_block(&block) >= targ {
      return Some(block);
    } else {
      block.rand = block.rand.wrapping_add(1);
    }
  }
  return None;
}

// Writes the shared MinerComm object
pub fn write_miner_comm(miner_comm: &SharedMinerComm, new_value: MinerComm) {
  let mut value = miner_comm.lock().unwrap();
  *value = new_value;
}

// Reads the shared MinerComm object
pub fn read_miner_comm(miner_comm: &SharedMinerComm) -> MinerComm {
  return (*miner_comm.lock().unwrap()).clone();
}

// Main miner loop: if asked, attempts to mine a block
pub fn miner_loop(miner_comm: SharedMinerComm) {
  loop {
    if let MinerComm::Request { prev, body, targ } = read_miner_comm(&miner_comm) {
      //println!("[miner] mining with target: {}", hex::encode(u256_to_bytes(targ)));
      let mined = try_mine(prev, body, targ, MINE_ATTEMPTS);
      if let Some(block) = mined {
        //println!("[miner] mined a block!");
        write_miner_comm(&miner_comm, MinerComm::Answer { block });
      }
    }
  }
}

// Node
// ====

fn node_gossip_tip_block(node: &mut Node, peer_count: u64) {
  let random_peers = get_random_peers(node, peer_count);
  for peer in random_peers {
    node_send_block_to(node, peer.address, node.block[&node.tip.1].clone());
  }
}

fn node_peers_timeout(node: &mut Node) {
  let mut forget = Vec::new();
  for (id,peer) in &node.peers {
    //println!("... {} < {} {}", peer.seen_at, get_time() - PEER_TIMEOUT, peer.seen_at < get_time() - PEER_TIMEOUT);
    if peer.seen_at < get_time() - PEER_TIMEOUT {
      forget.push(peer.address);
    }
  }
  for addr in forget {
    node_del_peer(node, addr);
  }
}

fn node_ask_missing_blocks(node: &mut Node) {
  for bhash in node.waiters.keys().cloned().collect::<Vec<U256>>() {
    if let None = node.seen.get(&bhash) {
      gossip(node, MISSING_INFO_ASK_FACTOR, &Message::AskBlock { bhash: bhash.clone() });
    }
  }
}

fn node_save_longest_chain(node: &mut Node) {
  for (index, block) in get_longest_chain(node).iter().enumerate() {
    let bdir = get_blocks_dir();
    let path = format!("{}/{}", bdir, index);
    let buff = bitvec_to_bytes(&serialized_block(&block));
    std::fs::create_dir_all(bdir).ok();
    std::fs::write(path, buff).ok();
  }
}

fn node_load_longest_chain(node: &mut Node) {
  let bdir = get_blocks_dir();
  std::fs::create_dir_all(&bdir).ok();
  for entry in std::fs::read_dir(&bdir).unwrap() {
    let buffer = std::fs::read(entry.unwrap().path()).unwrap();
    let block = deserialized_block(&bytes_to_bitvec(&buffer));
    node_add_block(node, &block);
  }
}

fn node_ask_mine(node: &Node, miner_comm: &SharedMinerComm, body: Body) {
  write_miner_comm(miner_comm, MinerComm::Request {
    prev: node.tip.1,
    body,
    targ: get_tip_target(node),
  });
}

pub fn node_loop(
  mut node: Node,
  miner_comm: SharedMinerComm,
  node_comm: NodeCommEdge,
) {
  let mut mined = 0;
  let mut tick = 0;
  let mut body_to_mine: Option<Body> = None;

  loop {
    tick += 1;

    {
      // If the miner thread mined a block, gets and registers it
      if let MinerComm::Answer { block } = read_miner_comm(&miner_comm) {
        mined += 1;
        node_add_block(&mut node, &block);
      }

      // Asks the miner to mine a block
      if tick % 3 == 0 {
        if let Some(body) = &body_to_mine {
          node_ask_mine(&node, &miner_comm, body.clone());
        }
      }

      // Receives and handles incoming messages
      if tick % 3 == 0 {
        node_message_receive(&mut node);
      }

      // Spreads the tip block
      if tick % 3 == 0 {
        node_gossip_tip_block(&mut node, 8);
      }

      // Requests missing blocks
      if tick % 3 == 0 {
        node_ask_missing_blocks(&mut node);
      }

      // Peer timeout
      // FIXME: should be less frequent
      if tick % 60 == 0 {
        node_peers_timeout(&mut node);
      }

      // Handle API messages
      if tick % 3 == 0 {
        handle_node_comm(&node, &node_comm, &miner_comm, &mut body_to_mine);
      }
    }

    // Sleep for 1/60 seconds
    std::thread::sleep(std::time::Duration::from_micros(16666));
  }
}

fn handle_node_comm(
  node: &Node,
  node_comm: &NodeCommEdge,
  miner_comm: &SharedMinerComm,
  body_to_mine: &mut Option<Body>,
) {
  use api::{NodeAns, NodeAsk};
  let (tx, rx) = node_comm;
  match rx.try_recv() {
    Ok(val) => match val {
      NodeAsk::Info { max_last_blocks: max_last } => {
        tx.send(NodeAns::NodeInfo(get_node_info(node, max_last))).unwrap();
      }
      NodeAsk::ToMine(body) => {
        *body_to_mine = Some(*body);
      }
    },
    Err(err) => match err {
      std::sync::mpsc::TryRecvError::Empty => (),
      std::sync::mpsc::TryRecvError::Disconnected => panic!("Error: {}", err),
    },
  }
}

fn get_node_info(node: &Node, max_last_blocks: Option<u64>) -> api::NodeInfo {
  let tip = node.tip.1;
  let height = *node.height.get(&tip).unwrap();
  let tip_target = *node.target.get(&tip).unwrap();
  let difficulty = target_to_difficulty(tip_target);
  let hash_rate = difficulty * u256(1000) / u256(TIME_PER_BLOCK);

  let mut num_pending: u64 = 0;
  let mut num_pending_seen: u64 = 0;
  for (bhash, _) in node.waiters.iter() {
    if node.seen.get(bhash).is_some() {
      num_pending_seen += 1;
    }
    num_pending += 1;
  }

  // TODO: max_last_blocks
  let last_blocks = get_longest_chain(node);

  api::NodeInfo {
    time: get_time(),
    num_peers: node.peers.len() as u64,
    num_blocks: node.block.len() as u64,
    num_pending,
    num_pending_seen,
    height,
    difficulty,
    hash_rate,
    tip_hash: tip,
    last_blocks,
  }
}
