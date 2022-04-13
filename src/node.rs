use bit_vec::BitVec;
use im::HashSet;
use primitive_types::U256;
use priority_queue::PriorityQueue;
use sha3::Digest;
use std::collections::HashMap;
use std::net::*;

use crate::algorithms::*;
use crate::constants::*;
use crate::network::*;
use crate::serializer::*;
use crate::types::*;

// don't accept blocks from 1 hour in the future
const DELAY_TOLERANCE : u64 = 60 * 60 * 1000;
  
// readjusts difficulty every 20 blocks
const BLOCKS_PER_PERIOD : u64 = 20;

// 1 second per block
const TIME_PER_BLOCK : u64 = 1000;

// readjusts difficulty every 60 seconds
const TIME_PER_PERIOD : u64 = TIME_PER_BLOCK * BLOCKS_PER_PERIOD;

// initial target of 256 hashes per block
pub fn INITIAL_TARGET() -> U256 {
  return compute_target(u256(256));
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

pub fn INITIAL_NODE_STATE() -> Node {
  return Node {
    block    : HashMap::from([(ZERO_HASH(), ROOT_BLOCK())]),
    children : HashMap::from([(ZERO_HASH(), vec![])]),
    waiters  : HashMap::new(),
    work     : HashMap::from([(ZERO_HASH(), u256(0))]),
    height   : HashMap::from([(ZERO_HASH(), 0)]),
    target   : HashMap::from([(ZERO_HASH(), INITIAL_TARGET())]),
    seen     : HashMap::from([]),
    tip      : (u256(0), ZERO_HASH()),
    mined    : HashMap::new(),
    pool     : PriorityQueue::new(),
    peer_id  : HashMap::new(),
    peers    : HashMap::new(),
  };
}

pub fn see_peer(node: &mut Node, addr: Address) {
  match node.peer_id.get(&addr) {
    None => {
      let index = node.peers.len() as u64;
      let peer = Peer { seen_at: get_time(), address: addr.clone() };
      node.peers.insert(index, peer);
      node.peer_id.insert(addr, index);
    }
    Some(index) => {
      let peer = node.peers.get_mut(&index).unwrap();
      peer.seen_at = get_time();
    }
  }
}

pub fn del_peer(node: &mut Node, addr: Address) {
  if let Some(index) = node.peer_id.get(&addr) {
    node.peers.remove(&index);
    node.peer_id.remove(&addr);
  }
}

pub fn get_random_peer(node: &mut Node) -> Option<Peer> {
  let len = node.peers.len() as u64;
  if len > 0 {
    return Some(node.peers[&(rand::random::<u64>() % len)]);
  } else {
    return None;
  }
}

pub fn get_random_peers(node: &mut Node, len: u64) -> Option<Vec<Peer>> {
  let mut result = vec![];
  for i in 0 .. len {
    result.push(get_random_peer(node)?);
  }
  return Some(result);
}

pub fn add_block(node: &mut Node, block: &Block, current_time: u64) {
  // Adding a block might trigger the addition of other blocks
  // that were waiting for it. Because of that, we loop here.
  let mut must_add = vec![block.clone()]; // blocks to be added
  // While there is a block to add...
  while let Some(block) = must_add.pop() {
    let btime = block.time; // the block timestamp
    // If block is too far into the future, ignore it
    if btime >= current_time + DELAY_TOLERANCE {
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
        if node.height[&bhash] > 0 && node.height[&bhash] % BLOCKS_PER_PERIOD == 0 {
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

pub fn handle_message(socket: &mut UdpSocket, node: &mut Node, sender: Address, message: &Message) {
  match message {
    Message::PutPeers { peers } => {
      for address in peers {
        see_peer(node, sender);
      }
    }
    Message::PutBlock { block } => {
      add_block(node, &block, get_time());
    }
    Message::AskBlock { bhash } => {
      if let Some(block) = node.block.get(&bhash) {
        udp_send(socket, sender, &Message::PutBlock { block: block.clone() });
      }
    }
  }
}

pub fn gossip(socket: &mut UdpSocket, node: &mut Node, peer_count: u64, message: &Message) {
  if let Some(peers) = get_random_peers(node, peer_count) {
    for peer in peers {
      udp_send(socket, peer.address, message);
    }
  }
}

pub fn gossip_tip_block(socket: &mut UdpSocket, node: &mut Node, peer_count: u64) {
  gossip(socket, node, peer_count, &Message::PutBlock { block: node.block[&node.tip.1].clone() });
}

pub fn request_missing_blocks(socket: &mut UdpSocket, node: &mut Node) {
  for bhash in node.waiters.keys().cloned().collect::<Vec<U256>>() {
    if let None = node.seen.get(&bhash) {
      gossip(socket, node, REQUEST_FACTOR, &Message::AskBlock { bhash: bhash.clone() });
    }
  }
}

// Returns ~/.kindelia/blocks
pub fn get_blocks_dir() -> String {
  let mut dir = dirs::home_dir().unwrap();
  dir.push(".kindelia");
  dir.push("blocks");
  return dir.to_str().unwrap().to_string();
}

pub fn save_longest_chain(node: &mut Node) {
  for (index, block) in get_longest_chain(node).iter().enumerate() {
    let bdir = get_blocks_dir();
    let path = format!("{}/{}", bdir, index);
    let buff = bitvec_to_bytes(&serialized_block(&block));
    std::fs::create_dir_all(bdir).ok();
    std::fs::write(path, buff).ok();
  }
}

pub fn load_longest_chain(node: &mut Node) {
  let bdir = get_blocks_dir();
  std::fs::create_dir_all(&bdir).ok();
  for entry in std::fs::read_dir(&bdir).unwrap() {
    let buffer = std::fs::read(entry.unwrap().path()).unwrap();
    let block = deserialized_block(&bytes_to_bitvec(&buffer));
    add_block(node, &block, get_time());
  }
}

pub fn display_node(node: &Node, lines: usize) -> String {
  let blocks = get_longest_chain(node);
  let lim = next_power_of_two(blocks.len() as f64) as u64;
  let add = if lim > 32 { lim / 32 } else { 1 };
  let mut text = "       # | time          | hash                                                             | head                                                             | work\n".to_string();
  for i in 0..blocks.len() - 1 {
    text += &show_block(node, &blocks[i], i);
    if (i as u64) % add == add - 1 {
      text += "\n";
    }
  }
  if blocks.len() > 1 {
    text += &show_block(node, &blocks[blocks.len() - 1], blocks.len() - 1);
  }
  return text;
}
