use bit_vec::BitVec;
use im::HashSet;
use primitive_types::U256;
use priority_queue::PriorityQueue;
use sha3::Digest;
use std::collections::HashMap;
use std::net::*;

use crate::constants::*;
use crate::serializer::*;
use crate::types::*;

// Numerics
// ========

pub fn next_power_of_two(x: f64) -> f64 {
  if x <= 1.0 { x } else { (2.0_f64).powf(x.log2().floor() + 1.0) }
}

pub fn u64_to_bytes(value: u64) -> Vec<u8> {
  return Vec::from(value.to_le_bytes());
}

pub fn u256_to_bytes(value: U256) -> Vec<u8> {
  let mut bytes = Vec::new();
  for i in 0 .. 32 {
    bytes.push(value.byte(i));
  }
  return bytes;
}

pub fn bitvec_to_bytes(bits: &BitVec) -> Vec<u8> {
  return bits.to_bytes();
}

pub fn compute_difficulty(target: U256) -> U256 {
  let p256 = U256::from("0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF");
  return p256 / (p256 - target);
}

pub fn compute_target(difficulty: U256) -> U256 {
  let p256 = U256::from("0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF");
  return p256 - p256 / difficulty;
}

// Computes next target by scaling the current difficulty by a `scale` factor
// Since the factor is an integer, it is divided by 2^32 to allow integer division
// - compute_next_target(t, 2n**32n / 2n): difficulty halves
// - compute_next_target(t, 2n**32n * 1n): nothing changes
// - compute_next_target(t, 2n**32n * 2n): difficulty doubles
pub fn compute_next_target(last_target: U256, scale: U256) -> U256 {
  let p32 = U256::from("0x100000000");
  let last_difficulty = compute_difficulty(last_target);
  let next_difficulty = U256::from(1) + (last_difficulty * scale - U256::from(1)) / p32;
  return compute_target(next_difficulty);
}

pub fn compute_next_target_f64(last_target: U256, scale: f64) -> U256 {
  return compute_next_target(last_target, U256::from((scale * 4294967296.0) as u64));
}

pub fn get_hash_work(hash: U256) -> U256 {
  if hash == U256::from(0) {
    return U256::from(0);
  } else {
    return compute_difficulty(hash);
  }
}

pub fn hash_u256(value: U256) -> U256 {
  return hash_bytes(u256_to_bytes(value).as_slice());
}

pub fn hash_bytes(bytes: &[u8]) -> U256 {
  let mut hasher = sha3::Keccak256::new();
  hasher.update(&bytes);
  let hash = hasher.finalize();
  return U256::from_little_endian(&hash);
}

pub fn hash_block(block: &Block) -> U256 {
  if block.time == 0 {
    return hash_bytes(&[]);
  } else {
    let mut bytes : Vec<u8> = Vec::new();
    bytes.extend_from_slice(&u256_to_bytes(block.prev));
    bytes.extend_from_slice(&u64_to_bytes(block.time));
    bytes.extend_from_slice(&u64_to_bytes(block.rand));
    bytes.extend_from_slice(&block.body.value);
    return hash_bytes(&bytes);
  }
}

pub fn mine(block: Block, target: U256, max_attempts: u64) -> Option<Block> {
  let mut block = block.clone();
  for _i in 0 .. max_attempts {
    if hash_block(&block) >= target {
      return Some(block);
    } else {
      block.rand = block.rand + 1;
    }
  }
  return None;
}

// Blockchain
// ==========

// FIXME: how to make the functions above constants instead?

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
  return U256::from(compute_target(U256::from(256)));
}

pub fn ZERO_HASH() -> U256 {
  return hash_u256(U256::from(0));
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

pub fn INITIAL_NODE_STATE() -> NodeState {
  return NodeState {
    block    : HashMap::from([(ZERO_HASH(), ROOT_BLOCK())]),
    children : HashMap::from([(ZERO_HASH(), vec![])]),
    waiters  : HashMap::new(),
    work     : HashMap::from([(ZERO_HASH(), U256::from(0))]),
    height   : HashMap::from([(ZERO_HASH(), 0)]),
    target   : HashMap::from([(ZERO_HASH(), INITIAL_TARGET())]),
    seen     : HashMap::from([]),
    tip      : (U256::from(0), ZERO_HASH()),
    mined    : HashMap::new(),
    pool     : PriorityQueue::new(),
  };
}

pub fn add_block(node: &mut NodeState, block: &Block, current_time: u64) {
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
      node.work.insert(bhash, U256::from(0 as u64)); // inits the work attr
      node.height.insert(bhash, 0); // inits the height attr
      node.target.insert(bhash, U256::from(0 as u64)); // inits the target attr
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
          let next_target = compute_next_target(last_target, U256::from(next_scaler));
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
    node.seen.insert(bhash, true);
  }
}

pub fn get_longest_chain(node: &NodeState) -> Vec<Block> {
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

// Stringification
// ===============

pub fn show_address_hostname(address: &Address) -> String {
  match address {
    Address::IPv4{ val0, val1, val2, val3, port } => {
      return format!("{}.{}.{}.{}", val0, val1, val2, val3);
    }
  }
}

pub fn show_block(chain: &NodeState, block: &Block, index: usize) -> String {
  let zero = U256::from(0);
  let bhash = hash_block(block);
  let work = chain.work.get(&bhash).unwrap_or(&zero);
  let show_index = format!("{}", index);
  let show_time = format!("{}", (block.time >> 192));
  let show_body = format!("{}", hex::encode(block.body.value));
  let show_hash = format!("{}", bhash);
  let show_work = format!("{}", work);
  return format!("{} | {} | {} | {} | {}", show_index, show_time, show_hash, show_body, show_work);
}

pub fn show_chain(node: &NodeState, lines: usize) -> String {
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

// Networking
// ==========

fn udp_init() -> UdpSocket {
  UdpSocket::bind("127.0.0.1:21000").expect("Can't bind socket.")
}

fn udp_send(socket: &mut UdpSocket, address: Address, message: Message) {
  match address {
    Address::IPv4 { val0, val1, val2, val3, port } => {
      let bits = bitvec_to_bytes(&serialized_message(message));
      let addr = SocketAddrV4::new(Ipv4Addr::new(val0, val1, val2, val3), port);
      socket.send_to(bits.as_slice(), addr).ok();
    }
  }
}

fn udp_receive(socket: &mut UdpSocket) -> Option<Message> {
  let mut buffer = [0; 65536];
  match socket.recv_from(&mut buffer) {
    Err(err) => {
      None
    }
    Ok((msg_len, sender_addr)) => {
      let bits = BitVec::from_bytes(&buffer[0 .. msg_len]);
      let msge = deserialized_message(&bits);
      Some(msge)
    }
  }
}
