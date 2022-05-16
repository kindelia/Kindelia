use bit_vec::BitVec;
use im::HashSet;
use pad::{PadStr, Alignment};
use primitive_types::U256;
use priority_queue::PriorityQueue;
use rand::seq::IteratorRandom;
use sha3::Digest;

use std::collections::HashMap;
use std::net::*;
use std::sync::{Arc, Mutex};
use std::thread;

use std::io::{stdin, stdout, Write};
use termion::event::Key;
use termion::input::TermRead;
use termion::raw::IntoRawMode;
use termion::{color, style};

use crate::util::*;
use crate::bits::*;
use crate::hvm::*;

// Types
// -----

#[derive(Debug, Clone, PartialEq)]
pub struct Body {
  pub value: [u8; BODY_SIZE],
}

#[derive(Debug, Clone)]
pub struct Block {
  pub time: u128,  // block timestamp
  pub rand: u128,  // block nonce
  pub prev: U256, // previous block (32 bytes)
  pub body: Body, // block contents (1280 bytes)
}

pub type Transaction = Vec<u8>;

pub struct Node {
  pub socket     : UdpSocket,                       // UDP socket
  pub port       : u16,                             // UDP port
  pub tip        : U256,                            // current ti
  pub block      : U256Map<Block>,                  // block hash -> block information
  pub children   : U256Map<Vec<U256>>,              // block hash -> blocks that have this as its parent
  pub waiters    : U256Map<Vec<Block>>,             // block hash -> blocks that are waiting for this block info
  pub work       : U256Map<U256>,                   // block hash -> accumulated work
  pub target     : U256Map<U256>,                   // block hash -> this block's target
  pub height     : U256Map<u128>,                   // block hash -> cached height
  pub seen       : U256Map<()>,                     // block hash -> have we received it yet?
  pub was_mined  : U256Map<HashSet<Transaction>>,   // block hash -> set of transaction hashes that were already mined
  pub pool       : PriorityQueue<Transaction,u128>, // transactions to be mined
  pub peer_id    : HashMap<Address, u128>,          // peer address -> peer id
  pub peers      : HashMap<u128, Peer>,             // peer id -> peer
  pub runtime    : Runtime,                         // Kindelia's runtime
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
  Stop
}

pub type Shared<T> = Arc<Mutex<T>>;
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
  }
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
  pub seen_at: u128,
  pub address: Address,
}

// Constants
// =========

// UDP port to listen to
pub const UDP_PORT : u16 = 42000;

// Size of a hash, in bytes
pub const HASH_SIZE : usize = 32;

// Size of a u128, in bytes
pub const U128_SIZE : usize = 64 / 8;

// Size of a block's body, in bytes
pub const BODY_SIZE : usize = 1280;

// Size of a block, in bytes
pub const BLOCK_SIZE : usize = HASH_SIZE + (U128_SIZE * 4) + BODY_SIZE;

// Size of an IPv4 address, in bytes
pub const IPV4_SIZE : usize = 4;

// Size of an IPv6 address, in bytes
pub const IPV6_SIZE : usize = 16;

// Size of an IP port, in bytes
pub const PORT_SIZE : usize = 2;

// How many nodes we gossip an information to?
pub const GOSSIP_FACTOR : u128 = 16;

// When we need missing info, how many nodes do we ask?
pub const MISSING_INFO_ASK_FACTOR : u128 = 3;

// How many times the mining thread attempts before unblocking?
pub const MINE_ATTEMPTS : u128 = 1024;

// Desired average time between mined blocks, in milliseconds
pub const TIME_PER_BLOCK : u128 = 3000;

// Don't accept blocks from N milliseconds in the future
pub const DELAY_TOLERANCE : u128 = 60 * 60 * 1000;
  
// Readjust difficulty every N blocks
pub const BLOCKS_PER_PERIOD : u128 = 20;

// Readjusts difficulty every N seconds
pub const TIME_PER_PERIOD : u128 = TIME_PER_BLOCK * BLOCKS_PER_PERIOD;

// Initial difficulty, in expected hashes per block
pub const INITIAL_DIFFICULTY : u128 = 256;

// How many milliseconds without notice until we forget a peer?
pub const PEER_TIMEOUT : u128 = 10 * 1000;

// How many peers we need to keep minimum?
pub const PEER_COUNT_MINIMUM : u128 = 256;

// How many peers we send when asked?
pub const SHARE_PEER_COUNT : u128 = 3;

// How many peers we keep on the last_seen object?
pub const LAST_SEEN_SIZE : u128 = 2;

// UDP
// ===

pub fn ipv4(val0: u8, val1: u8, val2: u8, val3: u8, port: u16) -> Address {
  Address::IPv4 { val0, val1, val2, val3, port }
}

pub fn udp_init(ports: &[u16]) -> Option<(UdpSocket,u16)> {
  for port in ports {
    if let Ok(socket) = UdpSocket::bind(&format!("127.0.0.1:{}",port)) {
      socket.set_nonblocking(true).ok();
      return Some((socket, *port));
    }
  }
  return None;
}

pub fn udp_send(socket: &mut UdpSocket, address: Address, message: &Message) {
  match address {
    Address::IPv4 { val0, val1, val2, val3, port } => {
      let bits = bitvec_to_bytes(&serialized_message(message));
      let addr = SocketAddrV4::new(Ipv4Addr::new(val0, val1, val2, val3), port);
      socket.send_to(bits.as_slice(), addr).ok();
    }
  }
}

pub fn udp_receive(socket: &mut UdpSocket) -> Vec<(Address, Message)> {
  let mut buffer = [0; 65536];
  let mut messages = Vec::new();
  while let Ok((msg_len, sender_addr)) = socket.recv_from(&mut buffer) {
    let bits = BitVec::from_bytes(&buffer[0 .. msg_len]);
    let msge = deserialized_message(&bits);
    let addr = match sender_addr.ip() {
      std::net::IpAddr::V4(v4addr) => {
        let [val0, val1, val2, val3] = v4addr.octets();
        Address::IPv4 { val0, val1, val2, val3, port: sender_addr.port() }
      }
      _ => {
        panic!("TODO: IPv6")
      }
    };
    messages.push((addr, msge));
  }
  return messages;
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

// Algorithms
// ----------

pub fn target_to_difficulty(target: U256) -> U256 {
  let p256 = U256::from("0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF");
  return p256 / (p256 - target);
}

pub fn difficulty_to_target(difficulty: U256) -> U256 {
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
  let last_difficulty = target_to_difficulty(last_target);
  let next_difficulty = u256(1) + (last_difficulty * scale - u256(1)) / p32;
  return difficulty_to_target(next_difficulty);
}

pub fn compute_next_target_f64(last_target: U256, scale: f64) -> U256 {
  return compute_next_target(last_target, u256((scale * 4294967296.0) as u128));
}

pub fn get_hash_work(hash: U256) -> U256 {
  if hash == u256(0) {
    return u256(0);
  } else {
    return target_to_difficulty(hash);
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
    bytes.extend_from_slice(&u128_to_bytes(block.time));
    bytes.extend_from_slice(&u128_to_bytes(block.rand));
    bytes.extend_from_slice(&block.body.value);
    return hash_bytes(&bytes);
  }
}

// Converts a string to a body, terminating with a null character.
// Truncates if the string length is larger than BODY_SIZE-1.
pub fn string_to_body(text: &str) -> Body {
  let mut body = Body { value: [0; BODY_SIZE] };
  let bytes = text.as_bytes();
  for i in 0 .. std::cmp::min(BODY_SIZE, bytes.len()) {
    body.value[i] = bytes[i];
  }
  return body;
}

pub fn body_to_string(body: &Body) -> String {
  match std::str::from_utf8(&body.value) {
    Ok(s)  => s.to_string(),
    Err(e) => "\n".repeat(BODY_SIZE),
  }
}

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
    tip        : ZERO_HASH(),
    was_mined  : HashMap::new(),
    pool       : PriorityQueue::new(),
    peer_id    : HashMap::new(),
    peers      : HashMap::new(),
    runtime    : init_runtime(),
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
      let index = node.peers.len() as u128;
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

pub fn get_random_peers(node: &mut Node, amount: u128) -> Vec<Peer> {
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
          let next_scaler = 2u128.pow(32) * TIME_PER_PERIOD / period_time;
          let next_target = compute_next_target(last_target, u256(next_scaler));
          // Sets the new target
          node.target.insert(bhash, next_target);
        // Otherwise, keep the old target
        } else {
          node.target.insert(bhash, node.target[&phash]);
        }
        // Updates the tip work and block hash
        let old_tip = node.tip;
        let new_tip = bhash;
        if node.work[&new_tip] > node.work[&old_tip] {
          node.tip = bhash;
          // Block reorganization (* marks blocks for which we have runtime snapshots):
          // tick: |  0 | *1 |  2 |  3 |  4 | *5 |  6 | *7 | *8 |
          // hash: |  A |  B |  C |  D |  E |  F |  G |  H |    |  <- old timeline
          // hash: |  A |  B |  C |  D |  P |  Q |  R |  S |  T |  <- new timeline
          //               |         '-> highest common block shared by both timelines
          //               '-----> highest runtime snapshot before block D
          let mut must_compute = Vec::new();
          let mut old_bhash = old_tip;
          let mut new_bhash = new_tip;
          // 1. Finds the highest block with same height on both timelines
          //    On the example above, we'd have `H, S`
          while node.height[&new_bhash] > node.height[&old_bhash] {
            must_compute.push(new_bhash);
            new_bhash = node.block[&new_bhash].prev;
          }
          while node.height[&old_bhash] > node.height[&new_bhash] {
            old_bhash = node.block[&old_bhash].prev;
          }
          // 2. Finds highest block with same value on both timelines
          //    On the example above, we'd have `D`
          while old_bhash != new_bhash {
            must_compute.push(new_bhash);
            old_bhash = node.block[&old_bhash].prev;
            new_bhash = node.block[&new_bhash].prev;
          }
          // 3. Reverts the runtime to a state older than that block
          //    On the example above, we'd find `runtime.tick = 1`
          let mut tick = node.height[&old_bhash];
          node.runtime.rollback(tick);
          // 4. Finds the last block included on the reverted runtime state
          //    On the example above, we'd find `new_bhash = B`
          while tick > node.runtime.get_tick() {
            must_compute.push(new_bhash);
            new_bhash = node.block[&new_bhash].prev;
            tick -= 1;
          }
          // 5. Computes every block after that on the new timeline
          //    On the example above, we'd compute `C, D, P, Q, R, S, T`
          for block in must_compute.iter().rev() {
            node_compute_block(node, &node.block[block].clone());
          }
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

pub fn node_compute_block(node: &mut Node, block: &Block) {
  let bits = BitVec::from_bytes(&block.body.value);
  let acts = deserialized_actions(&bits);
  node.runtime.run_actions(&acts);
}

pub fn get_longest_chain(node: &Node) -> Vec<Block> {
  let mut longest = Vec::new();
  let mut bhash = node.tip;
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

pub fn gossip(node: &mut Node, peer_count: u128, message: &Message) {
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

pub fn show_block(block: &Block) -> String {
  let hash = hash_block(block);
  return format!(
    "time: {}\nrand: {}\nbody: {}\nprev: {}\nhash: {} ({})\n-----\n",
    block.time,
    block.rand,
    body_to_string(&block.body),
    block.prev,
    hex::encode(u256_to_bytes(hash)),
    get_hash_work(hash),
  );
}

// Mining
// ======

// Get the current target
pub fn get_tip_target(node: &Node) -> U256 {
  node.target[&node.tip]
}

// Given a target, attempts to mine a block by changing its nonce up to `max_attempts` times
pub fn try_mine(prev: U256, body: Body, targ: U256, max_attempts: u128) -> Option<Block> {
  let rand = rand::random::<u128>();
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

fn node_gossip_tip_block(node: &mut Node, peer_count: u128) {
  let random_peers = get_random_peers(node, peer_count);
  for peer in random_peers {
    node_send_block_to(node, peer.address, node.block[&node.tip].clone());
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
    prev: node.tip,
    body,
    targ: get_tip_target(node),
  });
}

fn node_handle_input(node: &Node, miner_comm: &SharedMinerComm, input: &str) {
  if let Some('\n') = input.chars().last() {
    let mine_body = input.replace("\n", "");
    let mine_body = string_to_body(&mine_body);
    node_ask_mine(node, miner_comm, mine_body);
  }
}

pub fn node_loop(
  mut node: Node,
  miner_comm: SharedMinerComm,
  input: SharedInput,
  ui: bool,
) {
  let mut mined = 0;
  let mut tick = 0;
  let mut last_screen: Option<Vec<String>> = None;

  loop {
    tick += 1;

    {
      // If the miner thread mined a block, gets and registers it
      if let MinerComm::Answer { block } = read_miner_comm(&miner_comm) {
        mined += 1;
        node_add_block(&mut node, &block);
      }

      // Spreads the tip block
      if tick % 1 == 0 {
        node_gossip_tip_block(&mut node, 8);
      }

      // Receives and handles incoming messages
      if tick % 10 == 0 {
        node_message_receive(&mut node);
      }

      // Requests missing blocks
      if tick % 1 == 0 {
        node_ask_missing_blocks(&mut node);
      }

      // Peer timeout
      if tick % 1000 == 0 {
        node_peers_timeout(&mut node);
      }

      // Asks the miner to mine a block
      if tick % 10 == 0 {
        let input = (input.lock().unwrap()).clone();
        node_handle_input(&node, &miner_comm, &input);
      }

      // Display node info
      if ui && tick % 10 == 0 {
        let input = (input.lock().unwrap()).clone();
        display_tui(&node_get_info(&node, None), &input, &mut last_screen);
      } else if tick % 60 == 0 {
        display_simple(&node_get_info(&node, None));
      }
    }

    // Sleep for 1/100 seconds
    //std::thread::sleep(std::time::Duration::from_micros(16666));
  }
}

// Interface
// =========

// Input
// -----

pub fn new_input() -> SharedInput {
  Arc::new(Mutex::new(String::new()))
}

pub fn input_loop(input: &SharedInput) {
  let mut stdout = stdout().into_raw_mode().unwrap();

  for key in stdin().keys() {
    //print!("got {:?}\n\r", key);
    if let Ok(Key::Char(chr)) = key {
      (input.lock().unwrap()).push(chr);
      stdout.flush().unwrap();
    }
    if let Ok(Key::Backspace) = key {
      (input.lock().unwrap()).pop();
      stdout.flush().unwrap();
    }
    if let Ok(Key::Ctrl('c')) = key {
      break;
    }
    if let Ok(Key::Ctrl('q')) = key {
      break;
    }
  }

  drop(stdout);
  eprintln!("\n\nBye...");
  std::process::exit(0);
}

// Output
// ------

pub struct NodeInfo {
  pub time: u128,
  pub num_peers: u128,
  pub num_blocks: u128,
  pub num_pending: u128,
  pub num_pending_seen: u128,
  pub height: u128,
  pub difficulty: U256,
  pub hash_rate: U256,
  pub tip_hash: U256,
  pub last_blocks: Vec<Block>,
}

fn node_get_info(node: &Node, max_last_blocks: Option<u128>) -> NodeInfo {
  let tip = node.tip;
  let height = *node.height.get(&tip).unwrap();
  let tip_target = *node.target.get(&tip).unwrap();
  let difficulty = target_to_difficulty(tip_target);
  let hash_rate = difficulty * u256(1000) / u256(TIME_PER_BLOCK);

  let mut num_pending: u128 = 0;
  let mut num_pending_seen: u128 = 0;
  for (bhash, _) in node.waiters.iter() {
    if node.seen.get(bhash).is_some() {
      num_pending_seen += 1;
    }
    num_pending += 1;
  }

  // TODO: max_last_blocks
  let last_blocks = get_longest_chain(node);

  NodeInfo {
    time: get_time(),
    num_peers: node.peers.len() as u128,
    num_blocks: node.block.len() as u128,
    num_pending,
    num_pending_seen,
    height,
    difficulty,
    hash_rate,
    tip_hash: tip,
    last_blocks,
  }
}

fn display_simple(info: &NodeInfo) {
  let time = get_time();
  println!("{{");
  println!(r#"  "time": "{}","#, time);
  println!(r#"  "num_peers: {},"#, info.num_peers);
  println!(r#"  "tip_hash": "{}","#, info.tip_hash);
  println!(r#"  "tip_height": {}"#, info.height);
  println!("}}");
}

#[allow(clippy::useless_format)]
pub fn display_tui(info: &NodeInfo, input: &str, last_screen: &mut Option<Vec<String>>) {
  //─━│┃┄┅┆┇┈┉┊┋┌┍┎┏┐┑┒┓└┕┖┗┘┙┚┛├┝┞┟┠┡┢┣┤┥┦┧┨┩┪┫┬┭┮┯┰┱┲┳┴┵┶┷┸┹┺┻┼┽┾┿╀╁╂╃╄╅╆╇╈╉╊╋╌╍╎╏═║╒╓╔╕╖╗╘╙╚╛╜╝╞╟╠╡╢╣╤╥╦╧╨╩╪╫╬╭╮╯╰╱╲╳╴╵╶╷╸╹╺╻╼╽╾

  fn bold(text: &str) -> String {
    return format!("{}{}{}", "\x1b[1m", text, "\x1b[0m");
  }

  fn display_block(block: &Block, index: usize, width: u16) -> String {
    let zero = u256(0);
    let b_hash = hash_block(block);
    let show_index = format!("{}", index).pad(6, ' ', Alignment::Right, true);
    let show_time = format!("{}", block.time).pad(13, ' ', Alignment::Left, true);
    let show_hash = format!("{}", hex::encode(u256_to_bytes(b_hash)));
    let show_body = format!(
      "{}",
      body_to_string(&block.body).pad(
        std::cmp::max(width as i32 - 110, 0) as usize,
        ' ',
        Alignment::Left,
        true
      )
    );
    return format!("{} | {} | {} | {}", show_index, show_time, show_hash, show_body);
  }

  fn display_input(input: &str) -> String {
    let mut input: String = input.to_string();
    if let Some('\n') = input.chars().last() {
      input = bold(&input);
    }
    input.replace('\n', "")
  }

  // Gets the terminal width and height
  let (width, height) = termion::terminal_size().unwrap();
  let menu_width = 17;

  let mut menu_lines: Vec<(String, bool)> = vec![];
  macro_rules! menu_block {
    ($title:expr, $val:expr) => {
      menu_lines.push((format!("{}", $title), true));
      menu_lines.push((format!("{}", $val), false));
      menu_lines.push((format!("{}", "-------------"), false));
    };
  }

  menu_block!("Time", get_time());
  menu_block!("Peers", info.num_peers);
  menu_block!("Difficulty", info.difficulty);
  menu_block!("Hash Rate", info.hash_rate);
  menu_block!("Blocks", info.num_blocks);
  menu_block!("Height", info.height);
  menu_block!("Pending", format!("{} / {}", info.num_pending_seen, info.num_pending));

  let mut body_lines: Vec<String> = vec![];
  let blocks = &info.last_blocks;
  body_lines.push(format!("#block | time          | hash                                                             | body "));
  body_lines.push(format!("------ | ------------- | ---------------------------------------------------------------- | {}", "-".repeat(std::cmp::max(width as i64 - menu_width as i64 - 93, 0) as usize)));
  let min = std::cmp::max(blocks.len() as i64 - (height as i64 - 5), 0) as usize;
  let max = blocks.len();
  for i in min..max {
    body_lines.push(display_block(&blocks[i as usize], i as usize, width));
  }

  let mut screen = Vec::new();

  // Draws top bar
  screen.push(format!("  ╻╻           │"));
  screen.push(format!(" ╻┣┓  {} │ {}", bold("Kindelia"), { display_input(input) }));
  screen.push(format!("╺┛╹┗╸──────────┼{}", "─".repeat(width as usize - 16)));

  // Draws each line
  for i in 0..height - 4 {
    let mut line = String::new();

    // Draws menu item
    line.push_str(&format!(" "));
    if let Some((menu_line, menu_bold)) = menu_lines.get(i as usize) {
      let text = menu_line.pad(menu_width - 4, ' ', Alignment::Left, true);
      let text = if *menu_bold { bold(&text) } else { text };
      line.push_str(&format!("{}", text));
    } else {
      line.push_str(&format!("{}", " ".repeat(menu_width - 4)));
    }

    // Draws separator
    line.push_str(&format!(" │ "));

    // Draws body item
    line.push_str(&format!("{}", body_lines.get(i as usize).unwrap_or(&"".to_string())));

    // Draws line break
    screen.push(line);
  }

  render(last_screen, &screen);
}

// Prints screen, only re-printing lines that change
fn render(old_screen: &mut Option<Vec<String>>, new_screen: &Vec<String>) {
  fn redraw(screen: &Vec<String>) {
    print!("{esc}c", esc = 27 as char); // clear screen
    for line in screen {
      print!("{}\n\r", line);
    }
  }
  match old_screen {
    None => redraw(new_screen),
    Some(old_screen) => {
      if old_screen.len() != new_screen.len() {
        redraw(new_screen);
      } else {
        for y in 0..new_screen.len() {
          if let (Some(old_line), Some(new_line)) = (old_screen.get(y), new_screen.get(y)) {
            if old_line != new_line {
              print!("{}", termion::cursor::Hide);
              print!("{}", termion::cursor::Goto(1, (y + 1) as u16));
              print!("{}", new_line);
              print!("{}", termion::clear::UntilNewline);
            }
          }
        }
      }
    }
  }
  std::io::stdout().flush().ok();
  *old_screen = Some(new_screen.clone());
}
