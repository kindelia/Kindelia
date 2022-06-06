use bit_vec::BitVec;
use im::HashSet;
use json::object;
use primitive_types::U256;
use priority_queue::PriorityQueue;
use rand::seq::IteratorRandom;
use sha3::Digest;

use std::collections::HashMap;
use std::net::*;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};
use std::thread;
use std::sync::mpsc;
use std::sync::mpsc::{SyncSender, Receiver};
use futures::sync::oneshot;

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
  pub time: u128, // block timestamp
  pub rand: u128, // block nonce
  pub prev: U256, // previous block (32 bytes)
  pub body: Body, // block contents (1280 bytes)
}

pub type Transaction = Vec<u8>;

// TODO: refactor .block as map to struct? Better safety, less unwraps. Why not?
pub struct Node {
  pub path       : PathBuf,                         // path where files are saved
  pub socket     : UdpSocket,                       // UDP socket
  pub port       : u16,                             // UDP port
  pub tip        : U256,                            // current ti
  pub block      : U256Map<Block>,                  // block hash -> block information
  pub children   : U256Map<Vec<U256>>,              // block hash -> blocks that have this as its parent
  pub pending    : U256Map<Vec<Block>>,             // block hash -> blocks that are waiting for this block info
  pub work       : U256Map<U256>,                   // block hash -> accumulated work
  pub target     : U256Map<U256>,                   // block hash -> this block's target
  pub height     : U256Map<u128>,                   // block hash -> cached height
  pub seen       : U256Map<()>,                     // block hash -> have we received it yet?
  pub was_mined  : U256Map<HashSet<Transaction>>,   // block hash -> set of transaction hashes that were already mined
  pub pool       : PriorityQueue<Transaction,u128>, // transactions to be mined
  pub peer_id    : HashMap<Address, u128>,          // peer address -> peer id
  pub peers      : HashMap<u128, Peer>,             // peer id -> peer
  pub runtime    : Runtime,                         // Kindelia's runtime
  pub receiver   : Receiver<Request>,               // Receives an API request
}

// API request
pub enum Request {
  Double {
    value: u128,
    answer: oneshot::Sender<u128>,
  },
  GetTick {
    answer: oneshot::Sender<u128>,
  },
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

#[allow(clippy::large_enum_variant)]
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
    if let Ok(socket) = UdpSocket::bind(&format!("0.0.0.0:{}",port)) {
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

pub fn read_address(code: &str) -> Address {
  let strs = code.split(':').collect::<Vec<&str>>();
  let vals = strs[0].split('.').map(|o| o.parse::<u8>().unwrap()).collect::<Vec<u8>>();
  let port = strs[1].parse::<u16>().unwrap();
  Address::IPv4 {
    val0: vals[0],
    val1: vals[1],
    val2: vals[2],
    val3: vals[3],
    port: port,
  }
}

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
//pub fn string_to_body(text: &str) -> Body {
  //return bytes_to_body(text.as_bytes());
//}

pub fn bytes_to_body(bytes: &[u8]) -> Body {
  let mut body = Body { value: [0; BODY_SIZE] };
  let size = std::cmp::min(BODY_SIZE, bytes.len());
  body.value[..size].copy_from_slice(&bytes[..size]);
  return body;
}

pub fn code_to_body(code: &str) -> Body {
  let (_rest, acts) = crate::hvm::read_statements(code);
  let bits = serialized_statements(&acts);
  let body = bytes_to_body(&bitvec_to_bytes(&bits));
  return body;
}

pub fn body_to_string(body: &Body) -> String {
  match std::str::from_utf8(&body.value) {
    Ok(s)  => s.to_string(),
    Err(e) => "\n".repeat(BODY_SIZE),
  }
}

// Initial target of 256 hashes per block
pub fn INITIAL_TARGET() -> U256 {
  return difficulty_to_target(u256(INITIAL_DIFFICULTY));
}

pub fn ZERO_HASH() -> U256 {
  return hash_u256(u256(0));
}

pub fn GENESIS_BLOCK() -> Block {
  return Block {
    prev: ZERO_HASH(),
    time: 0,
    rand: 0,
    body: Body {
      value: [0; 1280]
    }
  }
}

pub fn new_node(kindelia_path: PathBuf) -> (SyncSender<Request>, Node) {
  let try_ports = [UDP_PORT, UDP_PORT + 1, UDP_PORT + 2];
  let (socket, port) = udp_init(&try_ports).expect("Couldn't open UDP socket.");
  let (query_sender, query_receiver) = mpsc::sync_channel(1);
  let mut node = Node {
    path       : kindelia_path,
    socket     : socket,
    port       : port,
    block      : HashMap::from([(ZERO_HASH(), GENESIS_BLOCK())]),
    children   : HashMap::from([(ZERO_HASH(), vec![])]),
    pending    : HashMap::new(),
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
    receiver   : query_receiver,
  };

  // UDP_PORT is the local port, it doesn't change existing node ports
  let default_peers: Vec<Address> = vec![
    "167.71.249.16:42000",
    "167.71.254.138:42000",
    "167.71.242.43:42000",
    "167.71.255.151:42000",
  ].iter().map(|x| read_address(x)).collect::<Vec<Address>>();

  let seen_at = get_time();
  default_peers.iter().for_each(|address| {
    return node_see_peer(&mut node, Peer { address: *address, seen_at });
  });

  node_see_peer(&mut node, Peer {
    address: Address::IPv4 { val0: 127, val1: 0, val2: 0, val3: 1, port: UDP_PORT },
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

  return (query_sender, node);
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
      node.peers.insert(index, peer);
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

// Registers a block on the node's database. This performs several actions:
// - If this block is too far into the future, ignore it.
// - If this block's parent isn't available:
//   - Add this block to the parent's pending list
//   - When the parent is available, register this block again
// - If this block's parent is available:
//   - Compute the block accumulated work, target, etc.
//   - If this block is the new tip:
//     - In case of a reorg, rollback to the block before it
//     - Run that block's code, updating the HVM state
//     - Updates the longest chain saved on disk
pub fn node_add_block(node: &mut Node, block: &Block) {
  // Adding a block might trigger the addition of other blocks
  // that were waiting for it. Because of that, we loop here.
  let mut must_add = vec![block.clone()]; // blocks to be added
  //println!("- add_block");
  // While there is a block to add...
  while let Some(block) = must_add.pop() {
    let btime = block.time; // the block timestamp
    //println!("- add block time={}", btime);
    // If block is too far into the future, ignore it
    if btime >= get_time() + DELAY_TOLERANCE {
      //println!("# new block: too late");
      continue;
    }
    let bhash = hash_block(&block); // hash of the block
    // If we already registered this block, ignore it
    if node.block.get(&bhash).is_some() {
      //println!("# new block: already in");
      continue;
    }
    let phash = block.prev; // hash of the previous block
    // If previous block is available, add the block to the chain
    if node.block.get(&phash).is_some() {
      //println!("- previous available");
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
        //println!("# new_block: enough work & advances_time");
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
          //println!("- hash: {:x}", bhash);
          //println!("- work: {}", node.work[&new_tip]);
          if true {
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
            // 3. Saves overwritten blocks to disk
            for bhash in must_compute.iter().rev() {
              let file_path = get_blocks_path(node).join(format!("{:0>32x}.kindelia_block.bin", node.height[bhash]));
              let file_buff = bitvec_to_bytes(&serialized_block(&node.block[bhash]));
              std::fs::write(file_path, file_buff).expect("Couldn't save block to disk.");
            }
            // 4. Reverts the runtime to a state older than that block
            //    On the example above, we'd find `runtime.tick = 1`
            let mut tick = node.height[&old_bhash];
            //println!("- tick: old={} new={}", node.runtime.get_tick(), tick);
            node.runtime.rollback(tick);
            // 5. Finds the last block included on the reverted runtime state
            //    On the example above, we'd find `new_bhash = B`
            while tick > node.runtime.get_tick() {
              must_compute.push(new_bhash);
              new_bhash = node.block[&new_bhash].prev;
              tick -= 1;
            }
            // 6. Computes every block after that on the new timeline
            //    On the example above, we'd compute `C, D, P, Q, R, S, T`
            for block in must_compute.iter().rev() {
              node_compute_block(node, &node.block[block].clone());
            }
          }
        }
      } else {
        //println!("# new_block: not enough work | not advances_time");
      }
      // Registers this block as a child of its parent
      node.children.insert(phash, vec![bhash]);
      // If there were blocks waiting for this one, mark them for addition
      for pending in node.pending.get(&bhash).unwrap_or(&vec![]) {
        must_add.push(pending.clone());
      }
      // Delete this block's pending vector
      if node.pending.contains_key(&bhash) {
        node.pending.remove(&bhash);
        //println!("- NOT PENDING ANYMORE {}", bhash);
      }
    // Otherwise (if previous block is unavailable), and if it is the
    // first time we see this block, add it to its parent's pending list
    } else if node.seen.get(&bhash).is_none() {
      //println!("# new block: previous unavailable");
      node.pending.insert(phash, vec![block.clone()]);
    }
    // Mark this block as seen
    node.seen.insert(bhash, ());
  }
}

pub fn node_compute_block(node: &mut Node, block: &Block) {
  let bits = BitVec::from_bytes(&block.body.value);
  let acts = deserialized_statements(&bits);
  //println!("Computing block:\n{}", view_statements(&acts));
  node.runtime.run_statements(&acts);
  node.runtime.tick();
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

pub fn node_handle_request(node: &mut Node, request: Request) {
  match request {
    Request::Double { value, answer } => {
      answer.send(value * 2).unwrap();
    }
    Request::GetTick { answer } => {
      answer.send(node.runtime.get_tick()).unwrap();
    }
  }
}

// Sends a block to a target address; also share some random peers
// FIXME: instead of sharing random peers, share recently active peers
pub fn node_send_block_to(node: &mut Node, addr: Address, block: Block) {
  //println!("- sending block: {:?}", block);
  let msg = Message::PutBlock {
    block: block,
    peers: get_random_peers(node, 3),
  };
  udp_send(&mut node.socket, addr, &msg);
}

pub fn node_message_handle(node: &mut Node, addr: Address, msg: &Message) {
  if addr != (Address::IPv4 { val0: 127, val1: 0, val2: 0, val3: 1, port: node.port }) {
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
        //println!("- getting block: {:?}", block);
        node_add_block(node, &block);
        //for peer in peers {
          //node_see_peer(node, *peer);
        //}
      }
    }
  }
}

pub fn gossip(node: &mut Node, peer_count: u128, message: &Message) {
  for peer in get_random_peers(node, peer_count) {
    udp_send(&mut node.socket, peer.address, message);
  }
}

pub fn get_blocks_path(node: &Node) -> PathBuf {
  node.path.join("state").join("blocks")
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
  //println!("- ask missing {}", node.pending.keys().len());
  for bhash in node.pending.keys().cloned().collect::<Vec<U256>>() {
    //println!("- ask missing {:x}", bhash);
    if let None = node.seen.get(&bhash) {
      gossip(node, MISSING_INFO_ASK_FACTOR, &Message::AskBlock { bhash });
    }
  }
}

fn node_load_blocks(node: &mut Node) {
  let blocks_dir = get_blocks_path(&node);
  std::fs::create_dir_all(&blocks_dir).ok();
  let mut file_paths : Vec<PathBuf> = vec![];
  for entry in std::fs::read_dir(&blocks_dir).unwrap() {
    file_paths.push(entry.unwrap().path());
  }
  file_paths.sort();
  for file_path in file_paths {
    let buffer = std::fs::read(file_path.clone()).unwrap();
    let block = deserialized_block(&bytes_to_bitvec(&buffer));
    println!("Adding block: {}", file_path.into_os_string().into_string().unwrap());
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

pub fn node_loop(
  mut node: Node,
  kindelia_path: PathBuf,
  miner_comm: SharedMinerComm,
  mine_file: Option<String>,
) -> ! {
  const TICKS_PER_SEC: u64 = 100;

  let mut tick: u64 = 0;
  let mut mined: u64 = 0;

  let init_body = code_to_body("");
  let mine_body = mine_file.map(|x| code_to_body(&x));

  // Loads all stored blocks
  eprintln!("Loading blocks from disk...");
  node_load_blocks(&mut node);

  loop {
    tick += 1;

    {
      // If the miner thread mined a block, gets and registers it
      if let MinerComm::Answer { block } = read_miner_comm(&miner_comm) {
        mined += 1;
        node_add_block(&mut node, &block);
      }

      // Spreads the tip block
      if tick % 2 == 0 {
        node_gossip_tip_block(&mut node, 8);
      }

      // Receives and handles incoming API requests
      if tick % 10 == 0 {
        if let Ok(request) = node.receiver.try_recv() {
          node_handle_request(&mut node, request);
        }
      }

      // Receives and handles incoming network messages
      if tick % 10 == 0 {
        node_message_receive(&mut node);
      }

      // Requests missing blocks
      if tick % 2 == 0 {
        node_ask_missing_blocks(&mut node);
      }

      // Asks the miner to mine a block
      if tick % 10 == 0 {
        // This branch is here for testing purposes. FIXME: remove
        if node.tip == ZERO_HASH() {
          node_ask_mine(&mut node, &miner_comm, init_body.clone());
        } else {
          if let Some(mine_body) = &mine_body {
            node_ask_mine(&mut node, &miner_comm, mine_body.clone()); // FIXME: avoid clone
          }
        }
      }

      // Peer timeout
      if tick % (10 * TICKS_PER_SEC) == 0 {
        node_peers_timeout(&mut node);
      }

      // Display node info
      if tick % TICKS_PER_SEC == 0 {
        log_heartbeat(&node);
      }
    }

    // Sleep for 1/100 seconds
    // TODO: just sleep remaining time <- good idea
    std::thread::sleep(std::time::Duration::from_micros(10000));
  }
}

fn log_heartbeat(node: &Node) {
  let blocks_num = node.block.keys().count();

  let tip = node.tip;
  let tip_height = *node.height.get(&tip).unwrap() as u64;

  let tip_target = *node.target.get(&tip).unwrap();
  let difficulty = target_to_difficulty(tip_target);
  let hash_rate = difficulty * u256(1000) / u256(TIME_PER_BLOCK);

  let mut pending_num: u64 = 0;
  let mut pending_seen_num: u64 = 0;
  for (bhash, _) in node.pending.iter() {
    if node.seen.get(bhash).is_some() {
      pending_seen_num += 1;
    }
    pending_num += 1;
  }
  //let last_blocks = get_longest_chain(node);

  let log = object!{
    event: "heartbeat",
    num_peers: node.peers.len(),
    tip: {
      height: tip_height,
      // target: u256_to_hex(tip_target),
      difficulty: difficulty.low_u64(),
      hash_rate: hash_rate.low_u64(),
    },
    blocks: {
      num: blocks_num,
      pending: { num: pending_num, seen: { num: pending_seen_num }, },
    },
    total_mana: node.runtime.get_mana() as u64,
  };

  println!("{}", log);
}
