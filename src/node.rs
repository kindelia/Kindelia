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
use tokio::sync::oneshot;

use crate::util::*;
use crate::bits::*;
use crate::hvm::{self,*};

// Types
// -----

// TODO: store number of used bits
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
// TODO: dashmap?
//
// Blocks have 4 states of inclusion:
//
//   has wait_list? | is on .waiting? | is on .block? | meaning
//   -------------- | --------------- | ------------- | ------------------------------------------------------
//   no             | no              | no            | unseen   : never seen, may not exist
//   yes            | no              | no            | missing  : some block cited it, but it wasn't downloaded
//   yes            | yes             | no            | pending  : downloaded, but waiting ancestors for inclusion
//   no             | yes             | yes           | included : fully included, as well as all its ancestors
//
pub struct Node {
  pub path       : PathBuf,                          // path where files are saved
  pub socket     : UdpSocket,                        // UDP socket
  pub port       : u16,                              // UDP port
  pub tip        : U256,                             // current tip
  pub block      : U256Map<Block>,                   // block_hash -> block
  pub waiting    : U256Map<Block>,                   // block_hash -> downloaded block, waiting for ancestors
  pub wait_list  : U256Map<Vec<U256>>,               // block_hash -> hashes of blocks that are waiting for this one
  pub children   : U256Map<Vec<U256>>,               // block_hash -> hashes of this block's children
  pub work       : U256Map<U256>,                    // block_hash -> accumulated work
  pub target     : U256Map<U256>,                    // block_hash -> this block's target
  pub height     : U256Map<u128>,                    // block_hash -> cached height
  pub results    : U256Map<Vec<StatementResult>>,    // block_hash -> results of the statements in this block
  pub was_mined  : U256Map<HashSet<Transaction>>,    // block_hash -> set of transaction hashes that were already mined
  pub pool       : PriorityQueue<Transaction, u128>, // transactions to be mined
  pub peer_id    : HashMap<Address, u128>,           // peer address -> peer id
  pub peers      : HashMap<u128, Peer>,              // peer id -> peer
  pub peer_idx   : u128,                             // peer id counter
  pub runtime    : Runtime,                          // Kindelia's runtime
  pub receiver   : Receiver<Request>,                // Receives an API request
}

// API
// ===

#[derive(Debug)]
pub struct BlockInfo {
  pub block: Block,
  pub hash: U256,
  pub height: u64,
  pub results: Vec<hvm::StatementResult>,
  pub content: Vec<hvm::Statement>,
}

type RequestAnswer<T> = oneshot::Sender<T>;

// TODO: store and serve tick where stuff where last changed
// TODO: interaction API
pub enum Request {
  GetTick {
    tx: RequestAnswer<u128>,
  },
  GetBlock {
    hash: U256,
    tx: RequestAnswer<Option<BlockInfo>>,
  },
  GetBlocks {
    range: (i64, i64),
    tx: RequestAnswer<Vec<BlockInfo>>,
  },
  GetFunctions {
    tx: RequestAnswer<HashSet<u64>>,
  },
  GetFunction {
    name: u128,
    tx: RequestAnswer<u128>,
  },
  GetState {
    name: u128,
    tx: RequestAnswer<Option<Term>>,
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

pub type SharedMinerComm = Arc<Mutex<MinerComm>>;

#[allow(clippy::large_enum_variant)]
#[derive(Debug, Clone)]
pub enum Message {
  PutBlock {
    block: Block,
    istip: bool,
    peers: Vec<Peer>,
  },
  AskBlock {
    bhash: Hash
  },
  MineTransaction {
    trans: Transaction
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

// How many times the mining thread attempts before unblocking?
pub const MINE_ATTEMPTS : u128 = 1024;

// Desired average time between mined blocks, in milliseconds
pub const TIME_PER_BLOCK : u128 = 3000;

// Don't accept blocks from N milliseconds in the future
pub const DELAY_TOLERANCE : u128 = 60 * 60 * 1000;
  
// Readjust difficulty every N blocks
pub const BLOCKS_PER_PERIOD : u128 = 20;

// How many ancestors do we send together with the requested missing block
pub const SEND_BLOCK_ANCESTORS : u128 = 64; // FIXME: not working properly; crashing the receiver node when big

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
  let (_rest, acts) = crate::hvm::read_statements(code).unwrap(); // TODO: handle error
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
  return hash_u256(u256(0)); // why though
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
// ------

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

// Creates a shared MinerComm object
pub fn new_miner_comm() -> SharedMinerComm {
  Arc::new(Mutex::new(MinerComm::Stop))
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
// ----

impl Node {
  pub fn new(kindelia_path: PathBuf) -> (SyncSender<Request>, Self) {
    let try_ports = [UDP_PORT, UDP_PORT + 1, UDP_PORT + 2];
    let (socket, port) = udp_init(&try_ports).expect("Couldn't open UDP socket.");
    let (query_sender, query_receiver) = mpsc::sync_channel(1);
    let mut node = Node {
      path       : kindelia_path,
      socket     : socket,
      port       : port,
      block      : HashMap::from([(ZERO_HASH(), GENESIS_BLOCK())]),
      waiting    : HashMap::new(),
      wait_list  : HashMap::new(),
      children   : HashMap::from([(ZERO_HASH(), vec![])]),
      work       : HashMap::from([(ZERO_HASH(), u256(0))]),
      height     : HashMap::from([(ZERO_HASH(), 0)]),
      target     : HashMap::from([(ZERO_HASH(), INITIAL_TARGET())]),
      results    : HashMap::from([(ZERO_HASH(), vec![])]),
      tip        : ZERO_HASH(),
      was_mined  : HashMap::new(),
      pool       : PriorityQueue::new(),
      peer_id    : HashMap::new(),
      peers      : HashMap::new(),
      peer_idx   : 0,
      runtime    : init_runtime(),
      receiver   : query_receiver,
    };

    // TODO: move out to config file
    let default_peers: Vec<Address> = vec![
      "167.71.249.16:42000",
      "167.71.254.138:42000",
      "167.71.242.43:42000",
      "167.71.255.151:42000",
    ].iter().map(|x| read_address(x)).collect::<Vec<Address>>();

    let seen_at = get_time();
    default_peers.iter().for_each(|address| {
      return node.see_peer(Peer { address: *address, seen_at });
    });

    // TODO: For testing purposes. Remove later.
    for &peer_port in try_ports.iter() {
      if peer_port != port {
        let address = Address::IPv4 { val0: 127, val1: 0, val2: 0, val3: 1, port: peer_port };
        node.see_peer(Peer { address: address, seen_at })
      }
    }

    (query_sender, node)
  }

  pub fn see_peer(&mut self, peer: Peer) {
    match self.peer_id.get(&peer.address) {
      None => {
        // TODO: improve this spaghetti
        let index = self.peer_idx;
        self.peer_idx += 1;
        self.peers.insert(index, peer);
        self.peer_id.insert(peer.address, index);
      }
      Some(index) => {
        let old_peer = self.peers.get_mut(&index);
        if let Some(old_peer) = old_peer {
          old_peer.seen_at = peer.seen_at;
        }
      }
    }
  }

  pub fn del_peer(&mut self, addr: Address) {
    if let Some(index) = self.peer_id.get(&addr) {
      self.peers.remove(&index);
      self.peer_id.remove(&addr);
    }
  }

  pub fn get_random_peers(&mut self, amount: u128) -> Vec<Peer> {
    let amount = amount as usize;
    let mut rng = rand::thread_rng();
    self.peers.values().cloned().choose_multiple(&mut rng, amount)
  }

  // Registers a block on the node's database. This performs several actions:
  // - If this block is too far into the future, ignore it.
  // - If this block's parent isn't available:
  //   - Add this block to the parent's wait_list
  //   - When the parent is available, register this block again
  // - If this block's parent is available:
  //   - Compute the block accumulated work, target, etc.
  //   - If this block is the new tip:
  //     - In case of a reorg, rollback to the block before it
  //     - Run that block's code, updating the HVM state
  //     - Updates the longest chain saved on disk
  pub fn add_block(&mut self, block: &Block) {
    // Adding a block might trigger the addition of other blocks
    // that were waiting for it. Because of that, we loop here.
    let mut must_include = vec![block.clone()]; // blocks to be added
    //println!("- add_block");
    // While there is a block to add...
    while let Some(block) = must_include.pop() {
      let btime = block.time; // the block timestamp
      //println!("- add block time={}", btime);
      // If block is too far into the future, ignore it
      if btime >= get_time() + DELAY_TOLERANCE {
        //println!("# new block: too late");
        continue;
      }
      let bhash = hash_block(&block); // hash of the block
      // If we already registered this block, ignore it
      if self.block.get(&bhash).is_some() {
        //println!("# new block: already in");
        continue;
      }
      let phash = block.prev; // hash of the previous block
      // If previous block is available, add the block to the chain
      if self.block.get(&phash).is_some() {
        //println!("- previous available");
        let work = get_hash_work(bhash); // block work score
        self.block.insert(bhash, block.clone()); // inserts the block
        self.work.insert(bhash, u256(0)); // inits the work attr
        self.height.insert(bhash, 0); // inits the height attr
        self.target.insert(bhash, u256(0)); // inits the target attr
        self.children.insert(bhash, vec![]); // inits the children attrs
        // Checks if this block PoW hits the target
        let has_enough_work = bhash >= self.target[&phash];
        // Checks if this block's timestamp is larger than its parent's timestamp
        // Note: Bitcoin checks if it is larger than the median of the last 11 blocks; should we?
        let advances_time = btime > self.block[&phash].time;
        // If the PoW hits the target and the block's timestamp is valid...
        if has_enough_work && advances_time {
          //println!("# new_block: enough work & advances_time");
          self.work.insert(bhash, self.work[&phash] + work); // sets this block accumulated work
          self.height.insert(bhash, self.height[&phash] + 1); // sets this block accumulated height
          // If this block starts a new period, computes the new target
          if self.height[&bhash] > 0 && self.height[&bhash] > BLOCKS_PER_PERIOD && self.height[&bhash] % BLOCKS_PER_PERIOD == 1 {
            // Finds the checkpoint hash (hash of the first block of the last period)
            let mut checkpoint_hash = phash;
            for _ in 0 .. BLOCKS_PER_PERIOD - 1 {
              checkpoint_hash = self.block[&checkpoint_hash].prev;
            }
            // Computes how much time the last period took to complete
            let period_time = btime - self.block[&checkpoint_hash].time;
            // Computes the target of this period
            let last_target = self.target[&phash];
            let next_scaler = 2u128.pow(32) * TIME_PER_PERIOD / period_time;
            let next_target = compute_next_target(last_target, u256(next_scaler));
            // Sets the new target
            self.target.insert(bhash, next_target);
          // Otherwise, keep the old target
          } else {
            self.target.insert(bhash, self.target[&phash]);
          }
          // Updates the tip work and block hash
          let old_tip = self.tip;
          let new_tip = bhash;
          if self.work[&new_tip] > self.work[&old_tip] {
            self.tip = bhash;
            //println!("- hash: {:x}", bhash);
            //println!("- work: {}", self.work[&new_tip]);
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
              while self.height[&new_bhash] > self.height[&old_bhash] {
                must_compute.push(new_bhash);
                new_bhash = self.block[&new_bhash].prev;
              }
              while self.height[&old_bhash] > self.height[&new_bhash] {
                old_bhash = self.block[&old_bhash].prev;
              }
              // 2. Finds highest block with same value on both timelines
              //    On the example above, we'd have `D`
              while old_bhash != new_bhash {
                must_compute.push(new_bhash);
                old_bhash = self.block[&old_bhash].prev;
                new_bhash = self.block[&new_bhash].prev;
              }
              // 3. Saves overwritten blocks to disk
              for bhash in must_compute.iter().rev() {
                let file_path = self.get_blocks_path().join(format!("{:0>32x}.kindelia_block.bin", self.height[bhash]));
                let file_buff = bitvec_to_bytes(&serialized_block(&self.block[bhash]));
                std::fs::write(file_path, file_buff).expect("Couldn't save block to disk.");
              }
              // 4. Reverts the runtime to a state older than that block
              //    On the example above, we'd find `runtime.tick = 1`
              let mut tick = self.height[&old_bhash];
              //println!("- tick: old={} new={}", self.runtime.get_tick(), tick);
              self.runtime.rollback(tick);
              // 5. Finds the last block included on the reverted runtime state
              //    On the example above, we'd find `new_bhash = B`
              while tick > self.runtime.get_tick() {
                must_compute.push(new_bhash);
                new_bhash = self.block[&new_bhash].prev;
                tick -= 1;
              }
              // 6. Computes every block after that on the new timeline
              //    On the example above, we'd compute `C, D, P, Q, R, S, T`
              for block in must_compute.iter().rev() {
                self.compute_block(&self.block[block].clone());
              }
            }
          }
        }
        // Registers this block as a child of its parent
        self.children.insert(phash, vec![bhash]);
        // If there were blocks waiting for this one, include them on the next loop
        // This will cause the block to be moved from self.waiting to self.block
        if let Some(wait_list) = self.wait_list.get(&bhash) {
          for waiting in wait_list {
            must_include.push(self.waiting.remove(waiting).expect("block"));
          }
          self.wait_list.remove(&bhash);
        }
      // Otherwise, include this block on .waiting, and on its parent's wait_list
      } else if self.waiting.get(&bhash).is_none() {
        self.waiting.insert(bhash, block.clone());
        self.wait_list.insert(phash, vec![bhash]);
      }
    }
  }

  pub fn compute_block(&mut self, block: &Block) {
    let bits = BitVec::from_bytes(&block.body.value);
    let acts = deserialized_statements(&bits);
    //println!("Computing block:\n{}", view_statements(&acts));
    let res = self.runtime.run_statements(&acts, false);
    let bhash = hash_block(block);
    self.results.insert(bhash, res);
    self.runtime.tick();
  }

  pub fn get_longest_chain(&self, num: Option<usize>) -> Vec<U256> {
    let mut longest = Vec::new();
    let mut bhash = self.tip;
    let mut count = 0;
    while self.block.contains_key(&bhash) && bhash != ZERO_HASH() {
      let block = self.block.get(&bhash).unwrap();
      longest.push(bhash);
      bhash = block.prev;
      count += 1;
      if let Some(num) = num {
        if count >= num {
          break;
        }
      }
    }
    longest.reverse();
    return longest;
  }

  pub fn receive_message(&mut self) {
    for (addr, msg) in udp_receive(&mut self.socket) {
      self.handle_message(addr, &msg);
    }
  }

  pub fn get_block_info(&self, hash: &U256) -> Option<BlockInfo> {
    let block = self.block.get(hash)?;
    let height = self.height.get(hash).expect("Missing block height.");
    let height: u64 = (*height).try_into().expect("Block height is too big.");
    let results = self.results.get(hash).expect("Missing block result.").clone();
    let bits = crate::bits::BitVec::from_bytes(&block.body.value);
    let content = crate::bits::deserialize_statements(&bits, &mut 0);
    let info = BlockInfo {
      block: block.clone(),
      hash: *hash,
      height,
      results,
      content,
    };
    Some(info)
  }

  pub fn handle_request(&mut self, request: Request) {
    // TODO: handle unwraps
    match request {
      Request::GetTick { tx: answer } => {
        answer.send(self.runtime.get_tick()).unwrap();
      }
      Request::GetBlocks { range, tx: answer } => {
        let (start, end) = range;
        debug_assert!(start <= end);
        debug_assert!(end == -1);
        let num = (end - start + 1) as usize;
        let hashes = self.get_longest_chain(Some(num));
        let infos = hashes.iter()
          .map(|h| 
            self.get_block_info(h).expect("Missing block.")
          ).collect();
        answer.send(infos).unwrap();
      },
      Request::GetBlock { hash, tx: answer } => {
        // TODO: actual indexing
        let info = self.get_block_info(&hash);
        answer.send(info).unwrap();
      },
      Request::GetFunctions { tx } => {
        let mut funcs: HashSet<u64> = HashSet::new();
        self.runtime.reduce_with(&mut funcs, |acc, heap| {
          for func in heap.disk.links.keys() {
            acc.insert(*func);
          }
        });
        tx.send(funcs).unwrap();
      },
      Request::GetFunction { name, tx: answer } => todo!(),
      Request::GetState { name, tx: answer } => {
        let state = self.runtime.read_disk_as_term(name);
        answer.send(state).unwrap();
      },
    }
  }

  // Sends a block to a target address; also share some random peers
  // FIXME: instead of sharing random peers, share recently active peers
  pub fn send_block_to(&mut self, addr: Address, block: Block, istip: bool) {
    //println!("- sending block: {:?}", block);
    let msg = Message::PutBlock {
      block: block,
      istip: istip,
      peers: self.get_random_peers(3),
    };
    udp_send(&mut self.socket, addr, &msg);
  }

  pub fn handle_message(&mut self, addr: Address, msg: &Message) {
    if addr != (Address::IPv4 { val0: 127, val1: 0, val2: 0, val3: 1, port: self.port }) {
      self.see_peer(Peer { address: addr, seen_at: get_time() });
      match msg {
        // Someone asked a block
        Message::AskBlock { bhash } => {
          // Sends the requested block, plus some of its ancestors
          let mut bhash = bhash;
          let mut chunk = vec![];
          while self.block.contains_key(&bhash) && *bhash != ZERO_HASH() && chunk.len() < SEND_BLOCK_ANCESTORS as usize {
            chunk.push(self.block[bhash].clone());
            bhash = &self.block[bhash].prev;
          }
          for block in chunk {
            self.send_block_to(addr, block.clone(), false);
          }
        }
        // Someone sent us a block
        Message::PutBlock { block, istip, peers } => {
          // Adds the block to the database
          self.add_block(&block);

          // Previously, we continuously requested missing blocks to neighbors. Now, we removed such
          // functionality. Now, when we receive a tip, we find the first missing ancestor, and
          // immediately ask it to the node that send that tip. That node, then, will send the
          // missing block, plus a few of its ancestors. This massively improves the amount of time
          // it will take to download all the missing blocks, and works in any situation. The only
          // problem is that, since we're not requesting missing blocks continuously, then, if the
          // packet where we ask the last missing ancestor is dropped, then we will never ask it
          // again. It will be missing forever. But that does not actually happen, because nodes are
          // constantly broadcasting their tips. So, if this packet is lost, we just wait until the
          // tip is received again, which will cause us to ask for that missing ancestor! In other
          // words, the old functionality of continuously requesting missing blocks was redundant and
          // detrimental. Note that the loop below is slightly CPU hungry, since it requires
          // traversing the whole history every time we receive the tip. As such, we don't do it when
          // the received tip is included on .block, which means we already have all its ancestors.
          // FIXME: this opens up a DoS vector where an attacker creates a very long chain, and sends
          // its tip to us, including all the ancestors, except the block #1. He then spam-sends the
          // same tip over and over. Since we'll never get the entire chain, we'll always run this
          // loop fully, exhausting this node's CPU resources. This isn't a very serious attack, but
          // there are some solutions, which might be investigated in a future.
          if *istip {
            let bhash = hash_block(&block);
            if !self.block.contains_key(&bhash) {
              let mut missing = bhash;
              // Finds the first ancestor that wasn't downloaded yet
              let mut count = 0;
              while self.waiting.contains_key(&missing) {
                count += 1;
                missing = self.waiting[&missing].prev;
              }
              println!("ask missing: {} {:x}", count, missing);
              udp_send(&mut self.socket, addr, &Message::AskBlock { bhash: missing })
            }
          }
        }
        // Someone sent us a transaction to mine
        Message::MineTransaction { trans } => {
          self.pool.push(trans.clone(), (hash_bytes(trans) >> 128).low_u128());
        }
      }
    }
  }

  pub fn gossip(&mut self, peer_count: u128, message: &Message) {
    for peer in self.get_random_peers(peer_count) {
      udp_send(&mut self.socket, peer.address, message);
    }
  }

  pub fn get_blocks_path(&self) -> PathBuf {
    self.path.join("state").join("blocks")
  }

  fn gossip_tip_block(&mut self, peer_count: u128) {
    let random_peers = self.get_random_peers(peer_count);
    for peer in random_peers {
      self.send_block_to(peer.address, self.block[&self.tip].clone(), true);
    }
  }

  fn peers_timeout(&mut self) {
    let mut forget = Vec::new();
    for (id,peer) in &self.peers {
      //println!("... {} < {} {}", peer.seen_at, get_time() - PEER_TIMEOUT, peer.seen_at < get_time() - PEER_TIMEOUT);
      if peer.seen_at < get_time() - PEER_TIMEOUT {
        forget.push(peer.address);
      }
    }
    for addr in forget {
      self.del_peer(addr);
    }
  }

  fn load_blocks(&mut self) {
    let blocks_dir = self.get_blocks_path();
    std::fs::create_dir_all(&blocks_dir).ok();
    let mut file_paths : Vec<PathBuf> = vec![];
    for entry in std::fs::read_dir(&blocks_dir).unwrap() {
      file_paths.push(entry.unwrap().path());
    }
    file_paths.sort();
    println!("Loading {} blocks from disk...", file_paths.len());
    for file_path in file_paths {
      let buffer = std::fs::read(file_path.clone()).unwrap();
      let block = deserialized_block(&bytes_to_bitvec(&buffer));
      self.add_block(&block);
    }
  }

  fn ask_mine(&self, miner_comm: &SharedMinerComm, body: Body) {
    write_miner_comm(miner_comm, MinerComm::Request {
      prev: self.tip,
      body,
      targ: self.get_tip_target(),
    });
  }

  pub fn main(
    mut self,
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
    println!("Port: {}", self.port);
    if self.port == 42000 { // for debugging, won't load blocks if it isn't the main self. FIXME: remove
      self.load_blocks();
    }

    #[allow(clippy::modulo_one)]
    loop {
      tick += 1;

      {

        // If the miner thread mined a block, gets and registers it
        if let MinerComm::Answer { block } = read_miner_comm(&miner_comm) {
          mined += 1;
          self.add_block(&block);
        }

        // Spreads the tip block
        if tick % 10 == 0 {
          self.gossip_tip_block(8);
        }

        // Receives and handles incoming API requests
        if tick % 5 == 0 {
          if let Ok(request) = self.receiver.try_recv() {
            self.handle_request(request);
          }
        }

        // Receives and handles incoming network messages
        // if tick % 1 == 0 {
        self.receive_message();
        // }

        // Asks the miner thread to mine a block
        if tick % 10 == 0 {
          // This branch is here for testing purposes. FIXME: remove
          if let Some(mine_body) = &mine_body {
            self.ask_mine(&miner_comm, mine_body.clone()); // FIXME: avoid clone
          }
        }

        // Peer timeout
        if tick % (10 * TICKS_PER_SEC) == 0 {
          self.peers_timeout();
        }

        // Display self info
        if tick % TICKS_PER_SEC == 0 {
          self.log_heartbeat();
        }
      }

      // Sleep for 1/100 seconds
      // TODO: just sleep remaining time <- good idea
      std::thread::sleep(std::time::Duration::from_micros(1000000 / TICKS_PER_SEC));
    }
  }

  fn log_heartbeat(&self) {

    let tip = self.tip;
    let tip_height = *self.height.get(&tip).unwrap() as u64;

    let tip_target = *self.target.get(&tip).unwrap();
    let difficulty = target_to_difficulty(tip_target);
    let hash_rate = difficulty * u256(1000) / u256(TIME_PER_BLOCK);

    // Counts missing, pending and included blocks
    let included_count = self.block.keys().count();
    let mut missing_count: u64 = 0;
    let mut pending_count: u64 = 0;
    for (bhash, _) in self.wait_list.iter() {
      if self.waiting.get(bhash).is_some() {
        pending_count += 1;
      }
      missing_count += 1;
    }

    let log = object!{
      event: "heartbeat",
      peers: self.peers.len(),
      tip: {
        height: tip_height,
        // target: u256_to_hex(tip_target),
        difficulty: difficulty.low_u64(),
        hash_rate: hash_rate.low_u64(),
      },
      blocks: {
        missing: missing_count,
        pending: pending_count,
        included: included_count,
      },
      total_mana: self.runtime.get_mana() as u64,
    };

    println!("{}", log);
  }

  // Get the current target
  pub fn get_tip_target(&self) -> U256 {
    self.target[&self.tip]
  }

}
