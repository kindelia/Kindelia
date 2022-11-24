#![allow(clippy::style)]

use std::collections::{HashMap, HashSet};
use std::ops::Deref;
use std::path::PathBuf;
use std::sync::{mpsc, Arc, Mutex};
use std::thread::JoinHandle;

use bit_vec::BitVec;
use primitive_types::U256;
use priority_queue::PriorityQueue;
use rand::seq::IteratorRandom;
use serde::{Deserialize, Serialize};
use sha3::Digest;
use thiserror::Error;

use kindelia_common::crypto::{self, Hashed, Keccakable};
use kindelia_common::Name;
use kindelia_lang::ast::Statement;
use kindelia_lang::parser;

use crate::api::{self, CtrInfo, RegInfo};
use crate::api::{BlockInfo, FuncInfo, NodeRequest};
use crate::bits;
use crate::bits::ProtoSerialize;
use crate::config::MineConfig;
use crate::constants;
use crate::hvm::*;
use crate::net::{ProtoAddr, ProtoComm};
use crate::persistence::BlockStorage;
use crate::util::*;

use crate::events::{NodeEventEmittedInfo, NodeEventType};
use crate::heartbeat;

macro_rules! emit_event {
  ($tx: expr, $event: expr) => {
    #[cfg(feature = "events")]
    if let Some(ref tx) = $tx {
      if let Err(_) = tx.send(($event, get_time_micro())) {
        eprintln!("Could not send event");
      }
    }
  };

  ($tx: expr, $event: expr, tags = $($tag:ident),+) => {
    #[cfg(feature = "events")]
    // #[cfg(any(all, $($tag),+))]
    if let Some(ref tx) = $tx {
      if let Err(_) = tx.send(($event, get_time_micro())) {
        eprintln!("Could not send event");
      }
    }
  };
}

// Block
// =====

// Kindelia's block format is agnostic to HVM. A Transaction is just a vector of
// bytes. A Body groups transactions in a single combined vector of bytes, using
// the following format:
//
//   body ::= TX_COUNT | LEN(tx_0) | tx_0 | LEN(tx_1) | tx_1 | ...
//
// TX_COUNT is a single byte storing the number of transactions in this block.
// The length of each transaction is stored using 2 bytes, called LEN.

// Transaction
// -----------

/// Represents transaction inside a block. It's a list of bytes with non-zero
/// multiple-of-5 number of bytes.
#[derive(Debug, Clone, Eq)]
pub struct Transaction {
  data: Vec<u8>,
  pub hash: U256,
}

/// Transaction's error handling
#[derive(Debug, Error, Serialize, Deserialize)]
pub enum TransactionError {
  #[error(
    "Transaction size ({len}) is greater than max transaction size ({})",
    MAX_TRANSACTION_SIZE
  )]
  Oversize { len: usize },
}

impl Transaction {
  pub fn new(mut data: Vec<u8>) -> Result<Self, TransactionError> {
    // Transaction length is always a non-zero multiple of 5
    while data.len() == 0 || data.len() % 5 != 0 {
      data.push(0);
    }
    if data.len() > MAX_TRANSACTION_SIZE {
      return Err(TransactionError::Oversize { len: data.len() });
    }
    let hash = hash_bytes(&data);
    Ok(Transaction { data, hash })
  }

  // Encodes a transaction length as a pair of 2 bytes
  pub fn encode_length(&self) -> (u8, u8) {
    let len = self.data.len() as u16;
    let num = (len as u16).reverse_bits();
    (((num >> 8) & 0xFF) as u8, (num & 0xFF) as u8)
  }

  // Decodes an encoded transaction length
  fn decode_length(pair: (u8, u8)) -> usize {
    (((pair.0 as u16) << 8) | (pair.1 as u16)).reverse_bits() as usize
  }

  pub fn to_statement(&self) -> Option<Statement> {
    Statement::proto_deserialized(&BitVec::from_bytes(&self.data))
  }
}

impl Deref for Transaction {
  type Target = [u8];
  fn deref(&self) -> &[u8] {
    &self.data
  }
}

impl TryFrom<&Statement> for Transaction {
  type Error = TransactionError;
  fn try_from(stmt: &Statement) -> Result<Self, Self::Error> {
    Transaction::new(stmt.proto_serialized().to_bytes())
  }
}

impl PartialEq for Transaction {
  fn eq(&self, other: &Self) -> bool {
    self.hash == other.hash
  }
}

impl std::hash::Hash for Transaction {
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
    self.hash.hash(state);
  }
}

// Block body
// ----------

#[derive(Debug, Clone, PartialEq)]
pub struct Body {
  pub data: Vec<u8>,
}

impl Body {
  /// Fills block body with first transactions from iterator that fit.
  pub fn fill_from<I, T>(transactions: I) -> Body
  where
    I: IntoIterator<Item = T>,
    T: Into<Transaction>,
  {
    let mut body_vec = vec![0];
    let mut tx_count = 0;
    for transaction in transactions.into_iter() {
      let transaction = transaction.into();
      let tx_len = transaction.data.len();
      if tx_len == 0 {
        continue;
      }
      if tx_count + 1 > 255 {
        break;
      }
      if add_transaction_to_body_vec(&mut body_vec, &transaction).is_err() {
        break;
      }
      tx_count += 1;
    }
    body_vec[0] = (tx_count as u8).reverse_bits();
    Body { data: body_vec }
  }

  /// Build a body from a sequence of transactions.
  /// Fails if they can't fit in a block body.
  pub fn from_transactions_iter<I, T>(transactions: I) -> Result<Body, ()>
  where
    I: IntoIterator<Item = T>,
    T: TryInto<Transaction>,
  {
    // Reserve a byte in the beginning for the number of transactions
    let mut data = vec![0];
    let mut tx_count = 0;
    for transaction in transactions.into_iter() {
      let transaction = transaction.try_into().map_err(|_| ())?;
      // Ignore transaction if it's empty
      let tx_len = transaction.data.len();
      if tx_len == 0 {
        continue;
      }
      // Fails if there's no space left for the transaction
      if data.len() + 2 + tx_len > MAX_BODY_SIZE {
        return Err(());
      }
      // Fails if tx count overflows 255, as we store it in a single byte.
      if tx_count + 1 > 255 {
        return Err(());
      }
      tx_count += 1;
      // Pair of bytes we will store as the length
      let len_bytes = transaction.encode_length();
      data.push(len_bytes.0);
      data.push(len_bytes.1);
      data.extend_from_slice(&transaction.data);
    }
    // Finally stores resulting transaction count on the first byte
    data[0] = (tx_count as u8).reverse_bits();
    Ok(Body { data })
  }
}

// The Block itself
// ----------------

pub type HashedBlock = Hashed<Block>;

#[derive(Debug, Clone)]
pub struct Block {
  /// 32 bytes hash of previous block.
  pub prev: U256,
  /// Block timestamp.
  pub time: u128,
  /// Block metadata.
  pub meta: u128,
  /// Block contents. 1280 bytes max.
  pub body: Body,
}

impl Block {
  pub fn new(prev: U256, time: u128, meta: u128, body: Body) -> Block {
    Block { prev, time, meta, body }
  }
}

impl crypto::Keccakable for Block {
  fn keccak256(&self) -> crypto::Hash {
    let mut bytes: Vec<u8> = Vec::new();
    bytes.extend_from_slice(&u256_to_bytes(self.prev));
    bytes.extend_from_slice(&u128_to_bytes(self.time));
    bytes.extend_from_slice(&u128_to_bytes(self.meta));
    bytes.extend_from_slice(&self.body.data);
    crypto::Hash::keccak256_from_bytes(&bytes)
  }
}

// Node
// ====

/// Blocks have 4 states of inclusion:
///
///   has wait_list? | is on .pending? | is on .block? | meaning
///   -------------- | --------------- | ------------- | ------------------------------------------------------
///   no             | no              | no            | UNSEEN   : never seen, may not exist
///   yes            | no              | no            | MISSING  : some block cited it, but it wasn't downloaded
///   yes            | yes             | no            | PENDING  : downloaded, but waiting ancestors for inclusion
///   no             | no              | yes           | INCLUDED : fully included, as well as all its ancestors
#[derive(Debug, Clone, PartialEq)]
pub enum InclusionState {
  UNSEEN,
  MISSING,
  PENDING,
  INCLUDED,
}

// TODO: refactor .block as map to struct? Better safety, less unwraps. Why not?
#[rustfmt::skip]
pub struct Node<C: ProtoComm, S: BlockStorage> {
  pub network_id : u32,                               // Network ID / magic number
  pub comm       : C,                                 // UDP socket
  pub addr       : C::Address,                        // UDP port
  pub storage    : S,                                 // A `BlockStorage` implementation
  pub runtime    : Runtime,                           // Kindelia's runtime
  pub query_recv   : mpsc::Receiver<NodeRequest<C>>,    // Receives an API request
  pub pool         : PriorityQueue<Transaction, u64>,   // transactions to be mined
  pub peers        : PeersStore<C::Address>,            // peers store and state control
  pub genesis_hash : U256,
  pub tip        : U256,                           // current tip
  pub block      : U256Map<HashedBlock>,           // block hash -> block
  pub pending    : U256Map<HashedBlock>,           // block hash -> downloaded block, waiting for ancestors
  pub ancestor   : U256Map<U256>,                  // block hash -> hash of its most recent missing ancestor (shortcut jump table)
  pub wait_list  : U256Map<Vec<U256>>,             // block hash -> hashes of blocks that are waiting for this one
  pub children   : U256Map<Vec<U256>>,             // block hash -> hashes of this block's children
  pub work       : U256Map<U256>,                  // block hash -> accumulated work
  pub target     : U256Map<U256>,                  // block hash -> this block's target
  pub height     : U256Map<u128>,                  // block hash -> cached height
  pub results    : U256Map<Vec<StatementResult>>,  // block hash -> results of the statements in this block

  #[cfg(feature = "events")]
  pub event_emitter : Option<mpsc::Sender<NodeEventEmittedInfo>>,
  pub miner_comm    : Option<MinerCommunication>,
}

/// Pool's error handling
#[derive(Debug, Error, Serialize, Deserialize)]
pub enum PoolError {
  #[error("Transaction {hash} already included on pool")]
  AlreadyIncluded { hash: api::Hash },
}

// Peers
// -----

#[derive(Debug, Copy, Clone, Serialize, Deserialize)]
pub struct Peer<A: ProtoAddr> {
  pub seen_at: u128,
  pub address: A,
}

pub struct PeersStore<A: ProtoAddr> {
  seen: HashMap<A, Peer<A>>,
  active: HashMap<A, Peer<A>>,
}

impl<A: ProtoAddr> PeersStore<A> {
  pub fn new() -> PeersStore<A> {
    PeersStore { seen: HashMap::new(), active: HashMap::new() }
  }

  /// This function checks and puts a peer as active on `PeerStore`.
  pub fn activate(&mut self, addr: &A, peer: Peer<A>) {
    let now = get_time();
    // Only activate if its `seen_at` is newer than `now - TIMEOUT`
    if peer.seen_at >= now - PEER_TIMEOUT {
      self.active.insert(*addr, peer);
    }
  }

  pub fn see_peer(
    &mut self,
    peer: Peer<A>,
    #[cfg(feature = "events")] event_emitter: Option<
      mpsc::Sender<NodeEventEmittedInfo>,
    >,
  ) {
    let addr = peer.address;
    match self.seen.get(&addr) {
      // New peer, not seen before
      None => {
        self.seen.insert(addr, peer);
        emit_event!(
          event_emitter,
          NodeEventType::see_peer_not_seen(&peer),
          tags = peers,
          see_peer
        );
        self.activate(&addr, peer);
      }
      // Peer seen before, but maybe not active
      Some(_) => {
        let old_peer = self.active.get_mut(&addr);
        match old_peer {
          // Peer not active, so activate it
          None => {
            emit_event!(
              event_emitter,
              NodeEventType::see_peer_activated(&peer),
              tags = peers,
              see_peer
            );
            self.activate(&addr, peer);
          }
          // Peer already active, so update it
          Some(old_peer) => {
            let new_seen_at = std::cmp::max(peer.seen_at, old_peer.seen_at);
            emit_event!(
              event_emitter,
              NodeEventType::see_peer_already_active(&old_peer, new_seen_at),
              tags = peers,
              see_peer
            );
            old_peer.seen_at = new_seen_at;
          }
        }
      }
    }
  }

  fn timeout(
    &mut self,
    #[cfg(feature = "events")] event_emitter: Option<
      mpsc::Sender<NodeEventEmittedInfo>,
    >,
  ) {
    let mut forget = Vec::new();
    for (_, peer) in &self.active {
      if peer.seen_at < get_time() - PEER_TIMEOUT {
        emit_event!(
          event_emitter,
          NodeEventType::timeout(&peer),
          tags = peers,
          timeout
        );
        forget.push(peer.address);
      }
    }
    for addr in forget {
      self.inactivate_peer(&addr);
    }
  }

  pub fn inactivate_peer(&mut self, addr: &A) {
    self.active.remove(addr);
  }

  pub fn get_all_active(&self) -> Vec<Peer<A>> {
    self.active.values().cloned().collect()
  }

  pub fn get_all(&self) -> Vec<Peer<A>> {
    self.seen.values().cloned().collect()
  }

  pub fn get_random_active(&self, amount: u128) -> Vec<Peer<A>> {
    let amount = amount as usize;
    let mut rng = rand::thread_rng();
    let peers = self.active.values().cloned().choose_multiple(&mut rng, amount);
    peers
  }
}

// Communication with miner thread
// -------------------------------

#[derive(Debug, Clone)]
pub enum MinerMessage {
  Request { prev: U256, body: Body, targ: U256 },
  Answer { block: HashedBlock },
  Stop,
}

#[derive(Debug, Clone)]
pub struct MinerCommunication {
  message: Arc<Mutex<MinerMessage>>,
}

// Protocol
// --------

#[allow(clippy::large_enum_variant)]
#[derive(Debug, Clone)]
pub enum Message<A: ProtoAddr> {
  NoticeTheseBlocks {
    magic: u32,
    gossip: bool,
    blocks: Vec<Block>,
    peers: Vec<Peer<A>>,
  },
  GiveMeThatBlock {
    magic: u32,
    bhash: Hash,
  },
  PleaseMineThisTransaction {
    magic: u32,
    tx: Transaction,
  },
}

// Constants
// =========

// Size of a hash, in bytes
pub const _HASH_SIZE: usize = 32;

// Size of a block's body, in bytes
pub const MAX_BODY_SIZE: usize = 1280;

/// Size, in bytes, of needed space for transaction length store
pub const TRANSACTION_LENGTH_ENCODE_SIZE: usize = 2;

/// Size of the largest possible transaction, in bytes
pub const MAX_TRANSACTION_SIZE: usize =
  MAX_BODY_SIZE - TRANSACTION_LENGTH_ENCODE_SIZE - 1;

// Max size of a big UDP packet, in bytes
pub const MAX_UDP_SIZE_SLOW: usize = 8000;

// Max size of a fast UDP packet, in bytes
pub const _MAX_UDP_SIZE_FAST: usize = 1500;

// TODO: enforce maximum block size on debug mode

// Size of a block, in bytes
//pub const BLOCK_SIZE : usize = HASH_SIZE + (U128_SIZE * 4) + BODY_SIZE;

// Size of an IPv4 address, in bytes
pub const _IPV4_SIZE: usize = 4;

// Size of an IPv6 address, in bytes
pub const _IPV6_SIZE: usize = 16;

// Size of an IP port, in bytes
pub const _PORT_SIZE: usize = 2;

// How many nodes we gossip an information to?
pub const _GOSSIP_FACTOR: u128 = 16;

// How many times the mining thread attempts before unblocking?
pub const MINE_ATTEMPTS: u128 = 1024;

// Desired average time between mined blocks, in milliseconds
pub const TIME_PER_BLOCK: u128 = 1000;

// Don't accept blocks from N milliseconds in the future
pub const DELAY_TOLERANCE: u128 = 60 * 60 * 1000;

// Readjust difficulty every N blocks
pub const BLOCKS_PER_PERIOD: u128 = 20;

// Readjusts difficulty every N seconds
pub const TIME_PER_PERIOD: u128 = TIME_PER_BLOCK * BLOCKS_PER_PERIOD;

// Initial difficulty, in expected hashes per block
pub const INITIAL_DIFFICULTY: u128 = 256;

// How many milliseconds without notice until we forget a peer?
pub const PEER_TIMEOUT: u128 = 10 * 1000;

// How many peers we need to keep minimum?
pub const _PEER_COUNT_MINIMUM: u128 = 256;

// How many peers we send when asked?
pub const _SHARE_PEER_COUNT: u128 = 3;

// How many peers we keep on the last_seen object?
pub const _LAST_SEEN_SIZE: u128 = 2;

// Delay between handling of network messages, in ms
pub const HANDLE_MESSAGE_DELAY: u128 = 20;

// Delay between handling of API requests, in ms
pub const HANDLE_REQUEST_DELAY: u128 = 20;

// This limits how many messages we accept at once
pub const _HANDLE_MESSAGE_LIMIT: u128 = 5;

// FIXME:
// With a handle_message_delay of 20ms, and the message limit of 5, we can handle up to 250
// messages per second. This number is made up. I do not know how many messages we're able to
// handle. We must stress test and benchmark the performance of Node::handle_message, in order to
// come up with a constant that is aligned. Furthermore, we can also greatly optimize the
// performance of Node::handle_message with some key changes, which would allow us to increase that
// limit considerably.
// 1. Use a faster hash function:
//   We can replace every usage of Keccak by K12 on Kindelia, with the only exception being the
//   hash of a public address to end up with an account's name, since Keccak is required to achieve
//   Ethereum account compatibility.
// 2. Receive the hash from the peer:
//   We can *perhaps*, receive a block's hash from the peer that sends it. Obviously, this would
//   open door for several vulnerabilities, but it might be used as a heuristic to avoid slow
//   branches. Of course, when the hash is needed for critical purposes, we must compute it.

// Algorithms
// ==========

// Target is a U256 number. A hash larger than or equal to that number hits the target.
// Difficulty is an estimation of how many hashes it takes to hit a given target.

/// Converts a target to a difficulty.
pub fn target_to_difficulty(target: U256) -> U256 {
  let p256 =
    "0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF";
  let p256 = U256::from(p256);
  return p256 / (p256 - target);
}

/// Converts a difficulty to a target.
pub fn difficulty_to_target(difficulty: U256) -> U256 {
  let p256 =
    "0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF";
  let p256 = U256::from(p256);
  return p256 - p256 / difficulty;
}

// Computes next target by scaling the current difficulty by a `scale` factor.
// Since the factor is an integer, it is divided by 2^32 to allow integer division.
// - compute_next_target(t, 2n**32n / 2n): difficulty halves
// - compute_next_target(t, 2n**32n * 1n): nothing changes
// - compute_next_target(t, 2n**32n * 2n): difficulty doubles
pub fn compute_next_target(last_target: U256, scale: U256) -> U256 {
  let p32 = U256::from("0x100000000");
  let last_difficulty = target_to_difficulty(last_target);
  let next_difficulty = u256(1) + (last_difficulty * scale - u256(1)) / p32;
  return difficulty_to_target(next_difficulty);
}

// Estimates how many hashes were necessary to get this one.
pub fn get_hash_work(hash: U256) -> U256 {
  if hash == u256(0) {
    return u256(0);
  } else {
    return target_to_difficulty(hash);
  }
}

// Hashes a U256 value.
pub fn hash_u256(value: U256) -> U256 {
  return hash_bytes(u256_to_bytes(value).as_slice());
}

// Hashes a byte array.
pub fn hash_bytes(bytes: &[u8]) -> U256 {
  let mut hasher = sha3::Keccak256::new();
  hasher.update(&bytes);
  let hash = hasher.finalize();
  return U256::from_little_endian(&hash);
}

/// Puts transaction inside `body_vec` if space is suficient
pub fn add_transaction_to_body_vec(
  body_vec: &mut Vec<u8>,
  transaction: &Transaction,
) -> Result<(), String> {
  let tx_len = transaction.data.len() + TRANSACTION_LENGTH_ENCODE_SIZE;
  let len_info = transaction.encode_length();
  if body_vec.len() + tx_len > MAX_BODY_SIZE {
    return Err("No enough space in block".to_string());
  }
  body_vec.push(len_info.0);
  body_vec.push(len_info.1);
  body_vec.extend_from_slice(&transaction.data);
  Ok(())
}

/// Converts a block body to a vector of transactions.
pub fn extract_transactions(body: &Body) -> Vec<Transaction> {
  let mut transactions = Vec::new();
  let mut index = 1;
  let tx_count = body.data[0].reverse_bits();
  for _ in 0..tx_count {
    if index >= body.data.len() {
      break;
    }
    let tx_len =
      Transaction::decode_length((body.data[index], body.data[index + 1]));
    index += 2;
    if index + tx_len > body.data.len() {
      break;
    }
    let transaction_body = body.data[index..index + tx_len].to_vec();
    match Transaction::new(transaction_body) {
      Ok(transaction) => transactions.push(transaction),
      Err(err) => eprintln!(
        "A transaction bigger than block was created from a block{}",
        err
      ), // in theory this can never happen
    };
    index += tx_len;
  }
  transactions
}

/// Initial target of 256 hashes per block.
pub fn initial_target() -> U256 {
  difficulty_to_target(u256(INITIAL_DIFFICULTY))
}

/// The hash of the genesis block's parent.
pub fn zero_hash() -> U256 {
  u256(0)
}

/// Builds the Genesis Block.
pub fn build_genesis_block(stmts: &[Statement]) -> Block {
  let body = Body::from_transactions_iter(stmts)
    .expect("Genesis statements should fit in a block body");
  Block::new(zero_hash(), 0, 0, body)
}

// Mining
// ------

// Given a target, attempts to mine a block by changing its nonce up to `max_attempts` times
pub fn try_mine(
  prev: U256,
  body: Body,
  targ: U256,
  max_attempts: u128,
) -> Option<HashedBlock> {
  let rand = rand::random::<u128>();
  let time = get_time();
  let mut block = Block::new(prev, time, rand, body);
  for _i in 0..max_attempts {
    block = {
      let hashed = block.hashed();
      let hash_n = U256::from(hashed.get_hash());
      if hash_n >= targ {
        return Some(hashed);
      }
      let mut block = hashed.take();
      block.meta = block.meta.wrapping_add(1);
      block
    }
  }
  None
}

impl MinerCommunication {
  // Creates a shared MinerCommunication object
  pub fn new() -> Self {
    MinerCommunication { message: Arc::new(Mutex::new(MinerMessage::Stop)) }
  }

  // Writes the shared MinerCommunication object
  pub fn write(&mut self, new_message: MinerMessage) {
    let mut value = self.message.lock().unwrap();
    *value = new_message;
  }

  pub fn read(&self) -> MinerMessage {
    return (*self.message.lock().unwrap()).clone();
  }
}

// Main miner loop: if asked, attempts to mine a block
pub fn miner_loop(
  mut miner_comm: MinerCommunication,
  slow_mining: Option<u64>,
  #[cfg(feature = "events")] event_emitter: Option<
    mpsc::Sender<NodeEventEmittedInfo>,
  >,
) {
  loop {
    if let MinerMessage::Request { prev, body, targ } = miner_comm.read() {
      let before = std::time::Instant::now();
      let mined = try_mine(prev, body, targ, MINE_ATTEMPTS);
      // Slow down mining, for debugging pourposes, if enabled
      if let Some(slow_ratio) = slow_mining {
        let elapsed = before.elapsed();
        let sleep_time = elapsed.saturating_mul(slow_ratio as u32);
        std::thread::sleep(sleep_time);
      }
      if let Some(block) = mined {
        emit_event!(
          event_emitter,
          NodeEventType::mined(block.get_hash().into(), targ),
          tags = mining,
          mined
        );
        miner_comm.write(MinerMessage::Answer { block });
      } else {
        emit_event!(
          event_emitter,
          NodeEventType::failed_mined(targ),
          tags = mining,
          failed_mined
        );
      }
    }
  }
}

// Node
// ----

impl<C: ProtoComm, S: BlockStorage> Node<C, S> {
  pub fn new(
    data_path: PathBuf,
    network_id: u32,
    initial_peers: Vec<C::Address>,
    comm: C,
    miner_comm: Option<MinerCommunication>,
    storage: S,
    #[cfg(feature = "events")] event_emitter: Option<
      mpsc::Sender<NodeEventEmittedInfo>,
    >,
  ) -> (mpsc::SyncSender<NodeRequest<C>>, Self) {
    let (query_sender, query_receiver) = mpsc::sync_channel(1);

    let genesis_stmts =
      parser::parse_code(constants::GENESIS_CODE).expect("Genesis code parses");
    let genesis_block = build_genesis_block(&genesis_stmts);
    let genesis_block = genesis_block.hashed();
    let genesis_hash = genesis_block.get_hash().into();

    let runtime = init_runtime(data_path.join("heaps"), &genesis_stmts);

    #[rustfmt::skip]
    let mut node = Node {
      network_id,
      addr: comm.get_addr(),
      comm,
      runtime,
      pool     : PriorityQueue:: new(),
      peers    : PeersStore:: new(),

      genesis_hash,
      tip      : genesis_hash,
      block    : u256map_from([(genesis_hash, genesis_block)]),
      pending  : u256map_new(),
      ancestor : u256map_new(),
      wait_list: u256map_new(),
      children : u256map_from([(genesis_hash, vec![]          )]),
      work     : u256map_from([(genesis_hash, u256(0)         )]),
      height   : u256map_from([(genesis_hash, 0               )]),
      target   : u256map_from([(genesis_hash, initial_target())]),
      results  : u256map_from([(genesis_hash, vec![]          )]),

      #[cfg(feature = "events")]
      event_emitter: event_emitter.clone(),
      storage,
      query_recv : query_receiver,
      miner_comm,
    };

    let now = get_time();

    initial_peers.iter().for_each(|address| {
      return node.peers.see_peer(
        Peer { address: *address, seen_at: now },
        #[cfg(feature = "events")] // TODO: remove (implement on Node)
        event_emitter.clone(),
      );
    });

    // TODO: For testing purposes. Remove later.
    // for &peer_port in try_ports.iter() {
    //   if peer_port != port {
    //     let address = Address::IPv4 { val0: 127, val1: 0, val2: 0, val3: 1, port: peer_port };
    //     node.peers.see_peer(Peer { address: address, seen_at: now })
    //   }
    // }

    (query_sender, node)
  }

  pub fn add_transaction(
    &mut self,
    transaction: Transaction,
  ) -> Result<(), PoolError> {
    let hash = transaction.hash;
    let t_score = hash.low_u64();
    if self.pool.get(&transaction).is_none() {
      self.pool.push(transaction, t_score);
      Ok(())
    } else {
      Err(PoolError::AlreadyIncluded { hash: hash.into() })
    }
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
  pub fn add_block(&mut self, block: &HashedBlock) {
    // Adding a block might trigger the addition of other blocks
    // that were waiting for it. Because of that, we loop here.

    // Blocks to be added
    let mut must_include = vec![block.clone()];
    // While there is a block to add...
    while let Some(block) = must_include.pop() {
      let btime = block.time;
      // If block is too far into the future, ignore it
      if btime >= get_time() + DELAY_TOLERANCE {
        emit_event!(
          self.event_emitter,
          NodeEventType::too_late(&block),
          tags = add_block,
          too_late
        );
        continue;
      }
      let bhash = block.get_hash().into();
      // If we already registered this block, ignore it
      if let Some(block) = self.block.get(&bhash) {
        let height = self.height[&bhash];
        emit_event!(
          self.event_emitter,
          NodeEventType::already_included(block, height),
          tags = add_block,
          already_included
        );
        continue;
      }
      let phash = block.prev;
      // If previous block is available, add the block to the chain
      if self.block.get(&phash).is_some() {
        let work = get_hash_work(bhash); // block work score
        self.block.insert(bhash, block.clone()); // inserts the block
        self.work.insert(bhash, u256(0)); // inits the work attr
        self.height.insert(bhash, 0); // inits the height attr
        self.target.insert(bhash, u256(0)); // inits the target attr
        self.children.insert(bhash, vec![]); // inits the children attrs
        self.ancestor.remove(&bhash); // remove it from the ancestor jump table

        // Checks if this block PoW hits the target
        let has_enough_work = bhash >= self.target[&phash];
        // Checks if this block's timestamp is larger than its parent's timestamp
        // Note: Bitcoin checks if it is larger than the median of the last 11 blocks; should we?
        let advances_time = btime > self.block[&phash].time;
        // If the PoW hits the target and the block's timestamp is valid...
        if has_enough_work && advances_time {
          self.work.insert(bhash, self.work[&phash] + work); // sets this block accumulated work
          self.height.insert(bhash, self.height[&phash] + 1); // sets this block accumulated height

          // If this block starts a new period, computes the new target
          if self.height[&bhash] > 0
            && self.height[&bhash] > BLOCKS_PER_PERIOD
            && self.height[&bhash] % BLOCKS_PER_PERIOD == 1
          {
            // Finds the checkpoint hash (hash of the first block of the last period)
            let mut checkpoint_hash = phash;
            for _ in 0..BLOCKS_PER_PERIOD - 1 {
              checkpoint_hash = self.block[&checkpoint_hash].prev;
            }
            // Computes how much time the last period took to complete
            let period_time = btime - self.block[&checkpoint_hash].time;
            // Computes the target of this period
            let last_target = self.target[&phash];
            let next_scaler = 2u128.pow(32) * TIME_PER_PERIOD / period_time;
            let next_target =
              compute_next_target(last_target, u256(next_scaler));
            // Sets the new target
            self.target.insert(bhash, next_target);
          // Otherwise, keep the old target
          } else {
            self.target.insert(bhash, self.target[&phash]);
          }
          // Updates the tip work and block hash
          let cur_tip = self.tip;
          let new_tip = bhash;
          // Reorgs happens
          if self.work[&new_tip] > self.work[&cur_tip] {
            // When the tip updates, stop mining the last built block, which is
            // based on the outdated tip
            self.send_to_miner(MinerMessage::Stop);
            emit_event!(
              self.event_emitter,
              NodeEventType::stop_mining(),
              tags = mining,
              stopped
            );
            // Removes this block's transactions from mempool
            for tx in extract_transactions(&block.body) {
              self.pool.remove(&tx);
            }
            self.tip = bhash;
            if true {
              // Block reorganization (* marks blocks for which we have runtime snapshots):
              // tick: |  0 | *1 |  2 |  3 |  4 | *5 |  6 | *7 | *8 |
              // hash: |  A |  B |  C |  D |  E |  F |  G |  H |    |  <- old timeline
              // hash: |  A |  B |  C |  D |  P |  Q |  R |  S |  T |  <- new timeline
              //               |         '-> highest common block shared by both timelines
              //               '-----> highest runtime snapshot before block D
              let mut must_compute = Vec::new();
              let mut old_bhash = cur_tip;
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
              // TODO: on separate thread
              for bhash_comp in must_compute.iter().rev() {
                let block_height = self.height[bhash_comp];
                self
                  .storage
                  .write_block(block_height, self.block[bhash_comp].clone());
              }
              // 4. Reverts the runtime to a state older than that block
              //    On the example above, we'd find `runtime.tick = 1`
              let mut tick = self.height[&old_bhash];

              let runtime_old_tick = self.runtime.get_tick();
              self.runtime.rollback(tick as u64);

              let old = (&self.block[&cur_tip], self.height[&cur_tip]);
              let new = (&self.block[&new_tip], self.height[&new_tip]);
              let common = (&self.block[&old_bhash], self.height[&old_bhash]); // common ancestor
              let ticks =
                (runtime_old_tick as u128, self.runtime.get_tick() as u128);
              emit_event!(
                self.event_emitter,
                NodeEventType::reorg(old, new, common, ticks, work),
                tags = add_block,
                reorg
              );

              // 5. Finds the last block included on the reverted runtime state
              //    On the example above, we'd find `new_bhash = B`
              while tick as u64 > self.runtime.get_tick() {
                must_compute.push(new_bhash);
                new_bhash = self.block[&new_bhash].prev;
                tick -= 1;
              }
              emit_event!(
                self.event_emitter,
                NodeEventType::computed(
                  &block,
                  self.height[&self.tip],
                  &must_compute
                ),
                tags = add_block,
                computed
              ); // emitting computed blocks for measurement

              // 6. Computes every block after that on the new timeline
              //    On the example above, we'd compute `C, D, P, Q, R, S, T`
              for bhash_comp in must_compute.iter().rev() {
                let block_comp = &self.block[bhash_comp];
                self.compute_block(&block_comp.clone()); // TODO: avoid clone
              }
            }
          }
        } else {
          emit_event!(
            self.event_emitter,
            NodeEventType::not_enough_work(&self.block[&bhash]),
            tags = add_block,
            not_enough_work
          );
        }

        let height = self.height.get(&bhash).copied();
        let siblings: Vec<_> =
          self.children[&block.prev].iter().copied().collect();
        emit_event!(
          self.event_emitter,
          NodeEventType::included(&block, height, &siblings, work),
          tags = add_block,
          block_included
        );

        // Registers this block as a child of its parent
        self.children.entry(phash).or_insert_with(Vec::new).push(bhash);

        // If there were blocks waiting for this one, include them on the next loop
        // This will cause the block to be moved from self.pending to self.block
        if let Some(wait_list) = self.wait_list.get(&bhash) {
          for waiting_for_me in wait_list {
            must_include
              .push(self.pending.remove(waiting_for_me).expect("block"));
          }
          self.wait_list.remove(&bhash);
        }
      // Otherwise, if the previous block isn't available,
      // include this block on .pending, and on its parent's wait_list
      } else if self.pending.get(&bhash).is_none() {
        self.pending.insert(bhash, block.clone());
        self.wait_list.entry(phash).or_insert_with(|| Vec::new()).push(bhash);
        emit_event!(
          self.event_emitter,
          NodeEventType::missing_parent(&block),
          tags = add_block,
          missing_parent
        );
      }
    }
  }

  pub fn compute_block(&mut self, block: &HashedBlock) {
    let transactions = extract_transactions(&block.body);
    let mut statements = Vec::new();
    for transaction in transactions {
      if let Some(statement) = transaction.to_statement() {
        statements.push(statement);
      }
    }
    let bhash = U256::from(block.get_hash());
    self.runtime.set_time(block.time >> 8);
    self.runtime.set_meta(block.meta >> 8);
    self.runtime.set_hax0((bhash >> 000).low_u128() >> 8);
    self.runtime.set_hax1((bhash >> 120).low_u128() >> 8);
    self.runtime.open();
    let result = self.runtime.run_statements(&statements, false, false);
    self.results.insert(bhash, result);
    self.runtime.commit();
  }

  // Get the current target
  pub fn get_tip_target(&self) -> U256 {
    self.target[&self.tip]
  }

  pub fn get_longest_chain(&self, max: Option<usize>) -> Vec<U256> {
    let mut longest = Vec::new();
    let mut bhash = self.tip;
    let mut count = 0;
    while self.block.contains_key(&bhash) && bhash != zero_hash() {
      // TODO: zero check seems redundant
      let block = self.block.get(&bhash).unwrap();
      longest.push(bhash);
      bhash = block.prev;
      count += 1;
      if let Some(num) = max {
        if count >= num {
          break;
        }
      }
    }
    longest.reverse();
    return longest;
  }

  pub fn receive_message(&mut self) {
    let mut count = 0;
    for (addr, msg) in self.comm.proto_recv() {
      //if count < HANDLE_MESSAGE_LIMIT {  TODO: ???
      self.handle_message(addr, &msg);
      count = count + 1;
      //}
    }
  }

  fn receive_request(&mut self) {
    if let Ok(request) = self.query_recv.try_recv() {
      self.handle_request(request);
    }
  }

  pub fn get_block_hash_by_index(&self, index: u64) -> Option<U256> {
    let mut hsh = self.tip;
    let mut idx = self.height[&hsh] as u64;
    if index > idx {
      return None;
    }
    while index < idx {
      hsh = self.block[&hsh].prev;
      idx = idx - 1;
    }
    return Some(hsh);
  }

  pub fn get_block_info(&self, hash: &U256) -> Option<BlockInfo> {
    // TODO: cache
    let block = self.block.get(hash)?;
    let height = self.height.get(hash).expect("Missing block height.");
    let height: u64 = (*height).try_into().expect("Block height is too big.");
    let results = self.results.get(hash).map(|r| r.clone());
    let info = BlockInfo {
      block: (&**block).into(),
      hash: (*hash).into(),
      height,
      results,
    };
    Some(info)
  }

  pub fn get_func_info(&self, name: &Name) -> Option<FuncInfo> {
    let comp_func = self.runtime.read_file(name)?;
    let func = comp_func.func;
    Some(FuncInfo { func })
  }
  pub fn get_ctr_info(&self, name: &Name) -> Option<CtrInfo> {
    if let Some(arit) = self.runtime.get_arity(name) {
      Some(CtrInfo { arit: arit as u64 })
    } else {
      None
    }
  }

  pub fn get_reg_info(&self, name: Name) -> Option<RegInfo> {
    let ownr = self.runtime.get_owner(&name)?;
    let ownr = Name::from(ownr);
    let pred = |c: &Name| c.to_string().starts_with(&format!("{}.", name));
    let ctrs: Vec<Name> =
      self.runtime.get_all_ctr().into_iter().filter(pred).collect();
    let funs: Vec<Name> =
      self.runtime.get_all_funs().into_iter().filter(pred).collect();
    let ns: Vec<Name> =
      self.runtime.get_all_ns().into_iter().filter(pred).collect();
    let stmt = [ctrs, funs, ns].concat();
    Some(RegInfo { ownr, stmt })
  }

  pub fn handle_request(&mut self, request: NodeRequest<C>) {
    fn handle_ans_err<T>(req_txt: &str, res: Result<(), T>) {
      if let Err(_) = res {
        eprintln!("WARN: failed to send node request {} answer back", req_txt);
      }
    }
    // TODO: handle unwraps
    // emit_event!(self.event_emitter, NodeEvent::handle_request(), tags = handle_request);
    match request {
      NodeRequest::GetStats { tx } => {
        let tick = self.runtime.get_tick();
        let mana = {
          let limit = self.runtime.get_mana_limit();
          let used = self.runtime.get_mana();
          let available = limit - used;
          api::LimitStats { limit, used, available }
        };
        let size = {
          let limit = self.runtime.get_size_limit();
          let used = self.runtime.get_size();
          let available = limit - used;
          api::LimitStats { limit, used, available }
        };
        let mut fun_count = 0;
        self.runtime.reduce_with(&mut fun_count, |acc, heap| {
          *acc += heap.get_fn_count();
        });
        let mut ctr_count = 0;
        self.runtime.reduce_with(&mut ctr_count, |acc, heap| {
          *acc += heap.get_ct_count();
        });
        let mut reg_count = 0;
        self.runtime.reduce_with(&mut reg_count, |acc, heap| {
          *acc += heap.get_ns_count();
        });
        let stats = api::Stats {
          tick,
          mana,
          space: size,
          fun_count,
          ctr_count,
          reg_count,
        };
        handle_ans_err("GetStats", tx.send(stats));
      }
      NodeRequest::GetBlocks { range, tx } => {
        let (start, end) = range;
        debug_assert!(start <= end);
        debug_assert!(end == -1);
        let num = (end - start + 1) as usize;
        let hashes = self.get_longest_chain(Some(num));
        let infos = hashes
          .iter()
          .map(|h| self.get_block_info(h).expect("Missing block."))
          .collect();
        handle_ans_err("GetBlocks", tx.send(infos));
      }
      NodeRequest::GetBlock { hash, tx } => {
        let info = self.get_block_info(&hash);
        handle_ans_err("GetBlock", tx.send(info));
      }
      NodeRequest::GetBlockHash { index, tx } => {
        let info = self.get_block_hash_by_index(index);
        handle_ans_err("GetBlockHash", tx.send(info));
      }
      NodeRequest::GetFunctions { tx } => {
        let mut funcs: HashSet<u128> = HashSet::new();
        self.runtime.reduce_with(&mut funcs, |acc, heap| {
          for func in heap.disk.links.keys() {
            acc.insert(**func);
          }
        });
        handle_ans_err("GetFunctions", tx.send(funcs));
      }
      NodeRequest::GetFunction { name, tx } => {
        let info = self.get_func_info(&name);
        handle_ans_err("GetFunction", tx.send(info));
      }
      NodeRequest::GetState { name, tx } => {
        let state = self.runtime.read_disk_as_term(name.into(), Some(1 << 16));
        handle_ans_err("GetState", tx.send(state));
      }
      NodeRequest::GetPeers { all, tx } => {
        let peers =
          if all { self.peers.get_all() } else { self.peers.get_all_active() };
        handle_ans_err("GetPeers", tx.send(peers));
      }
      NodeRequest::GetConstructor { name, tx } => {
        let info = self.get_ctr_info(&name);
        handle_ans_err("GetConstructor", tx.send(info));
      }
      NodeRequest::GetReg { name, tx } => {
        let info = self.get_reg_info(name);
        handle_ans_err("GetReg", tx.send(info));
      }
      NodeRequest::RunCode { code, tx } => {
        let result = self.runtime.test_statements_from_code(&code);
        handle_ans_err("RunCode", tx.send(result));
      }
      NodeRequest::PublishCode { code, tx } => {
        let statements = parser::parse_statements(&code)
          .map_err(|err| err.erro)
          .map(|(_, s)| s);
        let res = match statements {
          Err(err) => Err(err),
          Ok(stmts) => {
            let results: Vec<_> = stmts
              .into_iter()
              .map(|stmt| {
                let result: Result<(), api::PublishError> = {
                  let bytes = bitvec_to_bytes(&stmt.proto_serialized());
                  let t = Transaction::new(bytes)?; // transaction creation may fail
                  self.add_transaction(t)?; // transaction addition may fail
                  Ok(())
                };
                result
              })
              .collect();
            Ok(results)
          }
        };
        handle_ans_err("PublishCode", tx.send(res));
      }
      NodeRequest::Run { code, tx } => {
        let result = self.runtime.test_statements(&code);
        handle_ans_err("Run", tx.send(result));
      }
      NodeRequest::Publish { code, tx } => {
        let result: Vec<_> = code
          .into_iter()
          .map(|stmt| {
            let result: Result<(), api::PublishError> = {
              let bytes = bitvec_to_bytes(&stmt.proto_serialized());
              let t = Transaction::new(bytes)?; // transaction creation may fail
              self.add_transaction(t)?; // transaction addition may fail
              Ok(())
            };
            result
          })
          .collect();
        handle_ans_err("Publish", tx.send(result));
      }
    }
  }

  // Sends a block to a target address; also share some random peers
  // FIXME: instead of sharing random peers, share recently active peers
  pub fn send_blocks_to(
    &mut self,
    addrs: Vec<C::Address>,
    gossip: bool,
    blocks: Vec<Block>,
    share_peers: u128,
  ) {
    let magic = self.network_id;
    let peers = self.peers.get_random_active(share_peers);
    let msg = Message::NoticeTheseBlocks { magic, gossip, blocks, peers };
    self.comm.proto_send(addrs, &msg);
  }

  // Returns the block inclusion state
  pub fn inclusion_state(&self, bhash: &U256) -> InclusionState {
    if self.block.contains_key(bhash) {
      return InclusionState::INCLUDED;
    }
    if self.pending.contains_key(bhash) {
      return InclusionState::PENDING;
    }
    if self.wait_list.contains_key(bhash) {
      return InclusionState::MISSING;
    }
    return InclusionState::UNSEEN;
  }

  // Finds the most recent missing ancestor of a pending block
  pub fn find_missing_ancestor(&mut self, bhash: &U256) -> Option<U256> {
    if self.inclusion_state(bhash) == InclusionState::PENDING {
      let ancestor = {
        let mut ancestor = bhash;
        while self.inclusion_state(ancestor) == InclusionState::PENDING {
          if let Some(jump_to) = self.ancestor.get(&ancestor) {
            ancestor = jump_to;
          }
          if let Some(ancestor_block) = self.pending.get(&ancestor) {
            ancestor = &ancestor_block.prev;
          }
        }
        *ancestor
      };
      self.ancestor.insert(*bhash, ancestor);
      return Some(ancestor);
    }
    // This should NOT happen, since we only call find_missing_ancestor after add_block.
    debug_assert!(self.inclusion_state(bhash) != InclusionState::MISSING);
    debug_assert!(self.inclusion_state(bhash) != InclusionState::UNSEEN);
    return None;
  }

  // Requests the most recent missing ancestor
  pub fn request_missing_ancestor(&mut self, addr: C::Address, bhash: &U256) {
    if let Some(missing_ancestor) = self.find_missing_ancestor(bhash) {
      let magic = self.network_id;
      let msg = &Message::GiveMeThatBlock { magic, bhash: missing_ancestor };
      self.comm.proto_send(vec![addr], msg);
    }
  }

  pub fn handle_message(
    &mut self,
    addr: C::Address,
    msg: &Message<C::Address>,
  ) {
    if addr != self.addr {
      match msg {
        Message::GiveMeThatBlock { magic, .. }
        | Message::NoticeTheseBlocks { magic, .. }
        | Message::PleaseMineThisTransaction { magic, .. } => {
          if magic != &self.network_id {
            return;
          }
        }
      }

      self.peers.see_peer(
        Peer { address: addr, seen_at: get_time() },
        #[cfg(feature = "events")]
        self.event_emitter.clone(),
      );

      match msg {
        // Someone asked a block
        Message::GiveMeThatBlock { magic, bhash } => {
          emit_event!(
            self.event_emitter,
            NodeEventType::give_me_block(*magic, *bhash),
            tags = handle_message,
            give_me_block
          );
          // Sends the requested block, plus some of its ancestors
          let mut bhash = bhash;
          let mut chunk = vec![];
          let mut tsize = 0; // total size of the corresponding "NoticeTheseBlocks" message
          loop {
            if !self.block.contains_key(&bhash) {
              break;
            }
            if *bhash == zero_hash() {
              // TODO: this check seems redundant
              // Stops when it reaches genesis block non-existing parent
              break;
            }
            let block = &self.block[bhash];
            let bsize = bits::serialized_block_size(block) as usize;
            if tsize + bsize > MAX_UDP_SIZE_SLOW {
              break;
            }
            chunk.push((**block).clone());
            tsize += bsize;
            bhash = &block.prev;
          }
          self.send_blocks_to(vec![addr], false, chunk, 0);
        }
        // Someone sent us some blocks
        Message::NoticeTheseBlocks { magic, gossip, blocks, peers } => {
          let blocks: Vec<_> =
            blocks.iter().cloned().map(|block| block.hashed()).collect();
          emit_event!(
            self.event_emitter,
            NodeEventType::notice_blocks(*magic, *gossip, &blocks, peers),
            tags = handle_message,
            notice_blocks
          );

          // TODO: validate if blocks are sorted by age?

          // Notice received peers
          for peer in peers {
            self.peers.see_peer(
              *peer,
              #[cfg(feature = "events")]
              self.event_emitter.clone(),
            );
          }

          // Adds the block to the database
          for block in &blocks {
            self.add_block(&block);
          }

          // Requests missing ancestors
          if *gossip && blocks.len() > 0 {
            let bhash = U256::from(&blocks[0].keccak256());
            self.request_missing_ancestor(addr, &bhash);
          }
        }
        // Someone sent us a transaction to mine
        Message::PleaseMineThisTransaction { magic, tx } => {
          emit_event!(
            self.event_emitter,
            NodeEventType::mine_trans(*magic, tx.hash),
            tags = handle_message,
            mine_trans
          );
          if self.pool.get(&tx).is_none() {
            self.pool.push(tx.clone(), tx.hash.low_u64());
            self.gossip(5, msg);
          }
        }
      }
    }
  }

  pub fn gossip(&mut self, peer_count: u128, message: &Message<C::Address>) {
    let addrs = self
      .peers
      .get_random_active(peer_count)
      .iter()
      .map(|x| x.address)
      .collect();
    self.comm.proto_send(addrs, message);
  }

  fn broadcast_tip_block(&mut self) {
    let addrs: Vec<C::Address> =
      self.peers.get_all_active().iter().map(|x| x.address).collect();
    let blocks = vec![(*self.block[&self.tip]).clone()];
    self.send_blocks_to(addrs, true, blocks, 3);
  }

  fn gossip_tip_block(&mut self, peer_count: u128) {
    let addrs: Vec<C::Address> = self
      .peers
      .get_random_active(peer_count)
      .iter()
      .map(|x| x.address)
      .collect();
    let blocks = vec![(*self.block[&self.tip]).clone()];
    self.send_blocks_to(addrs, true, blocks, 3);
  }

  pub fn load_blocks(&mut self) {
    self.storage.disable();
    let storage = self.storage.clone();
    storage.read_blocks(|(block, file_path)| match block {
      Some(block) => {
        self.add_block(&block.hashed());
      }
      None => {
        eprintln!(
          "WARN: Could not load block from file '{}'",
          file_path.display()
        );
      }
    });
    self.storage.enable();
  }

  fn send_to_miner(&mut self, msg: MinerMessage) {
    if let Some(comm) = &mut self.miner_comm {
      comm.write(msg);
    }
  }

  fn do_ask_mine(&mut self, body: Body) {
    let targ = self.get_tip_target();
    emit_event!(
      self.event_emitter,
      NodeEventType::ask_mine(targ),
      tags = mining,
      ask_mine
    );
    self.send_to_miner(MinerMessage::Request { prev: self.tip, body, targ });
  }

  fn do_handle_mined_block(&mut self) {
    if let Some(miner_comm) = &mut self.miner_comm {
      if let MinerMessage::Answer { block } = miner_comm.read() {
        self.add_block(&block);
        self.broadcast_tip_block();
      }
    }
  }

  /// Builds the body to be mined.
  /// To convert back to a vector of transactions, use `extract_transactions()`.
  pub fn build_body_from_pool(&self) -> Body {
    let txs = self.pool.iter().map(|(tx, _score)| tx.clone());
    Body::fill_from(txs)
  }

  fn log_heartbeat(&self) {
    let tip = self.tip;
    let tip_height = *self.height.get(&tip).unwrap() as u64;
    let tip_work = *self.work.get(&tip).unwrap();

    let tip_target = *self.target.get(&tip).unwrap();
    let difficulty = target_to_difficulty(tip_target);

    // Counts missing, pending and included blocks
    let included_count = self.block.keys().count();
    let mut missing_count: u64 = 0;
    let mut pending_count: u64 = 0;
    for (bhash, _) in self.wait_list.iter() {
      if self.pending.get(bhash).is_some() {
        pending_count += 1;
      } else {
        missing_count += 1;
      }
    }

    let mana_cur = self.runtime.get_mana() as i64;
    let mana_lim = self.runtime.get_mana_limit() as i64;
    let size_cur = self.runtime.get_size() as i64;
    let size_lim = self.runtime.get_size_limit() as i64;
    let mana_avail = mana_lim - mana_cur;
    let size_avail = size_lim - size_cur;
    debug_assert!(size_avail >= 0);
    debug_assert!(mana_avail >= 0);

    let peers_num = self.peers.get_all_active().len();

    let mut tip_blocks = vec![];
    let mut block = &self.block[&self.tip];
    let mut count = 0;
    while block.prev != u256(0) && count < 10 {
      tip_blocks.push(block.get_hash().into());
      // eprintln!("prev {}", block.prev);
      block = &self.block[&block.prev];
      count += 1;
    }
    tip_blocks.reverse();

    let event = heartbeat! {
      peers: { num: peers_num },
      tip: {
        height: tip_height,
        difficulty: difficulty.low_u64(),
        work: tip_work,
      },
      blocks: {
        missing: missing_count,
        pending: pending_count,
        included: included_count,
      },
      runtime: {
        mana: {
          current: mana_cur,
          limit: mana_lim,
          available: mana_avail,
        },
        size: {
          current: size_cur,
          limit: size_lim,
          available: size_avail,
        }
      },
      tip_blocks: tip_blocks
    };

    emit_event!(self.event_emitter, event, tags = heartbeat);
  }

  pub fn main(mut self) -> ! {
    eprintln!("Genesis hash: {:#34x}", self.genesis_hash);
    eprintln!("UDP/protocol port: {}", self.addr);
    eprintln!("Initial peers: ");
    for peer in self.peers.get_all_active() {
      eprintln!("  - {}", peer.address);
    }

    eprintln!("Loading block from disk...");
    self.load_blocks();

    // A task that is executed continuously on the main loop
    struct Task<C: ProtoComm, S: BlockStorage> {
      pub delay: u128,
      pub action: fn(&mut Node<C, S>) -> (),
    }

    // The vector of tasks
    let mut tasks = vec![
      // Gossips the tip block
      Task {
        delay: 20,
        action: |node| {
          node.gossip_tip_block(8);
        },
      },
      // Receives and handles incoming network messages
      Task {
        delay: HANDLE_MESSAGE_DELAY,
        action: |node| {
          node.receive_message();
        },
      },
      // Receives and handles incoming API requests
      Task {
        delay: HANDLE_REQUEST_DELAY,
        action: |node| {
          node.receive_request();
        },
      },
      // Forgets inactive peers
      Task {
        delay: 5_000,
        action: |node| {
          node.peers.timeout(
            #[cfg(feature = "events")]
            node.event_emitter.clone(),
          );
        },
      },
      #[cfg(feature = "events")]
      // Prints stats
      Task {
        delay: 5_000,
        action: |node| {
          node.log_heartbeat();
        },
      },
    ];

    if let Some(_) = self.miner_comm {
      let miner_tasks = vec![
        // Asks the miner thread to mine a block
        Task {
          delay: 25,
          action: |node| {
            if let Some(comm) = &mut node.miner_comm {
              if let MinerMessage::Stop { .. } = comm.read() {
                node.do_ask_mine(node.build_body_from_pool());
              }
            }
          },
        },
        // If the miner mined a block, adds it
        Task {
          delay: 5,
          action: |node| {
            node.do_handle_mined_block();
          },
        },
      ];
      tasks.extend(miner_tasks);
    }

    // Last time a task was executed
    let mut last_tick_time: Vec<u128> = vec![0; tasks.len()];

    loop {
      let now = std::time::Instant::now();
      let system_time = get_time(); // Measured in milliseconds
      for (i, task) in tasks.iter().enumerate() {
        if last_tick_time[i] + task.delay <= system_time {
          (task.action)(&mut self);
          last_tick_time[i] = system_time;
        }
      }
      let elapsed = now.elapsed();
      let extra = std::time::Duration::from_millis(1).checked_sub(elapsed);
      // If the elapsed time is less than 1ms, sleep for the remaining time
      if let Some(extra) = extra {
        std::thread::sleep(extra);
      }
    }
  }
}

pub fn spawn_miner(
  mine_config: MineConfig,
  #[cfg(feature = "events")] event_tx: Option<
    mpsc::Sender<NodeEventEmittedInfo>,
  >,
) -> (Option<MinerCommunication>, Vec<JoinHandle<()>>) {
  // Only spaws thread if mining is enabled
  if mine_config.enabled {
    // Node to Miner communication object
    let miner_comm_0 = MinerCommunication::new();
    let miner_comm_1 = miner_comm_0.clone();
    let handle = std::thread::spawn(move || {
      miner_loop(
        miner_comm_0,
        mine_config.slow_mining,
        #[cfg(feature = "events")]
        event_tx,
      );
    });
    (Some(miner_comm_1), vec![handle])
  } else {
    (None, vec![])
  }
}
