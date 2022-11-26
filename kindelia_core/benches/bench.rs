use std::path::PathBuf;

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use kindelia_common::crypto::Keccakable;
use kindelia_core::persistence;
use kindelia_core::persistence::BlockStorage;
use kindelia_lang::parser;
use primitive_types::U256;

use kindelia_core::bits::ProtoSerialize;
use kindelia_core::{runtime, net, node, util};
use kindelia_core::net::ProtoComm;

// KHVM
// ====

// Util
// ----

pub fn temp_dir() -> PathBuf {
  let path =
    std::env::temp_dir().join(format!("crate.{:x}", fastrand::u128(..)));
  std::fs::create_dir_all(&path).unwrap();
  path
}

pub fn init_runtime(path: PathBuf) -> runtime::Runtime {
  const GENESIS_CODE: &str = include_str!("../genesis-tests.kdl");
  let genesis_stmts =
    parser::parse_code(GENESIS_CODE).expect("Genesis code parses.");
  runtime::init_runtime(path, &genesis_stmts)
}

fn khvm_benches(c: &mut Criterion) {
  c.bench_function("kvm_tree_sum", |b| {
    let pre_code = include_str!("kdl/tree_sum.pre.kdl");
    let code = include_str!("kdl/tree_sum.kdl");
    let dir_path = temp_dir();
    let mut runtime = init_runtime(dir_path);
    runtime.run_statements_from_code(pre_code, true, true);
    b.iter(|| {
      runtime.run_statements_from_code(code, true, true);
    })
  });
}

criterion_group!(khvm, khvm_benches);

// Serialization
// =============
fn max_block() -> node::Block {
  node::Block {
    body: node::Body { data: vec![u8::MAX; node::MAX_BODY_SIZE] },
    prev: U256::MAX,
    time: u128::MAX,
    meta: u128::MAX,
  }
}

fn max_message() -> node::Message<net::Address> {
  let max_block = max_block();
  let max_peer = node::Peer {
    address: net::Address::IPv4 {
      val0: u8::MAX,
      val1: u8::MAX,
      val2: u8::MAX,
      val3: u8::MAX,
      port: u16::MAX,
    },
    seen_at: u128::MAX,
  };

  node::Message::NoticeTheseBlocks {
    magic: u32::MAX,
    gossip: true,
    blocks: vec![max_block],
    peers: vec![max_peer, max_peer, max_peer],
  }
}

fn max_message_serialize(c: &mut Criterion) {
  c.bench_function("max_message_serialize", |b| {
    let max_message = max_message();
    b.iter(|| {
      // util::bitvec_to_bytes(&se_bits)
      black_box(&max_message).proto_serialized()
    })
  });
}

fn max_message_deserialize(c: &mut Criterion) {
  c.bench_function("max_message_deserialize", |b| {
    let max_message = max_message();
    let se_bits = max_message.proto_serialized();
    let se_bytes = util::bitvec_to_bytes(&se_bits);
    b.iter(|| {
      let se_bits = util::bytes_to_bitvec(black_box(&se_bytes));
      node::Message::<net::Address>::proto_deserialized(&se_bits).unwrap()
    })
  });
}

/// Benchmarks deserialization and extraction of statements from a block
fn block_with_txs_deserialize(c: &mut Criterion) {
  c.bench_function("deserialize_block_with_txs", |b| {
    let code = include_str!("kdl/inc.kdl");

    let (_, base_stmt) = parser::parse_statement(code).unwrap();
    let bytes = util::bitvec_to_bytes(&base_stmt.proto_serialized());
    let transaction = node::Transaction::new(bytes).unwrap();

    let body = node::Body::fill_from(std::iter::repeat(transaction));

    let block =
      node::Block { body, prev: U256::MAX, time: u128::MAX, meta: u128::MAX };

    let se_bits = block.proto_serialized();
    let se_bytes = util::bitvec_to_bytes(&se_bits);

    b.iter(|| {
      let de_bits = util::bytes_to_bitvec(black_box(&se_bytes));
      let block = node::Block::proto_deserialized(&de_bits).unwrap();
      let transactions = node::extract_transactions(&block.body);
      for transaction in transactions {
        let de_stmt = transaction.to_statement().unwrap();
        debug_assert_eq!(base_stmt, de_stmt);
        drop(de_stmt);
      }
    })
  });
}

criterion_group!(
  serialization,
  max_message_serialize,
  max_message_deserialize,
  block_with_txs_deserialize
);

fn block_loading(c: &mut Criterion) {
  // creates a temporary directory
  let dir = temp_dir();
  // creates the storage with temp dir
  let storage = persistence::SimpleFileStorage::new(dir.clone()).unwrap();

  // writes `n` max blocks in disk
  let n = 1000;
  let block = max_block();
  for i in 0..n {
    storage.write_block(i, block.clone().hashed()).unwrap();
  }

  // empty `ProtoComm` to pass the created node
  let comm = net::EmptySocket;
  let addr = comm.get_addr().unwrap();

  // create Node
  let (_, mut node) =
    node::Node::new(dir.clone(), 0, addr, &[], vec![], comm, None, storage, None);

  // benchmark block loading
  c.bench_function("block_loading", |b| b.iter(|| node.load_blocks()));

  // removes all the blocks
  std::fs::remove_dir_all(&dir).unwrap();
}

criterion_group!(node, block_loading);

criterion_main!(khvm, serialization, node);
