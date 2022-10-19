use criterion::{black_box, criterion_group, criterion_main, Criterion};

use primitive_types::U256;

use kindelia::bits::ProtoSerialize;
use kindelia::hvm;
use kindelia::net;
use kindelia::node;
use kindelia::util;

fn max_message() -> node::Message<net::Address> {
  let max_block = node::Block {
    body: node::Body { data: vec![u8::MAX; node::MAX_BODY_SIZE] },
    hash: U256::MAX,
    prev: U256::MAX,
    time: u128::MAX,
    meta: u128::MAX,
  };

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
    magic: u64::MAX,
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
    let code = include_str!("code/inc.kdl");

    let (_, base_stmt) = hvm::read_statement(code).unwrap();
    let bytes = util::bitvec_to_bytes(&base_stmt.proto_serialized());
    let transaction = node::Transaction::new(bytes);

    let body = node::Body::fill_from(std::iter::repeat(transaction));

    let block = node::Block {
      body,
      hash: U256::MAX,
      prev: U256::MAX,
      time: u128::MAX,
      meta: u128::MAX,
    };

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

criterion_main!(serialization);
