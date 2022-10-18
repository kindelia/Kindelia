#![feature(test)]
extern crate test;

// use rust_example::fib;

// use kindelia::net::ProtoAddr;
use test::Bencher;

use primitive_types::U256;

use kindelia::bits::ProtoSerialize;
use kindelia::hvm;
use kindelia::net;
use kindelia::node;
use kindelia::util;

fn max_message() -> node::Message<net::Address> {
  let max_block = node::Block {
    body: node::Body { data: [u8::MAX; node::MAX_BODY_SIZE].to_vec() },
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

  let max_message = node::Message::NoticeTheseBlocks {
    magic: u64::MAX,
    gossip: true,
    blocks: vec![max_block],
    peers: vec![max_peer.clone(), max_peer.clone(), max_peer.clone()],
  };

  max_message
}

#[bench]
fn max_message_serialize(b: &mut Bencher) {
  let max_message = max_message();
  b.iter(|| max_message.proto_serialized())
}

#[bench]
fn max_message_deserialize(b: &mut Bencher) {
  let max_message = max_message();
  let bits = max_message.proto_serialized();
  b.iter(|| node::Message::<net::Address>::proto_deserialized(&bits).unwrap())
}

// benchmark block serialization and extraction into transactions and statements
#[bench]
fn deserialize_block_with_transactions(b: &mut Bencher) {
  // read code
  let code = std::fs::read("./benches/codes/run_transactions.kdl").unwrap();
  let code = String::from_utf8(code.to_vec()).unwrap();

  let (_, statement) = hvm::read_statement(&code).unwrap(); // create statement from code
  let bytes = util::bitvec_to_bytes(&statement.proto_serialized()); // serialize it
  let transaction = node::Transaction::new(bytes); // create transaction from it

  // create body vector
  let mut body_vec = vec![0];
  // while body is not full
  while body_vec.len() < node::MAX_BODY_SIZE {
    // put the transaction in the body vector
    // it will raise an error if the body is full
    if let Err(_) =
      node::build_body_from_transaction(&transaction, &mut body_vec)
    {
      break;
    }
  }
  // create max block with the body
  let block = node::Block {
    body: node::Body { data: body_vec },
    hash: U256::MAX,
    prev: U256::MAX,
    time: u128::MAX,
    meta: u128::MAX,
  };
  let bits = block.proto_serialized(); // serialize it

  b.iter(|| {
    let block = node::Block::proto_deserialized(&bits).unwrap();
    let transactions = node::extract_transactions(&block.body);
    for transaction in transactions {
      let statement2 = transaction.to_statement().unwrap();
      assert_eq!(statement, statement2)
    }
  })
}
