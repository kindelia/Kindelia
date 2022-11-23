use std::collections::HashMap;

use bit_vec::BitVec;
use proptest::{collection::vec, proptest};

use kindelia_common::Name;
use kindelia_lang::ast;

use crate::test::strategies::{
  message, name, statement, u256 as u256_strategy,
};
use crate::{
  bits::{
    deserialize_fixlen_big, deserialize_list, deserialize_varlen, serialize_fixlen_big,
    serialize_list, serialize_varlen, ProtoSerialize,
  },
  net,
  node::Message,
  util::u256,
};

proptest! {
  #[test]
  fn serialize_deserialize_statements(statements in vec(statement(), 0..20)) {
    let s1 = ast::view_statements(&statements);
    let bits = statements.proto_serialized();
    let statements2 = Vec::proto_deserialized(&bits).unwrap();
    let s2 = ast::view_statements(&statements2);
    assert_eq!(s1, s2);
  }

  #[test]
  fn serialize_deserialize_message(message in message()) {
    let bits = message.proto_serialized();
    let message2: Message<net::Address> = Message::proto_deserialized(&bits).unwrap();
    assert_eq!(format!("{:?}", message), format!("{:?}", message2));
  }

  #[test]
  fn serialize_deserialize_name(name in name()) {
    let bits = name.proto_serialized();
    let name2 = Name::proto_deserialized(&bits).unwrap();
    assert_eq!(format!("{:?}", name), format!("{:?}", name2));
  }
}

#[test]
pub fn test_serializer_0() {
  let mut bits = BitVec::new();
  let a = u256(123);
  let b = u256(777);
  serialize_fixlen_big(10, &a, &mut bits);
  serialize_fixlen_big(16, &b, &mut bits);
  let mut index = 0;
  let x0 = deserialize_fixlen_big(10, &bits, &mut index).unwrap();
  let x1 = deserialize_fixlen_big(16, &bits, &mut index).unwrap();
  assert_eq!(a, x0);
  assert_eq!(b, x1);
}

#[test]
pub fn test_serializer_1() {
  let mut bits = BitVec::new();
  let a = 123;
  let b = 777;
  serialize_varlen(a, &mut bits);
  serialize_varlen(b, &mut bits);
  let mut index = 0;
  let x0 = deserialize_varlen(&bits, &mut index).unwrap();
  let x1 = deserialize_varlen(&bits, &mut index).unwrap();
  assert_eq!(a, x0);
  assert_eq!(b, x1);
}

#[test]
pub fn test_serializer_2() {
  let mut bits = BitVec::new();
  let a = u256(123);
  let b = u256(777);
  let c = u256(1000);
  let mut g_names = HashMap::new();
  let vals = vec![a, b, c];
  serialize_list(&vals, &mut bits, &mut g_names);
  let mut index = 0;
  let gots = deserialize_list(&bits, &mut index, &mut g_names).unwrap();
  assert_eq!(vals, gots);
}
