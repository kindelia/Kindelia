use crate::{
  bits::{
    deserialize_fixlen, deserialize_list, deserialize_varlen, deserialized_statements,
    serialize_fixlen, serialize_list, serialize_varlen, serialized_statements,
  },
  hvm::{view_statements, Term},
  test::strategies::statement,
  util::u256,
};
use bit_vec::BitVec;
use proptest::{collection::vec, proptest};

proptest! {
  #[test]
  fn serialize_deserialize_statements(statements in vec(statement(), 0..20)) {
    let s1 = view_statements(&statements);
    let bits = serialized_statements(&statements);
    let statements2 = deserialized_statements(&bits);
    let s2 = view_statements(&statements2);
    assert_eq!(s1, s2);
  }
}

#[test]
pub fn test_serializer_0() {
  let mut bits = BitVec::new();
  let a = u256(123);
  let b = u256(777);
  serialize_fixlen(10, &a, &mut bits);
  serialize_fixlen(16, &b, &mut bits);
  let mut index = 0;
  let x0 = deserialize_fixlen(10, &bits, &mut index);
  let x1 = deserialize_fixlen(16, &bits, &mut index);
  assert_eq!(a, x0);
  assert_eq!(b, x1);
}

#[test]
pub fn test_serializer_1() {
  let mut bits = BitVec::new();
  let a = u256(123);
  let b = u256(777);
  serialize_varlen(&a, &mut bits);
  serialize_varlen(&b, &mut bits);
  let mut index = 0;
  let x0 = deserialize_varlen(&bits, &mut index);
  let x1 = deserialize_varlen(&bits, &mut index);
  assert_eq!(a, x0);
  assert_eq!(b, x1);
}

#[test]
pub fn test_serializer_2() {
  let mut bits = BitVec::new();
  let a = u256(123);
  let b = u256(777);
  let c = u256(1000);
  let vals = vec![a, b, c];
  serialize_list(|x, bits| serialize_fixlen(10, x, bits), &vals, &mut bits);
  println!("{:?}", bits);
  let mut index = 0;
  let gots = deserialize_list(|bits, ix| deserialize_fixlen(10, bits, ix), &bits, &mut index);
  assert_eq!(vals, gots);
}
