use bit_vec::BitVec;
use primitive_types::U256;
use crate::constants::*;
use crate::types::*;

// Serializers
// ===========

// A number with a known amount of bits

pub fn serialize_fixlen(size: u64, value: U256, bits: &mut BitVec) {
  for i in 0 .. size {
    bits.push((value >> i).as_u64() & 1 == 1);
  }
}

pub fn deserialize_fixlen(size: u64, bits: &BitVec, index: &mut u64) -> U256 {
  let mut result = U256::from(0 as u64);
  for i in 0 .. size {
    result = result * U256::from(2 as u64) + U256::from(bits[(*index + size - i - 1) as usize] as u64); 
  }
  *index = *index + size;
  result
}

// A number with an unknown amount of bits

pub fn serialize_varlen(value: U256, bits: &mut BitVec) {
  let mut value = value;
  while value > U256::from(0) {
    bits.push(true);
    bits.push(value.as_u64() & 1 == 1);
    value = value >> 1;
  }
  bits.push(false);
}

pub fn deserialize_varlen(bits: &BitVec, index: &mut u64) -> U256 {
  let mut val : U256 = U256::from(0);
  let mut add : U256 = U256::from(1);
  while bits[*index as usize] {
    val = val + if bits[*index as usize + 1] { add } else { U256::from(0) };
    add = add * 2;
    *index = *index + 2;
  }
  *index = *index + 1;
  return val;
}

// A bitvec with an unknown amount of bits

pub fn serialize_bits(data: &BitVec, bits: &mut BitVec) {
  for bit in data.iter() {
    bits.push(true);
    bits.push(bit);
  }
  bits.push(false);
}

pub fn deserialize_bits(bits: &BitVec, index: &mut u64) -> BitVec {
  let mut result = BitVec::new();
  while bits[*index as usize] {
    result.push(bits[*index as usize + 1]);
    *index = *index + 2;
  }
  *index = *index + 1;
  return result;
}

// Many elements

pub fn serialize_many<T>(serialize_one: impl Fn(T, &mut BitVec) -> (), values: Vec<T>, bits: &mut BitVec) {
  for x in values {
    bits.push(true);
    serialize_one(x, bits);
  }
  bits.push(false);
}

pub fn deserialize_many<T>(deserialize_one: impl Fn(&BitVec, &mut u64) -> T, bits: &BitVec, index: &mut u64) -> Vec<T> {
  let mut result = Vec::new();
  while bits[*index as usize] {
    *index = *index + 1;
    result.push(deserialize_one(bits, index));
  }
  result
}

// An address

pub fn serialize_address(address: Address, bits: &mut BitVec) {
  match address {
    Address::IPv4 { val0, val1, val2, val3, port } => {
      bits.push(false);
      serialize_fixlen(8, U256::from(val0), bits);
      serialize_fixlen(8, U256::from(val1), bits);
      serialize_fixlen(8, U256::from(val2), bits);
      serialize_fixlen(8, U256::from(val3), bits);
      serialize_fixlen(16, U256::from(port), bits);
    }
  }
}

pub fn deserialize_address(bits: &BitVec, index: &mut u64) -> Address {
  if bits[*index as usize] as u64 == 0 {
    *index = *index + 1;
    let val0 = deserialize_fixlen(8, bits, index).as_u64() as u8;
    let val1 = deserialize_fixlen(8, bits, index).as_u64() as u8;
    let val2 = deserialize_fixlen(8, bits, index).as_u64() as u8;
    let val3 = deserialize_fixlen(8, bits, index).as_u64() as u8;
    let port = deserialize_fixlen(16, bits, index).as_u64() as u16;
    return Address::IPv4 { val0, val1, val2, val3, port };
  } else {
    panic!("Bad address deserialization.");
  }
}

// A block

pub fn serialize_block(block: Block, bits: &mut BitVec) {
  serialize_fixlen(256, block.prev, bits);
  serialize_fixlen(64, U256::from(block.time), bits);
  serialize_fixlen(64, U256::from(block.rand), bits);
  serialize_bytes(BODY_SIZE as u64, &block.body.value, bits);
}

pub fn deserialize_block(bits: &BitVec, index: &mut u64) -> Block {
  let prev = deserialize_fixlen(256, bits, index);
  let time = deserialize_fixlen(64, bits, index).as_u64();
  let rand = deserialize_fixlen(64, bits, index).as_u64();
  let body = deserialize_bytes(BODY_SIZE as u64, bits, index);
  let mut value : [u8; BODY_SIZE] = [0; BODY_SIZE];
  for i in 0 .. BODY_SIZE {
    value[i] = body[i];
  }
  return Block { prev, time, rand, body: Body { value } };
}

// A hash

pub fn serialize_hash(hash: Hash, bits: &mut BitVec) {
  serialize_varlen(hash, bits);
}

pub fn deserialize_hash(bits: &BitVec, index: &mut u64) -> Hash {
  deserialize_varlen(bits, index)
}

// Bytes

pub fn serialize_bytes(size: u64, bytes: &[u8], bits: &mut BitVec) {
  for byte in bytes {
    serialize_fixlen(8, U256::from(*byte), bits);
  }
}

pub fn deserialize_bytes(size: u64, bits: &BitVec, index: &mut u64) -> Vec<u8> {
  let mut result = Vec::new();
  for _ in 0 .. size {
    result.push(deserialize_fixlen(8, bits, index).as_u64() as u8);
  }
  result
}

// A message

pub fn serialize_message(message: Message, bits: &mut BitVec) {
  match message {
    Message::PutPeers { peers } => {
      serialize_fixlen(4, U256::from(0), bits);
      serialize_many(serialize_address, peers, bits);
    }
    Message::PutBlock { block } => {
      serialize_fixlen(4, U256::from(1), bits);
      serialize_block(block, bits);
    }
    Message::AskBlock { bhash } => {
      serialize_fixlen(4, U256::from(2), bits);
      serialize_hash(bhash, bits);
    }
  }
}

pub fn deserialize_message(bits: &BitVec, index: &mut u64) -> Message {
  let code = deserialize_fixlen(4, bits, index).as_u64();
  match code {
    0 => {
      let peers = deserialize_many(deserialize_address, bits, index);
      Message::PutPeers { peers }
    }
    1 => {
      let block = deserialize_block(bits, index);
      Message::PutBlock { block }
    }
    2 => {
      let bhash = deserialize_hash(bits, index);
      Message::AskBlock { bhash }
    }
    _ => panic!("Bad message code.")
  }
}

pub fn serialized_message(message: Message) -> BitVec {
  let mut bits = BitVec::new();
  serialize_message(message, &mut bits);
  return bits;
}

pub fn deserialized_message(bits: &BitVec) -> Message {
  let mut index = 0;
  deserialize_message(bits, &mut index)
}


// Tests
// =====

pub fn test_serializer_0() {
  let mut bits = BitVec::new();
  serialize_fixlen(10, U256::from(123), &mut bits);
  serialize_fixlen(16, U256::from(777), &mut bits);
  println!("{:?}", bits);
  let mut index = 0;
  let x0 = deserialize_fixlen(10, &bits, &mut index);
  let x1 = deserialize_fixlen(16, &bits, &mut index);
  println!("{:?}", x0);
  println!("{:?}", x1);
}

pub fn test_serializer_1() {
  let mut bits = BitVec::new();
  serialize_varlen(U256::from(123), &mut bits);
  serialize_varlen(U256::from(777), &mut bits);
  println!("{:?}", bits);
  let mut index = 0;
  let x0 = deserialize_varlen(&bits, &mut index);
  let x1 = deserialize_varlen(&bits, &mut index);
  println!("{:?}", x0);
  println!("{:?}", x1);
}

pub fn test_serializer_2() {
  let mut bits = BitVec::new();
  let vals = vec![U256::from(123), U256::from(777), U256::from(1000)];
  serialize_many(|x,bits| serialize_fixlen(10, x, bits), vals, &mut bits);
  println!("{:?}", bits);
  let mut index = 0;
  let gots = deserialize_many(|bits,ix| deserialize_fixlen(10, bits, ix), &bits, &mut index);
  println!("{:?}", gots);
}
