use bit_vec::BitVec;
use crate::algorithms::*;
use crate::constants::*;
use crate::hvm::*;
use crate::types::*;
use primitive_types::U256;

// Serializers
// ===========

// A number with a known amount of bits

pub fn serialize_fixlen(size: u64, value: &U256, bits: &mut BitVec) {
  for i in 0 .. size {
    bits.push((value >> i).low_u64() & 1 == 1);
  }
}

pub fn deserialize_fixlen(size: u64, bits: &BitVec, index: &mut u64) -> U256 {
  let mut result = u256(0);
  for i in 0 .. size {
    result = result * u256(2) + u256(bits[(*index + size - i - 1) as usize] as u64); 
  }
  *index = *index + size;
  result
}

// A number with an unknown amount of bits

pub fn serialize_varlen(value: &U256, bits: &mut BitVec) {
  let mut value : U256 = *value;
  while value > u256(0) {
    bits.push(true);
    bits.push(value.low_u64() & 1 == 1);
    value = value >> u256(1);
  }
  bits.push(false);
}

pub fn deserialize_varlen(bits: &BitVec, index: &mut u64) -> U256 {
  let mut val : U256 = u256(0);
  let mut add : U256 = u256(1);
  while bits[*index as usize] {
    val = val + if bits[*index as usize + 1] { add } else { u256(0) };
    add = add.saturating_mul(u256(2));
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

pub fn serialize_many<T>(serialize_one: impl Fn(&T, &mut BitVec) -> (), values: &[T], bits: &mut BitVec) {
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
  *index = *index + 1;
  result
}

// An address

pub fn serialize_address(address: &Address, bits: &mut BitVec) {
  match address {
    Address::IPv4 { val0, val1, val2, val3, port } => {
      bits.push(false);
      serialize_fixlen(8, &u256(*val0 as u64), bits);
      serialize_fixlen(8, &u256(*val1 as u64), bits);
      serialize_fixlen(8, &u256(*val2 as u64), bits);
      serialize_fixlen(8, &u256(*val3 as u64), bits);
      serialize_fixlen(16, &u256(*port as u64), bits);
    }
  }
}

pub fn deserialize_address(bits: &BitVec, index: &mut u64) -> Address {
  if bits[*index as usize] as u64 == 0 {
    *index = *index + 1;
    let val0 = deserialize_fixlen(8, bits, index).low_u64() as u8;
    let val1 = deserialize_fixlen(8, bits, index).low_u64() as u8;
    let val2 = deserialize_fixlen(8, bits, index).low_u64() as u8;
    let val3 = deserialize_fixlen(8, bits, index).low_u64() as u8;
    let port = deserialize_fixlen(16, bits, index).low_u64() as u16;
    return Address::IPv4 { val0, val1, val2, val3, port };
  } else {
    panic!("Bad address deserialization.");
  }
}

pub fn serialized_address(address: &Address) -> BitVec {
  let mut bits = BitVec::new();
  serialize_address(address, &mut bits);
  return bits;
}

pub fn deserialized_address(bits: &BitVec) -> Address {
  let mut index = 0;
  deserialize_address(bits, &mut index)
}

// A peer

pub fn serialize_peer(peer: &Peer, bits: &mut BitVec) {
  serialize_address(&peer.address, bits);
  serialize_fixlen(48, &u256(peer.seen_at as u64), bits);
}

pub fn deserialize_peer(bits: &BitVec, index: &mut u64) -> Peer {
  let address = deserialize_address(bits, index);
  let seen_at = deserialize_fixlen(48, bits, index).low_u64();
  return Peer { address, seen_at };
}

pub fn serialized_peer(peer: &Peer) -> BitVec {
  let mut bits = BitVec::new();
  serialize_peer(peer, &mut bits);
  return bits;
}

pub fn deserialized_peer(bits: &BitVec) -> Peer {
  let mut index = 0;
  deserialize_peer(bits, &mut index)
}

// A block

pub fn serialize_block(block: &Block, bits: &mut BitVec) {
  serialize_fixlen(256, &block.prev, bits);
  serialize_fixlen(64, &u256(block.time), bits);
  serialize_fixlen(64, &u256(block.rand), bits);
  serialize_bytes(BODY_SIZE as u64, &block.body.value, bits);
}

pub fn deserialize_block(bits: &BitVec, index: &mut u64) -> Block {
  let prev = deserialize_fixlen(256, bits, index);
  let time = deserialize_fixlen(64, bits, index).low_u64();
  let rand = deserialize_fixlen(64, bits, index).low_u64();
  let body = deserialize_bytes(BODY_SIZE as u64, bits, index);
  let mut value : [u8; BODY_SIZE] = [0; BODY_SIZE];
  for i in 0 .. BODY_SIZE {
    value[i] = body[i];
  }
  return Block { prev, time, rand, body: Body { value } };
}

pub fn serialized_block(block: &Block) -> BitVec {
  let mut bits = BitVec::new();
  serialize_block(block, &mut bits);
  return bits;
}

pub fn deserialized_block(bits: &BitVec) -> Block {
  let mut index = 0;
  deserialize_block(bits, &mut index)
}

// A hash

pub fn serialize_hash(hash: &Hash, bits: &mut BitVec) {
  serialize_varlen(hash, bits);
}

pub fn deserialize_hash(bits: &BitVec, index: &mut u64) -> Hash {
  deserialize_varlen(bits, index)
}

// Bytes

pub fn serialize_bytes(size: u64, bytes: &[u8], bits: &mut BitVec) {
  for byte in bytes {
    serialize_fixlen(8, &u256(*byte as u64), bits);
  }
}

pub fn deserialize_bytes(size: u64, bits: &BitVec, index: &mut u64) -> Vec<u8> {
  let mut result = Vec::new();
  for _ in 0 .. size {
    result.push(deserialize_fixlen(8, bits, index).low_u64() as u8);
  }
  result
}

// A message

pub fn serialize_message(message: &Message, bits: &mut BitVec) {
  match message {
    //Message::PutPeers { peers } => {
      //serialize_fixlen(4, &u256(0), bits);
      //serialize_many(serialize_address, peers, bits);
    //}
    Message::PutBlock { block, peers } => {
      serialize_fixlen(4, &u256(0), bits);
      serialize_block(block, bits);
      serialize_many(serialize_peer, peers, bits);
    }
    Message::AskBlock { bhash } => {
      serialize_fixlen(4, &u256(1), bits);
      serialize_hash(bhash, bits);
    }
  }
}

pub fn deserialize_message(bits: &BitVec, index: &mut u64) -> Message {
  let code = deserialize_fixlen(4, bits, index).low_u64();
  match code {
    //0 => {
      //let peers = deserialize_many(deserialize_address, bits, index);
      //Message::PutPeers { peers }
    //}
    0 => {
      let block = deserialize_block(bits, index);
      let peers = deserialize_many(deserialize_peer, bits, index);
      Message::PutBlock { block, peers }
    }
    1 => {
      let bhash = deserialize_hash(bits, index);
      Message::AskBlock { bhash }
    }
    _ => panic!("Bad message code.")
  }
}

pub fn serialized_message(message: &Message) -> BitVec {
  let mut bits = BitVec::new();
  serialize_message(message, &mut bits);
  return bits;
}

pub fn deserialized_message(bits: &BitVec) -> Message {
  let mut index = 0;
  deserialize_message(bits, &mut index)
}

//pub enum Term {
  //Var { name: u64 },
  //Dup { nam0: u64, nam1: u64, expr: Box<Term>, body: Box<Term> },
  //Lam { name: u64, body: Box<Term> },
  //App { func: Box<Term>, argm: Box<Term> },
  //Ctr { name: u64, args: Vec<Term> },
  //Fun { name: u64, args: Vec<Term> },
  //U60 { numb: u64 },
  //Op2 { oper: u64, val0: Box<Term>, val1: Box<Term> },
//}

//pub enum Oper {
  //Add, Sub, Mul, Div,
  //Mod, And, Or,  Xor,
  //Shl, Shr, Lte, Ltn,
  //Eql, Gte, Gtn, Neq,
//}

//pub enum Action {
  //Def { name: u64, func: Vec<(Term, Term)> },
  //Run { name: u64, expr: Term },
//}

// A Term

pub fn serialize_term(term: &Term, bits: &mut BitVec) {
  match term {
    Term::Var { name } => {
      serialize_fixlen(3, &u256(0), bits);
      serialize_fixlen(18, &u256(*name as u64), bits);
    }
    Term::Dup { nam0, nam1, expr, body } => {
      serialize_fixlen(3, &u256(1), bits);
      serialize_fixlen(18, &u256(*nam0 as u64), bits);
      serialize_fixlen(18, &u256(*nam1 as u64), bits);
      serialize_term(expr, bits);
      serialize_term(body, bits);
    }
    Term::Lam { name, body } => {
      serialize_fixlen(3, &u256(2), bits);
      serialize_fixlen(18, &u256(*name as u64), bits);
      serialize_term(body, bits);
    }
    Term::App { func, argm } => {
      serialize_fixlen(3, &u256(3), bits);
      serialize_term(func, bits);
      serialize_term(argm, bits);
    }
    Term::Ctr { name, args } => {
      serialize_fixlen(3, &u256(4), bits);
      serialize_fixlen(30, &u256(*name as u64), bits);
      serialize_many(serialize_term, args, bits);
    }
    Term::Fun { name, args } => {
      serialize_fixlen(3, &u256(5), bits);
      serialize_fixlen(30, &u256(*name as u64), bits);
      serialize_many(serialize_term, args, bits);
    }
    Term::U60 { numb } => {
      serialize_fixlen(3, &u256(6), bits);
      serialize_fixlen(60, &u256(*numb as u64), bits);
    }
    Term::Op2 { oper, val0, val1 } => {
      serialize_fixlen(3, &u256(7), bits);
      serialize_fixlen(8, &u256(*oper as u64), bits);
      serialize_term(val0, bits);
      serialize_term(val1, bits);
    }
  }
}

pub fn deserialize_term(bits: &BitVec, index: &mut u64) -> Term {
  let tag = deserialize_fixlen(3, bits, index);
  //println!("- tag.: {} {:?}", tag, bits.clone().split_off(*index as usize));
  match tag.low_u64() {
    0 => {
      let name = deserialize_fixlen(18, bits, index).low_u64();
      Term::Var { name }
    }
    1 => {
      let nam0 = deserialize_fixlen(18, bits, index).low_u64();
      let nam1 = deserialize_fixlen(18, bits, index).low_u64();
      let expr = Box::new(deserialize_term(bits, index));
      let body = Box::new(deserialize_term(bits, index));
      Term::Dup { nam0, nam1, expr, body }
    }
    2 => {
      let name = deserialize_fixlen(18, bits, index).low_u64();
      let body = Box::new(deserialize_term(bits, index));
      Term::Lam { name, body }
    }
    3 => {
      let func = Box::new(deserialize_term(bits, index));
      let argm = Box::new(deserialize_term(bits, index));
      Term::App { func, argm }
    }
    4 => {
      let name = deserialize_fixlen(30, bits, index).low_u64();
      let args = deserialize_many(|bits, index| {
        let term = deserialize_term(bits, index);
        return term;
      }, bits, index);
      Term::Ctr { name, args }
    }
    5 => {
      let name = deserialize_fixlen(30, bits, index).low_u64();
      let args = deserialize_many(deserialize_term, bits, index);
      Term::Fun { name, args }
    }
    6 => {
      let numb = deserialize_fixlen(60, bits, index).low_u64();
      Term::U60 { numb }
    }
    7 => {
      let oper = deserialize_fixlen(8, bits, index).low_u64();
      let val0 = Box::new(deserialize_term(bits, index));
      let val1 = Box::new(deserialize_term(bits, index));
      Term::Op2 { oper, val0, val1 }
    }
    _ => panic!("unknown term tag"),
  }
}

// An Action

pub fn serialize_action(action: &Action, bits: &mut BitVec) {
  match action {
    Action::Fun { name, arit, func } => {
      serialize_fixlen(4, &u256(0), bits);
      serialize_fixlen(30, &u256(*name as u64), bits);
      serialize_fixlen(4, &u256(*arit as u64), bits);
      serialize_many(|rule, bits| {
        serialize_term(&rule.0, bits);
        serialize_term(&rule.1, bits);
      }, func, bits);
    }
    Action::Ctr { name, arit } => {
      serialize_fixlen(4, &u256(1), bits);
      serialize_fixlen(30, &u256(*name as u64), bits);
      serialize_fixlen(4, &u256(*arit as u64), bits);
    }
    Action::Run { name, expr } => {
      serialize_fixlen(4, &u256(2), bits);
      serialize_fixlen(30, &u256(*name as u64), bits);
      serialize_term(expr, bits);
    }
  }
}

pub fn deserialize_action(bits: &BitVec, index: &mut u64) -> Action {
  let tag = deserialize_fixlen(4, bits, index);
  match tag.low_u64() {
    0 => {
      let name = deserialize_fixlen(30, bits, index).low_u64();
      let arit = deserialize_fixlen(4, bits, index).low_u64();
      let func = deserialize_many(|bits, index| {
        let lhs  = deserialize_term(bits, index);
        let rhs  = deserialize_term(bits, index);
        let rule = (lhs, rhs);
        return rule;
      }, bits, index);
      Action::Fun { name, arit, func }
    }
    1 => {
      let name = deserialize_fixlen(30, bits, index).low_u64();
      let arit = deserialize_fixlen(4, bits, index).low_u64();
      Action::Ctr { name, arit }
    }
    2 => {
      let name = deserialize_fixlen(30, bits, index).low_u64();
      let expr = deserialize_term(bits, index);
      Action::Run { name, expr }
    }
    _ => panic!("unknown action tag"),
  }
}

pub fn serialized_action(action: &Action) -> BitVec {
  let mut bits = BitVec::new();
  serialize_action(action, &mut bits);
  return bits;
}

pub fn deserialized_action(bits: &BitVec) -> Action {
  let mut index = 0;
  deserialize_action(bits, &mut index)
}

// Many actions

pub fn serialize_actions(actions: &[Action], bits: &mut BitVec) {
  serialize_many(serialize_action, actions, bits);
}

pub fn deserialize_actions(bits: &BitVec, index: &mut u64) -> Vec<Action> {
  deserialize_many(deserialize_action, bits, index)
}

pub fn serialized_actions(actions: &[Action]) -> BitVec {
  let mut bits = BitVec::new();
  serialize_actions(actions, &mut bits);
  return bits;
}

pub fn deserialized_actions(bits: &BitVec) -> Vec<Action> {
  let mut index = 0;
  deserialize_actions(bits, &mut index)
}

// Tests
// =====

pub fn test_serializer_0() {
  let mut bits = BitVec::new();
  serialize_fixlen(10, &u256(123), &mut bits);
  serialize_fixlen(16, &u256(777), &mut bits);
  println!("{:?}", bits);
  let mut index = 0;
  let x0 = deserialize_fixlen(10, &bits, &mut index);
  let x1 = deserialize_fixlen(16, &bits, &mut index);
  println!("{:?}", x0);
  println!("{:?}", x1);
}

pub fn test_serializer_1() {
  let mut bits = BitVec::new();
  serialize_varlen(&u256(123), &mut bits);
  serialize_varlen(&u256(777), &mut bits);
  println!("{:?}", bits);
  let mut index = 0;
  let x0 = deserialize_varlen(&bits, &mut index);
  let x1 = deserialize_varlen(&bits, &mut index);
  println!("{:?}", x0);
  println!("{:?}", x1);
}

pub fn test_serializer_2() {
  let mut bits = BitVec::new();
  let vals = vec![u256(123), u256(777), u256(1000)];
  serialize_many(|x,bits| serialize_fixlen(10, x, bits), &vals, &mut bits);
  println!("{:?}", bits);
  let mut index = 0;
  let gots = deserialize_many(|bits,ix| deserialize_fixlen(10, bits, ix), &bits, &mut index);
  println!("{:?}", gots);
}
