#![allow(dead_code)]
#![allow(clippy::style)]

use bit_vec::BitVec;
use std::collections::HashMap;

use crate::common::Name;
use crate::crypto;
use crate::hvm::*;
use crate::net;
use crate::net::ProtoAddr;
use crate::node::*;
use crate::util::*;

use primitive_types::U256;

pub type Names = HashMap<u128, u128>;

// Serializers
// ===========

// A number with a known amount of bits

#[allow(unused_variables)]
pub fn serialize_fixlen(
  size: u128,
  value: &U256,
  bits: &mut BitVec,
  names: &mut Names,
) {
  for i in 0..size {
    bits.push((value >> i).low_u128() & 1 == 1);
  }
}

#[allow(unused_variables)]
pub fn deserialize_fixlen(
  size: u128,
  bits: &BitVec,
  index: &mut u128,
  names: &mut Names,
) -> Option<U256> {
  let mut result = u256(0);
  for i in 0..size {
    let index = (*index + size - i - 1) as usize;
    if index >= bits.len() {
      return None;
    }
    result = result * u256(2) + u256(bits[index] as u128);
  }
  *index = *index + size;
  Some(result)
}

// A number with an unknown amount of bits

#[allow(unused_variables)]
pub fn serialize_varlen(value: &U256, bits: &mut BitVec, names: &mut Names) {
  let mut value: U256 = *value;
  while value > u256(0) {
    bits.push(true);
    bits.push(value.low_u128() & 1 == 1);
    value = value >> u256(1);
  }
  bits.push(false);
}

#[allow(unused_variables)]
pub fn deserialize_varlen(
  bits: &BitVec,
  index: &mut u128,
  names: &mut Names,
) -> Option<U256> {
  let mut val: U256 = u256(0);
  let mut add: U256 = u256(1);
  while bits.get(*index as usize)? {
    val = val + if bits.get(*index as usize + 1)? { add } else { u256(0) };
    add = add.saturating_mul(u256(2));
    *index = *index + 2;
  }
  *index = *index + 1;
  Some(val)
}

// A number

pub fn serialize_number(value: &U256, bits: &mut BitVec, names: &mut Names) {
  let size = value.bits() as u128;
  serialize_varlen(&u256(size), bits, names);
  serialize_fixlen(size, value, bits, names);
}

pub fn deserialize_number(
  bits: &BitVec,
  index: &mut u128,
  names: &mut Names,
) -> Option<U256> {
  let size = deserialize_varlen(&bits, index, names)?.low_u128();
  let numb = deserialize_fixlen(size, &bits, index, names)?;
  Some(numb)
}

// A bitvec with an unknown amount of bits

#[allow(unused_variables)]
pub fn serialize_bits(data: &BitVec, bits: &mut BitVec, names: &mut Names) {
  for bit in data.iter() {
    bits.push(true);
    bits.push(bit);
  }
  bits.push(false);
}

#[allow(unused_variables)]
pub fn deserialize_bits(
  bits: &BitVec,
  index: &mut u128,
  names: &mut Names,
) -> Option<BitVec> {
  let mut result = BitVec::new();
  while bits.get(*index as usize)? {
    result.push(bits.get(*index as usize + 1)?);
    *index = *index + 2;
  }
  *index = *index + 1;
  Some(result)
}

// Many elements, unknown length

pub fn serialize_list<T: ProtoSerialize>(
  values: &[T],
  bits: &mut BitVec,
  names: &mut Names,
) {
  for x in values {
    bits.push(true);
    x.proto_serialize(bits, names);
  }
  bits.push(false);
}

pub fn deserialize_list<T: ProtoSerialize>(
  bits: &BitVec,
  index: &mut u128,
  names: &mut Names,
) -> Option<Vec<T>> {
  let mut result = Vec::new();
  while bits.get(*index as usize)? {
    *index = *index + 1;
    result.push(T::proto_deserialize(bits, index, names)?);
  }
  *index = *index + 1;
  Some(result)
}

// Many elements, known length

pub fn serialize_vector<T: ProtoSerialize>(
  size: u128,
  data: &[T],
  bits: &mut BitVec,
  names: &mut Names,
) {
  if data.len() as u128 != size {
    panic!("Incorrect serialization vector size.");
  }
  for x in data {
    x.proto_serialize(bits, names);
  }
}

pub fn deserialize_vector<T: ProtoSerialize>(
  size: u128,
  bits: &BitVec,
  index: &mut u128,
  names: &mut Names,
) -> Option<Vec<T>> {
  let mut result = Vec::new();
  for _ in 0..size {
    result.push(T::proto_deserialize(bits, index, names)?);
  }
  Some(result)
}

// A block

pub fn serialized_block_size(block: &Block) -> u128 {
  return 32 + 16 + 16 + 2 + block.body.data.len() as u128;
}

// A hash

// Bytes

pub fn serialize_bytes(
  size: u128,
  bytes: &[u8],
  bits: &mut BitVec,
  names: &mut Names,
) {
  if size as usize != bytes.len() {
    panic!("Incorrect serialize_bytes size.");
  }
  for byte in bytes {
    serialize_fixlen(8, &u256(*byte as u128), bits, names);
  }
}

pub fn deserialize_bytes(
  size: u128,
  bits: &BitVec,
  index: &mut u128,
  names: &mut Names,
) -> Option<Vec<u8>> {
  let mut result = Vec::new();
  for _ in 0..size {
    result.push(deserialize_fixlen(8, bits, index, names)?.low_u128() as u8);
  }
  Some(result)
}

pub trait ProtoSerialize
where
  Self: Sized,
{
  fn proto_serialize(&self, bits: &mut BitVec, names: &mut Names);
  fn proto_deserialize(
    bits: &BitVec,
    index: &mut u128,
    names: &mut Names,
  ) -> Option<Self>;

  fn proto_serialized(&self) -> BitVec {
    let mut bits = BitVec::new();
    self.proto_serialize(&mut bits, &mut HashMap::new());
    return bits;
  }
  fn proto_deserialized(bits: &BitVec) -> Option<Self> {
    Self::proto_deserialize(bits, &mut 0, &mut HashMap::new())
  }
}

impl ProtoSerialize for Option<crypto::Signature> {
  fn proto_serialize(&self, bits: &mut BitVec, names: &mut Names) {
    if let Some(sign) = self {
      serialize_fixlen(1, &u256(1), bits, names);
      serialize_bytes(65, &sign.0, bits, names);
    } else {
      serialize_fixlen(1, &u256(0), bits, names);
    }
  }

  // The double Option layer keeps it consistent, since the returned value IS an Option
  fn proto_deserialize(
    bits: &BitVec,
    index: &mut u128,
    names: &mut Names,
  ) -> Option<Self> {
    match deserialize_fixlen(1, bits, index, names)?.low_u128() {
      1 => {
        let data: Option<[u8; 65]> =
          deserialize_bytes(65, bits, index, names)?.try_into().ok();
        if let Some(data) = data {
          Some(Some(crypto::Signature(data)))
        } else {
          None
        }
      }
      _ => Some(None),
    }
  }
}

impl ProtoSerialize for Name {
  fn proto_serialize(&self, bits: &mut BitVec, names: &mut Names) {
    if let Some(id) = names.get(self) {
      bits.push(true);
      serialize_varlen(&u256(*id), bits, names);
    } else {
      let mut name = **self;
      names.insert(name, names.len() as u128);
      bits.push(false); // compressed-name flag
      while name > 0 {
        bits.push(true);
        serialize_fixlen(6, &u256(name & 0x3F), bits, names);
        name = name >> 6;
      }
      bits.push(false);
    }
  }

  fn proto_deserialize(
    bits: &BitVec,
    index: &mut u128,
    names: &mut Names,
  ) -> Option<Self> {
    let mut nam: u128 = 0;
    let mut add: u128 = 1;
    let compressed = bits.get(*index as usize)?;
    *index += 1;
    if compressed {
      let id = deserialize_varlen(bits, index, names)?.low_u128();
      let nm = *names.get(&id)?;
      Some(Name::from_u128_unchecked(nm))
    } else {
      while bits.get(*index as usize)? {
        *index += 1;
        let got = deserialize_fixlen(6, bits, index, names)?.low_u128();
        nam = nam + add * got;
        add = add.saturating_mul(64);
      }
      *index = *index + 1;
      names.insert(names.len() as u128, nam);
      Some(Name::from_u128_unchecked(nam))
    }
  }
}

// TODO: avoid recursion here; important for checksum functionality
impl ProtoSerialize for Term {
  fn proto_serialize(&self, bits: &mut BitVec, names: &mut Names) {
    match self {
      Term::Var { name } => {
        serialize_fixlen(3, &u256(0), bits, names);
        name.proto_serialize(bits, names);
      }
      Term::Dup { nam0, nam1, expr, body } => {
        serialize_fixlen(3, &u256(1), bits, names);
        nam0.proto_serialize(bits, names);
        nam1.proto_serialize(bits, names);
        expr.proto_serialize(bits, names);
        body.proto_serialize(bits, names);
      }
      Term::Lam { name, body } => {
        serialize_fixlen(3, &u256(2), bits, names);
        name.proto_serialize(bits, names);
        body.proto_serialize(bits, names);
      }
      Term::App { func, argm } => {
        serialize_fixlen(3, &u256(3), bits, names);
        func.proto_serialize(bits, names);
        argm.proto_serialize(bits, names);
      }
      Term::Ctr { name, args } => {
        serialize_fixlen(3, &u256(4), bits, names);
        name.proto_serialize(bits, names);
        serialize_list(&args, bits, names);
      }
      Term::Fun { name, args } => {
        serialize_fixlen(3, &u256(5), bits, names);
        name.proto_serialize(bits, names);
        serialize_list(&args, bits, names);
      }
      Term::Num { numb } => {
        serialize_fixlen(3, &u256(6), bits, names);
        serialize_number(&u256(**numb), bits, names);
      }
      Term::Op2 { oper, val0, val1 } => {
        serialize_fixlen(3, &u256(7), bits, names);
        serialize_fixlen(4, &u256(*oper as u128), bits, names);
        val0.proto_serialize(bits, names);
        val1.proto_serialize(bits, names);
      }
    }
  }

  fn proto_deserialize(
    bits: &BitVec,
    index: &mut u128,
    names: &mut Names,
  ) -> Option<Self> {
    let tag = deserialize_fixlen(3, bits, index, names)?;
    //println!("- tag.: {} {:?}", tag, bits.clone().split_off(*index as usize));
    match tag.low_u128() {
      0 => {
        let name = Name::proto_deserialize(bits, index, names)?;
        Some(Term::var(name))
      }
      1 => {
        let nam0 = Name::proto_deserialize(bits, index, names)?;
        let nam1 = Name::proto_deserialize(bits, index, names)?;
        let expr = Box::new(Self::proto_deserialize(bits, index, names)?);
        let body = Box::new(Self::proto_deserialize(bits, index, names)?);
        let term = Term::dup(nam0, nam1, expr, body);
        Some(term)
      }
      2 => {
        let name = Name::proto_deserialize(bits, index, names)?;
        let body = Box::new(Self::proto_deserialize(bits, index, names)?);
        let term = Term::lam(name, body);
        Some(term)
      }
      3 => {
        let func = Box::new(Self::proto_deserialize(bits, index, names)?);
        let argm = Box::new(Self::proto_deserialize(bits, index, names)?);
        let term = Term::app(func, argm);
        Some(term)
      }
      4 => {
        let name = Name::proto_deserialize(bits, index, names)?;
        let args = deserialize_list(bits, index, names)?;
        let term = Term::ctr(name, args);
        Some(term)
      }
      5 => {
        let name = Name::proto_deserialize(bits, index, names)?;
        let args = deserialize_list(bits, index, names)?;
        let term = Term::fun(name, args);
        Some(term)
      }
      6 => {
        let numb = deserialize_number(bits, index, names)?.low_u128();
        let numb: U120 = numb.try_into().ok()?;
        let term = Term::num(numb);
        Some(term)
      }
      7 => {
        let oper = deserialize_fixlen(4, bits, index, names)?.low_u128();
        let oper = oper.try_into().ok()?;
        let val0 = Box::new(Self::proto_deserialize(bits, index, names)?);
        let val1 = Box::new(Self::proto_deserialize(bits, index, names)?);
        let term = Term::op2(oper, val0, val1);
        Some(term)
      }
      _ => None,
    }
  }
}

impl ProtoSerialize for Rule {
  fn proto_serialize(&self, bits: &mut BitVec, names: &mut Names) {
    self.lhs.proto_serialize(bits, names);
    self.rhs.proto_serialize(bits, names);
  }

  fn proto_deserialize(
    bits: &BitVec,
    index: &mut u128,
    names: &mut Names,
  ) -> Option<Self> {
    let lhs = Term::proto_deserialize(bits, index, names)?;
    let rhs = Term::proto_deserialize(bits, index, names)?;
    Some(Rule { lhs, rhs })
  }
}

impl ProtoSerialize for Func {
  fn proto_serialize(&self, bits: &mut BitVec, names: &mut Names) {
    serialize_list(&self.rules, bits, names);
  }

  fn proto_deserialize(
    bits: &BitVec,
    index: &mut u128,
    names: &mut Names,
  ) -> Option<Self> {
    let rules = deserialize_list(bits, index, names)?;
    Some(Func { rules })
  }
}

impl ProtoSerialize for Statement {
  fn proto_serialize(&self, bits: &mut BitVec, names: &mut Names) {
    match self {
      Statement::Fun { name, args, func, init, sign } => {
        serialize_fixlen(4, &u256(0), bits, names);
        name.proto_serialize(bits, names);
        serialize_list(args, bits, names);
        func.proto_serialize(bits, names);
        init.proto_serialize(bits, names);
        sign.proto_serialize(bits, names);
      }
      Statement::Ctr { name, args, sign } => {
        serialize_fixlen(4, &u256(1), bits, names);
        name.proto_serialize(bits, names);
        serialize_list(args, bits, names);
        sign.proto_serialize(bits, names);
      }
      Statement::Run { expr, sign } => {
        serialize_fixlen(4, &u256(2), bits, names);
        expr.proto_serialize(bits, names);
        sign.proto_serialize(bits, names);
      }
      Statement::Reg { name, ownr, sign } => {
        serialize_fixlen(4, &u256(3), bits, names);
        name.proto_serialize(bits, names);
        serialize_fixlen(128, &u256(**ownr), bits, names);
        sign.proto_serialize(bits, names);
      }
    }
  }

  fn proto_deserialize(
    bits: &BitVec,
    index: &mut u128,
    names: &mut Names,
  ) -> Option<Self> {
    let tag = deserialize_fixlen(4, bits, index, names)?.low_u128();
    match tag {
      0 => {
        let name = Name::proto_deserialize(bits, index, names)?;
        let args = deserialize_list(bits, index, names)?;
        let func = Func::proto_deserialize(bits, index, names)?;
        let init = Term::proto_deserialize(bits, index, names)?;
        let sign = Option::proto_deserialize(bits, index, names)?;
        Some(Statement::Fun { name, args, func, init, sign })
      }
      1 => {
        let name = Name::proto_deserialize(bits, index, names)?;
        let args = deserialize_list(bits, index, names)?;
        let sign = Option::proto_deserialize(bits, index, names)?;
        Some(Statement::Ctr { name, args, sign })
      }
      2 => {
        let expr = Term::proto_deserialize(bits, index, names)?;
        let sign = Option::proto_deserialize(bits, index, names)?;
        Some(Statement::Run { expr, sign })
      }
      3 => {
        let name = Name::proto_deserialize(bits, index, names)?;
        let ownr = deserialize_fixlen(128, bits, index, names)?.low_u128();
        let ownr: U120 = ownr.try_into().ok()?;
        let sign = Option::proto_deserialize(bits, index, names)?;
        Some(Statement::Reg { name, ownr, sign })
      }
      _ => None,
    }
  }
}

impl ProtoSerialize for Vec<Statement> {
  fn proto_serialize(&self, bits: &mut BitVec, names: &mut Names) {
    serialize_list(self, bits, names);
  }

  fn proto_deserialize(
    bits: &BitVec,
    index: &mut u128,
    names: &mut Names,
  ) -> Option<Self> {
    deserialize_list(bits, index, names)
  }
}

impl ProtoSerialize for Hash {
  fn proto_serialize(&self, bits: &mut BitVec, names: &mut Names) {
    serialize_fixlen(256, self, bits, names);
  }

  fn proto_deserialize(
    bits: &BitVec,
    index: &mut u128,
    names: &mut Names,
  ) -> Option<Self> {
    deserialize_fixlen(256, bits, index, names)
  }
}

impl ProtoSerialize for Block {
  fn proto_serialize(&self, bits: &mut BitVec, names: &mut Names) {
    serialize_fixlen(256, &self.prev, bits, names);
    serialize_fixlen(128, &u256(self.time), bits, names);
    serialize_fixlen(128, &u256(self.meta), bits, names);
    serialize_fixlen(16, &u256(self.body.data.len() as u128), bits, names);
    serialize_bytes(self.body.data.len() as u128, &self.body.data, bits, names);
  }

  fn proto_deserialize(
    bits: &BitVec,
    index: &mut u128,
    names: &mut Names,
  ) -> Option<Self> {
    let prev = deserialize_fixlen(256, bits, index, names)?;
    let time = deserialize_fixlen(128, bits, index, names)?.low_u128();
    let meta = deserialize_fixlen(128, bits, index, names)?.low_u128();
    let size = deserialize_fixlen(16, bits, index, names)?.low_u128();
    let data = deserialize_bytes(size, bits, index, names)?;
    let body = Body { data };
    return Some(new_block(prev, time, meta, body));
  }
}

impl ProtoSerialize for net::Address {
  fn proto_serialize(&self, bits: &mut BitVec, names: &mut Names) {
    match self {
      net::Address::IPv4 { val0, val1, val2, val3, port } => {
        bits.push(false);
        serialize_fixlen(8, &u256(*val0 as u128), bits, names);
        serialize_fixlen(8, &u256(*val1 as u128), bits, names);
        serialize_fixlen(8, &u256(*val2 as u128), bits, names);
        serialize_fixlen(8, &u256(*val3 as u128), bits, names);
        serialize_fixlen(16, &u256(*port as u128), bits, names);
      }
    }
  }

  fn proto_deserialize(
    bits: &BitVec,
    index: &mut u128,
    names: &mut Names,
  ) -> Option<net::Address> {
    if bits[*index as usize] as u128 == 0 {
      *index = *index + 1;
      let val0 = deserialize_fixlen(8, bits, index, names)?.low_u128() as u8;
      let val1 = deserialize_fixlen(8, bits, index, names)?.low_u128() as u8;
      let val2 = deserialize_fixlen(8, bits, index, names)?.low_u128() as u8;
      let val3 = deserialize_fixlen(8, bits, index, names)?.low_u128() as u8;
      let port = deserialize_fixlen(16, bits, index, names)?.low_u128() as u16;
      return Some(net::Address::IPv4 { val0, val1, val2, val3, port });
    } else {
      return None;
    }
  }
}

impl<A: ProtoAddr> ProtoSerialize for Peer<A> {
  fn proto_serialize(&self, bits: &mut BitVec, names: &mut Names) {
    self.address.proto_serialize(bits, names);
    serialize_fixlen(48, &u256(self.seen_at as u128), bits, names);
  }

  fn proto_deserialize(
    bits: &BitVec,
    index: &mut u128,
    names: &mut Names,
  ) -> Option<Self> {
    let address = A::proto_deserialize(bits, index, names)?;
    let seen_at = deserialize_fixlen(48, bits, index, names)?.low_u128();
    return Some(Peer { address, seen_at });
  }
}

impl<A: ProtoAddr> ProtoSerialize for Message<A> {
  fn proto_serialize(&self, bits: &mut BitVec, names: &mut Names) {
    match self {
      // This is supposed to use < 1500 bytes when blocks = 1, to avoid UDP fragmentation
      Message::NoticeTheseBlocks { magic, gossip, blocks, peers } => {
        serialize_fixlen(64, &u256(*magic as u128), bits, names);
        serialize_fixlen(4, &u256(0), bits, names);
        serialize_fixlen(1, &u256(*gossip as u128), bits, names);
        serialize_list(&blocks, bits, names);
        serialize_list(peers, bits, names);
      }
      Message::GiveMeThatBlock { magic, bhash } => {
        serialize_fixlen(64, &u256(*magic as u128), bits, names);
        serialize_fixlen(4, &u256(1), bits, names);
        bhash.proto_serialize(bits, names);
      }
      Message::PleaseMineThisTransaction { magic, tx } => {
        serialize_fixlen(64, &u256(*magic as u128), bits, names);
        let tx_len = tx.len();
        if tx_len == 0 {
          panic!("Invalid transaction length.");
        } else {
          serialize_fixlen(4, &u256(2), bits, names);
          serialize_fixlen(16, &u256(tx_len as u128), bits, names);
          serialize_bytes(tx_len as u128, tx, bits, names);
        }
      }
    }
  }
  fn proto_deserialize(
    bits: &BitVec,
    index: &mut u128,
    names: &mut Names,
  ) -> Option<Self> {
    let magic = deserialize_fixlen(64, bits, index, names)?.low_u128() as u64;
    let code = deserialize_fixlen(4, bits, index, names)?.low_u128();
    match code {
      0 => {
        let gossip = deserialize_fixlen(1, bits, index, names)?.low_u128() != 0;
        let blocks = deserialize_list(bits, index, names)?;
        let peers = deserialize_list(bits, index, names)?;
        Some(Message::NoticeTheseBlocks { magic, gossip, blocks, peers })
      }
      1 => {
        let bhash = Hash::proto_deserialize(bits, index, names)?;
        Some(Message::GiveMeThatBlock { magic, bhash })
      }
      2 => {
        let size = deserialize_fixlen(16, bits, index, names)?.low_u128();
        let data = deserialize_bytes(size, bits, index, names)?;
        Some(Message::PleaseMineThisTransaction {
          magic,
          tx: Transaction::new(data),
        })
      }
      _ => None,
    }
  }
}
