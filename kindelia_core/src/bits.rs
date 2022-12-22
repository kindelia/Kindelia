#![allow(dead_code)]
#![allow(clippy::style)]

use bit_vec::BitVec;
use std::collections::HashMap;

use crate::net;
use crate::net::ProtoAddr;
use crate::node::*;
use crate::util::*;
use kindelia_common::crypto::Signature;
use kindelia_common::{Name, U120};
use kindelia_lang::ast::{Func, Rule, Statement, Term};

use primitive_types::U256;

pub type Names = HashMap<u128, u128>;

// Serializers
// ===========

fn num_bits(n: u128) -> usize {
  (u128::BITS - n.leading_zeros()) as usize
}

// A number with a known amount of bits

pub fn serialize_fixlen(size: usize, value: u64, bits: &mut BitVec) {
  for i in 0..size {
    bits.push((value >> i) & 1 == 1);
  }
}

pub fn serialize_fixlen_big(size: usize, value: &U256, bits: &mut BitVec) {
  for i in 0..size {
    bits.push((value >> i).low_u128() & 1 == 1);
  }
}

pub fn deserialize_fixlen(
  size: usize,
  bits: &BitVec,
  index: &mut usize,
) -> Option<u64> {
  let mut result = 0;
  if *index + size > bits.len() {
    return None;
  }
  for i in 0..size {
    let index = *index + size - i - 1;
    result = (result << 1) + bits[index] as u64;
  }
  *index = *index + size;
  Some(result)
}

pub fn deserialize_fixlen_big(
  size: usize,
  bits: &BitVec,
  index: &mut usize,
) -> Option<U256> {
  let mut result = U256::from(0);
  if *index + size > bits.len() {
    return None;
  }
  for i in 0..size {
    let index = *index + size - i - 1;
    result = (result << 1) + U256::from(bits[index] as u8);
  }
  *index = *index + size;
  Some(result)
}

// A number with an unknown amount of bits

pub fn serialize_varlen(value: u128, bits: &mut BitVec) {
  let mut value: u128 = value;
  while value > 0 {
    bits.push(true);
    bits.push(value & 1 == 1);
    value = value >> 1;
  }
  bits.push(false);
}

pub fn deserialize_varlen(bits: &BitVec, index: &mut usize) -> Option<u128> {
  let mut val: u128 = 0;
  let mut add: u128 = 1;
  while bits.get(*index)? {
    val = val + if bits.get(*index + 1)? { add } else { 0 };
    add = add << 1;
    *index = *index + 2;
  }
  *index = *index + 1;
  Some(val)
}

// A number

pub fn serialize_number(value: u128, bits: &mut BitVec) {
  let size = num_bits(value);
  serialize_varlen(size as u128, bits);
  serialize_fixlen_big(size, &U256::from(value), bits);
}

pub fn deserialize_number(bits: &BitVec, index: &mut usize) -> Option<U256> {
  let size = deserialize_varlen(&bits, index)? as usize;
  let numb = deserialize_fixlen_big(size, &bits, index)?;
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
  index: &mut usize,
  names: &mut Names,
) -> Option<BitVec> {
  let mut result = BitVec::new();
  while bits.get(*index)? {
    result.push(bits.get(*index + 1)?);
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
  index: &mut usize,
  names: &mut Names,
) -> Option<Vec<T>> {
  let mut result = Vec::new();
  while bits.get(*index)? {
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
  index: &mut usize,
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

pub fn serialize_bytes(size: u128, bytes: &[u8], bits: &mut BitVec) {
  if size as usize != bytes.len() {
    panic!("Incorrect serialize_bytes size.");
  }
  for byte in bytes {
    serialize_fixlen(8, *byte as u64, bits);
  }
}

pub fn deserialize_bytes(
  size: u64,
  bits: &BitVec,
  index: &mut usize,
) -> Option<Vec<u8>> {
  let mut result = Vec::new();
  for _ in 0..size {
    result.push(deserialize_fixlen(8, bits, index)? as u8);
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
    index: &mut usize,
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

impl ProtoSerialize for Name {
  fn proto_serialize(&self, bits: &mut BitVec, names: &mut Names) {
    if let Some(id) = names.get(self) {
      bits.push(true);
      serialize_varlen(*id, bits);
    } else {
      let mut name = **self;
      names.insert(name, names.len() as u128);
      bits.push(false); // compressed-name flag
      while name > 0 {
        bits.push(true);
        serialize_fixlen(6, (name & 0x3F) as u64, bits);
        name = name >> 6;
      }
      bits.push(false);
    }
  }

  fn proto_deserialize(
    bits: &BitVec,
    index: &mut usize,
    names: &mut Names,
  ) -> Option<Self> {
    let mut nam: u128 = 0;
    let mut add: u128 = 1;
    let compressed = bits.get(*index)?;
    *index += 1;
    if compressed {
      let id = deserialize_varlen(bits, index)?;
      let nm = *names.get(&id)?;
      Some(Name::from_u128_unchecked(nm))
    } else {
      while bits.get(*index)? {
        *index += 1;
        let got = deserialize_fixlen(6, bits, index)?;
        nam = nam + add * got as u128;
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
        serialize_fixlen(3, 0, bits);
        name.proto_serialize(bits, names);
      }
      Term::Dup { nam0, nam1, expr, body } => {
        serialize_fixlen(3, 1, bits);
        nam0.proto_serialize(bits, names);
        nam1.proto_serialize(bits, names);
        expr.proto_serialize(bits, names);
        body.proto_serialize(bits, names);
      }
      Term::Lam { name, body } => {
        serialize_fixlen(3, 2, bits);
        name.proto_serialize(bits, names);
        body.proto_serialize(bits, names);
      }
      Term::App { func, argm } => {
        serialize_fixlen(3, 3, bits);
        func.proto_serialize(bits, names);
        argm.proto_serialize(bits, names);
      }
      Term::Ctr { name, args } => {
        serialize_fixlen(3, 4, bits);
        name.proto_serialize(bits, names);
        serialize_list(&args, bits, names);
      }
      Term::Fun { name, args } => {
        serialize_fixlen(3, 5, bits);
        name.proto_serialize(bits, names);
        serialize_list(&args, bits, names);
      }
      Term::Num { numb } => {
        serialize_fixlen(3, 6, bits);
        serialize_number(**numb, bits);
      }
      Term::Op2 { oper, val0, val1 } => {
        serialize_fixlen(3, 7, bits);
        serialize_fixlen(4, *oper as u64, bits);
        val0.proto_serialize(bits, names);
        val1.proto_serialize(bits, names);
      }
    }
  }

  fn proto_deserialize(
    bits: &BitVec,
    index: &mut usize,
    names: &mut Names,
  ) -> Option<Self> {
    let tag = deserialize_fixlen(3, bits, index)?;
    //println!("- tag.: {} {:?}", tag, bits.clone().split_off(*index as usize));
    match tag {
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
        let numb = deserialize_number(bits, index)?.low_u128();
        let numb: U120 = numb.try_into().ok()?;
        let term = Term::num(numb);
        Some(term)
      }
      7 => {
        let oper = deserialize_fixlen(4, bits, index)? as u128;
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
    index: &mut usize,
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
    index: &mut usize,
    names: &mut Names,
  ) -> Option<Self> {
    let rules = deserialize_list(bits, index, names)?;
    Some(Func { rules })
  }
}

impl<T: ProtoSerialize> ProtoSerialize for Option<T> {
  fn proto_serialize(&self, bits: &mut BitVec, names: &mut Names) {
    if let Some(value) = self {
      serialize_fixlen(1, 1, bits);
      value.proto_serialize(bits, names);
    } else {
      serialize_fixlen(1, 0, bits);
    }
  }

  fn proto_deserialize(
    bits: &BitVec,
    index: &mut usize,
    names: &mut Names,
  ) -> Option<Self> {
    let has = deserialize_fixlen(1, bits, index)?;
    let result = if has == 0 {
      None
    } else {
      let value = T::proto_deserialize(bits, index, names)?;
      Some(value)
    };
    Some(result)
  }
}

impl ProtoSerialize for Statement {
  fn proto_serialize(&self, bits: &mut BitVec, names: &mut Names) {
    match self {
      Statement::Fun { name, args, func, init, sign } => {
        serialize_fixlen(4, 0, bits);
        name.proto_serialize(bits, names);
        serialize_list(args, bits, names);
        func.proto_serialize(bits, names);
        init.proto_serialize(bits, names);
        sign.proto_serialize(bits, names);
      }
      Statement::Ctr { name, args, sign } => {
        serialize_fixlen(4, 1, bits);
        name.proto_serialize(bits, names);
        serialize_list(args, bits, names);
        sign.proto_serialize(bits, names);
      }
      Statement::Run { expr, sign } => {
        serialize_fixlen(4, 2, bits);
        expr.proto_serialize(bits, names);
        sign.proto_serialize(bits, names);
      }
      Statement::Reg { name, ownr, sign } => {
        serialize_fixlen(4, 3, bits);
        name.proto_serialize(bits, names);
        serialize_fixlen_big(128, &U256::from(**ownr), bits);
        sign.proto_serialize(bits, names);
      }
    }
  }

  fn proto_deserialize(
    bits: &BitVec,
    index: &mut usize,
    names: &mut Names,
  ) -> Option<Self> {
    let tag = deserialize_fixlen(4, bits, index)?;
    match tag {
      0 => {
        let name = Name::proto_deserialize(bits, index, names)?;
        let args = deserialize_list(bits, index, names)?;
        let func = Func::proto_deserialize(bits, index, names)?;
        let init = Option::<Term>::proto_deserialize(bits, index, names)?;
        let sign = Option::<Signature>::proto_deserialize(bits, index, names)?;
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
        let ownr = deserialize_fixlen_big(128, bits, index)?.low_u128();
        let ownr: U120 = ownr.try_into().ok()?;
        let sign = Option::proto_deserialize(bits, index, names)?;
        Some(Statement::Reg { name, ownr, sign })
      }
      _ => None,
    }
  }
}

impl ProtoSerialize for Option<Signature> {
  fn proto_serialize(&self, bits: &mut BitVec, _names: &mut Names) {
    if let Some(sign) = self {
      serialize_fixlen(1, 1, bits);
      serialize_bytes(65, &sign.0, bits);
    } else {
      serialize_fixlen(1, 0, bits);
    }
  }

  // The double Option layer keeps it consistent, since the returned value IS an Option
  fn proto_deserialize(
    bits: &BitVec,
    index: &mut usize,
    _names: &mut Names,
  ) -> Option<Self> {
    match deserialize_fixlen(1, bits, index)? {
      1 => {
        let data: Option<[u8; 65]> =
          deserialize_bytes(65, bits, index)?.try_into().ok();
        if let Some(data) = data {
          Some(Some(Signature(data)))
        } else {
          None
        }
      }
      _ => Some(None),
    }
  }
}

impl ProtoSerialize for Vec<Statement> {
  fn proto_serialize(&self, bits: &mut BitVec, names: &mut Names) {
    serialize_list(self, bits, names);
  }

  fn proto_deserialize(
    bits: &BitVec,
    index: &mut usize,
    names: &mut Names,
  ) -> Option<Self> {
    deserialize_list(bits, index, names)
  }
}

impl ProtoSerialize for Hash {
  fn proto_serialize(&self, bits: &mut BitVec, _names: &mut Names) {
    serialize_fixlen_big(256, self, bits);
  }

  fn proto_deserialize(
    bits: &BitVec,
    index: &mut usize,
    _names: &mut Names,
  ) -> Option<Self> {
    deserialize_fixlen_big(256, bits, index)
  }
}

impl ProtoSerialize for Block {
  fn proto_serialize(&self, bits: &mut BitVec, _names: &mut Names) {
    serialize_fixlen_big(256, &self.prev, bits);
    serialize_fixlen_big(128, &U256::from(self.time), bits);
    serialize_fixlen_big(128, &U256::from(self.meta), bits);
    serialize_fixlen(16, self.body.data.len() as u64, bits);
    serialize_bytes(self.body.data.len() as u128, &self.body.data, bits);
  }

  fn proto_deserialize(
    bits: &BitVec,
    index: &mut usize,
    _names: &mut Names,
  ) -> Option<Self> {
    let prev = deserialize_fixlen_big(256, bits, index)?;
    let time = deserialize_fixlen_big(128, bits, index)?.low_u128();
    let meta = deserialize_fixlen_big(128, bits, index)?.low_u128();
    let size = deserialize_fixlen(16, bits, index)?;
    let data = deserialize_bytes(size, bits, index)?;
    let body = Body { data };
    return Some(Block::new(prev, time, meta, body));
  }
}

impl ProtoSerialize for net::Address {
  fn proto_serialize(&self, bits: &mut BitVec, _names: &mut Names) {
    match self {
      net::Address::IPv4 { val0, val1, val2, val3, port } => {
        bits.push(false);
        serialize_fixlen(8, *val0 as u64, bits);
        serialize_fixlen(8, *val1 as u64, bits);
        serialize_fixlen(8, *val2 as u64, bits);
        serialize_fixlen(8, *val3 as u64, bits);
        serialize_fixlen(16, *port as u64, bits);
      }
    }
  }

  fn proto_deserialize(
    bits: &BitVec,
    index: &mut usize,
    _names: &mut Names,
  ) -> Option<net::Address> {
    if bits[*index] as u128 == 0 {
      *index = *index + 1;
      let val0 = deserialize_fixlen(8, bits, index)? as u8;
      let val1 = deserialize_fixlen(8, bits, index)? as u8;
      let val2 = deserialize_fixlen(8, bits, index)? as u8;
      let val3 = deserialize_fixlen(8, bits, index)? as u8;
      let port = deserialize_fixlen(16, bits, index)? as u16;
      return Some(net::Address::IPv4 { val0, val1, val2, val3, port });
    } else {
      return None;
    }
  }
}

impl<A: ProtoAddr> ProtoSerialize for Peer<A> {
  fn proto_serialize(&self, bits: &mut BitVec, names: &mut Names) {
    self.address.proto_serialize(bits, names);
    serialize_fixlen(48, self.seen_at as u64, bits);
  }

  fn proto_deserialize(
    bits: &BitVec,
    index: &mut usize,
    names: &mut Names,
  ) -> Option<Self> {
    let address = A::proto_deserialize(bits, index, names)?;
    let seen_at = deserialize_fixlen(48, bits, index)? as u128;
    return Some(Peer { address, seen_at });
  }
}

impl<A: ProtoAddr> ProtoSerialize for Message<A> {
  fn proto_serialize(&self, bits: &mut BitVec, names: &mut Names) {
    match self {
      // This is supposed to use < 1500 bytes when blocks = 1, to avoid UDP fragmentation
      Message::NoticeTheseBlocks { magic, gossip, blocks, peers } => {
        serialize_fixlen(32, *magic as u64, bits);
        serialize_fixlen(4, 0, bits);
        serialize_fixlen(1, *gossip as u64, bits);
        serialize_list(&blocks, bits, names);
        serialize_list(peers, bits, names);
      }
      Message::GiveMeThatBlock { magic, bhash } => {
        serialize_fixlen(32, *magic as u64, bits);
        serialize_fixlen(4, 1, bits);
        bhash.proto_serialize(bits, names);
      }
      Message::PleaseMineThisTransaction { magic, tx } => {
        serialize_fixlen(32, *magic as u64, bits);
        let tx_len = tx.len();
        if tx_len == 0 {
          panic!("Invalid transaction length.");
        } else {
          serialize_fixlen(4, 2, bits);
          serialize_fixlen(16, tx_len as u64, bits);
          serialize_bytes(tx_len as u128, tx, bits);
        }
      }
    }
  }
  fn proto_deserialize(
    bits: &BitVec,
    index: &mut usize,
    names: &mut Names,
  ) -> Option<Self> {
    let magic = deserialize_fixlen(32, bits, index)? as u32;
    let code = deserialize_fixlen(4, bits, index)?;
    match code {
      0 => {
        let gossip = deserialize_fixlen(1, bits, index)? != 0;
        let blocks = deserialize_list(bits, index, names)?;
        let peers = deserialize_list(bits, index, names)?;
        Some(Message::NoticeTheseBlocks { magic, gossip, blocks, peers })
      }
      1 => {
        let bhash = Hash::proto_deserialize(bits, index, names)?;
        Some(Message::GiveMeThatBlock { magic, bhash })
      }
      2 => {
        let size = deserialize_fixlen(16, bits, index)?;
        let data = deserialize_bytes(size, bits, index)?;
        let transaction = Transaction::new(data).ok()?;
        Some(Message::PleaseMineThisTransaction { magic, tx: transaction })
      }
      _ => None,
    }
  }
}
