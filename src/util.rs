use bit_vec::BitVec;
use im::HashSet;
use primitive_types::U256;
use priority_queue::PriorityQueue;
use sha3::Digest;
use std::collections::HashMap;
use std::net::*;

use crate::bits::*;

pub type U256Map<T> = HashMap<U256, T>;
pub type Hash = U256;

// Numerics
// ========

// Size of a u128, in bytes
pub const U128_SIZE : usize = 128 / 8;

pub fn u256(x: u128) -> U256 {
  return U256::from(x);
}

pub fn next_power_of_two(x: f64) -> f64 {
  if x <= 1.0 { x } else { (2.0_f64).powf(x.log2().floor() + 1.0) }
}

pub fn u128_to_bytes(value: u128) -> Vec<u8> {
  return Vec::from(value.to_le_bytes());
}

pub fn u256_to_bytes(value: U256) -> Vec<u8> {
  let mut bytes = Vec::new();
  for i in 0 .. 32 {
    bytes.push(value.byte(32 - i - 1));
  }
  return bytes;
}

pub fn bitvec_to_bytes(bits: &BitVec) -> Vec<u8> {
  return bits.to_bytes();
}

pub fn bytes_to_bitvec(bytes: &[u8]) -> BitVec {
  return BitVec::from_bytes(bytes);
}

// System
// ======

// Gets current timestamp in milliseconds
pub fn get_time() -> u128 {
  return std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH).unwrap().as_millis() as u128;
}
