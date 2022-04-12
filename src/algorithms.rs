use bit_vec::BitVec;
use im::HashSet;
use primitive_types::U256;
use priority_queue::PriorityQueue;
use sha3::Digest;
use std::collections::HashMap;
use std::net::*;

use crate::constants::*;
use crate::serializer::*;
use crate::types::*;

// Numerics
// ========

pub fn u256(x: u64) -> U256 {
  return U256::from(x);
}

pub fn next_power_of_two(x: f64) -> f64 {
  if x <= 1.0 { x } else { (2.0_f64).powf(x.log2().floor() + 1.0) }
}

pub fn u64_to_bytes(value: u64) -> Vec<u8> {
  return Vec::from(value.to_le_bytes());
}

pub fn u256_to_bytes(value: U256) -> Vec<u8> {
  let mut bytes = Vec::new();
  for i in 0 .. 32 {
    bytes.push(value.byte(i));
  }
  return bytes;
}

pub fn bitvec_to_bytes(bits: &BitVec) -> Vec<u8> {
  return bits.to_bytes();
}

pub fn compute_difficulty(target: U256) -> U256 {
  let p256 = U256::from("0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF");
  return p256 / (p256 - target);
}

pub fn compute_target(difficulty: U256) -> U256 {
  let p256 = U256::from("0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF");
  return p256 - p256 / difficulty;
}

// Computes next target by scaling the current difficulty by a `scale` factor
// Since the factor is an integer, it is divided by 2^32 to allow integer division
// - compute_next_target(t, 2n**32n / 2n): difficulty halves
// - compute_next_target(t, 2n**32n * 1n): nothing changes
// - compute_next_target(t, 2n**32n * 2n): difficulty doubles
pub fn compute_next_target(last_target: U256, scale: U256) -> U256 {
  let p32 = U256::from("0x100000000");
  let last_difficulty = compute_difficulty(last_target);
  let next_difficulty = u256(1) + (last_difficulty * scale - u256(1)) / p32;
  return compute_target(next_difficulty);
}

pub fn compute_next_target_f64(last_target: U256, scale: f64) -> U256 {
  return compute_next_target(last_target, u256((scale * 4294967296.0) as u64));
}

pub fn get_hash_work(hash: U256) -> U256 {
  if hash == u256(0) {
    return u256(0);
  } else {
    return compute_difficulty(hash);
  }
}

pub fn hash_u256(value: U256) -> U256 {
  return hash_bytes(u256_to_bytes(value).as_slice());
}

pub fn hash_bytes(bytes: &[u8]) -> U256 {
  let mut hasher = sha3::Keccak256::new();
  hasher.update(&bytes);
  let hash = hasher.finalize();
  return U256::from_little_endian(&hash);
}

pub fn hash_block(block: &Block) -> U256 {
  if block.time == 0 {
    return hash_bytes(&[]);
  } else {
    let mut bytes : Vec<u8> = Vec::new();
    bytes.extend_from_slice(&u256_to_bytes(block.prev));
    bytes.extend_from_slice(&u64_to_bytes(block.time));
    bytes.extend_from_slice(&u64_to_bytes(block.rand));
    bytes.extend_from_slice(&block.body.value);
    return hash_bytes(&bytes);
  }
}

pub fn mine(block: Block, target: U256, max_attempts: u64) -> Option<Block> {
  let mut block = block.clone();
  for _i in 0 .. max_attempts {
    if hash_block(&block) >= target {
      return Some(block);
    } else {
      block.rand = block.rand + 1;
    }
  }
  return None;
}

// System
// ======

// Gets current timestamp in milliseconds
pub fn get_time() -> u64 {
  return std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH).unwrap().as_millis() as u64;
}

// Stringification
// ===============

pub fn show_address_hostname(address: &Address) -> String {
  match address {
    Address::IPv4{ val0, val1, val2, val3, port } => {
      return format!("{}.{}.{}.{}", val0, val1, val2, val3);
    }
  }
}

pub fn show_block(chain: &Node, block: &Block, index: usize) -> String {
  let zero = u256(0);
  let bhash = hash_block(block);
  let work = chain.work.get(&bhash).unwrap_or(&zero);
  let show_index = format!("{}", index);
  let show_time = format!("{}", (block.time >> 192));
  let show_body = format!("{}", hex::encode(block.body.value));
  let show_hash = format!("{}", bhash);
  let show_work = format!("{}", work);
  return format!("{} | {} | {} | {} | {}", show_index, show_time, show_hash, show_body, show_work);
}
