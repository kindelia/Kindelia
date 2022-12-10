#![allow(dead_code)]
// #![allow(unused_imports)]
// #![allow(non_snake_case)]
// #![allow(unused_variables)]
// #![allow(clippy::style)]

use std::collections::HashMap;

pub use primitive_types::U256;
use bit_vec::BitVec;

use crate::NoHashHasher as NHH;
use crate::common::{Name, U120};
use crate::hvm::Loc;

pub type U64Map <T> = HashMap<u64 , T, std::hash::BuildHasherDefault<NHH::NoHashHasher<u64 >>>;
pub type U120Map<T> = HashMap<U120, T, std::hash::BuildHasherDefault<NHH::NoHashHasher<U120>>>;
pub type U128Map<T> = HashMap<u128, T, std::hash::BuildHasherDefault<NHH::NoHashHasher<u128>>>;
pub type U256Map<T> = HashMap<U256, T, std::hash::BuildHasherDefault<NHH::NoHashHasher<U256>>>;
pub type NameMap<T> = HashMap<Name, T, std::hash::BuildHasherDefault<NHH::NoHashHasher<Name>>>;
pub type LocMap <T> = HashMap<Loc , T, std::hash::BuildHasherDefault<NHH::NoHashHasher<Loc >>>;


pub type Hash = U256;

// Logs
// ====

#[macro_export]
macro_rules! dbg_println {
    () => {
        #[cfg(debug_assertions)]
        std::eprint!("\n")
    };
    ($($arg:tt)*) => {{
        #[cfg(debug_assertions)]
        // $crate::io::_eprint($crate::format_args_nl!($($arg)*));
        eprintln!($($arg)*);
    }};
}

pub fn print_type_of<T>(_: &T) {
  println!("{}", std::any::type_name::<T>())
}

// Numerics
// ========

/// Size of an u128, in bytes
pub const U128_SIZE : usize = 128 / 8;

/// Build bit mask
pub const fn mask(size: usize, pos: usize) -> u128 {
  ((1 << size) - 1) << pos
}

// TODO: remove
pub fn u256(x: u128) -> U256 {
  U256::from(x)
}

pub fn next_power_of_two(x: f64) -> f64 {
  if x <= 1.0 { x } else { (2.0_f64).powf(x.log2().floor() + 1.0) }
}

pub fn u64_to_bytes(value: u64) -> Vec<u8> {
  Vec::from(value.to_le_bytes())
}

pub fn u128_to_bytes(value: u128) -> Vec<u8> {
  Vec::from(value.to_le_bytes())
}

pub fn u256_to_bytes(value: U256) -> Vec<u8> {
  // TODO: primitive_types::U256::to_big_endian ?
  let mut bytes = Vec::new();
  for i in 0 .. 32 {
    bytes.push(value.byte(32 - i - 1));
  }
  bytes
}

pub fn u256_to_hex(value: U256) -> String {
  hex::encode(u256_to_bytes(value))
}

pub fn bitvec_to_bytes(bits: &BitVec) -> Vec<u8> {
  bits.to_bytes()
}

pub fn bytes_to_bitvec(bytes: &[u8]) -> BitVec {
  BitVec::from_bytes(bytes)
}

pub fn u128s_to_u8s(u128s: &[u128]) -> Vec<u8> {
  let mut u8s : Vec<u8> = vec![];
  for n in u128s {
    let bytes = n.to_le_bytes();
    u8s.extend_from_slice(&bytes);
  }
  u8s
}

pub fn u8s_to_u128s(u8s: &[u8]) -> Vec<u128> {
  let mut u8s = u8s.to_vec();
  u8s.resize((u8s.len() + 15) / 16 * 16, 0);
  let mut u128s : Vec<u128> = vec![];
  for i in 0 .. u8s.len() / 16 {
    u128s.push(u128::from_le_bytes(u8s[i * 16 .. i * 16 + 16].try_into().unwrap()));
  }
  u128s
}

// Maps
// ====

pub fn u64map_new<T>() -> U64Map<T> {
  HashMap::with_hasher(std::hash::BuildHasherDefault::default())
}

pub fn u64map_from<T, const N: usize>(a: [(u64, T); N]) -> U64Map<T> {
  let mut map = u64map_new();
  for (k, v) in a {
    map.insert(k, v);
  }
  map
}

pub fn u128map_new<T>() -> U128Map<T> {
  HashMap::with_hasher(std::hash::BuildHasherDefault::default())
}

pub fn u128map_from<T, const N: usize>(a: [(u128, T); N]) -> U128Map<T> {
  let mut map = u128map_new();
  for (k, v) in a {
    map.insert(k, v);
  }
  map
}

pub fn u256map_new<T>() -> U256Map<T> {
  HashMap::with_hasher(std::hash::BuildHasherDefault::default())
}

pub fn u256map_from<T, const N: usize>(a: [(U256, T); N]) -> U256Map<T> {
  let mut map = u256map_new();
  for (k, v) in a {
    map.insert(k, v);
  }
  map
}

// System
// ======

/// Gets current timestamp in milliseconds
pub fn get_time() -> u128 {
  std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH).unwrap().as_millis() as u128
}

pub fn get_time_micro() -> u128 {
  std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH).unwrap().as_micros() as u128
}

#[macro_export] macro_rules! print_with_timestamp {
  () => {
    print!("{} ~~", get_time_micro());
  };
  ($($arg:tt)*) => {
    println!("{} ~~ {}", get_time_micro(), format!($($arg)*));
  };
}
