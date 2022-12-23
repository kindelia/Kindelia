#![allow(dead_code)]
// #![allow(unused_imports)]
// #![allow(non_snake_case)]
// #![allow(unused_variables)]
// #![allow(clippy::style)]

use std::collections::HashMap;
use std::path::PathBuf;

use bit_vec::BitVec;

use kindelia_common::nohash_hasher::NoHashHasher;
use kindelia_common::{Name, U120, U256};
use std::time::{SystemTime, SystemTimeError, UNIX_EPOCH};

use crate::runtime::Loc;

use thiserror::Error;

pub type U64Map <T> = HashMap<u64 , T, std::hash::BuildHasherDefault<NoHashHasher<u64 >>>;
pub type U120Map<T> = HashMap<U120, T, std::hash::BuildHasherDefault<NoHashHasher<U120>>>;
pub type U128Map<T> = HashMap<u128, T, std::hash::BuildHasherDefault<NoHashHasher<u128>>>;
pub type U256Map<T> = HashMap<U256, T, std::hash::BuildHasherDefault<NoHashHasher<U256>>>;
pub type NameMap<T> = HashMap<Name, T, std::hash::BuildHasherDefault<NoHashHasher<Name>>>;
pub type LocMap <T> = HashMap<Loc , T, std::hash::BuildHasherDefault<NoHashHasher<Loc >>>;

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
pub const U128_SIZE: usize = 128 / 8;

/// Build bit mask
pub const fn mask(size: usize, pos: usize) -> u128 {
  ((1 << size) - 1) << pos
}

// TODO: remove
pub fn u256(x: u128) -> U256 {
  U256::from(x)
}

pub fn next_power_of_two(x: f64) -> f64 {
  if x <= 1.0 {
    x
  } else {
    (2.0_f64).powf(x.log2().floor() + 1.0)
  }
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
  for i in 0..32 {
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
  let mut u8s: Vec<u8> = vec![];
  for n in u128s {
    let bytes = n.to_le_bytes();
    u8s.extend_from_slice(&bytes);
  }
  u8s
}

pub fn u8s_to_u128s(
  u8s: &[u8],
) -> Result<Vec<u128>, std::array::TryFromSliceError> {
  let mut u8s = u8s.to_vec();
  u8s.resize((u8s.len() + 15) / 16 * 16, 0);
  let mut u128s: Vec<u128> = vec![];
  for i in 0..u8s.len() / 16 {
    u128s.push(u128::from_le_bytes(u8s[i * 16..i * 16 + 16].try_into()?));
  }
  Ok(u128s)
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
///
/// note: panics if system time is before unix epoch
///
/// deprecated: use try_get_time_micro instead when possible.
pub(crate) fn get_time() -> u128 {
  SystemTime::now()
    .duration_since(UNIX_EPOCH)
    .expect("system time should be later than unix epoch")
    .as_millis()
}

/// Gets current timestamp in microseconds
///
/// note: panics if system time is before unix epoch
///
/// deprecated: use try_get_time_micro instead when possible.
pub(crate) fn get_time_micro() -> u128 {
  std::time::SystemTime::now()
    .duration_since(UNIX_EPOCH)
    .expect("system time should be later than unix epoch")
    .as_micros()
}

/// Indicates that the system's time is before the unix epoch
/// which is not allowed because we represent times as u128
/// since epoch.
#[derive(Error, Debug)]
#[error("SystemTime precedes the unix epoch. {now:?} < {epoch:?}")]
pub struct EpochError {
  pub now: SystemTime,
  pub epoch: SystemTime,
  pub source: SystemTimeError,
}

/// Gets current timestamp in milliseconds
///
pub(crate) fn try_get_time() -> Result<u128, EpochError> {
  let now = SystemTime::now();
  let epoch = UNIX_EPOCH;

  Ok(
    now
      .duration_since(epoch)
      .map_err(|source| EpochError { now, epoch, source })?
      .as_millis(),
  )
}

pub(crate) fn try_get_time_micro() -> Result<u128, EpochError> {
  let now = SystemTime::now();
  let epoch = UNIX_EPOCH;

  Ok(
    now
      .duration_since(epoch)
      .map_err(|source| EpochError { now, epoch, source })?
      .as_micros(),
  )
}

// Errors
// ======

/// An error for providing metadata (path, context) for
/// filesystem errors.
///
/// This is useful because std::io::error does not provide
/// the path, and so error messages can be quite unhelpful
/// as one does not even know the path being operated on.
///
/// See: https://github.com/rust-lang/rfcs/issues/2885
///
/// It is intended that any calls to std lib filesystem calls
/// will map_err() the result to FileSystemError.
#[derive(Error, Debug)]
#[error("Filesystem error for path: {path}.  context: {context}")]
pub struct FileSystemError {
  pub path: PathBuf,
  pub context: String,
  pub source: std::io::Error,
}


// Genesis
// =======

#[derive(Error, Debug)]
pub(crate) enum GenesisPathError {
  #[error("Home directory not found")]
  HomeDirNotFound,
  #[error("File not found in {0}")] 
  FileNotFound(PathBuf)
}

pub(crate) fn genesis_path(network_id: u32) -> Result<PathBuf, GenesisPathError> {
  let path = dirs::home_dir().ok_or(GenesisPathError::HomeDirNotFound)?.join(".kindelia").join("genesis").join(format!("{:#02X}.kdl", network_id));
  match path.exists() {
    true => Ok(path),
    false => Err(GenesisPathError::FileNotFound(path)),
  }
}

#[derive(Error, Debug)]
pub(crate) enum GenesisCodeError {
  #[error(transparent)]
  PathError(#[from] GenesisPathError),

  #[error("Genesis block could not be read from {path:?}.")]
  ReadError {
    path: PathBuf,
    cause: std::io::Error,
  }
}

pub(crate) fn genesis_code(network_id: u32) -> Result<String, GenesisCodeError> {
  let path = genesis_path(network_id)?;
  std::fs::read_to_string(&path).map_err(|e| GenesisCodeError::ReadError{path, cause: e})
}
