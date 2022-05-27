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
  // TODO: primitive_types::U256::to_big_endian ?
  let mut bytes = Vec::new();
  for i in 0 .. 32 {
    bytes.push(value.byte(32 - i - 1));
  }
  return bytes;
}

pub fn u256_to_hex(value: U256) -> String {
  hex::encode(u256_to_bytes(value))
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

// Serialization
// =============

pub trait Sink<T: Copy, E = String> {
  fn write_val(&mut self, val: T) -> Result<(), E>;
  fn write_vals(&mut self, vals: &[T]) -> Result<(), E> {
    for &v in vals {
      self.write_val(v)?;
    }
    Ok(())
  }
}

pub trait Ser<T: Copy, E = String> {
  fn serialize<O: Sink<T>>(&self, output: &mut O) -> Result<(), E>;
}

// Serialization Implementations
// -----------------------------

impl Sink<u128> for std::fs::File
{
  fn write_val(&mut self, val: u128) -> Result<(), String> {
    use std::io::Write;
    let bytes = val.to_le_bytes();
    self.write_all(&bytes).map_err(|e| e.to_string())?;
    Ok(())
  }
}

impl <T: Copy> Sink<T> for Vec<T> {
  fn write_val(&mut self, val: T) -> Result<(), String> {
    self.push(val);
    Ok(())
  }
}

impl Ser<u128> for u128 {
  fn serialize<O: Sink<u128>>(&self, output: &mut O) -> Result<(), String> {
     output.write_val(*self)
  }
}

impl Ser<u128> for i128 {
  fn serialize<O: Sink<u128>>(&self, output: &mut O) -> Result<(), String> {
     output.write_val(*self as u128)
  }
}

impl Ser<u128> for (u128, u128) {
  fn serialize<O: Sink<u128>>(&self, output: &mut O) -> Result<(), String> {
    output.write_val(self.0)?;
    output.write_val(self.1)?;
    Ok(())
  }
}

impl Ser<u128> for bool {
    fn serialize<O: Sink<u128>>(&self, output: &mut O) -> Result<(), String> {
      output.write_val(u128::from(*self))
    }
}

impl <T> Ser<u128> for Option<T>
  where T: Ser<u128>
{
    fn serialize<O: Sink<u128>>(&self, output: &mut O) -> Result<(), String> {
        match self {
            None => output.write_val(0),
            Some(value) => {
              output.write_val(1)?;
              value.serialize(output)?;
              Ok(())
            },
        }
    }
}

impl <T> Ser<u128> for Vec<T>
  where T: Ser<u128>
{
    fn serialize<O: Sink<u128>>(&self, output: &mut O) -> Result<(), String> {
      output.write_val(self.len() as u128)?;
      for v in self {
        v.serialize(output)?;
      }
      Ok(())
    }
}
