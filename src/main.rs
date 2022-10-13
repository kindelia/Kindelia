#![allow(clippy::single_component_path_imports)]
mod cli;

pub use clap::{Parser, Subcommand};

// Starts the node process
fn main() -> Result<(), String> {
  cli::run_cli()?;
  Ok(())
}

// TODO: dunno, move into test?
//for i in 0 .. 3200 {
//let mut bits = bit_vec::BitVec::new();
//serialize_fixlen(16, &u256(i), &mut bits, &mut std::collections::HashMap::new());
//fn encode_length(len: usize) -> (u8, u8) {
//let num = (len as u16).reverse_bits();
//(((num >> 8) & 0xFF) as u8, (num & 0xFF) as u8)
//}
//fn decode_length(pair: (u8,u8)) -> u16 {
//(((pair.0 as u16) << 8) | (pair.1 as u16)).reverse_bits()
//}
//println!("{:?}", bits.to_bytes());
//println!("{:?}", encode_length(i as usize));
//println!("{} == {}", i, decode_length(encode_length(i as usize)));
//}
