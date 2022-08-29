#![allow(clippy::single_component_path_imports)]

#[cfg(test)]
mod test;
#[cfg(test)]
use rstest_reuse;

#[allow(non_snake_case)]
mod NoHashHasher;
mod api;
mod bits;
mod cli;
mod crypto;
mod hvm;
mod node;
mod util;

pub use clap::{Parser, Subcommand};

// TODO: move to config file
/// Testnet nodes
const ENTRY_PEERS: [&str; 3] =
  ["143.110.233.192", "164.92.151.251", "159.65.8.239"];

// Starts the node process
fn main() -> Result<(), String> {
  cli::run_cli()?;
  //start_node(dirs::home_dir().unwrap().join(".kindelia"), false);
  //hvm::test_statements_from_file("./example/block_1.kdl");
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
