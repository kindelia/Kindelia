#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(non_snake_case)]
#![allow(unused_variables)]

mod algorithms;
mod constants;
mod network;
mod node;
mod serializer;
mod types;

use crate::algorithms::*;
use crate::constants::*;
use crate::network::*;
use crate::node::*;
use crate::serializer::*;
use crate::types::*;

use primitive_types::U256;

use std::thread;

fn main() {

  let mut node = node_init();
  let     comm = comm_init();

  let a_comm = comm.clone();
  let miner_thread = thread::spawn(move || {
    miner_loop(&a_comm);
  });

  let b_comm = comm.clone();
  let node_thread = thread::spawn(move || {
    node_loop(&mut node, &b_comm);
  });

  miner_thread.join().unwrap();
  node_thread.join().unwrap();

}
