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

use std::thread;
use primitive_types::U256;

use std::io::Write;

fn main() {
  
  //print!("0123456789abcdef\n");
  //print!("0123456789abcdef\n");
  //print!("0123456789abcdef\n");
  //print!("0123456789abcdef\n");
  //print!("0123456789abcdef\n");
  //print!("0123456789abcdef\n");
  //print!("0123456789abcdef\n");
  //print!("0123456789abcdef\n");


  //let mut out = std::io::stdout();
  
  //print!("{}", termion::clear::All);
  //print!("{}", termion::cursor::Goto(4,4));
  //print!("{}", "XXXX");
  //print!("{}", "\n\n\n");
  //out.flush();

  // Node state object
  let mut node = new_node();

  // Node to Miner communication object
  let comm     = new_miner_comm();
  let comm_0   = comm.clone();
  let comm_1   = comm.clone();

  // User input object
  let input    = new_input();
  let input_0  = input.clone();
  let input_1  = input.clone();

  // Spawns the miner thread
  let miner_thread = thread::spawn(move || {
    miner_loop(&comm_0);
  });

  // Spawns the input thread
  let input_thread = thread::spawn(move || {
    input_loop(&input_0);
  });

  // Spawns the node thread
  let node_thread = thread::spawn(move || {
    node_loop(&mut node, &input_1, &comm_1);
  });

  // Joins all threads
  miner_thread.join().unwrap();
  input_thread.join().unwrap();
  node_thread.join().unwrap();

}
