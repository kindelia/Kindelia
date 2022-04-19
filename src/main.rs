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

use std::io::Write;
use std::sync::{Arc, Mutex};
use std::thread;

use primitive_types::U256;

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
  let node_0: SharedNode = Arc::new(Mutex::new(new_node()));
  let node_1 = node_0.clone();

  // Node to Miner communication object
  let comm_0 = new_miner_comm();
  let comm_1 = comm_0.clone();

  // User input object
  let input_0 = new_input();
  let input_1 = input_0.clone();
  let input_2 = input_0.clone();

  // Spawns the node thread
  let node_thread = thread::spawn(move || {
    node_loop(node_0, input_0, comm_0);
  });

  // Spawns the input thread
  let input_thread = thread::spawn(move || {
    input_loop(input_1);
  });

  // Spawns the output thread
  let output_thread = thread::spawn(move || {
    output_loop(node_1, input_2);
  });

  // Spawns the miner thread
  let miner_thread = thread::spawn(move || {
    miner_loop(comm_1);
  });

  // Joins all threads
  node_thread.join().unwrap();
  input_thread.join().unwrap();
  output_thread.join().unwrap();
  miner_thread.join().unwrap();
}

fn output_loop(node: SharedNode, input: SharedInput) {
  let mut last_screen: Option<Vec<String>> = None;

  loop {
    let input = {
      let input = input.lock().unwrap();
      input.clone()
    };

    {
      let node = node.lock().unwrap();
      node::node_display(&node, &input, &mut last_screen);
    }

    // Sleeps for 2 * 1/60 s
    std::thread::sleep(std::time::Duration::from_micros(13000));
  }
}
