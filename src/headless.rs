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

use std::sync::{Arc, Mutex};

use crate::node::*;
use crate::types::*;

use std::thread;

fn main() {
  // Node state object
  let node_0: SharedNode = Arc::new(Mutex::new(new_node()));
  let node_1 = node_0.clone();

  // Node to Miner communication object
  let comm_0 = new_miner_comm();
  let comm_1 = comm_0.clone();

  // User input object
  let input_0 = new_input();

  // Spawns the node thread
  let node_thread = thread::spawn(move || {
    node_loop(node_0, input_0, comm_0);
  });

  // Spawns the output thread
  let output_thread = thread::spawn(move || {
    output_loop(node_1);
  });

  // Spawns the miner thread
  let miner_thread = thread::spawn(move || {
    miner_loop(comm_1);
  });

  // Joins all threads
  node_thread.join().unwrap();
  output_thread.join().unwrap();
  miner_thread.join().unwrap();
}

fn output_loop(node: SharedNode) {
  loop {
    {
    let node = node.lock().unwrap();
    display(&node);
    }

    // Sleeps for 2 seconds
    std::thread::sleep(std::time::Duration::from_millis(1000));
  }
}

fn display(node: &Node) {
  println!("{{");

  let time = algorithms::get_time();

  let tip = node.tip;
  let l_hash = tip.1;
  let l_height = node.height[&l_hash];

  println!(r#"  "time": "{}","#, time);
  println!(r#"  "hash": "{}","#, l_hash);
  println!(r#"  "height": {}"#, l_height);

  println!("}}");
}
