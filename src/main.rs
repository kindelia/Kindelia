#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(non_snake_case)]
#![allow(unused_variables)]

mod algorithms;
mod api;
mod cli;
mod common;
mod constants;
mod frontend;
mod hvm;
mod network;
mod node;
mod serializer;
mod types;

use std::io::Write;
use std::sync::{Arc, Mutex};
use std::thread;

use cli::{Cli, Parser};
use primitive_types::U256;

use crate::algorithms::*;
use crate::constants::*;
use crate::network::*;
use crate::node::*;
use crate::serializer::*;
use crate::types::*;

use crate::api::Frontend;

fn main() {
  hvm::test_1();
}

fn run() {
  let cli_matches = cli::Cli::parse();

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

  let (node_comm, front_comm) = api::make_node_channels(1);

  // Node to Miner communication object
  let miner_comm_0 = new_miner_comm();
  let miner_comm_1 = miner_comm_0.clone();

  // User input object
  let input_0 = new_input();
  let input_1 = input_0.clone();
  let input_2 = input_0.clone();

  // Spawns the node thread
  let node_thread = thread::spawn(move || {
    node_loop(node_0, input_0, miner_comm_0, node_comm);
  });

  // Spawns the miner thread
  let miner_thread = thread::spawn(move || {
    miner_loop(miner_comm_1);
  });

  let io_threads = if cli_matches.no_ui {
    // Run headless mode threads

    let frontend = crate::frontend::headless::HeadlessFrontend::new();
    let tasks = frontend.get_tasks(front_comm);
    tasks.into_iter().map(thread::spawn).collect::<Vec<_>>()
  } else {
    // Run TUI threads

    // Spawns the input thread
    let input_thread = thread::spawn(move || {
      input_loop(input_1);
    });

    // Spawns the output thread
    let output_thread = thread::spawn(move || {
      output_loop_tui(node_1, input_2);
    });

    vec![input_thread, output_thread]
  };

  // Joins all threads
  node_thread.join().unwrap();
  miner_thread.join().unwrap();
  for thread in io_threads {
    thread.join().unwrap();
  }
}
