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
  let node = new_node();

  let (node_comm, front_comm) = api::make_node_channels(1);

  // Node to Miner communication object
  let miner_comm_0 = new_miner_comm();
  let miner_comm_1 = miner_comm_0.clone();

  // Spawns the node thread
  let node_thread = thread::spawn(move || {
    node_loop(node, miner_comm_0, node_comm);
  });

  // Spawns the miner thread
  let miner_thread = thread::spawn(move || {
    miner_loop(miner_comm_1);
  });

  // Spawns frontend threads
  let frontend: Box<dyn Frontend> = if cli_matches.no_ui {
    Box::new(crate::frontend::headless::HeadlessFrontend::new())
  } else {
    Box::new(crate::frontend::tui::TuiFrontend::new())
  };
  let tasks = frontend.get_tasks(front_comm);
  let front_threads = tasks.into_iter().map(thread::spawn).collect::<Vec<_>>();

  // Joins all threads
  node_thread.join().unwrap();
  miner_thread.join().unwrap();
  for thread in front_threads {
    thread.join().unwrap();
  }
}
