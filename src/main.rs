#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(non_snake_case)]
#![allow(unused_variables)]

mod algorithms;
mod cli;
mod common;
mod constants;
mod hvm;
mod network;
mod node;
mod serializer;
mod types;

use std::io::Write;
use std::sync::{Arc, Mutex};
use std::thread;

use cli::{Cli, CliCmd, Parser};
use primitive_types::U256;

use crate::algorithms::*;
use crate::constants::*;
use crate::network::*;
use crate::node::*;
use crate::serializer::*;
use crate::types::*;

fn main() -> Result<(), String> {
  // hvm::test_1();
  run_cli()
}

fn run_cli() -> Result<(), String> {
  let cli_matches = cli::Cli::parse();

  match cli_matches.command {
    CliCmd::Start { ui } => {
      start_node(ui);
    }
    CliCmd::Eval { file } => {
      let file = std::fs::read_to_string(file);
      match file {
        Err(err) => return Err(format!("{}", err)),
        Ok(code) => {
          hvm::test_actions_from_code(&code);
        }
      }
    }
  };
  Ok(())
}

fn start_node(ui: bool) {
  // Node state object
  let node = new_node();

  // Node to Miner communication object
  let miner_comm_0 = new_miner_comm();
  let miner_comm_1 = miner_comm_0.clone();

  // User input object
  let input_0 = new_input();
  let input_1 = input_0.clone();

  // Spawns the node thread
  let node_thread = thread::spawn(move || {
    node_loop(node, miner_comm_0, input_0, ui);
  });

  // Spawns the miner thread
  let miner_thread = thread::spawn(move || {
    miner_loop(miner_comm_1);
  });

  // Spawns the input thread
  let input_thread = thread::spawn(move || {
    if ui {
      input_loop(&input_1);
    }
  });

  // Joins all threads
  node_thread.join().unwrap();
  miner_thread.join().unwrap();
  input_thread.join().unwrap();
}
