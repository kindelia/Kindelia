#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(non_snake_case)]
#![allow(unused_variables)]
#![allow(clippy::style)]
// #![feature(async_closure)]

mod api;
mod bits;
mod hvm;
mod node;
mod test;
mod util;

//#[cfg(test)]
//mod test;

use std::path::PathBuf;
use std::sync::{Arc, Mutex};
use std::thread;

pub use clap::{Parser, Subcommand};

use crate::api::*;
use crate::bits::*;
use crate::hvm::*;
use crate::node::*;
use crate::test::*;
use crate::util::*;

// Starts the node process
fn main() -> Result<(), String> {
  return run_cli();
  // start_node(dirs::home_dir().unwrap().join(".kindelia"), Some("example/simple.kdl".to_string()));
  // return Ok(());
  //hvm::test_statements_from_file("./example/simple.kdl");
  //return Ok(());
}

// Environment variable where Kindelia path is stored
const KINDELIA_PATH_ENV_VAR: &str = "KINDELIA_PATH";

#[derive(Parser)]
#[clap(author, version, about, long_about = None)]
pub struct Cli {
  /// Path where blockchain data is stored
  #[clap(long)]
  path: Option<String>,
  #[clap(subcommand)]
  pub command: CliCmd,
}

#[derive(Subcommand)]
pub enum CliCmd {
  /// Starts a Kindelia node
  Start {
    //#[clap(short, long)]
    file: Option<String>,
  },
  /// Runs a Kindelia file
  Run {
    /// Input file
    file: String,
    // #[clap(short, long)]
    // debug: bool,
  },
}

// Returns the path where Kindelia files are saved
fn get_kindelia_path(dir_cli: Option<String>) -> Result<PathBuf, String> {
  match std::env::var(KINDELIA_PATH_ENV_VAR) {
    Ok(dir) => {
      return Ok(PathBuf::from(dir));
    }
    Err(err) => {
      if let std::env::VarError::NotPresent = err {
        return Ok(dirs::home_dir().unwrap().join(".kindelia"));
      } else {
        return Err(format!("{} environment variable is not valid: '{}'", KINDELIA_PATH_ENV_VAR, err));
      }
    }
  };
}

fn run_cli() -> Result<(), String> {
  let arguments = Cli::parse();

  let kindelia_path = get_kindelia_path(arguments.path)?;

  match arguments.command {

    // Starts the node process
    CliCmd::Start { file } => {
      eprintln!("Starting Kindelia node. Store path: {:?}", kindelia_path);
      start_node(kindelia_path, file);
    }

    // Runs a single block, for testing
    CliCmd::Run { file } => {
      let file = std::fs::read_to_string(file);
      match file {
        Err(err) => {
          return Err(format!("{}", err));
        }
        Ok(code) => {
          // TODO: flag to disable size limit / debug
          hvm::test_statements_from_code(&code);
        }
      }
    }
  };
  Ok(())
}

fn start_node(kindelia_path: PathBuf, file: Option<String>) {
  // Reads the file contents
  let file = file.map(|file| std::fs::read_to_string(file).expect("Block file not found."));

  // Node state object
  let (node_query_sender, node) = new_node(kindelia_path.clone());

  // Node to Miner communication object
  let miner_comm_0 = new_miner_comm();
  let miner_comm_1 = miner_comm_0.clone();

  // API thread channel
  //let (api_send, api_recv) = mpsc::channel();

  // Spawns the node thread
  let node_thread = thread::spawn(move || {
    node_loop(node, kindelia_path.clone(), miner_comm_0, file);
  });

  // Spawns the miner thread
  let miner_thread = thread::spawn(move || {
    miner_loop(miner_comm_1);
  });

  // Spawns the API thread
  let api_thread = thread::spawn(move || {
    api_loop(node_query_sender);
  });

  // Joins all threads
  node_thread.join().unwrap();
  miner_thread.join().unwrap();
  api_thread.join().unwrap();
}

