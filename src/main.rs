#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(non_snake_case)]
#![allow(unused_variables)]
#![allow(clippy::style)]

mod api;
mod bits;
mod crypto;
mod hvm;
mod node;
mod util;
mod NoHashHasher;

#[cfg(test)]
mod test;

use std::path::PathBuf;
use std::sync::{Arc, Mutex};
use std::thread;

pub use clap::{Parser, Subcommand};

use crate::api::api_loop;
use crate::bits::*;
use crate::hvm::*;
use crate::node::*;
use crate::util::*;

// Starts the node process
fn main() -> Result<(), String> {
  return run_cli();
  //start_node(dirs::home_dir().unwrap().join(".kindelia"));
  // hvm::test_statements_from_file("./example/block_4.kdl");
  //return Ok(());
}

/// Environment variable where Kindelia path should be passed.
const KINDELIA_PATH_ENV_VAR: &str = "KINDELIA_PATH";

#[derive(Parser)]
#[clap(author, version, about, long_about = None)]
pub struct Cli {
  /// Path where Kindelia files are stored
  #[clap(long)]
  path: Option<String>,
  #[clap(subcommand)]
  pub command: CliCmd,
}

#[derive(Subcommand)]
pub enum CliCmd {
  /// Starts a Kindelia node
  Start {
    // /// Source of code that will be executed on mined blocks
    //#[clap(short, long)]
    //file: Option<String>,
  },
  /// Runs a Kindelia file
  Run {
    /// Input file
    file: String,
    // #[clap(short, long)]
    // debug: bool,
  },
  /// Prints the address and subject of a secret key
  Subject {
    /// File containing the 256-bit secret key, as a hex string
    skey_file: String,
  },
  /// Signs the last statement in a file
  Sign {
    /// File containing the statement to be signed
    term_file: String,
    /// File containing the 256-bit secret key, as a hex string
    skey_file: String,
  },
  /// Posts the last statement in a file to the network
  Post {
    /// File where the statement is
    term_file: String,
    /// IP of the node to submit it to
    node_addr: String,
  },
}

/// Gets the path where Kindelia files should be saved.
///
/// Priority is:
/// 1. CLI argument
/// 2. Environment variable
/// 3. Default path (`$HOME/.kindelia`)
fn get_kindelia_path(dir_cli: Option<String>) -> Result<PathBuf, String> {
  let default = || dirs::home_dir().unwrap().join(".kindelia");
  if let Some(dir_cli) = dir_cli {
    return Ok(PathBuf::from(dir_cli));
  }
  match std::env::var(KINDELIA_PATH_ENV_VAR) {
    Ok(dir) => Ok(PathBuf::from(dir)),
    Err(err) => {
      if let std::env::VarError::NotPresent = err {
        Ok(default())
      } else {
        Err(format!("{} environment variable is not valid: '{}'", KINDELIA_PATH_ENV_VAR, err))
      }
    }
  }
}

fn run_cli() -> Result<(), String> {
  let arguments = Cli::parse();

  let kindelia_path = get_kindelia_path(arguments.path)?;

  match arguments.command {
    // Starts the node process
    CliCmd::Start { } => {
      eprintln!("Starting Kindelia node. Store path: {:?}", kindelia_path);
      start_node(kindelia_path);
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

    // Signs a run statement
    CliCmd::Sign { term_file, skey_file } => {
      fn format_sign(sign: &crypto::Signature) -> String {
        let hex = sign.to_hex();
        let mut text = String::new();
        for i in 0 .. 5 {
          text.push_str(&hex[i * 26 .. (i+1) * 26]);
          text.push_str("\n");
        }
        return text;
      }
      if let (Ok(code), Ok(skey)) = (std::fs::read_to_string(term_file), std::fs::read_to_string(skey_file)) {
        let statements = hvm::read_statements(&code).map_err(|err| err.erro)?;
        let statements = statements.1;
        if let Some(last_statement) = &statements.last() {
          let skey = hex::decode(&skey[0..64]).expect("hex string");
          let user = crypto::Account::from_private_key(&skey);
          let hash = hvm::hash_statement(&last_statement);
          let sign = user.sign(&hash);
          //println!("expr: {}", hvm::view_term(&expr));
          //println!("hash: {}", hex::encode(&hvm::hash_term(&expr).0));
          //println!("user: {}", hex::encode(sign.signer_address(&hash).unwrap().0));
          //println!("user: {}", hex::encode(crypto::Signature::from_hex(&format!("{}",sign.to_hex())).unwrap().signer_address(&hash).unwrap().0));
          println!("{}", format_sign(&sign));
          return Ok(());
        }
        panic!("File must have at least one statement.");
      } else {
        println!("Couldn't load term and secret key files.");
      }
    }

    // Signs a run statement
    CliCmd::Post { term_file, node_addr } => {
      if let Ok(code) = std::fs::read_to_string(term_file) {
        let statements = hvm::read_statements(&code).map_err(|err| err.erro)?;
        let statements = statements.1;
        if let Some(last_statement) = &statements.last() {
          let tx = Transaction::new(bitvec_to_bytes(&serialized_statement(last_statement)));
          let ms = Message::PleaseMineThisTransaction { trans: tx };
          let ip = read_address(&node_addr);
          let ports = [UDP_PORT + 100, UDP_PORT + 101, UDP_PORT + 102, UDP_PORT + 103];
          if let Some((mut socket, port)) = udp_init(&ports) {
            udp_send(&mut socket, ip, &ms);
            println!("Sent statement to {} via UDP:\n\n{}", node_addr, view_statement(last_statement));
            return Ok(());
          } else {
            panic!("Couldn't open UDP socket on ports: {:?}.", ports);
          }
        }
        panic!("File must have at least one statement.");
      } else {
        println!("Couldn't load term and secret key files.");
      }
    }

    // Prints the subject
    CliCmd::Subject { skey_file } => {
      if let Ok(skey) = std::fs::read_to_string(skey_file) {
        let skey = hex::decode(&skey[0..64]).expect("hex string");
        let acc  = crypto::Account::from_private_key(&skey);
        println!("Ethereum Address: {}", acc.address.show());
        println!("Kindelia Subject: {}", acc.name.show());
      } else {
        println!("Couldn't load term and secret key files.");
      }
    }
  };
  Ok(())
}

fn start_node(kindelia_path: PathBuf) {
  // Reads the file contents
  //let file = file.map(|file| std::fs::read_to_string(file).expect("Block file not found."));

  // Node state object
  let (node_query_sender, node) = Node::new(kindelia_path.clone());

  // Node to Miner communication object
  let miner_comm_0 = MinerCommunication::new();
  let miner_comm_1 = miner_comm_0.clone();

  // API thread channel
  //let (api_send, api_recv) = mpsc::channel();

  // Spawns the node thread
  let node_thread = thread::spawn(move || {
    //Node::node_loop(node, kindelia_path.clone(), miner_comm_0, file);
    node.main(kindelia_path.clone(), miner_comm_0);
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
