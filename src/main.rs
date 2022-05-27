#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(non_snake_case)]
#![allow(unused_variables)]
#![allow(clippy::style)]

mod util;
mod hvm;
mod node;
mod bits;

use primitive_types::U256;

use std::fs;
use std::io::Write;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};
use std::thread;

pub use clap::{Parser, Subcommand};

use crate::bits::*;
use crate::node::*;
use crate::util::*;

const KINDELIA_DIR_ENV: &str = "KINDELIA_DIR";
const KINDELIA_HOME_DEFAULT: &str = ".kindelia";

fn main() -> Result<(), String> {
  return run_cli();
  // test_from_file("./example/example.kindelia");
  // return Ok(());
}

#[derive(Parser)]
#[clap(author, version, about, long_about = None)]
pub struct Cli {
  #[clap(long)]
  dir: Option<String>,
  #[clap(subcommand)]
  pub command: CliCmd,
}

#[derive(Subcommand)]
pub enum CliCmd {
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
    #[clap(long)]
    save_heap: bool,
  },
}

fn run_cli() -> Result<(), String> {
  let cli_matches = Cli::parse();

  let base_dir = get_base_dir(cli_matches.dir)?;

  match cli_matches.command {
    CliCmd::Start { file } => {
      eprintln!("Starting node on directory: {:?}", base_dir);
      start_node(base_dir, file);
    }
    CliCmd::Run { file, save_heap } => {
      let file = std::fs::read_to_string(file);
      match file {
        Err(err) => return Err(format!("{}", err)),
        Ok(code) => {
          let heap_file = if save_heap {
            let heap_file = fs::File::create("./run.heap.bin").map_err(|e| e.to_string())?;
            Some(heap_file)
          } else {
            None
          };
          // TODO: flag to disable size limit / debug
          run_code(&code, heap_file)?;
        }
      }
    }
  };
  Ok(())
}

fn run_code(code: &str, heap_file: Option<fs::File>) -> Result<(), String> {
  let stmts = &hvm::read_statements(code).1;
  let rt = run_statements(stmts);
  if let Some(mut heap_file) = heap_file {
    eprint!("Writing heap to file... ");
    rt.get_heap(rt.curr).serialize(&mut heap_file)?;
    eprintln!("Done.");
  }
  Ok(())
}

// Serializes, deserializes and evaluates statements
fn run_statements(statements: &[hvm::Statement]) -> hvm::Runtime {
  let str_0 = hvm::view_statements(statements);
  let str_1 = hvm::view_statements(&crate::bits::deserialized_statements(&crate::bits::serialized_statements(&statements)));
  if str_0 != str_1 { eprintln!("ERROR: SERIALIZATION ERROR, PLEASE REPORT") };

  eprintln!("[Evaluation]");
  let mut rt = hvm::init_runtime();
  let init = std::time::Instant::now();
  rt.run_statements(&statements);
  let elapsed = init.elapsed();

  eprintln!();
  eprintln!("[Stats]");
  eprintln!("- cost: {} mana ({} rewrites)", rt.get_mana(), rt.get_rwts());
  eprintln!("- size: {} words", rt.get_size());
  eprintln!("- time: {} ms", elapsed.as_millis());
  rt
}

fn start_node(base_dir: PathBuf, file: Option<String>) {
  // Reads the file contents
  let file = file.map(|file| std::fs::read_to_string(file).expect("File not found."));

  // Node state object
  let node = new_node(base_dir.clone());

  // Node to Miner communication object
  let miner_comm_0 = new_miner_comm();
  let miner_comm_1 = miner_comm_0.clone();

  // User input object
  //let input_0 = new_input();
  //let input_1 = input_0.clone();

  // Spawns the node thread
  let node_thread = thread::spawn(move || {
    node_loop(node, base_dir.clone(), miner_comm_0, file);
  });

  // Spawns the miner thread
  let miner_thread = thread::spawn(move || {
    miner_loop(miner_comm_1);
  });

  // Spawns the input thread
  //let input_thread = thread::spawn(move || {
    //if ui {
      //input_loop(&input_1);
    //}
  //});

  // Joins all threads
  node_thread.join().unwrap();
  miner_thread.join().unwrap();
  //input_thread.join().unwrap();
}

fn get_base_dir(dir_cli: Option<String>) -> Result<PathBuf, String> {
  let dir_env = std::env::var(KINDELIA_DIR_ENV);
  let dir_env =
    match dir_env {
      Ok(dir) => Some(dir),
      Err(err) =>
        if let std::env::VarError::NotPresent = err {
          None
        } else {
          return Err(format!("{} environment variable is not valid: '{}'", KINDELIA_DIR_ENV, err))
        }
    };

  let mut dir_home = dirs::home_dir().unwrap();
  dir_home.push(KINDELIA_HOME_DEFAULT);

  let base_dir =
    dir_cli.or(dir_env).map(|x| PathBuf::from(x)).unwrap_or(dir_home);

  Ok(base_dir)
}

// Tests
// -----

pub fn test_statements_from_code(code: &str) {
  run_statements(&hvm::read_statements(code).1);
}

pub fn test_from_file(path: &str) {
  let file = &std::fs::read_to_string(path).expect("file not found");
  test_statements_from_code(file);
}
