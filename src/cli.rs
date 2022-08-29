#![warn(dead_code)]
#![warn(unused_imports)]
#![warn(non_snake_case)]
#![warn(unused_variables)]
#![warn(clippy::style)]

// TODO: `clean` CLI command

use core::panic;
use std::path::Path;
use std::{path::PathBuf, str::FromStr, thread};

use clap::{Parser, Subcommand};
use hvm::Name;
use warp::Future;

use crate::api::client as api_client;
use crate::bits::{deserialized_statement, serialized_statement};
use crate::crypto;
use crate::hvm::{self, view_statement, Statement};
use crate::node::{
  self, read_address, udp_init, udp_send, Message, MinerCommunication, Node,
  Transaction, UDP_PORT,
};
use crate::util::{bitvec_to_bytes, bytes_to_bitvec};
use crate::ENTRY_PEERS;

/*
== Client ==

kindelia test file.kdl

kindelia serialize code.kdl > code.hex.txt

kindelia deserialize stmt.hex.txt
kindelia deserialize <<< a67bd36d75da

kindelia [--pvt-file] ?
kindelia [--pvt-pass] ?

kindelia sign stmt.hex.txt
kindelia sign <<< a67bd36d75da

kindelia post stmt.hex.txt
kindelia sign <<< a67bd36d75da

kindelia completion zsh >> .zshrc

== Remote ==

kindelia get fn Count code
kindelia get fn Count state
kindelia get fn Count slots

kindelia get ns Foo.Bar owner
kindelia get ns Foo.Bar list

kindelia get bk 0xc7da4b76b4d7a64b7 | kindelia deserialize
kindelia get bk 751
kindelia get bk 2756

kindelia get ct Pair code
kindelia get ct Pair arity

kindelia get tick
kindelia get mana
kindelia get space

kindelia get fn-count
kindelia get ns-count
kindelia get ct-count

kindelia run  [--host ""] code.hex.txt
kindelia post [--host ""] code.hex.txt

== Node ==

kindelia node start --mine --local --log-events --nice-ui?
kindelia node clean [-f]       // asks confirmation

*/

// Clap Struct
// ===========

#[derive(Parser)]
#[clap(author, version, about, long_about = None)]
pub struct Cli {
  #[clap(subcommand)]
  command: CLICommand,
  /// Path to config file.
  config: Option<PathBuf>,
}

#[derive(Subcommand)]
pub enum CLICommand {
  /// Test a file.
  Test {
    /// The path to the file to test.
    file: PathBuf,
  },
  /// Serialize a code file.
  Serialize {
    /// The path to the file to serialize.
    file: PathBuf,
  },
  /// Deserialize a code file.
  Deserialize {
    /// The path to the file to deserialize.
    file: Option<PathBuf>,
  },
  /// Sign a code file.
  Sign {
    /// The path to the file to sign.
    file: Option<PathBuf>,
    /// File containing the 256-bit secret key, as a hex string
    #[clap(short, long)]
    skey: String,
  },
  /// Post a code file.
  Post {
    /// The path to the file to post.
    file: Option<PathBuf>,
    /// Node address to post to.
    #[clap(short, long)]
    host: Option<String>,
  },
  /// Auto-complete for a shell.
  Completion {
    /// The shell to generate completion for.
    shell: String,
  },
  /// Get remote information.
  Get {
    /// The kind of information to get.
    #[clap(subcommand)]
    kind: GetKind,
  },
  /// Runs a Kindelia (.kdl) file
  Run {
    /// Input file
    file: PathBuf,
  },
  /// Starts a Kindelia node
  Start {
    /// Path to store the node's data in
    #[clap(short, long)]
    kindelia_path: Option<PathBuf>,
    /// Adds testnet nodes as initial peers
    #[clap(long)]
    init_peers: Option<Vec<String>>,
    /// Mine blocks
    #[clap(long)]
    mine: bool,
  },
}

#[derive(Subcommand)]
pub enum GetKind {
  /// Get a function by name.
  Fn {
    /// The name of the function to get.
    name: Name,
    /// The stat of the function to get.
    #[clap(subcommand)]
    stat: GetFnKind,
  },
  /// Get a namespace by name.
  Ns {
    /// The name of the namespace to get.
    name: String, // ASK: use Name here too?
    /// The stat of the namespace to get.
    #[clap(subcommand)]
    stat: GetNsKind,
  },
  /// Get a block by hash.
  Bk {
    /// The hash of the block to get.
    hash: String,
  },
  /// Get a constructor by name.
  Ct {
    /// The name of the constructor to get.
    name: Name,
    /// The stat of the constructor to get.
    #[clap(subcommand)]
    stat: GetCtKind,
  },
  /// Get the runtime tick.
  Tick,
  /// Get the runtime mana.
  Mana,
  /// Get the runtime space.
  Space,
  /// Get the number of functions.
  FnCount,
  /// Get the number of namespaces.
  NsCount,
  /// Get the number of constructors.
  CtCount,
}

#[derive(Subcommand)]
pub enum GetFnKind {
  /// Get the code of a function.
  Code,
  /// Get the state of a function.
  State,
  /// Get the slots of a function.
  Slots,
}

#[derive(Subcommand)]
pub enum GetNsKind {
  /// Get the owner of a namespace.
  Owner,
  /// Get the list of statements in a namespace.
  List,
}

#[derive(Subcommand)]
pub enum GetCtKind {
  /// Get the code of a constructor.
  Code,
  /// Get the arity of a constructor.
  Arity,
}

struct ConfigValueOption<'a, T: Clone, F: Fn() -> T> {
  value: Option<T>,
  env: Option<&'a str>,
  config: ConfigFileOptions<'a>,
  default: F,
}

impl<'a, T: Clone, F: Fn() -> T> ConfigValueOption<'a, T, F> {
  /// Resolve config value.
  ///
  /// Priority is:
  /// 1. CLI argument
  /// 2. Environment variable
  /// 3. Config file
  /// 4. Default value
  fn get_value_config(self) -> T
  where
    T: ConvertFrom<String> + serde::Deserialize<'a>,
  {
    if let Some(value) = self.value {
      // read from var
      value
    } else if let Some(Ok(env_value)) = self.env.map(std::env::var) {
      // if env var is set and valid, read from env var
      T::convert(env_value)
    } else if let ConfigFileOptions {
      toml: Some(toml_value),
      prop: Some(prop),
    } = self.config
    {
      // if config file is set and valid, read from config file
      // doing this way because of issue #469 toml-rs
      toml_value.get(prop).unwrap().clone().try_into::<T>().unwrap()
    } else {
      (self.default)()
    }
  }
}

// Parse function
// ==============

/// Parse Cli arguments and do an action
pub fn run_cli() -> Result<(), String> {
  let parsed = Cli::parse();
  let default_kindelia_path = || dirs::home_dir().unwrap().join(".kindelia");

  // get possible config path and content
  let config_path = ConfigValueOption {
    value: parsed.config,
    env: Some("KINDELIA_CONFIG"),
    config: ConfigFileOptions::none(),
    default: || PathBuf::from_str("config.toml").expect("config.toml"),
  }
  .get_value_config();
  let config = read_toml(&config_path);

  match parsed.command {
    CLICommand::Serialize { file } => {
      serialize(&file);
      Ok(())
    }
    CLICommand::Deserialize { file } => {
      let content = get_value_stdin::<String>(file);
      deserialize(&content);
      Ok(())
    }
    CLICommand::Sign { file, skey } => {
      let content = get_value_stdin::<String>(file);
      sign(&content, &skey);
      Ok(())
    }
    CLICommand::Post { file, host } => {
      let content = get_value_stdin::<String>(file);
      post(&content, host);
      Ok(())
    }
    CLICommand::Test { file } => {
      run_file(&file);
      Ok(())
    }
    CLICommand::Run { file } => {
      run_file(&file);
      Ok(())
    }
    CLICommand::Start { kindelia_path, init_peers, mine } => {
      // get arguments from cli, env or config
      let path = ConfigValueOption {
        value: kindelia_path,
        env: Some("KINDELIA_PATH"),
        config: ConfigFileOptions::new(&config, "path"),
        default: default_kindelia_path,
      }
      .get_value_config();

      let init_peers = ConfigValueOption {
        value: init_peers,
        env: Some("KINDELIA_INIT_PEERS"),
        config: ConfigFileOptions::new(&config, "init_peers"),
        default: std::vec::Vec::new,
      }
      .get_value_config();

      let mine = ConfigValueOption {
        value: Some(mine), // TODO: fix
        env: Some("KINDELIA_MINE"),
        config: ConfigFileOptions::new(&config, "mine"),
        default: || false,
      }
      .get_value_config();

      // start node
      start(path, init_peers, mine);

      Ok(())
    }
    CLICommand::Completion { shell } => todo!(),
    CLICommand::Get { kind } => {
      let client =
        api_client::ApiClient::new("http://localhost:8000", None).map_err(|e| e.to_string())?;
      match kind {
        GetKind::Fn { name, stat } => match stat {
          GetFnKind::Code => todo!(),
          GetFnKind::State => {
            let state = run_async_blocking(client.get_function_state(name))?;
            println!("{}", state);
            Ok(())
          }
          GetFnKind::Slots => todo!(),
        },
        GetKind::Ns { name, stat } => todo!(),
        GetKind::Bk { hash } => todo!(),
        GetKind::Ct { name, stat } => todo!(),
        GetKind::Tick => todo!(),
        GetKind::Mana => todo!(),
        GetKind::Space => todo!(),
        GetKind::FnCount => todo!(),
        GetKind::NsCount => todo!(),
        GetKind::CtCount => todo!(),
      }
    }
  }
}

// TODO: ew
fn run_async_blocking<T, P>(prom: P) -> Result<T, String>
where
  P: Future<Output = Result<T, reqwest::Error>>,
{
  let runtime = tokio::runtime::Runtime::new().unwrap();
  let res = runtime.block_on(prom).map_err(|err| err.to_string())?;
  Ok(res)
}

// Main Actions
// ============

pub fn get_info(kind: GetKind) -> Result<(), String> {
  let client = api_client::ApiClient::new("http://localhost:8000", None)
    .map_err(|e| e.to_string())?;
  match kind {
    GetKind::Fn { name, stat } => match stat {
      GetFnKind::Code => todo!(),
      GetFnKind::State => {
        let state = run_async_blocking(client.get_function_state(name));
        // TODO: Display trait on `Term`
        println!("{:?}", state);
        Ok(())
      }
      GetFnKind::Slots => todo!(),
    },
    GetKind::Ns { name: _, stat: _ } => todo!(),
    GetKind::Bk { hash: _ } => todo!(),
    GetKind::Ct { name: _, stat: _ } => todo!(),
    GetKind::Tick => todo!(),
    GetKind::Mana => todo!(),
    GetKind::Space => todo!(),
    GetKind::FnCount => todo!(),
    GetKind::NsCount => todo!(),
    GetKind::CtCount => todo!(),
  }
}

pub fn serialize(file: &PathBuf) {
  if let Ok(code) = std::fs::read_to_string(file) {
    let statements =
      hvm::read_statements(&code).map_err(|err| err.erro).unwrap().1;
    for statement in statements {
      println!("{}", hex::encode(serialized_statement(&statement).to_bytes()));
    }
  } else {
    println!("Couldn't load file.");
  }
}

pub fn deserialize(content: &str) {
  let statements = get_statements(content);
  for statement in statements {
    match statement {
      None => println!("Could not deserialize into a statement"),
      Some(statement) => println!("{}", view_statement(&statement)),
    }
  }
}

pub fn sign(content: &str, skey_file: &str) {
  if let Ok(skey) = std::fs::read_to_string(skey_file) {
    if let Some(statement) = get_statement(content) {
      let skey = hex::decode(&skey[0..64]).expect("hex string");
      let user = crypto::Account::from_private_key(&skey);
      let hash = hvm::hash_statement(&statement);
      let sign = user.sign(&hash);
      let stat = hvm::set_sign(&statement, sign);
      println!("{}", hex::encode(serialized_statement(&stat).to_bytes()));
    } else {
      println!("Hex provided isn't a serialized statement.");
    }
  } else {
    println!("Couldn't load term and secret key files.");
  }
}

pub fn post(content: &str, host: Option<String>) {
  if let Some(statement) = get_statement(content) {
    let tx =
      Transaction::new(bitvec_to_bytes(&serialized_statement(&statement)));
    let ms = Message::PleaseMineThisTransaction { trans: tx };
    let ports =
      [UDP_PORT + 100, UDP_PORT + 101, UDP_PORT + 102, UDP_PORT + 103];
    if let Some((mut socket, _)) = udp_init(&ports) {
      let addrs = if let Some(host) = host {
        vec![read_address(&host)]
      } else {
        ENTRY_PEERS.iter().map(|x| read_address(x)).collect()
      };
      udp_send(&mut socket, addrs, &ms);
      println!("Published statement:\n\n{}", view_statement(&statement));
    } else {
      panic!("Couldn't open UDP socket on ports: {:?}.", ports);
    }
  } else {
    println!("Hex provided isn't a serialized statement.");
  }
}

pub fn run_file(file: &Path) {
  let file = std::fs::read_to_string(file);
  match file {
    Err(err) => {
      eprintln!("{}", err);
    }
    Ok(code) => {
      // TODO: flag to disable size limit / debug
      hvm::test_statements_from_code(&code);
    }
  }
}

fn start(kindelia_path: PathBuf, init_peers: Vec<String>, mine: bool) {
  eprintln!("Starting Kindelia node. Store path: {:?}", kindelia_path);
  let init_peers =
    init_peers.iter().map(|x| read_address(x)).collect::<Vec<_>>();
  let init_peers = if !init_peers.is_empty() { Some(init_peers) } else { None };

  dbg!(init_peers.clone());
  dbg!(kindelia_path.clone());
  dbg!(mine);

  // Reads the file contents
  //let file = file.map(|file| std::fs::read_to_string(file).expect("Block file not found."));

  // Node state object
  let (node_query_sender, node) = Node::new(kindelia_path, &init_peers);

  // Node to Miner communication object
  let miner_comm_0 = MinerCommunication::new();
  let miner_comm_1 = miner_comm_0.clone();

  // Threads
  let mut threads = vec![];

  // Spawns the node thread
  let node_thread = thread::spawn(move || {
    node.main(miner_comm_0, mine);
  });
  threads.push(node_thread);

  // Spawns the miner thread
  if mine {
    let miner_thread = thread::spawn(move || {
      node::miner_loop(miner_comm_1);
    });
    threads.push(miner_thread);
  }

  // Spawns the API thread
  let api_thread = thread::spawn(move || {
    crate::api::server::http_api_loop(node_query_sender);
  });
  threads.push(api_thread);

  // Joins all threads
  for thread in threads {
    thread.join().unwrap();
  }
}

// Auxiliar Functions and Structs
// ==================

#[derive(Debug, Clone)]
struct ConfigFileOptions<'a> {
  toml: Option<&'a toml::Value>,
  prop: Option<String>,
}

impl<'a> ConfigFileOptions<'a> {
  pub fn new(toml: &'a Option<toml::Value>, prop: &str) -> Self {
    match toml {
      Some(_) => {
        ConfigFileOptions { toml: toml.as_ref(), prop: Some(prop.into()) }
      }
      None => Self::none(),
    }
  }

  pub fn none() -> Self {
    ConfigFileOptions { toml: None, prop: None }
  }
}

#[allow(dead_code)]
fn get_statements(txt: &str) -> Vec<Option<Statement>> {
  txt.split(|c: char| c.is_whitespace()).map(get_statement).collect()
}

fn get_statement(hex: &str) -> Option<Statement> {
  deserialized_statement(&bytes_to_bitvec(
    &hex::decode(hex).expect("hex string"),
  ))
}

fn get_value_stdin<T: ConvertFrom<String>>(file: Option<PathBuf>) -> T {
  if let Some(file) = file {
    // read from file
    T::convert(std::fs::read_to_string(file).unwrap())
  } else {
    // read from stdin
    let mut input = String::new();
    if std::io::stdin().read_line(&mut input).is_ok() {
      T::convert(input.trim().to_string())
    } else {
      panic!("Could not read file path or stdin");
    }
  }
}

// fn get_value_config<'a, T, F>(
//   value: Option<T>,
//   env_options: Option<&str>,
//   config_options: ConfigOptions<'a>,
//   default: F,
// ) -> T
// where
//   T: ConvertFrom<String> + serde::Deserialize<'a>,
//   F: Fn() -> T,
// {
//   if let Some(value) = value {
//     // read from var
//     value
//   } else if let Some(Ok(env_value)) = env_options.map(|e| std::env::var(e)) {
//     // if env var is set and valid, read from env var
//     T::convert(env_value)
//   } else if let ConfigOptions(Some(toml_value), Some(prop)) = config_options {
//     // if config file is set and valid, read from config file
//     // doing this way because of issue #469 toml-rs
//     toml_value.get(prop).unwrap().clone().try_into::<T>().unwrap()
//   } else {
//     default()
//   }
// }

fn read_toml(file: &PathBuf) -> Option<toml::Value> {
  std::fs::read_to_string(file)
    .ok()
    .and_then(|content| content.parse::<toml::Value>().ok())
}

// Auxiliar Traits
// ===============

/// A trait to convert from anything to a type T.
/// It is equal to standard From trait, but
/// it has the From<String> for Vec<String> implementation.
/// As like From, the conversion must be perfect.
trait ConvertFrom<T> {
  fn convert(t: T) -> Self;
}

impl ConvertFrom<String> for String {
  fn convert(t: String) -> Self {
    t
  }
}

impl ConvertFrom<String> for Vec<String> {
  fn convert(t: String) -> Self {
    t.split(',').map(|x| x.to_string()).collect()
  }
}

impl ConvertFrom<String> for bool {
  fn convert(t: String) -> Self {
    if t == "true" {
      true
    } else if t == "false" {
      false
    } else {
      panic!("Invalid boolean value: {}", t);
    }
  }
}

impl ConvertFrom<String> for PathBuf {
  fn convert(t: String) -> Self {
    PathBuf::from_str(&t).expect("Invalid path")
  }
}
