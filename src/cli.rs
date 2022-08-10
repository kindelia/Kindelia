use crate::{
  bits::{deserialized_statement, serialized_statement},
  crypto,
  hvm::{self, view_statement, Statement},
  node::{
    self, read_address, udp_init, udp_send, Address, Message, MinerCommunication, Node,
    Transaction, UDP_PORT,
  },
  util::{bitvec_to_bytes, bytes_to_bitvec},
  ENTRY_PEERS,
};
use clap::{Parser, Subcommand};
use core::panic;
use std::collections::HashMap;
use std::{path::PathBuf, str::FromStr, thread};

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
pub struct CLI {
  #[clap(subcommand)]
  command: CLICommand,
}

#[derive(Subcommand)]
pub enum CLICommand {
  /// Test a file.
  Test {
    /// The path to the file to test.
    file: String,
  },
  /// Serialize a code file.
  Serialize {
    /// The path to the file to serialize.
    file: String,
  },
  /// Deserialize a code file.
  Deserialize {
    /// The path to the file to deserialize.
    file: Option<String>,
  },
  /// Sign a code file.
  Sign {
    /// The path to the file to sign.
    file: Option<String>,
    /// File containing the 256-bit secret key, as a hex string
    #[clap(short, long)]
    skey: String,
  },
  /// Post a code file.
  Post {
    /// The path to the file to post.
    file: Option<String>,
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
    file: String,
  },
  /// Starts a Kindelia node
  Start {
    /// Path to store the node's data in
    #[clap(short, long)]
    kindelia_path: Option<String>,
    /// Path to config file
    #[clap(short, long)]
    config_path: Option<String>,
    /// Adds testnet nodes as initial peers
    #[clap(long)]
    init_peers: Option<Vec<String>>,
    /// Mine blocks
    #[clap(long)]
    mine: Option<bool>,
  },
}

#[derive(Subcommand)]
pub enum GetKind {
  /// Get a function by name.
  Fn {
    /// The name of the function to get.
    name: String,
    /// The stat of the function to get.
    #[clap(subcommand)]
    stat: GetFnKind,
  },
  /// Get a namespace by name.
  Ns {
    /// The name of the namespace to get.
    name: String,
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
    name: String,
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

// Parse function
// ==============

/// Parse Cli arguments and do an action
pub fn parse() {
  let parsed = CLI::parse();
  match parsed.command {
    CLICommand::Serialize { file } => serialize(&file),
    CLICommand::Deserialize { file } => {
      let content = get_value_stdin::<String>(file);
      deserialize(&content);
    }
    CLICommand::Sign { file, skey } => {
      let content = get_value_stdin::<String>(file);
      sign(&content, &skey);
    }
    CLICommand::Post { file, host } => {
      let content = get_value_stdin::<String>(file);
      post(&content, host);
    }
    CLICommand::Test { file } => run(&file),
    CLICommand::Run { file } => run(&file),
    CLICommand::Start { kindelia_path, config_path, init_peers, mine } => {
      // get possible config path and content
      let config_path = get_value_config(
        config_path.map(PathBuf::from),
        Some("KINDELIA_CONFIG"),
        ConfigOptions::none(),
        PathBuf::from_str("config.toml").expect("config.toml"),
      );
      let config = read_toml(&config_path);

      // get arguments from cli, env or config
      let path = get_value_config(
        kindelia_path.map(PathBuf::from),
        Some("KINDELIA_PATH"),
        ConfigOptions::new(&config, "path"),
        dirs::home_dir().unwrap().join(".kindelia"),
      );
      let init_peers = get_value_config(
        init_peers,
        Some("KINDELIA_INIT_PEERS"),
        ConfigOptions::new(&config, "init_peers"),
        vec![],
      );
      let mine =
        get_value_config(mine, Some("KINDELIA_MINE"), ConfigOptions::new(&config, "mine"), false);

      // start node
      start(path, init_peers, mine);
    }
    _ => {
      unimplemented!()
    }
  }
}

// Main Actions
// ============

pub fn serialize(file: &str) {
  if let Ok(code) = std::fs::read_to_string(file) {
    let statements = hvm::read_statements(&code).map_err(|err| err.erro).unwrap().1;
    for statement in statements {
      println!("{}", hex::encode(serialized_statement(&statement).to_bytes()));
    }
  } else {
    println!("Couldn't load file.");
  }
}

pub fn deserialize(content: &str) {
  let statement = get_statement(&content).expect("invalid hex string");
  println!("{}", view_statement(&statement));
}

pub fn sign(content: &str, skey_file: &str) {
  if let Ok(skey) = std::fs::read_to_string(skey_file) {
    if let Some(statement) = get_statement(&content) {
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
  if let Some(statement) = get_statement(&content) {
    let tx = Transaction::new(bitvec_to_bytes(&serialized_statement(&statement)));
    let ms = Message::PleaseMineThisTransaction { trans: tx };
    let ports = [UDP_PORT + 100, UDP_PORT + 101, UDP_PORT + 102, UDP_PORT + 103];
    if let Some((mut socket, port)) = udp_init(&ports) {
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

pub fn test(file: &str) {
  let file = std::fs::read_to_string(file);
  match file {
    Err(err) => {
      return println!("{}", err);
    }
    Ok(code) => {
      // TODO: flag to disable size limit / debug
      hvm::test_statements_from_code(&code);
    }
  }
}

fn run(file: &str) {
  let code = std::fs::read_to_string(file).unwrap();
  // TODO: flag to disable size limit / debug
  hvm::test_statements_from_code(&code);
}

fn start(kindelia_path: PathBuf, init_peers: Vec<String>, mine: bool) {
  eprintln!("Starting Kindelia node. Store path: {:?}", kindelia_path);
  let init_peers = init_peers.iter().map(|x| read_address(x)).collect::<Vec<_>>();
  let init_peers = if init_peers.len() > 0 { Some(init_peers) } else { None };

  dbg!(init_peers.clone());
  dbg!(kindelia_path.clone());
  dbg!(mine);

  // Reads the file contents
  //let file = file.map(|file| std::fs::read_to_string(file).expect("Block file not found."));

  // Node state object
  let (node_query_sender, node) = Node::new(kindelia_path.clone(), &init_peers);

  // Node to Miner communication object
  let miner_comm_0 = MinerCommunication::new();
  let miner_comm_1 = miner_comm_0.clone();

  // API thread channel
  //let (api_send, api_recv) = mpsc::channel();

  // Threads
  let mut threads = vec![];

  // Spawns the node thread
  let node_thread = thread::spawn(move || {
    node.main(kindelia_path.clone(), miner_comm_0, mine);
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
    crate::api::http::http_api_loop(node_query_sender);
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
struct ConfigOptions<'a>(Option<toml::Value>, Option<&'a str>);
impl<'a> ConfigOptions<'a> {
  pub fn new(toml_value: &Option<toml::Value>, prop: &'a str) -> Self {
    ConfigOptions(toml_value.clone(), Some(prop))
  }
  pub fn none() -> Self {
    ConfigOptions(None, None)
  }
}

fn get_statements(txt: &str) -> Vec<Option<Statement>> {
  txt.split('\n').map(|hex| get_statement(hex)).collect()
}

fn get_statement(hex: &str) -> Option<Statement> {
  return deserialized_statement(&bytes_to_bitvec(&hex::decode(hex).expect("hex string")));
}

fn get_value_stdin<T: ConvertFrom<String>>(file: Option<String>) -> T {
  if let Some(file) = file {
    // read from file
    T::convert(std::fs::read_to_string(file).unwrap())
  } else {
    // read from stdin
    let mut input = String::new();
    if let Ok(_) = std::io::stdin().read_line(&mut input) {
      T::convert(input.trim().to_string())
    } else {
      panic!("Could not read file path or stdin");
    }
  }
}

fn get_value_config<'a, T: ConvertFrom<String> + serde::Deserialize<'a>>(
  value: Option<T>,
  env_options: Option<&str>,
  config_options: ConfigOptions<'a>,
  default: T,
) -> T {
  if let Some(value) = value {
    // read from var
    value
  } else if let Some(Ok(env_value)) = env_options.map(|e| std::env::var(e)) {
    // if env var is set and valid, read from env var
    T::convert(env_value)
  } else if let ConfigOptions(Some(toml_value), Some(prop)) = config_options {
    // if config file is set and valid, read from config file
    // doing this way because of issue #469 toml-rs
    toml_value.get(prop).unwrap().clone().try_into::<T>().unwrap()
  } else {
    default
  }
}

fn read_toml(file: &PathBuf) -> Option<toml::Value> {
  std::fs::read_to_string(file).ok().and_then(|content| content.parse::<toml::Value>().ok())
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
