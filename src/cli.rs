// TODO: `kindelia node clean` CLI command
// TODO: `kindelia get (ctr|reg|block) commands
// TODO: flag to enable printing events (heartbeat)
// TODO: some way to pretty-print events (heartbeat)

use std::fmt;
use std::io::Read;
use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::thread;
use std::time::Duration;

use clap::{Parser, Subcommand};
use hvm::Name;
use warp::Future;

use crate::api::{client as api_client, HexStatement};
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

kindelia deserialize code.hex.txt
kindelia deserialize <<< a67bd36d75da

kindelia run-remote --hex <<< a67bd36d75da
kindelia publish    --hex <<< a67bd36d75da

kindelia sign code.hex.txt
kindelia sign <<< a67bd36d75da > code.sig.hex.tx

kindelia completion zsh >> .zshrc

== Remote ==

kindelia get fun Count code
kindelia get fun Count state
kindelia get fun Count slots

kindelia get reg Foo.Bar owner
kindelia get reg Foo.Bar list

kindelia get block 0xc7da4b76b4d7a64b7 | kindelia deserialize
kindelia get block 751
kindelia get block 2756

kindelia get ctr Pair code
kindelia get ctr Pair arity

kindelia get run <BLOCK_IDX> <STM_IDX>

kindelia get stats

kindelia get stats tick
kindelia get stats mana
kindelia get stats space
kindelia get stats ctr-count
kindelia get stats fun-count
kindelia get stats reg-count

kindelia [--api ""] run-remote  code.hex.txt
kindelia [--api ""] publish     code.hex.txt

== Node ==

kindelia node start --mine --local --log-events --nice-ui?
kindelia node clean [-f]       // asks confirmation

== Accounts ==

kindelia account ...

*/

fn run_on_remote<T, P, F>(
  api_url: &str,
  file: FileInput,
  encoded: bool,
  f: F,
) -> Result<T, String>
where
  F: FnOnce(api_client::ApiClient, Vec<HexStatement>) -> P,
  P: Future<Output = Result<T, String>>,
{
  let code = arg_from_file_or_stdin::<String>(file)?;
  let stmts =
    if encoded { statments_from_hex_seq(&code)? } else { parse_code(&code)? };
  let stmts: Vec<HexStatement> = stmts.into_iter().map(|s| s.into()).collect();
  let client =
    api_client::ApiClient::new(api_url, None).map_err(|e| e.to_string())?;
  run_async_blocking(f(client, stmts))
}

// Clap CLI definitions
// ====================

#[derive(Parser)]
#[clap(author, version, about, long_about = None)]
pub struct Cli {
  #[clap(subcommand)]
  command: CliCommand,
  #[clap(long, short = 'c')]
  /// Path to config file.
  config: Option<PathBuf>,
  /// Url to server host
  #[clap(long)]
  api: Option<String>,
}

#[derive(Subcommand)]
pub enum CliCommand {
  /// Test a Kindelia code file (.kdl), running locally.
  Test {
    /// The path to the file to test.
    file: FileInput,
    /// Whether to consider size and mana in the execution.
    #[clap(long)]
    debug: bool,
  },
  /// Serialize a code file.
  Serialize {
    /// The path to the file to serialize.
    file: FileInput,
  },
  /// Deserialize a code file.
  Deserialize {
    /// The path to the file to deserialize.
    file: FileInput,
  },
  /// Sign a code file.
  Sign {
    /// The path to the file to sign.
    file: FileInput,
    /// File containing the 256-bit secret key, as a hex string
    #[clap(long, short = 's')]
    secret_file: PathBuf,
    #[clap(long, short = 'e')]
    encoded: bool,
    #[clap(long, short = 'E')]
    encoded_output: bool,
  },
  /// Test a Kindelia (.kdl) file, dry-running it on the current remote KVM state.
  RunRemote {
    /// Input file.
    file: FileInput,
    /// In case the input code is serialized.
    #[clap(long, short = 'e')]
    encoded: bool,
  },
  /// Post a Kindelia code file.
  Publish {
    /// The path to the file to post.
    file: FileInput,
    /// In case the input code is serialized.
    #[clap(long, short = 'e')]
    encoded: bool,
  },
  /// Post a Kindelia code file, using the UDP interface. [DEPRECATED]
  PostUdp {
    /// The path to the file to post.
    file: FileInput,
    /// Node address to post to.
    #[clap(long)]
    host: Option<String>,
  },
  /// Get remote information.
  Get {
    /// The kind of information to get.
    #[clap(subcommand)]
    kind: GetKind,
    #[clap(long, short)]
    /// Outputs JSON machine readable output.
    json: bool,
  },
  /// Initialize the configuration file.
  Init,
  /// Node commands.
  Node {
    /// Which command run.
    #[clap(subcommand)]
    command: NodeCommand,
  },
  /// Generate auto-completion for a shell.
  Completion {
    /// The shell to generate completion for.
    shell: String,
  },
}

#[derive(Subcommand)]
pub enum NodeCommand {
  /// [NOT IMPLEMENTED] Clean the node's data.
  Clean,
  /// Starts a Kindelia node.
  Start {
    /// Base path to store the node's data in.
    #[clap(long)]
    base_path: Option<PathBuf>,
    /// Initial peer nodes.
    #[clap(long, short = 'p')]
    initial_peers: Option<Vec<String>>,
    /// Mine blocks.
    #[clap(long, short = 'm')]
    mine: bool,
  },
}

#[derive(Subcommand)]
pub enum GetKind {
  /// Get a constructor by name.
  Ctr {
    /// The name of the constructor to get.
    name: Name,
    /// The stat of the constructor to get.
    #[clap(subcommand)]
    stat: GetCtrKind,
  },
  /// [NOT IMPLEMENTED] Get a block by hash.
  Block {
    /// The hash of the block to get.
    hash: String,
  },
  /// Get a function by name.
  Fun {
    /// The name of the function to get.
    name: Name,
    /// The stat of the function to get.
    #[clap(subcommand)]
    stat: GetFunKind,
  },
  /// [NOT IMPLEMENTED] Get a registered namespace by name.
  Reg {
    /// The name of the namespace to get.
    name: String, // ASK: use Name here too?
    /// The stat of the namespace to get.
    #[clap(subcommand)]
    stat: GetRegKind,
  },
  /// Get node stats.
  Stats {
    /// The stat of the node to get.
    #[clap(subcommand)]
    stat_kind: Option<GetStatsKind>,
  },
  Peers {
    /// Get all seen peers, including inactive ones
    #[clap(long)]
    all: bool
  },
}

#[derive(Subcommand)]
pub enum GetFunKind {
  /// Get the code of a function.
  Code,
  /// Get the state of a function.
  State,
  /// Get the slots of a function.
  Slots,
}

#[derive(Subcommand)]
pub enum GetRegKind {
  /// Get the owner of a namespace.
  Owner,
  /// Get the list of statements in a namespace.
  List,
}

#[derive(Subcommand)]
pub enum GetCtrKind {
  /// Get the code of a constructor.
  Code,
  /// Get the arity of a constructor.
  Arity,
}

#[derive(Subcommand)]
pub enum GetStatsKind {
  /// Get the tick (tip block height).
  Tick,
  /// Get the used mana.
  Mana,
  /// Get the quantity of used space.
  // TODO: we should measure this as slots/nodes/cells, not bits
  Space,
  /// Get the number of functions.
  FunCount,
  /// Get the number of constructors.
  CtrCount,
  /// Get the number of namespaces.
  RegCount,
}

struct ConfigValueOption<'a, T, F>
where
  T: Clone + Sized,
  F: Fn() -> Result<T, String>,
{
  value: Option<T>,
  env: Option<&'a str>,
  config: ConfigFileOptions<'a>,
  default: F,
}

impl<'a, T, F> ConfigValueOption<'a, T, F>
where
  T: Clone + Sized,
  F: Fn() -> Result<T, String>,
{
  /// Resolve config value.
  ///
  /// Priority is:
  /// 1. CLI argument
  /// 2. Environment variable
  /// 3. Config file
  /// 4. Default value
  fn get_config_value(self) -> Result<T, String>
  where
    T: ArgumentFrom<String> + ArgumentFrom<toml::Value>,
  {
    if let Some(value) = self.value {
      // read from var
      Ok(value)
    } else if let Some(Ok(env_value)) = self.env.map(std::env::var) {
      // if env var is set and valid, read from env var
      T::arg_from(env_value)
    } else if let ConfigFileOptions {
      toml: Some(toml_value),
      prop: Some(prop_path),
    } = self.config
    {
      // if config file is set and valid, read from config file
      // doing this way because of issue #469 toml-rs
      let props: Vec<_> = prop_path.split('.').collect();
      let mut value = toml_value.get(&props[0]).ok_or(format!(
        "Could not found prop '{}' in config file.",
        prop_path
      ))?;
      for prop in &props[1..] {
        value = value.get(&prop).ok_or(format!(
          "Could not found prop {} in config file.",
          prop_path
        ))?;
      }
      T::arg_from(value.clone()).map_err(|_| {
        format!("Could not convert value '{}' into desired type.", value)
      })
    } else {
      (self.default)()
    }
  }
}

// CLI main function
// =================

// TODO: refactor into main?

/// Parse Cli arguments and do an action
pub fn run_cli() -> Result<(), String> {
  let parsed = Cli::parse();
  let default_kindelia_path = || {
    let home_dir = dirs::home_dir().ok_or("Could not find $HOME")?;
    Ok(home_dir.join(".kindelia"))
  };

  let default_config_path = || {
    let kindelia_path = default_kindelia_path()?;
    Ok(kindelia_path.join("kindelia.toml"))
  };

  // get possible config path and content
  let config_path = ConfigValueOption {
    value: parsed.config,
    env: Some("KINDELIA_CONFIG"),
    config: ConfigFileOptions::none(),
    default: default_config_path,
  }
  .get_config_value()?;

  let api_url = ConfigValueOption {
    value: parsed.api,
    env: Some("KINDELIA_API_URL"),
    config: ConfigFileOptions::none(),
    default: || Ok("http://localhost:8000".to_string()),
  }
  .get_config_value()?;

  match parsed.command {
    CliCommand::Test { file, debug } => {
      let code: String = file.read_to_string()?;
      test_code(&code, debug);
      Ok(())
    }
    CliCommand::Serialize { file } => {
      let code: String = file.read_to_string()?;
      serialize_code(&code);
      Ok(())
    }
    CliCommand::Deserialize { file } => {
      let code: String = file.read_to_string()?;
      deserialize_code(&code)
    }
    CliCommand::Sign { file, secret_file, encoded, encoded_output } => {
      let skey: String = arg_from_file_or_stdin(secret_file.into())?;
      let skey = skey.trim();
      let skey = hex::decode(skey).map_err(|err| {
        format!("Secret key should be valid hex string: {}", err)
      })?;
      let skey: [u8; 32] = skey
        .try_into()
        .map_err(|_| "Secret key should have exactly 64 bytes".to_string())?;
      let code = load_code(file, encoded)?;
      let statement = match &code[..] {
        [stmt] => sign_code(stmt, &skey),
        _ => Err("Input file should contain exactly one statement".to_string()),
      }?;
      if encoded_output {
        println!(
          "{}",
          hex::encode(serialized_statement(&statement).to_bytes())
        );
      } else {
        println!("{}", view_statement(&statement));
      };
      Ok(())
    }
    CliCommand::RunRemote { file, encoded } => {
      // TODO: client timeout
      let f = |client: api_client::ApiClient, stmts| async move {
        client.run_code(stmts).await
      };
      let results = run_on_remote(&api_url, file, encoded, f)?;
      for result in results {
        println!("{}", result);
      }
      Ok(())
    }
    CliCommand::Publish { file, encoded } => {
      let f = |client: api_client::ApiClient, stmts| async move {
        client.publish_code(stmts).await
      };
      let results = run_on_remote(&api_url, file, encoded, f)?;
      for (i, result) in results.iter().enumerate() {
        print!("Transaction #{}: ", i);
        match result {
          Ok(_) => println!("PUBLISHED (tx added to mempool)"),
          Err(_) => {
            println!("NOT PUBLISHED [tx is probably already on mempool]")
          }
        }
      }
      Ok(())
    }
    CliCommand::PostUdp { file, host } => {
      let content = arg_from_file_or_stdin::<String>(file)?;
      post_udp(&content, host)
    }
    CliCommand::Get { kind, json } => {
      let prom = get_info(kind, json, &api_url);
      run_async_blocking(prom)
    }
    CliCommand::Init => {
      let path = default_config_path()?;
      eprintln!("Writing default configuration to '{}'...", path.display());
      init_config_file(&path)?;
      Ok(())
    }
    CliCommand::Node { command } => {
      match command {
        NodeCommand::Clean => todo!("`kindelia node clean`"),
        NodeCommand::Start {
          base_path: kindelia_path,
          initial_peers,
          mine,
        } => {
          // TODO: refactor config resolution out of command handling (how?)

          let config = Some(handle_config_file(&config_path)?);

          // get arguments from cli, env or config
          let path = ConfigValueOption {
            value: kindelia_path,
            env: Some("KINDELIA_PATH"),
            config: ConfigFileOptions::new(&config, "node.data.dir"),
            default: default_kindelia_path,
          }
          .get_config_value()?;

          let initial_peers = ConfigValueOption {
            value: initial_peers,
            env: Some("KINDELIA_INITIAL_PEERS"),
            config: ConfigFileOptions::new(
              &config,
              "node.network.initial_peers",
            ),
            default: || Ok(Vec::new()),
          }
          .get_config_value()?;

          let mine = ConfigValueOption {
            value: Some(mine), // TODO: fix boolean resolution
            env: Some("KINDELIA_MINE"),
            config: ConfigFileOptions::new(&config, "node.data.mine"),
            default: || Ok(false),
          }
          .get_config_value()?;

          // start node
          start(path, initial_peers, mine);

          Ok(())
        }
      }
    }
    CliCommand::Completion { .. } => todo!(),
  }
}

// Main Actions
// ============

pub async fn get_info(
  kind: GetKind,
  json: bool,
  host_url: &str,
) -> Result<(), String> {
  let client =
    api_client::ApiClient::new(host_url, None).map_err(|e| e.to_string())?;
  match kind {
    GetKind::Block { hash: _ } => todo!(),
    GetKind::Ctr { name, stat } => {
      let ctr_info = client.get_constructor(name).await?;
      match stat {
        GetCtrKind::Arity => {
          println!("{}", ctr_info.arit)
        }
        GetCtrKind::Code => {
          let args = (0..ctr_info.arit)
            .map(|x| format!("x{}", x))
            .collect::<Vec<_>>()
            .join(" ");
          println!("{{{} {}}}", name, args)
        }
      }
      Ok(())
    }
    GetKind::Fun { name, stat } => match stat {
      GetFunKind::Code => {
        let func_info = client.get_function(name).await?;
        if json {
          println!("{}", serde_json::to_string(&func_info).unwrap());
        } else {
          let func = func_info.func;
          let statement = hvm::Statement::Fun {
            name,
            args: vec![Name::NONE],
            func,
            init: hvm::Term::var(Name::NONE),
            sign: None,
          };
          println!("{}", statement);
        }
        Ok(())
      }
      GetFunKind::State => {
        let state = client.get_function_state(name).await?;
        if json {
          println!("{}", serde_json::to_string_pretty(&state).unwrap());
        } else {
          println!("{}", state);
        }
        Ok(())
      }
      GetFunKind::Slots => todo!(),
    },
    GetKind::Reg { name: _, stat: _ } => todo!(),
    GetKind::Stats { stat_kind } => {
      let stats = client.get_stats().await?;
      match stat_kind {
        None => {
          if json {
            println!("{}", serde_json::to_string_pretty(&stats).unwrap());
          } else {
            println!("{:#?}", stats);
          }
        }
        Some(stat_kind) => {
          let val = match stat_kind {
            GetStatsKind::Tick => stats.tick,
            GetStatsKind::Mana => stats.mana,
            GetStatsKind::Space => stats.space,
            GetStatsKind::FunCount => stats.fun_count,
            GetStatsKind::CtrCount => stats.ctr_count,
            GetStatsKind::RegCount => stats.reg_count,
          };
          println!("{}", val);
        }
      };
      Ok(())
    }
    GetKind::Peers { all } => {
      let peers = client.get_peers(all).await?;
      for peer in peers {
        println!("{}", peer.address)
      }
      Ok(())
    }
  }
}

pub fn serialize_code(code: &str) {
  let statements =
    hvm::read_statements(code).map_err(|err| err.erro).unwrap().1;
  for statement in statements {
    println!("{}", hex::encode(serialized_statement(&statement).to_bytes()));
  }
}

pub fn deserialize_code(content: &str) -> Result<(), String> {
  let statements = statments_from_hex_seq(content)?;
  for statement in statements {
    println!("{}", view_statement(&statement))
  }
  Ok(())
}

// TODO: should not open file
pub fn sign_code(
  statement: &Statement,
  skey: &[u8; 32],
) -> Result<Statement, String> {
  let user = crypto::Account::from_private_key(skey);
  let hash = hvm::hash_statement(statement);
  let sign = user.sign(&hash);
  match statement {
    Statement::Fun { sign, .. }
    | Statement::Ctr { sign, .. }
    | Statement::Run { sign, .. }
    | Statement::Reg { sign, .. } => {
      if sign.is_some() {
        return Err("Statement already has a signature.".to_string());
      }
    }
  };
  let stat = hvm::set_sign(statement, sign);
  Ok(stat)
}

pub fn post_udp(content: &str, host: Option<String>) -> Result<(), String> {
  let statements = statments_from_hex_seq(content)?;
  for statement in statements {
    let tx =
      Transaction::new(bitvec_to_bytes(&serialized_statement(&statement)));
    let ms = Message::PleaseMineThisTransaction { trans: tx };
    let ports =
      [UDP_PORT + 100, UDP_PORT + 101, UDP_PORT + 102, UDP_PORT + 103];
    if let Some((mut socket, _)) = udp_init(&ports) {
      let addrs = if let Some(ref host) = host {
        vec![read_address(host)]
      } else {
        ENTRY_PEERS.iter().map(|x| read_address(x)).collect()
      };
      udp_send(&mut socket, addrs, &ms);
      println!("Published statement:\n\n{}", view_statement(&statement));
    } else {
      return Err(format!("Couldn't open UDP socket on ports: {:?}.", ports));
    }
  }
  Ok(())
}

pub fn test_code(code: &str, debug: bool) {
  hvm::test_statements_from_code(code, debug);
}

fn start(kindelia_path: PathBuf, init_peers: Vec<String>, mine: bool) {
  eprintln!("Starting Kindelia node. Store path: {:?}", kindelia_path);
  let init_peers =
    init_peers.iter().map(|x| read_address(x)).collect::<Vec<_>>();
  let init_peers = if !init_peers.is_empty() { Some(init_peers) } else { None };

  // dbg!(init_peers.clone());
  // dbg!(kindelia_path.clone());
  // dbg!(mine);

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

// Auxiliar
// ========

// Async
// -----

fn run_async_blocking<T, E: ToString, P>(prom: P) -> Result<T, E>
where
  P: Future<Output = Result<T, E>>,
{
  let runtime = tokio::runtime::Runtime::new().unwrap();
  runtime.block_on(prom)
}

// Config
// ------

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

fn handle_config_file(path: &Path) -> Result<toml::Value, String> {
  if !path.exists() {
    eprintln!("WARNING: Config file not found. Default config file will be created on '{}'...\n", path.display());
    init_config_file(path)?;
    thread::sleep(Duration::from_millis(5000));
  }
  let content = std::fs::read_to_string(path).map_err(|e| {
    format!("Error reading config file from '{}': {}", path.display(), e)
  })?;
  let config = content.parse::<toml::Value>().map_err(|e| {
    format!("Error parsing config file from '{}': {}", path.display(), e)
  })?;
  Ok(config)
}

fn init_config_file(path: &Path) -> Result<(), String> {
  let dir_path = path.parent().ok_or_else(|| {
    format!("Failed to resolve parent directory for '{}'", path.display())
  })?;
  let default_content = include_str!("../default.toml");
  std::fs::create_dir_all(&dir_path).map_err(|e| {
    format!("Could not create '{}' directory: {}", dir_path.display(), e)
  })?;
  std::fs::write(path, default_content)
    .map_err(|e| format!("Could not write to '{}': {}", path.display(), e))
}

// Code
// ----

fn load_code(file: FileInput, encoded: bool) -> Result<Vec<Statement>, String> {
  let code = file.read_to_string()?;
  handle_code(&code, encoded)
}

fn handle_code(code: &str, encoded: bool) -> Result<Vec<Statement>, String> {
  if encoded {
    statments_from_hex_seq(code)
  } else {
    parse_code(code)
  }
}

fn parse_code(code: &str) -> Result<Vec<hvm::Statement>, String> {
  let statements = hvm::read_statements(code);
  match statements {
    Ok((code, statements)) => {
      if code.is_empty() {
        Ok(statements)
      } else {
        Err(format!("Your code was not parsed entirely: {}", code))
      }
    }
    Err(hvm::ParseErr { erro, .. }) => Err(erro),
  }
}

fn statments_from_hex_seq(txt: &str) -> Result<Vec<Statement>, String> {
  txt
    .trim()
    .split(|c: char| c.is_whitespace())
    .map(statement_from_hex)
    .collect()
}

fn statement_from_hex(hex: &str) -> Result<Statement, String> {
  let bytes = hex::decode(hex)
    .map_err(|err| format!("Invalid hexadecimal '{}': {}", hex, err))?;
  deserialized_statement(&bytes_to_bitvec(&bytes))
    .ok_or(format!("Failed to deserialize '{}'", hex))
}

fn arg_from_file_or_stdin<T: ArgumentFrom<String>>(
  file: FileInput,
) -> Result<T, String> {
  match file {
    FileInput::Path { path } => {
      // read from file
      let content = std::fs::read_to_string(&path).map_err(|err| {
        format!("Cannot read from '{:?}' file: {}", path, err)
      })?;
      T::arg_from(content)
    }
    FileInput::Stdin => {
      // read from stdin
      let mut input = String::new();
      match std::io::stdin().read_line(&mut input) {
        Ok(_) => T::arg_from(input.trim().to_string()),
        Err(err) => Err(format!("Could not read from stdin: {}", err)),
      }
    }
  }
}

// Auxiliar traits and types
// =========================

// FileInput
// ---------

/// Represents input from a file or stdin.
#[derive(Debug)]
pub enum FileInput {
  Stdin,
  Path { path: PathBuf },
}

impl From<PathBuf> for FileInput {
  fn from(path: PathBuf) -> Self {
    FileInput::Path { path }
  }
}

impl FromStr for FileInput {
  type Err = std::convert::Infallible;
  fn from_str(txt: &str) -> Result<Self, Self::Err> {
    let val = if txt == "-" {
      Self::Stdin
    } else {
      let path = txt.into();
      Self::Path { path }
    };
    Ok(val)
  }
}

impl fmt::Display for FileInput {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Self::Path { path } => write!(f, "{}", path.display()),
      Self::Stdin => write!(f, "<stdin>"),
    }
  }
}

// TODO: alternative that do not read the whole file immediately
impl FileInput {
  fn read_to_string(&self) -> Result<String, String> {
    match self {
      FileInput::Path { path } => {
        // read from file
        std::fs::read_to_string(&path)
          .map_err(|e| format!("Cannot read from '{:?}' file: {}", path, e))
      }
      FileInput::Stdin => {
        // read from stdin
        let mut buff = String::new();
        std::io::stdin()
          .read_to_string(&mut buff)
          .map_err(|e| format!("Could not read from stdin: {}", e))?;
        Ok(buff)
      }
    }
  }
}

// ArgumentFrom
// ------------

/// A trait to convert from anything to a type T.
/// It is equal to standard From trait, but
/// it has the From<String> for Vec<String> implementation.
/// As like From, the conversion must be perfect.
pub trait ArgumentFrom<T>: Sized {
  fn arg_from(value: T) -> Result<Self, String>;
}

impl ArgumentFrom<String> for String {
  fn arg_from(t: String) -> Result<Self, String> {
    Ok(t)
  }
}

impl ArgumentFrom<String> for Vec<String> {
  fn arg_from(t: String) -> Result<Self, String> {
    Ok(t.split(',').map(|x| x.to_string()).collect())
  }
}

impl ArgumentFrom<String> for bool {
  fn arg_from(t: String) -> Result<Self, String> {
    if t == "true" {
      Ok(true)
    } else if t == "false" {
      Ok(false)
    } else {
      Err(format!("Invalid boolean value: {}", t))
    }
  }
}

impl ArgumentFrom<String> for PathBuf {
  fn arg_from(t: String) -> Result<Self, String> {
    if let Some(path) = t.strip_prefix("~/") {
      let home_dir =
        dirs::home_dir().ok_or("Could not find $HOME directory.")?;
      Ok(home_dir.join(path))
    } else {
      PathBuf::from_str(&t).map_err(|_| format!("Invalid path: {}", t))
    }
  }
}

impl ArgumentFrom<toml::Value> for PathBuf {
  fn arg_from(value: toml::Value) -> Result<Self, String> {
    let t: String =
      value.try_into().map_err(|_| "Could not convert value to PahtBuf")?;
    PathBuf::arg_from(t)
  }
}

impl ArgumentFrom<toml::Value> for String {
  fn arg_from(t: toml::Value) -> Result<Self, String> {
    t.try_into().map_err(|_| "Could not convert value into String".to_string())
  }
}

impl ArgumentFrom<toml::Value> for Vec<String> {
  fn arg_from(t: toml::Value) -> Result<Self, String> {
    t.try_into().map_err(|_| "Could not convert value into array".to_string())
  }
}

impl ArgumentFrom<toml::Value> for bool {
  fn arg_from(t: toml::Value) -> Result<Self, String> {
    t.as_bool().ok_or(format!("Invalid boolean value: {}", t))
  }
}
