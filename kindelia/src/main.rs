mod cli;
mod config;
mod files;
mod util;

use std::future::Future;
use std::net::{SocketAddr, UdpSocket};
use std::path::Path;

use clap::{CommandFactory, Parser};
use clap_complete::Shell;
use serde::Serialize;

use cli::{
  Cli, CliCommand, GetCtrKind, GetFunKind, GetKind, GetRegKind, NodeCommand,
  UtilCommand,
};
use config::{arg_from_file_or_stdin, ConfigSettingsBuilder};

use files::FileInput;
use kindelia_client::ApiClient;
use kindelia_common::{crypto, Name};
use kindelia_core::api::{Hash, HexStatement};
use kindelia_core::bits::ProtoSerialize;
use kindelia_core::config::{ApiConfig, MineConfig, NodeConfig, UiConfig};
use kindelia_core::net::{Address, ProtoComm};
use kindelia_core::node::{
  spawn_miner, Node, Transaction, TransactionError, MAX_TRANSACTION_SIZE,
};
use kindelia_core::persistence::{
  get_ordered_blocks_path, SimpleFileStorage, BLOCKS_DIR,
};
use kindelia_core::util::bytes_to_bitvec;
use kindelia_core::{events, hvm, net};
use kindelia_lang::{ast, parser};
use util::{
  bytes_to_u128, flag_to_option, handle_config_file, run_async_blocking,
};

use crate::cli::{GetStatsKind, NodeCleanBlocksCommand, NodeCleanCommand};
use crate::util::init_config_file;

fn main() -> Result<(), String> {
  run_cli()
}

// Macros
// ======

macro_rules! resolve_cfg {
  // Resolve config value with no file
  // ---------------------------------

  // With default value
  (env = $env:expr, default_fn = $default:expr, cli_val = $cli:expr $(,)*) => {
    ConfigSettingsBuilder::default()
      .env($env)
      .default_value($default)
      .build()
      .unwrap()
      .resolve($cli, None)?
  };

  // No default value
  (env = $env:expr, no_default_fn = $default:expr, cli_val = $cli:expr $(,)*) => {
    ConfigSettingsBuilder::default()
      .env($env)
      .prop($prop)
      .default_value($default)
      .build()
      .unwrap()
      .resolve($cli, None)?
  };

  // Resolve config value with file
  // ------------------------------

  // With default value
  (env = $env:expr, prop = $prop:expr, default = $default:expr, cli_val = $cli:expr, cfg = $cfg:expr $(,)*) => {
    ConfigSettingsBuilder::default()
      .env($env)
      .prop($prop)
      .default_value(|| Ok($default))
      .build()
      .unwrap()
      .resolve($cli, $cfg)?
  };

  // No default value
  (env = $env:expr, prop = $prop:expr, no_default = $default:expr, cli_val = $cli:expr, cfg = $cfg:expr $(,)*) => {
    ConfigSettingsBuilder::default()
      .env($env)
      .prop($prop)
      .default_value(|| Err($default))
      .build()
      .unwrap()
      .resolve($cli, $cfg)?
  };
}

// TODO: create own error with `thiserror`
// TODO: separate file in `mod`'s?
/// Parse CLI arguments and run
pub fn run_cli() -> Result<(), String> {
  let parsed = Cli::parse();
  let default_base_path = || {
    let home_dir = dirs::home_dir().ok_or("Could not find $HOME path")?;
    Ok::<_, String>(home_dir.join(".kindelia"))
  };
  let default_node_data_path =
    || Ok::<_, String>(default_base_path()?.join("state"));
  let default_config_path = || Ok(default_base_path()?.join("kindelia.toml"));
  let default_api_url = || Ok("http://localhost:8000".to_string());

  // Resolve path where to load config file from
  let config_path = resolve_cfg!(
    env = "KINDELIA_CONFIG",
    default_fn = default_config_path,
    cli_val = parsed.config,
  );

  let api_url = resolve_cfg!(
    env = "KINDELIA_API_URL",
    default_fn = default_api_url,
    cli_val = parsed.api,
  );

  match parsed.command {
    CliCommand::Test { file, sudo } => {
      let code: String = file.read_to_string()?;
      test_code(&code, sudo);
      Ok(())
    }
    CliCommand::Check { file, encoded, command } => {
      let code = file.read_to_string()?;
      let stmts = if encoded {
        statements_from_hex_seq(&code)?
      } else {
        parser::parse_code(&code)?
      };
      match command {
        cli::CheckCommand::Transaction => {
          for ref stmt in stmts {
            let transaction: Transaction = stmt
              .try_into()
              .map_err(|err: TransactionError| err.to_string())?;

            // size printing
            {
              let size = transaction.len();
              let percent = size as f32 / MAX_TRANSACTION_SIZE as f32 * 100.;
              let size_message = {
                if size > MAX_TRANSACTION_SIZE {
                  "❌ Doesn't fit in one block"
                } else {
                  "✅"
                }
              };
              println!("Size: {} ({:.2}%) {}", size, percent, size_message);
            }
            // println!("Name: {}") // TODO
            // header printing
            println!("{}\n", ast::view_statement_header(stmt));
          }
        }
      };
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
    CliCommand::Unserialize { stmt } => deserialize_code(&stmt),
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
        println!("{}", hex::encode(statement.proto_serialized().to_bytes()));
      } else {
        println!("{}", statement);
      };
      Ok(())
    }
    CliCommand::RunRemote { file, encoded } => {
      // TODO: client timeout
      let code = file.read_to_string()?;
      let f =
        |client: ApiClient, stmts| async move { client.run_code(stmts).await };
      let stmts = if encoded {
        statements_from_hex_seq(&code)?
      } else {
        parser::parse_code(&code)?
      };
      let results = run_on_remote(&api_url, stmts, f)?;
      for result in results {
        println!("{}", result);
      }
      Ok(())
    }
    CliCommand::Publish { file, encoded, hosts } => {
      let code = file.read_to_string()?;
      let stmts = if encoded {
        statements_from_hex_seq(&code)?
      } else {
        parser::parse_code(&code)?
      };
      publish_code(&api_url, stmts, hosts)
    }
    CliCommand::Post { stmt, hosts } => {
      let stmts = statements_from_hex_seq(&stmt)?;
      publish_code(&api_url, stmts, hosts)
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
    CliCommand::Node { command, data_dir, network_id } => {
      let config = handle_config_file(&config_path)?;
      let config = Some(&config);

      let network_id = resolve_cfg!(
        env = "KINDELIA_NETWORK_ID",
        prop = "node.network.network_id".to_string(),
        no_default = "Missing `network_id` parameter.".to_string(),
        cli_val = network_id,
        cfg = config,
      );

      let data_path = resolve_cfg!(
        env = "KINDELIA_NODE_DATA_DIR",
        prop = "node.data.dir".to_string(),
        default = default_node_data_path()?,
        cli_val = data_dir,
        cfg = config,
      )
      .join(format!("{:#02X}", network_id));

      match command {
        NodeCommand::Clean { command } => clean(&data_path, command)
          .map_err(|err| format!("Could not clean kindelia's data: {}", err)),
        NodeCommand::Start { initial_peers, mine, json } => {
          // TODO: refactor config resolution out of command handling (how?)

          // Get arguments from cli, env or config

          let initial_peers = resolve_cfg!(
            env = "KINDELIA_NODE_INITIAL_PEERS",
            prop = format!("node.networks.{:#02X}.initial_peers", network_id),
            default = vec![],
            cli_val = initial_peers,
            cfg = config,
          );

          let mine = resolve_cfg!(
            env = "KINDELIA_MINE",
            prop = "node.mining.enable".to_string(),
            default = false,
            cli_val = flag_to_option(mine),
            cfg = config,
          );

          let slow_mining = ConfigSettingsBuilder::default()
            .env("KINDELIA_SLOW_MINING")
            .prop("node.debug.slow_mining".to_string())
            .default_value(|| Ok(0))
            .build()
            .unwrap()
            .resolve_from_file_opt(config)?;

          let api_config = ConfigSettingsBuilder::default()
            .prop("node.api".to_string())
            .default_value(|| Ok(ApiConfig::default()))
            .build()
            .unwrap()
            .resolve_from_file_only(config)?;

          // Start
          let node_comm = init_socket().expect("Could not open a UDP socket");
          let initial_peers = initial_peers
            .iter()
            .map(|x| net::parse_address(x))
            .collect::<Vec<_>>();

          let node_cfg = NodeConfig {
            network_id,
            data_path,
            mining: MineConfig { enabled: mine, slow_mining },
            ui: Some(UiConfig {
              json,
              tags: vec![events::NodeEventDiscriminant::Heartbeat],
            }),
            ws: None, // TODO: load from config file
          };

          let api_config = Some(api_config);
          start_node(node_cfg, api_config, node_comm, initial_peers);

          Ok(())
        }
      }
    }
    CliCommand::Util { command } => match command {
      UtilCommand::DecodeName { file } => {
        let txt = file.read_to_string()?;
        let input = txt.trim();
        let data: Result<Vec<Vec<u8>>, _> = input
          .split(|c: char| c.is_whitespace())
          .map(|s| s.strip_prefix("0x").unwrap_or(s))
          .map(hex::decode)
          .collect();
        let data =
          data.map_err(|err| format!("Invalid hex string: {}", err))?;
        let nums = data.iter().map(|v| bytes_to_u128(v));
        for num in nums {
          if let Some(num) = num {
            if let Ok(name) = Name::try_from(num) {
              println!("{}", name);
              continue;
            }
          }
          println!();
        }
        Ok(())
      }
    },
    CliCommand::Completion { shell } => print_shell_completions(shell),
  }
}

// Client util
// ===========

fn run_on_remote<T, P, F>(
  api_url: &str,
  stmts: Vec<ast::Statement>,
  f: F,
) -> Result<T, String>
where
  F: FnOnce(ApiClient, Vec<HexStatement>) -> P,
  P: Future<Output = Result<T, String>>,
{
  let stmts: Vec<HexStatement> = stmts.into_iter().map(|s| s.into()).collect();
  let client = ApiClient::new(api_url, None).map_err(|e| e.to_string())?;
  run_async_blocking(f(client, stmts))
}

fn print_json_else<T: Serialize, F: Fn(T)>(
  json: bool,
  printable: T,
  when_not_json: F,
) {
  if json {
    println!("{}", serde_json::to_string_pretty(&printable).unwrap());
  } else {
    when_not_json(printable)
  }
}

// Client
// ======

/// This client is meant to talk with a node implementing UDP protocol
/// communication (the default).
type NC = UdpSocket;

pub async fn get_info(
  kind: GetKind,
  json: bool,
  host_url: &str,
) -> Result<(), String> {
  let client = ApiClient::new(host_url, None).map_err(|e| e.to_string())?;
  match kind {
    GetKind::BlockHash { index } => {
      let block_hash = client.get_block_hash(index).await?;
      println!("{}", block_hash);
      Ok(())
    }
    GetKind::Block { hash } => {
      let hash = Hash::try_from(hash.as_str())?;
      let block = client.get_block(hash).await?;
      println!("{:#?}", block);
      Ok(())
    }
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
        print_json_else(json, func_info, |func_info| {
          let func = func_info.func;
          let statement = ast::Statement::Fun {
            name,
            args: vec![Name::NONE],
            func,
            init: Some(ast::Term::var(Name::NONE)), // to show that we are actually not returning the initial state
            sign: None,
          };
          println!("{}", statement);
        });
        Ok(())
      }
      GetFunKind::State => {
        let state = client.get_function_state(name).await?;
        print_json_else(json, &state, |state| println!("{}", state));
        Ok(())
      }
      GetFunKind::Slots => todo!(),
    },
    GetKind::Reg { name, stat } => {
      let reg_info = client.get_reg_info(&name).await?;
      match stat {
        GetRegKind::Owner => {
          println!("{:x}", *(reg_info.ownr))
        }
        GetRegKind::List => {
          for name in reg_info.stmt {
            println!("{}", name)
          }
        }
      }
      Ok(())
    }
    GetKind::Stats { stat_kind } => {
      let stats = client.get_stats().await?;
      match stat_kind {
        None => {
          print_json_else(json, &stats, |stats| println!("{:#?}", stats));
        }
        Some(stat_kind) => {
          match stat_kind {
            GetStatsKind::Tick => println!("{}", stats.tick),
            GetStatsKind::FunCount => println!("{}", stats.fun_count),
            GetStatsKind::CtrCount => println!("{}", stats.ctr_count),
            GetStatsKind::RegCount => println!("{}", stats.reg_count),
            GetStatsKind::Mana { limit_stat: Some(limit_stat) } => {
              let stat = limit_stat.get_field(stats.mana);
              println!("{}", stat)
            }
            GetStatsKind::Mana { limit_stat: None } => {
              print_json_else(json, &stats.mana, |stats| {
                println!("{:#?}", stats)
              });
            }
            GetStatsKind::Space { limit_stat: Some(limit_stat) } => {
              let stat = limit_stat.get_field(stats.space);
              println!("{}", stat)
            }
            GetStatsKind::Space { limit_stat: None } => {
              print_json_else(json, &stats.space, |stats| {
                println!("{:#?}", stats)
              });
            }
          };
        }
      };
      Ok(())
    }
    GetKind::Peers { all } => {
      let peers = client.get_peers::<NC>(all).await?;
      for peer in peers {
        println!("{}", peer.address)
      }
      Ok(())
    }
  }
}

// Code
// ====

pub fn serialize_code(code: &str) {
  let statements =
    parser::parse_statements(code).map_err(|err| err.erro).unwrap().1;
  for statement in statements {
    println!("{}", hex::encode(statement.proto_serialized().to_bytes()));
  }
}

pub fn deserialize_code(content: &str) -> Result<(), String> {
  let statements = statements_from_hex_seq(content)?;
  for statement in statements {
    println!("{}", statement)
  }
  Ok(())
}

pub fn sign_code(
  statement: &ast::Statement,
  skey: &[u8; 32],
) -> Result<ast::Statement, String> {
  let user = crypto::Account::from_private_key(skey);
  let hash = hvm::hash_statement(statement);
  let sign = user.sign(&hash);
  match statement {
    ast::Statement::Fun { sign, .. }
    | ast::Statement::Ctr { sign, .. }
    | ast::Statement::Run { sign, .. }
    | ast::Statement::Reg { sign, .. } => {
      if sign.is_some() {
        return Err("Statement already has a signature.".to_string());
      }
    }
  };
  let stat = ast::set_sign(statement, sign);
  Ok(stat)
}

fn load_code(file: FileInput, encoded: bool) -> Result<Vec<ast::Statement>, String> {
  let code = file.read_to_string()?;
  handle_code(&code, encoded)
}

fn handle_code(code: &str, encoded: bool) -> Result<Vec<ast::Statement>, String> {
  if encoded {
    statements_from_hex_seq(code)
  } else {
    parser::parse_code(code)
  }
}

fn statements_from_hex_seq(txt: &str) -> Result<Vec<ast::Statement>, String> {
  txt
    .trim()
    .split(|c: char| c.is_whitespace())
    .map(statement_from_hex)
    .collect()
}

fn statement_from_hex(hex: &str) -> Result<ast::Statement, String> {
  let bytes = hex::decode(hex)
    .map_err(|err| format!("Invalid hexadecimal '{}': {}", hex, err))?;
  ast::Statement::proto_deserialized(&bytes_to_bitvec(&bytes))
    .ok_or(format!("Failed to deserialize '{}'", hex))
}
pub fn publish_code(
  api_url: &str,
  stmts: Vec<ast::Statement>,
  hosts: Vec<SocketAddr>,
) -> Result<(), String> {
  // setup tokio runtime and unordered joinset (tasks).
  let runtime = tokio::runtime::Runtime::new().map_err(|e| e.to_string())?;
  let mut tasks = tokio::task::JoinSet::new();

  let client = ApiClient::new(api_url, None).map_err(|e| e.to_string())?;

  let peer_urls: Vec<String> = if hosts.is_empty() {
    // obtain list of active peers known to "our" node.
    let prom = async move { client.get_peers::<NC>(false).await };
    let peers = runtime.block_on(prom)?;
    let mut urls: Vec<String> = peers
      .iter()
      .map(|p| match p.address {
        Address::IPv4 { val0, val1, val2, val3, port: _ } => {
          // strips the port, so port 80 is assumed/default.
          // note:  we just assume/guess that this host has
          //        a webservice running on port 80. (gross)
          //        this is a fragile and temporary hack until
          //        code is relayed properly by p2p nodes.
          format!("http://{}.{}.{}.{}", val0, val1, val2, val3)
        }
      })
      .collect();
    // add api_url if not present in peers list.
    if !urls.iter().any(|x| x == api_url) {
      urls.push(api_url.to_string());
    }
    urls
  } else {
    hosts.iter().map(|h| format!("http://{}", h)).collect()
  };

  let stmts_hex: Vec<HexStatement> =
    stmts.into_iter().map(|s| s.into()).collect();

  for peer_url in peer_urls.into_iter() {
    // these are move'd into the spawned task, so they must be
    // created for each iteration.
    let client = ApiClient::new(peer_url, None).map_err(|e| e.to_string())?;
    let stmts_hex = stmts_hex.clone();

    // spawn a new task for contacting each peer.  Because it is
    // spawn'd, the task should begin executing immediately.
    tasks.spawn_on(
      async move {
        let results = match client.publish_code(stmts_hex.clone()).await {
          Ok(r) => r,
          Err(e) => {
            println!("NOT PUBLISHED to {}. ({})", *client, e);
            return Err(e);
          }
        };
        for (i, result) in results.iter().enumerate() {
          print!("Transaction #{}: ", i);
          match result {
            Ok(_) => {
              println!("PUBLISHED to {} (tx added to mempool)", *client)
            }
            Err(err) => {
              println!("NOT PUBLISHED to {}: {}", *client, err)
            }
          }
        }
        Ok(())
      },
      runtime.handle(),
    );
  }
  // wait for all tasks to complete.
  runtime.block_on(join_all(tasks))
}

async fn join_all(
  mut tasks: tokio::task::JoinSet<Result<(), String>>,
) -> Result<(), String> {
  while let Some(_res) = tasks.join_next().await {}
  Ok(())
}

pub fn test_code(code: &str, sudo: bool) {
  hvm::test_statements_from_code(code, sudo);
}

fn init_socket() -> Option<UdpSocket> {
  let try_ports =
    [net::UDP_PORT, net::UDP_PORT + 1, net::UDP_PORT + 2, net::UDP_PORT + 3];
  for port in try_ports {
    if let Ok(socket) = UdpSocket::bind(&format!("0.0.0.0:{}", port)) {
      socket.set_nonblocking(true).ok();
      return Some(socket);
    }
  }
  None
}

// Clean
fn clean(
  data_path: &Path,
  command: NodeCleanCommand,
) -> Result<(), std::io::Error> {
  fn user_confirm(data_path: &Path) -> Result<bool, std::io::Error> {
    // warning
    println!(
      "WARNING! This will delete permanently the selected files present in '{}'...",
      data_path.display()
    );
    // confirmation
    println!("Do you want to continue? ['y' for YES / or else NO]");
    let mut answer = String::new();
    std::io::stdin().read_line(&mut answer)?;
    // only accept 'y' as positive answer, anything else will be ignored
    if answer.trim().to_lowercase() != "y" {
      println!("Canceling operation.");
      return Ok(false);
    }
    Ok(true)
  }

  fn exclude_n_files(
    data_path: &std::path::Path,
    n: usize,
  ) -> Result<(), std::io::Error> {
    let entries = get_ordered_blocks_path(data_path);
    let mut count = 0;
    for entry in entries.iter().rev() {
      if entry.1.is_file() {
        std::fs::remove_file(&entry.1)?
      }
      count += 1;
      if count >= n {
        break;
      }
    }
    Ok(())
  }

  fn clean_node(
    data_path: &Path,
    command: NodeCleanCommand,
  ) -> Result<(), std::io::Error> {
    match command {
      NodeCleanCommand::All => std::fs::remove_dir_all(data_path)?,
      NodeCleanCommand::Blocks { command } => {
        let data_path = data_path.join(BLOCKS_DIR);
        match command {
          NodeCleanBlocksCommand::All => std::fs::remove_dir_all(data_path)?,
          NodeCleanBlocksCommand::Half => {
            let dir = std::fs::read_dir(&data_path)?;
            let half = dir.count() / 2;
            exclude_n_files(&data_path, half)?
          }
          NodeCleanBlocksCommand::N { number_of_blocks } => {
            exclude_n_files(&data_path, number_of_blocks)?
          }
        }
      }
    }
    Ok(())
  }

  if user_confirm(data_path)? {
    clean_node(data_path, command)?
  }

  Ok(())
}

// Node main thread
// ================

// event threads
pub fn spawn_event_handlers<A: net::ProtoAddr + 'static>(
  ws_config: kindelia_core::config::WsConfig,
  ui_config: Option<UiConfig>,
  addr: A,
) -> (
  std::sync::mpsc::Sender<(events::NodeEventType, u128)>,
  Vec<std::thread::JoinHandle<()>>,
) {
  let (event_tx, event_rx) =
    std::sync::mpsc::channel::<(events::NodeEventType, u128)>();
  let (ws_tx, _ws_rx) = tokio::sync::broadcast::channel(ws_config.buffer_size);

  eprintln!("Events WS on port: {}", ws_config.port);
  let ws_tx1 = ws_tx.clone();
  let thread_1 = std::thread::spawn(move || {
    kindelia_ws::ws_loop::<events::NodeEventType, events::NodeEventDiscriminant>(
      ws_config.port,
      ws_tx1,
    );
  });

  let ws_tx2 = ws_tx;
  let thread_2 = std::thread::spawn(move || {
    while let Ok((event, time)) = event_rx.recv() {
      if ws_tx2.receiver_count() > 0 {
        if let Err(err) = ws_tx2.send(event.clone()) {
          eprintln!("Could not send event to websocket: {}", err);
        };
      }
      if let Some(ref ui_cfg) = &ui_config {
        if ui_cfg.tags.is_empty()
          || ui_cfg.tags.contains(&(event.clone()).into())
        {
          let event = events::NodeEvent { time, addr, event };
          if ui_cfg.json {
            println!("{}", serde_json::to_string(&event).unwrap());
          } else {
            println!("{}", event);
          }
        }
      }
    }
  });

  (event_tx, vec![thread_1, thread_2])
}

// TODO: I don't know why 'static is needed here or why it works
pub fn start_node<C: ProtoComm + 'static>(
  node_config: NodeConfig,
  api_config: Option<ApiConfig>,
  comm: C,
  initial_peers: Vec<C::Address>,
) {
  eprintln!("Starting Kindelia node...");
  eprintln!("Store path: {:?}", node_config.data_path);
  eprintln!("Network ID: {:#X}", node_config.network_id);

  // Threads
  let mut threads = vec![];

  // Events
  #[cfg(feature = "events")]
  let event_tx = {
    let addr = comm.get_addr();
    let (event_tx, event_thrds) = spawn_event_handlers(
      node_config.ws.unwrap_or_default(),
      node_config.ui,
      addr,
    );
    threads.extend(event_thrds);
    event_tx
  };

  // Mining
  let (miner_comm, miner_thrds) =
    spawn_miner(node_config.mining, Some(event_tx.clone()));
  threads.extend(miner_thrds.into_iter());

  // File writter
  let file_writter = SimpleFileStorage::new(node_config.data_path.clone());

  // Node state object
  let (node_query_sender, node) = Node::new(
    node_config.data_path,
    node_config.network_id,
    initial_peers,
    comm,
    miner_comm,
    file_writter,
    #[cfg(feature = "events")]
    Some(event_tx),
  );

  // Spawns the API thread
  if let Some(api_config) = api_config {
    let api_thread = std::thread::spawn(move || {
      kindelia_server::http_api_loop(node_query_sender, api_config);
    });
    threads.push(api_thread);
  }

  // Spawns the node thread
  let node_thread = std::thread::spawn(move || {
    node.main();
  });
  threads.insert(0, node_thread);

  // Joins all threads
  for thread in threads {
    thread.join().unwrap();
  }
}

// Shell completion
// ================

// prints completions for a given shell, eg bash.
fn print_shell_completions(shell: Shell) -> Result<(), String> {
  // obtain name of present executable
  let exec_name = std::env::current_exe()
    .map_err(|e| format!("Error getting current executable: {}", e))?
    .file_name()
    .ok_or_else(|| "Error getting executable file name".to_string())?
    .to_str()
    .ok_or_else(|| "Error decoding executable name as utf8".to_string())?
    .to_string();

  // Generates completions for <shell> and prints to stdout
  clap_complete::generator::generate(
    shell,
    &mut Cli::command(),
    exec_name,
    &mut std::io::stdout(),
  );

  Ok(())
}
