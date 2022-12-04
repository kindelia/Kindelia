mod cli;
mod config;
mod files;
mod genesis;
mod util;

use anyhow::{anyhow, Context};
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
use kindelia_core::config::{
  ApiConfig, MineConfig, NodeConfig, UiConfig, WsConfig,
};
use kindelia_core::net::{Address, ProtoComm};
use kindelia_core::node::{
  spawn_miner, Node, Transaction, MAX_TRANSACTION_SIZE,
};
use kindelia_core::persistence::{
  get_ordered_blocks_path, SimpleFileStorage, BLOCKS_DIR,
};
use kindelia_core::util::bytes_to_bitvec;
use kindelia_core::{events, net, runtime};
use kindelia_lang::{ast, parser};
use util::{
  bytes_to_u128, flag_to_option, handle_config_file, run_async_blocking,
};

use crate::cli::{GetStatsKind, NodeCleanBlocksCommand, NodeCleanCommand};
use crate::genesis::{genesis_code, init_genesis};
use crate::util::init_config_file;

fn main() -> anyhow::Result<()> {
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
      .build()?
      .resolve($cli, None)?
  };

  // No default value
  (env = $env:expr, no_default_fn = $default:expr, cli_val = $cli:expr $(,)*) => {
    ConfigSettingsBuilder::default()
      .env($env)
      .prop($prop)
      .default_value($default)
      .build()?
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
      .build()?
      .resolve($cli, $cfg)?
  };

  // No default value
  (env = $env:expr, prop = $prop:expr, no_default = $default:expr, cli_val = $cli:expr, cfg = $cfg:expr $(,)*) => {
    ConfigSettingsBuilder::default()
      .env($env)
      .prop($prop)
      .default_value(|| Err($default))
      .build()?
      .resolve($cli, $cfg)?
  };
}

// TODO: create own error with `thiserror`
// TODO: separate file in `mod`'s?
/// Parse CLI arguments and run
pub fn run_cli() -> anyhow::Result<()> {
  let parsed = Cli::parse();
  let default_base_path = || {
    let home_dir = dirs::home_dir().context("Could not find $HOME path")?;
    Ok::<_, anyhow::Error>(home_dir.join(".kindelia"))
  };
  let default_node_data_path =
    || Ok::<_, anyhow::Error>(default_base_path()?.join("state"));
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
    CliCommand::Test { file, sudo, genesis } => {
      let genesis_code = match genesis {
        Some(p) => std::fs::read_to_string(&p).context(anyhow!(
          "reading user-provided genesis prelude in {}",
          p.display()
        ))?,
        None => {
          include_str!("../../kindelia_core/genesis-tests.kdl").to_string()
        }
      };
      let code = file
        .read_to_string()
        .context(anyhow!("reading user-provided code in {}", file))?;

      test_code(&genesis_code, &code, sudo);
      Ok(())
    }
    CliCommand::Check { file, encoded, command } => {
      let code = file.read_to_string()?;
      let stmts = if encoded {
        statements_from_hex_seq(&code)?
      } else {
        parser::parse_code(&code).map_err(|e| anyhow!(e))?
      };
      match command {
        cli::CheckCommand::Transaction => {
          for ref stmt in stmts {
            let transaction: Transaction = stmt.try_into()?;

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
      serialize_code(&code)
    }
    CliCommand::Deserialize { file } => {
      let code: String = file.read_to_string()?;
      deserialize_code(&code)
    }
    CliCommand::Unserialize { stmt } => deserialize_code(&stmt),
    CliCommand::Sign { file, secret_file, encoded, encoded_output } => {
      let skey: String = arg_from_file_or_stdin(secret_file.into())?;
      let skey = skey.trim();
      let skey =
        hex::decode(skey).context("Secret key should be valid hex string")?;
      let skey: [u8; 32] = skey
        .try_into()
        .map_err(|_| anyhow!("Secret key should have exactly 64 bytes"))?;
      let code = load_code(file, encoded)?;
      let statement = match &code[..] {
        [stmt] => sign_code(stmt, &skey),
        _ => Err(anyhow!("Input file should contain exactly one statement")),
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
      let f = |client: ApiClient, stmts| async move {
        client.run_code(stmts).await.map_err(|e| anyhow!(e))
      };
      let stmts = if encoded {
        statements_from_hex_seq(&code)?
      } else {
        parser::parse_code(&code).map_err(|e| anyhow!(e))?
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
        parser::parse_code(&code).map_err(|e| anyhow!(e))?
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
      init_config_file(&path).map_err(|e| anyhow!(e))?;
      init_genesis(&default_base_path()?.join("genesis"))?;
      Ok(())
    }
    CliCommand::Node { command, data_dir, network_id } => {
      let config = handle_config_file(&config_path).map_err(|e| anyhow!(e))?;
      let config = Some(&config);

      let network_id = resolve_cfg!(
        env = "KINDELIA_NETWORK_ID",
        prop = "node.network.network_id".to_string(),
        no_default = anyhow!("Missing `network_id` parameter."),
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
        NodeCommand::Clean { command } => {
          clean(&data_path, command).context("Could not clean kindelia's data")
        }
        NodeCommand::Start { initial_peers, mine, json } => {
          // TODO: refactor config resolution out of command handling (how?)

          // Get arguments from cli, env or config

          let initial_peers = resolve_cfg!(
            env = "KINDELIA_NODE_INITIAL_PEERS",
            prop = format!("node.network.{:#02X}.initial_peers", network_id),
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
            .build()?
            .resolve_from_file_opt(config)?;

          let api_config = ConfigSettingsBuilder::default()
            .prop("node.api".to_string())
            .default_value(|| Ok(ApiConfig::default()))
            .build()?
            .resolve_from_file_only(config)?;

          // TODO: nest on `node.api`
          let ws_config = ConfigSettingsBuilder::default()
            .prop("node.ws".to_string())
            .default_value(|| Ok(WsConfig::default()))
            .build()?
            .resolve_from_file_only(config)?;

          // Start
          let node_comm =
            init_socket().context("Could not open a UDP socket")?;
          let initial_peers = initial_peers
            .into_iter()
            .map(|x| net::parse_address(&x).context("parsing peer addr"))
            .collect::<Result<Vec<_>, anyhow::Error>>()?;

          let node_cfg = NodeConfig {
            network_id,
            data_path,
            mining: MineConfig { enabled: mine, slow_mining },
            ui: Some(UiConfig {
              json,
              tags: vec![events::NodeEventDiscriminant::Heartbeat],
            }),
            ws: Some(ws_config),
          };

          let api_config = Some(api_config);
          start_node(node_cfg, api_config, node_comm, initial_peers)
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
        let data = data.context("Invalid hex string")?;
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
) -> anyhow::Result<T>
where
  F: FnOnce(ApiClient, Vec<HexStatement>) -> P,
  P: Future<Output = anyhow::Result<T>>,
{
  let stmts: Vec<HexStatement> = stmts.into_iter().map(|s| s.into()).collect();
  let client = ApiClient::new(api_url, None)?;
  run_async_blocking(f(client, stmts))
}

fn print_json_else<T: Serialize, F: Fn(T)>(
  json: bool,
  printable: T,
  when_not_json: F,
) -> anyhow::Result<()> {
  if json {
    println!("{}", serde_json::to_string_pretty(&printable)?);
  } else {
    when_not_json(printable)
  }
  Ok(())
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
) -> anyhow::Result<()> {
  let client = ApiClient::new(host_url, None)?;
  match kind {
    GetKind::BlockHash { index } => {
      let block_hash =
        client.get_block_hash(index).await.map_err(|e| anyhow!(e))?;
      println!("{}", block_hash);
      Ok(())
    }
    GetKind::Block { hash } => {
      let hash = Hash::try_from(hash.as_str()).map_err(|e| anyhow!(e))?;
      let block = client.get_block(hash).await.map_err(|e| anyhow!(e))?;
      println!("{:#?}", block);
      Ok(())
    }
    GetKind::Ctr { name, stat } => {
      let ctr_info =
        client.get_constructor(name).await.map_err(|e| anyhow!(e))?;
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
        let func_info =
          client.get_function(name).await.map_err(|e| anyhow!(e))?;
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
        })
      }
      GetFunKind::State => {
        let state =
          client.get_function_state(name).await.map_err(|e| anyhow!(e))?;
        print_json_else(json, &state, |state| println!("{}", state))
      }
      GetFunKind::Slots => todo!(),
    },
    GetKind::Reg { name, stat } => {
      let reg_info =
        client.get_reg_info(&name).await.map_err(|e| anyhow!(e))?;
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
      let stats = client.get_stats().await.map_err(|e| anyhow!(e))?;
      match stat_kind {
        None => {
          print_json_else(json, &stats, |stats| println!("{:#?}", stats))?;
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
              })?;
            }
            GetStatsKind::Space { limit_stat: Some(limit_stat) } => {
              let stat = limit_stat.get_field(stats.space);
              println!("{}", stat)
            }
            GetStatsKind::Space { limit_stat: None } => {
              print_json_else(json, &stats.space, |stats| {
                println!("{:#?}", stats)
              })?;
            }
          };
        }
      };
      Ok(())
    }
    GetKind::Peers { all } => {
      let peers = client.get_peers::<NC>(all).await.map_err(|e| anyhow!(e))?;
      for peer in peers {
        println!("{}", peer.address)
      }
      Ok(())
    }
  }
}

// Code
// ====

pub fn serialize_code(code: &str) -> anyhow::Result<()> {
  let statements = parser::parse_statements(code)?.1;
  for statement in statements {
    println!("{}", hex::encode(statement.proto_serialized().to_bytes()));
  }
  Ok(())
}

pub fn deserialize_code(content: &str) -> anyhow::Result<()> {
  let statements = statements_from_hex_seq(content)?;
  for statement in statements {
    println!("{}", statement)
  }
  Ok(())
}

pub fn sign_code(
  statement: &ast::Statement,
  skey: &[u8; 32],
) -> anyhow::Result<ast::Statement> {
  let user = crypto::Account::from_private_key(skey);
  let hash = runtime::hash_statement(statement);
  let sign = user.sign(&hash);
  match statement {
    ast::Statement::Fun { sign, .. }
    | ast::Statement::Ctr { sign, .. }
    | ast::Statement::Run { sign, .. }
    | ast::Statement::Reg { sign, .. } => {
      if sign.is_some() {
        return Err(anyhow!("Statement already has a signature."));
      }
    }
  };
  let stat = ast::set_sign(statement, sign);
  Ok(stat)
}

fn load_code(
  file: FileInput,
  encoded: bool,
) -> anyhow::Result<Vec<ast::Statement>> {
  let code = file.read_to_string()?;
  handle_code(&code, encoded)
}

fn handle_code(
  code: &str,
  encoded: bool,
) -> anyhow::Result<Vec<ast::Statement>> {
  if encoded {
    statements_from_hex_seq(code)
  } else {
    parser::parse_code(code).map_err(|e| anyhow!(e))
  }
}

fn statements_from_hex_seq(txt: &str) -> anyhow::Result<Vec<ast::Statement>> {
  txt
    .trim()
    .split(|c: char| c.is_whitespace())
    .map(statement_from_hex)
    .collect()
}

fn statement_from_hex(hex: &str) -> anyhow::Result<ast::Statement> {
  let bytes =
    hex::decode(hex).context(anyhow!("Invalid hexadecimal '{}'", hex))?;
  ast::Statement::proto_deserialized(&bytes_to_bitvec(&bytes))
    .context(anyhow!("Failed to deserialize '{}'", hex))
}
pub fn publish_code(
  api_url: &str,
  stmts: Vec<ast::Statement>,
  hosts: Vec<SocketAddr>,
) -> anyhow::Result<()> {
  // setup tokio runtime and unordered joinset (tasks).
  let runtime = tokio::runtime::Runtime::new()?;
  let mut tasks = tokio::task::JoinSet::new();

  let client = ApiClient::new(api_url, None)?;

  let peer_urls: Vec<String> = if hosts.is_empty() {
    // obtain list of active peers known to "our" node.
    let prom = async move {
      client.get_peers::<NC>(false).await.map_err(|e| anyhow!(e))
    };
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
    let client = ApiClient::new(peer_url, None)?;
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
) -> anyhow::Result<()> {
  while let Some(_res) = tasks.join_next().await {}
  Ok(())
}

pub fn test_code(genesis_code: &str, code: &str, sudo: bool) {
  let genesis_stmts =
    parser::parse_code(genesis_code).expect("Genesis code parses");

  runtime::test_statements_from_code(&genesis_stmts, code, sudo);
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
fn clean(data_path: &Path, command: NodeCleanCommand) -> anyhow::Result<()> {
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
  ) -> anyhow::Result<()> {
    let entries = get_ordered_blocks_path(data_path)?;
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
  ) -> anyhow::Result<()> {
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

// TODO: refactor return value
#[allow(clippy::type_complexity)]
pub fn spawn_event_handlers<A: net::ProtoAddr + 'static>(
  ws_config: kindelia_core::config::WsConfig,
  ui_config: Option<UiConfig>,
  addr: A,
) -> anyhow::Result<(
  std::sync::mpsc::Sender<(events::NodeEventType, u128)>,
  tokio::sync::broadcast::Sender<events::NodeEventType>,
  Vec<std::thread::JoinHandle<()>>,
)> {
  let (event_tx, event_rx) =
    std::sync::mpsc::channel::<(events::NodeEventType, u128)>();
  let (ws_tx, _ws_rx) = tokio::sync::broadcast::channel(ws_config.buffer_size);

  let ws_tx1 = ws_tx.clone();

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
            match serde_json::to_string(&event) {
              Ok(s) => println!("{}", s),
              Err(e) => eprintln!("json error: {}", e),
            }
          } else {
            println!("{}", event);
          }
        }
      }
    }
  });

  Ok((event_tx, ws_tx1, vec![thread_2]))
}

// TODO: I don't know why 'static is needed here or why it works
pub fn start_node<C: ProtoComm + 'static>(
  node_config: NodeConfig,
  api_config: Option<ApiConfig>,
  comm: C,
  initial_peers: Vec<C::Address>,
) -> anyhow::Result<()> {
  eprintln!("Starting Kindelia node...");
  eprintln!("Store path: {:?}", node_config.data_path);
  eprintln!("Network ID: {:#X}", node_config.network_id);

  let addr = comm.get_addr()?;

  // Threads
  let mut threads = vec![];

  // Events
  #[cfg(feature = "events")]
  let (event_tx, ws_tx) = {
    let (event_tx, ws_tx, event_thrds) = spawn_event_handlers(
      node_config.ws.unwrap_or_default(),
      node_config.ui,
      addr,
    )?;
    threads.extend(event_thrds);
    (event_tx, ws_tx)
  };

  // Mining
  let (miner_comm, miner_thrds) =
    spawn_miner(node_config.mining, Some(event_tx.clone()));
  threads.extend(miner_thrds.into_iter());

  // File writter
  let file_writter = SimpleFileStorage::new(node_config.data_path.clone())?;

  let genesis_stmts = parser::parse_code(
    &genesis_code(node_config.network_id).expect("Genesis code loads"),
  )
  .expect("Genesis code parses");

  // Node state object
  let (node_query_sender, node) = Node::new(
    node_config.data_path,
    node_config.network_id,
    addr,
    &genesis_stmts,
    initial_peers,
    comm,
    miner_comm,
    file_writter,
    #[cfg(feature = "events")]
    Some(event_tx),
  );

  // WebSocket API router
  let ws_router = kindelia_ws::ws_router::<
    events::NodeEventType,
    events::NodeEventDiscriminant,
  >(ws_tx);

  // Spawns the API thread
  if let Some(api_config) = api_config {
    let api_thread = std::thread::spawn(move || {
      let http_api_task =
        kindelia_server::api_serve(node_query_sender, api_config, ws_router);
      let runtime = tokio::runtime::Runtime::new().unwrap();
      runtime.block_on(http_api_task);
    });
    threads.push(api_thread);
  }

  // Spawns the node thread
  let node_thread = std::thread::spawn(move || {
    node.main();
  });
  threads.insert(0, node_thread);

  // Joins all threads and checks for panics.
  for thread in threads {
    match thread.join() {
      Ok(_) => {
        // For now at least, child threads are expected to handle their
        // own errors, eg by logging.  We do not propagate them up.
        // For a way to do it, see:
        //   https://stackoverflow.com/a/62823352/10087197
      }
      Err(e) => {
        // When join() returns an error, that means a child thread panicked.
        let msg = {
          if let Some(s) = e.downcast_ref::<&str>() {
            format!("Child thread panicked with message:\n{}", s)
          } else if let Some(s) = e.downcast_ref::<String>() {
            format!("Child thread panicked with message:\n{}", s)
          } else {
            format!("Child thread panicked!\n{:?}", e)
          }
        };

        // We return the panic error to propagate upwards so that
        // panic in any thread will result in process termination
        // as per rust default behavior.
        // Alternatively we could just log the error and keep going
        // without the thread that panicked, or even restart it.
        return Err(anyhow!(msg));
      }
    }
  }

  Ok(())
}

// Shell completion
// ================

// prints completions for a given shell, eg bash.
fn print_shell_completions(shell: Shell) -> anyhow::Result<()> {
  // obtain name of present executable
  let exec_name = std::env::current_exe()
    .context("getting current executable")?
    .file_name()
    .context("getting executable file name")?
    .to_str()
    .context("decoding executable name as utf8")?
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
