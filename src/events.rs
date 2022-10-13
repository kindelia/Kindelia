use futures_util::{SinkExt, StreamExt};
use primitive_types::U256;
use serde;
use std::convert::Infallible;
use tokio::{self, sync::broadcast};
use warp::ws::{Message, WebSocket};
use warp::{Filter, Rejection, Reply};

use crate::api::Hash;
use crate::config::{UiConfig, WsConfig};
use crate::net::{ProtoAddr, ProtoComm};
use crate::node::{Block, Peer, Node};

// TODO: rename and organize structs and constructors

#[derive(Debug, Clone, serde::Serialize)]
pub struct NodeEvent<A: ProtoAddr> {
  pub addr: A,
  pub event: NodeEventType,
}

#[derive(Debug, Clone, serde::Serialize)]
pub enum NodeEventType {
  AddBlock {
    block: BlockInfo,
    event: Box<AddBlockEvent>, // added `Box` because of lint warning
  },
  Mining {
    event: MiningEvent,
  },
  // HandleRequest,
  Peers {
    event: PeersEvent,
  },
  HandleMessage {
    event: HandleMessageEvent,
  },
  Heartbeat {
    peers: HeartbeatPeers,
    tip: HeartbeatTip,
    blocks: HeartbeatBlocks,
    runtime: HeartbeatRuntime,
  },
}

#[derive(Debug, Clone, serde::Serialize)]
pub struct BlockInfo {
  hash: Hash,
  height: u128,
  parent: Hash,
}

#[derive(Debug, Clone, serde::Serialize)]
pub enum AddBlockEvent {
  AlreadyIncluded,
  NotEnoughWork,
  Reorg {
    old_tip: BlockInfo,             // old network's tip
    common_block: BlockInfo,        // first common block in both timelines
    rollback: Option<RollbackInfo>, // if ocurred a rollback, store its info
  },
  Included {
    siblings: Vec<Hash>,
  },
  MissingParent {
    parent: Hash,
  },
  TooLate,
}

#[derive(Debug, Clone, serde::Serialize)]
pub struct RollbackInfo {
  runtime_tick: u128,
  common_tick: u128,
}

#[derive(Debug, Clone, serde::Serialize)]
pub enum MiningEvent {
  Success { block: Hash, target: Hash },
  Failure { target: Hash },
  AskMine { target: Hash },
  Stop,
}

#[derive(Debug, Clone, serde::Serialize)]
// The peers are represented as strings (addr display)
// to avoid type parameter
pub enum PeersEvent {
  SeePeer { addr: String, seen_at: u128, result: SeePeerResult },
  Timeout { addr: String, seen_at: u128 },
}

#[derive(Debug, Clone, serde::Serialize)]
pub enum SeePeerResult {
  NotSeenBefore,
  Activated,
  AlreadyActive { new_seen_at: u128 },
}

#[derive(Debug, Clone, serde::Serialize)]
pub enum HandleMessageEvent {
  NoticeTheseBlocks {
    magic: u64,
    gossip: bool,
    blocks: Vec<Hash>,
    peers: Vec<String>, // peer not used to avoid type parameter
  },
  GiveMeThatBlock {
    magic: u64,
    bhash: Hash,
  },
  PleaseMineThisTransaction {
    magic: u64,
    trans: Hash, // shoul we guard the data of transaction too?
  },
}

#[derive(Debug, Clone, serde::Serialize)]
pub struct HeartbeatPeers {
  pub num: usize,
}

#[derive(Debug, Clone, serde::Serialize)]
pub struct HeartbeatTip {
  pub height: u64,
  pub difficulty: u64,
}

#[derive(Debug, Clone, serde::Serialize)]
pub struct HeartbeatBlocks {
  pub missing: u64,
  pub pending: u64,
  pub included: usize,
}

#[derive(Debug, Clone, serde::Serialize)]
pub struct HeartbeatRuntime {
  pub mana: HeartbeatStatInfo,
  pub size: HeartbeatStatInfo,
}

#[derive(Debug, Clone, serde::Serialize)]
pub struct HeartbeatStatInfo {
  pub current: i64,
  pub limit: i64,
  pub available: i64,
}

// ========================================================
// Display

impl std::fmt::Display for HeartbeatPeers {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.write_fmt(format_args!("peers: {}", self.num))
  }
}

impl std::fmt::Display for HeartbeatTip {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.write_fmt(format_args!(
      "tip: {{ height: {} | difficulty: {} }}",
      self.height, self.difficulty
    ))
  }
}

impl std::fmt::Display for HeartbeatBlocks {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.write_fmt(format_args!(
      "blocks: {{ included: {} | missing: {} | pending: {} }}",
      self.included, self.missing, self.pending
    ))
  }
}

impl std::fmt::Display for HeartbeatRuntime {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    // Do not display mana stats right now as they are too verbose
    f.write_fmt(format_args!("runtime: {{ size: {} }}", self.size))
    // f.write_fmt(format_args!(
    //   "[runtime] [mana] {} | [size] {}",
    //   self.mana, self.size
    // ))
  }
}

impl std::fmt::Display for HeartbeatStatInfo {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.write_fmt(format_args!(
      "current: {} | limit: {} | available: {}",
      self.current, self.limit, self.available
    ))
  }
}

impl std::fmt::Display for HandleMessageEvent {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let message = match self {
      HandleMessageEvent::NoticeTheseBlocks {
        magic,
        gossip,
        blocks,
        peers,
      } => {
        let blocks =
          blocks.iter().map(|h| h.to_string()).collect::<Vec<_>>().join(", ");
        let peers = peers.join(", ");
        format!(
          "[notice_blocks] magic: {} | gossip: {} | blocks: {} | peers: {}",
          magic, gossip, blocks, peers
        )
      }
      HandleMessageEvent::GiveMeThatBlock { magic, bhash } => {
        format!("[give_me_that_block] magic: {} | block: {}", magic, bhash)
      }
      HandleMessageEvent::PleaseMineThisTransaction { magic, trans } => {
        format!("[mine_trans] magic: {} | trans: {}", magic, trans)
      }
    };
    f.write_fmt(format_args!("{}", message))
  }
}

impl std::fmt::Display for PeersEvent {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let formatted = match self {
      PeersEvent::SeePeer { addr, seen_at, result } => {
        let formatted_result = match result {
          SeePeerResult::NotSeenBefore => "[not_seen_before]".to_string(),
          SeePeerResult::Activated => "[activated]".to_string(),
          SeePeerResult::AlreadyActive { new_seen_at } => {
            format!("[activated] new_seen_at: {}", new_seen_at)
          }
        };
        format!(
          "[see_peer] addr: {} | seen_at: {} | result: {}",
          addr, seen_at, formatted_result
        )
      }
      PeersEvent::Timeout { addr, seen_at } => {
        format!("[timeout] addr: {} | seen_at: {}", addr, seen_at)
      }
    };
    f.write_fmt(format_args!("{}", formatted))
  }
}

impl std::fmt::Display for RollbackInfo {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.write_fmt(format_args!(
      "common_tick: {} | runtime_tick: {}",
      self.common_tick, self.runtime_tick
    ))
  }
}

impl std::fmt::Display for BlockInfo {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.write_fmt(format_args!("{}", self.hash))
  }
}

impl std::fmt::Display for NodeEventType {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let str_res = match self {
      NodeEventType::AddBlock { block, event } => match &**event {
        AddBlockEvent::AlreadyIncluded => {
          format!(
            "[add_block] [already_included] {} was alredy included",
            block
          )
        }
        AddBlockEvent::NotEnoughWork => {
          format!(
            "[add_block] [not_enough_work] {} didn't have enough work",
            block
          )
        }
        AddBlockEvent::Reorg { old_tip, rollback, .. } => {
          let rollback = if let Some(rollback) = rollback {
            format!("rollback: {}", rollback)
          } else {
            String::new()
          };
          format!("[add_block] [reorg] old_block {} was reorganized in favor of new_block {}; old height: {}; new height: {} {}", old_tip.hash, block, old_tip.height, block.height, rollback)
        }
        AddBlockEvent::MissingParent { parent } => {
          format!(
            "[add_block] [missing_parent] block {} has missing parent {}",
            block, parent
          )
        }
        AddBlockEvent::TooLate => {
          format!(
            "[add_block] [too_late] block {} was too late to be added",
            block
          )
        }
        AddBlockEvent::Included { .. } => {
          format!("[included] block {}", block)
        }
      },
      NodeEventType::Mining { event } => match event {
        MiningEvent::Success { block, target } => {
          format!(
            "[mining] [success] block {} was mined successfully with target {}",
            block, target
          )
        }
        MiningEvent::Failure { target } => {
          format!("[mining] [failure] failed in mining with target {}", target)
        }
        MiningEvent::AskMine { target } => {
          format!(
            "[mining] [ask_mine] asking miner to mine with target {}",
            target
          )
        }
        MiningEvent::Stop => "[mining] [stop] stopping mining".to_string(),
      },
      // NodeEvent::HandleRequest => {
      //   "[handle_request] something was requested in node api".to_string()
      // }
      NodeEventType::Peers { event } => {
        format!("[peers] {}", event)
      }
      NodeEventType::HandleMessage { event } => {
        format!("[handle_message] {}", event)
      }
      NodeEventType::Heartbeat { peers, tip, blocks, runtime } => {
        format!("[heartbeat] {} {} {} {}", peers, tip, blocks, runtime)
      }
    };

    f.write_fmt(format_args!("{}", str_res))
  }
}

impl<A: ProtoAddr> std::fmt::Display for NodeEvent<A> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.write_fmt(format_args!("[node] {} [event] {}", self.addr, self.event))
  }
}

// ========================================================
// Constructors

impl NodeEventType {
  // ADD BLOCK
  pub fn not_enough_work<C: ProtoComm>(node: &Node<C>, block: U256) -> Self {
    let height = node.height[&block];
    let block = &node.block[&block];

    NodeEventType::AddBlock {
      block: BlockInfo {
        hash: block.hash.into(),
        height,
        parent: block.prev.into(),
      },
      event: Box::new(AddBlockEvent::NotEnoughWork),
    }
  }
  pub fn included<C: ProtoComm>(node: &Node<C>, block: U256) -> Self {
    let height = node.height[&block];
    let block = &node.block[&block];

    let brothers =
      node.children[&block.prev].iter().map(|hash| (*hash).into()).collect();

    NodeEventType::AddBlock {
      block: BlockInfo {
        hash: block.hash.into(),
        height,
        parent: block.prev.into(),
      },
      event: Box::new(AddBlockEvent::Included { siblings: brothers }),
    }
  }
  pub fn reorg<C: ProtoComm>(
    node: &Node<C>,
    old_tip: U256,
    new_tip: U256,
    common: U256,
  ) -> Self {
    let old_height = node.height[&old_tip];
    let old_block = &node.block[&old_tip];

    let new_height = node.height[&new_tip];
    let new_block = &node.block[&new_tip];

    let common_height = node.height[&common];
    let common_block = &node.block[&common];

    let common_tick = node.height[&common];
    let runtime_tick = node.runtime.get_tick();

    let rollback = {
      if common_tick < runtime_tick {
        Some(RollbackInfo { common_tick, runtime_tick })
      } else {
        None
      }
    };

    NodeEventType::AddBlock {
      block: BlockInfo {
        hash: new_block.hash.into(),
        height: new_height,
        parent: new_block.prev.into(),
      },
      event: Box::new(AddBlockEvent::Reorg {
        old_tip: BlockInfo {
          hash: old_tip.into(),
          height: old_height,
          parent: old_block.prev.into(),
        },
        common_block: BlockInfo {
          hash: common.into(),
          height: common_height,
          parent: common_block.prev.into(),
        },
        rollback,
      }),
    }
  }
  pub fn already_included<C: ProtoComm>(node: &Node<C>, block: U256) -> Self {
    let height = node.height[&block];
    let block = &node.block[&block];

    NodeEventType::AddBlock {
      block: BlockInfo {
        hash: block.hash.into(),
        height,
        parent: block.prev.into(),
      },
      event: Box::new(AddBlockEvent::AlreadyIncluded),
    }
  }
  pub fn missing_parent<C: ProtoComm>(
    node: &Node<C>,
    block: U256,
    parent: U256,
  ) -> Self {
    let height = node.height[&block];
    let block = &node.block[&block];

    NodeEventType::AddBlock {
      block: BlockInfo {
        hash: block.hash.into(),
        height,
        parent: block.prev.into(),
      },
      event: Box::new(AddBlockEvent::MissingParent { parent: parent.into() }),
    }
  }
  pub fn too_late<C: ProtoComm>(node: &Node<C>, block: U256) -> Self {
    let height = node.height[&block];
    let block = &node.block[&block];

    NodeEventType::AddBlock {
      block: BlockInfo {
        hash: block.hash.into(),
        height,
        parent: block.prev.into(),
      },
      event: Box::new(AddBlockEvent::TooLate),
    }
  }

  // MINING
  pub fn mined(block: U256, target: U256) -> Self {
    NodeEventType::Mining {
      event: MiningEvent::Success {
        block: block.into(),
        target: target.into(),
      },
    }
  }
  pub fn failed_mined(target: U256) -> Self {
    NodeEventType::Mining {
      event: MiningEvent::Failure { target: target.into() },
    }
  }
  pub fn ask_mine(target: U256) -> Self {
    NodeEventType::Mining {
      event: MiningEvent::AskMine { target: target.into() },
    }
  }
  pub fn stopped() -> Self {
    NodeEventType::Mining { event: MiningEvent::Stop }
  }

  // PEERS
  pub fn timeout<A: ProtoAddr>(peer: &Peer<A>) -> Self {
    NodeEventType::Peers {
      event: PeersEvent::Timeout {
        addr: peer.address.to_string(),
        seen_at: peer.seen_at,
      },
    }
  }
  pub fn see_peer_activated<A: ProtoAddr>(peer: &Peer<A>) -> Self {
    NodeEventType::Peers {
      event: PeersEvent::SeePeer {
        addr: peer.address.to_string(),
        seen_at: peer.seen_at,
        result: SeePeerResult::Activated,
      },
    }
  }
  pub fn see_peer_not_seen<A: ProtoAddr>(peer: &Peer<A>) -> Self {
    NodeEventType::Peers {
      event: PeersEvent::SeePeer {
        addr: peer.address.to_string(),
        seen_at: peer.seen_at,
        result: SeePeerResult::NotSeenBefore,
      },
    }
  }
  pub fn see_peer_already_active<A: ProtoAddr>(
    peer: &Peer<A>,
    new_seen_at: u128,
  ) -> Self {
    NodeEventType::Peers {
      event: PeersEvent::SeePeer {
        addr: peer.address.to_string(),
        seen_at: peer.seen_at,
        result: SeePeerResult::AlreadyActive { new_seen_at },
      },
    }
  }

  // HANDLE REQUEST
  // pub fn handle_request() -> Self {
  //   NodeEvent::HandleRequest
  // }

  // HANDLE MESSAGE
  pub fn notice_blocks<A: ProtoAddr>(
    magic: u64,
    gossip: bool,
    blocks: &[Block],
    peers: &[Peer<A>],
  ) -> Self {
    let event = HandleMessageEvent::NoticeTheseBlocks {
      magic,
      gossip,
      blocks: blocks.iter().map(|b| b.hash.into()).collect(),
      peers: peers.iter().map(|p| format!("{}", p.address)).collect(),
    };
    NodeEventType::HandleMessage { event }
  }
  pub fn give_me_block(magic: u64, block: U256) -> Self {
    let event =
      HandleMessageEvent::GiveMeThatBlock { magic, bhash: block.into() };
    NodeEventType::HandleMessage { event }
  }
  pub fn mine_trans(magic: u64, trans: U256) -> Self {
    let event = HandleMessageEvent::PleaseMineThisTransaction {
      magic,
      trans: trans.into(),
    };
    NodeEventType::HandleMessage { event }
  }

  // UTILS
  pub fn get_tag(&self) -> String {
    match self {
      NodeEventType::AddBlock { .. } => "add_block".to_string(),
      NodeEventType::Mining { .. } => "mining".to_string(),
      NodeEventType::Peers { .. } => "peers".to_string(),
      NodeEventType::HandleMessage { .. } => "handle_message".to_string(),
      NodeEventType::Heartbeat { .. } => "heartbeat".to_string(),
    }
  }
}

#[macro_export]
macro_rules! heartbeat {
  (
    peers: { num: $peers_num:expr },
    tip: {
      height: $tip_height:expr,
      difficulty: $difficulty:expr,
    },
    blocks: {
      missing: $missing_count:expr,
      pending: $pending_count:expr,
      included: $included_count:expr,
    },
    runtime: {
      mana: {
        current: $mana_cur:expr,
        limit: $mana_lim:expr,
        available: $mana_avail:expr,
      },
      size: {
        current: $size_cur:expr,
        limit: $size_lim:expr,
        available: $size_avail:expr,
      }
    }
  ) => {
    NodeEventType::Heartbeat {
      peers: $crate::events::HeartbeatPeers { num: $peers_num },
      tip: $crate::events::HeartbeatTip {
        height: $tip_height,
        difficulty: $difficulty,
      },
      blocks: $crate::events::HeartbeatBlocks {
        missing: $missing_count,
        pending: $pending_count,
        included: $included_count,
      },
      runtime: $crate::events::HeartbeatRuntime {
        mana: $crate::events::HeartbeatStatInfo {
          current: $mana_cur,
          limit: $mana_lim,
          available: $mana_avail,
        },
        size: $crate::events::HeartbeatStatInfo {
          current: $size_cur,
          limit: $size_lim,
          available: $size_avail,
        },
      },
    }
  };
}

// =======================================================
// Events tasks

// type EventTask = Box<dyn FnMut(NodeEvent)>;

// fn pre_process_file(file_name: &str) -> std::fs::File {
//   let file_path = std::path::Path::new(&file_name);
//   if file_path.exists() {
//     std::fs::remove_file(&file_path).unwrap();
//   }
//   let mut file = std::fs::OpenOptions::new()
//     .write(true)
//     .create(true)
//     .open(&file_path)
//     .unwrap();
//   file
// }

// pub fn event_tasks<A: ProtoAddr>(
//   addr: &A,
//   ws_tx: broadcast::Sender<NodeEvent>,
// ) -> Vec<EventTask> {
// pre-process
//   let file_name = format!("log-{}.json", addr);
//   let file = pre_process_file(&file_name);

//   // tasks
//   let print = |event: NodeEvent| {
//     println!("{}", event);
//   };
//   let print = Box::new(print);

//   let ws_tx = ws_tx.clone();
//   let websocket_send = |event: NodeEvent| {
//     if ws_tx.receiver_count() > 0 {
//       if let Err(err) = ws_tx.send(event.clone()) {
//         eprintln!("Could not send event to websocket: {}", err);
//       };
//     }
//   };
//   let websocket_send = Box::new(websocket_send);

//   let file = |event: NodeEvent| {
//     let txt = format!("{},\n", serde_json::to_string(&event.clone()).unwrap());
//     // saves it in a file
//     std::fs::
//     file.write_all(txt.as_bytes()).unwrap();
//   };
//   let file = Box::new(file);

//   vec![print, websocket_send, file]
// }

// =======================================================
// Websocket server

#[derive(serde::Deserialize)]
pub struct QueryParams {
  pub tags: Option<String>,
}

pub struct Query {
  pub tags: Vec<String>,
}

pub fn ws_loop(port: u16, ws_tx: broadcast::Sender<NodeEventType>) {
  let runtime = tokio::runtime::Runtime::new().unwrap();
  runtime.block_on(async move {
    ws_server(port, ws_tx).await;
  });
}

async fn ws_server(port: u16, ws_tx: broadcast::Sender<NodeEventType>) {
  let ws_route = warp::ws()
    .and(with_rx(ws_tx.clone()))
    .and(warp::query::<QueryParams>().map(parse_query))
    .and_then(ws_handler);
  warp::serve(ws_route).run(([127, 0, 0, 1], port)).await;
}

fn parse_query(query: QueryParams) -> Query {
  let tags = match query.tags {
    Some(tags) => tags.split(',').map(str::to_string).collect(),
    None => vec![],
  };
  Query { tags }
}

fn with_rx(
  ws_tx: broadcast::Sender<NodeEventType>,
) -> impl Filter<Extract = (broadcast::Sender<NodeEventType>,), Error = Infallible>
     + Clone {
  warp::any().map(move || ws_tx.clone())
}

pub async fn ws_handler(
  ws: warp::ws::Ws,
  ws_tx: broadcast::Sender<NodeEventType>,
  query: Query,
) -> Result<impl Reply, Rejection> {
  Ok(ws.on_upgrade(move |socket| client_connection(socket, ws_tx, query.tags)))
}

pub async fn client_connection(
  ws: WebSocket,
  ws_tx: broadcast::Sender<NodeEventType>,
  tags: Vec<String>,
) {
  let (mut client_ws_sender, _) = ws.split();
  let mut ws_rx = ws_tx.subscribe();
  let mut count = 0;

  while let Ok(event) = ws_rx.recv().await {
    if tags.is_empty() || tags.contains(&event.get_tag()) {
      let json_stringfied = serde_json::to_string(&event).unwrap();
      if let Err(err) =
        client_ws_sender.send(Message::text(json_stringfied)).await
      {
        eprintln!("Could not send message through websocket: {}", err);
        count += 1;
      } else {
        count = 0;
      };
      // After 10 consecutive fails we close the connection
      if count == 10 {
        break;
      };
    }
  }

  eprintln!("Disconnected");
}

//

// TODO
pub fn spawn_event_handlers<A: ProtoAddr>(
  ws_config: WsConfig,
  ui_config: Option<UiConfig>,
  _addr: A
) -> (std::sync::mpsc::Sender<NodeEventType>, Vec<std::thread::JoinHandle<()>>) {
  let (event_tx, event_rx) = std::sync::mpsc::channel::<NodeEventType>();
  let (ws_tx, _ws_rx) = tokio::sync::broadcast::channel(ws_config.buffer_size);

  eprintln!("Events WS on port: {}", ws_config.port);
  let ws_tx1 = ws_tx.clone();
  let thread_1 = std::thread::spawn(move || {
    ws_loop(ws_config.port, ws_tx1);
  });

  let ws_tx2 = ws_tx;
  let thread_2 = std::thread::spawn(move || {
    while let Ok(event) = event_rx.recv() {
      if ws_tx2.receiver_count() > 0 {
        if let Err(err) = ws_tx2.send(event.clone()) {
          eprintln!("Could not send event to websocket: {}", err);
        };
      }
      // TODO: way to select which logs to print
      if let (Some(ref ui_cfg), NodeEventType::Heartbeat { .. }) =
        (&ui_config, &event)
      {
        if ui_cfg.json {
          println!("{}", serde_json::to_string(&event).unwrap());
        } else {
          println!("{}", event);
        }
      }
    }
  });

  (event_tx, vec![thread_1, thread_2])
}
