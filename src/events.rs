use futures_util::{SinkExt, StreamExt};
use primitive_types::U256;
use serde;
use std::convert::Infallible;
use tokio::{self, sync::broadcast};
use warp::{
  ws::{Message, WebSocket},
  Filter, Rejection, Reply,
};

use crate::{
  api::Hash,
  net::ProtoAddr,
  node::{Block, Peer},
};

#[derive(Debug, Clone, serde::Serialize)]
pub enum NodeEvent {
  AddBlock {
    block: Hash,
    event: AddBlockEvent,
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
pub enum AddBlockEvent {
  AlreadyIncluded,
  NotEnoughWork,
  Reorg { old: Hash, height: u128 },
  MissingParent { parent: Hash },
  TooLate,
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
  pub hashrate: u64,
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
    f.write_fmt(format_args!("[peers] {}", self.num))
  }
}

impl std::fmt::Display for HeartbeatTip {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.write_fmt(format_args!(
      "[tip] height: {} | diff: {} | hashrate: {}",
      self.height, self.difficulty, self.hashrate
    ))
  }
}

impl std::fmt::Display for HeartbeatBlocks {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.write_fmt(format_args!(
      "[tip] included: {} | missing: {} | pending: {}",
      self.included, self.missing, self.pending
    ))
  }
}

impl std::fmt::Display for HeartbeatRuntime {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.write_fmt(format_args!(
      "[runtime] [mana] {} | [size] {}",
      self.mana, self.size
    ))
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
        format!("[give_me_block] magic: {} | block: {}", magic, bhash)
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

impl std::fmt::Display for NodeEvent {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let str_res = match self {
      NodeEvent::AddBlock { block, event } => match event {
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
        AddBlockEvent::Reorg { old, height } => {
          format!("[add_block] [reorg] old_block {} was reorganized in favor of new_block {}; common height at {}", old, block, height)
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
      },
      NodeEvent::Mining { event } => match event {
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
      NodeEvent::Peers { event } => {
        format!("[peers] {}", event)
      }
      NodeEvent::HandleMessage { event } => {
        format!("[handle_message] {}", event)
      }
      NodeEvent::Heartbeat { peers, tip, blocks, runtime } => {
        format!("[heartbeat] {} {} {} {}", peers, tip, blocks, runtime)
      }
    };

    f.write_fmt(format_args!("{}", str_res))
  }
}

// ========================================================
// Constructors

impl NodeEvent {
  // ADD BLOCK
  pub fn not_enough_work(block: U256) -> Self {
    NodeEvent::AddBlock {
      block: block.into(),
      event: AddBlockEvent::NotEnoughWork,
    }
  }
  pub fn reorg(block: U256, old: U256, height: u128) -> Self {
    NodeEvent::AddBlock {
      block: block.into(),
      event: AddBlockEvent::Reorg { old: old.into(), height },
    }
  }
  pub fn already_included(block: U256) -> Self {
    NodeEvent::AddBlock {
      block: block.into(),
      event: AddBlockEvent::AlreadyIncluded,
    }
  }
  pub fn missing_parent(block: U256, parent: U256) -> Self {
    NodeEvent::AddBlock {
      block: block.into(),
      event: AddBlockEvent::MissingParent { parent: parent.into() },
    }
  }
  pub fn too_late(block: U256) -> Self {
    NodeEvent::AddBlock { block: block.into(), event: AddBlockEvent::TooLate }
  }

  // MINING
  pub fn mined(block: U256, target: U256) -> Self {
    NodeEvent::Mining {
      event: MiningEvent::Success {
        block: block.into(),
        target: target.into(),
      },
    }
  }
  pub fn failed_mined(target: U256) -> Self {
    NodeEvent::Mining { event: MiningEvent::Failure { target: target.into() } }
  }
  pub fn ask_mine(target: U256) -> Self {
    NodeEvent::Mining { event: MiningEvent::AskMine { target: target.into() } }
  }
  pub fn stopped() -> Self {
    NodeEvent::Mining { event: MiningEvent::Stop }
  }

  // PEERS
  pub fn timeout<A: ProtoAddr>(peer: &Peer<A>) -> Self {
    NodeEvent::Peers {
      event: PeersEvent::Timeout {
        addr: peer.address.to_string(),
        seen_at: peer.seen_at,
      },
    }
  }
  pub fn see_peer_activated<A: ProtoAddr>(peer: &Peer<A>) -> Self {
    NodeEvent::Peers {
      event: PeersEvent::SeePeer {
        addr: peer.address.to_string(),
        seen_at: peer.seen_at,
        result: SeePeerResult::Activated,
      },
    }
  }
  pub fn see_peer_not_seen<A: ProtoAddr>(peer: &Peer<A>) -> Self {
    NodeEvent::Peers {
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
    NodeEvent::Peers {
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
    NodeEvent::HandleMessage { event }
  }
  pub fn give_me_block(magic: u64, block: U256) -> Self {
    let event =
      HandleMessageEvent::GiveMeThatBlock { magic, bhash: block.into() };
    NodeEvent::HandleMessage { event }
  }
  pub fn mine_trans(magic: u64, trans: U256) -> Self {
    let event = HandleMessageEvent::PleaseMineThisTransaction {
      magic,
      trans: trans.into(),
    };
    NodeEvent::HandleMessage { event }
  }

  // UTILS
  pub fn get_tag(&self) -> String {
    match self {
      NodeEvent::AddBlock { .. } => "add_block".to_string(),
      NodeEvent::Mining { .. } => "mining".to_string(),
      NodeEvent::Peers { .. } => "peers".to_string(),
      NodeEvent::HandleMessage { .. } => "handle_message".to_string(),
      NodeEvent::Heartbeat { .. } => "heartbeat".to_string(),
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
      hashrate: $hashrate:expr,
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
    NodeEvent::Heartbeat {
      peers: $crate::events::HeartbeatPeers { num: $peers_num },
      tip: $crate::events::HeartbeatTip {
        height: $tip_height,
        difficulty: $difficulty,
        hashrate: $hashrate,
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
// Websocket server

pub struct WsConfig {
  pub port: u16,
  pub buffer_size: usize,
}

#[derive(serde::Deserialize)]
pub struct QueryParams {
  pub tags: Option<String>,
}

pub struct Query {
  pub tags: Vec<String>,
}

pub fn ws_loop(port: u16, ws_tx: broadcast::Sender<NodeEvent>) {
  let runtime = tokio::runtime::Runtime::new().unwrap();
  runtime.block_on(async move {
    ws_server(port, ws_tx).await;
  });
}

async fn ws_server(port: u16, ws_tx: broadcast::Sender<NodeEvent>) {
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
  ws_tx: broadcast::Sender<NodeEvent>,
) -> impl Filter<Extract = (broadcast::Sender<NodeEvent>,), Error = Infallible> + Clone
{
  warp::any().map(move || ws_tx.clone())
}

pub async fn ws_handler(
  ws: warp::ws::Ws,
  ws_tx: broadcast::Sender<NodeEvent>,
  query: Query,
) -> Result<impl Reply, Rejection> {
  Ok(ws.on_upgrade(move |socket| client_connection(socket, ws_tx, query.tags)))
}

pub async fn client_connection(
  ws: WebSocket,
  ws_tx: broadcast::Sender<NodeEvent>,
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
