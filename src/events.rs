use crate::{
  api::{Hash, NodeRequest},
  net::ProtoAddr,
  node::{Block, Peer},
};
use primitive_types::U256;
use serde;
use std::sync::mpsc;

#[derive(Debug, serde::Serialize)]
pub enum AddBlockEvent {
  AlreadyIncluded,
  NotEnoughWork,
  Reorg { old: Hash, height: u128 },
  MissingParent { parent: Hash },
  TooLate,
}

#[derive(Debug, serde::Serialize)]
pub enum MiningEvent {
  Success { block: Hash, target: Hash },
  Failure { target: Hash },
  AskMine { target: Hash },
  Stop,
}

#[derive(Debug, serde::Serialize)]
pub struct HeartbeatPeers {
  pub num: usize,
}

#[derive(Debug, serde::Serialize)]
pub struct HeartbeatTip {
  pub height: u64,
  pub difficulty: u64,
  pub hashrate: u64,
}

#[derive(Debug, serde::Serialize)]
pub struct HeartbeatBlocks {
  pub missing: u64,
  pub pending: u64,
  pub included: usize,
}

#[derive(Debug, serde::Serialize)]
pub struct HeartbeatRuntime {
  pub mana: HeartbeatStatInfo,
  pub size: HeartbeatStatInfo,
}

#[derive(Debug, serde::Serialize)]
pub struct HeartbeatStatInfo {
  pub current: i64,
  pub limit: i64,
  pub available: i64,
}

#[derive(Debug, serde::Serialize)]
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

#[derive(Debug, serde::Serialize)]
pub enum NodeEvent {
  AddBlock {
    block: Hash,
    event: AddBlockEvent,
  },
  Mining {
    event: MiningEvent,
  },
  HandleRequest,
  Heartbeat {
    peers: HeartbeatPeers,
    tip: HeartbeatTip,
    blocks: HeartbeatBlocks,
    runtime: HeartbeatRuntime,
  },
  HandleMessage {
    event: HandleMessageEvent,
  },
}

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
      NodeEvent::HandleRequest => {
        "[handle_request] something was requested in node api".to_string()
      }
      NodeEvent::Heartbeat { peers, tip, blocks, runtime } => {
        format!("[heartbeat] {} {} {} {}", peers, tip, blocks, runtime)
      }
      NodeEvent::HandleMessage { event } => {
        format!("[handle_message] {}", event)
      }
    };

    f.write_fmt(format_args!("{}", str_res))
  }
}

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

  // HANDLE REQUEST
  pub fn handle_request() -> Self {
    NodeEvent::HandleRequest
  }

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
}

pub fn new_event_channel(
) -> (mpsc::Sender<NodeEvent>, mpsc::Receiver<NodeEvent>) {
  let (tx, rx) = mpsc::channel::<NodeEvent>();
  (tx, rx)
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
      peers: crate::events::HeartbeatPeers { num: $peers_num },
      tip: crate::events::HeartbeatTip {
        height: $tip_height,
        difficulty: $difficulty,
        hashrate: $hashrate,
      },
      blocks: crate::events::HeartbeatBlocks {
        missing: $missing_count,
        pending: $pending_count,
        included: $included_count,
      },
      runtime: crate::events::HeartbeatRuntime {
        mana: crate::events::HeartbeatStatInfo {
          current: $mana_cur,
          limit: $mana_lim,
          available: $mana_avail,
        },
        size: crate::events::HeartbeatStatInfo {
          current: $size_cur,
          limit: $size_lim,
          available: $size_avail,
        },
      },
    }
  };
}
