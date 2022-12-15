use primitive_types::U256;
use serde;

use crate::api::Hash;
use crate::net::ProtoAddr;
use crate::node::{HashedBlock, Peer};
use crate::runtime::{StatementErr, StatementInfo};

use add_block::{AddBlockEvent, BlockInfo, RollbackInfo};
mod add_block {
  use primitive_types::U256;

  use crate::{
    api::Hash,
    runtime::{StatementErr, StatementInfo},
  };

  #[allow(clippy::large_enum_variant)]
  #[derive(Debug, Clone, serde::Serialize)]
  pub enum AddBlockEvent {
    AlreadyIncluded,
    NotEnoughWork,
    Reorg {
      old_tip: BlockInfo,             // old network's tip
      common_block: BlockInfo,        // first common block in both timelines
      rollback: Option<RollbackInfo>, // if ocurred a rollback, store its info
      work: Hash,
    },
    Included {
      siblings: Vec<Hash>,
      work: Hash,
    },
    Computed {
      block: BlockInfo,
      results: Vec<Result<StatementInfo, StatementErr>>,
    },
    MissingParent {
      parent: Hash,
    },
    TooLate,
  }

  #[derive(Debug, Clone, serde::Serialize)]
  pub struct BlockInfo {
    pub hash: Hash,
    parent: Hash,
    pub height: Option<u128>,
  }

  impl BlockInfo {
    pub fn new(hash: U256, parent: U256, height: Option<u128>) -> Self {
      BlockInfo { hash: hash.into(), parent: parent.into(), height }
    }
  }

  impl std::fmt::Display for BlockInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      f.write_fmt(format_args!("{}", self.hash))
    }
  }

  #[derive(Debug, Clone, serde::Serialize)]
  pub struct RollbackInfo {
    runtime_tick: u128,
    common_tick: u128,
    rolled_to: u128,
  }

  impl RollbackInfo {
    pub fn new(
      common_tick: u128,
      runtime_tick: u128,
      rolled_to: u128,
    ) -> Option<Self> {
      if common_tick < runtime_tick {
        Some(RollbackInfo { common_tick, runtime_tick, rolled_to })
      } else {
        None
      }
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
}

use mining::MiningEvent;
mod mining {
  use crate::api::Hash;

  #[derive(Debug, Clone, serde::Serialize)]
  pub enum MiningEvent {
    Success { block: Hash, target: Hash },
    Failure { target: Hash },
    AskMine { target: Hash },
    Stop,
  }
}

use peers::{PeersEvent, SeePeerResult};
mod peers {
  #[derive(Debug, Clone, serde::Serialize)]
  // The peers are represented as strings (addr display)
  // to avoid type parameter
  pub enum PeersEvent {
    SeePeer { addr: String, seen_at: u128, result: SeePeerResult },
    Timeout { addr: String, seen_at: u128 },
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

  #[derive(Debug, Clone, serde::Serialize)]
  pub enum SeePeerResult {
    NotSeenBefore,
    Activated,
    AlreadyActive { new_seen_at: u128 },
  }
}

use handle_message::HandleMessageEvent;
mod handle_message {
  use crate::api::Hash;

  #[derive(Debug, Clone, serde::Serialize)]
  pub enum HandleMessageEvent {
    NoticeTheseBlocks {
      magic: u32,
      gossip: bool,
      blocks: Vec<Hash>,
      peers: Vec<String>, // peer not used to avoid type parameter
    },
    GiveMeThatBlock {
      magic: u32,
      bhash: Hash,
    },
    PleaseMineThisTransaction {
      magic: u32,
      trans: Hash, // shoul we guard the data of transaction too?
    },
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
}

pub use heartbeat::{
  HeartbeatBlocks, HeartbeatPeers, HeartbeatRuntime, HeartbeatStatInfo,
  HeartbeatTip,
};
mod heartbeat {
  use crate::api::Hash;

  #[derive(Debug, Clone, serde::Serialize)]
  pub struct HeartbeatPeers {
    pub num: usize,
  }

  impl std::fmt::Display for HeartbeatPeers {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      f.write_fmt(format_args!("peers: {}", self.num))
    }
  }

  #[derive(Debug, Clone, serde::Serialize)]
  pub struct HeartbeatTip {
    pub height: u64,
    pub difficulty: u64,
    pub work: Hash,
  }

  impl std::fmt::Display for HeartbeatTip {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      f.write_fmt(format_args!(
        "tip: {{ height: {} | difficulty: {} }}",
        self.height, self.difficulty
      ))
    }
  }

  #[derive(Debug, Clone, serde::Serialize)]
  pub struct HeartbeatBlocks {
    pub missing: u64,
    pub pending: u64,
    pub included: usize,
  }

  impl std::fmt::Display for HeartbeatBlocks {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      f.write_fmt(format_args!(
        "blocks: {{ included: {} | missing: {} | pending: {} }}",
        self.included, self.missing, self.pending
      ))
    }
  }

  #[derive(Debug, Clone, serde::Serialize)]
  pub struct HeartbeatRuntime {
    pub mana: HeartbeatStatInfo,
    pub size: HeartbeatStatInfo,
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

  #[derive(Debug, Clone, serde::Serialize)]
  pub struct HeartbeatStatInfo {
    pub current: i64,
    pub limit: i64,
    pub available: i64,
  }

  impl std::fmt::Display for HeartbeatStatInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      f.write_fmt(format_args!(
        "current: {} | limit: {} | available: {}",
        self.current, self.limit, self.available
      ))
    }
  }

  #[macro_export]
  macro_rules! heartbeat_gen {
    (
    peers: { num: $peers_num:expr },
    tip: {
      height: $tip_height:expr,
      difficulty: $difficulty:expr,
      work: $work: expr,
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
    },
    tip_blocks: $tip_blocks:expr
  ) => {
      NodeEventType::Heartbeat {
        peers: $crate::events::HeartbeatPeers { num: $peers_num },
        tip: $crate::events::HeartbeatTip {
          height: $tip_height,
          difficulty: $difficulty,
          work: $work.into(),
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
        tip_blocks: $tip_blocks.iter().map(|x: &U256| (*x).into()).collect(),
      }
    };
  }
}

pub use discriminant::NodeEventDiscriminant;
/// Event discriminant util (used to choose which event we want to listen)
mod discriminant {
  use super::NodeEventType;

  #[derive(
    PartialEq, Eq, Debug, Clone, serde::Serialize, serde::Deserialize,
  )]
  pub enum NodeEventDiscriminant {
    AddBlock,
    HandleMessage,
    Heartbeat,
    Mining,
    Peers,
  }

  impl std::str::FromStr for NodeEventDiscriminant {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
      match s {
        "add_block" => Ok(NodeEventDiscriminant::AddBlock),
        "mining" => Ok(NodeEventDiscriminant::Mining),
        "peers" => Ok(NodeEventDiscriminant::Peers),
        "handle_message" => Ok(NodeEventDiscriminant::HandleMessage),
        "heartbeat" => Ok(NodeEventDiscriminant::Heartbeat),
        _ => Err(format!(
          "Was not possible to convert from {} to an event discriminant",
          s
        )),
      }
    }
  }

  impl From<NodeEventType> for NodeEventDiscriminant {
    fn from(event: NodeEventType) -> Self {
      match event {
        NodeEventType::AddBlock { .. } => NodeEventDiscriminant::AddBlock,
        NodeEventType::Mining { .. } => NodeEventDiscriminant::Mining,
        NodeEventType::Peers { .. } => NodeEventDiscriminant::Peers,
        NodeEventType::HandleMessage { .. } => {
          NodeEventDiscriminant::HandleMessage
        }
        NodeEventType::Heartbeat { .. } => NodeEventDiscriminant::Heartbeat,
      }
    }
  }
}

/// An node event. It is formed by the timestamp, the node address
/// and the type of the event.
#[derive(Debug, Clone, serde::Serialize)]
pub struct NodeEvent<A: ProtoAddr> {
  pub time: u128,
  pub addr: A,
  pub event: NodeEventType,
}

// "Pretty" print of the node event
impl<A: ProtoAddr> std::fmt::Display for NodeEvent<A> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.write_fmt(format_args!("[node] {} [event] {}", self.addr, self.event))
  }
}

/// This represents the emitted event
/// by the node channel, in the form of:
/// `(event_type, timestamp)`.
///
/// With this information the `NodeEvent` is formed on channel.recv
/// in the function `spawn_event_handlers`.
pub type NodeEventEmittedInfo = (NodeEventType, u128);

/// The type of the event.
#[derive(Debug, Clone, serde::Serialize)]
pub enum NodeEventType {
  AddBlock {
    block: BlockInfo,
    event: Box<AddBlockEvent>, // added `Box` because of lint warning
  },
  Mining {
    event: MiningEvent,
  },
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
    tip_blocks: Vec<Hash>,
  },
}

// This is only used for "pretty" print the event
impl std::fmt::Display for NodeEventType {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    fn show_opt<T: std::fmt::Display>(x: Option<T>) -> String {
      match x {
        None => "~".to_string(),
        Some(x) => x.to_string(),
      }
    }

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
          format!("[add_block] [reorg] old_block {} was reorganized in favor of new_block {}; old height: {}; new height: {} {}", old_tip.hash, block, show_opt(old_tip.height), show_opt(block.height), rollback)
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
        AddBlockEvent::Computed { block, results } => {
          let results: String = results
            .iter()
            .map(|statement_result| match statement_result {
              Ok(statement_info) => format!("{}", statement_info),
              Err(statement_err) => statement_err.err.to_string(),
            })
            .collect();
          format!("[computed] block {} | results: {}", block, results)
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
      NodeEventType::Heartbeat { peers, tip, blocks, runtime, .. } => {
        format!("[heartbeat] {} {} {} {}", peers, tip, blocks, runtime)
      }
    };

    f.write_fmt(format_args!("{}", str_res))
  }
}

// Constructors for each event type and subtype.
impl NodeEventType {
  pub fn not_enough_work(block: &HashedBlock) -> Self {
    let hash = U256::from(block.get_hash());
    NodeEventType::AddBlock {
      block: BlockInfo::new(hash, block.prev, None),
      event: Box::new(AddBlockEvent::NotEnoughWork),
    }
  }
  pub fn included(
    block: &HashedBlock,
    height: Option<u128>,
    siblings: &[U256],
    work: U256,
  ) -> Self {
    let hash = U256::from(block.get_hash());
    let siblings = siblings.iter().map(|h| (*h).into()).collect();
    NodeEventType::AddBlock {
      block: BlockInfo::new(hash, block.prev, height),
      event: Box::new(AddBlockEvent::Included { siblings, work: work.into() }),
    }
  }
  pub fn computed(
    added_block: (&HashedBlock, u128),
    block: (&HashedBlock, u128),
    results: &[Result<StatementInfo, StatementErr>],
  ) -> Self {
    let (added_block, added_height) = added_block;
    let (block, height) = block;
    let added_hash = U256::from(added_block.get_hash());
    let hash = U256::from(block.get_hash());

    NodeEventType::AddBlock {
      block: BlockInfo::new(added_hash, added_block.prev, Some(added_height)),
      event: Box::new(AddBlockEvent::Computed {
        block: BlockInfo::new(hash, block.prev, Some(height)),
        results: results.to_vec(),
      }),
    }
  }
  pub fn reorg(
    old: (&HashedBlock, u128),
    new: (&HashedBlock, u128),
    common: (&HashedBlock, u128),
    ticks: (u128, u128), // (old tick, new tick)
    work: U256,
  ) -> Self {
    let (old_block, old_height) = old;
    let (new_block, new_height) = new;
    let (common_block, common_height) = common;
    let (runtime_tick, rolled_to) = ticks;
    let common_tick = common_height;

    // TODO: remove these conversions
    let new_hash = U256::from(new_block.get_hash());
    let old_hash = U256::from(old_block.get_hash());
    let common_hash = U256::from(common_block.get_hash());

    let rollback = RollbackInfo::new(common_tick, runtime_tick, rolled_to);

    NodeEventType::AddBlock {
      block: BlockInfo::new(new_hash, new_block.prev, Some(new_height)),
      event: Box::new(AddBlockEvent::Reorg {
        old_tip: BlockInfo::new(old_hash, old_block.prev, Some(old_height)),
        common_block: BlockInfo::new(
          common_hash,
          common_block.prev,
          Some(common_height),
        ),
        rollback,
        work: work.into(),
      }),
    }
  }
  pub fn already_included(block: &HashedBlock, height: u128) -> Self {
    let bhash = U256::from(block.get_hash());
    NodeEventType::AddBlock {
      block: BlockInfo::new(bhash, block.prev, Some(height)),
      event: Box::new(AddBlockEvent::AlreadyIncluded),
    }
  }
  pub fn missing_parent(block: &HashedBlock) -> Self {
    let bhash = U256::from(block.get_hash());
    let parent: Hash = block.prev.into();
    NodeEventType::AddBlock {
      block: BlockInfo::new(bhash, block.prev, None),
      event: Box::new(AddBlockEvent::MissingParent { parent }),
    }
  }
  pub fn too_late(block: &HashedBlock) -> Self {
    let bhash = U256::from(block.get_hash());
    NodeEventType::AddBlock {
      block: BlockInfo::new(bhash, block.prev, None),
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
  pub fn stop_mining() -> Self {
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

  // HANDLE MESSAGE
  pub fn notice_blocks<A: ProtoAddr>(
    magic: u32,
    gossip: bool,
    blocks: &[HashedBlock],
    peers: &[Peer<A>],
  ) -> Self {
    let event = HandleMessageEvent::NoticeTheseBlocks {
      magic,
      gossip,
      blocks: blocks.iter().map(|b| U256::from(b.get_hash()).into()).collect(),
      peers: peers.iter().map(|p| format!("{}", p.address)).collect(),
    };
    NodeEventType::HandleMessage { event }
  }
  pub fn give_me_block(magic: u32, block: U256) -> Self {
    let event =
      HandleMessageEvent::GiveMeThatBlock { magic, bhash: block.into() };
    NodeEventType::HandleMessage { event }
  }
  pub fn mine_trans(magic: u32, trans: U256) -> Self {
    let event = HandleMessageEvent::PleaseMineThisTransaction {
      magic,
      trans: trans.into(),
    };
    NodeEventType::HandleMessage { event }
  }
}
