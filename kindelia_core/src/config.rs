use std::path::PathBuf;

use derive_builder::Builder;
use serde::{Deserialize, Serialize};

use crate::events::NodeEventDiscriminant;

// Node config
// ===========

#[derive(Debug, Clone, Builder)]
#[builder(setter(strip_option))]
pub struct NodeConfig {
  pub data_path: PathBuf, // TODO: abstract node file handling
  #[builder(default)]
  pub network_id: u32,
  #[builder(default)]
  pub mining: MineConfig,
  #[builder(default)]
  pub ui: Option<UiConfig>,
  #[builder(default)]
  pub ws: Option<WsConfig>,
}

// Mineration config
// =================

#[derive(Debug, Clone, Builder, Serialize, Deserialize, Default)]
#[builder(setter(strip_option))]
pub struct MineConfig {
  pub enabled: bool,
  pub slow_mining: Option<u64>,
}

// User Interface config
// =====================

#[derive(Debug, Clone, Builder, Serialize, Deserialize, Default)]
#[builder(setter(strip_option))]
pub struct UiConfig {
  pub json: bool,
  pub tags: Vec<NodeEventDiscriminant>
}

// API config
// ==========

#[derive(Debug, Clone, Builder, Serialize, Deserialize)]
#[builder(setter(strip_option))]
pub struct ApiConfig {
  pub port: u16,
}

impl Default for ApiConfig {
  fn default() -> Self {
    ApiConfig { port: 8000 }
  }
}

// Websocket config
// ----------------

#[derive(Debug, Clone, Builder, Serialize, Deserialize)]
#[builder(setter(strip_option))]
pub struct WsConfig {
  pub port: u16,
  pub buffer_size: usize,
}

impl Default for WsConfig {
  fn default() -> Self {
    WsConfig { port: 3000, buffer_size: 1024 * 2 }
  }
}
