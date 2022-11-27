use std::{future::Future, path::Path, thread, time::Duration};

pub fn flag_to_option(flag: bool) -> Option<bool> {
  if flag {
    Some(true)
  } else {
    None
  }
}

pub fn bytes_to_u128(bytes: &[u8]) -> Option<u128> {
  let mut num: u128 = 0;
  for byte in bytes {
    num = num.checked_shl(8)?;
    num += *byte as u128;
  }
  Some(num)
}

// Async
// =====

pub fn run_async_blocking<T, P>(prom: P) -> anyhow::Result<T>
where
  P: Future<Output = anyhow::Result<T>>,
{
  let runtime = tokio::runtime::Runtime::new().unwrap();
  runtime.block_on(prom)
}

// Config
// ======

pub fn handle_config_file(path: &Path) -> Result<toml::Value, String> {
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

pub fn init_config_file(path: &Path) -> Result<(), String> {
  let dir_path = path.parent().ok_or_else(|| {
    format!("Failed to resolve parent directory for '{}'", path.display())
  })?;
  let default_content = include_str!("../default.toml");
  std::fs::create_dir_all(dir_path).map_err(|e| {
    format!("Could not create '{}' directory: {}", dir_path.display(), e)
  })?;
  std::fs::write(path, default_content)
    .map_err(|e| format!("Could not write to '{}': {}", path.display(), e))
}
