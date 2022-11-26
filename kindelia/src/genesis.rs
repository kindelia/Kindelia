use include_dir::{include_dir, Dir, File};
use std::path::{Path, PathBuf};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum GenesisPathError {
  #[error("Home directory not found")]
  HomeDirNotFound,
  #[error("File not found in {0}")]
  FileNotFound(PathBuf),
}

pub fn genesis_path(network_id: u32) -> Result<PathBuf, GenesisPathError> {
  let path = dirs::home_dir()
    .ok_or(GenesisPathError::HomeDirNotFound)?
    .join(".kindelia")
    .join("genesis")
    .join(format!("{:#02X}.kdl", network_id));
  match path.exists() {
    true => Ok(path),
    false => Err(GenesisPathError::FileNotFound(path)),
  }
}

#[derive(Error, Debug)]
pub enum GenesisCodeError {
  #[error(transparent)]
  PathError(#[from] GenesisPathError),

  #[error("Genesis block could not be read from {path:?}.")]
  ReadError { path: PathBuf, cause: std::io::Error },
}

pub fn genesis_code(network_id: u32) -> Result<String, GenesisCodeError> {
  let path = genesis_path(network_id)?;
  std::fs::read_to_string(&path)
    .map_err(|e| GenesisCodeError::ReadError { path, cause: e })
}

#[derive(Error, Debug)]
pub enum InitGenesisError {
  #[error("Could not create directory: {path:?}")]
  DirNotCreated { path: std::path::PathBuf, cause: std::io::Error },
  #[error("Unable to write genesis file: {path:?}")]
  FileNotWritten { path: std::path::PathBuf, cause: std::io::Error },
  #[error("Genesis file is missing from the executable")]
  Missing,
}

static GENESIS_DIR: Dir<'_> =
  include_dir!("$CARGO_MANIFEST_DIR/genesis/networks");

/// Copies latest file from kindelia_core/genesis to <homedir>/.kindelia/genesis
/// Creates target dir if not existing.
///
/// The way this works is that all the files in kindelia_core/genesis get compiled
/// into the executable by the include_dir!() macro.  With this trick we are able
/// to include files dynamically, whereas include_str!() requires a static str.
///
/// note: we could copy over all files from kindelia_core/genesis instead.
pub fn init_genesis(dir_path: &Path) -> Result<(), InitGenesisError> {
  let mut files: Vec<&File> = GENESIS_DIR.files().collect();

  // files should be named as hex values, so we sort case insensitively
  // The goal here is to find highest numeric (hex) value.
  files.sort_by_cached_key(|f| f.path().as_os_str().to_ascii_uppercase());

  let file = files.last().ok_or(InitGenesisError::Missing)?;
  let fname = file.path().file_name().ok_or(InitGenesisError::Missing)?;
  let fpath = dir_path.join(fname);

  let default_content = file.contents();
  std::fs::create_dir_all(dir_path).map_err(|e| {
    InitGenesisError::DirNotCreated { path: dir_path.to_path_buf(), cause: e }
  })?;

  std::fs::write(&fpath, default_content)
    .map_err(|e| InitGenesisError::FileNotWritten { path: fpath, cause: e })
}
