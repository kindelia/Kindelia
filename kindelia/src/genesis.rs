use include_dir::{include_dir, Dir};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum GenesisCodeError {
  #[error("Unknown network: {0}.")]
  UnknownNetwork(u32),

  #[error("Invalid Utf8 in genesis block for network: {network_id:?}.")]
  InvalidUtf8 { network_id: u32, source: std::str::Utf8Error },
}

// Todo: it would be cleaner to return the contents as &[u8] without
// UTF8 conversion. However the result ultimately gets passed to
// parse_code() as &str, so the UTF-8 conversion has to happen somewhere.
// Likely the correct thing would be to change parse_code to accept
// &[u8] instead.
pub fn genesis_code(network_id: u32) -> Result<&'static str, GenesisCodeError> {
  const GENESIS_DIR: Dir<'_> =
    include_dir!("$CARGO_MANIFEST_DIR/genesis/networks");

  let fname = format!("{:#02X}.kdl", network_id);
  let contents = GENESIS_DIR
    .get_file(&fname)
    .ok_or(GenesisCodeError::UnknownNetwork(network_id))?
    .contents();

  std::str::from_utf8(contents)
    .map_err(|e| GenesisCodeError::InvalidUtf8 { network_id, source: e })
}
