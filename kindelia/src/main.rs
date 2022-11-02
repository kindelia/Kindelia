#![allow(clippy::single_component_path_imports)]
mod cli;

pub use clap::{Parser, Subcommand};

fn main() -> Result<(), String> {
  cli::run_cli()?;
  Ok(())
}
