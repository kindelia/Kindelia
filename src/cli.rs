pub use clap::{Parser, Subcommand};

#[derive(Parser)]
#[clap(author, version, about, long_about = None)]
pub struct Cli {
  #[clap(subcommand)]
  pub command: CliCmd,
}

#[derive(Subcommand)]
pub enum CliCmd {
  Start {
    #[clap(long)]
    ui: bool, 
  },
  /// Runs a Kindelia file
  Run { 
    /// Input file
    file: String,
    // #[clap(short, long)]
    // debug: bool,
  },
}
