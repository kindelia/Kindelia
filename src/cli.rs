pub use clap::Parser;

#[derive(Parser)]
#[clap(author, version, about, long_about = None)]
pub struct Cli {
  #[clap(long)]
  pub no_ui: bool,
}
