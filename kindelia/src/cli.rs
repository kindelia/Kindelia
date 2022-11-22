use std::path::PathBuf;

use clap::{Parser, Subcommand};
use clap_complete::Shell;

use kindelia_core::{api::LimitStats, common::Name};

use crate::files::FileInput;

/*

== Client ==

kindelia test file.kdl

kindelia serialize code.kdl > code.hex.txt

kindelia deserialize code.hex.txt
kindelia deserialize <<< a67bd36d75da

kindelia run-remote --hex <<< a67bd36d75da
kindelia publish    --hex <<< a67bd36d75da

kindelia sign code.hex.txt
kindelia sign <<< a67bd36d75da > code.sig.hex.tx

kindelia completion zsh >> .zshrc

== Remote ==

kindelia get fun Count code
kindelia get fun Count state
kindelia get fun Count slots

kindelia get reg Foo.Bar owner
kindelia get reg Foo.Bar list

kindelia get block 0xc7da4b76b4d7a64b7 | kindelia deserialize
kindelia get block 751
kindelia get block 2756

kindelia get ctr Pair code
kindelia get ctr Pair arity

kindelia get run <BLOCK_IDX> <STM_IDX>

kindelia get stats

kindelia get stats tick
kindelia get stats mana  [limit | used | available](TODO)
kindelia get stats space [limit | used | available](TODO)
kindelia get stats ctr-count
kindelia get stats fun-count
kindelia get stats reg-count

kindelia [--api ""] run-remote  code.hex.txt
kindelia [--api ""] publish     code.hex.txt

== Node ==

kindelia node start --mine --local --log-events --nice-ui?
kindelia node clean [-f]       // asks confirmation
kindelia node clean blocks [all|half|n <NUMBER_OF_BLOCKS>]

== Accounts ==

kindelia account ...

*/

macro_rules! discriminant {
  (
    $struct_type:ty => $result_type:ty, // discriminant of this struct
    pub enum $enum_name:ident { // enum name
      $($enum_variant:ident = $struct_variant:ident),+ // enum variants
      $(,)? // optional comma in the end
    }
  ) => {
    // enum creation
    #[derive(Subcommand)]
    pub enum $enum_name {
      $($enum_variant),+,
    }

    // enum implementation
    impl $enum_name {
      pub fn get_field(&self, structure: $struct_type) -> $result_type {
        match &self {
          $($enum_name::$enum_variant => structure.$struct_variant),+,
        }
      }
    }
  };
}

// Clap CLI definitions
// ====================

#[derive(Parser)]
#[clap(author, version, about, long_about = None)]
pub struct Cli {
  #[clap(subcommand)]
  pub command: CliCommand,
  #[clap(long, short = 'c')]
  /// Path to config file.
  pub config: Option<PathBuf>,
  /// Url to server host
  #[clap(long)]
  pub api: Option<String>,
}

#[derive(Subcommand)]
pub enum CliCommand {
  /// Test a Kindelia code file (.kdl), running locally.
  Test {
    /// The path to the file to test.
    file: FileInput,
    /// Whether to consider size and mana in the execution.
    #[clap(long)]
    sudo: bool,
  },
  /// Checks for statements in kdl file
  Check {
    /// The path to the file to test.
    file: FileInput,
    #[clap(long, short = 'e')]
    encoded: bool,
    /// Check subcommand.
    #[clap(subcommand)]
    command: CheckCommand,
  },
  /// Serialize a code file.
  Serialize {
    /// The path to the file to serialize.
    file: FileInput,
  },
  /// Deserialize a code file.
  Deserialize {
    /// The path to the file to deserialize.
    file: FileInput,
  },
  /// Deserialize a hex string of a encoded statement.
  Unserialize {
    /// Hex string of the serialized statement.
    stmt: String,
  },
  /// Sign a code file.
  Sign {
    /// The path to the file to sign.
    file: FileInput,
    /// File containing the 256-bit secret key, as a hex string
    #[clap(long, short = 's')]
    secret_file: PathBuf,
    #[clap(long, short = 'e')]
    encoded: bool,
    #[clap(long, short = 'E')]
    encoded_output: bool,
  },
  /// Test a Kindelia (.kdl) file, dry-running it on the current remote KVM state.
  RunRemote {
    /// Input file.
    file: FileInput,
    /// In case the input code is serialized.
    #[clap(long, short = 'e')]
    encoded: bool,
  },
  /// Post a Kindelia code file.
  Publish {
    /// The path to the file to post.
    file: FileInput,
    /// In case the input code is serialized.
    #[clap(long, short = 'e')]
    encoded: bool,
  },
  // Post a (serialized) statement
  Post {
    /// Hex string of the serialized statement.
    stmt: String,
  },
  /// Get remote information.
  Get {
    /// The kind of information to get.
    #[clap(subcommand)]
    kind: GetKind,
    /// Outputs JSON machine readable output.
    #[clap(long, short)]
    json: bool,
  },
  /// Initialize the configuration file.
  Init,
  /// Node commands.
  Node {
    /// Which command run.
    #[clap(subcommand)]
    command: NodeCommand,
    /// Base path to store the node's data in.
    #[clap(long)]
    data_dir: Option<PathBuf>,
    /// Network id / magic number.
    #[clap(long)]
    network_id: Option<u32>,
  },
  /// Generate auto-completion for a shell.
  Completion {
    /// The shell to generate completion for.
    shell: Shell,
  },
  Util {
    /// Which command run.
    #[clap(subcommand)]
    command: UtilCommand,
  },
}

#[derive(Subcommand)]
pub enum NodeCommand {
  /// Clean the node's data.
  Clean {
    #[clap(subcommand)]
    command: NodeCleanCommand,
  },
  /// Starts a Kindelia node.
  Start {
    /// Initial peer nodes.
    #[clap(long, short = 'p')]
    initial_peers: Option<Vec<String>>,
    /// Mine blocks.
    #[clap(long, short = 'm')]
    mine: bool,
    /// Log events as JSON
    #[clap(long, short)]
    json: bool,
  },
}

#[derive(Subcommand)]
pub enum NodeCleanCommand {
  /// Cleans all kindelia content.
  All,
  /// Cleans kindelia blocks
  Blocks {
    #[clap(subcommand)]
    command: NodeCleanBlocksCommand,
  },
}

#[derive(Subcommand)]
pub enum NodeCleanBlocksCommand {
  /// Cleans all kindelia blocks.
  All,
  /// Cleans half of kindelia blocks (the most recent ones).
  Half,
  /// Cleans <NUMBER_OF_BLOCKS> kindelia blocks (the most recent ones).
  N { number_of_blocks: usize },
}

#[derive(Subcommand)]
pub enum UtilCommand {
  /// Decodes a Kindelia 72-bit name (hex) to it's string.
  DecodeName { file: FileInput },
}

#[derive(Subcommand)]
pub enum CheckCommand {
  /// Give information about the transaction generated by this code.
  Transaction,
}

#[derive(Subcommand)]
pub enum GetKind {
  /// Get a constructor by name.
  Ctr {
    /// The name of the constructor to get.
    name: Name,
    /// The stat of the constructor to get.
    #[clap(subcommand)]
    stat: GetCtrKind,
  },
  /// [NOT IMPLEMENTED] Get a block by hash.
  Block {
    /// The hash of the block to get.
    hash: String,
  },
  /// Get a function by name.
  Fun {
    /// The name of the function to get.
    name: Name,
    /// The stat of the function to get.
    #[clap(subcommand)]
    stat: GetFunKind,
  },
  /// Get a registered namespace by name.
  Reg {
    /// The name of the namespace to get.
    name: String,
    /// The stat of the namespace to get.
    #[clap(subcommand)]
    stat: GetRegKind,
  },
  BlockHash {
    index: u64,
  },
  /// Get node stats.
  Stats {
    /// The stat of the node to get.
    #[clap(subcommand)]
    stat_kind: Option<GetStatsKind>,
  },
  Peers {
    /// Get all seen peers, including inactive ones
    #[clap(long)]
    all: bool,
  },
}

#[derive(Subcommand)]
pub enum GetFunKind {
  /// Get the code of a function.
  Code,
  /// Get the state of a function.
  State,
  /// Get the slots of a function.
  Slots,
}

#[derive(Subcommand)]
pub enum GetRegKind {
  /// Get the owner of a namespace.
  Owner,
  /// Get the list of statements in a namespace.
  List,
}

#[derive(Subcommand)]
pub enum GetCtrKind {
  /// Get the code of a constructor.
  Code,
  /// Get the arity of a constructor.
  Arity,
}

#[derive(Subcommand)]
pub enum GetStatsKind {
  /// Get the tick (tip block height).
  Tick,
  /// Get the used mana.
  Mana {
    /// Optional specification of the stat
    #[clap(subcommand)]
    limit_stat: Option<LimitStatsDiscriminant>,
  },
  /// Get the quantity of used space.
  // TODO: we should measure this as slots/nodes/cells, not bits
  Space {
    /// Optional specification of the stat
    #[clap(subcommand)]
    limit_stat: Option<LimitStatsDiscriminant>,
  },
  /// Get the number of functions.
  FunCount,
  /// Get the number of constructors.
  CtrCount,
  /// Get the number of namespaces.
  RegCount,
}

discriminant!(
  LimitStats => u64,
  pub enum LimitStatsDiscriminant {
    Limit = limit,
    Used = used,
    Available = available,
  }
);
