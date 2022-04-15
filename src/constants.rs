// UDP port to listen to
pub const UDP_PORT : u64 = 42000;

// Size of a hash, in bytes
pub const HASH_SIZE : usize = 32;

// Size of a u64, in bytes
pub const U64_SIZE : usize = 64 / 8;

// Size of a block's body, in bytes
pub const BODY_SIZE : usize = 1280;

// Size of a block, in bytes
pub const BLOCK_SIZE : usize = HASH_SIZE + (U64_SIZE * 4) + BODY_SIZE;

// Size of an IPv4 address, in bytes
pub const IPV4_SIZE : usize = 4;

// Size of an IPv6 address, in bytes
pub const IPV6_SIZE : usize = 16;

// Size of an IP port, in bytes
pub const PORT_SIZE : usize = 2;

// How many nodes we gossip an information to?
pub const GOSSIP_FACTOR : u64 = 16;

// How many nodes we request missing data from?
pub const REQUEST_FACTOR : u64 = 4;

// How many times the mining thread attempts before unblocking?
pub const MINE_ATTEMPTS : u64 = 1024;

// Desired average time between mined blocks is 3 seconds
pub const TIME_PER_BLOCK : u64 = 3000;

// don't accept blocks from 1 hour in the future
pub const DELAY_TOLERANCE : u64 = 60 * 60 * 1000;
  
// readjusts difficulty every 20 blocks
pub const BLOCKS_PER_PERIOD : u64 = 20;

// readjusts difficulty every 60 seconds
pub const TIME_PER_PERIOD : u64 = TIME_PER_BLOCK * BLOCKS_PER_PERIOD;

// initial difficulty of 256 hashes per block
pub const INITIAL_DIFFICULTY : u64 = 256;
