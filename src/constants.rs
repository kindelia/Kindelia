// UDP port to listen to
pub const UDP_PORT : u16 = 42000;

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

// When we need missing info, how many nodes do we ask?
pub const MISSING_INFO_ASK_FACTOR : u64 = 3;

// How many times the mining thread attempts before unblocking?
pub const MINE_ATTEMPTS : u64 = 1024;

// Desired average time between mined blocks, in milliseconds
pub const TIME_PER_BLOCK : u64 = 3000;

// Don't accept blocks from N milliseconds in the future
pub const DELAY_TOLERANCE : u64 = 60 * 60 * 1000;
  
// Readjust difficulty every N blocks
pub const BLOCKS_PER_PERIOD : u64 = 20;

// Readjusts difficulty every N seconds
pub const TIME_PER_PERIOD : u64 = TIME_PER_BLOCK * BLOCKS_PER_PERIOD;

// Initial difficulty, in expected hashes per block
pub const INITIAL_DIFFICULTY : u64 = 256;

// How many milliseconds without notice until we forget a peer?
pub const PEER_TIMEOUT : u64 = 10 * 1000;

// How many peers we need to keep minimum?
pub const PEER_COUNT_MINIMUM : u64 = 256;

// How many peers we send when asked?
pub const SHARE_PEER_COUNT : u64 = 3;

// How many peers we keep on the last_seen object?
pub const LAST_SEEN_SIZE : u64 = 2;

