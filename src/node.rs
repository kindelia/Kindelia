use bit_vec::BitVec;
use im::HashSet;
use pad::{PadStr, Alignment};
use primitive_types::U256;
use priority_queue::PriorityQueue;
use sha3::Digest;

use std::collections::HashMap;
use std::net::*;
use std::sync::{Arc, Mutex};
use std::thread;

use std::io::{stdin, stdout, Write};
use termion::event::Key;
use termion::input::TermRead;
use termion::raw::IntoRawMode;
use termion::{color, style};

use crate::algorithms::*;
use crate::constants::*;
use crate::network::*;
use crate::serializer::*;
use crate::types::*;

// initial target of 256 hashes per block
pub fn INITIAL_TARGET() -> U256 {
  return difficulty_to_target(u256(INITIAL_DIFFICULTY));
}

pub fn ZERO_HASH() -> U256 {
  return hash_u256(u256(0));
}

pub fn ROOT_BLOCK() -> Block {
  return Block {
    prev: ZERO_HASH(),
    time: 0,
    rand: 0,
    body: Body {
      value: [0; 1280]
    }
  }
}

pub fn new_node() -> Node {
  let try_ports = [UDP_PORT, UDP_PORT + 1, UDP_PORT + 2];
  let (socket, port) = udp_init(&try_ports).expect("Couldn't open UDP socket.");
  let mut node = Node {
    socket     : socket,
    port       : port,
    block      : HashMap::from([(ZERO_HASH(), ROOT_BLOCK())]),
    children   : HashMap::from([(ZERO_HASH(), vec![])]),
    waiters    : HashMap::new(),
    work       : HashMap::from([(ZERO_HASH(), u256(0))]),
    height     : HashMap::from([(ZERO_HASH(), 0)]),
    target     : HashMap::from([(ZERO_HASH(), INITIAL_TARGET())]),
    seen       : HashMap::from([(ZERO_HASH(), ())]),
    tip        : (u256(0), ZERO_HASH()),
    was_mined  : HashMap::new(),
    pool       : PriorityQueue::new(),
    peer_id    : HashMap::new(),
    peers      : HashMap::new(),
  };
  node_see_peer(&mut node, Peer {
    address: Address::IPv4 { val0: 127, val1: 0, val2: 0, val3: 1, port: UDP_PORT + 0 },
    seen_at: get_time(),
  });
  node_see_peer(&mut node, Peer {
    address: Address::IPv4 { val0: 127, val1: 0, val2: 0, val3: 1, port: UDP_PORT + 1 },
    seen_at: get_time(),
  });
  node_see_peer(&mut node, Peer {
    address: Address::IPv4 { val0: 127, val1: 0, val2: 0, val3: 1, port: UDP_PORT + 2 },
    seen_at: get_time(),
  });
  return node;
}

pub fn new_miner_comm() -> SharedMinerComm {
  return Arc::new(Mutex::new(MinerComm::Stop));
}

pub fn new_input() -> SharedInput {
  return Arc::new(Mutex::new(String::new()));
}

pub fn node_see_peer(node: &mut Node, peer: Peer) {
  match node.peer_id.get(&peer.address) {
    None => {
      let index = node.peers.len() as u64;
      node.peers.insert(index, peer.clone());
      node.peer_id.insert(peer.address, index);
    }
    Some(index) => {
      let old_peer = node.peers.get_mut(&index).unwrap();
      old_peer.seen_at = peer.seen_at;
    }
  }
}

pub fn node_del_peer(node: &mut Node, addr: Address) {
  if let Some(index) = node.peer_id.get(&addr) {
    node.peers.remove(&index);
    node.peer_id.remove(&addr);
  }
}

pub fn get_random_peer(node: &mut Node) -> Option<Peer> {
  let len = node.peers.len() as u64;
  if len > 0 {
    let index = rand::random::<u64>() % len;
    return Some(node.peers[&index]);
  } else {
    return None;
  }
}

pub fn get_random_peers_go(node: &mut Node, len: u64) -> Option<Vec<Peer>> {
  let mut result = vec![];
  let len = std::cmp::max(len, node.peers.len() as u64);
  for i in 0 .. len {
    result.push(get_random_peer(node)?);
  }
  return Some(result);
}

pub fn get_random_peers(node: &mut Node, len: u64) -> Vec<Peer> {
  match get_random_peers_go(node, len) {
    None    => vec![],
    Some(x) => x,
  }
}


pub fn node_add_block(node: &mut Node, block: &Block) {
  // Adding a block might trigger the addition of other blocks
  // that were waiting for it. Because of that, we loop here.
  let mut must_add = vec![block.clone()]; // blocks to be added
  //println!("- add_block");
  // While there is a block to add...
  while let Some(block) = must_add.pop() {
    let btime = block.time; // the block timestamp
    // If block is too far into the future, ignore it
    if btime >= get_time() + DELAY_TOLERANCE {
      continue;
    }
    let bhash = hash_block(&block); // hash of the block
    // If we already registered this block, ignore it
    if node.block.get(&bhash).is_some() {
      continue;
    }
    let phash = block.prev; // hash of the previous block
    // If previous block is available, add the block to the chain
    if node.block.get(&phash).is_some() {
      let work = get_hash_work(bhash); // block work score
      node.block.insert(bhash, block.clone()); // inserts the block
      node.work.insert(bhash, u256(0)); // inits the work attr
      node.height.insert(bhash, 0); // inits the height attr
      node.target.insert(bhash, u256(0)); // inits the target attr
      node.children.insert(bhash, vec![]); // inits the children attrs
      // Checks if this block PoW hits the target
      let has_enough_work = bhash >= node.target[&phash];
      // Checks if this block's timestamp is larger than its parent's timestamp
      // Note: Bitcoin checks if it is larger than the median of the last 11 blocks; should we?
      let advances_time = btime > node.block[&phash].time;
      // If the PoW hits the target and the block's timestamp is valid...
      if has_enough_work && advances_time {
        node.work.insert(bhash, node.work[&phash] + work); // sets this block accumulated work
        node.height.insert(bhash, node.height[&phash] + 1); // sets this block accumulated height
        // If this block starts a new period, computes the new target
        if node.height[&bhash] > 0 && node.height[&bhash] > BLOCKS_PER_PERIOD && node.height[&bhash] % BLOCKS_PER_PERIOD == 1 {
          // Finds the checkpoint hash (hash of the first block of the last period)
          let mut checkpoint_hash = phash;
          for _ in 0 .. BLOCKS_PER_PERIOD - 1 {
            checkpoint_hash = node.block[&checkpoint_hash].prev;
          }
          // Computes how much time the last period took to complete
          let period_time = btime - node.block[&checkpoint_hash].time;
          // Computes the target of this period
          let last_target = node.target[&phash];
          let next_scaler = 2u64.pow(32) * TIME_PER_PERIOD / period_time;
          let next_target = compute_next_target(last_target, u256(next_scaler));
          // Sets the new target
          node.target.insert(bhash, next_target);
        // Otherwise, keep the old target
        } else {
          node.target.insert(bhash, node.target[&phash]);
        }
        // Updates the tip work and block hash
        if node.work[&bhash] > node.tip.0 {
          node.tip = (node.work[&bhash], bhash);
        }
      }
      // Registers this block as a child of its parent
      node.children.insert(phash, vec![bhash]);
      // If there were blocks waiting for this one, mark them for addition
      for waiter in node.waiters.get(&bhash).unwrap_or(&vec![]) {
        must_add.push(waiter.clone());
      }
      // Delete this block's waiters vector
      node.waiters.remove(&bhash);
    // Otherwise (if previous block is unavailable), and if it is the
    // first time we see this block, add it to its parent's waiters list
    } else if node.seen.get(&bhash).is_none() {
      node.waiters.insert(phash, vec![block.clone()]);
    }
    // Mark this block as seen
    node.seen.insert(bhash, ());
  }
}

pub fn get_longest_chain(node: &Node) -> Vec<Block> {
  let mut longest = Vec::new();
  let mut bhash = node.tip.1;
  while node.block.contains_key(&bhash) && bhash != ZERO_HASH() {
    let block = node.block.get(&bhash).unwrap();
    longest.push(block.clone());
    bhash = block.prev;
  }
  longest.reverse();
  return longest;
}

pub fn node_message_receive(node: &mut Node) {
  for (addr, msg) in udp_receive(&mut node.socket) {
    node_message_handle(node, addr, &msg);
  }
}

// Sends a block to a target address; also share some random peers
// FIXME: instead of sharing random peers, share recently active peers
pub fn node_send_block_to(node: &mut Node, addr: Address, block: Block) {
  let msg = Message::PutBlock {
    block: block,
    peers: get_random_peers(node, 3).clone(),
  };
  udp_send(&mut node.socket, addr, &msg);
}

pub fn node_message_handle(node: &mut Node, addr: Address, msg: &Message) {
  node_see_peer(node, Peer { address: addr, seen_at: get_time() });
  match msg {
    // Someone asked a block
    Message::AskBlock { bhash } => {
      if let Some(block) = node.block.get(&bhash) {
        let block = block.clone();
        node_send_block_to(node, addr, block);
      }
    }
    // Someone sent us a block
    Message::PutBlock { block, peers } => {
      node_add_block(node, &block);
      for peer in peers {
        //node_see_peer(node, *peer);
      }
    }
  }
}

pub fn gossip(node: &mut Node, peer_count: u64, message: &Message) {
  for peer in get_random_peers(node, peer_count) {
    udp_send(&mut node.socket, peer.address, message);
  }
}

pub fn get_blocks_dir() -> String {
  let mut dir = dirs::home_dir().unwrap();
  dir.push(".kindelia");
  dir.push("blocks");
  return dir.to_str().unwrap().to_string();
}

// Mining
// ======

// Get the current target
pub fn get_tip_target(node: &mut Node) -> U256 {
  return node.target[&node.tip.1];
}

// Given a target, attempts to mine a block by changing its nonce up to `max_attempts` times
pub fn try_mine(prev: U256, body: Body, targ: U256, max_attempts: u64) -> Option<Block> {
  let rand = rand::random::<u64>();
  let time = get_time();
  let mut block = Block { time, rand, prev, body };
  for _i in 0 .. max_attempts {
    if hash_block(&block) >= targ {
      return Some(block);
    } else {
      block.rand = block.rand.wrapping_add(1);
    }
  }
  return None;
}

// Writes the shared MinerComm object
pub fn write_miner_comm(miner_comm: &SharedMinerComm, new_value: MinerComm) {
  let mut value = miner_comm.lock().unwrap();
  *value = new_value;
}

// Reads the shared MinerComm object
pub fn read_miner_comm(miner_comm: &SharedMinerComm) -> MinerComm {
  return (*miner_comm.lock().unwrap()).clone();
}

// Main miner loop: if asked, attempts to mine a block
pub fn miner_loop(miner_comm: &SharedMinerComm) {
  loop {
    if let MinerComm::Request { prev, body, targ } = read_miner_comm(miner_comm) {
      //println!("[miner] mining with target: {}", hex::encode(u256_to_bytes(targ)));
      let mined = try_mine(prev, body, targ, MINE_ATTEMPTS);
      if let Some(block) = mined {
        //println!("[miner] mined a block!");
        write_miner_comm(miner_comm, MinerComm::Answer { block });
      }
    }
  }
}

// Input
// =====

pub fn input_loop(input: &SharedInput) {
  let mut stdout = stdout().into_raw_mode().unwrap();

  for key in stdin().keys() {
    //print!("got {:?}\n\r", key);
    if let Ok(Key::Char(chr)) = key {
      (input.lock().unwrap()).push(chr);
      stdout.flush().unwrap();
    }
    if let Ok(Key::Backspace) = key {
      (input.lock().unwrap()).pop();
      stdout.flush().unwrap();
    }
    if let Ok(Key::Ctrl('c')) = key {
      std::process::exit(0);
    }
    if let Ok(Key::Ctrl('q')) = key {
      std::process::exit(0);
    }
  }
}

// Node
// ====

fn node_gossip_tip_block(node: &mut Node, peer_count: u64) {
  let random_peers = get_random_peers(node, peer_count);
  for peer in random_peers {
    node_send_block_to(node, peer.address, node.block[&node.tip.1].clone());
  }
}

fn node_peers_timeout(node: &mut Node) {
  let mut forget = Vec::new();
  for (id,peer) in &node.peers {
    //println!("... {} < {} {}", peer.seen_at, get_time() - PEER_TIMEOUT, peer.seen_at < get_time() - PEER_TIMEOUT);
    if peer.seen_at < get_time() - PEER_TIMEOUT {
      forget.push(peer.address);
    }
  }
  for addr in forget {
    node_del_peer(node, addr);
  }
}

fn node_ask_missing_blocks(node: &mut Node) {
  for bhash in node.waiters.keys().cloned().collect::<Vec<U256>>() {
    if let None = node.seen.get(&bhash) {
      gossip(node, MISSING_INFO_ASK_FACTOR, &Message::AskBlock { bhash: bhash.clone() });
    }
  }
}

fn node_save_longest_chain(node: &mut Node) {
  for (index, block) in get_longest_chain(node).iter().enumerate() {
    let bdir = get_blocks_dir();
    let path = format!("{}/{}", bdir, index);
    let buff = bitvec_to_bytes(&serialized_block(&block));
    std::fs::create_dir_all(bdir).ok();
    std::fs::write(path, buff).ok();
  }
}

fn node_load_longest_chain(node: &mut Node) {
  let bdir = get_blocks_dir();
  std::fs::create_dir_all(&bdir).ok();
  for entry in std::fs::read_dir(&bdir).unwrap() {
    let buffer = std::fs::read(entry.unwrap().path()).unwrap();
    let block = deserialized_block(&bytes_to_bitvec(&buffer));
    node_add_block(node, &block);
  }
}

fn node_ask_mine(node: &mut Node, input: &SharedInput, miner_comm: &SharedMinerComm) {
  let mine_body = { (input.lock().unwrap()).clone() };
  if let Some('\n') = mine_body.chars().last() {
    write_miner_comm(miner_comm, MinerComm::Request {
      prev: node.tip.1,
      body: string_to_body(&mine_body.replace("\n", "")),
      targ: get_tip_target(node),
    });
  }
}

fn node_display(node: &Node, input: &SharedInput, last_screen: &mut Option<Vec<String>>, redraw: bool) {
  //─━│┃┄┅┆┇┈┉┊┋┌┍┎┏┐┑┒┓└┕┖┗┘┙┚┛├┝┞┟┠┡┢┣┤┥┦┧┨┩┪┫┬┭┮┯┰┱┲┳┴┵┶┷┸┹┺┻┼┽┾┿╀╁╂╃╄╅╆╇╈╉╊╋╌╍╎╏═║╒╓╔╕╖╗╘╙╚╛╜╝╞╟╠╡╢╣╤╥╦╧╨╩╪╫╬╭╮╯╰╱╲╳╴╵╶╷╸╹╺╻╼╽╾
  
  fn bold(text: &str) -> String {
    return format!("{}{}{}", "\x1b[1m", text, "\x1b[0m");
  }

  fn display_block(chain: &Node, block: &Block, index: usize, width: u16) -> String {
    let zero = u256(0);
    let bhash = hash_block(block);
    let work = chain.work.get(&bhash).unwrap_or(&zero);
    let diff = target_to_difficulty(*chain.target.get(&bhash).unwrap_or(&u256(0)));
    let show_index = format!("{}", index).pad(6, ' ', Alignment::Right, true);
    let show_time = format!("{}", block.time).pad(13, ' ', Alignment::Left, true);
    let show_hash = format!("{}", hex::encode(u256_to_bytes(bhash)));
    let show_body = format!("{}", body_to_string(&block.body).pad(width as usize - 110, ' ', Alignment::Left, true));
    return format!("{} | {} | {} | {}", show_index, show_time, show_hash, show_body);
  }

  fn display_input(input: &str) -> String {
    let mut input : String = input.to_string();
    if let Some('\n') = input.chars().last() {
      input = bold(&input);
    }
    input = input.replace("\n", "");
    return input;
  }

  // Gets the terminal width and height
  let (width, height) = termion::terminal_size().unwrap();
  let menu_width = 17;

  let targ = node.target[&node.tip.1];
  let diff = target_to_difficulty(targ);
  let rate = diff * u256(1000) / u256(TIME_PER_BLOCK);
  let mut pending_size = 0;
  let mut pending_seen = 0;
  for (bhash, _) in node.waiters.iter() {
    if node.seen.get(bhash).is_some() {
      pending_seen += 1;
    }
    pending_size += 1;
  }

  let mut menu_lines : Vec<(String,bool)> = vec![];
  menu_lines.push((format!("Time"), true));
  menu_lines.push((format!("{}", get_time()), false));
  menu_lines.push((format!("{}", "-------------"), false));
  menu_lines.push((format!("Peers"), true));
  menu_lines.push((format!("{}", node.peers.len()), false));
  menu_lines.push((format!("{}", "-------------"), false));
  menu_lines.push((format!("Difficulty"), true));
  menu_lines.push((format!("{}", diff), false));
  menu_lines.push((format!("{}", "-------------"), false));
  menu_lines.push((format!("HashRate"), true));
  menu_lines.push((format!("{}", rate), false));
  menu_lines.push((format!("{}", "-------------"), false));
  menu_lines.push((format!("Blocks"), true));
  menu_lines.push((format!("{}", node.block.len()), false));
  menu_lines.push((format!("{}", "-------------"), false));
  menu_lines.push((format!("Height"), true));
  menu_lines.push((format!("{}", node.height[&node.tip.1]), false));
  menu_lines.push((format!("{}", "-------------"), false));
  menu_lines.push((format!("Pending"), true));
  menu_lines.push((format!("{} / {}", pending_seen, pending_size), false));
  menu_lines.push((format!("{}", "-------------"), false));

  let mut body_lines : Vec<String> = vec![];
  let blocks = get_longest_chain(node);
  body_lines.push(format!("#block | time          | hash                                                             | body "));
  body_lines.push(format!("------ | ------------- | ---------------------------------------------------------------- | {}", "-".repeat(std::cmp::max(width as i64 - menu_width as i64 - 93, 0) as usize)));
  let min = std::cmp::max(blocks.len() as i64 - (height as i64 - 5), 0) as usize;
  let max = blocks.len();   
  for i in min .. max {
    body_lines.push(display_block(node, &blocks[i as usize], i as usize, width));
  }

  let mut screen = Vec::new();

  // Draws top bar
  screen.push(format!("  ╻╻           │"));
  screen.push(format!(" ╻┣┓  {} │ {}", bold("Kindelia"), { display_input(&(input.lock().unwrap()).clone()) }));
  screen.push(format!("╺┛╹┗╸──────────┼{}", "─".repeat(width as usize - 16)));

  // Draws each line
  for i in 0 .. height - 4 {
    let mut line = String::new();

    // Draws menu item
    line.push_str(&format!(" "));
    if let Some((menu_line, menu_bold)) = menu_lines.get(i as usize) {
      let text = menu_line.pad(menu_width - 4, ' ', Alignment::Left, true);
      let text = if *menu_bold { bold(&text) } else { text };
      line.push_str(&format!("{}", text));
    } else {
      line.push_str(&format!("{}", " ".repeat(menu_width - 4)));
    }

    // Draws separator
    line.push_str(&format!(" │ "));

    // Draws body item
    line.push_str(&format!("{}", body_lines.get(i as usize).unwrap_or(&"".to_string())));

    // Draws line break
    screen.push(line);
  }

  render(last_screen, &screen, redraw);
}

// Prints screen, only re-printing lines that change
pub fn render(old_screen: &mut Option<Vec<String>>, new_screen: &Vec<String>, redraw: bool) {
  fn redraw(screen: &Vec<String>) {
    print!("{esc}c", esc = 27 as char); // clear screen
    for y in 0 .. screen.len() {
      print!("{}\n\r", screen[y]);
    }
  }
  match old_screen {
    None => redraw(new_screen),
    Some(old_screen) => {
      if old_screen.len() != new_screen.len() {
        redraw(new_screen);
      } else {
        for y in 0 .. new_screen.len() {
          if let (Some(old_line), Some(new_line)) = (old_screen.get(y), new_screen.get(y)) {
            if old_line != new_line {
              print!("{}", termion::cursor::Hide);
              print!("{}", termion::cursor::Goto(1, (y + 1) as u16));
              print!("{}", new_line);
              print!("{}", termion::clear::UntilNewline);
            }
          }
        }
      }
    }
  }
  std::io::stdout().flush().ok();
  *old_screen = Some(new_screen.clone());
}

pub fn node_loop(node: &mut Node, input: &SharedInput, miner_comm: &SharedMinerComm) {
  let mut mined = 0;
  let mut tick = 0;
  let mut last_screen : Option<Vec<String>> = None;

  loop {
    tick = tick + 1;

    // If the miner thread mined a block, gets and registers it
    if let MinerComm::Answer { block } = read_miner_comm(miner_comm) {
      mined += 1;
      node_add_block(node, &block);
    }

    // Asks the miner to mine a block
    if tick % 3 == 0 {
      node_ask_mine(node, input, miner_comm);
    }

    // Receives and handles incoming messages
    if tick % 3 == 0 {
      node_message_receive(node);
    }

    // Spreads the tip block
    if tick % 3 == 0 {
      node_gossip_tip_block(node, 8);
    }

    // Requests missing blocks
    if tick % 3 == 0 {
      node_ask_missing_blocks(node);
    }

    // Peer timeout
    // FIXME: should be less frequent
    if tick % 60 == 0 {
      node_peers_timeout(node);
    }

    // Displays the UI
    if tick % 2 == 0 {
      node_display(node, input, &mut last_screen, tick % 120 == 0);
    }

    // Sleep for 1/60 seconds
    std::thread::sleep(std::time::Duration::from_micros(16666));
  }
}
