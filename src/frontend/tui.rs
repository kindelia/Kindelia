// TODO: improve input UX
// TODO: migrate to `tui-rs`

use std::io::{stdin, stdout, Write};
use std::sync::{Arc, Mutex};

use pad::{Alignment, PadStr};
use termion::event::Key;
use termion::input::TermRead;
use termion::raw::IntoRawMode;

use crate::algorithms::*;
use crate::api::{self, ClientCommEdge, NodeAns, NodeAsk, NodeInfo};
use crate::types::{Block, Shared};

type SharedInput = Shared<String>;

pub struct TuiFrontend {
  input: Shared<String>,
}

impl TuiFrontend {
  pub fn new() -> TuiFrontend {
    TuiFrontend { input: Arc::new(Mutex::new(String::new())) }
  }
}

impl api::Frontend for TuiFrontend {
  fn get_tasks(&self, comm_edge: ClientCommEdge) -> Vec<Box<dyn FnOnce() + Send + 'static>> {
    let input = self.input.clone();
    let io_task = move || {
      io_loop(comm_edge, input);
    };
    vec![Box::new(io_task)]
  }
}

fn io_loop(comm: ClientCommEdge, input: SharedInput) {
  let mut node_info: Option<NodeInfo>;
  let mut last_screen: Option<Vec<String>> = None;

  // This variable should be bound, even if not used, because of RAII, I guess.
  let raw_out = stdout().into_raw_mode().unwrap();

  let async_in = termion::async_stdin();
  let mut keys_it = async_in.keys();

  loop {
    {
      let keys = keys_it.by_ref().map(|k| k.unwrap()).collect::<Vec<Key>>();
      handle_input(keys, &comm, &input);
    }
    let (tx, rx) = &comm;
    tx.send(NodeAsk::Info { max_last_blocks: Some(0) }).unwrap();
    match rx.recv() {
      Err(err) => panic!("Error: {}", err),
      Ok(msg) => match msg {
        NodeAns::NodeInfo(info) => {
          node_info = Some(info);
        }
      },
    }
    if let Some(info) = &node_info {
      let input = { input.lock().unwrap().clone() };
      display_tui(info, &input, &mut last_screen);
    }
    std::thread::sleep(std::time::Duration::from_micros(16666));
  }
}

pub fn handle_input(keys: Vec<Key>, comm: &ClientCommEdge, input: &SharedInput) {
  use termion::event::Key;
  for key in keys {
    match key {
      Key::Ctrl(chr) => match chr {
        'c' | 'q' | 'e' => {
          std::process::exit(0);
        }
        _ => (),
      },
      Key::Char(chr) => {
        if chr == '\n' {
          let txt = {
            let mut input = input.lock().unwrap();
            let txt = input.clone();
            *input = "".to_string();
            txt
          };
          let (tx, _) = comm;
          tx.send(NodeAsk::ToMine(Box::new(string_to_body(&txt)))).unwrap();
        } else {
          {
            let mut input = input.lock().unwrap();
            input.push(chr);
          }
        }
        // FIXME:: is this necessary?
        // stdout.flush().unwrap();
      }
      Key::Backspace => {
        {
          let mut input = input.lock().unwrap();
          input.pop();
        }
        // stdout.flush().unwrap();
      }
      _ => (),
    }
  }
}

#[allow(clippy::useless_format)]
pub fn display_tui(info: &NodeInfo, input: &str, last_screen: &mut Option<Vec<String>>) {
  //─━│┃┄┅┆┇┈┉┊┋┌┍┎┏┐┑┒┓└┕┖┗┘┙┚┛├┝┞┟┠┡┢┣┤┥┦┧┨┩┪┫┬┭┮┯┰┱┲┳┴┵┶┷┸┹┺┻┼┽┾┿╀╁╂╃╄╅╆╇╈╉╊╋╌╍╎╏═║╒╓╔╕╖╗╘╙╚╛╜╝╞╟╠╡╢╣╤╥╦╧╨╩╪╫╬╭╮╯╰╱╲╳╴╵╶╷╸╹╺╻╼╽╾

  fn bold(text: &str) -> String {
    return format!("{}{}{}", "\x1b[1m", text, "\x1b[0m");
  }

  fn display_block(block: &Block, index: usize, width: u16) -> String {
    let zero = u256(0);
    let b_hash = hash_block(block);
    let show_index = format!("{}", index).pad(6, ' ', Alignment::Right, true);
    let show_time = format!("{}", block.time).pad(13, ' ', Alignment::Left, true);
    let show_hash = format!("{}", hex::encode(u256_to_bytes(b_hash)));
    let show_body = format!(
      "{}",
      body_to_string(&block.body).pad(
        std::cmp::max(width as i32 - 110, 0) as usize,
        ' ',
        Alignment::Left,
        true
      )
    );
    return format!("{} | {} | {} | {}", show_index, show_time, show_hash, show_body);
  }

  fn display_input(input: &str) -> String {
    let mut input: String = input.to_string();
    if let Some('\n') = input.chars().last() {
      input = bold(&input);
    }
    input.replace('\n', "")
  }

  // Gets the terminal width and height
  let (width, height) = termion::terminal_size().unwrap();
  let menu_width = 17;

  let mut menu_lines: Vec<(String, bool)> = vec![];
  macro_rules! menu_block {
    ($title:expr, $val:expr) => {
      menu_lines.push((format!("{}", $title), true));
      menu_lines.push((format!("{}", $val), false));
      menu_lines.push((format!("{}", "-------------"), false));
    };
  }

  menu_block!("Time", get_time());
  menu_block!("Peers", info.num_peers);
  menu_block!("Difficulty", info.difficulty);
  menu_block!("Hash Rate", info.hash_rate);
  menu_block!("Blocks", info.num_blocks);
  menu_block!("Height", info.height);
  menu_block!("Pending", format!("{} / {}", info.num_pending_seen, info.num_pending));

  let mut body_lines: Vec<String> = vec![];
  let blocks = &info.last_blocks;
  body_lines.push(format!("#block | time          | hash                                                             | body "));
  body_lines.push(format!("------ | ------------- | ---------------------------------------------------------------- | {}", "-".repeat(std::cmp::max(width as i64 - menu_width as i64 - 93, 0) as usize)));
  let min = std::cmp::max(blocks.len() as i64 - (height as i64 - 5), 0) as usize;
  let max = blocks.len();
  for i in min..max {
    body_lines.push(display_block(&blocks[i as usize], i as usize, width));
  }

  let mut screen = Vec::new();

  // Draws top bar
  screen.push(format!("  ╻╻           │"));
  screen.push(format!(" ╻┣┓  {} │ {}", bold("Kindelia"), { display_input(input) }));
  screen.push(format!("╺┛╹┗╸──────────┼{}", "─".repeat(width as usize - 16)));

  // Draws each line
  for i in 0..height - 4 {
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

  render(last_screen, &screen);
}

// Prints screen, only re-printing lines that change
fn render(old_screen: &mut Option<Vec<String>>, new_screen: &Vec<String>) {
  fn redraw(screen: &Vec<String>) {
    print!("{esc}c", esc = 27 as char); // clear screen
    for line in screen {
      print!("{}\n\r", line);
    }
  }
  match old_screen {
    None => redraw(new_screen),
    Some(old_screen) => {
      if old_screen.len() != new_screen.len() {
        redraw(new_screen);
      } else {
        for y in 0..new_screen.len() {
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
