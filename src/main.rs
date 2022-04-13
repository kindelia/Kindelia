#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(non_snake_case)]
#![allow(unused_variables)]

mod algorithms;
mod constants;
mod network;
mod node;
mod serializer;
mod types;

use crate::algorithms::*;
use crate::constants::*;
use crate::network::*;
use crate::serializer::*;
use crate::types::*;

use primitive_types::U256;

fn node_main_loop(socket: &mut std::net::UdpSocket, port: u16) {
  loop {
    let got_msgs = udp_receive(socket);
    println!("- got: {:?}", got_msgs);

    for i in 0 .. 2 {
      let addr = ipv4(127, 0, 0, 1, 42000 + i);
      let msge = Message::AskBlock { bhash: u256(port as u64) };
      udp_send(socket, addr, &msge);
    }

    std::thread::sleep(std::time::Duration::from_millis(100));
  }
}

fn main() {
  let buff = [100, 101, 102];
  std::fs::create_dir_all("./foo/bar").ok();
  std::fs::write("./foo/bar/test", &buff).ok();

  //let (mut socket, port) = udp_init(&[42000, 42001, 42002, 42003]).unwrap();
  //node_main_loop(&mut socket, port);
}
