use bit_vec::BitVec;
use std::net::*;

use crate::algorithms::*;
use crate::serializer::*;
use crate::types::*;

// UDP
// ===

pub fn ipv4(val0: u8, val1: u8, val2: u8, val3: u8, port: u16) -> Address {
  Address::IPv4 { val0, val1, val2, val3, port }
}

pub fn udp_init(ports: &[u16]) -> Option<(UdpSocket,u16)> {
  for port in ports {
    if let Ok(socket) = UdpSocket::bind(&format!("127.0.0.1:{}",port)) {
      socket.set_nonblocking(true).ok();
      return Some((socket, *port));
    }
  }
  return None;
}

pub fn udp_send(socket: &mut UdpSocket, address: Address, message: &Message) {
  match address {
    Address::IPv4 { val0, val1, val2, val3, port } => {
      let bits = bitvec_to_bytes(&serialized_message(message));
      let addr = SocketAddrV4::new(Ipv4Addr::new(val0, val1, val2, val3), port);
      socket.send_to(bits.as_slice(), addr).ok();
    }
  }
}

pub fn udp_receive(socket: &mut UdpSocket) -> Vec<(Address, Message)> {
  let mut buffer = [0; 65536];
  let mut messages = Vec::new();
  while let Ok((msg_len, sender_addr)) = socket.recv_from(&mut buffer) {
    let bits = BitVec::from_bytes(&buffer[0 .. msg_len]);
    let msge = deserialized_message(&bits);
    let addr = match sender_addr.ip() {
      std::net::IpAddr::V4(v4addr) => {
        let [val0, val1, val2, val3] = v4addr.octets();
        Address::IPv4 { val0, val1, val2, val3, port: sender_addr.port() }
      }
      _ => {
        panic!("TODO: IPv6")
      }
    };
    messages.push((addr, msge));
  }
  return messages;
}
