use std::fmt::{Debug, Display};
use std::hash::Hash;
pub use std::net::UdpSocket;
use std::net::{Ipv4Addr, SocketAddrV4};

use bit_vec::BitVec;
use serde;

use crate::bits::ProtoSerialize;
use crate::node::Message;
use crate::util::bitvec_to_bytes;

// Traits
// ======

/// A representation of a ProtoComm address
pub trait ProtoAddr
where
  Self: ProtoSerialize
    + Eq
    + Hash
    + Debug
    + Display
    + Copy
    + Clone
    + PartialEq
    + Send
    + serde::Serialize,
{
}

/// Defines how the messages will be sent and received
/// by the chain nodes.
pub trait ProtoComm
where
  Self: Sized + Send,
{
  type Address: ProtoAddr;
  fn proto_send(
    &mut self,
    addresses: Vec<Self::Address>,
    message: &Message<Self::Address>,
  );
  fn proto_recv(&mut self) -> Vec<(Self::Address, Message<Self::Address>)>;
  fn get_addr(&self) -> Self::Address;
}

// UDP Implementation
// ==================

/// Default UDP port to listen to.
pub const UDP_PORT: u16 = 42000;

/// An UDP address representation.
#[derive(
  Debug, Copy, Clone, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize,
)]
pub enum Address {
  IPv4 { val0: u8, val1: u8, val2: u8, val3: u8, port: u16 },
  // TODO: IPv6
}

impl ProtoAddr for Address {}

impl std::fmt::Display for Address {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Address::IPv4 { val0, val1, val2, val3, port } => f.write_fmt(
        format_args!("{}.{}.{}.{}:{}", val0, val1, val2, val3, port),
      ),
    }
  }
}

/// Converts a string to an UDP Address.
/// TODO: UNSAFE.
pub fn parse_address(code: &str) -> Address {
  let strs = code.split(':').collect::<Vec<&str>>();
  let vals =
    strs[0].split('.').map(|o| o.parse::<u8>().unwrap()).collect::<Vec<u8>>();
  let port = strs.get(1).map(|s| s.parse::<u16>().unwrap()).unwrap_or(UDP_PORT);
  Address::IPv4 {
    val0: vals[0],
    val1: vals[1],
    val2: vals[2],
    val3: vals[3],
    port,
  }
}

/// The UDP implementation based on `std::netUdpSocket` struct
impl ProtoComm for UdpSocket {
  type Address = Address;
  fn proto_send(
    &mut self,
    addresses: Vec<Self::Address>,
    message: &Message<Self::Address>,
  ) {
    let bytes = bitvec_to_bytes(&message.proto_serialized());
    for address in addresses {
      match address {
        Address::IPv4 { val0, val1, val2, val3, port } => {
          let addr =
            SocketAddrV4::new(Ipv4Addr::new(val0, val1, val2, val3), port);
          self.send_to(bytes.as_slice(), addr).ok();
        }
      }
    }
  }
  fn proto_recv(&mut self) -> Vec<(Self::Address, Message<Self::Address>)> {
    let mut buffer = [0; 65536];
    let mut messages = Vec::new();
    while let Ok((msg_len, sender_addr)) = self.recv_from(&mut buffer) {
      let bits = BitVec::from_bytes(&buffer[0..msg_len]);
      if let Some(msge) = Message::proto_deserialized(&bits) {
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
    }
    messages
  }
  fn get_addr(&self) -> Self::Address {
    // TODO: remove unwrap and panic
    let addr = self.local_addr().unwrap();
    if let std::net::IpAddr::V4(v4addr) = addr.ip() {
      let [val0, val1, val2, val3] = v4addr.octets();
      Address::IPv4 { val0, val1, val2, val3, port: addr.port() }
    } else {
      panic!("TODO: IPv6")
    }
  }
}
