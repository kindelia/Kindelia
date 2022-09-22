use std::net::{Ipv4Addr, SocketAddrV4, UdpSocket};

use bit_vec::BitVec;

use crate::{
  bits::{deserialized_message, serialized_message, ProtoSerialize},
  node::{Address, Message},
  util::bitvec_to_bytes,
};

// Addresses
pub trait ProtoCommAddress
where
  Self: ProtoSerialize
    + Eq
    + std::hash::Hash
    + std::fmt::Debug
    + std::fmt::Display
    + Copy
    + Clone
    + PartialEq
    + Eq,
{
}

// Addresses impl
impl ProtoCommAddress for Address {}

// Communication
pub trait ProtoComm
where
  Self: Sized,
{
  type Address: ProtoCommAddress;
  fn send(
    &mut self,
    addresses: Vec<Self::Address>,
    message: &Message<Self::Address>,
  );
  fn recv(&mut self) -> Vec<(Self::Address, Message<Self::Address>)>;
  // fn get_addr(&self) -> Self::Address;
}

// Communication impl
impl ProtoComm for UdpSocket {
  type Address = Address;
  fn send(
    &mut self,
    addresses: Vec<Self::Address>,
    message: &Message<Self::Address>,
  ) {
    let bytes = bitvec_to_bytes(&serialized_message(message));
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
  fn recv(&mut self) -> Vec<(Self::Address, Message<Self::Address>)> {
    let mut buffer = [0; 65536];
    let mut messages = Vec::new();
    while let Ok((msg_len, sender_addr)) = self.recv_from(&mut buffer) {
      let bits = BitVec::from_bytes(&buffer[0..msg_len]);
      if let Some(msge) = deserialized_message(&bits) {
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
}
