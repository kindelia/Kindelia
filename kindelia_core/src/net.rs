use std::fmt::{Debug, Display};
use std::hash::Hash;

use serde;
use thiserror::Error;

use crate::bits::ProtoSerialize;
use crate::node::Message;

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

/// Errors associated with the ProtoComm trait.
///
/// Source error types are dynamic to enable trait implementors
/// to provide their own error type if needed.
#[derive(Error, Debug)]
pub enum ProtoCommError {
  #[error("Address is unavailable")]
  AddrUnavailable { source: Box<dyn std::error::Error + Send + Sync + 'static> },
  #[error(transparent)]
  Other(#[from] Box<dyn std::error::Error + Send + Sync + 'static>),
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
  fn get_addr(&self) -> Result<Self::Address, ProtoCommError>;
}

pub use udp::{parse_address, Address, UDP_PORT};
/// UDP implementation for `ProtoComm`
mod udp {
  use std::{
    net::{Ipv4Addr, SocketAddrV4, UdpSocket},
    num::ParseIntError,
  };

  use bit_vec::BitVec;
  use thiserror::Error;

  use crate::{bits::ProtoSerialize, node::Message, util::bitvec_to_bytes};

  use super::{ProtoAddr, ProtoComm, ProtoCommError};

  /// Default UDP port to listen to.
  pub const UDP_PORT: u16 = 42000;

  /// An UDP address representation.
  #[derive(
    Debug,
    Copy,
    Clone,
    PartialEq,
    Eq,
    Hash,
    serde::Serialize,
    serde::Deserialize,
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

  #[derive(Error, Debug)]
  pub enum ParseAddressError {
    #[error("Invalid IP Address '{value:?}'")]
    InvalidAddress { value: String, source: ParseIntError },
    #[error("Invalid Port '{value:?}'")]
    InvalidPort { value: String, source: ParseIntError },
  }

  /// Converts a string to an UDP Address.
  /// TODO: UNSAFE.
  pub fn parse_address(code: &str) -> Result<Address, ParseAddressError> {
    let strs = code.split(':').collect::<Vec<&str>>();
    let vals = strs[0]
      .split('.')
      .map(|o| {
        o.parse::<u8>().map_err(|e| ParseAddressError::InvalidAddress {
          value: strs[0].to_string(),
          source: e,
        })
      })
      .collect::<Result<Vec<u8>, ParseAddressError>>()?;
    let port = match strs.get(1) {
      Some(s) => {
        s.parse::<u16>().map_err(|e| ParseAddressError::InvalidAddress {
          value: strs[0].to_string(),
          source: e,
        })?
      }
      None => UDP_PORT,
    };
    Ok(Address::IPv4 {
      val0: vals[0],
      val1: vals[1],
      val2: vals[2],
      val3: vals[3],
      port,
    })
  }

  /// The UDP implementation based on `std::net::UdpSocket` struct
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
              unimplemented!("TODO: IPv6")
            }
          };
          messages.push((addr, msge));
        }
      }
      messages
    }
    fn get_addr(&self) -> Result<Self::Address, ProtoCommError> {
      // TODO: impl IPV6
      let addr = self
        .local_addr()
        .map_err(|e| ProtoCommError::AddrUnavailable { source: Box::new(e) })?;
      if let std::net::IpAddr::V4(v4addr) = addr.ip() {
        let [val0, val1, val2, val3] = v4addr.octets();
        Ok(Address::IPv4 { val0, val1, val2, val3, port: addr.port() })
      } else {
        unimplemented!("TODO: IPv6")
      }
    }
  }
}

pub use empty::{EmptyAddress, EmptySocket};
/// Created for testing purposes, it is a mock representation for `ProtoComm`
mod empty {
  use bit_vec::BitVec;

  use crate::{bits::ProtoSerialize, node::Message};

  use super::{ProtoAddr, ProtoComm, ProtoCommError};

  pub struct EmptySocket;

  /// Created for testing purposes
  #[derive(Eq, Hash, Debug, Copy, Clone, PartialEq, serde::Serialize)]
  pub struct EmptyAddress;

  impl std::fmt::Display for EmptyAddress {
    fn fmt(&self, _: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      Ok(())
    }
  }

  impl ProtoSerialize for EmptyAddress {
    fn proto_deserialize(
      _: &BitVec,
      _: &mut usize,
      _: &mut crate::bits::Names,
    ) -> Option<Self> {
      Some(EmptyAddress)
    }
    fn proto_serialize(&self, _: &mut BitVec, _: &mut crate::bits::Names) {}
  }

  impl ProtoAddr for EmptyAddress {}

  impl ProtoComm for EmptySocket {
    type Address = EmptyAddress;

    fn get_addr(&self) -> Result<Self::Address, ProtoCommError> {
      Ok(EmptyAddress)
    }
    fn proto_recv(&mut self) -> Vec<(Self::Address, Message<Self::Address>)> {
      vec![]
    }
    fn proto_send(
      &mut self,
      _addresses: Vec<Self::Address>,
      _message: &Message<Self::Address>,
    ) {
    }
  }
}
