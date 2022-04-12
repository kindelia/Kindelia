#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(non_snake_case)]
#![allow(unused_variables)]

mod algorithms;
mod constants;
mod serializer;
mod types;

use crate::algorithms::*;
use crate::constants::*;
use crate::serializer::*;
use crate::types::*;

fn main() {
  serializer::test_serializer_0();
  serializer::test_serializer_1();
  serializer::test_serializer_2();
}

//fn main() -> std::io::Result<()> {
    //let socket = UdpSocket::bind("127.0.0.1:21000")?;
    //loop {
        //// Receives a single datagram message on the socket. If `buf` is too small to hold
        //// the message, it will be cut off.
        //let mut buf = [0; 10];
        //let (amt, src) = socket.recv_from(&mut buf)?;

        //println!("{} {} {:?}", amt, src, buf);

        //// Redeclare `buf` as slice of the received data and send reverse data back to origin.
        ////let buf = &mut buf[..amt];
        ////buf.reverse();
        ////socket.send_to(buf, &src)?;
    //} // the socket is closed here
//}
