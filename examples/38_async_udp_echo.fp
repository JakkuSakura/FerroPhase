#!/usr/bin/env fp run
//! Async UDP echo server using std::net.

use std::net::addr::SocketAddr;
use std::net::UdpSocket;

async fn main() {
    println!("Example: 38_async_udp_echo.fp");
    println!("Focus: async std::net UDP recv_from/send_to");

    let addr = SocketAddr::new("127.0.0.1", 9091);
    let mut socket = UdpSocket::bind(addr).await;
    println!("listening on 127.0.0.1:9091 (UDP)");

    let mut buf: [u8; 1024] = [0; 1024];
    loop {
        let (n, peer) = socket.recv_from(&mut buf).await;
        if n <= 0 {
            continue;
        }
        let _ = socket.send_to(&buf[0..n as usize], peer).await;
    }
}
