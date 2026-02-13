#!/usr/bin/env fp run
//! Async TCP echo server using std::net and std::task.

use std::net::addr::SocketAddr;
use std::net::TcpListener;
use std::net::TcpStream;

async fn handle_client(mut stream: TcpStream) {
    let mut buf: [u8; 1024] = [0; 1024];
    loop {
        let n = stream.read(&mut buf).await;
        if n <= 0 {
            break;
        }
        let _ = stream.write(&buf[0..n as usize]).await;
    }
}

async fn main() {
    println!("Example: 37_async_tcp_echo.fp");
    println!("Focus: async std::net TCP + std::task::spawn");

    let addr = SocketAddr::new("127.0.0.1", 9090);
    let mut listener = TcpListener::bind(addr).await;
    println!("listening on 127.0.0.1:9090");

    loop {
        let stream = listener.accept().await;
        let _task = std::task::spawn(handle_client(stream));
    }
}
