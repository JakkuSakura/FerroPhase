#!/usr/bin/env fp run
//! Async HTTP server using std::net and std::task.
//!
//! Listens on 127.0.0.1:8081 and responds with a fixed text body.

use std::net::addr::SocketAddr;
use std::net::TcpListener;
use std::net::TcpStream;

const HTTP_HEADER: &str = "HTTP/1.1 200 OK\r\nContent-Type: text/plain; charset=utf-8\r\nContent-Length: 32\r\nConnection: close\r\n\r\n";
const HTTP_BODY: &str = "Hello from FP async HTTP server\n";

async fn handle_client(mut stream: TcpStream) {
    let mut buf: [u8; 1024] = [0; 1024];
    let _ = stream.read(&mut buf).await;
    let _ = stream.write(HTTP_HEADER).await;
    let _ = stream.write(HTTP_BODY).await;
    let _ = stream.shutdown().await;
}

async fn main() {
    println!("Example: 39_async_http_server.fp");
    println!("Focus: async std::net TCP server responding with HTTP");

    let addr = SocketAddr::new("127.0.0.1", 8081);
    let mut listener = TcpListener::bind(addr).await;
    println!("listening on 127.0.0.1:8081");

    loop {
        let stream = listener.accept().await;
        handle_client(stream).await;
    }
}
