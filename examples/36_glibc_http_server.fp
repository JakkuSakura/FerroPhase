#!/usr/bin/env fp run
//! Minimal TCP HTTP server using glibc sockets.
//!
//! Linux-only (glibc) and intended for `fp interpret`.

struct SockAddrIn {
    sin_family: u16,
    sin_port: u16,
    sin_addr: u32,
    sin_zero: [u8; 8],
}

extern "C" fn socket(domain: i32, sock_type: i32, protocol: i32) -> i32;
extern "C" fn bind(fd: i32, addr: &SockAddrIn, addrlen: u32) -> i32;
extern "C" fn listen(fd: i32, backlog: i32) -> i32;
extern "C" fn accept(fd: i32, addr: &mut SockAddrIn, addrlen: &mut u32) -> i32;
extern "C" fn read(fd: i32, buf: &u8, count: usize) -> i64;
extern "C" fn write(fd: i32, buf: &u8, count: usize) -> i64;
extern "C" fn close(fd: i32) -> i32;
extern "C" fn htons(hostshort: u16) -> u16;

const AF_INET: i32 = 2;
const SOCK_STREAM: i32 = 1;
const INADDR_ANY: u32 = 0;
const SOCKADDR_LEN: u32 = 16;
const RESPONSE_LEN: usize = 70;

const RESPONSE: [u8; 70] = [
    72, 84, 84, 80, 47, 49, 46, 49, 32, 50, 48, 48,
    32, 79, 75, 13, 10, 67, 111, 110, 116, 101, 110, 116,
    45, 76, 101, 110, 103, 116, 104, 58, 32, 49, 50, 13,
    10, 67, 111, 110, 110, 101, 99, 116, 105, 111, 110, 58,
    32, 99, 108, 111, 115, 101, 13, 10, 13, 10, 72, 101,
    108, 108, 111, 32, 119, 111, 114, 108, 100, 10,
];

fn make_addr(port: u16) -> SockAddrIn {
    SockAddrIn {
        sin_family: AF_INET as u16,
        sin_port: htons(port),
        sin_addr: INADDR_ANY,
        sin_zero: [0; 8],
    }
}

fn main() {
    let server_fd = socket(AF_INET, SOCK_STREAM, 0);
    if server_fd < 0 {
        println!("socket failed");
        return;
    }

    let addr = make_addr(8080);
    if bind(server_fd, &addr, SOCKADDR_LEN) < 0 {
        println!("bind failed");
        close(server_fd);
        return;
    }

    if listen(server_fd, 16) < 0 {
        println!("listen failed");
        close(server_fd);
        return;
    }

    println!("listening on 0.0.0.0:8080");

    loop {
        let mut client_addr = make_addr(0);
        let mut client_len: u32 = SOCKADDR_LEN;
        let client_fd = accept(server_fd, &mut client_addr, &mut client_len);
        if client_fd < 0 {
            println!("accept failed");
            continue;
        }

        let mut buffer: [u8; 1024] = [0; 1024];
        let _ = read(client_fd, &buffer[0], 1024);
        let _ = write(client_fd, &RESPONSE[0], RESPONSE_LEN);
        close(client_fd);
    }
}
