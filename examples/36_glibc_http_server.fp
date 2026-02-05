#!/usr/bin/env fp run
//! Minimal TCP HTTP server using glibc sockets.
//!
//! Linux-only (glibc) and intended for `fp interpret`.

struct InAddr {
    s_addr: u32,
}

struct SockAddrIn {
    sin_family: u16,
    sin_port: u16,
    sin_addr: InAddr,
    sin_zero0: u32,
    sin_zero1: u32,
}

mod libc {
    extern "C" fn socket(domain: i32, sock_type: i32, protocol: i32) -> i32;
    extern "C" fn bind(fd: i32, addr: *const SockAddrIn, addrlen: u32) -> i32;
    extern "C" fn listen(fd: i32, backlog: i32) -> i32;
    extern "C" fn accept(fd: i32, addr: *mut SockAddrIn, addrlen: *mut u32) -> i32;
    extern "C" fn write(fd: i32, buf: *const u8, count: usize) -> i64;
    extern "C" fn close(fd: i32) -> i32;
    extern "C" fn htons(hostshort: u16) -> u16;
}

const AF_INET: i32 = 2;
const SOCK_STREAM: i32 = 1;
const INADDR_ANY: u32 = 0;
const SOCKADDR_LEN: u32 = 16;
const RESPONSE_LEN: usize = 111;
const RESPONSE: str =
    "HTTP/1.1 200 OK\r\nContent-Length: 12\r\nContent-Type: text/plain; charset=utf-8\r\nConnection: close\r\n\r\nHello world\n";

fn make_addr(port: u16) -> SockAddrIn {
    SockAddrIn {
        sin_family: AF_INET as u16,
        sin_port: libc::htons(port),
        sin_addr: InAddr { s_addr: INADDR_ANY },
        sin_zero0: 0,
        sin_zero1: 0,
    }
}

fn main() {
    let server_fd = libc::socket(AF_INET, SOCK_STREAM, 0);
    if server_fd < 0 {
        println!("socket failed");
        return;
    }

    let addr = make_addr(8080);
    let addr_ptr = (&addr as *const SockAddrIn);
    let bind_rc = libc::bind(server_fd, addr_ptr, SOCKADDR_LEN);
    if bind_rc < 0 {
        println!("bind failed");
        libc::close(server_fd);
        return;
    }

    let listen_rc = libc::listen(server_fd, 16);
    if listen_rc < 0 {
        println!("listen failed");
        libc::close(server_fd);
        return;
    }

    println!("listening on 0.0.0.0:8080");

    loop {
        let mut client_addr = make_addr(0);
        let mut client_len: u32 = SOCKADDR_LEN;
        let client_addr_ptr = (&mut client_addr as *mut SockAddrIn);
        let client_len_ptr = (&mut client_len as *mut u32);
        let client_fd = libc::accept(server_fd, client_addr_ptr, client_len_ptr);
        if client_fd < 0 {
            println!("accept failed");
            continue;
        }

        let response_ptr = (RESPONSE as *const u8);
        let _ = libc::write(client_fd, response_ptr, RESPONSE_LEN);
        libc::close(client_fd);
    }
}
