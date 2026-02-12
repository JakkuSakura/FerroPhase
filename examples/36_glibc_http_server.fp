#!/usr/bin/env fp run
//! Minimal TCP HTTP server using libc sockets.
//!
//! Linux (glibc) and macOS only, intended for `fp interpret`.

type SockAddrIn = [u8; 16];

mod libc {
    extern "C" fn socket(domain: i32, sock_type: i32, protocol: i32) -> i32;
    extern "C" fn setsockopt(
        fd: i32,
        level: i32,
        optname: i32,
        optval: *const u8,
        optlen: u32,
    ) -> i32;
    extern "C" fn bind(fd: i32, addr: *const u8, addrlen: u32) -> i32;
    extern "C" fn listen(fd: i32, backlog: i32) -> i32;
    extern "C" fn accept(fd: i32, addr: *mut u8, addrlen: *mut u32) -> i32;
    extern "C" fn write(fd: i32, buf: &std::ffi::CStr, count: usize) -> i64;
    extern "C" fn strlen(s: &std::ffi::CStr) -> usize;
    extern "C" fn perror(s: &std::ffi::CStr);
    extern "C" fn close(fd: i32) -> i32;
}

const AF_INET: i32 = 2;
const SOCK_STREAM: i32 = 1;
const SOL_SOCKET: i32 = 1;
const SO_REUSEADDR: i32 = 2;
const SOCKADDR_LEN: u32 = 16;
const RESPONSE: &std::ffi::CStr =
    "HTTP/1.1 200 OK\r\nContent-Length: 12\r\nContent-Type: text/plain; charset=utf-8\r\nConnection: close\r\n\r\nHello world\n";
const SOCKET_ERR: &std::ffi::CStr = "socket";
const BIND_ERR: &std::ffi::CStr = "bind";
const LISTEN_ERR: &std::ffi::CStr = "listen";
const ACCEPT_ERR: &std::ffi::CStr = "accept";
#[cfg(target_os = "linux")]
fn make_addr(port: u16) -> SockAddrIn {
    [
        AF_INET as u8, 0,
        ((port >> 8) & 0xff) as u8, (port & 0xff) as u8,
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0,
    ]
}

#[cfg(target_os = "macos")]
fn make_addr(port: u16) -> SockAddrIn {
    [
        SOCKADDR_LEN as u8, AF_INET as u8,
        ((port >> 8) & 0xff) as u8, (port & 0xff) as u8,
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0,
    ]
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
fn main() {
    let server_fd = libc::socket(AF_INET, SOCK_STREAM, 0) as i32;
    println!("socket fd: {}", server_fd);
    if server_fd < 0 as i32 {
        libc::perror(SOCKET_ERR);
        return;
    }

    let reuse: i32 = 1;
    let reuse_ptr = &reuse as *const i32;
    let _ = libc::setsockopt(
        server_fd,
        SOL_SOCKET,
        SO_REUSEADDR,
        reuse_ptr as *const u8,
        4,
    );

    let mut addr = make_addr(8080 as u16);
    let addr_ptr = &mut addr as *mut SockAddrIn;
    let bind_rc = libc::bind(server_fd, addr_ptr as *const u8, SOCKADDR_LEN);
    println!("bind rc: {}", bind_rc);
    if bind_rc != 0 as i32 {
        libc::perror(BIND_ERR);
        libc::close(server_fd);
        return;
    }

    let listen_rc = libc::listen(server_fd, 16);
    println!("listen rc: {}", listen_rc);
    if listen_rc != 0 as i32 {
        libc::perror(LISTEN_ERR);
    }

    println!("listening on 0.0.0.0:8080");

    loop {
        let mut client_addr = make_addr(0 as u16);
        let mut client_len: u32 = SOCKADDR_LEN;
        let client_addr_ptr = &mut client_addr as *mut SockAddrIn;
        let client_len_ptr = &mut client_len as *mut u32;
        let client_fd = libc::accept(server_fd, client_addr_ptr as *mut u8, client_len_ptr) as i32;
        if client_fd < 0 as i32 {
            libc::perror(ACCEPT_ERR);
            continue;
        }

        let response_len = libc::strlen(RESPONSE);
        let _ = libc::write(client_fd, RESPONSE, response_len);
        libc::close(client_fd);
    }
}

#[cfg(not(any(target_os = "linux", target_os = "macos")))]
fn main() {
    println!("This example requires Linux (glibc) or macOS.");
}
