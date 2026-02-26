#!/usr/bin/env fp run
//! HTTP server using libc sockets (compiled-mode friendly).

mod libc {
    extern "C" fn socket(domain: i32, sock_type: i32, protocol: i32) -> i32;
    extern "C" fn setsockopt(fd: i32, level: i32, optname: i32, optval: *const u8, optlen: u32) -> i32;
    extern "C" fn bind(fd: i32, addr: *const u8, addrlen: u32) -> i32;
    extern "C" fn listen(fd: i32, backlog: i32) -> i32;
    extern "C" fn accept(fd: i32, addr: *mut u8, addrlen: *mut u32) -> i32;
    extern "C" fn write(fd: i32, buf: &std::ffi::CStr, count: usize) -> i64;
    extern "C" fn strlen(s: &std::ffi::CStr) -> usize;
    extern "C" fn close(fd: i32) -> i32;
}

const AF_INET: i32 = 2;
const SOCK_STREAM: i32 = 1;
const SOL_SOCKET: i32 = 1;
const SO_REUSEADDR: i32 = 2;
const SOCKADDR_LEN: u32 = 16;
const RESPONSE: &std::ffi::CStr =
    "HTTP/1.1 200 OK\r\nContent-Length: 3\r\nConnection: close\r\n\r\nOK\n";

#[cfg(target_os = "linux")]
fn make_addr(port_hi: u8, port_lo: u8) -> [u8; 16] {
    [
        2, 0,
        port_hi, port_lo,
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0,
    ]
}

#[cfg(target_os = "macos")]
fn make_addr(port_hi: u8, port_lo: u8) -> [u8; 16] {
    [
        SOCKADDR_LEN as u8, AF_INET as u8,
        port_hi, port_lo,
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0,
    ]
}

fn main() {
    let fd = libc::socket(AF_INET, SOCK_STREAM, 0);
    if fd < 0 {
        return;
    }

    let reuse: i32 = 1;
    let reuse_ptr = &reuse as *const i32;
    let _ = libc::setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, reuse_ptr as *const u8, 4);

    let mut addr = make_addr(31, 145);
    let addr_ptr = &mut addr as *mut [u8; 16];
    if libc::bind(fd, addr_ptr as *const u8, SOCKADDR_LEN) != 0 {
        let _ = libc::close(fd);
        return;
    }
    if libc::listen(fd, 128) != 0 {
        let _ = libc::close(fd);
        return;
    }

    println!("listening on 127.0.0.1:8081");

    loop {
        let mut peer = make_addr(0, 0);
        let mut peer_len: u32 = SOCKADDR_LEN;
        let peer_ptr = &mut peer as *mut [u8; 16];
        let peer_len_ptr = &mut peer_len as *mut u32;
        let client = libc::accept(fd, peer_ptr as *mut u8, peer_len_ptr);
        if client < 0 {
            continue;
        }

        let response_len = libc::strlen(RESPONSE);
        let _ = libc::write(client, RESPONSE, response_len);
        let _ = libc::close(client);
    }
}
