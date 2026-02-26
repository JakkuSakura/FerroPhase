#!/usr/bin/env fp run
//! UDP echo server using libc sockets (compiled-mode friendly).

mod libc {
    extern "C" fn socket(domain: i32, sock_type: i32, protocol: i32) -> i32;
    extern "C" fn bind(fd: i32, addr: *const u8, addrlen: u32) -> i32;
    extern "C" fn recvfrom(fd: i32, buf: *mut u8, len: usize, flags: i32, addr: *mut u8, addrlen: *mut u32) -> i64;
    extern "C" fn sendto(fd: i32, buf: *const u8, len: usize, flags: i32, addr: *const u8, addrlen: u32) -> i64;
    extern "C" fn close(fd: i32) -> i32;
}

const AF_INET: i32 = 2;
const SOCK_DGRAM: i32 = 2;
const SOCKADDR_LEN: u32 = 16;

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
    let fd = libc::socket(AF_INET, SOCK_DGRAM, 0);
    if fd < 0 {
        return;
    }

    let mut addr = make_addr(35, 131);
    let addr_ptr = &mut addr as *mut [u8; 16];
    if libc::bind(fd, addr_ptr as *const u8, SOCKADDR_LEN) != 0 {
        let _ = libc::close(fd);
        return;
    }

    println!("listening on 127.0.0.1:9091 (UDP)");

    let mut buf: [u8; 1024] = [0; 1024];
    loop {
        let mut peer = make_addr(0, 0);
        let mut peer_len: u32 = SOCKADDR_LEN;
        let peer_ptr = &mut peer as *mut [u8; 16];
        let peer_len_ptr = &mut peer_len as *mut u32;

        let buf_ptr = &mut buf as *mut [u8; 1024];
        let n = libc::recvfrom(
            fd,
            buf_ptr as *mut u8,
            1024,
            0,
            peer_ptr as *mut u8,
            peer_len_ptr,
        );
        if n > 0 {
            let _ = libc::sendto(
                fd,
                buf_ptr as *const u8,
                n as usize,
                0,
                peer_ptr as *const u8,
                peer_len,
            );
        }
    }
}
