#!/usr/bin/env fp interpret
//! Minimal host-derived struct and host static syntax example.

#[derive(Host)]
#[repr(C)]
struct HostHandle {
    raw: usize,
}

#[host]
static HOST_HANDLE: HostHandle = HostHandle { raw: 0 };

#[host]
static mut HOST_STATE: HostHandle = HostHandle { raw: 1 };

fn read_host_handle() -> usize {
    HOST_HANDLE.raw
}

fn main() {
    println!("host handle raw = {}", read_host_handle());
    println!("host state raw = {}", HOST_STATE.raw);
}
