#!/usr/bin/env fp run
//! FFI basics: call libc strlen via extern "C" declarations.

extern "C" fn strlen(s: &std::ffi::CStr) -> i64;

fn main() {
    let input = "hello from ffi";
    let length = strlen(input);
    println!("strlen('{}') = {}", input, length);
}
