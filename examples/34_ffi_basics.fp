#!/usr/bin/env fp interpret
//! FFI basics: call libc strlen via extern "C" declarations.

extern "C" fn strlen(s: &std::ffi::CStr) -> i64;

fn main() {
    let input: &std::ffi::CStr = c"hello from ffi";
    let length = strlen(input);
    println!("strlen('hello from ffi') = {}", length);
}
