#!/usr/bin/env fp run
//! Minimal rodata + external call coverage for ELF PIE output.

fn sum(values: &[i64; 4]) -> i64 {
    let mut idx = 0;
    let mut total = 0;
    while idx < 4 {
        total += values[idx];
        idx += 1;
    }
    total
}

fn main() {
    let banner = "ELF PIE rodata check";
    let values: [i64; 4] = [10, 20, 30, 40];
    let total = sum(&values);
    println!("{}: sum={}", banner, total);
}
