#!/usr/bin/env fp run
//! Quote and splice demonstration (parser support TBD)
//! This file documents intended surface usage; the AST-level support exists.

fn first_gt(const xs: [i32], ys: [i32]) -> i32 {
    const {
        for (i, x) in xs.iter().enumerate() {
            // Sugar: emit! { if x > ys[i] { return x; } }
            splice quote stmt {
                if x > ys[i] { return x; }
            };
        }
    }
    0
}

fn main() {
    // Intended usage once parser is wired
    let _ = first_gt([1, 2, 5], [0, 1, 3]);
}

