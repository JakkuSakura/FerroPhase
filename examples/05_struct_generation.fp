#!/usr/bin/env fp run
//! Struct generation with compile-time conditionals

fn main() {
    // Const flags affect struct initialization
    const FLAG_A: bool = true;
    const FLAG_B: bool = false;

    struct Config {
        x: i64,
        y: i64,
    }

    // Different values based on const conditions
    const CONFIG: Config = Config {
        x: if FLAG_A { 100 } else { 10 },
        y: if FLAG_B { 200 } else { 20 },
    };

    println!("x={}, y={}", CONFIG.x, CONFIG.y);

    // Nested const evaluation
    const SIZE: usize = if FLAG_A { 256 } else { 128 };
    const ITEMS: [i64; SIZE] = [0; SIZE];
    println!("array size: {}", ITEMS.len());
}
