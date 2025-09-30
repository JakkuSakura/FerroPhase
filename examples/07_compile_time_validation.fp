#!/usr/bin/env fp run
//! Compile-time validation using const expressions and introspection

fn main() {
    struct Data {
        a: i64,
        b: i64,
        c: [u8; 16],
    }

    // Introspection macros
    const SIZE: usize = sizeof!(Data);
    const FIELDS: usize = field_count!(Data);
    const HAS_A: bool = hasfield!(Data, "a");
    const HAS_X: bool = hasfield!(Data, "x");

    println!("sizeof={}, fields={}", SIZE, FIELDS);
    println!("has_a={}, has_x={}", HAS_A, HAS_X);

    // Const validation
    const MAX_SIZE: usize = 64;
    const SIZE_OK: bool = SIZE <= MAX_SIZE;
    const IS_ALIGNED: bool = SIZE % 8 == 0;

    println!("size_ok={}, aligned={}", SIZE_OK, IS_ALIGNED);

    // Compile-time branch based on validation
    const MODE: &str = if SIZE_OK && IS_ALIGNED {
        "optimized"
    } else {
        "default"
    };
    println!("mode: {}", MODE);
}