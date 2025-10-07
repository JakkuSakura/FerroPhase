#!/usr/bin/env fp run
//! Showcase compile-time Vec and HashMap construction.
//! Demonstrates literal, repeat, and const-block usage.

use std::collections::HashMap;

const PRIME_COUNT: i64 = const {
    let primes = vec![2, 3, 5, 7, 11, 13];
    primes.len() as i64
};

const ZERO_BUFFER_CAPACITY: i64 = const {
    let zeros = vec![0; 16];
    zeros.len() as i64
};

const HTTP_STATUS_COUNT: i64 = const {
    let statuses = HashMap::from([
        ("ok", 200),
        ("created", 201),
        ("accepted", 202),
        ("not_found", 404),
    ]);

    statuses.len() as i64
};

fn main() {
    println!("=== Compile-time Collections ===");

    println!("Vec literals:");
    println!("  primes: {} elements", PRIME_COUNT);
    println!("  zero buffer: {} elements", ZERO_BUFFER_CAPACITY);

    println!("\nHashMap literal via HashMap::from:");
    println!("  tracked HTTP statuses: {} entries", HTTP_STATUS_COUNT);

}
