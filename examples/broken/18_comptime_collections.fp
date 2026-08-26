#!/usr/bin/env fp interpret
//! Showcase compile-time Vec and HashMap construction.
//! Demonstrates literal, repeat, and const-block usage with real data.

use std::collections::HashMap;
use std::collections::HashMapEntry;

const PRIMES: Vec<i64> = const {
    vec![2, 3, 5, 7, 11, 13]
};

const ZERO_BUFFER: Vec<i64> = const {
    vec![0; 16]
};

const HTTP_STATUSES: HashMap<&str, i64> = const {
    HashMap::from([
        HashMapEntry { key: "ok", value: 200 },
        HashMapEntry { key: "created", value: 201 },
        HashMapEntry { key: "accepted", value: 202 },
        HashMapEntry { key: "not_found", value: 404 },
    ])
};

fn main() {
    println!("📘 Tutorial: 18_comptime_collections.fp");
    println!("🧭 Focus: Showcase compile-time Vec and HashMap construction.");
    println!("🧪 What to look for: labeled outputs below");
    println!("✅ Expectation: outputs match labels");
    println!("");
    println!("=== Compile-time Collections ===");

    println!("Vec literals:");
    println!("  primes: {} elements -> {}", PRIMES.len(), PRIMES);
    println!("  zero buffer: {} elements", ZERO_BUFFER.len());
    println!("  first four zeros: [{}, {}, {}, {}]", ZERO_BUFFER[0], ZERO_BUFFER[1], ZERO_BUFFER[2], ZERO_BUFFER[3]);

    println!("\nHashMap literal via HashMap::from:");
    println!("  tracked HTTP statuses: {} entries", HTTP_STATUSES.len());
    println!("  ok => {}", HTTP_STATUSES["ok"]);
    println!("  not_found => {}", HTTP_STATUSES["not_found"]);
}
