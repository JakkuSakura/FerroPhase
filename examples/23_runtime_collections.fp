#!/usr/bin/env fp interpret
//! Showcase runtime Vec/HashMap access without a runtime std.
//! Demonstrates list indexing, map lookup, and len() in runtime code.

use std::collections::HashMap;
use std::collections::HashMapEntry;

fn main() {
    println!("📘 Tutorial: 23_runtime_collections.fp");
    println!("🧭 Focus: runtime list/map access with linear search.");
    println!("🧪 What to look for: values resolved from container accesses");
    println!("✅ Expectation: outputs match labels");
    println!("");
    println!("=== Runtime Collections ===");

    let numbers: Vec<i64> = vec![10, 20, 30, 40];
    let idx = 2;
    println!("numbers[{}] = {}", idx, numbers[idx]);
    println!("numbers.len = {}", numbers.len());

    let statuses: HashMap<&str, i64> = HashMap::from([
        HashMapEntry { key: "ok", value: 200 },
        HashMapEntry { key: "accepted", value: 202 },
        HashMapEntry { key: "nope", value: 404 },
    ]);
    let key = "accepted";
    println!("statuses[{}] = {}", key, statuses[key]);
    println!("statuses.len = {}", statuses.len());
}
