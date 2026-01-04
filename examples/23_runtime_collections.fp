#!/usr/bin/env fp run
//! Showcase runtime Vec/HashMap access without a runtime std.
//! Demonstrates list indexing, map lookup, and len() in runtime code.

use std::collections::HashMap;

fn main() {
    println!("📘 Tutorial: 23_runtime_collections.fp");
    println!("🧭 Focus: runtime list/map access with linear search.");
    println!("🧪 What to look for: values resolved from container accesses");
    println!("✅ Expectation: outputs match labels");
    println!("");
    println!("=== Runtime Collections ===");

    let numbers: Vec<i64> = [10, 20, 30, 40];
    let idx = 2;
    println!("numbers[{}] = {}", idx, numbers[idx]);
    println!("numbers.len = {}", numbers.len());

    let statuses: HashMap<&'static str, i64> = HashMap::from([
        ["ok", 200],
        ["accepted", 202],
        ["nope", 404],
    ]);
    let key = "accepted";
    println!("statuses[{}] = {}", key, statuses[key]);
    println!("statuses.len = {}", statuses.len());
}
