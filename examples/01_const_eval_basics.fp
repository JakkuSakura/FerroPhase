#!/usr/bin/env fp run
//! Basic const evaluation with compile-time arithmetic and const blocks

fn main() {
    println!("📘 Tutorial: 01_const_eval_basics.fp");
    println!("🧭 Focus: Basic const evaluation with compile-time arithmetic and const blocks");
    println!("🧪 What to look for: labeled outputs below");
    println!("✅ Expectation: outputs match labels");
    println!("");
    // Basic compile-time computation
    const BUFFER_SIZE: i64 = 1024 * 4;
    const MAX_CONNECTIONS: i64 = 150;
    const FACTORIAL_5: i64 = 5 * 4 * 3 * 2 * 1;
    const IS_LARGE: bool = BUFFER_SIZE > 2048;

    println!("Buffer: {}KB, factorial(5)={}, large={}",
             BUFFER_SIZE / 1024, FACTORIAL_5, IS_LARGE);

    // Struct with const defaults
    struct Config {
        buffer_size: i64,
        max_connections: i64,
    }

    const DEFAULT_CONFIG: Config = Config {
        buffer_size: BUFFER_SIZE,
        max_connections: MAX_CONNECTIONS,
    };

    println!("Config: {}KB buffer, {} connections",
             DEFAULT_CONFIG.buffer_size / 1024,
             DEFAULT_CONFIG.max_connections);

    // Const blocks: inline compile-time computation
    let runtime_multiplier = 3;
    let optimized_size = const { BUFFER_SIZE * 2 };
    let cache_strategy = const {
        if BUFFER_SIZE > 2048 {
            "large"
        } else {
            "small"
        }
    };
    let total_memory = runtime_multiplier * const { BUFFER_SIZE * MAX_CONNECTIONS };

    println!("Const blocks: size={}, strategy={}, memory={}",
             optimized_size, cache_strategy, total_memory);
}
