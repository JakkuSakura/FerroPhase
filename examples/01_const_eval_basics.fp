#!/usr/bin/env fp run
//! Basic const evaluation with compile-time arithmetic and struct defaults

fn main() {
    // Basic compile-time computation
    const BUFFER_SIZE: usize = 1024 * 4;
    println!("Buffer size: {} Bytes", BUFFER_SIZE);
    const MAX_CONNECTIONS: i32 = 100 + 50;
    const TIMEOUT_MS: u64 = 30 * 1000;

    // Complex compile-time expressions
    const FACTORIAL_5: i64 = 5 * 4 * 3 * 2 * 1;
    const IS_POWER_OF_TWO: bool = (BUFFER_SIZE & (BUFFER_SIZE - 1)) == 0;

    // Conditional compilation
    const DEBUG_LEVEL: i32 = if cfg!(debug_assertions) { 2 } else { 0 };

    // Struct with const-computed defaults
    struct Config {
        buffer_size: usize,
        max_connections: i32,
        timeout_ms: u64,
        debug_level: i32,
    }

    const DEFAULT_CONFIG: Config = Config {
        buffer_size: BUFFER_SIZE,
        max_connections: MAX_CONNECTIONS,
        timeout_ms: TIMEOUT_MS,
        debug_level: DEBUG_LEVEL,
    };

    println!(
        "Config: buffer={}KB, connections={}, timeout={}ms, debug={}",
        DEFAULT_CONFIG.buffer_size / 1024,
        DEFAULT_CONFIG.max_connections,
        DEFAULT_CONFIG.timeout_ms,
        DEFAULT_CONFIG.debug_level
    );

    println!(
        "Computed: factorial={}, is_pow2={}",
        FACTORIAL_5, IS_POWER_OF_TWO
    );
}
