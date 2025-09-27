#!/usr/bin/env fp run
//! Compile-time configuration for generated structs

fn main() {
    const ENABLE_LOGGING: bool = true;
    const ENABLE_PROFILING: bool = false;
    const BASE_PRIORITY: i32 = if ENABLE_LOGGING { 1 } else { 0 };
    const MAX_CLIENTS: i64 = if ENABLE_PROFILING { 256 } else { 128 };

    struct AppConfig {
        enable_logging: bool,
        enable_profiling: bool,
        base_priority: i32,
        max_clients: i64,
    }

    const APP_CONFIG: AppConfig = AppConfig {
        enable_logging: ENABLE_LOGGING,
        enable_profiling: ENABLE_PROFILING,
        base_priority: BASE_PRIORITY,
        max_clients: MAX_CLIENTS,
    };

    struct MetricsConfig {
        window_size: i64,
        smoothing_factor: i64,
    }

    const METRICS_CONFIG: MetricsConfig = MetricsConfig {
        window_size: if ENABLE_PROFILING { 16 } else { 8 },
        smoothing_factor: if ENABLE_LOGGING { 3 } else { 1 },
    };

    println!("=== Generated Struct Configuration ===");
    println!(
        "  logging={} profiling={} priority={} clients={}",
        APP_CONFIG.enable_logging,
        APP_CONFIG.enable_profiling,
        APP_CONFIG.base_priority,
        APP_CONFIG.max_clients,
    );

    println!(
        "  metrics: window={} smoothing={} ",
        METRICS_CONFIG.window_size,
        METRICS_CONFIG.smoothing_factor,
    );
    println!("\n[ok] Struct generation scenarios complete");
}
