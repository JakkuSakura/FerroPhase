#!/usr/bin/env fp run
//! Struct generation and specialization with compile-time configuration

use std::time::{SystemTime, UNIX_EPOCH};

fn main() {
    println!("=== Struct Generation Toolkit ===");

    //------------------------------------------------------------------
    // 1. Configuration-driven definition
    //------------------------------------------------------------------
    const LOG_LEVEL: &str = "debug";
    const ENABLE_PROFILING: bool = true;
    const ENABLE_METRICS: bool = false;
    const TARGET_PLATFORM: &str = "linux";

    t! {
        struct Application {
            // Always-present fields
            name: String,
            pid: u32,
            start_time: u64,

            // Debug-only state
            debug_enabled: bool,
            log_file: String,

            // Profiling data (only meaningful when ENABLE_PROFILING)
            profiler_samples: Vec<u32>,
            sampling_rate: u32,

            // Platform specific knobs
            platform: &'static str,
            priority: i32,

            fn new(name: String, pid: u32) -> Self {
                Self {
                    name,
                    pid,
                    start_time: SystemTime::now()
                        .duration_since(UNIX_EPOCH)
                        .unwrap()
                        .as_secs(),
                    debug_enabled: LOG_LEVEL == "debug",
                    log_file: format!("{}.log", name),
                    profiler_samples: if ENABLE_PROFILING { Vec::new() } else { Vec::new() },
                    sampling_rate: if ENABLE_PROFILING { 1_000 } else { 0 },
                    platform: TARGET_PLATFORM,
                    priority: 0,
                }
            }

            fn log(&self, message: &str) {
                if self.debug_enabled {
                    println!("[debug:{}] {}", self.log_file, message);
                }
            }

            fn record_sample(&mut self, sample_ns: u32) {
                if ENABLE_PROFILING {
                    self.profiler_samples.push(sample_ns);
                }
            }

            fn set_priority(&mut self, priority: i32) {
                if TARGET_PLATFORM == "linux" {
                    self.priority = priority.clamp(-20, 19);
                }
            }
        }
    };

    let mut app = Application::new("FerroPhase", 4242);
    app.log("Application booted");
    app.record_sample(1_280);
    app.set_priority(-5);

    println!("  Application struct size: {} bytes", sizeof!(Application));
    println!("  Fields available: {}", field_count!(Application));
    println!("  Debug enabled: {}", app.debug_enabled);
    println!("  Profiling supported: {}", ENABLE_PROFILING);
    println!("  Platform: {} priority:{}", app.platform, app.priority);

    //------------------------------------------------------------------
    // 2. Dimensional vector specialisations
    //------------------------------------------------------------------
    t! {
        struct Vector2D {
            x: f32,
            y: f32,

            fn new(x: f32, y: f32) -> Self {
                Self { x, y }
            }

            fn magnitude(&self) -> f32 {
                (self.x * self.x + self.y * self.y).sqrt()
            }
        }

        struct Vector3D {
            x: f64,
            y: f64,
            z: f64,

            fn new(x: f64, y: f64, z: f64) -> Self {
                Self { x, y, z }
            }

            fn dot(&self, other: &Self) -> f64 {
                self.x * other.x + self.y * other.y + self.z * other.z
            }
        }
    };

    const UNIT_X: Vector2D = Vector2D::new(1.0, 0.0);
    const UNIT_Y: Vector2D = Vector2D::new(0.0, 1.0);
    const GRID_DISTANCE: f32 = UNIT_X.magnitude() + UNIT_Y.magnitude();

    const FORWARD: Vector3D = Vector3D::new(0.0, 0.0, 1.0);
    const CAMERA: Vector3D = Vector3D::new(0.0, 1.0, 1.0);
    const ORIENTATION: f64 = FORWARD.dot(&CAMERA);

    println!("\n  Vector2D magnitude sum: {}", GRID_DISTANCE);
    println!("  Vector3D forward·camera: {}", ORIENTATION);

    //------------------------------------------------------------------
    // 3. Feature matrix specialisation
    //------------------------------------------------------------------
    t! {
        struct MetricsWindow {
            window: [f64; 8],
            len: usize,
            rolling_average: f64,

            fn push(&mut self, value: f64) {
                if self.len < self.window.len() {
                    self.window[self.len] = value;
                    self.len += 1;
                }
                self.update_average();
            }

            fn update_average(&mut self) {
                if self.len == 0 {
                    self.rolling_average = 0.0;
                } else {
                    let sum: f64 = self.window[0..self.len].iter().sum();
                    self.rolling_average = sum / self.len as f64;
                }
            }
        }
    };

    let mut metrics = MetricsWindow {
        window: [0.0; 8],
        len: 0,
        rolling_average: 0.0,
    };
    metrics.push(12.5);
    metrics.push(9.0);
    metrics.push(10.5);

    println!("\n  Metrics window average: {} (len={})", metrics.rolling_average, metrics.len);
    println!("\n✓ Struct generation scenarios complete");
}
