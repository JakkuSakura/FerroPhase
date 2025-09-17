#!/usr/bin/env fp run
//! String processing and validation at compile time using const evaluation

fn main() {
    // Basic string constants
    const APP_NAME: String = "FerroPhase";
    const VERSION: String = "1.0.0";
    
    // Compile-time string concatenation using different methods
    const BANNER: String = APP_NAME + " v" + VERSION;              // Using + operator
    const CONFIG_FILE: String = concat!(APP_NAME, ".config");       // Using concat! macro
    const LOG_FILE: String = concat!(APP_NAME, "_", VERSION, ".log"); // Multiple concatenation
    
    // Compile-time string property analysis
    const APP_NAME_LENGTH: i64 = APP_NAME.len();
    const VERSION_LENGTH: i64 = VERSION.len();
    const BANNER_LENGTH: i64 = BANNER.len();
    const UPPERCASE_NAME: String = "FERROPHASE"; // Computed at compile time
    
    // String content validation at compile time
    const VERSION_HAS_BETA: bool = VERSION.contains("beta");
    const VERSION_HAS_DOT: bool = VERSION.contains(".");
    const BANNER_HAS_VERSION: bool = BANNER.contains("1.0.0");
    const APP_NAME_IS_EMPTY: bool = APP_NAME_LENGTH == 0;
    
    // Conditional buffer sizing based on string properties
    const BUFFER_SIZE: i64 = if BANNER_LENGTH > 20 { 1024 } else { 512 };
    const NEEDS_LARGE_BUFFER: bool = strlen!(LOG_FILE) > 15;
    
    // Simple compile-time hash computation
    const HASH_BASE: i64 = 31;
    const SIMPLE_HASH: i64 = APP_NAME_LENGTH * HASH_BASE + VERSION_LENGTH;
    
    // More advanced hash using string search positions  
    const DOT_POSITION: i64 = VERSION.find(".");
    const COMPUTED_HASH: i64 = if DOT_POSITION >= 0 { 
        SIMPLE_HASH + DOT_POSITION 
    } else { 
        SIMPLE_HASH 
    };
    
    // Dynamic string creation using concatenation
    const DEBUG_PREFIX: String = "[" + APP_NAME + "]";
    const STATUS_MESSAGE: String = concat!("Application ", APP_NAME, " version ", VERSION, " loaded");
    const ERROR_LOG_FILE: String = APP_NAME + "_errors.log";
    
    // Application info struct with computed string properties
    struct AppInfo {
        banner: String,
        config_file: String,
        log_file: String,
        error_log_file: String,
        debug_prefix: String,
        status_message: String,
        app_name_length: i64,
        buffer_size: i64,
        hash_code: i64,
        has_version_dot: bool,
    }
    
    const APP_INFO: AppInfo = AppInfo {
        banner: BANNER,
        config_file: CONFIG_FILE,
        log_file: LOG_FILE,
        error_log_file: ERROR_LOG_FILE,
        debug_prefix: DEBUG_PREFIX,
        status_message: STATUS_MESSAGE,
        app_name_length: APP_NAME_LENGTH,
        buffer_size: BUFFER_SIZE,
        hash_code: COMPUTED_HASH,
        has_version_dot: VERSION_HAS_DOT,
    };
    
    // Demonstrate const evaluation results
    println!("=== String Processing Results ===");
    println!("App name: {} (length: {})", APP_NAME, APP_NAME_LENGTH);
    println!("Version: {} (length: {})", VERSION, VERSION_LENGTH);
    println!("Banner: {} (length: {})", BANNER, BANNER_LENGTH);
    
    println!("\n=== String Analysis ===");
    println!("Version has 'beta': {}", VERSION_HAS_BETA);
    println!("Version has '.': {}", VERSION_HAS_DOT);
    println!("Banner contains version: {}", BANNER_HAS_VERSION);
    println!("App name is empty: {}", APP_NAME_IS_EMPTY);
    println!("Dot position in version: {}", DOT_POSITION);
    
    println!("\n=== String Concatenation Results ===");
    println!("Banner (+ operator): {}", APP_INFO.banner);
    println!("Config file (concat! macro): {}", APP_INFO.config_file);
    println!("Log file (multi-concat): {}", APP_INFO.log_file);
    println!("Error log file (+ operator): {}", APP_INFO.error_log_file);
    println!("Debug prefix: {}", APP_INFO.debug_prefix);
    println!("Status message: {}", APP_INFO.status_message);
    
    println!("\n=== File Configuration ===");
    println!("Buffer size: {}KB", APP_INFO.buffer_size / 1024);
    println!("Needs large buffer: {}", NEEDS_LARGE_BUFFER);
    
    println!("\n=== Computed Values ===");
    println!("Simple hash: {}", SIMPLE_HASH);
    println!("Computed hash: {}", APP_INFO.hash_code);
    println!("Hash computation: ({} * {}) + {} + {} = {}", 
             APP_NAME_LENGTH, HASH_BASE, VERSION_LENGTH, DOT_POSITION, COMPUTED_HASH);
    
    // Validation using computed string properties
    const IS_VALID_APP: bool = !APP_NAME_IS_EMPTY && VERSION_HAS_DOT && APP_NAME_LENGTH >= 5;
    const CONFIG_COMPLEXITY: i64 = APP_NAME_LENGTH + VERSION_LENGTH + BANNER_LENGTH;
    const OPTIMIZATION_LEVEL: i64 = if CONFIG_COMPLEXITY > 30 { 3 } else if CONFIG_COMPLEXITY > 20 { 2 } else { 1 };
    
    println!("\n=== Validation & Optimization ===");
    println!("Is valid app config: {}", IS_VALID_APP);
    println!("Config complexity: {}", CONFIG_COMPLEXITY);
    println!("Recommended optimization level: {}", OPTIMIZATION_LEVEL);
    
    if IS_VALID_APP {
        println!("✓ Application configuration is valid");
    } else {
        println!("⚠ Application configuration needs review");
    }
    
    // Demonstrate conditional compilation based on string analysis
    if VERSION_HAS_BETA {
        println!("🚧 Development/Beta build detected");
    } else {
        println!("🚀 Production build ready");
    }
    
    println!("\n=== Concatenation Methods Demo ===");
    const DEMO_PLUS: String = "Method" + ": " + "+ operator";
    const DEMO_CONCAT: String = concat!("Method", ": ", "concat! macro");
    println!("{}", DEMO_PLUS);
    println!("{}", DEMO_CONCAT);
    
    println!("\n=== Summary ===");
    const TOTAL_CHARS: i64 = APP_NAME_LENGTH + VERSION_LENGTH + BANNER_LENGTH;
    const SUMMARY: String = concat!("Processed 3 base strings with ", TOTAL_CHARS, " total characters");
    println!("{}", SUMMARY);
    println!("Uppercase variant: {}", UPPERCASE_NAME);
    println!("Generated {} concatenated strings during compilation", 7);
}