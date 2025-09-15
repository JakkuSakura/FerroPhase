#!/usr/bin/env fp run
//! String concatenation and processing at compile time

fn main() {
    // Basic string constants
    const APP_NAME: &str = "FerroPhase";
    const VERSION: &str = "1.0.0";
    
    // Compile-time string operations
    const BANNER: &str = concat!(APP_NAME, " v", VERSION);
    const CONFIG_FILE: &str = concat!(APP_NAME, ".config");
    const LOG_FILE: &str = concat!(APP_NAME, "_", VERSION, ".log");
    
    // Advanced string processing
    const BANNER_LENGTH: usize = strlen!(BANNER);
    const UPPERCASE_NAME: &str = to_upper!(APP_NAME);
    const HASH_CODE: u64 = hash!(BANNER);
    
    // Conditional buffer sizing based on string properties
    const BUFFER_SIZE: usize = if BANNER_LENGTH > 20 { 1024 } else { 512 };
    const NEEDS_LARGE_BUFFER: bool = strlen!(LOG_FILE) > 30;
    
    // String validation at compile time
    if strlen!(APP_NAME) == 0 {
        compile_error!("App name cannot be empty!");
    }
    
    if contains!(VERSION, "beta") {
        compile_warning!("Using beta version");
    }
    
    // Application info with computed string properties
    struct AppInfo {
        banner: &'static str,
        config_file: &'static str,
        log_file: &'static str,
        buffer_size: usize,
        hash_code: u64,
    }
    
    const APP_INFO: AppInfo = AppInfo {
        banner: BANNER,
        config_file: CONFIG_FILE,
        log_file: LOG_FILE,
        buffer_size: BUFFER_SIZE,
        hash_code: HASH_CODE,
    };
    
    println!("{} (hash: 0x{:X})", APP_INFO.banner, APP_INFO.hash_code);
    println!("Config: {}, Log: {}, Buffer: {}KB", 
             APP_INFO.config_file, APP_INFO.log_file, APP_INFO.buffer_size / 1024);
    println!("Uppercase: {}, Large buffer needed: {}", UPPERCASE_NAME, NEEDS_LARGE_BUFFER);
}