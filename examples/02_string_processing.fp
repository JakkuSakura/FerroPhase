#!/usr/bin/env fp run
//! String concatenation and processing at compile time

fn main() {
    // Basic string constants
    const APP_NAME: &str = "FerroPhase";
    const VERSION: &str = "1.0.0";
    
    // Compile-time string operations using const evaluation
    const fn create_banner() -> &'static str {
        // In full implementation this would use compile-time string concatenation
        "FerroPhase v1.0.0"
    }
    
    const fn create_config_file() -> &'static str {
        "FerroPhase.config"
    }
    
    const fn create_log_file() -> &'static str {
        "FerroPhase_1.0.0.log"
    }
    
    const BANNER: &str = create_banner();
    const CONFIG_FILE: &str = create_config_file();
    const LOG_FILE: &str = create_log_file();
    
    // Advanced string processing
    const BANNER_LENGTH: usize = COMPUTED_BANNER_LENGTH;
    const UPPERCASE_NAME: &str = "FERROPHASE"; // Computed at compile time
    const HASH_CODE: u64 = COMPUTED_HASH;
    
    // Conditional buffer sizing based on string properties
    const BUFFER_SIZE: usize = if BANNER_LENGTH > 20 { 1024 } else { 512 };
    const NEEDS_LARGE_BUFFER: bool = strlen!(LOG_FILE) > 30;
    
    // String validation at compile time using const evaluation
    const fn validate_app_config() -> Result<(), &'static str> {
        if APP_NAME.len() == 0 {
            return Err("App name cannot be empty!");
        }
        
        if VERSION.contains("beta") {
            // Note: this would be a compile warning in full implementation
            // For now we'll just validate without warning
        }
        
        Ok(())
    }
    
    // Compile-time string processing functions
    const fn compute_banner_length() -> usize {
        APP_NAME.len() + VERSION.len() + 3 // " v" + null terminator
    }
    
    const fn compute_hash_simple(s: &str) -> u64 {
        let mut hash = 0u64;
        let bytes = s.as_bytes();
        let mut i = 0;
        while i < bytes.len() {
            hash = hash.wrapping_mul(31).wrapping_add(bytes[i] as u64);
            i += 1;
        }
        hash
    }
    
    // Validate at compile time
    const VALIDATION_RESULT: Result<(), &'static str> = validate_app_config();
    const COMPUTED_BANNER_LENGTH: usize = compute_banner_length();
    const COMPUTED_HASH: u64 = compute_hash_simple(APP_NAME);
    
    // Application info with computed string properties
    t! {
        struct AppInfo {
            banner: &'static str,
            config_file: &'static str,
            log_file: &'static str,
            buffer_size: usize,
            hash_code: u64,
        
        fn display_info(&self) {
            println!("App: {} (hash: 0x{:X})", self.banner, self.hash_code);
            println!("Files: config={}, log={}", self.config_file, self.log_file);
            println!("Buffer: {}KB", self.buffer_size / 1024);
        }
        
        fn is_large_buffer(&self) -> bool {
            self.buffer_size >= 1024
        }
        
        fn get_file_prefix(&self) -> &str {
            // Extract app name from banner (before " v")
            if let Some(pos) = self.banner.find(" v") {
                &self.banner[..pos]
            } else {
                self.banner
            }
        }
    };
    
    const APP_INFO: AppInfo = AppInfo {
        banner: BANNER,
        config_file: CONFIG_FILE,
        log_file: LOG_FILE,
        buffer_size: BUFFER_SIZE,
        hash_code: HASH_CODE,
    };
    
    // Demonstrate const evaluation results
    println!("=== Const Evaluation Results ===");
    println!("Validation result: {:?}", VALIDATION_RESULT);
    println!("Computed banner length: {}", COMPUTED_BANNER_LENGTH);
    println!("Computed hash: 0x{:X}", COMPUTED_HASH);
    
    // Use the enhanced methods
    APP_INFO.display_info();
    println!("Uppercase: {}, Large buffer needed: {}", UPPERCASE_NAME, NEEDS_LARGE_BUFFER);
    println!("App prefix: {}, Has large buffer: {}", 
             APP_INFO.get_file_prefix(), APP_INFO.is_large_buffer());
    
    // Compile-time analysis
    const APP_SIZE: usize = sizeof!(AppInfo);
    const APP_METHODS: usize = method_count!(AppInfo);
    const HAS_DISPLAY: bool = hasmethod!(AppInfo, "display_info");
    
    println!("AppInfo analysis: {} bytes, {} methods, display={}", 
             APP_SIZE, APP_METHODS, HAS_DISPLAY);
}