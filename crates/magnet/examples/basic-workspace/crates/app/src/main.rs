//! Example application demonstrating magnet workspace management

use core_crate::{create_data, CoreData};
use utils_crate::{log_data_processing, to_json};

fn main() {
    // Initialize logger
    env_logger::init_from_env(env_logger::Env::default().default_filter_or("info"));
    
    println!("Example App for Magnet Tool Demo");
    println!("===============================\n");
    
    // Create some sample data
    let data = create_data(42, "Example Data", 3.14159);
    
    // Process the data using the utility functions
    let processed = log_data_processing(&data);
    println!("Processing result: {}", processed);
    
    // Convert to JSON
    let json = to_json(&data);
    println!("\nJSON representation:\n{}", json);
    
    println!("\nApp completed successfully!");
}