//! Utility functions for the example project

use core_crate::{CoreData, process_data};
use chrono::Utc;

/// Log data processing with a timestamp
pub fn log_data_processing(data: &CoreData) -> String {
    let timestamp = Utc::now().to_rfc3339();
    let processed = process_data(data);
    let result = format!("[{}] {}", timestamp, processed);
    
    log::info!("{}", result);
    result
}

/// Format data as JSON
pub fn to_json(data: &CoreData) -> String {
    serde_json::to_string_pretty(data).unwrap_or_else(|_| "Error serializing data".to_string())
}