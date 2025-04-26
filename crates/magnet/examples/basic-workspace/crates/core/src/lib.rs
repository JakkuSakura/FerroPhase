//! Core functionality for the example project

use serde::{Deserialize, Serialize};

/// A data structure used throughout the project
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CoreData {
    pub id: u64,
    pub name: String,
    pub value: f64,
}

/// Process some core data
pub fn process_data(data: &CoreData) -> String {
    format!("Processed: {} (id: {}, value: {})", data.name, data.id, data.value)
}

/// Create a new CoreData instance
pub fn create_data(id: u64, name: &str, value: f64) -> CoreData {
    CoreData {
        id,
        name: name.to_string(),
        value,
    }
}