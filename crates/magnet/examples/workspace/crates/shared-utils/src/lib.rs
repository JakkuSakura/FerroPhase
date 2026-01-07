//! Shared utilities for use across projects

use shared_types::User;
use chrono::Utc;

/// Format a user's name and email for display
pub fn format_user(user: &User) -> String {
    format!("{} <{}> (ID: {}, UUID: {})", 
            user.name, 
            user.email, 
            user.id, 
            user.uuid)
}

/// Get the current timestamp as an ISO 8601 string
pub fn current_timestamp() -> String {
    Utc::now().to_rfc3339()
}

/// Add a timestamp to a message
pub fn with_timestamp(message: &str) -> String {
    format!("[{}] {}", current_timestamp(), message)
}