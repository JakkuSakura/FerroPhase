//! API functionality for Project 1

use anyhow::Result;
use shared_types::{User, CommonError};
use shared_utils::{format_user, with_timestamp};

/// Process a user in the API
pub fn process_user(user: &User) -> Result<String, CommonError> {
    log::info!("Project 1 API processing user: {}", user.id);
    
    // Example processing logic
    if user.name.is_empty() || user.email.is_empty() {
        log::error!("Invalid user data: missing name or email");
        return Err(CommonError::InvalidInput);
    }
    
    let formatted = format_user(user);
    let result = with_timestamp(&format!("User processed: {}", formatted));
    
    log::info!("Processing successful: {}", result);
    Ok(result)
}

/// Create a new user with the given details
pub fn create_user(id: u64, name: &str, email: &str) -> User {
    log::info!("Creating new user: {} <{}>", name, email);
    User::new(id, name.to_string(), email.to_string())
}