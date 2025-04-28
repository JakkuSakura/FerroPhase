//! Service functionality for Project 2

use shared_types::{User, CommonError};
use shared_utils::{format_user, with_timestamp};

/// Create a new user in the service
pub async fn create_user(name: &str, email: &str) -> Result<User, CommonError> {
    if name.is_empty() || email.is_empty() {
        log::error!("Cannot create user with empty name or email");
        return Err(CommonError::InvalidInput);
    }
    
    // In a real service, this would likely interact with a database
    log::info!("Creating new user asynchronously: {} <{}>", name, email);
    
    // Simulate async operation
    tokio::time::sleep(tokio::time::Duration::from_millis(100)).await;
    
    let user = User::new(
        // In a real application, this would be generated or from a database
        42,
        name.to_string(),
        email.to_string(),
    );
    
    log::info!("User created: {}", format_user(&user));
    Ok(user)
}

/// Process a batch of users
pub async fn process_users(users: &[User]) -> Vec<String> {
    log::info!("Processing batch of {} users", users.len());
    
    let mut results = Vec::with_capacity(users.len());
    
    for user in users {
        let formatted = format_user(user);
        let result = with_timestamp(&format!("Batch processed: {}", formatted));
        results.push(result);
    }
    
    log::info!("Batch processing complete");
    results
}