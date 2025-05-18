//! Shared types for use across projects

use serde::{Deserialize, Serialize};
use uuid::Uuid;

/// A common user type used across projects
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct User {
    pub id: u64,
    pub uuid: Uuid,
    pub name: String,
    pub email: String,
}

impl User {
    pub fn new(id: u64, name: String, email: String) -> Self {
        Self {
            id,
            uuid: Uuid::new_v4(),
            name,
            email,
        }
    }
}

/// A common error type
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum CommonError {
    InvalidInput,
    NotFound,
    Unauthorized,
    InternalError,
}