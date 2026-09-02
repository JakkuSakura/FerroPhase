use std::fmt::{self, Display};

use fp_core::ast::path::InPackagePath;
use serde::{Deserialize, Serialize};

/// Resolved semantic identity for a work subject after identity-forming
/// generic and comptime arguments are known.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct FullyInPackagePath {
    path: InPackagePath,
}

impl FullyInPackagePath {
    pub fn new(path: InPackagePath) -> Self {
        Self { path }
    }

    pub fn from_segments(segments: Vec<String>) -> Self {
        Self {
            path: InPackagePath::new(segments),
        }
    }

    pub fn path(&self) -> &InPackagePath {
        &self.path
    }

    pub fn with_segment(&self, segment: impl Into<String>) -> Self {
        Self {
            path: self.path.with_segment(segment.into()),
        }
    }

    pub fn to_key(&self) -> String {
        self.path.to_key()
    }
}

impl Display for FullyInPackagePath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.path.to_key().fmt(f)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn resolved_identity_uses_qualified_path() {
        let identity = FullyInPackagePath::from_segments(vec![
            "std".to_string(),
            "vec".to_string(),
            "Vec#{type i32}".to_string(),
        ]);

        assert_eq!(identity.to_key(), "std::vec::Vec#{type i32}");
    }
}
