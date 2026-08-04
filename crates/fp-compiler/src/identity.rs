use std::fmt::{self, Display};

use fp_core::module::path::QualifiedPath;
use serde::{Deserialize, Serialize};

/// Resolved semantic identity for a work subject after identity-forming
/// generic and comptime arguments are known.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct FullyQualifiedPath {
    path: QualifiedPath,
}

impl FullyQualifiedPath {
    pub fn new(path: QualifiedPath) -> Self {
        Self { path }
    }

    pub fn from_segments(segments: Vec<String>) -> Self {
        Self {
            path: QualifiedPath::new(segments),
        }
    }

    pub fn path(&self) -> &QualifiedPath {
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

impl Display for FullyQualifiedPath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.path.to_key().fmt(f)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn resolved_identity_uses_qualified_path() {
        let identity = FullyQualifiedPath::from_segments(vec![
            "std".to_string(),
            "vec".to_string(),
            "Vec#{type i32}".to_string(),
        ]);

        assert_eq!(identity.to_key(), "std::vec::Vec#{type i32}");
    }
}
