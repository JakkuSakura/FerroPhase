use std::fmt::{self, Display};

use fp_core::module::path::QualifiedPath;
use serde::{Deserialize, Serialize};

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct RequestId(u64);

impl RequestId {
    pub fn new(value: u64) -> Self {
        Self(value)
    }

    pub fn as_u64(self) -> u64 {
        self.0
    }
}

impl Display for RequestId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "request#{}", self.0)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct SourceId {
    key: String,
}

impl SourceId {
    pub fn new(key: impl Into<String>) -> Self {
        Self { key: key.into() }
    }

    pub fn as_str(&self) -> &str {
        &self.key
    }
}

impl Display for SourceId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.key.fmt(f)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
/// Resolved semantic identity for a work subject after identity-forming generic
/// and comptime arguments are known.
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

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct ScopeId {
    key: String,
}

impl ScopeId {
    pub fn new(key: impl Into<String>) -> Self {
        Self { key: key.into() }
    }

    pub fn as_str(&self) -> &str {
        &self.key
    }
}

impl Display for ScopeId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.key.fmt(f)
    }
}

macro_rules! define_storage_id {
    ($name:ident, $doc:literal) => {
        #[doc = $doc]
        #[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
        pub struct $name {
            key: String,
        }

        impl $name {
            pub fn new(key: impl Into<String>) -> Self {
                Self { key: key.into() }
            }

            pub fn as_str(&self) -> &str {
                &self.key
            }
        }

        impl Display for $name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                self.key.fmt(f)
            }
        }
    };
}

define_storage_id!(RawAstId, "Storage identity for parsed, unnormalized AST.");
define_storage_id!(AstId, "Storage identity for canonical AST.");
define_storage_id!(TypedAstId, "Storage identity for typed canonical AST.");
define_storage_id!(HirId, "Storage identity for HIR.");
define_storage_id!(MirId, "Storage identity for MIR.");
define_storage_id!(LirId, "Storage identity for LIR.");
define_storage_id!(ConstValueId, "Storage identity for a compile-time value.");
define_storage_id!(
    RuntimeValueId,
    "Storage identity for a runtime interpreter value."
);
define_storage_id!(BytecodeId, "Storage identity for serialized bytecode.");
define_storage_id!(NativeObjectId, "Storage identity for native object output.");
define_storage_id!(JitObjectId, "Storage identity for JIT-ready native code.");
define_storage_id!(SavedOutputId, "Storage identity for a saved output record.");

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
