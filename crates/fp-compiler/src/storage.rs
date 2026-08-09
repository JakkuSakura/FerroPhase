use serde::{Deserialize, Serialize};

use std::fmt::{self, Display};

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
