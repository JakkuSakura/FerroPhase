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

define_storage_id!(ConstValueId, "Storage identity for a compile-time value.");
