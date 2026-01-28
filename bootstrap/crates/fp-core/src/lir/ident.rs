//! LIR-specific identifier types
//!
//! LIR uses simple string-based names as it's close to machine code.

use std::fmt::{Display, Formatter};

/// A LIR name - a simple string identifier for low-level IR
#[derive(Debug, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct Name {
    pub value: String,
}

impl Name {
    pub fn new(value: impl Into<String>) -> Self {
        Self {
            value: value.into(),
        }
    }

    pub fn as_str(&self) -> &str {
        self.value.as_str()
    }
}

impl Display for Name {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl From<String> for Name {
    fn from(value: String) -> Self {
        Name::new(value)
    }
}

impl From<&str> for Name {
    fn from(value: &str) -> Self {
        Name::new(value)
    }
}

impl From<Name> for String {
    fn from(name: Name) -> Self {
        name.value
    }
}

impl From<&Name> for String {
    fn from(name: &Name) -> Self {
        name.value.clone()
    }
}

/// Convert from MIR symbol to LIR name
impl From<crate::mir::Symbol> for Name {
    fn from(symbol: crate::mir::Symbol) -> Self {
        Name::new(symbol.name)
    }
}

impl From<&crate::mir::Symbol> for Name {
    fn from(symbol: &crate::mir::Symbol) -> Self {
        Name::new(symbol.name.clone())
    }
}

impl AsRef<str> for Name {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl std::ops::Deref for Name {
    type Target = str;

    fn deref(&self) -> &str {
        self.as_str()
    }
}
