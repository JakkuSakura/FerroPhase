//! MIR-specific identifier types
//!
//! MIR uses a Symbol type and a simplified path representation.

use serde::{Deserialize, Serialize};
use std::fmt::{Display, Formatter};

/// A MIR symbol - an identifier in the mid-level IR
#[derive(Debug, Clone, Serialize, Deserialize, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct Symbol {
    pub name: String,
}

impl Symbol {
    pub fn new(name: impl Into<String>) -> Self {
        Self { name: name.into() }
    }

    pub fn as_str(&self) -> &str {
        self.name.as_str()
    }
}

impl Display for Symbol {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl From<String> for Symbol {
    fn from(name: String) -> Self {
        Symbol::new(name)
    }
}

impl From<&str> for Symbol {
    fn from(name: &str) -> Self {
        Symbol::new(name)
    }
}

impl From<Symbol> for String {
    fn from(symbol: Symbol) -> Self {
        symbol.name
    }
}

impl From<&Symbol> for String {
    fn from(symbol: &Symbol) -> Self {
        symbol.name.clone()
    }
}

/// Convert from HIR symbol to MIR symbol
impl From<crate::hir::Symbol> for Symbol {
    fn from(symbol: crate::hir::Symbol) -> Self {
        Symbol::new(symbol.name)
    }
}

impl From<&crate::hir::Symbol> for Symbol {
    fn from(symbol: &crate::hir::Symbol) -> Self {
        Symbol::new(symbol.name.clone())
    }
}

impl AsRef<str> for Symbol {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl std::ops::Deref for Symbol {
    type Target = str;

    fn deref(&self) -> &str {
        self.as_str()
    }
}

impl PartialEq<&str> for Symbol {
    fn eq(&self, other: &&str) -> bool {
        self.as_str() == *other
    }
}

impl PartialEq<Symbol> for &str {
    fn eq(&self, other: &Symbol) -> bool {
        *self == other.as_str()
    }
}

/// MIR path is a sequence of symbols
#[derive(Debug, Clone, Serialize, Deserialize, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct Path {
    pub segments: Vec<Symbol>,
}

impl Path {
    pub fn new(segments: Vec<Symbol>) -> Self {
        Self { segments }
    }

    pub fn from_symbol(symbol: Symbol) -> Self {
        Self {
            segments: vec![symbol],
        }
    }

    pub fn join(&self, separator: &str) -> String {
        self.segments
            .iter()
            .map(|s| s.as_str())
            .collect::<Vec<_>>()
            .join(separator)
    }
}

impl Display for Path {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}", self.join("::"))
    }
}
