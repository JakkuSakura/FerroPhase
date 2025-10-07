//! HIR-specific identifier types
//!
//! HIR uses a Symbol type (interned string) and a Path type for qualified names.

use serde::{Deserialize, Serialize};
use std::fmt::{Display, Formatter};

/// A HIR symbol - an interned string identifier
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

/// Convert from AST identifier to HIR symbol
impl From<crate::ast::Ident> for Symbol {
    fn from(ident: crate::ast::Ident) -> Self {
        Symbol::new(ident.name)
    }
}

impl From<&crate::ast::Ident> for Symbol {
    fn from(ident: &crate::ast::Ident) -> Self {
        Symbol::new(ident.name.clone())
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
