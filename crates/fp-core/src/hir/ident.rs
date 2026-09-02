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

/// A definition's fully-qualified path (module segments + its own name),
/// e.g. the recorded path for `math::add`. Named and shaped after rustc's
/// own `rustc_hir::definitions::DefPath` — the type that answers "what is
/// this definition's fully-qualified path" — but deliberately without its
/// per-segment disambiguator or crate id: those exist in rustc to name
/// *unnamed* nodes (closures, impls) and to stay identifiable across
/// incremental-compilation sessions, neither of which applies here (every
/// entry is a named item, already uniquely keyed by its `DefId`, within one
/// compilation).
#[derive(Debug, Clone, Serialize, Deserialize, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct DefPath {
    pub segments: Vec<Symbol>,
}

impl DefPath {
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

    /// Convert to an `ast::Path` for re-emission during `hir_to_ast`
    /// lowering — rustc has no analog since it never lowers HIR back into
    /// an AST.
    pub fn to_ast_path(&self) -> crate::ast::Path {
        crate::ast::Path::plain(
            self.segments
                .iter()
                .map(|s| crate::ast::Ident::new(s.as_str()))
                .collect(),
        )
    }

    /// Segment names as owned strings, for test assertions that need to
    /// compare against a literal `vec![...]` of expected names. Production
    /// code should render a `DefPath` via `Display`/`to_string()` or
    /// convert it via `to_ast_path()` instead of walking `segments`.
    pub fn to_segments(&self) -> Vec<String> {
        self.segments.iter().map(|s| s.name.clone()).collect()
    }

    /// Build a `DefPath` from an `ast::path::QualifiedPath` — the
    /// resolved-against-the-module-tree form a name takes on *before* its
    /// `DefId` is assigned. The one place this segment-list conversion
    /// happens, so callers never hand-roll
    /// `segments.iter().cloned().map(Symbol::new).collect()` themselves.
    pub fn from_qualified_path(path: &crate::ast::path::QualifiedPath) -> Self {
        Self::new(path.segments.iter().cloned().map(Symbol::new).collect())
    }
}

impl Display for DefPath {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}", self.join("::"))
    }
}
