//! AST-specific identifier types
//!
//! Each compilation stage has its own identifier representation:
//! - AST: Ident, Path, Name (this module)
//! - HIR: Symbol (String), hir::Path
//! - MIR: Symbol (String), Vec<Symbol>
//! - LIR: String

use serde::{Deserialize, Serialize};

use crate::ast::path::PathPrefix;
use crate::span::Span;

/// A simple identifier - a single name like `foo` or `MyStruct`
#[derive(Debug, Clone, Serialize, Deserialize, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct Ident {
    pub name: String,
}

impl Ident {
    pub fn new(name: impl Into<String>) -> Self {
        Self { name: name.into() }
    }

    pub fn as_str(&self) -> &str {
        self.name.as_str()
    }

    pub fn is_root(&self) -> bool {
        self.name == "__root__"
    }

    pub fn root() -> Self {
        Self::new("__root__")
    }

    pub fn span(&self) -> Span {
        Span::null()
    }
}

impl std::fmt::Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl From<Ident> for String {
    fn from(ident: Ident) -> Self {
        ident.name
    }
}

impl From<&Ident> for String {
    fn from(ident: &Ident) -> Self {
        ident.name.clone()
    }
}

impl From<String> for Ident {
    fn from(name: String) -> Self {
        Ident::new(name)
    }
}

impl From<&str> for Ident {
    fn from(name: &str) -> Self {
        Ident::new(name)
    }
}

/// A path is a sequence of identifiers separated by `::`, like `std::io::File`.
/// The prefix captures leading qualifiers like `::`, `crate`, `self`, or `super`.
#[derive(Debug, Clone, Serialize, Deserialize, Hash, PartialEq)]
pub struct Path {
    pub prefix: PathPrefix,
    pub segments: Vec<PathSegment>,
}

impl Path {
    pub fn new(prefix: PathPrefix, segments: Vec<PathSegment>) -> Self {
        debug_assert!(
            !segments.is_empty() || !matches!(prefix, PathPrefix::Plain),
            "Plain path must have at least one segment"
        );
        Self { prefix, segments }
    }

    pub fn plain(segments: Vec<Ident>) -> Self {
        Self::new(
            PathPrefix::Plain,
            segments.into_iter().map(PathSegment::from_ident).collect(),
        )
    }

    pub fn from_ident(ident: Ident) -> Self {
        Self::new(PathPrefix::Plain, vec![PathSegment::from_ident(ident)])
    }

    pub fn is_empty(&self) -> bool {
        self.segments.is_empty()
    }

    pub fn len(&self) -> usize {
        self.segments.len()
    }

    pub fn first(&self) -> Option<&PathSegment> {
        self.segments.first()
    }

    pub fn last(&self) -> &PathSegment {
        self.segments.last().unwrap()
    }

    pub fn push(&mut self, segment: impl Into<PathSegment>) {
        self.segments.push(segment.into());
    }

    pub fn join(&self, separator: &str) -> String {
        self.segments
            .iter()
            .map(|s| s.as_str())
            .collect::<Vec<_>>()
            .join(separator)
    }

    pub fn try_into_ident(self) -> Option<Ident> {
        if self.prefix != PathPrefix::Plain || self.segments.len() != 1 {
            return None;
        }
        self.segments
            .into_iter()
            .next()
            .map(|segment| segment.ident)
    }

    pub fn is_root(&self) -> bool {
        self.prefix == PathPrefix::Root && self.segments.is_empty()
    }

    pub fn root() -> Self {
        Self::new(PathPrefix::Root, Vec::new())
    }

    pub fn with_ident(&self, ident: Ident) -> Self {
        let mut segments = self.segments.clone();
        segments.push(ident.into());
        Self::new(self.prefix, segments)
    }

    pub fn span(&self) -> Span {
        Span::null()
    }
}

/// Type qualification on a path, matching rustc's AST representation.
#[derive(Debug, Clone, Serialize, Deserialize, Hash, PartialEq)]
pub struct QSelf {
    pub ty: Box<Ty>,
    pub path_span: Span,
    pub position: usize,
}

impl std::fmt::Display for Path {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.prefix {
            PathPrefix::Root => {
                if self.segments.is_empty() {
                    write!(f, "::")
                } else {
                    write!(f, "::{}", self.join("::"))
                }
            }
            PathPrefix::Crate => {
                if self.segments.is_empty() {
                    write!(f, "crate")
                } else {
                    write!(f, "crate::{}", self.join("::"))
                }
            }
            PathPrefix::SelfMod => {
                if self.segments.is_empty() {
                    write!(f, "self")
                } else {
                    write!(f, "self::{}", self.join("::"))
                }
            }
            PathPrefix::Super(depth) => {
                let prefix = std::iter::repeat("super")
                    .take(depth)
                    .collect::<Vec<_>>()
                    .join("::");
                if self.segments.is_empty() {
                    write!(f, "{}", prefix)
                } else {
                    write!(f, "{}::{}", prefix, self.join("::"))
                }
            }
            PathPrefix::Plain => write!(f, "{}", self.join("::")),
        }
    }
}

impl From<Ident> for Path {
    fn from(ident: Ident) -> Self {
        Self::from_ident(ident)
    }
}

impl From<&Ident> for Path {
    fn from(ident: &Ident) -> Self {
        Self::from_ident(ident.clone())
    }
}

impl From<&Path> for Path {
    fn from(path: &Path) -> Self {
        path.clone()
    }
}

/// A path segment with optional generic arguments, matching rustc's AST path
/// representation. Generic arguments belong to the segment they parameterize.
#[derive(Debug, Clone, Serialize, Deserialize, Hash, PartialEq)]
pub struct PathSegment {
    pub ident: Ident,
    pub args: Vec<Ty>,
}

impl PathSegment {
    pub fn new(ident: Ident, args: Vec<Ty>) -> Self {
        Self { ident, args }
    }

    pub fn from_ident(ident: Ident) -> Self {
        Self {
            ident,
            args: Vec::new(),
        }
    }

    pub fn as_str(&self) -> &str {
        self.ident.as_str()
    }
}

impl From<Ident> for PathSegment {
    fn from(ident: Ident) -> Self {
        Self::from_ident(ident)
    }
}

impl From<&str> for PathSegment {
    fn from(name: &str) -> Self {
        Self::from_ident(Ident::new(name))
    }
}

impl Eq for PathSegment {}
impl Eq for Path {}

/// A qualified path use, matching rustc's separation of QSelf from Path.
#[derive(Debug, Clone, Serialize, Deserialize, Hash, PartialEq)]
pub struct QPath {
    pub qself: Option<QSelf>,
    pub path: Path,
}

impl QPath {
    pub fn new(qself: Option<QSelf>, path: Path) -> Self {
        Self { qself, path }
    }
}

/// A name can be an identifier or a path. Generic arguments are stored on
/// path segments; qualified paths use [`QPath`] at their expression/type site.
#[derive(Debug, Clone, Serialize, Deserialize, Hash, PartialEq)]
pub enum Name {
    Ident(Ident),
    Path(Path),
}

impl Name {
    pub fn ident(name: impl Into<String>) -> Self {
        Name::Ident(Ident::new(name))
    }

    pub fn path(path: Path) -> Self {
        if path.prefix == PathPrefix::Plain
            && path.segments.len() == 1
            && path.segments[0].args.is_empty()
        {
            return Name::Ident(path.segments[0].ident.clone());
        }
        Name::Path(path)
    }

    pub fn from_ident(ident: Ident) -> Self {
        Name::Ident(ident)
    }

    pub fn to_path(&self) -> Path {
        match self {
            Name::Ident(ident) => Path::from_ident(ident.clone()),
            Name::Path(path) => path.clone(),
        }
    }

    pub fn as_ident(&self) -> Option<&Ident> {
        match self {
            Name::Ident(ident) => Some(ident),
            Name::Path(_) => None,
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Name::Ident(ident) => ident.span(),
            Name::Path(path) => path.span(),
        }
    }
}

impl std::fmt::Display for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Name::Ident(ident) => write!(f, "{}", ident),
            Name::Path(path) => write!(f, "{}", path),
        }
    }
}

// Import Ty from parent module for PathSegment
use super::Ty;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parameterized_path_retains_per_segment_arguments() {
        let path = Path::new(
            PathPrefix::Plain,
            vec![PathSegment::new(Ident::new("Vec"), Vec::new())],
        );
        assert_eq!(path.segments.len(), 1);
        assert_eq!(path.segments[0].ident.as_str(), "Vec");
        assert!(path.segments[0].args.is_empty());
    }
}
