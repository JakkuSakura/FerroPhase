//! AST-specific identifier types
//!
//! Each compilation stage has its own identifier representation:
//! - AST: Ident, Path, ParameterPath, Name (this module)
//! - HIR: Symbol (String), hir::Path
//! - MIR: Symbol (String), Vec<Symbol>
//! - LIR: String

use serde::{Deserialize, Serialize};

use crate::module::path::PathPrefix;
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
#[derive(Debug, Clone, Serialize, Deserialize, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct Path {
    pub prefix: PathPrefix,
    pub segments: Vec<Ident>,
}

impl Path {
    pub fn new(prefix: PathPrefix, segments: Vec<Ident>) -> Self {
        debug_assert!(
            !segments.is_empty() || !matches!(prefix, PathPrefix::Plain),
            "Plain path must have at least one segment"
        );
        Self { prefix, segments }
    }

    pub fn plain(segments: Vec<Ident>) -> Self {
        Self::new(PathPrefix::Plain, segments)
    }

    pub fn from_ident(ident: Ident) -> Self {
        Self::new(PathPrefix::Plain, vec![ident])
    }

    pub fn is_empty(&self) -> bool {
        self.segments.is_empty()
    }

    pub fn len(&self) -> usize {
        self.segments.len()
    }

    pub fn first(&self) -> Option<&Ident> {
        self.segments.first()
    }

    pub fn last(&self) -> &Ident {
        self.segments.last().unwrap()
    }

    pub fn push(&mut self, ident: Ident) {
        self.segments.push(ident);
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
        self.segments.into_iter().next()
    }

    pub fn is_root(&self) -> bool {
        self.prefix == PathPrefix::Root && self.segments.is_empty()
    }

    pub fn root() -> Self {
        Self::new(PathPrefix::Root, Vec::new())
    }

    pub fn with_ident(&self, ident: Ident) -> Self {
        let mut segments = self.segments.clone();
        segments.push(ident);
        Self::new(self.prefix, segments)
    }

    pub fn span(&self) -> Span {
        Span::null()
    }
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

/// A segment of a parameterized path, like `Vec<i32>` in `std::collections::Vec<i32>`
#[derive(Debug, Clone, Serialize, Deserialize, Hash, PartialEq)]
pub struct ParameterPathSegment {
    pub ident: Ident,
    pub args: Vec<Ty>,
}

impl ParameterPathSegment {
    pub fn new(ident: Ident, args: Vec<Ty>) -> Self {
        Self { ident, args }
    }

    pub fn from_ident(ident: Ident) -> Self {
        Self {
            ident,
            args: Vec::new(),
        }
    }
}

/// A parameterized path like `std::collections::Vec<i32>`
#[derive(Debug, Clone, Serialize, Deserialize, Hash, PartialEq)]
pub struct ParameterPath {
    pub prefix: PathPrefix,
    pub segments: Vec<ParameterPathSegment>,
}

impl ParameterPath {
    pub fn new(prefix: PathPrefix, segments: Vec<ParameterPathSegment>) -> Self {
        Self { prefix, segments }
    }

    pub fn from_ident(ident: Ident) -> Self {
        Self {
            prefix: PathPrefix::Plain,
            segments: vec![ParameterPathSegment::from_ident(ident)],
        }
    }

    pub fn from_path(path: Path) -> Self {
        Self {
            prefix: path.prefix,
            segments: path
                .segments
                .into_iter()
                .map(ParameterPathSegment::from_ident)
                .collect(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.segments.is_empty()
    }

    pub fn len(&self) -> usize {
        self.segments.len()
    }

    pub fn first(&self) -> Option<&ParameterPathSegment> {
        self.segments.first()
    }

    pub fn last(&self) -> Option<&ParameterPathSegment> {
        self.segments.last()
    }

    pub fn span(&self) -> Span {
        Span::null()
    }
}

/// A locator can be an identifier, a path, or a parameterized path
#[derive(Debug, Clone, Serialize, Deserialize, Hash, PartialEq)]
pub enum Name {
    Ident(Ident),
    Path(Path),
    ParameterPath(ParameterPath),
}

impl Name {
    pub fn ident(name: impl Into<String>) -> Self {
        Name::Ident(Ident::new(name))
    }

    pub fn path(path: Path) -> Self {
        if path.prefix == PathPrefix::Plain && path.segments.len() == 1 {
            return Name::Ident(path.segments[0].clone());
        }
        Name::Path(path)
    }

    pub fn parameter_path(path: ParameterPath) -> Self {
        // if no parameters, convert to path
        if path.segments.iter().all(|seg| seg.args.is_empty()) {
            let segments = path
                .segments
                .into_iter()
                .map(|seg| seg.ident)
                .collect::<Vec<_>>();
            return Name::path(Path::new(path.prefix, segments));
        }
        Name::ParameterPath(path)
    }

    pub fn from_ident(ident: Ident) -> Self {
        Name::Ident(ident)
    }

    pub fn to_path(&self) -> Path {
        match self {
            Name::Ident(ident) => Path::from_ident(ident.clone()),
            Name::Path(path) => path.clone(),
            Name::ParameterPath(path) => Path::new(
                path.prefix,
                path.segments.iter().map(|seg| seg.ident.clone()).collect(),
            ),
        }
    }

    pub fn as_ident(&self) -> Option<&Ident> {
        match self {
            Name::Ident(ident) => Some(ident),
            _ => None,
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Name::Ident(ident) => ident.span(),
            Name::Path(path) => path.span(),
            Name::ParameterPath(path) => path.span(),
        }
    }
}

impl std::fmt::Display for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Name::Ident(ident) => write!(f, "{}", ident),
            Name::Path(path) => write!(f, "{}", path),
            Name::ParameterPath(path) => {
                match path.prefix {
                    PathPrefix::Root => write!(f, "::")?,
                    PathPrefix::Crate => write!(f, "crate")?,
                    PathPrefix::SelfMod => write!(f, "self")?,
                    PathPrefix::Super(depth) => {
                        let prefix = std::iter::repeat("super")
                            .take(depth)
                            .collect::<Vec<_>>()
                            .join("::");
                        write!(f, "{}", prefix)?;
                    }
                    PathPrefix::Plain => {}
                }
                for (i, seg) in path.segments.iter().enumerate() {
                    if i > 0 || path.prefix != PathPrefix::Plain {
                        write!(f, "::")?;
                    }
                    write!(f, "{}", seg.ident)?;
                    if !seg.args.is_empty() {
                        write!(f, "<")?;
                        for (j, arg) in seg.args.iter().enumerate() {
                            if j > 0 {
                                write!(f, ", ")?;
                            }
                            write!(f, "{}", arg)?;
                        }
                        write!(f, ">")?;
                    }
                }
                Ok(())
            }
        }
    }
}

// Import Ty from parent module for ParameterPathSegment
use super::Ty;
