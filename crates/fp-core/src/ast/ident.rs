//! AST-specific identifier types
//!
//! Each compilation stage has its own identifier representation:
//! - AST: Ident, Path, ParameterPath, Locator (this module)
//! - HIR: Symbol (String), hir::Path
//! - MIR: Symbol (String), Vec<Symbol>
//! - LIR: String

use serde::{Deserialize, Serialize};

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

/// A path is a sequence of identifiers separated by `::`, like `std::io::File`
#[derive(Debug, Clone, Serialize, Deserialize, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct Path {
    pub segments: Vec<Ident>,
}

impl Path {
    pub fn new(segments: Vec<Ident>) -> Self {
        debug_assert!(segments.len() > 0, "Path must have at least one segment");
        Self { segments }
    }

    pub fn from_ident(ident: Ident) -> Self {
        Self {
            segments: vec![ident],
        }
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
        if self.segments.len() != 1 {
            return None;
        }
        self.segments.into_iter().next()
    }

    pub fn is_root(&self) -> bool {
        self.segments.len() == 1 && self.segments[0].is_root()
    }

    pub fn root() -> Self {
        Self::new(vec![Ident::root()])
    }

    pub fn with_ident(&self, ident: Ident) -> Self {
        let mut segments = self.segments.clone();
        segments.push(ident);
        Self::new(segments)
    }
}

impl std::fmt::Display for Path {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.join("::"))
    }
}

impl From<Ident> for Path {
    fn from(ident: Ident) -> Self {
        Self {
            segments: vec![ident],
        }
    }
}

impl From<&Ident> for Path {
    fn from(ident: &Ident) -> Self {
        Self {
            segments: vec![ident.clone()],
        }
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
    pub segments: Vec<ParameterPathSegment>,
}

impl ParameterPath {
    pub fn new(segments: Vec<ParameterPathSegment>) -> Self {
        Self { segments }
    }

    pub fn from_ident(ident: Ident) -> Self {
        Self {
            segments: vec![ParameterPathSegment::from_ident(ident)],
        }
    }

    pub fn from_path(path: Path) -> Self {
        Self {
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
}

/// A locator can be an identifier, a path, or a parameterized path
#[derive(Debug, Clone, Serialize, Deserialize, Hash, PartialEq)]
pub enum Locator {
    Ident(Ident),
    Path(Path),
    ParameterPath(ParameterPath),
}

impl Locator {
    pub fn ident(name: impl Into<String>) -> Self {
        Locator::Ident(Ident::new(name))
    }

    pub fn path(path: Path) -> Self {
        if path.segments.len() == 1 {
            return Locator::Ident(path.segments[0].clone());
        }
        Locator::Path(path)
    }

    pub fn parameter_path(path: ParameterPath) -> Self {
        // if no parameters, convert to path
        if path.segments.iter().all(|seg| seg.args.is_empty()) {
            let segments = path
                .segments
                .into_iter()
                .map(|seg| seg.ident)
                .collect::<Vec<_>>();
            return Locator::path(Path::new(segments));
        }
        Locator::ParameterPath(path)
    }

    pub fn from_ident(ident: Ident) -> Self {
        Locator::Ident(ident)
    }

    pub fn to_path(&self) -> Path {
        match self {
            Locator::Ident(ident) => Path::from_ident(ident.clone()),
            Locator::Path(path) => path.clone(),
            _ => unreachable!(),
        }
    }

    pub fn as_ident(&self) -> Option<&Ident> {
        match self {
            Locator::Ident(ident) => Some(ident),
            _ => None,
        }
    }
}

impl std::fmt::Display for Locator {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Locator::Ident(ident) => write!(f, "{}", ident),
            Locator::Path(path) => write!(f, "{}", path),
            Locator::ParameterPath(path) => {
                for (i, seg) in path.segments.iter().enumerate() {
                    if i > 0 {
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
