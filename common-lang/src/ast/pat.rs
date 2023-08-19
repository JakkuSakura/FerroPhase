use common::*;
use std::fmt::{Debug, Display, Formatter};

#[derive(Clone, Serialize, Deserialize, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub enum Pat {
    Ident(Ident),
    Path(Path),
}
impl Pat {
    pub fn ident(ident: Ident) -> Self {
        Self::Ident(ident)
    }
    pub fn path(path: Path) -> Self {
        if path.segments.len() == 1 {
            return Self::Ident(path.segments[0].clone());
        }
        Self::Path(path)
    }
}
impl Into<Path> for Pat {
    fn into(self) -> Path {
        match self {
            Self::Ident(ident) => ident.into(),
            Self::Path(path) => path,
        }
    }
}
impl<'a> Into<Path> for &'a Pat {
    fn into(self) -> Path {
        match self {
            Pat::Ident(ident) => ident.into(),
            Pat::Path(path) => path.clone(),
        }
    }
}
impl Display for Pat {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Ident(ident) => Display::fmt(ident, f),
            Self::Path(path) => Display::fmt(path, f),
        }
    }
}
impl Debug for Pat {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Ident(ident) => Debug::fmt(ident, f),
            Self::Path(path) => Debug::fmt(path, f),
        }
    }
}
#[derive(Clone, Serialize, Deserialize, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct Ident {
    pub name: String,
}
impl Display for Ident {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.name)
    }
}
impl Debug for Ident {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("#")?;
        f.write_str(&self.name)
    }
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
impl<T: Into<String>> From<T> for Ident {
    fn from(value: T) -> Self {
        Self::new(value)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct Path {
    pub segments: Vec<Ident>,
}

impl Path {
    pub fn new(segments: Vec<Ident>) -> Self {
        Self { segments }
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
impl Display for Path {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut first = true;
        for seg in &self.segments {
            if !first {
                write!(f, "::")?;
            }
            first = false;
            write!(f, "{}", seg.name)?;
        }
        Ok(())
    }
}
impl From<Ident> for Path {
    fn from(ident: Ident) -> Self {
        Self {
            segments: vec![ident],
        }
    }
}
impl<'a> From<&'a Ident> for Path {
    fn from(ident: &Ident) -> Self {
        Self {
            segments: vec![ident.clone()],
        }
    }
}
impl<'a> From<&'a Path> for Path {
    fn from(path: &Path) -> Self {
        path.clone()
    }
}
