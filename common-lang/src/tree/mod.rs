use common::*;
use std::fmt::{Debug, Display, Formatter};

mod expr;
mod item;

mod anybox;
mod typing;

pub use anybox::*;
pub use expr::*;
pub use item::*;
pub use typing::*;
/// Tree is any syntax tree element
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Tree {
    Item(Item),
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
