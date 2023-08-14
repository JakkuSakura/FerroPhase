use common::*;
use std::fmt::{Debug, Display, Formatter};
use std::rc::Rc;

mod expr;
mod item;

mod typing;

pub use expr::*;
pub use item::*;
pub use typing::*;

/// Tree is any syntax tree element
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Tree {
    Item(Item),
}

#[derive(Clone)]
pub struct AnyBox {
    pub debug: String,
    pub value: Rc<dyn Any>,
}
impl AnyBox {
    pub fn new<T: Any + Debug>(t: T) -> Self {
        Self {
            debug: format!("{:?}", t),
            value: Rc::new(t),
        }
    }

    pub fn downcast_ref<T: Any>(&self) -> Option<&T> {
        self.value.downcast_ref()
    }
}
impl Debug for AnyBox {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.debug)
    }
}
impl Serialize for AnyBox {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        self.debug.serialize(serializer)
    }
}
impl<'de> Deserialize<'de> for AnyBox {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let name = String::deserialize(deserializer)?;
        Ok(Self {
            debug: name,
            value: Rc::new(()),
        })
    }
}
#[derive(Debug, Clone, Serialize, Deserialize, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct Ident {
    pub name: String,
}
impl Display for Ident {
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
