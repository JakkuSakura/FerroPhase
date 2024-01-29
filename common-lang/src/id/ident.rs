use common::{Deserialize, Serialize};
use std::fmt::{Debug, Display, Formatter};
use std::ops::Deref;

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
impl Deref for Ident {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.name.as_str()
    }
}
impl<T: Into<String>> From<T> for Ident {
    fn from(value: T) -> Self {
        Self::new(value)
    }
}
