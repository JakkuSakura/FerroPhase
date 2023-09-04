use crate::common_derives;
use crate::value::TypeValue;
use common::*;
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;

common_derives! {
    no_debug
    pub enum Locator {
        Ident(Ident),
        Path(Path),
        ParameterPath(ParameterPath),
    }

}
impl Locator {
    pub fn ident(ident: Ident) -> Self {
        Self::Ident(ident)
    }
    pub fn path(path: Path) -> Self {
        if path.segments.len() == 1 {
            return Self::Ident(path.segments[0].clone());
        }
        Self::Path(path)
    }
    pub fn parameter_path(path: ParameterPath) -> Self {
        // if no parameters, convert to path
        if path.segments.iter().all(|seg| seg.args.is_empty()) {
            let segments = path
                .segments
                .into_iter()
                .map(|seg| seg.ident)
                .collect::<Vec<_>>();
            return Self::path(Path::new(segments));
        }
        Self::ParameterPath(path)
    }
}
impl Into<Path> for Locator {
    fn into(self) -> Path {
        match self {
            Self::Ident(ident) => ident.into(),
            Self::Path(path) => path,
            Self::ParameterPath(path) => panic!("cannot convert ParameterPath to Path: {:?}", path),
        }
    }
}
impl<'a> Into<Path> for &'a Locator {
    fn into(self) -> Path {
        match self {
            Locator::Ident(ident) => ident.into(),
            Locator::Path(path) => path.clone(),
            Locator::ParameterPath(path) => {
                panic!("cannot convert ParameterPath to Path: {:?}", path)
            }
        }
    }
}
impl Display for Locator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Ident(ident) => Display::fmt(ident, f),
            Self::Path(path) => Display::fmt(path, f),
            Self::ParameterPath(path) => Display::fmt(path, f),
        }
    }
}
impl Debug for Locator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Ident(ident) => Debug::fmt(ident, f),
            Self::Path(path) => Debug::fmt(path, f),
            Self::ParameterPath(path) => Debug::fmt(path, f),
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

common_derives! {
    pub struct ParameterPathSegment {
        pub ident: Ident,
        pub args: Vec<TypeValue>,
    }
}
impl ParameterPathSegment {
    pub fn new(ident: Ident, args: Vec<TypeValue>) -> Self {
        Self { ident, args }
    }
}

common_derives! {
    /// ParameterPath is a specialized locator for paths like Foo::<T>::bar<U>
    /// it is equivalent to Invoke(Select(Invoke(Foo, T), bar), U)
    pub struct ParameterPath {
        pub segments: Vec<ParameterPathSegment>,
    }
}
impl ParameterPath {
    pub fn last(&self) -> &ParameterPathSegment {
        self.segments.last().unwrap()
    }
}

impl Display for ParameterPath {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut first = true;
        for seg in &self.segments {
            if !first {
                write!(f, "::")?;
            }
            first = false;
            write!(f, "{}", seg.ident.name)?;
            if !seg.args.is_empty() {
                write!(f, "<")?;
                let mut first = true;
                for arg in &seg.args {
                    if !first {
                        write!(f, ", ")?;
                    }
                    first = false;
                    write!(f, "{}", arg)?;
                }
                write!(f, ">")?;
            }
        }
        Ok(())
    }
}
