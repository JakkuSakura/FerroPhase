use crate::common_struct;
use crate::id::Ident;
use crate::ty::TypeValue;
use common::{Deserialize, Serialize};
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, Serialize, Deserialize, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct Path {
    pub segments: Vec<Ident>,
}

impl Path {
    pub fn new(segments: Vec<Ident>) -> Self {
        debug_assert!(segments.len() > 0, "Path must have at least one segment");
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
    pub fn last(&self) -> &Ident {
        self.segments.last().unwrap()
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

common_struct! {
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

common_struct! {
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
