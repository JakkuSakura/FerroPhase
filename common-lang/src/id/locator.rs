use crate::common_enum;
use crate::id::{Ident, ParameterPath, Path};
use common::*;
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;

common_enum! {
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
