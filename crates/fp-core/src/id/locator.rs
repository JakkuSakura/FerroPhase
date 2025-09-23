use crate::common_enum;
use crate::id::{Ident, ParameterPath, Path};
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;

common_enum! {
    no_debug
    pub enum Locator {
        Ident(Ident),
        #[try_into(ignore)]
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
    pub fn to_path(&self) -> Path {
        match self {
            Self::Ident(ident) => ident.into(),
            Self::Path(path) => path.clone(),
            _ => unreachable!(),
        }
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

    pub fn as_ident(&self) -> Option<&Ident> {
        match self {
            Self::Ident(ident) => Some(ident),
            _ => None,
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
