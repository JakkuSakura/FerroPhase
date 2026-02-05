use std::fmt::{self, Display};

use crate::vfs::VirtualPath;

pub type FeatureRef = String;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ModuleId(pub String);

impl ModuleId {
    pub fn new<S: Into<String>>(path: S) -> Self {
        Self(path.into())
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl Display for ModuleId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ModuleLanguage {
    Ferro,
    Rust,
    TypeScript,
    Python,
    Other(String),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SymbolKind {
    Function,
    Type,
    Const,
    Module,
    Trait,
    Struct,
    Enum,
    Other(String),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SymbolDescriptor {
    pub name: String,
    pub symbol_kind: SymbolKind,
    pub signature: Option<String>,
    pub docs: Option<String>,
}

impl SymbolDescriptor {
    pub fn function(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            symbol_kind: SymbolKind::Function,
            signature: None,
            docs: None,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ModuleDescriptor {
    pub id: ModuleId,
    pub package: crate::package::PackageId,
    pub language: ModuleLanguage,
    pub module_path: Vec<String>,
    pub source: VirtualPath,
    pub exports: Vec<SymbolDescriptor>,
    pub requires_features: Vec<FeatureRef>,
}

pub mod resolver;
pub mod resolvers;
pub mod path;
pub mod resolution;
