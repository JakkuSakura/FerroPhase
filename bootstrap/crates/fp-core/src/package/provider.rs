use std::sync::Arc;

use crate::module::{ModuleDescriptor, ModuleId};
use crate::package::{PackageDescriptor, PackageId};

pub type ProviderResult<T> = Result<T, ProviderError>;

#[derive(Debug)]
pub enum ProviderError {
    PackageNotFound(PackageId),
    ModuleNotFound(ModuleId),
    Metadata(String),
    Other(String),
}

impl std::fmt::Display for ProviderError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ProviderError::PackageNotFound(id) => write!(f, "package not found: {}", id),
            ProviderError::ModuleNotFound(id) => write!(f, "module not found: {}", id),
            ProviderError::Metadata(message) => write!(f, "metadata error: {}", message),
            ProviderError::Other(message) => write!(f, "{}", message),
        }
    }
}

impl std::error::Error for ProviderError {}

impl ProviderError {
    pub fn metadata(err: impl Into<String>) -> Self {
        Self::Metadata(err.into())
    }

    pub fn other(message: impl Into<String>) -> Self {
        Self::Other(message.into())
    }
}

pub trait PackageProvider: Send + Sync {
    fn list_packages(&self) -> ProviderResult<Vec<PackageId>>;
    fn load_package(&self, id: &PackageId) -> ProviderResult<Arc<PackageDescriptor>>;
    fn refresh(&self) -> ProviderResult<()>;
}

pub trait ModuleSource: Send + Sync {
    fn modules_for_package(&self, id: &PackageId) -> ProviderResult<Vec<ModuleId>>;
    fn load_module_descriptor(&self, id: &ModuleId) -> ProviderResult<Arc<ModuleDescriptor>>;
}

pub trait ModuleProvider: Send + Sync {
    fn modules_for_package(&self, id: &PackageId) -> ProviderResult<Vec<ModuleId>>;
    fn load_module(&self, id: &ModuleId) -> ProviderResult<Arc<ModuleDescriptor>>;
    fn refresh(&self, id: &PackageId) -> ProviderResult<()>;
}
