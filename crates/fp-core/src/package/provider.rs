use std::sync::Arc;

use crate::module::{ModuleDescriptor, ModuleId};
use crate::package::{PackageDescriptor, PackageId};

pub type ProviderResult<T> = Result<T, ProviderError>;

#[derive(Debug, thiserror::Error)]
pub enum ProviderError {
    #[error("package not found: {0}")]
    PackageNotFound(PackageId),
    #[error("module not found: {0}")]
    ModuleNotFound(ModuleId),
    #[error("metadata error: {0}")]
    Metadata(String),
    #[error("{0}")]
    Other(String),
}

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
