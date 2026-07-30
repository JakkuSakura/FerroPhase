use std::sync::Arc;

use crate::module::{ModuleDescriptor, ModuleId};
use crate::package::{PackageCrate, PackageDescriptor, PackageId};

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

    /// Load a package's modules — discovery, parsing, and graph construction
    /// are the implementor's job. The returned `PackageCrate`'s `items`,
    /// `module_paths`, and `graph` are populated; typing tables (`struct_defs`
    /// etc.) are left empty for the typer to fill in afterward. Defaulted to
    /// unsupported so existing implementors don't need to change.
    fn load_package_items(&self, id: &PackageId) -> ProviderResult<PackageCrate> {
        Err(ProviderError::other(format!(
            "load_package_items not supported for {id}"
        )))
    }
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
