use std::sync::Arc;

use crate::module::{ModuleDescriptor, ModuleId};
use crate::package::{PackageDescriptor, PackageId, PackageSource};

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

pub trait PackageProvider {
    /// Resolve a source-level package key owned by this provider.
    /// Providers may accept aliases or qualified keys instead of only package
    /// names returned by `list_packages`.
    fn resolve_package(&self, key: &str) -> Option<PackageId> {
        self.list_packages()
            .ok()?
            .into_iter()
            .find(|id| id.as_str() == key)
    }
    fn list_packages(&self) -> ProviderResult<Vec<PackageId>>;
    fn load_package_metadata(&self, id: &PackageId) -> ProviderResult<Arc<PackageDescriptor>>;
    fn refresh(&self) -> ProviderResult<()>;

    /// Load a package's modules — discovery, parsing, and graph construction
    /// are the implementor's job. The returned `PackageSource`'s `items`,
    /// `module_paths`, and `graph` are populated; typing tables (`struct_defs`
    /// etc.) are left empty for the typer to fill in afterward. Defaulted to
    /// unsupported so existing implementors don't need to change.
    fn load_package_source(&self, id: &PackageId) -> ProviderResult<PackageSource> {
        Err(ProviderError::other(format!(
            "load_package_source not supported for {id}"
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
