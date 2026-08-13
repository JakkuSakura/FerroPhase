use std::sync::Arc;

use crate::ast::module::ModuleId;
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
    fn list_packages(&self) -> ProviderResult<Vec<PackageId>>;
    fn load_package_metadata(&self, id: &PackageId) -> ProviderResult<Arc<PackageDescriptor>>;
    fn refresh(&self) -> ProviderResult<()>;

    /// Load a package's modules. Discovery, parsing, and graph construction
    /// are the implementor's job. The returned `PackageSource`'s `items`,
    /// `module_paths`, and `graph` are populated; compiler-owned registries
    /// are left empty for the compiler to fill in.
    fn load_package_source(&self, id: &PackageId) -> ProviderResult<PackageSource>;
}
