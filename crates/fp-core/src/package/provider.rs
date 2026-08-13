use std::sync::Arc;

use crate::ast::module::ModuleId;
use crate::package::{PackageDescriptor, PackageId, PackageMetadata, PackageSource};
use crate::vfs::VirtualPath;

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

/// A `PackageProvider` that always hands back one already-built
/// `PackageSource` — for tests that construct `ast::Item`s directly with
/// no real frontend/disk parsing involved. Every real provider still
/// builds a `PackageSource` directly (e.g. `FerroPhaseProvider`'s
/// `load_embedded_package`); this just skips the "discover it from disk"
/// step, while still requiring callers to obtain their `CompiledPackage`
/// through the normal `PackageProvider` -> `WorkspaceContext::begin_package`
/// path rather than hand-rolling one.
pub struct FixedPackageProvider {
    package_id: PackageId,
    descriptor: Arc<PackageDescriptor>,
    source: PackageSource,
}

impl FixedPackageProvider {
    pub fn new(descriptor: PackageDescriptor, source: PackageSource) -> Self {
        Self {
            package_id: descriptor.id.clone(),
            descriptor: Arc::new(descriptor),
            source,
        }
    }

    /// Convenience constructor for tests that don't care about manifest
    /// metadata at all — builds a minimal descriptor with an empty root
    /// path.
    pub fn for_source(package_id: PackageId, source: PackageSource) -> Self {
        let descriptor = PackageDescriptor {
            id: package_id.clone(),
            name: package_id.as_str().to_string(),
            version: None,
            manifest_path: VirtualPath::new_relative(Vec::<String>::new()),
            root: VirtualPath::new_relative(Vec::<String>::new()),
            metadata: PackageMetadata::default(),
            modules: Vec::new(),
        };
        Self::new(descriptor, source)
    }
}

impl PackageProvider for FixedPackageProvider {
    fn list_packages(&self) -> ProviderResult<Vec<PackageId>> {
        Ok(vec![self.package_id.clone()])
    }

    fn load_package_metadata(&self, id: &PackageId) -> ProviderResult<Arc<PackageDescriptor>> {
        if id != &self.package_id {
            return Err(ProviderError::PackageNotFound(id.clone()));
        }
        Ok(self.descriptor.clone())
    }

    fn refresh(&self) -> ProviderResult<()> {
        Ok(())
    }

    fn load_package_source(&self, id: &PackageId) -> ProviderResult<PackageSource> {
        if id != &self.package_id {
            return Err(ProviderError::PackageNotFound(id.clone()));
        }
        Ok(self.source.clone())
    }
}
