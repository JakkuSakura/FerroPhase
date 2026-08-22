use std::path::PathBuf;
use std::sync::Arc;

use fp_core::package::provider::{PackageProvider, ProviderResult};
use fp_core::package::{PackageDescriptor, PackageId, PackageSource};

/// Mirrors every other secondary-language `PackageProvider` in this
/// workspace (`fp-toml`, `fp-golang`, ...), which are themselves currently
/// `todo!()` stubs pending the broader package-provider pipeline (see
/// `fp-cli/src/languages/package_provider_registry.rs`) being finished —
/// not a Lean-specific gap.
#[derive(Debug)]
pub struct LeanPackageProvider {
    root: PathBuf,
}

impl LeanPackageProvider {
    pub fn new(root: PathBuf) -> Self {
        Self { root }
    }

    pub fn discover(root: &std::path::Path) -> ProviderResult<Self> {
        Ok(Self::new(root.to_path_buf()))
    }
}

impl PackageProvider for LeanPackageProvider {
    fn list_packages(&self) -> ProviderResult<Vec<PackageId>> {
        todo!()
    }

    fn workspace_packages(&self) -> ProviderResult<Vec<PackageId>> {
        self.list_packages()
    }

    fn load_package_metadata(&self, _id: &PackageId) -> ProviderResult<Arc<PackageDescriptor>> {
        todo!()
    }

    fn load_package_source(&self, _id: &PackageId) -> ProviderResult<PackageSource> {
        todo!()
    }

    fn refresh(&self) -> ProviderResult<()> {
        todo!()
    }
}
