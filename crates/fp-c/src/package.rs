use std::path::PathBuf;
use std::sync::Arc;

use fp_core::package::provider::{PackageProvider, ProviderError, ProviderResult};
use fp_core::package::{PackageDescriptor, PackageId, PackageSource};

#[derive(Debug)]
pub struct CPackageProvider {
    root: PathBuf,
}

impl CPackageProvider {
    pub fn new(root: PathBuf) -> Self {
        Self { root }
    }

    pub fn discover(root: &std::path::Path) -> ProviderResult<Self> {
        Ok(Self::new(root.to_path_buf()))
    }
}

impl PackageProvider for CPackageProvider {
    fn list_packages(&self) -> ProviderResult<Vec<PackageId>> {
        todo!()
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
