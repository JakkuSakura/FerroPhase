use std::path::PathBuf;
use std::sync::Arc;

use fp_core::ast::package::provider::{PackageProvider, ProviderResult};
use fp_core::ast::package::{PackageDescriptor, PackageId, PackageSource};

#[derive(Debug)]
pub struct JsonPackageProvider {
    #[allow(dead_code)]
    root: PathBuf,
}

impl JsonPackageProvider {
    pub fn new(root: PathBuf) -> Self {
        Self { root }
    }

    pub fn discover(root: &std::path::Path) -> ProviderResult<Self> {
        Ok(Self::new(root.to_path_buf()))
    }
}

impl PackageProvider for JsonPackageProvider {
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
