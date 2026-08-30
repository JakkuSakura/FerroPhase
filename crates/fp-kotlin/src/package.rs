use std::path::PathBuf;
use std::sync::Arc;

use fp_core::ast::package::provider::{PackageProvider, ProviderResult};
use fp_core::ast::package::{AstPackage, PackageDescriptor, PackageId};

#[derive(Debug)]
pub struct KotlinPackageProvider {
    #[allow(dead_code)]
    root: PathBuf,
}

impl KotlinPackageProvider {
    pub fn new(root: PathBuf) -> Self {
        Self { root }
    }

    pub fn discover(root: &std::path::Path) -> ProviderResult<Self> {
        Ok(Self::new(root.to_path_buf()))
    }
}

impl PackageProvider for KotlinPackageProvider {
    fn list_packages(&self) -> ProviderResult<Vec<PackageId>> {
        Err(fp_core::ast::package::provider::ProviderError::Other(
            "Kotlin package discovery is not implemented".into(),
        ))
    }

    fn workspace_packages(&self) -> ProviderResult<Vec<PackageId>> {
        self.list_packages()
    }

    fn intrinsic_normalizer(&self) -> Box<dyn fp_core::intrinsics::IntrinsicNormalizer> {
        Box::new(fp_core::intrinsics::NoopIntrinsicNormalizer)
    }

    fn load_package_metadata(&self, _id: &PackageId) -> ProviderResult<Arc<PackageDescriptor>> {
        Err(fp_core::ast::package::provider::ProviderError::Other(
            "Kotlin package metadata loading is not implemented".into(),
        ))
    }

    fn load_package_source(&self, _id: &PackageId) -> ProviderResult<AstPackage> {
        Err(fp_core::ast::package::provider::ProviderError::Other(
            "Kotlin package source loading is not implemented".into(),
        ))
    }

    fn refresh(&self) -> ProviderResult<()> {
        Err(fp_core::ast::package::provider::ProviderError::Other(
            "Kotlin package refresh is not implemented".into(),
        ))
    }
}
