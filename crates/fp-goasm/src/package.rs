use std::path::{Path, PathBuf};
use std::sync::Arc;

use fp_core::ast::package::provider::{PackageProvider, ProviderResult};
use fp_core::ast::package::{PackageDescriptor, PackageId, AstPackage};

#[derive(Debug)]
pub struct GoPackageProvider {
    #[allow(dead_code)]
    root: PathBuf,
}

impl GoPackageProvider {
    pub fn new(root: PathBuf) -> Self {
        Self { root }
    }

    pub fn discover(root: &std::path::Path) -> ProviderResult<Self> {
        Ok(Self::new(root.to_path_buf()))
    }
}

impl PackageProvider for GoPackageProvider {
    fn list_packages(&self) -> ProviderResult<Vec<PackageId>> {
        todo!()
    }

    fn workspace_packages(&self) -> ProviderResult<Vec<PackageId>> {
        self.list_packages()
    }

    fn load_package_metadata(&self, _id: &PackageId) -> ProviderResult<Arc<PackageDescriptor>> {
        todo!()
    }

    fn load_package_source(&self, _id: &PackageId) -> ProviderResult<AstPackage> {
        todo!()
    }

    fn refresh(&self) -> ProviderResult<()> {
        todo!()
    }
}

/// A standalone `.goasm` file (not a project directory) is Go-style native
/// assembly text — lift it once at construction into a target-independent
/// `LirBlob` via `fp_core::ast::package::provider::lir_from_text`, so every
/// LIR-consuming target (native/goasm/urcl/cil/jvm-bytecode) can retarget
/// it with no backend-specific handling. A directory input is a real
/// multi-file project, still owned by `GoPackageProvider` (currently
/// unimplemented).
pub fn file_provider(root: &Path) -> Option<Arc<dyn PackageProvider>> {
    if root.is_file() {
        fp_core::ast::package::provider::lir_from_text(root, |text| {
            crate::parse_program(text).map(|(lir, _target)| lir)
        })
    } else {
        Some(Arc::new(GoPackageProvider::new(root.to_path_buf())) as Arc<dyn PackageProvider>)
    }
}
