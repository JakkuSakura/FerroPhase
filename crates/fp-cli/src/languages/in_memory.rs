//! Wraps an already-in-memory, non-file-backed `File` (no path to lazily
//! read) as a one-member package — there's no file to parse lazily, just
//! an AST that already exists.

use std::path::Path;
use std::sync::Arc;

use fp_core::ast::path::QualifiedPath;
use fp_core::ast::package::provider::{PackageProvider, ProviderError, ProviderResult};
use fp_core::ast::package::{PackageDescriptor, PackageId, AstPackage};
use fp_core::vfs::{UnixFileSystem, VirtualPath};
use fp_lang::module_source::FerroModuleSourceResolver;

pub fn in_memory_provider(
    package_id: PackageId,
    module_path: QualifiedPath,
    source: fp_core::ast::File,
) -> ProviderResult<Arc<dyn PackageProvider>> {
    let descriptor = PackageDescriptor {
        id: package_id.clone(),
        name: package_id.as_str().to_owned(),
        version: None,
        manifest_path: VirtualPath::from_path(&source.path),
        root: VirtualPath::from_path(source.path.parent().unwrap_or_else(|| Path::new("."))),
        metadata: Default::default(),
        modules: Vec::new(),
    };
    let resolver = FerroModuleSourceResolver::new(Arc::new(UnixFileSystem::new("/")));
    let package_source = resolver
        .resolve_package_source(descriptor.clone(), module_path, source)
        .map_err(|e| ProviderError::other(e.to_string()))?;
    Ok(Arc::new(InMemoryPackageProvider {
        package_id,
        descriptor: Arc::new(descriptor),
        source: package_source,
    }))
}

struct InMemoryPackageProvider {
    package_id: PackageId,
    descriptor: Arc<PackageDescriptor>,
    source: AstPackage,
}

impl PackageProvider for InMemoryPackageProvider {
    fn list_packages(&self) -> ProviderResult<Vec<PackageId>> {
        Ok(vec![self.package_id.clone()])
    }

    fn workspace_packages(&self) -> ProviderResult<Vec<PackageId>> {
        self.list_packages()
    }

    fn intrinsic_normalizer(&self) -> Box<dyn fp_core::intrinsics::IntrinsicNormalizer> {
        Box::new(fp_core::intrinsics::NoopIntrinsicNormalizer)
    }

    fn load_package_metadata(&self, id: &PackageId) -> ProviderResult<Arc<PackageDescriptor>> {
        if id != &self.package_id {
            return Err(ProviderError::PackageNotFound(id.clone()));
        }
        Ok(self.descriptor.clone())
    }

    fn load_package_source(&self, id: &PackageId) -> ProviderResult<AstPackage> {
        if id != &self.package_id {
            return Err(ProviderError::PackageNotFound(id.clone()));
        }
        Ok(self.source.clone())
    }

    fn refresh(&self) -> ProviderResult<()> {
        Ok(())
    }
}
