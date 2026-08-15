//! A standalone input file, with no discoverable enclosing package, modeled
//! as a one-member `PackageProvider` — the degenerate case of "package",
//! not a separate file-focused code path. Parses lazily inside
//! `load_package_source`, matching every real provider (`RustPackageProvider`,
//! `FerroPhaseProvider`) instead of requiring an already-parsed AST from the
//! caller.

use std::path::{Path, PathBuf};
use std::sync::{Arc, RwLock};

use fp_core::ast::path::QualifiedPath;
use fp_core::ast::register_threadlocal_serializer;
use fp_core::frontend::{FrontendParseMode, LanguageFrontend};
use fp_core::package::provider::{PackageProvider, ProviderError, ProviderResult};
use fp_core::package::{PackageDescriptor, PackageId, PackageSource};
use fp_core::vfs::{UnixFileSystem, VirtualPath};
use fp_lang::module_source::FerroModuleSourceResolver;

pub struct SingleFilePackageProvider {
    package_id: PackageId,
    module_path: QualifiedPath,
    path: PathBuf,
    frontend: Box<dyn LanguageFrontend>,
    parse_mode: FrontendParseMode,
    cache: RwLock<Option<PackageSource>>,
}

impl SingleFilePackageProvider {
    pub fn new(
        package_id: PackageId,
        module_path: QualifiedPath,
        path: PathBuf,
        frontend: Box<dyn LanguageFrontend>,
        parse_mode: FrontendParseMode,
    ) -> Self {
        Self {
            package_id,
            module_path,
            path,
            frontend,
            parse_mode,
            cache: RwLock::new(None),
        }
    }

    fn descriptor(&self, id: &PackageId) -> PackageDescriptor {
        PackageDescriptor {
            id: id.clone(),
            name: id.as_str().to_owned(),
            version: None,
            manifest_path: VirtualPath::from_path(&self.path),
            root: VirtualPath::from_path(self.path.parent().unwrap_or_else(|| Path::new("."))),
            metadata: Default::default(),
            modules: Vec::new(),
        }
    }
}

impl PackageProvider for SingleFilePackageProvider {
    fn list_packages(&self) -> ProviderResult<Vec<PackageId>> {
        Ok(vec![self.package_id.clone()])
    }

    fn load_package_metadata(&self, id: &PackageId) -> ProviderResult<Arc<PackageDescriptor>> {
        if id != &self.package_id {
            return Err(ProviderError::PackageNotFound(id.clone()));
        }
        Ok(Arc::new(self.descriptor(id)))
    }

    fn refresh(&self) -> ProviderResult<()> {
        if let Ok(mut cache) = self.cache.write() {
            *cache = None;
        }
        Ok(())
    }

    fn load_package_source(&self, id: &PackageId) -> ProviderResult<PackageSource> {
        if id != &self.package_id {
            return Err(ProviderError::PackageNotFound(id.clone()));
        }
        if let Ok(cache) = self.cache.read() {
            if let Some(source) = &*cache {
                return Ok(source.clone());
            }
        }

        self.frontend.set_parse_mode(self.parse_mode);
        let source_text = std::fs::read_to_string(&self.path)
            .map_err(|e| ProviderError::other(format!("read {}: {e}", self.path.display())))?;
        let result = self
            .frontend
            .parse_file(&source_text, &self.path)
            .map_err(|e| ProviderError::other(format!("parse {}: {e}", self.path.display())))?;
        register_threadlocal_serializer(result.serializer.clone());
        let mut ast = result.ast;
        fp_core::ast::annotate_collected_items(&mut ast);

        // Sibling `mod foo;` discovery only exists for FerroPhase's own
        // dialect: `FerroModuleSourceResolver` hardcodes `.fp` sources and
        // `FerroFrontend` parsing for anything it discovers. Every other
        // frontend's AST never marks a module `is_external`, so calling it
        // for e.g. a standalone Rust/TypeScript file is a no-op walk that
        // just flattens `ast`'s own items — there's no equivalent resolver
        // for those languages yet.
        let resolver = FerroModuleSourceResolver::new(Arc::new(UnixFileSystem::new("/")));
        let package_source = resolver
            .resolve_package_source(self.descriptor(id), self.module_path.clone(), ast)
            .map_err(|e| ProviderError::other(e.to_string()))?;

        if let Ok(mut cache) = self.cache.write() {
            *cache = Some(package_source.clone());
        }
        Ok(package_source)
    }
}

pub fn single_file_provider(
    package_id: PackageId,
    module_path: QualifiedPath,
    path: PathBuf,
    frontend: Box<dyn LanguageFrontend>,
    parse_mode: FrontendParseMode,
) -> Arc<dyn PackageProvider> {
    Arc::new(SingleFilePackageProvider::new(
        package_id, module_path, path, frontend, parse_mode,
    ))
}

/// Wraps an already-in-memory, non-file-backed `File` (e.g. `eval_script`'s
/// synthetic `"<eval>"` script, which has no path to lazily read) as a
/// one-member package. A real difference from `single_file_provider` above —
/// not a file to parse lazily, just an AST that already exists — not a
/// second file-focused code path.
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
    source: PackageSource,
}

impl PackageProvider for InMemoryPackageProvider {
    fn list_packages(&self) -> ProviderResult<Vec<PackageId>> {
        Ok(vec![self.package_id.clone()])
    }

    fn load_package_metadata(&self, id: &PackageId) -> ProviderResult<Arc<PackageDescriptor>> {
        if id != &self.package_id {
            return Err(ProviderError::PackageNotFound(id.clone()));
        }
        Ok(self.descriptor.clone())
    }

    fn load_package_source(&self, id: &PackageId) -> ProviderResult<PackageSource> {
        if id != &self.package_id {
            return Err(ProviderError::PackageNotFound(id.clone()));
        }
        Ok(self.source.clone())
    }

    fn refresh(&self) -> ProviderResult<()> {
        Ok(())
    }
}
