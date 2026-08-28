use std::path::Path;
use std::sync::Arc;

use fp_core::ast::module::{ModuleDescriptor, ModuleId, ModuleLanguage};
use fp_core::ast::package::graph::PackageGraph;
use fp_core::ast::package::provider::{PackageProvider, ProviderError, ProviderResult};
use fp_core::ast::package::{
    AstPackage, DependencyDescriptor, DependencyKind, PackageDescriptor, PackageId, PackageItem,
    PackageMetadata,
};
use fp_core::ast::path::QualifiedPath;
use fp_core::ast::{File, Item, ItemKind};
use fp_core::frontend::LanguageFrontend;
use fp_core::vfs::{UnixFileSystem, VirtualPath};

use crate::FerroFrontend;
use crate::embedded_std;
use crate::module_source::FerroModuleSourceResolver;

/// `PackageProvider` for the embedded Ferro standard library. `std` is
/// baked into the binary (see `embedded_std`), so there's no real
/// filesystem to watch — `refresh` is a no-op.
pub struct FerroPhaseProvider;

const CORE_PACKAGE_NAME: &str = "core";
const ALLOC_PACKAGE_NAME: &str = "alloc";
const STD_PACKAGE_NAME: &str = "std";
const LIBC_PACKAGE_NAME: &str = "libc";

/// A dependency on `std` — `CompilerDriver::compile_dependencies` compiles
/// and installs the prelude for any declared dependency named `"std"`
/// generically, so a real FerroPhase source package opts into std/prelude
/// support by declaring this, the same way `std` itself declares `libc`
/// below (`load_package_metadata`'s `STD_PACKAGE_NAME` arm).
pub(crate) fn std_dependency() -> DependencyDescriptor {
    DependencyDescriptor {
        package: STD_PACKAGE_NAME.to_string(),
        resolved_package_id: Some(PackageId::new(STD_PACKAGE_NAME)),
        constraint: None,
        kind: DependencyKind::Normal,
        features: Vec::new(),
        optional: false,
        target: Default::default(),
    }
}

fn flatten_items(path: &QualifiedPath, items: &[Item], output: &mut Vec<PackageItem>) {
    for item in items {
        if let ItemKind::Module(module) = item.kind() {
            flatten_items(
                &path.with_segment(module.name.as_str().to_owned()),
                &module.items,
                output,
            );
        } else {
            output.push(PackageItem {
                module_path: path.clone(),
                item: item.clone(),
            });
        }
    }
}

fn load_embedded_package(
    package_name: &str,
    root: std::path::PathBuf,
    module_paths: &'static [&'static str],
    read: fn(&std::path::Path) -> Option<&'static str>,
) -> ProviderResult<AstPackage> {
    let frontend = FerroFrontend::new();
    let package_id = PackageId::new(package_name);
    let mut descriptors = Vec::new();
    let mut items = Vec::new();

    for relative_str in module_paths {
        let path = root.join(relative_str);
        let Some(source) = read(&path) else {
            continue;
        };
        let module_path = relative_to_module_segments(package_name, relative_str);
        if module_path.is_empty() {
            continue;
        }
        let result = frontend
            .parse_file(source, &path)
            .map_err(|e| ProviderError::other(format!("failed to parse {relative_str}: {e}")))?;
        flatten_items(
            &QualifiedPath::new(module_path.clone()),
            &result.ast.items,
            &mut items,
        );
        descriptors.push(ModuleDescriptor {
            id: ModuleId::new(module_path.join("::")),
            package: package_id.clone(),
            language: ModuleLanguage::Ferro,
            module_path,
            source: VirtualPath::from_path(&path),
            exports: Vec::new(),
            requires_features: Vec::new(),
        });
    }

    let module_ids = descriptors.iter().map(|desc| desc.id.clone()).collect();
    let package = PackageDescriptor {
        id: package_id.clone(),
        name: package_name.to_string(),
        version: None,
        manifest_path: VirtualPath::from_path(&root.join("fp.toml")),
        root: VirtualPath::from_path(&root),
        metadata: Default::default(),
        modules: module_ids,
    };
    let mut graph = PackageGraph::new(vec![package]);
    for descriptor in descriptors {
        graph.insert_module(descriptor);
    }
    let mut krate = AstPackage::new(PackageId::new(package_name), package_name, graph);
    krate.items = items;
    Ok(krate)
}

impl PackageProvider for FerroPhaseProvider {
    fn list_packages(&self) -> ProviderResult<Vec<PackageId>> {
        Ok(vec![
            PackageId::new(CORE_PACKAGE_NAME),
            PackageId::new(ALLOC_PACKAGE_NAME),
            PackageId::new(LIBC_PACKAGE_NAME),
            PackageId::new(STD_PACKAGE_NAME),
        ])
    }

    fn workspace_packages(&self) -> ProviderResult<Vec<PackageId>> {
        self.list_packages()
    }

    // Only ever blended in as a `CompositeProvider` *dependency* (std/libc),
    // never the primary `workspace` provider — `CompositeProvider::
    // intrinsic_normalizer` always defers to `self.workspace`'s own choice
    // instead, so this one is never actually consulted; `Noop` regardless.
    fn intrinsic_normalizer(&self) -> Box<dyn fp_core::intrinsics::IntrinsicNormalizer> {
        Box::new(fp_core::intrinsics::NoopIntrinsicNormalizer)
    }

    fn load_package_metadata(&self, id: &PackageId) -> ProviderResult<Arc<PackageDescriptor>> {
        let root = match id.as_str() {
            CORE_PACKAGE_NAME | ALLOC_PACKAGE_NAME | STD_PACKAGE_NAME | LIBC_PACKAGE_NAME => {
                embedded_std::package_root(id.as_str())
            }
            _ => return Err(ProviderError::PackageNotFound(id.clone())),
        };
        let mut metadata = PackageMetadata::default();
        for dependency in match id.as_str() {
            CORE_PACKAGE_NAME => &[][..],
            ALLOC_PACKAGE_NAME => &[CORE_PACKAGE_NAME, LIBC_PACKAGE_NAME][..],
            LIBC_PACKAGE_NAME => &[CORE_PACKAGE_NAME][..],
            STD_PACKAGE_NAME => &[CORE_PACKAGE_NAME, ALLOC_PACKAGE_NAME, LIBC_PACKAGE_NAME][..],
            _ => &[][..],
        } {
            metadata.dependencies.push(DependencyDescriptor {
                package: dependency.to_string(),
                resolved_package_id: Some(PackageId::new(*dependency)),
                constraint: None,
                kind: DependencyKind::Normal,
                features: Vec::new(),
                optional: false,
                target: Default::default(),
            });
        }
        Ok(Arc::new(PackageDescriptor {
            id: id.clone(),
            name: id.as_str().to_string(),
            version: None,
            manifest_path: VirtualPath::from_path(&root.join("fp.toml")),
            root: VirtualPath::from_path(&root),
            metadata,
            modules: Vec::new(),
        }))
    }

    fn refresh(&self) -> ProviderResult<()> {
        Ok(())
    }

    fn load_package_source(&self, id: &PackageId) -> ProviderResult<AstPackage> {
        match id.as_str() {
            CORE_PACKAGE_NAME | ALLOC_PACKAGE_NAME | STD_PACKAGE_NAME | LIBC_PACKAGE_NAME => {
                load_embedded_package(
                    id.as_str(),
                    embedded_std::package_root(id.as_str()),
                    embedded_std::package_paths(id.as_str()),
                    embedded_std::read,
                )
            }
            _ => Err(ProviderError::PackageNotFound(id.clone())),
        }
    }
}

/// `PackageProvider` wrapping a single already-parsed `File` as a
/// one-member package, discovering any real `mod foo;` sibling modules on
/// disk via `FerroModuleSourceResolver` — the correct mechanism for a
/// genuinely standalone file with no enclosing package/manifest.
struct InputPackageProvider {
    package_id: PackageId,
    descriptor: Arc<PackageDescriptor>,
    source: AstPackage,
}

impl InputPackageProvider {
    fn new(
        package_id: PackageId,
        module_path: QualifiedPath,
        source: File,
    ) -> ProviderResult<Self> {
        let descriptor = PackageDescriptor {
            id: package_id.clone(),
            name: package_id.as_str().to_owned(),
            version: None,
            manifest_path: VirtualPath::from_path(&source.path),
            root: VirtualPath::from_path(source.path.parent().unwrap_or(Path::new("."))),
            metadata: PackageMetadata {
                dependencies: vec![std_dependency()],
                ..Default::default()
            },
            modules: Vec::new(),
        };
        let resolver = FerroModuleSourceResolver::new(Arc::new(UnixFileSystem::new("/")));
        let package_source =
            resolver.resolve_package_source(descriptor.clone(), module_path, source)?;
        Ok(Self {
            package_id,
            descriptor: Arc::new(descriptor),
            source: package_source,
        })
    }
}

impl PackageProvider for InputPackageProvider {
    fn list_packages(&self) -> ProviderResult<Vec<PackageId>> {
        Ok(vec![self.package_id.clone()])
    }

    fn workspace_packages(&self) -> ProviderResult<Vec<PackageId>> {
        self.list_packages()
    }

    fn intrinsic_normalizer(&self) -> Box<dyn fp_core::intrinsics::IntrinsicNormalizer> {
        Box::new(crate::normalization::FerroIntrinsicNormalizer::new())
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

/// Wraps an already-parsed single file as a one-member `PackageProvider`,
/// via `InputPackageProvider` (disk-based sibling-module discovery through
/// `FerroModuleSourceResolver` — the correct mechanism for a genuinely
/// standalone file with no enclosing package/manifest).
pub fn single_file_provider(
    package_id: PackageId,
    module_path: QualifiedPath,
    source: File,
) -> fp_core::error::Result<Arc<dyn PackageProvider>> {
    let provider = InputPackageProvider::new(package_id, module_path, source)
        .map_err(|e| fp_core::error::Error::from(e.to_string()))?;
    Ok(Arc::new(provider))
}

fn relative_to_module_segments(package_name: &str, relative: &str) -> Vec<String> {
    let mut segments: Vec<String> = vec![package_name.to_string()];
    let parts: Vec<&str> = relative.trim_end_matches(".fp").split('/').collect();
    if parts.len() == 1 && parts[0] == "mod" {
        return segments;
    }
    for part in parts {
        if part == "mod" {
            continue;
        }
        segments.push(part.to_string());
    }
    segments
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn exposes_rust_style_package_graph() {
        let provider = FerroPhaseProvider;
        let packages = provider
            .list_packages()
            .expect("embedded provider should list packages");
        assert_eq!(
            packages,
            vec![
                PackageId::new("core"),
                PackageId::new("alloc"),
                PackageId::new("libc"),
                PackageId::new("std"),
            ]
        );

        let dependencies = |package: &str| {
            provider
                .load_package_metadata(&PackageId::new(package))
                .expect("embedded package metadata should load")
                .metadata
                .dependencies
                .iter()
                .map(|dependency| dependency.package.as_str().to_owned())
                .collect::<Vec<_>>()
        };
        assert_eq!(dependencies("core"), Vec::<String>::new());
        assert_eq!(dependencies("alloc"), vec!["core", "libc"]);
        assert_eq!(dependencies("libc"), vec!["core"]);
        assert_eq!(dependencies("std"), vec!["core", "alloc", "libc"]);
    }

    #[test]
    fn package_sources_have_package_qualified_roots() {
        let provider = FerroPhaseProvider;
        for package in ["core", "alloc", "libc", "std"] {
            let source = provider
                .load_package_source(&PackageId::new(package))
                .expect("embedded package source should load");
            assert!(
                source.module_paths.iter().all(|path| path
                    .segments
                    .first()
                    .is_some_and(|segment| segment == package)),
                "all {package} modules should be rooted at {package}"
            );
        }
    }
}
