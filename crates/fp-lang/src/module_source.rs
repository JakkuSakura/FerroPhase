use std::collections::HashSet;
use std::sync::Arc;

use fp_core::ast::{File, Item, ItemKind};
use fp_core::frontend::LanguageFrontend;
use fp_core::ast::path::QualifiedPath;
use fp_core::ast::module::{ModuleDescriptor, ModuleId, ModuleLanguage};
use fp_core::package::graph::PackageGraph;
use fp_core::package::provider::{ProviderError, ProviderResult};
use fp_core::package::{PackageDescriptor, PackageItem, PackageSource};
use fp_core::vfs::{VirtualFileSystem, VirtualPath};

use crate::FerroFrontend;

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

/// Resolves a Ferro root file and its declared external modules into one
/// package source snapshot. Filesystem access stays behind the VFS boundary.
pub struct FerroModuleSourceResolver {
    frontend: FerroFrontend,
    filesystem: Arc<dyn VirtualFileSystem>,
}

impl FerroModuleSourceResolver {
    pub fn new(filesystem: Arc<dyn VirtualFileSystem>) -> Self {
        Self {
            frontend: FerroFrontend::new(),
            filesystem,
        }
    }

    pub fn resolve_package_source(
        &self,
        mut package: PackageDescriptor,
        root_module_path: QualifiedPath,
        root_file: File,
    ) -> ProviderResult<PackageSource> {
        if package.id.as_str() != package_name_from_path(&root_module_path)? {
            return Err(ProviderError::other(format!(
                "root module {} does not belong to package {}",
                root_module_path.to_key(),
                package.id
            )));
        }

        let root_source_path = VirtualPath::from_path(&root_file.path);
        let mut modules = Vec::new();
        let mut items = Vec::new();
        let mut module_paths = HashSet::new();
        let mut source_paths = HashSet::new();

        self.collect_module(
            &package,
            root_module_path,
            root_source_path,
            root_file.items,
            true,
            &mut modules,
            &mut items,
            &mut module_paths,
            &mut source_paths,
        )?;

        modules.sort_by_key(|module| module.module_path.join("::"));
        let module_ids = modules.iter().map(|module| module.id.clone()).collect();
        package.modules = module_ids;

        let package_id = package.id.clone();
        let package_name = package.name.clone();
        let mut graph = PackageGraph::new(vec![package]);
        for module in modules {
            graph.insert_module(module);
        }

        let mut source = PackageSource::new(package_id, package_name, graph);
        source.items = items;
        source.module_paths = module_paths;
        Ok(source)
    }

    fn collect_module(
        &self,
        package: &PackageDescriptor,
        module_path: QualifiedPath,
        source_path: VirtualPath,
        module_items: Vec<Item>,
        is_root: bool,
        modules: &mut Vec<ModuleDescriptor>,
        items: &mut Vec<PackageItem>,
        module_paths: &mut HashSet<QualifiedPath>,
        source_paths: &mut HashSet<VirtualPath>,
    ) -> ProviderResult<()> {
        if !module_paths.insert(module_path.clone()) {
            return Err(ProviderError::other(format!(
                "duplicate module path {}",
                module_path.to_key()
            )));
        }
        if !source_paths.insert(source_path.clone()) {
            return Err(ProviderError::other(format!(
                "source file {} belongs to multiple modules",
                source_path
            )));
        }

        let module_id = ModuleId::new(module_path.to_key());
        modules.push(ModuleDescriptor {
            id: module_id,
            package: package.id.clone(),
            language: ModuleLanguage::Ferro,
            module_path: module_path.segments.clone(),
            source: source_path.clone(),
            exports: Vec::new(),
            requires_features: Vec::new(),
        });
        flatten_items(&module_path, &module_items, items);

        self.collect_external_declarations(
            package,
            &module_path,
            &source_path,
            &module_items,
            is_root,
            modules,
            items,
            module_paths,
            source_paths,
        )
    }

    fn collect_external_declarations(
        &self,
        package: &PackageDescriptor,
        parent_module_path: &QualifiedPath,
        parent_source_path: &VirtualPath,
        items: &[Item],
        is_root: bool,
        modules: &mut Vec<ModuleDescriptor>,
        source_items: &mut Vec<PackageItem>,
        module_paths: &mut HashSet<QualifiedPath>,
        source_paths: &mut HashSet<VirtualPath>,
    ) -> ProviderResult<()> {
        for item in items {
            let ItemKind::Module(module) = item.kind() else {
                continue;
            };
            if module.is_external {
                let child_path = parent_module_path.with_segment(module.name.as_str().to_owned());
                let (source_path, source) = self.load_external_module(
                    parent_source_path,
                    module.name.as_str(),
                    &child_path,
                    is_root,
                )?;
                let parsed = self
                    .frontend
                    .parse_file(&source, &source_path.to_path_buf())
                    .map_err(|error| {
                        ProviderError::other(format!(
                            "failed to parse module {} at {}: {}",
                            child_path.to_key(),
                            source_path,
                            error
                        ))
                    })?;
                self.collect_module(
                    package,
                    child_path,
                    source_path,
                    parsed.ast.items,
                    false,
                    modules,
                    source_items,
                    module_paths,
                    source_paths,
                )?;
            } else {
                self.collect_external_declarations(
                    package,
                    &parent_module_path.with_segment(module.name.as_str().to_owned()),
                    parent_source_path,
                    &module.items,
                    is_root,
                    modules,
                    source_items,
                    module_paths,
                    source_paths,
                )?;
            }
        }
        Ok(())
    }

    fn load_external_module(
        &self,
        parent_source_path: &VirtualPath,
        name: &str,
        module_path: &QualifiedPath,
        is_root: bool,
    ) -> ProviderResult<(VirtualPath, String)> {
        let parent = parent_source_path
            .parent()
            .ok_or_else(|| ProviderError::other("module source has no parent directory"))?;
        let source_native_path = parent_source_path.to_path_buf();
        let source_file_name = source_native_path
            .file_name()
            .and_then(|file| file.to_str());
        let source_directory = if source_file_name == Some("mod.fp") || is_root {
            parent
        } else if is_root {
            parent
        } else {
            let stem = source_native_path
                .file_stem()
                .and_then(|stem| stem.to_str())
                .ok_or_else(|| ProviderError::other("module source has no file stem"))?;
            parent.join(stem)
        };
        let file_path = source_directory.join(format!("{name}.fp"));
        let directory_path = source_directory.join(name).join("mod.fp");
        let file_exists = self.filesystem.exists(&file_path);
        let directory_exists = self.filesystem.exists(&directory_path);
        match (file_exists, directory_exists) {
            (true, true) => Err(ProviderError::other(format!(
                "ambiguous source for module {}: {} and {}",
                module_path.to_key(),
                file_path,
                directory_path
            ))),
            (false, false) => Err(ProviderError::ModuleNotFound(ModuleId::new(
                module_path.to_key(),
            ))),
            (true, false) => self.read_source(&file_path),
            (false, true) => self.read_source(&directory_path),
        }
    }

    fn read_source(&self, path: &VirtualPath) -> ProviderResult<(VirtualPath, String)> {
        let bytes = self
            .filesystem
            .read(path)
            .map_err(|error| ProviderError::other(format!("failed to read {path}: {error}")))?;
        let source = String::from_utf8(bytes)
            .map_err(|error| ProviderError::other(format!("invalid UTF-8 in {path}: {error}")))?;
        Ok((path.clone(), source))
    }
}

fn package_name_from_path(path: &QualifiedPath) -> ProviderResult<&str> {
    path.head()
        .ok_or_else(|| ProviderError::other("root module path has no package segment"))
}

#[cfg(test)]
mod tests {
    use super::*;
    use fp_core::package::{PackageId, PackageMetadata};
    use fp_core::vfs::{InMemoryFileSystem, VirtualFileSystem};

    fn package() -> PackageDescriptor {
        PackageDescriptor {
            id: PackageId::new("app"),
            name: "app".into(),
            version: None,
            manifest_path: VirtualPath::from_path("/app/Ferrophase.toml"),
            root: VirtualPath::from_path("/app"),
            metadata: PackageMetadata::default(),
            modules: Vec::new(),
        }
    }

    fn root_file(frontend: &FerroFrontend) -> File {
        frontend
            .parse_file(
                "mod modules; fn main() {}",
                std::path::Path::new("/app/main.fp"),
            )
            .expect("parse root module")
            .ast
    }

    #[test]
    fn resolves_nested_external_modules_with_rust_layout() {
        let filesystem = Arc::new(InMemoryFileSystem::new());
        filesystem
            .write(
                &VirtualPath::from_path("/app/modules/mod.fp"),
                b"mod helpers;",
            )
            .expect("write module root");
        filesystem
            .write(
                &VirtualPath::from_path("/app/modules/helpers.fp"),
                b"mod math;",
            )
            .expect("write helpers");
        filesystem
            .write(
                &VirtualPath::from_path("/app/modules/helpers/math.fp"),
                b"pub fn add(a: i64, b: i64) -> i64 { a + b }",
            )
            .expect("write math");

        let frontend = FerroFrontend::new();
        let source = FerroModuleSourceResolver::new(filesystem)
            .resolve_package_source(
                package(),
                QualifiedPath::new(vec!["app".into(), "main".into()]),
                root_file(&frontend),
            )
            .expect("resolve package source");

        let paths = source
            .module_paths
            .iter()
            .map(QualifiedPath::to_key)
            .collect::<HashSet<_>>();
        assert_eq!(paths.len(), 4);
        assert!(paths.contains("app::main"));
        assert!(paths.contains("app::main::modules"));
        assert!(paths.contains("app::main::modules::helpers"));
        assert!(paths.contains("app::main::modules::helpers::math"));
    }

    #[test]
    fn rejects_ambiguous_external_module_sources() {
        let filesystem = Arc::new(InMemoryFileSystem::new());
        for path in ["/app/modules.fp", "/app/modules/mod.fp"] {
            filesystem
                .write(&VirtualPath::from_path(path), b"pub const VALUE: i64 = 1;")
                .expect("write ambiguous module source");
        }

        let frontend = FerroFrontend::new();
        let error = FerroModuleSourceResolver::new(filesystem)
            .resolve_package_source(
                package(),
                QualifiedPath::new(vec!["app".into(), "main".into()]),
                root_file(&frontend),
            )
            .expect_err("ambiguous module source must fail");
        assert!(error.to_string().contains("ambiguous source"));
    }
}
