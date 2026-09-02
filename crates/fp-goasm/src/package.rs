use std::path::{Path, PathBuf};
use std::sync::Arc;

use fp_core::ast::module::{ModuleDescriptor, ModuleLanguage};
use fp_core::ast::package::PackageDescriptor;
use fp_core::ast::package::provider::{PackageProvider, ProviderResult};
use fp_core::ast::package::{AstPackage, PackageDescriptor, PackageId, PackageItem};
use fp_core::ast::path::QualifiedPath;
use fp_core::vfs::VirtualPath;

#[derive(Debug)]
pub struct GoPackageProvider {
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
        Ok(vec![self.package_id()])
    }

    fn workspace_packages(&self) -> ProviderResult<Vec<PackageId>> {
        self.list_packages()
    }

    fn intrinsic_normalizer(&self) -> Box<dyn fp_core::intrinsics::IntrinsicNormalizer> {
        Box::new(fp_core::intrinsics::NoopIntrinsicNormalizer)
    }

    fn load_package_metadata(&self, id: &PackageId) -> ProviderResult<Arc<PackageDescriptor>> {
        let package = self.snapshot()?;
        if &package.package_id != id {
            return Err(
                fp_core::ast::package::provider::ProviderError::PackageNotFound(id.clone()),
            );
        }
        package
            .package
            .package(id)
            .cloned()
            .map(Arc::new)
            .ok_or_else(|| {
                fp_core::ast::package::provider::ProviderError::PackageNotFound(id.clone())
            })
    }

    fn load_package_source(&self, id: &PackageId) -> ProviderResult<AstPackage> {
        let package = self.snapshot()?;
        if &package.package_id != id {
            return Err(
                fp_core::ast::package::provider::ProviderError::PackageNotFound(id.clone()),
            );
        }
        Ok(package)
    }

    fn refresh(&self) -> ProviderResult<()> {
        Ok(())
    }
}

impl GoPackageProvider {
    fn package_id(&self) -> PackageId {
        PackageId::new(
            self.root
                .file_name()
                .and_then(|name| name.to_str())
                .unwrap_or("main"),
        )
    }

    fn snapshot(&self) -> ProviderResult<AstPackage> {
        if !self.root.is_dir() {
            return Err(fp_core::ast::package::provider::ProviderError::other(
                format!(
                    "GoASM project root is not a directory: {}",
                    self.root.display()
                ),
            ));
        }

        let package_id = self.package_id();
        let mut files = Vec::new();
        collect_goasm_files(&self.root, &mut files)?;
        files.sort();
        if files.is_empty() {
            return Err(fp_core::ast::package::provider::ProviderError::other(
                format!(
                    "GoASM project contains no `.goasm` files: {}",
                    self.root.display()
                ),
            ));
        }

        let mut descriptors = Vec::with_capacity(files.len());
        let mut items = Vec::with_capacity(files.len());
        let mut module_paths = std::collections::HashSet::new();
        for file in files {
            let relative = file.strip_prefix(&self.root).map_err(|error| {
                fp_core::ast::package::provider::ProviderError::other(format!(
                    "failed to relativize GoASM source {}: {error}",
                    file.display()
                ))
            })?;
            let module_path = module_path_for(relative);
            let text = std::fs::read_to_string(&file).map_err(|error| {
                fp_core::ast::package::provider::ProviderError::other(format!(
                    "failed to read GoASM source {}: {error}",
                    file.display()
                ))
            })?;
            let (lir, _) = crate::parse_program(&text).map_err(|error| {
                fp_core::ast::package::provider::ProviderError::other(format!(
                    "failed to parse GoASM source {}: {error}",
                    file.display()
                ))
            })?;
            let module_id = module_path.to_key();
            module_paths.insert(module_path.clone());
            descriptors.push(ModuleDescriptor {
                id: module_id,
                package: package_id.clone(),
                language: ModuleLanguage::Other("goasm".to_string()),
                module_path: module_path.segments.clone(),
                source: VirtualPath::from_path(&file),
                exports: Vec::new(),
                requires_features: Vec::new(),
            });
            items.push(PackageItem {
                module_path,
                item: fp_core::ast::Item::precompiled_lir(lir),
            });
        }

        let descriptor = PackageDescriptor {
            id: package_id.clone(),
            name: package_id.as_str().to_string(),
            version: None,
            manifest_path: VirtualPath::from_path(&self.root.join("goasm.toml")),
            root: VirtualPath::from_path(&self.root),
            metadata: Default::default(),
        };
        let graph = descriptor;
        let package_name = package_id.as_str().to_string();
        let mut package = AstPackage::new(package_id, package_name, graph, Vec::new());
        package.modules.push(fp_core::ast::Module {
            attrs: Vec::new(),
            name: fp_core::ast::Ident::new(""),
            collected_items: Vec::new(),
            items,
            visibility: fp_core::ast::Visibility::Public,
            is_external: false,
        });
        Ok(package)
    }
}

fn collect_goasm_files(root: &Path, files: &mut Vec<PathBuf>) -> ProviderResult<()> {
    for entry in std::fs::read_dir(root).map_err(|error| {
        fp_core::ast::package::provider::ProviderError::other(format!(
            "failed to read GoASM project directory {}: {error}",
            root.display()
        ))
    })? {
        let entry = entry.map_err(|error| {
            fp_core::ast::package::provider::ProviderError::other(format!(
                "failed to inspect GoASM project directory {}: {error}",
                root.display()
            ))
        })?;
        let path = entry.path();
        if path.is_dir() {
            collect_goasm_files(&path, files)?;
        } else if path
            .extension()
            .and_then(|extension| extension.to_str())
            .is_some_and(|extension| extension.eq_ignore_ascii_case("goasm"))
        {
            files.push(path);
        }
    }
    Ok(())
}

fn module_path_for(relative: &Path) -> QualifiedPath {
    let mut segments = Vec::new();
    for component in relative.components() {
        let std::path::Component::Normal(segment) = component else {
            continue;
        };
        let Some(segment) = segment.to_str() else {
            continue;
        };
        if let Some(stem) = segment.strip_suffix(".goasm") {
            segments.push(stem.to_string());
        } else {
            segments.push(segment.to_string());
        }
    }
    QualifiedPath::new(segments)
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
