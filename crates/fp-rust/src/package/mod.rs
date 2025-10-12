use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::{Arc, RwLock};

use cargo_metadata::{
    Dependency, DependencyKind as CargoDepKind, Metadata, MetadataCommand, Package, Target,
};

use fp_core::module::{ModuleDescriptor, ModuleId, ModuleLanguage};
use fp_core::package::{
    DependencyDescriptor, DependencyKind, PackageDescriptor, PackageId, PackageMetadata,
    TargetFilter,
};
use fp_core::vfs::VirtualPath;

use fp_core::package::provider::{
    ModuleProvider, ModuleSource, PackageProvider, ProviderError, ProviderResult,
};

pub struct CargoPackageProvider {
    workspace_root: PathBuf,
    packages: RwLock<HashMap<PackageId, Arc<PackageDescriptor>>>,
    modules: RwLock<HashMap<ModuleId, Arc<ModuleDescriptor>>>,
    modules_by_package: RwLock<HashMap<PackageId, Vec<ModuleId>>>,
}

impl CargoPackageProvider {
    pub fn new(workspace_root: PathBuf) -> Self {
        Self {
            workspace_root,
            packages: RwLock::new(HashMap::new()),
            modules: RwLock::new(HashMap::new()),
            modules_by_package: RwLock::new(HashMap::new()),
        }
    }

    fn workspace_manifest(&self) -> PathBuf {
        self.workspace_root.join("Cargo.toml")
    }

    fn metadata(&self) -> ProviderResult<Metadata> {
        MetadataCommand::new()
            .manifest_path(&self.workspace_manifest())
            .exec()
            .map_err(|err| ProviderError::metadata(err.to_string()))
    }

    fn convert_package(
        &self,
        pkg: &Package,
        module_map: &mut HashMap<ModuleId, Arc<ModuleDescriptor>>,
        modules_by_pkg: &mut HashMap<PackageId, Vec<ModuleId>>,
    ) -> ProviderResult<PackageDescriptor> {
        let manifest_path = pkg.manifest_path.clone().into_std_path_buf();
        let root = manifest_path
            .parent()
            .ok_or_else(|| {
                ProviderError::other(format!(
                    "manifest {} has no parent directory",
                    manifest_path.display()
                ))
            })?
            .to_path_buf();

        let package_id = PackageId::new(&pkg.name);

        let metadata = PackageMetadata {
            edition: Some(pkg.edition.to_string()),
            authors: pkg.authors.clone(),
            description: pkg.description.clone(),
            license: pkg.license.clone(),
            keywords: pkg.keywords.clone(),
            registry: pkg
                .publish
                .as_ref()
                .and_then(|publish| publish.first().cloned()),
            features: pkg
                .features
                .iter()
                .map(|(key, values)| (key.clone(), values.clone()))
                .collect(),
            dependencies: pkg.dependencies.iter().map(convert_dependency).collect(),
        };

        let mut module_ids = Vec::new();
        for target in &pkg.targets {
            if !is_rust_target(target) {
                continue;
            }
            let descriptor = build_module_descriptor(target, &package_id, &root)?;
            module_ids.push(descriptor.id.clone());
            module_map.insert(descriptor.id.clone(), Arc::new(descriptor));
        }
        modules_by_pkg.insert(package_id.clone(), module_ids.clone());

        let descriptor = PackageDescriptor {
            id: package_id,
            name: pkg.name.clone(),
            version: Some(pkg.version.clone()),
            manifest_path: VirtualPath::from_path(&manifest_path),
            root: VirtualPath::from_path(&root),
            metadata,
            modules: module_ids,
        };

        Ok(descriptor)
    }
}

impl PackageProvider for CargoPackageProvider {
    fn list_packages(&self) -> ProviderResult<Vec<PackageId>> {
        Ok(self.packages.read().unwrap().keys().cloned().collect())
    }

    fn load_package(&self, id: &PackageId) -> ProviderResult<Arc<PackageDescriptor>> {
        self.packages
            .read()
            .unwrap()
            .get(id)
            .cloned()
            .ok_or_else(|| ProviderError::PackageNotFound(id.clone()))
    }

    fn refresh(&self) -> ProviderResult<()> {
        let metadata = self.metadata()?;

        let mut package_map = HashMap::new();
        let mut module_map = HashMap::new();
        let mut modules_by_pkg = HashMap::new();

        for member in &metadata.workspace_members {
            let pkg = metadata
                .packages
                .iter()
                .find(|candidate| &candidate.id == member)
                .ok_or_else(|| ProviderError::other(format!("missing package for id {member}")))?;

            let descriptor = self.convert_package(pkg, &mut module_map, &mut modules_by_pkg)?;
            package_map.insert(descriptor.id.clone(), Arc::new(descriptor));
        }

        *self.packages.write().unwrap() = package_map;
        *self.modules.write().unwrap() = module_map;
        *self.modules_by_package.write().unwrap() = modules_by_pkg;
        Ok(())
    }
}

impl ModuleSource for CargoPackageProvider {
    fn modules_for_package(&self, id: &PackageId) -> ProviderResult<Vec<ModuleId>> {
        self.modules_by_package
            .read()
            .unwrap()
            .get(id)
            .cloned()
            .ok_or_else(|| ProviderError::PackageNotFound(id.clone()))
    }

    fn load_module_descriptor(&self, id: &ModuleId) -> ProviderResult<Arc<ModuleDescriptor>> {
        self.modules
            .read()
            .unwrap()
            .get(id)
            .cloned()
            .ok_or_else(|| ProviderError::ModuleNotFound(id.clone()))
    }
}

pub struct RustModuleProvider<P> {
    packages: Arc<P>,
}

impl<P> RustModuleProvider<P>
where
    P: PackageProvider + ModuleSource + 'static,
{
    pub fn new(packages: Arc<P>) -> Self {
        Self { packages }
    }
}

impl<P> ModuleProvider for RustModuleProvider<P>
where
    P: PackageProvider + ModuleSource + 'static,
{
    fn modules_for_package(&self, id: &PackageId) -> ProviderResult<Vec<ModuleId>> {
        self.packages.modules_for_package(id)
    }

    fn load_module(&self, id: &ModuleId) -> ProviderResult<Arc<ModuleDescriptor>> {
        self.packages.load_module_descriptor(id)
    }

    fn refresh(&self, _id: &PackageId) -> ProviderResult<()> {
        self.packages.refresh()
    }
}

// -----------------------------------------------------------------------------
// Helpers
// -----------------------------------------------------------------------------

fn convert_dependency(dep: &Dependency) -> DependencyDescriptor {
    let kind = match dep.kind {
        CargoDepKind::Development => DependencyKind::Development,
        CargoDepKind::Build => DependencyKind::Build,
        CargoDepKind::Normal | CargoDepKind::Unknown => DependencyKind::Normal,
    };

    let cfg = dep.target.as_ref().map(|platform| platform.to_string());

    DependencyDescriptor {
        package: dep.rename.clone().unwrap_or_else(|| dep.name.clone()),
        constraint: Some(dep.req.clone()),
        kind,
        features: dep.features.clone(),
        optional: dep.optional,
        target: TargetFilter {
            cfg,
            languages: Vec::new(),
        },
    }
}

fn is_rust_target(target: &Target) -> bool {
    target.kind.iter().any(|kind| match kind.as_str() {
        "lib" | "rlib" | "dylib" | "staticlib" | "cdylib" | "proc-macro" | "bin" | "test"
        | "bench" | "example" => true,
        _ => false,
    })
}

fn build_module_descriptor(
    target: &Target,
    package_id: &PackageId,
    crate_root: &Path,
) -> ProviderResult<ModuleDescriptor> {
    let src_path = target.src_path.clone().into_std_path_buf();
    let relative = src_path
        .strip_prefix(crate_root)
        .unwrap_or(&src_path)
        .to_path_buf();

    let module_path = module_path_from_relative(&relative);
    let module_id = make_module_id(package_id, &module_path);

    Ok(ModuleDescriptor {
        id: module_id,
        package: package_id.clone(),
        language: ModuleLanguage::Rust,
        module_path,
        source: VirtualPath::from_path(&src_path),
        exports: Vec::new(),
        requires_features: target.required_features.clone(),
    })
}

fn module_path_from_relative(relative: &Path) -> Vec<String> {
    let mut components: Vec<String> = relative
        .components()
        .filter_map(|component| component.as_os_str().to_str().map(|s| s.to_string()))
        .collect();

    if !components.is_empty() && components[0] == "src" {
        components.remove(0);
    }

    if components.is_empty() {
        return vec!["lib".to_string()];
    }

    let last = components.pop().unwrap();
    let mut path = components;
    let stem = Path::new(&last)
        .file_stem()
        .and_then(|stem| stem.to_str())
        .unwrap_or(&last)
        .to_string();

    if stem != "mod" {
        path.push(stem);
    }

    if path.is_empty() {
        vec!["lib".to_string()]
    } else {
        path
    }
}

fn make_module_id(package: &PackageId, module_path: &[String]) -> ModuleId {
    if module_path.is_empty() {
        return ModuleId::new(package.as_str());
    }
    let mut identifier = String::from(package.as_str());
    identifier.push_str("::");
    identifier.push_str(&module_path.join("::"));
    ModuleId::new(identifier)
}
