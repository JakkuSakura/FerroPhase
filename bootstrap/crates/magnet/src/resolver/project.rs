use std::collections::{BTreeMap, HashSet};
use std::path::{Path, PathBuf};

use fp_core::module::{ModuleDescriptor, ModuleId, ModuleLanguage};
use fp_core::package::{DependencyDescriptor, DependencyKind, PackageDescriptor, PackageId, PackageMetadata, TargetFilter};
use fp_core::package::graph::PackageGraph;
use fp_core::vfs::VirtualPath;

use crate::configs::{DependencyConfig, ManifestConfig, PackageConfig, WorkspaceConfig};
use crate::models::{DependencyModel, DependencyKind as ModelDepKind, PackageModel, WorkspaceModel};
use crate::utils::{collect_sources, find_furthest_manifest, get_table, map_get_string, map_get_string_list, read_toml_file};

pub fn resolve_workspace(path: &Path) -> crate::Result<WorkspaceModel> {
    let (root, manifest) = find_furthest_manifest(path)?;
    let (workspace_config, packages) = if manifest.file_name().and_then(|n| n.to_str()) == Some("Magnet.toml") {
        resolve_workspace_from_magnet(&root, &manifest)?
    } else {
        resolve_workspace_from_cargo(&root, &manifest)?
    };
    Ok(WorkspaceModel {
        root_path: root,
        manifest_path: manifest,
        workspace: workspace_config,
        packages,
    })
}

pub fn resolve_graph(path: &Path) -> crate::Result<PackageGraph> {
    let workspace = resolve_workspace(path)?;
    build_graph_from_workspace(&workspace)
}

fn resolve_workspace_from_magnet(root: &Path, manifest: &Path) -> crate::Result<(WorkspaceConfig, Vec<PackageModel>)> {
    let config = ManifestConfig::from_file(manifest)?;
    let workspace = config.workspace.clone().unwrap_or_default();
    let members = resolve_workspace_members(root, &workspace)?;
    let packages = collect_packages_from_members(&members)?;
    Ok((workspace, packages))
}

fn resolve_workspace_from_cargo(root: &Path, manifest: &Path) -> crate::Result<(WorkspaceConfig, Vec<PackageModel>)> {
    let value = read_toml_file(manifest)?;
    let workspace = if let Some(workspace_table) = get_table(&value, "workspace") {
        WorkspaceConfig {
            members: map_get_string_list(workspace_table, "members"),
            exclude: map_get_string_list(workspace_table, "exclude"),
        }
    } else {
        WorkspaceConfig::default()
    };
    let members = resolve_workspace_members(root, &workspace)?;
    let packages = collect_packages_from_members(&members)?;
    let root_package = parse_package_from_manifest(manifest)?;
    let mut list = packages;
    if let Some(root_package) = root_package {
        if !list.iter().any(|pkg| pkg.name == root_package.name) {
            list.push(root_package);
        }
    }
    Ok((workspace, list))
}

fn resolve_workspace_members(root: &Path, workspace: &WorkspaceConfig) -> crate::Result<Vec<PathBuf>> {
    let mut members = Vec::new();
    let mut exclude = HashSet::new();
    for pattern in &workspace.exclude {
        for path in crate::utils::glob_relative(root, pattern)? {
            exclude.insert(path);
        }
    }
    if workspace.members.is_empty() {
        return Ok(members);
    }
    for pattern in &workspace.members {
        for path in crate::utils::glob_relative(root, pattern)? {
            if exclude.contains(&path) {
                continue;
            }
            members.push(path);
        }
    }
    Ok(members)
}

fn collect_packages_from_members(members: &[PathBuf]) -> crate::Result<Vec<PackageModel>> {
    let mut packages = Vec::new();
    for member in members {
        let magnet = member.join("Magnet.toml");
        let cargo = member.join("Cargo.toml");
        let manifest = if magnet.exists() { magnet } else { cargo };
        if !manifest.exists() {
            continue;
        }
        if let Some(pkg) = parse_package_from_manifest(&manifest)? {
            packages.push(pkg);
        }
    }
    Ok(packages)
}

fn parse_package_from_manifest(path: &Path) -> crate::Result<Option<PackageModel>> {
    let value = read_toml_file(path)?;
    let Some(table) = get_table(&value, "package") else {
        return Ok(None);
    };
    let config = PackageConfig {
        name: map_get_string(table, "name"),
        version: map_get_string(table, "version"),
        edition: map_get_string(table, "edition"),
        authors: map_get_string_list(table, "authors"),
        description: map_get_string(table, "description"),
        license: map_get_string(table, "license"),
        keywords: map_get_string_list(table, "keywords"),
    };
    let name = match &config.name {
        Some(name) => name.clone(),
        None => return Ok(None),
    };
    let root_path = path.parent().unwrap_or(Path::new(".")).to_path_buf();
    let dependencies = parse_dependencies_from_manifest(&value, ModelDepKind::Normal);
    let dev_dependencies = parse_dependencies_from_manifest(&value, ModelDepKind::Development);
    let build_dependencies = parse_dependencies_from_manifest(&value, ModelDepKind::Build);
    let mut all = Vec::new();
    all.extend(dependencies);
    all.extend(dev_dependencies);
    all.extend(build_dependencies);
    Ok(Some(PackageModel {
        name,
        version: config.version.clone(),
        root_path,
        manifest_path: path.to_path_buf(),
        metadata: config,
        dependencies: all,
    }))
}

fn parse_dependencies_from_manifest(value: &fp_core::ast::Value, kind: ModelDepKind) -> Vec<DependencyModel> {
    let key = match kind {
        ModelDepKind::Normal => "dependencies",
        ModelDepKind::Development => "dev-dependencies",
        ModelDepKind::Build => "build-dependencies",
    };
    let Some(table) = get_table(value, key) else {
        return Vec::new();
    };
    let mut deps = Vec::new();
    for entry in &table.entries {
        let name = match &entry.key {
            fp_core::ast::Value::String(s) => s.value.clone(),
            _ => continue,
        };
        let dep = parse_dependency_value(&name, &entry.value, kind.clone());
        deps.push(dep);
    }
    deps
}

fn parse_dependency_value(name: &str, value: &fp_core::ast::Value, kind: ModelDepKind) -> DependencyModel {
    match value {
        fp_core::ast::Value::String(s) => DependencyModel {
            name: name.to_string(),
            version: Some(s.value.clone()),
            path: None,
            optional: false,
            features: Vec::new(),
            kind,
        },
        fp_core::ast::Value::Map(map) => {
            let version = map_get_string(map, "version");
            let path = map_get_string(map, "path");
            let optional = map.entries.iter().find_map(|entry| match &entry.key {
                fp_core::ast::Value::String(s) if s.value == "optional" => match &entry.value {
                    fp_core::ast::Value::Bool(b) => Some(b.value),
                    _ => None,
                },
                _ => None,
            }).unwrap_or(false);
            let features = map_get_string_list(map, "features");
            DependencyModel {
                name: name.to_string(),
                version,
                path,
                optional,
                features,
                kind,
            }
        }
        _ => DependencyModel {
            name: name.to_string(),
            version: None,
            path: None,
            optional: false,
            features: Vec::new(),
            kind,
        },
    }
}

fn build_graph_from_workspace(workspace: &WorkspaceModel) -> crate::Result<PackageGraph> {
    let mut graph = PackageGraph::new(Vec::new());
    for package in &workspace.packages {
        let descriptor = package_descriptor(package)?;
        graph.insert_package(descriptor);
    }
    for package in &workspace.packages {
        let modules = collect_modules_for_package(package)?;
        for module in modules {
            graph.insert_module(module);
        }
    }
    Ok(graph)
}

fn package_descriptor(package: &PackageModel) -> crate::Result<PackageDescriptor> {
    let metadata = package_metadata(package);
    Ok(PackageDescriptor {
        id: PackageId::new(package.name.clone()),
        name: package.name.clone(),
        version: package.version.clone(),
        manifest_path: VirtualPath::from_path(&package.manifest_path),
        root: VirtualPath::from_path(&package.root_path),
        metadata,
        modules: Vec::new(),
    })
}

fn package_metadata(package: &PackageModel) -> PackageMetadata {
    let mut metadata = PackageMetadata::default();
    metadata.edition = package.metadata.edition.clone();
    metadata.authors = package.metadata.authors.clone();
    metadata.description = package.metadata.description.clone();
    metadata.license = package.metadata.license.clone();
    metadata.keywords = package.metadata.keywords.clone();
    metadata.dependencies = package
        .dependencies
        .iter()
        .map(|dep| DependencyDescriptor {
            package: dep.name.clone(),
            constraint: dep.version.clone(),
            kind: match dep.kind {
                ModelDepKind::Normal => DependencyKind::Normal,
                ModelDepKind::Development => DependencyKind::Development,
                ModelDepKind::Build => DependencyKind::Build,
            },
            features: dep.features.clone(),
            optional: dep.optional,
            target: TargetFilter::default(),
        })
        .collect();
    metadata
}

fn collect_modules_for_package(package: &PackageModel) -> crate::Result<Vec<ModuleDescriptor>> {
    let src_root = package.root_path.join("src");
    let mut modules = Vec::new();
    let sources = collect_sources(&src_root, &["fp", "rs"])?;
    for source in sources {
        let language = match source.extension().and_then(|ext| ext.to_str()) {
            Some("rs") => ModuleLanguage::Rust,
            _ => ModuleLanguage::Ferro,
        };
        let module_path = module_path_from_source(&src_root, &source);
        let id = ModuleId::new(module_path.join("::"));
        modules.push(ModuleDescriptor {
            id,
            package: PackageId::new(package.name.clone()),
            language,
            module_path,
            source: VirtualPath::from_path(&source),
            exports: Vec::new(),
            requires_features: Vec::new(),
        });
    }
    Ok(modules)
}

fn module_path_from_source(src_root: &Path, source: &Path) -> Vec<String> {
    let rel = source.strip_prefix(src_root).unwrap_or(source);
    let mut parts = rel
        .components()
        .filter_map(|component| component.as_os_str().to_str())
        .map(|segment| segment.to_string())
        .collect::<Vec<_>>();
    if let Some(last) = parts.last_mut() {
        if let Some(stem) = Path::new(last).file_stem().and_then(|s| s.to_str()) {
            *last = stem.to_string();
        }
    }
    if parts.last().map(|v| v == "mod").unwrap_or(false) {
        parts.pop();
    }
    parts
}

fn resolve_dependency_config(dep: &DependencyModel) -> DependencyConfig {
    DependencyConfig {
        version: dep.version.clone(),
        path: dep.path.clone(),
        optional: dep.optional,
        features: dep.features.clone(),
    }
}

pub fn dependency_map_for_package(package: &PackageModel) -> BTreeMap<String, DependencyConfig> {
    let mut map = BTreeMap::new();
    for dep in &package.dependencies {
        map.insert(dep.name.clone(), resolve_dependency_config(dep));
    }
    map
}
