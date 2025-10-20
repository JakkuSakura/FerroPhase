use std::collections::{BTreeMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use fp_core::ast::{File, Ident, Item, ItemKind, Module, Node, NodeKind, Visibility};
use fp_core::error::{Error as CoreError, Result as CoreResult};
use fp_core::module::ModuleLanguage;
use fp_core::package::provider::{ModuleProvider, PackageProvider};
use fp_core::package::DependencyKind;
use fp_core::workspace::{
    WorkspaceDependency, WorkspaceDocument, WorkspaceModule, WorkspacePackage,
};

use crate::package::{CargoPackageProvider, RustModuleProvider};
use crate::parser::RustParser;

/// Parse every Rust module in the workspace described by the given Cargo manifest and return a
/// synthetic FerroPhase AST file that groups items under packages.
pub fn parse_cargo_workspace(manifest_path: &Path) -> CoreResult<Node> {
    let workspace_root = manifest_path
        .parent()
        .ok_or_else(|| CoreError::from("Cargo manifest has no parent directory"))?
        .to_path_buf();

    let provider = Arc::new(CargoPackageProvider::new(workspace_root));
    provider
        .refresh()
        .map_err(|err| CoreError::from(err.to_string()))?;
    let module_provider = RustModuleProvider::new(provider.clone());

    let mut package_items: BTreeMap<String, Vec<Item>> = BTreeMap::new();
    let mut visited_paths: HashSet<PathBuf> = HashSet::new();
    let mut parser = RustParser::new();

    for package_id in provider
        .list_packages()
        .map_err(|err| CoreError::from(err.to_string()))?
    {
        for module_id in module_provider
            .modules_for_package(&package_id)
            .map_err(|err| CoreError::from(err.to_string()))?
        {
            let descriptor = module_provider
                .load_module(&module_id)
                .map_err(|err| CoreError::from(err.to_string()))?;
            let path = descriptor.source.to_path_buf();
            let canonical = path.canonicalize().unwrap_or(path.clone());

            if !visited_paths.insert(canonical.clone()) {
                continue;
            }

            let source = fs::read_to_string(&canonical).map_err(CoreError::from)?;
            let file = parser.parse_file(&source, &canonical)?;
            let ast = Node::file(file);

            let entry = package_items
                .entry(package_id.as_str().to_string())
                .or_insert_with(Vec::new);

            match ast.kind() {
                NodeKind::File(file) => entry.extend(file.items.iter().cloned()),
                NodeKind::Item(item) => entry.push(item.clone()),
                NodeKind::Expr(expr) => entry.push(Item::from(ItemKind::Expr(expr.clone()))),
                NodeKind::Query(_) | NodeKind::Schema(_) => {
                    // Queries are not represented in Rust workspace aggregation.
                }
                NodeKind::Workspace(_) => {}
            }
        }
    }

    let mut items = Vec::new();
    for (package, package_ast) in package_items {
        if package_ast.is_empty() {
            continue;
        }
        let module = Module {
            name: Ident::new(sanitise_package_ident(&package)),
            items: package_ast,
            visibility: Visibility::Public,
        };
        items.push(Item::from(module));
    }

    let file = File {
        path: manifest_path.to_path_buf(),
        items,
    };

    Ok(Node::file(file))
}

fn sanitise_package_ident(name: &str) -> String {
    let mut ident = String::with_capacity(name.len());
    for ch in name.chars() {
        match ch {
            'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => ident.push(ch),
            '-' => ident.push('_'),
            _ => ident.push('_'),
        }
    }
    if ident.is_empty() {
        ident.push_str("pkg");
    }
    ident
}

pub fn summarize_cargo_workspace(manifest_path: &Path) -> CoreResult<WorkspaceDocument> {
    let workspace_root = manifest_path
        .parent()
        .ok_or_else(|| CoreError::from("Cargo manifest has no parent directory"))?
        .to_path_buf();

    let provider = Arc::new(CargoPackageProvider::new(workspace_root));
    provider
        .refresh()
        .map_err(|err| CoreError::from(err.to_string()))?;
    let module_provider = RustModuleProvider::new(provider.clone());

    let mut packages = Vec::new();

    for package_id in provider
        .list_packages()
        .map_err(|err| CoreError::from(err.to_string()))?
    {
        let descriptor = provider
            .load_package(&package_id)
            .map_err(|err| CoreError::from(err.to_string()))?;

        let mut modules = Vec::new();
        for module_id in module_provider
            .modules_for_package(&package_id)
            .map_err(|err| CoreError::from(err.to_string()))?
        {
            let module = module_provider
                .load_module(&module_id)
                .map_err(|err| CoreError::from(err.to_string()))?;

            let language = match module.language {
                ModuleLanguage::Ferro => Some("ferro".to_string()),
                ModuleLanguage::Rust => Some("rust".to_string()),
                ModuleLanguage::TypeScript => Some("typescript".to_string()),
                ModuleLanguage::Python => Some("python".to_string()),
                ModuleLanguage::Other(ref kind) => Some(kind.clone()),
            };

            let source_path = module.source.to_path_buf();
            let workspace_module =
                WorkspaceModule::new(module.id.to_string(), source_path.display().to_string())
                    .with_module_path(module.module_path.clone())
                    .with_language(language)
                    .with_required_features(module.requires_features.clone());

            modules.push(workspace_module);
        }

        modules.sort_by(|a, b| a.path.cmp(&b.path));

        let metadata = &descriptor.metadata;
        let mut features: Vec<String> = metadata.features.keys().cloned().collect();
        features.sort();

        let mut dependencies: Vec<WorkspaceDependency> = metadata
            .dependencies
            .iter()
            .map(|dep| {
                WorkspaceDependency::new(
                    dep.package.clone(),
                    Some(format_dependency_kind(dep.kind.clone())),
                )
            })
            .collect();
        dependencies.sort_by(|a, b| a.name.cmp(&b.name));

        let package = WorkspacePackage::new(
            descriptor.name.clone(),
            descriptor.manifest_path.to_string(),
            descriptor.root.to_string(),
        )
        .with_version(descriptor.version.as_ref().map(|v| v.to_string()))
        .with_modules(modules)
        .with_features(features)
        .with_dependencies(dependencies);

        packages.push(package);
    }

    packages.sort_by(|a, b| a.name.cmp(&b.name));

    Ok(WorkspaceDocument::new(manifest_path.to_string_lossy()).with_packages(packages))
}

fn format_dependency_kind(kind: DependencyKind) -> String {
    match kind {
        DependencyKind::Normal => "normal".to_string(),
        DependencyKind::Development => "dev".to_string(),
        DependencyKind::Build => "build".to_string(),
    }
}
