use fp_core::module::ModuleLanguage;
use fp_core::package::DependencyKind;
use fp_core::package::graph::PackageGraph;
use fp_core::workspace::{WorkspaceDependency, WorkspaceDocument, WorkspaceModule, WorkspacePackage};
use std::path::Path;

pub fn build_workspace_document(
    graph: &PackageGraph,
    manifest_path: &Path,
) -> WorkspaceDocument {
    let mut packages = Vec::new();
    for package in graph.packages() {
        let modules = build_modules(graph, &package.id);
        let dependencies = package
            .metadata
            .dependencies
            .iter()
            .map(|dep| WorkspaceDependency::new(dep.package.clone(), dependency_kind(&dep.kind)))
            .collect();
        let version = package.version.as_ref().map(|ver| ver.to_string());
        packages.push(
            WorkspacePackage::new(
                package.name.clone(),
                package.manifest_path.to_string(),
                package.root.to_string(),
            )
            .with_version(version)
            .with_modules(modules)
            .with_dependencies(dependencies),
        );
    }
    WorkspaceDocument::new(manifest_path.to_string_lossy()).with_packages(packages)
}

pub fn write_workspace_graph(
    graph: &PackageGraph,
    manifest_path: &Path,
    output_path: &Path,
) -> crate::Result<()> {
    let workspace = build_workspace_document(graph, manifest_path);
    let payload = serde_json::to_string_pretty(&workspace)?;
    if let Some(parent) = output_path.parent() {
        std::fs::create_dir_all(parent)?;
    }
    std::fs::write(output_path, payload)?;
    Ok(())
}

fn build_modules(
    graph: &PackageGraph,
    package_id: &fp_core::package::PackageId,
) -> Vec<WorkspaceModule> {
    let mut modules = Vec::new();
    let Some(module_ids) = graph.modules_for_package(package_id) else {
        return modules;
    };
    for module_id in module_ids {
        let Some(module) = graph.module(module_id) else {
            continue;
        };
        modules.push(
            WorkspaceModule::new(module.id.as_str().to_string(), module.source.to_string())
                .with_module_path(module.module_path.clone())
                .with_language(Some(module_language(&module.language)))
                .with_required_features(module.requires_features.clone()),
        );
    }
    modules
}

fn module_language(language: &ModuleLanguage) -> String {
    match language {
        ModuleLanguage::Ferro => "ferro".to_string(),
        ModuleLanguage::Rust => "rust".to_string(),
        ModuleLanguage::TypeScript => "typescript".to_string(),
        ModuleLanguage::Python => "python".to_string(),
        ModuleLanguage::Other(other) => other.clone(),
    }
}

fn dependency_kind(kind: &DependencyKind) -> Option<String> {
    match kind {
        DependencyKind::Normal => None,
        DependencyKind::Development => Some("dev".to_string()),
        DependencyKind::Build => Some("build".to_string()),
    }
}
