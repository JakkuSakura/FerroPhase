//! Command implementation for displaying workspace hierarchy as a tree

use crate::models::{DependencyModel, ManifestModel, NexusModel, PackageModel, WorkspaceModel};
use crate::registry::{RegistryClient, RegistryOptions};
use eyre::Result;
use std::collections::HashSet;
use std::path::Path;
use tracing::info;

/// Tree command for visualizing workspace structure
pub fn tree(config_path: &Path) -> Result<()> {
    // Create a nexus manager
    let manifest = ManifestModel::from_dir(config_path)?;

    let offline = env_flag_enabled("MAGNET_OFFLINE");
    let registry = Some(RegistryClient::new(RegistryOptions {
        offline,
        cache_dir: Some(manifest_root(&manifest).join("target").join("magnet")),
    })?);
    let mut visited = HashSet::new();
    let mut visited_registry = HashSet::new();
    print_manifest_tree(&manifest, 0, "", true, registry.as_ref(), &mut visited, &mut visited_registry)?;

    Ok(())
}

fn print_manifest_tree(
    manifest: &ManifestModel,
    depth: u32,
    prefix: &str,
    is_last: bool,
    registry: Option<&RegistryClient>,
    visited: &mut HashSet<String>,
    visited_registry: &mut HashSet<String>,
) -> Result<()> {
    match manifest {
        ManifestModel::Nexus(nexus) => print_nexus_tree(nexus, depth, prefix, registry, visited, visited_registry),
        ManifestModel::Workspace(workspace) => {
            print_workspace_tree(workspace, depth, prefix, is_last, registry, visited, visited_registry)
        }
        ManifestModel::Package(package) => print_package_tree(package, prefix, prefix, is_last, registry, visited, visited_registry),
    }
}
fn print_nexus_tree(
    nexus: &NexusModel,
    depth: u32,
    prefix: &str,
    registry: Option<&RegistryClient>,
    visited: &mut HashSet<String>,
    visited_registry: &mut HashSet<String>,
) -> Result<()> {
    // Print Nexus details
    info!(
        "{}{} 🧲 Nexus: {} ({})",
        "  ".repeat(depth as usize),
        prefix,
        nexus.name,
        nexus.root_path.display()
    );

    // Print workspace tree
    let workspaces = nexus.list_workspaces()?;
    for (idx, workspace) in workspaces.iter().enumerate() {
        let is_last_workspace = idx == workspaces.len() - 1;
        let workspace_prefix = if is_last_workspace {
            "└── "
        } else {
            "├── "
        };
        print_workspace_tree(
            workspace,
            depth + 1,
            workspace_prefix,
            is_last_workspace,
            registry,
            visited,
            visited_registry,
        )?;
    }
    // Print packages
    let packages = nexus.list_packages()?;
    for (idx, package) in packages.iter().enumerate() {
        let is_last_package = idx == packages.len() - 1;
        let package_prefix = if is_last_package {
            "└── "
        } else {
            "├── "
        };
        print_package_tree(
            package,
            prefix,
            package_prefix,
            is_last_package,
            registry,
            visited,
            visited_registry,
        )?;
    }
    Ok(())
}
/// Print the workspace tree
fn print_workspace_tree(
    workspace: &WorkspaceModel,
    depth: u32,
    prefix: &str,
    is_last: bool,
    registry: Option<&RegistryClient>,
    visited: &mut HashSet<String>,
    visited_registry: &mut HashSet<String>,
) -> Result<()> {
    // Print workspace name
    info!(
        "{}{} 🏢 Workspace: {} ({})",
        "  ".repeat(depth as usize),
        prefix,
        workspace.name,
        &workspace.root_path.display()
    );

    // Print workspace root
    let indent = if is_last { "    " } else { "│   " };

    let packages = workspace.list_packages()?;
    let package_map = packages
        .iter()
        .map(|pkg| (pkg.name.clone(), pkg.clone()))
        .collect::<std::collections::HashMap<_, _>>();
    for (idx, package) in packages.iter().enumerate() {
        let is_last_package = idx == packages.len() - 1;
        let package_prefix = if is_last_package {
            "└── "
        } else {
            "├── "
        };

        // Use the correct indentation for packages
        let package_indent = format!("{}{}", "  ".repeat(depth as usize), indent);
        print_package_tree_with_map(
            &package,
            package_indent.as_str(),
            package_prefix,
            is_last_package,
            &package_map,
            registry,
            visited,
            visited_registry,
        )?;
    }

    Ok(())
}

fn print_package_tree(
    package: &PackageModel,
    parent_indent: &str,
    prefix: &str,
    is_last: bool,
    registry: Option<&RegistryClient>,
    visited: &mut HashSet<String>,
    visited_registry: &mut HashSet<String>,
) -> Result<()> {
    let package_map = std::collections::HashMap::new();
    print_package_tree_with_map(
        package,
        parent_indent,
        prefix,
        is_last,
        &package_map,
        registry,
        visited,
        visited_registry,
    )
}

fn print_package_tree_with_map(
    package: &PackageModel,
    parent_indent: &str,
    prefix: &str,
    is_last: bool,
    package_map: &std::collections::HashMap<String, PackageModel>,
    registry: Option<&RegistryClient>,
    visited: &mut HashSet<String>,
    visited_registry: &mut HashSet<String>,
) -> Result<()> {
    // Print package name
    info!(
        "{}{} 📦 Package: {} ({})",
        parent_indent,
        prefix,
        package.name,
        package.root_path.display()
    );

    // Prepare the indent for the package children
    let next_indent = format!("{}{}", parent_indent, if is_last { "    " } else { "│   " });

    let mut all_deps: Vec<ResolvedDep> = Vec::new();
    collect_deps(&package.dependencies, DepKind::Normal, &mut all_deps);
    collect_deps(&package.dev_dependencies, DepKind::Dev, &mut all_deps);
    collect_deps(&package.build_dependencies, DepKind::Build, &mut all_deps);

    for (idx, dep) in all_deps.iter().enumerate() {
        let is_last_dep = idx == all_deps.len() - 1;
        let dep_prefix = if is_last_dep { "└── " } else { "├── " };
        info!(
            "{}{} 📄[{}] {} = {}",
            next_indent,
            dep_prefix,
            dep.kind.label(),
            dep.name,
            dep.model
        );
        let resolved_name = dep
            .model
            .package
            .as_deref()
            .unwrap_or(dep.name.as_str());
        if let Some(child) = package_map.get(resolved_name) {
            let key = format!("workspace:{}", child.name);
            if visited.insert(key) {
                print_package_tree_with_map(
                    child,
                    &next_indent,
                    if is_last_dep { "    " } else { "│   " },
                    true,
                    package_map,
                    registry,
                    visited,
                    visited_registry,
                )?;
            }
        } else if let Some(registry) = registry {
            if let Some((resolved, deps)) = resolve_registry_deps(
                registry,
                resolved_name,
                dep.model.version.as_deref(),
            )?
            {
                let key = format!("registry:{}@{}", resolved.name, resolved.version);
                if visited_registry.insert(key) {
                    print_registry_deps(
                        &next_indent,
                        if is_last_dep { "    " } else { "│   " },
                        &resolved,
                        deps,
                        registry,
                        visited,
                        visited_registry,
                    )?;
                }
            }
        }
    }

    Ok(())
}

fn print_registry_deps(
    parent_indent: &str,
    prefix: &str,
    resolved: &ResolvedCrateSummary,
    deps: RegistryDeps,
    registry: &RegistryClient,
    visited: &mut HashSet<String>,
    visited_registry: &mut HashSet<String>,
) -> Result<()> {
    let indent = format!("{}{}", parent_indent, prefix);
    info!(
        "{} 📦 {}@{} (registry)",
        indent, resolved.name, resolved.version
    );
    let next_indent = format!("{}{}", parent_indent, if prefix.trim().is_empty() { "    " } else { "│   " });
    let mut all_deps = Vec::new();
    collect_deps(&deps.dependencies, DepKind::Normal, &mut all_deps);
    collect_deps(&deps.dev_dependencies, DepKind::Dev, &mut all_deps);
    collect_deps(&deps.build_dependencies, DepKind::Build, &mut all_deps);
    for (idx, dep) in all_deps.iter().enumerate() {
        let is_last_dep = idx == all_deps.len() - 1;
        let dep_prefix = if is_last_dep { "└── " } else { "├── " };
        info!(
            "{}{} 📄[{}] {} = {}",
            next_indent,
            dep_prefix,
            dep.kind.label(),
            dep.name,
            dep.model
        );
        let resolved_name = dep
            .model
            .package
            .as_deref()
            .unwrap_or(dep.name.as_str());
        if let Some((next_resolved, next_deps)) =
            resolve_registry_deps(registry, resolved_name, dep.model.version.as_deref())?
        {
            let key = format!("registry:{}@{}", next_resolved.name, next_resolved.version);
            if visited_registry.insert(key) {
                print_registry_deps(
                    &next_indent,
                    if is_last_dep { "    " } else { "│   " },
                    &next_resolved,
                    next_deps,
                    registry,
                    visited,
                    visited_registry,
                )?;
            }
        }
    }
    Ok(())
}

#[derive(Clone, Copy)]
enum DepKind {
    Normal,
    Dev,
    Build,
}

impl DepKind {
    fn label(self) -> &'static str {
        match self {
            DepKind::Normal => "dep",
            DepKind::Dev => "dev",
            DepKind::Build => "build",
        }
    }
}

struct ResolvedDep {
    name: String,
    model: DependencyModel,
    kind: DepKind,
}

fn collect_deps(
    deps: &std::collections::HashMap<String, DependencyModel>,
    kind: DepKind,
    out: &mut Vec<ResolvedDep>,
) {
    for (name, model) in deps {
        out.push(ResolvedDep {
            name: name.clone(),
            model: model.clone(),
            kind,
        });
    }
}

struct ResolvedCrateSummary {
    name: String,
    version: String,
}

struct RegistryDeps {
    dependencies: std::collections::HashMap<String, DependencyModel>,
    dev_dependencies: std::collections::HashMap<String, DependencyModel>,
    build_dependencies: std::collections::HashMap<String, DependencyModel>,
}

fn resolve_registry_deps(
    registry: &RegistryClient,
    name: &str,
    version_req: Option<&str>,
) -> Result<Option<(ResolvedCrateSummary, RegistryDeps)>> {
    let resolved = match registry.resolve(name, version_req) {
        Ok(resolved) => resolved,
        Err(err) => {
            tracing::warn!("registry resolution for {} failed: {}", name, err);
            return Ok(None);
        }
    };
    let deps = parse_cargo_manifest(&resolved.manifest_path)?;
    Ok(Some((
        ResolvedCrateSummary {
            name: resolved.name,
            version: resolved.version,
        },
        deps,
    )))
}

fn parse_cargo_manifest(path: &Path) -> Result<RegistryDeps> {
    let content = std::fs::read_to_string(path)?;
    let value: toml::Value = toml::from_str(&content)?;
    let dependencies = parse_cargo_deps(value.get("dependencies"));
    let dev_dependencies = parse_cargo_deps(value.get("dev-dependencies"));
    let build_dependencies = parse_cargo_deps(value.get("build-dependencies"));
    Ok(RegistryDeps {
        dependencies,
        dev_dependencies,
        build_dependencies,
    })
}

fn parse_cargo_deps(section: Option<&toml::Value>) -> std::collections::HashMap<String, DependencyModel> {
    let mut out = std::collections::HashMap::new();
    let Some(table) = section.and_then(|v| v.as_table()) else {
        return out;
    };
    for (name, value) in table {
        let mut model = DependencyModel::default();
        match value {
            toml::Value::String(version) => {
                model.version = Some(version.to_string());
            }
            toml::Value::Table(table) => {
                if let Some(version) = table.get("version").and_then(|v| v.as_str()) {
                    model.version = Some(version.to_string());
                }
                if let Some(path) = table.get("path").and_then(|v| v.as_str()) {
                    model.path = Some(Path::new(path).to_path_buf());
                }
                if let Some(git) = table.get("git").and_then(|v| v.as_str()) {
                    model.git = Some(git.to_string());
                }
                if let Some(branch) = table.get("branch").and_then(|v| v.as_str()) {
                    model.branch = Some(branch.to_string());
                }
                if let Some(tag) = table.get("tag").and_then(|v| v.as_str()) {
                    model.tag = Some(tag.to_string());
                }
                if let Some(rev) = table.get("rev").and_then(|v| v.as_str()) {
                    model.rev = Some(rev.to_string());
                }
                if let Some(features) = table.get("features").and_then(|v| v.as_array()) {
                    let list = features
                        .iter()
                        .filter_map(|v| v.as_str())
                        .map(|s| s.to_string())
                        .collect::<Vec<_>>();
                    if !list.is_empty() {
                        model.features = Some(list);
                    }
                }
                if let Some(default_features) = table.get("default-features").and_then(|v| v.as_bool()) {
                    model.default_features = Some(default_features);
                }
                if let Some(optional) = table.get("optional").and_then(|v| v.as_bool()) {
                    model.optional = Some(optional);
                }
                if let Some(package) = table.get("package").and_then(|v| v.as_str()) {
                    model.package = Some(package.to_string());
                }
                if let Some(registry) = table.get("registry").and_then(|v| v.as_str()) {
                    model.registry = Some(registry.to_string());
                }
            }
            _ => {}
        }
        out.insert(name.to_string(), model);
    }
    out
}

fn env_flag_enabled(name: &str) -> bool {
    std::env::var(name)
        .map(|value| matches!(value.as_str(), "1" | "true" | "TRUE" | "yes" | "YES"))
        .unwrap_or(false)
}

fn manifest_root(manifest: &ManifestModel) -> std::path::PathBuf {
    match manifest {
        ManifestModel::Nexus(nexus) => nexus.root_path.clone(),
        ManifestModel::Workspace(workspace) => workspace.root_path.clone(),
        ManifestModel::Package(package) => package.root_path.clone(),
    }
}
