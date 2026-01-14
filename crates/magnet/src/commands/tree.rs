//! Command implementation for displaying workspace hierarchy as a tree

use crate::models::{DependencyModel, ManifestModel, NexusModel, PackageModel, WorkspaceModel};
use crate::registry::RegistryOptions;
use crate::models::LockIndex;
use crate::resolver::lock::match_locked_registry;
use crate::resolver::project::{load_lock_index, load_manifest};
use crate::resolver::{RegistryLoader, ResolvedRegistry};
use eyre::Result;
use semver::VersionReq;
use std::collections::HashSet;
use std::path::Path;
use tracing::info;

/// Tree command for visualizing workspace structure
pub fn tree(config_path: &Path) -> Result<()> {
    // Create a nexus manager
    let manifest = load_manifest(config_path)?;

    let offline = env_flag_enabled("MAGNET_OFFLINE");
    let registry = RegistryLoader::new(RegistryOptions {
        offline,
        cache_dir: None,
    })?;
    let lock_index = load_lock_index(&manifest_root(&manifest))?;
    let mut visited = HashSet::new();
    let mut visited_registry = HashSet::new();
    print_manifest_tree(
        &manifest,
        0,
        "",
        true,
        &registry,
        lock_index.as_ref(),
        &mut visited,
        &mut visited_registry,
    )?;

    Ok(())
}

fn print_manifest_tree(
    manifest: &ManifestModel,
    depth: u32,
    prefix: &str,
    is_last: bool,
    registry: &RegistryLoader,
    lock_index: Option<&LockIndex>,
    visited: &mut HashSet<String>,
    visited_registry: &mut HashSet<String>,
) -> Result<()> {
    match manifest {
        ManifestModel::Nexus(nexus) => {
            print_nexus_tree(nexus, depth, prefix, registry, lock_index, visited, visited_registry)
        }
        ManifestModel::Workspace(workspace) => {
            print_workspace_tree(
                workspace,
                depth,
                prefix,
                is_last,
                registry,
                lock_index,
                visited,
                visited_registry,
            )
        }
        ManifestModel::Package(package) => {
            print_package_tree(
                package,
                prefix,
                prefix,
                is_last,
                registry,
                lock_index,
                visited,
                visited_registry,
            )
        }
    }
}
fn print_nexus_tree(
    nexus: &NexusModel,
    depth: u32,
    prefix: &str,
    registry: &RegistryLoader,
    lock_index: Option<&LockIndex>,
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
            lock_index,
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
            lock_index,
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
    registry: &RegistryLoader,
    lock_index: Option<&LockIndex>,
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
            lock_index,
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
    registry: &RegistryLoader,
    lock_index: Option<&LockIndex>,
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
        lock_index,
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
    registry: &RegistryLoader,
    lock_index: Option<&LockIndex>,
    visited: &mut HashSet<String>,
    visited_registry: &mut HashSet<String>,
) -> Result<()> {
    // Print package name
    info!(
        "{}{} 📦  Package: {} ({})",
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
            "{}{} 📄 [{}] {} = {}",
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
                    lock_index,
                    visited,
                    visited_registry,
                )?;
            }
        } else if let Some(resolved) =
            resolve_registry_deps(registry, lock_index, resolved_name, dep.model.version.as_deref())?
        {
            let key = format!(
                "registry:{}@{}",
                resolved.resolved.name, resolved.resolved.version
            );
            if visited_registry.insert(key) {
                print_registry_deps(
                    &next_indent,
                    if is_last_dep { "    " } else { "│   " },
                    &resolved,
                    registry,
                    lock_index,
                    visited,
                    visited_registry,
                )?;
            }
        }
    }

    Ok(())
}

fn print_registry_deps(
    parent_indent: &str,
    prefix: &str,
    resolved: &ResolvedRegistry,
    registry: &RegistryLoader,
    lock_index: Option<&LockIndex>,
    visited: &mut HashSet<String>,
    visited_registry: &mut HashSet<String>,
) -> Result<()> {
    let indent = format!("{}{}", parent_indent, prefix);
    info!(
        "{} 📦  {}@{} (registry)",
        indent, resolved.resolved.name, resolved.resolved.version
    );
    let next_indent = format!("{}{}", parent_indent, if prefix.trim().is_empty() { "    " } else { "│   " });
    let mut all_deps = Vec::new();
    collect_deps(&resolved.deps.dependencies, DepKind::Normal, &mut all_deps);
    collect_deps(&resolved.deps.dev_dependencies, DepKind::Dev, &mut all_deps);
    collect_deps(&resolved.deps.build_dependencies, DepKind::Build, &mut all_deps);
    for (idx, dep) in all_deps.iter().enumerate() {
        let is_last_dep = idx == all_deps.len() - 1;
        let dep_prefix = if is_last_dep { "└── " } else { "├── " };
        info!(
            "{}{} 📄 [{}] {} = {}",
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
        if let Some(next_resolved) =
            resolve_registry_deps(registry, lock_index, resolved_name, dep.model.version.as_deref())?
        {
            let key = format!(
                "registry:{}@{}",
                next_resolved.resolved.name, next_resolved.resolved.version
            );
            if visited_registry.insert(key) {
                print_registry_deps(
                    &next_indent,
                    if is_last_dep { "    " } else { "│   " },
                    &next_resolved,
                    registry,
                    lock_index,
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

fn resolve_registry_deps(
    registry: &RegistryLoader,
    lock_index: Option<&LockIndex>,
    name: &str,
    version_req: Option<&str>,
) -> Result<Option<ResolvedRegistry>> {
    if version_req.is_none() {
        tracing::warn!("registry resolution for {} skipped: missing version", name);
        return Ok(None);
    }
    let requested = version_req.unwrap_or_default();
    let mut locked_version = None;
    if let Ok(req) = VersionReq::parse(requested) {
        if let Some(locked) = match_locked_registry(lock_index, name, &req) {
            locked_version = Some(locked.version);
        }
    }
    let resolved = match registry.resolve_with_deps(name, locked_version.as_deref().or(version_req)) {
        Ok(resolved) => resolved,
        Err(err) => {
            tracing::warn!("registry resolution for {} failed: {}", name, err);
            return Ok(None);
        }
    };
    Ok(Some(resolved))
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
