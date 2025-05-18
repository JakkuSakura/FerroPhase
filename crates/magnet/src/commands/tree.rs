//! Command implementation for displaying workspace hierarchy as a tree

use crate::models::{ManifestModel, NexusModel, PackageModel, WorkspaceModel};
use eyre::Result;
use std::path::Path;

/// Tree command for visualizing workspace structure
pub fn tree(config_path: &Path) -> Result<()> {
    // Create a nexus manager
    let manifest = ManifestModel::from_dir(config_path)?;

    print_manifest_tree(&manifest, 0, "", true)?;

    Ok(())
}

fn print_manifest_tree(
    manifest: &ManifestModel,
    depth: u32,
    prefix: &str,
    is_last: bool,
) -> Result<()> {
    match manifest {
        ManifestModel::Nexus(nexus) => print_nexus_tree(nexus, depth, prefix),
        ManifestModel::Workspace(workspace) => {
            print_workspace_tree(workspace, depth, prefix, is_last)
        }
        ManifestModel::Package(package) => print_package_tree(package, prefix, prefix, is_last),
    }
}
fn print_nexus_tree(nexus: &NexusModel, depth: u32, prefix: &str) -> Result<()> {
    // Print Nexus details
    println!(
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
        print_workspace_tree(workspace, depth + 1, workspace_prefix, is_last_workspace)?;
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
        print_package_tree(package, prefix, package_prefix, is_last_package)?;
    }
    Ok(())
}
/// Print the workspace tree
fn print_workspace_tree(
    workspace: &WorkspaceModel,
    depth: u32,
    prefix: &str,
    is_last: bool,
) -> Result<()> {
    // Print workspace name
    println!(
        "{}{} 🏢 Workspace: {} ({})",
        "  ".repeat(depth as usize),
        prefix,
        workspace.name,
        &workspace.root_path.display()
    );

    // Print workspace root
    let indent = if is_last { "    " } else { "│   " };

    let packages = workspace.list_packages()?;
    for (idx, package) in packages.iter().enumerate() {
        let is_last_package = idx == packages.len() - 1;
        let package_prefix = if is_last_package {
            "└── "
        } else {
            "├── "
        };

        // Use the correct indentation for packages
        let package_indent = format!("{}{}", "  ".repeat(depth as usize), indent);
        print_package_tree(
            &package,
            package_indent.as_str(),
            package_prefix,
            is_last_package,
        )?;
    }

    Ok(())
}

fn print_package_tree(
    package: &PackageModel,
    parent_indent: &str,
    prefix: &str,
    is_last: bool,
) -> Result<()> {
    // Print package name
    println!(
        "{}{} 📦 Package: {} ({})",
        parent_indent,
        prefix,
        package.name,
        package.root_path.display()
    );

    // Prepare the indent for the package children
    let next_indent = format!("{}{}", parent_indent, if is_last { "    " } else { "│   " });

    // Dependencies
    if !package.dependencies.is_empty() {
        for (idx, (crate_, dep)) in package.dependencies.iter().enumerate() {
            let is_last_dep = idx == package.dependencies.len() - 1;
            let dep_prefix = if is_last_dep {
                "└── "
            } else {
                "├── "
            };
            println!("{}{} 📄{} = {}", next_indent, dep_prefix, crate_, dep);
        }
    }

    Ok(())
}
