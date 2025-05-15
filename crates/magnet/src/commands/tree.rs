//! Command implementation for displaying workspace hierarchy as a tree

use crate::manager::NexusManager;
use crate::models::PackageModel;
use eyre::{Result, eyre};
use std::path::Path;

/// Tree command for visualizing workspace structure
pub fn tree(config_path: &Path) -> Result<()> {
    // Create a nexus manager
    let nexus_manager = NexusManager::from_nexus(config_path)?;

    print_nexus_tree(&nexus_manager, 0)?;
    Ok(())
}

fn print_nexus_tree(nexus_manager: &NexusManager, depth: u32) -> Result<()> {
    // Print the root directory
    println!("🧲 Nexus Root: {}", nexus_manager.nexus_path.display());

    // Print the workspaces
    println!("Workspaces:");
    for (idx, (name, _)) in nexus_manager.workspaces.iter().enumerate() {
        let is_last = idx == nexus_manager.workspaces.len() - 1;
        let prefix = if is_last { "└── " } else { "├── " };
        print_workspace_tree(nexus_manager, name, depth + 1, prefix, is_last)?;
    }

    Ok(())
}

/// Print the workspace tree
fn print_workspace_tree(
    nexus_manager: &NexusManager,
    workspace_name: &str,
    depth: u32,
    prefix: &str,
    is_last: bool,
) -> Result<()> {
    // Get workspace
    let workspace = nexus_manager
        .get_workspace(workspace_name)
        .ok_or_else(|| eyre!("Workspace '{}' not found", workspace_name))?;

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

    // Print packages
    println!("{}{}Packages:", "  ".repeat(depth as usize), indent);

    let packages = workspace.list_packages()?;
    for (idx, package_name) in packages.iter().enumerate() {
        let package_path = (&workspace.root_path).join(package_name);
        let package = PackageModel::from_root_path(&package_path)?;
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
    println!("{}{} 📦 Package: {} ({})", parent_indent, prefix, package.name, package.root_path.display());

    // Prepare the indent for the package children
    let next_indent = format!("{}{}", parent_indent, if is_last { "    " } else { "│   " });

    // Dependencies
    if !package.dependencies.is_empty() {
        println!("{}Dependencies:", next_indent);

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
