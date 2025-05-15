//! Command implementation for displaying workspace hierarchy as a tree

use crate::manager::NexusManager;
use crate::models::PackageModel;
use eyre::{eyre, Result};
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
    println!("Nexus Root: {}", nexus_manager.nexus_path.display());

    // Print the workspaces
    println!("Workspaces:");
    for (_idx, (name, _)) in nexus_manager.workspaces.iter().enumerate() {
        print_workspace_tree(nexus_manager, name, depth + 1)?;
    }

    Ok(())
}

/// Print the workspace tree
fn print_workspace_tree(
    nexus_manager: &NexusManager,
    workspace_name: &str,
    depth: u32,
) -> Result<()> {
    // Get workspace
    let workspace = nexus_manager
        .get_workspace(workspace_name)
        .ok_or_else(|| eyre!("Workspace '{}' not found", workspace_name))?;

    // Print workspace name
    println!(
        "{}Workspace: {}",
        "  ".repeat(depth as usize),
        workspace.name
    );

    // Print workspace root
    println!(
        "{}Root: {}",
        "  ".repeat(depth as usize + 1),
        workspace.root_path().display()
    );

    // Print crates
    // println!("{}Crates:", "  ".repeat(depth as usize + 1));
    // for (idx, crate_info) in workspace.crates.iter().enumerate() {
    //     let is_last = idx == workspace.crates.len() - 1;
    //     let crate_prefix = if is_last { "└── " } else { "├── " };
    //
    //     println!(
    //         "{}{}{}",
    //         "  ".repeat(depth as usize + 2),
    //         crate_prefix,
    //         crate_info.name
    //     );
    //
    //     // Print crate path
    //     let path_prefix = if is_last { "    " } else { "│   " };
    //     println!(
    //         "{}{}Path: {}",
    //         "  ".repeat(depth as usize + 2),
    //         path_prefix,
    //         crate_info.path.display()
    //     );
    // }
    // Print packages
    println!("{}Packages:", "  ".repeat(depth as usize + 1));
    for package in workspace.get_members()? {
        let package_path = workspace.root_path().join(&package);
        let package = PackageModel::from_root_path(&package_path)?;
        print_package_tree(&package, depth + 2)?;
    }

    Ok(())
}
fn print_package_tree(package: &PackageModel, depth: u32) -> Result<()> {
    // Print package name
    println!("{}Package: {}", "  ".repeat(depth as usize), package.name);

    // dependency
    println!("{}Dependencies:", "  ".repeat(depth as usize + 1));
    for (idx, (crate_, dep)) in package.dependencies.iter().enumerate() {
        let is_last = idx == package.dependencies.len() - 1;
        let dep_prefix = if is_last { "└── " } else { "├── " };

        println!(
            "{}{}{}{:?}",
            "  ".repeat(depth as usize + 2),
            dep_prefix,
            crate_,
            dep
        );
    }
    Ok(())
}
