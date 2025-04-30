//! Command implementation for displaying workspace hierarchy as a tree

use crate::configs::MagnetConfig;
use crate::manager::WorkspaceManager;
use eyre::{Result, eyre};
use std::collections::HashMap;
use std::path::{Path, PathBuf};

/// Display workspace hierarchy as a tree
pub fn tree(config_path: &Path, show_dependencies: bool) -> Result<()> {
    // Resolve the config path
    let config_path = if config_path.is_relative() {
        std::env::current_dir()?.join(config_path)
    } else {
        config_path.to_path_buf()
    };

    // Make sure the config file exists
    if !MagnetConfig::exists_at(&config_path) {
        return Err(eyre!("Magnet.toml not found at {}", config_path.display()));
    }

    // Load the configuration
    let config = MagnetConfig::from_file(&config_path)?;

    // Get the base directory (where Magnet.toml is located)
    let base_dir = config_path.parent().unwrap_or(Path::new(".")).to_path_buf();

    // Create a workspace manager to help us identify relationships
    let mut workspace_manager = WorkspaceManager::new(config.clone(), &base_dir)?;
    workspace_manager.discover_related_workspaces()?;

    // First, collect all Magnet.toml files in the directory tree
    let magnet_files = MagnetConfig::find_all_in_directory(&base_dir)?;

    // Map out workspace relationships
    let workspace_map = build_workspace_relationship_map(&magnet_files, &workspace_manager)?;

    // Get nexus information for the root node display
    let nexus_name = if let Some(project_name) = config.get_name() {
        project_name
    } else {
        base_dir
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("nexus")
            .to_string()
    };

    // Print the nexus root directly - no header line
    println!("🧲 nexus: {} (Magnet.toml)", nexus_name);

    // Get configured member directories
    let configured_members = config.get_configured_members(&base_dir)?;
    let mut subdirs = if !configured_members.is_empty() {
        configured_members
    } else {
        // Fall back to scanning for directories with project files
        std::fs::read_dir(&base_dir)?
            .filter_map(|e| e.ok().map(|e| e.path()))
            .filter(|p| {
                p.is_dir()
                    && (MagnetConfig::has_cargo_toml(p)
                        || MagnetConfig::has_magnet_toml(p)
                        || MagnetConfig::has_project_files(p).unwrap_or(false))
            })
            .collect()
    };

    // Sort directories
    subdirs.sort();

    // Print each child with the appropriate prefix
    for (idx, subdir) in subdirs.iter().enumerate() {
        let is_last = idx == subdirs.len() - 1;

        // Process each directory according to its configuration
        print_unified_tree(
            &base_dir,
            subdir,
            "",
            is_last,
            &workspace_map,
            &workspace_manager,
            show_dependencies,
        )?;
    }

    Ok(())
}

/// Build a map of workspace relationships
fn build_workspace_relationship_map(
    magnet_files: &[PathBuf],
    _workspace_manager: &WorkspaceManager,
) -> Result<HashMap<PathBuf, Vec<PathBuf>>> {
    let mut result = HashMap::new();

    // For each Magnet.toml file, load it and find relationships
    for magnet_file in magnet_files {
        let workspace_dir = magnet_file.parent().unwrap_or_else(|| Path::new("."));
        let config = MagnetConfig::from_file(magnet_file)?;

        // Look for related workspaces in search_paths
        if let Some(search_paths) = &config.workspace.search_paths {
            for related_path in search_paths.values() {
                let absolute_path = if related_path.is_absolute() {
                    related_path.clone()
                } else {
                    // Resolve relative path
                    workspace_dir.join(related_path)
                };

                // Normalize path
                let absolute_path = absolute_path
                    .canonicalize()
                    .unwrap_or_else(|_| absolute_path);

                // Check if this path contains a Magnet.toml
                if MagnetConfig::has_magnet_toml(&absolute_path) {
                    // Add to relationship map
                    result
                        .entry(workspace_dir.to_path_buf())
                        .or_insert_with(Vec::new)
                        .push(absolute_path);
                }
            }
        }

        // For nexus configuration, also check nexus.search_paths
        if let Some(nexus) = &config.nexus {
            if let Some(search_paths) = &nexus.search_paths {
                for related_path in search_paths.values() {
                    let absolute_path = if related_path.is_absolute() {
                        related_path.clone()
                    } else {
                        // Resolve relative path
                        workspace_dir.join(related_path)
                    };

                    // Normalize path
                    let absolute_path = absolute_path
                        .canonicalize()
                        .unwrap_or_else(|_| absolute_path);

                    // Check if this path contains a Magnet.toml
                    if MagnetConfig::has_magnet_toml(&absolute_path) {
                        // Add to relationship map
                        result
                            .entry(workspace_dir.to_path_buf())
                            .or_insert_with(Vec::new)
                            .push(absolute_path);
                    }
                }
            }
        }
    }

    Ok(result)
}

/// Print a unified tree of workspaces and crates
fn print_unified_tree(
    root_dir: &Path,
    current_dir: &Path,
    prefix: &str,
    is_last: bool,
    workspace_map: &HashMap<PathBuf, Vec<PathBuf>>,
    workspace_manager: &WorkspaceManager,
    show_dependencies: bool,
) -> Result<()> {
    // Get the directory name
    let dir_name = current_dir
        .file_name()
        .unwrap_or_else(|| std::ffi::OsStr::new(current_dir.to_str().unwrap_or("[invalid-path]")))
        .to_string_lossy();

    // Skip hidden directories
    if dir_name.starts_with('.') && dir_name != "." {
        return Ok(());
    }

    // Get file paths
    let cargo_toml_path = current_dir.join("Cargo.toml");
    let magnet_toml_path = current_dir.join("Magnet.toml");

    // Check what files exist
    let has_cargo_toml = MagnetConfig::has_cargo_toml(current_dir);
    let has_magnet_toml = MagnetConfig::has_magnet_toml(current_dir);

    // Get the display path
    let display_path = if current_dir == root_dir {
        ".".to_string()
    } else {
        pathdiff::diff_paths(current_dir, root_dir)
            .unwrap_or_else(|| current_dir.to_path_buf())
            .to_string_lossy()
            .to_string()
    };

    // Print the current node with its prefix
    let current_prefix = if current_dir == root_dir {
        "└── ".to_string()
    } else if is_last {
        format!("{}└── ", prefix)
    } else {
        format!("{}├── ", prefix)
    };

    // Get node name using the MagnetConfig helper
    let node_name = if has_magnet_toml {
        if let Ok(config) = MagnetConfig::from_file(&magnet_toml_path) {
            let config_type = if config.nexus.is_some() {
                ("🧲", "nexus") // Nexus uses magnet icon
            } else if !config.workspace.members.is_empty()
                || config.workspace.search_paths.is_some()
            {
                ("📦", "workspace") // Normal workspace uses package icon
            } else {
                ("📄", "package") // Package uses document icon
            };

            let display_name = config.get_node_display_name(current_dir);

            // For root directory, use different naming
            if current_dir == root_dir {
                let file_name = magnet_toml_path
                    .file_name()
                    .unwrap_or_default()
                    .to_string_lossy();

                (
                    config_type.0,
                    format!("{}: {} ({})", config_type.1, display_name, file_name),
                )
            } else {
                (
                    config_type.0,
                    format!("{} ({})", display_name, config_type.1),
                )
            }
        } else {
            // Fallback if we can't read the config
            ("🧲", dir_name.to_string())
        }
    } else if has_cargo_toml {
        // Only Cargo.toml exists - it's a crate
        if let Ok(content) = std::fs::read_to_string(&cargo_toml_path) {
            if let Ok(cargo_toml) = toml::from_str::<toml::Value>(&content) {
                if let Some(package) = cargo_toml.get("package") {
                    if let Some(name) = package.get("name") {
                        if let Some(name_str) = name.as_str() {
                            ("", name_str.to_string())
                        } else {
                            ("", dir_name.to_string())
                        }
                    } else {
                        ("", dir_name.to_string())
                    }
                } else {
                    ("", dir_name.to_string())
                }
            } else {
                ("", dir_name.to_string())
            }
        } else {
            ("", dir_name.to_string())
        }
    } else {
        // Regular directory
        ("", display_path)
    };

    println!("{}{} {}", current_prefix, node_name.0, node_name.1);

    // Get subdirectories based on configuration
    let mut subdirs = if has_magnet_toml {
        // If this directory has a Magnet.toml, read its configuration to determine children
        if let Ok(config) = MagnetConfig::from_file(&magnet_toml_path) {
            let members = config.get_configured_members(current_dir)?;
            if !members.is_empty() {
                members
            } else {
                // Fall back to scanning all directories if no members are configured
                std::fs::read_dir(current_dir)?
                    .filter_map(|e| e.ok().map(|e| e.path()))
                    .filter(|p| p.is_dir())
                    .collect()
            }
        } else {
            // Fallback if we can't read the config
            std::fs::read_dir(current_dir)?
                .filter_map(|e| e.ok().map(|e| e.path()))
                .filter(|p| p.is_dir())
                .collect()
        }
    } else {
        // If no Magnet.toml exists, scan all directories
        std::fs::read_dir(current_dir)?
            .filter_map(|e| e.ok().map(|e| e.path()))
            .filter(|p| p.is_dir())
            .collect()
    };

    // Sort directories
    subdirs.sort();

    // Calculate the new prefix for children
    let child_prefix = if current_dir == root_dir {
        "".to_string()
    } else if is_last {
        format!("{}    ", prefix)
    } else {
        format!("{}│   ", prefix)
    };

    // Recursively process subdirectories
    for (idx, subdir) in subdirs.iter().enumerate() {
        let is_last_child = idx == subdirs.len() - 1;

        // Check if this subdirectory should be included
        let should_show = if has_magnet_toml {
            // If parent has Magnet.toml, we already filtered based on configuration
            true
        } else {
            // Otherwise, check if it has project files
            MagnetConfig::has_cargo_toml(subdir)
                || MagnetConfig::has_magnet_toml(subdir)
                || MagnetConfig::has_project_files(subdir)?
        };

        if should_show {
            print_unified_tree(
                root_dir,
                subdir,
                &child_prefix,
                is_last_child,
                workspace_map,
                workspace_manager,
                show_dependencies,
            )?;
        }
    }

    // Show dependencies if requested
    if show_dependencies && has_cargo_toml {
        print_workspace_dependencies(&cargo_toml_path, workspace_manager, &child_prefix)?;
    }

    Ok(())
}

/// Print the dependencies of a workspace crate
fn print_workspace_dependencies(
    cargo_toml_path: &Path,
    workspace_manager: &WorkspaceManager,
    prefix: &str,
) -> Result<()> {
    // Get dependencies using our utility function
    let deps = match MagnetConfig::get_cargo_dependencies(cargo_toml_path) {
        Ok(deps) if !deps.is_empty() => deps,
        _ => return Ok(()),
    };

    // Print each dependency that is within our workspaces
    let mut workspace_deps = Vec::new();

    for dep_name in deps {
        // Skip standard library crates and external dependencies
        if let Some(dep_crate) = workspace_manager.find_crate(&dep_name) {
            workspace_deps.push((dep_name, dep_crate));
        }
    }

    if workspace_deps.is_empty() {
        return Ok(());
    }

    // Print dependencies
    println!("{}Dependencies:", prefix);

    for (idx, (dep_name, dep_crate)) in workspace_deps.iter().enumerate() {
        let is_last = idx == workspace_deps.len() - 1;
        let dep_prefix = if is_last {
            format!("{}    └── ", prefix)
        } else {
            format!("{}    ├── ", prefix)
        };

        // Get the crate path
        let rel_path = pathdiff::diff_paths(
            dep_crate.path.as_path(),
            cargo_toml_path.parent().unwrap_or(Path::new(".")),
        )
        .unwrap_or_else(|| dep_crate.path.clone());

        println!("{}↳ {} ({})", dep_prefix, dep_name, rel_path.display());
    }

    Ok(())
}
