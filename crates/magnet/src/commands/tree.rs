//! Command implementation for displaying workspace hierarchy as a tree

use crate::configs::MagnetConfig;
use crate::workspace_manager::{CrateInfo, WorkspaceManager};
use anyhow::{Result, anyhow};
use glob;
use std::collections::HashMap;
use std::path::{Path, PathBuf};

// Create additional helper functions to parse configs and process workspaces

/// Get configured member paths from a Magnet.toml or Cargo.toml configuration
fn get_configured_members(config_path: &Path, base_dir: &Path) -> Result<Vec<PathBuf>> {
    if !config_path.exists() {
        return Ok(Vec::new());
    }

    // Load the configuration
    let config = MagnetConfig::from_file(config_path)?;
    let mut result = Vec::new();

    // For nexus configuration, check nexus.search_paths
    if config.nexus.is_some() {
        if let Some(search_paths) = &config.nexus.as_ref().unwrap().search_paths {
            for path in search_paths.values() {
                let abs_path = if path.is_absolute() {
                    path.clone()
                } else {
                    base_dir.join(path)
                };

                if abs_path.exists() {
                    result.push(abs_path);
                }
            }
        }
    }

    // For workspace configuration, check workspace.members
    if !config.workspace.members.is_empty() {
        for pattern in &config.workspace.members {
            // Process the glob pattern to find actual directory paths
            match expand_workspace_member_pattern(base_dir, pattern) {
                Ok(paths) => result.extend(paths),
                Err(_) => {
                    // If glob fails, try a direct path
                    let dir_path = base_dir.join(pattern);
                    if dir_path.exists() && dir_path.is_dir() {
                        result.push(dir_path);
                    }
                }
            }
        }
    }

    // Return all discovered paths
    Ok(result)
}

/// Expand a workspace member pattern into a list of matching directories
fn expand_workspace_member_pattern(base_dir: &Path, pattern: &str) -> Result<Vec<PathBuf>> {
    let mut result = Vec::new();
    let full_pattern = format!("{}/{}", base_dir.display(), pattern);

    for entry in glob::glob(&full_pattern)? {
        if let Ok(path) = entry {
            if path.is_dir() {
                result.push(path);
            } else if let Some(parent) = path.parent() {
                // For patterns that match files (like */Cargo.toml), use the parent directory
                if path
                    .file_name()
                    .map_or(false, |name| name == "Cargo.toml" || name == "Magnet.toml")
                {
                    result.push(parent.to_path_buf());
                }
            }
        }
    }

    Ok(result)
}

/// Display workspace hierarchy as a tree
pub fn tree(config_path: &Path, show_dependencies: bool) -> Result<()> {
    // Resolve the config path
    let config_path = if config_path.is_relative() {
        std::env::current_dir()?.join(config_path)
    } else {
        config_path.to_path_buf()
    };

    // Make sure the config file exists
    if !config_path.exists() {
        return Err(anyhow!(
            "Magnet.toml not found at {}",
            config_path.display()
        ));
    }

    // Load the configuration
    let config = MagnetConfig::from_file(&config_path)?;

    // Get the base directory (where Magnet.toml is located)
    let base_dir = config_path.parent().unwrap_or(Path::new(".")).to_path_buf();

    // Create a workspace manager to help us identify relationships
    let mut workspace_manager = WorkspaceManager::new(config.clone(), base_dir.clone())?;
    workspace_manager.discover_related_workspaces()?;

    // First, collect all Magnet.toml files in the directory tree
    let mut magnet_files = Vec::new();
    find_all_magnet_files(&base_dir, &mut magnet_files)?;

    // Map out workspace relationships
    let workspace_map = build_workspace_relationship_map(&magnet_files, &workspace_manager)?;

    // Get nexus information for the root node display
    let nexus_name = if let Some(project_name) = config.project.name {
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
    let configured_members = get_configured_members(&config_path, &base_dir)?;
    let mut subdirs = if !configured_members.is_empty() {
        configured_members
    } else {
        // Fall back to scanning for directories with project files
        std::fs::read_dir(&base_dir)?
            .filter_map(|e| e.ok().map(|e| e.path()))
            .filter(|p| {
                p.is_dir()
                    && (p.join("Cargo.toml").exists()
                        || p.join("Magnet.toml").exists()
                        || has_nested_project_files(p).unwrap_or(false))
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

/// Find all Magnet.toml files in a directory tree
fn find_all_magnet_files(dir: &Path, result: &mut Vec<PathBuf>) -> Result<()> {
    // Check for Magnet.toml in current directory
    let magnet_path = dir.join("Magnet.toml");
    if magnet_path.exists() {
        result.push(magnet_path);
    }

    // Scan subdirectories
    if dir.is_dir() {
        for entry in std::fs::read_dir(dir)? {
            let entry = entry?;
            let path = entry.path();

            // Skip hidden directories
            if path.is_dir() {
                let file_name = path.file_name().and_then(|n| n.to_str()).unwrap_or("");

                if !file_name.starts_with('.') {
                    find_all_magnet_files(&path, result)?;
                }
            }
        }
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
                if absolute_path.join("Magnet.toml").exists() {
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
                    if absolute_path.join("Magnet.toml").exists() {
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
    let has_cargo_toml = cargo_toml_path.exists();
    let has_magnet_toml = magnet_toml_path.exists();

    // Get the display path
    let display_path = if current_dir == root_dir {
        ".".to_string()
    } else {
        pathdiff::diff_paths(current_dir, root_dir)
            .unwrap_or_else(|| current_dir.to_path_buf())
            .to_string_lossy()
            .to_string()
    };

    // Determine node type and label
    let (node_type, node_name) = if has_magnet_toml {
        // Determine the type based on the Magnet.toml content
        if let Ok(config) = MagnetConfig::from_file(&magnet_toml_path) {
            let (config_type, config_label) = if config.nexus.is_some() {
                ("🧲", "nexus") // Nexus uses magnet icon
            } else if !config.workspace.members.is_empty()
                || config.workspace.search_paths.is_some()
            {
                ("📦", "workspace") // Normal workspace uses package icon
            } else {
                ("📄", "package") // Package uses document icon
            };

            // For root directory, use different naming
            if current_dir == root_dir {
                let file_name = magnet_toml_path
                    .file_name()
                    .unwrap_or_default()
                    .to_string_lossy();

                // Get name for nexus - use project name if available, otherwise use parent directory name
                let nexus_name = if let Some(project_name) = config.project.name {
                    project_name
                } else {
                    // Use parent directory name if project name is missing
                    current_dir
                        .file_name()
                        .and_then(|n| n.to_str())
                        .unwrap_or("unknown")
                        .to_string()
                };

                (
                    config_type,
                    format!("{}: {} ({})", config_label, nexus_name, file_name),
                )
            } else {
                let dir_name = dir_name.to_string();
                (
                    config_type,
                    format!(
                        "{} ({})",
                        config.project.name.unwrap_or_else(|| dir_name.clone()),
                        config_label
                    ),
                )
            }
        } else {
            (
                "🧲",
                get_node_name(
                    current_dir,
                    Some(&magnet_toml_path),
                    cargo_path_opt(&cargo_toml_path),
                )?,
            )
        }
    } else if has_cargo_toml {
        // Only Cargo.toml exists - it's a crate
        (
            "",
            get_node_name(current_dir, None, Some(&cargo_toml_path))?,
        )
    } else {
        // Regular directory
        ("", display_path)
    };

    // Print the current node
    let current_prefix = if current_dir == root_dir {
        "└── ".to_string()
    } else if is_last {
        format!("{}└── ", prefix)
    } else {
        format!("{}├── ", prefix)
    };

    println!("{}{} {}", current_prefix, node_type, node_name);

    // Get subdirectories based on configuration
    let mut subdirs = if has_magnet_toml {
        // If this directory has a Magnet.toml, read its configuration to determine children
        let members = get_configured_members(&magnet_toml_path, current_dir)?;
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
            subdir.join("Cargo.toml").exists()
                || subdir.join("Magnet.toml").exists()
                || has_nested_project_files(subdir)?
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

/// Check if a directory contains nested Cargo.toml or Magnet.toml files
fn has_nested_project_files(dir: &Path) -> Result<bool> {
    if !dir.is_dir() {
        return Ok(false);
    }

    // Directly check common project directories first for performance
    for common_dir in &["src", "crates", "packages"] {
        let test_path = dir.join(common_dir);
        if test_path.is_dir() {
            return Ok(true);
        }
    }

    // Scan the directory (but not recursively) for Cargo.toml or Magnet.toml
    for entry in std::fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();

        if path.is_dir() {
            // Skip hidden directories
            if let Some(name) = path.file_name() {
                if let Some(name_str) = name.to_str() {
                    if name_str.starts_with('.') {
                        continue;
                    }
                }
            }

            // Check if this subdirectory has project files
            if path.join("Cargo.toml").exists() || path.join("Magnet.toml").exists() {
                return Ok(true);
            }
        }
    }

    Ok(false)
}

/// Helper function to check if a cargo_toml exists and return it as an Option
fn cargo_path_opt(cargo_toml_path: &Path) -> Option<&Path> {
    if cargo_toml_path.exists() {
        Some(cargo_toml_path)
    } else {
        None
    }
}

/// Get the name for a node in the tree (from Cargo.toml, Magnet.toml, or directory name)
fn get_node_name(
    dir: &Path,
    magnet_path: Option<&Path>,
    cargo_path: Option<&Path>,
) -> Result<String> {
    // Try to get name from Magnet.toml first
    if let Some(magnet_path) = magnet_path {
        if magnet_path.exists() {
            if let Ok(config) = MagnetConfig::from_file(magnet_path) {
                // For nexus and workspace, use the project name
                if let Some(project_name) = config.project.name {
                    let dir_name = dir
                        .file_name()
                        .and_then(|n| n.to_str())
                        .unwrap_or_else(|| dir.to_str().unwrap_or("[invalid-path]"));

                    return Ok(format!("{} ({})", project_name, dir_name));
                }
            }
        }
    }

    // Try to get name from Cargo.toml if Magnet.toml didn't have what we need
    if let Some(cargo_path) = cargo_path {
        if cargo_path.exists() {
            if let Ok(content) = std::fs::read_to_string(cargo_path) {
                if let Ok(cargo_toml) = toml::from_str::<toml::Value>(&content) {
                    if let Some(package) = cargo_toml.get("package") {
                        if let Some(name) = package.get("name") {
                            if let Some(name_str) = name.as_str() {
                                return Ok(name_str.to_string());
                            }
                        }
                    }
                }
            }
        }
    }

    // Fall back to directory name
    let display_path = dir
        .file_name()
        .and_then(|n| n.to_str())
        .unwrap_or_else(|| dir.to_str().unwrap_or("[invalid-path]"));

    if dir.to_string_lossy() == "." {
        return Ok("./".to_string());
    }

    Ok(display_path.to_string())
}

/// Get crate information from a Cargo.toml file
fn get_crate_info(cargo_toml_path: &Path) -> Result<Option<CrateInfo>> {
    // Read the Cargo.toml file
    let content = std::fs::read_to_string(cargo_toml_path)?;
    let cargo_toml: toml::Value = toml::from_str(&content)?;

    // Extract the crate name and version
    let name = cargo_toml
        .get("package")
        .and_then(|p| p.get("name"))
        .and_then(|n| n.as_str())
        .map(|s| s.to_string());

    let version = cargo_toml
        .get("package")
        .and_then(|p| p.get("version"))
        .and_then(|v| v.as_str())
        .map(|s| s.to_string());

    if let Some(name) = name {
        let crate_path = cargo_toml_path
            .parent()
            .unwrap_or(Path::new("."))
            .to_path_buf();

        let crate_info = CrateInfo {
            name: name.clone(),
            version,
            path: crate_path,
            cargo_toml_path: cargo_toml_path.to_path_buf(),
            magnet_toml_path: None,
            has_custom_config: false,
        };

        return Ok(Some(crate_info));
    }

    Ok(None)
}

/// Print the dependencies of a workspace crate
fn print_workspace_dependencies(
    cargo_toml_path: &Path,
    workspace_manager: &WorkspaceManager,
    prefix: &str,
) -> Result<()> {
    // Read the Cargo.toml file
    let content = match std::fs::read_to_string(cargo_toml_path) {
        Ok(content) => content,
        Err(_) => return Ok(()),
    };

    let cargo_toml: toml::Value = match toml::from_str(&content) {
        Ok(toml) => toml,
        Err(_) => return Ok(()),
    };

    // Extract the crate name
    let crate_name = cargo_toml
        .get("package")
        .and_then(|p| p.get("name"))
        .and_then(|n| n.as_str())
        .map(|s| s.to_string());

    if crate_name.is_none() {
        return Ok(());
    }

    // Extract dependencies
    let deps = cargo_toml
        .get("dependencies")
        .and_then(|d| d.as_table())
        .map(|t| t.keys().cloned().collect::<Vec<String>>())
        .unwrap_or_default();

    if deps.is_empty() {
        return Ok(());
    }

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
