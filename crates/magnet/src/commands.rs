//! Command implementations for the magnet CLI

use crate::config::{MagnetConfig, MagnetConfigType};
use crate::workspace_manager::{CrateInfo, WorkspaceManager};
use anyhow::{Context, Result};
use std::path::{Path, PathBuf};

/// Initialize a new Magnet.toml file at the specified path
pub fn init(path: &Path) -> Result<()> {
    // Create the directory if it doesn't exist
    if !path.exists() {
        std::fs::create_dir_all(path)
            .context(format!("Failed to create directory at {}", path.display()))?;
    }

    // Detect existing workspace structure
    let target_path = if path.is_dir() {
        path.join("Magnet.toml")
    } else {
        path.to_path_buf()
    };

    // Auto-detect the configuration type based on directory structure
    let config_type = detect_config_type_from_path(path);

    // Create an appropriate minimal configuration
    let config = create_minimal_config(path, config_type)?;

    // Write the configuration to file
    config.save_to_file(&target_path).context(format!(
        "Failed to write Magnet.toml to {}",
        target_path.display()
    ))?;

    println!(
        "Created new {} Magnet.toml at {}",
        match config_type {
            crate::config::MagnetConfigType::Nexus => "nexus",
            crate::config::MagnetConfigType::Workspace => "workspace",
            crate::config::MagnetConfigType::Package => "package",
        },
        target_path.display()
    );

    Ok(())
}

/// Detect the configuration type based on the path
fn detect_config_type_from_path(path: &Path) -> crate::config::MagnetConfigType {
    use crate::config::MagnetConfigType;

    // Get normalized path for analysis
    let dir_path = if path.is_dir() {
        path.to_path_buf()
    } else {
        path.parent().unwrap_or(Path::new(".")).to_path_buf()
    };

    // Extract directory components
    let components: Vec<&str> = dir_path
        .components()
        .filter_map(|c| {
            if let std::path::Component::Normal(name) = c {
                name.to_str()
            } else {
                None
            }
        })
        .collect();

    // Check for src directory (indicates package)
    let has_src_dir = dir_path.join("src").is_dir();

    // Check for crates directory (indicates workspace or nexus)
    let has_crates_dir = dir_path.join("crates").is_dir();

    // Check if in a crates directory (indicates package)
    let is_in_crates_dir =
        components.contains(&"crates") && components.last().map_or(false, |last| *last != "crates");

    // Check if in nexus directory (indicates nexus)
    let is_in_nexus_dir = components.contains(&"nexus");

    // Logic to determine configuration type
    if is_in_nexus_dir || (has_crates_dir && !is_in_crates_dir) {
        MagnetConfigType::Nexus
    } else if has_crates_dir || (!is_in_crates_dir && !has_src_dir) {
        MagnetConfigType::Workspace
    } else {
        MagnetConfigType::Package
    }
}

/// Create a minimal configuration of the specified type
fn create_minimal_config(
    _path: &Path,
    config_type: crate::config::MagnetConfigType,
) -> Result<crate::config::MagnetConfig> {
    use crate::config::MagnetConfig;

    // Create a minimal config with the specified type
    let config = MagnetConfig::new_with_type(config_type);

    Ok(config)
}

/// Generate Cargo.toml files from Magnet.toml configuration
pub fn generate(config_path: &Path) -> Result<()> {
    // Resolve the config path
    let config_path = if config_path.is_relative() {
        std::env::current_dir()?.join(config_path)
    } else {
        config_path.to_path_buf()
    };

    // Make sure the config file exists
    if !config_path.exists() {
        return Err(anyhow::anyhow!(
            "Magnet.toml not found at {}",
            config_path.display()
        ));
    }

    // Load the configuration
    let config = MagnetConfig::from_file(&config_path)?;

    // Get the base directory (where Magnet.toml is located)
    let base_dir = config_path.parent().unwrap_or(Path::new(".")).to_path_buf();

    // Create a workspace manager
    let mut workspace_manager = WorkspaceManager::new(config.clone(), base_dir.clone())?;

    // Discover related workspaces
    workspace_manager.discover_related_workspaces()?;

    // Create a dependency resolver
    let resolver = crate::resolver::DependencyResolver::new(
        workspace_manager.primary_workspace.clone(),
        workspace_manager
            .related_workspaces
            .values()
            .cloned()
            .collect(),
    );

    // Create a generator
    let mut generator = crate::generator::CargoGenerator::new(workspace_manager, resolver);

    // Generate all Cargo.toml files
    generator.generate_all()?;

    println!("Cargo.toml files updated successfully");

    Ok(())
}

/// Check Magnet.toml for issues
pub fn check(config_path: &Path) -> Result<()> {
    // Resolve the config path
    let config_path = if config_path.is_relative() {
        std::env::current_dir()?.join(config_path)
    } else {
        config_path.to_path_buf()
    };

    // Make sure the config file exists
    if !config_path.exists() {
        return Err(anyhow::anyhow!(
            "Magnet.toml not found at {}",
            config_path.display()
        ));
    }

    // Load the configuration
    let config = MagnetConfig::from_file(&config_path)?;

    // Get the base directory (where Magnet.toml is located)
    let base_dir = config_path.parent().unwrap_or(Path::new(".")).to_path_buf();

    // Create a workspace manager
    let workspace_manager = WorkspaceManager::new(config, base_dir)?;

    // Count the crates found
    let crates = workspace_manager.get_all_crates();

    if crates.is_empty() {
        println!(
            "Warning: No workspace members found. Check your 'workspace.members' configuration."
        );

        // Show more detailed information to help with debugging
        println!("\nDetailed information for troubleshooting:");
        println!(
            "  Workspace root: {}",
            workspace_manager.get_root_path().display()
        );
        println!("  Patterns searched:");
        for pattern in &workspace_manager.primary_workspace.config.workspace.members {
            let full_pattern = format!(
                "{}/{}/Cargo.toml",
                workspace_manager.get_root_path().display(),
                pattern
            );
            println!("    - {}", full_pattern);
        }

        if workspace_manager
            .primary_workspace
            .config
            .workspace
            .members
            .is_empty()
        {
            println!(
                "\nYour 'workspace.members' array is empty. You need to add glob patterns to find your crates."
            );
            println!(
                "Example: Add `\"crates/*\"` to workspace.members in Magnet.toml if your crates are in a 'crates' directory."
            );
        } else {
            println!("\nPossible reasons for not finding any workspace members:");
            println!(
                "  1. The glob patterns in 'workspace.members' don't match any directories containing Cargo.toml files"
            );
            println!("  2. The crates directory doesn't exist or is in a different location");
            println!("  3. Your crates don't have Cargo.toml files");
            println!("\nTry running this command to see all directories in the workspace root:");
            println!("  ls -la {}", workspace_manager.get_root_path().display());
        }
    } else {
        println!("Found {} workspace members", crates.len());
    }

    // Check if there's a mismatch between Magnet.toml and Cargo.toml
    let cargo_toml_path = workspace_manager.get_root_path().join("Cargo.toml");

    if cargo_toml_path.exists() {
        let cargo_content = std::fs::read_to_string(&cargo_toml_path).context(format!(
            "Failed to read Cargo.toml at {}",
            cargo_toml_path.display()
        ))?;

        let cargo_doc: toml::Table = toml::from_str(&cargo_content).context(format!(
            "Failed to parse Cargo.toml at {}",
            cargo_toml_path.display()
        ))?;

        if let Some(toml::Value::Table(workspace)) = cargo_doc.get("workspace") {
            // Compare workspace members
            if let Some(toml::Value::Array(members)) = workspace.get("members") {
                let cargo_members: Vec<String> = members
                    .iter()
                    .filter_map(|v| v.as_str())
                    .map(|s| s.to_string())
                    .collect();

                let magnet_members = &workspace_manager.primary_workspace.config.workspace.members;

                if &cargo_members != magnet_members {
                    println!(
                        "Warning: Mismatch between Cargo.toml and Magnet.toml workspace members"
                    );
                    println!("Run 'magnet generate' to update Cargo.toml");
                }
            }
        } else {
            println!("Warning: Cargo.toml exists but doesn't define a workspace");
        }
    }

    println!("Configuration check completed successfully");

    Ok(())
}

/// List all crates in the workspace
pub fn list(config_path: &Path) -> Result<()> {
    // Resolve the config path
    let config_path = if config_path.is_relative() {
        std::env::current_dir()?.join(config_path)
    } else {
        config_path.to_path_buf()
    };

    // Make sure the config file exists
    if !config_path.exists() {
        return Err(anyhow::anyhow!(
            "Magnet.toml not found at {}",
            config_path.display()
        ));
    }

    // Load the configuration
    let config = MagnetConfig::from_file(&config_path)?;

    // Get the base directory (where Magnet.toml is located)
    let base_dir = config_path.parent().unwrap_or(Path::new(".")).to_path_buf();

    // Create a workspace manager
    let mut workspace_manager = WorkspaceManager::new(config, base_dir)?;

    // Discover related workspaces
    workspace_manager.discover_related_workspaces()?;

    // Get all crates in the workspace
    let all_crates = workspace_manager.get_all_crates();

    println!("\nCrates in workspace:");
    println!("{:<30} {:<15} {:<50}", "NAME", "VERSION", "PATH");
    println!("{:-<30} {:-<15} {:-<50}", "", "", "");

    for crate_info in all_crates {
        let crate_path = crate_info.cargo_toml_path.parent().unwrap_or(Path::new(""));
        let rel_path = pathdiff::diff_paths(crate_path, &workspace_manager.get_root_path())
            .unwrap_or_else(|| crate_path.to_path_buf());

        println!(
            "{:<30} {:<15} {:<50}",
            crate_info.name,
            crate_info.version.unwrap_or_else(|| "unknown".to_string()),
            rel_path.display()
        );
    }

    // Count external workspaces if any
    let external_workspaces = workspace_manager.get_external_workspaces();
    if !external_workspaces.is_empty() {
        println!("\nExternal workspaces: {}", external_workspaces.len());
        for workspace in external_workspaces {
            println!("  - {}", workspace.get_root_path().display());
        }
    }

    Ok(())
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
        return Err(anyhow::anyhow!(
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
            .unwrap_or("nexus").to_string()
    };
    
    // Print the nexus root directly - no header line
    println!("🧲 nexus: {} (Magnet.toml)", nexus_name);
    
    // Get all the subdirectories that should be shown as children
    let mut subdirs: Vec<PathBuf> = std::fs::read_dir(&base_dir)?
        .filter_map(|e| e.ok().map(|e| e.path()))
        .filter(|p| p.is_dir())
        .collect();
    
    // Sort directories
    subdirs.sort();
    
    // Print each child with the appropriate prefix
    for (idx, subdir) in subdirs.iter().enumerate() {
        let is_last = idx == subdirs.len() - 1;
        
        let subdir_canonical = subdir
            .canonicalize()
            .unwrap_or_else(|_| subdir.to_path_buf());
            
        // Check if this subdirectory contains any Magnet/Cargo files before showing
        let should_show = subdir_canonical.join("Cargo.toml").exists()
            || subdir_canonical.join("Magnet.toml").exists()
            || has_nested_project_files(&subdir_canonical)?;
            
        if should_show {
            // Skip "crates" directory but still check its children
            if subdir.file_name().and_then(|n| n.to_str()) == Some("crates") {
                // Process crates subdirectories
                let crates_subdirs: Vec<PathBuf> = std::fs::read_dir(subdir)?
                    .filter_map(|e| e.ok().map(|e| e.path()))
                    .filter(|p| p.is_dir())
                    .collect();
                
                // Handle each crate
                for (crate_idx, crate_dir) in crates_subdirs.iter().enumerate() {
                    let crate_is_last = is_last && crate_idx == crates_subdirs.len() - 1;
                    let crate_prefix = if crate_is_last { "└── " } else { "├── " };
                    
                    print_unified_tree(
                        &base_dir,
                        crate_dir,
                        "",
                        crate_is_last,
                        &workspace_map,
                        &workspace_manager,
                        show_dependencies,
                    )?;
                }
            } else {
                // Regular directory - show normally
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
        }
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
    workspace_manager: &WorkspaceManager,
) -> Result<std::collections::HashMap<PathBuf, Vec<PathBuf>>> {
    let mut result = std::collections::HashMap::new();

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
    }

    Ok(result)
}

/// Print a unified tree of workspaces and crates
fn print_unified_tree(
    root_dir: &Path,
    current_dir: &Path,
    prefix: &str,
    is_last: bool,
    workspace_map: &std::collections::HashMap<PathBuf, Vec<PathBuf>>,
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

    // Skip the "crates" directory itself, but process its children
    if dir_name == "crates" && current_dir != root_dir {
        // Calculate the new prefix for children
        let child_prefix = if is_last {
            format!("{}    ", prefix)
        } else {
            format!("{}│   ", prefix)
        };

        // Process subdirectories without printing the "crates" directory itself
        let mut subdirs: Vec<PathBuf> = std::fs::read_dir(current_dir)?
            .filter_map(|e| e.ok().map(|e| e.path()))
            .filter(|p| p.is_dir())
            .collect();

        // Sort directories
        subdirs.sort();
        
        // Recursively process subdirectories
        for (idx, subdir) in subdirs.iter().enumerate() {
            let is_last_child = idx == subdirs.len() - 1;

            let subdir_canonical = subdir
                .canonicalize()
                .unwrap_or_else(|_| subdir.to_path_buf());

            // Check if this subdirectory contains any Magnet/Cargo files before recursing
            let should_show = subdir_canonical.join("Cargo.toml").exists()
                || subdir_canonical.join("Magnet.toml").exists()
                || has_nested_project_files(&subdir_canonical)?;

            if should_show {
                print_unified_tree(
                    root_dir,
                    subdir,
                    &prefix,  // Use parent's prefix to skip a level
                    idx == subdirs.len() - 1,  // Update is_last based on parent level
                    workspace_map,
                    workspace_manager,
                    show_dependencies,
                )?;
            }
        }
        
        return Ok(());
    }

    // Determine node type and label
    let (node_type, node_name) = if has_magnet_toml {
        // Determine the type based on the Magnet.toml content
        if let Ok(config) = MagnetConfig::from_file(&magnet_toml_path) {
            let (config_type, config_label) = if config.nexus.is_some() {
                ("🧲", "nexus")  // Nexus uses magnet icon
            } else if !config.workspace.members.is_empty() || config.workspace.search_paths.is_some() {
                ("📦", "workspace")  // Normal workspace uses package icon
            } else {
                ("📄", "package")  // Package uses document icon
            };
            
            // For root directory, use different naming
            if current_dir == root_dir {
                let file_name = magnet_toml_path.file_name().unwrap_or_default().to_string_lossy();
                
                // Get name for nexus - use project name if available, otherwise use parent directory name
                let nexus_name = if let Some(project_name) = config.project.name {
                    project_name
                } else {
                    // Use parent directory name if project name is missing
                    current_dir
                        .file_name()
                        .and_then(|n| n.to_str())
                        .unwrap_or("unknown").to_string()
                };
                
                (config_type, format!("{}: {} ({})", 
                    config_label,
                    nexus_name,
                    file_name))
            } else {
                let dir_name = dir_name.to_string();
                (config_type, format!("{} ({})", 
                    config.project.name.unwrap_or_else(|| dir_name.clone()),
                    config_label))
            }
        } else {
            ("🧲", get_node_name(current_dir, Some(&magnet_toml_path), cargo_path_opt(&cargo_toml_path))?)
        }
    } else if has_cargo_toml {
        // Only Cargo.toml exists - it's a crate
        ("", get_node_name(current_dir, None, Some(&cargo_toml_path))?)
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

    // Process subdirectories
    let mut subdirs: Vec<PathBuf> = std::fs::read_dir(current_dir)?
        .filter_map(|e| e.ok().map(|e| e.path()))
        .filter(|p| p.is_dir())
        .collect();

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

        // Don't show related workspaces here, we showed symbolic links above
        let subdir_canonical = subdir
            .canonicalize()
            .unwrap_or_else(|_| subdir.to_path_buf());

        // Check if this subdirectory contains any Magnet/Cargo files before recursing
        let should_show = subdir_canonical.join("Cargo.toml").exists()
            || subdir_canonical.join("Magnet.toml").exists()
            || has_nested_project_files(&subdir_canonical)?;

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
