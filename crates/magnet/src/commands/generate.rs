//! Command implementation for generating Cargo.toml files from Magnet.toml

use crate::configs::MagnetConfig;
use crate::generator::CargoGenerator;
use crate::resolver::DependencyResolver;
use crate::workspace_manager::WorkspaceManager;
use anyhow::{Context, Result, anyhow};
use std::collections::HashSet;
use std::path::{Path, PathBuf};

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
        return Err(anyhow!(
            "Magnet.toml not found at {}",
            config_path.display()
        ));
    }

    // Process the root configuration file and recursively generate all nested workspaces
    let mut processed_paths = HashSet::new();
    generate_recursively(&config_path, &mut processed_paths, 0)?;

    println!("Cargo.toml files updated successfully");

    Ok(())
}

/// Recursively generate Cargo.toml files for a workspace and all its nested workspaces
fn generate_recursively(
    config_path: &Path,
    processed_paths: &mut HashSet<PathBuf>,
    depth: usize,
) -> Result<()> {
    // Add this config path to the processed set
    processed_paths.insert(config_path.to_path_buf());

    // Create indentation for better log readability
    let indent = "  ".repeat(depth);

    // Load the configuration
    let config = MagnetConfig::from_file(config_path).context(format!(
        "Failed to load Magnet.toml at {}",
        config_path.display()
    ))?;

    // Get the base directory (where Magnet.toml is located)
    let base_dir = config_path.parent().unwrap_or(Path::new(".")).to_path_buf();

    println!("{}Processing: {}", indent, base_dir.display());

    // Create a workspace manager for this config
    let mut workspace_manager =
        WorkspaceManager::new(config.clone(), base_dir.clone()).context(format!(
            "Failed to create workspace manager for {}",
            config_path.display()
        ))?;

    // Discover related workspaces
    workspace_manager
        .discover_related_workspaces()
        .context("Failed to discover related workspaces")?;

    // Create a dependency resolver
    let resolver = DependencyResolver::new(
        workspace_manager.primary_workspace.clone(),
        workspace_manager
            .related_workspaces
            .values()
            .cloned()
            .collect(),
    );

    // Create a generator
    let mut generator = CargoGenerator::new(workspace_manager.clone(), resolver);

    // Generate all Cargo.toml files for this workspace
    generator.generate_all().context(format!(
        "Failed to generate Cargo.toml files for {}",
        config_path.display()
    ))?;

    println!(
        "{}Generated Cargo.toml files for workspace at {}",
        indent,
        base_dir.display()
    );

    // Now process all nested workspaces

    // 1. Check if this is a nexus and process its workspaces
    if config.nexus.is_some() {
        if let Some(search_paths) = &config.nexus.as_ref().unwrap().search_paths {
            for (name, path) in search_paths {
                let abs_path = if path.is_absolute() {
                    path.clone()
                } else {
                    base_dir.join(path)
                };

                let nested_config_path = abs_path.join("Magnet.toml");

                if nested_config_path.exists() && !processed_paths.contains(&nested_config_path) {
                    println!(
                        "{}Processing nexus workspace: {} at {}",
                        indent,
                        name,
                        abs_path.display()
                    );
                    generate_recursively(&nested_config_path, processed_paths, depth + 1)?;
                }
            }
        }
    }

    // 2. Process directly related workspaces from workspace.search_paths
    if let Some(search_paths) = &config.workspace.search_paths {
        for (name, path) in search_paths {
            let abs_path = if path.is_absolute() {
                path.clone()
            } else {
                base_dir.join(path)
            };

            let nested_config_path = abs_path.join("Magnet.toml");

            if nested_config_path.exists() && !processed_paths.contains(&nested_config_path) {
                println!(
                    "{}Processing related workspace: {} at {}",
                    indent,
                    name,
                    abs_path.display()
                );
                generate_recursively(&nested_config_path, processed_paths, depth + 1)?;
            }
        }
    }

    // 3. Process each package that has its own Magnet.toml
    for crate_info in workspace_manager.get_all_crates() {
        if let Some(magnet_path) = &crate_info.magnet_toml_path {
            if magnet_path.exists() && !processed_paths.contains(magnet_path) {
                println!(
                    "{}Processing package: {} at {}",
                    indent,
                    crate_info.name,
                    crate_info.path.display()
                );
                generate_recursively(magnet_path, processed_paths, depth + 1)?;
            }
        }
    }

    // 4. Look for workspace members with their own Magnet.toml files
    for pattern in &config.workspace.members {
        let full_pattern = format!("{}/{}/Magnet.toml", base_dir.display(), pattern);

        // Process using glob patterns
        for entry in glob::glob(&full_pattern)? {
            if let Ok(nested_config_path) = entry {
                if nested_config_path.exists() && !processed_paths.contains(&nested_config_path) {
                    let member_dir = nested_config_path.parent().unwrap_or(Path::new("."));
                    println!(
                        "{}Processing workspace member at {}",
                        indent,
                        member_dir.display()
                    );
                    generate_recursively(&nested_config_path, processed_paths, depth + 1)?;
                }
            }
        }
    }

    // 5. Special handling for common "crates/" directory pattern
    let crates_dir = base_dir.join("crates");
    if crates_dir.exists() && crates_dir.is_dir() {
        // Scan for Magnet.toml files within crates directory
        if let Ok(entries) = std::fs::read_dir(crates_dir) {
            for entry in entries.filter_map(Result::ok) {
                let path = entry.path();
                if path.is_dir() {
                    let nested_config_path = path.join("Magnet.toml");
                    if nested_config_path.exists() && !processed_paths.contains(&nested_config_path)
                    {
                        println!("{}Processing crate at {}", indent, path.display());
                        generate_recursively(&nested_config_path, processed_paths, depth + 1)?;
                    }
                }
            }
        }
    }

    // 6. Deep scan for nested projects with specific patterns
    // Check for project1/crates/, project2/crates/, etc. patterns
    if let Ok(entries) = std::fs::read_dir(&base_dir) {
        for entry in entries.filter_map(Result::ok) {
            let path = entry.path();
            if path.is_dir() {
                // Skip already processed paths and hidden directories
                if path
                    .file_name()
                    .and_then(|n| n.to_str())
                    .map_or(false, |name| name.starts_with('.'))
                {
                    continue;
                }

                // Skip the crates directory itself (already handled above)
                if path.file_name().and_then(|n| n.to_str()) == Some("crates") {
                    continue;
                }

                // Check for this_dir/Magnet.toml
                let direct_config_path = path.join("Magnet.toml");
                if direct_config_path.exists() && !processed_paths.contains(&direct_config_path) {
                    println!("{}Processing project at {}", indent, path.display());
                    generate_recursively(&direct_config_path, processed_paths, depth + 1)?;
                }

                // Check for this_dir/crates/ pattern
                let nested_crates_dir = path.join("crates");
                if nested_crates_dir.exists() && nested_crates_dir.is_dir() {
                    if let Ok(crate_entries) = std::fs::read_dir(nested_crates_dir) {
                        for crate_entry in crate_entries.filter_map(Result::ok) {
                            let crate_path = crate_entry.path();
                            if crate_path.is_dir() {
                                let crate_config_path = crate_path.join("Magnet.toml");
                                if crate_config_path.exists()
                                    && !processed_paths.contains(&crate_config_path)
                                {
                                    println!(
                                        "{}Processing nested crate at {}",
                                        indent,
                                        crate_path.display()
                                    );
                                    generate_recursively(
                                        &crate_config_path,
                                        processed_paths,
                                        depth + 1,
                                    )?;
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    Ok(())
}
