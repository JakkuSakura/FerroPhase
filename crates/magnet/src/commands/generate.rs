//! Command implementation for generating Cargo.toml files from Magnet.toml

use crate::generator::CargoGenerator;
use crate::manager::ManifestManager;
use crate::models::WorkspaceModel;
use crate::utils;
use eyre::{Context, Result};
use std::path::PathBuf;
use tracing::{debug, info};

/// Configuration options for the generate command
pub struct GenerateOptions {
    /// Path to the Magnet.toml file
    pub config_path: PathBuf,
    /// Whether to clean the directories before generating files
    pub clean: bool,
    /// Copy Cargo.lock file if it exists
    pub copy_lock: bool,
    /// Link or copy .cargo directory if it exists
    pub include_cargo_dir: bool,
    /// Whether to create symlinks for .cargo directory (true) or copy it (false)
    pub symlink_cargo_dir: bool,
}

impl Default for GenerateOptions {
    fn default() -> Self {
        Self {
            config_path: PathBuf::from("."),
            clean: false,
            copy_lock: true,
            include_cargo_dir: true,
            symlink_cargo_dir: true,
        }
    }
}

/// Generate Cargo.toml files from Magnet.toml configuration
pub fn generate(options: &GenerateOptions) -> Result<()> {
    let config_path = &options.config_path;

    info!("Processing: {}", config_path.canonicalize()?.display());
    // Process the root configuration file and recursively generate all nested workspaces
    let nexus_manager = ManifestManager::from_dir(config_path)?;

    // Load the configuration
    let workspace = WorkspaceModel::from_dir(config_path)?;

    // Clean directories if requested
    if options.clean {
        clean_workspace_directories(&workspace)?;
    }

    // Create a generator
    let mut generator = CargoGenerator::new(nexus_manager.clone());

    // Generate all Cargo.toml files for this workspace
    generator.generate_all(&workspace).context(format!(
        "Failed to generate Cargo.toml files for {}",
        config_path.display()
    ))?;

    // Handle Cargo.lock file if it exists and option is enabled
    if options.copy_lock {
        copy_cargo_lock(&workspace)?;
    }

    // Handle .cargo directory if it exists and option is enabled
    if options.include_cargo_dir {
        handle_cargo_dir(&workspace, options.symlink_cargo_dir)?;
    }

    info!("Cargo.toml files updated successfully");

    Ok(())
}

/// Clean directories in the workspace before generation
fn clean_workspace_directories(workspace: &WorkspaceModel) -> Result<()> {
    info!("Cleaning workspace directories before generation");

    // Clean the root workspace directory, excluding important files/dirs
    let exclude_patterns = &[
        "Magnet.toml",
        ".git*",
        "src/*",
        "tests/*",
        "benches/*",
        "examples/*",
        "target/*",
    ];

    utils::clean_directory(&workspace.root_path, exclude_patterns)?;

    // Clean each package directory if needed
    let packages = workspace.list_packages()?;
    for package in packages {
        let package_path = &package.root_path;
        if package_path != &workspace.root_path {
            utils::clean_directory(package_path, exclude_patterns)?;
        }
    }

    Ok(())
}

/// Copy Cargo.lock file if it exists in the workspace root
fn copy_cargo_lock(workspace: &WorkspaceModel) -> Result<()> {
    // Check if Cargo.lock exists in the workspace root
    let source_lock = workspace.source_path.parent().unwrap().join("Cargo.lock");
    if source_lock.exists() {
        let dest_lock = workspace.root_path.join("Cargo.lock");
        debug!(
            "Copying Cargo.lock from {} to {}",
            source_lock.display(),
            dest_lock.display()
        );
        std::fs::copy(&source_lock, &dest_lock).context(format!(
            "Failed to copy Cargo.lock from {} to {}",
            source_lock.display(),
            dest_lock.display()
        ))?;
        info!("Copied Cargo.lock file");
    }

    Ok(())
}

/// Handle .cargo directory (symlink or copy)
fn handle_cargo_dir(workspace: &WorkspaceModel, create_symlink: bool) -> Result<()> {
    // Check if .cargo directory exists in the workspace root
    let source_cargo_dir = workspace.source_path.parent().unwrap().join(".cargo");
    if source_cargo_dir.exists() && source_cargo_dir.is_dir() {
        let dest_cargo_dir = workspace.root_path.join(".cargo");

        // Remove existing destination if it exists
        if dest_cargo_dir.exists() {
            if dest_cargo_dir.is_dir() {
                std::fs::remove_dir_all(&dest_cargo_dir).context(format!(
                    "Failed to remove existing .cargo directory at {}",
                    dest_cargo_dir.display()
                ))?;
            } else {
                std::fs::remove_file(&dest_cargo_dir).context(format!(
                    "Failed to remove existing .cargo file at {}",
                    dest_cargo_dir.display()
                ))?;
            }
        }

        if create_symlink {
            debug!(
                "Creating symlink for .cargo directory from {} to {}",
                source_cargo_dir.display(),
                dest_cargo_dir.display()
            );
            utils::create_symlink(&source_cargo_dir, &dest_cargo_dir)?;
            info!("Created symlink for .cargo directory");
        } else {
            debug!(
                "Copying .cargo directory from {} to {}",
                source_cargo_dir.display(),
                dest_cargo_dir.display()
            );
            utils::copy_path(&source_cargo_dir, &dest_cargo_dir)?;
            info!("Copied .cargo directory");
        }
    }

    Ok(())
}
