//! Command implementation for exporting local dependencies
//!
//! This module provides functionality to export local dependencies from a Rust project,
//! creating symlinks to target/export/crates/ and generating a workspace Cargo.toml.
//! It supports workspace = true and nexus = true dependencies by resolving them to paths.

use crate::generator::CargoGenerator;
use crate::manager::ManifestManager;
use crate::models::{DependencyModel, ManifestModel, PackageModel, PatchMap, WorkspaceModel};
use crate::utils::maybe_join;
use eyre::{Context, Result};
use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};
use tracing::{debug, info, warn};
// -----------------------------------------------------------------------------
// Public API
// -----------------------------------------------------------------------------

/// Configuration options for the export command
#[derive(Debug, Clone)]
pub struct ExportOptions {
    /// Path to the package or workspace directory
    pub package_path: PathBuf,
    /// Path to the export directory (default: ./target/export)
    pub export_dir: Option<PathBuf>,
    /// Name of the crates subdirectory (default: "crates")
    pub crates_dir: String,
    /// Copy Cargo.lock file if it exists
    pub copy_lock: bool,
    /// Link or copy .cargo directory if it exists
    pub include_cargo_dir: bool,
    /// Whether to create symlinks for .cargo directory (true) or copy it (false)
    pub symlink_cargo_dir: bool,
    /// Clean the export directory before exporting
    pub clean: bool,
}

impl Default for ExportOptions {
    fn default() -> Self {
        Self {
            package_path: PathBuf::from("."),
            export_dir: None, // Default to None, we'll use ./target/export at runtime
            crates_dir: "crates".to_string(),
            copy_lock: true,
            include_cargo_dir: true,
            symlink_cargo_dir: true,
            clean: true,
        }
    }
}

/// Export command - exports local dependencies of a package/workspace
///
/// Creates soft links to target/export/crates/ and generates a workspace Cargo.toml
/// that brings all dependencies together in one workspace.
pub fn export(options: &ExportOptions) -> Result<()> {
    let exporter = Exporter::new(options)?;
    exporter.run(options)
}

// -----------------------------------------------------------------------------
// Exporter Implementation
// -----------------------------------------------------------------------------

/// Struct to manage the export process
struct Exporter {
    /// The manifest model (workspace or package)
    manifest: ManifestModel,
    /// Root path for export operations
    root_path: PathBuf,
    /// Export directory path
    export_dir: PathBuf,
    /// Export crates directory path
    export_crates_dir: PathBuf,
    /// Set of paths that have been processed already
    processed_paths: HashSet<PathBuf>,
    /// Set of crate names that have been processed
    processed_crates: HashSet<String>,
    /// List of workspace members
    workspace_members: Vec<String>,
    /// Nexus manager for resolving dependencies
    nexus_manager: ManifestManager,
    /// Name of the crates directory
    crates_dir_name: String,
    patch: PatchMap,
}

impl Exporter {
    /// Create a new exporter instance
    fn new(options: &ExportOptions) -> Result<Self> {
        info!(
            "Preparing to export local dependencies from {}",
            options.package_path.display()
        );

        // Parse the manifest
        let manifest = ManifestModel::from_dir(&options.package_path)?;

        // Get the current working directory for determining the export path
        let current_dir =
            std::env::current_dir().context("Failed to get current working directory")?;

        // Define export directory paths
        let export_dir = match &options.export_dir {
            Some(path) => path.clone(),
            None => current_dir.join("target/export"),
        };

        let export_crates_dir = export_dir.join(&options.crates_dir);

        // Create nexus manager for resolving workspace and nexus dependencies
        let nexus_manager = ManifestManager::from_dir(&options.package_path)?;

        Ok(Self {
            manifest,
            root_path: nexus_manager.root_path.clone(),
            export_dir,
            export_crates_dir,
            processed_paths: HashSet::new(),
            processed_crates: HashSet::new(),
            workspace_members: Vec::new(),
            patch: nexus_manager.root_manifest.patch().clone(),
            nexus_manager,
            crates_dir_name: options.crates_dir.clone(),
        })
    }

    // -------------------------------------------------------------------------
    // Main Execution Flow
    // -------------------------------------------------------------------------

    /// Run the export process
    fn run(mut self, options: &ExportOptions) -> Result<()> {
        // Initialize the export directory
        if options.clean {
            self.init_export_directory()?;
        }

        if let ManifestModel::Package(pkg) = &self.manifest.clone() {
            self.process_package(pkg, true)?;
        };

        // Process all packages from the manifest
        let packages = self.manifest.list_packages()?;
        for package in &packages {
            self.process_package(package, false)?;
        }

        self.process_manifest_patches()?;

        // Sort workspace members for consistent output
        self.workspace_members.sort();

        // Create export workspace model with updated dependency paths
        let export_workspace = self.create_export_workspace()?;

        // Generate Cargo.toml files using the generator
        self.generate_cargo_toml_files(&export_workspace)?;

        // Optionally copy Cargo.lock file
        if options.copy_lock {
            self.copy_cargo_lock()?;
        }

        // Optionally link or copy .cargo directory
        if options.include_cargo_dir {
            self.handle_cargo_directory(options.symlink_cargo_dir)?;
        }

        // Print summary
        self.print_summary();

        Ok(())
    }

    /// Initialize the export directory structure
    fn init_export_directory(&self) -> Result<()> {
        // Clean up existing directory if it exists
        if self.export_dir.exists() {
            fs::remove_dir_all(&self.export_dir).context(format!(
                "Failed to clean up existing export directory: {}",
                self.export_dir.display()
            ))?;
        }

        // Create fresh directories
        fs::create_dir_all(&self.export_crates_dir).context(format!(
            "Failed to create export crates directory: {}",
            self.export_crates_dir.display()
        ))?;

        Ok(())
    }

    // -------------------------------------------------------------------------
    // Core Dependency Processing
    // -------------------------------------------------------------------------

    /// Unified method to process a package and its dependencies
    /// This replaces export_main_package, process_packages, and create_symlinks_for_package
    fn process_package(&mut self, package: &PackageModel, is_main_package: bool) -> Result<()> {
        // Skip if already processed
        if self.processed_crates.contains(&package.name) {
            return Ok(());
        }

        // 1. First handle the package itself (symlink creation)
        let target_dir = self.export_crates_dir.join(&package.name);

        // Create symbolic link for the package
        self.create_symlink(&package.root_path, &target_dir)?;

        // Update tracking information
        self.processed_paths.insert(package.root_path.clone());
        self.processed_crates.insert(package.name.clone());
        self.workspace_members
            .push(format!("{}/{}", self.crates_dir_name, package.name));

        // Log appropriately
        if is_main_package {
            info!(
                "Linked main package {} -> {}",
                target_dir.display(),
                package.root_path.display()
            );
        } else {
            info!(
                "Linked package {} -> {}",
                target_dir.display(),
                package.root_path.display()
            );
        }

        // 2. Process the package's dependencies

        // Clone the package to allow for mutable operations
        let package_clone = package.clone();

        // Track dependencies that need recursive processing
        let mut deps_to_process_recursively = Vec::new();

        // Process all dependencies with paths (original and newly resolved ones)
        for (crate_name, dep) in &package_clone.dependencies {
            // Skip already processed crates
            if self.processed_crates.contains(crate_name) {
                continue;
            }
            if !(dep.path.is_some() || dep.workspace() || dep.nexus()) {
                continue;
            }

            // Process path dependency
            if let Some(canonical_path) =
                self.process_path_dependency(&package_clone.root_path, crate_name, dep)?
            {
                // Queue this dependency for recursive processing
                deps_to_process_recursively.push((crate_name.clone(), canonical_path));
            }
        }
        // Recursively process all dependencies' dependencies
        self.process_recursive_dependencies(deps_to_process_recursively)?;

        Ok(())
    }

    /// Process a path dependency and create a symlink for it
    /// Returns Some(canonical_path) if the dependency was processed, None otherwise
    fn process_path_dependency(
        &mut self,
        manifest_root_path: &Path,
        crate_name: &str,
        dep: &DependencyModel,
    ) -> Result<Option<PathBuf>> {
        // Convert to absolute path
        let dep = self
            .nexus_manager
            .resolve_dependency(manifest_root_path, &crate_name, &dep)?;
        let Some(dep_path) = &dep.path else {
            warn!("No path found for dependency {}", crate_name);
            return Ok(None);
        };
        // Canonicalize the path to resolve any '..' components
        let canonical_path = maybe_join(manifest_root_path, dep_path).canonicalize()?;
        // Skip if already processed
        if self.processed_paths.contains(&canonical_path) {
            return Ok(None);
        }

        let target_dir = self.export_crates_dir.join(crate_name);

        // Create symbolic link and update tracking
        self.create_symlink(&canonical_path, &target_dir)?;
        self.workspace_members
            .push(format!("{}/{}", self.crates_dir_name, crate_name));
        info!(
            "Linked dependency {} -> {}",
            target_dir.display(),
            canonical_path.display()
        );

        self.processed_paths.insert(canonical_path.clone());
        self.processed_crates.insert(crate_name.to_string());

        Ok(Some(canonical_path))
    }

    /// Recursively process dependencies of dependencies
    fn process_recursive_dependencies(&mut self, deps: Vec<(String, PathBuf)>) -> Result<()> {
        for (dep_name, dep_path) in deps {
            debug!("Recursively processing dependencies of {}", dep_name);

            // Try to load dependency's manifest
            match ManifestModel::from_dir(&dep_path) {
                Ok(manifest) => {
                    // Get all packages from the manifest
                    match manifest.list_packages() {
                        Ok(dep_packages) => {
                            for dep_package in &dep_packages {
                                // Recursively process each package's dependencies
                                // Use our unified process_package method instead of create_symlinks_for_package
                                if let Err(err) = self.process_package(dep_package, false) {
                                    warn!(
                                        "Error recursively processing dependencies of {}: {}",
                                        dep_name, err
                                    );
                                    // Continue with other dependencies
                                }
                            }
                        }
                        Err(err) => {
                            warn!(
                                "Failed to list packages from dependency {}: {}",
                                dep_name, err
                            );
                        }
                    }
                }
                Err(err) => {
                    warn!(
                        "Failed to load manifest for dependency {}: {}",
                        dep_name, err
                    );
                }
            }
        }

        Ok(())
    }

    /// Process patches from a package
    fn process_manifest_patches(&mut self) -> Result<()> {
        let patch_table = self.patch.clone();
        if patch_table.is_empty() {
            return Ok(());
        }

        for (registry_name, registry_patches) in patch_table.iter() {
            info!("Processing registry patches for: {}", registry_name);

            // For each patched crate
            for (crate_name, patch_config) in registry_patches.iter() {
                // Skip already processed crates
                if self.processed_crates.contains(crate_name) {
                    continue;
                }

                let Some(path) = patch_config.path.as_ref() else {
                    continue;
                };
                if path.is_absolute() {
                    continue;
                }
                info!(
                    "Found patch with relative path for {}: {}",
                    crate_name,
                    path.display()
                );

                // Process the patch as a path dependency
                self.process_path_dependency(&self.root_path.clone(), crate_name, &patch_config)?;
                self.patch.get_mut(registry_name).unwrap().insert(
                    crate_name.clone(),
                    DependencyModel {
                        path: Some(PathBuf::from(format!(
                            "{}/{}",
                            self.crates_dir_name, crate_name
                        ))),
                        ..patch_config.clone()
                    },
                );
            }
        }

        Ok(())
    }

    // -------------------------------------------------------------------------
    // Workspace Generation
    // -------------------------------------------------------------------------

    /// Create a workspace model for the export directory
    /// This method also prepares the models with correct dependency paths
    fn create_export_workspace(&self) -> Result<WorkspaceModel> {
        // Get source information from original manifest
        let (name, description, resolver) = match &self.manifest {
            ManifestModel::Workspace(ws) => {
                (ws.name.clone(), ws.description.clone(), ws.resolver.clone())
            }
            ManifestModel::Package(pkg) => (
                pkg.name.clone(),
                Some(pkg.description.clone()),
                Some("2".to_string()),
            ),
            ManifestModel::Nexus(nexus) => (
                nexus.name.clone(),
                nexus.description.clone(),
                Some("2".to_string()),
            ),
        };

        // Create workspace model directly with original dependencies if available
        let mut dependencies = HashMap::new();
        if let ManifestModel::Workspace(ws) = &self.manifest {
            dependencies = ws.dependencies.clone();
        }

        // Ensure all exported packages are defined in workspace dependencies
        self.update_workspace_dependencies(&mut dependencies);

        // Create workspace model
        let workspace = WorkspaceModel {
            name,
            description,
            members: self.workspace_members.clone(),
            exclude: Vec::new(),
            resolver,
            custom: HashMap::new(),
            dependencies,
            patch: self.patch.clone(),
            root_path: self.export_dir.clone(),
            source_path: self.export_dir.join("Cargo.toml"),
        };

        Ok(workspace)
    }

    /// Update workspace dependencies to include all exported packages
    fn update_workspace_dependencies(&self, dependencies: &mut HashMap<String, DependencyModel>) {
        // Ensure all exported packages are defined in workspace dependencies
        // This allows packages to reference each other through workspace dependencies
        for crate_name in &self.processed_crates {
            // Only add if not already in dependencies
            if !dependencies.contains_key(crate_name) {
                // Create a workspace dependency pointing to the crate directory
                let path = PathBuf::from(format!("{}/{}", self.crates_dir_name, crate_name));
                let dep = DependencyModel {
                    path: Some(path),
                    ..DependencyModel::default()
                };

                dependencies.insert(crate_name.clone(), dep);

                debug!("Added workspace dependency for {}", crate_name);
            }
        }

        // Update paths in existing dependencies
        for (dep_name, detailed) in dependencies.iter_mut() {
            // If this is a dependency on an exported crate, update its path
            if self.processed_crates.contains(dep_name) {
                detailed.path = Some(PathBuf::from(format!(
                    "./{}/{}",
                    self.crates_dir_name, dep_name
                )));

                debug!("Updated workspace dependency path for {}", dep_name);
            }
        }
    }

    /// Generate Cargo.toml files using CargoGenerator
    fn generate_cargo_toml_files(&self, workspace: &WorkspaceModel) -> Result<()> {
        // Create the Cargo.toml generator
        let mut generator = CargoGenerator::new(self.nexus_manager.clone());

        // Generate workspace and package Cargo.toml files
        generator
            .generate_all(workspace)
            .context("Failed to generate Cargo.toml files")?;

        Ok(())
    }

    // -------------------------------------------------------------------------
    // Helper Methods
    // -------------------------------------------------------------------------

    /// Create a symbolic link with platform-specific implementation
    fn create_symlink(&self, source: &Path, target: &Path) -> Result<()> {
        #[cfg(unix)]
        {
            std::os::unix::fs::symlink(source, target).context(format!(
                "Failed to create symbolic link from {} to {}",
                source.display(),
                target.display()
            ))?;
        }

        #[cfg(windows)]
        {
            std::os::windows::fs::symlink_dir(source, target).context(format!(
                "Failed to create symbolic link from {} to {}",
                source.display(),
                target.display()
            ))?;
        }

        Ok(())
    }

    /// Copy Cargo.lock file to the export directory if it exists
    fn copy_cargo_lock(&self) -> Result<()> {
        let source_lock = self.root_path.join("Cargo.lock");
        if source_lock.exists() {
            let dest_lock = self.export_dir.join("Cargo.lock");
            debug!(
                "Copying Cargo.lock from {} to {}",
                source_lock.display(),
                dest_lock.display()
            );
            fs::copy(&source_lock, &dest_lock).context(format!(
                "Failed to copy Cargo.lock from {} to {}",
                source_lock.display(),
                dest_lock.display()
            ))?;
            info!("Copied Cargo.lock file");
        }

        Ok(())
    }

    /// Handle .cargo directory (symlink or copy)
    fn handle_cargo_directory(&self, create_symlink: bool) -> Result<()> {
        let source_cargo_dir = self.root_path.join(".cargo");
        if source_cargo_dir.exists() && source_cargo_dir.is_dir() {
            let dest_cargo_dir = self.export_dir.join(".cargo");

            // Remove existing destination if it exists
            if dest_cargo_dir.exists() {
                if dest_cargo_dir.is_dir() {
                    fs::remove_dir_all(&dest_cargo_dir).context(format!(
                        "Failed to remove existing .cargo directory at {}",
                        dest_cargo_dir.display()
                    ))?;
                } else {
                    fs::remove_file(&dest_cargo_dir).context(format!(
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
                self.create_symlink(&source_cargo_dir, &dest_cargo_dir)?;
                info!("Created symlink for .cargo directory");
            } else {
                debug!(
                    "Copying .cargo directory from {} to {}",
                    source_cargo_dir.display(),
                    dest_cargo_dir.display()
                );
                crate::utils::copy_path(&source_cargo_dir, &dest_cargo_dir)?;
                info!("Copied .cargo directory");
            }
        }

        Ok(())
    }

    /// Print summary information after export
    fn print_summary(&self) {
        info!(
            "Successfully exported {} local dependencies to {}",
            self.workspace_members.len(),
            self.export_dir.display()
        );

        info!("You can build all exported crates using:");
        info!("  cd {}", self.export_dir.display());
        info!("  cargo build");
    }
}
