//! Command implementation for exporting local dependencies

use crate::configs::DetailedDependency;
use crate::generator::CargoGenerator;
use crate::manager::ManifestManager;
use crate::models::{ManifestModel, PackageModel, WorkspaceModel};
use eyre::{Context, Result};
use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};
use tracing::{debug, info, warn};

/// Configuration options for the export command
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
            crates_dir: "crates".to_string(), // Default crates directory name
            copy_lock: true,
            include_cargo_dir: true,
            symlink_cargo_dir: true,
            clean: true, // Default to cleaning for export
        }
    }
}

/// Export command - exports local dependencies of a package/workspace
/// Creates soft links to target/export/crates/ and generates a workspace Cargo.toml
pub fn export(options: &ExportOptions) -> Result<()> {
    let exporter = Exporter::new(options)?;
    exporter.run(options)
}

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
    /// Nexus manager
    nexus_manager: ManifestManager,
    /// Name of the crates directory
    crates_dir_name: String,
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

        // Find the root path for exports - this is the package path
        let package_root_path = options.package_path.canonicalize()?;

        // Get the current working directory (PWD) for determining the export path
        let current_dir =
            std::env::current_dir().context("Failed to get current working directory")?;

        // Define export directory paths
        // - If user provided an explicit path, use it
        // - Otherwise, use PWD/target/export regardless of package location
        let export_dir = match &options.export_dir {
            Some(path) => path.clone(),
            None => current_dir.join("target/export"),
        };

        let export_crates_dir = export_dir.join(&options.crates_dir);

        // Create nexus manager
        let nexus_manager = ManifestManager::from_dir(&options.package_path)?;

        Ok(Self {
            manifest,
            root_path: package_root_path,
            export_dir,
            export_crates_dir,
            processed_paths: HashSet::new(),
            processed_crates: HashSet::new(),
            workspace_members: Vec::new(),
            nexus_manager,
            crates_dir_name: options.crates_dir.clone(),
        })
    }

    /// Create a workspace model for the export directory without renaming packages
    /// This method also prepares the models with correct dependency paths
    fn create_export_workspace(&self) -> Result<WorkspaceModel> {
        // Get source information from original manifest
        let (name, description, resolver, patch) = match &self.manifest {
            ManifestModel::Workspace(ws) => (
                ws.name.clone(),
                ws.description.clone(),
                ws.resolver.clone(),
                ws.patch.clone(),
            ),
            ManifestModel::Package(pkg) => (
                pkg.name.clone(),
                Some(pkg.description.clone()),
                Some("2".to_string()),
                pkg.patch.clone(),
            ),
            ManifestModel::Nexus(nexus) => (
                nexus.name.clone(),
                nexus.description.clone(),
                Some("2".to_string()),
                None,
            ),
        };

        // Create workspace model directly with original dependencies if available
        let mut dependencies = HashMap::new();
        if let ManifestModel::Workspace(ws) = &self.manifest {
            dependencies = ws.dependencies.clone();
        }

        // Ensure all exported packages are defined in workspace dependencies
        // This allows packages to reference each other through workspace dependencies
        for crate_name in &self.processed_crates {
            // Only add if not already in dependencies
            if !dependencies.contains_key(crate_name) {
                // Create a workspace dependency pointing to the crate directory
                let path = PathBuf::from(format!("./{}/{}", self.crates_dir_name, crate_name));
                let dep = DetailedDependency {
                    path: Some(path),
                    ..DetailedDependency::default()
                };

                dependencies.insert(
                    crate_name.clone(),
                    crate::configs::DependencyConfig::Detailed(dep),
                );

                debug!("Added workspace dependency for {}", crate_name);
            }
        }

        // Update paths in existing dependencies
        for (dep_name, dep_config) in dependencies.iter_mut() {
            if let crate::configs::DependencyConfig::Detailed(detailed) = dep_config {
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

        // Create workspace model
        let workspace = WorkspaceModel {
            name,
            description,
            members: self.workspace_members.clone(),
            exclude: Vec::new(),
            resolver,
            search_paths: HashMap::new(),
            paths: HashMap::new(),
            custom: HashMap::new(),
            dependencies,
            patch,
            root_path: self.export_dir.clone(),
            source_path: self.export_dir.join("Cargo.toml"),
        };

        Ok(workspace)
    }

    /// Run the export process
    fn run(mut self, options: &ExportOptions) -> Result<()> {
        // Initialize the export directory
        if options.clean {
            self.init_export_directory()?;
        }

        // Export the main package itself first (the one being explicitly exported)
        self.export_main_package()?;

        // Process packages and their dependencies
        self.process_packages()?;

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

    /// Process all packages and their dependencies
    fn process_packages(&mut self) -> Result<()> {
        // Get all packages from the manifest
        let packages = self.manifest.list_packages()?;

        for package in &packages {
            self.create_symlinks_for_package(package)?;
        }

        // Sort workspace members for consistent output
        self.workspace_members.sort();

        Ok(())
    }

    /// Create symlinks for a package's dependencies
    /// Recursively processes dependencies of dependencies
    fn create_symlinks_for_package(&mut self, package: &PackageModel) -> Result<()> {
        // Clone the package to allow for mutable operations
        let mut package_clone = package.clone();
        
        // First, resolve any workspace or nexus dependencies
        let mut deps_to_resolve = Vec::new();
        
        // First pass: collect dependencies that need resolution
        for (crate_name, dep_config) in &package_clone.dependencies {
            // Only process DetailedDependency configs that have workspace=true or nexus=true
            if let crate::configs::DependencyConfig::Detailed(dep) = dep_config {
                // Try to resolve workspace dependencies
                if dep.workspace == Some(true) || dep.nexus == Some(true) {
                    deps_to_resolve.push(crate_name.clone());
                    debug!("Scheduled {} for resolution with workspace={:?}, nexus={:?}", 
                           crate_name, dep.workspace, dep.nexus);
                }
            }
        }
        
        // Second pass: resolve the collected dependencies
        for crate_name in deps_to_resolve {
            // Create a temporary copy for the resolution
            let pkg_for_resolve = package_clone.clone();
            
            if let Some(dep_config) = package_clone.dependencies.get_mut(&crate_name) {
                if let crate::configs::DependencyConfig::Detailed(dep) = dep_config {
                    // Create a temporary clone for resolution - we don't directly use the mutable reference
                    let dep_clone = crate::configs::DependencyConfig::Detailed(dep.clone());
                    
                    // Use the nexus_manager to resolve this dependency
                    match self.nexus_manager.resolve_dependency(&pkg_for_resolve, &crate_name, &dep_clone) {
                        Ok(resolved_dep) => {
                            // Update with the resolved dependency that now has a path
                            *dep = resolved_dep;
                            debug!("Successfully resolved {} to path: {:?}", crate_name, dep.path);
                        },
                        Err(err) => {
                            // Log warning but continue with other dependencies
                            warn!("Failed to resolve dependency {}: {}", crate_name, err);
                            continue;
                        }
                    }
                }
            }
        }

        // Track dependencies that need recursive processing
        let mut deps_to_process_recursively = Vec::new();

        // Now process all dependencies with paths (original and newly resolved ones)
        for (crate_name, dep_config) in &package_clone.dependencies {
            // Skip already processed crates
            if self.processed_crates.contains(crate_name) {
                continue;
            }

            // Only handle detailed dependency configs
            let crate::configs::DependencyConfig::Detailed(dep) = dep_config else {
                continue;
            };

            let Some(path) = &dep.path else {
                continue;
            };

            // Convert to absolute path
            let absolute_path = if path.is_absolute() {
                path.clone()
            } else {
                package.root_path.join(path)
            };

            // Canonicalize the path to resolve any '..' components
            let canonical_path = match absolute_path.canonicalize() {
                Ok(path) => path,
                Err(err) => {
                    warn!("Failed to canonicalize path for dependency {}: {} - {:?}",
                          crate_name, absolute_path.display(), err);
                    continue;
                }
            };

            // Skip if already processed
            if self.processed_paths.contains(&canonical_path) {
                continue;
            }

            let target_dir = self.export_crates_dir.join(crate_name);

            // Create symbolic link and update tracking
            self.create_symlink(&canonical_path, &target_dir)?;
            self.workspace_members
                .push(format!("{}/{}", self.crates_dir_name, crate_name));
            info!(
                "Linked {} -> {}",
                target_dir.display(),
                canonical_path.display()
            );

            self.processed_paths.insert(canonical_path.clone());
            self.processed_crates.insert(crate_name.clone());
            
            // Queue this dependency for recursive processing
            deps_to_process_recursively.push((crate_name.clone(), canonical_path));
        }

        // Process patches from this package
        self.process_package_patches(package)?;
        
        // Recursively process all dependencies' dependencies
        for (dep_name, dep_path) in deps_to_process_recursively {
            debug!("Recursively processing dependencies of {}", dep_name);
            
            // Try to load dependency's manifest
            match crate::models::ManifestModel::from_dir(&dep_path) {
                Ok(manifest) => {
                    // Get all packages from the manifest
                    match manifest.list_packages() {
                        Ok(dep_packages) => {
                            for dep_package in &dep_packages {
                                // Recursively process each package's dependencies
                                if let Err(err) = self.create_symlinks_for_package(dep_package) {
                                    warn!("Error recursively processing dependencies of {}: {}", dep_name, err);
                                    // Continue with other dependencies
                                }
                            }
                        },
                        Err(err) => {
                            warn!("Failed to list packages from dependency {}: {}", dep_name, err);
                            // Continue with other dependencies
                        }
                    }
                },
                Err(err) => {
                    warn!("Failed to load manifest for dependency {}: {}", dep_name, err);
                    // Continue with other dependencies
                }
            }
        }

        Ok(())
    }

    /// Process patches from a package
    fn process_package_patches(&mut self, package: &PackageModel) -> Result<()> {
        let Some(patch_table) = &package.patch else {
            return Ok(());
        };

        for (registry_name, registry_patches) in patch_table.iter() {
            debug!("Processing registry patches for: {}", registry_name);

            let Some(registry_table) = registry_patches.as_table() else {
                continue;
            };

            // For each patched crate
            for (crate_name, patch_config) in registry_table.iter() {
                // Skip already processed crates
                if self.processed_crates.contains(crate_name) {
                    continue;
                }

                let Some(patch_table) = patch_config.as_table() else {
                    continue;
                };

                // Check if it has a local path
                let Some(path_str) = patch_table.get("path").and_then(|v| v.as_str()) else {
                    continue;
                };

                let path = PathBuf::from(path_str);
                debug!(
                    "Found patch with path for {}: {}",
                    crate_name,
                    path.display()
                );

                // Convert to absolute path if relative
                let absolute_path = if path.is_absolute() {
                    path.clone()
                } else {
                    package.root_path.join(&path)
                };

                // Try to canonicalize the path
                let canonical_path = match absolute_path.canonicalize() {
                    Ok(path) => path,
                    Err(e) => {
                        warn!(
                            "Failed to canonicalize path for patch {}: {} - {:?}",
                            crate_name,
                            absolute_path.display(),
                            e
                        );
                        continue;
                    }
                };

                // Skip if already processed
                if self.processed_paths.contains(&canonical_path) {
                    continue;
                }

                let target_dir = self.export_crates_dir.join(crate_name);

                // Make sure parent directories exist
                if let Some(parent) = target_dir.parent() {
                    fs::create_dir_all(parent)?;
                }

                // Create symlink and update tracking
                self.create_symlink(&canonical_path, &target_dir)?;
                info!(
                    "Linked patch {} -> {}",
                    target_dir.display(),
                    canonical_path.display()
                );

                self.processed_paths.insert(canonical_path);
                self.processed_crates.insert(crate_name.clone());
                self.workspace_members
                    .push(format!("{}/{}", self.crates_dir_name, crate_name));
            }
        }

        Ok(())
    }

    /// Generate Cargo.toml files using CargoGenerator
    fn generate_cargo_toml_files(&mut self, workspace: &WorkspaceModel) -> Result<()> {
        // Create the Cargo.toml generator
        let mut generator = CargoGenerator::new(self.nexus_manager.clone());

        // Generate workspace and package Cargo.toml files
        generator
            .generate_all(workspace)
            .context("Failed to generate Cargo.toml files")?;

        Ok(())
    }

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

    /// Export the main package that's being explicitly exported
    fn export_main_package(&mut self) -> Result<()> {
        // Get the name and path of the main package being exported
        let (main_package_name, main_package_path) = match &self.manifest {
            ManifestModel::Package(pkg) => (pkg.name.clone(), pkg.root_path.clone()),
            ManifestModel::Workspace(_) => {
                // For workspace, use the workspace name and path
                let workspace_name = self
                    .root_path
                    .file_name()
                    .ok_or_else(|| eyre::eyre!("Could not determine workspace name"))?
                    .to_string_lossy()
                    .to_string();
                (workspace_name, self.root_path.clone())
            }
            ManifestModel::Nexus(_) => {
                // For nexus, use the nexus name and path
                let nexus_name = self
                    .root_path
                    .file_name()
                    .ok_or_else(|| eyre::eyre!("Could not determine nexus name"))?
                    .to_string_lossy()
                    .to_string();
                (nexus_name, self.root_path.clone())
            }
        };

        // Skip if already processed
        if self.processed_crates.contains(&main_package_name) {
            return Ok(());
        }

        // Create the target directory for this package
        let target_dir = self.export_crates_dir.join(&main_package_name);

        // Create symbolic link and update tracking
        self.create_symlink(&main_package_path, &target_dir)?;
        info!(
            "Linked main package {} -> {}",
            target_dir.display(),
            main_package_path.display()
        );

        self.processed_paths.insert(main_package_path);
        self.processed_crates.insert(main_package_name.clone());
        self.workspace_members
            .push(format!("{}/{}", self.crates_dir_name, main_package_name));

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
}
