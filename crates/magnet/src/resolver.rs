//! Dependency resolver for magnet
//!
//! This module is responsible for resolving dependencies across workspaces,
//! handling path resolution, and managing dependency conflicts.

use anyhow::{Result, anyhow};
use std::collections::HashMap;
use std::path::PathBuf;

use crate::config::{DependencyConfig, DetailedDependency};
use crate::workspace::{CrateInfo, WorkspaceInfo};

/// Status of a dependency resolution
#[derive(Debug, Clone)]
pub enum ResolutionStatus {
    /// Dependency was resolved to a local path
    Resolved(PathBuf),
    /// Dependency is external (from crates.io or git)
    NotFound,
    /// Dependency resolution is ambiguous (multiple matches)
    Ambiguous(Vec<PathBuf>),
    /// Error during resolution
    Error(String),
}

/// A resolved dependency
#[derive(Debug, Clone)]
pub struct ResolvedDependency {
    /// Name of the dependency
    pub name: String,
    /// Original dependency configuration
    pub original_config: DependencyConfig,
    /// Merged dependency configuration
    pub resolved_config: DetailedDependency,
    /// Resolution status
    pub status: ResolutionStatus,
}

/// Dependency resolver
pub struct DependencyResolver {
    /// Primary workspace
    primary_workspace: WorkspaceInfo,
    /// Related workspaces
    related_workspaces: Vec<WorkspaceInfo>,
    /// Cache of resolved dependencies
    resolved_dependencies: HashMap<String, ResolvedDependency>,
}

impl DependencyResolver {
    /// Create a new dependency resolver
    pub fn new(primary_workspace: WorkspaceInfo, related_workspaces: Vec<WorkspaceInfo>) -> Self {
        Self {
            primary_workspace,
            related_workspaces,
            resolved_dependencies: HashMap::new(),
        }
    }

    /// Get a cached resolved dependency
    pub fn get_resolved_dependency(&self, name: &str) -> Option<&ResolvedDependency> {
        self.resolved_dependencies.get(name)
    }

    /// Resolve a dependency
    pub fn resolve_dependency(
        &mut self,
        name: &str,
        config: &DependencyConfig,
        from_crate: Option<&CrateInfo>,
    ) -> Result<ResolvedDependency> {
        // If already resolved, return from cache
        if let Some(resolved) = self.resolved_dependencies.get(name) {
            return Ok(resolved.clone());
        }

        // Extract the detailed configuration
        let detailed_config = match config {
            DependencyConfig::Simple(version) => DetailedDependency {
                version: Some(version.clone()),
                ..Default::default()
            },
            DependencyConfig::Detailed(detailed) => detailed.clone(),
        };

        // If auto is set to true, try to find the dependency in the workspace
        if let Some(true) = detailed_config.auto {
            // Auto-discovery: try to find the dependency in any workspace
            let mut matching_crates = Vec::new();

            // First check in the primary workspace
            for (crate_name, crate_info) in &self.primary_workspace.crates {
                // Match by converting kebab-case names to match the pattern
                // For example, "core-crate" in Magnet.toml matches "core" directory/crate
                let kebab_name = format!("{}-crate", crate_name.replace("_", "-"));
                if name == crate_name || name == &kebab_name {
                    matching_crates
                        .push(crate_info.cargo_toml_path.parent().unwrap().to_path_buf());
                }
            }

            // Then check in related workspaces
            for workspace in &self.related_workspaces {
                for (crate_name, crate_info) in &workspace.crates {
                    // Same matching logic as above
                    let kebab_name = format!("{}-crate", crate_name.replace("_", "-"));
                    if name == crate_name || name == &kebab_name {
                        matching_crates
                            .push(crate_info.cargo_toml_path.parent().unwrap().to_path_buf());
                    }
                }
            }

            // If we found exactly one match, use it
            if matching_crates.len() == 1 {
                let resolved = ResolvedDependency {
                    name: name.to_string(),
                    original_config: config.clone(),
                    resolved_config: detailed_config,
                    status: ResolutionStatus::Resolved(matching_crates[0].clone()),
                };

                // Cache and return
                self.resolved_dependencies
                    .insert(name.to_string(), resolved.clone());
                return Ok(resolved);
            } else if matching_crates.len() > 1 {
                // Multiple matches - try to select the best one or report ambiguity
                if from_crate.is_some() {
                    match self.select_closest_match(name, from_crate.unwrap(), &matching_crates) {
                        Ok(path) => {
                            let resolved = ResolvedDependency {
                                name: name.to_string(),
                                original_config: config.clone(),
                                resolved_config: detailed_config,
                                status: ResolutionStatus::Resolved(path),
                            };

                            // Cache and return
                            self.resolved_dependencies
                                .insert(name.to_string(), resolved.clone());
                            return Ok(resolved);
                        }
                        Err(_) => {
                            // Couldn't select a match, continue with normal resolution
                        }
                    }
                }
            }
        }

        // If explicit path is given, resolve relative to workspace root
        if let Some(path) = &detailed_config.path {
            let resolved_path = if path.is_absolute() {
                PathBuf::from(path)
            } else if let Some(crate_info) = from_crate {
                // Resolve relative to the crate directory
                let crate_dir = crate_info.cargo_toml_path.parent().ok_or_else(|| {
                    anyhow!(
                        "Failed to get parent directory of {}",
                        crate_info.cargo_toml_path.display()
                    )
                })?;
                crate_dir.join(path)
            } else {
                // Resolve relative to workspace root
                let workspace_root = &self.primary_workspace.root_path;
                workspace_root.join(path)
            };

            // Check if the path exists
            if resolved_path.exists() {
                let resolved = ResolvedDependency {
                    name: name.to_string(),
                    original_config: config.clone(),
                    resolved_config: detailed_config,
                    status: ResolutionStatus::Resolved(resolved_path),
                };

                // Cache and return
                self.resolved_dependencies
                    .insert(name.to_string(), resolved.clone());
                return Ok(resolved);
            } else {
                // Path doesn't exist
                let resolved = ResolvedDependency {
                    name: name.to_string(),
                    original_config: config.clone(),
                    resolved_config: detailed_config,
                    status: ResolutionStatus::Error(format!(
                        "Path does not exist: {}",
                        resolved_path.display()
                    )),
                };

                // Cache and return
                self.resolved_dependencies
                    .insert(name.to_string(), resolved.clone());
                return Ok(resolved);
            }
        }

        // If git or version is specified, it's an external dependency
        if detailed_config.git.is_some() || detailed_config.version.is_some() {
            let resolved = ResolvedDependency {
                name: name.to_string(),
                original_config: config.clone(),
                resolved_config: detailed_config,
                status: ResolutionStatus::NotFound,
            };

            // Cache and return
            self.resolved_dependencies
                .insert(name.to_string(), resolved.clone());
            return Ok(resolved);
        }

        // Look for matching crates in all workspaces
        let mut matching_crates = Vec::new();

        // First check in the primary workspace
        if let Some(crate_info) = self.primary_workspace.crates.get(name) {
            matching_crates.push(crate_info.cargo_toml_path.parent().unwrap().to_path_buf());
        }

        // Then check in other workspaces
        for workspace in &self.related_workspaces {
            if let Some(crate_info) = workspace.crates.get(name) {
                matching_crates.push(crate_info.cargo_toml_path.parent().unwrap().to_path_buf());
            }
        }

        // Determine the resolution status
        let status = match matching_crates.len() {
            0 => {
                // No matching crates found - it's an external dependency
                ResolutionStatus::NotFound
            }
            1 => {
                // Exactly one matching crate found
                ResolutionStatus::Resolved(matching_crates[0].clone())
            }
            _ => {
                // Multiple matching crates found
                if from_crate.is_some() {
                    // Attempt to select the closest match
                    let closest =
                        self.select_closest_match(name, from_crate.unwrap(), &matching_crates)?;
                    ResolutionStatus::Resolved(closest)
                } else {
                    // Ambiguous resolution
                    ResolutionStatus::Ambiguous(matching_crates)
                }
            }
        };

        // Create the resolved dependency
        let resolved = ResolvedDependency {
            name: name.to_string(),
            original_config: config.clone(),
            resolved_config: detailed_config,
            status,
        };

        // Cache and return
        self.resolved_dependencies
            .insert(name.to_string(), resolved.clone());
        Ok(resolved)
    }

    /// Select the closest matching crate
    fn select_closest_match(
        &self,
        _name: &str,
        from_crate: &CrateInfo,
        candidates: &[PathBuf],
    ) -> Result<PathBuf> {
        // Check if the from_crate exists in primary workspace
        let from_workspace = if self.primary_workspace.crates.contains_key(&from_crate.name) {
            &self.primary_workspace
        } else {
            self.related_workspaces
                .iter()
                .find(|w| w.crates.contains_key(&from_crate.name))
                .ok_or_else(|| anyhow!("Failed to find workspace for crate {}", from_crate.name))?
        };

        // First, prefer crates in the same workspace
        let same_workspace_candidates: Vec<_> = candidates
            .iter()
            .filter(|p| {
                let rel_path = p.strip_prefix(&from_workspace.root_path).ok();
                rel_path.is_some()
            })
            .collect();

        if same_workspace_candidates.len() == 1 {
            return Ok(same_workspace_candidates[0].clone());
        } else if same_workspace_candidates.len() > 1 {
            // If multiple in the same workspace, select by shortest path
            return Ok(same_workspace_candidates
                .iter()
                .min_by_key(|p| p.components().count())
                .unwrap()
                .to_path_buf());
        }

        // If none in the same workspace, use the most "specific" one
        // (with the longest path, assuming more nested = more specific)
        let most_specific = candidates
            .iter()
            .max_by_key(|p| p.components().count())
            .ok_or_else(|| anyhow!("Failed to select a dependency from candidates"))?;

        Ok(most_specific.clone())
    }

    /// Resolve all dependencies in a workspace
    pub fn resolve_all_workspace_dependencies(&mut self) -> Result<()> {
        // First, get all workspaces to be scanned
        let mut workspaces = vec![self.primary_workspace.clone()];
        workspaces.extend(self.related_workspaces.clone().into_iter());

        // For each workspace, resolve all dependencies
        for workspace in workspaces {
            self.resolve_workspace_dependencies(&workspace)?;
        }

        Ok(())
    }

    /// Resolve all dependencies in a specific workspace
    fn resolve_workspace_dependencies(&mut self, workspace: &WorkspaceInfo) -> Result<()> {
        // Resolve workspace-level dependencies
        for (name, config) in &workspace.config.dependencies {
            if !self.resolved_dependencies.contains_key(name) {
                let _ = self.resolve_dependency(name, config, None)?;
            }
        }

        // Resolve crate-specific dependencies
        for (_, crate_info) in &workspace.crates {
            // Load crate-specific config if available
            if let Some(magnet_toml_path) = &crate_info.magnet_toml_path {
                match crate::config::MagnetConfig::from_file(magnet_toml_path) {
                    Ok(config) => {
                        // Resolve all dependencies from this config
                        for (name, dep_config) in &config.dependencies {
                            if !self.resolved_dependencies.contains_key(name) {
                                let _ =
                                    self.resolve_dependency(name, dep_config, Some(&crate_info))?;
                            }
                        }
                    }
                    Err(e) => {
                        eprintln!(
                            "Warning: Failed to load Magnet.toml for {}: {}",
                            crate_info.name, e
                        );
                    }
                }
            }
        }

        Ok(())
    }
}
