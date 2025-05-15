use crate::configs::{DependencyConfig, DetailedDependency};
use std::path::PathBuf;

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
}
