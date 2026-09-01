use fp_core::ast::path::QualifiedPath;
use std::collections::HashMap;

/// Names resolved while lowering expressions, before type checking.
pub type ResolvedNameTable = HashMap<QualifiedPath, ResolvedName>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ResolvedNameNamespace {
    Value,
    Type,
    Module,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResolvedName {
    pub namespace: ResolvedNameNamespace,
    pub path: QualifiedPath,
}

#[derive(Clone, Debug, Default)]
pub struct HirLoweringConfig {
    pub capabilities: fp_core::capabilities::LanguageCapabilities,
    /// Operation declarations supplied by the active source/provider
    /// packages. Resolution must consult these declarations, never a core
    /// hardcoded operation catalog.
    pub operations: fp_core::lang::LangItemRegistry,
    /// Dependency metadata lowering publishes definitions, signatures, and
    /// impl headers without constructing executable bodies. This matches
    /// rustc's crate-metadata boundary: downstream resolution never needs a
    /// dependency's HIR bodies.
    pub resolution_only: bool,
}
