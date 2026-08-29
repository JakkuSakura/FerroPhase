use fp_core::ast::ExprId;
use fp_core::ast::path::QualifiedPath;
use std::collections::HashMap;

/// Names resolved while lowering expressions, before type checking.
pub type ResolvedNameTable = HashMap<ExprId, ResolvedName>;

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
    /// Dependency metadata lowering publishes definitions, signatures, and
    /// impl headers without constructing executable bodies. This matches
    /// rustc's crate-metadata boundary: downstream resolution never needs a
    /// dependency's HIR bodies.
    pub resolution_only: bool,
}
