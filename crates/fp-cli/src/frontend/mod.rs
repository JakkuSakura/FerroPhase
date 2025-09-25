use std::path::Path;
use std::sync::Arc;

use crate::CliError;
use fp_core::ast::{AstSerializer, Node};

pub mod registry;
pub mod rust;

pub use registry::FrontendRegistry;
pub use rust::RustFrontend;

/// Snapshot of the language-specific AST (LAST) produced by a frontend stage.
///
/// The snapshot is intentionally lightweight: it records provenance data and a
/// serialised form that downstream tooling can inspect or persist alongside the
/// canonical AST.
#[derive(Debug, Clone)]
pub struct FrontendSnapshot {
    pub language: String,
    pub description: String,
    pub serialized: Option<String>,
}

/// Result produced by a language frontend after normalising source code.
#[derive(Clone)]
pub struct FrontendResult {
    pub ast: Node,
    pub serializer: Arc<dyn AstSerializer>,
    pub snapshot: Option<FrontendSnapshot>,
}

/// Trait implemented by every source-language frontend.
pub trait LanguageFrontend: Send + Sync {
    fn language(&self) -> &'static str;
    fn extensions(&self) -> &'static [&'static str];
    fn parse(&self, source: &str, path: Option<&Path>) -> Result<FrontendResult, CliError>;
}
