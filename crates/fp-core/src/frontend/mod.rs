use crate::ast::{AstSerializer, MacroExpansionParser, Node};
use crate::diagnostics::DiagnosticManager;
use crate::error::Result;
use crate::intrinsics::IntrinsicNormalizer;
use std::path::Path;
use std::sync::Arc;

/// Controls how a frontend treats parse errors.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum FrontendParseMode {
    /// Parse failures are fatal errors.
    Strict,
    /// Parse failures are downgraded to warnings so processing can continue.
    Loose,
}

impl Default for FrontendParseMode {
    fn default() -> Self {
        Self::Strict
    }
}

/// Snapshot of the language-specific AST (LAST) produced by a frontend stage.
///
/// Records provenance as well as an optional serialised representation that downstream
/// tooling can inspect or persist alongside the canonical AST.
#[derive(Debug, Clone)]
pub struct FrontendSnapshot {
    pub language: String,
    pub description: String,
    pub serialized: Option<String>,
}

/// Result produced by a language frontend after normalising source code.
#[derive(Clone)]
pub struct FrontendResult {
    pub last: Node,
    pub ast: Node,
    pub serializer: Arc<dyn AstSerializer>,
    pub intrinsic_normalizer: Option<Arc<dyn IntrinsicNormalizer>>,
    pub macro_parser: Option<Arc<dyn MacroExpansionParser>>,
    pub snapshot: Option<FrontendSnapshot>,
    pub diagnostics: Arc<DiagnosticManager>,
}

/// Trait implemented by every source-language frontend.
pub trait LanguageFrontend: Send + Sync {
    fn language(&self) -> &'static str;
    fn extensions(&self) -> &'static [&'static str];

    /// Parse source as a standalone expression.
    fn parse_expr(&self, source: &str) -> Result<FrontendResult> {
        self.parse(source, None)
    }

    /// Parse source as a file (items).
    fn parse_file(&self, source: &str, path: &Path) -> Result<FrontendResult> {
        self.parse(source, Some(path))
    }

    /// Parse source, dispatching to parse_expr or parse_file based on path.
    fn parse(&self, source: &str, path: Option<&Path>) -> Result<FrontendResult> {
        match path {
            Some(p) => self.parse_file(source, p),
            None => self.parse_expr(source),
        }
    }

    fn parse_mode(&self) -> FrontendParseMode {
        FrontendParseMode::default()
    }

    fn set_parse_mode(&self, _mode: FrontendParseMode) {}
}
