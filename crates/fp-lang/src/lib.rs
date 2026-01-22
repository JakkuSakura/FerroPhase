use std::path::{Path, PathBuf};
use std::sync::Arc;

use crate::ast::FerroPhaseParser;
mod macro_parser;
mod normalization;
mod serializer;
use crate::macro_parser::FerroMacroExpansionParser;
use crate::normalization::FerroIntrinsicNormalizer;
use fp_core::ast::{Expr, ExprKind, Node};
pub use serializer::PrettyAstSerializer;
use fp_core::diagnostics::Diagnostic;
use fp_core::frontend::{FrontendResult, FrontendSnapshot, LanguageFrontend};
use fp_core::span::FileId;
use fp_core::intrinsics::IntrinsicNormalizer;
use fp_core::Result as CoreResult;

/// Canonical identifier for the FerroPhase source language.
pub const FERROPHASE: &str = "ferrophase";

/// Frontend that parses FerroPhase sources using the existing Rust infrastructure.
pub struct FerroFrontend {
    ferro: FerroPhaseParser,
}

fn register_source(path: PathBuf, source: &str) -> FileId {
    fp_core::source_map::source_map().register_or_update(path, source)
}

impl FerroFrontend {
    pub fn new() -> Self {
        Self {
            ferro: FerroPhaseParser::new(),
        }
    }

    fn clean_source(&self, source: &str) -> String {
        if source.starts_with("#!") {
            source.lines().skip(1).collect::<Vec<_>>().join("\n")
        } else {
            source.to_string()
        }
    }
}

fn strip_async_block(expr: Expr) -> Expr {
    if let ExprKind::Async(async_expr) = expr.kind() {
        if let ExprKind::Block(_) = async_expr.expr.kind() {
            return (*async_expr.expr).clone();
        }
    }
    expr
}

impl LanguageFrontend for FerroFrontend {
    fn language(&self) -> &'static str {
        FERROPHASE
    }

    fn extensions(&self) -> &'static [&'static str] {
        &["fp", "ferro", "rs", "rust", "ferrophase"]
    }

    fn parse(&self, source: &str, path: Option<&Path>) -> CoreResult<FrontendResult> {
        let cleaned = self.clean_source(source);
        let serializer = Arc::new(PrettyAstSerializer::new());
        let intrinsic_normalizer: Arc<dyn IntrinsicNormalizer> =
            Arc::new(FerroIntrinsicNormalizer::default());
        let macro_parser = Arc::new(FerroMacroExpansionParser::new());
        let source_path = match path {
            Some(path) => path.canonicalize().unwrap_or_else(|_| path.to_path_buf()),
            None => PathBuf::from("<expr>"),
        };
        let file_id = register_source(source_path.clone(), &cleaned);
        let source_path_display = source_path.clone();

        if path.is_some() {
            self.ferro.clear_diagnostics();
            return match self
                .ferro
                .parse_items_ast_with_file(&cleaned, file_id, Some(&source_path))
            {
                Ok(items) => {
                    let file = fp_core::ast::File {
                        path: source_path.clone(),
                        items,
                    };
                    let diagnostics = self.ferro.diagnostics();

                    let last = Node::file(file);
                    let mut ast = last.clone();
                    // Perform intrinsic normalization (includes Rust macro lowering via the
                    // provided normalizer strategy).
                    fp_core::intrinsics::normalize_intrinsics_with(
                        &mut ast,
                        intrinsic_normalizer.as_ref(),
                    )
                    .map_err(|e| fp_core::error::Error::from(e.to_string()))?;
                    let snapshot = FrontendSnapshot {
                        language: self.language().to_string(),
                        description: format!("FerroPhase LAST for {}", source_path_display.display()),
                        serialized: None,
                    };

                    Ok(FrontendResult {
                        last,
                        ast,
                        serializer,
                        intrinsic_normalizer: Some(intrinsic_normalizer.clone()),
                        macro_parser: Some(macro_parser.clone()),
                        snapshot: Some(snapshot),
                        diagnostics,
                    })
                }
                Err(err) => {
                    let mut diagnostic =
                        Diagnostic::error(format!("failed to parse items (file mode): {err}"));
                    if let Some(span) = self
                        .ferro
                        .diagnostics()
                        .get_diagnostics()
                        .iter()
                        .find_map(|diag| diag.span)
                    {
                        diagnostic = diagnostic.with_span(span);
                    }
                    Err(fp_core::error::Error::diagnostic(diagnostic))
                }
            };
        }

        // Expression-only mode (no resolved file path).

        self.ferro.clear_diagnostics();
        if let Ok(expr) = self.ferro.parse_expr_ast_with_file(&cleaned, file_id) {
            let expr = strip_async_block(expr);
            let diagnostics = self.ferro.diagnostics();
            let last = Node::expr(expr.clone());
            let mut ast = last.clone();
            fp_core::intrinsics::normalize_intrinsics_with(&mut ast, intrinsic_normalizer.as_ref())
                .map_err(|e| fp_core::error::Error::from(e.to_string()))?;
            return Ok(FrontendResult {
                last,
                ast,
                serializer,
                intrinsic_normalizer: Some(intrinsic_normalizer),
                macro_parser: Some(macro_parser.clone()),
                snapshot: None,
                diagnostics,
            });
        }

        match self
            .ferro
            .parse_items_ast_with_file(&cleaned, file_id, Some(&source_path))
        {
            Ok(items) => {
                let file = fp_core::ast::File {
                    path: Path::new("<expr>").to_path_buf(),
                    items,
                };
                let diagnostics = self.ferro.diagnostics();
                let last = Node::file(file);
                let mut ast = last.clone();
                fp_core::intrinsics::normalize_intrinsics_with(
                    &mut ast,
                    intrinsic_normalizer.as_ref(),
                )
                .map_err(|e| fp_core::error::Error::from(e.to_string()))?;
                Ok(FrontendResult {
                    last,
                    ast,
                    serializer,
                    intrinsic_normalizer: Some(intrinsic_normalizer),
                    macro_parser: Some(macro_parser.clone()),
                    snapshot: None,
                    diagnostics,
                })
            }
            Err(err) => {
                let mut diagnostic = Diagnostic::error(format!(
                    "failed to parse source as expression or items: {err}"
                ));
                if let Some(span) = self
                    .ferro
                    .diagnostics()
                    .get_diagnostics()
                    .iter()
                    .find_map(|diag| diag.span)
                {
                    diagnostic = diagnostic.with_span(span);
                }
                Err(fp_core::error::Error::diagnostic(diagnostic))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn language_identifier_is_ferrophase() {
        let frontend = FerroFrontend::new();
        assert_eq!(frontend.language(), FERROPHASE);
    }
}

pub mod ast;
pub mod cst;
pub mod lexer;
pub mod syntax;
pub(crate) mod tokens;
