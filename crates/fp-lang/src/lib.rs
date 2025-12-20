use std::path::Path;
use std::sync::Arc;

use crate::ast::FerroPhaseParser;
use fp_core::ast::{Expr, ExprKind, Node};
use fp_core::diagnostics::Diagnostic;
use fp_core::frontend::{FrontendResult, FrontendSnapshot, LanguageFrontend};
use fp_core::intrinsics::IntrinsicNormalizer;
use fp_core::Result as CoreResult;
use fp_rust::normalization::RustIntrinsicNormalizer;
use fp_rust::printer::RustPrinter;

/// Canonical identifier for the FerroPhase source language.
pub const FERROPHASE: &str = "ferrophase";

/// Frontend that parses FerroPhase sources using the existing Rust infrastructure.
pub struct FerroFrontend {
    ferro: FerroPhaseParser,
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
        let serializer = Arc::new(RustPrinter::new_with_rustfmt());
        let intrinsic_normalizer: Arc<dyn IntrinsicNormalizer> =
            Arc::new(RustIntrinsicNormalizer::default());

        if let Some(path) = path {
            self.ferro.clear_diagnostics();
            match self.ferro.parse_items_ast(&cleaned) {
                Ok(items) => {
                    let file = fp_core::ast::File {
                        path: path.to_path_buf(),
                        items,
                    };
                    let diagnostics = self.ferro.diagnostics();

                    let last = Node::file(file);
                    let mut ast = last.clone();
                    // Perform intrinsic normalization (includes Rust macro lowering via the
                    // provided normalizer strategy).
                    fp_optimize::passes::normalize_intrinsics_with(
                        &mut ast,
                        intrinsic_normalizer.as_ref(),
                    )
                    .map_err(|e| fp_core::error::Error::from(e.to_string()))?;
                    let snapshot = FrontendSnapshot {
                        language: self.language().to_string(),
                        description: format!("FerroPhase LAST for {}", path.display()),
                        serialized: None,
                    };

                    return Ok(FrontendResult {
                        last,
                        ast,
                        serializer,
                        intrinsic_normalizer: Some(intrinsic_normalizer.clone()),
                        snapshot: Some(snapshot),
                        diagnostics,
                    });
                }
                Err(err) => {
                    return Err(fp_core::error::Error::diagnostic(Diagnostic::error(
                        format!("failed to parse items (file mode): {err}"),
                    )));
                }
            }
        }

        // Expression-only mode (no resolved file path).

        self.ferro.clear_diagnostics();
        if let Ok(expr) = self.ferro.parse_expr_ast(&cleaned) {
            let expr = strip_async_block(expr);
            let diagnostics = self.ferro.diagnostics();
            let last = Node::expr(expr.clone());
            let mut ast = last.clone();
            fp_optimize::passes::normalize_intrinsics_with(&mut ast, intrinsic_normalizer.as_ref())
                .map_err(|e| fp_core::error::Error::from(e.to_string()))?;
            return Ok(FrontendResult {
                last,
                ast,
                serializer,
                intrinsic_normalizer: Some(intrinsic_normalizer),
                snapshot: None,
                diagnostics,
            });
        }

        match self.ferro.parse_items_ast(&cleaned) {
            Ok(items) => {
                let file = fp_core::ast::File {
                    path: Path::new("<expr>").to_path_buf(),
                    items,
                };
                let diagnostics = self.ferro.diagnostics();
                let last = Node::file(file);
                let mut ast = last.clone();
                fp_optimize::passes::normalize_intrinsics_with(
                    &mut ast,
                    intrinsic_normalizer.as_ref(),
                )
                .map_err(|e| fp_core::error::Error::from(e.to_string()))?;
                Ok(FrontendResult {
                    last,
                    ast,
                    serializer,
                    intrinsic_normalizer: Some(intrinsic_normalizer),
                    snapshot: None,
                    diagnostics,
                })
            }
            Err(err) => Err(fp_core::error::Error::diagnostic(Diagnostic::error(
                format!("failed to parse source as expression or items: {err}"),
            ))),
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
