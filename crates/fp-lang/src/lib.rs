use std::path::Path;
use std::sync::{Arc, Mutex};

use crate::parser::lower::lower_expr_from_cst;
use crate::parser::FerroPhaseParser;
use fp_core::ast::{Expr, ExprKind, Node};
use fp_core::diagnostics::Diagnostic;
use fp_core::frontend::{FrontendResult, FrontendSnapshot, LanguageFrontend};
use fp_core::intrinsics::IntrinsicNormalizer;
use fp_core::Result as CoreResult;
use fp_rust::normalization::normalize_last_to_ast;
use fp_rust::normalization::RustIntrinsicNormalizer;
use fp_rust::parser::RustParser;
use fp_rust::printer::RustPrinter;

/// Canonical identifier for the FerroPhase source language.
pub const FERROPHASE: &str = "ferrophase";

/// Frontend that parses FerroPhase sources using the existing Rust infrastructure.
pub struct FerroFrontend {
    parser: Mutex<RustParser>,
    ferro: FerroPhaseParser,
}

impl FerroFrontend {
    pub fn new() -> Self {
        Self {
            parser: Mutex::new(RustParser::new()),
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
            // File mode: first attempt to parse items directly using the
            // FerroPhase winnow-based parser. If that fails (due to
            // incomplete grammar coverage), fall back to the legacy
            // rewrite-to-Rust pipeline.
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
                    normalize_last_to_ast(&mut ast, Some(diagnostics.as_ref()));
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
                    // 如果未允许回退，直接返回 winnow 错误
                    if std::env::var_os("FP_ALLOW_FALLBACK").is_none() {
                        return Err(fp_core::error::Error::diagnostic(Diagnostic::error(
                            format!("strict winnow (file mode): {err}"),
                        )));
                    }

                    // 允许回退时，继续走 rewrite_to_rust
                    let rewritten = self.ferro.rewrite_to_rust(&cleaned)?;
                    let mut parser = match self.parser.lock() {
                        Ok(g) => g,
                        Err(poison) => poison.into_inner(),
                    };
                    parser.clear_diagnostics();
                    let file = parser.parse_file(&rewritten, path)?;
                    let diagnostics = parser.diagnostics();
                    drop(parser);

                    let last = Node::file(file);
                    let mut ast = last.clone();
                    normalize_last_to_ast(&mut ast, Some(diagnostics.as_ref()));
                    let snapshot = FrontendSnapshot {
                        language: self.language().to_string(),
                        description: format!("Rust LAST for {}", path.display()),
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
            }

            // let rewritten = self.ferro.rewrite_to_rust(&cleaned)?;
            // // Avoid panicking on poisoned lock; recover from poison
            // let mut parser = match self.parser.lock() {
            //     Ok(g) => g,
            //     Err(poison) => poison.into_inner(),
            // };
            // parser.clear_diagnostics();
            // let file = parser.parse_file(&rewritten, path)?;
            // let diagnostics = parser.diagnostics();
            // drop(parser);

            // let last = Node::file(file);
            // let mut ast = last.clone();
            // normalize_last_to_ast(&mut ast, Some(diagnostics.as_ref()));
            // let snapshot = FrontendSnapshot {
            //     language: self.language().to_string(),
            //     description: format!("Rust LAST for {}", path.display()),
            //     serialized: None,
            // };

            // return Ok(FrontendResult {
            //     last,
            //     ast,
            //     serializer,
            //     intrinsic_normalizer: Some(intrinsic_normalizer.clone()),
            //     snapshot: Some(snapshot),
            //     diagnostics,
            // });
        }

        // Expression-only mode (no resolved file path). Prefer the
        // winnow CST + lowering pipeline when possible, with a
        // fallback to the legacy Rust-based parser.
        // Special-case simple `let name = expr;` forms by peeling the binding
        // and parsing only the RHS expression. This keeps legacy tests that
        // expect a pure expression AST in "expr mode".
        // TODO(G): 在 expr 模式直接支持 let 绑定/语句块的 CST→AST 降级，
        //           移除这里的字符串剥离权宜之计。
        if let Some(stripped) = cleaned.strip_prefix("let ") {
            if let Some(eq_pos) = stripped.find('=') {
                let rhs = &stripped[eq_pos + 1..];
                let rhs = rhs.trim_end_matches(';').trim();
                if let Ok(expr) = self.ferro.parse_expr_ast(rhs) {
                    let last = Node::expr(expr.clone());
                    let mut ast = last.clone();
                    let diagnostics = self.ferro.diagnostics();
                    normalize_last_to_ast(&mut ast, Some(diagnostics.as_ref()));
                    return Ok(FrontendResult {
                        last,
                        ast,
                        serializer,
                        intrinsic_normalizer: Some(intrinsic_normalizer.clone()),
                        snapshot: None,
                        diagnostics,
                    });
                }
            }
        }

        self.ferro.clear_diagnostics();
        match self.ferro.parse_to_cst(&cleaned) {
            Ok(cst) => {
                if let Some(expr_ast) = lower_expr_from_cst(&cst) {
                    let expr_ast = strip_async_block(expr_ast);
                    let last = Node::expr(expr_ast.clone());
                    let mut ast = last.clone();

                    // Use diagnostics collected by the winnow-based parser
                    // for this path.
                    let diagnostics = self.ferro.diagnostics();
                    normalize_last_to_ast(&mut ast, Some(diagnostics.as_ref()));

                    return Ok(FrontendResult {
                        last,
                        ast,
                        serializer,
                        intrinsic_normalizer: Some(intrinsic_normalizer),
                        snapshot: None,
                        diagnostics,
                    });
                } else {
                    // Lowering failed: try direct expression parsing first so we
                    // still return an Expr in the common case; if that fails,
                    // fall back to full item parsing to keep fn/struct bodies usable.
                    if let Ok(expr) = self.ferro.parse_expr_ast(&cleaned) {
                        let expr = strip_async_block(expr);
                        let diagnostics = self.ferro.diagnostics();
                        let last = Node::expr(expr.clone());
                        let mut ast = last.clone();
                        normalize_last_to_ast(&mut ast, Some(diagnostics.as_ref()));
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
                            normalize_last_to_ast(&mut ast, Some(diagnostics.as_ref()));
                            return Ok(FrontendResult {
                                last,
                                ast,
                                serializer,
                                intrinsic_normalizer: Some(intrinsic_normalizer),
                                snapshot: None,
                                diagnostics,
                            });
                        }
                        Err(err) => {
                            return Err(fp_core::error::Error::diagnostic(Diagnostic::error(
                                format!("strict winnow (expr mode): {err}"),
                            )));
                        }
                    }
                }
            }
            Err(err) => {
                if std::env::var_os("FP_ALLOW_FALLBACK").is_none() {
                    return Err(fp_core::error::Error::diagnostic(Diagnostic::error(
                        format!("strict winnow (expr mode): {err}"),
                    )));
                }

                // 允许回退时，继续走 rewrite_to_rust
                let rewritten = self.ferro.rewrite_to_rust(&cleaned)?;
                let (expr, diagnostics) = {
                    let parser = match self.parser.lock() {
                        Ok(g) => g,
                        Err(poison) => poison.into_inner(),
                    };
                    parser.clear_diagnostics();
                    let expr = parser
                        .try_parse_as_file(&rewritten)
                        .or_else(|_| parser.try_parse_block_expression(&rewritten))
                        .or_else(|_| parser.try_parse_simple_expression(&rewritten))?;
                    let diagnostics = parser.diagnostics();
                    (expr, diagnostics)
                };

                let last = Node::expr(expr.as_ref().clone());
                let mut ast = last.clone();
                normalize_last_to_ast(&mut ast, Some(diagnostics.as_ref()));

                return Ok(FrontendResult {
                    last,
                    ast,
                    serializer,
                    intrinsic_normalizer: Some(intrinsic_normalizer),
                    snapshot: None,
                    diagnostics,
                });
            }
        }
        // let rewritten = self.ferro.rewrite_to_rust(&cleaned)?;
        // let (expr, diagnostics) = {
        //     let parser = match self.parser.lock() {
        //         Ok(g) => g,
        //         Err(poison) => poison.into_inner(),
        //     };
        //     parser.clear_diagnostics();
        //     let expr = parser
        //         .try_parse_as_file(&rewritten)
        //         .or_else(|_| parser.try_parse_block_expression(&rewritten))
        //         .or_else(|_| parser.try_parse_simple_expression(&rewritten))?;
        //     let diagnostics = parser.diagnostics();
        //     (expr, diagnostics)
        // };

        // let last = Node::expr(expr.as_ref().clone());
        // let mut ast = last.clone();
        // normalize_last_to_ast(&mut ast, Some(diagnostics.as_ref()));

        // Ok(FrontendResult {
        //     last,
        //     ast,
        //     serializer,
        //     intrinsic_normalizer: Some(intrinsic_normalizer),
        //     snapshot: None,
        //     diagnostics,
        // })
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

pub mod lexer;
pub mod parser;
