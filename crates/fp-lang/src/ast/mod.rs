use eyre::Result;
use fp_core::ast::Expr;
use fp_core::diagnostics::{Diagnostic, DiagnosticLevel, DiagnosticManager};

const FERRO_CONTEXT: &str = "ferrophase.parser";

/// Parser for the FerroPhase language.
pub struct FerroPhaseParser {
    diagnostics: std::sync::Arc<DiagnosticManager>,
}

impl Default for FerroPhaseParser {
    fn default() -> Self {
        Self {
            diagnostics: std::sync::Arc::new(DiagnosticManager::new()),
        }
    }
}

impl FerroPhaseParser {
    /// Create a new parser instance.
    pub fn new() -> Self {
        Self::default()
    }

    /// Access the diagnostics collected by the parser.
    pub fn diagnostics(&self) -> std::sync::Arc<DiagnosticManager> {
        self.diagnostics.clone()
    }

    /// Clear any previously collected diagnostics.
    pub fn clear_diagnostics(&self) {
        self.diagnostics.clear();
    }

    fn record_diagnostic(&self, level: DiagnosticLevel, message: impl Into<String>) {
        let message = message.into();
        let diagnostic = match level {
            DiagnosticLevel::Error => Diagnostic::error(message),
            DiagnosticLevel::Warning => Diagnostic::warning(message),
            DiagnosticLevel::Info => Diagnostic::info(message),
        }
        .with_source_context(FERRO_CONTEXT.to_string());

        self.diagnostics.add_diagnostic(diagnostic);
    }

    fn record_error(&self, message: impl Into<String>) {
        self.record_diagnostic(DiagnosticLevel::Error, message);
    }

    /// Parse a FerroPhase expression directly into an fp-core AST expression.
    pub fn parse_expr_ast(&self, source: &str) -> Result<Expr> {
        let lexemes = crate::lexer::tokenizer::lex_lexemes(source).map_err(|err| {
            self.record_error(format!("failed to lex expression: {err}"));
            eyre::eyre!(err)
        })?;
        let (cst, idx) =
            crate::cst::parse_expr_lexemes_prefix_to_cst(&lexemes, 0).map_err(|err| {
                self.record_error(format!("failed to parse expression CST: {err}"));
                eyre::eyre!(err)
            })?;
        if lexemes[idx..].iter().any(|lexeme| !lexeme.is_trivia()) {
            self.record_error("trailing tokens after expression");
            return Err(eyre::eyre!("trailing tokens after expression"));
        }
        crate::ast::lower_expr_from_cst(&cst).map_err(|err| {
            self.record_error(format!("failed to lower expression CST: {err}"));
            eyre::eyre!(err)
        })
    }

    /// Parse an expression into CST.
    pub fn parse_expr_cst(&self, source: &str) -> Result<crate::syntax::SyntaxNode> {
        let lexemes = crate::lexer::tokenizer::lex_lexemes(source).map_err(|err| {
            self.record_error(format!("failed to lex expression: {err}"));
            eyre::eyre!(err)
        })?;
        crate::cst::parse_expr_lexemes_to_cst(&lexemes, 0).map_err(|err| {
            self.record_error(format!("failed to parse expression CST: {err}"));
            eyre::eyre!(err)
        })
    }

    /// Parse a sequence of items into fp-core AST items.
    pub fn parse_items_ast(&self, source: &str) -> Result<Vec<fp_core::ast::Item>> {
        let tokens = crate::lexer::tokenizer::lex(source).map_err(|err| {
            self.record_error(format!("failed to lex items: {err}"));
            eyre::eyre!(err)
        })?;
        let tokens = crate::tokens::rewrite::lower_tokens(tokens).map_err(|err| {
            self.record_error(format!("failed to lower tokens: {err}"));
            eyre::eyre!(err)
        })?;
        let cst = crate::cst::items::parse_items_tokens_to_cst(&tokens).map_err(|err| {
            self.record_error(format!("failed to parse items CST: {err}"));
            eyre::eyre!(err)
        })?;
        crate::ast::lower_items_from_cst(&cst).map_err(|err| {
            self.record_error(format!("failed to lower items CST: {err}"));
            eyre::eyre!(err)
        })
    }
}

pub(crate) mod expr;
pub(crate) mod items;

pub(crate) use expr::lower_expr_from_cst;
pub(crate) use items::lower_items_from_cst;

#[cfg(test)]
mod tests;
