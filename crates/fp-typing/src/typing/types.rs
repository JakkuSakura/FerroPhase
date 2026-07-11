use fp_core::module::path::QualifiedPath;
use fp_core::span::Span;
use std::collections::HashMap;

#[derive(Clone, Copy)]
pub enum TypingDiagnosticLevel {
    Error,
    Warning,
}

pub struct TypingDiagnostic {
    pub level: TypingDiagnosticLevel,
    pub message: String,
    pub span: Option<Span>,
}

impl TypingDiagnostic {
    pub fn error(message: impl Into<String>) -> Self {
        Self {
            level: TypingDiagnosticLevel::Error,
            message: message.into(),
            span: None,
        }
    }

    pub fn error_with_span(message: impl Into<String>, span: Span) -> Self {
        Self {
            level: TypingDiagnosticLevel::Error,
            message: message.into(),
            span: Some(span),
        }
    }

    pub fn warning(message: impl Into<String>) -> Self {
        Self {
            level: TypingDiagnosticLevel::Warning,
            message: message.into(),
            span: None,
        }
    }

    pub fn warning_with_span(message: impl Into<String>, span: Span) -> Self {
        Self {
            level: TypingDiagnosticLevel::Warning,
            message: message.into(),
            span: Some(span),
        }
    }
}

pub struct TypingOutcome {
    pub diagnostics: Vec<TypingDiagnostic>,
    pub has_errors: bool,
    pub resolved_names: ResolvedNameTable,
}

pub type ExprId = fp_core::ast::ExprId;

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
