use crate::span::Span;
use eyre::Error as EyreError;
use std::result;
use thiserror::Error;

#[derive(Debug)]
pub struct SyntaxError {}

#[derive(Debug)]
pub struct OptimizationError {
    pub message: String,
    pub code: Option<String>,
}

impl std::fmt::Display for OptimizationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(code) = &self.code {
            write!(f, "{} (code: {})", self.message, code)
        } else {
            write!(f, "{}", self.message)
        }
    }
}

#[derive(Error, Debug)]
pub enum Error {
    #[error("Syntax error: {0:?}")]
    SyntaxError(Span, SyntaxError),
    #[error("Optimization error: {1}")]
    OptimizationError(Span, OptimizationError),
    #[error("Runtime error: {0}")]
    RuntimeError(crate::ast::RuntimeError),
    #[error("Generic error: {0}")]
    Generic(EyreError),
}

pub type Result<T> = result::Result<T, Error>;

// Convert from eyre::Error to our Error type
impl From<EyreError> for Error {
    fn from(err: EyreError) -> Self {
        Error::Generic(err)
    }
}

// Convert from std::io::Error to our Error type
impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Self {
        Error::Generic(e.into())
    }
}
impl From<String> for Error {
    fn from(s: String) -> Self {
        Error::Generic(EyreError::msg(s))
    }
}
impl From<&str> for Error {
    fn from(s: &str) -> Self {
        Error::Generic(EyreError::msg(s.to_string()))
    }
}
impl From<serde_json::Error> for Error {
    fn from(e: serde_json::Error) -> Self {
        Error::Generic(EyreError::new(e))
    }
}
impl From<crate::ast::RuntimeError> for Error {
    fn from(e: crate::ast::RuntimeError) -> Self {
        Error::RuntimeError(e)
    }
}

// ========== New Error Tolerance Infrastructure ==========

/// Error that occurred during transformation (for error tolerance)
#[derive(Debug, Clone)]
pub struct TransformationError {
    pub kind: TransformationErrorKind,
    pub stage: TransformationStage,
    pub message: String,
    pub span: Option<Span>,
    pub suggestions: Vec<String>,
}

/// Warning that occurred during transformation
#[derive(Debug, Clone)]
pub struct TransformationWarning {
    pub kind: TransformationWarningKind,
    pub stage: TransformationStage,
    pub message: String,
    pub span: Option<Span>,
}

#[derive(Debug, Clone, Copy)]
pub enum TransformationErrorKind {
    UnresolvedSymbol,
    DuplicateSymbol,
    TypeMismatch,
    InvalidSyntax,
    ImportError,
    BorrowError,
    ConstError,
}

#[derive(Debug, Clone, Copy)]
pub enum TransformationWarningKind {
    UnusedImport,
    UnusedVariable,
    UnusedFunction,
    DeprecatedSyntax,
    StylisticIssue,
    PerformanceHint,
}

#[derive(Debug, Clone, Copy)]
pub enum TransformationStage {
    Parse,
    ConstEval,
    AstToHir,
    HirToThir,
    ThirToMir,
    MirToLir,
    LirToLlvm,
}

impl TransformationError {
    pub fn new(kind: TransformationErrorKind, stage: TransformationStage, message: String) -> Self {
        Self {
            kind,
            stage,
            message,
            span: None,
            suggestions: Vec::new(),
        }
    }

    pub fn with_span(mut self, span: Span) -> Self {
        self.span = Some(span);
        self
    }

    pub fn with_suggestion(mut self, suggestion: String) -> Self {
        self.suggestions.push(suggestion);
        self
    }

    pub fn with_suggestions(mut self, suggestions: Vec<String>) -> Self {
        self.suggestions.extend(suggestions);
        self
    }
}

impl TransformationWarning {
    pub fn new(
        kind: TransformationWarningKind,
        stage: TransformationStage,
        message: String,
    ) -> Self {
        Self {
            kind,
            stage,
            message,
            span: None,
        }
    }

    pub fn with_span(mut self, span: Span) -> Self {
        self.span = Some(span);
        self
    }
}

impl std::fmt::Display for TransformationStage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TransformationStage::Parse => write!(f, "parse"),
            TransformationStage::ConstEval => write!(f, "const-eval"),
            TransformationStage::AstToHir => write!(f, "ast-to-hir"),
            TransformationStage::HirToThir => write!(f, "hir-to-thir"),
            TransformationStage::ThirToMir => write!(f, "thir-to-mir"),
            TransformationStage::MirToLir => write!(f, "mir-to-lir"),
            TransformationStage::LirToLlvm => write!(f, "lir-to-llvm"),
        }
    }
}
