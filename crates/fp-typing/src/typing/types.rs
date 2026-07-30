use fp_core::ast::{Expr, Ty};
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
    pub resolved_names: ResolvedNameTable,
    /// Pending work discovered only after the full typing pass finishes.
    pub pending_requests: Vec<PendingTypingRequest>,
    /// Generic instantiations with resolved concrete types ready for monomorphization.
    pub pending_generics: Vec<GenericMonorph>,
    /// Structs resolved from a workspace crate rather than the local one
    /// (e.g. `std::meta::TypeBuilder`, via `TypeBuilder::new(...)`).
    pub cross_crate_struct_refs: Vec<QualifiedPath>,
}

/// A generic function invocation whose concrete type arguments have been resolved
/// and are ready for monomorphization (specialization).
#[derive(Debug, Clone)]
pub struct GenericMonorph {
    /// Qualified path of the generic function being called
    pub function_path: QualifiedPath,
    /// Names of the generic parameters (in definition order)
    pub generic_params: Vec<String>,
    /// Resolved concrete types for each generic parameter (in same order)
    pub concrete_types: Vec<Ty>,
}

impl GenericMonorph {
    pub fn new(function_path: QualifiedPath, generic_params: Vec<String>, concrete_types: Vec<Ty>) -> Self {
        Self { function_path, generic_params, concrete_types }
    }
}

#[derive(Clone, Debug, PartialEq)]
/// Work discovered by a completed typing pass.
pub struct PendingTypingRequest {
    pub kind: PendingTypingRequestKind,
    pub expr: Expr,
}

impl PendingTypingRequest {
    pub fn unknown_type(expr: Expr) -> Self {
        Self {
            kind: PendingTypingRequestKind::Unresolved,
            expr,
        }
    }

    pub fn generic(expr: Expr) -> Self {
        Self {
            kind: PendingTypingRequestKind::Generic,
            expr,
        }
    }

    pub fn comptime(expr: Expr) -> Self {
        Self {
            kind: PendingTypingRequestKind::Comptime,
            expr,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PendingTypingRequestKind {
    Unresolved,
    Generic,
    Comptime,
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
