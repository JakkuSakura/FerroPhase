use fp_core::ast::Ty;
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
    /// Generic instantiations with resolved concrete types ready for monomorphization.
    pub pending_generics: Vec<GenericMonorph>,
    /// Structs resolved from a workspace crate rather than the local one
    /// (e.g. `std::meta::TypeBuilder`, via `TypeBuilder::new(...)`).
    pub cross_crate_struct_refs: Vec<QualifiedPath>,
}

pub type ItemId = fp_core::ast::ItemId;

/// A generic function invocation whose concrete type arguments have been resolved
/// and are ready for monomorphization (specialization).
#[derive(Debug, Clone)]
pub struct GenericMonorph {
    /// Stable identity of the `ItemDefFunction` node being specialized (see
    /// `fp_core::ast::ItemId`'s doc comment) -- this, not `function_path`, is
    /// what a later pass (`CompilerDriver::enqueue_generic`) uses to find the
    /// function again in the stored typed AST. `function_path` is a
    /// qualification convention (prefixed by whatever module/compile-unit
    /// context was active when the signature was registered) and doesn't
    /// generally correspond to real nested-module structure in that AST, so
    /// it's kept only for the specialized function's display name and the
    /// dedup key, not for re-locating the original definition.
    pub item_id: ItemId,
    /// Qualified path of the generic function being called
    pub function_path: QualifiedPath,
    /// Names of the generic parameters (in definition order)
    pub generic_params: Vec<String>,
    /// Resolved concrete types for each generic parameter (in same order)
    pub concrete_types: Vec<Ty>,
}

impl GenericMonorph {
    pub fn new(
        item_id: ItemId,
        function_path: QualifiedPath,
        generic_params: Vec<String>,
        concrete_types: Vec<Ty>,
    ) -> Self {
        Self { item_id, function_path, generic_params, concrete_types }
    }
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
