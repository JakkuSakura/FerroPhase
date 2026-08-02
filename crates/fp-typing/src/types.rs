use fp_core::ast::Ty;
use fp_core::hir;
use fp_core::hir::ty::Ty as HirTy;
use fp_core::hir::HirId;
use fp_core::module::path::QualifiedPath;
use fp_core::span::Span;
use std::collections::HashMap;

/// Semantic information produced by HIR type checking. HIR itself remains a
/// source-shaped tree; inferred types and resolutions are keyed by HIR node.
#[derive(Debug, Clone, Default)]
pub struct TypeckResults {
    pub expr_types: HashMap<HirId, HirTy>,
    pub type_expr_types: HashMap<HirId, HirTy>,
    pub pat_types: HashMap<HirId, HirTy>,
    pub resolutions: HashMap<HirId, hir::Res>,
    pub method_resolutions: HashMap<HirId, hir::DefId>,
    pub generic_call_args: HashMap<HirId, GenericCallResolution>,
}

#[derive(Debug, Clone)]
pub struct GenericCallResolution {
    pub def_id: hir::DefId,
    pub args: Vec<HirTy>,
}

impl TypeckResults {
    pub fn record_expr_type(&mut self, id: HirId, ty: HirTy) {
        self.expr_types.insert(id, ty);
    }

    pub fn record_type_expr_type(&mut self, id: HirId, ty: HirTy) {
        self.type_expr_types.insert(id, ty);
    }

    pub fn record_pat_type(&mut self, id: HirId, ty: HirTy) {
        self.pat_types.insert(id, ty);
    }
}

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
    /// Structs resolved from a workspace crate rather than the local one
    /// (e.g. `std::meta::TypeBuilder`, via `TypeBuilder::new(...)`).
    pub cross_crate_struct_refs: Vec<QualifiedPath>,
}

pub type ItemId = fp_core::ast::ItemId;

/// A generic function invocation whose concrete type arguments have been
/// resolved and are ready for monomorphization (specialization). Written
/// directly into the shared `TypingContext::ready_generics` the moment
/// `infer_generic_function_call_body` resolves one -- not accumulated on
/// the typer's own per-pass state and returned via `TypingOutcome` once
/// the whole compile unit finishes, so the driver can act on it
/// immediately (see `CompilerDriver::handle_resolved_task`).
#[derive(Debug, Clone)]
pub struct GenericMonorph {
    /// Stable identity of the `ItemDefFunction` node being specialized (see
    /// `fp_core::ast::ItemId`'s doc comment) -- this, not `function_path`, is
    /// what `handle_resolved_task` uses to find the function again in the
    /// compile unit's own pre-typing stored AST. `function_path` is a
    /// qualification convention (prefixed by whatever module/compile-unit
    /// context was active when the signature was registered) and doesn't
    /// generally correspond to real nested-module structure in that AST, so
    /// it's kept only for the specialized function's display name and the
    /// dedup key, not for re-locating the original definition.
    pub item_id: ItemId,
    /// The discovering compile unit's own `AstId` (as a plain string --
    /// fp-typing can't name `fp-compiler`'s `AstId` type), carried verbatim
    /// from the discovering compile unit rather than re-derived later from
    /// `function_path`/a naming convention: `handle_resolved_task` runs
    /// once the pool is fully drained, with no compile-unit-specific
    /// context of its own, so this is the only way it knows which stored
    /// `File` to search for `item_id`.
    pub ast_key: String,
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
        ast_key: String,
        function_path: QualifiedPath,
        generic_params: Vec<String>,
        concrete_types: Vec<Ty>,
    ) -> Self {
        Self {
            item_id,
            ast_key,
            function_path,
            generic_params,
            concrete_types,
        }
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
