mod support;

pub mod context;
pub mod hir_typeck;
pub mod types;

pub use context::TypingContext;
pub use hir_typeck::HirTypeChecker;
pub use support::{block_on, default_extern_prelude, impl_self_ty_name};
pub use types::{
    ExprId, GenericMonorph, ResolvedName, ResolvedNameNamespace, ResolvedNameTable,
    TypeckResults, TypingDiagnostic, TypingDiagnosticLevel, TypingOutcome,
};
