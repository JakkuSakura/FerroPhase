mod support;

pub mod context;
pub mod hir_typeck;
pub mod types;

pub use context::{ComptimeRequest, PendingComptimeRequest, TypingContext};
pub use hir_typeck::HirTypeChecker;
#[cfg(test)]
pub(crate) use support::block_on;
pub use support::{default_extern_prelude, impl_self_ty_name, BoxFuture};
pub use types::{
    ExprId, GenericMonorph, ResolvedName, ResolvedNameNamespace, ResolvedNameTable, TypeckResults,
    TypingDiagnostic, TypingDiagnosticLevel, TypingOutcome,
};
