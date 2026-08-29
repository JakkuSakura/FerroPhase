pub mod ast_to_hir;
pub mod hir_materialization;
pub mod hir_to_ast;
pub mod hir_to_mir;
pub mod lir_to_mir;
pub mod mir_to_hir;
pub mod mir_to_lir;

pub use ast_to_hir::*;
pub use hir_to_ast::*;
pub use hir_to_mir::*;
pub use lir_to_mir::*;
pub use mir_to_hir::*;
pub use mir_to_lir::*;

/// A call's callee is only ever a compiler intrinsic/portable op because
/// its *own resolved declaration* was tagged `#[intrinsic = "..."]`/
/// `#[op(func = "...")]`/`#[op(method = "...")]`/`#[op(variant = "...")]`
/// (`hir::HirPackage::op_defs`/`intrinsic_defs`, populated once during
/// `ast_to_hir` lowering) — resolved here by the callee's real `DefId`,
/// never by re-deriving it from the call site's own name/path, which
/// can't tell a builtin/portable-op declaration apart from a same-named
/// real user function.
pub fn resolve_call_kind(
    _op_defs: &std::collections::HashMap<fp_core::hir::DefId, fp_core::intrinsics::PortableOp>,
    intrinsic_defs: &std::collections::HashMap<fp_core::hir::DefId, fp_core::intrinsics::CallKind>,
    def_id: fp_core::hir::DefId,
) -> Option<fp_core::intrinsics::CallKind> {
    // `CallKind::Op` was retired — portable ops are no longer promoted to a
    // shared, name-tagged `IntrinsicCall` here; target backends recognize
    // them directly (temporarily, by name) instead.
    intrinsic_defs.get(&def_id).cloned()
}

pub fn resolve_portable_op(
    op_defs: &std::collections::HashMap<fp_core::hir::DefId, fp_core::intrinsics::PortableOp>,
    def_id: &fp_core::hir::DefId,
) -> Option<fp_core::intrinsics::PortableOp> {
    op_defs.get(def_id).cloned()
}
