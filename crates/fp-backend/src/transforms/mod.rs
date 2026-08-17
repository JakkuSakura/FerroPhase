pub mod ast_to_hir;
pub mod hir_materialization;
pub mod hir_normalization;
pub mod hir_to_ast;
pub mod hir_to_mir;
pub mod mir_to_lir;

pub use ast_to_hir::*;
pub use hir_to_ast::*;
pub use hir_to_mir::*;
pub use mir_to_lir::*;

/// A call's callee is only ever a compiler intrinsic/portable op because
/// its *own resolved declaration* was tagged `#[intrinsic = "..."]`/
/// `#[op(func = "...")]`/`#[op(method = "...")]`/`#[op(variant = "...")]`
/// (`hir::Program::op_defs`/`intrinsic_defs`, populated once during
/// `ast_to_hir` lowering) — resolved here by the callee's real `DefId`,
/// never by re-deriving it from the call site's own name/path, which
/// can't tell a builtin/portable-op declaration apart from a same-named
/// real user function. Shared by every POST-typecheck consumer of this
/// identity — currently just `hir_normalization::normalize_program`, the
/// single pass that reclassifies `Call`/`Struct`/`Path`/`MethodCall` nodes
/// for both pipelines — so the lookup itself lives in exactly one place.
pub fn resolve_call_kind(
    op_defs: &std::collections::HashMap<fp_core::hir::DefId, fp_core::intrinsics::OpKind>,
    intrinsic_defs: &std::collections::HashMap<fp_core::hir::DefId, fp_core::intrinsics::CallKind>,
    def_id: fp_core::hir::DefId,
) -> Option<fp_core::intrinsics::CallKind> {
    if let Some(op) = op_defs.get(&def_id).copied() {
        return Some(fp_core::intrinsics::CallKind::Op(op));
    }
    intrinsic_defs.get(&def_id).copied()
}
