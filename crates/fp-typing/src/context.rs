use fp_core::ast::Value;

use crate::BoxFuture;
use fp_core::hir::PackageTypes;

pub struct ComptimeRequest {
    /// Every *already-published* package's own HIR (each shared as the
    /// same `Rc` its `CompiledPackage`/`AstProgram` holds — see
    /// `AstProgram::publish_hir_program`, which maintains this
    /// incrementally, one package at a time, as each finishes — never
    /// rebuilt/re-scanned on demand). `current` (below) is *not* in here
    /// yet — it's still being type-checked, not yet published — so the
    /// receiving `MirLowering` checks `current` first for any `DefId`
    /// naming it, and falls through to `program` for every other
    /// package's own `DefId`s. Replaces the old design of pre-merging
    /// every dependency's `def_map` into one pretend-single-package
    /// `HirPackage` (or, worse, deep-cloning that merged result) per request.
    pub program: std::rc::Rc<fp_core::hir::HirProgram>,
    /// This request's own package — same `Rc` `TypingShared::program`
    /// already is, so this is an `Rc` clone, not a deep clone.
    pub current: std::rc::Rc<fp_core::hir::HirPackage>,
    pub typeck_results: PackageTypes,
    /// This comptime unit's own identity — a `const { .. }` block's or
    /// `const fn`'s `DefId` (see `hir::ExprConstBlock::def_id`), which
    /// already carries its owning package (`DefId.package_id`). Comptime
    /// resolution is never package-level (`fp-typing`'s own
    /// `spawn_comptime_task` already dedups by this same `DefId`) — the
    /// driver reads this directly instead of falling back to the
    /// workspace's mutable, ambient `current_package()`.
    pub def_id: fp_core::hir::DefId,
    /// The exact HIR block encountered by the type checker. The driver may
    /// provide a backend entrypoint for it, but must not reconstruct the
    /// block through a synthetic const or a definition lookup.
    pub block: fp_core::hir::Block,
    /// HIR identity of the original const-block expression. Results are
    /// associated with this identity by the caller that requested evaluation.
    pub expression_id: fp_core::hir::HirId,
    pub expected_ty: fp_core::hir::TypeExpr,
}

/// Resolves one comptime request end-to-end (HIR->MIR->LIR lowering plus
/// interpretation) — supplied by `fp-compiler` at `TypingShared`
/// construction, since only the driver's `CompilerState` knows how to do
/// that; `fp-typing` only knows *when* a request is needed
/// (`request_comptime`), not how to answer it. Living behind this closure
/// (rather than a queue the driver polls) is what lets `request_comptime`
/// just `.await` the answer directly, instead of parking on a reply and
/// relying on driver-level code to notice and drain a side queue.
pub type ComptimeResolver = std::rc::Rc<dyn Fn(ComptimeRequest) -> BoxFuture<'static, fp_core::Result<Value>>>;

/// Tags a diagnostic as a hard item-check abort (see `record_item_check_failure`)
/// within `TypingShared::diagnostics`'s single unified manager.
pub const ITEM_CHECK_FAILURE_CODE: &str = "item-check-failure";
