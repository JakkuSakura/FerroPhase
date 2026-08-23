use fp_core::ast::Value;

use crate::BoxFuture;

pub struct ComptimeRequest {
    /// This request's own package — the driver resolves everything else
    /// (HIR, typed results, the const block itself) by looking `def_id` up
    /// against its shared `CompilerState` instead of the request carrying
    /// its own snapshot of any of that.
    pub package_id: fp_core::hir::PackageId,
    /// This comptime unit's own identity — a `const { .. }` block's or
    /// `const fn`'s `DefId` (see `hir::ExprConstBlock::def_id`). Comptime
    /// resolution is never package-level (`fp-typing`'s own
    /// `spawn_comptime_task` already dedups by this same `DefId`) — the
    /// driver reads this directly instead of falling back to the
    /// workspace's mutable, ambient `current_package()`.
    pub def_id: fp_core::hir::DefId,
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
