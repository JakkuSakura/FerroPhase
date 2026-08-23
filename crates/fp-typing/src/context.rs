use std::cell::RefCell;
use std::collections::HashMap;

use fp_core::ast::Value;
use fp_core::diagnostics::DiagnosticManager;

use crate::types::GenericMonorph;
use crate::BoxFuture;
use fp_core::hir::PackageTypes;

pub struct ComptimeRequest {
    /// Every *already-published* package's own HIR (each shared as the
    /// same `Rc` its `CompiledPackage`/`WorkspaceContext` holds — see
    /// `WorkspaceContext::publish_hir_program`, which maintains this
    /// incrementally, one package at a time, as each finishes — never
    /// rebuilt/re-scanned on demand). `current` (below) is *not* in here
    /// yet — it's still being type-checked, not yet published — so the
    /// receiving `MirLowering` checks `current` first for any `DefId`
    /// naming it, and falls through to `program` for every other
    /// package's own `DefId`s. Replaces the old design of pre-merging
    /// every dependency's `def_map` into one pretend-single-package
    /// `Package` (or, worse, deep-cloning that merged result) per request.
    pub program: std::rc::Rc<fp_core::hir::Program>,
    /// This request's own package — same `Rc` `TypingShared::program`
    /// already is, so this is an `Rc` clone, not a deep clone.
    pub current: std::rc::Rc<fp_core::hir::Package>,
    pub typeck_results: PackageTypes,
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
/// interpretation) — supplied by `fp-compiler` at `TypingContext`
/// construction, since only the driver's `CompilerState` knows how to do
/// that; `fp-typing` only knows *when* a request is needed
/// (`request_comptime`), not how to answer it. Living behind this closure
/// (rather than a queue the driver polls) is what lets `request_comptime`
/// just `.await` the answer directly, instead of parking on a reply and
/// relying on driver-level code to notice and drain a side queue.
pub type ComptimeResolver = std::rc::Rc<dyn Fn(ComptimeRequest) -> BoxFuture<'static, fp_core::Result<Value>>>;

/// Shared mutable state between the compiler driver and the type inferencer.
///
/// Created once by the driver and reused across typing passes (initial + retry
/// after comptime evaluation).  `RefCell` interior mutability allows both the
/// driver and the typer to read/write without threading state through function
/// parameters.
///
/// Holds only typing-owned state — the compiled-package registry
/// (`WorkspaceContext`), target ABI data (`LirDataLayout`), and the shared
/// task pool (`ExecutorHandle`) all live on `fp-compiler`'s `CompilerState`
/// instead and get passed explicitly wherever typing needs them, since
/// `TypingContext` had no real abstraction over them (every caller reached
/// straight through the field) and `CompilerState` already owns the task
/// pool independently.
pub struct TypingContext {
    /// Accumulated typing diagnostics (warnings + errors) — includes both
    /// genuinely fatal item-check aborts and deliberately non-fatal,
    /// recovered mismatches (e.g. `require_same`'s isolated type
    /// mismatches, recorded via plain `record_error` specifically so one
    /// bad expression doesn't abort the whole item's check). Typer appends
    /// during inference; driver reads after each pass. Backed by the same
    /// `fp_core::diagnostics::DiagnosticManager` every other pipeline stage
    /// (frontend parsing, etc.) uses — one unified manager per package's
    /// typing session (`TypingContext` itself is reset per package by the
    /// driver), rather than a typing-specific type plus a second, separate
    /// manager. `record_item_check_failure`'s hard-abort diagnostics are
    /// tagged with `ITEM_CHECK_FAILURE_CODE` so `has_typing_errors` can
    /// still distinguish them from an isolated, already-recovered mismatch
    /// without a second manager.
    pub diagnostics: DiagnosticManager,

    /// Generic function calls whose concrete type arguments have been
    /// resolved and are ready for monomorphization, written the moment
    /// typing discovers each one (see `infer_generic_function_call_body`),
    /// keyed by the same string the trivial "ready to specialize" task is
    /// spawned under the compiler task pool. The task's only job is to make "this generic
    /// call is ready" show up through the shared task pool's normal
    /// resolve-and-dispatch loop; the actual payload the pool's `Result<()>`
    /// output can't carry lives here instead, read back out (and removed)
    /// by `CompilerDriver::handle_resolved_task` the moment that key
    /// resolves.
    pub ready_generics: RefCell<HashMap<String, GenericMonorph>>,

    /// Answers requests made by HIR while checking compile-time constants —
    /// see `ComptimeResolver`'s doc comment. `None` until `fp-compiler` wires
    /// one up via `with_comptime_resolver`; calling `request_comptime` before
    /// that is a caller bug (there is no compile-time value to hand back).
    comptime_resolver: RefCell<Option<ComptimeResolver>>,
}

impl TypingContext {
    pub fn new() -> Self {
        Self {
            diagnostics: DiagnosticManager::new(),
            ready_generics: RefCell::new(HashMap::new()),
            comptime_resolver: RefCell::new(None),
        }
    }

    /// Wires up how `request_comptime` answers a request — called once by
    /// `fp-compiler` right after constructing a package's `TypingContext`
    /// (it's the only side that can build a `ComptimeResolver`, since
    /// answering one requires `CompilerState`).
    pub fn with_comptime_resolver(self, resolver: ComptimeResolver) -> Self {
        *self.comptime_resolver.borrow_mut() = Some(resolver);
        self
    }

    /// Request a compile-time value — awaits `ComptimeResolver` directly, so
    /// the caller (an item's typecheck task) just suspends naturally until
    /// the answer is ready, with no manual queue-draining/polling by
    /// driver-level code required.
    pub async fn request_comptime(&self, request: ComptimeRequest) -> fp_core::Result<Value> {
        let resolver = self
            .comptime_resolver
            .borrow()
            .clone()
            .expect("TypingContext::request_comptime called before with_comptime_resolver");
        resolver(request).await
    }

    /// `tcx.sess.has_errors()`-style query: true once any item's check has
    /// hard-aborted (tagged `ITEM_CHECK_FAILURE_CODE`, see `diagnostics`'s
    /// doc comment) — the only category that leaves a real `PackageTypes`
    /// gap, and thus the only category safe to gate later stages on.
    pub fn has_typing_errors(&self) -> bool {
        self.diagnostics
            .get_diagnostics()
            .iter()
            .any(|diagnostic| diagnostic.code.as_deref() == Some(ITEM_CHECK_FAILURE_CODE))
    }
}

/// Tags a diagnostic as a hard item-check abort (see `record_item_check_failure`)
/// within `TypingContext::diagnostics`'s single unified manager.
pub const ITEM_CHECK_FAILURE_CODE: &str = "item-check-failure";

impl Default for TypingContext {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::future::Future;

    #[test]
    fn comptime_request_returns_resolver_value_directly() {
        let context = TypingContext::new().with_comptime_resolver(std::rc::Rc::new(|_request| {
            Box::pin(async { Ok(Value::unit()) })
        }));
        let request = ComptimeRequest {
            program: std::rc::Rc::new(fp_core::hir::Program::new()),
            current: std::rc::Rc::new(fp_core::hir::Package::new()),
            typeck_results: PackageTypes::default(),
            block: fp_core::hir::Block {
                hir_id: fp_core::hir::HirId::new(fp_core::hir::PackageId(0), 0),
                stmts: Vec::new(),
                expr: None,
            },
            expression_id: fp_core::hir::HirId::new(fp_core::hir::PackageId(0), 0),
            expected_ty: fp_core::hir::TypeExpr {
                hir_id: fp_core::hir::HirId::new(fp_core::hir::PackageId(0), 0),
                kind: fp_core::hir::TypeExprKind::Tuple(Vec::new()),
                span: fp_core::span::Span::null(),
            },
        };
        let mut future = Box::pin(context.request_comptime(request));
        let waker = std::task::Waker::noop();
        let mut cx = std::task::Context::from_waker(waker);
        let value = match future.as_mut().poll(&mut cx) {
            std::task::Poll::Ready(result) => result.expect("comptime value"),
            std::task::Poll::Pending => panic!("resolver-backed comptime request should resolve immediately"),
        };
        assert!(value.is_unit());
    }
}
