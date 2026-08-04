use std::cell::RefCell;
use std::collections::{HashMap, VecDeque};
use std::task::Waker;
use std::task::Poll;

use fp_core::ast::{ExprResolutionTable, TypeStruct, Value};
use fp_core::lir::LirDataLayout;
use fp_core::workspace::WorkspaceContext;

use crate::TypingDiagnostic;
use crate::types::{GenericMonorph, TypeckResults};

pub struct ComptimeRequest {
    pub program: fp_core::hir::Program,
    pub typeck_results: TypeckResults,
    pub target: fp_core::hir::DefId,
    pub expected_ty: fp_core::hir::TypeExpr,
}

pub struct PendingComptimeRequest {
    pub request: ComptimeRequest,
    reply: std::rc::Rc<RefCell<ComptimeReply>>,
}

struct ComptimeReply {
    result: Option<fp_core::Result<Value>>,
    wakers: Vec<Waker>,
}

impl PendingComptimeRequest {
    pub fn request(&self) -> &ComptimeRequest {
        &self.request
    }

    pub fn complete(self, result: fp_core::Result<Value>) {
        let mut reply = self.reply.borrow_mut();
        reply.result = Some(result);
        let wakers = std::mem::take(&mut reply.wakers);
        drop(reply);
        for waker in wakers {
            waker.wake();
        }
    }
}

/// Shared mutable state between the compiler driver and the type inferencer.
///
/// Created once by the driver and reused across typing passes (initial + retry
/// after comptime evaluation).  `RefCell` interior mutability allows both the
/// driver and the typer to read/write without threading state through function
/// parameters.
pub struct TypingContext {
    /// Target ABI data shared by typing-triggered comptime probes and normal
    /// MIR-to-LIR lowering for this compilation session.
    pub data_layout: LirDataLayout,
    /// Comptime-evaluated const values, keyed by const name.
    /// Driver writes after each comptime pass; typer reads on next pass.
    pub resolved_consts: RefCell<HashMap<String, Value>>,

    /// Struct type definitions resolved via comptime evaluation.
    /// Driver writes after comptime pass; typer merges into `struct_defs`.
    pub resolved_types: RefCell<HashMap<String, TypeStruct>>,

    /// Compiled dependency crates in topological order.
    /// The typer queries this for fully-qualified symbol lookups.
    pub env_ctx: std::rc::Rc<WorkspaceContext>,

    /// Expression resolution table: maps `ExprId` → source expression and
    /// optionally a pre-evaluated comptime value.
    pub expr_resolutions: RefCell<ExprResolutionTable>,

    /// Accumulated typing diagnostics (warnings + errors).
    /// Typer appends during inference; driver reads after each pass.
    pub diagnostics: RefCell<Vec<TypingDiagnostic>>,

    /// Wakers of typing tasks currently suspended on a package (keyed by
    /// package name) not yet loaded by the compiler's HIR pipeline.
    /// Drained by the driver once it finishes loading that package.
    pub package_wakers: RefCell<HashMap<String, Vec<Waker>>>,

    /// Wakers of typing tasks currently suspended on a comptime value (keyed
    /// by const/type-alias name) not yet resolved — see
    /// comptime resolution. Precisely
    /// (not broadcast) woken by whichever write site
    /// (`resolved_consts`/`resolved_types`) actually resolves that name.
    pub comptime_wakers: RefCell<HashMap<String, Vec<Waker>>>,

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

    /// Requests made by HIR while checking compile-time constants. The
    /// driver drains this queue and completes each request; the result is
    /// delivered to the awaiting checker rather than read from a cache.
    comptime_requests: RefCell<VecDeque<PendingComptimeRequest>>,
}

impl TypingContext {
    pub fn new(data_layout: LirDataLayout, env_ctx: std::rc::Rc<WorkspaceContext>) -> Self {
        Self {
            data_layout,
            resolved_consts: RefCell::new(HashMap::new()),
            resolved_types: RefCell::new(HashMap::new()),
            env_ctx,
            expr_resolutions: RefCell::new(ExprResolutionTable::default()),
            diagnostics: RefCell::new(Vec::new()),
            package_wakers: RefCell::new(HashMap::new()),
            comptime_wakers: RefCell::new(HashMap::new()),
            ready_generics: RefCell::new(HashMap::new()),
            comptime_requests: RefCell::new(VecDeque::new()),
        }
    }

    /// Wake every task parked on `name`'s package load — call this right
    /// after `name` finishes loading (`CompilerDriver::load_package`).
    /// Mirrors `wake_comptime` exactly. Without this, any task whose
    /// suspension registered a *real*
    /// pool waker under this name — not just the top-level module task, any
    /// nested one too — would never be re-enqueued onto the pool's ready
    /// queue and would park forever.
    pub fn wake_package(&self, name: &str) {
        let wakers = self.package_wakers.borrow_mut().remove(name);
        if let Some(wakers) = wakers {
            for waker in wakers {
                waker.wake();
            }
        }
    }

    /// Await a provider-owned package key. Package discovery and loading stay
    /// outside typing; the compiler driver observes the registered waker and
    /// services the corresponding `LoadPackage` request.
    pub async fn await_package(
        &self,
        key: &str,
    ) -> fp_core::Result<fp_core::package::PackageId> {
        let key = key.to_owned();
        std::future::poll_fn(|cx| {
            let Some(package_id) = self.env_ctx.resolve_package(&key) else {
                return Poll::Ready(Err(fp_core::Error::from(format!(
                    "unresolved package `{key}`"
                ))));
            };
            if self.env_ctx.is_loaded(package_id.as_str()) {
                return Poll::Ready(Ok(package_id));
            }
            self.package_wakers
                .borrow_mut()
                .entry(key.clone())
                .or_default()
                .push(cx.waker().clone());
            Poll::Pending
        })
        .await
    }

    /// Wake every task parked on `name`'s comptime value — call this right
    /// after writing `name`'s resolution into `resolved_consts`/
    /// `resolved_types` (the three write sites are all in `fp-compiler`'s
    /// driver). Precise, not broadcast: only tasks registered under this
    /// exact name are woken.
    pub fn wake_comptime(&self, name: &str) {
        // `.remove(name)` is extracted into its own statement (rather than
        // used directly as an `if let` scrutinee) so the `borrow_mut()`
        // guard drops before `waker.wake()` runs -- an `if let` scrutinee's
        // temporaries are otherwise kept alive for the whole `if let`, which
        // would hold this guard across every woken task's `wake()` call.
        let wakers = self.comptime_wakers.borrow_mut().remove(name);
        if let Some(wakers) = wakers {
            for waker in wakers {
                waker.wake();
            }
        }
    }

    /// Request a compile-time value. The first request for a key is exposed
    /// to the compiler driver; subsequent awaiters share the driver's answer.
    pub async fn request_comptime(&self, request: ComptimeRequest) -> fp_core::Result<Value> {
        let reply = std::rc::Rc::new(RefCell::new(ComptimeReply {
            result: None,
            wakers: Vec::new(),
        }));
        let mut request = Some(request);
        let reply_for_poll = reply.clone();
        std::future::poll_fn(|cx| {
            if let Some(result) = reply_for_poll.borrow_mut().result.take() {
                return Poll::Ready(result);
            }
            if let Some(request) = request.take() {
                self.comptime_requests.borrow_mut().push_back(PendingComptimeRequest {
                    request,
                    reply: reply_for_poll.clone(),
                });
            }
            reply_for_poll.borrow_mut().wakers.push(cx.waker().clone());
            Poll::Pending
        })
        .await
    }

    pub fn take_comptime_requests(&self) -> Vec<PendingComptimeRequest> {
        self.comptime_requests.borrow_mut().drain(..).collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use fp_core::workspace::WorkspaceContext;
    use std::future::Future;

    #[test]
    fn comptime_request_returns_driver_value_directly() {
        let context = TypingContext::new(
            LirDataLayout {
                pointer_size_bits: 64,
                pointer_alignment: 8,
                integer_alignments: vec![(8, 1), (16, 2), (32, 4), (64, 8), (128, 16)],
            },
            std::rc::Rc::new(WorkspaceContext::new()),
        );
        let request = ComptimeRequest {
            program: fp_core::hir::Program::new(),
            typeck_results: TypeckResults::default(),
            target: fp_core::hir::DefId::local(0),
            expected_ty: fp_core::hir::TypeExpr {
                hir_id: 0,
                kind: fp_core::hir::TypeExprKind::Tuple(Vec::new()),
                span: fp_core::span::Span::null(),
            },
        };
        let mut future = Box::pin(context.request_comptime(request));
        let waker = std::task::Waker::noop();
        let mut cx = std::task::Context::from_waker(waker);
        assert!(matches!(future.as_mut().poll(&mut cx), Poll::Pending));
        let pending = context
            .take_comptime_requests()
            .into_iter()
            .next()
            .expect("comptime request");
        pending.complete(Ok(Value::unit()));
        let value = match future.as_mut().poll(&mut cx) {
            Poll::Ready(result) => result.expect("comptime value"),
            Poll::Pending => panic!("completed comptime request remained pending"),
        };
        assert!(value.is_unit());
    }
}
