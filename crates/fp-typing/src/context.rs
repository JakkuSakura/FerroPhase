use std::cell::RefCell;
use std::collections::HashMap;
use std::task::Waker;

use fp_core::ast::{ExprResolutionTable, TypeStruct, Value};
use fp_core::lir::LirDataLayout;
use fp_core::workspace::WorkspaceContext;

use crate::types::GenericMonorph;
use crate::TypingDiagnostic;

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
}
