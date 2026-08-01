use std::cell::RefCell;
use std::collections::HashMap;
use std::task::Waker;

use fp_core::ast::{ExprResolutionTable, TypeStruct, Value};
use fp_core::workspace::WorkspaceContext;

use crate::typing::types::GenericMonorph;
use crate::TypingDiagnostic;

/// Shared mutable state between the compiler driver and the type inferencer.
///
/// Created once by the driver and reused across typing passes (initial + retry
/// after comptime evaluation).  `RefCell` interior mutability allows both the
/// driver and the typer to read/write without threading state through function
/// parameters.
pub struct TypingContext {
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
    /// package name) not yet loaded — see `AstTypeInferencer::await_package`.
    /// Drained by the driver once it finishes loading that package.
    pub package_wakers: RefCell<HashMap<String, Vec<Waker>>>,

    /// Wakers of typing tasks currently suspended on a comptime value (keyed
    /// by const/type-alias name) not yet resolved — see
    /// `AstTypeInferencer::await_comptime`/`await_struct_alias`. Precisely
    /// (not broadcast) woken by whichever write site
    /// (`resolved_consts`/`resolved_types`) actually resolves that name.
    pub comptime_wakers: RefCell<HashMap<String, Vec<Waker>>>,

    /// The one shared task executor concurrent item-resolution (one task
    /// per const/type-alias item, spawned during `predeclare_item`) and the
    /// per-compile-unit driver loop both spawn into/poll. `Executor` is
    /// already internally interior-mutable (its own methods take `&self`,
    /// specifically so a task can reentrantly `spawn`/`contains`-check this
    /// same executor from within its own poll) — wrapping it in another
    /// outer `RefCell` here would reintroduce exactly the double-borrow
    /// hazard that design avoids, so it's a plain field.
    pub tasks: fp_core::executor::Executor<fp_core::error::Result<()>>,

    /// Generic function calls whose concrete type arguments have been
    /// resolved and are ready for monomorphization, written the moment
    /// typing discovers each one (see `infer_generic_function_call_body`)
    /// -- shared driver-visible state, like `resolved_consts`/
    /// `package_wakers`, not a per-typer-pass `Vec` threaded back only once
    /// typing finishes. The driver drains this continuously from within
    /// `drive_typing_to_completion`'s poll loop, the same way it already
    /// services `package_wakers` there, rather than waiting for the whole
    /// compile unit to finish and processing it as a separate batch.
    pub pending_generics: RefCell<Vec<GenericMonorph>>,
}

impl TypingContext {
    pub fn new(env_ctx: std::rc::Rc<WorkspaceContext>) -> Self {
        Self {
            resolved_consts: RefCell::new(HashMap::new()),
            resolved_types: RefCell::new(HashMap::new()),
            env_ctx,
            expr_resolutions: RefCell::new(ExprResolutionTable::default()),
            diagnostics: RefCell::new(Vec::new()),
            package_wakers: RefCell::new(HashMap::new()),
            comptime_wakers: RefCell::new(HashMap::new()),
            tasks: fp_core::executor::Executor::new(),
            pending_generics: RefCell::new(Vec::new()),
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
