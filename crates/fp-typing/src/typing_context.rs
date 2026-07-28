use std::cell::RefCell;
use std::collections::HashMap;

use fp_core::ast::{ExprResolutionTable, TypeStruct, Value};
use fp_core::module::path::QualifiedPath;
use fp_core::module::resolution::ModuleResolutionContext;

use crate::TypingDiagnostic;
use crate::AstTypeInferencer;

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

    /// Per-AST module resolution context (package graph, resolvers).
    /// Driver sets before each typing pass.
    pub module_resolution: RefCell<Option<ModuleResolutionContext>>,

    /// Expression resolution table: maps `ExprId` → source expression and
    /// optionally a pre-evaluated comptime value.
    pub expr_resolutions: RefCell<ExprResolutionTable>,

    /// Accumulated typing diagnostics (warnings + errors).
    /// Typer appends during inference; driver reads after each pass.
    pub diagnostics: RefCell<Vec<TypingDiagnostic>>,
}

impl TypingContext {
    pub fn new() -> Self {
        Self {
            resolved_consts: RefCell::new(HashMap::new()),
            resolved_types: RefCell::new(HashMap::new()),
            module_resolution: RefCell::new(None),
            expr_resolutions: RefCell::new(ExprResolutionTable::default()),
            diagnostics: RefCell::new(Vec::new()),
        }
    }

    /// Seed an inferencer's fields from the context. Called once before each
    /// `infer()` invocation.  The resolved consts and types stay in the
    /// context's `RefCell`s; the inferencer reads them on-the-fly during
    /// typing via `self.typing_ctx`.
    pub(crate) fn seed_inferencer<'ctx>(&self, inferencer: &mut AstTypeInferencer<'ctx>) {
        if let Some(ctx) = self.module_resolution.borrow().as_ref() {
            inferencer.seed_modules_from_resolution_context(ctx);
        }
        let types = std::mem::take(&mut *self.resolved_types.borrow_mut());
        for (name, struct_ty) in types {
            let path = QualifiedPath::new(vec![name.clone()]);
            inferencer.struct_defs.insert(path, struct_ty);
        }
    }
}

impl Default for TypingContext {
    fn default() -> Self {
        Self::new()
    }
}
