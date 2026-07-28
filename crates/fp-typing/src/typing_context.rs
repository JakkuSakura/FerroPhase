use std::cell::RefCell;
use std::collections::HashMap;

use fp_core::ast::{ExprResolutionTable, TypeStruct, Value};
use fp_core::module::resolution::ModuleResolutionContext;

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
}

impl Default for TypingContext {
    fn default() -> Self {
        Self::new()
    }
}
