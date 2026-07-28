use std::cell::RefCell;
use std::collections::HashMap;

use fp_core::ast::{ExprResolutionTable, Item, TypeStruct, Value};
use fp_core::module::path::QualifiedPath;
use fp_core::package::graph::PackageGraph;

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

    /// Package graph containing module topology and pre-parsed std items.
    /// Set once by the CLI driver; the typer reads it for lazy module loading.
    pub package_graph: RefCell<Option<std::rc::Rc<PackageGraph>>>,

    /// Pre-parsed std-library module items, keyed by module path.
    /// Populated by the CLI before typing. The typer loads these
    /// lazily via `ensure_module_loaded`.
    pub std_items: RefCell<HashMap<QualifiedPath, Vec<Item>>>,

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
            package_graph: RefCell::new(None),
            std_items: RefCell::new(HashMap::new()),
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
