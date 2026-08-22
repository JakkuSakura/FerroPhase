//! Minimal `AstSerializer` for the Lean frontend — every method defaults
//! to `bail!` in the trait; nothing in the v1 pipeline calls back into a
//! Lean-specific serialize path, so there is nothing to override yet.

use fp_core::ast::AstSerializer;

#[derive(Debug, Default, Clone)]
pub struct LeanSerializer;

impl AstSerializer for LeanSerializer {}
