use super::*;

// Placeholder for future statement-specific evaluation.
// Current implementation evaluates statements as part of eval_block.
impl<'ctx> AstInterpreter<'ctx> {
    #[allow(dead_code)]
    pub(super) fn eval_stmt(&mut self, _stmt: &mut BlockStmt) -> Value {
        Value::unit()
    }
}

