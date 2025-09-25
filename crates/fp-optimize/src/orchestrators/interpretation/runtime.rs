use super::*;

impl InterpretationOrchestrator {
    pub fn interpret_expr_runtime(
        &self,
        expr: &Expr,
        ctx: &SharedScopedContext,
    ) -> Result<RuntimeValue> {
        match expr {
            Expr::Locator(Locator::Ident(ident)) => {
                // Try to get runtime value first
                if let Some(runtime_value) = ctx.get_runtime_value_recursive_path(ident) {
                    Ok(runtime_value)
                } else {
                    // Fall back to literal interpretation
                    let literal = self.interpret_ident(ident, ctx, true)?;
                    Ok(self.runtime_pass.create_runtime_value(literal))
                }
            }
            Expr::Select(select) => {
                let obj = self.interpret_expr_runtime(&select.obj.get(), ctx)?;
                self.runtime_pass
                    .access_field(obj, &select.field.name)
                    .map_err(|e| optimization_error(format!("Field access failed: {}", e)))
            }
            Expr::Invoke(invoke) => self.interpret_invoke_runtime(invoke, ctx),
            _ => {
                // For other expressions, interpret as literal then wrap
                let literal = self.interpret_expr(expr, ctx)?;
                Ok(self.runtime_pass.create_runtime_value(literal))
            }
        }
    }

    /// Runtime-aware method invocation

    pub fn interpret_invoke_runtime(
        &self,
        node: &ExprInvoke,
        ctx: &SharedScopedContext,
    ) -> Result<RuntimeValue> {
        match &node.target {
            ExprInvokeTarget::Method(select) => {
                // Method call with runtime semantics
                let obj = self.interpret_expr_runtime(&select.obj.get(), ctx)?;
                let args: Vec<RuntimeValue> = node
                    .args
                    .iter()
                    .map(|arg| self.interpret_expr_runtime(arg, ctx))
                    .try_collect()?;

                self.runtime_pass
                    .call_method(obj, &select.field.name, args)
                    .map_err(|e| optimization_error(format!("Method call failed: {}", e)))
            }
            _ => {
                // Fall back to regular interpretation for non-method calls
                let result = self.interpret_invoke(node, ctx)?;
                Ok(self.runtime_pass.create_runtime_value(result))
            }
        }
    }

    /// Runtime-aware assignment

    pub fn assign_runtime(
        &self,
        target: &Expr,
        value: RuntimeValue,
        ctx: &SharedScopedContext,
    ) -> Result<()> {
        self.runtime_pass
            .assign(target, value, ctx)
            .map_err(|e| optimization_error(format!("Assignment failed: {}", e)))
    }
}
