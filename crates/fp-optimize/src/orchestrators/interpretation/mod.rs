mod builtins;
mod expressions;
mod invoke;
mod items;
mod runtime;
mod typing;

use crate::utils::{FoldOptimizer, OptimizePass};
// Replace common::* with specific imports
use fp_core::ast::*;
use fp_core::context::SharedScopedContext;
use fp_core::ctx::{Context, ValueSystem};
use fp_core::diagnostics::DiagnosticManager;
use fp_core::error::Result;
use fp_core::id::{Ident, Locator};
use fp_core::ops::*;
use fp_core::passes::{LiteralRuntimePass, RuntimePass};
use fp_core::utils::conv::TryConv;
use itertools::Itertools;
use std::sync::Arc;
use tracing::debug;
// use std::any::Any;

// Import our error helpers
use crate::error::optimization_error;
use crate::opt_bail;
use crate::opt_ensure;

#[derive(Clone)]
pub struct InterpretationOrchestrator {
    pub serializer: Arc<dyn AstSerializer>,
    pub ignore_missing_items: bool,
    pub runtime_pass: Arc<dyn RuntimePass>,
    pub diagnostic_manager: Option<Arc<DiagnosticManager>>,
    pub abort_on_first_error: bool,
}

impl InterpretationOrchestrator {
    pub fn new(serializer: Arc<dyn AstSerializer>) -> Self {
        Self {
            serializer,
            ignore_missing_items: false,
            runtime_pass: Arc::new(LiteralRuntimePass::default()),
            diagnostic_manager: None,
            abort_on_first_error: true, // Default to current behavior
        }
    }

    pub fn with_runtime_pass(mut self, runtime_pass: Arc<dyn RuntimePass>) -> Self {
        self.runtime_pass = runtime_pass;
        self
    }

    pub fn set_runtime_pass(&mut self, runtime_pass: Arc<dyn RuntimePass>) {
        self.runtime_pass = runtime_pass;
    }

    pub fn with_diagnostic_manager(mut self, diagnostic_manager: Arc<DiagnosticManager>) -> Self {
        self.diagnostic_manager = Some(diagnostic_manager);
        self
    }

    pub fn set_diagnostic_manager(&mut self, diagnostic_manager: Option<Arc<DiagnosticManager>>) {
        self.diagnostic_manager = diagnostic_manager;
    }

    pub fn with_error_tolerance(mut self, abort_on_first_error: bool) -> Self {
        self.abort_on_first_error = abort_on_first_error;
        self
    }

    pub fn set_error_tolerance(&mut self, abort_on_first_error: bool) {
        self.abort_on_first_error = abort_on_first_error;
    }

    // Evaluation and runtime helpers live in submodules.
}

impl OptimizePass for InterpretationOrchestrator {
    fn name(&self) -> &str {
        "interpretation"
    }
    fn optimize_expr(&self, expr: Expr, ctx: &SharedScopedContext) -> Result<Expr> {
        let value = self.interpret_expr_no_resolve(&expr, ctx)?;
        Ok(Expr::value(value))
    }

    fn optimize_item(&self, _item: Item, _ctx: &SharedScopedContext) -> Result<Item> {
        Ok(Item::unit())
    }

    fn evaluate_condition(&self, expr: Expr, ctx: &SharedScopedContext) -> Result<ControlFlow> {
        let value = self.interpret_expr_no_resolve(&expr, ctx)?;
        match value {
            Value::Bool(b) => {
                if b.value {
                    Ok(ControlFlow::IntoAndBreak(None))
                } else {
                    Ok(ControlFlow::Continue)
                }
            }
            _ => opt_bail!(format!("Failed to interpret {:?} => {:?}", expr, value)),
        }
    }
    fn evaluate_invoke(
        &self,
        _invoke: ExprInvoke,
        _ctx: &SharedScopedContext,
    ) -> Result<ControlFlow> {
        Ok(ControlFlow::Into)
    }
    fn optimize_invoke(
        &self,
        invoke: ExprInvoke,
        func: &Value,
        ctx: &SharedScopedContext,
    ) -> Result<Expr> {
        match func {
            Value::Function(func) => self.interpret_expr(&func.body.get(), ctx).map(Expr::value),
            Value::BinOpKind(kind) => self
                .interpret_invoke_binop(kind.clone(), &invoke.args, ctx)
                .map(Expr::value),
            Value::UnOpKind(func) => {
                opt_ensure!(
                    invoke.args.len() == 1,
                    format!("Expected 1 arg for {:?}", func)
                );
                let arg = self.interpret_expr(&invoke.args[0].get(), ctx)?;
                self.interpret_invoke_unop(func.clone(), arg, ctx)
                    .map(Expr::value)
            }
            _ => opt_bail!(format!("Could not invoke {:?}", func)),
        }
    }

    fn try_evaluate_expr(&self, pat: &Expr, ctx: &SharedScopedContext) -> Result<Expr> {
        // First try the simple approach for basic expressions
        if let Some(value) = ctx.try_get_value_from_expr(pat) {
            return Ok(Expr::value(value));
        }

        // If simple approach fails, use full interpretation for complex expressions
        let value = self.interpret_expr(pat, ctx)?;
        Ok(Expr::value(value))
    }
}

impl ValueSystem for InterpretationOrchestrator {
    fn get_value_from_expr(&self, ctx: &Context, expr: &Expr) -> Result<Value> {
        let fold = FoldOptimizer::new(self.serializer.clone(), Box::new(self.clone()));
        let expr = fold.optimize_expr(expr.clone(), &ctx.values)?;
        match expr {
            Expr::Value(value) => Ok(*value),
            _ => opt_bail!(format!("Expected value, got {:?}", expr)),
        }
    }
}
