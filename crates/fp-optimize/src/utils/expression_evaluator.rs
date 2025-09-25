use std::sync::Arc;

// Update imports to use our new error module
use crate::error::optimization_error;
use fp_core::error::Result;

use fp_core::ast::{AstSerializer, Expr, Item, Module, Node};
use fp_core::ast::{File, Value};
use fp_core::context::SharedScopedContext;

use crate::orchestrators::InterpretationOrchestrator;
use crate::utils::FoldOptimizer;

/// Simple expression evaluator utility
/// Wraps the interpretation orchestrator for basic expression evaluation
pub struct ExpressionEvaluator {
    pub opt: FoldOptimizer,
}

impl ExpressionEvaluator {
    pub fn new(serializer: Arc<dyn AstSerializer>) -> Self {
        let orchestrator = InterpretationOrchestrator::new(serializer.clone());
        Self {
            opt: FoldOptimizer::new(serializer, Box::new(orchestrator)),
        }
    }
    fn extract_expr(&self, node: Expr) -> Result<Value> {
        match node {
            Expr::Value(value) => Ok(value.get()),
            Expr::Block(block) => {
                // Handle block expressions
                if block.stmts.is_empty() {
                    // Empty block evaluates to unit
                    Ok(Value::unit())
                } else {
                    // Try to extract value from the last statement in block
                    if let Some(last_stmt) = block.stmts.last() {
                        match last_stmt {
                            fp_core::ast::BlockStmt::Expr(block_expr) => {
                                // Extract value from the expression
                                self.extract_expr(*block_expr.expr.clone())
                            }
                            _ => {
                                // Non-expression statement, block evaluates to unit
                                Ok(Value::unit())
                            }
                        }
                    } else {
                        Ok(Value::unit())
                    }
                }
            }
            _ => Err(optimization_error(format!(
                "Failed to extract Value from {}",
                node
            ))),
        }
    }
    fn extract_module(&self, _node: Module) -> Result<Value> {
        Ok(Value::unit())
    }
    fn extract_file(&self, _node: File) -> Result<Value> {
        Ok(Value::unit())
    }
    fn extract_item(&self, node: Item) -> Result<Value> {
        match node {
            Item::Expr(expr) => self.extract_expr(expr),
            Item::Module(module) => self.extract_module(module),
            _ => Err(optimization_error(format!(
                "Failed to extract Value from {:?}",
                node
            ))),
        }
    }
    fn extract_tree(&self, node: Node) -> Result<Value> {
        match node {
            Node::Expr(expr) => self.extract_expr(expr),
            Node::Item(item) => self.extract_item(item),
            Node::File(file) => self.extract_file(file),
        }
    }
    pub fn interpret_tree(&self, node: Node, ctx: &SharedScopedContext) -> Result<Value> {
        let value = self.opt.optimize_tree(node, ctx)?;

        self.extract_tree(value)
    }
    pub fn interpret_expr(&self, node: Expr, ctx: &SharedScopedContext) -> Result<Value> {
        let value = self.opt.optimize_expr(node, ctx)?;
        self.extract_expr(value)
    }
}
