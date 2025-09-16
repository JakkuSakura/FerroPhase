use std::sync::Arc;

// Update imports to use our new error module
use fp_core::error::Result;
use crate::error::optimization_error;

use fp_core::ast::{AstExpr, AstItem, AstModule, AstNode, AstSerializer};
use fp_core::ast::{AstFile, AstValue};
use fp_core::context::SharedScopedContext;

use crate::utils::FoldOptimizer;
use crate::orchestrators::InterpretationOrchestrator;

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
    fn extract_expr(&self, node: AstExpr) -> Result<AstValue> {
        match node {
            AstExpr::Value(value) => Ok(value.get()),
            AstExpr::Block(block) => {
                // Handle block expressions
                if block.stmts.is_empty() {
                    // Empty block evaluates to unit
                    Ok(AstValue::unit())
                } else {
                    // Try to extract value from the last statement in block
                    if let Some(last_stmt) = block.stmts.last() {
                        match last_stmt {
                            fp_core::ast::BlockStmt::Expr(block_expr) => {
                                // Extract value from the expression
                                self.extract_expr(*block_expr.expr.clone())
                            },
                            _ => {
                                // Non-expression statement, block evaluates to unit
                                Ok(AstValue::unit())
                            }
                        }
                    } else {
                        Ok(AstValue::unit())
                    }
                }
            },
            _ => Err(optimization_error(format!("Failed to extract Value from {}", node))),
        }
    }
    fn extract_module(&self, _node: AstModule) -> Result<AstValue> {
        Ok(AstValue::unit())
    }
    fn extract_file(&self, _node: AstFile) -> Result<AstValue> {
        Ok(AstValue::unit())
    }
    fn extract_item(&self, node: AstItem) -> Result<AstValue> {
        match node {
            AstItem::Expr(expr) => self.extract_expr(expr),
            AstItem::Module(module) => self.extract_module(module),
            _ => Err(optimization_error(format!("Failed to extract Value from {:?}", node))),
        }
    }
    fn extract_tree(&self, node: AstNode) -> Result<AstValue> {
        match node {
            AstNode::Expr(expr) => self.extract_expr(expr),
            AstNode::Item(item) => self.extract_item(item),
            AstNode::File(file) => self.extract_file(file),
        }
    }
    pub fn interpret_tree(&self, node: AstNode, ctx: &SharedScopedContext) -> Result<AstValue> {
        let value = self.opt.optimize_tree(node, ctx)?;

        self.extract_tree(value)
    }
    pub fn interpret_expr(&self, node: AstExpr, ctx: &SharedScopedContext) -> Result<AstValue> {
        let value = self.opt.optimize_expr(node, ctx)?;
        self.extract_expr(value)
    }
}
