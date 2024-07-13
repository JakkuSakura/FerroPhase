use std::sync::Arc;

use common::*;

use lang_core::ast::{AstExpr, AstItem, AstModule, AstNode, AstSerializer};
use lang_core::ast::{AstFile, Value};
use lang_core::context::SharedScopedContext;

use crate::pass::{FoldOptimizer, InterpreterPass};

pub struct Interpreter {
    pub opt: FoldOptimizer,
}
impl Interpreter {
    pub fn new(serializer: Arc<dyn AstSerializer>) -> Self {
        let pass = InterpreterPass::new(serializer.clone());
        Self {
            opt: FoldOptimizer::new(serializer, Box::new(pass)),
        }
    }
    fn extract_expr(&self, node: AstExpr) -> Result<Value> {
        match node {
            AstExpr::Value(value) => Ok(value.get()),
            _ => bail!("Failed to extract Value from {}", node),
        }
    }
    fn extract_module(&self, _node: AstModule) -> Result<Value> {
        Ok(Value::unit())
    }
    fn extract_file(&self, _node: AstFile) -> Result<Value> {
        Ok(Value::unit())
    }
    fn extract_item(&self, node: AstItem) -> Result<Value> {
        match node {
            AstItem::Expr(expr) => self.extract_expr(expr),
            AstItem::Module(module) => self.extract_module(module),
            _ => bail!("Failed to extract Value from {:?}", node),
        }
    }
    fn extract_tree(&self, node: AstNode) -> Result<Value> {
        match node {
            AstNode::Expr(expr) => self.extract_expr(expr),
            AstNode::Item(item) => self.extract_item(item),
            AstNode::File(file) => self.extract_file(file),
        }
    }
    pub fn interpret_tree(&self, node: AstNode, ctx: &SharedScopedContext) -> Result<Value> {
        let value = self.opt.optimize_tree(node, ctx)?;

        self.extract_tree(value)
    }
    pub fn interpret_expr(&self, node: AstExpr, ctx: &SharedScopedContext) -> Result<Value> {
        let value = self.opt.optimize_expr(node, ctx)?;
        self.extract_expr(value)
    }
}
