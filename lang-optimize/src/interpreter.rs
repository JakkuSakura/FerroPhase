use std::sync::Arc;

use common::*;

use lang_core::ast::Value;
use lang_core::ast::{AstExpr, AstItem, AstSerializer, AstTree, Module};
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
    fn extract_module(&self, _node: Module) -> Result<Value> {
        Ok(Value::unit())
    }
    fn extract_item(&self, node: AstItem) -> Result<Value> {
        match node {
            AstItem::Expr(expr) => self.extract_expr(expr),
            AstItem::Module(module) => self.extract_module(module),
            _ => bail!("Failed to extract Value from {:?}", node),
        }
    }
    fn extract_tree(&self, node: AstTree) -> Result<Value> {
        match node {
            AstTree::Expr(expr) => self.extract_expr(expr),
            AstTree::Item(item) => self.extract_item(item),
            AstTree::File(file) => self.extract_module(file.module),
        }
    }
    pub fn interpret_tree(&self, node: AstTree, ctx: &SharedScopedContext) -> Result<Value> {
        let value = self.opt.optimize_tree(node, ctx)?;

        self.extract_tree(value)
    }
    pub fn interpret_expr(&self, node: AstExpr, ctx: &SharedScopedContext) -> Result<Value> {
        let value = self.opt.optimize_expr(node, ctx)?;
        self.extract_expr(value)
    }
}
