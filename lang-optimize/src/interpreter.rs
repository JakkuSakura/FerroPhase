use std::sync::Arc;

use common::*;

use lang_core::ast::{Expr, Item, Module, Tree};
use lang_core::context::SharedScopedContext;
use lang_core::value::Value;
use lang_core::Serializer;

use crate::pass::{FoldOptimizer, InterpreterPass};

pub struct Interpreter {
    pub opt: FoldOptimizer,
}
impl Interpreter {
    pub fn new(serializer: Arc<dyn Serializer>) -> Self {
        let pass = InterpreterPass::new(serializer.clone());
        Self {
            opt: FoldOptimizer::new(serializer, Box::new(pass)),
        }
    }
    fn extract_expr(&self, node: Expr) -> Result<Value> {
        match node {
            Expr::Value(value) => Ok(value.get()),
            _ => bail!("Failed to extract Value from {}", node),
        }
    }
    fn extract_module(&self, _node: Module) -> Result<Value> {
        Ok(Value::unit())
    }
    fn extract_item(&self, node: Item) -> Result<Value> {
        match node {
            Item::Expr(expr) => self.extract_expr(expr),
            Item::Module(module) => self.extract_module(module),
            _ => bail!("Failed to extract Value from {:?}", node),
        }
    }
    fn extract_tree(&self, node: Tree) -> Result<Value> {
        match node {
            Tree::Expr(expr) => self.extract_expr(expr),
            Tree::Item(item) => self.extract_item(item),
            Tree::File(file) => self.extract_module(file.module),
        }
    }
    pub fn interpret_tree(&self, node: Tree, ctx: &SharedScopedContext) -> Result<Value> {
        let value = self.opt.optimize_tree(node, ctx)?;

        self.extract_tree(value)
    }
    pub fn interpret_expr(&self, node: Expr, ctx: &SharedScopedContext) -> Result<Value> {
        let value = self.opt.optimize_expr(node, ctx)?;
        self.extract_expr(value)
    }
}
