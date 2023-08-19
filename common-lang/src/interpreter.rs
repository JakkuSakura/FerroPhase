use crate::ast::*;
use crate::context::ArcScopedContext;
use crate::optimizer::FoldOptimizer;
use crate::passes::InterpreterPass;
use crate::value::*;
use crate::*;
use common::*;
use std::rc::Rc;

pub struct OptimizeInterpreter {
    pub opt: FoldOptimizer<InterpreterPass>,
}
impl OptimizeInterpreter {
    pub fn new(serializer: Rc<dyn Serializer>) -> Self {
        let pass = InterpreterPass::new(serializer.clone());
        Self {
            opt: FoldOptimizer::new(serializer, pass),
        }
    }
    fn extract_expr(&self, node: Expr) -> Result<Value> {
        match node {
            Expr::Value(value) => Ok(value),
            _ => bail!("Failed to extract Value from {:?}", node),
        }
    }
    fn extract_module(&self, node: Module) -> Result<Value> {
        match node.items.len() {
            0 => Ok(Value::unit()),
            1 => self.extract_item(node.items.into_iter().next().unwrap()),
            _ => bail!("Failed to extract Value from {:?}", node),
        }
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
    pub fn interpret_tree(&self, node: Tree, ctx: &ArcScopedContext) -> Result<Value> {
        let value = self.opt.optimize_tree(node, ctx)?;

        self.extract_tree(value)
    }
    pub fn interpret_expr(&self, node: Expr, ctx: &ArcScopedContext) -> Result<Value> {
        let value = self.opt.optimize_expr(node, ctx)?;
        self.extract_expr(value)
    }
}
