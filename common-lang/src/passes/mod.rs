use crate::context::ExecutionContext;
use crate::tree::*;
use crate::*;
use common::*;

mod inliner;
mod optimizer;
mod specializer;
pub use inliner::*;
pub use optimizer::*;
pub use specializer::*;
#[allow(unused_variables)]
pub trait OptimizePass {
    fn optimize_item(&self, item: Item, ctx: &ExecutionContext) -> Result<Option<Item>> {
        Ok(Some(item))
    }
    fn optimize_expr(&self, expr: Expr, ctx: &ExecutionContext) -> Result<Expr> {
        Ok(expr)
    }
    fn optimize_tree(&self, node: Tree, ctx: &ExecutionContext) -> Result<Option<Tree>> {
        Ok(Some(node))
    }
    fn optimize_def(&self, def: Define, ctx: &ExecutionContext) -> Result<Option<Define>> {
        Ok(Some(def))
    }
}

pub fn load_optimizer(serializer: Rc<dyn Serializer>) -> Optimizer {
    let mut opt = Optimizer::new(serializer.clone());
    opt.add_pass(Specializer::new(serializer.clone()));
    opt.add_pass(Inliner::new(serializer.clone()));
    opt
}
