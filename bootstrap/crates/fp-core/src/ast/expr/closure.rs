use crate::ast::BExpr;
use crate::span::Span;
use crate::context::SharedScopedContext;
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;

/// A special Closure expression that captures the current context
#[derive(Clone, PartialEq)]
pub struct ExprClosured {
    pub ctx: SharedScopedContext,
    pub expr: BExpr,
}
impl ExprClosured {
    pub fn new(ctx: SharedScopedContext, expr: BExpr) -> Self {
        Self { ctx, expr }
    }

    pub fn span(&self) -> Span {
        self.expr.span()
    }
}

impl Display for ExprClosured {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Closure({})", self.expr)
    }
}
impl Debug for ExprClosured {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Closure({:?})", self.expr)
    }
}
impl Hash for ExprClosured {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let address = (&*self.ctx) as *const _ as usize;
        address.hash(state);
        self.expr.hash(state);
    }
}
