use crate::ast::BExpr;
use crate::context::SharedScopedContext;
use serde::{Deserialize, Serialize};
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;

/// A special Closure expression that captures the current context
#[derive(Clone, PartialEq, Eq)]
pub struct ExprClosured {
    pub ctx: SharedScopedContext,
    pub expr: BExpr,
}
impl ExprClosured {
    pub fn new(ctx: SharedScopedContext, expr: BExpr) -> Self {
        Self { ctx, expr }
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
impl Serialize for ExprClosured {
    fn serialize<S: serde::Serializer>(&self, _serializer: S) -> Result<S::Ok, S::Error> {
        unreachable!("Closure should not be serialized")
    }
}
impl<'de> Deserialize<'de> for ExprClosured {
    fn deserialize<D: serde::Deserializer<'de>>(_deserializer: D) -> Result<Self, D::Error> {
        unreachable!("Closure should not be deserialized")
    }
}
