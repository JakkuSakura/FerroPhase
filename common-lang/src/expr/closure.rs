use crate::context::SharedScopedContext;
use crate::expr::Expr;
use serde::{Deserialize, Serialize};
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;

/// A special Closure expression that captures the current context
#[derive(Clone, PartialEq, Eq)]
pub struct Closure<Expr> {
    pub ctx: SharedScopedContext,
    pub expr: Expr,
}
impl<Expr> Closure<Expr> {
    pub fn new(ctx: SharedScopedContext, expr: Expr) -> Self {
        Self { ctx, expr }
    }
}

impl<Expr: Display> Display for Closure<Expr> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Closure({})", self.expr)
    }
}
impl<Expr: Debug> Debug for Closure<Expr> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Closure({:?})", self.expr)
    }
}
impl<Expr: Hash> Hash for Closure<Expr> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let address = (&*self.ctx) as *const _ as usize;
        address.hash(state);
        self.expr.hash(state);
    }
}
impl Serialize for Closure<Expr> {
    fn serialize<S: serde::Serializer>(&self, _serializer: S) -> Result<S::Ok, S::Error> {
        unreachable!("Closure should not be serialized")
    }
}
impl<'de> Deserialize<'de> for Closure<Expr> {
    fn deserialize<D: serde::Deserializer<'de>>(_deserializer: D) -> Result<Self, D::Error> {
        unreachable!("Closure should not be deserialized")
    }
}
