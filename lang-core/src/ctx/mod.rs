use std::ops::Deref;
use std::sync::Arc;

mod serde;
mod ty;
mod value;

pub use serde::*;
pub use ty::*;
pub use value::*;

#[derive(Clone)]
pub struct SharedContext(Arc<Context>);

impl Deref for SharedContext {
    type Target = Context;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
pub struct Context {
    pub typing: Arc<dyn TypingSystem>,
    pub value: Arc<dyn ValueSystem>,
    pub ser: Arc<dyn SerializeSystem>,
    pub de: Arc<dyn DeserializeSystem>,
}
impl Context {
    pub fn new() -> Self {
        Self {
            typing: Arc::new(()),
            value: Arc::new(()),
            ser: Arc::new(()),
            de: Arc::new(()),
        }
    }
    pub fn into_shared(self) -> SharedContext {
        SharedContext(Arc::new(self))
    }
}
