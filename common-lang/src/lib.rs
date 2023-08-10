pub mod ast;
pub mod interpreter;
pub mod preloader;
pub mod specializer;
// pub mod visitor;

use ast::*;
use common::*;

use std::rc::Rc;

pub trait Serializer {
    fn serialize(&self, node: &Expr) -> Result<String>;
}

pub trait Deserializer {
    fn deserialize(&self, code: &str) -> Result<Expr>;
}
