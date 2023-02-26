#![feature(decl_macro)]
extern crate core;

pub mod ast;
pub mod interpreter;
pub mod specializer;

use ast::*;
use common::*;

use std::rc::Rc;

pub trait Serializer {
    fn serialize_expr(&self, node: &Expr) -> Result<String> {
        self.serialize(&**node)
    }
    fn serialize(&self, node: &dyn AnyAst) -> Result<String>;
}

pub trait Deserializer {
    fn deserialize(&self, code: &str) -> Result<Expr>;
}
