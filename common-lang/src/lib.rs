pub mod context;
pub mod interpreter;
pub mod ops;
pub mod optimizer;
pub mod passes;
pub mod tree;
pub mod type_system;
pub mod value;
use common::*;
use tree::*;

use crate::tree::Tree;

use crate::value::Value;
use std::rc::Rc;

pub trait Serializer {
    fn serialize_tree(&self, node: &Tree) -> Result<String>;
    fn serialize_expr(&self, node: &Expr) -> Result<String>;
    fn serialize_args(&self, nodes: &[Expr]) -> Result<String> {
        let mut s = String::new();
        for (i, node) in nodes.iter().enumerate() {
            if i > 0 {
                s.push_str(", ");
            }
            s.push_str(&self.serialize_expr(node)?);
        }
        Ok(s)
    }
    fn serialize_invoke(&self, node: &Invoke) -> Result<String>;
    fn serialize_item(&self, node: &Item) -> Result<String>;
    fn serialize_block(&self, node: &Block) -> Result<String>;
    fn serialize_module(&self, node: &Module) -> Result<String>;
    fn serialize_value(&self, node: &Value) -> Result<String>;
    fn serialize_values(&self, nodes: &[Value]) -> Result<String> {
        let mut s = String::new();
        for (i, node) in nodes.iter().enumerate() {
            if i > 0 {
                s.push_str(", ");
            }
            s.push_str(&self.serialize_value(node)?);
        }
        Ok(s)
    }
    fn serialize_stmt(&self, node: &Statement) -> Result<String>;
}

pub trait Deserializer {
    fn deserialize(&self, code: &str) -> Result<Tree>;
}
