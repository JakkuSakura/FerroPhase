use crate::ast::{Block, Expr, Invoke, Item, Module, Statement, Tree};
use crate::value::{FunctionValue, TypeValue, Value};
use common::*;
use std::cell::RefCell;
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
    fn serialize_type(&self, node: &TypeValue) -> Result<String>;
    fn serialize_stmt(&self, node: &Statement) -> Result<String>;
    fn serialize_function(&self, node: &FunctionValue) -> Result<String>;
}

thread_local! {
    static SERIALIZER: RefCell<Option<Rc<dyn Serializer>>> = RefCell::new(None);
}
pub fn register_threadlocal_serializer(serializer: Rc<dyn Serializer>) {
    SERIALIZER.with(move |s| {
        *s.borrow_mut() = Some(serializer);
    });
}

pub fn get_threadlocal_serializer() -> Rc<dyn Serializer> {
    SERIALIZER.with(|s| {
        s.borrow()
            .as_ref()
            .expect("serializer not registered")
            .clone()
    })
}
