use crate::ast::{File, Item, Module, Tree};
use crate::expr::{AExpr, Block, Expr, Invoke, Statement};
use crate::value::{Type, Value, ValueFunction};
use common::*;
use std::cell::RefCell;
use std::sync::Arc;

#[allow(unused_variables)]
pub trait Serializer: Send + Sync {
    fn serialize_tree(&self, node: &Tree) -> Result<String> {
        match node {
            Tree::Item(item) => self.serialize_item(item),
            Tree::Expr(expr) => self.serialize_expr(expr),
            Tree::File(file) => self.serialize_file(file),
        }
    }
    fn serialize_expr(&self, node: &Expr) -> Result<String> {
        bail!("not implemented: serialize_expr")
    }
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
    fn serialize_args_arena(&self, nodes: &[AExpr]) -> Result<String> {
        let mut v = vec![];
        for node in nodes.iter() {
            v.push(node.get());
        }
        self.serialize_args(&v)
    }
    fn serialize_invoke(&self, node: &Invoke) -> Result<String> {
        bail!("not implemented: serialize_invoke")
    }
    fn serialize_item(&self, node: &Item) -> Result<String> {
        bail!("not implemented: serialize_item")
    }
    fn serialize_block(&self, node: &Block) -> Result<String> {
        bail!("not implemented: serialize_block")
    }
    fn serialize_file(&self, node: &File) -> Result<String> {
        bail!("not implemented: serialize_file")
    }
    fn serialize_module(&self, node: &Module) -> Result<String> {
        bail!("not implemented: serialize_module")
    }
    fn serialize_value(&self, node: &Value) -> Result<String> {
        bail!("not implemented: serialize_value")
    }
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
    fn serialize_type(&self, node: &Type) -> Result<String> {
        bail!("not implemented: serialize_type")
    }
    fn serialize_stmt(&self, node: &Statement) -> Result<String> {
        bail!("not implemented: serialize_stmt")
    }
    fn serialize_function(&self, node: &ValueFunction) -> Result<String> {
        bail!("not implemented: serialize_function")
    }
}

thread_local! {
    static SERIALIZER: RefCell<Option<Arc<dyn Serializer>>> = RefCell::new(None);
}
pub fn register_threadlocal_serializer(serializer: Arc<dyn Serializer>) {
    SERIALIZER.with(move |s| {
        *s.borrow_mut() = Some(serializer);
    });
}

pub fn get_threadlocal_serializer() -> Arc<dyn Serializer> {
    SERIALIZER.with(|s| {
        s.borrow()
            .as_ref()
            .expect("serializer not registered")
            .clone()
    })
}
