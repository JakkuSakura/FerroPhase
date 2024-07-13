use crate::ast::{
    AstExpr, AstFile, AstItem, AstModule, AstNode, BExpr, BlockStmt, ExprBlock, ExprInvoke,
    ItemDefFunction,
};
use crate::ast::{AstType, Value, ValueFunction};
use common::*;
use std::cell::RefCell;
use std::sync::Arc;

#[allow(unused_variables)]
pub trait AstSerializer: Send + Sync {
    fn serialize_node(&self, node: &AstNode) -> Result<String> {
        match node {
            AstNode::Item(item) => self.serialize_item(item),
            AstNode::Expr(expr) => self.serialize_expr(expr),
            AstNode::File(file) => self.serialize_file(file),
        }
    }
    fn serialize_expr(&self, node: &AstExpr) -> Result<String> {
        bail!("not implemented: serialize_expr")
    }
    fn serialize_args(&self, nodes: &[AstExpr]) -> Result<String> {
        let mut s = String::new();
        for (i, node) in nodes.iter().enumerate() {
            if i > 0 {
                s.push_str(", ");
            }
            s.push_str(&self.serialize_expr(node)?);
        }
        Ok(s)
    }
    fn serialize_args_arena(&self, nodes: &[BExpr]) -> Result<String> {
        let mut v = vec![];
        for node in nodes.iter() {
            v.push(*node.clone());
        }
        self.serialize_args(&v)
    }
    fn serialize_invoke(&self, node: &ExprInvoke) -> Result<String> {
        bail!("not implemented: serialize_invoke")
    }
    fn serialize_item(&self, node: &AstItem) -> Result<String> {
        bail!("not implemented: serialize_item")
    }
    fn serialize_block(&self, node: &ExprBlock) -> Result<String> {
        bail!("not implemented: serialize_block")
    }
    fn serialize_file(&self, node: &AstFile) -> Result<String> {
        bail!("not implemented: serialize_file")
    }
    fn serialize_module(&self, node: &AstModule) -> Result<String> {
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
    fn serialize_type(&self, node: &AstType) -> Result<String> {
        bail!("not implemented: serialize_type")
    }
    fn serialize_stmt(&self, node: &BlockStmt) -> Result<String> {
        bail!("not implemented: serialize_stmt")
    }
    fn serialize_value_function(&self, node: &ValueFunction) -> Result<String> {
        bail!("not implemented: serialize_function")
    }
    fn serialize_def_function(&self, node: &ItemDefFunction) -> Result<String> {
        bail!("not implemented: serialize_def_function")
    }
}

thread_local! {
    static SERIALIZER: RefCell<Option<Arc<dyn AstSerializer >>> = RefCell::new(None);
}
pub fn register_threadlocal_serializer(serializer: Arc<dyn AstSerializer>) {
    SERIALIZER.with(move |s| {
        *s.borrow_mut() = Some(serializer);
    });
}

pub fn get_threadlocal_serializer() -> Arc<dyn AstSerializer> {
    SERIALIZER.with(|s| {
        s.borrow()
            .as_ref()
            .expect("serializer not registered")
            .clone()
    })
}
