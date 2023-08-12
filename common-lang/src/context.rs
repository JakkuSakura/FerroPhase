use crate::tree::{Expr, FuncDecl, Ident, Tree};
use crate::value::TypeValue;
use crate::Serializer;
use common::*;
use std::cell::RefCell;
use std::collections::HashMap;
use std::mem::replace;
use std::rc::Rc;

#[derive(Default)]
pub struct InterpreterContextInner {
    parent: Option<ExecutionContext>,
    trees: HashMap<Ident, Tree>,
    func_decls: HashMap<Ident, FuncDecl>,
    exprs: HashMap<Ident, Expr>,
    types: HashMap<Ident, TypeValue>,
    is_specialized: HashMap<Ident, bool>,
    buffer: Vec<String>,
}

#[derive(Clone)]
pub struct ExecutionContext {
    inner: Rc<RefCell<InterpreterContextInner>>,
}

impl ExecutionContext {
    pub fn new() -> Self {
        Self {
            inner: Rc::new(RefCell::new(InterpreterContextInner::default())),
        }
    }
    pub fn child(&self) -> ExecutionContext {
        Self {
            inner: Rc::new(RefCell::new(InterpreterContextInner {
                parent: Some(self.clone()),
                ..Default::default()
            })),
        }
    }
    pub fn insert_tree(&self, key: Ident, value: Tree) {
        self.inner.borrow_mut().trees.insert(key, value);
    }
    pub fn insert_type(&self, key: Ident, value: TypeValue) {
        self.inner.borrow_mut().types.insert(key, value);
    }
    pub fn insert_func_decl(&self, key: Ident, value: FuncDecl) {
        self.inner.borrow_mut().func_decls.insert(key, value);
    }
    pub fn insert_expr(&self, key: Ident, value: Expr) {
        self.inner.borrow_mut().exprs.insert(key, value.into());
    }
    pub fn print_values(&self, s: impl Serializer) -> Result<()> {
        let inner = self.inner.borrow();
        for (k, v) in &inner.trees {
            info!("{}: {}", k.as_str(), s.serialize_tree(v)?)
        }
        Ok(())
    }
    pub fn insert_specialized(&self, key: Ident, value: FuncDecl) {
        self.inner
            .borrow_mut()
            .func_decls
            .insert(key.clone(), value);
        self.inner.borrow_mut().is_specialized.insert(key, true);
    }
    pub fn get_func_decl(&self, key: &Ident) -> Option<FuncDecl> {
        let inner = self.inner.borrow();
        inner
            .func_decls
            .get(key)
            .cloned()
            .or_else(|| inner.parent.as_ref()?.get_func_decl(key))
    }
    pub fn get_tree(&self, key: &Ident) -> Option<Tree> {
        let inner = self.inner.borrow();
        inner
            .trees
            .get(key)
            .cloned()
            .or_else(|| inner.parent.as_ref()?.get_tree(key))
    }
    pub fn get_expr(&self, key: &Ident) -> Option<Expr> {
        let inner = self.inner.borrow();
        inner
            .exprs
            .get(key)
            .cloned()
            .or_else(|| inner.parent.as_ref()?.get_expr(key))
    }
    pub fn get_type(&self, key: &Ident) -> Option<TypeValue> {
        let inner = self.inner.borrow();
        inner
            .types
            .get(key)
            .cloned()
            .or_else(|| inner.parent.as_ref()?.get_type(key))
    }
    pub fn root(&self) -> ExecutionContext {
        self.inner
            .borrow()
            .parent
            .as_ref()
            .map(|x| x.root())
            .unwrap_or_else(|| self.clone())
    }
    pub fn print_str(&self, s: String) {
        self.inner.borrow_mut().buffer.push(s);
    }
    pub fn take_outputs(&self) -> Vec<String> {
        replace(&mut self.inner.borrow_mut().buffer, vec![])
    }
    pub fn list_specialized(&self) -> Vec<Ident> {
        self.inner
            .borrow()
            .is_specialized
            .iter()
            .filter(|x| *x.1)
            .map(|x| x.0.clone())
            .collect()
    }
}
