use crate::tree::*;
use crate::value::{FunctionValue, TypeValue, Value};
use crate::Serializer;
use common::*;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use std::mem::replace;
use std::rc::Rc;

#[derive(Default)]
pub struct InterpreterContextInner {
    parent: Option<ExecutionContext>,
    trees: HashMap<Path, Tree>,
    func_decls: HashMap<Path, FunctionValue>,
    values: HashMap<Path, Value>,
    types: HashMap<Path, TypeValue>,
    is_specialized: HashMap<Path, bool>,
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
    pub fn insert_tree(&self, key: Path, value: Tree) {
        self.inner.borrow_mut().trees.insert(key, value);
    }
    pub fn insert_type(&self, key: impl Into<Path>, value: TypeValue) {
        self.inner.borrow_mut().types.insert(key.into(), value);
    }
    pub fn insert_func_decl(&self, key: impl Into<Path>, value: FunctionValue) {
        let key = key.into();
        self.insert_expr(key.clone(), Expr::value(Value::Function(value.clone())));
        self.inner.borrow_mut().func_decls.insert(key.into(), value);
    }
    pub fn insert_expr(&self, key: Path, value: Expr) {
        self.insert_value(key, Value::expr(value));
    }
    pub fn insert_value(&self, key: impl Into<Path>, value: Value) {
        self.inner.borrow_mut().values.insert(key.into(), value);
    }

    pub fn print_values(&self, s: impl Serializer) -> Result<()> {
        let inner = self.inner.borrow();
        for (k, v) in &inner.trees {
            info!("{}: {}", k, s.serialize_tree(v)?)
        }
        Ok(())
    }
    pub fn insert_specialized(&self, key: Path, value: FunctionValue) {
        self.inner
            .borrow_mut()
            .func_decls
            .insert(key.clone(), value);
        self.inner.borrow_mut().is_specialized.insert(key, true);
    }
    pub fn get_func_decl(&self, key: impl Into<Path>) -> Option<FunctionValue> {
        let inner = self.inner.borrow();
        let key = key.into();
        inner
            .func_decls
            .get(&key)
            .cloned()
            .or_else(|| inner.parent.as_ref()?.get_func_decl(key))
    }
    pub fn get_tree(&self, key: impl Into<Path>) -> Option<Tree> {
        let inner = self.inner.borrow();
        let key = key.into();

        inner
            .trees
            .get(&key)
            .cloned()
            .or_else(|| inner.parent.as_ref()?.get_tree(key))
    }
    pub fn get_value(&self, key: impl Into<Path>) -> Option<Value> {
        let inner = self.inner.borrow();
        let key = key.into();

        inner
            .values
            .get(&key)
            .cloned()
            .or_else(|| inner.parent.as_ref()?.get_value(key))
    }
    pub fn get_expr(&self, key: impl Into<Path>) -> Option<Expr> {
        self.get_value(key).map(Expr::value)
    }
    pub fn get_type(&self, key: impl Into<Path>) -> Option<TypeValue> {
        let inner = self.inner.borrow();
        let key = key.into();

        inner
            .types
            .get(&key)
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
    pub fn list_specialized(&self) -> Vec<Path> {
        self.inner
            .borrow()
            .is_specialized
            .iter()
            .filter(|x| *x.1)
            .map(|x| x.0.clone())
            .collect()
    }
}
#[derive(Clone)]
pub struct LazyValue<Expr> {
    pub ctx: ExecutionContext,
    pub expr: Expr,
}
impl<Expr: Debug> Debug for LazyValue<Expr> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "LazyValue({:?})", self.expr)
    }
}
impl Serialize for LazyValue<Expr> {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_str(&format!("{:?}", self))
    }
}
impl<'de> Deserialize<'de> for LazyValue<Expr> {
    fn deserialize<D: serde::Deserializer<'de>>(_deserializer: D) -> Result<Self, D::Error> {
        unreachable!()
    }
}
