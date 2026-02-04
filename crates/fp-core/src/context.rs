use crate::ast::{
    Expr, ExprClosured, ExprKind, Ident, Path, RuntimeValue, Ty, Value, ValueFunction, Visibility,
};
use crate::collections::ConcurrentMap;
use std::ops::Deref;
use std::sync::{Arc, Mutex, Weak};

/// Execution mode for the interpreter
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExecutionMode {
    /// Compile-time evaluation (const items, types, etc.)
    CompileTime,
    /// Runtime evaluation (function bodies)
    Runtime,
}

#[derive(Clone, Default)]
pub struct ValueSlot {
    value: Option<Value>,
    runtime_value: Option<RuntimeValue>,
    ty: Option<Ty>,
    closure: Option<Arc<ScopedContext>>,
}
#[derive(Clone, Default)]
pub struct SharedValueSlot {
    storage: Arc<Mutex<ValueSlot>>,
}
impl SharedValueSlot {
    pub fn new() -> Self {
        Self {
            storage: Arc::new(Mutex::new(ValueSlot::default())),
        }
    }
    pub fn with_storage<R>(&self, func: impl FnOnce(&mut ValueSlot) -> R) -> R {
        match self.storage.lock() {
            Ok(mut storage) => func(&mut storage),
            Err(poison) => {
                // Recover from a poisoned lock by taking the inner value
                let mut storage = poison.into_inner();
                func(&mut storage)
            }
        }
    }
    pub fn value(&self) -> Option<Value> {
        self.with_storage(|x| x.value.clone())
    }
    pub fn ty(&self) -> Option<Ty> {
        self.with_storage(|x| x.ty.clone())
    }
    pub fn set_value(&self, value: Value) {
        self.with_storage(|x| x.value = Some(value));
    }
    pub fn runtime_value(&self) -> Option<RuntimeValue> {
        self.with_storage(|x| x.runtime_value.clone())
    }
    pub fn set_runtime_value(&self, value: RuntimeValue) {
        self.with_storage(|x| x.runtime_value = Some(value));
    }
    pub fn set_ty(&self, ty: Ty) {
        self.with_storage(|x| x.ty = Some(ty));
    }
    pub fn closure(&self) -> Option<Arc<ScopedContext>> {
        self.with_storage(|x| x.closure.clone())
    }
    pub fn set_closure(&self, closure: Arc<ScopedContext>) {
        self.with_storage(|x| x.closure = Some(closure));
    }
}
pub struct ScopedContext {
    parent: Option<Weak<Self>>,
    // Identifier for debug/provenance; currently unused outside construction
    _ident: Ident,
    path: Path,
    storages: ConcurrentMap<Ident, SharedValueSlot>,
    childs: ConcurrentMap<Ident, Arc<Self>>,
    buffer: Mutex<Vec<String>>,
    // Visibility tracking placeholder; reserved for future policy checks
    _visibility: Visibility,
    access_parent_locals: bool,
    execution_mode: ExecutionMode,
}

impl ScopedContext {
    pub fn new() -> Self {
        ScopedContext {
            parent: None,
            _ident: Ident::root(),
            path: Path::root(),
            storages: Default::default(),
            childs: Default::default(),
            buffer: Mutex::new(vec![]),
            _visibility: Visibility::Public,
            access_parent_locals: false,
            execution_mode: ExecutionMode::CompileTime, // Default to compile-time
        }
    }

    pub fn new_with_mode(mode: ExecutionMode) -> Self {
        ScopedContext {
            parent: None,
            _ident: Ident::root(),
            path: Path::root(),
            storages: Default::default(),
            childs: Default::default(),
            buffer: Mutex::new(vec![]),
            _visibility: Visibility::Public,
            access_parent_locals: false,
            execution_mode: mode,
        }
    }

    pub fn execution_mode(&self) -> ExecutionMode {
        self.execution_mode
    }

    pub fn insert_value(&self, key: impl Into<Ident>, value: Value) {
        let store = self.storages.get_or_insert_default(key.into());
        store.set_value(value);
    }

    pub fn insert_expr(&self, key: impl Into<Ident>, value: Expr) {
        self.insert_value(key, Value::expr(value));
    }

    pub fn insert_runtime_value(&self, key: impl Into<Ident>, value: RuntimeValue) {
        let store = self.storages.get_or_insert_default(key.into());
        store.set_runtime_value(value);
    }

    pub fn get_runtime_value(&self, key: &Ident) -> Option<RuntimeValue> {
        self.storages.get_cloned(key)?.runtime_value()
    }

    pub fn get_runtime_value_recursive(&self, path: &Path) -> Option<RuntimeValue> {
        if path.is_root() {
            return None;
        }

        if path.segments.len() == 1 {
            if let Some(runtime_value) = self.get_runtime_value(&path.segments[0]) {
                return Some(runtime_value);
            }
        }

        // Search in parent contexts
        if let Some(parent_ref) = &self.parent {
            if let Some(parent) = parent_ref.upgrade() {
                return parent.get_runtime_value_recursive(path);
            }
        }

        None
    }

    pub fn print_local_values(&self) -> Result<(), crate::Error> {
        debug!("Values in {}", self.path);
        self.storages.for_each(|k, v| {
            v.with_storage(|v| {
                let value = v.value.as_ref().unwrap_or(&Value::UNDEFINED);

                let ty = v.ty.as_ref().unwrap_or(&Ty::UNKNOWN);
                debug!("{}: val:{} ty:{}", k, value, ty)
            })
        });
        Ok(())
    }

    pub fn print_str(&self, s: String) {
        match self.buffer.lock() {
            Ok(mut buf) => buf.push(s),
            Err(poison) => poison.into_inner().push(s),
        }
    }
    pub fn take_outputs(&self) -> Vec<String> {
        match self.buffer.lock() {
            Ok(mut buf) => std::mem::replace(&mut *buf, vec![]),
            Err(poison) => {
                let mut buf = poison.into_inner();
                std::mem::replace(&mut *buf, vec![])
            }
        }
    }
}

#[derive(Clone)]
pub struct SharedScopedContext(Arc<ScopedContext>);
impl Deref for SharedScopedContext {
    type Target = ScopedContext;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl SharedScopedContext {
    pub fn new() -> Self {
        Self(Arc::new(ScopedContext::new()))
    }

    pub fn new_with_mode(mode: ExecutionMode) -> Self {
        Self(Arc::new(ScopedContext::new_with_mode(mode)))
    }

    pub fn child(&self, name: Ident, visibility: Visibility, access_parent_locals: bool) -> Self {
        let child = Self(Arc::new(ScopedContext {
            parent: Some(Arc::downgrade(&self.0)),
            _ident: name.clone(),
            path: self.path.with_ident(name.clone()),
            storages: Default::default(),
            childs: Default::default(),
            buffer: Mutex::new(vec![]),
            _visibility: visibility,
            access_parent_locals,
            execution_mode: self.execution_mode, // Inherit execution mode from parent
        }));
        self.childs.insert(name, child.0.clone());
        child
    }

    pub fn get_function(&self, key: impl Into<Path>) -> Option<(ValueFunction, Self)> {
        let value = self.get_storage(key, true)?;
        value.with_storage(|value| match value.value.clone()? {
            Value::Function(func) => {
                let closure = value.closure.clone()?;
                Some((func.clone(), Self(closure)))
            }
            _ => None,
        })
    }
    pub fn get_module_recursive(
        self: &SharedScopedContext,
        key: impl Into<Path>,
    ) -> Option<SharedScopedContext> {
        let key = key.into();
        let mut this = self.clone();
        if key.segments.is_empty() {
            return Some(this);
        }
        for seg in &key.segments {
            if seg.is_root() {
                this = this.root().clone();
                continue;
            }
            let v = this.childs.get_cloned(seg)?;
            this = Self(v);
        }

        Some(this)
    }
    pub fn get_storage(&self, key: impl Into<Path>, access_local: bool) -> Option<SharedValueSlot> {
        let key = key.into();
        debug!(
            "get_storage in {} {} access_local={}",
            self.path, key, access_local
        );
        let _ = self.print_local_values();
        if key.segments.is_empty() {
            return None;
        }
        if key.segments.len() == 1 {
            // TODO: when calling function, use context of its own
            // if access_local {
            if let Some(value) = self.storages.get_cloned(&key.segments[0]) {
                return Some(value);
            }
            // }
            return self
                .get_parent()?
                .get_storage(key, self.access_parent_locals);
        }

        let (paths, key) = key.segments.split_at(key.segments.len() - 1);
        let this = self.get_module_recursive(Path::plain(paths.to_owned()))?;
        this.storages.get_cloned(&key[0])
    }
    pub fn get_value(&self, key: impl Into<Path>) -> Option<Value> {
        let storage = self.get_storage(key, true)?;
        storage.value()
    }
    pub fn insert_value_with_ctx(&self, key: impl Into<Ident>, value: Value) {
        let store = self.storages.get_or_insert_default(key.into());
        store.with_storage(|store| {
            store.value = Some(value);
            store.closure = Some(self.clone().0);
        });
    }

    pub fn insert_runtime_value_with_ctx(&self, key: impl Into<Ident>, value: RuntimeValue) {
        let store = self.storages.get_or_insert_default(key.into());
        store.with_storage(|store| {
            store.runtime_value = Some(value);
            store.closure = Some(self.clone().0);
        });
    }

    pub fn get_runtime_value_storage(&self, key: impl Into<Path>) -> Option<RuntimeValue> {
        let storage = self.get_storage(key, true)?;
        storage.runtime_value()
    }

    pub fn get_runtime_value_recursive_path(&self, key: impl Into<Path>) -> Option<RuntimeValue> {
        let key = key.into();
        debug!("get_runtime_value_recursive {}", key);

        // Try direct lookup first
        if let Some(runtime_value) = self.get_runtime_value_storage(&key) {
            return Some(runtime_value);
        }

        // If we have a regular value, convert it to runtime value
        if let Some(ast_value) = self.get_value(&key) {
            return Some(RuntimeValue::literal(ast_value));
        }

        None
    }

    pub fn get_runtime_value_mut(&self, key: &str) -> Option<RuntimeValue> {
        // Note: This is a simplified implementation
        // In a full implementation, we'd need to track mutability properly
        self.get_runtime_value_storage(Ident::new(key))
    }
    /// insert type inference
    pub fn insert_type(&self, key: impl Into<Ident>, ty: Ty) {
        let store = self.storages.get_or_insert_default(key.into());
        store.set_ty(ty)
    }
    pub fn get_expr(&self, key: impl Into<Path>) -> Option<Expr> {
        self.get_value(key).map(Expr::value)
    }
    pub fn get_expr_with_ctx(&self, key: impl Into<Path>) -> Option<Expr> {
        let storage = self.get_storage(key, true)?;
        storage.with_storage(|storage| {
            let expr = storage.value.clone().map(Expr::value)?;
            if let Some(closure) = storage.closure.clone() {
                return Some(ExprClosured::new(Self(closure), expr.into()).into());
            }
            Some(expr)
        })
    }
    pub fn get_type(&self, key: impl Into<Path>) -> Option<Ty> {
        let storage = self.get_storage(key, true)?;
        storage.ty()
    }
    pub fn root(&self) -> Self {
        self.get_parent()
            .map(|x| x.root())
            .unwrap_or_else(|| self.clone())
    }
    // TODO: integrate it to optimizers
    pub fn try_get_value_from_expr(&self, expr: &Expr) -> Option<Value> {
        // info!("try_get_value_from_expr {}", expr);
        let ret = match expr.kind() {
            ExprKind::Name(ident) => self.get_value(ident.to_path()),
            ExprKind::Value(value) => Some(value.get()),
            _ => None,
        };
        debug!(
            "try_get_value_from_expr {} => {}",
            expr,
            ret.as_ref().map(|x| x.to_string()).unwrap_or_default()
        );
        ret
    }
    pub fn get_value_recursive(&self, key: impl Into<Path>) -> Option<Value> {
        let key = key.into();
        debug!("get_value_recursive {}", key);
        let expr = self.get_expr(&key)?;
        debug!("get_value_recursive {} => {:?}", key, expr);
        match expr.kind() {
            ExprKind::Name(ident) => self.get_value_recursive(ident.to_path()),
            _ => Some(Value::expr(expr.clone())),
        }
    }
    pub fn get_parent(&self) -> Option<Self> {
        match &self.parent {
            Some(parent) => parent.upgrade().map(Self),
            _ => None,
        }
    }

    pub fn print_values(&self) -> Result<(), crate::Error> {
        if let Some(parent) = self.get_parent() {
            parent.print_values()?;
        }
        self.print_local_values()
    }
    pub fn list_values(&self) -> Vec<Path> {
        let mut values = if let Some(parent) = self.get_parent() {
            parent.list_values()
        } else {
            vec![]
        };
        let mut local_keys: Vec<_> = Vec::new();
        self.storages.for_each(|k, _| {
            local_keys.push(k.clone());
        });
        local_keys.sort();
        values.extend(local_keys.into_iter().map(|x| self.path.with_ident(x)));
        values
    }
}
impl PartialEq for SharedScopedContext {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}
impl Eq for SharedScopedContext {}
