use crate::ast::{Expr, ExprId};
use crate::ast::{FunctionParam, Ty, Value};
use crate::ctx::Context;
use crate::diagnostics::report_error;
use crate::error::Result;
use std::collections::HashMap;
use std::sync::RwLock;

/// Unique identifier for types in the registry
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(pub u64);

static TYPE_ID_COUNTER: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);

impl TypeId {
    pub fn new() -> Self {
        TypeId(TYPE_ID_COUNTER.fetch_add(1, std::sync::atomic::Ordering::SeqCst))
    }
}

/// Information about a field for reflection
#[derive(Debug, Clone)]
pub struct FieldInfo {
    pub name: String,
    pub type_id: TypeId,
    pub ast_type: Ty,
    pub attributes: Vec<String>,
}

/// Information about a method for reflection
#[derive(Debug, Clone)]
pub struct MethodInfo {
    pub name: String,
    pub params: Vec<FunctionParam>,
    pub return_type: Option<Ty>,
    pub attributes: Vec<String>,
}

/// Complete type information for introspection
#[derive(Debug, Clone)]
pub struct TypeInfo {
    pub id: TypeId,
    pub name: String,
    pub ast_type: Ty,
    pub size_bytes: Option<usize>,
    pub fields: Vec<FieldInfo>,
    pub methods: Vec<MethodInfo>,
    pub traits_implemented: Vec<String>,
}

/// Central registry for type information and introspection
#[derive(Debug, Default)]
pub struct TypeRegistry {
    types: RwLock<HashMap<TypeId, TypeInfo>>,
    name_to_id: RwLock<HashMap<String, TypeId>>,
    primitive_sizes: RwLock<HashMap<String, usize>>,
}

impl TypeRegistry {
    pub fn new() -> Self {
        let registry = Self::default();
        registry.initialize_primitives();
        registry
    }

    fn initialize_primitives(&self) {
        let mut sizes = self.primitive_sizes.write().unwrap();
        sizes.insert("i8".to_string(), 1);
        sizes.insert("i16".to_string(), 2);
        sizes.insert("i32".to_string(), 4);
        sizes.insert("i64".to_string(), 8);
        sizes.insert("u8".to_string(), 1);
        sizes.insert("u16".to_string(), 2);
        sizes.insert("u32".to_string(), 4);
        sizes.insert("u64".to_string(), 8);
        sizes.insert("f32".to_string(), 4);
        sizes.insert("f64".to_string(), 8);
        sizes.insert("bool".to_string(), 1);
        sizes.insert("char".to_string(), 4);
    }

    /// Register a new type in the registry
    pub fn register_type(&self, type_info: TypeInfo) -> TypeId {
        let mut types = self.types.write().unwrap();
        let mut name_to_id = self.name_to_id.write().unwrap();

        types.insert(type_info.id, type_info.clone());
        name_to_id.insert(type_info.name.clone(), type_info.id);

        type_info.id
    }

    /// Get type information by ID
    pub fn get_type_info(&self, type_id: TypeId) -> Option<TypeInfo> {
        self.types.read().unwrap().get(&type_id).cloned()
    }

    /// Get type information by name
    pub fn get_type_by_name(&self, name: &str) -> Option<TypeInfo> {
        let name_to_id = self.name_to_id.read().unwrap();
        let type_id = *name_to_id.get(name)?;
        self.get_type_info(type_id)
    }

    /// Get size of a type in bytes
    pub fn get_size(&self, type_id: TypeId) -> Result<usize> {
        if let Some(type_info) = self.get_type_info(type_id) {
            if let Some(size) = type_info.size_bytes {
                return Ok(size);
            }

            // Calculate size based on type
            match &type_info.ast_type {
                Ty::Expr(expr) => {
                    if let crate::ast::ExprKind::Locator(locator) = expr.as_ref().kind() {
                        if let Some(ident) = locator.as_ident() {
                            let sizes = self.primitive_sizes.read().unwrap();
                            return sizes.get(&ident.name).copied().ok_or_else(|| {
                                report_error(format!("Unknown primitive type: {}", ident.name))
                            });
                        }
                    }
                    Ok(8) // Default size for complex expressions
                }
                Ty::Struct(_) => {
                    // Calculate struct size as sum of field sizes
                    let mut total_size = 0;
                    for field in &type_info.fields {
                        total_size += self.get_size(field.type_id)?;
                    }
                    Ok(total_size)
                }
                _ => Ok(8), // Default size
            }
        } else {
            Err(report_error(format!("Type not found: {:?}", type_id)))
        }
    }

    /// Get fields of a struct type
    pub fn get_fields(&self, type_id: TypeId) -> Result<Vec<FieldInfo>> {
        if let Some(type_info) = self.get_type_info(type_id) {
            Ok(type_info.fields)
        } else {
            Err(report_error(format!("Type not found: {:?}", type_id)))
        }
    }

    /// Check if a type has a specific method
    pub fn type_has_method(&self, type_id: TypeId, method_name: &str) -> bool {
        if let Some(type_info) = self.get_type_info(type_id) {
            type_info
                .methods
                .iter()
                .any(|method| method.name == method_name)
        } else {
            false
        }
    }

    /// Get methods of a type
    pub fn get_methods(&self, type_id: TypeId) -> Result<Vec<MethodInfo>> {
        if let Some(type_info) = self.get_type_info(type_id) {
            Ok(type_info.methods)
        } else {
            Err(report_error(format!("Type not found: {:?}", type_id)))
        }
    }

    /// Add a method to an existing type
    pub fn add_method(&self, type_id: TypeId, method: MethodInfo) -> Result<()> {
        let mut types = self.types.write().unwrap();
        if let Some(type_info) = types.get_mut(&type_id) {
            type_info.methods.push(method);
            Ok(())
        } else {
            Err(report_error(format!("Type not found: {:?}", type_id)))
        }
    }

    /// Add a field to an existing struct type (for metaprogramming)
    pub fn add_field(&self, type_id: TypeId, field: FieldInfo) -> Result<()> {
        let mut types = self.types.write().unwrap();
        if let Some(type_info) = types.get_mut(&type_id) {
            type_info.fields.push(field);
            // Invalidate cached size
            type_info.size_bytes = None;
            Ok(())
        } else {
            Err(report_error(format!("Type not found: {:?}", type_id)))
        }
    }

    /// Get all registered types
    pub fn list_types(&self) -> Vec<(TypeId, String)> {
        let types = self.types.read().unwrap();
        types
            .iter()
            .map(|(id, info)| (*id, info.name.clone()))
            .collect()
    }
}

pub trait TypeSystem {
    fn get_ty_from_expr(&self, ctx: &Context, expr: &Expr) -> Result<Ty> {
        let _ = ctx;
        let _ = expr;
        unimplemented!()
    }
    fn get_ty_from_expr_id(&self, ctx: &Context, id: ExprId) -> Result<Ty> {
        let _ = ctx;
        let _ = id;
        unimplemented!()
    }
    fn get_ty_from_value(&self, ctx: &Context, value: &Value) -> Result<Ty> {
        let _ = ctx;
        let _ = value;
        unimplemented!()
    }
    fn get_ty_from_value_id(&self, ctx: &Context, id: u32) -> Result<Ty> {
        let _ = ctx;
        let _ = id;
        unimplemented!()
    }

    /// Get the type registry for introspection (if supported)
    fn get_type_registry(&self) -> Option<&TypeRegistry> {
        None
    }
}

impl TypeSystem for () {}
