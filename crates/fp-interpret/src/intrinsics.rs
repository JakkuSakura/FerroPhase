//! Runtime operations for const evaluation and interpretation.
//!
//! This module contains the operations (arithmetic, comparison, logical, I/O, string)
//! that can execute during const evaluation and interpretation.

use fp_core::ast::*;
use fp_core::context::SharedScopedContext;
use fp_core::error::Result;
use fp_core::ast::Ident;
use fp_core::ops::BinOpKind;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::sync::Arc;

use crate::error::interpretation_error;

// ===== INTRINSIC FUNCTION TYPES =====

/// Identifier for intrinsic functions - can be either a binary operator or a named function
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum IntrinsicFunctionName {
    BinOpKind(BinOpKind),
    Name(Ident),
}

impl Display for IntrinsicFunctionName {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            IntrinsicFunctionName::BinOpKind(op) => write!(f, "{:?}", op),
            IntrinsicFunctionName::Name(name) => write!(f, "{}", name),
        }
    }
}

/// An intrinsic function that can be called during interpretation
pub struct IntrinsicFunction {
    pub name: IntrinsicFunctionName,
    func: Arc<dyn Fn(&[Value], &SharedScopedContext) -> Result<Value> + Send + Sync>,
}

impl IntrinsicFunction {
    pub fn new(
        name: IntrinsicFunctionName,
        func: impl Fn(&[Value], &SharedScopedContext) -> Result<Value> + Send + Sync + 'static,
    ) -> Self {
        Self {
            name,
            func: Arc::new(func),
        }
    }

    pub fn new_with_ident(
        name: Ident,
        func: impl Fn(&[Value], &SharedScopedContext) -> Result<Value> + Send + Sync + 'static,
    ) -> Self {
        Self::new(IntrinsicFunctionName::Name(name), func)
    }

    pub fn call(&self, args: &[Value], ctx: &SharedScopedContext) -> Result<Value> {
        (self.func)(args, ctx)
    }
}

impl Debug for IntrinsicFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("IntrinsicFunction")
            .field("name", &self.name)
            .finish()
    }
}

// ===== INTRINSICS REGISTRY =====

/// Registry of intrinsic functions available during interpretation
pub struct IntrinsicsRegistry {
    functions: HashMap<String, IntrinsicFunction>,
}

impl IntrinsicsRegistry {
    /// Create a new intrinsics registry with all standard intrinsics
    pub fn new() -> Self {
        let mut registry = Self {
            functions: HashMap::new(),
        };
        registry.register_all_intrinsics();
        registry
    }

    /// Register an intrinsic function
    pub fn register(&mut self, name: impl Into<String>, func: IntrinsicFunction) {
        self.functions.insert(name.into(), func);
    }

    /// Look up an intrinsic function by name
    pub fn get(&self, name: &str) -> Option<&IntrinsicFunction> {
        self.functions.get(name)
    }

    /// Register all standard intrinsics
    fn register_all_intrinsics(&mut self) {
        // Metaprogramming intrinsics
        self.register("sizeof!", intrinsic_sizeof());
        self.register("reflect_fields!", intrinsic_reflect_fields());
        self.register("hasmethod!", intrinsic_hasmethod());
        self.register("type_name!", intrinsic_type_name());

        // Struct creation and manipulation intrinsics
        self.register("create_struct!", intrinsic_create_struct());
        self.register("clone_struct!", intrinsic_clone_struct());
        self.register("addfield!", intrinsic_addfield());

        // Struct querying intrinsics
        self.register("hasfield!", intrinsic_hasfield());
        self.register("field_count!", intrinsic_field_count());
        self.register("method_count!", intrinsic_method_count());
        self.register("field_type!", intrinsic_field_type());
        self.register("struct_size!", intrinsic_struct_size());

        // Code generation intrinsics
        self.register("generate_method!", intrinsic_generate_method());

        // Compile-time validation intrinsics
        self.register("compile_error!", intrinsic_compile_error());
        self.register("compile_warning!", intrinsic_compile_warning());
    }
}

impl Default for IntrinsicsRegistry {
    fn default() -> Self {
        Self::new()
    }
}

// ===== METAPROGRAMMING INTRINSICS =====

/// sizeof! intrinsic - get size of a type in bytes
pub fn intrinsic_sizeof() -> IntrinsicFunction {
    IntrinsicFunction::new_with_ident("sizeof!".into(), move |args, _ctx| {
        if args.len() != 1 {
            return Err(interpretation_error(format!(
                "sizeof! expects 1 argument, got: {:?}",
                args
            )));
        }

        // Debug: print what we received
        // eprintln!("[DEBUG sizeof!] Received argument: {:?}", &args[0]);

        match &args[0] {
            Value::Type(ast_type) => {
                let size = match ast_type {
                    Ty::Expr(expr) => {
                        if let ExprKind::Locator(locator) = expr.as_ref().kind() {
                            if let Some(ident) = locator.as_ident() {
                                match ident.name.as_str() {
                                    "i8" | "u8" => 1,
                                    "i16" | "u16" => 2,
                                    "i32" | "u32" | "f32" => 4,
                                    "i64" | "u64" | "f64" => 8,
                                    "bool" => 1,
                                    "char" => 4,
                                    _ => 8,
                                }
                            } else {
                                8
                            }
                        } else {
                            8
                        }
                    }
                    Ty::Struct(type_struct) => type_struct.fields.len() * 8,
                    _ => 8,
                };
                Ok(Value::int(size as i64))
            }
            _ => Err(interpretation_error(format!(
                "sizeof! expects a type argument, got: {:?}",
                args[0]
            ))),
        }
    })
}

/// reflect_fields! intrinsic - get field information of a struct type
pub fn intrinsic_reflect_fields() -> IntrinsicFunction {
    IntrinsicFunction::new_with_ident("reflect_fields!".into(), move |args, _ctx| {
        if args.len() != 1 {
            return Err(interpretation_error(format!(
                "reflect_fields! expects 1 argument, got: {:?}",
                args
            )));
        }

        match &args[0] {
            Value::Type(Ty::Struct(type_struct)) => {
                let field_descriptors: Vec<Value> = type_struct
                    .fields
                    .iter()
                    .map(|field| {
                        let fields = vec![
                            ValueField {
                                name: "name".into(),
                                value: Value::string(field.name.name.clone()),
                            },
                            ValueField {
                                name: "type_name".into(),
                                value: Value::string(format!("{}", field.value)),
                            },
                        ];
                        Value::Structural(ValueStructural { fields })
                    })
                    .collect();

                Ok(Value::List(ValueList::new(field_descriptors)))
            }
            _ => Err(interpretation_error(format!(
                "reflect_fields! expects a struct type argument, got: {:?}",
                args[0]
            ))),
        }
    })
}

/// hasmethod! intrinsic - check if a type has a specific method
pub fn intrinsic_hasmethod() -> IntrinsicFunction {
    IntrinsicFunction::new_with_ident("hasmethod!".into(), move |args, _ctx| {
        if args.len() != 2 {
            return Err(interpretation_error(format!(
                "hasmethod! expects 2 arguments (type, method_name), got: {:?}",
                args
            )));
        }

        let method_name = match &args[1] {
            Value::String(s) => s.value.as_str(),
            _ => {
                return Err(interpretation_error(format!(
                    "hasmethod! expects a string for method name, got: {:?}",
                    args[1]
                )))
            }
        };

        match &args[0] {
            Value::Type(_) => {
                let has_method = match method_name {
                    "to_string" => true,
                    _ => false,
                };
                Ok(Value::bool(has_method))
            }
            _ => Err(interpretation_error(format!(
                "hasmethod! expects a type argument, got: {:?}",
                args[0]
            ))),
        }
    })
}

/// addfield! intrinsic - add a field to a struct type
pub fn intrinsic_addfield() -> IntrinsicFunction {
    IntrinsicFunction::new_with_ident("addfield!".into(), move |args, _ctx| {
        if args.len() != 3 {
            return Err(interpretation_error(format!(
                "addfield! expects 3 arguments (struct_type, field_name, field_type), got: {:?}",
                args
            )));
        }

        let field_name = match &args[1] {
            Value::String(s) => s.value.clone(),
            _ => {
                return Err(interpretation_error(format!(
                    "addfield! expects a string for field name, got: {:?}",
                    args[1]
                )))
            }
        };

        let field_type = match &args[2] {
            Value::Type(ty) => ty.clone(),
            _ => {
                return Err(interpretation_error(format!(
                    "addfield! expects a type for field type, got: {:?}",
                    args[2]
                )))
            }
        };

        match &args[0] {
            Value::Type(Ty::Struct(type_struct)) => {
                let mut new_fields = type_struct.fields.clone();

                if new_fields.iter().any(|f| f.name.name == field_name) {
                    return Err(interpretation_error(format!(
                        "Field '{}' already exists in struct",
                        field_name
                    )));
                }

                new_fields.push(StructuralField {
                    name: field_name.into(),
                    value: field_type,
                });

                let modified_struct = TypeStruct {
                    name: type_struct.name.clone(),
                    fields: new_fields,
                };

                Ok(Value::Type(Ty::Struct(modified_struct)))
            }
            _ => Err(interpretation_error(format!(
                "addfield! expects a struct type as first argument, got: {:?}",
                args[0]
            ))),
        }
    })
}

/// generate_method! intrinsic - generate a method for a type
pub fn intrinsic_generate_method() -> IntrinsicFunction {
    IntrinsicFunction::new_with_ident("generate_method!".into(), move |args, ctx| {
        if args.len() != 2 {
            return Err(interpretation_error(format!(
                "generate_method! expects 2 arguments (method_name, method_body), got: {:?}",
                args
            )));
        }

        let method_name = match &args[0] {
            Value::String(s) => s.value.clone(),
            _ => {
                return Err(interpretation_error(format!(
                    "generate_method! expects a string for method name, got: {:?}",
                    args[0]
                )))
            }
        };

        ctx.root().print_str(format!(
            "generate_method!: Generating method '{}' for current type",
            method_name
        ));

        Ok(Value::unit())
    })
}

/// type_name! intrinsic - get the name of a type as a string
pub fn intrinsic_type_name() -> IntrinsicFunction {
    IntrinsicFunction::new_with_ident("type_name!".into(), move |args, _ctx| {
        if args.len() != 1 {
            return Err(interpretation_error(format!(
                "type_name! expects 1 argument, got: {:?}",
                args
            )));
        }

        let type_name = match &args[0] {
            Value::Type(ast_type) => format!("{}", ast_type),
            other => match other {
                Value::Int(_) => "i64".to_string(),
                Value::Bool(_) => "bool".to_string(),
                Value::Decimal(_) => "f64".to_string(),
                Value::String(_) => "String".to_string(),
                Value::Unit(_) => "()".to_string(),
                _ => "unknown".to_string(),
            },
        };

        Ok(Value::string(type_name))
    })
}

/// create_struct! intrinsic - create a new struct type dynamically
pub fn intrinsic_create_struct() -> IntrinsicFunction {
    IntrinsicFunction::new_with_ident("create_struct!".into(), move |args, _ctx| {
        if args.len() != 1 {
            return Err(interpretation_error(format!(
                "create_struct! expects 1 argument (struct_name), got: {:?}",
                args
            )));
        }

        let struct_name = match &args[0] {
            Value::String(s) => s.value.clone(),
            _ => {
                return Err(interpretation_error(format!(
                    "create_struct! expects a string for struct name, got: {:?}",
                    args[0]
                )))
            }
        };

        let struct_type = TypeStruct {
            name: struct_name.into(),
            fields: Vec::new(),
        };

        Ok(Value::Type(Ty::Struct(struct_type)))
    })
}

/// clone_struct! intrinsic - clone an existing struct type
pub fn intrinsic_clone_struct() -> IntrinsicFunction {
    IntrinsicFunction::new_with_ident("clone_struct!".into(), move |args, _ctx| {
        if args.len() != 1 {
            return Err(interpretation_error(format!(
                "clone_struct! expects 1 argument (struct_type), got: {:?}",
                args
            )));
        }

        match &args[0] {
            Value::Type(Ty::Struct(type_struct)) => {
                let cloned_struct = TypeStruct {
                    name: format!("Clone_{}", type_struct.name.name).into(),
                    fields: type_struct.fields.clone(),
                };

                Ok(Value::Type(Ty::Struct(cloned_struct)))
            }
            _ => Err(interpretation_error(format!(
                "clone_struct! expects a struct type argument, got: {:?}",
                args[0]
            ))),
        }
    })
}

/// hasfield! intrinsic - check if a struct has a specific field
pub fn intrinsic_hasfield() -> IntrinsicFunction {
    IntrinsicFunction::new_with_ident("hasfield!".into(), move |args, _ctx| {
        if args.len() != 2 {
            return Err(interpretation_error(format!(
                "hasfield! expects 2 arguments (struct_type, field_name), got: {:?}",
                args
            )));
        }

        let field_name = match &args[1] {
            Value::String(s) => &s.value,
            _ => {
                return Err(interpretation_error(format!(
                    "hasfield! expects a string for field name, got: {:?}",
                    args[1]
                )))
            }
        };

        let has_field = match &args[0] {
            Value::Type(Ty::Struct(type_struct)) => type_struct
                .fields
                .iter()
                .any(|field| field.name.name == *field_name),
            Value::Struct(value_struct) => value_struct
                .ty
                .fields
                .iter()
                .any(|field| field.name.name == *field_name),
            _ => {
                return Err(interpretation_error(format!(
                    "hasfield! expects a struct type or instance, got: {:?}",
                    args[0]
                )))
            }
        };

        Ok(Value::bool(has_field))
    })
}

/// field_count! intrinsic - get the number of fields in a struct
pub fn intrinsic_field_count() -> IntrinsicFunction {
    IntrinsicFunction::new_with_ident("field_count!".into(), move |args, _ctx| {
        if args.len() != 1 {
            return Err(interpretation_error(format!(
                "field_count! expects 1 argument (struct_type), got: {:?}",
                args
            )));
        }

        let field_count = match &args[0] {
            Value::Type(Ty::Struct(type_struct)) => type_struct.fields.len(),
            Value::Struct(value_struct) => value_struct.ty.fields.len(),
            _ => {
                return Err(interpretation_error(format!(
                    "field_count! expects a struct type or instance, got: {:?}",
                    args[0]
                )))
            }
        };

        Ok(Value::int(field_count as i64))
    })
}

/// method_count! intrinsic - get the number of methods in a struct
pub fn intrinsic_method_count() -> IntrinsicFunction {
    IntrinsicFunction::new_with_ident("method_count!".into(), move |args, _ctx| {
        if args.len() != 1 {
            return Err(interpretation_error(format!(
                "method_count! expects 1 argument (struct_type), got: {:?}",
                args
            )));
        }

        let method_count = match &args[0] {
            Value::Type(Ty::Struct(_type_struct)) => 0,
            Value::Struct(_value_struct) => 0,
            _ => {
                return Err(interpretation_error(format!(
                    "method_count! expects a struct type or instance, got: {:?}",
                    args[0]
                )))
            }
        };

        Ok(Value::int(method_count as i64))
    })
}

/// field_type! intrinsic - get the type of a specific field
pub fn intrinsic_field_type() -> IntrinsicFunction {
    IntrinsicFunction::new_with_ident("field_type!".into(), move |args, _ctx| {
        if args.len() != 2 {
            return Err(interpretation_error(format!(
                "field_type! expects 2 arguments (struct_type, field_name), got: {:?}",
                args
            )));
        }

        let field_name = match &args[1] {
            Value::String(s) => &s.value,
            _ => {
                return Err(interpretation_error(format!(
                    "field_type! expects a string for field name, got: {:?}",
                    args[1]
                )))
            }
        };

        match &args[0] {
            Value::Type(Ty::Struct(type_struct)) => {
                if let Some(field) = type_struct
                    .fields
                    .iter()
                    .find(|f| f.name.name == *field_name)
                {
                    Ok(Value::Type(field.value.clone()))
                } else {
                    Err(interpretation_error(format!(
                        "Field '{}' not found in struct",
                        field_name
                    )))
                }
            }
            Value::Struct(value_struct) => {
                if let Some(field) = value_struct
                    .ty
                    .fields
                    .iter()
                    .find(|f| f.name.name == *field_name)
                {
                    Ok(Value::Type(field.value.clone()))
                } else {
                    Err(interpretation_error(format!(
                        "Field '{}' not found in struct",
                        field_name
                    )))
                }
            }
            _ => Err(interpretation_error(format!(
                "field_type! expects a struct type or instance, got: {:?}",
                args[0]
            ))),
        }
    })
}

/// struct_size! intrinsic - get the size of a struct in bytes
pub fn intrinsic_struct_size() -> IntrinsicFunction {
    IntrinsicFunction::new_with_ident("struct_size!".into(), move |args, _ctx| {
        if args.len() != 1 {
            return Err(interpretation_error(format!(
                "struct_size! expects 1 argument (struct_type), got: {:?}",
                args
            )));
        }

        let size = match &args[0] {
            Value::Type(Ty::Struct(type_struct)) => type_struct
                .fields
                .iter()
                .map(|field| calculate_field_size(&field.value))
                .sum::<usize>(),
            Value::Struct(value_struct) => value_struct
                .ty
                .fields
                .iter()
                .map(|field| calculate_field_size(&field.value))
                .sum::<usize>(),
            _ => {
                return Err(interpretation_error(format!(
                    "struct_size! expects a struct type or instance, got: {:?}",
                    args[0]
                )))
            }
        };

        Ok(Value::int(size as i64))
    })
}

fn calculate_field_size(ty: &Ty) -> usize {
    match ty {
        Ty::Expr(expr) => {
            if let ExprKind::Locator(locator) = expr.as_ref().kind() {
                if let Some(ident) = locator.as_ident() {
                    match ident.name.as_str() {
                        "i8" | "u8" => 1,
                        "i16" | "u16" => 2,
                        "i32" | "u32" | "f32" => 4,
                        "i64" | "u64" | "f64" => 8,
                        "bool" => 1,
                        "char" => 4,
                        "String" => 24,
                        _ => 8,
                    }
                } else {
                    8
                }
            } else {
                8
            }
        }
        Ty::Struct(nested) => nested.fields.len() * 8,
        _ => 8,
    }
}

/// compile_error! intrinsic - generate a compile-time error
pub fn intrinsic_compile_error() -> IntrinsicFunction {
    IntrinsicFunction::new_with_ident("compile_error!".into(), move |args, _ctx| {
        if args.len() != 1 {
            return Err(interpretation_error(format!(
                "compile_error! expects 1 argument (error_message), got: {:?}",
                args
            )));
        }

        let error_message = match &args[0] {
            Value::String(s) => &s.value,
            _ => {
                return Err(interpretation_error(format!(
                    "compile_error! expects a string message, got: {:?}",
                    args[0]
                )))
            }
        };

        Err(interpretation_error(format!(
            "Compile-time error: {}",
            error_message
        )))
    })
}

/// compile_warning! intrinsic - generate a compile-time warning
pub fn intrinsic_compile_warning() -> IntrinsicFunction {
    IntrinsicFunction::new_with_ident("compile_warning!".into(), move |args, ctx| {
        if args.len() != 1 {
            return Err(interpretation_error(format!(
                "compile_warning! expects 1 argument (warning_message), got: {:?}",
                args
            )));
        }

        let warning_message = match &args[0] {
            Value::String(s) => &s.value,
            _ => {
                return Err(interpretation_error(format!(
                    "compile_warning! expects a string message, got: {:?}",
                    args[0]
                )))
            }
        };

        ctx.root()
            .print_str(format!("Warning: {}", warning_message));

        Ok(Value::unit())
    })
}
