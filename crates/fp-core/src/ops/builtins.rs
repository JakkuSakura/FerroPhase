use super::runtime_format::{format_runtime_string, format_value_with_spec};
use crate::ast::*;
use crate::bail;
use crate::context::SharedScopedContext;
use crate::id::Ident;
use crate::ops::BinOpKind;
use serde::{Deserialize, Serialize};
use std::fmt::{Debug, Display, Formatter};
use std::sync::Arc;

#[derive(Clone, Serialize, Deserialize, PartialEq)]
pub enum BuiltinFnName {
    BinOpKind(BinOpKind),
    Name(Ident),
}
impl Display for BuiltinFnName {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            BuiltinFnName::BinOpKind(k) => std::fmt::Display::fmt(k, f),
            BuiltinFnName::Name(n) => std::fmt::Display::fmt(n, f),
        }
    }
}
impl Debug for BuiltinFnName {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            BuiltinFnName::BinOpKind(k) => Debug::fmt(k, f),
            BuiltinFnName::Name(n) => std::fmt::Debug::fmt(n, f),
        }
    }
}
#[derive(Clone)]
pub struct BuiltinFn {
    pub name: BuiltinFnName,
    func: Arc<dyn Fn(&[Value], &SharedScopedContext) -> Result<Value, crate::Error> + Send + Sync>,
}
impl BuiltinFn {
    pub fn new(
        name: BinOpKind,
        f: impl Fn(&[Value], &SharedScopedContext) -> Result<Value, crate::Error>
            + Send
            + Sync
            + 'static,
    ) -> Self {
        Self {
            name: BuiltinFnName::BinOpKind(name),
            func: Arc::new(f),
        }
    }
    pub fn new_with_ident(
        name: Ident,
        f: impl Fn(&[Value], &SharedScopedContext) -> Result<Value, crate::Error>
            + Send
            + Sync
            + 'static,
    ) -> Self {
        Self {
            name: BuiltinFnName::Name(name),
            func: Arc::new(f),
        }
    }
    pub fn invoke(&self, args: &[Value], ctx: &SharedScopedContext) -> Result<Value, crate::Error> {
        (self.func)(args, ctx)
    }
}

impl Debug for BuiltinFn {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("BuiltinFn")
            .field("name", &self.name)
            .finish_non_exhaustive()
    }
}
impl Serialize for BuiltinFn {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        self.name.serialize(serializer)
    }
}
impl<'de> Deserialize<'de> for BuiltinFn {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let name = BuiltinFnName::deserialize(deserializer)?;
        Ok(Self {
            name,
            func: Arc::new(|_, _| unreachable!()),
        })
    }
}

impl PartialEq for BuiltinFn {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}
impl Eq for BuiltinFn {}
pub fn operate_on_literals(
    name: BinOpKind,
    op_i64: impl Fn(&[i64]) -> i64 + Send + Sync + 'static,
    op_f64: impl Fn(&[f64]) -> f64 + Send + Sync + 'static,
) -> BuiltinFn {
    BuiltinFn::new(name, move |args, _ctx| {
        let mut args_i64 = vec![];
        let mut args_f64 = vec![];
        for arg in args {
            match arg {
                Value::Int(x) => args_i64.push(x.value),
                Value::Decimal(x) => args_f64.push(x.value),
                _ => bail!("Does not support argument type {:?}", args),
            }
        }
        if !args_i64.is_empty() && !args_f64.is_empty() {
            bail!("Does not support argument type {:?}", args)
        }
        if !args_i64.is_empty() {
            return Ok(Value::int(op_i64(&args_i64)));
        }
        if !args_f64.is_empty() {
            return Ok(Value::decimal(op_f64(&args_f64)));
        }
        bail!("Does not support argument type {:?}", args)
    })
}
pub fn binary_comparison_on_literals(
    name: BinOpKind,
    op_i64: impl Fn(i64, i64) -> bool + Send + Sync + 'static,
    op_f64: impl Fn(f64, f64) -> bool + Send + Sync + 'static,
) -> BuiltinFn {
    BuiltinFn::new(name, move |args, _ctx| {
        if args.len() != 2 {
            bail!("Argument expected 2, got: {:?}", args)
        }
        let mut args_i64 = vec![];
        let mut args_f64 = vec![];
        let mut args_string = vec![];
        let mut args_bool = vec![];

        for arg in args {
            match arg {
                Value::Int(x) => args_i64.push(x.value),
                Value::Decimal(x) => args_f64.push(x.value),
                Value::String(x) => args_string.push(x.value.clone()),
                Value::Bool(x) => args_bool.push(x.value),
                _ => bail!("Does not support argument type {:?}", args),
            }
        }

        // Count non-empty argument type vectors
        let type_count = [
            !args_i64.is_empty(),
            !args_f64.is_empty(),
            !args_string.is_empty(),
            !args_bool.is_empty(),
        ]
        .iter()
        .filter(|&&x| x)
        .count();

        if type_count > 1 {
            bail!("Cannot compare different types: {:?}", args)
        }

        if !args_i64.is_empty() {
            return Ok(Value::bool(op_i64(args_i64[0], args_i64[1])));
        }
        if !args_f64.is_empty() {
            return Ok(Value::bool(op_f64(args_f64[0], args_f64[1])));
        }
        if !args_string.is_empty() {
            // For string comparison, use string equality
            return Ok(Value::bool(args_string[0] == args_string[1]));
        }
        if !args_bool.is_empty() {
            // For boolean comparison
            return Ok(Value::bool(args_bool[0] == args_bool[1]));
        }

        bail!("Does not support argument type {:?}", args)
    })
}
pub fn builtin_add() -> BuiltinFn {
    BuiltinFn::new(BinOpKind::Add, move |args, _ctx| {
        if args.len() != 2 {
            bail!("Add expects 2 arguments, got: {:?}", args)
        }

        match (&args[0], &args[1]) {
            // String concatenation
            (Value::String(a), Value::String(b)) => {
                let mut result = a.value.clone();
                result.push_str(&b.value);
                Ok(Value::string(result))
            }
            // Numeric addition
            (Value::Int(a), Value::Int(b)) => Ok(Value::int(a.value + b.value)),
            (Value::Decimal(a), Value::Decimal(b)) => Ok(Value::decimal(a.value + b.value)),
            (Value::Int(a), Value::Decimal(b)) => Ok(Value::decimal(a.value as f64 + b.value)),
            (Value::Decimal(a), Value::Int(b)) => Ok(Value::decimal(a.value + b.value as f64)),
            _ => bail!(
                "Add operation not supported for types: {:?} + {:?}",
                args[0],
                args[1]
            ),
        }
    })
}
pub fn builtin_sub() -> BuiltinFn {
    operate_on_literals(
        BinOpKind::Sub,
        |x| {
            x.into_iter()
                .enumerate()
                .map(|(i, &x)| if i > 0 { -x } else { x })
                .sum()
        },
        |x| {
            x.into_iter()
                .enumerate()
                .map(|(i, &x)| if i > 0 { -x } else { x })
                .sum()
        },
    )
}

pub fn builtin_mul() -> BuiltinFn {
    operate_on_literals(
        BinOpKind::Mul,
        |x| x.into_iter().product(),
        |x| x.into_iter().product(),
    )
}

pub fn builtin_gt() -> BuiltinFn {
    binary_comparison_on_literals(BinOpKind::Gt, |x, y| x > y, |x, y| x > y)
}

pub fn builtin_ge() -> BuiltinFn {
    binary_comparison_on_literals(BinOpKind::Ge, |x, y| x >= y, |x, y| x >= y)
}
pub fn builtin_lt() -> BuiltinFn {
    binary_comparison_on_literals(BinOpKind::Lt, |x, y| x < y, |x, y| x < y)
}
pub fn builtin_le() -> BuiltinFn {
    binary_comparison_on_literals(BinOpKind::Le, |x, y| x <= y, |x, y| x <= y)
}
pub fn builtin_eq() -> BuiltinFn {
    binary_comparison_on_literals(BinOpKind::Eq, |x, y| x == y, |x, y| x == y)
}
pub fn builtin_ne() -> BuiltinFn {
    binary_comparison_on_literals(BinOpKind::Ne, |x, y| x != y, |x, y| x != y)
}

pub fn builtin_and() -> BuiltinFn {
    BuiltinFn::new(BinOpKind::And, move |args, _ctx| {
        if args.len() != 2 {
            bail!("And expects 2 arguments, got: {:?}", args)
        }
        match (&args[0], &args[1]) {
            (Value::Bool(a), Value::Bool(b)) => Ok(Value::bool(a.value && b.value)),
            _ => bail!(
                "And operation requires boolean operands, got: {:?} && {:?}",
                args[0],
                args[1]
            ),
        }
    })
}

pub fn builtin_or() -> BuiltinFn {
    BuiltinFn::new(BinOpKind::Or, move |args, _ctx| {
        if args.len() != 2 {
            bail!("Or expects 2 arguments, got: {:?}", args)
        }
        match (&args[0], &args[1]) {
            (Value::Bool(a), Value::Bool(b)) => Ok(Value::bool(a.value || b.value)),
            _ => bail!(
                "Or operation requires boolean operands, got: {:?} || {:?}",
                args[0],
                args[1]
            ),
        }
    })
}

pub fn builtin_print(_se: Arc<dyn AstSerializer>) -> BuiltinFn {
    BuiltinFn::new_with_ident("print".into(), move |args, ctx| {
        use crate::context::ExecutionMode;

        // Format all arguments and join with spaces (Python-like behavior)
        let formatted: Vec<String> = args
            .iter()
            .map(|value| format_value_for_print(value))
            .collect();

        let output = if formatted.is_empty() {
            String::new()
        } else {
            formatted.join(" ")
        };

        tracing::debug!(
            "builtin_print called with mode: {:?}",
            ctx.root().execution_mode()
        );
        match ctx.root().execution_mode() {
            ExecutionMode::CompileTime => {
                // Execute immediately during const evaluation
                tracing::debug!("Executing print at compile-time: {}", output);
                // Python-like print: add newline
                ctx.root().print_str(format!("{}\n", output));
            }
            ExecutionMode::Runtime => {
                // TODO: Generate side effect for runtime print call
                // For now, create a runtime value that represents a print call
                // This should be handled by the pipeline to generate actual LLVM calls
                tracing::debug!("Runtime print call: {}", output);
                // The call should be preserved in the AST for later compilation phases
            }
        }

        Ok(Value::unit())
    })
}

/// println! macro - print arguments with newline
pub fn builtin_println(_se: Arc<dyn AstSerializer>) -> BuiltinFn {
    BuiltinFn::new_with_ident("println!".into(), move |args, ctx| {
        use crate::context::ExecutionMode;

        let output = if args.is_empty() {
            String::new()
        } else if args.len() == 1 {
            format_value_with_spec(&args[0], None)?
        } else {
            let format_str = format_value_with_spec(&args[0], None)?;
            format_runtime_string(&format_str, &args[1..])?
        };

        tracing::debug!(
            "builtin_println called with mode: {:?}",
            ctx.root().execution_mode()
        );
        match ctx.root().execution_mode() {
            ExecutionMode::CompileTime => {
                // Execute immediately during const evaluation
                tracing::debug!("Executing printf at compile-time: {}", output);
                ctx.root().print_str(format!("{}\n", output));
            }
            ExecutionMode::Runtime => {
                // TODO: Generate side effect for runtime printf call
                // For now, create a runtime value that represents a printf call
                // This should be handled by the pipeline to generate actual LLVM calls
                tracing::debug!("Runtime printf call: {}", output);
                // The call should be preserved in the AST for later compilation phases
            }
        }

        Ok(Value::unit())
    })
}

fn format_value(value: &Value) -> String {
    match value {
        Value::String(s) => s.value.clone(),
        Value::Int(i) => i.value.to_string(),
        Value::Bool(b) => b.value.to_string(),
        Value::Decimal(d) => d.value.to_string(),
        Value::Unit(_) => "()".to_string(),
        _ => format!("{:?}", value), // Fallback for complex types
    }
}

/// Format a value for the `print!` helpers. Currently reuses the default formatting logic.
fn format_value_for_print(value: &Value) -> String {
    format_value(value)
}

fn builtin_strlen_named(name: &str) -> BuiltinFn {
    let ident = Ident::new(name);
    let name_owned = name.to_string();
    BuiltinFn::new_with_ident(ident, move |args, _ctx| {
        let name = &name_owned;
        if args.len() != 1 {
            bail!("{} expects 1 argument, got: {:?}", name, args);
        }

        match &args[0] {
            Value::String(s) => Ok(Value::int(s.value.len() as i64)),
            Value::Bytes(b) => Ok(Value::int(b.value.len() as i64)),
            other => bail!("{} expects a string-like argument, got: {:?}", name, other),
        }
    })
}

fn builtin_concat_named(name: &str) -> BuiltinFn {
    let ident = Ident::new(name);
    BuiltinFn::new_with_ident(ident, move |args, _ctx| {
        if args.is_empty() {
            return Ok(Value::string(String::new()));
        }

        let mut result = String::new();
        for value in args {
            result.push_str(&format_value(value));
        }

        Ok(Value::string(result))
    })
}

pub fn builtin_strlen() -> BuiltinFn {
    builtin_strlen_named("strlen!")
}

pub fn builtin_strlen_fn() -> BuiltinFn {
    builtin_strlen_named("strlen")
}

pub fn builtin_concat() -> BuiltinFn {
    builtin_concat_named("concat!")
}

pub fn builtin_concat_fn() -> BuiltinFn {
    builtin_concat_named("concat")
}
pub fn builtin_some() -> BuiltinFn {
    BuiltinFn::new_with_ident("Some".into(), move |args, _ctx| {
        if args.len() != 1 {
            bail!("Some expects 1 argument, got: {:?}", args)
        }
        Ok(Value::Some(ValueSome::new(args[0].clone().into())))
    })
}

// ===== METAPROGRAMMING INTRINSICS =====

/// sizeof! intrinsic - get size of a type in bytes
pub fn builtin_sizeof() -> BuiltinFn {
    BuiltinFn::new_with_ident("sizeof!".into(), move |args, _ctx| {
        if args.len() != 1 {
            bail!("sizeof! expects 1 argument, got: {:?}", args)
        }

        match &args[0] {
            Value::Type(ast_type) => {
                // For now, return estimated sizes for common types
                let size = match ast_type {
                    Ty::Expr(expr) => {
                        if let crate::ast::Expr::Locator(locator) = expr.as_ref() {
                            if let Some(ident) = locator.as_ident() {
                                match ident.name.as_str() {
                                    "i8" | "u8" => 1,
                                    "i16" | "u16" => 2,
                                    "i32" | "u32" | "f32" => 4,
                                    "i64" | "u64" | "f64" => 8,
                                    "bool" => 1,
                                    "char" => 4,
                                    _ => 8, // Default size for unknown types
                                }
                            } else {
                                8 // Default size for complex expressions
                            }
                        } else {
                            8 // Default size for complex expressions
                        }
                    }
                    Ty::Struct(type_struct) => {
                        // Calculate struct size as sum of field sizes
                        type_struct.fields.len() * 8 // Simplified: assume each field is 8 bytes
                    }
                    _ => 8, // Default size for other types
                };
                Ok(Value::int(size as i64))
            }
            _ => bail!("sizeof! expects a type argument, got: {:?}", args[0]),
        }
    })
}

/// reflect_fields! intrinsic - get field information of a struct type
pub fn builtin_reflect_fields() -> BuiltinFn {
    BuiltinFn::new_with_ident("reflect_fields!".into(), move |args, _ctx| {
        if args.len() != 1 {
            bail!("reflect_fields! expects 1 argument, got: {:?}", args)
        }

        match &args[0] {
            Value::Type(Ty::Struct(type_struct)) => {
                let field_descriptors: Vec<Value> = type_struct
                    .fields
                    .iter()
                    .map(|field| {
                        // Create a field descriptor struct
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
            _ => bail!(
                "reflect_fields! expects a struct type argument, got: {:?}",
                args[0]
            ),
        }
    })
}

/// hasmethod! intrinsic - check if a type has a specific method
pub fn builtin_hasmethod() -> BuiltinFn {
    BuiltinFn::new_with_ident("hasmethod!".into(), move |args, _ctx| {
        if args.len() != 2 {
            bail!(
                "hasmethod! expects 2 arguments (type, method_name), got: {:?}",
                args
            )
        }

        let method_name = match &args[1] {
            Value::String(s) => s.value.as_str(),
            _ => bail!(
                "hasmethod! expects a string for method name, got: {:?}",
                args[1]
            ),
        };

        // For now, return false for all methods since we don't have a complete method registry
        // This would be enhanced when the full type system is implemented
        match &args[0] {
            Value::Type(_) => {
                // Basic hardcoded method checks for common types
                let has_method = match method_name {
                    "to_string" => true, // Most types can be converted to string
                    _ => false,
                };
                Ok(Value::bool(has_method))
            }
            _ => bail!("hasmethod! expects a type argument, got: {:?}", args[0]),
        }
    })
}

/// addfield! intrinsic - add a field to a struct type
pub fn builtin_addfield() -> BuiltinFn {
    BuiltinFn::new_with_ident("addfield!".into(), move |args, _ctx| {
        if args.len() != 3 {
            bail!(
                "addfield! expects 3 arguments (struct_type, field_name, field_type), got: {:?}",
                args
            )
        }

        let field_name = match &args[1] {
            Value::String(s) => s.value.clone(),
            _ => bail!(
                "addfield! expects a string for field name, got: {:?}",
                args[1]
            ),
        };

        let field_type = match &args[2] {
            Value::Type(ty) => ty.clone(),
            _ => bail!(
                "addfield! expects a type for field type, got: {:?}",
                args[2]
            ),
        };

        match &args[0] {
            Value::Type(Ty::Struct(type_struct)) => {
                // Create a new struct with the additional field
                let mut new_fields = type_struct.fields.clone();

                // Check for duplicate field names
                if new_fields.iter().any(|f| f.name.name == field_name) {
                    bail!("Field '{}' already exists in struct", field_name);
                }

                // Add the new field
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
            _ => bail!(
                "addfield! expects a struct type as first argument, got: {:?}",
                args[0]
            ),
        }
    })
}

/// generate_method! intrinsic - generate a method for a type
pub fn builtin_generate_method() -> BuiltinFn {
    BuiltinFn::new_with_ident("generate_method!".into(), move |args, ctx| {
        if args.len() != 2 {
            bail!(
                "generate_method! expects 2 arguments (method_name, method_body), got: {:?}",
                args
            )
        }

        let method_name = match &args[0] {
            Value::String(s) => s.value.clone(),
            _ => bail!(
                "generate_method! expects a string for method name, got: {:?}",
                args[0]
            ),
        };

        let _method_body = &args[1]; // Could be a string or expression

        // For now, just record that we want to generate this method
        ctx.root().print_str(format!(
            "generate_method!: Generating method '{}' for current type",
            method_name
        ));

        Ok(Value::unit())
    })
}

/// type_name! intrinsic - get the name of a type as a string
pub fn builtin_type_name() -> BuiltinFn {
    BuiltinFn::new_with_ident("type_name!".into(), move |args, _ctx| {
        if args.len() != 1 {
            bail!("type_name! expects 1 argument, got: {:?}", args)
        }

        let type_name = match &args[0] {
            Value::Type(ast_type) => {
                format!("{}", ast_type)
            }
            other => {
                // Return the type of the value
                match other {
                    Value::Int(_) => "i64".to_string(),
                    Value::Bool(_) => "bool".to_string(),
                    Value::Decimal(_) => "f64".to_string(),
                    Value::String(_) => "String".to_string(),
                    Value::Unit(_) => "()".to_string(),
                    _ => "unknown".to_string(),
                }
            }
        };

        Ok(Value::string(type_name))
    })
}

// ===== ENHANCED CONST EVALUATION INTRINSICS =====

/// create_struct! intrinsic - create a new struct type dynamically
pub fn builtin_create_struct() -> BuiltinFn {
    BuiltinFn::new_with_ident("create_struct!".into(), move |args, _ctx| {
        if args.len() != 1 {
            bail!(
                "create_struct! expects 1 argument (struct_name), got: {:?}",
                args
            )
        }

        let struct_name = match &args[0] {
            Value::String(s) => s.value.clone(),
            _ => bail!(
                "create_struct! expects a string for struct name, got: {:?}",
                args[0]
            ),
        };

        // Create a new empty struct type
        let struct_type = TypeStruct {
            name: struct_name.into(),
            fields: Vec::new(),
        };

        Ok(Value::Type(Ty::Struct(struct_type)))
    })
}

/// clone_struct! intrinsic - clone an existing struct type
pub fn builtin_clone_struct() -> BuiltinFn {
    BuiltinFn::new_with_ident("clone_struct!".into(), move |args, _ctx| {
        if args.len() != 1 {
            bail!(
                "clone_struct! expects 1 argument (struct_type), got: {:?}",
                args
            )
        }

        match &args[0] {
            Value::Type(Ty::Struct(type_struct)) => {
                // Clone the struct definition
                let cloned_struct = TypeStruct {
                    name: format!("Clone_{}", type_struct.name.name).into(),
                    fields: type_struct.fields.clone(),
                };

                Ok(Value::Type(Ty::Struct(cloned_struct)))
            }
            _ => bail!(
                "clone_struct! expects a struct type argument, got: {:?}",
                args[0]
            ),
        }
    })
}

/// hasfield! intrinsic - check if a struct has a specific field
pub fn builtin_hasfield() -> BuiltinFn {
    BuiltinFn::new_with_ident("hasfield!".into(), move |args, _ctx| {
        if args.len() != 2 {
            bail!(
                "hasfield! expects 2 arguments (struct_type, field_name), got: {:?}",
                args
            )
        }

        let field_name = match &args[1] {
            Value::String(s) => &s.value,
            _ => bail!(
                "hasfield! expects a string for field name, got: {:?}",
                args[1]
            ),
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
            _ => bail!(
                "hasfield! expects a struct type or instance, got: {:?}",
                args[0]
            ),
        };

        Ok(Value::bool(has_field))
    })
}

/// field_count! intrinsic - get the number of fields in a struct
pub fn builtin_field_count() -> BuiltinFn {
    BuiltinFn::new_with_ident("field_count!".into(), move |args, _ctx| {
        if args.len() != 1 {
            bail!(
                "field_count! expects 1 argument (struct_type), got: {:?}",
                args
            )
        }

        let field_count = match &args[0] {
            Value::Type(Ty::Struct(type_struct)) => type_struct.fields.len(),
            Value::Struct(value_struct) => value_struct.ty.fields.len(),
            _ => bail!(
                "field_count! expects a struct type or instance, got: {:?}",
                args[0]
            ),
        };

        Ok(Value::int(field_count as i64))
    })
}

/// method_count! intrinsic - get the number of methods in a struct
pub fn builtin_method_count() -> BuiltinFn {
    BuiltinFn::new_with_ident("method_count!".into(), move |args, _ctx| {
        if args.len() != 1 {
            bail!(
                "method_count! expects 1 argument (struct_type), got: {:?}",
                args
            )
        }

        // For now, return 0 as methods are not yet tracked in the AST
        // This will be enhanced when the full method registry is implemented
        let method_count = match &args[0] {
            Value::Type(Ty::Struct(_type_struct)) => {
                // Methods are not yet tracked in TypeStruct
                // This is a placeholder for future implementation
                0
            }
            Value::Struct(_value_struct) => {
                // Methods are not yet tracked in ValueStruct
                // This is a placeholder for future implementation
                0
            }
            _ => bail!(
                "method_count! expects a struct type or instance, got: {:?}",
                args[0]
            ),
        };

        Ok(Value::int(method_count as i64))
    })
}

/// field_type! intrinsic - get the type of a specific field
pub fn builtin_field_type() -> BuiltinFn {
    BuiltinFn::new_with_ident("field_type!".into(), move |args, _ctx| {
        if args.len() != 2 {
            bail!(
                "field_type! expects 2 arguments (struct_type, field_name), got: {:?}",
                args
            )
        }

        let field_name = match &args[1] {
            Value::String(s) => &s.value,
            _ => bail!(
                "field_type! expects a string for field name, got: {:?}",
                args[1]
            ),
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
                    bail!("Field '{}' not found in struct", field_name)
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
                    bail!("Field '{}' not found in struct", field_name)
                }
            }
            _ => bail!(
                "field_type! expects a struct type or instance, got: {:?}",
                args[0]
            ),
        }
    })
}

/// struct_size! intrinsic - get the size of a struct in bytes
pub fn builtin_struct_size() -> BuiltinFn {
    BuiltinFn::new_with_ident("struct_size!".into(), move |args, _ctx| {
        if args.len() != 1 {
            bail!(
                "struct_size! expects 1 argument (struct_type), got: {:?}",
                args
            )
        }

        let size = match &args[0] {
            Value::Type(Ty::Struct(type_struct)) => {
                // Simple size calculation: sum of field sizes
                type_struct
                    .fields
                    .iter()
                    .map(|field| {
                        // Estimate field size based on type
                        match &field.value {
                            Ty::Expr(expr) => {
                                if let crate::ast::Expr::Locator(locator) = expr.as_ref() {
                                    if let Some(ident) = locator.as_ident() {
                                        match ident.name.as_str() {
                                            "i8" | "u8" => 1,
                                            "i16" | "u16" => 2,
                                            "i32" | "u32" | "f32" => 4,
                                            "i64" | "u64" | "f64" => 8,
                                            "bool" => 1,
                                            "char" => 4,
                                            "String" => 24, // Vec<u8> + capacity + len
                                            _ => 8,         // Default size
                                        }
                                    } else {
                                        8 // Default for complex expressions
                                    }
                                } else {
                                    8 // Default for complex expressions
                                }
                            }
                            Ty::Struct(nested) => nested.fields.len() * 8, // Recursive estimation
                            _ => 8,                                        // Default size
                        }
                    })
                    .sum::<usize>()
            }
            Value::Struct(value_struct) => {
                // Same calculation for struct instances
                value_struct
                    .ty
                    .fields
                    .iter()
                    .map(|field| match &field.value {
                        Ty::Expr(expr) => {
                            if let crate::ast::Expr::Locator(locator) = expr.as_ref() {
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
                    })
                    .sum::<usize>()
            }
            _ => bail!(
                "struct_size! expects a struct type or instance, got: {:?}",
                args[0]
            ),
        };

        Ok(Value::int(size as i64))
    })
}

/// compile_error! intrinsic - generate a compile-time error
pub fn builtin_compile_error() -> BuiltinFn {
    BuiltinFn::new_with_ident("compile_error!".into(), move |args, _ctx| {
        if args.len() != 1 {
            bail!(
                "compile_error! expects 1 argument (error_message), got: {:?}",
                args
            )
        }

        let error_message = match &args[0] {
            Value::String(s) => &s.value,
            _ => bail!(
                "compile_error! expects a string message, got: {:?}",
                args[0]
            ),
        };

        // Generate a compile-time error
        bail!("Compile-time error: {}", error_message)
    })
}

/// compile_warning! intrinsic - generate a compile-time warning
pub fn builtin_compile_warning() -> BuiltinFn {
    BuiltinFn::new_with_ident("compile_warning!".into(), move |args, ctx| {
        if args.len() != 1 {
            bail!(
                "compile_warning! expects 1 argument (warning_message), got: {:?}",
                args
            )
        }

        let warning_message = match &args[0] {
            Value::String(s) => &s.value,
            _ => bail!(
                "compile_warning! expects a string message, got: {:?}",
                args[0]
            ),
        };

        // Print warning (in a full implementation, this would go through a proper warning system)
        ctx.root()
            .print_str(format!("Warning: {}", warning_message));

        Ok(Value::unit())
    })
}
