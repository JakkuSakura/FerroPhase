use crate::ast::*;
use crate::context::SharedScopedContext;
use crate::id::Ident;
use crate::ops::BinOpKind;
use crate::bail;
use itertools::*;
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
    func: Arc<dyn Fn(&[AstValue], &SharedScopedContext) -> Result<AstValue, crate::Error> + Send + Sync>,
}
impl BuiltinFn {
    pub fn new(
        name: BinOpKind,
        f: impl Fn(&[AstValue], &SharedScopedContext) -> Result<AstValue, crate::Error> + Send + Sync + 'static,
    ) -> Self {
        Self {
            name: BuiltinFnName::BinOpKind(name),
            func: Arc::new(f),
        }
    }
    pub fn new_with_ident(
        name: Ident,
        f: impl Fn(&[AstValue], &SharedScopedContext) -> Result<AstValue, crate::Error> + Send + Sync + 'static,
    ) -> Self {
        Self {
            name: BuiltinFnName::Name(name),
            func: Arc::new(f),
        }
    }
    pub fn invoke(&self, args: &[AstValue], ctx: &SharedScopedContext) -> Result<AstValue, crate::Error> {
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
                AstValue::Int(x) => args_i64.push(x.value),
                AstValue::Decimal(x) => args_f64.push(x.value),
                _ => bail!("Does not support argument type {:?}", args),
            }
        }
        if !args_i64.is_empty() && !args_f64.is_empty() {
            bail!("Does not support argument type {:?}", args)
        }
        if !args_i64.is_empty() {
            return Ok(AstValue::int(op_i64(&args_i64)));
        }
        if !args_f64.is_empty() {
            return Ok(AstValue::decimal(op_f64(&args_f64)));
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
        for arg in args {
            match arg {
                AstValue::Int(x) => args_i64.push(x.value),
                AstValue::Decimal(x) => args_f64.push(x.value),
                _ => bail!("Does not support argument type {:?}", args),
            }
        }
        if !args_i64.is_empty() && !args_f64.is_empty() {
            bail!("Does not support argument type {:?}", args)
        }
        if !args_i64.is_empty() {
            return Ok(AstValue::bool(op_i64(args_i64[0], args_i64[1])));
        }
        if !args_f64.is_empty() {
            return Ok(AstValue::bool(op_f64(args_f64[0], args_f64[1])));
        }

        bail!("Does not support argument type {:?}", args)
    })
}
pub fn builtin_add() -> BuiltinFn {
    operate_on_literals(
        BinOpKind::Add,
        |x| x.into_iter().sum(),
        |x| x.into_iter().sum(),
    )
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

pub fn builtin_print(se: Arc<dyn AstSerializer>) -> BuiltinFn {
    BuiltinFn::new_with_ident("print".into(), move |args, ctx| {
        let formatted: Vec<_> = args
            .into_iter()
            .map(|x| se.serialize_value(x))
            .try_collect()?;
        ctx.root().print_str(formatted.join(" "));
        Ok(AstValue::unit())
    })
}
pub fn builtin_some() -> BuiltinFn {
    BuiltinFn::new_with_ident("Some".into(), move |args, _ctx| {
        if args.len() != 1 {
            bail!("Some expects 1 argument, got: {:?}", args)
        }
        Ok(AstValue::Some(ValueSome::new(args[0].clone().into())))
    })
}

// ===== METAPROGRAMMING INTRINSICS =====

/// @sizeof intrinsic - get size of a type in bytes
pub fn builtin_sizeof() -> BuiltinFn {
    BuiltinFn::new_with_ident("@sizeof".into(), move |args, ctx| {
        if args.len() != 1 {
            bail!("@sizeof expects 1 argument, got: {:?}", args)
        }
        
        match &args[0] {
            AstValue::Type(ast_type) => {
                // For now, return estimated sizes for common types
                let size = match ast_type {
                    AstType::Expr(expr) => {
                        if let crate::ast::AstExpr::Locator(locator) = expr.as_ref() {
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
                    },
                    AstType::Struct(type_struct) => {
                        // Calculate struct size as sum of field sizes
                        type_struct.fields.len() * 8 // Simplified: assume each field is 8 bytes
                    },
                    _ => 8, // Default size for other types
                };
                Ok(AstValue::int(size as i64))
            },
            _ => bail!("@sizeof expects a type argument, got: {:?}", args[0])
        }
    })
}

/// @reflect_fields intrinsic - get field information of a struct type
pub fn builtin_reflect_fields() -> BuiltinFn {
    BuiltinFn::new_with_ident("@reflect_fields".into(), move |args, _ctx| {
        if args.len() != 1 {
            bail!("@reflect_fields expects 1 argument, got: {:?}", args)
        }
        
        match &args[0] {
            AstValue::Type(AstType::Struct(type_struct)) => {
                let field_descriptors: Vec<AstValue> = type_struct.fields.iter()
                    .map(|field| {
                        // Create a field descriptor struct
                        let fields = vec![
                            ValueField {
                                name: "name".into(),
                                value: AstValue::string(field.name.name.clone()),
                            },
                            ValueField {
                                name: "type_name".into(), 
                                value: AstValue::string(format!("{}", field.value)),
                            }
                        ];
                        
                        AstValue::Structural(ValueStructural { fields })
                    })
                    .collect();
                
                Ok(AstValue::List(ValueList::new(field_descriptors)))
            },
            _ => bail!("@reflect_fields expects a struct type argument, got: {:?}", args[0])
        }
    })
}

/// @hasmethod intrinsic - check if a type has a specific method
pub fn builtin_hasmethod() -> BuiltinFn {
    BuiltinFn::new_with_ident("@hasmethod".into(), move |args, _ctx| {
        if args.len() != 2 {
            bail!("@hasmethod expects 2 arguments (type, method_name), got: {:?}", args)
        }
        
        let method_name = match &args[1] {
            AstValue::String(s) => s.value.as_str(),
            _ => bail!("@hasmethod expects a string for method name, got: {:?}", args[1])
        };
        
        // For now, return false for all methods since we don't have a complete method registry
        // This would be enhanced when the full type system is implemented
        match &args[0] {
            AstValue::Type(_) => {
                // Basic hardcoded method checks for common types
                let has_method = match method_name {
                    "to_string" => true, // Most types can be converted to string
                    _ => false,
                };
                Ok(AstValue::bool(has_method))
            },
            _ => bail!("@hasmethod expects a type argument, got: {:?}", args[0])
        }
    })
}

/// @addfield intrinsic - add a field to a struct type (placeholder for metaprogramming)
pub fn builtin_addfield() -> BuiltinFn {
    BuiltinFn::new_with_ident("@addfield".into(), move |args, ctx| {
        if args.len() != 2 {
            bail!("@addfield expects 2 arguments (field_name, field_type), got: {:?}", args)
        }
        
        let field_name = match &args[0] {
            AstValue::String(s) => s.value.clone(),
            _ => bail!("@addfield expects a string for field name, got: {:?}", args[0])
        };
        
        let _field_type = match &args[1] {
            AstValue::Type(ty) => ty.clone(),
            _ => bail!("@addfield expects a type for field type, got: {:?}", args[1])
        };
        
        // For now, just record that we want to add this field
        // In a full implementation, this would modify the current struct being processed
        ctx.root().print_str(format!("@addfield: Adding field '{}' to current struct", field_name));
        
        Ok(AstValue::unit())
    })
}

/// @generate_method intrinsic - generate a method for a type
pub fn builtin_generate_method() -> BuiltinFn {
    BuiltinFn::new_with_ident("@generate_method".into(), move |args, ctx| {
        if args.len() != 2 {
            bail!("@generate_method expects 2 arguments (method_name, method_body), got: {:?}", args)
        }
        
        let method_name = match &args[0] {
            AstValue::String(s) => s.value.clone(),
            _ => bail!("@generate_method expects a string for method name, got: {:?}", args[0])
        };
        
        let _method_body = &args[1]; // Could be a string or expression
        
        // For now, just record that we want to generate this method
        ctx.root().print_str(format!("@generate_method: Generating method '{}' for current type", method_name));
        
        Ok(AstValue::unit())
    })
}

/// @type_name intrinsic - get the name of a type as a string
pub fn builtin_type_name() -> BuiltinFn {
    BuiltinFn::new_with_ident("@type_name".into(), move |args, _ctx| {
        if args.len() != 1 {
            bail!("@type_name expects 1 argument, got: {:?}", args)
        }
        
        let type_name = match &args[0] {
            AstValue::Type(ast_type) => {
                format!("{}", ast_type)
            },
            other => {
                // Return the type of the value
                match other {
                    AstValue::Int(_) => "i64".to_string(),
                    AstValue::Bool(_) => "bool".to_string(), 
                    AstValue::Decimal(_) => "f64".to_string(),
                    AstValue::String(_) => "String".to_string(),
                    AstValue::Unit(_) => "()".to_string(),
                    _ => "unknown".to_string(),
                }
            }
        };
        
        Ok(AstValue::string(type_name))
    })
}
