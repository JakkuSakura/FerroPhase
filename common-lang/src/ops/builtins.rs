use crate::context::ExecutionContext;
use crate::tree::Expr;
use crate::value::{BoolValue, DecimalValue, IntValue, Value};
use crate::Serializer;
use common::*;
use std::fmt::{Debug, Formatter};
use std::rc::Rc;

#[derive(Clone)]
pub struct BuiltinFn {
    pub name: String,
    f: Rc<dyn Fn(&[Expr], &ExecutionContext) -> Result<Expr>>,
}
impl BuiltinFn {
    pub fn new(
        name: String,
        f: impl Fn(&[Expr], &ExecutionContext) -> Result<Expr> + 'static,
    ) -> Self {
        Self {
            name,
            f: Rc::new(f),
        }
    }
    pub fn call(&self, args: &[Expr], ctx: &ExecutionContext) -> Result<Expr> {
        (self.f)(args, ctx)
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
        serializer.serialize_str(&self.name)
    }
}
impl<'de> Deserialize<'de> for BuiltinFn {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let name = String::deserialize(deserializer)?;
        Ok(Self::new(name, |_, _| unreachable!()))
    }
}

pub fn operate_on_literals(
    name: &str,
    op_i64: impl Fn(&[i64]) -> i64 + 'static,
    op_f64: impl Fn(&[f64]) -> f64 + 'static,
) -> BuiltinFn {
    BuiltinFn::new(name.to_string(), move |args, _ctx| {
        let mut args_i64 = vec![];
        let mut args_f64 = vec![];
        for arg in args {
            match arg {
                Expr::Value(Value::Int(x)) => args_i64.push(x.value),
                Expr::Value(Value::Decimal(x)) => args_f64.push(x.value),
                _ => bail!("Does not support argument type {:?}", args),
            }
        }
        if !args_i64.is_empty() && !args_f64.is_empty() {
            bail!("Does not support argument type {:?}", args)
        }
        if !args_i64.is_empty() {
            return Ok(Expr::Value(Value::Int(IntValue::new(op_i64(&args_i64)))));
        }
        if !args_f64.is_empty() {
            return Ok(Expr::Value(Value::Decimal(DecimalValue::new(op_f64(
                &args_f64,
            )))));
        }
        bail!("Does not support argument type {:?}", args)
    })
}
pub fn binary_comparison_on_literals(
    name: &str,
    op_i64: impl Fn(i64, i64) -> bool + 'static,
    op_f64: impl Fn(f64, f64) -> bool + 'static,
) -> BuiltinFn {
    BuiltinFn::new(name.to_string(), move |args, _ctx| {
        if args.len() != 2 {
            bail!("Argument expected 2, got: {:?}", args)
        }
        let mut args_i64 = vec![];
        let mut args_f64 = vec![];
        for arg in args {
            match arg {
                Expr::Value(Value::Int(x)) => args_i64.push(x.value),
                Expr::Value(Value::Decimal(x)) => args_f64.push(x.value),
                _ => bail!("Does not support argument type {:?}", args),
            }
        }
        if !args_i64.is_empty() && !args_f64.is_empty() {
            bail!("Does not support argument type {:?}", args)
        }
        if !args_i64.is_empty() {
            return Ok(Expr::Value(Value::Bool(BoolValue::new(op_i64(
                args_i64[0],
                args_i64[1],
            )))));
        }
        if !args_f64.is_empty() {
            return Ok(Expr::Value(Value::Bool(BoolValue::new(op_f64(
                args_f64[0],
                args_f64[1],
            )))));
        }

        bail!("Does not support argument type {:?}", args)
    })
}
pub fn builtin_add() -> BuiltinFn {
    operate_on_literals("+", |x| x.into_iter().sum(), |x| x.into_iter().sum())
}
pub fn builtin_sub() -> BuiltinFn {
    operate_on_literals(
        "-",
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
        "*",
        |x| x.into_iter().product(),
        |x| x.into_iter().product(),
    )
}

pub fn builtin_gt() -> BuiltinFn {
    binary_comparison_on_literals(">", |x, y| x > y, |x, y| x > y)
}

pub fn builtin_gte() -> BuiltinFn {
    binary_comparison_on_literals(">=", |x, y| x >= y, |x, y| x >= y)
}
pub fn builtin_lt() -> BuiltinFn {
    binary_comparison_on_literals("<", |x, y| x < y, |x, y| x < y)
}
pub fn builtin_lte() -> BuiltinFn {
    binary_comparison_on_literals("<=", |x, y| x <= y, |x, y| x <= y)
}
pub fn builtin_eq() -> BuiltinFn {
    binary_comparison_on_literals("==", |x, y| x == y, |x, y| x == y)
}
pub fn builtin_ne() -> BuiltinFn {
    binary_comparison_on_literals("!=", |x, y| x != y, |x, y| x != y)
}

pub fn builtin_print(se: Rc<dyn Serializer>) -> BuiltinFn {
    BuiltinFn::new("print".to_string(), move |args, ctx| {
        let formatted: Vec<_> = args
            .into_iter()
            .map(|x| se.serialize_expr(x))
            .try_collect()?;
        ctx.root().print_str(formatted.join(" "));
        Ok(Expr::unit())
    })
}
