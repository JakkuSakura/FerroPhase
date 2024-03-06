mod typing;

use crate::ast::{
    DefConst, DefEnum, DefFunction, DefStruct, DefType, Import, Item, Module, Tree, Visibility,
};
use crate::context::SharedScopedContext;
use crate::expr::*;
use crate::id::{Ident, Locator};
use crate::ops::*;
use crate::pass::OptimizePass;
use crate::utils::conv::TryConv;
use crate::value::*;
use crate::Serializer;
use common::*;
use itertools::Itertools;
use std::sync::Arc;

#[derive(Clone)]
pub struct InterpreterPass {
    pub serializer: Arc<dyn Serializer>,
    pub ignore_missing_items: bool,
}

impl InterpreterPass {
    pub fn new(serializer: Arc<dyn Serializer>) -> Self {
        Self {
            serializer,
            ignore_missing_items: false,
        }
    }

    pub fn interpret_module(&self, node: &Module, ctx: &SharedScopedContext) -> Result<Value> {
        let result: Vec<_> = node
            .items
            .iter()
            .map(|x| self.interpret_item(x, ctx))
            .try_collect()?;
        Ok(result.into_iter().next().unwrap_or(Value::unit()))
    }
    pub fn interpret_invoke(&self, node: &Invoke, ctx: &SharedScopedContext) -> Result<Value> {
        // FIXME: call stack may not work properly
        match node.func.get() {
            Expr::Value(value) => match *value {
                Value::BinOpKind(kind) => {
                    self.interpret_invoke_binop(kind.clone(), &node.args, ctx)
                }
                Value::UnOpKind(func) => {
                    ensure!(node.args.len() == 1, "Expected 1 arg for {:?}", func);
                    let arg = self.interpret_expr(&node.args[0].get(), ctx)?;
                    self.interpret_invoke_unop(func.clone(), arg, ctx)
                }
                _ => bail!("Could not invoke {}", node),
            },

            Expr::Any(any) => {
                if let Some(exp) = any.downcast_ref::<BuiltinFn>() {
                    let args = self.interpret_args(&node.args, ctx)?;
                    exp.invoke(&args, ctx)
                } else {
                    bail!("Could not invoke {:?}", node)
                }
            }
            Expr::Locator(Locator::Ident(ident)) => {
                let func = self.interpret_ident(&ident, ctx, true)?;
                self.interpret_invoke(
                    &Invoke {
                        func: Expr::value(func).into(),
                        args: node.args.clone(),
                    },
                    ctx,
                )
            }
            Expr::Select(select) => match select.field.as_str() {
                "to_string" => match &select.obj {
                    Expr::Value(value) => match &**value {
                        Value::String(obj) => {
                            let mut obj = obj.clone();
                            obj.owned = true;
                            Ok(Value::String(obj))
                        }
                        _ => bail!("Expected string for {:?}", select),
                    },
                    _ => bail!("Expected struct for {:?}", select),
                },
                x => bail!("Could not invoke {:?}", x),
            },
            kind => bail!("Could not invoke {:?}", kind),
        }
    }
    pub fn interpret_import(&self, _node: &Import, _ctx: &SharedScopedContext) -> Result<()> {
        Ok(())
    }
    pub fn interpret_block(&self, node: &Block, ctx: &SharedScopedContext) -> Result<Value> {
        let ctx = ctx.child(Ident::new("__block__"), Visibility::Private, true);
        for (i, stmt) in node.stmts.iter().enumerate() {
            let ret = self.interpret_stmt(&stmt, &ctx)?;
            if let Some(ret) = ret {
                if i == node.stmts.len() - 1 {
                    return Ok(ret);
                }
            }
        }
        Ok(Value::unit())
    }

    pub fn interpret_cond(&self, node: &Match, ctx: &SharedScopedContext) -> Result<Value> {
        for case in &node.cases {
            let interpret = self.interpret_expr(&case.cond, ctx)?;
            match interpret {
                Value::Bool(x) => {
                    if x.value {
                        return self.interpret_expr(&case.body, ctx);
                    } else {
                        continue;
                    }
                }
                _ => {
                    bail!("Failed to interpret {:?} => {:?}", case.cond, interpret)
                }
            }
        }
        Ok(Value::unit())
    }
    pub fn interpret_print(
        se: &dyn Serializer,
        args: &[Expr],
        ctx: &SharedScopedContext,
    ) -> Result<()> {
        let formatted: Vec<_> = args
            .into_iter()
            .map(|x| se.serialize_expr(x))
            .try_collect()?;
        ctx.root().print_str(formatted.join(" "));
        Ok(())
    }
    pub fn interpret_ident(
        &self,
        ident: &Ident,
        ctx: &SharedScopedContext,
        resolve: bool,
    ) -> Result<Value> {
        return match ident.as_str() {
            // TODO: can we remove these?
            "+" if resolve => Ok(Value::any(builtin_add())),
            "-" if resolve => Ok(Value::any(builtin_sub())),
            "*" if resolve => Ok(Value::any(builtin_mul())),
            ">" if resolve => Ok(Value::any(builtin_gt())),
            ">=" if resolve => Ok(Value::any(builtin_ge())),
            "==" if resolve => Ok(Value::any(builtin_eq())),
            "<=" if resolve => Ok(Value::any(builtin_le())),
            "<" if resolve => Ok(Value::any(builtin_lt())),
            "print" if resolve => Ok(Value::any(builtin_print(self.serializer.clone()))),
            "true" => Ok(Value::bool(true)),
            "false" => Ok(Value::bool(false)),
            "None" => Ok(Value::None(ValueNone)),
            "null" => Ok(Value::Null(ValueNull)),
            "unit" => Ok(Value::Unit(ValueUnit)),
            "undefined" => Ok(Value::Undefined(ValueUndefined)),
            "Some" => Ok(Value::any(builtin_some())),
            _ => {
                info!("Get value recursive {:?}", ident);
                ctx.print_values()?;
                ctx.get_value_recursive(ident)
                    .with_context(|| format!("could not find {:?} in context", ident.name))
            }
        };
    }
    pub fn lookup_bin_op_kind(&self, op: BinOpKind) -> Result<BuiltinFn> {
        match op {
            BinOpKind::Add => Ok(builtin_add()),
            BinOpKind::AddTrait => {
                let this = self.clone();
                Ok(BuiltinFn::new(op, move |args, value| {
                    let args: Vec<_> = args
                        .into_iter()
                        .map(|x| {
                            let value = this.interpret_value(x, value, true)?;
                            match value {
                                Value::Type(TypeValue::ImplTraits(impls)) => Ok(impls.bounds),
                                _ => bail!("Expected impl Traits, got {:?}", value),
                            }
                        })
                        .try_collect()?;
                    Ok(TypeValue::ImplTraits(ImplTraits {
                        bounds: TypeBounds {
                            bounds: args.into_iter().flat_map(|x| x.bounds).collect(),
                        },
                    })
                    .into())
                }))
            }
            BinOpKind::Sub => Ok(builtin_sub()),
            BinOpKind::Mul => Ok(builtin_mul()),
            // BinOpKind::Div => Ok(builtin_div()),
            // BinOpKind::Mod => Ok(builtin_mod()),
            BinOpKind::Gt => Ok(builtin_gt()),
            BinOpKind::Lt => Ok(builtin_lt()),
            BinOpKind::Ge => Ok(builtin_ge()),
            BinOpKind::Le => Ok(builtin_le()),
            BinOpKind::Eq => Ok(builtin_eq()),
            BinOpKind::Ne => Ok(builtin_ne()),
            // BinOpKind::LogicalOr => {}
            // BinOpKind::LogicalAnd => {}
            // BinOpKind::BitOr => {}
            // BinOpKind::BitAnd => {}
            // BinOpKind::BitXor => {}
            // BinOpKind::Any(_) => {}
            _ => bail!("Could not process {:?}", op),
        }
    }

    pub fn interpret_def_function(
        &self,
        def: &DefFunction,
        ctx: &SharedScopedContext,
    ) -> Result<()> {
        let name = &def.name;
        ctx.insert_value_with_ctx(name.clone(), Value::Function(def.value.clone()));
        Ok(())
    }
    pub fn interpret_def_struct(&self, def: &DefStruct, ctx: &SharedScopedContext) -> Result<()> {
        ctx.insert_value_with_ctx(
            def.name.clone(),
            TypeValue::Struct(def.value.clone()).into(),
        );
        Ok(())
    }
    pub fn interpret_def_enum(&self, def: &DefEnum, ctx: &SharedScopedContext) -> Result<()> {
        ctx.insert_value_with_ctx(def.name.clone(), TypeValue::Enum(def.value.clone()).into());
        Ok(())
    }
    pub fn interpret_def_type(&self, def: &DefType, ctx: &SharedScopedContext) -> Result<()> {
        ctx.insert_value_with_ctx(def.name.clone(), Value::Type(def.value.clone()));
        Ok(())
    }
    pub fn interpret_def_const(&self, def: &DefConst, ctx: &SharedScopedContext) -> Result<()> {
        ctx.insert_value_with_ctx(def.name.clone(), def.value.clone());
        Ok(())
    }
    pub fn interpret_args(&self, node: &[AExpr], ctx: &SharedScopedContext) -> Result<Vec<Value>> {
        let args: Vec<_> = node
            .iter()
            .map(|x| self.try_evaluate_expr(&x.get(), ctx).map(Value::expr))
            .try_collect()?;
        Ok(args)
    }
    pub fn interpret_struct_expr(
        &self,
        node: &StructExpr,
        ctx: &SharedScopedContext,
    ) -> Result<ValueStruct> {
        let value: Value = self.interpret_expr(&node.name, ctx)?.try_conv()?;
        let ty: TypeValue = value.try_conv()?;
        let struct_ = ty.try_conv()?;
        let fields: Vec<_> = node
            .fields
            .iter()
            .map(|x| {
                Ok::<_, Error>(FieldValue {
                    name: x.name.clone(),
                    value: self.interpret_value(&x.value, ctx, true)?,
                })
            })
            .try_collect()?;
        Ok(ValueStruct {
            ty: struct_,
            structural: ValueStructural { fields },
        })
    }
    pub fn interpret_struct_value(
        &self,
        node: &ValueStruct,
        ctx: &SharedScopedContext,
    ) -> Result<ValueStruct> {
        let fields: Vec<_> = node
            .structural
            .fields
            .iter()
            .map(|x| {
                Ok::<_, Error>(FieldValue {
                    name: x.name.clone(),
                    value: self.interpret_value(&x.value, ctx, true)?,
                })
            })
            .try_collect()?;
        Ok(ValueStruct {
            ty: node.ty.clone(),
            structural: ValueStructural { fields },
        })
    }
    pub fn interpret_select(&self, s: &Select, ctx: &SharedScopedContext) -> Result<Value> {
        let obj0 = self.interpret_expr(&s.obj, ctx)?;
        let obj = obj0.as_structural().with_context(|| {
            format!(
                "Expected structural type, got {}",
                self.serializer.serialize_value(&obj0).unwrap()
            )
        })?;
        let value = obj.get_field(&s.field).with_context(|| {
            format!(
                "Could not find field {} in {}",
                s.field,
                self.serializer.serialize_value(&obj0).unwrap()
            )
        })?;
        Ok(value.value.clone())
    }
    pub fn interpret_tuple(
        &self,
        node: &ValueTuple,
        ctx: &SharedScopedContext,
        resolve: bool,
    ) -> Result<ValueTuple> {
        let values: Vec<_> = node
            .values
            .iter()
            .map(|x| self.interpret_value(x, ctx, resolve))
            .try_collect()?;
        Ok(ValueTuple {
            values: values.into_iter().map(|x| x.into()).collect(),
        })
    }
    pub fn interpret_type(&self, node: &TypeValue, ctx: &SharedScopedContext) -> Result<TypeValue> {
        // TODO: handle closure
        self.evaluate_type_value(node, ctx)
    }
    pub fn interpret_function_value(
        &self,
        node: &ValueFunction,
        ctx: &SharedScopedContext,
    ) -> Result<ValueFunction> {
        // TODO: handle unnamed function, need to pass closure to here
        let (_, context) = ctx
            .get_function(node.name.clone().unwrap())
            .with_context(|| {
                format!(
                    "Could not find function {} in context",
                    node.sig.name.as_ref().unwrap()
                )
            })?;
        let sub = context.child(Ident::new("__call__"), Visibility::Private, true);
        for generic in &node.generics_params {
            let ty = self.evaluate_type_bounds(&generic.bounds, ctx)?;
            sub.insert_value_with_ctx(generic.name.clone(), ty.into());
        }
        let params: Vec<_> = node
            .params
            .iter()
            .map(|x| {
                Ok::<_, Error>(FunctionParam {
                    name: x.name.clone(),
                    ty: self.interpret_type(&x.ty, &sub)?,
                })
            })
            .try_collect()?;
        let sig = FunctionSignature {
            name: node.sig.name.clone(),
            params,
            generics_params: node.generics_params.clone(),
            ret: self.interpret_type(&node.ret, &sub)?,
        };

        Ok(ValueFunction {
            sig,
            body: node.body.clone(),
        })
    }
    pub fn interpret_value(
        &self,
        val: &Value,
        ctx: &SharedScopedContext,
        resolve: bool,
    ) -> Result<Value> {
        match val {
            Value::Type(n) => self.interpret_type(n, ctx).map(Value::Type),
            Value::Struct(n) => self.interpret_struct_value(n, ctx).map(Value::Struct),
            Value::Structural(_) => bail!("Failed to interpret {:?}", val),
            Value::Function(n) => self.interpret_function_value(n, ctx).map(Value::Function),
            Value::Tuple(n) => self.interpret_tuple(n, ctx, resolve).map(Value::Tuple),
            Value::Expr(n) => self.interpret_expr(n, ctx),
            Value::Any(_n) => {
                if self.ignore_missing_items {
                    return Ok(val.clone());
                }

                bail!("Failed to interpret {:?}", val)
            }
            Value::Some(val) => Ok(Value::Some(ValueSome::new(
                self.interpret_value(&val.value, ctx, resolve)?.into(),
            ))),
            Value::Option(value) => Ok(Value::Option(ValueOption::new(
                value
                    .value
                    .as_ref()
                    .map(|x| self.interpret_value(&x, ctx, resolve))
                    .transpose()?,
            ))),
            Value::BinOpKind(x) if resolve => {
                self.lookup_bin_op_kind(x.clone()).map(|x| Value::any(x))
            }
            _ => Ok(val.clone()),
        }
    }
    pub fn interpret_invoke_binop(
        &self,
        op: BinOpKind,
        args: &[AExpr],
        ctx: &SharedScopedContext,
    ) -> Result<Value> {
        let builtin_fn = self.lookup_bin_op_kind(op)?;
        let args = self.interpret_args(args, ctx)?;
        builtin_fn.invoke(&args, ctx)
    }
    pub fn interpret_invoke_unop(
        &self,
        op: UnOpKind,
        arg: Value,
        _ctx: &SharedScopedContext,
    ) -> Result<Value> {
        match op {
            UnOpKind::Neg => match arg {
                Value::Int(val) => Ok(Value::Int(ValueInt::new(-val.value))),
                Value::Decimal(val) => Ok(Value::Decimal(ValueDecimal::new(-val.value))),
                _ => bail!("Failed to interpret {:?}", op),
            },
            UnOpKind::Not => match arg {
                Value::Bool(val) => Ok(Value::Bool(ValueBool::new(!val.value))),
                _ => bail!("Failed to interpret {:?}", op),
            },
            _ => bail!("Could not process {:?}", op),
        }
    }
    pub fn interpret_expr_common(
        &self,
        node: &Expr,
        ctx: &SharedScopedContext,
        resolve: bool,
    ) -> Result<Value> {
        match node {
            Expr::Locator(Locator::Ident(n)) => self.interpret_ident(n, ctx, resolve),
            Expr::Locator(n) => ctx
                .get_value_recursive(n.to_path())
                .with_context(|| format!("could not find {:?} in context", n)),
            Expr::Value(n) => self.interpret_value(n, ctx, resolve),
            Expr::Block(n) => self.interpret_block(n, ctx),
            Expr::Match(c) => self.interpret_cond(c, ctx),
            Expr::Invoke(invoke) => self.interpret_invoke(invoke, ctx),
            Expr::Any(n) => Ok(Value::Any(n.clone())),
            Expr::Select(s) => self.interpret_select(s, ctx),
            Expr::Struct(s) => self.interpret_struct_expr(s, ctx).map(Value::Struct),
            _ => bail!("Failed to interpret {:?}", node),
        }
    }
    pub fn interpret_expr(&self, node: &Expr, ctx: &SharedScopedContext) -> Result<Value> {
        self.interpret_expr_common(node, ctx, true)
    }
    pub fn interpret_expr_no_resolve(
        &self,
        node: &Expr,
        ctx: &SharedScopedContext,
    ) -> Result<Value> {
        self.interpret_expr_common(node, ctx, false)
    }
    pub fn interpret_item(&self, node: &Item, ctx: &SharedScopedContext) -> Result<Value> {
        debug!("Interpreting {}", self.serializer.serialize_item(&node)?);
        match node {
            Item::Module(n) => self.interpret_module(n, ctx),
            Item::DefFunction(n) => self.interpret_def_function(n, ctx).map(|_| Value::unit()),
            Item::DefStruct(n) => self.interpret_def_struct(n, ctx).map(|_| Value::unit()),
            Item::DefEnum(n) => self.interpret_def_enum(n, ctx).map(|_| Value::unit()),
            Item::DefType(n) => self.interpret_def_type(n, ctx).map(|_| Value::unit()),
            Item::DefConst(n) => self.interpret_def_const(n, ctx).map(|_| Value::unit()),
            Item::Import(n) => self.interpret_import(n, ctx).map(|_| Value::unit()),

            Item::Any(n) => Ok(Value::Any(n.clone())),
            _ => bail!("Failed to interpret {:?}", node),
        }
    }

    pub fn interpret_let(&self, node: &StatementLet, ctx: &SharedScopedContext) -> Result<Value> {
        let value = self.interpret_expr(&node.value, ctx)?;
        ctx.insert_value(
            node.pat.as_ident().context("Only supports ident")?.as_str(),
            value.clone(),
        );
        Ok(value)
    }

    pub fn interpret_stmt(
        &self,
        node: &Statement,
        ctx: &SharedScopedContext,
    ) -> Result<Option<Value>> {
        debug!("Interpreting {}", self.serializer.serialize_stmt(&node)?);
        match node {
            Statement::Let(n) => self.interpret_let(n, ctx).map(|_| None),
            Statement::Expr(n) => self.interpret_expr(n, ctx).map(|x| {
                if matches!(x, Value::Unit(_)) {
                    None
                } else {
                    Some(x)
                }
            }),
            Statement::SideEffect(n) => self.interpret_expr(&n.expr, ctx).map(|_| None),
            Statement::Item(_) => Ok(None),
            _ => bail!("Failed to interpret {:?}", node),
        }
    }

    pub fn interpret_tree(&self, node: &Tree, ctx: &SharedScopedContext) -> Result<Value> {
        match node {
            Tree::Item(item) => self.interpret_item(item, ctx),
            Tree::Expr(expr) => self.interpret_expr(expr, ctx),
            Tree::File(file) => self.interpret_module(&file.module, ctx),
        }
    }
}

impl OptimizePass for InterpreterPass {
    fn name(&self) -> &str {
        "interpreter"
    }
    fn optimize_expr(&self, expr: Expr, ctx: &SharedScopedContext) -> Result<Expr> {
        let value = self.interpret_expr_no_resolve(&expr, ctx)?;
        Ok(Expr::value(value))
    }

    fn optimize_item(&self, _item: Item, _ctx: &SharedScopedContext) -> Result<Item> {
        Ok(Item::unit())
    }

    fn evaluate_condition(&self, expr: Expr, ctx: &SharedScopedContext) -> Result<ControlFlow> {
        let value = self.interpret_expr_no_resolve(&expr, ctx)?;
        match value {
            Value::Bool(b) => {
                if b.value {
                    Ok(ControlFlow::IntoAndBreak(None))
                } else {
                    Ok(ControlFlow::Continue)
                }
            }
            _ => bail!("Failed to interpret {:?} => {:?}", expr, value),
        }
    }
    fn evaluate_invoke(&self, _invoke: Invoke, _ctx: &SharedScopedContext) -> Result<ControlFlow> {
        Ok(ControlFlow::Into)
    }
    fn optimize_invoke(
        &self,
        invoke: Invoke,
        func: &Value,
        ctx: &SharedScopedContext,
    ) -> Result<Expr> {
        match func {
            Value::Function(func) => self.interpret_expr(&func.body, ctx).map(Expr::value),
            Value::BinOpKind(kind) => self
                .interpret_invoke_binop(kind.clone(), &invoke.args, ctx)
                .map(Expr::value),
            Value::UnOpKind(func) => {
                ensure!(invoke.args.len() == 1, "Expected 1 arg for {:?}", func);
                let arg = self.interpret_expr(&invoke.args[0].get(), ctx)?;
                self.interpret_invoke_unop(func.clone(), arg, ctx)
                    .map(Expr::value)
            }
            _ => bail!("Could not invoke {:?}", func),
        }
    }

    fn try_evaluate_expr(&self, pat: &Expr, ctx: &SharedScopedContext) -> Result<Expr> {
        let value = ctx.try_get_value_from_expr(pat).with_context(|| {
            format!(
                "could not find {:?} in context {:?}",
                pat,
                ctx.list_values()
                    .into_iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
            )
        })?;
        Ok(Expr::value(value))
    }
}
