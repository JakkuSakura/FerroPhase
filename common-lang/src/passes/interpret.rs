use crate::ast::*;
use crate::context::{ArcScopedContext, LazyValue};
use crate::ops::{
    builtin_add, builtin_eq, builtin_ge, builtin_gt, builtin_le, builtin_lt, builtin_mul,
    builtin_ne, builtin_print, builtin_sub, BinOpKind, BuiltinFn,
};
use crate::passes::OptimizePass;
use crate::type_system::TypeSystem;
use crate::value::{
    FieldValue, FunctionParam, FunctionValue, StructValue, TupleValue, TypeValue, Value,
};
use crate::Serializer;
use common::{bail, debug, info, ContextCompat, Error, Itertools};
use std::rc::Rc;

pub struct InterpreterPass {
    pub serializer: Rc<dyn Serializer>,
    pub type_system: TypeSystem,
    pub ignore_missing_items: bool,
}

impl InterpreterPass {
    pub fn new(serializer: Rc<dyn Serializer>) -> Self {
        Self {
            type_system: TypeSystem::new(serializer.clone()),
            serializer,
            ignore_missing_items: false,
        }
    }

    pub fn interpret_module(&self, node: &Module, ctx: &ArcScopedContext) -> common::Result<Value> {
        node.items.iter().for_each(|x| match x {
            Item::Def(x) => match &x.value {
                DefValue::Function(n) => {
                    debug!("Inserting function {} into context", x.name);

                    ctx.insert_function(x.name.clone(), n.clone());
                }
                _ => {}
            },
            _ => {}
        });
        let result: Vec<_> = node
            .items
            .iter()
            .map(|x| self.interpret_item(x, ctx))
            .try_collect()?;
        Ok(result.into_iter().next().unwrap_or(Value::unit()))
    }
    pub fn interpret_invoke(&self, node: &Invoke, ctx: &ArcScopedContext) -> common::Result<Value> {
        debug!(
            "Will execute call {}",
            self.serializer.serialize_invoke(&node)?
        );
        let fun = self.interpret_expr(&node.func, ctx)?;
        debug!(
            "Will call function {}",
            self.serializer.serialize_value(&fun)?
        );
        let args = self.interpret_args(&node.args, ctx)?;
        match fun {
            Value::Function(f) => {
                let name = self.serializer.serialize_expr(&node.func)?;
                let sub = ctx.child(Ident::new("__func__"), Visibility::Private, false);
                for (i, arg) in args.iter().cloned().enumerate() {
                    let param = f
                        .params
                        .get(i)
                        .with_context(|| format!("Couldn't find {} parameter of {:?}", i, f))?;
                    // TODO: type check here

                    sub.insert_value(param.name.clone(), arg);
                }
                debug!(
                    "Invoking {} with {}",
                    name,
                    self.serializer.serialize_values(&args)?
                );
                let ret = self.interpret_expr(&f.body, &sub)?;
                debug!(
                    "Invoked {} with {} => {}",
                    name,
                    self.serializer.serialize_values(&args)?,
                    self.serializer.serialize_value(&ret)?
                );
                return Ok(ret);
            }

            Value::Expr(x) => {
                match &*x {
                    Expr::Select(s) => {
                        // FIXME this is hack for rust
                        if s.field.as_str() == "into" {
                            return Ok(Value::expr(*s.obj.clone()));
                        }
                    }
                    _ => {}
                }
                bail!("Failed to interpret {:?}: {:?}", node, x);
            }
            Value::Any(any) => {
                if let Some(f) = any.downcast_ref::<BuiltinFn>() {
                    let args_ = self.serializer.serialize_values(&args)?;

                    debug!("Invoking {} with {}", f.name, args_);
                    let ret = f.call(&args, ctx)?;
                    let ret_ = self.serializer.serialize_value(&ret)?;

                    debug!("Invoked {} with {} => {}", f.name, args_, ret_);
                    return Ok(ret);
                }
                bail!("Failed to interpret {:?}", node);
            }
            _ => bail!("Failed to interpret {:?}", node),
        }
    }
    pub fn interpret_import(&self, _node: &Import, _ctx: &ArcScopedContext) -> common::Result<()> {
        Ok(())
    }
    pub fn interpret_block(&self, node: &Block, ctx: &ArcScopedContext) -> common::Result<Value> {
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
    pub fn interpret_cond(&self, node: &Cond, ctx: &ArcScopedContext) -> common::Result<Value> {
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
        ctx: &ArcScopedContext,
    ) -> common::Result<()> {
        let formatted: Vec<_> = args
            .into_iter()
            .map(|x| se.serialize_expr(x))
            .try_collect()?;
        ctx.root().print_str(formatted.join(" "));
        Ok(())
    }
    pub fn interpret_ident(&self, ident: &Ident, ctx: &ArcScopedContext) -> common::Result<Value> {
        return match ident.as_str() {
            "+" => Ok(Value::any(builtin_add())),
            "-" => Ok(Value::any(builtin_sub())),
            "*" => Ok(Value::any(builtin_mul())),
            ">" => Ok(Value::any(builtin_gt())),
            ">=" => Ok(Value::any(builtin_ge())),
            "==" => Ok(Value::any(builtin_eq())),
            "<=" => Ok(Value::any(builtin_le())),
            "<" => Ok(Value::any(builtin_lt())),
            "print" => Ok(Value::any(builtin_print(self.serializer.clone()))),
            "true" => Ok(Value::bool(true)),
            "false" => Ok(Value::bool(false)),
            _ => {
                info!("Get value recursive {:?}", ident);
                ctx.print_values(&*self.serializer)?;
                ctx.get_value_recursive(ident)
                    .with_context(|| format!("could not find {:?} in context", ident.name))
            }
        };
    }
    pub fn interpret_bin_op_kind(
        &self,
        op: BinOpKind,
        _ctx: &ArcScopedContext,
    ) -> common::Result<BuiltinFn> {
        match op {
            BinOpKind::Add => Ok(builtin_add()),
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
    pub fn interpret_def(&self, def: &Define, ctx: &ArcScopedContext) -> common::Result<()> {
        match &def.value {
            DefValue::Function(n) => {
                if def.name == Ident::new("main") {
                    self.interpret_expr(&n.body, ctx).map(|_| ())
                } else {
                    let name = &def.name;
                    ctx.insert_function(name.clone(), n.clone());
                    Ok(())
                }
            }
            DefValue::Type(n) => {
                ctx.insert_type(
                    def.name.clone(),
                    TypeValue::any_box(LazyValue {
                        ctx: ctx.clone(),
                        expr: n.clone(),
                    }),
                );
                Ok(())
            }
            DefValue::Const(n) => {
                ctx.insert_value(
                    def.name.clone(),
                    Value::any(LazyValue {
                        ctx: ctx.clone(),
                        expr: n.clone(),
                    }),
                );
                Ok(())
            }
        }
    }
    pub fn interpret_args(
        &self,
        node: &[Expr],
        ctx: &ArcScopedContext,
    ) -> common::Result<Vec<Value>> {
        let args: Vec<_> = node
            .iter()
            .map(|x| self.interpret_expr(x, ctx))
            .try_collect()?;
        Ok(args)
    }
    pub fn interpret_struct_value(
        &self,
        node: &StructValue,
        ctx: &ArcScopedContext,
    ) -> common::Result<StructValue> {
        let fields: Vec<_> = node
            .fields
            .iter()
            .map(|x| {
                Ok::<_, Error>(FieldValue {
                    name: x.name.clone(),
                    value: self.interpret_value(&x.value, ctx)?,
                })
            })
            .try_collect()?;
        Ok(StructValue {
            name: node.name.clone(),
            fields,
        })
    }
    pub fn interpret_select(&self, s: &Select, ctx: &ArcScopedContext) -> common::Result<Expr> {
        let obj = self.interpret_expr(&s.obj, ctx)?;
        // TODO: try to select values
        Ok(Expr::Select(Select {
            obj: Expr::value(obj).into(),
            field: s.field.clone(),
            select: s.select,
        }))
    }
    pub fn interpret_tuple(
        &self,
        node: &TupleValue,
        ctx: &ArcScopedContext,
    ) -> common::Result<TupleValue> {
        let values: Vec<_> = node
            .values
            .iter()
            .map(|x| self.interpret_value(x, ctx))
            .try_collect()?;
        Ok(TupleValue {
            values: values.into_iter().map(|x| x.into()).collect(),
        })
    }
    pub fn interpret_type(
        &self,
        node: &TypeValue,
        ctx: &ArcScopedContext,
    ) -> common::Result<TypeValue> {
        match node {
            TypeValue::AnyBox(n) => {
                if let Some(exp) = n.downcast_ref::<LazyValue<TypeExpr>>() {
                    return self.type_system.evaluate_type_expr(&exp.expr, &exp.ctx);
                }
                bail!("Failed to interpret type {:?}", node)
            }
            _ => self.type_system.evaluate_type_value(node, ctx),
        }
    }
    pub fn interpret_function_value(
        &self,
        node: &FunctionValue,
        ctx: &ArcScopedContext,
    ) -> common::Result<FunctionValue> {
        let sub = ctx.child(Ident::new("__func__"), Visibility::Private, false);
        for generic in &node.generics_params {
            let ty = self
                .type_system
                .evaluate_type_bounds(&generic.bounds, ctx)?;
            sub.insert_type(generic.name.clone(), ty);
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

        Ok(FunctionValue {
            name: node.name.clone(),
            params,
            generics_params: node.generics_params.clone(),
            ret: self.interpret_type(&node.ret, &sub)?,
            body: node.body.clone(),
        })
    }
    pub fn interpret_value(&self, node: &Value, ctx: &ArcScopedContext) -> common::Result<Value> {
        match node {
            Value::Type(n) => self.interpret_type(n, ctx).map(Value::Type),
            Value::Struct(n) => self.interpret_struct_value(n, ctx).map(Value::Struct),
            Value::Function(n) => self.interpret_function_value(n, ctx).map(Value::Function),
            Value::Tuple(n) => self.interpret_tuple(n, ctx).map(Value::Tuple),
            Value::Expr(n) => self.interpret_expr(n, ctx),
            Value::Any(n) => {
                if let Some(exp) = n.downcast_ref::<LazyValue<Expr>>() {
                    let ret = self.interpret_expr(&exp.expr, &exp.ctx)?;
                    return Ok(ret);
                }

                if self.ignore_missing_items {
                    return Ok(node.clone());
                }

                bail!("Failed to interpret {:?}", node)
            }
            Value::Int(_) => Ok(node.clone()),
            Value::Bool(_) => Ok(node.clone()),
            Value::Decimal(_) => Ok(node.clone()),
            Value::Char(_) => Ok(node.clone()),
            Value::String(_) => Ok(node.clone()),
            Value::List(_) => Ok(node.clone()),
            Value::Unit(_) => Ok(node.clone()),
            Value::Null(_) => Ok(node.clone()),
            Value::Undefined(_) => Ok(node.clone()),
            Value::BinOpKind(x) => self
                .interpret_bin_op_kind(x.clone(), ctx)
                .map(|x| Value::any(x)),
        }
    }

    pub fn interpret_expr(&self, node: &Expr, ctx: &ArcScopedContext) -> common::Result<Value> {
        match node {
            Expr::Pat(Pat::Ident(n)) => self.interpret_ident(n, ctx),
            Expr::Pat(n) => ctx
                .get_value_recursive(n)
                .with_context(|| format!("could not find {:?} in context", n)),
            Expr::Value(n) => Ok(self.interpret_value(n, ctx)?.into()),
            Expr::Block(n) => self.interpret_block(n, ctx),
            Expr::Cond(c) => self.interpret_cond(c, ctx),
            Expr::Invoke(invoke) => self.interpret_invoke(invoke, ctx),
            Expr::Any(n) => Ok(Value::Any(n.clone())),

            _ => bail!("Failed to interpret {:?}", node),
        }
    }
    pub fn interpret_item_inner(
        &self,
        node: &Item,
        ctx: &ArcScopedContext,
    ) -> common::Result<Value> {
        debug!("Interpreting {}", self.serializer.serialize_item(&node)?);
        match node {
            Item::Module(n) => self.interpret_module(n, ctx),
            Item::Def(n) => self.interpret_def(n, ctx).map(|_| Value::unit()),
            Item::Import(n) => self.interpret_import(n, ctx).map(|_| Value::unit()),

            Item::Any(n) => Ok(Value::Any(n.clone())),
            _ => bail!("Failed to interpret {:?}", node),
        }
    }
    pub fn interpret_item(&self, node: &Item, ctx: &ArcScopedContext) -> common::Result<Value> {
        debug!("Interpreting {}", self.serializer.serialize_item(&node)?);
        let result = self.interpret_item_inner(node, ctx);
        match result {
            Ok(result) => {
                debug!(
                    "Interpreted {} => {}",
                    self.serializer.serialize_item(&node)?,
                    self.serializer.serialize_value(&result)?
                );
                Ok(result)
            }
            Err(err) => {
                // warn!(
                //     "Failed to interpret {} => {:?}",
                //     self.serializer.serialize_item(&node)?,
                //     err
                // );
                Err(err)
            }
        }
    }
    pub fn interpret_let(&self, node: &Let, ctx: &ArcScopedContext) -> common::Result<Value> {
        let value = self.interpret_expr(&node.value, ctx)?;
        ctx.insert_value(node.name.clone(), value.clone());
        Ok(value)
    }

    pub fn interpret_stmt(
        &self,
        node: &Statement,
        ctx: &ArcScopedContext,
    ) -> common::Result<Option<Value>> {
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
            Statement::Any(_) => bail!("Failed to interpret {:?}", node),
        }
    }

    pub fn interpret_tree(&self, node: &Tree, ctx: &ArcScopedContext) -> common::Result<Value> {
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
    fn optimize_expr_post(&self, expr: Expr, ctx: &ArcScopedContext) -> common::Result<Expr> {
        let value = self.interpret_expr(&expr, ctx)?;
        Ok(Expr::value(value))
    }
    fn optimize_item_pre(&self, item: Item, _ctx: &ArcScopedContext) -> common::Result<Item> {
        match item {
            Item::Def(def) if def.name.as_str() == "main" => match def.value {
                DefValue::Function(func) => Ok(Item::Expr(*func.body)),
                _ => bail!("main should be a function"),
            },
            _ => Ok(Item::Expr(Expr::unit())),
        }
    }
    fn evaluate_condition(
        &self,
        expr: Expr,
        ctx: &ArcScopedContext,
    ) -> common::Result<Option<ControlFlow>> {
        let value = self.interpret_expr(&expr, ctx)?;
        match value {
            Value::Bool(b) => {
                if b.value {
                    Ok(Some(ControlFlow::IntoAndBreak(None)))
                } else {
                    Ok(Some(ControlFlow::Continue))
                }
            }
            _ => bail!("Failed to interpret {:?} => {:?}", expr, value),
        }
    }
}
