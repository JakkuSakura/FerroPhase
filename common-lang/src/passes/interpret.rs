use crate::ast::*;
use crate::context::{ArcScopedContext, LazyValue};
use crate::ops::*;
use crate::passes::OptimizePass;
use crate::typing::TypeSystem;
use crate::value::*;
use crate::Serializer;
use common::*;
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

    pub fn interpret_module(&self, node: &Module, ctx: &ArcScopedContext) -> Result<Value> {
        let result: Vec<_> = node
            .items
            .iter()
            .map(|x| self.interpret_item(x, ctx))
            .try_collect()?;
        Ok(result.into_iter().next().unwrap_or(Value::unit()))
    }
    pub fn interpret_invoke(&self, node: &Invoke, ctx: &ArcScopedContext) -> Result<Value> {
        // FIXME: call stack may not work properly
        match &node.func {
            Expr::Value(value) => match &**value {
                Value::BinOpKind(kind) => {
                    self.interpret_invoke_binop(kind.clone(), &node.args, ctx)
                }
                Value::UnOpKind(func) => {
                    ensure!(node.args.len() == 1, "Expected 1 arg for {:?}", func);
                    let arg = self.interpret_expr(&node.args[0], ctx)?;
                    self.interpret_invoke_unop(func.clone(), arg, ctx)
                }
                _ => bail!("Could not invoke {:?}", node),
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
                let func = self.interpret_ident(ident, ctx, true)?;
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
    pub fn interpret_import(&self, _node: &Import, _ctx: &ArcScopedContext) -> Result<()> {
        Ok(())
    }
    pub fn interpret_block(&self, node: &Block, ctx: &ArcScopedContext) -> Result<Value> {
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
    pub fn interpret_un_op(&self, node: &UnOp<Expr>, ctx: &ArcScopedContext) -> Result<Value> {
        let value = self.interpret_expr(&node.expr, ctx)?;
        match node.kind {
            UnOpKind::Neg => match value {
                Value::Int(x) => Ok(Value::Int(IntValue::new(-x.value))),
                Value::Decimal(x) => Ok(Value::Decimal(DecimalValue::new(-x.value))),
                _ => bail!("Failed to interpret {:?}", node),
            },
            UnOpKind::Not => match value {
                Value::Bool(x) => Ok(Value::Bool(BoolValue::new(!x.value))),
                _ => bail!("Failed to interpret {:?}", node),
            },
            _ => bail!("Failed to interpret {:?}", node),
        }
    }
    pub fn interpret_cond(&self, node: &Match, ctx: &ArcScopedContext) -> Result<Value> {
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
        ctx: &ArcScopedContext,
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
            "None" => Ok(Value::None(NoneValue)),
            "null" => Ok(Value::Null(NullValue)),
            "unit" => Ok(Value::Unit(UnitValue)),
            "undefined" => Ok(Value::Undefined(UndefinedValue)),
            "Some" => Ok(Value::any(builtin_some())),
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
    ) -> Result<BuiltinFn> {
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

    pub fn interpret_def(&self, def: &Define, ctx: &ArcScopedContext) -> Result<()> {
        match &def.value {
            DefineValue::Function(n) => {
                if def.name == Ident::new("main") {
                    self.interpret_expr(&n.body, ctx).map(|_| ())
                } else {
                    let name = &def.name;
                    ctx.insert_function(name.clone(), n.clone());
                    Ok(())
                }
            }
            DefineValue::Type(n) => {
                ctx.insert_type(
                    def.name.clone(),
                    TypeValue::any_box(LazyValue {
                        ctx: ctx.clone(),
                        expr: n.clone(),
                    }),
                );
                Ok(())
            }
            DefineValue::Const(n) => {
                ctx.insert_value(
                    def.name.clone(),
                    Value::any(LazyValue {
                        ctx: ctx.clone(),
                        expr: n.clone(),
                    }),
                );
                Ok(())
            }
            _ => bail!("Failed to interpret {:?}", def),
        }
    }
    pub fn interpret_args(&self, node: &[Expr], ctx: &ArcScopedContext) -> Result<Vec<Value>> {
        let args: Vec<_> = node
            .iter()
            .map(|x| self.try_evaluate_expr(x, ctx).map(Value::expr))
            .try_collect()?;
        Ok(args)
    }
    pub fn interpret_struct_expr(
        &self,
        node: &StructExpr,
        ctx: &ArcScopedContext,
    ) -> Result<StructValue> {
        let struct_ = self
            .type_system
            .evaluate_type_expr(&node.name, ctx)?
            .as_struct()
            .context("Expected struct")?
            .clone();
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
        Ok(StructValue {
            ty: struct_,
            structural: StructuralValue { fields },
        })
    }
    pub fn interpret_struct_value(
        &self,
        node: &StructValue,
        ctx: &ArcScopedContext,
    ) -> Result<StructValue> {
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
        Ok(StructValue {
            ty: node.ty.clone(),
            structural: StructuralValue { fields },
        })
    }
    pub fn interpret_select(&self, s: &Select, ctx: &ArcScopedContext) -> Result<Value> {
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
        node: &TupleValue,
        ctx: &ArcScopedContext,
        resolve: bool,
    ) -> Result<TupleValue> {
        let values: Vec<_> = node
            .values
            .iter()
            .map(|x| self.interpret_value(x, ctx, resolve))
            .try_collect()?;
        Ok(TupleValue {
            values: values.into_iter().map(|x| x.into()).collect(),
        })
    }
    pub fn interpret_type(&self, node: &TypeValue, ctx: &ArcScopedContext) -> Result<TypeValue> {
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
    ) -> Result<FunctionValue> {
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
        let sig = FunctionSignature {
            name: node.sig.name.clone(),
            params,
            generics_params: node.generics_params.clone(),
            ret: self.interpret_type(&node.ret, &sub)?,
        };

        Ok(FunctionValue {
            sig,
            body: node.body.clone(),
        })
    }
    pub fn interpret_value(
        &self,
        val: &Value,
        ctx: &ArcScopedContext,
        resolve: bool,
    ) -> Result<Value> {
        match val {
            Value::Type(n) => self.interpret_type(n, ctx).map(Value::Type),
            Value::Struct(n) => self.interpret_struct_value(n, ctx).map(Value::Struct),
            Value::Structural(_) => bail!("Failed to interpret {:?}", val),
            Value::Function(n) => self.interpret_function_value(n, ctx).map(Value::Function),
            Value::Tuple(n) => self.interpret_tuple(n, ctx, resolve).map(Value::Tuple),
            Value::Expr(n) => self.interpret_expr(n, ctx),
            Value::Any(n) => {
                if let Some(exp) = n.downcast_ref::<LazyValue<Expr>>() {
                    let ret = self.interpret_expr(&exp.expr, &exp.ctx)?;
                    return Ok(ret);
                }

                if self.ignore_missing_items {
                    return Ok(val.clone());
                }

                bail!("Failed to interpret {:?}", val)
            }
            Value::Int(_) => Ok(val.clone()),
            Value::Bool(_) => Ok(val.clone()),
            Value::Decimal(_) => Ok(val.clone()),
            Value::Char(_) => Ok(val.clone()),
            Value::String(_) => Ok(val.clone()),
            Value::List(_) => Ok(val.clone()),
            Value::Unit(_) => Ok(val.clone()),
            Value::Null(_) => Ok(val.clone()),
            Value::None(_) => Ok(val.clone()),
            Value::Some(val) => Ok(Value::Some(SomeValue::new(
                self.interpret_value(&val.value, ctx, resolve)?.into(),
            ))),
            Value::Option(value) => Ok(Value::Option(OptionValue::new(
                value
                    .value
                    .as_ref()
                    .map(|x| self.interpret_value(&x, ctx, resolve))
                    .transpose()?,
            ))),
            Value::Undefined(_) => Ok(val.clone()),
            Value::BinOpKind(x) if resolve => self
                .interpret_bin_op_kind(x.clone(), ctx)
                .map(|x| Value::any(x)),
            Value::BinOpKind(_) => Ok(val.clone()),
            Value::UnOpKind(_) => Ok(val.clone()),
        }
    }
    pub fn interpret_invoke_binop(
        &self,
        op: BinOpKind,
        args: &[Expr],
        ctx: &ArcScopedContext,
    ) -> Result<Value> {
        let builtin_fn = self.interpret_bin_op_kind(op, ctx)?;
        let args = self.interpret_args(args, ctx)?;
        builtin_fn.invoke(&args, ctx)
    }
    pub fn interpret_invoke_unop(
        &self,
        op: UnOpKind,
        arg: Value,
        _ctx: &ArcScopedContext,
    ) -> Result<Value> {
        match op {
            UnOpKind::Neg => match arg {
                Value::Int(val) => Ok(Value::Int(IntValue::new(-val.value))),
                Value::Decimal(val) => Ok(Value::Decimal(DecimalValue::new(-val.value))),
                _ => bail!("Failed to interpret {:?}", op),
            },
            UnOpKind::Not => match arg {
                Value::Bool(val) => Ok(Value::Bool(BoolValue::new(!val.value))),
                _ => bail!("Failed to interpret {:?}", op),
            },
            _ => bail!("Could not process {:?}", op),
        }
    }
    pub fn interpret_expr_common(
        &self,
        node: &Expr,
        ctx: &ArcScopedContext,
        resolve: bool,
    ) -> Result<Value> {
        match node {
            Expr::Locator(Locator::Ident(n)) => self.interpret_ident(n, ctx, resolve),
            Expr::Locator(n) => ctx
                .get_value_recursive(n)
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
    pub fn interpret_expr(&self, node: &Expr, ctx: &ArcScopedContext) -> Result<Value> {
        self.interpret_expr_common(node, ctx, true)
    }
    pub fn interpret_expr_no_resolve(&self, node: &Expr, ctx: &ArcScopedContext) -> Result<Value> {
        self.interpret_expr_common(node, ctx, false)
    }
    pub fn interpret_item(&self, node: &Item, ctx: &ArcScopedContext) -> Result<Value> {
        debug!("Interpreting {}", self.serializer.serialize_item(&node)?);
        match node {
            Item::Module(n) => self.interpret_module(n, ctx),
            Item::Define(n) => self.interpret_def(n, ctx).map(|_| Value::unit()),
            Item::Import(n) => self.interpret_import(n, ctx).map(|_| Value::unit()),

            Item::Any(n) => Ok(Value::Any(n.clone())),
            _ => bail!("Failed to interpret {:?}", node),
        }
    }

    pub fn interpret_let(&self, node: &StatementLet, ctx: &ArcScopedContext) -> Result<Value> {
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
        ctx: &ArcScopedContext,
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

    pub fn interpret_tree(&self, node: &Tree, ctx: &ArcScopedContext) -> Result<Value> {
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
    fn optimize_expr_post(&self, expr: Expr, ctx: &ArcScopedContext) -> Result<Expr> {
        let value = self.interpret_expr_no_resolve(&expr, ctx)?;
        Ok(Expr::value(value))
    }
    fn optimize_item_pre(&self, item: Item, _ctx: &ArcScopedContext) -> Result<Item> {
        match item {
            Item::Define(def) if def.name.as_str() == "main" => match def.value {
                DefineValue::Function(func) => Ok(Item::Expr(func.body)),
                _ => bail!("main should be a function"),
            },
            _ => Ok(Item::Expr(Expr::unit())),
        }
    }
    fn evaluate_condition(
        &self,
        expr: Expr,
        ctx: &ArcScopedContext,
    ) -> Result<Option<ControlFlow>> {
        let value = self.interpret_expr_no_resolve(&expr, ctx)?;
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
    fn evaluate_invoke(
        &self,
        _invoke: Invoke,
        _ctx: &ArcScopedContext,
    ) -> Result<Option<ControlFlow>> {
        Ok(Some(ControlFlow::Into))
    }
    fn try_evaluate_expr(&self, pat: &Expr, ctx: &ArcScopedContext) -> Result<Expr> {
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
    fn optimize_bin_op(&self, invoke: Invoke, ctx: &ArcScopedContext) -> Result<Expr> {
        let value = self.interpret_invoke(&invoke, ctx)?;
        Ok(Expr::value(value))
    }
}
