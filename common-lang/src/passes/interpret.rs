use crate::ast::*;
use crate::context::{ArcScopedContext, LazyValue};
use crate::ops::*;
use crate::passes::OptimizePass;
use crate::type_system::TypeSystem;
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
        match &*node.func {
            Expr::Value(Value::BinOpKind(kind)) => {
                self.interpret_invoke_binop(kind.clone(), &node.args, ctx)
            }
            Expr::Pat(pat) if pat.to_string() == "print" => {
                Self::interpret_print(self.serializer.as_ref(), &node.args, ctx)?;
                Ok(Value::unit())
            }
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
    pub fn interpret_cond(&self, node: &Cond, ctx: &ArcScopedContext) -> Result<Value> {
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
    pub fn interpret_args(&self, node: &[Expr], ctx: &ArcScopedContext) -> Result<Vec<Value>> {
        let args: Vec<_> = node
            .iter()
            .map(|x| self.try_evaluate_expr(x, ctx).map(Value::expr))
            .try_collect()?;
        Ok(args)
    }
    pub fn interpret_struct_value(
        &self,
        node: &StructValue,
        ctx: &ArcScopedContext,
    ) -> Result<StructValue> {
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
            name: node.name.clone(),
            fields,
        })
    }
    pub fn interpret_select(&self, s: &Select, ctx: &ArcScopedContext) -> Result<Expr> {
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

        Ok(FunctionValue {
            name: node.name.clone(),
            params,
            generics_params: node.generics_params.clone(),
            ret: self.interpret_type(&node.ret, &sub)?,
            body: node.body.clone(),
        })
    }
    pub fn interpret_value(
        &self,
        node: &Value,
        ctx: &ArcScopedContext,
        resolve: bool,
    ) -> Result<Value> {
        match node {
            Value::Type(n) => self.interpret_type(n, ctx).map(Value::Type),
            Value::Struct(n) => self.interpret_struct_value(n, ctx).map(Value::Struct),
            Value::Function(n) => self.interpret_function_value(n, ctx).map(Value::Function),
            Value::Tuple(n) => self.interpret_tuple(n, ctx, resolve).map(Value::Tuple),
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
            Value::BinOpKind(x) if resolve => self
                .interpret_bin_op_kind(x.clone(), ctx)
                .map(|x| Value::any(x)),
            Value::BinOpKind(_) => Ok(node.clone()),
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
    pub fn interpret_expr(&self, node: &Expr, ctx: &ArcScopedContext) -> Result<Value> {
        match node {
            Expr::Pat(Pat::Ident(n)) => self.interpret_ident(n, ctx, true),
            Expr::Pat(n) => ctx
                .get_value_recursive(n)
                .with_context(|| format!("could not find {:?} in context", n)),
            Expr::Value(n) => Ok(self.interpret_value(n, ctx, true)?.into()),
            Expr::Block(n) => self.interpret_block(n, ctx),
            Expr::Cond(c) => self.interpret_cond(c, ctx),
            Expr::Invoke(invoke) => self.interpret_invoke(invoke, ctx),
            Expr::Any(n) => Ok(Value::Any(n.clone())),

            _ => bail!("Failed to interpret {:?}", node),
        }
    }
    pub fn interpret_expr_no_resolve(&self, node: &Expr, ctx: &ArcScopedContext) -> Result<Value> {
        match node {
            Expr::Pat(Pat::Ident(n)) => self.interpret_ident(n, ctx, false),
            Expr::Pat(n) => ctx
                .get_value_recursive(n)
                .with_context(|| format!("could not find {:?} in context", n)),
            Expr::Value(n) => Ok(self.interpret_value(n, ctx, false)?.into()),
            Expr::Block(n) => self.interpret_block(n, ctx),
            Expr::Cond(c) => self.interpret_cond(c, ctx),
            Expr::Invoke(invoke) => self.interpret_invoke(invoke, ctx),
            Expr::Any(n) => Ok(Value::Any(n.clone())),

            _ => bail!("Failed to interpret {:?}", node),
        }
    }
    pub fn interpret_item(&self, node: &Item, ctx: &ArcScopedContext) -> Result<Value> {
        debug!("Interpreting {}", self.serializer.serialize_item(&node)?);
        match node {
            Item::Module(n) => self.interpret_module(n, ctx),
            Item::Def(n) => self.interpret_def(n, ctx).map(|_| Value::unit()),
            Item::Import(n) => self.interpret_import(n, ctx).map(|_| Value::unit()),

            Item::Any(n) => Ok(Value::Any(n.clone())),
            _ => bail!("Failed to interpret {:?}", node),
        }
    }

    pub fn interpret_let(&self, node: &Let, ctx: &ArcScopedContext) -> Result<Value> {
        let value = self.interpret_expr(&node.value, ctx)?;
        ctx.insert_value(node.name.clone(), value.clone());
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
            Statement::Any(_) => bail!("Failed to interpret {:?}", node),
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
