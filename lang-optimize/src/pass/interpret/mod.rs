mod typing;

use crate::pass::{FoldOptimizer, OptimizePass};
use common::*;
use itertools::Itertools;
use lang_core::ast::*;
use lang_core::context::SharedScopedContext;
use lang_core::ctx::{Context, ValueSystem};
use lang_core::id::{Ident, Locator};
use lang_core::ops::*;
use lang_core::utils::conv::TryConv;
use std::sync::Arc;

#[derive(Clone)]
pub struct InterpreterPass {
    pub serializer: Arc<dyn AstSerializer>,
    pub ignore_missing_items: bool,
}

impl InterpreterPass {
    pub fn new(serializer: Arc<dyn AstSerializer>) -> Self {
        Self {
            serializer,
            ignore_missing_items: false,
        }
    }

    pub fn interpret_items(&self, node: &ItemChunk, ctx: &SharedScopedContext) -> Result<AstValue> {
        let result: Vec<_> = node
            .iter()
            .map(|x| self.interpret_item(x, ctx))
            .try_collect()?;
        Ok(result.into_iter().next().unwrap_or(AstValue::unit()))
    }
    pub fn interpret_invoke(
        &self,
        node: &ExprInvoke,
        ctx: &SharedScopedContext,
    ) -> Result<AstValue> {
        // FIXME: call stack may not work properly
        match &node.target {
            ExprInvokeTarget::Function(Locator::Ident(ident)) => {
                let func = self.interpret_ident(&ident, ctx, true)?;
                self.interpret_invoke(
                    &ExprInvoke {
                        target: ExprInvokeTarget::expr(AstExpr::value(func).into()),
                        args: node.args.clone(),
                    },
                    ctx,
                )
            }
            ExprInvokeTarget::Method(select) => match select.field.as_str() {
                "to_string" => match &select.obj.get() {
                    AstExpr::Value(value) => match value.as_ref() {
                        AstValue::String(obj) => {
                            let mut obj = obj.clone();
                            obj.owned = true;
                            Ok(AstValue::String(obj))
                        }
                        _ => bail!("Expected string for {:?}", select),
                    },
                    _ => bail!("Expected struct for {:?}", select),
                },
                x => bail!("Could not invoke {:?}", x),
            },
            ExprInvokeTarget::Expr(e) => match e.as_ref() {
                AstExpr::Value(value) => match value.as_ref() {
                    AstValue::BinOpKind(kind) => {
                        self.interpret_invoke_binop(kind.clone(), &node.args, ctx)
                    }
                    AstValue::UnOpKind(func) => {
                        ensure!(node.args.len() == 1, "Expected 1 arg for {:?}", func);
                        let arg = self.interpret_expr(&node.args[0].get(), ctx)?;
                        self.interpret_invoke_unop(func.clone(), arg, ctx)
                    }
                    _ => bail!("Could not invoke {}", node),
                },

                AstExpr::Any(any) => {
                    if let Some(exp) = any.downcast_ref::<BuiltinFn>() {
                        let args = self.interpret_args(&node.args, ctx)?;
                        exp.invoke(&args, ctx)
                    } else {
                        bail!("Could not invoke {:?}", node)
                    }
                }
                _ => bail!("Could not invoke {:?}", node),
            },
            kind => bail!("Could not invoke {:?}", kind),
        }
    }
    pub fn interpret_import(&self, _node: &ItemImport, _ctx: &SharedScopedContext) -> Result<()> {
        Ok(())
    }
    pub fn interpret_block(&self, node: &ExprBlock, ctx: &SharedScopedContext) -> Result<AstValue> {
        let ctx = ctx.child(Ident::new("__block__"), Visibility::Private, true);
        for stmt in node.first_stmts() {
            self.interpret_stmt(&stmt, &ctx)?;
        }
        if let Some(expr) = node.last_expr() {
            self.interpret_expr(&expr, &ctx)
        } else {
            Ok(AstValue::unit())
        }
    }

    pub fn interpret_cond(&self, node: &ExprMatch, ctx: &SharedScopedContext) -> Result<AstValue> {
        for case in &node.cases {
            let interpret = self.interpret_expr(&case.cond, ctx)?;
            match interpret {
                AstValue::Bool(x) => {
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
        Ok(AstValue::unit())
    }
    pub fn interpret_print(
        se: &dyn AstSerializer,
        args: &[AstExpr],
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
    ) -> Result<AstValue> {
        match ident.as_str() {
            // TODO: can we remove these?
            "+" if resolve => Ok(AstValue::any(builtin_add())),
            "-" if resolve => Ok(AstValue::any(builtin_sub())),
            "*" if resolve => Ok(AstValue::any(builtin_mul())),
            ">" if resolve => Ok(AstValue::any(builtin_gt())),
            ">=" if resolve => Ok(AstValue::any(builtin_ge())),
            "==" if resolve => Ok(AstValue::any(builtin_eq())),
            "<=" if resolve => Ok(AstValue::any(builtin_le())),
            "<" if resolve => Ok(AstValue::any(builtin_lt())),
            "print" if resolve => Ok(AstValue::any(builtin_print(self.serializer.clone()))),
            "true" => Ok(AstValue::bool(true)),
            "false" => Ok(AstValue::bool(false)),
            "None" => Ok(AstValue::None(ValueNone)),
            "null" => Ok(AstValue::Null(ValueNull)),
            "unit" => Ok(AstValue::Unit(ValueUnit)),
            "undefined" => Ok(AstValue::Undefined(ValueUndefined)),
            "Some" => Ok(AstValue::any(builtin_some())),
            _ => {
                info!("Get value recursive {:?}", ident);
                ctx.print_values()?;
                ctx.get_value_recursive(ident)
                    .with_context(|| format!("could not find {:?} in context", ident.name))
            }
        }
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
                                AstValue::Type(AstType::ImplTraits(impls)) => Ok(impls.bounds),
                                _ => bail!("Expected impl Traits, got {:?}", value),
                            }
                        })
                        .try_collect()?;
                    Ok(AstType::ImplTraits(ImplTraits {
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
        def: &ItemDefFunction,
        ctx: &SharedScopedContext,
    ) -> Result<()> {
        let name = &def.name;
        ctx.insert_value_with_ctx(name.clone(), AstValue::Function(def._to_value()));
        Ok(())
    }
    pub fn interpret_def_struct(
        &self,
        def: &ItemDefStruct,
        ctx: &SharedScopedContext,
    ) -> Result<()> {
        ctx.insert_value_with_ctx(def.name.clone(), AstType::Struct(def.value.clone()).into());
        Ok(())
    }
    pub fn interpret_def_enum(&self, def: &ItemDefEnum, ctx: &SharedScopedContext) -> Result<()> {
        ctx.insert_value_with_ctx(def.name.clone(), AstType::Enum(def.value.clone()).into());
        Ok(())
    }
    pub fn interpret_def_type(&self, def: &ItemDefType, ctx: &SharedScopedContext) -> Result<()> {
        ctx.insert_value_with_ctx(def.name.clone(), AstValue::Type(def.value.clone()));
        Ok(())
    }
    pub fn interpret_def_const(&self, def: &ItemDefConst, ctx: &SharedScopedContext) -> Result<()> {
        let value = self.interpret_expr(&def.value, ctx)?;
        ctx.insert_value_with_ctx(def.name.clone(), value);
        Ok(())
    }
    pub fn interpret_args(
        &self,
        node: &[AstExpr],
        ctx: &SharedScopedContext,
    ) -> Result<Vec<AstValue>> {
        let args: Vec<_> = node
            .iter()
            .map(|x| self.try_evaluate_expr(&x.get(), ctx).map(AstValue::expr))
            .try_collect()?;
        Ok(args)
    }
    pub fn interpret_struct_expr(
        &self,
        node: &ExprStruct,
        ctx: &SharedScopedContext,
    ) -> Result<ValueStruct> {
        let value: AstValue = self.interpret_expr(&node.name.get(), ctx)?.try_conv()?;
        let ty: AstType = value.try_conv()?;
        let struct_ = ty.try_conv()?;
        let fields: Vec<_> = node
            .fields
            .iter()
            .map(|x| {
                Ok::<_, Error>(ValueField {
                    name: x.name.clone(),

                    value: match &x.value {
                        Some(value) => self.interpret_expr(value, ctx)?,
                        None => self.interpret_expr(&AstExpr::ident(x.name.clone()), ctx)?,
                    },
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
                Ok::<_, Error>(ValueField {
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
    pub fn interpret_select(&self, s: &ExprSelect, ctx: &SharedScopedContext) -> Result<AstValue> {
        let obj0 = self.interpret_expr(&s.obj.get(), ctx)?;
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
    pub fn interpret_type(&self, node: &AstType, ctx: &SharedScopedContext) -> Result<AstType> {
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
            receiver: None,
            params,
            generics_params: node.generics_params.clone(),
            ret_ty: if let Some(ret_ty) = &node.sig.ret_ty {
                Some(self.interpret_type(ret_ty, &sub)?)
            } else {
                None
            },
        };

        Ok(ValueFunction {
            sig,
            body: node.body.clone(),
        })
    }
    pub fn interpret_value(
        &self,
        val: &AstValue,
        ctx: &SharedScopedContext,
        resolve: bool,
    ) -> Result<AstValue> {
        match val {
            AstValue::Type(n) => self.interpret_type(n, ctx).map(AstValue::Type),
            AstValue::Struct(n) => self.interpret_struct_value(n, ctx).map(AstValue::Struct),
            AstValue::Structural(_) => bail!("Failed to interpret {:?}", val),
            AstValue::Function(n) => self
                .interpret_function_value(n, ctx)
                .map(AstValue::Function),
            AstValue::Tuple(n) => self.interpret_tuple(n, ctx, resolve).map(AstValue::Tuple),
            AstValue::Expr(n) => self.interpret_expr(&n.get(), ctx),
            AstValue::Any(_n) => {
                if self.ignore_missing_items {
                    return Ok(val.clone());
                }

                bail!("Failed to interpret {:?}", val)
            }
            AstValue::Some(val) => Ok(AstValue::Some(ValueSome::new(
                self.interpret_value(&val.value, ctx, resolve)?.into(),
            ))),
            AstValue::Option(value) => Ok(AstValue::Option(ValueOption::new(
                value
                    .value
                    .as_ref()
                    .map(|x| self.interpret_value(&x, ctx, resolve))
                    .transpose()?,
            ))),
            AstValue::BinOpKind(x) if resolve => {
                self.lookup_bin_op_kind(x.clone()).map(|x| AstValue::any(x))
            }
            _ => Ok(val.clone()),
        }
    }
    pub fn interpret_binop(
        &self,
        binop: &ExprBinOp,
        ctx: &SharedScopedContext,
    ) -> Result<AstValue> {
        let builtin_fn = self.lookup_bin_op_kind(binop.kind.clone())?;
        let lhs = self.interpret_expr(&binop.lhs.get(), ctx)?;
        let rhs = self.interpret_expr(&binop.rhs.get(), ctx)?;
        builtin_fn.invoke(&vec![lhs, rhs], ctx)
    }
    pub fn interpret_invoke_binop(
        &self,
        op: BinOpKind,
        args: &[AstExpr],
        ctx: &SharedScopedContext,
    ) -> Result<AstValue> {
        let builtin_fn = self.lookup_bin_op_kind(op)?;
        let args = self.interpret_args(args, ctx)?;
        builtin_fn.invoke(&args, ctx)
    }
    pub fn interpret_invoke_unop(
        &self,
        op: UnOpKind,
        arg: AstValue,
        _ctx: &SharedScopedContext,
    ) -> Result<AstValue> {
        match op {
            UnOpKind::Neg => match arg {
                AstValue::Int(val) => Ok(AstValue::Int(ValueInt::new(-val.value))),
                AstValue::Decimal(val) => Ok(AstValue::Decimal(ValueDecimal::new(-val.value))),
                _ => bail!("Failed to interpret {:?}", op),
            },
            UnOpKind::Not => match arg {
                AstValue::Bool(val) => Ok(AstValue::Bool(ValueBool::new(!val.value))),
                _ => bail!("Failed to interpret {:?}", op),
            },
            _ => bail!("Could not process {:?}", op),
        }
    }
    pub fn interpret_expr_common(
        &self,
        node: &AstExpr,
        ctx: &SharedScopedContext,
        resolve: bool,
    ) -> Result<AstValue> {
        match node {
            AstExpr::Locator(Locator::Ident(n)) => self.interpret_ident(n, ctx, resolve),
            AstExpr::Locator(n) => ctx
                .get_value_recursive(n.to_path())
                .with_context(|| format!("could not find {:?} in context", n)),
            AstExpr::Value(n) => self.interpret_value(n, ctx, resolve),
            AstExpr::Block(n) => self.interpret_block(n, ctx),
            AstExpr::Match(c) => self.interpret_cond(c, ctx),
            AstExpr::Invoke(invoke) => self.interpret_invoke(invoke, ctx),
            AstExpr::BinOp(op) => self.interpret_binop(op, ctx),
            AstExpr::Any(n) => Ok(AstValue::Any(n.clone())),
            AstExpr::Select(s) => self.interpret_select(s, ctx),
            AstExpr::Struct(s) => self.interpret_struct_expr(s, ctx).map(AstValue::Struct),
            _ => bail!("Failed to interpret {:?}", node),
        }
    }
    pub fn interpret_expr(&self, node: &AstExpr, ctx: &SharedScopedContext) -> Result<AstValue> {
        self.interpret_expr_common(node, ctx, true)
    }
    pub fn interpret_expr_no_resolve(
        &self,
        node: &AstExpr,
        ctx: &SharedScopedContext,
    ) -> Result<AstValue> {
        self.interpret_expr_common(node, ctx, false)
    }
    pub fn interpret_item(&self, node: &AstItem, ctx: &SharedScopedContext) -> Result<AstValue> {
        debug!("Interpreting {}", self.serializer.serialize_item(&node)?);
        match node {
            AstItem::Module(n) => self.interpret_items(&n.items, ctx),
            AstItem::DefFunction(n) => self
                .interpret_def_function(n, ctx)
                .map(|_| AstValue::unit()),
            AstItem::DefStruct(n) => self.interpret_def_struct(n, ctx).map(|_| AstValue::unit()),
            AstItem::DefEnum(n) => self.interpret_def_enum(n, ctx).map(|_| AstValue::unit()),
            AstItem::DefType(n) => self.interpret_def_type(n, ctx).map(|_| AstValue::unit()),
            AstItem::DefConst(n) => self.interpret_def_const(n, ctx).map(|_| AstValue::unit()),
            AstItem::Import(n) => self.interpret_import(n, ctx).map(|_| AstValue::unit()),

            AstItem::Any(n) => Ok(AstValue::Any(n.clone())),
            _ => bail!("Failed to interpret {:?}", node),
        }
    }

    pub fn interpret_let(&self, node: &StmtLet, ctx: &SharedScopedContext) -> Result<AstValue> {
        if let Some(init) = &node.init {
            let value = self.interpret_expr(&init, ctx)?;
            ctx.insert_value(
                node.pat.as_ident().context("Only supports ident")?.as_str(),
                value.clone(),
            );
            Ok(value)
        } else {
            ctx.insert_value(
                node.pat.as_ident().context("Only supports ident")?.as_str(),
                AstValue::undefined(),
            );
            Ok(AstValue::unit())
        }
    }

    pub fn interpret_stmt(
        &self,
        node: &BlockStmt,
        ctx: &SharedScopedContext,
    ) -> Result<Option<AstValue>> {
        debug!("Interpreting {}", self.serializer.serialize_stmt(&node)?);
        match node {
            BlockStmt::Let(n) => self.interpret_let(n, ctx).map(|_| None),
            BlockStmt::Expr(n) => {
                self.interpret_expr(&n.expr, ctx).map(
                    |x| {
                        if n.has_value() {
                            Some(x)
                        } else {
                            None
                        }
                    },
                )
            }
            BlockStmt::Item(_) => Ok(None),
            _ => bail!("Failed to interpret {:?}", node),
        }
    }

    pub fn interpret_tree(&self, node: &AstNode, ctx: &SharedScopedContext) -> Result<AstValue> {
        match node {
            AstNode::Item(item) => self.interpret_item(item, ctx),
            AstNode::Expr(expr) => self.interpret_expr(expr, ctx),
            AstNode::File(file) => self.interpret_items(&file.items, ctx),
        }
    }
}

impl OptimizePass for InterpreterPass {
    fn name(&self) -> &str {
        "interpreter"
    }
    fn optimize_expr(&self, expr: AstExpr, ctx: &SharedScopedContext) -> Result<AstExpr> {
        let value = self.interpret_expr_no_resolve(&expr, ctx)?;
        Ok(AstExpr::value(value))
    }

    fn optimize_item(&self, _item: AstItem, _ctx: &SharedScopedContext) -> Result<AstItem> {
        Ok(AstItem::unit())
    }

    fn evaluate_condition(&self, expr: AstExpr, ctx: &SharedScopedContext) -> Result<ControlFlow> {
        let value = self.interpret_expr_no_resolve(&expr, ctx)?;
        match value {
            AstValue::Bool(b) => {
                if b.value {
                    Ok(ControlFlow::IntoAndBreak(None))
                } else {
                    Ok(ControlFlow::Continue)
                }
            }
            _ => bail!("Failed to interpret {:?} => {:?}", expr, value),
        }
    }
    fn evaluate_invoke(
        &self,
        _invoke: ExprInvoke,
        _ctx: &SharedScopedContext,
    ) -> Result<ControlFlow> {
        Ok(ControlFlow::Into)
    }
    fn optimize_invoke(
        &self,
        invoke: ExprInvoke,
        func: &AstValue,
        ctx: &SharedScopedContext,
    ) -> Result<AstExpr> {
        match func {
            AstValue::Function(func) => self
                .interpret_expr(&func.body.get(), ctx)
                .map(AstExpr::value),
            AstValue::BinOpKind(kind) => self
                .interpret_invoke_binop(kind.clone(), &invoke.args, ctx)
                .map(AstExpr::value),
            AstValue::UnOpKind(func) => {
                ensure!(invoke.args.len() == 1, "Expected 1 arg for {:?}", func);
                let arg = self.interpret_expr(&invoke.args[0].get(), ctx)?;
                self.interpret_invoke_unop(func.clone(), arg, ctx)
                    .map(AstExpr::value)
            }
            _ => bail!("Could not invoke {:?}", func),
        }
    }

    fn try_evaluate_expr(&self, pat: &AstExpr, ctx: &SharedScopedContext) -> Result<AstExpr> {
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
        Ok(AstExpr::value(value))
    }
}

impl ValueSystem for InterpreterPass {
    fn get_value_from_expr(&self, ctx: &Context, expr: &AstExpr) -> Result<AstValue> {
        let fold = FoldOptimizer::new(self.serializer.clone(), Box::new(self.clone()));
        let expr = fold.optimize_expr(expr.clone(), &ctx.values)?;
        match expr {
            AstExpr::Value(value) => Ok(*value),
            _ => bail!("Expected value, got {:?}", expr),
        }
    }
}
