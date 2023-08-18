use crate::ast::*;
use crate::context::{LazyValue, ScopedContext};
use crate::ops::*;
use crate::type_system::TypeSystem;
use crate::value::*;
use crate::*;
use common::*;
use std::rc::Rc;

pub struct Interpreter {
    pub serializer: Rc<dyn Serializer>,
    pub type_system: TypeSystem,
    pub ignore_missing_items: bool,
}
impl Interpreter {
    pub fn new(serializer: Rc<dyn Serializer>) -> Self {
        Self {
            type_system: TypeSystem::new(serializer.clone()),
            serializer,
            ignore_missing_items: false,
        }
    }
    pub fn interpret_module(&self, node: &Module, ctx: &ScopedContext) -> Result<Value> {
        node.items.iter().for_each(|x| match x {
            Item::Def(x) => match &x.value {
                DefValue::Function(n) => {
                    debug!("Inserting function {} into context", x.name);

                    ctx.insert_function(&x.name, n.clone());
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
    pub fn interpret_invoke(&self, node: &Invoke, ctx: &ScopedContext) -> Result<Value> {
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
                let sub = ctx.child();
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
    pub fn interpret_import(&self, _node: &Import, _ctx: &ScopedContext) -> Result<()> {
        Ok(())
    }
    pub fn interpret_block(&self, node: &Block, ctx: &ScopedContext) -> Result<Value> {
        let ctx = ctx.child();
        let ret: Vec<_> = node
            .stmts
            .iter()
            .map(|x| self.interpret_stmt(x, &ctx))
            .try_collect()?;
        if !ret.is_empty() {
            Ok(ret.last().cloned().unwrap())
        } else {
            Ok(Value::unit())
        }
    }
    pub fn interpret_cond(&self, node: &Cond, ctx: &ScopedContext) -> Result<Value> {
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
    pub fn interpret_print(se: &dyn Serializer, args: &[Expr], ctx: &ScopedContext) -> Result<()> {
        let formatted: Vec<_> = args
            .into_iter()
            .map(|x| se.serialize_expr(x))
            .try_collect()?;
        ctx.root().print_str(formatted.join(" "));
        Ok(())
    }
    pub fn interpret_ident(&self, ident: &Ident, ctx: &ScopedContext) -> Result<Value> {
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
                ctx.get_value_recursive(ident)
                    // .or_else(|| {
                    //     if self.ignore_missing_items {
                    //         Some(Value::expr(Expr::Ident(ident.clone())))
                    //     } else {
                    //         None
                    //     }
                    // })
                    .with_context(|| format!("could not find {:?} in context", ident.name))
            }
        };
    }
    pub fn interpret_bin_op_kind(&self, op: BinOpKind, _ctx: &ScopedContext) -> Result<BuiltinFn> {
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
    pub fn interpret_def(&self, def: &Define, ctx: &ScopedContext) -> Result<()> {
        match &def.value {
            DefValue::Function(n) => {
                return if def.name == Ident::new("main") {
                    self.interpret_expr(&n.body, ctx).map(|_| ())
                } else {
                    let name = &def.name;
                    ctx.insert_function(name, n.clone());
                    Ok(())
                };
            }
            DefValue::Type(n) => {
                ctx.insert_type(
                    &def.name,
                    TypeValue::any_box(LazyValue {
                        ctx: ctx.clone(),
                        expr: n.clone(),
                    }),
                );
                return Ok(());
            }
            DefValue::Const(n) => {
                ctx.insert_value(
                    &def.name,
                    Value::any(LazyValue {
                        ctx: ctx.clone(),
                        expr: n.clone(),
                    }),
                );
                return Ok(());
            }
        }
    }
    pub fn interpret_args(&self, node: &[Expr], ctx: &ScopedContext) -> Result<Vec<Value>> {
        let args: Vec<_> = node
            .iter()
            .map(|x| self.interpret_expr(x, ctx))
            .try_collect()?;
        Ok(args)
    }
    pub fn interpret_struct_value(
        &self,
        node: &StructValue,
        ctx: &ScopedContext,
    ) -> Result<StructValue> {
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
    pub fn interpret_select(&self, s: &Select, ctx: &ScopedContext) -> Result<Expr> {
        let obj = self.interpret_expr(&s.obj, ctx)?;
        // TODO: try to select values
        Ok(Expr::Select(Select {
            obj: Expr::value(obj).into(),
            field: s.field.clone(),
            select: s.select,
        }))
    }
    pub fn interpret_tuple(&self, node: &TupleValue, ctx: &ScopedContext) -> Result<TupleValue> {
        let values: Vec<_> = node
            .values
            .iter()
            .map(|x| self.interpret_value(x, ctx))
            .try_collect()?;
        Ok(TupleValue {
            values: values.into_iter().map(|x| x.into()).collect(),
        })
    }
    pub fn interpret_type(&self, node: &TypeValue, ctx: &ScopedContext) -> Result<TypeValue> {
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
        ctx: &ScopedContext,
    ) -> Result<FunctionValue> {
        let sub = ctx.child();
        for generic in &node.generics_params {
            let ty = self
                .type_system
                .evaluate_type_bounds(&generic.bounds, ctx)?;
            sub.insert_type(&generic.name, ty);
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
    pub fn interpret_value(&self, node: &Value, ctx: &ScopedContext) -> Result<Value> {
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
    pub fn interpret_expr_inner(&self, node: &Expr, ctx: &ScopedContext) -> Result<Value> {
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
    pub fn interpret_expr(&self, node: &Expr, ctx: &ScopedContext) -> Result<Value> {
        // trace!("Interpreting {}", self.serializer.serialize_expr(&node)?);
        let result = self.interpret_expr_inner(node, ctx);
        match result {
            Ok(result) => {
                debug!(
                    "Interpreted {} => {}",
                    self.serializer.serialize_expr(&node)?,
                    self.serializer.serialize_value(&result)?
                );
                Ok(result)
            }
            Err(err) => {
                // warn!(
                //     "Failed to interpret {} => {:?}",
                //     self.serializer.serialize_expr(&node)?,
                //     err
                // );
                Err(err)
            }
        }
    }
    pub fn interpret_item_inner(&self, node: &Item, ctx: &ScopedContext) -> Result<Value> {
        debug!("Interpreting {}", self.serializer.serialize_item(&node)?);
        match node {
            Item::Module(n) => self.interpret_module(n, ctx),
            Item::Def(n) => self.interpret_def(n, ctx).map(|_| Value::unit()),
            Item::Import(n) => self.interpret_import(n, ctx).map(|_| Value::unit()),

            Item::Any(n) => Ok(Value::Any(n.clone())),
            _ => bail!("Failed to interpret {:?}", node),
        }
    }
    pub fn interpret_item(&self, node: &Item, ctx: &ScopedContext) -> Result<Value> {
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
    pub fn interpret_let(&self, node: &Let, ctx: &ScopedContext) -> Result<Value> {
        let value = self.interpret_expr(&node.value, ctx)?;
        ctx.insert_value(node.name.clone(), value.clone());
        Ok(value)
    }
    pub fn interpret_stmt_inner(&self, node: &Statement, ctx: &ScopedContext) -> Result<Value> {
        debug!("Interpreting {}", self.serializer.serialize_stmt(&node)?);
        match node {
            Statement::Item(n) => self.interpret_item(n, ctx),
            Statement::Let(n) => self.interpret_let(n, ctx).map(|_| Value::unit()),
            Statement::Expr(n) => self.interpret_expr(n, ctx),
            Statement::SideEffect(n) => self.interpret_expr(&n.expr, ctx).map(|_| Value::unit()),
            Statement::Any(n) => Ok(Value::Any(n.clone())),
        }
    }
    pub fn interpret_stmt(&self, node: &Statement, ctx: &ScopedContext) -> Result<Value> {
        debug!("Interpreting {}", self.serializer.serialize_stmt(&node)?);
        let result = self.interpret_stmt_inner(node, ctx);
        match result {
            Ok(result) => {
                debug!(
                    "Interpreted {} => {}",
                    self.serializer.serialize_stmt(&node)?,
                    self.serializer.serialize_value(&result)?
                );
                Ok(result)
            }
            Err(err) => {
                // warn!(
                //     "Failed to interpret {} => {:?}",
                //     self.serializer.serialize_stmt(&node)?,
                //     err
                // );
                Err(err)
            }
        }
    }

    pub fn interpret_tree(&self, node: &Tree, ctx: &ScopedContext) -> Result<Value> {
        match node {
            Tree::Item(item) => self.interpret_item(item, ctx),
            Tree::Expr(expr) => self.interpret_expr(expr, ctx),
            Tree::File(file) => self.interpret_module(&file.module, ctx),
        }
    }
}
