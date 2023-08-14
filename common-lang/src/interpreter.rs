use crate::context::ExecutionContext;
use crate::ops::*;
use crate::tree::*;
use crate::type_system::TypeSystem;
use crate::value::*;
use crate::*;
use common::*;

use std::rc::Rc;

pub struct Interpreter {
    pub serializer: Rc<dyn Serializer>,
    pub ignore_missing_items: bool,
}
impl Interpreter {
    pub fn new(serializer: Rc<dyn Serializer>) -> Self {
        Self {
            serializer,
            ignore_missing_items: false,
        }
    }
    pub fn interpret_module(&self, node: &Module, ctx: &ExecutionContext) -> Result<Expr> {
        node.items.iter().for_each(|x| match x {
            Item::Def(x) => match &x.value {
                DefValue::Function(n) => {
                    debug!("Inserting function {} into context", x.name);

                    ctx.insert_func_decl(&x.name, n.clone());
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
        Ok(result.into_iter().next().unwrap_or(Expr::unit()))
    }
    pub fn interpret_invoke(&self, node: &Invoke, ctx: &ExecutionContext) -> Result<Expr> {
        debug!(
            "Will execute call {}",
            self.serializer.serialize_invoke(&node)?
        );
        let fun = self.interpret_expr(&node.fun, ctx)?;
        debug!(
            "Will call function {}",
            self.serializer.serialize_expr(&fun)?
        );
        let args = self.interpret_args(&node.args, ctx)?;
        match fun {
            Expr::Value(Value::Function(f)) => {
                let name = self.serializer.serialize_expr(&node.fun)?;
                let sub = ctx.child();
                for (i, arg) in args.iter().cloned().enumerate() {
                    let param = f
                        .params
                        .get(i)
                        .with_context(|| format!("Couldn't find {} parameter of {:?}", i, f))?;
                    // TODO: type check here

                    sub.insert_expr(param.name.clone().into(), arg);
                }
                let args_ = self.serializer.serialize_exprs(&args)?;
                debug!("Invoking {} with {}", name, args_);
                let ret = self.interpret_block(&f.body, &sub)?.unwrap_or(Expr::unit());
                let ret_ = self.serializer.serialize_expr(&ret)?;
                debug!("Invoked {} with {} => {}", name, args_, ret_);
                return Ok(ret);
            }

            Expr::Select(s) => {
                // FIXME this is hack for rust
                if s.field.as_str() == "into" {
                    return Ok(*s.obj);
                }
                bail!("Failed to interpret {:?}", node);
            }
            Expr::Any(any) => {
                if let Some(f) = any.downcast_ref::<BuiltinFn>() {
                    let args_ = self.serializer.serialize_exprs(&args)?;

                    debug!("Invoking {} with {}", f.name, args_);
                    let ret = f.call(&args, ctx)?;
                    let ret_ = self.serializer.serialize_expr(&ret)?;

                    debug!("Invoked {} with {} => {}", f.name, args_, ret_);
                    return Ok(ret);
                }
                bail!("Failed to interpret {:?}", node);
            }
            _ => bail!("Failed to interpret {:?}", node),
        }
    }
    pub fn interpret_import(&self, _node: &Import, _ctx: &ExecutionContext) -> Result<()> {
        Ok(())
    }
    pub fn interpret_block(&self, node: &Block, ctx: &ExecutionContext) -> Result<Option<Expr>> {
        let ret: Vec<_> = node
            .stmts
            .iter()
            .map(|x| self.interpret_item(x, ctx))
            .try_collect()?;
        if node.last_value && !ret.is_empty() {
            Ok(Some(ret.last().cloned().unwrap()))
        } else {
            Ok(None)
        }
    }
    pub fn interpret_cond(&self, node: &Cond, ctx: &ExecutionContext) -> Result<Expr> {
        for case in &node.cases {
            let interpret = self.interpret_expr(&case.cond, ctx)?;
            match interpret {
                Expr::Value(Value::Bool(x)) => {
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
        Ok(Expr::unit())
    }
    pub fn interpret_print(
        se: &dyn Serializer,
        args: &[Expr],
        ctx: &ExecutionContext,
    ) -> Result<()> {
        let formatted: Vec<_> = args
            .into_iter()
            .map(|x| se.serialize_expr(x))
            .try_collect()?;
        ctx.root().print_str(formatted.join(" "));
        Ok(())
    }
    pub fn interpret_ident(&self, ident: &Ident, ctx: &ExecutionContext) -> Result<Expr> {
        return match ident.as_str() {
            "+" => Ok(Expr::any(builtin_add())),
            "-" => Ok(Expr::any(builtin_sub())),
            "*" => Ok(Expr::any(builtin_mul())),
            ">" => Ok(Expr::any(builtin_gt())),
            ">=" => Ok(Expr::any(builtin_ge())),
            "==" => Ok(Expr::any(builtin_eq())),
            "<=" => Ok(Expr::any(builtin_le())),
            "<" => Ok(Expr::any(builtin_lt())),
            "print" => Ok(Expr::any(builtin_print(self.serializer.clone()))),
            "true" => Ok(Expr::value(Value::bool(true))),
            "false" => Ok(Expr::value(Value::bool(false))),
            _ => ctx
                .get_expr(ident)
                .or_else(|| {
                    if self.ignore_missing_items {
                        Some(Expr::Ident(ident.clone()))
                    } else {
                        None
                    }
                })
                .with_context(|| format!("could not find {:?} in context", ident.name)),
        };
    }
    pub fn interpret_bin_op_kind(&self, op: BinOpKind, _ctx: &ExecutionContext) -> Result<Expr> {
        match op {
            BinOpKind::Add => Ok(Expr::any(builtin_add())),
            BinOpKind::Sub => Ok(Expr::any(builtin_sub())),
            BinOpKind::Mul => Ok(Expr::any(builtin_mul())),
            // BinOpKind::Div => Ok(Expr::any(builtin_div())),
            // BinOpKind::Mod => Ok(Expr::any(builtin_mod())),
            BinOpKind::Gt => Ok(Expr::any(builtin_gt())),
            BinOpKind::Lt => Ok(Expr::any(builtin_lt())),
            BinOpKind::Ge => Ok(Expr::any(builtin_ge())),
            BinOpKind::Le => Ok(Expr::any(builtin_le())),
            BinOpKind::Eq => Ok(Expr::any(builtin_eq())),
            BinOpKind::Ne => Ok(Expr::any(builtin_ne())),
            // BinOpKind::LogicalOr => {}
            // BinOpKind::LogicalAnd => {}
            // BinOpKind::BitOr => {}
            // BinOpKind::BitAnd => {}
            // BinOpKind::BitXor => {}
            // BinOpKind::Any(_) => {}
            _ => bail!("Could not process {:?}", op),
        }
    }
    pub fn interpret_def(&self, def: &Define, ctx: &ExecutionContext) -> Result<()> {
        match &def.value {
            DefValue::Function(n) => {
                return if def.name == Ident::new("main") {
                    self.interpret_block(&n.body, ctx).map(|_| ())
                } else {
                    let name = &def.name;
                    ctx.insert_func_decl(name, n.clone());
                    Ok(())
                };
            }
            DefValue::Type(_) => {}
            DefValue::Const(_) => {}
            DefValue::Variable(_) => {}
        }

        bail!("Could not process {:?}", def)
    }
    pub fn interpret_args(&self, node: &[Expr], ctx: &ExecutionContext) -> Result<Vec<Expr>> {
        let args: Vec<_> = node
            .iter()
            .map(|x| self.interpret_expr(x, ctx))
            .try_collect()?;
        Ok(args)
    }
    pub fn interpret_struct_value(
        &self,
        node: &StructValue,
        ctx: &ExecutionContext,
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
    pub fn interpret_select(&self, s: &Select, ctx: &ExecutionContext) -> Result<Expr> {
        let obj = self.interpret_expr(&s.obj, ctx)?;
        // TODO: try to select values
        Ok(Expr::Select(Select {
            obj: obj.into(),
            field: s.field.clone(),
            select: s.select,
        }))
    }
    pub fn interpret_tuple(&self, node: &TupleValue, ctx: &ExecutionContext) -> Result<TupleValue> {
        let values: Vec<_> = node
            .values
            .iter()
            .map(|x| self.interpret_value(x, ctx))
            .try_collect()?;
        Ok(TupleValue {
            values: values.into_iter().map(|x| x.into()).collect(),
        })
    }
    pub fn interpret_type(&self, node: &TypeValue, ctx: &ExecutionContext) -> Result<TypeValue> {
        let ty = TypeSystem::new(self.serializer.clone());
        ty.evaluate_type_value(node, ctx)
    }
    pub fn interpret_function_value(
        &self,
        node: &FunctionValue,
        ctx: &ExecutionContext,
    ) -> Result<FunctionValue> {
        let params: Vec<_> = node
            .params
            .iter()
            .map(|x| {
                Ok::<_, Error>(FunctionParam {
                    name: x.name.clone(),
                    ty: self.interpret_type(&x.ty, ctx)?,
                })
            })
            .try_collect()?;

        Ok(FunctionValue {
            name: node.name.clone(),
            params,
            generics_params: node.generics_params.clone(),
            ret: self.interpret_type(&node.ret, ctx)?,
            body: node.body.clone(),
        })
    }
    pub fn interpret_value(&self, node: &Value, ctx: &ExecutionContext) -> Result<Value> {
        match node {
            Value::Int(_) => Ok(node.clone()),
            Value::Bool(_) => Ok(node.clone()),
            Value::Decimal(_) => Ok(node.clone()),
            Value::Char(_) => Ok(node.clone()),
            Value::String(_) => Ok(node.clone()),
            Value::List(_) => Ok(node.clone()),
            Value::Unit(_) => Ok(node.clone()),
            Value::Type(n) => self.interpret_type(n, ctx).map(Value::Type),
            Value::Struct(n) => self.interpret_struct_value(n, ctx).map(Value::Struct),
            Value::Function(n) => self.interpret_function_value(n, ctx).map(Value::Function),
            Value::Tuple(n) => self.interpret_tuple(n, ctx).map(Value::Tuple),
            Value::Expr(n) => self.interpret_expr(n, ctx).map(Box::new).map(Value::Expr),
        }
    }
    pub fn interpret_expr_inner(&self, node: &Expr, ctx: &ExecutionContext) -> Result<Expr> {
        debug!("Interpreting {}", self.serializer.serialize_expr(&node)?);
        match node {
            Expr::Ident(n) => return self.interpret_ident(n, ctx),
            Expr::Path(n) => {
                return ctx
                    .get_expr(n)
                    .with_context(|| format!("could not find {:?} in context", n))
            }
            Expr::Value(n) => return Ok(Expr::value(self.interpret_value(n, ctx)?.into())),
            Expr::Block(n) => {
                return self
                    .interpret_block(n, ctx)
                    .map(|x| x.unwrap_or(Expr::unit()))
            }
            Expr::Cond(c) => {
                return self.interpret_cond(c, ctx);
            }
            Expr::Invoke(invoke) => {
                return self.interpret_invoke(invoke, ctx);
            }
            Expr::Any(_) => {
                return Ok(node.clone());
            }
            Expr::BinOpKind(bin_op) => {
                return self.interpret_bin_op_kind(bin_op.clone(), ctx);
            }
            _ => {}
        }

        bail!("Failed to interpret {:?}", node)
    }
    pub fn interpret_expr(&self, node: &Expr, ctx: &ExecutionContext) -> Result<Expr> {
        debug!("Interpreting {}", self.serializer.serialize_expr(&node)?);
        let result = self.interpret_expr_inner(node, ctx);
        debug!(
            "Interpreted {} => {:?}",
            self.serializer.serialize_expr(&node)?,
            result
        );
        result
    }
    pub fn interpret_item(&self, node: &Item, ctx: &ExecutionContext) -> Result<Expr> {
        debug!("Interpreting {}", self.serializer.serialize_item(&node)?);
        match node {
            Item::Module(n) => self.interpret_module(n, ctx),
            Item::Def(n) => self.interpret_def(n, ctx).map(|_| Expr::unit()),
            Item::Import(n) => self.interpret_import(n, ctx).map(|_| Expr::unit()),
            Item::Expr(n) => self.interpret_expr(n, ctx),
            Item::Any(n) => Ok(Expr::any(n.clone())),
            _ => bail!("Failed to interpret {:?}", node),
        }
    }
    pub fn interpret_tree(&self, node: &Tree, ctx: &ExecutionContext) -> Result<Expr> {
        match node {
            Tree::Item(item) => self.interpret_item(item, ctx),
        }
    }
}
