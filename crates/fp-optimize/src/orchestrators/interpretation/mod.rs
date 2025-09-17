mod typing;

use crate::utils::{FoldOptimizer, OptimizePass};
// Replace common::* with specific imports
use itertools::Itertools;
use tracing::debug;
use fp_core::error::Result;
use fp_core::ast::*;
use fp_core::context::SharedScopedContext;
use fp_core::ctx::{Context, ValueSystem};
use fp_core::id::{Ident, Locator};
use fp_core::ops::*;
use fp_core::utils::conv::TryConv;
use std::sync::Arc;
// use std::any::Any;

// Import our error helpers
use crate::error::optimization_error;
use crate::opt_ensure;
use crate::opt_bail;

#[derive(Clone)]
pub struct InterpretationOrchestrator {
    pub serializer: Arc<dyn AstSerializer>,
    pub ignore_missing_items: bool,
}

impl InterpretationOrchestrator {
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
            ExprInvokeTarget::Function(Locator::Path(path)) => {
                // For single-segment paths like "println", try to get it as an identifier first
                if path.segments.len() == 1 {
                    let func = self.interpret_ident(&path.segments[0], ctx, true)?;
                    self.interpret_invoke(
                        &ExprInvoke {
                            target: ExprInvokeTarget::expr(AstExpr::value(func).into()),
                            args: node.args.clone(),
                        },
                        ctx,
                    )
                } else {
                    // For multi-segment paths, use context lookup
                    let func = ctx
                        .get_value_recursive(path)
                        .ok_or_else(|| optimization_error(format!("could not find function {:?} in context", path)))?;
                    self.interpret_invoke(
                        &ExprInvoke {
                            target: ExprInvokeTarget::expr(AstExpr::value(func).into()),
                            args: node.args.clone(),
                        },
                        ctx,
                    )
                }
            }
            ExprInvokeTarget::Method(select) => {
                // First evaluate the object
                let obj_value = self.interpret_expr(&select.obj.get(), ctx)?;
                
                match select.field.as_str() {
                    "to_string" => match obj_value {
                        AstValue::String(mut obj) => {
                            obj.owned = true;
                            Ok(AstValue::String(obj))
                        }
                        _ => opt_bail!(format!("to_string not supported for {:?}", obj_value)),
                    },
                    "len" => match obj_value {
                        AstValue::String(s) => Ok(AstValue::int(s.value.len() as i64)),
                        _ => opt_bail!(format!("len() not supported for {:?}", obj_value)),
                    },
                    "contains" => {
                        if node.args.len() != 1 {
                            opt_bail!(format!("contains() expects 1 argument, got {}", node.args.len()));
                        }
                        let search_arg = self.interpret_expr(&node.args[0], ctx)?;
                        match (&obj_value, &search_arg) {
                            (AstValue::String(s), AstValue::String(search)) => {
                                Ok(AstValue::bool(s.value.contains(&search.value)))
                            }
                            _ => opt_bail!(format!("contains() expects string arguments, got {:?}.contains({:?})", obj_value, search_arg)),
                        }
                    },
                    "find" => {
                        if node.args.len() != 1 {
                            opt_bail!(format!("find() expects 1 argument, got {}", node.args.len()));
                        }
                        let search_arg = self.interpret_expr(&node.args[0], ctx)?;
                        match (&obj_value, &search_arg) {
                            (AstValue::String(s), AstValue::String(search)) => {
                                match s.value.find(&search.value) {
                                    Some(pos) => {
                                        // Return Some(pos) - for now just return the position as int
                                        Ok(AstValue::int(pos as i64))
                                    }
                                    None => {
                                        // Return None - for now return -1 to indicate not found
                                        Ok(AstValue::int(-1))
                                    }
                                }
                            }
                            _ => opt_bail!(format!("find() expects string arguments, got {:?}.find({:?})", obj_value, search_arg)),
                        }
                    },
                    "as_bytes" => match obj_value {
                        AstValue::String(s) => {
                            // Return the byte representation as a list of integers
                            let bytes: Vec<AstValue> = s.value.bytes().map(|b| AstValue::int(b as i64)).collect();
                            Ok(AstValue::List(fp_core::ast::ValueList { values: bytes }))
                        }
                        _ => opt_bail!(format!("as_bytes() not supported for {:?}", obj_value)),
                    },
                    x => opt_bail!(format!("Method '{}' not implemented", x)),
                }
            },
            ExprInvokeTarget::Expr(e) => match e.as_ref() {
                AstExpr::Value(value) => match value.as_ref() {
                    AstValue::BinOpKind(kind) => {
                        self.interpret_invoke_binop(kind.clone(), &node.args, ctx)
                    }
                    AstValue::UnOpKind(func) => {
                        opt_ensure!(node.args.len() == 1, format!("Expected 1 arg for {:?}", func));
                        let arg = self.interpret_expr(&node.args[0].get(), ctx)?;
                        self.interpret_invoke_unop(func.clone(), arg, ctx)
                    }
                    _ => opt_bail!(format!("Could not invoke {}", node)),
                },

                AstExpr::Any(any) => {
                    if let Some(exp) = any.downcast_ref::<BuiltinFn>() {
                        let args = self.interpret_args(&node.args, ctx)?;
                        Ok(exp.invoke(&args, ctx)?)
                    } else {
                        opt_bail!(format!("Could not invoke {:?}", node))
                    }
                }
                _ => opt_bail!(format!("Could not invoke {:?}", node)),
            },
            kind => opt_bail!(format!("Could not invoke {:?}", kind)),
        }
    }
    pub fn interpret_import(&self, _node: &ItemImport, _ctx: &SharedScopedContext) -> Result<()> {
        Ok(())
    }
    pub fn interpret_block(&self, node: &ExprBlock, ctx: &SharedScopedContext) -> Result<AstValue> {
        let ctx = ctx.child(Ident::new("__block__"), Visibility::Private, true);
        
        // FIRST PASS: Process all items (const declarations, structs, functions)
        // Items can reference each other and need to be processed before statements
        for stmt in node.first_stmts().iter() {
            if let BlockStmt::Item(item) = stmt {
                self.interpret_item(item, &ctx)?;
            }
        }
        
        // SECOND PASS: Process all non-item statements (expressions, let statements, etc.)
        for stmt in node.first_stmts().iter() {
            if !matches!(stmt, BlockStmt::Item(_)) {
                self.interpret_stmt(&stmt, &ctx)?;
            }
        }
        
        // Process final expression if any
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
                    opt_bail!(format!("Failed to interpret {:?} => {:?}", case.cond, interpret))
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
            "println!" if resolve => Ok(AstValue::any(builtin_println(self.serializer.clone()))),
            "println" if resolve => Ok(AstValue::any(builtin_println(self.serializer.clone()))),
            "true" => Ok(AstValue::bool(true)),
            "false" => Ok(AstValue::bool(false)),
            "None" => Ok(AstValue::None(ValueNone)),
            "null" => Ok(AstValue::Null(ValueNull)),
            "unit" => Ok(AstValue::Unit(ValueUnit)),
            "undefined" => Ok(AstValue::Undefined(ValueUndefined)),
            "Some" => Ok(AstValue::any(builtin_some())),
            // Metaprogramming intrinsics
            // Core introspection intrinsics
            "sizeof!" if resolve => Ok(AstValue::any(builtin_sizeof())),
            "reflect_fields!" if resolve => Ok(AstValue::any(builtin_reflect_fields())),
            "hasmethod!" if resolve => Ok(AstValue::any(builtin_hasmethod())),
            "type_name!" if resolve => Ok(AstValue::any(builtin_type_name())),
            
            // Struct creation and manipulation intrinsics
            "create_struct!" if resolve => Ok(AstValue::any(builtin_create_struct())),
            "clone_struct!" if resolve => Ok(AstValue::any(builtin_clone_struct())),
            "addfield!" if resolve => Ok(AstValue::any(builtin_addfield())),
            
            // Struct querying intrinsics
            "hasfield!" if resolve => Ok(AstValue::any(builtin_hasfield())),
            "field_count!" if resolve => Ok(AstValue::any(builtin_field_count())),
            "method_count!" if resolve => Ok(AstValue::any(builtin_method_count())),
            "field_type!" if resolve => Ok(AstValue::any(builtin_field_type())),
            "struct_size!" if resolve => Ok(AstValue::any(builtin_struct_size())),
            
            // Code generation intrinsics
            "generate_method!" if resolve => Ok(AstValue::any(builtin_generate_method())),
            
            // Compile-time validation intrinsics
            "compile_error!" if resolve => Ok(AstValue::any(builtin_compile_error())),
            "compile_warning!" if resolve => Ok(AstValue::any(builtin_compile_warning())),
            _ => {
                debug!("Get value recursive {:?}", ident);
                ctx.get_value_recursive(ident)
                    .ok_or_else(|| optimization_error(format!("could not find {:?} in context", ident.name)))
            }
        }
    }
    // Bitwise AND operation
    pub fn builtin_bitand(&self) -> BuiltinFn {
        BuiltinFn::new_with_ident("&".into(), move |args, _ctx| {
            if args.len() != 2 {
                return Err(optimization_error(format!("BitAnd expects 2 arguments, got: {}", args.len())));
            }
            
            match (&args[0], &args[1]) {
                (AstValue::Int(a), AstValue::Int(b)) => {
                    Ok(AstValue::int(a.value & b.value))
                },
                _ => Err(optimization_error(format!("BitAnd operation not supported for types: {:?} & {:?}", args[0], args[1])))
            }
        })
    }

    // Logical AND operation
    pub fn builtin_logical_and(&self) -> BuiltinFn {
        BuiltinFn::new_with_ident("&&".into(), move |args, _ctx| {
            if args.len() != 2 {
                return Err(optimization_error(format!("LogicalAnd expects 2 arguments, got: {}", args.len())));
            }
            
            match (&args[0], &args[1]) {
                (AstValue::Bool(a), AstValue::Bool(b)) => {
                    Ok(AstValue::bool(a.value && b.value))
                },
                _ => Err(optimization_error(format!("LogicalAnd operation not supported for types: {:?} && {:?}", args[0], args[1])))
            }
        })
    }

    // Logical OR operation
    pub fn builtin_logical_or(&self) -> BuiltinFn {
        BuiltinFn::new_with_ident("||".into(), move |args, _ctx| {
            if args.len() != 2 {
                return Err(optimization_error(format!("LogicalOr expects 2 arguments, got: {}", args.len())));
            }
            
            match (&args[0], &args[1]) {
                (AstValue::Bool(a), AstValue::Bool(b)) => {
                    Ok(AstValue::bool(a.value || b.value))
                },
                _ => Err(optimization_error(format!("LogicalOr operation not supported for types: {:?} || {:?}", args[0], args[1])))
            }
        })
    }

    // Division operation
    pub fn builtin_div(&self) -> BuiltinFn {
        BuiltinFn::new_with_ident("/".into(), move |args, _ctx| {
            if args.len() != 2 {
                return Err(optimization_error(format!("Division expects 2 arguments, got: {}", args.len())));
            }
            
            match (&args[0], &args[1]) {
                (AstValue::Int(a), AstValue::Int(b)) => {
                    if b.value == 0 {
                        return Err(optimization_error("Division by zero".to_string()));
                    }
                    Ok(AstValue::int(a.value / b.value))
                },
                _ => Err(optimization_error(format!("Division operation not supported for types: {:?} / {:?}", args[0], args[1])))
            }
        })
    }

    // If expression handler  
    pub fn interpret_if_expr(&self, if_expr: &fp_core::ast::ExprIf, ctx: &SharedScopedContext) -> Result<AstValue> {
        // Evaluate the condition
        let condition = self.interpret_expr(&if_expr.cond, ctx)?;
        
        // Check if condition is a boolean
        match condition {
            AstValue::Bool(b) => {
                if b.value {
                    // Execute then branch
                    self.interpret_expr(&if_expr.then, ctx)
                } else {
                    // Execute else branch if it exists
                    if let Some(else_expr) = &if_expr.elze {
                        self.interpret_expr(else_expr, ctx)
                    } else {
                        Ok(AstValue::unit()) // No else branch, return unit
                    }
                }
            },
            AstValue::Any(_) => {
                // Handle the case where condition contains unsupported expressions (like cfg! macro)
                Err(optimization_error(
                    "Cannot evaluate if condition: contains unsupported cfg! macro. The cfg!(debug_assertions) macro is not supported in const evaluation. Use a literal boolean instead.".to_string()
                ))
            },
            _ => {
                Err(optimization_error(format!(
                    "If condition must be boolean, got: {:?}", 
                    condition
                )))
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
                                _ => opt_bail!(format!("Expected impl Traits, got {:?}", value)),
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
            BinOpKind::Div => Ok(self.builtin_div()),
            // BinOpKind::Mod => Ok(builtin_mod()),
            BinOpKind::Gt => Ok(builtin_gt()),
            BinOpKind::Lt => Ok(builtin_lt()),
            BinOpKind::Ge => Ok(builtin_ge()),
            BinOpKind::Le => Ok(builtin_le()),
            BinOpKind::Eq => Ok(builtin_eq()),
            BinOpKind::Ne => Ok(builtin_ne()),
            BinOpKind::BitAnd => Ok(self.builtin_bitand()),
            BinOpKind::Or => Ok(self.builtin_logical_or()),
            BinOpKind::And => Ok(self.builtin_logical_and()),
            // BinOpKind::BitOr => {}
            // BinOpKind::BitXor => {}
            // BinOpKind::Any(_) => {}
            _ => opt_bail!(format!("Could not process {:?}", op)),
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
        tracing::debug!("Storing const {}: {:?}", def.name.name, value);
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
                Ok::<_, fp_core::error::Error>(ValueField {
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
                Ok::<_, fp_core::error::Error>(ValueField {
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
        tracing::debug!("Interpreting field access: {}.{}", 
            self.serializer.serialize_expr(&s.obj.get()).unwrap_or_default(), 
            s.field.name);
        let obj0 = self.interpret_expr(&s.obj.get(), ctx)?;
        tracing::debug!("Field access object resolved to: {:?}", obj0);
        let obj = obj0.as_structural()
            .ok_or_else(|| optimization_error(format!(
                "Expected structural type, got {}",
                self.serializer.serialize_value(&obj0).unwrap_or_default()
            )))?;
            
        let value = obj.get_field(&s.field)
            .ok_or_else(|| optimization_error(format!(
                "Could not find field {} in {}",
                s.field,
                self.serializer.serialize_value(&obj0).unwrap_or_default()
            )))?;
            
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
            .ok_or_else(|| optimization_error(format!(
                "Could not find function {} in context",
                node.sig.name.as_ref().unwrap()
            )))?;
            
        let sub = context.child(Ident::new("__call__"), Visibility::Private, true);
        for generic in &node.generics_params {
            let ty = self.evaluate_type_bounds(&generic.bounds, ctx)?;
            sub.insert_value_with_ctx(generic.name.clone(), ty.into());
        }
        let params: Vec<_> = node
            .params
            .iter()
            .map(|x| {
                Ok::<_, fp_core::error::Error>(FunctionParam::new(
                    x.name.clone(),
                    self.interpret_type(&x.ty, &sub)?,
                ))
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
            AstValue::Structural(_) => opt_bail!(format!("Failed to interpret {:?}", val)),
            AstValue::Function(n) => self
                .interpret_function_value(n, ctx)
                .map(AstValue::Function),
            AstValue::Tuple(n) => self.interpret_tuple(n, ctx, resolve).map(AstValue::Tuple),
            AstValue::Expr(n) => self.interpret_expr(&n.get(), ctx),
            AstValue::Any(_n) => {
                if self.ignore_missing_items {
                    return Ok(val.clone());
                }

                opt_bail!(format!("Failed to interpret {:?}", val))
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
        Ok(builtin_fn.invoke(&vec![lhs, rhs], ctx)?)
    }
    pub fn interpret_invoke_binop(
        &self,
        op: BinOpKind,
        args: &[AstExpr],
        ctx: &SharedScopedContext,
    ) -> Result<AstValue> {
        let builtin_fn = self.lookup_bin_op_kind(op)?;
        let args = self.interpret_args(args, ctx)?;
        Ok(builtin_fn.invoke(&args, ctx)?)
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
                _ => opt_bail!(format!("Failed to interpret {:?}", op)),
            },
            UnOpKind::Not => match arg {
                AstValue::Bool(val) => Ok(AstValue::Bool(ValueBool::new(!val.value))),
                _ => opt_bail!(format!("Failed to interpret {:?}", op)),
            },
            _ => opt_bail!(format!("Could not process {:?}", op)),
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
                .ok_or_else(|| optimization_error(format!("could not find {:?} in context", n))),
            AstExpr::Value(n) => self.interpret_value(n, ctx, resolve),
            AstExpr::Block(n) => self.interpret_block(n, ctx),
            AstExpr::Match(c) => self.interpret_cond(c, ctx),
            AstExpr::Invoke(invoke) => self.interpret_invoke(invoke, ctx),
            AstExpr::BinOp(op) => self.interpret_binop(op, ctx),
            AstExpr::UnOp(op) => {
                let arg = self.interpret_expr(&op.val, ctx)?;
                self.interpret_invoke_unop(op.op.clone(), arg, ctx)
            },
            AstExpr::Any(n) => {
                // Handle macros specially
                if let Some(raw_macro) = n.downcast_ref::<fp_rust_lang::RawExprMacro>() {
                    // Check if this is a cfg! macro by looking at the macro path
                    if raw_macro.raw.mac.path.is_ident("cfg") {
                        // cfg! macro - for now, assume debug mode is disabled in const evaluation
                        return Ok(AstValue::bool(false));
                    }
                    
                    // Handle strlen! macro
                    if raw_macro.raw.mac.path.is_ident("strlen") {
                        // Parse the argument inside the macro
                        let tokens = &raw_macro.raw.mac.tokens;
                        let tokens_str = tokens.to_string();
                        
                        // Simple parsing: remove whitespace and try to interpret as identifier
                        let arg_name = tokens_str.trim();
                        
                        // Try to get the value from context
                        let ident = fp_core::id::Ident::new(arg_name);
                        if let Some(value) = ctx.get_value(fp_core::id::Path::from(ident)) {
                            match value {
                                AstValue::String(s) => return Ok(AstValue::int(s.value.len() as i64)),
                                _ => opt_bail!(format!("strlen! expects string argument, got {:?}", value)),
                            }
                        } else {
                            opt_bail!(format!("strlen! could not find variable: {}", arg_name));
                        }
                    }
                    
                    // Handle concat! macro
                    if raw_macro.raw.mac.path.is_ident("concat") {
                        // Parse the arguments inside the macro
                        let tokens = &raw_macro.raw.mac.tokens;
                        let tokens_str = tokens.to_string();
                        
                        // Simple parsing: split by comma and evaluate each argument
                        let mut result = String::new();
                        
                        for arg in tokens_str.split(',') {
                            let arg = arg.trim();
                            
                            // Try to interpret as string literal first
                            if arg.starts_with('"') && arg.ends_with('"') {
                                // String literal - remove quotes
                                let literal = &arg[1..arg.len()-1];
                                result.push_str(literal);
                            } else {
                                // Try to get the value from context
                                let ident = fp_core::id::Ident::new(arg);
                                if let Some(value) = ctx.get_value(fp_core::id::Path::from(ident)) {
                                    match value {
                                        AstValue::String(s) => result.push_str(&s.value),
                                        AstValue::Int(i) => result.push_str(&i.value.to_string()),
                                        AstValue::Bool(b) => result.push_str(&b.value.to_string()),
                                        _ => opt_bail!(format!("concat! cannot convert {:?} to string", value)),
                                    }
                                } else {
                                    opt_bail!(format!("concat! could not find variable: {}", arg));
                                }
                            }
                        }
                        
                        return Ok(AstValue::string(result));
                    }
                    
                    // Handle introspection macros
                    if raw_macro.raw.mac.path.is_ident("sizeof") {
                        // Parse the argument inside the macro
                        let tokens = &raw_macro.raw.mac.tokens;
                        let tokens_str = tokens.to_string();
                        let arg_name = tokens_str.trim();
                        
                        // Try to get the struct type from context
                        let ident = fp_core::id::Ident::new(arg_name);
                        
                        // First try as a type
                        if let Some(type_value) = ctx.get_type(fp_core::id::Path::from(ident.clone())) {
                            let sizeof_builtin = builtin_sizeof();
                            return sizeof_builtin.invoke(&[AstValue::Type(type_value)], ctx);
                        }
                        
                        // Then try as a value (which might be a struct definition)
                        if let Some(value) = ctx.get_value(fp_core::id::Path::from(ident)) {
                            if let AstValue::Type(type_value) = value {
                                let sizeof_builtin = builtin_sizeof();
                                return sizeof_builtin.invoke(&[AstValue::Type(type_value)], ctx);
                            }
                        }
                        
                        opt_bail!(format!("sizeof! could not find type: {}", arg_name));
                    }
                    
                    if raw_macro.raw.mac.path.is_ident("field_count") {
                        let tokens = &raw_macro.raw.mac.tokens;
                        let tokens_str = tokens.to_string();
                        let arg_name = tokens_str.trim();
                        
                        let ident = fp_core::id::Ident::new(arg_name);
                        
                        // First try as a type
                        if let Some(type_value) = ctx.get_type(fp_core::id::Path::from(ident.clone())) {
                            let field_count_builtin = builtin_field_count();
                            return field_count_builtin.invoke(&[AstValue::Type(type_value)], ctx);
                        }
                        
                        // Then try as a value (which might be a struct definition)
                        if let Some(value) = ctx.get_value(fp_core::id::Path::from(ident)) {
                            if let AstValue::Type(type_value) = value {
                                let field_count_builtin = builtin_field_count();
                                return field_count_builtin.invoke(&[AstValue::Type(type_value)], ctx);
                            }
                        }
                        
                        opt_bail!(format!("field_count! could not find type: {}", arg_name));
                    }
                    
                    if raw_macro.raw.mac.path.is_ident("method_count") {
                        let tokens = &raw_macro.raw.mac.tokens;
                        let tokens_str = tokens.to_string();
                        let arg_name = tokens_str.trim();
                        
                        let ident = fp_core::id::Ident::new(arg_name);
                        
                        // First try as a type
                        if let Some(type_value) = ctx.get_type(fp_core::id::Path::from(ident.clone())) {
                            let method_count_builtin = builtin_method_count();
                            return method_count_builtin.invoke(&[AstValue::Type(type_value)], ctx);
                        }
                        
                        // Then try as a value (which might be a struct definition)
                        if let Some(value) = ctx.get_value(fp_core::id::Path::from(ident)) {
                            if let AstValue::Type(type_value) = value {
                                let method_count_builtin = builtin_method_count();
                                return method_count_builtin.invoke(&[AstValue::Type(type_value)], ctx);
                            }
                        }
                        
                        opt_bail!(format!("method_count! could not find type: {}", arg_name));
                    }
                    
                    if raw_macro.raw.mac.path.is_ident("hasfield") {
                        let tokens = &raw_macro.raw.mac.tokens;
                        let tokens_str = tokens.to_string();
                        
                        // Parse: Type, "field_name"
                        if let Some((type_name, field_name)) = tokens_str.split_once(',') {
                            let type_name = type_name.trim();
                            let field_name = field_name.trim();
                            
                            // Remove quotes from field_name
                            let field_name = if field_name.starts_with('"') && field_name.ends_with('"') {
                                &field_name[1..field_name.len()-1]
                            } else {
                                field_name
                            };
                            
                            let ident = fp_core::id::Ident::new(type_name);
                            
                            // First try as a type
                            if let Some(type_value) = ctx.get_type(fp_core::id::Path::from(ident.clone())) {
                                let hasfield_builtin = builtin_hasfield();
                                return hasfield_builtin.invoke(&[
                                    AstValue::Type(type_value),
                                    AstValue::string(field_name.to_string())
                                ], ctx);
                            }
                            
                            // Then try as a value (which might be a struct definition)
                            if let Some(value) = ctx.get_value(fp_core::id::Path::from(ident)) {
                                if let AstValue::Type(type_value) = value {
                                    let hasfield_builtin = builtin_hasfield();
                                    return hasfield_builtin.invoke(&[
                                        AstValue::Type(type_value),
                                        AstValue::string(field_name.to_string())
                                    ], ctx);
                                }
                            }
                            
                            opt_bail!(format!("hasfield! could not find type: {}", type_name));
                        } else {
                            opt_bail!("hasfield! expects 2 arguments: hasfield!(Type, \"field_name\")");
                        }
                    }
                }
                Ok(AstValue::Any(n.clone()))
            },
            AstExpr::Select(s) => self.interpret_select(s, ctx),
            AstExpr::Struct(s) => self.interpret_struct_expr(s, ctx).map(AstValue::Struct),
            AstExpr::Paren(p) => self.interpret_expr(&p.expr, ctx),
            AstExpr::If(if_expr) => self.interpret_if_expr(if_expr, ctx),
            _ => opt_bail!(format!("Unsupported expression type in interpreter: {:?}", node)),
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
            _ => opt_bail!(format!("Failed to interpret {:?}", node)),
        }
    }

    pub fn interpret_let(&self, node: &StmtLet, ctx: &SharedScopedContext) -> Result<AstValue> {
        if let Some(init) = &node.init {
            let value = self.interpret_expr(&init, ctx)?;
            ctx.insert_value(
                node.pat.as_ident()
                    .ok_or_else(|| optimization_error("Only supports ident"))?
                    .as_str(),
                value.clone(),
            );
            Ok(value)
        } else {
            ctx.insert_value(
                node.pat.as_ident()
                    .ok_or_else(|| optimization_error("Only supports ident"))?
                    .as_str(),
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
        // eprintln!("DEBUG: interpret_stmt called with: {:?}", node);
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
            BlockStmt::Item(item) => self.interpret_item(item, ctx).map(|_| None),
            BlockStmt::Any(any_box) => {
                // Handle macro statements  
                if any_box.downcast_ref::<fp_rust_lang::RawStmtMacro>().is_some() {
                    // Macros like println! are now converted to function calls at parse time
                    // For any remaining macros, just return unit
                    Ok(None)
                } else {
                    opt_bail!(format!("Unsupported Any statement type: {:?}", any_box))
                }
            },
            BlockStmt::Noop => Ok(None),
        }
    }


    pub fn interpret_tree(&self, node: &AstNode, ctx: &SharedScopedContext) -> Result<AstValue> {
        match node {
            AstNode::Item(item) => self.interpret_item(item, ctx),
            AstNode::Expr(expr) => self.interpret_expr(expr, ctx),
            AstNode::File(file) => self.interpret_items(&file.items, ctx),
        }
    }

    /// Evaluate a const expression with side-effect-aware intrinsics
    /// This is the main method for const evaluation
    pub fn evaluate_const_expression(
        &self, 
        expr: &AstExpr, 
        ctx: &SharedScopedContext,
        _intrinsic_context: &crate::utils::IntrinsicEvaluationContext
    ) -> fp_core::error::Result<AstValue> {
        // For now, delegate to the existing interpreter
        // TODO: Integrate with side-effect-aware intrinsics
        self.interpret_expr_no_resolve(expr, ctx)
    }
}

impl OptimizePass for InterpretationOrchestrator {
    fn name(&self) -> &str {
        "interpretation"
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
            _ => opt_bail!(format!("Failed to interpret {:?} => {:?}", expr, value)),
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
                opt_ensure!(invoke.args.len() == 1, format!("Expected 1 arg for {:?}", func));
                let arg = self.interpret_expr(&invoke.args[0].get(), ctx)?;
                self.interpret_invoke_unop(func.clone(), arg, ctx)
                    .map(AstExpr::value)
            }
            _ => opt_bail!(format!("Could not invoke {:?}", func)),
        }
    }

    fn try_evaluate_expr(&self, pat: &AstExpr, ctx: &SharedScopedContext) -> Result<AstExpr> {
        // First try the simple approach for basic expressions
        if let Some(value) = ctx.try_get_value_from_expr(pat) {
            return Ok(AstExpr::value(value));
        }
        
        // If simple approach fails, use full interpretation for complex expressions
        let value = self.interpret_expr(pat, ctx)?;
        Ok(AstExpr::value(value))
    }
}

impl ValueSystem for InterpretationOrchestrator {
    fn get_value_from_expr(&self, ctx: &Context, expr: &AstExpr) -> Result<AstValue> {
        let fold = FoldOptimizer::new(self.serializer.clone(), Box::new(self.clone()));
        let expr = fold.optimize_expr(expr.clone(), &ctx.values)?;
        match expr {
            AstExpr::Value(value) => Ok(*value),
            _ => opt_bail!(format!("Expected value, got {:?}", expr)),
        }
    }
}
