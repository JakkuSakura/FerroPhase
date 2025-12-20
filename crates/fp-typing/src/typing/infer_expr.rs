use crate::typing::unify::{FunctionTerm, TypeTerm};
use crate::typing_error;
use crate::{AstTypeInferencer, EnvEntry, PatternBinding, PatternInfo, TypeVarId};
use fp_core::ast::*;
use fp_core::error::Result;
use fp_core::intrinsics::{IntrinsicCallKind, IntrinsicCallPayload};
use fp_core::ops::{BinOpKind, UnOpKind};

/// Infer the fragment kind for an unkinded quote based on its block shape.
/// - Single trailing expression and no statements => Expr
/// - All items at top level => Item
/// - Otherwise => Stmt
pub(crate) fn infer_quote_kind(block: &ExprBlock) -> QuoteFragmentKind {
    // Prefer Expr when a trailing expression is present and the block does not
    // contain control-flow intrinsics such as `return`.
    if block.last_expr().is_some() && !block_contains_return(block) {
        return QuoteFragmentKind::Expr;
    }

    // Only items => Item
    let all_items = block.stmts.iter().all(|s| matches!(s, BlockStmt::Item(_)));
    if all_items {
        return QuoteFragmentKind::Item;
    }

    // Otherwise => Stmt
    QuoteFragmentKind::Stmt
}

fn block_contains_return(block: &ExprBlock) -> bool {
    block.stmts.iter().any(|stmt| match stmt {
        BlockStmt::Expr(expr_stmt) => expr_contains_return(expr_stmt.expr.as_ref()),
        BlockStmt::Let(stmt_let) => {
            stmt_let
                .init
                .as_ref()
                .is_some_and(|e| expr_contains_return(e))
                || stmt_let
                    .diverge
                    .as_ref()
                    .is_some_and(|e| expr_contains_return(e))
        }
        _ => false,
    })
}

fn expr_contains_return(expr: &Expr) -> bool {
    match expr.kind() {
        ExprKind::IntrinsicCall(call) => matches!(call.kind, IntrinsicCallKind::Return),
        ExprKind::Block(block) => block_contains_return(block),
        ExprKind::If(expr_if) => {
            expr_contains_return(expr_if.cond.as_ref())
                || expr_contains_return(expr_if.then.as_ref())
                || expr_if
                    .elze
                    .as_ref()
                    .is_some_and(|e| expr_contains_return(e))
        }
        ExprKind::Loop(expr_loop) => expr_contains_return(expr_loop.body.as_ref()),
        ExprKind::For(expr_for) => {
            expr_contains_return(expr_for.iter.as_ref())
                || expr_contains_return(expr_for.body.as_ref())
        }
        ExprKind::While(expr_while) => {
            expr_contains_return(expr_while.cond.as_ref())
                || expr_contains_return(expr_while.body.as_ref())
        }
        ExprKind::Match(expr_match) => {
            expr_match
                .scrutinee
                .as_ref()
                .is_some_and(|e| expr_contains_return(e))
                || expr_match.cases.iter().any(|case| {
                    expr_contains_return(case.cond.as_ref())
                        || case.guard
                            .as_ref()
                            .is_some_and(|e| expr_contains_return(e))
                        || expr_contains_return(case.body.as_ref())
                })
        }
        ExprKind::Invoke(invoke) => {
            let target_has_return = match &invoke.target {
                ExprInvokeTarget::Expr(inner) => expr_contains_return(inner.as_ref()),
                ExprInvokeTarget::Method(select) => expr_contains_return(select.obj.as_ref()),
                ExprInvokeTarget::Closure(closure) => expr_contains_return(closure.body.as_ref()),
                _ => false,
            };
            target_has_return || invoke.args.iter().any(|arg| expr_contains_return(arg))
        }
        ExprKind::Paren(paren) => expr_contains_return(paren.expr.as_ref()),
        ExprKind::Quote(quote) => block_contains_return(&quote.block),
        ExprKind::Splice(splice) => expr_contains_return(splice.token.as_ref()),
        _ => false,
    }
}

impl<'ctx> AstTypeInferencer<'ctx> {
    pub(crate) fn infer_expr(&mut self, expr: &mut Expr) -> Result<TypeVarId> {
        let existing_ty = expr.ty().cloned();
        let var = match expr.kind_mut() {
            ExprKind::Quote(quote) => {
                let kind = match quote.kind {
                    Some(k) => k,
                    None => infer_quote_kind(&quote.block),
                };
                let inner = if matches!(kind, QuoteFragmentKind::Expr) {
                    let block_var = self.infer_block(&mut quote.block)?;
                    Some(self.resolve_to_ty(block_var)?)
                } else {
                    None
                };
                let ty = Ty::QuoteToken(Box::new(TypeQuoteToken {
                    kind,
                    inner: inner.map(Box::new),
                }));
                let var = self.fresh_type_var();
                self.bind(var, TypeTerm::Custom(ty.clone()));
                expr.set_ty(ty);
                var
            }
            ExprKind::Splice(splice) => {
                // Expression-position splice must carry an expr token
                let token_var = self.infer_expr(splice.token.as_mut())?;
                let token_ty = self.resolve_to_ty(token_var)?;
                match token_ty {
                    Ty::QuoteToken(qt) => match qt.kind {
                        QuoteFragmentKind::Expr => {
                            if let Some(inner) = qt.inner.clone() {
                                let var = self.fresh_type_var();
                                self.bind(var, TypeTerm::Custom((*inner).clone()));
                                expr.set_ty(*inner);
                                var
                            } else {
                                self.emit_warning(
                                    "splice expr token lacks inner type; defaulting to any",
                                );
                                let any_var = self.fresh_type_var();
                                self.bind(any_var, TypeTerm::Any);
                                any_var
                            }
                        }
                        other => {
                            self.emit_error(format!(
                                "splice in expression position requires expr token, found {:?}",
                                other
                            ));
                            self.error_type_var()
                        }
                    },
                    _ => {
                        self.emit_error("splice expects a quote token expression");
                        self.error_type_var()
                    }
                }
            }
            ExprKind::IntrinsicContainer(collection) => {
                self.infer_intrinsic_container(collection)?
            }
            ExprKind::Value(value) => {
                if let Value::List(list) = value.as_ref() {
                    if matches!(existing_ty.as_ref(), Some(Ty::Array(_))) {
                        self.infer_value(value.as_ref())?
                    } else {
                        self.infer_list_value_as_vec(list)?
                    }
                } else {
                    self.infer_value(value.as_ref())?
                }
            }
            ExprKind::Locator(locator) => {
                let var = self.lookup_locator(locator)?;
                if let Some(ty) = existing_ty.as_ref() {
                    let annot = self.type_from_ast_ty(ty)?;
                    self.unify(var, annot)?;
                }
                var
            }
            ExprKind::Block(block) => self.infer_block(block)?,
            ExprKind::If(if_expr) => self.infer_if(if_expr)?,
            ExprKind::BinOp(binop) => self.infer_binop(binop)?,
            ExprKind::UnOp(unop) => self.infer_unop(unop)?,
            ExprKind::Assign(assign) => {
                let target = self.infer_expr(assign.target.as_mut())?;
                let value = self.infer_expr(assign.value.as_mut())?;
                self.unify(target, value)?;
                value
            }
            ExprKind::Cast(cast) => {
                let _ = self.infer_expr(cast.expr.as_mut())?;
                self.type_from_ast_ty(&cast.ty)?
            }
            ExprKind::Let(expr_let) => {
                let value = self.infer_expr(expr_let.expr.as_mut())?;
                let pattern_info = self.infer_pattern(expr_let.pat.as_mut())?;
                self.unify(pattern_info.var, value)?;
                self.apply_pattern_generalization(&pattern_info)?;
                value
            }
            ExprKind::Invoke(invoke) => self.infer_invoke(invoke)?,
            ExprKind::Select(select) => {
                let obj_var = self.infer_expr(select.obj.as_mut())?;
                self.lookup_struct_field(obj_var, &select.field)?
            }
            ExprKind::Struct(struct_expr) => {
                if let Some(ty) = existing_ty.as_ref() {
                    if matches!(ty, Ty::Function(_)) {
                        self.type_from_ast_ty(ty)?
                    } else {
                        self.resolve_struct_literal(struct_expr)?
                    }
                } else {
                    self.resolve_struct_literal(struct_expr)?
                }
            }
            ExprKind::Tuple(tuple) => {
                let mut element_vars = Vec::new();
                for expr in &mut tuple.values {
                    element_vars.push(self.infer_expr(expr)?);
                }
                let tuple_var = self.fresh_type_var();
                self.bind(tuple_var, TypeTerm::Tuple(element_vars));
                tuple_var
            }
            ExprKind::Array(array) => {
                let mut iter = array.values.iter_mut();
                let elem_var = if let Some(first) = iter.next() {
                    let first_var = self.infer_expr(first)?;
                    for value in iter {
                        let next = self.infer_expr(value)?;
                        self.unify(first_var, next)?;
                    }
                    first_var
                } else {
                    self.fresh_type_var()
                };
                let array_var = self.fresh_type_var();
                self.bind(array_var, TypeTerm::Vec(elem_var));
                array_var
            }
            ExprKind::ArrayRepeat(array_repeat) => {
                let elem_var = self.infer_expr(array_repeat.elem.as_mut())?;
                let len_var = self.infer_expr(array_repeat.len.as_mut())?;
                let expected_len = self.fresh_type_var();
                self.bind(
                    expected_len,
                    TypeTerm::Primitive(TypePrimitive::Int(TypeInt::U64)),
                );
                self.unify(len_var, expected_len)?;

                let elem_ty = self.resolve_to_ty(elem_var)?;
                let length_expr = array_repeat.len.as_ref().get();
                let array_ty = Ty::Array(TypeArray {
                    elem: Box::new(elem_ty.clone()),
                    len: length_expr.into(),
                });
                let array_var = self.fresh_type_var();
                self.bind(array_var, TypeTerm::Custom(array_ty.clone()));
                expr.set_ty(array_ty);
                array_var
            }
            ExprKind::Paren(paren) => self.infer_expr(paren.expr.as_mut())?,
            ExprKind::FormatString(_) => {
                let var = self.fresh_type_var();
                self.bind(var, TypeTerm::Primitive(TypePrimitive::String));
                var
            }
            ExprKind::Match(match_expr) => self.infer_match(match_expr)?,
            ExprKind::Loop(loop_expr) => self.infer_loop(loop_expr)?,
            ExprKind::For(for_expr) => {
                let _pat_info = self.infer_pattern(for_expr.pat.as_mut())?;
                let _iter_ty = self.infer_expr(for_expr.iter.as_mut())?;
                // For now, treat `for` as producing unit.
                let unit_var = self.fresh_type_var();
                self.bind(unit_var, TypeTerm::Unit);
                self.infer_expr(for_expr.body.as_mut())?;
                unit_var
            }
            ExprKind::While(while_expr) => self.infer_while(while_expr)?,
            ExprKind::Try(try_expr) => self.infer_expr(try_expr.expr.as_mut())?,
            ExprKind::Reference(reference) => self.infer_reference(reference)?,
            ExprKind::Dereference(dereference) => self.infer_dereference(dereference)?,
            ExprKind::Index(index) => self.infer_index(index)?,
            ExprKind::Closure(closure) => self.infer_closure(closure)?,
            ExprKind::IntrinsicCall(call) => self.infer_intrinsic(call)?,
            ExprKind::Range(range) => self.infer_range(range)?,
            ExprKind::Await(await_expr) => self.infer_expr(await_expr.base.as_mut())?,
            ExprKind::Async(async_expr) => self.infer_expr(async_expr.expr.as_mut())?,
            ExprKind::Splat(splat) => self.infer_splat(splat)?,
            ExprKind::SplatDict(splat) => self.infer_splat_dict(splat)?,
            ExprKind::Macro(macro_expr) => {
                self.emit_error(format!(
                    "macro `{}` was not lowered before type checking",
                    macro_expr.invocation.path
                ));
                self.error_type_var()
            }
            ExprKind::Any(_any) => {
                let any_var = self.fresh_type_var();
                self.bind(any_var, TypeTerm::Any);
                any_var
            }
            ExprKind::Item(_) | ExprKind::Closured(_) | ExprKind::Structural(_) => {
                let any_var = self.fresh_type_var();
                self.bind(any_var, TypeTerm::Any);
                any_var
            }
            ExprKind::Id(_) => {
                self.emit_error("detached expression identifiers are not supported");
                self.error_type_var()
            }
        };

        if let Some(existing_ty) = existing_ty {
            if !matches!(existing_ty, Ty::Unknown(_)) {
                let existing_var = self.type_from_ast_ty(&existing_ty)?;
                self.unify(var, existing_var)?;
            }
        }

        let ty = self.resolve_to_ty(var)?;
        expr.set_ty(ty);
        Ok(var)
    }

    pub(crate) fn infer_binop(&mut self, binop: &mut ExprBinOp) -> Result<TypeVarId> {
        let lhs = self.infer_expr(binop.lhs.as_mut())?;
        let rhs = self.infer_expr(binop.rhs.as_mut())?;
        match binop.kind {
            BinOpKind::Add
            | BinOpKind::Sub
            | BinOpKind::Mul
            | BinOpKind::Div
            | BinOpKind::Mod
            | BinOpKind::Shl
            | BinOpKind::Shr => {
                self.ensure_numeric(lhs, "binary operand")?;
                self.unify(lhs, rhs)?;
                Ok(lhs)
            }
            BinOpKind::Eq
            | BinOpKind::Ne
            | BinOpKind::Lt
            | BinOpKind::Le
            | BinOpKind::Gt
            | BinOpKind::Ge => {
                self.unify(lhs, rhs)?;
                let bool_var = self.fresh_type_var();
                self.bind(bool_var, TypeTerm::Primitive(TypePrimitive::Bool));
                Ok(bool_var)
            }
            BinOpKind::And | BinOpKind::Or => {
                self.ensure_bool(lhs, "logical operand")?;
                self.ensure_bool(rhs, "logical operand")?;
                let bool_var = self.fresh_type_var();
                self.bind(bool_var, TypeTerm::Primitive(TypePrimitive::Bool));
                Ok(bool_var)
            }
            _ => Ok(lhs),
        }
    }

    pub(crate) fn infer_unop(&mut self, unop: &mut ExprUnOp) -> Result<TypeVarId> {
        let value_var = self.infer_expr(unop.val.as_mut())?;
        match unop.op {
            UnOpKind::Not => {
                self.ensure_bool(value_var, "unary not")?;
                Ok(value_var)
            }
            UnOpKind::Neg => {
                self.ensure_numeric(value_var, "unary negation")?;
                Ok(value_var)
            }
            UnOpKind::Deref | UnOpKind::Any(_) => {
                let message = "unsupported unary operator in type inference".to_string();
                self.emit_error(message.clone());
                Err(typing_error(message))
            }
        }
    }

    pub(crate) fn infer_reference(&mut self, reference: &mut ExprReference) -> Result<TypeVarId> {
        let inner_var = self.infer_expr(reference.referee.as_mut())?;
        let reference_var = self.fresh_type_var();
        self.bind(reference_var, TypeTerm::Reference(inner_var));
        Ok(reference_var)
    }

    pub(crate) fn infer_dereference(
        &mut self,
        dereference: &mut ExprDereference,
    ) -> Result<TypeVarId> {
        let target_var = self.infer_expr(dereference.referee.as_mut())?;
        self.expect_reference(target_var, "dereference expression")
    }

    pub(crate) fn infer_index(&mut self, index: &mut ExprIndex) -> Result<TypeVarId> {
        let object_var = self.infer_expr(index.obj.as_mut())?;
        let idx_var = self.infer_expr(index.index.as_mut())?;
        self.ensure_integer(idx_var, "index expression")?;

        let elem_vec_var = self.fresh_type_var();
        let vec_var = self.fresh_type_var();
        self.bind(vec_var, TypeTerm::Vec(elem_vec_var));
        if self.unify(object_var, vec_var).is_ok() {
            return Ok(elem_vec_var);
        }

        let elem_slice_var = self.fresh_type_var();
        let slice_var = self.fresh_type_var();
        self.bind(slice_var, TypeTerm::Slice(elem_slice_var));
        if self.unify(object_var, slice_var).is_err() {
            self.emit_error("indexing is only supported on vector or slice types");
            return Ok(self.error_type_var());
        }
        Ok(elem_slice_var)
    }

    pub(crate) fn infer_range(&mut self, range: &mut ExprRange) -> Result<TypeVarId> {
        let element_var = self.fresh_type_var();

        if let Some(start) = range.start.as_mut() {
            let start_var = self.infer_expr(start)?;
            self.unify(element_var, start_var)?;
        }

        if let Some(end) = range.end.as_mut() {
            let end_var = self.infer_expr(end)?;
            self.unify(element_var, end_var)?;
        }

        if let Some(step) = range.step.as_mut() {
            let step_var = self.infer_expr(step)?;
            self.ensure_numeric(step_var, "range step")?;
        }

        self.ensure_numeric(element_var, "range bounds")?;

        let range_var = self.fresh_type_var();
        self.bind(range_var, TypeTerm::Vec(element_var));
        Ok(range_var)
    }

    pub(crate) fn infer_splat(&mut self, splat: &mut ExprSplat) -> Result<TypeVarId> {
        self.infer_expr(splat.iter.as_mut())
    }

    pub(crate) fn infer_splat_dict(&mut self, splat: &mut ExprSplatDict) -> Result<TypeVarId> {
        self.infer_expr(splat.dict.as_mut())
    }

    pub(crate) fn infer_intrinsic(&mut self, call: &mut ExprIntrinsicCall) -> Result<TypeVarId> {
        let mut arg_vars = Vec::new();

        match &mut call.payload {
            IntrinsicCallPayload::Args { args } => {
                for arg in args {
                    arg_vars.push(self.infer_expr(arg)?);
                }
            }
            IntrinsicCallPayload::Format { template } => {
                for arg in &mut template.args {
                    arg_vars.push(self.infer_expr(arg)?);
                }
                for kwarg in &mut template.kwargs {
                    arg_vars.push(self.infer_expr(&mut kwarg.value)?);
                }
            }
        }

        match call.kind {
            IntrinsicCallKind::ConstBlock => {
                if let Some(&body_var) = arg_vars.first() {
                    return Ok(body_var);
                }
                self.emit_error("const block intrinsic expects a body expression");
                return Ok(self.error_type_var());
            }
            IntrinsicCallKind::Break => {
                if arg_vars.len() > 1 {
                    self.emit_error("`break` accepts at most one value");
                }
                let value_var = if let Some(&var) = arg_vars.first() {
                    var
                } else {
                    self.unit_type_var()
                };

                let loop_var = if let Some(context) = self.loop_stack.last_mut() {
                    context.saw_break = true;
                    Some(context.result_var)
                } else {
                    None
                };

                if let Some(result_var) = loop_var {
                    self.unify(result_var, value_var)?;
                    return Ok(result_var);
                }

                self.emit_error("`break` used outside of a loop");
                return Ok(self.error_type_var());
            }
            IntrinsicCallKind::Continue => {
                if !arg_vars.is_empty() {
                    self.emit_error("`continue` does not accept a value");
                }
                if self.loop_stack.is_empty() {
                    self.emit_error("`continue` used outside of a loop");
                    return Ok(self.error_type_var());
                }
                return Ok(self.nothing_type_var());
            }
            IntrinsicCallKind::Return => {
                // Treat `return` as diverging for typing purposes. Backends are
                // responsible for lowering it into their native control-flow.
                if arg_vars.len() > 1 {
                    self.emit_error("`return` accepts at most one value");
                }
                return Ok(self.nothing_type_var());
            }
            _ => {}
        }

        let result_var = self.fresh_type_var();
        match call.kind {
            IntrinsicCallKind::Print | IntrinsicCallKind::Println => {
                self.bind(result_var, TypeTerm::Unit);
            }
            IntrinsicCallKind::Len
            | IntrinsicCallKind::SizeOf
            | IntrinsicCallKind::FieldCount
            | IntrinsicCallKind::MethodCount
            | IntrinsicCallKind::StructSize => {
                if arg_vars.len() != 1 {
                    self.emit_error(format!(
                        "intrinsic {:?} expects 1 argument, found {}",
                        call.kind,
                        arg_vars.len()
                    ));
                }
                self.bind(
                    result_var,
                    TypeTerm::Primitive(TypePrimitive::Int(TypeInt::U64)),
                );
            }
            IntrinsicCallKind::DebugAssertions
            | IntrinsicCallKind::HasField
            | IntrinsicCallKind::HasMethod => {
                let expected = if matches!(call.kind, IntrinsicCallKind::DebugAssertions) {
                    0
                } else {
                    2
                };
                if arg_vars.len() != expected {
                    self.emit_error(format!(
                        "intrinsic {:?} expects {} argument(s), found {}",
                        call.kind,
                        expected,
                        arg_vars.len()
                    ));
                }
                self.bind(result_var, TypeTerm::Primitive(TypePrimitive::Bool));
            }
            IntrinsicCallKind::Input => {
                if arg_vars.len() > 1 {
                    self.emit_error(format!(
                        "intrinsic {:?} expects at most 1 argument, found {}",
                        call.kind,
                        arg_vars.len()
                    ));
                }
                self.bind(result_var, TypeTerm::Primitive(TypePrimitive::String));
            }
            IntrinsicCallKind::TypeName => {
                if arg_vars.len() != 1 {
                    self.emit_error(format!(
                        "intrinsic {:?} expects 1 argument, found {}",
                        call.kind,
                        arg_vars.len()
                    ));
                }
                self.bind(result_var, TypeTerm::Primitive(TypePrimitive::String));
            }
            IntrinsicCallKind::ReflectFields => {
                if arg_vars.len() != 1 {
                    self.emit_error(format!(
                        "intrinsic {:?} expects 1 argument, found {}",
                        call.kind,
                        arg_vars.len()
                    ));
                }
                self.bind(result_var, TypeTerm::Any);
            }
            IntrinsicCallKind::CreateStruct
            | IntrinsicCallKind::CloneStruct
            | IntrinsicCallKind::AddField
            | IntrinsicCallKind::FieldType => {
                let expected = match call.kind {
                    IntrinsicCallKind::AddField => 3,
                    IntrinsicCallKind::FieldType => 2,
                    _ => 1,
                };
                if arg_vars.len() != expected {
                    self.emit_error(format!(
                        "intrinsic {:?} expects {} argument(s), found {}",
                        call.kind,
                        expected,
                        arg_vars.len()
                    ));
                }
                self.bind(result_var, TypeTerm::Custom(Ty::Type(TypeType)));
            }
            IntrinsicCallKind::GenerateMethod => {
                if arg_vars.len() != 2 {
                    self.emit_error(format!(
                        "intrinsic {:?} expects 2 arguments, found {}",
                        call.kind,
                        arg_vars.len()
                    ));
                }
                self.bind(result_var, TypeTerm::Unit);
            }
            IntrinsicCallKind::CompileError => {
                if arg_vars.len() != 1 {
                    self.emit_error(format!(
                        "intrinsic {:?} expects 1 argument, found {}",
                        call.kind,
                        arg_vars.len()
                    ));
                }
                self.bind(result_var, TypeTerm::Nothing);
            }
            IntrinsicCallKind::CompileWarning => {
                if arg_vars.len() != 1 {
                    self.emit_error(format!(
                        "intrinsic {:?} expects 1 argument, found {}",
                        call.kind,
                        arg_vars.len()
                    ));
                }
                self.bind(result_var, TypeTerm::Unit);
            }
            _ => {
                self.bind(result_var, TypeTerm::Any);
            }
        }

        Ok(result_var)
    }

    pub(crate) fn infer_closure(&mut self, closure: &mut ExprClosure) -> Result<TypeVarId> {
        self.enter_scope();
        let mut param_vars = Vec::new();
        for param in &mut closure.params {
            let info = self.infer_pattern(param)?;
            param_vars.push(info.var);
        }

        let body_var = self.infer_expr(closure.body.as_mut())?;
        let ret_var = if let Some(ret_ty) = &closure.ret_ty {
            let annot_var = self.type_from_ast_ty(ret_ty)?;
            self.unify(body_var, annot_var)?;
            annot_var
        } else {
            body_var
        };

        self.exit_scope();

        let closure_var = self.fresh_type_var();
        self.bind(
            closure_var,
            TypeTerm::Function(FunctionTerm {
                params: param_vars,
                ret: ret_var,
            }),
        );
        Ok(closure_var)
    }

    pub(crate) fn infer_invoke(&mut self, invoke: &mut ExprInvoke) -> Result<TypeVarId> {
        if let Some(result) = self.try_infer_collection_call(invoke)? {
            return Ok(result);
        }

        if let ExprInvokeTarget::Function(locator) = &mut invoke.target {
            if let Some(ident) = locator.as_ident() {
                if ident.as_str() == "printf" {
                    return self.infer_builtin_printf(invoke);
                }
            }
        }

        let func_var = match &mut invoke.target {
            ExprInvokeTarget::Function(locator) => {
                if let Some(var) = self.lookup_associated_function(locator)? {
                    var
                } else {
                    self.lookup_locator(locator)?
                }
            }
            ExprInvokeTarget::Expr(expr) => self.infer_expr(expr.as_mut())?,
            ExprInvokeTarget::Closure(_) => {
                let message = "invoking closure values is not yet supported".to_string();
                self.emit_error(message.clone());
                return Ok(self.error_type_var());
            }
            ExprInvokeTarget::BinOp(_) => {
                let message = "invoking binary operators as functions is not supported".to_string();
                self.emit_error(message.clone());
                return Ok(self.error_type_var());
            }
            ExprInvokeTarget::Type(ty) => self.type_from_ast_ty(ty)?,
            ExprInvokeTarget::Method(select) => {
                let obj_var = self.infer_expr(select.obj.as_mut())?;
                if let Some(result) =
                    self.try_infer_primitive_method(obj_var, &select.field, invoke.args.len())?
                {
                    return Ok(result);
                }
                if select.field.name.as_str() == "len" && invoke.args.is_empty() {
                    if let Ok(obj_ty) = self.resolve_to_ty(obj_var) {
                        if Self::is_collection_with_len(&obj_ty) {
                            let result_var = self.fresh_type_var();
                            self.bind(
                                result_var,
                                TypeTerm::Primitive(TypePrimitive::Int(TypeInt::I64)),
                            );
                            return Ok(result_var);
                        }
                    }
                }
                self.lookup_struct_method(obj_var, &select.field)?
            }
        };

        let func_info = self.ensure_function(func_var, invoke.args.len())?;
        for (arg_expr, param_var) in invoke.args.iter_mut().zip(func_info.params.iter()) {
            let arg_var = self.infer_expr(arg_expr)?;
            self.unify(*param_var, arg_var)?;
        }
        Ok(func_info.ret)
    }

    fn try_infer_collection_call(&mut self, invoke: &mut ExprInvoke) -> Result<Option<TypeVarId>> {
        let locator = match &invoke.target {
            ExprInvokeTarget::Function(locator) => locator,
            _ => return Ok(None),
        };
        if Self::locator_matches_suffix(locator, &["Vec", "new"]) {
            return self.infer_vec_new(invoke).map(Some);
        }
        if Self::locator_matches_suffix(locator, &["Vec", "with_capacity"]) {
            return self.infer_vec_with_capacity(invoke).map(Some);
        }
        if Self::locator_matches_suffix(locator, &["Vec", "from"]) {
            return self.infer_vec_from(invoke).map(Some);
        }
        if Self::locator_matches_suffix(locator, &["HashMap", "new"]) {
            return self.infer_hashmap_new(invoke).map(Some);
        }
        if Self::locator_matches_suffix(locator, &["HashMap", "with_capacity"]) {
            return self.infer_hashmap_with_capacity(invoke).map(Some);
        }
        if Self::locator_matches_suffix(locator, &["HashMap", "from"]) {
            return self.infer_hashmap_from(invoke).map(Some);
        }
        Ok(None)
    }

    fn infer_vec_new(&mut self, invoke: &mut ExprInvoke) -> Result<TypeVarId> {
        if !invoke.args.is_empty() {
            for arg in &mut invoke.args {
                let _ = self.infer_expr(arg);
            }
            self.emit_error("Vec::new does not take arguments");
        }
        let elem_var = self.fresh_type_var();
        let vec_var = self.fresh_type_var();
        self.bind(vec_var, TypeTerm::Vec(elem_var));
        Ok(vec_var)
    }

    fn infer_vec_with_capacity(&mut self, invoke: &mut ExprInvoke) -> Result<TypeVarId> {
        if invoke.args.len() != 1 {
            for arg in &mut invoke.args {
                let _ = self.infer_expr(arg);
            }
            self.emit_error("Vec::with_capacity expects a single capacity argument");
        } else {
            let capacity_var = self.infer_expr(&mut invoke.args[0])?;
            let expected = self.fresh_type_var();
            self.bind(
                expected,
                TypeTerm::Primitive(TypePrimitive::Int(TypeInt::U64)),
            );
            self.unify(capacity_var, expected)?;
        }
        let elem_var = self.fresh_type_var();
        let vec_var = self.fresh_type_var();
        self.bind(vec_var, TypeTerm::Vec(elem_var));
        Ok(vec_var)
    }

    fn infer_vec_from(&mut self, invoke: &mut ExprInvoke) -> Result<TypeVarId> {
        if invoke.args.len() != 1 {
            for arg in &mut invoke.args {
                let _ = self.infer_expr(arg);
            }
            self.emit_error("Vec::from expects a single iterable argument");
        } else {
            let _ = self.infer_expr(&mut invoke.args[0])?;
        }
        let elem_var = self.fresh_type_var();
        let vec_var = self.fresh_type_var();
        self.bind(vec_var, TypeTerm::Vec(elem_var));
        Ok(vec_var)
    }

    fn infer_hashmap_new(&mut self, invoke: &mut ExprInvoke) -> Result<TypeVarId> {
        if !invoke.args.is_empty() {
            for arg in &mut invoke.args {
                let _ = self.infer_expr(arg);
            }
            self.emit_error("HashMap::new does not take arguments");
        }
        let map_var = self.fresh_type_var();
        let map_ty = self.make_hashmap_ty();
        self.bind(map_var, TypeTerm::Custom(map_ty));
        Ok(map_var)
    }

    fn infer_hashmap_with_capacity(&mut self, invoke: &mut ExprInvoke) -> Result<TypeVarId> {
        if invoke.args.len() != 1 {
            for arg in &mut invoke.args {
                let _ = self.infer_expr(arg);
            }
            self.emit_error("HashMap::with_capacity expects a single capacity argument");
        } else {
            let capacity_var = self.infer_expr(&mut invoke.args[0])?;
            let expected = self.fresh_type_var();
            self.bind(
                expected,
                TypeTerm::Primitive(TypePrimitive::Int(TypeInt::U64)),
            );
            self.unify(capacity_var, expected)?;
        }
        let map_var = self.fresh_type_var();
        let map_ty = self.make_hashmap_ty();
        self.bind(map_var, TypeTerm::Custom(map_ty));
        Ok(map_var)
    }

    fn infer_hashmap_from(&mut self, invoke: &mut ExprInvoke) -> Result<TypeVarId> {
        if invoke.args.len() != 1 {
            for arg in &mut invoke.args {
                let _ = self.infer_expr(arg);
            }
            self.emit_error("HashMap::from expects a single iterable argument");
        } else {
            let _ = self.infer_expr(&mut invoke.args[0])?;
        }
        let map_var = self.fresh_type_var();
        let map_ty = self.make_hashmap_ty();
        self.bind(map_var, TypeTerm::Custom(map_ty));
        Ok(map_var)
    }

    fn make_hashmap_ty(&self) -> Ty {
        Ty::Struct(TypeStruct {
            name: Ident::new("HashMap"),
            generics_params: Vec::new(),
            fields: Vec::new(),
        })
    }

    fn locator_matches_suffix(locator: &Locator, suffix: &[&str]) -> bool {
        let segments = Self::locator_segments(locator);
        if segments.len() < suffix.len() {
            return false;
        }
        segments
            .iter()
            .rev()
            .zip(suffix.iter().rev())
            .all(|(segment, expected)| segment == expected)
    }

    fn locator_segments(locator: &Locator) -> Vec<String> {
        match locator {
            Locator::Ident(ident) => vec![ident.as_str().to_string()],
            Locator::Path(path) => path
                .segments
                .iter()
                .map(|s| s.as_str().to_string())
                .collect(),
            Locator::ParameterPath(path) => path
                .segments
                .iter()
                .map(|seg| seg.ident.as_str().to_string())
                .collect(),
        }
    }

    fn is_collection_with_len(ty: &Ty) -> bool {
        match ty {
            Ty::Array(_) | Ty::Slice(_) | Ty::Vec(_) => true,
            Ty::Struct(struct_ty) => struct_ty.name.as_str() == "HashMap",
            _ => false,
        }
    }

    fn infer_builtin_printf(&mut self, invoke: &mut ExprInvoke) -> Result<TypeVarId> {
        if invoke.args.is_empty() {
            self.emit_error("printf requires a format string argument");
            return Ok(self.error_type_var());
        }
        let format_var = self.infer_expr(&mut invoke.args[0])?;
        let expected_format = self.fresh_type_var();
        self.bind(expected_format, TypeTerm::Primitive(TypePrimitive::String));
        self.unify(format_var, expected_format)?;
        for arg in invoke.args.iter_mut().skip(1) {
            let _ = self.infer_expr(arg)?;
        }
        let result_var = self.fresh_type_var();
        self.bind(result_var, TypeTerm::Unit);
        Ok(result_var)
    }

    pub(crate) fn infer_list_value_as_vec(&mut self, list: &ValueList) -> Result<TypeVarId> {
        let elem_var = if let Some(first) = list.values.first() {
            let first_var = self.infer_value(first)?;
            for value in list.values.iter().skip(1) {
                let next_var = self.infer_value(value)?;
                self.unify(first_var, next_var)?;
            }
            first_var
        } else {
            let fresh = self.fresh_type_var();
            self.bind(fresh, TypeTerm::Any);
            fresh
        };
        let vec_var = self.fresh_type_var();
        self.bind(vec_var, TypeTerm::Vec(elem_var));
        Ok(vec_var)
    }

    fn infer_intrinsic_container(
        &mut self,
        collection: &mut ExprIntrinsicContainer,
    ) -> Result<TypeVarId> {
        match collection {
            ExprIntrinsicContainer::VecElements { elements } => {
                let elem_var = if let Some(first) = elements.first_mut() {
                    let first_var = self.infer_expr(first)?;
                    for expr in elements.iter_mut().skip(1) {
                        let next_var = self.infer_expr(expr)?;
                        self.unify(first_var, next_var)?;
                    }
                    first_var
                } else {
                    let fresh = self.fresh_type_var();
                    self.bind(fresh, TypeTerm::Any);
                    fresh
                };
                let vec_var = self.fresh_type_var();
                self.bind(vec_var, TypeTerm::Vec(elem_var));
                Ok(vec_var)
            }
            ExprIntrinsicContainer::VecRepeat { elem, len } => {
                let elem_var = self.infer_expr(elem.as_mut())?;
                let len_var = self.infer_expr(len.as_mut())?;
                let expected = self.fresh_type_var();
                self.bind(
                    expected,
                    TypeTerm::Primitive(TypePrimitive::Int(TypeInt::U64)),
                );
                self.unify(len_var, expected)?;
                let vec_var = self.fresh_type_var();
                self.bind(vec_var, TypeTerm::Vec(elem_var));
                Ok(vec_var)
            }
            ExprIntrinsicContainer::HashMapEntries { entries } => {
                for entry in entries {
                    let _ = self.infer_expr(&mut entry.key)?;
                    let _ = self.infer_expr(&mut entry.value)?;
                }
                let map_var = self.fresh_type_var();
                let map_ty = self.make_hashmap_ty();
                self.bind(map_var, TypeTerm::Custom(map_ty));
                Ok(map_var)
            }
        }
    }

    pub(crate) fn infer_value(&mut self, value: &Value) -> Result<TypeVarId> {
        let var = self.fresh_type_var();
        match value {
            Value::Int(_) => {
                self.literal_ints.insert(var);
                self.bind(var, TypeTerm::Primitive(TypePrimitive::Int(TypeInt::I64)));
            }
            Value::Bool(_) => self.bind(var, TypeTerm::Primitive(TypePrimitive::Bool)),
            Value::Decimal(_) => self.bind(
                var,
                TypeTerm::Primitive(TypePrimitive::Decimal(DecimalType::F64)),
            ),
            Value::String(_) => {
                let inner = self.fresh_type_var();
                self.bind(inner, TypeTerm::Primitive(TypePrimitive::String));
                self.bind(var, TypeTerm::Reference(inner));
            }
            Value::List(list) => {
                let elem_var = if let Some(first) = list.values.first() {
                    self.infer_value(first)?
                } else {
                    let fresh = self.fresh_type_var();
                    self.bind(fresh, TypeTerm::Any);
                    fresh
                };
                for value in list.values.iter().skip(1) {
                    let next_var = self.infer_value(value)?;
                    self.unify(elem_var, next_var)?;
                }
                let len = list.values.len() as i64;
                let elem_ty = self.resolve_to_ty(elem_var)?;
                let array_ty = Ty::Array(TypeArray {
                    elem: Box::new(elem_ty),
                    len: Expr::value(Value::int(len)).into(),
                });
                self.bind(var, TypeTerm::Custom(array_ty));
            }
            Value::Char(_) => self.bind(var, TypeTerm::Primitive(TypePrimitive::Char)),
            Value::Unit(_) => self.bind(var, TypeTerm::Unit),
            Value::Null(_) | Value::None(_) => self.bind(var, TypeTerm::Nothing),
            Value::Struct(struct_val) => {
                self.bind(var, TypeTerm::Struct(struct_val.ty.clone()));
            }
            Value::Structural(structural) => {
                let fields = structural
                    .fields
                    .iter()
                    .map(|field| StructuralField::new(field.name.clone(), Ty::Any(TypeAny)))
                    .collect();
                self.bind(var, TypeTerm::Structural(TypeStructural { fields }));
            }
            Value::Tuple(tuple) => {
                let mut vars = Vec::new();
                for elem in &tuple.values {
                    vars.push(self.infer_value(elem)?);
                }
                self.bind(var, TypeTerm::Tuple(vars));
            }
            Value::Function(func) => {
                let fn_ty = self.ty_from_function_signature(&func.sig)?;
                let fn_var = self.type_from_ast_ty(&fn_ty)?;
                self.unify(var, fn_var)?;
            }
            Value::Type(ty) => {
                let ty_var = self.type_from_ast_ty(ty)?;
                self.unify(var, ty_var)?;
            }
            Value::Expr(_) => {
                let message = "embedded expression values are not yet supported".to_string();
                self.emit_error(message.clone());
                return Ok(self.error_type_var());
            }
            _ => {
                let message = format!("value {:?} is not supported by type inference", value);
                self.emit_error(message.clone());
                return Ok(self.error_type_var());
            }
        }
        Ok(var)
    }

    pub(crate) fn infer_pattern(&mut self, pattern: &mut Pattern) -> Result<PatternInfo> {
        let existing_ty = pattern.ty().cloned();
        let info = match pattern.kind_mut() {
            PatternKind::Ident(ident) => {
                let var = self.fresh_type_var();
                self.insert_env(ident.ident.as_str().to_string(), EnvEntry::Mono(var));
                PatternInfo::new(var).with_binding(ident.ident.as_str().to_string(), var)
            }
            PatternKind::Type(inner) => {
                let inner_info = self.infer_pattern(inner.pat.as_mut())?;
                let annot_var = self.type_from_ast_ty(&inner.ty)?;
                self.unify(inner_info.var, annot_var)?;
                inner_info
            }
            PatternKind::Wildcard(_) => PatternInfo::new(self.fresh_type_var()),
            PatternKind::Tuple(tuple) => {
                let mut vars = Vec::new();
                let mut bindings = Vec::new();
                for pat in &mut tuple.patterns {
                    let child = self.infer_pattern(pat)?;
                    vars.push(child.var);
                    bindings.extend(child.bindings);
                }
                let tuple_var = self.fresh_type_var();
                self.bind(tuple_var, TypeTerm::Tuple(vars));
                PatternInfo {
                    var: tuple_var,
                    bindings,
                }
            }
            PatternKind::Struct(struct_pat) => {
                let struct_name = struct_pat.name.as_str().to_string();
                let struct_var = self.fresh_type_var();
                if let Some(struct_def) = self.struct_defs.get(&struct_name).cloned() {
                    self.bind(struct_var, TypeTerm::Struct(struct_def.clone()));
                    let mut bindings = Vec::new();
                    for field in &mut struct_pat.fields {
                        if let Some(rename) = field.rename.as_mut() {
                            let child = self.infer_pattern(rename)?;
                            bindings.extend(child.bindings);
                            if let Some(def_field) =
                                struct_def.fields.iter().find(|f| f.name == field.name)
                            {
                                let expected = self.type_from_ast_ty(&def_field.value)?;
                                self.unify(child.var, expected)?;
                            }
                        } else if let Some(def_field) =
                            struct_def.fields.iter().find(|f| f.name == field.name)
                        {
                            let var = self.fresh_type_var();
                            self.insert_env(field.name.as_str().to_string(), EnvEntry::Mono(var));
                            let expected = self.type_from_ast_ty(&def_field.value)?;
                            self.unify(var, expected)?;
                            bindings.push(PatternBinding {
                                name: field.name.as_str().to_string(),
                                var,
                            });
                        }
                    }
                    PatternInfo {
                        var: struct_var,
                        bindings,
                    }
                } else {
                    self.emit_error(format!("unknown struct {} in pattern", struct_name));
                    PatternInfo::new(self.error_type_var())
                }
            }
            PatternKind::TupleStruct(tuple_struct) => {
                // Handles tuple-struct patterns and enum tuple variants.
                let mut bindings = Vec::new();
                let mut element_vars = Vec::new();
                for pat in &mut tuple_struct.patterns {
                    let child = self.infer_pattern(pat)?;
                    element_vars.push(child.var);
                    bindings.extend(child.bindings);
                }

                let tuple_var = self.fresh_type_var();
                self.bind(tuple_var, TypeTerm::Tuple(element_vars.clone()));

                // Try to resolve as an enum variant: `Enum::Variant(...)`.
                let locator = &tuple_struct.name;
                if let Locator::Path(path) = locator {
                    if path.segments.len() >= 2 {
                        let enum_name = path.segments[path.segments.len() - 2].as_str().to_string();
                        let variant_name = path.segments[path.segments.len() - 1].as_str();
                        if let Some(enum_def) = self.enum_defs.get(&enum_name).cloned() {
                            if let Some(variant) =
                                enum_def.variants.iter().find(|v| v.name.as_str() == variant_name)
                            {
                                if let Ty::Tuple(tuple_ty) = &variant.value {
                                    for (idx, expected_ty) in
                                        tuple_ty.types.iter().enumerate().take(element_vars.len())
                                    {
                                        let expected_var = self.type_from_ast_ty(expected_ty)?;
                                        self.unify(element_vars[idx], expected_var)?;
                                    }
                                }

                                let enum_var = self.fresh_type_var();
                                self.bind(enum_var, TypeTerm::Enum(enum_def));
                                return Ok(PatternInfo {
                                    var: enum_var,
                                    bindings,
                                });
                            }
                        }
                    }
                }

                // Fallback: treat as a tuple value.
                PatternInfo {
                    var: tuple_var,
                    bindings,
                }
            }
            PatternKind::Variant(variant) => {
                // Enum variant patterns (unit and struct-like) and literal patterns.
                match variant.name.kind() {
                    ExprKind::Locator(locator) => {
                        if let Locator::Path(path) = locator {
                            if path.segments.len() >= 2 {
                                let enum_name =
                                    path.segments[path.segments.len() - 2].as_str().to_string();
                                let variant_name = path.segments[path.segments.len() - 1].as_str();
                                if let Some(enum_def) = self.enum_defs.get(&enum_name).cloned() {
                                    let enum_var = self.fresh_type_var();
                                    self.bind(enum_var, TypeTerm::Enum(enum_def.clone()));

                                    if let Some(inner) = variant.pattern.as_mut() {
                                        if let Some(def_variant) = enum_def
                                            .variants
                                            .iter()
                                            .find(|v| v.name.as_str() == variant_name)
                                        {
                                            // Struct-like enum variant patterns: `Enum::Variant { ... }`.
                                            if let (Ty::Structural(structural), PatternKind::Structural(pat)) =
                                                (&def_variant.value, inner.kind_mut())
                                            {
                                                let mut bindings = Vec::new();
                                                for field in &mut pat.fields {
                                                    if let Some(expected_field) = structural
                                                        .fields
                                                        .iter()
                                                        .find(|f| f.name == field.name)
                                                    {
                                                        let expected_var =
                                                            self.type_from_ast_ty(&expected_field.value)?;
                                                        if let Some(rename) = field.rename.as_mut() {
                                                            let child = self.infer_pattern(rename)?;
                                                            bindings.extend(child.bindings);
                                                            self.unify(child.var, expected_var)?;
                                                        } else {
                                                            let var = self.fresh_type_var();
                                                            self.insert_env(
                                                                field.name.as_str().to_string(),
                                                                EnvEntry::Mono(var),
                                                            );
                                                            self.unify(var, expected_var)?;
                                                            bindings.push(PatternBinding {
                                                                name: field.name.as_str().to_string(),
                                                                var,
                                                            });
                                                        }
                                                    }
                                                }
                                                return Ok(PatternInfo {
                                                    var: enum_var,
                                                    bindings,
                                                });
                                            }
                                        }
                                    }

                                    return Ok(PatternInfo::new(enum_var));
                                }
                            }
                        }
                        // Otherwise treat as a binding-like identifier.
                        let var = self.fresh_type_var();
                        PatternInfo::new(var)
                    }
                    _ => {
                        // Literal pattern.
                        let lit_var = self.infer_expr(&mut variant.name)?;
                        PatternInfo::new(lit_var)
                    }
                }
            }
            _ => {
                self.emit_error("pattern is not supported by type inference");
                PatternInfo::new(self.error_type_var())
            }
        };
        if let Some(ty) = existing_ty.as_ref() {
            let var = self.type_from_ast_ty(ty)?;
            self.unify(info.var, var)?;
        }
        Ok(info)
    }

    fn lookup_struct_method(&mut self, obj_var: TypeVarId, field: &Ident) -> Result<TypeVarId> {
        let ty = self.resolve_to_ty(obj_var)?;
        let resolved_ty = Self::peel_reference(ty.clone());
        let struct_name = match resolved_ty {
            Ty::Struct(struct_ty) => struct_ty.name.as_str().to_string(),
            Ty::Enum(enum_ty) => enum_ty.name.as_str().to_string(),
            other => {
                if let Some(var) = self.lookup_trait_method_for_receiver(obj_var, field)? {
                    return Ok(var);
                }
                if matches!(other, Ty::Any(_) | Ty::Unknown(_)) {
                    if let Some(var) = self.lookup_unique_trait_method(field)? {
                        return Ok(var);
                    }
                }
                self.emit_error(format!(
                    "cannot call method {} on value of type {}",
                    field, other
                ));
                return Ok(self.error_type_var());
            }
        };
        let record = self
            .struct_methods
            .get(&struct_name)
            .and_then(|methods| methods.get(field.as_str()))
            .cloned();
        if let Some(record) = record {
            if let Some(expected) = record.receiver_ty.as_ref() {
                let receiver_var = self.type_from_ast_ty(expected)?;
                let expect_ref = matches!(expected, Ty::Reference(_));
                let actual_ref = matches!(ty, Ty::Reference(_));
                if !expect_ref || actual_ref {
                    self.unify(obj_var, receiver_var)?;
                }
            }
            if let Some(scheme) = record.scheme.as_ref() {
                return Ok(self.instantiate_scheme(scheme));
            }
            if let Some(var) = self.lookup_env_var(field.as_str()) {
                return Ok(var);
            }
        }
        self.emit_error(format!(
            "unknown method {} on struct {}",
            field, struct_name
        ));
        Ok(self.error_type_var())
    }

    fn lookup_unique_trait_method(&mut self, field: &Ident) -> Result<Option<TypeVarId>> {
        let mut found: Option<(String, FunctionSignature)> = None;
        for (trait_name, methods) in &self.trait_method_sigs {
            if let Some(sig) = methods.get(field.as_str()) {
                if found.is_some() {
                    return Ok(None);
                }
                found = Some((trait_name.clone(), sig.clone()));
            }
        }
        let Some((_trait_name, sig)) = found else {
            return Ok(None);
        };
        let scheme = self.scheme_from_method_signature(&sig)?;
        Ok(Some(self.instantiate_scheme(&scheme)))
    }

    fn lookup_trait_method_for_receiver(
        &mut self,
        obj_var: TypeVarId,
        field: &Ident,
    ) -> Result<Option<TypeVarId>> {
        let mut receiver = obj_var;
        loop {
            let root = self.find(receiver);
            match self.type_vars[root].kind.clone() {
                crate::typing::unify::TypeVarKind::Bound(TypeTerm::Reference(inner)) => {
                    receiver = inner;
                }
                crate::typing::unify::TypeVarKind::Link(next) => {
                    receiver = next;
                }
                _ => {
                    receiver = root;
                    break;
                }
            }
        }

        let Some(traits) = self.generic_trait_bounds.get(&receiver).cloned() else {
            return Ok(None);
        };

        for trait_name in traits {
            let Some(methods) = self.trait_method_sigs.get(&trait_name) else {
                continue;
            };
            let Some(sig) = methods.get(field.as_str()).cloned() else {
                continue;
            };
            let scheme = self.scheme_from_method_signature(&sig)?;
            return Ok(Some(self.instantiate_scheme(&scheme)));
        }
        Ok(None)
    }

    fn try_infer_primitive_method(
        &mut self,
        obj_var: TypeVarId,
        field: &Ident,
        arg_len: usize,
    ) -> Result<Option<TypeVarId>> {
        if arg_len != 0 {
            return Ok(None);
        }
        match field.name.as_str() {
            "to_string" => {
                let obj_ty = match self.resolve_to_ty(obj_var) {
                    Ok(ty) => Self::peel_reference(ty),
                    Err(_) => return Ok(None),
                };
                let result_var = self.fresh_type_var();
                self.bind(result_var, TypeTerm::Primitive(TypePrimitive::String));
                match obj_ty {
                    Ty::Primitive(TypePrimitive::String)
                    | Ty::Primitive(TypePrimitive::Bool)
                    | Ty::Primitive(TypePrimitive::Char)
                    | Ty::Primitive(TypePrimitive::Int(_))
                    | Ty::Primitive(TypePrimitive::Decimal(_)) => Ok(Some(result_var)),
                    _ => Ok(None),
                }
            }
            // Keep iterator methods permissive for now; these are primarily
            // used by examples and desugar into Rust iterator chains.
            "iter" | "enumerate" => {
                let result_var = self.fresh_type_var();
                self.bind(result_var, TypeTerm::Any);
                Ok(Some(result_var))
            }
            _ => Ok(None),
        }
    }

    pub(crate) fn lookup_struct_field(
        &mut self,
        obj_var: TypeVarId,
        field: &Ident,
    ) -> Result<TypeVarId> {
        let ty = self.resolve_to_ty(obj_var)?;
        let resolved_ty = Self::peel_reference(ty);
        match resolved_ty {
            Ty::Struct(struct_ty) => {
                if let Some(def_field) = struct_ty.fields.iter().find(|f| f.name == *field) {
                    let var = self.type_from_ast_ty(&def_field.value)?;
                    Ok(var)
                } else {
                    self.emit_error(format!(
                        "unknown field {} on struct {}",
                        field, struct_ty.name
                    ));
                    Ok(self.error_type_var())
                }
            }
            Ty::Structural(structural) => {
                if let Some(def_field) = structural.fields.iter().find(|f| f.name == *field) {
                    let var = self.type_from_ast_ty(&def_field.value)?;
                    Ok(var)
                } else {
                    self.emit_error(format!("unknown field {}", field));
                    Ok(self.error_type_var())
                }
            }
            other => {
                self.emit_error(format!(
                    "cannot access field {} on value of type {}",
                    field, other
                ));
                Ok(self.error_type_var())
            }
        }
    }

    pub(crate) fn resolve_struct_literal(
        &mut self,
        struct_expr: &mut ExprStruct,
    ) -> Result<TypeVarId> {
        let struct_name = match self.struct_name_from_expr(&struct_expr.name) {
            Some(name) => name,
            None => {
                self.emit_error("struct literal target could not be resolved");
                return Ok(self.error_type_var());
            }
        };
        if let Some(def) = self.struct_defs.get(&struct_name).cloned() {
            let var = self.fresh_type_var();
            self.bind(var, TypeTerm::Struct(def.clone()));
            for field in &mut struct_expr.fields {
                if let Some(value) = field.value.as_mut() {
                    let value_var = self.infer_expr(value)?;
                    if let Some(struct_field) = def.fields.iter().find(|f| f.name == field.name) {
                        let ty_var = self.type_from_ast_ty(&struct_field.value)?;
                        self.unify(value_var, ty_var)?;
                    } else {
                        self.emit_error(format!(
                            "unknown field {} on struct {}",
                            field.name, def.name
                        ));
                        return Ok(self.error_type_var());
                    }
                }
            }
            Ok(var)
        } else {
            // Enum struct variants: `Enum::Variant { ... }`.
            if let ExprKind::Locator(Locator::Path(path)) = struct_expr.name.kind() {
                if path.segments.len() >= 2 {
                    let enum_name = path.segments[path.segments.len() - 2].as_str().to_string();
                    let variant_name = path.segments[path.segments.len() - 1].as_str();
                    if let Some(enum_def) = self.enum_defs.get(&enum_name).cloned() {
                        if let Some(variant) = enum_def
                            .variants
                            .iter()
                            .find(|v| v.name.as_str() == variant_name)
                        {
                            if let Ty::Structural(structural) = &variant.value {
                                for field in &mut struct_expr.fields {
                                    if let Some(value) = field.value.as_mut() {
                                        let value_var = self.infer_expr(value)?;
                                        if let Some(def_field) = structural
                                            .fields
                                            .iter()
                                            .find(|f| f.name == field.name)
                                        {
                                            let expected = self.type_from_ast_ty(&def_field.value)?;
                                            self.unify(value_var, expected)?;
                                        } else {
                                            self.emit_error(format!(
                                                "unknown field {} on enum variant {}::{}",
                                                field.name, enum_name, variant_name
                                            ));
                                            return Ok(self.error_type_var());
                                        }
                                    }
                                }
                                let var = self.fresh_type_var();
                                self.bind(var, TypeTerm::Enum(enum_def));
                                return Ok(var);
                            }
                        }
                    }
                }
            }

            self.emit_error(format!("unknown struct literal target: {}", struct_name));
            Ok(self.error_type_var())
        }
    }
}
