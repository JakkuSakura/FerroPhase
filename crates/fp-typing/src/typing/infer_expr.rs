use fp_core::ast::*;
use fp_core::error::Result;
use crate::{AstTypeInferencer, TypeVarId};
use crate::typing::unify::TypeTerm;
use fp_core::ops::{BinOpKind, UnOpKind};
use crate::typing_error;

/// Infer the fragment kind for an unkinded quote based on its block shape.
/// - Single trailing expression and no statements => Expr
/// - All items at top level => Item
/// - Otherwise => Stmt
pub(crate) fn infer_quote_kind(block: &ExprBlock) -> QuoteFragmentKind {
    let all_items = block
        .stmts
        .iter()
        .all(|s| matches!(s, BlockStmt::Item(_)));
    let has_non_item_stmt = block
        .stmts
        .iter()
        .any(|s| !matches!(s, BlockStmt::Item(_)));

    if block.stmts.is_empty() {
        if block.last_expr().is_some() {
            return QuoteFragmentKind::Expr;
        }
        // Empty quote defaults to Stmt
        return QuoteFragmentKind::Stmt;
    }

    if all_items {
        // If there are only items and no trailing expression (or even if there is), it's an item fragment
        return QuoteFragmentKind::Item;
    }

    if !has_non_item_stmt && block.last_expr().is_some() {
        return QuoteFragmentKind::Expr;
    }

    QuoteFragmentKind::Stmt
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
                let ty = Ty::QuoteToken(Box::new(TypeQuoteToken { kind, inner: inner.map(Box::new) }));
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
                let new_expr = collection.clone().into_const_expr();
                *expr = new_expr;
                return self.infer_expr(expr);
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
                if let Some(ty) = existing_ty.as_ref() {
                    self.type_from_ast_ty(ty)?
                } else {
                    self.lookup_locator(locator)?
                }
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
            ExprKind::While(while_expr) => self.infer_while(while_expr)?,
            ExprKind::Try(try_expr) => self.infer_expr(try_expr.expr.as_mut())?,
            ExprKind::Reference(reference) => self.infer_reference(reference)?,
            ExprKind::Dereference(dereference) => self.infer_dereference(dereference)?,
            ExprKind::Index(index) => self.infer_index(index)?,
            ExprKind::Closure(closure) => self.infer_closure(closure)?,
            ExprKind::IntrinsicCall(call) => self.infer_intrinsic(call)?,
            ExprKind::Range(range) => self.infer_range(range)?,
            ExprKind::Await(await_expr) => self.infer_expr(await_expr.base.as_mut())?,
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
            BinOpKind::Add | BinOpKind::Sub | BinOpKind::Mul | BinOpKind::Div | BinOpKind::Mod => {
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
}
