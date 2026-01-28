use crate::typing::unify::TypeTerm;
use crate::{typing_error, AstTypeInferencer, LoopContext, TypeVarId};
use fp_core::ast::*;
use fp_core::error::Result;

impl<'ctx> AstTypeInferencer<'ctx> {
    pub(crate) fn is_stmt_or_item_quote(&self, ty: &Ty) -> bool {
        match ty {
            Ty::Quote(quote) => matches!(
                quote.kind,
                QuoteFragmentKind::Stmt | QuoteFragmentKind::Item | QuoteFragmentKind::Expr
            ),
            Ty::Vec(vec) => self.is_stmt_or_item_quote(vec.ty.as_ref()),
            Ty::Array(array) => self.is_stmt_or_item_quote(array.elem.as_ref()),
            Ty::Slice(slice) => self.is_stmt_or_item_quote(slice.elem.as_ref()),
            _ => false,
        }
    }

    pub(crate) fn is_item_quote(&self, ty: &Ty) -> bool {
        match ty {
            Ty::Quote(quote) => matches!(quote.kind, QuoteFragmentKind::Item),
            Ty::Vec(vec) => self.is_item_quote(vec.ty.as_ref()),
            Ty::Array(array) => self.is_item_quote(array.elem.as_ref()),
            Ty::Slice(slice) => self.is_item_quote(slice.elem.as_ref()),
            _ => false,
        }
    }

    pub(crate) fn infer_block(&mut self, block: &mut ExprBlock) -> Result<TypeVarId> {
        self.enter_scope();
        let mut last = self.fresh_type_var();
        self.bind(last, TypeTerm::Unit);
        for stmt in &mut block.stmts {
            match stmt {
                BlockStmt::Item(item) => {
                    self.predeclare_item(item);
                    self.infer_item(item)?;
                    last = self.fresh_type_var();
                    self.bind(last, TypeTerm::Unit);
                }
                BlockStmt::Let(stmt_let) => {
                    let init_var = if let Some(init) = stmt_let.init.as_mut() {
                        if let PatternKind::Type(typed) = &stmt_let.pat.kind {
                            init.set_ty(typed.ty.clone());
                        }
                        self.infer_expr(init)?
                    } else {
                        let unit = self.fresh_type_var();
                        self.bind(unit, TypeTerm::Unit);
                        unit
                    };
                    let pattern_info = self.infer_pattern(&mut stmt_let.pat)?;
                    self.unify(pattern_info.var, init_var)?;
                    self.apply_pattern_generalization(&pattern_info)?;
                    last = self.fresh_type_var();
                    self.bind(last, TypeTerm::Unit);
                }
                BlockStmt::Expr(expr_stmt) => {
                    // If this is a splice in statement position, enforce stmt token
                    if let ExprKind::Splice(splice) = expr_stmt.expr.kind_mut() {
                        let token_var = self.infer_expr(splice.token.as_mut())?;
                        let token_ty = self.resolve_to_ty(token_var)?;
                        if !self.is_stmt_or_item_quote(&token_ty) {
                            match token_ty {
                                Ty::Quote(quote) => {
                                    self.emit_error(format!(
                                        "splice in statement position requires stmt/item/expr token, found {:?}",
                                        quote.kind
                                    ));
                                }
                                _ => self.emit_error("splice expects a quote token expression"),
                            }
                        }
                        // Statements do not contribute a value
                        last = self.fresh_type_var();
                        self.bind(last, TypeTerm::Unit);
                        continue;
                    }
                    let expr_var = self.infer_expr(expr_stmt.expr.as_mut())?;
                    if expr_stmt.has_value() {
                        last = expr_var;
                    } else {
                        last = self.fresh_type_var();
                        self.bind(last, TypeTerm::Unit);
                    }
                }
                BlockStmt::Noop => {
                    last = self.fresh_type_var();
                    self.bind(last, TypeTerm::Unit);
                }
                BlockStmt::Any(_) => {
                    let unit = self.unit_type_var();
                    last = unit;
                }
            }
        }
        self.exit_scope();
        Ok(last)
    }

    pub(crate) fn infer_if(&mut self, if_expr: &mut ExprIf) -> Result<TypeVarId> {
        let cond = self.infer_expr(if_expr.cond.as_mut())?;
        self.ensure_bool(cond, "if condition")?;
        let then_ty = self.infer_expr(if_expr.then.as_mut())?;
        if let Some(elze) = if_expr.elze.as_mut() {
            let else_ty = self.infer_expr(elze)?;
            self.unify(then_ty, else_ty)?;
        }
        Ok(then_ty)
    }

    pub(crate) fn infer_loop(&mut self, expr_loop: &mut ExprLoop) -> Result<TypeVarId> {
        let loop_result_var = self.fresh_type_var();
        self.loop_stack.push(LoopContext::new(loop_result_var));

        let body_var = match self.infer_expr(expr_loop.body.as_mut()) {
            Ok(var) => var,
            Err(err) => {
                self.loop_stack.pop();
                return Err(err);
            }
        };

        let unit_var = self.unit_type_var();
        if let Err(err) = self.unify(body_var, unit_var) {
            self.loop_stack.pop();
            return Err(err);
        }

        let Some(context) = self.loop_stack.pop() else {
            let message = "loop stack underflow when finishing loop inference".to_string();
            self.emit_error(message.clone());
            return Err(typing_error(message));
        };

        if !context.saw_break {
            self.bind(loop_result_var, TypeTerm::Nothing);
        }

        Ok(loop_result_var)
    }

    pub(crate) fn infer_while(&mut self, expr_while: &mut ExprWhile) -> Result<TypeVarId> {
        let cond_var = self.infer_expr(expr_while.cond.as_mut())?;
        self.ensure_bool(cond_var, "while condition")?;
        let loop_unit_var = self.unit_type_var();
        self.loop_stack.push(LoopContext::new(loop_unit_var));

        let body_var = match self.infer_expr(expr_while.body.as_mut()) {
            Ok(var) => var,
            Err(err) => {
                self.loop_stack.pop();
                return Err(err);
            }
        };

        if let Err(err) = self.unify(body_var, loop_unit_var) {
            self.loop_stack.pop();
            return Err(err);
        }

        let Some(_context) = self.loop_stack.pop() else {
            let message = "loop stack underflow when finishing while inference".to_string();
            self.emit_error(message.clone());
            return Err(typing_error(message));
        };

        Ok(loop_unit_var)
    }

    pub(crate) fn infer_match(&mut self, match_expr: &mut ExprMatch) -> Result<TypeVarId> {
        let mut result_var: Option<TypeVarId> = None;

        if let Some(scrutinee) = match_expr.scrutinee.as_mut() {
            let scrutinee_var = self.infer_expr(scrutinee.as_mut())?;
            let scrutinee_ty = self.resolve_to_ty(scrutinee_var).ok();
            for case in &mut match_expr.cases {
                self.enter_scope();

                if let Some(pat) = case.pat.as_mut() {
                    if let Some(Ty::Enum(enum_ty)) = scrutinee_ty.as_ref() {
                        qualify_enum_variant_pattern(pat, enum_ty);
                    }
                    let pat_info = self.infer_pattern(pat.as_mut())?;
                    self.unify(pat_info.var, scrutinee_var)?;
                    self.apply_pattern_generalization(&pat_info)?;
                }

                if let Some(guard) = case.guard.as_mut() {
                    let guard_var = self.infer_expr(guard.as_mut())?;
                    self.ensure_bool(guard_var, "match guard")?;
                }

                let body_var = self.infer_expr(case.body.as_mut())?;
                if let Some(existing) = result_var {
                    self.unify(existing, body_var)?;
                } else {
                    result_var = Some(body_var);
                }
                self.exit_scope();
            }
        } else {
            // Legacy lowering: cases are boolean conditions.
            for case in &mut match_expr.cases {
                let cond_var = self.infer_expr(case.cond.as_mut())?;
                self.ensure_bool(cond_var, "match case condition")?;

                let body_var = self.infer_expr(case.body.as_mut())?;
                if let Some(existing) = result_var {
                    self.unify(existing, body_var)?;
                } else {
                    result_var = Some(body_var);
                }
            }
        }

        match result_var {
            Some(var) => Ok(var),
            None => {
                self.emit_error("match expression requires at least one case");
                Ok(self.error_type_var())
            }
        }
    }
}

fn qualify_enum_variant_pattern(pat: &mut Pattern, enum_ty: &TypeEnum) {
    let PatternKind::Variant(variant) = pat.kind_mut() else {
        return;
    };
    let ExprKind::Locator(locator) = variant.name.kind() else {
        return;
    };
    let variant_name = match locator {
        Locator::Ident(ident) => ident.clone(),
        Locator::Path(path) if path.segments.len() == 1 => path.segments[0].clone(),
        _ => return,
    };
    if !enum_ty
        .variants
        .iter()
        .any(|variant| variant.name.as_str() == variant_name.as_str())
    {
        return;
    }

    let enum_ident = enum_ty.name.clone();
    let path = Path::new(vec![enum_ident, variant_name]);
    variant.name = Expr::path(path);
}
