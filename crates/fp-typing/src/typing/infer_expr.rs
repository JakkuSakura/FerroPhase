use crate::typing::unify::{FunctionTerm, TypeTerm, TypeVarKind};
use crate::{AstTypeInferencer, EnvEntry, PatternBinding, PatternInfo, TypeVarId};
use fp_core::ast::*;
use fp_core::error::Result;
use fp_core::intrinsics::IntrinsicCallKind;
use fp_core::module::path::{PathPrefix, QualifiedPath};
use fp_core::ops::{BinOpKind, UnOpKind};
use fp_core::span::Span;

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

fn quote_item_type_from_items(items: &[&Item]) -> Option<Ty> {
    let mut item_ty = None;
    for item in items {
        let Some(current) = quote_item_type_from_item(*item) else {
            return None;
        };
        match item_ty.as_ref() {
            Some(existing) if existing != &current => return None,
            None => item_ty = Some(current),
            _ => {}
        }
    }
    item_ty
}

fn quote_item_type_from_item(item: &Item) -> Option<Ty> {
    match item.kind() {
        ItemKind::DefFunction(_) | ItemKind::DeclFunction(_) => Some(Ty::Quote(TypeQuote {
            span: Span::null(),
            kind: QuoteFragmentKind::Item,
            item: Some(QuoteItemKind::Function),
            inner: None,
        })),
        ItemKind::DefStruct(_) | ItemKind::DefStructural(_) => Some(Ty::Quote(TypeQuote {
            span: Span::null(),
            kind: QuoteFragmentKind::Item,
            item: Some(QuoteItemKind::Struct),
            inner: None,
        })),
        ItemKind::DefEnum(_) => Some(Ty::Quote(TypeQuote {
            span: Span::null(),
            kind: QuoteFragmentKind::Item,
            item: Some(QuoteItemKind::Enum),
            inner: None,
        })),
        ItemKind::DefTrait(_) => Some(Ty::Quote(TypeQuote {
            span: Span::null(),
            kind: QuoteFragmentKind::Item,
            item: Some(QuoteItemKind::Trait),
            inner: None,
        })),
        ItemKind::Impl(_) => Some(Ty::Quote(TypeQuote {
            span: Span::null(),
            kind: QuoteFragmentKind::Item,
            item: Some(QuoteItemKind::Impl),
            inner: None,
        })),
        ItemKind::DefConst(_) | ItemKind::DeclConst(_) => Some(Ty::Quote(TypeQuote {
            span: Span::null(),
            kind: QuoteFragmentKind::Item,
            item: Some(QuoteItemKind::Const),
            inner: None,
        })),
        ItemKind::DefStatic(_) | ItemKind::DeclStatic(_) => Some(Ty::Quote(TypeQuote {
            span: Span::null(),
            kind: QuoteFragmentKind::Item,
            item: Some(QuoteItemKind::Static),
            inner: None,
        })),
        ItemKind::Module(_) => Some(Ty::Quote(TypeQuote {
            span: Span::null(),
            kind: QuoteFragmentKind::Item,
            item: Some(QuoteItemKind::Module),
            inner: None,
        })),
        ItemKind::Import(_) => Some(Ty::Quote(TypeQuote {
            span: Span::null(),
            kind: QuoteFragmentKind::Item,
            item: Some(QuoteItemKind::Use),
            inner: None,
        })),
        ItemKind::Macro(_) => Some(Ty::Quote(TypeQuote {
            span: Span::null(),
            kind: QuoteFragmentKind::Item,
            item: Some(QuoteItemKind::Macro),
            inner: None,
        })),
        ItemKind::DefType(_) | ItemKind::DeclType(_) => Some(Ty::Quote(TypeQuote {
            span: Span::null(),
            kind: QuoteFragmentKind::Item,
            item: Some(QuoteItemKind::Type),
            inner: None,
        })),
        _ => None,
    }
}

fn quote_ty_from_fragment(kind: QuoteFragmentKind, inner: Option<Ty>) -> Ty {
    Ty::Quote(TypeQuote {
        span: Span::null(),
        kind,
        item: None,
        inner: inner.map(Box::new),
    })
}

fn make_std_task_param_ty(name: &str, arg: Ty) -> Ty {
    let path = ParameterPath::new(
        PathPrefix::Plain,
        vec![
            ParameterPathSegment::from_ident(Ident::new("std")),
            ParameterPathSegment::from_ident(Ident::new("task")),
            ParameterPathSegment::new(Ident::new(name), vec![arg]),
        ],
    );
    Ty::expr(Expr::name(Name::parameter_path(path)))
}

fn extract_std_task_inner_ty(ty: &Ty, container: &str) -> Option<Ty> {
    let Ty::Expr(expr) = ty else {
        return None;
    };
    let ExprKind::Name(Name::ParameterPath(path)) = expr.kind() else {
        return None;
    };
    let [std_seg, task_seg, container_seg] = path.segments.as_slice() else {
        return None;
    };
    if std_seg.ident.as_str() != "std" || task_seg.ident.as_str() != "task" {
        return None;
    }
    if container_seg.ident.as_str() != container {
        return None;
    }
    if container_seg.args.len() != 1 {
        return None;
    }
    Some(container_seg.args[0].clone())
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
        ExprKind::Return(_) => true,
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
                        || case.guard.as_ref().is_some_and(|e| expr_contains_return(e))
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
            target_has_return
                || invoke.args.iter().any(|arg| expr_contains_return(arg))
                || invoke
                    .kwargs
                    .iter()
                    .any(|arg| expr_contains_return(&arg.value))
        }
        ExprKind::Paren(paren) => expr_contains_return(paren.expr.as_ref()),
        ExprKind::Quote(quote) => block_contains_return(&quote.block),
        ExprKind::Splice(splice) => expr_contains_return(splice.token.as_ref()),
        _ => false,
    }
}

impl<'ctx> AstTypeInferencer<'ctx> {
    pub(crate) fn infer_expr(&mut self, expr: &mut Expr) -> Result<TypeVarId> {
        let span = expr.span();
        let previous = self.current_span;
        let active = self.span_or_previous(span, previous);
        self.current_span = active;
        let result = (|| {
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
                    let ty = if matches!(kind, QuoteFragmentKind::Item) {
                        let mut item_like = 0usize;
                        let mut items = Vec::new();
                        for stmt in &quote.block.stmts {
                            match stmt {
                                BlockStmt::Item(item) => {
                                    item_like += 1;
                                    items.push(item.as_ref());
                                }
                                BlockStmt::Expr(expr_stmt) => {
                                    if let ExprKind::Item(item) = expr_stmt.expr.kind() {
                                        item_like += 1;
                                        items.push(item.as_ref());
                                    }
                                }
                                _ => {}
                            }
                        }
                        let has_non_items = item_like != quote.block.stmts.len();
                        if items.len() == 1 && quote.block.stmts.len() == 1 {
                            quote_item_type_from_item(items[0])
                                .unwrap_or_else(|| quote_ty_from_fragment(kind, inner.clone()))
                        } else if quote.block.stmts.len() > 1 {
                            if has_non_items {
                                self.emit_error("quote<item> expects only item statements");
                            }
                            let elem_ty = quote_item_type_from_items(&items).unwrap_or_else(|| {
                                if items.is_empty() {
                                    Ty::Quote(TypeQuote {
                                        span: Span::null(),
                                        kind: QuoteFragmentKind::Item,
                                        item: None,
                                        inner: None,
                                    })
                                } else {
                                    self.emit_error(
                                        "quote<item> contains multiple item kinds; using item type",
                                    );
                                    Ty::Quote(TypeQuote {
                                        span: Span::null(),
                                        kind: QuoteFragmentKind::Item,
                                        item: None,
                                        inner: None,
                                    })
                                }
                            });
                            Ty::Slice(TypeSlice {
                                elem: Box::new(elem_ty),
                            })
                        } else {
                            if has_non_items {
                                self.emit_error("quote<item> expects only item statements");
                            }
                            quote_ty_from_fragment(kind, inner)
                        }
                    } else {
                        quote_ty_from_fragment(kind, inner)
                    };
                    let var = self.type_from_ast_ty(&ty)?;
                    expr.set_ty(ty);
                    var
                }
                ExprKind::Splice(splice) => {
                    // Expression-position splice must carry an expr token
                    let token_var = self.infer_expr(splice.token.as_mut())?;
                    let token_ty = self.resolve_to_ty(token_var)?;
                    match token_ty {
                        Ty::Quote(quote) if quote.kind == QuoteFragmentKind::Expr => {
                            if let Some(inner) = quote.inner.clone() {
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
                        Ty::Quote(quote) => {
                            self.emit_error(format!(
                                "splice in expression position requires expr token, found {:?}",
                                quote.kind
                            ));
                            self.error_type_var()
                        }
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
                        let hint_ty = if let Some(ty) = existing_ty.as_ref() {
                            self.type_from_ast_ty(ty)
                                .ok()
                                .and_then(|var| self.resolve_to_ty(var).ok())
                        } else {
                            None
                        };
                        if matches!(hint_ty.as_ref(), Some(Ty::Array(_))) {
                            self.infer_value(value.as_ref())?
                        } else if let Some(Ty::Vec(vec_ty)) = hint_ty.as_ref() {
                            let elem_var = self.type_from_ast_ty(&vec_ty.ty)?;
                            for value in &list.values {
                                let value_var = self.infer_value(value)?;
                                self.unify(value_var, elem_var)?;
                            }
                            let vec_var = self.fresh_type_var();
                            self.bind(vec_var, TypeTerm::Vec(elem_var));
                            vec_var
                        } else if let Some(Ty::Slice(slice_ty)) = hint_ty.as_ref() {
                            let elem_var = self.type_from_ast_ty(&slice_ty.elem)?;
                            for value in &list.values {
                                let value_var = self.infer_value(value)?;
                                self.unify(value_var, elem_var)?;
                            }
                            let slice_var = self.fresh_type_var();
                            self.bind(slice_var, TypeTerm::Slice(elem_var));
                            slice_var
                        } else {
                            self.infer_list_value_as_vec(list)?
                        }
                    } else {
                        self.infer_value(value.as_ref())?
                    }
                }
                ExprKind::Name(locator) => {
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
                            let resolved = match ty {
                                Ty::Enum(_) => Some(ty.clone()),
                                _ => self
                                    .type_from_ast_ty(ty)
                                    .ok()
                                    .and_then(|var| self.resolve_to_ty(var).ok()),
                            };
                            if let Some(Ty::Enum(enum_ty)) = resolved.as_ref() {
                                if let Some(var) = self
                                    .resolve_struct_literal_as_enum_variant(struct_expr, enum_ty)?
                                {
                                    var
                                } else {
                                    self.resolve_struct_literal(struct_expr)?
                                }
                            } else {
                                self.resolve_struct_literal(struct_expr)?
                            }
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
                    if let Some(Ty::Vec(vec_ty)) = existing_ty.as_ref() {
                        let elem_var = self.type_from_ast_ty(&vec_ty.ty)?;
                        for value in &mut array.values {
                            let next = self.infer_expr(value)?;
                            self.unify(elem_var, next)?;
                        }
                        let vec_var = self.fresh_type_var();
                        self.bind(vec_var, TypeTerm::Vec(elem_var));
                        let vec_ty = self.resolve_to_ty(vec_var)?;
                        expr.set_ty(vec_ty);
                        vec_var
                    } else {
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
                        let len_expr = Expr::value(Value::int(array.values.len() as i64)).into();
                        let array_var = self.fresh_type_var();
                        self.bind(array_var, TypeTerm::Array(elem_var, Some(len_expr)));
                        let array_ty = self.resolve_to_ty(array_var)?;
                        expr.set_ty(array_ty);
                        array_var
                    }
                }
                ExprKind::ArrayRepeat(array_repeat) => {
                    if let Some(Ty::Vec(vec_ty)) = existing_ty.as_ref() {
                        let elem_var = self.type_from_ast_ty(&vec_ty.ty)?;
                        let value_var = self.infer_expr(array_repeat.elem.as_mut())?;
                        self.unify(elem_var, value_var)?;
                        let vec_var = self.fresh_type_var();
                        self.bind(vec_var, TypeTerm::Vec(elem_var));
                        let vec_ty = self.resolve_to_ty(vec_var)?;
                        expr.set_ty(vec_ty);
                        vec_var
                    } else {
                        let elem_var = self.infer_expr(array_repeat.elem.as_mut())?;
                        let len_var = self.infer_expr(array_repeat.len.as_mut())?;
                        let expected_len = self.fresh_type_var();
                        self.bind(
                            expected_len,
                            TypeTerm::Primitive(TypePrimitive::Int(TypeInt::U64)),
                        );
                        self.unify(len_var, expected_len)?;

                        let length_expr = array_repeat.len.as_ref().get();
                        let array_var = self.fresh_type_var();
                        self.bind(
                            array_var,
                            TypeTerm::Array(elem_var, Some(length_expr.into())),
                        );
                        let array_ty = self.resolve_to_ty(array_var)?;
                        expr.set_ty(array_ty);
                        array_var
                    }
                }
                ExprKind::Paren(paren) => self.infer_expr(paren.expr.as_mut())?,
                ExprKind::FormatString(_) => {
                    let var = self.fresh_type_var();
                    self.bind(var, TypeTerm::Primitive(TypePrimitive::String));
                    var
                }
                ExprKind::Match(match_expr) => self.infer_match(match_expr)?,
                ExprKind::Loop(loop_expr) => self.infer_loop(loop_expr)?,
                ExprKind::Return(ret) => {
                    if let Some(value) = ret.value.as_mut() {
                        self.infer_expr(value)?;
                    }
                    // Diverging expression.
                    self.nothing_type_var()
                }
                ExprKind::Break(brk) => {
                    let value_var = if let Some(value) = brk.value.as_mut() {
                        self.infer_expr(value)?
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
                        result_var
                    } else {
                        self.emit_error("`break` used outside of a loop");
                        self.error_type_var()
                    }
                }
                ExprKind::Continue(_) => {
                    if self.loop_stack.is_empty() {
                        self.emit_error("`continue` used outside of a loop");
                        self.error_type_var()
                    } else {
                        self.nothing_type_var()
                    }
                }
                ExprKind::ConstBlock(const_block) => self.infer_expr(const_block.expr.as_mut())?,
                ExprKind::For(for_expr) => {
                    let pat_info = self.infer_pattern(for_expr.pat.as_mut())?;
                    let iter_var = self.infer_expr(for_expr.iter.as_mut())?;
                    if let Ok(iter_ty) = self.resolve_to_ty(iter_var) {
                        if let Some(elem_var) = self.iter_element_var_from_ty(&iter_ty) {
                            self.unify(pat_info.var, elem_var)?;
                        }
                    }
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
                ExprKind::Await(await_expr) => {
                    let base_var = self.infer_expr(await_expr.base.as_mut())?;
                    let base_ty = self.resolve_to_ty(base_var)?;

                    if let Some(inner_ty) = extract_std_task_inner_ty(&base_ty, "Future") {
                        self.type_from_ast_ty(&inner_ty)?
                    } else if let Some(inner_ty) = extract_std_task_inner_ty(&base_ty, "Task") {
                        self.type_from_ast_ty(&inner_ty)?
                    } else {
                        base_var
                    }
                }
                ExprKind::Async(async_expr) => {
                    let inner_var = self.infer_expr(async_expr.expr.as_mut())?;
                    let inner_ty = self.resolve_to_ty(inner_var)?;
                    let future_ty = make_std_task_param_ty("Future", inner_ty);
                    self.type_from_ast_ty(&future_ty)?
                }
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
        })();
        self.current_span = previous;
        result.map_err(|err| self.error_with_span(err, active))
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
                if matches!(binop.kind, BinOpKind::Add) {
                    let lhs_ty = self.resolve_to_ty(lhs)?;
                    let rhs_ty = self.resolve_to_ty(rhs)?;
                    let is_string_ref = |ty: &Ty| {
                        matches!(
                            ty,
                            Ty::Reference(reference)
                                if matches!(reference.ty.as_ref(), Ty::Primitive(TypePrimitive::String))
                        )
                    };
                    if is_string_ref(&lhs_ty) && is_string_ref(&rhs_ty) {
                        self.unify(lhs, rhs)?;
                        return Ok(lhs);
                    }
                }
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
            UnOpKind::Deref => self.expect_reference(value_var, "dereference expression"),
            UnOpKind::Any(_) => Ok(value_var),
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
        if matches!(index.index.kind(), ExprKind::Range(_)) {
            if let ExprKind::Range(range) = index.index.kind_mut() {
                let _ = self.infer_range(range)?;
            }
            return self.infer_slice_index(object_var);
        }

        let idx_var = self.infer_expr(index.index.as_mut())?;

        if let Some((key_var, value_var)) = self.lookup_hashmap_args(object_var) {
            self.unify(key_var, idx_var)?;
            return Ok(value_var);
        }

        if let Ok(obj_ty) = self.resolve_to_ty(object_var) {
            match Self::peel_reference(obj_ty) {
                Ty::Vec(vec) => return self.type_from_ast_ty(&vec.ty),
                Ty::Array(array) => return self.type_from_ast_ty(&array.elem),
                Ty::Slice(slice) => return self.type_from_ast_ty(&slice.elem),
                Ty::Primitive(TypePrimitive::String) => {
                    self.ensure_integer(idx_var, "string index")?;
                    return self.type_from_ast_ty(&Ty::Primitive(TypePrimitive::String));
                }
                _ => {}
            }
        }

        let idx_ty = self.resolve_to_ty(idx_var)?;
        let idx_root = self.find(idx_var);
        let idx_bound_reference = match self.type_vars[idx_root].kind.clone() {
            TypeVarKind::Bound(TypeTerm::Reference(_)) => true,
            TypeVarKind::Link(next) => {
                let root = self.find(next);
                matches!(
                    self.type_vars[root].kind,
                    TypeVarKind::Bound(TypeTerm::Reference(_))
                )
            }
            _ => false,
        };
        let idx_non_integer = idx_bound_reference
            || matches!(
                idx_ty,
                Ty::Reference(_) | Ty::Primitive(TypePrimitive::String)
            );
        let idx_is_string_literal = matches!(
            index.index.kind(),
            ExprKind::Value(value) if matches!(value.as_ref(), Value::String(_))
        );
        if matches!(self.resolve_to_ty(object_var), Ok(Ty::Struct(struct_ty)) if struct_ty.name.as_str() == "HashMap")
        {
            let any_var = self.fresh_type_var();
            self.bind(any_var, TypeTerm::Any);
            return Ok(any_var);
        }
        if idx_non_integer || idx_is_string_literal {
            let map_var = self.fresh_type_var();
            let map_ty = self.make_hashmap_struct();
            self.bind(map_var, TypeTerm::Struct(map_ty));
            if self.unify(object_var, map_var).is_ok() {
                let any_var = self.fresh_type_var();
                self.bind(any_var, TypeTerm::Any);
                return Ok(any_var);
            }

            let map_var = self.fresh_type_var();
            let map_ty = self.make_hashmap_struct();
            self.bind(map_var, TypeTerm::Struct(map_ty));
            let ref_var = self.fresh_type_var();
            self.bind(ref_var, TypeTerm::Reference(map_var));
            if self.unify(object_var, ref_var).is_ok() {
                let any_var = self.fresh_type_var();
                self.bind(any_var, TypeTerm::Any);
                return Ok(any_var);
            }

            self.emit_error("indexing with a non-integer key requires a HashMap");
            return Ok(self.error_type_var());
        }

        let elem_vec_var = self.fresh_type_var();
        let vec_var = self.fresh_type_var();
        self.bind(vec_var, TypeTerm::Vec(elem_vec_var));
        if self.unify(object_var, vec_var).is_ok() {
            self.ensure_integer(idx_var, "index expression")?;
            return Ok(elem_vec_var);
        }

        let elem_slice_var = self.fresh_type_var();
        let slice_var = self.fresh_type_var();
        self.bind(slice_var, TypeTerm::Slice(elem_slice_var));
        if self.unify(object_var, slice_var).is_err() {
            let object_ty = self.resolve_to_ty(object_var)?;
            match object_ty {
                Ty::Array(array_ty) => {
                    self.ensure_integer(idx_var, "index expression")?;
                    let elem_var = self.type_from_ast_ty(&array_ty.elem)?;
                    return Ok(elem_var);
                }
                Ty::Reference(reference) => {
                    if let Ty::Array(array_ty) = *reference.ty {
                        self.ensure_integer(idx_var, "index expression")?;
                        let elem_var = self.type_from_ast_ty(&array_ty.elem)?;
                        return Ok(elem_var);
                    }
                }
                Ty::Struct(struct_ty) if struct_ty.name.as_str() == "HashMap" => {
                    let any_var = self.fresh_type_var();
                    self.bind(any_var, TypeTerm::Any);
                    return Ok(any_var);
                }
                _ => {}
            }
            self.emit_error("indexing is only supported on string, vector, slice, or array types");
            return Ok(self.error_type_var());
        }
        self.ensure_integer(idx_var, "index expression")?;
        Ok(elem_slice_var)
    }

    fn infer_slice_index(&mut self, object_var: TypeVarId) -> Result<TypeVarId> {
        let is_string_like = |ty: &Ty| {
            matches!(ty, Ty::Primitive(TypePrimitive::String))
                || matches!(
                    ty,
                    Ty::Reference(reference)
                        if matches!(reference.ty.as_ref(), Ty::Primitive(TypePrimitive::String))
                )
        };
        if let Ok(obj_ty) = self.resolve_to_ty(object_var) {
            match Self::peel_reference(obj_ty) {
                Ty::Primitive(TypePrimitive::String) => {
                    return self.type_from_ast_ty(&Ty::Primitive(TypePrimitive::String));
                }
                Ty::Vec(vec) => {
                    if is_string_like(vec.ty.as_ref()) {
                        return self.type_from_ast_ty(&Ty::Primitive(TypePrimitive::String));
                    }
                    let elem_var = self.type_from_ast_ty(&vec.ty)?;
                    let slice_var = self.fresh_type_var();
                    self.bind(slice_var, TypeTerm::Slice(elem_var));
                    return Ok(slice_var);
                }
                Ty::Slice(slice) => {
                    if is_string_like(slice.elem.as_ref()) {
                        return self.type_from_ast_ty(&Ty::Primitive(TypePrimitive::String));
                    }
                    let elem_var = self.type_from_ast_ty(&slice.elem)?;
                    let slice_var = self.fresh_type_var();
                    self.bind(slice_var, TypeTerm::Slice(elem_var));
                    return Ok(slice_var);
                }
                Ty::Array(array) => {
                    if is_string_like(array.elem.as_ref()) {
                        return self.type_from_ast_ty(&Ty::Primitive(TypePrimitive::String));
                    }
                    let elem_var = self.type_from_ast_ty(&array.elem)?;
                    let slice_var = self.fresh_type_var();
                    self.bind(slice_var, TypeTerm::Slice(elem_var));
                    return Ok(slice_var);
                }
                _ => {}
            }
        }

        let string_var = self.fresh_type_var();
        self.bind(string_var, TypeTerm::Primitive(TypePrimitive::String));
        let vec_string_var = self.fresh_type_var();
        self.bind(vec_string_var, TypeTerm::Vec(string_var));
        if self.unify(object_var, vec_string_var).is_ok() {
            return Ok(string_var);
        }
        let ref_string_var = self.fresh_type_var();
        self.bind(ref_string_var, TypeTerm::Reference(string_var));
        let vec_ref_string_var = self.fresh_type_var();
        self.bind(vec_ref_string_var, TypeTerm::Vec(ref_string_var));
        if self.unify(object_var, vec_ref_string_var).is_ok() {
            return Ok(string_var);
        }
        let slice_string_var = self.fresh_type_var();
        self.bind(slice_string_var, TypeTerm::Slice(string_var));
        if self.unify(object_var, slice_string_var).is_ok() {
            return Ok(string_var);
        }
        let slice_ref_string_var = self.fresh_type_var();
        self.bind(slice_ref_string_var, TypeTerm::Slice(ref_string_var));
        if self.unify(object_var, slice_ref_string_var).is_ok() {
            return Ok(string_var);
        }
        let array_string_var = self.fresh_type_var();
        self.bind(array_string_var, TypeTerm::Array(string_var, None));
        if self.unify(object_var, array_string_var).is_ok() {
            return Ok(string_var);
        }
        let ref_string_vec = self.fresh_type_var();
        self.bind(ref_string_vec, TypeTerm::Reference(vec_string_var));
        if self.unify(object_var, ref_string_vec).is_ok() {
            return Ok(string_var);
        }
        let ref_string_slice = self.fresh_type_var();
        self.bind(ref_string_slice, TypeTerm::Reference(slice_string_var));
        if self.unify(object_var, ref_string_slice).is_ok() {
            return Ok(string_var);
        }

        let elem_var = self.fresh_type_var();
        let slice_var = self.fresh_type_var();
        self.bind(slice_var, TypeTerm::Slice(elem_var));

        let vec_var = self.fresh_type_var();
        self.bind(vec_var, TypeTerm::Vec(elem_var));
        if self.unify(object_var, vec_var).is_ok() {
            return Ok(slice_var);
        }

        if self.unify(object_var, slice_var).is_ok() {
            return Ok(slice_var);
        }

        let string_var = self.fresh_type_var();
        self.bind(string_var, TypeTerm::Primitive(TypePrimitive::String));
        if self.unify(object_var, string_var).is_ok() {
            return Ok(string_var);
        }

        if let Ok(obj_ty) = self.resolve_to_ty(object_var) {
            if let Ty::Reference(reference) = obj_ty {
                match *reference.ty {
                    Ty::Vec(vec) => {
                        if matches!(vec.ty.as_ref(), Ty::Primitive(TypePrimitive::String)) {
                            return self.type_from_ast_ty(&Ty::Primitive(TypePrimitive::String));
                        }
                        let elem_var = self.type_from_ast_ty(&vec.ty)?;
                        let slice_var = self.fresh_type_var();
                        self.bind(slice_var, TypeTerm::Slice(elem_var));
                        return Ok(slice_var);
                    }
                    Ty::Slice(slice) => {
                        if matches!(slice.elem.as_ref(), Ty::Primitive(TypePrimitive::String)) {
                            return self.type_from_ast_ty(&Ty::Primitive(TypePrimitive::String));
                        }
                        let elem_var = self.type_from_ast_ty(&slice.elem)?;
                        let slice_var = self.fresh_type_var();
                        self.bind(slice_var, TypeTerm::Slice(elem_var));
                        return Ok(slice_var);
                    }
                    Ty::Array(array) => {
                        if matches!(array.elem.as_ref(), Ty::Primitive(TypePrimitive::String)) {
                            return self.type_from_ast_ty(&Ty::Primitive(TypePrimitive::String));
                        }
                        let elem_var = self.type_from_ast_ty(&array.elem)?;
                        let slice_var = self.fresh_type_var();
                        self.bind(slice_var, TypeTerm::Slice(elem_var));
                        return Ok(slice_var);
                    }
                    Ty::Primitive(TypePrimitive::String) => {
                        return self.type_from_ast_ty(&Ty::Primitive(TypePrimitive::String));
                    }
                    _ => {}
                }
            }
        }

        self.emit_error("slicing is only supported on string, vector, slice, or array types");
        Ok(self.error_type_var())
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

        for arg in &mut call.args {
            arg_vars.push(self.infer_expr(arg)?);
        }
        for kwarg in &mut call.kwargs {
            arg_vars.push(self.infer_expr(&mut kwarg.value)?);
        }

        match call.kind {
            IntrinsicCallKind::Panic => {
                if arg_vars.len() > 1 {
                    self.emit_error("panic expects at most one argument");
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
            IntrinsicCallKind::Format => {
                self.bind(result_var, TypeTerm::Primitive(TypePrimitive::String));
            }
            IntrinsicCallKind::Slice => {
                let elem_var = self.fresh_type_var();
                self.bind(elem_var, TypeTerm::Any);
                self.bind(result_var, TypeTerm::Slice(elem_var));
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
            IntrinsicCallKind::CatchUnwind => {
                if arg_vars.len() != 1 {
                    self.emit_error(format!(
                        "intrinsic {:?} expects 1 argument, found {}",
                        call.kind,
                        arg_vars.len()
                    ));
                }
                if let Some(&arg_var) = arg_vars.first() {
                    let ret_var = self.unit_type_var();
                    let fn_var = self.fresh_type_var();
                    self.bind(
                        fn_var,
                        TypeTerm::Function(FunctionTerm {
                            params: Vec::new(),
                            ret: ret_var,
                        }),
                    );
                    self.unify(arg_var, fn_var)?;
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
            IntrinsicCallKind::TypeOf => {
                if arg_vars.len() != 1 {
                    self.emit_error(format!(
                        "intrinsic {:?} expects 1 argument, found {}",
                        call.kind,
                        arg_vars.len()
                    ));
                }
                self.bind(
                    result_var,
                    TypeTerm::Custom(Ty::Type(TypeType::new(Span::null()))),
                );
            }
            IntrinsicCallKind::ReflectFields => {
                if arg_vars.len() != 1 {
                    self.emit_error(format!(
                        "intrinsic {:?} expects 1 argument, found {}",
                        call.kind,
                        arg_vars.len()
                    ));
                }
                let string_ref = Ty::Reference(TypeReference {
                    ty: Box::new(Ty::Primitive(TypePrimitive::String)),
                    mutability: None,
                    lifetime: None,
                });
                let fields = vec![
                    StructuralField::new(Ident::new("name".to_string()), string_ref.clone()),
                    StructuralField::new(Ident::new("type_name".to_string()), string_ref),
                ];
                let struct_ty = TypeStructural { fields };
                let elem_var = self.fresh_type_var();
                self.bind(elem_var, TypeTerm::Structural(struct_ty));
                self.bind(result_var, TypeTerm::Vec(elem_var));
            }
            IntrinsicCallKind::CreateStruct
            | IntrinsicCallKind::CloneStruct
            | IntrinsicCallKind::AddField
            | IntrinsicCallKind::FieldType
            | IntrinsicCallKind::VecType => {
                let expected = match call.kind {
                    IntrinsicCallKind::AddField => 3,
                    IntrinsicCallKind::FieldType => 2,
                    IntrinsicCallKind::VecType => 1,
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
                self.bind(
                    result_var,
                    TypeTerm::Custom(Ty::Type(TypeType::new(Span::null()))),
                );
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

        if !invoke.kwargs.is_empty() {
            match &invoke.target {
                ExprInvokeTarget::Function(locator) => {
                    let Some(sig) = self.lookup_function_signature(locator) else {
                        self.emit_error(
                            "keyword arguments require a known function signature".to_string(),
                        );
                        return Ok(self.error_type_var());
                    };
                    if !self.apply_kwargs_to_invoke(invoke, &sig) {
                        return Ok(self.error_type_var());
                    }
                }
                _ => {
                    self.emit_error("keyword arguments are only supported on function calls");
                    return Ok(self.error_type_var());
                }
            }
        }

        if let ExprInvokeTarget::Function(locator) = &mut invoke.target {
            if let Some(ident) = locator.as_ident() {
                if ident.as_str() == "printf" {
                    return self.infer_builtin_printf(invoke);
                }
                if ident.as_str() == "type" {
                    if invoke.args.len() != 1 {
                        self.emit_error("type() expects exactly one argument");
                        return Ok(self.error_type_var());
                    }
                    let _ = self.infer_expr(&mut invoke.args[0])?;
                    let type_var = self.fresh_type_var();
                    self.bind(
                        type_var,
                        TypeTerm::Custom(Ty::Type(TypeType::new(Span::null()))),
                    );
                    return Ok(type_var);
                }
            }
        }

        if let ExprInvokeTarget::Function(locator) = &invoke.target {
            if let Some(sig) = self.lookup_extern_function_signature(locator) {
                if invoke.args.len() != sig.params.len() {
                    self.emit_error("extern \"C\" call arity mismatch");
                    return Ok(self.error_type_var());
                }
                for (arg_expr, param) in invoke.args.iter_mut().zip(sig.params.iter()) {
                    let arg_var = self.infer_expr(arg_expr)?;
                    let param_var = self.type_from_ast_ty(&param.ty)?;
                    let expects_cstr = self
                        .resolve_to_ty(param_var)
                        .ok()
                        .map(|ty| match ty {
                            Ty::Struct(struct_ty) => struct_ty.name.as_str() == "CStr",
                            Ty::Reference(reference) => matches!(
                                reference.ty.as_ref(),
                                Ty::Struct(struct_ty) if struct_ty.name.as_str() == "CStr"
                            ),
                            _ => false,
                        })
                        .unwrap_or(false);
                    if expects_cstr {
                        if matches!(arg_expr.kind(), ExprKind::Value(value) if matches!(value.as_ref(), Value::String(_)))
                        {
                            arg_expr.set_ty(param.ty.clone());
                            continue;
                        }
                        if let Some(arg_ty) = arg_expr.ty() {
                            if self.is_string_like_type(arg_ty) {
                                arg_expr.set_ty(param.ty.clone());
                                continue;
                            }
                        }
                        if let Ok(arg_ty) = self.resolve_to_ty(arg_var) {
                            if self.is_string_like_type(&arg_ty) {
                                arg_expr.set_ty(param.ty.clone());
                                continue;
                            }
                        }
                    }
                    self.unify(arg_var, param_var)?;
                }
                let ret_var = if let Some(ret_ty) = &sig.ret_ty {
                    self.type_from_ast_ty(ret_ty)?
                } else {
                    let unit = self.fresh_type_var();
                    self.bind(unit, TypeTerm::Unit);
                    unit
                };
                return Ok(ret_var);
            }
            if let Some(sig) = self.lookup_function_signature(locator) {
                if sig.abi == Abi::C && sig.generics_params.is_empty() && sig.receiver.is_none() {
                    if invoke.args.len() != sig.params.len() {
                        self.emit_error("extern \"C\" call arity mismatch");
                        return Ok(self.error_type_var());
                    }
                    for (arg_expr, param) in invoke.args.iter_mut().zip(sig.params.iter()) {
                        let arg_var = self.infer_expr(arg_expr)?;
                        let param_var = self.type_from_ast_ty(&param.ty)?;
                        let expects_cstr = self
                            .resolve_to_ty(param_var)
                            .ok()
                            .map(|ty| match ty {
                                Ty::Struct(struct_ty) => struct_ty.name.as_str() == "CStr",
                                Ty::Reference(reference) => matches!(
                                    reference.ty.as_ref(),
                                    Ty::Struct(struct_ty) if struct_ty.name.as_str() == "CStr"
                                ),
                                _ => false,
                            })
                            .unwrap_or(false);
                        if expects_cstr {
                            if matches!(arg_expr.kind(), ExprKind::Value(value) if matches!(value.as_ref(), Value::String(_)))
                            {
                                arg_expr.set_ty(param.ty.clone());
                                continue;
                            }
                            if let Some(arg_ty) = arg_expr.ty() {
                                if self.is_string_like_type(arg_ty) {
                                    arg_expr.set_ty(param.ty.clone());
                                    continue;
                                }
                            }
                            if let Ok(arg_ty) = self.resolve_to_ty(arg_var) {
                                if self.is_string_like_type(&arg_ty) {
                                    arg_expr.set_ty(param.ty.clone());
                                    continue;
                                }
                            }
                        }
                        self.unify(arg_var, param_var)?;
                    }
                    let ret_var = if let Some(ret_ty) = &sig.ret_ty {
                        self.type_from_ast_ty(ret_ty)?
                    } else {
                        let unit = self.fresh_type_var();
                        self.bind(unit, TypeTerm::Unit);
                        unit
                    };
                    return Ok(ret_var);
                }
            }
        }

        let func_var = match &mut invoke.target {
            ExprInvokeTarget::Function(locator) => {
                if let Some(var) = self.lookup_associated_function(locator)? {
                    var
                } else {
                    let var = self.lookup_locator(locator)?;
                    if let Ok(resolved) = self.resolve_to_ty(var) {
                        if matches!(
                            resolved,
                            Ty::Struct(_) | Ty::Enum(_) | Ty::Structural(_) | Ty::TypeBounds(_)
                        ) {
                            self.emit_error(format!(
                                "cannot invoke type {} as a function",
                                locator
                            ));
                            return Ok(self.error_type_var());
                        }
                    }
                    var
                }
            }
            ExprInvokeTarget::Expr(expr) => self.infer_expr(expr.as_mut())?,
            ExprInvokeTarget::Closure(_) => {
                let fn_ty = match &invoke.target {
                    ExprInvokeTarget::Closure(func) => {
                        self.ty_from_function_signature(&func.sig)?
                    }
                    _ => Ty::Unknown(TypeUnknown),
                };
                self.type_from_ast_ty(&fn_ty)?
            }
            ExprInvokeTarget::BinOp(kind) => {
                if invoke.args.len() != 2 {
                    let message = "binary operator invocation expects two arguments".to_string();
                    self.emit_error(message.clone());
                    return Ok(self.error_type_var());
                }
                let lhs = self.infer_expr(&mut invoke.args[0])?;
                let rhs = self.infer_expr(&mut invoke.args[1])?;
                let result = match kind {
                    BinOpKind::Add
                    | BinOpKind::Sub
                    | BinOpKind::Mul
                    | BinOpKind::Div
                    | BinOpKind::Mod
                    | BinOpKind::Shl
                    | BinOpKind::Shr => {
                        if matches!(kind, BinOpKind::Add) {
                            let lhs_ty = self.resolve_to_ty(lhs)?;
                            let rhs_ty = self.resolve_to_ty(rhs)?;
                            let is_string_ref = |ty: &Ty| {
                                matches!(
                                    ty,
                                    Ty::Reference(reference)
                                        if matches!(
                                            reference.ty.as_ref(),
                                            Ty::Primitive(TypePrimitive::String)
                                        )
                                )
                            };
                            if is_string_ref(&lhs_ty) && is_string_ref(&rhs_ty) {
                                self.unify(lhs, rhs)?;
                                return Ok(lhs);
                            }
                        }
                        self.ensure_numeric(lhs, "binary operand")?;
                        self.unify(lhs, rhs)?;
                        lhs
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
                        bool_var
                    }
                    BinOpKind::And | BinOpKind::Or => {
                        self.ensure_bool(lhs, "logical operand")?;
                        self.ensure_bool(rhs, "logical operand")?;
                        let bool_var = self.fresh_type_var();
                        self.bind(bool_var, TypeTerm::Primitive(TypePrimitive::Bool));
                        bool_var
                    }
                    _ => lhs,
                };
                return Ok(result);
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
                        let peeled = Self::peel_reference(obj_ty.clone());
                        if matches!(peeled, Ty::Primitive(TypePrimitive::String))
                            || Self::is_collection_with_len(&obj_ty)
                        {
                            let result_var = self.fresh_type_var();
                            self.bind(
                                result_var,
                                TypeTerm::Primitive(TypePrimitive::Int(TypeInt::I64)),
                            );
                            return Ok(result_var);
                        }
                    }
                }
                if select.field.name.as_str() == "contains" {
                    if invoke.args.len() != 1 {
                        self.emit_error("contains expects exactly one argument");
                        return Ok(self.error_type_var());
                    }
                    let _ = self.infer_expr(&mut invoke.args[0])?;
                    if let Ok(obj_ty) = self.resolve_to_ty(obj_var) {
                        let peeled = Self::peel_reference(obj_ty);
                        if !matches!(peeled, Ty::Primitive(TypePrimitive::List))
                            && !Self::is_collection_with_len(&peeled)
                        {
                            self.emit_error("contains expects a list receiver");
                        }
                    }
                    let result_var = self.fresh_type_var();
                    self.bind(result_var, TypeTerm::Primitive(TypePrimitive::Bool));
                    return Ok(result_var);
                }

                if let Ok(obj_ty) = self.resolve_to_ty(obj_var) {
                    let peeled = Self::peel_reference(obj_ty);
                    if matches!(peeled, Ty::Type(_)) {
                        let method = select.field.name.as_str();
                        match method {
                            "has_field" => {
                                if invoke.args.len() != 1 {
                                    self.emit_error("has_field expects exactly one argument");
                                    return Ok(self.error_type_var());
                                }
                                let arg_var = self.infer_expr(&mut invoke.args[0])?;
                                let string_var = self.fresh_type_var();
                                self.bind(string_var, TypeTerm::Primitive(TypePrimitive::String));
                                self.unify(arg_var, string_var)?;
                                let result_var = self.fresh_type_var();
                                self.bind(result_var, TypeTerm::Primitive(TypePrimitive::Bool));
                                return Ok(result_var);
                            }
                            "has_method" => {
                                if invoke.args.len() != 1 {
                                    self.emit_error("has_method expects exactly one argument");
                                    return Ok(self.error_type_var());
                                }
                                let arg_var = self.infer_expr(&mut invoke.args[0])?;
                                let string_var = self.fresh_type_var();
                                self.bind(string_var, TypeTerm::Primitive(TypePrimitive::String));
                                self.unify(arg_var, string_var)?;
                                let result_var = self.fresh_type_var();
                                self.bind(result_var, TypeTerm::Primitive(TypePrimitive::Bool));
                                return Ok(result_var);
                            }
                            "struct_size" => {
                                if !invoke.args.is_empty() {
                                    self.emit_error(format!("{} expects no arguments", method));
                                    return Ok(self.error_type_var());
                                }
                                let result_var = self.fresh_type_var();
                                self.bind(
                                    result_var,
                                    TypeTerm::Primitive(TypePrimitive::Int(TypeInt::I64)),
                                );
                                return Ok(result_var);
                            }
                            "method_count" => {
                                if !invoke.args.is_empty() {
                                    self.emit_error("method_count expects no arguments");
                                    return Ok(self.error_type_var());
                                }
                                let result_var = self.fresh_type_var();
                                self.bind(
                                    result_var,
                                    TypeTerm::Primitive(TypePrimitive::Int(TypeInt::I64)),
                                );
                                return Ok(result_var);
                            }
                            "field_name_at" => {
                                if invoke.args.len() != 1 {
                                    self.emit_error("field_name_at expects exactly one argument");
                                    return Ok(self.error_type_var());
                                }
                                let arg_var = self.infer_expr(&mut invoke.args[0])?;
                                let int_var = self.fresh_type_var();
                                self.bind(
                                    int_var,
                                    TypeTerm::Primitive(TypePrimitive::Int(TypeInt::I64)),
                                );
                                self.unify(arg_var, int_var)?;
                                let result_var = self.fresh_type_var();
                                self.bind(result_var, TypeTerm::Primitive(TypePrimitive::String));
                                return Ok(result_var);
                            }
                            "field_type" => {
                                if invoke.args.len() != 1 {
                                    self.emit_error("field_type expects exactly one argument");
                                    return Ok(self.error_type_var());
                                }
                                let arg_var = self.infer_expr(&mut invoke.args[0])?;
                                let string_var = self.fresh_type_var();
                                self.bind(string_var, TypeTerm::Primitive(TypePrimitive::String));
                                self.unify(arg_var, string_var)?;
                                let result_var = self.fresh_type_var();
                                self.bind(
                                    result_var,
                                    TypeTerm::Custom(Ty::Type(TypeType::new(Span::null()))),
                                );
                                return Ok(result_var);
                            }
                            "fields" => {
                                if !invoke.args.is_empty() {
                                    self.emit_error("fields expects no arguments");
                                    return Ok(self.error_type_var());
                                }
                                return self.type_fields_list_var();
                            }
                            "type_name" => {
                                if !invoke.args.is_empty() {
                                    self.emit_error("type_name expects no arguments");
                                    return Ok(self.error_type_var());
                                }
                                let result_var = self.fresh_type_var();
                                self.bind(result_var, TypeTerm::Primitive(TypePrimitive::String));
                                return Ok(result_var);
                            }
                            _ => {}
                        }
                    }
                }

                if let Some(field_var) =
                    self.try_infer_field_function_call(obj_var, &select.field)?
                {
                    field_var
                } else {
                    self.lookup_struct_method(obj_var, &select.field)?
                }
            }
        };

        if let ExprInvokeTarget::Function(locator) = &invoke.target {
            if let Some(sig) = self.lookup_function_signature(locator) {
                if let Ok(fn_ty) = self.ty_from_function_signature(&sig) {
                    if let Ok(sig_var) = self.type_from_ast_ty(&fn_ty) {
                        let _ = self.unify(func_var, sig_var);
                    }
                }
            }
        }

        let func_info = self.ensure_function(func_var, invoke.args.len())?;
        for (arg_expr, param_var) in invoke.args.iter_mut().zip(func_info.params.iter()) {
            let arg_var = self.infer_expr(arg_expr)?;
            self.unify(*param_var, arg_var)?;
        }
        Ok(func_info.ret)
    }

    fn apply_kwargs_to_invoke(&mut self, invoke: &mut ExprInvoke, sig: &FunctionSignature) -> bool {
        if invoke.kwargs.is_empty() {
            return true;
        }

        let mut slots: Vec<Option<Expr>> = vec![None; sig.params.len()];
        for (idx, arg) in invoke.args.drain(..).enumerate() {
            if idx >= sig.params.len() {
                self.emit_error(format!(
                    "function expects {} arguments, found {}",
                    sig.params.len(),
                    idx + 1
                ));
                return false;
            }
            slots[idx] = Some(arg);
        }

        for kwarg in invoke.kwargs.drain(..) {
            let pos = sig
                .params
                .iter()
                .position(|param| param.name.as_str() == kwarg.name.as_str());
            let Some(index) = pos else {
                self.emit_error(format!("unknown keyword argument '{}'", kwarg.name));
                return false;
            };
            if slots[index].is_some() {
                self.emit_error(format!("duplicate keyword argument '{}'", kwarg.name));
                return false;
            }
            slots[index] = Some(kwarg.value);
        }

        for (idx, slot) in slots.iter_mut().enumerate() {
            if slot.is_some() {
                continue;
            }
            if let Some(default) = sig.params[idx].default.as_ref() {
                *slot = Some(Expr::value(default.clone()));
                continue;
            }
            self.emit_error(format!(
                "missing argument '{}' at position {}",
                sig.params[idx].name.as_str(),
                idx
            ));
            return false;
        }

        invoke.args = slots.into_iter().map(|slot| slot.unwrap()).collect();
        true
    }

    fn try_infer_field_function_call(
        &mut self,
        obj_var: TypeVarId,
        field: &Ident,
    ) -> Result<Option<TypeVarId>> {
        let ty = self.resolve_to_ty(obj_var)?;
        let resolved_ty = Self::peel_reference(ty);
        let field_ty = match resolved_ty {
            Ty::Struct(struct_ty) => struct_ty
                .fields
                .iter()
                .find(|f| f.name == *field)
                .map(|f| f.value.clone()),
            Ty::Structural(structural) => structural
                .fields
                .iter()
                .find(|f| f.name == *field)
                .map(|f| f.value.clone()),
            _ => None,
        };
        let Some(field_ty) = field_ty else {
            return Ok(None);
        };
        if !matches!(field_ty, Ty::Function(_)) {
            return Ok(None);
        }
        let field_var = self.type_from_ast_ty(&field_ty)?;
        Ok(Some(field_var))
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
        let key_var = self.fresh_type_var();
        let value_var = self.fresh_type_var();
        let map_var = self.fresh_type_var();
        let map_ty = self.make_hashmap_struct();
        self.bind(map_var, TypeTerm::Struct(map_ty));
        self.record_hashmap_args(map_var, key_var, value_var);
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
        let key_var = self.fresh_type_var();
        let value_var = self.fresh_type_var();
        let map_var = self.fresh_type_var();
        let map_ty = self.make_hashmap_struct();
        self.bind(map_var, TypeTerm::Struct(map_ty));
        self.record_hashmap_args(map_var, key_var, value_var);
        Ok(map_var)
    }

    fn infer_hashmap_from(&mut self, invoke: &mut ExprInvoke) -> Result<TypeVarId> {
        let mut key_var = self.fresh_type_var();
        let mut value_var = self.fresh_type_var();
        if invoke.args.len() != 1 {
            for arg in &mut invoke.args {
                let _ = self.infer_expr(arg);
            }
            self.emit_error("HashMap::from expects a single iterable argument");
        } else {
            let arg = &mut invoke.args[0];
            if let ExprKind::Array(entries) = arg.kind_mut() {
                for (idx, entry) in entries.values.iter_mut().enumerate() {
                    if let ExprKind::Struct(struct_expr) = entry.kind_mut() {
                        let mut key_expr = None;
                        let mut value_expr = None;
                        for field in struct_expr.fields.iter_mut() {
                            match field.name.as_str() {
                                "key" => key_expr = field.value.as_mut(),
                                "value" => value_expr = field.value.as_mut(),
                                _ => {}
                            }
                        }
                        if let (Some(key_expr), Some(value_expr)) = (key_expr, value_expr) {
                            let key = self.infer_expr(key_expr)?;
                            let value = self.infer_expr(value_expr)?;
                            if idx == 0 {
                                key_var = key;
                                value_var = value;
                            } else {
                                let _ = self.unify(key_var, key);
                                let _ = self.unify(value_var, value);
                            }
                            continue;
                        }
                    }
                    if let ExprKind::Tuple(tuple_expr) = entry.kind_mut() {
                        if tuple_expr.values.len() == 2 {
                            let key = self.infer_expr(&mut tuple_expr.values[0])?;
                            let value = self.infer_expr(&mut tuple_expr.values[1])?;
                            if idx == 0 {
                                key_var = key;
                                value_var = value;
                            } else {
                                let _ = self.unify(key_var, key);
                                let _ = self.unify(value_var, value);
                            }
                            continue;
                        }
                    }
                    if let ExprKind::Array(array_expr) = entry.kind_mut() {
                        if array_expr.values.len() == 2 {
                            let key = self.infer_expr(&mut array_expr.values[0])?;
                            let value = self.infer_expr(&mut array_expr.values[1])?;
                            if idx == 0 {
                                key_var = key;
                                value_var = value;
                            } else {
                                let _ = self.unify(key_var, key);
                                let _ = self.unify(value_var, value);
                            }
                            continue;
                        }
                    }
                    let _ = self.infer_expr(entry)?;
                    self.emit_error("HashMap::from expects HashMapEntry { key, value } entries");
                }
            } else {
                let _ = self.infer_expr(arg)?;
                self.emit_error("HashMap::from expects an array literal of entries");
            }
        }
        let map_var = self.fresh_type_var();
        let map_ty = self.make_hashmap_struct();
        self.bind(map_var, TypeTerm::Struct(map_ty));
        self.record_hashmap_args(map_var, key_var, value_var);
        Ok(map_var)
    }

    pub(crate) fn make_hashmap_struct(&self) -> TypeStruct {
        let key = QualifiedPath::new(vec!["HashMap".to_string()]);
        if let Some(existing) = self.struct_defs.get(&key) {
            return existing.clone();
        }
        TypeStruct {
            name: Ident::new("HashMap"),
            generics_params: Vec::new(),
            fields: Vec::new(),
        }
    }

    fn locator_matches_suffix(locator: &Name, suffix: &[&str]) -> bool {
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

    fn locator_segments(locator: &Name) -> Vec<String> {
        match locator {
            Name::Ident(ident) => vec![ident.as_str().to_string()],
            Name::Path(path) => path
                .segments
                .iter()
                .map(|s| s.as_str().to_string())
                .collect(),
            Name::ParameterPath(path) => path
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
            self.fresh_type_var()
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
                    self.fresh_type_var()
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
                let map_ty = self.make_hashmap_struct();
                self.bind(map_var, TypeTerm::Struct(map_ty));
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
                let len_expr = Expr::value(Value::int(len)).into();
                self.bind(var, TypeTerm::Array(elem_var, Some(len_expr)));
            }
            Value::Char(_) => self.bind(var, TypeTerm::Primitive(TypePrimitive::Char)),
            Value::Unit(_) => self.bind(var, TypeTerm::Unit),
            Value::Null(_) | Value::None(_) => self.bind(var, TypeTerm::Nothing),
            Value::Struct(struct_val) => {
                self.bind(var, TypeTerm::Struct(struct_val.ty.clone()));
            }
            Value::Structural(structural) => {
                let mut fields = Vec::with_capacity(structural.fields.len());
                for field in &structural.fields {
                    let field_var = self.infer_value(&field.value)?;
                    let field_ty = self.resolve_to_ty(field_var)?;
                    fields.push(StructuralField::new(field.name.clone(), field_ty));
                }
                self.bind(var, TypeTerm::Structural(TypeStructural { fields }));
            }
            Value::Tuple(tuple) => {
                let mut vars = Vec::new();
                for elem in &tuple.values {
                    vars.push(self.infer_value(elem)?);
                }
                self.bind(var, TypeTerm::Tuple(vars));
            }
            Value::Map(map) => {
                for entry in &map.entries {
                    let _ = self.infer_value(&entry.key)?;
                    let _ = self.infer_value(&entry.value)?;
                }
                let map_ty = self.make_hashmap_struct();
                self.bind(var, TypeTerm::Struct(map_ty));
            }
            Value::Function(func) => {
                let fn_ty = self.ty_from_function_signature(&func.sig)?;
                let fn_var = self.type_from_ast_ty(&fn_ty)?;
                self.unify(var, fn_var)?;
            }
            Value::Type(_) => {
                let type_var = self.type_from_ast_ty(&Ty::Type(TypeType::new(Span::null())))?;
                self.unify(var, type_var)?;
            }
            Value::QuoteToken(token) => {
                let quote_ty = match token.kind {
                    QuoteFragmentKind::Item => match &token.value {
                        QuoteTokenValue::Items(items) if items.len() == 1 => {
                            quote_item_type_from_item(&items[0])
                                .unwrap_or_else(|| quote_ty_from_fragment(token.kind, None))
                        }
                        QuoteTokenValue::Items(items) if items.len() > 1 => {
                            let item_refs: Vec<&Item> = items.iter().collect();
                            let elem_ty =
                                quote_item_type_from_items(&item_refs).unwrap_or_else(|| {
                                    self.emit_error(
                                        "quote<item> contains multiple item kinds; using item type",
                                    );
                                    Ty::Quote(TypeQuote {
                                        span: Span::null(),
                                        kind: QuoteFragmentKind::Item,
                                        item: None,
                                        inner: None,
                                    })
                                });
                            Ty::Slice(TypeSlice {
                                elem: Box::new(elem_ty),
                            })
                        }
                        _ => quote_ty_from_fragment(token.kind, None),
                    },
                    _ => quote_ty_from_fragment(token.kind, None),
                };
                let quote_var = self.type_from_ast_ty(&quote_ty)?;
                self.unify(var, quote_var)?;
            }
            Value::TokenStream(_) => {
                let ts_var = self.type_from_ast_ty(&Ty::TokenStream(TypeTokenStream))?;
                self.unify(var, ts_var)?;
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
            PatternKind::Bind(bind) => {
                let inner_info = self.infer_pattern(bind.pattern.as_mut())?;
                let var = inner_info.var;
                self.insert_env(bind.ident.ident.as_str().to_string(), EnvEntry::Mono(var));
                let mut info = inner_info;
                info.bindings.push(PatternBinding {
                    name: bind.ident.ident.as_str().to_string(),
                    var,
                });
                info
            }
            PatternKind::Type(inner) => {
                let inner_info = self.infer_pattern(inner.pat.as_mut())?;
                let annot_var = self.type_from_ast_ty(&inner.ty)?;
                self.unify(inner_info.var, annot_var)?;
                inner_info
            }
            PatternKind::Quote(quote) => {
                let quote_ty = match quote.item {
                    Some(item) => Ty::Quote(TypeQuote {
                        span: Span::null(),
                        kind: QuoteFragmentKind::Item,
                        item: Some(item),
                        inner: None,
                    }),
                    _ => quote_ty_from_fragment(quote.fragment, None),
                };
                let var = self.type_from_ast_ty(&quote_ty)?;
                PatternInfo::new(var)
            }
            PatternKind::QuotePlural(quote) => {
                let quote_ty = quote_ty_from_fragment(quote.fragment, None);
                let elem_var = self.type_from_ast_ty(&quote_ty)?;
                let list_var = self.fresh_type_var();
                self.bind(list_var, TypeTerm::Vec(elem_var));
                PatternInfo::new(list_var)
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
                let struct_name = self
                    .qualified_name(struct_pat.name.as_str())
                    .unwrap_or_else(|| {
                        QualifiedPath::new(vec![struct_pat.name.as_str().to_string()])
                    });
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
                    self.emit_error(format!(
                        "unknown struct {} in pattern",
                        struct_name.to_key()
                    ));
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
                if let Name::Path(path) = locator {
                    if path.segments.len() >= 2 {
                        let variant_name = path.segments[path.segments.len() - 1].as_str();
                        let enum_segments = path
                            .segments
                            .iter()
                            .take(path.segments.len() - 1)
                            .map(|seg| seg.as_str().to_string())
                            .collect::<Vec<_>>();
                        if let Some(enum_key) =
                            self.resolve_segments_key(path.prefix, &enum_segments)
                        {
                            if let Some(enum_def) = self.enum_defs.get(&enum_key).cloned() {
                                if let Some(variant) = enum_def
                                    .variants
                                    .iter()
                                    .find(|v| v.name.as_str() == variant_name)
                                {
                                    if let Ty::Tuple(tuple_ty) = &variant.value {
                                        for (idx, expected_ty) in tuple_ty
                                            .types
                                            .iter()
                                            .enumerate()
                                            .take(element_vars.len())
                                        {
                                            let expected_var =
                                                self.type_from_ast_ty(expected_ty)?;
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
                    ExprKind::Name(locator) => {
                        if let Name::Path(path) = locator {
                            if path.segments.len() >= 2 {
                                let variant_name = path.segments[path.segments.len() - 1].as_str();
                                let enum_segments = path
                                    .segments
                                    .iter()
                                    .take(path.segments.len() - 1)
                                    .map(|seg| seg.as_str().to_string())
                                    .collect::<Vec<_>>();
                                if let Some(enum_key) =
                                    self.resolve_segments_key(path.prefix, &enum_segments)
                                {
                                    if let Some(enum_def) = self.enum_defs.get(&enum_key).cloned() {
                                        let enum_var = self.fresh_type_var();
                                        self.bind(enum_var, TypeTerm::Enum(enum_def.clone()));

                                        if let Some(inner) = variant.pattern.as_mut() {
                                            if let Some(def_variant) = enum_def
                                                .variants
                                                .iter()
                                                .find(|v| v.name.as_str() == variant_name)
                                            {
                                                // Struct-like enum variant patterns: `Enum::Variant { ... }`.
                                                if let (
                                                    Ty::Structural(structural),
                                                    PatternKind::Structural(pat),
                                                ) = (&def_variant.value, inner.kind_mut())
                                                {
                                                    let mut bindings = Vec::new();
                                                    for field in &mut pat.fields {
                                                        if let Some(expected_field) = structural
                                                            .fields
                                                            .iter()
                                                            .find(|f| f.name == field.name)
                                                        {
                                                            let expected_var = self
                                                                .type_from_ast_ty(
                                                                    &expected_field.value,
                                                                )?;
                                                            if let Some(rename) =
                                                                field.rename.as_mut()
                                                            {
                                                                let child =
                                                                    self.infer_pattern(rename)?;
                                                                bindings.extend(child.bindings);
                                                                self.unify(
                                                                    child.var,
                                                                    expected_var,
                                                                )?;
                                                            } else {
                                                                let var = self.fresh_type_var();
                                                                self.insert_env(
                                                                    field.name.as_str().to_string(),
                                                                    EnvEntry::Mono(var),
                                                                );
                                                                self.unify(var, expected_var)?;
                                                                bindings.push(PatternBinding {
                                                                    name: field
                                                                        .name
                                                                        .as_str()
                                                                        .to_string(),
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
                        }
                        // Struct patterns are lowered as `PatternKind::Variant` with a
                        // single-segment path and structural payload. Bind fields against
                        // known struct definitions so identifiers enter the environment.
                        let resolved = self.resolve_locator_key(locator);
                        let struct_name = resolved.or_else(|| match locator {
                            Name::Path(path)
                                if path.prefix == PathPrefix::Plain && path.segments.len() == 1 =>
                            {
                                Some(QualifiedPath::new(vec![path.segments[0]
                                    .as_str()
                                    .to_string()]))
                            }
                            Name::Ident(ident) => {
                                Some(QualifiedPath::new(vec![ident.as_str().to_string()]))
                            }
                            _ => None,
                        });
                        if let Some(struct_name) = struct_name {
                            if let Some(struct_def) = self.struct_defs.get(&struct_name).cloned() {
                                let struct_var = self.fresh_type_var();
                                self.bind(struct_var, TypeTerm::Struct(struct_def.clone()));

                                if let Some(inner) = variant.pattern.as_mut() {
                                    if let PatternKind::Structural(pat) = inner.kind_mut() {
                                        let mut bindings = Vec::new();
                                        for field in &mut pat.fields {
                                            if let Some(def_field) = struct_def
                                                .fields
                                                .iter()
                                                .find(|f| f.name == field.name)
                                            {
                                                let expected =
                                                    self.type_from_ast_ty(&def_field.value)?;
                                                if let Some(rename) = field.rename.as_mut() {
                                                    let child = self.infer_pattern(rename)?;
                                                    bindings.extend(child.bindings);
                                                    self.unify(child.var, expected)?;
                                                } else {
                                                    let var = self.fresh_type_var();
                                                    self.insert_env(
                                                        field.name.as_str().to_string(),
                                                        EnvEntry::Mono(var),
                                                    );
                                                    self.unify(var, expected)?;
                                                    bindings.push(PatternBinding {
                                                        name: field.name.as_str().to_string(),
                                                        var,
                                                    });
                                                }
                                            }
                                        }

                                        return Ok(PatternInfo {
                                            var: struct_var,
                                            bindings,
                                        });
                                    }
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
        let struct_path = match resolved_ty {
            Ty::Struct(struct_ty) => QualifiedPath::new(vec![struct_ty.name.as_str().to_string()]),
            Ty::Enum(enum_ty) => QualifiedPath::new(vec![enum_ty.name.as_str().to_string()]),
            other => {
                if let Some(var) = self.lookup_trait_method_for_receiver(obj_var, field)? {
                    return Ok(var);
                }
                if matches!(other, Ty::Any(_) | Ty::Unknown(_)) {
                    if let Some(var) = self.lookup_unique_trait_method(field)? {
                        return Ok(var);
                    }
                    if self.lossy_mode {
                        let var = self.fresh_type_var();
                        self.bind(var, TypeTerm::Any);
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
        let candidates = self.struct_name_variants_for_path(&struct_path, true);
        let mut struct_key = None;
        let mut record = None;
        for candidate in &candidates {
            if let Some(methods) = self.struct_methods.get(candidate) {
                if let Some(found) = methods.get(field.as_str()) {
                    struct_key = Some(candidate.clone());
                    record = Some(found.clone());
                    break;
                }
            }
        }
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
        }

        if let Some(struct_key) = struct_key.as_ref() {
            let qualified = struct_key.with_segment(field.as_str().to_string());
            if let Some(var) = self.lookup_env_var(&qualified.to_key()) {
                return Ok(var);
            }
        }
        for candidate in candidates {
            let qualified = candidate.with_segment(field.as_str().to_string());
            if let Some(var) = self.lookup_env_var(&qualified.to_key()) {
                return Ok(var);
            }
        }
        if let Some(var) = self.lookup_env_var(field.as_str()) {
            return Ok(var);
        }
        self.emit_error(format!(
            "unknown method {} on struct {}",
            field,
            struct_path.to_key()
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
        match field.name.as_str() {
            "push" if arg_len == 1 => {
                let obj_ty = match self.resolve_to_ty(obj_var) {
                    Ok(ty) => Self::peel_reference(ty),
                    Err(_) => return Ok(None),
                };
                if matches!(obj_ty, Ty::Vec(_)) {
                    let unit_var = self.fresh_type_var();
                    self.bind(unit_var, TypeTerm::Unit);
                    Ok(Some(unit_var))
                } else {
                    Ok(None)
                }
            }
            "to_string" => {
                if arg_len != 0 {
                    return Ok(None);
                }
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
                if arg_len != 0 {
                    return Ok(None);
                }
                let obj_ty = match self.resolve_to_ty(obj_var) {
                    Ok(ty) => Self::peel_reference(ty),
                    Err(_) => {
                        let result_var = self.fresh_type_var();
                        self.bind(result_var, TypeTerm::Any);
                        return Ok(Some(result_var));
                    }
                };

                let elem_ty = match obj_ty {
                    Ty::Vec(vec) => Some(*vec.ty.clone()),
                    Ty::Array(array) => Some(*array.elem.clone()),
                    Ty::Slice(slice) => Some(*slice.elem.clone()),
                    _ => None,
                };

                let elem_ty = match elem_ty {
                    Some(ty) => ty,
                    None => {
                        let result_var = self.fresh_type_var();
                        self.bind(result_var, TypeTerm::Any);
                        return Ok(Some(result_var));
                    }
                };

                let iter_elem_var = if field.name.as_str() == "enumerate" {
                    let index_var = self.fresh_type_var();
                    self.bind(
                        index_var,
                        TypeTerm::Primitive(TypePrimitive::Int(TypeInt::I64)),
                    );
                    let value_var = self.type_from_ast_ty(&elem_ty)?;
                    let tuple_var = self.fresh_type_var();
                    self.bind(tuple_var, TypeTerm::Tuple(vec![index_var, value_var]));
                    tuple_var
                } else {
                    self.type_from_ast_ty(&elem_ty)?
                };

                let result_var = self.fresh_type_var();
                self.bind(result_var, TypeTerm::Vec(iter_elem_var));
                Ok(Some(result_var))
            }
            _ => Ok(None),
        }
    }

    fn iter_element_var_from_ty(&mut self, ty: &Ty) -> Option<TypeVarId> {
        match ty {
            Ty::Vec(vec) => self.type_from_ast_ty(&vec.ty).ok(),
            Ty::Array(array) => self.type_from_ast_ty(&array.elem).ok(),
            Ty::Slice(slice) => self.type_from_ast_ty(&slice.elem).ok(),
            _ => None,
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
            Ty::Quote(ref quote) if quote.kind == QuoteFragmentKind::Item => match field
                .name
                .as_str()
            {
                "name" => {
                    let var = self.fresh_type_var();
                    self.bind(var, TypeTerm::Primitive(TypePrimitive::String));
                    Ok(var)
                }
                "value" | "fn" => {
                    if matches!(quote.item, Some(QuoteItemKind::Function) | None) {
                        let fn_ty = Ty::Function(TypeFunction {
                            params: Vec::new(),
                            generics_params: Vec::new(),
                            ret_ty: Some(Box::new(Ty::Unit(TypeUnit))),
                        });
                        let var = self.type_from_ast_ty(&fn_ty)?;
                        Ok(var)
                    } else {
                        self.emit_error(format!("field {} requires a quoted function item", field));
                        Ok(self.error_type_var())
                    }
                }
                _ => {
                    self.emit_error(format!(
                        "cannot access field {} on value of type {}",
                        field, resolved_ty
                    ));
                    Ok(self.error_type_var())
                }
            },
            Ty::Type(_) if field.name.as_str() == "fields" => self.type_fields_list_var(),
            Ty::Type(_) if field.name.as_str() == "name" => {
                let result_var = self.fresh_type_var();
                self.bind(result_var, TypeTerm::Primitive(TypePrimitive::String));
                Ok(result_var)
            }
            Ty::Type(_) if field.name.as_str() == "methods" => {
                let result_var = self.fresh_type_var();
                let elem_var = self.fresh_type_var();
                self.bind(elem_var, TypeTerm::Primitive(TypePrimitive::String));
                self.bind(result_var, TypeTerm::Vec(elem_var));
                Ok(result_var)
            }
            Ty::Type(_) if field.name.as_str() == "size" => {
                let result_var = self.fresh_type_var();
                self.bind(
                    result_var,
                    TypeTerm::Primitive(TypePrimitive::Int(TypeInt::I64)),
                );
                Ok(result_var)
            }
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
                if self.lossy_mode && matches!(other, Ty::Any(_) | Ty::Unknown(_)) {
                    let var = self.fresh_type_var();
                    self.bind(var, TypeTerm::Any);
                    Ok(var)
                } else {
                    self.emit_error(format!(
                        "cannot access field {} on value of type {}",
                        field, other
                    ));
                    Ok(self.error_type_var())
                }
            }
        }
    }

    pub(crate) fn resolve_struct_literal(
        &mut self,
        struct_expr: &mut ExprStruct,
    ) -> Result<TypeVarId> {
        let resolved_name = match struct_expr.name.kind() {
            ExprKind::Name(locator) => self.resolve_locator_key(locator),
            _ => None,
        };
        let struct_name = match resolved_name
            .clone()
            .or_else(|| self.struct_name_from_expr(&struct_expr.name))
        {
            Some(name) => name,
            None => {
                self.emit_error("struct literal target could not be resolved");
                return Ok(self.error_type_var());
            }
        };
        if let ExprKind::Name(locator) = struct_expr.name.kind() {
            if self.check_unimplemented_locator(locator) {
                return Ok(self.error_type_var());
            }
        } else if let Some(tail) = struct_name.tail() {
            if self.check_unimplemented_locator(&Name::Ident(Ident::new(tail.to_string()))) {
                return Ok(self.error_type_var());
            }
        }
        let mut candidates = Vec::new();
        if let Some(resolved) = resolved_name {
            candidates.push(resolved);
        }
        for candidate in
            self.struct_name_variants_for_path(&struct_name, struct_name.segments.len() == 1)
        {
            if !candidates.contains(&candidate) {
                candidates.push(candidate);
            }
        }
        if let Some(def) = candidates
            .iter()
            .find_map(|name| self.struct_defs.get(name).cloned())
            .or_else(|| {
                self.lookup_struct_def_by_name(&struct_name.to_key())
                    .map(|(_, def)| def)
            })
        {
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
            if let ExprKind::Name(Name::Path(path)) = struct_expr.name.kind() {
                if path.segments.len() >= 2 {
                    let variant_name = path.segments[path.segments.len() - 1].as_str();
                    let enum_segments = path
                        .segments
                        .iter()
                        .take(path.segments.len() - 1)
                        .map(|seg| seg.as_str().to_string())
                        .collect::<Vec<_>>();
                    if let Some(enum_key) = self.resolve_segments_key(path.prefix, &enum_segments) {
                        if let Some(enum_def) = self.enum_defs.get(&enum_key).cloned() {
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
                                                let expected =
                                                    self.type_from_ast_ty(&def_field.value)?;
                                                self.unify(value_var, expected)?;
                                            } else {
                                                self.emit_error(format!(
                                                    "unknown field {} on enum variant {}::{}",
                                                    field.name,
                                                    enum_key.to_key(),
                                                    variant_name
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
            }

            self.emit_error(format!(
                "unknown struct literal target: {}",
                struct_name.to_key()
            ));
            Ok(self.error_type_var())
        }
    }

    fn type_fields_list_var(&mut self) -> Result<TypeVarId> {
        let result_var = self.fresh_type_var();
        let string_ref = Ty::Reference(TypeReference {
            ty: Box::new(Ty::Primitive(TypePrimitive::String)),
            mutability: None,
            lifetime: None,
        });
        let fields = vec![
            StructuralField::new(Ident::new("name".to_string()), string_ref),
            StructuralField::new(
                Ident::new("ty".to_string()),
                Ty::Type(TypeType::new(Span::null())),
            ),
        ];
        let struct_ty = TypeStructural { fields };
        let elem_var = self.fresh_type_var();
        self.bind(elem_var, TypeTerm::Structural(struct_ty));
        self.bind(result_var, TypeTerm::Vec(elem_var));
        Ok(result_var)
    }

    fn resolve_struct_literal_as_enum_variant(
        &mut self,
        struct_expr: &mut ExprStruct,
        enum_ty: &TypeEnum,
    ) -> Result<Option<TypeVarId>> {
        let struct_name = match self.struct_name_from_expr(&struct_expr.name) {
            Some(name) => name,
            None => return Ok(None),
        };

        let variant = enum_ty
            .variants
            .iter()
            .find(|variant| struct_name.tail() == Some(variant.name.as_str()));
        let Some(variant) = variant else {
            return Ok(None);
        };

        let structural = match &variant.value {
            Ty::Structural(structural) => Some(structural.fields.clone()),
            Ty::Struct(struct_ty) => Some(struct_ty.fields.clone()),
            Ty::Expr(expr) => match expr.kind() {
                ExprKind::Name(locator) => self
                    .resolve_locator_key(locator)
                    .as_ref()
                    .and_then(|key| self.struct_defs.get(key).cloned())
                    .or_else(|| {
                        locator.as_ident().and_then(|ident| {
                            self.struct_defs
                                .get(&QualifiedPath::new(vec![ident.as_str().to_string()]))
                                .cloned()
                        })
                    })
                    .map(|struct_ty| struct_ty.fields),
                _ => None,
            },
            _ => None,
        };

        let Some(structural) = structural else {
            return Ok(None);
        };

        for field in &mut struct_expr.fields {
            if let Some(value) = field.value.as_mut() {
                let value_var = self.infer_expr(value)?;
                if let Some(def_field) = structural.iter().find(|f| f.name == field.name) {
                    let expected = self.type_from_ast_ty(&def_field.value)?;
                    self.unify(value_var, expected)?;
                } else {
                    self.emit_error(format!(
                        "unknown field {} on enum variant {}::{}",
                        field.name, enum_ty.name, variant.name
                    ));
                    return Ok(Some(self.error_type_var()));
                }
            }
        }

        let var = self.fresh_type_var();
        self.bind(var, TypeTerm::Enum(enum_ty.clone()));
        Ok(Some(var))
    }
}
