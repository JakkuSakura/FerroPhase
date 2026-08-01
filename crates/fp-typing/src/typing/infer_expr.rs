use crate::runtime_types::bytes_value_is_borrowed_string;
use crate::typing::unify::TypeVarKind;
use crate::{
    std_result_inner_types, AstTypeInferencer, BoxFuture, EnvEntry, GenericMonorph,
    PatternBinding, PatternInfo, TypeVarId,
};
use fp_core::ast::*;
use fp_core::error::Result;
use fp_core::intrinsics::IntrinsicCallKind;
use fp_core::module::path::{PathPrefix, QualifiedPath};
use fp_core::ops::{BinOpKind, UnOpKind};
use fp_core::query::lower_fp_expr_to_query;
use fp_core::span::Span;
use std::collections::{HashMap, HashSet};

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
        ExprKind::SplicePending(pending) => expr_contains_return(pending.token.as_ref()),
        _ => false,
    }
}

impl AstTypeInferencer {
    fn normalize_signature_module_path(&self, module: QualifiedPath) -> QualifiedPath {
        if module.is_empty() {
            return module;
        }
        if self.inner.borrow().module_defs.contains(&module) {
            return module;
        }
        let has_root = module
            .head()
            .map(|head| self.inner.borrow().root_modules.contains(head))
            .unwrap_or(false);
        if has_root {
            return module;
        }
        let root_modules = self.inner.borrow().root_modules.clone();
        for root in &root_modules {
            let mut segments = Vec::with_capacity(module.segments.len() + 1);
            segments.push(root.to_string());
            segments.extend(module.segments.iter().cloned());
            let candidate = QualifiedPath::new(segments);
            if self.inner.borrow().module_defs.contains(&candidate) {
                return candidate;
            }
        }
        module
    }

    fn signature_module_path(&self, locator: &Name, sig_path: &QualifiedPath) -> QualifiedPath {
        let sig_module = sig_path
            .parent_n(1)
            .unwrap_or_else(|| QualifiedPath::new(Vec::new()));
        let sig_module = self.normalize_signature_module_path(sig_module);
        let locator_module = self
            .resolve_locator_key(locator)
            .or_else(|| self.fallback_locator_key(locator))
            .and_then(|path| path.parent_n(1))
            .map(|path| self.normalize_signature_module_path(path));

        let Some(locator_module) = locator_module else {
            return sig_module;
        };

        if sig_module.is_empty() {
            return locator_module;
        }

        let sig_head = sig_path
            .head()
            .map(|head| self.inner.borrow().root_modules.contains(head))
            .unwrap_or(false);
        let locator_head = locator_module
            .head()
            .map(|head| self.inner.borrow().root_modules.contains(head))
            .unwrap_or(false);

        if !sig_head && locator_head {
            return locator_module;
        }

        sig_module
    }

    pub(crate) fn infer_expr_inner<'a>(
        &self,
        expr: &'a mut Expr,
    ) -> BoxFuture<'a, Result<TypeVarId>> {
        let this = self.clone();
        Box::pin(async move {
            let span = expr.span();
            let previous = this.inner.borrow().current_span;
            let active = this.span_or_previous(span, previous);
            this.inner.borrow_mut().current_span = active;
            let result = this.infer_expr_inner_body(expr).await;
            this.inner.borrow_mut().current_span = previous;
            result.map_err(|err| this.error_with_span(err, active))
        })
    }

    /// Split out of `infer_expr_inner` so the span save/restore around it
    /// (which must run even on error) doesn't itself need to live inside a
    /// plain (sync) closure -- a sync closure can't contain `.await`, so this
    /// replaces the old IIFE-closure trick.
    fn infer_expr_inner_body<'a>(
        &self,
        expr: &'a mut Expr,
    ) -> BoxFuture<'a, Result<TypeVarId>> {
        let this = self.clone();
        Box::pin(async move {
            let expr_id = this.expr_id(expr);
            let existing_ty = expr.ty().cloned();
            let var = match expr.kind_mut() {
                ExprKind::Quote(quote) => {
                    let kind = match quote.kind {
                        Some(k) => k,
                        None => infer_quote_kind(&quote.block),
                    };
                    let inner = if matches!(kind, QuoteFragmentKind::Expr) {
                        let block_var = this.infer_block(&mut quote.block).await?;
                        Some(this.resolve_to_ty(block_var).await?)
                    } else {
                        None
                    };
                    let expect_slice = matches!(
                        existing_ty.as_ref(),
                        Some(Ty::Quote(q)) if q.inner.as_deref().map_or(false, |i| matches!(i, Ty::Slice(_)))
                    );
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
                        if items.len() == 1 && quote.block.stmts.len() == 1 && !expect_slice {
                            quote_item_type_from_item(items[0])
                                .unwrap_or_else(|| quote_ty_from_fragment(kind, inner.clone()))
                        } else if quote.block.stmts.len() > 1 || expect_slice {
                            if has_non_items {
                                this.emit_error("quote<item> expects only item statements");
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
                                    this.emit_error(
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
                            Ty::Quote(TypeQuote {
                                span: Span::null(),
                                kind,
                                item: None,
                                inner: Some(Box::new(Ty::Slice(TypeSlice {
                                    elem: Box::new(elem_ty),
                                }))),
                            })
                        } else {
                            if has_non_items {
                                this.emit_error("quote<item> expects only item statements");
                            }
                            quote_ty_from_fragment(kind, inner)
                        }
                    } else {
                        quote_ty_from_fragment(kind, inner)
                    };
                    let var = this.type_from_ast_ty(&ty).await?;
                    expr.set_ty(ty);
                    var
                }
                ExprKind::Splice(splice) => {
                    // Expression-position splice must carry an expr token
                    let token_var = this.infer_expr_inner(splice.token.as_mut()).await?;
                    let token_ty = this.resolve_to_ty(token_var).await?;
                    match token_ty {
                        Ty::Quote(quote) if quote.kind == QuoteFragmentKind::Expr => {
                            if let Some(inner) = quote.inner.clone() {
                                let var = this.fresh_type_var();
                                this.bind(var, (*inner).clone());
                                expr.set_ty(*inner);
                                var
                            } else {
                                this.emit_warning(
                                    "splice expr token lacks inner type; leaving result unresolved",
                                );
                                this.fresh_type_var()
                            }
                        }
                        Ty::Quote(quote) => {
                            this.emit_error(format!(
                                "splice in expression position requires expr token, found {:?}",
                                quote.kind
                            ));
                            this.error_type_var()
                        }
                        _ => {
                            this.emit_error("splice expects a quote token expression");
                            this.error_type_var()
                        }
                    }
                }
                ExprKind::SplicePending(pending) => {
                    let token_var = this.infer_expr_inner(pending.token.as_mut()).await?;
                    let token_ty = this.resolve_to_ty(token_var).await?;
                    match token_ty {
                        Ty::Quote(quote) if quote.kind == QuoteFragmentKind::Expr => {
                            if let Some(inner) = quote.inner.clone() {
                                let var = this.fresh_type_var();
                                this.bind(var, (*inner).clone());
                                expr.set_ty(*inner);
                                var
                            } else {
                                this.emit_warning(
                                    "splice expr token lacks inner type; leaving result unresolved",
                                );
                                this.fresh_type_var()
                            }
                        }
                        Ty::Quote(quote) => {
                            this.emit_error(format!(
                                "splice in expression position requires expr token, found {:?}",
                                quote.kind
                            ));
                            this.error_type_var()
                        }
                        _ => {
                            this.emit_error("splice expects a quote token expression");
                            this.error_type_var()
                        }
                    }
                }
                ExprKind::IntrinsicContainer(collection) => {
                    this.infer_intrinsic_container(collection).await?
                }
                ExprKind::Value(value) => {
                    if let Value::List(list) = value.as_ref() {
                        let hint_ty = if let Some(ty) = existing_ty.as_ref() {
                            match this.type_from_ast_ty(ty).await {
                                Ok(var) => this.resolve_to_ty(var).await.ok(),
                                Err(_) => None,
                            }
                        } else {
                            None
                        };
                        if matches!(hint_ty.as_ref(), Some(Ty::Array(_))) {
                            this.infer_value(value.as_ref()).await?
                        } else if let Some(Ty::Vec(vec_ty)) = hint_ty.as_ref() {
                            let elem_var = this.type_from_ast_ty(&vec_ty.ty).await?;
                            for value in &list.values {
                                let value_var = this.infer_value(value).await?;
                                this.unify(value_var, elem_var).await?;
                            }
                            let vec_var = this.fresh_type_var();
                            this.bind_vec_term(vec_var, elem_var);
                            vec_var
                        } else if let Some(Ty::Slice(slice_ty)) = hint_ty.as_ref() {
                            let elem_var = this.type_from_ast_ty(&slice_ty.elem).await?;
                            for value in &list.values {
                                let value_var = this.infer_value(value).await?;
                                this.unify(value_var, elem_var).await?;
                            }
                            let slice_var = this.fresh_type_var();
                            this.bind_slice_term(slice_var, elem_var);
                            slice_var
                        } else {
                            this.infer_list_value_as_vec(list).await?
                        }
                    } else {
                        this.infer_value(value.as_ref()).await?
                    }
                }
                ExprKind::Name(locator) => {
                    let (var, resolved_name) = this.lookup_locator_with_resolution(locator).await?;
                    if let Some(resolved_name) = resolved_name {
                        this.record_resolved_name(expr_id, resolved_name);
                    }
                    if let Some(ty) = existing_ty.as_ref() {
                        let annot = this.type_from_ast_ty(ty).await?;
                        this.unify(var, annot).await?;
                    }
                    var
                }
                ExprKind::Block(block) => this.infer_block(block).await?,
                ExprKind::If(if_expr) => this.infer_if(if_expr).await?,
                ExprKind::With(expr_with) => this.infer_with(expr_with).await?,
                ExprKind::BinOp(binop) => this.infer_binop(binop).await?,
                ExprKind::UnOp(unop) => this.infer_unop(unop).await?,
                ExprKind::Assign(assign) => {
                    let target = this.infer_expr_inner(assign.target.as_mut()).await?;
                    let value = this.infer_expr_inner(assign.value.as_mut()).await?;
                    this.unify(target, value).await?;
                    this.unit_type_var()
                }
                ExprKind::Cast(cast) => {
                    let _ = this.infer_expr_inner(cast.expr.as_mut()).await?;
                    this.type_from_ast_ty(&cast.ty).await?
                }
                ExprKind::Let(expr_let) => {
                    let value = this.infer_expr_inner(expr_let.expr.as_mut()).await?;
                    let pattern_info = this.infer_pattern(expr_let.pat.as_mut()).await?;
                    this.unify(pattern_info.var, value).await?;
                    this.apply_pattern_generalization(&pattern_info).await?;
                    value
                }
                ExprKind::Invoke(invoke) => this.infer_invoke(invoke).await?,
                ExprKind::Select(select) => {
                    let obj_var = this.infer_expr_inner(select.obj.as_mut()).await?;
                    this.lookup_struct_field(obj_var, &select.field).await?
                }
                ExprKind::Struct(struct_expr) => {
                    if let Some(ty) = existing_ty.as_ref() {
                        if matches!(ty, Ty::Function(_)) {
                            this.type_from_ast_ty(ty).await?
                        } else {
                            let resolved = match ty {
                                Ty::Enum(_) => Some(ty.clone()),
                                _ => match this.type_from_ast_ty(ty).await {
                                    Ok(var) => this.resolve_to_ty(var).await.ok(),
                                    Err(_) => None,
                                },
                            };
                            if let Some(Ty::Enum(enum_ty)) = resolved.as_ref() {
                                if let Some(var) = this
                                    .resolve_struct_literal_as_enum_variant(struct_expr, enum_ty).await?
                                {
                                    var
                                } else {
                                    this.resolve_struct_literal(struct_expr).await?
                                }
                            } else {
                                this.resolve_struct_literal(struct_expr).await?
                            }
                        }
                    } else if let ExprKind::Name(locator) = struct_expr.name.kind() {
                        // Try resolving through the environment first for
                        // locally-defined type aliases (DefType).
                        if let Some(var) = this.lookup_env_name(locator).await? {
                            if let Ok(ty) = this.resolve_to_ty(var).await {
                                if let Ty::Struct(ref struct_def) = ty {
                                    if let Some(struct_var) = this
                                        .resolve_struct_literal_from_def(struct_expr, &struct_def).await?
                                    {
                                        return Ok(struct_var);
                                    }
                                }
                                // Const-block type aliases (Ty::Type) — the struct
                                // will be resolved by comptime eval on retry.
                                // Defer to a placeholder for now.
                                if matches!(ty, Ty::Type(_)) {
                                    let placeholder = this.fresh_type_var();
                                    this.bind(placeholder, Ty::Type(TypeType::new(fp_core::span::Span::null())));
                                    return Ok(placeholder);
                                }
                            }
                        }
                        this.resolve_struct_literal(struct_expr).await?
                    } else {
                        this.resolve_struct_literal(struct_expr).await?
                    }
                }
                ExprKind::Tuple(tuple) => {
                    let mut element_vars = Vec::new();
                    for expr in &mut tuple.values {
                        element_vars.push(this.infer_expr_inner(expr).await?);
                    }
                    let tuple_var = this.fresh_type_var();
                    this.bind_tuple_term(tuple_var, element_vars);
                    tuple_var
                }
                ExprKind::Array(array) => {
                    let mut iter = array.values.iter_mut();
                    let elem_var = if let Some(first) = iter.next() {
                        let first_var = this.infer_expr_inner(first).await?;
                        for value in iter {
                            let next = this.infer_expr_inner(value).await?;
                            this.unify(first_var, next).await?;
                        }
                        first_var
                    } else {
                        this.fresh_type_var()
                    };
                    let array_var = this.fresh_type_var();
                    let len = Expr::value(Value::int(array.values.len() as i64)).into();
                    this.bind_array_term(array_var, elem_var, Some(len));
                    let array_ty = this.resolve_to_ty(array_var).await?;
                    expr.set_ty(array_ty);
                    array_var
                }
                ExprKind::ArrayRepeat(array_repeat) => {
                    let elem_var = this.infer_expr_inner(array_repeat.elem.as_mut()).await?;
                    let array_var = this.fresh_type_var();
                    this.bind_array_term(array_var, elem_var, Some(array_repeat.len.clone()));
                    let array_ty = this.resolve_to_ty(array_var).await?;
                    expr.set_ty(array_ty);
                    array_var
                }
                ExprKind::Paren(paren) => this.infer_expr_inner(paren.expr.as_mut()).await?,
                ExprKind::FormatString(_) => {
                    let var = this.fresh_type_var();
                    this.bind(var, Ty::Primitive(TypePrimitive::String));
                    var
                }
                ExprKind::Match(match_expr) => this.infer_match(match_expr).await?,
                ExprKind::Loop(loop_expr) => this.infer_loop(loop_expr).await?,
                ExprKind::Return(ret) => {
                    if let Some(value) = ret.value.as_mut() {
                        this.infer_expr_inner(value).await?;
                    }
                    // Diverging expression.
                    this.nothing_type_var()
                }
                ExprKind::Break(brk) => {
                    let value_var = if let Some(value) = brk.value.as_mut() {
                        this.infer_expr_inner(value).await?
                    } else {
                        this.unit_type_var()
                    };
                    let loop_var = {
                        let mut inner = this.inner.borrow_mut();
                        if let Some(context) = inner.loop_stack.last_mut() {
                            context.saw_break = true;
                            Some(context.result_var)
                        } else {
                            None
                        }
                    };
                    if let Some(result_var) = loop_var {
                        this.unify(result_var, value_var).await?;
                        result_var
                    } else {
                        this.emit_error("`break` used outside of a loop");
                        this.error_type_var()
                    }
                }
                ExprKind::Continue(_) => {
                    if this.inner.borrow().loop_stack.is_empty() {
                        this.emit_error("`continue` used outside of a loop");
                        this.error_type_var()
                    } else {
                        this.nothing_type_var()
                    }
                }
                ExprKind::ConstBlock(const_block) => {
                    let ctx = this.typing_ctx.clone();
                    let already_resolved =
                        ctx.expr_resolutions.borrow().resolved_value(expr_id).cloned();
                    if let Some(value) = already_resolved {
                        return this.infer_value(&value).await;
                    }
                    // Type the inner expression first (structural inference
                    // alone — it doesn't need the comptime result), *then*
                    // try to resolve its compile-time value: the hook needs
                    // a concretely-typed expression to lower.
                    let _ = this.infer_expr_inner(const_block.expr.as_mut()).await?;
                    let key = format!("__fp_expr_{expr_id}");
                    let value = this.await_comptime(&key, &const_block.expr).await?;
                    return this.infer_value(&value).await;
                }
                ExprKind::For(for_expr) => {
                    let pat_info = this.infer_pattern(for_expr.pat.as_mut()).await?;
                    let iter_var = this.infer_expr_inner(for_expr.iter.as_mut()).await?;
                    if let Ok(iter_ty) = this.resolve_to_ty(iter_var).await {
                        if let Some(elem_var) = this.iter_element_var_from_ty(&iter_ty).await {
                            this.unify(pat_info.var, elem_var).await?;
                        }
                    }
                    // For now, treat `for` as producing unit.
                    let unit_var = this.fresh_type_var();
                    this.bind(unit_var, Ty::Unit(TypeUnit));
                    this.infer_expr_inner(for_expr.body.as_mut()).await?;
                    unit_var
                }
                ExprKind::While(while_expr) => this.infer_while(while_expr).await?,
                ExprKind::Try(try_expr) => {
                    if try_expr.catches.is_empty()
                        && try_expr.elze.is_none()
                        && try_expr.finally.is_none()
                    {
                        return this.infer_try_operator(try_expr).await;
                    }
                    let result_var = this.infer_expr_inner(try_expr.expr.as_mut()).await?;
                    for catch in &mut try_expr.catches {
                        this.enter_scope();
                        if let Some(pat) = catch.pat.as_mut() {
                            let panic_var = this.fresh_type_var();
                            this.bind(panic_var, Ty::Primitive(TypePrimitive::String));
                            let pattern_info = this.infer_pattern(pat.as_mut()).await?;
                            this.unify(pattern_info.var, panic_var).await?;
                            this.apply_pattern_generalization(&pattern_info).await?;
                        }
                        let catch_var = this.infer_expr_inner(catch.body.as_mut()).await?;
                        this.unify(result_var, catch_var).await?;
                        this.exit_scope();
                    }
                    if let Some(elze) = try_expr.elze.as_mut() {
                        let else_var = this.infer_expr_inner(elze.as_mut()).await?;
                        this.unify(result_var, else_var).await?;
                    }
                    if let Some(finally) = try_expr.finally.as_mut() {
                        let _ = this.infer_expr_inner(finally.as_mut()).await?;
                    }
                    result_var
                }
                ExprKind::Reference(reference) => this.infer_reference(reference).await?,
                ExprKind::Dereference(dereference) => this.infer_dereference(dereference).await?,
                ExprKind::Index(index) => this.infer_index(index).await?,
                ExprKind::Closure(closure) => this.infer_closure(closure).await?,
                ExprKind::IntrinsicCall(call) => this.infer_intrinsic(call).await?,
                ExprKind::Range(range) => this.infer_range(range).await?,
                ExprKind::Await(await_expr) => {
                    let base_var = this.infer_expr_inner(await_expr.base.as_mut()).await?;
                    let base_ty = this.resolve_to_ty(base_var).await?;

                    if let Some(inner_ty) = extract_std_task_inner_ty(&base_ty, "Future") {
                        this.type_from_ast_ty(&inner_ty).await?
                    } else {
                        base_var
                    }
                }
                ExprKind::Async(async_expr) => {
                    let inner_var = this.infer_expr_inner(async_expr.expr.as_mut()).await?;
                    let inner_ty = this.resolve_to_ty(inner_var).await?;
                    let future_ty = make_std_task_param_ty("Future", inner_ty);
                    this.type_from_ast_ty(&future_ty).await?
                }
                ExprKind::Splat(splat) => this.infer_splat(splat).await?,
                ExprKind::SplatDict(splat) => this.infer_splat_dict(splat).await?,
                ExprKind::Macro(macro_expr) => {
                    this.emit_error(format!(
                        "macro `{}` was not lowered before type checking",
                        macro_expr.invocation.path
                    ));
                    this.error_type_var()
                }
                ExprKind::Any(_any) => {
                    let any_var = this.fresh_type_var();
                    this.bind(any_var, Ty::Any(TypeAny));
                    any_var
                }
                ExprKind::Item(_) | ExprKind::Closured(_) | ExprKind::Structural(_) => {
                    this.error_type_var()
                }
                ExprKind::Id(expr_id) => {
                    let ctx = this.typing_ctx.clone();
                    let (resolved_value, source_expr) = {
                        let table = ctx.expr_resolutions.borrow();
                        (
                            table.resolved_value(*expr_id).cloned(),
                            table.source_expr(*expr_id).cloned(),
                        )
                    };
                    if let Some(value) = resolved_value {
                        return this.infer_value(&value).await;
                    }
                    if let Some(source_expr) = source_expr {
                        return this.infer_expr_inner(&mut source_expr.clone()).await;
                    }
                    this.emit_error(format!(
                        "missing source expression for expression id {expr_id}"
                    ));
                    this.error_type_var()
                }
            };

            if let Some(existing_ty) = existing_ty {
                if !matches!(existing_ty, Ty::Unknown(_) | Ty::ErrorType(_)) {
                    let existing_var = this.type_from_ast_ty(&existing_ty).await?;
                    this.unify(var, existing_var).await?;
                }
            }

            let ty = this.resolve_to_ty(var).await?;
            expr.set_ty(ty);
            Ok(var)
        })
    }

    pub(crate) async fn infer_binop(&self, binop: &mut ExprBinOp) -> Result<TypeVarId> {
        let lhs = self.infer_expr_inner(binop.lhs.as_mut()).await?;
        let rhs = self.infer_expr_inner(binop.rhs.as_mut()).await?;
        match binop.kind {
            BinOpKind::Add
            | BinOpKind::Sub
            | BinOpKind::Mul
            | BinOpKind::Div
            | BinOpKind::Mod
            | BinOpKind::Shl
            | BinOpKind::Shr => {
                if matches!(binop.kind, BinOpKind::Add) {
                    let lhs_ty = self.resolve_to_ty(lhs).await?;
                    let rhs_ty = self.resolve_to_ty(rhs).await?;
                    let is_string_ref = |ty: &Ty| {
                        matches!(
                            ty,
                            Ty::Reference(reference)
                                if matches!(reference.ty.as_ref(), Ty::Primitive(TypePrimitive::String))
                        )
                    };
                    if is_string_ref(&lhs_ty) && is_string_ref(&rhs_ty) {
                        self.unify(lhs, rhs).await?;
                        return Ok(lhs);
                    }
                }
                self.ensure_numeric(lhs, "binary operand")?;
                self.unify(lhs, rhs).await?;
                Ok(lhs)
            }
            BinOpKind::Eq
            | BinOpKind::Ne
            | BinOpKind::Lt
            | BinOpKind::Le
            | BinOpKind::Gt
            | BinOpKind::Ge => {
                self.unify(lhs, rhs).await?;
                let bool_var = self.fresh_type_var();
                self.bind(bool_var, Ty::Primitive(TypePrimitive::Bool));
                Ok(bool_var)
            }
            BinOpKind::And | BinOpKind::Or => {
                self.ensure_bool(lhs, "logical operand")?;
                self.ensure_bool(rhs, "logical operand")?;
                let bool_var = self.fresh_type_var();
                self.bind(bool_var, Ty::Primitive(TypePrimitive::Bool));
                Ok(bool_var)
            }
            _ => Ok(lhs),
        }
    }

    pub(crate) async fn infer_unop(&self, unop: &mut ExprUnOp) -> Result<TypeVarId> {
        let value_var = self.infer_expr_inner(unop.val.as_mut()).await?;
        match unop.op {
            UnOpKind::Not => {
                self.ensure_bool(value_var, "unary not")?;
                Ok(value_var)
            }
            UnOpKind::Neg => {
                self.ensure_numeric(value_var, "unary negation")?;
                Ok(value_var)
            }
            UnOpKind::Deref => self.expect_reference(value_var, "dereference expression").await,
            UnOpKind::Any(_) => Ok(value_var),
        }
    }

    pub(crate) async fn infer_reference(&self, reference: &mut ExprReference) -> Result<TypeVarId> {
        let inner_var = self.infer_expr_inner(reference.referee.as_mut()).await?;
        let reference_var = self.fresh_type_var();
        self.bind_reference_term(reference_var, inner_var);
        Ok(reference_var)
    }

    pub(crate) async fn infer_dereference(
        &self,
        dereference: &mut ExprDereference,
    ) -> Result<TypeVarId> {
        let target_var = self.infer_expr_inner(dereference.referee.as_mut()).await?;
        self.expect_reference(target_var, "dereference expression").await
    }

    pub(crate) async fn infer_index(&self, index: &mut ExprIndex) -> Result<TypeVarId> {
        let object_var = self.infer_expr_inner(index.obj.as_mut()).await?;
        if matches!(index.index.kind(), ExprKind::Range(_)) {
            if let ExprKind::Range(range) = index.index.kind_mut() {
                let _ = self.infer_range(range).await?;
            }
            return self.infer_slice_index(object_var).await;
        }

        let idx_var = self.infer_expr_inner(index.index.as_mut()).await?;

        if let Some((key_var, value_var)) = self.lookup_hashmap_args(object_var).await {
            self.unify(key_var, idx_var).await?;
            return Ok(value_var);
        }

        if let Ok(obj_ty) = self.resolve_to_ty(object_var).await {
            match Self::peel_reference(obj_ty) {
                Ty::Vec(vec) => return self.type_from_ast_ty(&vec.ty).await,
                Ty::Array(array) => return self.type_from_ast_ty(&array.elem).await,
                Ty::Slice(slice) => return self.type_from_ast_ty(&slice.elem).await,
                Ty::Primitive(TypePrimitive::String) => {
                    self.ensure_integer(idx_var, "string index")?;
                    return self.type_from_ast_ty(&Ty::Primitive(TypePrimitive::String)).await;
                }
                _ => {}
            }
        }

        let idx_ty = self.resolve_to_ty(idx_var).await?;
        let idx_root = self.find(idx_var);
        let idx_root_kind = self.inner.borrow().type_vars[idx_root].kind.clone();
        let idx_bound_reference = match idx_root_kind {
            TypeVarKind::Bound(ty) => self.reference_inner_from_ty(&ty).await.is_some(),
            TypeVarKind::Link(next) => {
                let root = self.find(next);
                let root_kind = self.inner.borrow().type_vars[root].kind.clone();
                match root_kind {
                    TypeVarKind::Bound(ty) => self.reference_inner_from_ty(&ty).await.is_some(),
                    _ => false,
                }
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
        if matches!(self.resolve_to_ty(object_var).await, Ok(Ty::Struct(struct_ty)) if struct_ty.name.as_str() == "HashMap")
        {
            return Ok(self.fresh_type_var());
        }
        if idx_non_integer || idx_is_string_literal {
            let map_var = self.fresh_type_var();
            let map_ty = self.make_hashmap_struct();
            self.bind(map_var, Ty::Struct(map_ty));
            if self.unify(object_var, map_var).await.is_ok() {
                return Ok(self.fresh_type_var());
            }

            let map_var = self.fresh_type_var();
            let map_ty = self.make_hashmap_struct();
            self.bind(map_var, Ty::Struct(map_ty));
            let ref_var = self.fresh_type_var();
            self.bind_reference_term(ref_var, map_var);
            if self.unify(object_var, ref_var).await.is_ok() {
                return Ok(self.fresh_type_var());
            }

            self.emit_error("indexing with a non-integer key requires a HashMap");
            return Ok(self.error_type_var());
        }

        let elem_vec_var = self.fresh_type_var();
        let vec_var = self.fresh_type_var();
        self.bind_vec_term(vec_var, elem_vec_var);
        if self.unify(object_var, vec_var).await.is_ok() {
            self.ensure_integer(idx_var, "index expression")?;
            return Ok(elem_vec_var);
        }

        let elem_slice_var = self.fresh_type_var();
        let slice_var = self.fresh_type_var();
        self.bind_slice_term(slice_var, elem_slice_var);
        if self.unify(object_var, slice_var).await.is_err() {
            let object_ty = self.resolve_to_ty(object_var).await?;
            match object_ty {
                Ty::Array(array_ty) => {
                    self.ensure_integer(idx_var, "index expression")?;
                    let elem_var = self.type_from_ast_ty(&array_ty.elem).await?;
                    return Ok(elem_var);
                }
                Ty::Reference(reference) => {
                    if let Ty::Array(array_ty) = *reference.ty {
                        self.ensure_integer(idx_var, "index expression")?;
                        let elem_var = self.type_from_ast_ty(&array_ty.elem).await?;
                        return Ok(elem_var);
                    }
                }
                Ty::Struct(struct_ty) if struct_ty.name.as_str() == "HashMap" => {
                    return Ok(self.fresh_type_var());
                }
                _ => {}
            }
            self.emit_error("indexing is only supported on string, vector, slice, or array types");
            return Ok(self.error_type_var());
        }
        self.ensure_integer(idx_var, "index expression")?;
        Ok(elem_slice_var)
    }

    async fn infer_slice_index(&self, object_var: TypeVarId) -> Result<TypeVarId> {
        let is_string_like = |ty: &Ty| {
            matches!(ty, Ty::Primitive(TypePrimitive::String))
                || matches!(
                    ty,
                    Ty::Reference(reference)
                        if matches!(reference.ty.as_ref(), Ty::Primitive(TypePrimitive::String))
                )
        };
        if let Ok(obj_ty) = self.resolve_to_ty(object_var).await {
            match Self::peel_reference(obj_ty) {
                Ty::Primitive(TypePrimitive::String) => {
                    return self.type_from_ast_ty(&Ty::Reference(TypeReference {
                        ty: Box::new(Ty::Primitive(TypePrimitive::String)),
                        mutability: None,
                        lifetime: None,
                    })).await;
                }
                Ty::Vec(vec) => {
                    if is_string_like(vec.ty.as_ref()) {
                        return self.type_from_ast_ty(&Ty::Primitive(TypePrimitive::String)).await;
                    }
                    let elem_var = self.type_from_ast_ty(&vec.ty).await?;
                    let slice_var = self.fresh_type_var();
                    self.bind_slice_term(slice_var, elem_var);
                    return Ok(slice_var);
                }
                Ty::Slice(slice) => {
                    if is_string_like(slice.elem.as_ref()) {
                        return self.type_from_ast_ty(&Ty::Primitive(TypePrimitive::String)).await;
                    }
                    let elem_var = self.type_from_ast_ty(&slice.elem).await?;
                    let slice_var = self.fresh_type_var();
                    self.bind_slice_term(slice_var, elem_var);
                    return Ok(slice_var);
                }
                Ty::Array(array) => {
                    if is_string_like(array.elem.as_ref()) {
                        return self.type_from_ast_ty(&Ty::Primitive(TypePrimitive::String)).await;
                    }
                    let elem_var = self.type_from_ast_ty(&array.elem).await?;
                    let slice_var = self.fresh_type_var();
                    self.bind_slice_term(slice_var, elem_var);
                    return Ok(slice_var);
                }
                _ => {}
            }
        }

        let string_var = self.fresh_type_var();
        self.bind(string_var, Ty::Primitive(TypePrimitive::String));
        let vec_string_var = self.fresh_type_var();
        self.bind_vec_term(vec_string_var, string_var);
        if self.unify(object_var, vec_string_var).await.is_ok() {
            return Ok(string_var);
        }
        let ref_string_var = self.fresh_type_var();
        self.bind_reference_term(ref_string_var, string_var);
        let vec_ref_string_var = self.fresh_type_var();
        self.bind_vec_term(vec_ref_string_var, ref_string_var);
        if self.unify(object_var, vec_ref_string_var).await.is_ok() {
            return Ok(string_var);
        }
        let slice_string_var = self.fresh_type_var();
        self.bind_slice_term(slice_string_var, string_var);
        if self.unify(object_var, slice_string_var).await.is_ok() {
            return Ok(string_var);
        }
        let slice_ref_string_var = self.fresh_type_var();
        self.bind_slice_term(slice_ref_string_var, ref_string_var);
        if self.unify(object_var, slice_ref_string_var).await.is_ok() {
            return Ok(string_var);
        }
        let array_string_var = self.fresh_type_var();
        self.bind_array_term(array_string_var, string_var, None);
        if self.unify(object_var, array_string_var).await.is_ok() {
            return Ok(string_var);
        }
        let ref_string_vec = self.fresh_type_var();
        self.bind_reference_term(ref_string_vec, vec_string_var);
        if self.unify(object_var, ref_string_vec).await.is_ok() {
            return Ok(string_var);
        }
        let ref_string_slice = self.fresh_type_var();
        self.bind_reference_term(ref_string_slice, slice_string_var);
        if self.unify(object_var, ref_string_slice).await.is_ok() {
            return Ok(string_var);
        }

        let elem_var = self.fresh_type_var();
        let slice_var = self.fresh_type_var();
        self.bind_slice_term(slice_var, elem_var);

        let vec_var = self.fresh_type_var();
        self.bind_vec_term(vec_var, elem_var);
        if self.unify(object_var, vec_var).await.is_ok() {
            return Ok(slice_var);
        }

        if self.unify(object_var, slice_var).await.is_ok() {
            return Ok(slice_var);
        }

        let string_var = self.fresh_type_var();
        self.bind(string_var, Ty::Primitive(TypePrimitive::String));
        if self.unify(object_var, string_var).await.is_ok() {
            return Ok(string_var);
        }

        if let Ok(obj_ty) = self.resolve_to_ty(object_var).await {
            if let Ty::Reference(reference) = obj_ty {
                match *reference.ty {
                    Ty::Vec(vec) => {
                        if matches!(vec.ty.as_ref(), Ty::Primitive(TypePrimitive::String)) {
                            return self.type_from_ast_ty(&Ty::Primitive(TypePrimitive::String)).await;
                        }
                        let elem_var = self.type_from_ast_ty(&vec.ty).await?;
                        let slice_var = self.fresh_type_var();
                        self.bind_slice_term(slice_var, elem_var);
                        return Ok(slice_var);
                    }
                    Ty::Slice(slice) => {
                        if matches!(slice.elem.as_ref(), Ty::Primitive(TypePrimitive::String)) {
                            return self.type_from_ast_ty(&Ty::Primitive(TypePrimitive::String)).await;
                        }
                        let elem_var = self.type_from_ast_ty(&slice.elem).await?;
                        let slice_var = self.fresh_type_var();
                        self.bind_slice_term(slice_var, elem_var);
                        return Ok(slice_var);
                    }
                    Ty::Array(array) => {
                        if matches!(array.elem.as_ref(), Ty::Primitive(TypePrimitive::String)) {
                            return self.type_from_ast_ty(&Ty::Primitive(TypePrimitive::String)).await;
                        }
                        let elem_var = self.type_from_ast_ty(&array.elem).await?;
                        let slice_var = self.fresh_type_var();
                        self.bind_slice_term(slice_var, elem_var);
                        return Ok(slice_var);
                    }
                    Ty::Primitive(TypePrimitive::String) => {
                        return self.type_from_ast_ty(&Ty::Primitive(TypePrimitive::String)).await;
                    }
                    _ => {}
                }
            }
        }

        self.emit_error("slicing is only supported on string, vector, slice, or array types");
        Ok(self.error_type_var())
    }

    pub(crate) async fn infer_range(&self, range: &mut ExprRange) -> Result<TypeVarId> {
        let element_var = self.fresh_type_var();

        if let Some(start) = range.start.as_mut() {
            let start_var = self.infer_expr_inner(start).await?;
            self.unify(element_var, start_var).await?;
        }

        if let Some(end) = range.end.as_mut() {
            let end_var = self.infer_expr_inner(end).await?;
            self.unify(element_var, end_var).await?;
        }

        if let Some(step) = range.step.as_mut() {
            let step_var = self.infer_expr_inner(step).await?;
            self.ensure_numeric(step_var, "range step")?;
        }

        self.ensure_numeric(element_var, "range bounds")?;

        let range_var = self.fresh_type_var();
        self.bind_vec_term(range_var, element_var);
        Ok(range_var)
    }

    pub(crate) async fn infer_splat(&self, splat: &mut ExprSplat) -> Result<TypeVarId> {
        self.infer_expr_inner(splat.iter.as_mut()).await
    }

    pub(crate) async fn infer_splat_dict(&self, splat: &mut ExprSplatDict) -> Result<TypeVarId> {
        self.infer_expr_inner(splat.dict.as_mut()).await
    }

    pub(crate) async fn infer_intrinsic(&self, call: &mut ExprIntrinsicCall) -> Result<TypeVarId> {
        let mut arg_vars = Vec::new();

        for arg in &mut call.args {
            arg_vars.push(self.infer_expr_inner(arg).await?);
        }
        for kwarg in &mut call.kwargs {
            arg_vars.push(self.infer_expr_inner(&mut kwarg.value).await?);
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
                self.bind(result_var, Ty::Unit(TypeUnit));
            }
            IntrinsicCallKind::Format => {
                self.bind(result_var, Ty::Primitive(TypePrimitive::String));
            }
            IntrinsicCallKind::Slice => {
                let elem_var = self.fresh_type_var();
                self.bind_slice_term(result_var, elem_var);
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
                self.bind(result_var, Ty::Primitive(TypePrimitive::Int(TypeInt::U64)));
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
                self.bind(result_var, Ty::Primitive(TypePrimitive::Bool));
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
                    self.bind_function_term(fn_var, Vec::new(), ret_var);
                    self.unify(arg_var, fn_var).await?;
                }
                self.bind(result_var, Ty::Primitive(TypePrimitive::Bool));
            }
            IntrinsicCallKind::CatchUnwindResult => {
                if arg_vars.len() != 1 {
                    self.emit_error(format!(
                        "intrinsic {:?} expects 1 argument, found {}",
                        call.kind,
                        arg_vars.len()
                    ));
                }
                let value_var = self.fresh_type_var();
                if let Some(&arg_var) = arg_vars.first() {
                    let fn_var = self.fresh_type_var();
                    self.bind_function_term(fn_var, Vec::new(), value_var);
                    self.unify(arg_var, fn_var).await?;
                }
                let ok_var = self.fresh_type_var();
                self.bind(ok_var, Ty::Primitive(TypePrimitive::Bool));
                self.bind_tuple_term(result_var, vec![ok_var, value_var]);
            }
            IntrinsicCallKind::Input => {
                if arg_vars.len() > 1 {
                    self.emit_error(format!(
                        "intrinsic {:?} expects at most 1 argument, found {}",
                        call.kind,
                        arg_vars.len()
                    ));
                }
                self.bind(result_var, Ty::Primitive(TypePrimitive::String));
            }
            IntrinsicCallKind::TypeName => {
                if arg_vars.len() != 1 {
                    self.emit_error(format!(
                        "intrinsic {:?} expects 1 argument, found {}",
                        call.kind,
                        arg_vars.len()
                    ));
                }
                self.bind(result_var, Ty::Primitive(TypePrimitive::String));
            }
            IntrinsicCallKind::TypeOf => {
                if arg_vars.len() != 1 {
                    self.emit_error(format!(
                        "intrinsic {:?} expects 1 argument, found {}",
                        call.kind,
                        arg_vars.len()
                    ));
                }
                self.bind(result_var, Ty::Type(TypeType::new(Span::null())));
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
                self.bind(elem_var, Ty::Structural(struct_ty));
                self.bind_vec_term(result_var, elem_var);
            }
            IntrinsicCallKind::FieldType
            | IntrinsicCallKind::VecType => {
                let expected = match call.kind {
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
                self.bind(result_var, Ty::Type(TypeType::new(Span::null())));
            }
            // create_struct, addfield, clone_struct — intercepted by lang
            // system but typed opaquely here. Struct building happens at
            // comptime via LIR interpreter; typer just binds to a type-type.
            IntrinsicCallKind::CreateStruct
            | IntrinsicCallKind::AddField
            | IntrinsicCallKind::CloneStruct => {
                self.bind(result_var, Ty::Type(TypeType::new(Span::null())));
            }
            IntrinsicCallKind::BuildType => {
                let inner_var = self.fresh_type_var();
                self.bind(result_var, Ty::Type(TypeType {
                    span: Span::null(),
                    inner: Some(Box::new(Ty::InferVar(TypeInferVar { id: inner_var }))),
                }));
            }
            IntrinsicCallKind::GenerateMethod => {
                if arg_vars.len() != 2 {
                    self.emit_error(format!(
                        "intrinsic {:?} expects 2 arguments, found {}",
                        call.kind,
                        arg_vars.len()
                    ));
                }
                self.bind(result_var, Ty::Unit(TypeUnit));
            }
            IntrinsicCallKind::CompileError => {
                if arg_vars.len() != 1 {
                    self.emit_error(format!(
                        "intrinsic {:?} expects 1 argument, found {}",
                        call.kind,
                        arg_vars.len()
                    ));
                }
                self.bind(result_var, Ty::Nothing(TypeNothing));
            }
            IntrinsicCallKind::CompileWarning => {
                if arg_vars.len() != 1 {
                    self.emit_error(format!(
                        "intrinsic {:?} expects 1 argument, found {}",
                        call.kind,
                        arg_vars.len()
                    ));
                }
                self.bind(result_var, Ty::Unit(TypeUnit));
            }
            _ => {
                self.bind_error(result_var);
            }
        }

        Ok(result_var)
    }

    pub(crate) async fn infer_closure(&self, closure: &mut ExprClosure) -> Result<TypeVarId> {
        self.enter_scope();
        let exception_policy =
            self.exception_policy_for_ret(closure.ret_ty.as_ref().map(|ty| ty.as_ref()));
        let _exception_guard = self.push_exception_context(exception_policy);
        let mut param_vars = Vec::new();
        for param in &mut closure.params {
            let info = self.infer_pattern(param).await?;
            param_vars.push(info.var);
        }

        let body_var = self.infer_expr_inner(closure.body.as_mut()).await?;
        let ret_var = if matches!(
            exception_policy,
            super::super::ExceptionReturnPolicy::AutoResult
        ) {
            let body_ty = self.resolve_to_ty(body_var).await?;
            let result_ty = super::super::make_std_result_ty(body_ty, super::super::std_error_ty());
            self.type_from_ast_ty(&result_ty).await?
        } else if let Some(ret_ty) = &closure.ret_ty {
            let annot_var = self.type_from_ast_ty(ret_ty).await?;
            self.unify(body_var, annot_var).await?;
            annot_var
        } else {
            body_var
        };

        self.exit_scope();

        let closure_var = self.fresh_type_var();
        self.bind_function_term(closure_var, param_vars, ret_var);
        Ok(closure_var)
    }

    async fn infer_try_operator(&self, try_expr: &mut ExprTry) -> Result<TypeVarId> {
        let policy = self.current_exception_policy();
        if !matches!(
            policy,
            super::super::ExceptionReturnPolicy::AutoResult
                | super::super::ExceptionReturnPolicy::ExplicitResult
        ) {
            self.emit_error("`?` is only allowed in exception-enabled functions");
            return Ok(self.error_type_var());
        }

        let result_var = self.infer_expr_inner(try_expr.expr.as_mut()).await?;
        let result_ty = self.resolve_to_ty(result_var).await?;
        let Some((ok_ty, _err_ty)) = std_result_inner_types(&result_ty) else {
            self.emit_error("`?` expects a Result value");
            return Ok(self.error_type_var());
        };
        self.type_from_ast_ty(&ok_ty).await
    }

    pub(crate) async fn infer_with(&self, expr_with: &mut ExprWith) -> Result<TypeVarId> {
        let context_var = self.infer_expr_inner(expr_with.context.as_mut()).await?;
        let context_ty = self.resolve_to_ty(context_var).await?;
        self.enter_scope();
        self.push_context_binding(context_ty, expr_with.context.as_ref().clone());
        let result = self.infer_expr_inner(expr_with.body.as_mut()).await;
        self.exit_scope();
        result
    }

    pub(crate) async fn infer_invoke(&self, invoke: &mut ExprInvoke) -> Result<TypeVarId> {
                        if let Some(result) = self.try_infer_query_pipeline_call(invoke) {
            return result;
        }

        if let Some(result) = self.try_infer_collection_call(invoke).await? {
            return Ok(result);
        }

        if !invoke.kwargs.is_empty() && !matches!(invoke.target, ExprInvokeTarget::Function(_)) {
            self.emit_error("keyword arguments are only supported on function calls");
            return Ok(self.error_type_var());
        }

        if let ExprInvokeTarget::Function(locator) = &mut invoke.target {
            if let Some(ident) = locator.as_ident() {
                if ident.as_str() == "printf" {
                    return self.infer_builtin_printf(invoke).await;
                }
                if ident.as_str() == "type" {
                    if invoke.args.len() != 1 {
                        self.emit_error("type() expects exactly one argument");
                        return Ok(self.error_type_var());
                    }
                    let _ = self.infer_expr_inner(&mut invoke.args[0]).await?;
                    let type_var = self.fresh_type_var();
                    self.bind(type_var, Ty::Type(TypeType::new(Span::null())));
                    return Ok(type_var);
                }
            }
        }

        if matches!(invoke.target, ExprInvokeTarget::Function(_)) {
            if let Some((sig_path, sig)) = {
                let locator = match &invoke.target {
                    ExprInvokeTarget::Function(locator) => locator,
                    _ => unreachable!(),
                };
                self.lookup_extern_function_signature_with_path(locator)
            } {
                let sig_module = {
                    let locator = match &invoke.target {
                        ExprInvokeTarget::Function(locator) => locator,
                        _ => unreachable!(),
                    };
                    self.signature_module_path(locator, &sig_path)
                };
                if !self.apply_kwargs_to_invoke(invoke, &sig) {
                    return Ok(self.error_type_var());
                }
                if invoke.args.len() != sig.params.len() {
                    self.emit_error("extern \"C\" call arity mismatch");
                    return Ok(self.error_type_var());
                }
                for (arg_expr, param) in invoke.args.iter_mut().zip(sig.params.iter()) {
                    let arg_var = self.infer_expr_inner(arg_expr).await?;
                    let param_var = self.type_from_ast_ty_in_module(&param.ty, &sig_module).await?;
                    let expects_cstr = self
                        .resolve_to_ty(param_var).await
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
                        if let Ok(arg_ty) = self.resolve_to_ty(arg_var).await {
                            if self.is_string_like_type(&arg_ty) {
                                arg_expr.set_ty(param.ty.clone());
                                continue;
                            }
                        }
                    }
                    self.unify(arg_var, param_var).await?;
                }
                let ret_var = if let Some(ret_ty) = &sig.ret_ty {
                    self.type_from_ast_ty_in_module(ret_ty, &sig_module).await?
                } else {
                    let unit = self.fresh_type_var();
                    self.bind(unit, Ty::Unit(TypeUnit));
                    unit
                };
                return Ok(ret_var);
            }
            let resolved_sig = {
                let locator = match &invoke.target {
                    ExprInvokeTarget::Function(locator) => locator.clone(),
                    _ => unreachable!(),
                };
                match self.lookup_function_signature_with_path(&locator).await {
                    Some(found) => Some(found),
                    None => self.lookup_function_signature(&locator).map(|sig| {
                        let sig_path = self
                            .resolve_locator_key(&locator)
                            .or_else(|| self.fallback_locator_key(&locator))
                            .unwrap_or_else(|| QualifiedPath::new(Vec::new()));
                        (sig_path, sig)
                    }),
                }
            };
            if let Some((sig_path, sig)) = resolved_sig {
                
                let sig_module = {
                    let locator = match &invoke.target {
                        ExprInvokeTarget::Function(locator) => locator,
                        _ => unreachable!(),
                    };
                    self.signature_module_path(locator, &sig_path)
                };
                if !self.apply_kwargs_to_invoke(invoke, &sig) {
                    return Ok(self.error_type_var());
                }
                let generic_args = {
                    let locator = match &invoke.target {
                        ExprInvokeTarget::Function(locator) => locator,
                        _ => unreachable!(),
                    };
                    Self::locator_generic_args(locator).map(|args| args.to_vec())
                };
                if generic_args.is_some() && sig.receiver.is_some() {
                    self.emit_error(
                        "explicit generic arguments are only supported on function calls",
                    );
                    return Ok(self.error_type_var());
                }
                if sig.receiver.is_none() && !sig.generics_params.is_empty() {
                    if let Some(explicit_args) = generic_args.as_ref() {
                        if sig.generics_params.len() != explicit_args.len() {
                            self.emit_error("generic argument count mismatch");
                            return Ok(self.error_type_var());
                        }
                    }
                    return self.infer_generic_function_call(
                        invoke,
                        &sig,
                        &sig_module,
                        generic_args.as_deref(),
                        &sig_path,
                    ).await;
                }
                if sig.abi.is_c() && sig.generics_params.is_empty() && sig.receiver.is_none() {
                    if invoke.args.len() != sig.params.len() {
                        self.emit_error("extern \"C\" call arity mismatch");
                        return Ok(self.error_type_var());
                    }
                    for (arg_expr, param) in invoke.args.iter_mut().zip(sig.params.iter()) {
                        let arg_var = self.infer_expr_inner(arg_expr).await?;
                        let param_var = self.type_from_ast_ty_in_module(&param.ty, &sig_module).await?;
                        let expects_cstr = self
                            .resolve_to_ty(param_var).await
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
                            if let Ok(arg_ty) = self.resolve_to_ty(arg_var).await {
                                if self.is_string_like_type(&arg_ty) {
                                    arg_expr.set_ty(param.ty.clone());
                                    continue;
                                }
                            }
                        }
                        self.unify(arg_var, param_var).await?;
                    }
                    let ret_var = if let Some(ret_ty) = &sig.ret_ty {
                        self.type_from_ast_ty_in_module(ret_ty, &sig_module).await?
                    } else {
                        let unit = self.fresh_type_var();
                        self.bind(unit, Ty::Unit(TypeUnit));
                        unit
                    };
                    return Ok(ret_var);
                }
                if sig.generics_params.is_empty() && sig.receiver.is_none() {
                    if invoke.args.len() != sig.params.len() {
                        self.emit_error("call arity mismatch");
                        return Ok(self.error_type_var());
                    }
                    for (arg_expr, param) in invoke.args.iter_mut().zip(sig.params.iter()) {
                        let arg_var = self.infer_expr_inner(arg_expr).await?;
                        let param_var = self.type_from_ast_ty_in_module(&param.ty, &sig_module).await?;
                        self.unify(arg_var, param_var).await?;
                    }
                    let ret_var = if let Some(ret_ty) = &sig.ret_ty {
                        self.type_from_ast_ty_in_module(ret_ty, &sig_module).await?
                    } else {
                        let unit = self.fresh_type_var();
                        self.bind(unit, Ty::Unit(TypeUnit));
                        unit
                    };
                    return Ok(ret_var);
                }
            }
        }

        let enum_ctor = if let ExprInvokeTarget::Function(locator) = &invoke.target {
            self.enum_variant_from_locator(locator).await
        } else {
            None
        };

        let func_var = match &mut invoke.target {
            ExprInvokeTarget::Function(locator) => {
                if let Some(ident) = locator.as_ident() {
                    if ident.as_str() == "panic" {
                        if invoke.args.len() > 1 {
                            self.emit_error("panic expects at most one argument");
                        }
                        for arg in &mut invoke.args {
                            let _ = self.infer_expr_inner(arg).await?;
                        }
                        return Ok(self.nothing_type_var());
                    }
                }
                if let Some(var) = self.lookup_associated_function(locator).await? {
                    var
                } else {
                    let var = self.lookup_locator(locator).await?;
                    if let Ok(resolved) = self.resolve_to_ty(var).await {
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
            ExprInvokeTarget::Expr(expr) => self.infer_expr_inner(expr.as_mut()).await?,
            ExprInvokeTarget::Closure(_) => {
                let fn_ty = match &invoke.target {
                    ExprInvokeTarget::Closure(func) => {
                        self.ty_from_function_signature(&func.sig)?
                    }
                    _ => Ty::Unknown(TypeUnknown),
                };
                self.type_from_ast_ty(&fn_ty).await?
            }
            ExprInvokeTarget::BinOp(kind) => {
                if invoke.args.len() != 2 {
                    let message = "binary operator invocation expects two arguments".to_string();
                    self.emit_error(message.clone());
                    return Ok(self.error_type_var());
                }
                let lhs = self.infer_expr_inner(&mut invoke.args[0]).await?;
                let rhs = self.infer_expr_inner(&mut invoke.args[1]).await?;
                let result = match kind {
                    BinOpKind::Add
                    | BinOpKind::Sub
                    | BinOpKind::Mul
                    | BinOpKind::Div
                    | BinOpKind::Mod
                    | BinOpKind::Shl
                    | BinOpKind::Shr => {
                        if matches!(kind, BinOpKind::Add) {
                            let lhs_ty = self.resolve_to_ty(lhs).await?;
                            let rhs_ty = self.resolve_to_ty(rhs).await?;
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
                                self.unify(lhs, rhs).await?;
                                return Ok(lhs);
                            }
                        }
                        self.ensure_numeric(lhs, "binary operand")?;
                        self.unify(lhs, rhs).await?;
                        lhs
                    }
                    BinOpKind::Eq
                    | BinOpKind::Ne
                    | BinOpKind::Lt
                    | BinOpKind::Le
                    | BinOpKind::Gt
                    | BinOpKind::Ge => {
                        self.unify(lhs, rhs).await?;
                        let bool_var = self.fresh_type_var();
                        self.bind(bool_var, Ty::Primitive(TypePrimitive::Bool));
                        bool_var
                    }
                    BinOpKind::And | BinOpKind::Or => {
                        self.ensure_bool(lhs, "logical operand")?;
                        self.ensure_bool(rhs, "logical operand")?;
                        let bool_var = self.fresh_type_var();
                        self.bind(bool_var, Ty::Primitive(TypePrimitive::Bool));
                        bool_var
                    }
                    _ => lhs,
                };
                return Ok(result);
            }
            ExprInvokeTarget::Type(ty) => self.type_from_ast_ty(ty).await?,
            ExprInvokeTarget::Method(select) => {
                if std::env::var("FP_DEBUG_UNWRAP").is_ok()
                    && select.field.name.as_str() == "unwrap"
                {
                    eprintln!("debug unwrap invoke: method field=unwrap");
                }
                let obj_var = match self.infer_expr_inner(select.obj.as_mut()).await {
                    Ok(var) => var,
                    Err(err) => {
                        if std::env::var("FP_DEBUG_UNWRAP").is_ok()
                            && select.field.name.as_str() == "unwrap"
                        {
                            eprintln!("debug unwrap invoke: obj infer error: {:?}", err);
                        }
                        return Err(err);
                    }
                };
                if std::env::var("FP_DEBUG_UNWRAP").is_ok()
                    && select.field.name.as_str() == "unwrap"
                {
                    eprintln!("debug unwrap invoke: after infer_expr");
                    if let Ok(obj_ty) = self.resolve_to_ty(obj_var).await {
                        eprintln!("debug unwrap invoke: obj_ty={:?}", obj_ty);
                    }
                }
                if let Some(result) =
                    self.try_infer_primitive_method(obj_var, &select.field, invoke.args.len()).await?
                {
                    return Ok(result);
                }
                if select.field.name.as_str() == "len" && invoke.args.is_empty() {
                    if let Ok(obj_ty) = self.resolve_to_ty(obj_var).await {
                        let peeled = Self::peel_reference(obj_ty.clone());
                        if matches!(peeled, Ty::Primitive(TypePrimitive::String))
                            || Self::is_collection_with_len(&obj_ty)
                            || matches!(peeled, Ty::Quote(_))
                        {
                            let result_var = self.fresh_type_var();
                            self.bind(result_var, Ty::Primitive(TypePrimitive::Int(TypeInt::I64)));
                            return Ok(result_var);
                        }
                    }
                }
                if select.field.name.as_str() == "contains" {
                    if invoke.args.len() != 1 {
                        self.emit_error("contains expects exactly one argument");
                        return Ok(self.error_type_var());
                    }
                    let _ = self.infer_expr_inner(&mut invoke.args[0]).await?;
                    if let Ok(obj_ty) = self.resolve_to_ty(obj_var).await {
                        let peeled = Self::peel_reference(obj_ty);
                        if !matches!(peeled, Ty::Primitive(TypePrimitive::List))
                            && !Self::is_collection_with_len(&peeled)
                        {
                            self.emit_error("contains expects a list receiver");
                        }
                    }
                    let result_var = self.fresh_type_var();
                    self.bind(result_var, Ty::Primitive(TypePrimitive::Bool));
                    return Ok(result_var);
                }

                if let Ok(obj_ty) = self.resolve_to_ty(obj_var).await {
                    let peeled = Self::peel_reference(obj_ty);
                    if matches!(peeled, Ty::Type(_)) {
                        let method = select.field.name.as_str();
                        match method {
                            "has_field" => {
                                if invoke.args.len() != 1 {
                                    self.emit_error("has_field expects exactly one argument");
                                    return Ok(self.error_type_var());
                                }
                                let arg_var = self.infer_expr_inner(&mut invoke.args[0]).await?;
                                let string_var = self.borrowed_string_var();
                                self.unify(arg_var, string_var).await?;
                                let result_var = self.fresh_type_var();
                                self.bind(result_var, Ty::Primitive(TypePrimitive::Bool));
                                return Ok(result_var);
                            }
                            "has_method" => {
                                if invoke.args.len() != 1 {
                                    self.emit_error("has_method expects exactly one argument");
                                    return Ok(self.error_type_var());
                                }
                                let arg_var = self.infer_expr_inner(&mut invoke.args[0]).await?;
                                let string_var = self.borrowed_string_var();
                                self.unify(arg_var, string_var).await?;
                                let result_var = self.fresh_type_var();
                                self.bind(result_var, Ty::Primitive(TypePrimitive::Bool));
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
                                    Ty::Primitive(TypePrimitive::Int(TypeInt::I64)),
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
                                    Ty::Primitive(TypePrimitive::Int(TypeInt::I64)),
                                );
                                return Ok(result_var);
                            }
                            "field_name_at" => {
                                if invoke.args.len() != 1 {
                                    self.emit_error("field_name_at expects exactly one argument");
                                    return Ok(self.error_type_var());
                                }
                                let arg_var = self.infer_expr_inner(&mut invoke.args[0]).await?;
                                let int_var = self.fresh_type_var();
                                self.bind(int_var, Ty::Primitive(TypePrimitive::Int(TypeInt::I64)));
                                self.unify(arg_var, int_var).await?;
                                return Ok(self.borrowed_string_var());
                            }
                            "field_type" => {
                                if invoke.args.len() != 1 {
                                    self.emit_error("field_type expects exactly one argument");
                                    return Ok(self.error_type_var());
                                }
                                let arg_var = self.infer_expr_inner(&mut invoke.args[0]).await?;
                                let string_var = self.borrowed_string_var();
                                self.unify(arg_var, string_var).await?;
                                let result_var = self.fresh_type_var();
                                self.bind(result_var, Ty::Type(TypeType::new(Span::null())));
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
                                return Ok(self.borrowed_string_var());
                            }
                            _ => {}
                        }
                    }
                }

                if std::env::var("FP_DEBUG_UNWRAP").is_ok()
                    && select.field.name.as_str() == "unwrap"
                {
                    eprintln!("debug unwrap invoke: before try_infer_field_function_call");
                }
                let field_call = self.try_infer_field_function_call(obj_var, &select.field).await?;
                if std::env::var("FP_DEBUG_UNWRAP").is_ok()
                    && select.field.name.as_str() == "unwrap"
                {
                    eprintln!(
                        "debug unwrap invoke: try_infer_field_function_call result={}",
                        field_call.is_some()
                    );
                }
                if let Some(field_var) = field_call {
                    field_var
                } else {
                    if std::env::var("FP_DEBUG_UNWRAP").is_ok()
                        && select.field.name.as_str() == "unwrap"
                    {
                        eprintln!("debug unwrap invoke: before lookup_struct_method");
                    }
                    self.lookup_struct_method(obj_var, &select.field, &mut invoke.args).await?
                }
            }
        };

        if let ExprInvokeTarget::Function(locator) = &invoke.target {
            if let Some(sig) = self.lookup_function_signature(locator) {
                if let Ok(fn_ty) = self.ty_from_function_signature(&sig) {
                    if let Ok(sig_var) = self.type_from_ast_ty(&fn_ty).await {
                        let _ = self.unify(func_var, sig_var).await;
                    }
                }
            }
        }

        let func_info = self.ensure_function(func_var, invoke.args.len()).await?;
        let mut arg_vars = Vec::with_capacity(invoke.args.len());
        for (arg_expr, param_var) in invoke.args.iter_mut().zip(func_info.params.iter()) {
            let arg_var = self.infer_expr_inner(arg_expr).await?;
            arg_vars.push(arg_var);
            self.unify(*param_var, arg_var).await?;
        }
        if let Some((enum_def, variant)) = enum_ctor {
            self.bind_enum_constructor_return(&enum_def, &variant, &arg_vars, func_info.ret).await?;
        }
        Ok(func_info.ret)
    }

    async fn enum_variant_from_locator(&self, locator: &Name) -> Option<(TypeEnum, EnumTypeVariant)> {
        let Name::Path(path) = locator else {
            return None;
        };
        if path.segments.len() < 2 {
            return None;
        }
        let variant_name = path.segments.last().map(|seg| seg.as_str())?;
        let enum_segments = path
            .segments
            .iter()
            .take(path.segments.len() - 1)
            .map(|seg| seg.as_str().to_string())
            .collect::<Vec<_>>();
        let enum_key = self.resolve_segments_key(path.prefix, &enum_segments)?;
        let enum_def = self.own_enum_defs().get(&enum_key).cloned()?;
        let variant = enum_def
            .variants
            .iter()
            .find(|v| v.name.as_str() == variant_name)
            .cloned()?;
        Some((enum_def, variant))
    }

    fn enum_variant_param_types(variant: &EnumTypeVariant) -> Vec<Ty> {
        match &variant.value {
            Ty::Unit(_) => Vec::new(),
            Ty::Tuple(tuple_ty) => tuple_ty.types.clone(),
            other => vec![other.clone()],
        }
    }

    async fn bind_enum_constructor_return(
        &self,
        enum_def: &TypeEnum,
        variant: &EnumTypeVariant,
        arg_vars: &[TypeVarId],
        ret_var: TypeVarId,
    ) -> Result<()> {
        let param_types = Self::enum_variant_param_types(variant);
        if param_types.len() != arg_vars.len() {
            return Ok(());
        }
        self.enter_scope();
        let mut generic_vars: Vec<(String, TypeVarId)> = Vec::new();
        for param in &enum_def.generics_params {
            let var = self.register_generic_param(param.name.as_str());
            generic_vars.push((param.name.as_str().to_string(), var));
            let bounds = Self::extract_trait_bounds(&param.bounds);
            if !bounds.is_empty() {
                self.inner.borrow_mut().generic_trait_bounds.insert(var, bounds);
            }
        }

        for (param_ty, arg_var) in param_types.iter().zip(arg_vars.iter()) {
            let param_var = self.type_from_ast_ty(param_ty).await?;
            self.unify(param_var, *arg_var).await?;
        }

        if let Ok(Ty::Enum(expected_enum)) = self.resolve_to_ty(ret_var).await {
            if expected_enum.name.as_str() == enum_def.name.as_str()
                && !enum_def.generics_params.is_empty()
            {
                let generic_names: HashSet<String> = enum_def
                    .generics_params
                    .iter()
                    .map(|param| param.name.as_str().to_string())
                    .collect();
                let mut mapping: HashMap<String, Ty> = HashMap::new();
                for def_variant in &enum_def.variants {
                    if let Some(concrete_variant) = expected_enum
                        .variants
                        .iter()
                        .find(|variant| variant.name == def_variant.name)
                    {
                        self.collect_enum_generic_mapping(
                            &def_variant.value,
                            &concrete_variant.value,
                            &generic_names,
                            &mut mapping,
                        );
                    }
                }
                for (name, var) in &generic_vars {
                    if let Some(expected_ty) = mapping.get(name) {
                        let resolved = self.resolve_to_ty(*var).await.unwrap_or(Ty::Unknown(TypeUnknown));
                        if matches!(resolved, Ty::Unknown(_) | Ty::Any(_)) {
                            let expected_var = self.type_from_ast_ty(expected_ty).await?;
                            self.unify(expected_var, *var).await?;
                        }
                    }
                }
            }
        }

        let mut args = Vec::with_capacity(generic_vars.len());
        for (name, var) in &generic_vars {
            let resolved = self.resolve_to_ty(*var).await.unwrap_or(Ty::Unknown(TypeUnknown));
            let ty = match resolved {
                Ty::Unknown(_) | Ty::Any(_) => Ty::ident(Ident::new(name)),
                other => other,
            };
            args.push(ty);
        }
        let concrete = self.apply_generic_args_to_enum(enum_def, &args);
        self.exit_scope();
        let enum_var = self.fresh_type_var();
        self.bind(enum_var, Ty::Enum(concrete));
        self.unify(enum_var, ret_var).await?;
        Ok(())
    }

    fn apply_kwargs_to_invoke(&self, invoke: &mut ExprInvoke, sig: &FunctionSignature) -> bool {
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
            if let Some(context_arg) = self.resolve_context_argument(&sig.params[idx]) {
                *slot = Some(context_arg);
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

    async fn infer_generic_function_call(
        &self,
        invoke: &mut ExprInvoke,
        sig: &FunctionSignature,
        sig_module: &QualifiedPath,
        explicit_generic_args: Option<&[Ty]>,
        sig_path: &QualifiedPath,
    ) -> Result<TypeVarId> {
        if invoke.args.len() != sig.params.len() {
            self.emit_error("call arity mismatch");
            return Ok(self.error_type_var());
        }

        self.enter_scope();
        // Split into a helper so the early `?`-returns don't skip
        // `exit_scope()` -- a plain (sync) closure can't contain `.await`,
        // so this replaces the old IIFE-closure trick.
        let result = self
            .infer_generic_function_call_body(invoke, sig, sig_module, explicit_generic_args, sig_path)
            .await;
        self.exit_scope();
        result
    }

    async fn infer_generic_function_call_body(
        &self,
        invoke: &mut ExprInvoke,
        sig: &FunctionSignature,
        sig_module: &QualifiedPath,
        explicit_generic_args: Option<&[Ty]>,
        sig_path: &QualifiedPath,
    ) -> Result<TypeVarId> {
        {
            let mut generic_vars = Vec::with_capacity(sig.generics_params.len());
            for param in &sig.generics_params {
                let var = self.register_generic_param(param.name.as_str());
                generic_vars.push(var);
                let bounds = Self::extract_trait_bounds(&param.bounds);
                if !bounds.is_empty() {
                    self.inner.borrow_mut().generic_trait_bounds.insert(var, bounds);
                }
            }

            if let Some(explicit_args) = explicit_generic_args {
                for (idx, arg_ty) in explicit_args.iter().enumerate() {
                    if Self::is_inferred_generic_placeholder(arg_ty) {
                        continue;
                    }
                    let param_var = generic_vars[idx];
                    let arg_var = self.type_from_ast_ty_in_module(arg_ty, sig_module).await?;
                    self.unify(arg_var, param_var).await?;
                }
            }

            for (arg_expr, param) in invoke.args.iter_mut().zip(sig.params.iter()) {
                let arg_var = self.infer_expr_inner(arg_expr).await?;
                let param_var = self.type_from_ast_ty_in_module(&param.ty, sig_module).await?;
                self.unify(arg_var, param_var).await?;
            }

            let ret_var = if let Some(ret_ty) = &sig.ret_ty {
                self.type_from_ast_ty_in_module(ret_ty, sig_module).await?
            } else {
                let unit = self.fresh_type_var();
                self.bind(unit, Ty::Unit(TypeUnit));
                unit
            };

            let mut concrete_types = Vec::with_capacity(generic_vars.len());
            let mut param_names = Vec::with_capacity(generic_vars.len());
            let mut all_resolved = true;
            for (i, param) in sig.generics_params.iter().enumerate() {
                param_names.push(param.name.as_str().to_string());
                match self.resolve_to_ty(generic_vars[i]).await {
                    Ok(ty) if !matches!(ty, Ty::Unknown(_)) => {
                        concrete_types.push(ty);
                    }
                    _ => {
                        all_resolved = false;
                        break;
                    }
                }
            }
            if all_resolved && !param_names.is_empty() {
                // Only a locally-defined function (registered in
                // `own_function_item_ids` alongside its `own_function_sigs`
                // entry -- see that map's doc comment) has an `Item` this
                // compile unit's own typed AST can find again for
                // specialization; a cross-crate/workspace signature
                // (resolved via `env_ctx.find_function_sig`) has no such
                // entry, and monomorphizing it isn't something this
                // mechanism supports.
                if let Some(item_id) = self.own_function_item_ids().get(sig_path).copied() {
                    self.inner.borrow_mut().pending_generics.push(GenericMonorph::new(
                        item_id,
                        sig_path.clone(),
                        param_names,
                        concrete_types,
                    ));
                }
            }

            Ok(ret_var)
        }
    }

    fn is_inferred_generic_placeholder(arg_ty: &Ty) -> bool {
        // `_` generic placeholders are parsed as `Ty::Unknown`.
        matches!(arg_ty, Ty::Unknown(_))
    }

    async fn try_infer_field_function_call(
        &self,
        obj_var: TypeVarId,
        field: &Ident,
    ) -> Result<Option<TypeVarId>> {
        let ty = self.resolve_to_ty(obj_var).await?;
        let resolved_ty = Self::peel_reference(ty);
        if std::env::var("FP_DEBUG_UNWRAP").is_ok() && field.name.as_str() == "unwrap" {
            eprintln!(
                "debug unwrap invoke: try_infer_field_function_call resolved_ty={:?}",
                resolved_ty
            );
        }
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
        if std::env::var("FP_DEBUG_UNWRAP").is_ok() && field.name.as_str() == "unwrap" {
            eprintln!(
                "debug unwrap invoke: try_infer_field_function_call field_ty={:?}",
                field_ty
            );
        }
        if !matches!(field_ty, Ty::Function(_)) {
            return Ok(None);
        }
        let field_var = self.type_from_ast_ty(&field_ty).await?;
        Ok(Some(field_var))
    }

    async fn try_infer_collection_call(&self, invoke: &mut ExprInvoke) -> Result<Option<TypeVarId>> {
        let locator = match &invoke.target {
            ExprInvokeTarget::Function(locator) => locator,
            _ => return Ok(None),
        };
        if Self::locator_matches_suffix(locator, &["Vec", "new"]) {
            return self.infer_vec_new(invoke).await.map(Some);
        }
        if Self::locator_matches_suffix(locator, &["Vec", "with_capacity"]) {
            return self.infer_vec_with_capacity(invoke).await.map(Some);
        }
        if Self::locator_matches_suffix(locator, &["Vec", "from"]) {
            return self.infer_vec_from(invoke).await.map(Some);
        }
        if Self::locator_matches_suffix(locator, &["HashMap", "new"]) {
            return self.infer_hashmap_new(invoke).await.map(Some);
        }
        if Self::locator_matches_suffix(locator, &["HashMap", "with_capacity"]) {
            return self.infer_hashmap_with_capacity(invoke).await.map(Some);
        }
        if Self::locator_matches_suffix(locator, &["HashMap", "from"]) {
            return self.infer_hashmap_from(invoke).await.map(Some);
        }
        Ok(None)
    }

    async fn infer_vec_new(&self, invoke: &mut ExprInvoke) -> Result<TypeVarId> {
        if !invoke.args.is_empty() {
            for arg in &mut invoke.args {
                let _ = self.infer_expr_inner(arg).await;
            }
            self.emit_error("Vec::new does not take arguments");
        }
        let elem_var = self.fresh_type_var();
        let vec_var = self.fresh_type_var();
        self.bind_vec_term(vec_var, elem_var);
        Ok(vec_var)
    }

    async fn infer_vec_with_capacity(&self, invoke: &mut ExprInvoke) -> Result<TypeVarId> {
        if invoke.args.len() != 1 {
            for arg in &mut invoke.args {
                let _ = self.infer_expr_inner(arg).await;
            }
            self.emit_error("Vec::with_capacity expects a single capacity argument");
        } else {
            let capacity_var = self.infer_expr_inner(&mut invoke.args[0]).await?;
            let expected = self.fresh_type_var();
            self.bind(expected, Ty::Primitive(TypePrimitive::Int(TypeInt::U64)));
            self.unify(capacity_var, expected).await?;
        }
        let elem_var = self.fresh_type_var();
        let vec_var = self.fresh_type_var();
        self.bind_vec_term(vec_var, elem_var);
        Ok(vec_var)
    }

    async fn infer_vec_from(&self, invoke: &mut ExprInvoke) -> Result<TypeVarId> {
        if invoke.args.len() != 1 {
            for arg in &mut invoke.args {
                let _ = self.infer_expr_inner(arg).await;
            }
            self.emit_error("Vec::from expects a single iterable argument");
        } else {
            let _ = self.infer_expr_inner(&mut invoke.args[0]).await?;
        }
        let elem_var = self.fresh_type_var();
        let vec_var = self.fresh_type_var();
        self.bind_vec_term(vec_var, elem_var);
        Ok(vec_var)
    }

    async fn infer_hashmap_new(&self, invoke: &mut ExprInvoke) -> Result<TypeVarId> {
        if !invoke.args.is_empty() {
            for arg in &mut invoke.args {
                let _ = self.infer_expr_inner(arg).await;
            }
            self.emit_error("HashMap::new does not take arguments");
        }
        let key_var = self.fresh_type_var();
        let value_var = self.fresh_type_var();
        let map_var = self.fresh_type_var();
        let map_ty = self.make_hashmap_struct();
        self.bind(map_var, Ty::Struct(map_ty));
        self.record_hashmap_args(map_var, key_var, value_var);
        Ok(map_var)
    }

    async fn infer_hashmap_with_capacity(&self, invoke: &mut ExprInvoke) -> Result<TypeVarId> {
        if invoke.args.len() != 1 {
            for arg in &mut invoke.args {
                let _ = self.infer_expr_inner(arg).await;
            }
            self.emit_error("HashMap::with_capacity expects a single capacity argument");
        } else {
            let capacity_var = self.infer_expr_inner(&mut invoke.args[0]).await?;
            let expected = self.fresh_type_var();
            self.bind(expected, Ty::Primitive(TypePrimitive::Int(TypeInt::U64)));
            self.unify(capacity_var, expected).await?;
        }
        let key_var = self.fresh_type_var();
        let value_var = self.fresh_type_var();
        let map_var = self.fresh_type_var();
        let map_ty = self.make_hashmap_struct();
        self.bind(map_var, Ty::Struct(map_ty));
        self.record_hashmap_args(map_var, key_var, value_var);
        Ok(map_var)
    }

    async fn infer_hashmap_from(&self, invoke: &mut ExprInvoke) -> Result<TypeVarId> {
        let mut key_var = self.fresh_type_var();
        let mut value_var = self.fresh_type_var();
        if invoke.args.len() != 1 {
            for arg in &mut invoke.args {
                let _ = self.infer_expr_inner(arg).await;
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
                            let key = self.infer_expr_inner(key_expr).await?;
                            let value = self.infer_expr_inner(value_expr).await?;
                            if idx == 0 {
                                key_var = key;
                                value_var = value;
                            } else {
                                let _ = self.unify(key_var, key).await;
                                let _ = self.unify(value_var, value).await;
                            }
                            continue;
                        }
                    }
                    if let ExprKind::Tuple(tuple_expr) = entry.kind_mut() {
                        if tuple_expr.values.len() == 2 {
                            let key = self.infer_expr_inner(&mut tuple_expr.values[0]).await?;
                            let value = self.infer_expr_inner(&mut tuple_expr.values[1]).await?;
                            if idx == 0 {
                                key_var = key;
                                value_var = value;
                            } else {
                                let _ = self.unify(key_var, key).await;
                                let _ = self.unify(value_var, value).await;
                            }
                            continue;
                        }
                    }
                    if let ExprKind::Array(array_expr) = entry.kind_mut() {
                        if array_expr.values.len() == 2 {
                            let key = self.infer_expr_inner(&mut array_expr.values[0]).await?;
                            let value = self.infer_expr_inner(&mut array_expr.values[1]).await?;
                            if idx == 0 {
                                key_var = key;
                                value_var = value;
                            } else {
                                let _ = self.unify(key_var, key).await;
                                let _ = self.unify(value_var, value).await;
                            }
                            continue;
                        }
                    }
                    let _ = self.infer_expr_inner(entry).await?;
                    self.emit_error("HashMap::from expects HashMapEntry { key, value } entries");
                }
            } else {
                let _ = self.infer_expr_inner(arg).await?;
                self.emit_error("HashMap::from expects an array literal of entries");
            }
        }
        let map_var = self.fresh_type_var();
        let map_ty = self.make_hashmap_struct();
        self.bind(map_var, Ty::Struct(map_ty));
        self.record_hashmap_args(map_var, key_var, value_var);
        Ok(map_var)
    }

    pub(crate) fn make_hashmap_struct(&self) -> TypeStruct {
        let key = QualifiedPath::new(vec!["HashMap".to_string()]);
        if let Some(existing) = self.own_struct_defs().get(&key) {
            return existing.clone();
        }
        TypeStruct {
            name: Ident::new("HashMap"),
            generics_params: Vec::new(),
            repr: ReprOptions::default(),
            method_sigs: Vec::new(),
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

    fn locator_generic_args(locator: &Name) -> Option<&[Ty]> {
        let Name::ParameterPath(path) = locator else {
            return None;
        };
        let segment = path
            .segments
            .iter()
            .rev()
            .find(|seg| !seg.args.is_empty())?;
        Some(segment.args.as_slice())
    }

    fn is_collection_with_len(ty: &Ty) -> bool {
        match ty {
            Ty::Array(_) | Ty::Slice(_) | Ty::Vec(_) => true,
            Ty::Struct(struct_ty) => struct_ty.name.as_str() == "HashMap",
            _ => false,
        }
    }

    fn try_infer_query_pipeline_call(&self, invoke: &ExprInvoke) -> Option<Result<TypeVarId>> {
        let expr = Expr::new(ExprKind::Invoke(invoke.clone()));
        if lower_fp_expr_to_query(&expr, None).is_none() {
            return None;
        }

        Some(Ok(self.fresh_type_var()))
    }

    async fn infer_builtin_printf(&self, invoke: &mut ExprInvoke) -> Result<TypeVarId> {
        if invoke.args.is_empty() {
            self.emit_error("printf requires a format string argument");
            return Ok(self.error_type_var());
        }
        let format_var = self.infer_expr_inner(&mut invoke.args[0]).await?;
        let expected_format = self.fresh_type_var();
        self.bind(expected_format, Ty::Primitive(TypePrimitive::String));
        self.unify(format_var, expected_format).await?;
        for arg in invoke.args.iter_mut().skip(1) {
            let _ = self.infer_expr_inner(arg).await?;
        }
        let result_var = self.fresh_type_var();
        self.bind(result_var, Ty::Unit(TypeUnit));
        Ok(result_var)
    }

    pub(crate) async fn infer_list_value_as_vec(&self, list: &ValueList) -> Result<TypeVarId> {
        let elem_var = if let Some(first) = list.values.first() {
            let first_var = self.infer_value(first).await?;
            for value in list.values.iter().skip(1) {
                let next_var = self.infer_value(value).await?;
                self.unify(first_var, next_var).await?;
            }
            first_var
        } else {
            self.fresh_type_var()
        };
        let vec_var = self.fresh_type_var();
        self.bind_vec_term(vec_var, elem_var);
        Ok(vec_var)
    }

    async fn infer_intrinsic_container(
        &self,
        collection: &mut ExprIntrinsicContainer,
    ) -> Result<TypeVarId> {
        match collection {
            ExprIntrinsicContainer::VecElements { elements } => {
                let elem_var = if let Some(first) = elements.first_mut() {
                    let first_var = self.infer_expr_inner(first).await?;
                    for expr in elements.iter_mut().skip(1) {
                        let next_var = self.infer_expr_inner(expr).await?;
                        self.unify(first_var, next_var).await?;
                    }
                    first_var
                } else {
                    self.fresh_type_var()
                };
                let vec_var = self.fresh_type_var();
                self.bind_vec_term(vec_var, elem_var);
                Ok(vec_var)
            }
            ExprIntrinsicContainer::VecRepeat { elem, len } => {
                let elem_var = self.infer_expr_inner(elem.as_mut()).await?;
                let len_var = self.infer_expr_inner(len.as_mut()).await?;
                let expected = self.fresh_type_var();
                self.bind(expected, Ty::Primitive(TypePrimitive::Int(TypeInt::U64)));
                self.unify(len_var, expected).await?;
                let vec_var = self.fresh_type_var();
                self.bind_vec_term(vec_var, elem_var);
                Ok(vec_var)
            }
            ExprIntrinsicContainer::HashMapEntries { entries } => {
                for entry in entries {
                    let _ = self.infer_expr_inner(&mut entry.key).await?;
                    let _ = self.infer_expr_inner(&mut entry.value).await?;
                }
                let map_var = self.fresh_type_var();
                let map_ty = self.make_hashmap_struct();
                self.bind(map_var, Ty::Struct(map_ty));
                Ok(map_var)
            }
        }
    }

    pub(crate) fn infer_value<'a>(
        &self,
        value: &'a Value,
    ) -> BoxFuture<'a, Result<TypeVarId>> {
        let this = self.clone();
        Box::pin(async move {
        let var = this.fresh_type_var();
        match value {
            Value::Int(_) => {
                this.inner.borrow_mut().literal_ints.insert(var);
                this.bind(var, Ty::Primitive(TypePrimitive::Int(TypeInt::I64)));
            }
            Value::UInt(_) => {
                this.inner.borrow_mut().literal_ints.insert(var);
                this.bind(var, Ty::Primitive(TypePrimitive::Int(TypeInt::U64)));
            }
            Value::Bool(_) => this.bind(var, Ty::Primitive(TypePrimitive::Bool)),
            Value::Decimal(_) => {
                this.bind(var, Ty::Primitive(TypePrimitive::Decimal(DecimalType::F64)))
            }
            Value::String(_) => {
                let inner = this.fresh_type_var();
                this.bind(inner, Ty::Primitive(TypePrimitive::String));
                this.bind_reference_term(var, inner);
            }
            Value::Bytes(bytes) if bytes_value_is_borrowed_string(bytes) => {
                let string_var = this.borrowed_string_var();
                this.unify(var, string_var).await?;
            }
            Value::List(list) => {
                let elem_var = if let Some(first) = list.values.first() {
                    this.infer_value(first).await?
                } else {
                    this.fresh_type_var()
                };
                for value in list.values.iter().skip(1) {
                    let next_var = this.infer_value(value).await?;
                    this.unify(elem_var, next_var).await?;
                }
                let len = list.values.len() as i64;
                let len_expr = Expr::value(Value::int(len)).into();
                this.bind_array_term(var, elem_var, Some(len_expr));
            }
            Value::Char(_) => this.bind(var, Ty::Primitive(TypePrimitive::Char)),
            Value::Unit(_) => this.bind(var, Ty::Unit(TypeUnit)),
            Value::Null(_) | Value::None(_) => this.bind(var, Ty::Nothing(TypeNothing)),
            Value::Struct(struct_val) => {
                this.bind(var, Ty::Struct(struct_val.ty.clone()));
            }
            Value::Structural(structural) => {
                let mut fields = Vec::with_capacity(structural.fields.len());
                for field in &structural.fields {
                    let field_var = this.infer_value(&field.value).await?;
                    let field_ty = this.resolve_to_ty(field_var).await?;
                    fields.push(StructuralField::new(field.name.clone(), field_ty));
                }
                this.bind(var, Ty::Structural(TypeStructural { fields }));
            }
            Value::Tuple(tuple) => {
                let mut vars = Vec::new();
                for elem in &tuple.values {
                    vars.push(this.infer_value(elem).await?);
                }
                this.bind_tuple_term(var, vars);
            }
            Value::Map(map) => {
                for entry in &map.entries {
                    let _ = this.infer_value(&entry.key).await?;
                    let _ = this.infer_value(&entry.value).await?;
                }
                let map_ty = this.make_hashmap_struct();
                this.bind(var, Ty::Struct(map_ty));
            }
            Value::Function(func) => {
                let fn_ty = this.ty_from_function_signature(&func.sig)?;
                let fn_var = this.type_from_ast_ty(&fn_ty).await?;
                this.unify(var, fn_var).await?;
            }
            Value::Type(inner) => {
                if let Ty::Struct(_) = inner {
                    // Wrap the concrete type inside Ty::Type with inner set
                    let type_var = this.type_from_ast_ty(
                        &Ty::Type(TypeType::new(Span::null()).with_inner(inner.clone()))
                    ).await?;
                    this.unify(var, type_var).await?;
                } else {
                    let type_var = this.type_from_ast_ty(&Ty::Type(TypeType::new(Span::null()))).await?;
                    this.unify(var, type_var).await?;
                }
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
                                    this.emit_error(
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
                let quote_var = this.type_from_ast_ty(&quote_ty).await?;
                this.unify(var, quote_var).await?;
            }
            Value::TokenStream(_) => {
                let ts_var = this.type_from_ast_ty(&Ty::TokenStream(TypeTokenStream)).await?;
                this.unify(var, ts_var).await?;
            }
            Value::Expr(_) => {
                let message = "embedded expression values are not yet supported".to_string();
                this.emit_error(message.clone());
                return Ok(this.error_type_var());
            }
            _ => {
                let message = format!("value {:?} is not supported by type inference", value);
                this.emit_error(message.clone());
                return Ok(this.error_type_var());
            }
        }
        Ok(var)
        })
    }

    pub(crate) fn infer_pattern<'a>(
        &self,
        pattern: &'a mut Pattern,
    ) -> BoxFuture<'a, Result<PatternInfo>> {
        let this = self.clone();
        Box::pin(async move {
        let existing_ty = pattern.ty().cloned();
        let info = match pattern.kind_mut() {
            PatternKind::Ident(ident) => {
                let var = this.fresh_type_var();
                this.insert_env(ident.ident.as_str().to_string(), EnvEntry::Mono(var));
                PatternInfo::new(var).with_binding(ident.ident.as_str().to_string(), var)
            }
            PatternKind::Bind(bind) => {
                let inner_info = this.infer_pattern(bind.pattern.as_mut()).await?;
                let var = inner_info.var;
                this.insert_env(bind.ident.ident.as_str().to_string(), EnvEntry::Mono(var));
                let mut info = inner_info;
                info.bindings.push(PatternBinding {
                    name: bind.ident.ident.as_str().to_string(),
                    var,
                });
                info
            }
            PatternKind::Type(inner) => {
                let inner_info = this.infer_pattern(inner.pat.as_mut()).await?;
                let annot_var = this.type_from_ast_ty(&inner.ty).await?;
                this.unify(inner_info.var, annot_var).await?;
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
                let var = this.type_from_ast_ty(&quote_ty).await?;
                PatternInfo::new(var)
            }
            PatternKind::QuotePlural(quote) => {
                let quote_ty = quote_ty_from_fragment(quote.fragment, None);
                let elem_var = this.type_from_ast_ty(&quote_ty).await?;
                let list_var = this.fresh_type_var();
                this.bind_vec_term(list_var, elem_var);
                PatternInfo::new(list_var)
            }
            PatternKind::Wildcard(_) => PatternInfo::new(this.fresh_type_var()),
            PatternKind::Tuple(tuple) => {
                let mut vars = Vec::new();
                let mut bindings = Vec::new();
                for pat in &mut tuple.patterns {
                    let child = this.infer_pattern(pat).await?;
                    vars.push(child.var);
                    bindings.extend(child.bindings);
                }
                let tuple_var = this.fresh_type_var();
                this.bind_tuple_term(tuple_var, vars);
                PatternInfo {
                    var: tuple_var,
                    bindings,
                }
            }
            PatternKind::Struct(struct_pat) => {
                let struct_name = this
                    .qualified_name(struct_pat.name.as_str())
                    .unwrap_or_else(|| {
                        QualifiedPath::new(vec![struct_pat.name.as_str().to_string()])
                    });
                let struct_var = this.fresh_type_var();
                let struct_def = this.own_struct_defs().get(&struct_name).cloned();
                if let Some(struct_def) = struct_def {
                    this.bind(struct_var, Ty::Struct(struct_def.clone()));
                    let mut bindings = Vec::new();
                    for field in &mut struct_pat.fields {
                        if let Some(rename) = field.rename.as_mut() {
                            let child = this.infer_pattern(rename).await?;
                            bindings.extend(child.bindings);
                            if let Some(def_field) =
                                struct_def.fields.iter().find(|f| f.name == field.name)
                            {
                                let expected = this.type_from_ast_ty(&def_field.value).await?;
                                this.unify(child.var, expected).await?;
                            }
                        } else if let Some(def_field) =
                            struct_def.fields.iter().find(|f| f.name == field.name)
                        {
                            let var = this.fresh_type_var();
                            this.insert_env(field.name.as_str().to_string(), EnvEntry::Mono(var));
                            let expected = this.type_from_ast_ty(&def_field.value).await?;
                            this.unify(var, expected).await?;
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
                    this.emit_error(format!(
                        "unknown struct {} in pattern",
                        struct_name.to_key()
                    ));
                    PatternInfo::new(this.error_type_var())
                }
            }
            PatternKind::TupleStruct(tuple_struct) => {
                // Handles tuple-struct patterns and enum tuple variants.
                let mut bindings = Vec::new();
                let mut element_vars = Vec::new();
                for pat in &mut tuple_struct.patterns {
                    let child = this.infer_pattern(pat).await?;
                    element_vars.push(child.var);
                    bindings.extend(child.bindings);
                }

                let tuple_var = this.fresh_type_var();
                this.bind_tuple_term(tuple_var, element_vars.clone());

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
                        let mut enum_def = None;
                        if let Some(enum_key) =
                            this.resolve_segments_key(path.prefix, &enum_segments)
                        {
                            enum_def = this.own_enum_defs().get(&enum_key).cloned();
                        }
                        if enum_def.is_none() {
                            let enum_name = enum_segments.join("::");
                            enum_def = this.lookup_enum_def_by_name(&enum_name).map(|(_, def)| def);
                        }
                        if let Some(enum_def) = enum_def {
                            if let Some(variant) = enum_def
                                .variants
                                .iter()
                                .find(|v| v.name.as_str() == variant_name)
                            {
                                this.enter_scope();
                                let mut generic_vars: Vec<(String, TypeVarId)> = Vec::new();
                                for param in &enum_def.generics_params {
                                    let var = this.register_generic_param(param.name.as_str());
                                    generic_vars.push((param.name.as_str().to_string(), var));
                                    let bounds = Self::extract_trait_bounds(&param.bounds);
                                    if !bounds.is_empty() {
                                        this.inner.borrow_mut().generic_trait_bounds.insert(var, bounds);
                                    }
                                }
                                match &variant.value {
                                    Ty::Tuple(tuple_ty) => {
                                        for (idx, expected_ty) in tuple_ty
                                            .types
                                            .iter()
                                            .enumerate()
                                            .take(element_vars.len())
                                        {
                                            let expected_var =
                                                this.type_from_ast_ty(expected_ty).await?;
                                            this.unify(element_vars[idx], expected_var).await?;
                                        }
                                    }
                                    _ if element_vars.len() == 1 => {
                                        let expected_var = this.type_from_ast_ty(&variant.value).await?;
                                        this.unify(element_vars[0], expected_var).await?;
                                    }
                                    _ => {}
                                }

                                let mut args = Vec::with_capacity(generic_vars.len());
                                for (_, var) in &generic_vars {
                                    let ty = this
                                        .resolve_to_ty(*var).await
                                        .unwrap_or(Ty::Unknown(TypeUnknown));
                                    args.push(ty);
                                }
                                let concrete = this.apply_generic_args_to_enum(&enum_def, &args);
                                this.exit_scope();
                                let enum_var = this.fresh_type_var();
                                this.bind(enum_var, Ty::Enum(concrete));
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
                                let mut enum_def = None;
                                if let Some(enum_key) =
                                    this.resolve_segments_key(path.prefix, &enum_segments)
                                {
                                    enum_def = this.own_enum_defs().get(&enum_key).cloned();
                                }
                                if enum_def.is_none() {
                                    let enum_name = enum_segments.join("::");
                                    enum_def = this
                                        .lookup_enum_def_by_name(&enum_name)
                                        .map(|(_, def)| def);
                                }
                                if let Some(enum_def) = enum_def {
                                    let enum_var = this.fresh_type_var();

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
                                                this.enter_scope();
                                                let mut generic_vars: Vec<(String, TypeVarId)> =
                                                    Vec::new();
                                                for param in &enum_def.generics_params {
                                                    let var = this.register_generic_param(
                                                        param.name.as_str(),
                                                    );
                                                    generic_vars.push((
                                                        param.name.as_str().to_string(),
                                                        var,
                                                    ));
                                                    let bounds =
                                                        Self::extract_trait_bounds(&param.bounds);
                                                    if !bounds.is_empty() {
                                                        this.inner
                                                            .borrow_mut()
                                                            .generic_trait_bounds
                                                            .insert(var, bounds);
                                                    }
                                                }
                                                for field in &mut pat.fields {
                                                    if let Some(expected_field) = structural
                                                        .fields
                                                        .iter()
                                                        .find(|f| f.name == field.name)
                                                    {
                                                        let expected_var = this.type_from_ast_ty(
                                                            &expected_field.value,
                                                        ).await?;
                                                        if let Some(rename) = field.rename.as_mut()
                                                        {
                                                            let child =
                                                                this.infer_pattern(rename).await?;
                                                            bindings.extend(child.bindings);
                                                            this.unify(child.var, expected_var).await?;
                                                        } else {
                                                            let var = this.fresh_type_var();
                                                            this.insert_env(
                                                                field.name.as_str().to_string(),
                                                                EnvEntry::Mono(var),
                                                            );
                                                            this.unify(var, expected_var).await?;
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
                                                let mut args =
                                                    Vec::with_capacity(generic_vars.len());
                                                for (_, var) in &generic_vars {
                                                    let ty = this
                                                        .resolve_to_ty(*var).await
                                                        .unwrap_or(Ty::Unknown(TypeUnknown));
                                                    args.push(ty);
                                                }
                                                let concrete = this
                                                    .apply_generic_args_to_enum(&enum_def, &args);
                                                this.exit_scope();
                                                this.bind(enum_var, Ty::Enum(concrete));
                                                return Ok(PatternInfo {
                                                    var: enum_var,
                                                    bindings,
                                                });
                                            }
                                        }
                                    }

                                    this.bind(enum_var, Ty::Enum(enum_def.clone()));
                                    return Ok(PatternInfo::new(enum_var));
                                }
                            }
                        }
                        // Struct patterns are lowered as `PatternKind::Variant` with a
                        // single-segment path and structural payload. Bind fields against
                        // known struct definitions so identifiers enter the environment.
                        let resolved = this.resolve_locator_key(locator);
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
                            let struct_def = this.own_struct_defs().get(&struct_name).cloned();
                            if let Some(struct_def) = struct_def {
                                let struct_var = this.fresh_type_var();
                                this.bind(struct_var, Ty::Struct(struct_def.clone()));
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
                                                    this.type_from_ast_ty(&def_field.value).await?;
                                                if let Some(rename) = field.rename.as_mut() {
                                                    let child = this.infer_pattern(rename).await?;
                                                    bindings.extend(child.bindings);
                                                    this.unify(child.var, expected).await?;
                                                } else {
                                                    let var = this.fresh_type_var();
                                                    this.insert_env(
                                                        field.name.as_str().to_string(),
                                                        EnvEntry::Mono(var),
                                                    );
                                                    this.unify(var, expected).await?;
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
                        let var = this.fresh_type_var();
                        PatternInfo::new(var)
                    }
                    _ => {
                        // Literal pattern.
                        let lit_var = this.infer_expr_inner(&mut variant.name).await?;
                        PatternInfo::new(lit_var)
                    }
                }
            }
            _ => {
                this.emit_error("pattern is not supported by type inference");
                PatternInfo::new(this.error_type_var())
            }
        };
        if let Some(ty) = existing_ty.as_ref() {
            let var = this.type_from_ast_ty(ty).await?;
            this.unify(info.var, var).await?;
        }
        Ok(info)
        })
    }

    async fn lookup_struct_method(&self, obj_var: TypeVarId, field: &Ident, args: &mut [Expr]) -> Result<TypeVarId> {
        let ty = self.resolve_to_ty(obj_var).await?;
        let resolved_ty = Self::peel_reference(ty.clone());
        let (type_name, struct_path) = match &resolved_ty {
            Ty::Struct(struct_ty) => {
                let name = struct_ty.name.as_str().to_string();
                (name.clone(), QualifiedPath::new(vec![name]))
            }
            Ty::Enum(enum_ty) => {
                let name = enum_ty.name.as_str().to_string();
                (name.clone(), QualifiedPath::new(vec![name]))
            }
            _ => {
                if let Some(var) = self.lookup_trait_method_for_receiver(obj_var, field).await? {
                    return Ok(var);
                }
                if matches!(resolved_ty, Ty::Any(_) | Ty::Unknown(_)) {
                    if let Some(var) = self.lookup_unique_trait_method(field).await? {
                        return Ok(var);
                    }
                    if self.inner.borrow().lossy_mode {
                        return Ok(self.fresh_type_var());
                    }
                }
                self.emit_error(format!(
                    "cannot call method {} on value of type {}",
                    field, resolved_ty
                ));
                return Ok(self.error_type_var());
            }
        };
        if std::env::var("FP_DEBUG_UNWRAP").is_ok() && field.as_str() == "unwrap" {
            eprintln!(
                "debug unwrap pre: resolved_ty={:?} type_name={}",
                resolved_ty, type_name
            );
        }
        let is_result_like = match &resolved_ty {
            Ty::Enum(enum_ty) => {
                let mut has_ok = false;
                let mut has_err = false;
                for variant in &enum_ty.variants {
                    match variant.name.as_str() {
                        "Ok" => has_ok = true,
                        "Err" => has_err = true,
                        _ => {}
                    }
                }
                has_ok && has_err
            }
            _ => false,
        };
        let mut sig_found: Option<FunctionSignature> = None;
        let method_sigs = match &resolved_ty {
            Ty::Struct(s) => s.method_sigs.clone(),
            Ty::Enum(_) => Vec::new(),
            _ => Vec::new(),
        };
        if let Some((_, sig)) = method_sigs.iter().find(|(n, _)| n == field.as_str()) {
            sig_found = Some(sig.clone());
        }

        if let Some(sig) = sig_found {
            if sig.receiver.is_some() {
                let rec_ty = resolved_ty.clone();
                let receiver_var = self.type_from_ast_ty(&rec_ty).await?;
                let expect_ref = matches!(rec_ty, Ty::Reference(_));
                let actual_ref = matches!(ty, Ty::Reference(_));
                if !expect_ref || actual_ref {
                    self.unify(obj_var, receiver_var).await?;
                }
            }
            if !sig.generics_params.is_empty() {
                if let Ok(scheme) = self.scheme_from_method_signature(&sig).await {
                    return Ok(self.instantiate_scheme(&scheme).await);
                }
            }
            // Type the invoke arguments against the method params
            if args.len() == sig.params.len() {
                for (arg_expr, param) in args.iter_mut().zip(sig.params.iter()) {
                    let arg_var = self.infer_expr_inner(arg_expr).await?;
                    let param_var = self.type_from_ast_ty(&param.ty).await?;
                    self.unify(arg_var, param_var).await?;
                }
            }
            let ret_var = if let Some(ret_ty) = &sig.ret_ty {
                self.type_from_ast_ty(ret_ty).await?
            } else {
                let unit = self.fresh_type_var();
                self.bind(unit, Ty::Unit(TypeUnit));
                unit
            };
            let sig_params: Vec<_> = sig.params.iter().map(|p| p.ty.clone()).collect();
            let fn_ty = Ty::Function(TypeFunction {
                params: sig_params,
                generics_params: Vec::new(),
                ret_ty: Some(Box::new(Ty::infer_var(ret_var))),
            });
            let fn_var = self.type_from_ast_ty(&fn_ty).await?;
            return Ok(fn_var);
        }

        if type_name == "Result" || is_result_like {
            match field.as_str() {
                "is_ok" | "is_err" => {
                    let result_var = self.fresh_type_var();
                    self.bind(result_var, Ty::Primitive(TypePrimitive::Bool));
                    let fn_var = self.fresh_type_var();
                    self.bind_function_term(fn_var, Vec::new(), result_var);
                    return Ok(fn_var);
                }
                "unwrap" => {
                    if std::env::var("FP_DEBUG_UNWRAP").is_ok() {
                        eprintln!("debug unwrap: resolved_ty={:?}", resolved_ty);
                    }
                    let ret_var = if let Ty::Enum(enum_ty) = &resolved_ty {
                        let expected = self
                            .resolve_enum_variant_expected_value(enum_ty, "Ok")?
                            .or_else(|| {
                                enum_ty
                                    .variants
                                    .iter()
                                    .find(|variant| variant.name.as_str() == "Ok")
                                    .map(|variant| variant.value.clone())
                            });
                        if let Some(expected) = expected {
                            if enum_ty.generics_params.is_empty() {
                                self.type_from_ast_ty(&expected).await?
                            } else {
                                self.enter_scope();
                                for param in &enum_ty.generics_params {
                                    let var = self.register_generic_param(param.name.as_str());
                                    let bounds = Self::extract_trait_bounds(&param.bounds);
                                    if !bounds.is_empty() {
                                        self.inner.borrow_mut().generic_trait_bounds.insert(var, bounds);
                                    }
                                }
                                let ret = self.type_from_ast_ty(&expected).await;
                                self.exit_scope();
                                ret?
                            }
                        } else {
                            self.fresh_type_var()
                        }
                    } else {
                        self.fresh_type_var()
                    };
                    let fn_var = self.fresh_type_var();
                    self.bind_function_term(fn_var, Vec::new(), ret_var);
                    return Ok(fn_var);
                }
                _ => {}
            }
        }

        if let Some(var) = self.lookup_env_var(field.as_str()).await {
            return Ok(var);
        }
        if type_name == "Option" {
            match field.as_str() {
                "is_some" | "is_none" => {
                    let result_var = self.fresh_type_var();
                    self.bind(result_var, Ty::Primitive(TypePrimitive::Bool));
                    let fn_var = self.fresh_type_var();
                    self.bind_function_term(fn_var, Vec::new(), result_var);
                    return Ok(fn_var);
                }
                _ => {}
            }
        }
        if type_name == "ProcessResult" {
            match field.as_str() {
                "success" => {
                    let result_var = self.fresh_type_var();
                    self.bind(result_var, Ty::Primitive(TypePrimitive::Bool));
                    let fn_var = self.fresh_type_var();
                    self.bind_function_term(fn_var, Vec::new(), result_var);
                    return Ok(fn_var);
                }
                "status" => {
                    let result_var = self.fresh_type_var();
                    self.bind(result_var, Ty::Primitive(TypePrimitive::Int(TypeInt::I64)));
                    let fn_var = self.fresh_type_var();
                    self.bind_function_term(fn_var, Vec::new(), result_var);
                    return Ok(fn_var);
                }
                "stdout" | "stderr" => {
                    let string_var = self.fresh_type_var();
                    self.bind(string_var, Ty::Primitive(TypePrimitive::String));
                    let ref_var = self.fresh_type_var();
                    self.bind_reference_term(ref_var, string_var);
                    let fn_var = self.fresh_type_var();
                    self.bind_function_term(fn_var, Vec::new(), ref_var);
                    return Ok(fn_var);
                }
                "into_stdout" | "into_stderr" => {
                    let result_var = self.fresh_type_var();
                    self.bind(result_var, Ty::Primitive(TypePrimitive::String));
                    let fn_var = self.fresh_type_var();
                    self.bind_function_term(fn_var, Vec::new(), result_var);
                    return Ok(fn_var);
                }
                _ => {}
            }
        }
        self.emit_error(format!(
            "unknown method {} on struct {}",
            field,
            struct_path.to_key()
        ));
        Ok(self.error_type_var())
    }

    async fn lookup_unique_trait_method(&self, field: &Ident) -> Result<Option<TypeVarId>> {
        let mut found: Option<(String, FunctionSignature)> = None;
        let trait_method_sigs = self.inner.borrow().trait_method_sigs.clone();
        for (trait_name, methods) in &trait_method_sigs {
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
        let scheme = self.scheme_from_method_signature(&sig).await?;
        Ok(Some(self.instantiate_scheme(&scheme).await))
    }

    fn trait_name_candidates(&self, trait_name: &str) -> Vec<String> {
        let mut candidates = Vec::new();
        candidates.push(trait_name.to_string());
        if let Some(last) = trait_name.rsplit("::").next() {
            if last != trait_name {
                candidates.push(last.to_string());
            }
        }
        candidates
    }

    async fn lookup_trait_method_for_receiver(
        &self,
        obj_var: TypeVarId,
        field: &Ident,
    ) -> Result<Option<TypeVarId>> {
        let mut receiver = obj_var;
        loop {
            let root = self.find(receiver);
            let root_kind = self.inner.borrow().type_vars[root].kind.clone();
            match root_kind {
                crate::typing::unify::TypeVarKind::Bound(ty) => {
                    if let Some(inner) = self.reference_inner_from_ty(&ty).await {
                        receiver = inner;
                    } else {
                        receiver = root;
                        break;
                    }
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

        let Some(traits) = self.inner.borrow().generic_trait_bounds.get(&receiver).cloned() else {
            return Ok(None);
        };

        for trait_name in traits {
            for candidate in self.trait_name_candidates(&trait_name) {
                let Some(sig) = self
                    .inner
                    .borrow()
                    .trait_method_sigs
                    .get(&candidate)
                    .and_then(|methods| methods.get(field.as_str()))
                    .cloned()
                else {
                    continue;
                };
                let scheme = self.scheme_from_method_signature(&sig).await?;
                return Ok(Some(self.instantiate_scheme(&scheme).await));
            }
        }
        Ok(None)
    }

    async fn try_infer_primitive_method(
        &self,
        obj_var: TypeVarId,
        field: &Ident,
        arg_len: usize,
    ) -> Result<Option<TypeVarId>> {
        match field.name.as_str() {
            "len" if arg_len == 0 => {
                let obj_ty = match self.resolve_to_ty(obj_var).await {
                    Ok(ty) => Self::peel_reference(ty),
                    Err(_) => return Ok(None),
                };
                match obj_ty {
                    Ty::Vec(_)
                    | Ty::Slice(_)
                    | Ty::Array(_)
                    | Ty::Primitive(TypePrimitive::String) => {
                        let result_var = self.fresh_type_var();
                        self.bind(result_var, Ty::Primitive(TypePrimitive::Int(TypeInt::I64)));
                        Ok(Some(result_var))
                    }
                    _ => Ok(None),
                }
            }
            "push" if arg_len == 1 => {
                let obj_ty = match self.resolve_to_ty(obj_var).await {
                    Ok(ty) => Self::peel_reference(ty),
                    Err(_) => return Ok(None),
                };
                if matches!(obj_ty, Ty::Vec(_)) {
                    let unit_var = self.fresh_type_var();
                    self.bind(unit_var, Ty::Unit(TypeUnit));
                    Ok(Some(unit_var))
                } else {
                    Ok(None)
                }
            }
            "to_string" => {
                if arg_len != 0 {
                    return Ok(None);
                }
                let obj_ty = match self.resolve_to_ty(obj_var).await {
                    Ok(ty) => Self::peel_reference(ty),
                    Err(_) => return Ok(None),
                };
                let result_var = self.fresh_type_var();
                self.bind(result_var, Ty::Primitive(TypePrimitive::String));
                match obj_ty {
                    Ty::Primitive(TypePrimitive::String)
                    | Ty::Primitive(TypePrimitive::Bool)
                    | Ty::Primitive(TypePrimitive::Char)
                    | Ty::Primitive(TypePrimitive::Int(_))
                    | Ty::Primitive(TypePrimitive::Decimal(_)) => Ok(Some(result_var)),
                    _ => Ok(None),
                }
            }
            "starts_with" | "ends_with" | "contains" => {
                if arg_len != 1 {
                    return Ok(None);
                }
                let obj_ty = match self.resolve_to_ty(obj_var).await {
                    Ok(ty) => Self::peel_reference(ty),
                    Err(_) => return Ok(None),
                };
                if matches!(obj_ty, Ty::Primitive(TypePrimitive::String)) {
                    let result_var = self.fresh_type_var();
                    self.bind(result_var, Ty::Primitive(TypePrimitive::Bool));
                    return Ok(Some(result_var));
                }
                Ok(None)
            }
            "replace" => {
                if arg_len != 2 {
                    return Ok(None);
                }
                let obj_ty = match self.resolve_to_ty(obj_var).await {
                    Ok(ty) => Self::peel_reference(ty),
                    Err(_) => return Ok(None),
                };
                if matches!(obj_ty, Ty::Primitive(TypePrimitive::String)) {
                    let result_var = self.fresh_type_var();
                    self.bind(result_var, Ty::Primitive(TypePrimitive::String));
                    return Ok(Some(result_var));
                }
                Ok(None)
            }
            "split" => {
                if arg_len != 1 {
                    return Ok(None);
                }
                let obj_ty = match self.resolve_to_ty(obj_var).await {
                    Ok(ty) => Self::peel_reference(ty),
                    Err(_) => return Ok(None),
                };
                if !matches!(obj_ty, Ty::Primitive(TypePrimitive::String)) {
                    return Ok(None);
                }
                let elem_var = self.fresh_type_var();
                self.bind(elem_var, Ty::Primitive(TypePrimitive::String));
                let result_var = self.fresh_type_var();
                self.bind_vec_term(result_var, elem_var);
                Ok(Some(result_var))
            }
            "join" => {
                if arg_len != 1 {
                    return Ok(None);
                }
                let obj_ty = match self.resolve_to_ty(obj_var).await {
                    Ok(ty) => Self::peel_reference(ty),
                    Err(_) => return Ok(None),
                };
                match obj_ty {
                    Ty::Vec(_) | Ty::Slice(_) => {
                        let result_var = self.fresh_type_var();
                        self.bind(result_var, Ty::Primitive(TypePrimitive::String));
                        Ok(Some(result_var))
                    }
                    _ => Ok(None),
                }
            }
            // Keep iterator methods permissive for now; these are primarily
            // used by examples and desugar into Rust iterator chains.
            "iter" | "enumerate" => {
                if arg_len != 0 {
                    return Ok(None);
                }
                let obj_ty = match self.resolve_to_ty(obj_var).await {
                    Ok(ty) => Self::peel_reference(ty),
                    Err(_) => {
                        return Ok(Some(self.fresh_type_var()));
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
                        return Ok(Some(self.fresh_type_var()));
                    }
                };

                let iter_elem_var = if field.name.as_str() == "enumerate" {
                    let index_var = self.fresh_type_var();
                    self.bind(index_var, Ty::Primitive(TypePrimitive::Int(TypeInt::I64)));
                    let value_var = self.type_from_ast_ty(&elem_ty).await?;
                    let tuple_var = self.fresh_type_var();
                    self.bind_tuple_term(tuple_var, vec![index_var, value_var]);
                    tuple_var
                } else {
                    self.type_from_ast_ty(&elem_ty).await?
                };

                let result_var = self.fresh_type_var();
                self.bind_vec_term(result_var, iter_elem_var);
                Ok(Some(result_var))
            }
            _ => Ok(None),
        }
    }

    async fn iter_element_var_from_ty(&self, ty: &Ty) -> Option<TypeVarId> {
        match ty {
            Ty::Vec(vec) => self.type_from_ast_ty(&vec.ty).await.ok(),
            Ty::Array(array) => self.type_from_ast_ty(&array.elem).await.ok(),
            Ty::Slice(slice) => self.type_from_ast_ty(&slice.elem).await.ok(),
            _ => None,
        }
    }

    pub(crate) async fn lookup_struct_field(
        &self,
        obj_var: TypeVarId,
        field: &Ident,
    ) -> Result<TypeVarId> {
        let ty = self.resolve_to_ty(obj_var).await?;
        let resolved_ty = Self::peel_reference(ty);
        match resolved_ty {
            Ty::Quote(ref quote) if quote.kind == QuoteFragmentKind::Item => match field
                .name
                .as_str()
            {
                "name" => Ok(self.borrowed_string_var()),
                "len" | "count" => {
                    let var = self.fresh_type_var();
                    self.bind(var, Ty::Primitive(TypePrimitive::Int(TypeInt::I64)));
                    Ok(var)
                }
                "value" | "fn" => {
                    if matches!(quote.item, Some(QuoteItemKind::Function) | None) {
                        let fn_ty = Ty::Function(TypeFunction {
                            params: Vec::new(),
                            generics_params: Vec::new(),
                            ret_ty: Some(Box::new(Ty::Unit(TypeUnit))),
                        });
                        let var = self.type_from_ast_ty(&fn_ty).await?;
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
            Ty::Type(_) if field.name.as_str() == "name" => Ok(self.borrowed_string_var()),
            Ty::Type(_) if field.name.as_str() == "methods" => {
                let result_var = self.fresh_type_var();
                let elem_var = self.borrowed_string_var();
                self.bind_vec_term(result_var, elem_var);
                Ok(result_var)
            }
            Ty::Type(_) if field.name.as_str() == "size" => {
                let result_var = self.fresh_type_var();
                self.bind(result_var, Ty::Primitive(TypePrimitive::Int(TypeInt::I64)));
                Ok(result_var)
            }
            Ty::Type(ref tt) => {
                // For const-block type aliases, the struct may have been
                // resolved via comptime eval since the field access was typed.
                // Try to resolve the inner struct from resolved_types.
                if let Some(inner) = &tt.inner {
                    if let Ty::Struct(ref struct_ty) = inner.as_ref() {
                        if let Some(def_field) = struct_ty.fields.iter().find(|f| f.name == *field) {
                            let var = self.type_from_ast_ty(&def_field.value).await?;
                            return Ok(var);
                        }
                    }
                }
                // Look up by struct name from resolved_types
                let struct_name = {
                    let ty = self.resolve_to_ty(obj_var).await.ok();
                    ty.and_then(|t| match t {
                        Ty::Type(TypeType { inner: Some(inner), .. }) => {
                            if let Ty::Struct(s) = *inner {
                                Some(s.name.as_str().to_string())
                            } else { None }
                        }
                        _ => None,
                    })
                };
                if let Some(name) = struct_name {
                    // Genuinely awaits this alias's struct shape if it isn't
                    // resolved yet (its own independently-spawned task --
                    // see `predeclare_item`/`await_struct_alias`), instead of
                    // returning a placeholder and hoping a whole-module
                    // retry fixes it later.
                    let struct_ty = self.await_struct_alias(&name).await?;
                    if let Some(def_field) = struct_ty.fields.iter().find(|f| f.name == *field) {
                        let var = self.type_from_ast_ty(&def_field.value).await?;
                        return Ok(var);
                    }
                }
                if tt.inner.is_none() {
                    let placeholder = self.fresh_type_var();
                    self.bind(placeholder, Ty::Type(TypeType::new(fp_core::span::Span::null())));
                    return Ok(placeholder);
                }
                self.emit_error(format!(
                    "cannot access field {} on value of type {}",
                    field, resolved_ty
                ));
                Ok(self.error_type_var())
            }
            Ty::Struct(struct_ty) => {
                if let Some(def_field) = struct_ty.fields.iter().find(|f| f.name == *field) {
                    let var = self.type_from_ast_ty(&def_field.value).await?;
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
                    let var = self.type_from_ast_ty(&def_field.value).await?;
                    Ok(var)
                } else {
                    self.emit_error(format!("unknown field {}", field));
                    Ok(self.error_type_var())
                }
            }
            other => {
                if self.inner.borrow().lossy_mode && matches!(other, Ty::Any(_) | Ty::Unknown(_)) {
                    Ok(self.fresh_type_var())
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

    pub(crate) async fn resolve_struct_literal(
        &self,
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
        let own_def = candidates
            .iter()
            .find_map(|name| self.own_struct_defs().get(name).cloned());
        let def = match own_def {
            Some(def) => Some(def),
            None => self
                .lookup_struct_def_by_name(&struct_name.to_key())
                .await
                .map(|(_, def)| def),
        };
        if let Some(def) = def {
            let var = self.fresh_type_var();
            self.bind(var, Ty::Struct(def.clone()));
            for field in &mut struct_expr.fields {
                if let Some(value) = field.value.as_mut() {
                    let value_var = self.infer_expr_inner(value).await?;
                    if let Some(struct_field) = def.fields.iter().find(|f| f.name == field.name) {
                        let ty_var = self.type_from_ast_ty(&struct_field.value).await?;
                        self.unify(value_var, ty_var).await?;
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
                        let enum_def = self.own_enum_defs().get(&enum_key).cloned();
                        if let Some(enum_def) = enum_def {
                            if let Some(variant) = enum_def
                                .variants
                                .iter()
                                .find(|v| v.name.as_str() == variant_name)
                            {
                                if let Ty::Structural(structural) = &variant.value {
                                    for field in &mut struct_expr.fields {
                                        if let Some(value) = field.value.as_mut() {
                                            let value_var = self.infer_expr_inner(value).await?;
                                            if let Some(def_field) = structural
                                                .fields
                                                .iter()
                                                .find(|f| f.name == field.name)
                                            {
                                                let expected =
                                                    self.type_from_ast_ty(&def_field.value).await?;
                                                self.unify(value_var, expected).await?;
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
                                    self.bind(var, Ty::Enum(enum_def));
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

    fn type_fields_list_var(&self) -> Result<TypeVarId> {
        let result_var = self.fresh_type_var();
        let fields = vec![
            StructuralField::new(Ident::new("name".to_string()), Self::borrowed_string_ty()),
            StructuralField::new(
                Ident::new("ty".to_string()),
                Ty::Type(TypeType::new(Span::null())),
            ),
        ];
        let struct_ty = TypeStructural { fields };
        let elem_var = self.fresh_type_var();
        self.bind(elem_var, Ty::Structural(struct_ty));
        self.bind_vec_term(result_var, elem_var);
        Ok(result_var)
    }

    fn borrowed_string_ty() -> Ty {
        Ty::Reference(TypeReference {
            ty: Box::new(Ty::Primitive(TypePrimitive::String)),
            mutability: None,
            lifetime: None,
        })
    }

    pub(crate) fn borrowed_string_var(&self) -> TypeVarId {
        let string_var = self.fresh_type_var();
        self.bind(string_var, Ty::Primitive(TypePrimitive::String));
        let ref_var = self.fresh_type_var();
        self.bind_reference_term(ref_var, string_var);
        ref_var
    }

    async fn resolve_struct_literal_as_enum_variant(
        &self,
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
                    .and_then(|key| self.own_struct_defs().get(key).cloned())
                    .or_else(|| {
                        locator.as_ident().and_then(|ident| {
                            self.own_struct_defs()
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
                let value_var = self.infer_expr_inner(value).await?;
                if let Some(def_field) = structural.iter().find(|f| f.name == field.name) {
                    let expected = self.type_from_ast_ty(&def_field.value).await?;
                    self.unify(value_var, expected).await?;
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
        self.bind(var, Ty::Enum(enum_ty.clone()));
        Ok(Some(var))
    }

    async fn lookup_env_name(&self, locator: &Name) -> Result<Option<TypeVarId>> {
        let key = locator.to_string();
        let mut poly_ty: Option<Ty> = None;
        let env = self.inner.borrow().env.clone();
        for scope in env.iter().rev() {
            if let Some(entry) = scope.get(&key) {
                match entry {
                    EnvEntry::Mono(var) => return Ok(Some(*var)),
                    EnvEntry::Poly(ty) if matches!(ty, Ty::Struct(_) | Ty::Type(_)) => {
                        poly_ty = Some(ty.clone());
                        break;
                    }
                    _ => {}
                }
            }
        }
        if let Some(ty) = poly_ty {
            let var = self.fresh_type_var();
            self.bind(var, ty);
            return Ok(Some(var));
        }
        Ok(None)
    }

    async fn resolve_struct_literal_from_def(
        &self,
        struct_expr: &mut ExprStruct,
        struct_def: &TypeStruct,
    ) -> Result<Option<TypeVarId>> {
        if struct_expr.fields.len() != struct_def.fields.len() {
            return Ok(None);
        }
        // Sort def fields by name
        let mut def_fields: Vec<_> = struct_def.fields.iter().collect();
        def_fields.sort_by_key(|f| f.name.as_str());
        // Collect expression field names for matching
        let field_names: Vec<_> = struct_expr.fields.iter()
            .map(|f| f.name.as_str().to_string())
            .collect();
        // Type-check each field
        for (_i, def_field) in def_fields.iter().enumerate() {
            let pos = field_names.iter().position(|n| n == def_field.name.as_str());
            let Some(idx) = pos else { return Ok(None) };
            let field_var = self.type_from_ast_ty(&def_field.value).await?;
            if let Some(value) = struct_expr.fields[idx].value.as_mut() {
                let value_var = self.infer_expr_inner(value).await?;
                self.unify(value_var, field_var).await?;
            }
        }
        let result_var = self.type_from_ast_ty(&Ty::Struct(struct_def.clone())).await?;
        Ok(Some(result_var))
    }
}
