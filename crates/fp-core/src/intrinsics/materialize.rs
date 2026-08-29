use crate::ast::{self};
use crate::error::Result as CoreResult;
use crate::intrinsics::IntrinsicMaterializer;
use crate::span::Span;

pub fn materialize_file(
    mut file: ast::File,
    strategy: &dyn IntrinsicMaterializer,
) -> CoreResult<ast::File> {
    strategy.prepare_file(&mut file);
    let mut items = Vec::with_capacity(file.items.len());
    for item in file.items {
        items.push(materialize_item(item, strategy)?);
    }
    file.items = items;
    Ok(file)
}

pub fn materialize_item(
    item: ast::Item,
    strategy: &dyn IntrinsicMaterializer,
) -> CoreResult<ast::Item> {
    let ast::Item { id, span, kind } = item;
    let new_kind = match kind {
        ast::ItemKind::Macro(item) => ast::ItemKind::Macro(item),
        ast::ItemKind::Module(mut module) => {
            let mut items = Vec::with_capacity(module.items.len());
            for child in module.items {
                items.push(materialize_item(child, strategy)?);
            }
            module.items = items;
            ast::ItemKind::Module(module)
        }
        ast::ItemKind::Impl(mut impl_block) => {
            let mut items = Vec::with_capacity(impl_block.items.len());
            for child in impl_block.items {
                items.push(materialize_item(child, strategy)?);
            }
            impl_block.items = items;
            ast::ItemKind::Impl(impl_block)
        }
        ast::ItemKind::DefFunction(mut func) => {
            func.body = materialize_block(func.body, strategy)?;
            ast::ItemKind::DefFunction(func)
        }
        ast::ItemKind::DefConst(mut def) => {
            def.value = Box::new(materialize_expr(*def.value, strategy)?);
            ast::ItemKind::DefConst(def)
        }
        ast::ItemKind::DefStatic(mut def) => {
            def.value = Box::new(materialize_expr(*def.value, strategy)?);
            ast::ItemKind::DefStatic(def)
        }
        ast::ItemKind::Expr(expr) => ast::ItemKind::Expr(materialize_expr(expr, strategy)?),
        ast::ItemKind::DefStruct(_)
        | ast::ItemKind::DefStructural(_)
        | ast::ItemKind::DefEnum(_)
        | ast::ItemKind::DefType(_)
        | ast::ItemKind::OpaqueType(_)
        | ast::ItemKind::DeclConst(_)
        | ast::ItemKind::DeclStatic(_)
        | ast::ItemKind::DeclFunction(_)
        | ast::ItemKind::DeclType(_)
        | ast::ItemKind::Import(_)
        | ast::ItemKind::DefTrait(_)
        | ast::ItemKind::ConstBlock(_)
        | ast::ItemKind::PrecompiledAsm(_)
        | ast::ItemKind::PrecompiledLir(_)
        | ast::ItemKind::PrecompiledArtifact(_) => kind,
    };
    Ok(ast::Item {
        id,
        span,
        kind: new_kind,
    })
}

pub fn materialize_block(
    block: ast::ExprBlock,
    strategy: &dyn IntrinsicMaterializer,
) -> CoreResult<ast::ExprBlock> {
    let mut stmts = Vec::with_capacity(block.stmts.len());
    for stmt in block.stmts {
        stmts.push(materialize_stmt(stmt, strategy)?);
    }
    let mut collected_items = Vec::with_capacity(block.collected_items.len());
    for item in block.collected_items {
        collected_items.push(materialize_item(item, strategy)?);
    }
    Ok(ast::ExprBlock {
        stmts,
        collected_items,
        ..block
    })
}

pub fn materialize_stmt(
    stmt: ast::BlockStmt,
    strategy: &dyn IntrinsicMaterializer,
) -> CoreResult<ast::BlockStmt> {
    match stmt {
        ast::BlockStmt::Expr(mut expr_stmt) => {
            expr_stmt.expr = Box::new(materialize_expr(*expr_stmt.expr, strategy)?);
            Ok(ast::BlockStmt::Expr(expr_stmt))
        }
        ast::BlockStmt::Let(mut stmt_let) => {
            if let Some(init) = stmt_let.init {
                stmt_let.init = Some(materialize_expr(init, strategy)?);
            }
            if let Some(diverge) = stmt_let.diverge {
                stmt_let.diverge = Some(materialize_expr(diverge, strategy)?);
            }
            Ok(ast::BlockStmt::Let(stmt_let))
        }
        ast::BlockStmt::Item(item) => Ok(ast::BlockStmt::Item(Box::new(materialize_item(
            *item, strategy,
        )?))),
        ast::BlockStmt::Defer(mut stmt_defer) => {
            stmt_defer.expr = Box::new(materialize_expr(*stmt_defer.expr, strategy)?);
            Ok(ast::BlockStmt::Defer(stmt_defer))
        }
        ast::BlockStmt::Noop => Ok(ast::BlockStmt::Noop),
    }
}

pub fn materialize_expr(
    expr: ast::Expr,
    strategy: &dyn IntrinsicMaterializer,
) -> CoreResult<ast::Expr> {
    let ast::Expr { id, span, kind } = expr;
    // Looking this expr's resolved type up by its own (preserved-through-
    // materialization — see `new_expr.id = id;` below) id, rather than an
    // `Expr.ty` cache field carried on the node itself (removed): every
    // constructed replacement expr keeps the original id, so a lookup here
    // finds whatever `HirToAstLifter` recorded for this exact node, same as
    // reading `.ty()` used to.
    let expr_ty = crate::ast::resolved_expr_type(id);
    let mut new_expr = match kind {
        ast::ExprKind::Block(block) => {
            ast::Expr::new(ast::ExprKind::Block(materialize_block(block, strategy)?))
        }
        ast::ExprKind::If(mut expr_if) => {
            expr_if.cond = Box::new(materialize_expr(*expr_if.cond, strategy)?);
            expr_if.then = Box::new(materialize_expr(*expr_if.then, strategy)?);
            if let Some(elze) = expr_if.elze {
                expr_if.elze = Some(Box::new(materialize_expr(*elze, strategy)?));
            }
            ast::Expr::new(ast::ExprKind::If(expr_if))
        }
        ast::ExprKind::Loop(mut expr_loop) => {
            expr_loop.body = Box::new(materialize_expr(*expr_loop.body, strategy)?);
            ast::Expr::new(ast::ExprKind::Loop(expr_loop))
        }
        ast::ExprKind::While(mut expr_while) => {
            expr_while.cond = Box::new(materialize_expr(*expr_while.cond, strategy)?);
            expr_while.body = Box::new(materialize_expr(*expr_while.body, strategy)?);
            ast::Expr::new(ast::ExprKind::While(expr_while))
        }
        // Was previously caught by the blanket fallback below (see its
        // removal comment) with zero recursion, meaning e.g. a `Some(x)`/
        // `None` call promoted to `IntrinsicCall(Op(OptionSome/OptionNone))`
        // anywhere inside a `for` loop body never reached
        // `strategy.materialize_call`, and was rendered verbatim by the
        // Kotlin serializer's `op_optionsome`/`op_optionnone`-shaped
        // fallback instead of real Kotlin syntax.
        ast::ExprKind::For(mut expr_for) => {
            expr_for.iter = Box::new(materialize_expr(*expr_for.iter, strategy)?);
            expr_for.body = Box::new(materialize_expr(*expr_for.body, strategy)?);
            ast::Expr::new(ast::ExprKind::For(expr_for))
        }
        ast::ExprKind::Match(mut match_expr) => {
            if let Some(scrutinee) = match_expr.scrutinee {
                match_expr.scrutinee = Some(Box::new(materialize_expr(*scrutinee, strategy)?));
            }
            let mut cases = Vec::with_capacity(match_expr.cases.len());
            for mut case in match_expr.cases {
                case.cond = Box::new(materialize_expr(*case.cond, strategy)?);
                if let Some(guard) = case.guard {
                    case.guard = Some(Box::new(materialize_expr(*guard, strategy)?));
                }
                case.body = Box::new(materialize_expr(*case.body, strategy)?);
                cases.push(case);
            }
            match_expr.cases = cases;
            ast::Expr::new(ast::ExprKind::Match(match_expr))
        }
        ast::ExprKind::Let(mut expr_let) => {
            expr_let.expr = Box::new(materialize_expr(*expr_let.expr, strategy)?);
            ast::Expr::new(ast::ExprKind::Let(expr_let))
        }
        ast::ExprKind::Assign(mut expr_assign) => {
            expr_assign.target = Box::new(materialize_expr(*expr_assign.target, strategy)?);
            expr_assign.value = Box::new(materialize_expr(*expr_assign.value, strategy)?);
            ast::Expr::new(ast::ExprKind::Assign(expr_assign))
        }
        ast::ExprKind::Invoke(mut invoke) => {
            invoke.target = materialize_invoke_target(invoke.target, strategy)?;
            let mut args = Vec::with_capacity(invoke.args.len());
            for arg in invoke.args {
                args.push(materialize_expr(arg, strategy)?);
            }
            invoke.args = args;
            for kwarg in &mut invoke.kwargs {
                let value =
                    std::mem::replace(&mut kwarg.value, ast::Expr::value(ast::Value::unit()));
                kwarg.value = materialize_expr(value, strategy)?;
            }
            if let Some(expr) = strategy.materialize_invoke(&mut invoke, &expr_ty)? {
                materialize_expr(expr, strategy)?
            } else {
                ast::Expr::new(ast::ExprKind::Invoke(invoke))
            }
        }
        ast::ExprKind::Select(mut select) => {
            select.obj = Box::new(materialize_expr(*select.obj, strategy)?);
            ast::Expr::new(ast::ExprKind::Select(select))
        }
        ast::ExprKind::Struct(mut struct_expr) => {
            for field in &mut struct_expr.fields {
                if let Some(value) = field.value.take() {
                    field.value = Some(materialize_expr(value, strategy)?);
                }
            }
            if let Some(new_expr) = strategy.materialize_struct(&mut struct_expr, &expr_ty)? {
                new_expr
            } else {
                ast::Expr::new(ast::ExprKind::Struct(struct_expr))
            }
        }
        ast::ExprKind::Structural(mut struct_expr) => {
            for field in &mut struct_expr.fields {
                if let Some(value) = field.value.take() {
                    field.value = Some(materialize_expr(value, strategy)?);
                }
            }
            if let Some(new_expr) = strategy.materialize_structural(&mut struct_expr, &expr_ty)? {
                new_expr
            } else {
                ast::Expr::new(ast::ExprKind::Structural(struct_expr))
            }
        }
        ast::ExprKind::Array(mut array_expr) => {
            let mut values = Vec::with_capacity(array_expr.values.len());
            for value in array_expr.values {
                values.push(materialize_expr(value, strategy)?);
            }
            array_expr.values = values;
            ast::Expr::new(ast::ExprKind::Array(array_expr))
        }
        ast::ExprKind::ArrayRepeat(mut array_repeat) => {
            array_repeat.elem = Box::new(materialize_expr(*array_repeat.elem, strategy)?);
            array_repeat.len = Box::new(materialize_expr(*array_repeat.len, strategy)?);
            ast::Expr::new(ast::ExprKind::ArrayRepeat(array_repeat))
        }
        ast::ExprKind::Tuple(mut tuple_expr) => {
            let mut values = Vec::with_capacity(tuple_expr.values.len());
            for value in tuple_expr.values {
                values.push(materialize_expr(value, strategy)?);
            }
            tuple_expr.values = values;
            ast::Expr::new(ast::ExprKind::Tuple(tuple_expr))
        }
        ast::ExprKind::BinOp(mut binop) => {
            binop.lhs = Box::new(materialize_expr(*binop.lhs, strategy)?);
            binop.rhs = Box::new(materialize_expr(*binop.rhs, strategy)?);
            ast::Expr::new(ast::ExprKind::BinOp(binop))
        }
        ast::ExprKind::UnOp(mut unop) => {
            unop.val = Box::new(materialize_expr(*unop.val, strategy)?);
            ast::Expr::new(ast::ExprKind::UnOp(unop))
        }
        ast::ExprKind::Reference(mut reference) => {
            reference.referee = Box::new(materialize_expr(*reference.referee, strategy)?);
            ast::Expr::new(ast::ExprKind::Reference(reference))
        }
        ast::ExprKind::Dereference(mut expr_deref) => {
            expr_deref.referee = Box::new(materialize_expr(*expr_deref.referee, strategy)?);
            ast::Expr::new(ast::ExprKind::Dereference(expr_deref))
        }
        ast::ExprKind::Index(mut expr_index) => {
            expr_index.obj = Box::new(materialize_expr(*expr_index.obj, strategy)?);
            expr_index.index = Box::new(materialize_expr(*expr_index.index, strategy)?);
            if is_hashmap_expr(expr_index.obj.as_ref()) {
                build_hashmap_get_expr(expr_index, expr_ty)
            } else {
                ast::Expr::new(ast::ExprKind::Index(expr_index))
            }
        }
        ast::ExprKind::Splat(mut expr_splat) => {
            expr_splat.iter = Box::new(materialize_expr(*expr_splat.iter, strategy)?);
            ast::Expr::new(ast::ExprKind::Splat(expr_splat))
        }
        ast::ExprKind::SplatDict(mut expr_splat) => {
            expr_splat.dict = Box::new(materialize_expr(*expr_splat.dict, strategy)?);
            ast::Expr::new(ast::ExprKind::SplatDict(expr_splat))
        }
        ast::ExprKind::Try(mut expr_try) => {
            expr_try.expr = Box::new(materialize_expr(*expr_try.expr, strategy)?);
            for catch in &mut expr_try.catches {
                let body = std::mem::replace(&mut catch.body, Box::new(ast::Expr::unit()));
                catch.body = Box::new(materialize_expr(*body, strategy)?);
            }
            if let Some(elze) = expr_try.elze.take() {
                expr_try.elze = Some(Box::new(materialize_expr(*elze, strategy)?));
            }
            if let Some(finally) = expr_try.finally.take() {
                expr_try.finally = Some(Box::new(materialize_expr(*finally, strategy)?));
            }
            ast::Expr::new(ast::ExprKind::Try(expr_try))
        }
        // `return Ok(x);`/`break Some(x);` — omitted from this walker
        // before, so a portable op used directly as a `return`/`break`
        // value (rather than immediately bound to a `let` or passed as an
        // ordinary argument) fell into the catch-all below with zero
        // recursion, silently skipping materialization for its inner
        // value entirely.
        ast::ExprKind::Return(mut expr_return) => {
            if let Some(value) = expr_return.value.take() {
                expr_return.value = Some(Box::new(materialize_expr(*value, strategy)?));
            }
            ast::Expr::new(ast::ExprKind::Return(expr_return))
        }
        ast::ExprKind::Break(mut expr_break) => {
            if let Some(value) = expr_break.value.take() {
                expr_break.value = Some(Box::new(materialize_expr(*value, strategy)?));
            }
            ast::Expr::new(ast::ExprKind::Break(expr_break))
        }
        ast::ExprKind::Closure(mut closure) => {
            closure.body = Box::new(materialize_expr(*closure.body, strategy)?);
            ast::Expr::new(ast::ExprKind::Closure(closure))
        }
        ast::ExprKind::Closured(mut closured) => {
            closured.expr = Box::new(materialize_expr(*closured.expr, strategy)?);
            ast::Expr::new(ast::ExprKind::Closured(closured))
        }
        ast::ExprKind::Paren(mut paren) => {
            paren.expr = Box::new(materialize_expr(*paren.expr, strategy)?);
            ast::Expr::new(ast::ExprKind::Paren(paren))
        }
        ast::ExprKind::FormatString(format) => ast::Expr::new(ast::ExprKind::FormatString(format)),
        ast::ExprKind::Item(item) => ast::Expr::new(ast::ExprKind::Item(Box::new(
            materialize_item(*item, strategy)?,
        ))),
        ast::ExprKind::Value(value) => {
            let value = materialize_value(*value, strategy)?;
            ast::Expr::new(ast::ExprKind::Value(Box::new(value)))
        }
        ast::ExprKind::IntrinsicCall(mut call) => {
            let mut args = Vec::with_capacity(call.args.len());
            for arg in call.args.drain(..) {
                args.push(materialize_expr(arg, strategy)?);
            }
            call.args = args;
            for kwarg in &mut call.kwargs {
                let value =
                    std::mem::replace(&mut kwarg.value, ast::Expr::value(ast::Value::unit()));
                kwarg.value = materialize_expr(value, strategy)?;
            }

            if let Some(expr) = strategy.materialize_call(&mut call, &expr_ty)? {
                materialize_expr(expr, strategy)?
            } else {
                ast::Expr::new(ast::ExprKind::IntrinsicCall(call))
            }
        }
        ast::ExprKind::PortableOpCall(mut call) => {
            let mut args = Vec::with_capacity(call.args.len());
            for arg in call.args.drain(..) {
                args.push(materialize_expr(arg, strategy)?);
            }
            call.args = args;
            for kwarg in &mut call.kwargs {
                let value =
                    std::mem::replace(&mut kwarg.value, ast::Expr::value(ast::Value::unit()));
                kwarg.value = materialize_expr(value, strategy)?;
            }
            if strategy.capabilities().portable_operations {
                if let Some(expr) = strategy.materialize_portable_op(&mut call, &expr_ty)? {
                    materialize_expr(expr, strategy)?
                } else {
                    ast::Expr::new(ast::ExprKind::PortableOpCall(call))
                }
            } else {
                ast::Expr::new(ast::ExprKind::PortableOpCall(call))
            }
        }
        ast::ExprKind::IntrinsicContainer(mut collection) => {
            match &mut collection {
                ast::ExprIntrinsicContainer::VecElements { elements } => {
                    let mut next = Vec::with_capacity(elements.len());
                    for element in elements.drain(..) {
                        next.push(materialize_expr(element, strategy)?);
                    }
                    *elements = next;
                }
                ast::ExprIntrinsicContainer::VecRepeat { elem, len } => {
                    let elem_value =
                        std::mem::replace(elem, Box::new(ast::Expr::value(ast::Value::unit())));
                    let len_value =
                        std::mem::replace(len, Box::new(ast::Expr::value(ast::Value::unit())));
                    *elem = Box::new(materialize_expr(*elem_value, strategy)?);
                    *len = Box::new(materialize_expr(*len_value, strategy)?);
                }
                ast::ExprIntrinsicContainer::HashMapEntries { entries } => {
                    for entry in entries.iter_mut() {
                        let key =
                            std::mem::replace(&mut entry.key, ast::Expr::value(ast::Value::unit()));
                        let value = std::mem::replace(
                            &mut entry.value,
                            ast::Expr::value(ast::Value::unit()),
                        );
                        entry.key = materialize_expr(key, strategy)?;
                        entry.value = materialize_expr(value, strategy)?;
                    }
                }
            }

            if let ast::ExprIntrinsicContainer::HashMapEntries { entries } = &collection {
                if is_hashmap_ty_slot(&expr_ty) {
                    return Ok(build_hashmap_from_entries(entries, expr_ty));
                }
            }
            if let Some(new_expr) = strategy.materialize_container(&mut collection, &expr_ty)? {
                new_expr
            } else {
                ast::Expr::new(ast::ExprKind::IntrinsicContainer(collection))
            }
        }
        // Exhaustive from here down instead of a blanket `other => ..`
        // catch-all: that fallback is exactly what let the `For` bug above
        // (and, per its own comment, an earlier `Return`/`Break` bug)
        // silently skip materialization for a whole node's contents rather
        // than failing to compile — the next variant added to `ExprKind`
        // that actually needs recursion will now be a compile error here
        // instead of a silent runtime bug.
        ast::ExprKind::With(mut expr_with) => {
            expr_with.context = Box::new(materialize_expr(*expr_with.context, strategy)?);
            expr_with.body = Box::new(materialize_expr(*expr_with.body, strategy)?);
            ast::Expr::new(ast::ExprKind::With(expr_with))
        }
        ast::ExprKind::Cast(mut expr_cast) => {
            expr_cast.expr = Box::new(materialize_expr(*expr_cast.expr, strategy)?);
            ast::Expr::new(ast::ExprKind::Cast(expr_cast))
        }
        ast::ExprKind::Async(mut expr_async) => {
            expr_async.expr = Box::new(materialize_expr(*expr_async.expr, strategy)?);
            ast::Expr::new(ast::ExprKind::Async(expr_async))
        }
        ast::ExprKind::ConstBlock(mut const_block) => {
            const_block.expr = Box::new(materialize_expr(*const_block.expr, strategy)?);
            let mut collected_items = Vec::with_capacity(const_block.collected_items.len());
            for item in const_block.collected_items {
                collected_items.push(materialize_item(item, strategy)?);
            }
            const_block.collected_items = collected_items;
            ast::Expr::new(ast::ExprKind::ConstBlock(const_block))
        }
        ast::ExprKind::Quote(mut quote) => {
            quote.block = materialize_block(quote.block, strategy)?;
            ast::Expr::new(ast::ExprKind::Quote(quote))
        }
        ast::ExprKind::Splice(mut splice) => {
            splice.token = Box::new(materialize_expr(*splice.token, strategy)?);
            ast::Expr::new(ast::ExprKind::Splice(splice))
        }
        ast::ExprKind::SplicePending(mut pending) => {
            pending.token = Box::new(materialize_expr(*pending.token, strategy)?);
            ast::Expr::new(ast::ExprKind::SplicePending(pending))
        }
        ast::ExprKind::Await(mut expr_await) => {
            expr_await.base = Box::new(materialize_expr(*expr_await.base, strategy)?);
            ast::Expr::new(ast::ExprKind::Await(expr_await))
        }
        ast::ExprKind::Range(mut expr_range) => {
            if let Some(start) = expr_range.start.take() {
                expr_range.start = Some(Box::new(materialize_expr(*start, strategy)?));
            }
            if let Some(end) = expr_range.end.take() {
                expr_range.end = Some(Box::new(materialize_expr(*end, strategy)?));
            }
            if let Some(step) = expr_range.step.take() {
                expr_range.step = Some(Box::new(materialize_expr(*step, strategy)?));
            }
            ast::Expr::new(ast::ExprKind::Range(expr_range))
        }
        // Leaves — no nested `Expr`/`Item` to recurse into.
        ast::ExprKind::Id(id) => ast::Expr::new(ast::ExprKind::Id(id)),
        ast::ExprKind::Name(name) => ast::Expr::new(ast::ExprKind::Name(name)),
        ast::ExprKind::Continue(expr_continue) => {
            ast::Expr::new(ast::ExprKind::Continue(expr_continue))
        }
        // Already fully expanded by fp-lang's macro engine before this pass
        // ever runs (mirrors `materialize_item`'s identical treatment of
        // `ItemKind::Macro`) — nothing left inside to materialize.
        ast::ExprKind::Macro(macro_expr) => ast::Expr::new(ast::ExprKind::Macro(macro_expr)),
    };
    new_expr.id = id;
    new_expr.span = span;
    Ok(new_expr)
}

fn is_hashmap_expr(expr: &ast::Expr) -> bool {
    crate::ast::resolved_expr_type(expr.id())
        .as_ref()
        .map(is_hashmap_ty)
        .unwrap_or(false)
}

fn is_hashmap_ty_slot(ty: &ast::TySlot) -> bool {
    ty.as_ref().map(is_hashmap_ty).unwrap_or(false)
}

fn is_hashmap_ty(ty: &ast::Ty) -> bool {
    match ty {
        ast::Ty::Struct(struct_ty) => struct_ty.name.as_str() == "HashMap",
        ast::Ty::Expr(expr) => match expr.kind() {
            ast::ExprKind::Name(name) => match name {
                ast::Name::Ident(ident) => ident.as_str() == "HashMap",
                ast::Name::Path(path) => path
                    .segments
                    .last()
                    .map(|seg| seg.as_str() == "HashMap")
                    .unwrap_or(false),
                ast::Name::ParameterPath(path) => path
                    .segments
                    .last()
                    .map(|seg| seg.ident.as_str() == "HashMap")
                    .unwrap_or(false),
            },
            _ => false,
        },
        _ => false,
    }
}

fn build_hashmap_get_expr(expr_index: ast::ExprIndex, expr_ty: ast::TySlot) -> ast::Expr {
    let select = ast::ExprSelect {
        obj: expr_index.obj,
        field: ast::Ident::new("get_unchecked"),
        generic_args: Vec::new(),
        select: ast::ExprSelectType::Method,
        span: Span::null(),
    };
    let invoke = ast::ExprInvoke {
        target: ast::ExprInvokeTarget::Method(select),
        args: vec![*expr_index.index],
        kwargs: Vec::new(),
        span: Span::null(),
    };
    let node = ast::Expr::new(ast::ExprKind::Invoke(invoke));
    if let Some(ty) = expr_ty {
        crate::ast::set_resolved_expr_type(node.id(), ty);
    }
    node
}

fn build_hashmap_from_entries(
    entries: &[ast::ExprIntrinsicContainerEntry],
    expr_ty: ast::TySlot,
) -> ast::Expr {
    let mut elements = Vec::with_capacity(entries.len());
    for entry in entries {
        let name = ast::Expr::path(ast::Path::plain(vec![
            ast::Ident::new("std"),
            ast::Ident::new("collections"),
            ast::Ident::new("HashMapEntry"),
        ]));
        let fields = vec![
            ast::ExprField::new(ast::Ident::new("key"), entry.key.clone()),
            ast::ExprField::new(ast::Ident::new("value"), entry.value.clone()),
        ];
        let pair = ast::ExprStruct::new(name.into(), fields);
        elements.push(ast::Expr::new(ast::ExprKind::Struct(pair)));
    }

    let vec_entries = ast::Expr::new(ast::ExprKind::IntrinsicContainer(
        ast::ExprIntrinsicContainer::VecElements { elements },
    ));

    let name = ast::Name::path(ast::Path::plain(vec![
        ast::Ident::new("std"),
        ast::Ident::new("collections"),
        ast::Ident::new("HashMap"),
        ast::Ident::new("from"),
    ]));
    let invoke = ast::ExprInvoke {
        target: ast::ExprInvokeTarget::Function(name),
        args: vec![vec_entries],
        kwargs: Vec::new(),
        span: Span::null(),
    };

    let node = ast::Expr::new(ast::ExprKind::Invoke(invoke));
    if let Some(ty) = expr_ty {
        crate::ast::set_resolved_expr_type(node.id(), ty);
    }
    node
}

pub fn materialize_value(
    value: ast::Value,
    strategy: &dyn IntrinsicMaterializer,
) -> CoreResult<ast::Value> {
    match value {
        ast::Value::Expr(expr) => Ok(ast::Value::Expr(Box::new(materialize_expr(
            *expr, strategy,
        )?))),
        ast::Value::Function(mut func) => {
            func.body = Box::new(materialize_expr(*func.body, strategy)?);
            Ok(ast::Value::Function(func))
        }
        other => Ok(other),
    }
}

pub fn materialize_invoke_target(
    target: ast::ExprInvokeTarget,
    strategy: &dyn IntrinsicMaterializer,
) -> CoreResult<ast::ExprInvokeTarget> {
    match target {
        ast::ExprInvokeTarget::Method(mut select) => {
            select.obj = Box::new(materialize_expr(*select.obj, strategy)?);
            Ok(ast::ExprInvokeTarget::Method(select))
        }
        ast::ExprInvokeTarget::Expr(expr) => Ok(ast::ExprInvokeTarget::Expr(Box::new(
            materialize_expr(*expr, strategy)?,
        ))),
        ast::ExprInvokeTarget::Closure(mut closure) => {
            closure.body = Box::new(materialize_expr(*closure.body, strategy)?);
            Ok(ast::ExprInvokeTarget::Closure(closure))
        }
        other => Ok(other),
    }
}
