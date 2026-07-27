use crate::ast::{
    BlockStmt, Expr, ExprBlock, ExprIntrinsicCall, ExprIntrinsicContainer, ExprInvoke,
    ExprInvokeTarget, ExprKind, ExprStringTemplate, Item, ItemKind, Node, NodeKind, Ty, Value,
};
use crate::error::Result;
use crate::intrinsics::{IntrinsicNormalizer, NoopIntrinsicNormalizer, NormalizeOutcome};
use std::cell::RefCell;
use std::collections::HashMap;
mod format;

thread_local! {
    static CONST_BOOLS: RefCell<HashMap<String, bool>> = RefCell::new(HashMap::new());
}

/// Normalize intrinsic expressions into a canonical AST form so that typing and
/// downstream passes can assume consistent structures.
pub fn normalize_intrinsics(node: &mut Node) -> Result<()> {
    normalize_intrinsics_with(node, &NoopIntrinsicNormalizer)
}

pub fn normalize_intrinsics_with(
    node: &mut Node,
    strategy: &dyn IntrinsicNormalizer,
) -> Result<()> {
    normalize_node(node, strategy)
}

fn normalize_node(node: &mut Node, strategy: &dyn IntrinsicNormalizer) -> Result<()> {
    match node.kind_mut() {
        NodeKind::File(file) => {
            let mut const_bools = scan_const_bools(&file.items);
            scan_items(&file.collected_items, &mut const_bools);
            CONST_BOOLS.with(|cb| *cb.borrow_mut() = const_bools);
            resolve_all_splices(&mut file.items);
            for item in &mut file.items {
                normalize_item(item, strategy)?;
            }
        }
        NodeKind::Item(item) => normalize_item(item, strategy)?,
        NodeKind::Expr(expr) => normalize_expr(expr, strategy)?,
        NodeKind::Schema(_) | NodeKind::Query(_) | NodeKind::Workspace(_) => {}
    }
    Ok(())
}

fn scan_const_bools(items: &[Item]) -> HashMap<String, bool> {
    let mut map = HashMap::new();
    scan_items(items, &mut map);
    map
}

fn scan_items(items: &[Item], map: &mut HashMap<String, bool>) {
    for item in items {
        match item.kind() {
            ItemKind::DefConst(def) => {
                if let ExprKind::Value(v) = def.value.kind() {
                    if let Value::Bool(b) = v.as_ref() {
                        map.insert(def.name.as_str().to_string(), b.value);
                    }
                }
            }
            ItemKind::Module(m) => scan_items(&m.items, map),
            ItemKind::DefFunction(f) => scan_block(&f.body, map),
            _ => {}
        }
    }
}

fn scan_block(expr: &Expr, map: &mut HashMap<String, bool>) {
    if let ExprKind::Block(block) = expr.kind() {
        scan_items(&block.collected_items, map);
        for stmt in &block.stmts {
            match stmt {
                BlockStmt::Item(item) => scan_items(std::slice::from_ref(item), map),
                BlockStmt::Expr(e) => scan_block(&e.expr, map),
                BlockStmt::Let(s) => {
                    if let Some(init) = &s.init {
                        scan_block(init, map);
                    }
                }
                _ => {}
            }
        }
    }
}

fn normalize_def_type_item(
    item: &mut Item,
    strategy: &dyn IntrinsicNormalizer,
    _const_bools: &HashMap<String, bool>,
) -> Result<()> {
    if let ItemKind::DefType(def) = item.kind_mut() {
        if let Ty::Expr(expr) = &mut def.value {
            try_lower_type_builder_const_block(expr);
        }
        normalize_ty(&mut def.value, strategy)?;
    } else {
        normalize_item(item, strategy)?;
    }
    Ok(())
}

fn normalize_item(item: &mut Item, strategy: &dyn IntrinsicNormalizer) -> Result<()> {
    match item.kind_mut() {
        ItemKind::Macro(_) => {}
        ItemKind::Module(module) => {
            for child in &mut module.items {
                normalize_item(child, strategy)?;
            }
        }
        ItemKind::Impl(impl_block) => {
            for child in &mut impl_block.items {
                normalize_item(child, strategy)?;
            }
        }
        ItemKind::DefFunction(function) => {
            for param in &mut function.sig.params {
                if let Some(ty) = param.ty_annotation.as_mut() {
                    normalize_ty(ty, strategy)?;
                }
                normalize_ty(&mut param.ty, strategy)?;
            }
            if let Some(ret_ty) = function.sig.ret_ty.as_mut() {
                normalize_ty(ret_ty, strategy)?;
            }
            normalize_expr(function.body.as_mut(), strategy)?
        }
        ItemKind::DefConst(def) => {
            if let Some(ty) = def.ty_annotation.as_mut() {
                normalize_ty(ty, strategy)?;
            }
            if let Some(ty) = def.ty.as_mut() {
                normalize_ty(ty, strategy)?;
            }
            normalize_expr(def.value.as_mut(), strategy)?;
        }
        ItemKind::DefStatic(def) => {
            if let Some(ty) = def.ty_annotation.as_mut() {
                normalize_ty(ty, strategy)?;
            }
            normalize_ty(&mut def.ty, strategy)?;
            normalize_expr(def.value.as_mut(), strategy)?;
        }
        ItemKind::DefStruct(_)
        | ItemKind::DefStructural(_)
        | ItemKind::DefEnum(_)
        | ItemKind::DeclConst(_)
        | ItemKind::DeclStatic(_)
        | ItemKind::DeclFunction(_)
        | ItemKind::DeclType(_)
        | ItemKind::OpaqueType(_)
        | ItemKind::Import(_)
        | ItemKind::Any(_) => {}
        ItemKind::DefTrait(def_trait) => {
            for child in &mut def_trait.items {
                normalize_item(child, strategy)?;
            }
        }
        ItemKind::DefType(def) => {
            if let Ty::Expr(expr) = &mut def.value {
                try_lower_type_builder_const_block(expr);
            }
            normalize_ty(&mut def.value, strategy)?;
        }
        ItemKind::Expr(expr) => normalize_expr(expr, strategy)?,
    }
    Ok(())
}

fn normalize_block(block: &mut ExprBlock, strategy: &dyn IntrinsicNormalizer) -> Result<()> {
    for stmt in &mut block.stmts {
        match stmt {
            BlockStmt::Expr(expr_stmt) => normalize_expr(expr_stmt.expr.as_mut(), strategy)?,
            BlockStmt::Let(stmt_let) => {
                if let Some(init) = stmt_let.init.as_mut() {
                    normalize_expr(init, strategy)?;
                }
                if let Some(diverge) = stmt_let.diverge.as_mut() {
                    normalize_expr(diverge, strategy)?;
                }
            }
            BlockStmt::Defer(stmt_defer) => normalize_expr(stmt_defer.expr.as_mut(), strategy)?,
            BlockStmt::Item(item) => normalize_item(item.as_mut(), strategy)?,
            BlockStmt::Noop | BlockStmt::Any(_) => {}
        }
    }
    Ok(())
}

fn normalize_expr(expr: &mut Expr, strategy: &dyn IntrinsicNormalizer) -> Result<()> {
    loop {
        let original_span = expr.span;
        let mut replacement: Option<Expr> = None;

        let strat_outcome = match expr.kind() {
            ExprKind::Macro(_) => {
                Some(strategy.normalize_macro(std::mem::replace(expr, Expr::unit()))?)
            }
            ExprKind::IntrinsicCall(_) => {
                Some(strategy.normalize_call(std::mem::replace(expr, Expr::unit()))?)
            }
            ExprKind::IntrinsicContainer(_) => {
                Some(strategy.normalize_container(std::mem::replace(expr, Expr::unit()))?)
            }
            ExprKind::Struct(_) => {
                Some(strategy.normalize_struct(std::mem::replace(expr, Expr::unit()))?)
            }
            ExprKind::Structural(_) => {
                Some(strategy.normalize_structural(std::mem::replace(expr, Expr::unit()))?)
            }
            ExprKind::Invoke(_) => {
                Some(strategy.normalize_invoke(std::mem::replace(expr, Expr::unit()))?)
            }
            _ => None,
        };

        if let Some(outcome) = strat_outcome {
            match outcome {
                NormalizeOutcome::Ignored(mut expr_back) => {
                    if expr_back.span.is_none() {
                        expr_back.span = original_span;
                    }
                    *expr = expr_back;
                }
                NormalizeOutcome::Normalized(mut expr_new) => {
                    if expr_new.span.is_none() {
                        expr_new.span = original_span;
                    }
                    *expr = expr_new;
                    continue;
                }
            }
        }

        match expr.kind_mut() {
            ExprKind::Block(block) => normalize_block(block, strategy)?,
            ExprKind::If(expr_if) => {
                normalize_expr(expr_if.cond.as_mut(), strategy)?;
                normalize_expr(expr_if.then.as_mut(), strategy)?;
                if let Some(else_expr) = expr_if.elze.as_mut() {
                    normalize_expr(else_expr, strategy)?;
                }
            }
            ExprKind::Loop(expr_loop) => normalize_expr(expr_loop.body.as_mut(), strategy)?,
            ExprKind::For(expr_for) => {
                normalize_expr(expr_for.iter.as_mut(), strategy)?;
                normalize_expr(expr_for.body.as_mut(), strategy)?;
            }
            ExprKind::While(expr_while) => {
                normalize_expr(expr_while.cond.as_mut(), strategy)?;
                normalize_expr(expr_while.body.as_mut(), strategy)?;
            }
            ExprKind::With(expr_with) => {
                normalize_expr(expr_with.context.as_mut(), strategy)?;
                normalize_expr(expr_with.body.as_mut(), strategy)?;
            }
            ExprKind::Return(expr_return) => {
                if let Some(value) = expr_return.value.as_mut() {
                    normalize_expr(value.as_mut(), strategy)?;
                }
            }
            ExprKind::Break(expr_break) => {
                if let Some(value) = expr_break.value.as_mut() {
                    normalize_expr(value.as_mut(), strategy)?;
                }
            }
            ExprKind::Continue(_) => {}
            ExprKind::ConstBlock(const_block) => {
                normalize_expr(const_block.expr.as_mut(), strategy)?;
            }
            ExprKind::Match(expr_match) => {
                for case in &mut expr_match.cases {
                    normalize_expr(case.cond.as_mut(), strategy)?;
                    normalize_expr(case.body.as_mut(), strategy)?;
                }
            }
            ExprKind::Let(expr_let) => normalize_expr(expr_let.expr.as_mut(), strategy)?,
            ExprKind::Macro(_) => {}
            ExprKind::Assign(assign) => {
                normalize_expr(assign.target.as_mut(), strategy)?;
                normalize_expr(assign.value.as_mut(), strategy)?;
            }
            ExprKind::Cast(cast) => {
                normalize_expr(cast.expr.as_mut(), strategy)?;
            }
            ExprKind::Invoke(invoke) => {
                normalize_invoke(invoke, strategy)?;

                if let Some(intrinsic_call) = crate::ast::intrinsic_call_from_invoke(invoke) {
                    replacement = Some(Expr::new(ExprKind::IntrinsicCall(intrinsic_call)));
                } else if let Some(mut collection) = ExprIntrinsicContainer::from_invoke(invoke) {
                    let new_expr = apply_intrinsic_collection(&mut collection, strategy)?;
                    replacement = Some(new_expr);
                }
            }
            ExprKind::Await(await_expr) => normalize_expr(await_expr.base.as_mut(), strategy)?,
            ExprKind::Select(select) => normalize_expr(select.obj.as_mut(), strategy)?,
            ExprKind::Struct(struct_expr) => {
                for field in &mut struct_expr.fields {
                    if let Some(value) = field.value.as_mut() {
                        normalize_expr(value, strategy)?;
                    }
                }
            }
            ExprKind::Structural(struct_expr) => {
                for field in &mut struct_expr.fields {
                    if let Some(value) = field.value.as_mut() {
                        normalize_expr(value, strategy)?;
                    }
                }
            }
            ExprKind::Array(array_expr) => {
                for value in &mut array_expr.values {
                    normalize_expr(value, strategy)?;
                }
            }
            ExprKind::ArrayRepeat(repeat) => {
                normalize_expr(repeat.elem.as_mut(), strategy)?;
                normalize_expr(repeat.len.as_mut(), strategy)?;
            }
            ExprKind::Tuple(tuple_expr) => {
                for value in &mut tuple_expr.values {
                    normalize_expr(value, strategy)?;
                }
            }
            ExprKind::BinOp(binop) => {
                normalize_expr(binop.lhs.as_mut(), strategy)?;
                normalize_expr(binop.rhs.as_mut(), strategy)?;
            }
            ExprKind::UnOp(unop) => normalize_expr(unop.val.as_mut(), strategy)?,
            ExprKind::Reference(reference) => normalize_expr(reference.referee.as_mut(), strategy)?,
            ExprKind::Dereference(deref) => normalize_expr(deref.referee.as_mut(), strategy)?,
            ExprKind::Index(index) => {
                normalize_expr(index.obj.as_mut(), strategy)?;
                normalize_expr(index.index.as_mut(), strategy)?;
            }
            ExprKind::Splat(splat) => normalize_expr(splat.iter.as_mut(), strategy)?,
            ExprKind::SplatDict(splat) => normalize_expr(splat.dict.as_mut(), strategy)?,
            ExprKind::Try(expr_try) => {
                normalize_expr(expr_try.expr.as_mut(), strategy)?;
                for catch in &mut expr_try.catches {
                    normalize_expr(catch.body.as_mut(), strategy)?;
                }
                if let Some(elze) = expr_try.elze.as_mut() {
                    normalize_expr(elze.as_mut(), strategy)?;
                }
                if let Some(finally) = expr_try.finally.as_mut() {
                    normalize_expr(finally.as_mut(), strategy)?;
                }
            }
            ExprKind::Async(async_expr) => normalize_expr(async_expr.expr.as_mut(), strategy)?,
            ExprKind::Closure(closure) => normalize_expr(closure.body.as_mut(), strategy)?,
            ExprKind::Closured(closured) => normalize_expr(closured.expr.as_mut(), strategy)?,
            ExprKind::Paren(paren) => normalize_expr(paren.expr.as_mut(), strategy)?,
            ExprKind::FormatString(format_expr) => normalize_format_string(format_expr, strategy)?,
            ExprKind::Item(item) => normalize_item(item.as_mut(), strategy)?,
            ExprKind::Value(value) => normalize_value(value, strategy)?,
            ExprKind::IntrinsicCall(call) => {
                normalize_intrinsic_call(call, strategy)?;
            }
            ExprKind::IntrinsicContainer(collection) => match collection {
                ExprIntrinsicContainer::VecElements { elements } => {
                    for element in elements {
                        normalize_expr(element, strategy)?;
                    }
                }
                ExprIntrinsicContainer::VecRepeat { elem, len } => {
                    normalize_expr(elem.as_mut(), strategy)?;
                    normalize_expr(len.as_mut(), strategy)?;
                }
                ExprIntrinsicContainer::HashMapEntries { entries } => {
                    for entry in entries {
                        normalize_expr(&mut entry.key, strategy)?;
                        normalize_expr(&mut entry.value, strategy)?;
                    }
                }
            },
            ExprKind::Range(range) => {
                if let Some(start) = range.start.as_mut() {
                    normalize_expr(start, strategy)?;
                }
                if let Some(end) = range.end.as_mut() {
                    normalize_expr(end, strategy)?;
                }
                if let Some(step) = range.step.as_mut() {
                    normalize_expr(step, strategy)?;
                }
            }
            ExprKind::Quote(q) => normalize_block(&mut q.block, strategy)?,
            ExprKind::Splice(s) => normalize_expr(s.token.as_mut(), strategy)?,
            ExprKind::Id(_) | ExprKind::Name(_) | ExprKind::Any(_) => {}
        }
        if let Some(new_expr) = replacement {
            let old_ty = expr.ty.clone();
            let old_id = expr.id();
            *expr = new_expr.with_ty_slot(old_ty);
            expr.set_id(old_id);
            continue;
        }

        break;
    }

    Ok(())
}

fn normalize_ty(ty: &mut Ty, strategy: &dyn IntrinsicNormalizer) -> Result<()> {
    match ty {
        Ty::Expr(expr) => normalize_expr(expr.as_mut(), strategy)?,
        Ty::Array(array) => {
            normalize_ty(array.elem.as_mut(), strategy)?;
            normalize_expr(array.len.as_mut(), strategy)?;
        }
        Ty::Vec(vec) => normalize_ty(vec.ty.as_mut(), strategy)?,
        Ty::Tuple(tuple) => {
            for entry in &mut tuple.types {
                normalize_ty(entry, strategy)?;
            }
        }
        Ty::Struct(struct_ty) => {
            for field in &mut struct_ty.fields {
                normalize_ty(&mut field.value, strategy)?;
            }
        }
        Ty::Structural(structural) => {
            for field in &mut structural.fields {
                normalize_ty(&mut field.value, strategy)?;
            }
        }
        Ty::Enum(enum_ty) => {
            for variant in &mut enum_ty.variants {
                normalize_ty(&mut variant.value, strategy)?;
                if let Some(discriminant) = variant.discriminant.as_mut() {
                    normalize_expr(discriminant.as_mut(), strategy)?;
                }
            }
        }
        Ty::Function(function) => {
            for param in &mut function.params {
                normalize_ty(param, strategy)?;
            }
            if let Some(ret) = function.ret_ty.as_mut() {
                normalize_ty(ret.as_mut(), strategy)?;
            }
        }
        Ty::ImplTraits(impl_traits) => {
            for bound in &mut impl_traits.bounds.bounds {
                normalize_expr(bound, strategy)?;
            }
        }
        Ty::TypeBounds(bounds) => {
            for bound in &mut bounds.bounds {
                normalize_expr(bound, strategy)?;
            }
        }
        Ty::Reference(reference) => normalize_ty(reference.ty.as_mut(), strategy)?,
        Ty::RawPtr(raw_ptr) => normalize_ty(raw_ptr.ty.as_mut(), strategy)?,
        Ty::Slice(slice) => normalize_ty(slice.elem.as_mut(), strategy)?,
        Ty::Value(value) => {
            if let Value::Expr(expr) = value.value.as_mut() {
                normalize_expr(expr.as_mut(), strategy)?;
            }
        }
        Ty::Quote(quote) => {
            if let Some(inner) = quote.inner.as_mut() {
                normalize_ty(inner.as_mut(), strategy)?;
            }
        }
        Ty::TypeBinaryOp(op) => {
            normalize_ty(op.lhs.as_mut(), strategy)?;
            normalize_ty(op.rhs.as_mut(), strategy)?;
        }
        Ty::Primitive(_)
        | Ty::TokenStream(_)
        | Ty::Any(_)
        | Ty::Unit(_)
        | Ty::GenericVar(_)
        | Ty::Unknown(_)
        | Ty::Nothing(_)
        | Ty::ErrorType(_)
        | Ty::Type(_)
        | Ty::AnyBox(_)
        | Ty::InferVar(_) => {}
    }
    Ok(())
}

fn normalize_invoke(invoke: &mut ExprInvoke, strategy: &dyn IntrinsicNormalizer) -> Result<()> {
    match &mut invoke.target {
        ExprInvokeTarget::Expr(inner) => normalize_expr(inner.as_mut(), strategy)?,
        ExprInvokeTarget::Method(select) => normalize_expr(select.obj.as_mut(), strategy)?,
        ExprInvokeTarget::Closure(closure) => normalize_expr(closure.body.as_mut(), strategy)?,
        ExprInvokeTarget::Function(_) | ExprInvokeTarget::Type(_) | ExprInvokeTarget::BinOp(_) => {}
    }
    for arg in &mut invoke.args {
        normalize_expr(arg, strategy)?;
    }
    for kwarg in &mut invoke.kwargs {
        normalize_expr(&mut kwarg.value, strategy)?;
    }
    Ok(())
}

fn normalize_format_string(
    format_expr: &mut ExprStringTemplate,
    strategy: &dyn IntrinsicNormalizer,
) -> Result<()> {
    let _ = (format_expr, strategy);
    Ok(())
}

fn normalize_value(value: &mut Value, strategy: &dyn IntrinsicNormalizer) -> Result<()> {
    match value {
        Value::Expr(expr) => normalize_expr(expr.as_mut(), strategy),
        Value::Function(function) => normalize_expr(function.body.as_mut(), strategy),
        _ => Ok(()),
    }
}

fn apply_intrinsic_collection(
    collection: &mut ExprIntrinsicContainer,
    strategy: &dyn IntrinsicNormalizer,
) -> Result<Expr> {
    match collection {
        ExprIntrinsicContainer::VecElements { elements } => {
            for element in elements {
                normalize_expr(element, strategy)?;
            }
        }
        ExprIntrinsicContainer::VecRepeat { elem, len } => {
            normalize_expr(elem.as_mut(), strategy)?;
            normalize_expr(len.as_mut(), strategy)?;
        }
        ExprIntrinsicContainer::HashMapEntries { entries } => {
            for entry in entries {
                normalize_expr(&mut entry.key, strategy)?;
                normalize_expr(&mut entry.value, strategy)?;
            }
        }
    }

    Ok(Expr::new(ExprKind::IntrinsicContainer(collection.clone())))
}

fn normalize_intrinsic_call(
    call: &mut ExprIntrinsicCall,
    strategy: &dyn IntrinsicNormalizer,
) -> Result<()> {
    for arg in call.args.iter_mut() {
        normalize_expr(arg, strategy)?;
    }
    for kwarg in call.kwargs.iter_mut() {
        normalize_expr(&mut kwarg.value, strategy)?;
    }

    if matches!(
        call.kind,
        crate::intrinsics::IntrinsicCallKind::Print | crate::intrinsics::IntrinsicCallKind::Println
    ) {
        if !matches!(
            call.args.first().map(|arg| arg.kind()),
            Some(crate::ast::ExprKind::FormatString(_))
        ) {
            if let Some((template, skip)) = format::convert_print_args_to_template(&call.args) {
                let rest: Vec<_> = call.args.drain(skip..).collect();
                call.args.clear();
                let mut args = Vec::with_capacity(1 + rest.len());
                args.push(Expr::new(crate::ast::ExprKind::FormatString(template)));
                args.extend(rest);
                call.args = args;
                call.kwargs.clear();
            }
        }
    }

    Ok(())
}

fn try_lower_type_builder_const_block(expr: &mut Expr) {
    let mut struct_name = String::new();
    let mut fields: Vec<(String, Ty)> = Vec::new();

    // Phase 1: extract data (immutable borrow)
    {
        let ExprKind::ConstBlock(const_block) = expr.kind_mut() else { return };
        let ExprKind::Block(body) = const_block.expr.kind() else { return };
        if body.stmts.is_empty() { return; }

        let last_idx = body.stmts.len() - 1;
        let build_result = match &body.stmts[last_idx] {
            BlockStmt::Expr(e) => extract_builder_from_invoke(&e.expr),
            _ => None,
        };
        let Some((builder_var, _)) = build_result else { return };

        for stmt in &body.stmts[..last_idx] {
            match stmt {
                BlockStmt::Let(stmt_let) => {
                    let Some(pat_name) = stmt_let.pat.single_name() else { continue };
                    if pat_name != builder_var { continue; }
                    let Some(init) = &stmt_let.init else { continue };
                    if let Some(n) = extract_type_builder_new_name(init) {
                        struct_name = n;
                    }
                }
                BlockStmt::Expr(stmt_expr) => {
                    match stmt_expr.expr.kind() {
                        ExprKind::Assign(assign) => {
                            let target_var = assign.target.single_name();
                            if target_var != Some(builder_var.as_str()) { continue; }
                            if let Some((field_name, field_ty)) =
                                extract_with_field_from_assign(&assign.value)
                            {
                                fields.push((field_name, field_ty));
                            }
                        }
                        ExprKind::If(expr_if) => {
                            if let Some(cond_val) = eval_const_bool(&expr_if.cond) {
                                if cond_val {
                                    if let ExprKind::Block(if_body) = expr_if.then.kind() {
                                        process_builder_if_body(&if_body.stmts, &builder_var, &mut struct_name, &mut fields);
                                    }
                                } else if let Some(elze) = &expr_if.elze {
                                    if let ExprKind::Block(else_body) = elze.kind() {
                                        process_builder_if_body(&else_body.stmts, &builder_var, &mut struct_name, &mut fields);
                                    }
                                }
                            }
                        }
                        _ => {}
                    }
                }
                _ => {}
            }
        }

        if struct_name.is_empty() {
            struct_name = extract_name_from_build_target(&body.stmts);
        }
    } // immutable borrow ends

    if struct_name.is_empty() { return; }

    // Phase 2: mutate
    let mut args = vec![Expr::value(Value::string(struct_name))];
    for (field_name, field_ty) in fields {
        args.push(Expr::value(Value::string(field_name)));
        args.push(Expr::value(Value::Type(field_ty)));
    }
    let call = ExprIntrinsicCall::new(
        crate::intrinsics::IntrinsicCallKind::CreateStruct,
        args,
        Vec::new(),
    );
    let new_block = ExprBlock::new_stmts(vec![BlockStmt::Expr(
        crate::ast::BlockStmtExpr::new(Expr::new(ExprKind::IntrinsicCall(call)))
            .with_semicolon(false),
    )]);

    let ExprKind::ConstBlock(const_block) = expr.kind_mut() else { return };
    const_block.expr = Box::new(Expr::new(ExprKind::Block(new_block)));
}

fn eval_const_bool(cond: &Expr) -> Option<bool> {
    match cond.kind() {
        ExprKind::Name(n) => {
            let key = n.to_string();
            CONST_BOOLS.with(|cb| cb.borrow().get(&key).copied())
        }
        ExprKind::Value(v) => match v.as_ref() {
            Value::Bool(b) => Some(b.value),
            _ => None,
        },
        _ => None,
    }
}

fn process_builder_if_body(
    stmts: &[BlockStmt],
    builder_var: &str,
    struct_name: &mut String,
    fields: &mut Vec<(String, Ty)>,
) {
    for stmt in stmts {
        if let BlockStmt::Expr(stmt_expr) = stmt {
            if let ExprKind::Assign(assign) = stmt_expr.expr.kind() {
                let target_var = assign.target.single_name();
                if target_var != Some(builder_var) { continue; }
                if let Some((field_name, field_ty)) =
                    extract_with_field_from_assign(&assign.value)
                {
                    fields.push((field_name, field_ty));
                }
            }
        }
    }
}

fn extract_builder_from_invoke(expr: &Expr) -> Option<(String, bool)> {
    let ExprKind::Invoke(invoke) = expr.kind() else { return None };
    match &invoke.target {
        ExprInvokeTarget::Method(select) if select.field.as_str() == "build" => {
            let var = select.obj.single_name()?;
            Some((var.to_string(), true))
        }
        _ => None,
    }
}

fn extract_type_builder_new_name(expr: &Expr) -> Option<String> {
    // Already normalized to CreateStruct
    if let ExprKind::IntrinsicCall(call) = expr.kind() {
        if call.kind == crate::intrinsics::IntrinsicCallKind::CreateStruct {
            return call.args.first().and_then(|a| match a.kind() {
                ExprKind::Value(v) => match v.as_ref() {
                    Value::String(s) => Some(s.value.clone()),
                    _ => None,
                },
                _ => None,
            });
        }
    }
    // Original TypeBuilder::new(...) invoke
    let ExprKind::Invoke(invoke) = expr.kind() else { return None };
    match &invoke.target {
        ExprInvokeTarget::Function(path) => {
            let is_tb = match path {
                crate::ast::Name::Path(p) => p.segments.iter()
                    .any(|s| s.name.as_str() == "TypeBuilder" || s.name.as_str() == "new"),
                crate::ast::Name::Ident(i) => i.as_str() == "new",
                _ => false,
            };
            if !is_tb { return None; }
            invoke.args.first().and_then(|a| match a.kind() {
                ExprKind::Value(v) => match v.as_ref() {
                    Value::String(s) => Some(s.value.clone()),
                    _ => None,
                },
                _ => None,
            })
        }
        _ => None,
    }
}

fn extract_with_field_from_assign(expr: &Expr) -> Option<(String, Ty)> {
    let ExprKind::Invoke(invoke) = expr.kind() else { return None };
    match &invoke.target {
        ExprInvokeTarget::Method(select) if select.field.as_str() == "with_field" => {
            if invoke.args.len() < 2 { return None; }
            let field_name = match invoke.args[0].kind() {
                ExprKind::Value(v) => match v.as_ref() {
                    Value::String(s) => Some(s.value.clone()),
                    _ => None,
                },
                _ => None,
            }?;
            let field_ty = extract_type_from_normalize_expr(&invoke.args[1])?;
            Some((field_name, field_ty))
        }
        _ => None,
    }
}

fn extract_type_from_normalize_expr(expr: &Expr) -> Option<Ty> {
    // Direct type value
    if let ExprKind::Value(v) = expr.kind() {
        if let Value::Type(ty) = v.as_ref() {
            return Some(ty.clone());
        }
    }
    match expr.kind() {
        ExprKind::Name(loc) => {
            let s = loc.to_string();
            match s.as_str() {
                "i64" => Some(Ty::Primitive(crate::ast::TypePrimitive::Int(
                    crate::ast::TypeInt::I64,
                ))),
                "i32" => Some(Ty::Primitive(crate::ast::TypePrimitive::Int(
                    crate::ast::TypeInt::I32,
                ))),
                "i8" => Some(Ty::Primitive(crate::ast::TypePrimitive::Int(
                    crate::ast::TypeInt::I8,
                ))),
                "u64" => Some(Ty::Primitive(crate::ast::TypePrimitive::Int(
                    crate::ast::TypeInt::U64,
                ))),
                "bool" => Some(Ty::Primitive(crate::ast::TypePrimitive::Bool)),
                "f64" => Some(Ty::Primitive(crate::ast::TypePrimitive::Decimal(
                    crate::ast::DecimalType::F64,
                ))),
                "str" => Some(Ty::Primitive(crate::ast::TypePrimitive::String)),
                s if s.starts_with('&') => {
                    let inner = s.trim_start_matches('&').trim()
                        .trim_start_matches("'static").trim();
                    if inner == "str" {
                        Some(Ty::Reference(crate::ast::TypeReference {
                            ty: Box::new(Ty::Primitive(crate::ast::TypePrimitive::String)),
                            mutability: None,
                            lifetime: None,
                        }))
                    } else {
                        None
                    }
                }
                _ => None,
            }
        }
        _ => None,
    }
}

fn extract_name_from_build_target(stmts: &[BlockStmt]) -> String {
    for stmt in stmts.iter() {
        if let BlockStmt::Let(s) = stmt {
            if let Some(init) = &s.init {
                if let Some(name) = extract_type_builder_new_name(init) {
                    return name;
                }
            }
        }
    }
    String::new()
}

trait SingleName {
    fn single_name(&self) -> Option<&str>;
}

impl SingleName for Expr {
    fn single_name(&self) -> Option<&str> {
        match self.kind() {
            ExprKind::Name(crate::ast::Name::Ident(ident)) => Some(ident.as_str()),
            ExprKind::Name(crate::ast::Name::Path(path)) => {
                path.segments.last().map(|s| s.name.as_str())
            }
            _ => None,
        }
    }
}

impl SingleName for crate::ast::Pattern {
    fn single_name(&self) -> Option<&str> {
        match &self.kind {
            crate::ast::PatternKind::Ident(ident) => Some(ident.ident.name.as_str()),
            _ => None,
        }
    }
}

fn resolve_all_splices(items: &mut [Item]) {
    // Collect quote values from DefConst items
    let mut quote_values: HashMap<String, Expr> = HashMap::new();
    for item in items.iter() {
        if let ItemKind::DefConst(def) = item.kind() {
            if matches!(def.value.kind(), ExprKind::Quote(_)) {
                quote_values.insert(
                    def.name.as_str().to_string(),
                    (*def.value).clone(),
                );
            }
        }
    }
    // Replace splices in function bodies with quote items
    for item in items.iter_mut() {
        if let ItemKind::DefFunction(func) = item.kind_mut() {
            resolve_splices_in_expr(&mut func.body, &quote_values);
        }
    }
}

fn resolve_splices_in_expr(expr: &mut Expr, quote_values: &HashMap<String, Expr>) {
    if let ExprKind::Block(block) = expr.kind_mut() {
        let mut new_stmts: Vec<BlockStmt> = Vec::new();
        for stmt in block.stmts.drain(..) {
            match stmt {
                BlockStmt::Expr(mut expr_stmt)
                    if matches!(expr_stmt.expr.kind(), ExprKind::Splice(_)) =>
                {
                    if let ExprKind::Splice(splice) = expr_stmt.expr.kind() {
                        if let ExprKind::Name(name) = splice.token.kind() {
                            let key = name.to_string();
                            if let Some(quote_expr) = quote_values.get(&key) {
                                if let ExprKind::Quote(quote) = quote_expr.kind() {
                                    for quote_stmt in &quote.block.stmts {
                                        if let BlockStmt::Item(item) = quote_stmt {
                                            new_stmts.push(BlockStmt::Item(
                                                item.clone(),
                                            ));
                                        }
                                    }
                                    continue;
                                }
                            }
                        }
                    }
                    // Couldn't resolve — keep the splice
                    new_stmts.push(BlockStmt::Expr(expr_stmt));
                }
                other => new_stmts.push(other),
            }
        }
        block.stmts = new_stmts;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::FormatTemplatePart;

    #[test]
    fn test_convert_print_args_to_template() {
        let lit = Expr::new(ExprKind::Value(Box::new(Value::string("hello".into()))));
        let (out, _skip) =
            crate::intrinsics::normalize::format::convert_print_args_to_template(&[lit])
                .expect("format");
        assert_eq!(out.parts.len(), 1);
        match &out.parts[0] {
            FormatTemplatePart::Literal(s) => assert_eq!(s, "hello"),
            _ => panic!("expected literal"),
        }
    }
}
