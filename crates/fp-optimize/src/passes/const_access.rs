use fp_core::ast::{
    BlockStmt, BlockStmtExpr, Expr, ExprField, ExprInvokeTarget, ExprKind, ExprMatchCase,
    ExprSelect, ExprTuple, ExprWhile, Item, ItemKind, Locator, Node, NodeKind, Value,
};
use fp_core::ast::{ExprIntrinsicContainer, ExprIntrinsicContainerEntry};
use std::collections::HashMap;

pub fn rewrite_const_accesses(node: &mut Node, constants: &HashMap<String, Value>) {
    match node.kind_mut() {
        NodeKind::File(file) => {
            for item in &mut file.items {
                rewrite_item(item, constants);
            }
        }
        NodeKind::Item(item) => rewrite_item(item, constants),
        NodeKind::Expr(expr) => rewrite_expr(expr, constants),
        NodeKind::Query(_) | NodeKind::Schema(_) | NodeKind::Workspace(_) => {}
    }
}

fn rewrite_item(item: &mut Item, constants: &HashMap<String, Value>) {
    match item.kind_mut() {
        ItemKind::Module(module) => {
            for child in &mut module.items {
                rewrite_item(child, constants);
            }
        }
        ItemKind::Impl(impl_block) => {
            for child in &mut impl_block.items {
                rewrite_item(child, constants);
            }
        }
        ItemKind::DefTrait(def) => {
            for child in &mut def.items {
                rewrite_item(child, constants);
            }
        }
        ItemKind::DefFunction(def) => rewrite_expr(def.body.as_mut(), constants),
        ItemKind::DefConst(def) => rewrite_expr(def.value.as_mut(), constants),
        ItemKind::Expr(expr) => rewrite_expr(expr, constants),
        _ => {}
    }
}

fn rewrite_expr(expr: &mut Expr, constants: &HashMap<String, Value>) {
    match expr.kind_mut() {
        ExprKind::Block(block) => {
            for stmt in &mut block.stmts {
                rewrite_stmt(stmt, constants);
            }
        }
        ExprKind::Match(expr_match) => {
            if let Some(scrutinee) = expr_match.scrutinee.as_mut() {
                rewrite_expr(scrutinee.as_mut(), constants);
            }
            for case in &mut expr_match.cases {
                rewrite_match_case(case, constants);
            }
        }
        ExprKind::If(expr_if) => {
            rewrite_expr(expr_if.cond.as_mut(), constants);
            rewrite_expr(expr_if.then.as_mut(), constants);
            if let Some(elze) = expr_if.elze.as_mut() {
                rewrite_expr(elze.as_mut(), constants);
            }
        }
        ExprKind::Loop(loop_expr) => rewrite_expr(loop_expr.body.as_mut(), constants),
        ExprKind::While(expr_while) => rewrite_while(expr_while, constants),
        ExprKind::For(expr_for) => {
            rewrite_expr(expr_for.iter.as_mut(), constants);
            rewrite_expr(expr_for.body.as_mut(), constants);
        }
        ExprKind::Invoke(invoke) => {
            match &mut invoke.target {
                ExprInvokeTarget::Method(select) => rewrite_select(select, constants),
                ExprInvokeTarget::Expr(target) => rewrite_expr(target.as_mut(), constants),
                ExprInvokeTarget::Function(_) | ExprInvokeTarget::Type(_) => {}
                ExprInvokeTarget::Closure(_) | ExprInvokeTarget::BinOp(_) => {}
            }
            for arg in &mut invoke.args {
                rewrite_expr(arg.as_mut(), constants);
            }
            if let Some(replacement) = eval_const_len(invoke, constants) {
                replace_expr(expr, replacement);
            }
        }
        ExprKind::BinOp(binop) => {
            rewrite_expr(binop.lhs.as_mut(), constants);
            rewrite_expr(binop.rhs.as_mut(), constants);
        }
        ExprKind::UnOp(unop) => rewrite_expr(unop.val.as_mut(), constants),
        ExprKind::Assign(assign) => {
            rewrite_expr(assign.target.as_mut(), constants);
            rewrite_expr(assign.value.as_mut(), constants);
        }
        ExprKind::Select(select) => rewrite_select(select, constants),
        ExprKind::Index(index_expr) => {
            rewrite_expr(index_expr.obj.as_mut(), constants);
            rewrite_expr(index_expr.index.as_mut(), constants);
            if let Some(replacement) = eval_const_index(
                index_expr.obj.as_ref(),
                index_expr.index.as_ref(),
                constants,
            ) {
                replace_expr(expr, replacement);
            }
        }
        ExprKind::Struct(struct_expr) => {
            rewrite_expr(struct_expr.name.as_mut(), constants);
            for field in &mut struct_expr.fields {
                rewrite_struct_field(field, constants);
            }
            if let Some(update) = struct_expr.update.as_mut() {
                rewrite_expr(update.as_mut(), constants);
            }
        }
        ExprKind::Structural(struct_expr) => {
            for field in &mut struct_expr.fields {
                rewrite_struct_field(field, constants);
            }
        }
        ExprKind::Cast(cast) => rewrite_expr(cast.expr.as_mut(), constants),
        ExprKind::Reference(reference) => rewrite_expr(reference.referee.as_mut(), constants),
        ExprKind::Dereference(deref) => rewrite_expr(deref.referee.as_mut(), constants),
        ExprKind::Tuple(tuple) => rewrite_tuple(tuple, constants),
        ExprKind::Try(expr_try) => rewrite_expr(expr_try.expr.as_mut(), constants),
        ExprKind::Let(expr_let) => rewrite_expr(expr_let.expr.as_mut(), constants),
        ExprKind::Closure(closure) => rewrite_expr(closure.body.as_mut(), constants),
        ExprKind::Array(array) => {
            for value in &mut array.values {
                rewrite_expr(value, constants);
            }
        }
        ExprKind::ArrayRepeat(repeat) => {
            rewrite_expr(repeat.elem.as_mut(), constants);
            rewrite_expr(repeat.len.as_mut(), constants);
        }
        ExprKind::IntrinsicContainer(container) => rewrite_intrinsic_container(container, constants),
        ExprKind::Quote(quote) => {
            rewrite_expr(quote.block.as_mut(), constants);
        }
        ExprKind::Splice(splice) => {
            rewrite_expr(splice.token.as_mut(), constants);
        }
        ExprKind::Closured(closured) => {
            rewrite_expr(closured.expr.as_mut(), constants);
        }
        ExprKind::Await(await_expr) => rewrite_expr(await_expr.base.as_mut(), constants),
        ExprKind::Paren(paren) => rewrite_expr(paren.expr.as_mut(), constants),
        ExprKind::Range(range) => {
            if let Some(start) = range.start.as_mut() {
                rewrite_expr(start.as_mut(), constants);
            }
            if let Some(end) = range.end.as_mut() {
                rewrite_expr(end.as_mut(), constants);
            }
        }
        ExprKind::Splat(splat) => rewrite_expr(splat.iter.as_mut(), constants),
        ExprKind::SplatDict(splat) => rewrite_expr(splat.dict.as_mut(), constants),
        ExprKind::Value(_)
        | ExprKind::IntrinsicCall(_)
        | ExprKind::FormatString(_)
        | ExprKind::Macro(_)
        | ExprKind::Item(_)
        | ExprKind::Any(_) => {}
        ExprKind::Async(expr_async) => rewrite_expr(expr_async.expr.as_mut(), constants),
    }
}

fn rewrite_stmt(stmt: &mut BlockStmt, constants: &HashMap<String, Value>) {
    match stmt {
        BlockStmt::Item(item) => rewrite_item(item.as_mut(), constants),
        BlockStmt::Let(stmt_let) => {
            if let Some(init) = stmt_let.init.as_mut() {
                rewrite_expr(init, constants);
            }
            if let Some(diverge) = stmt_let.diverge.as_mut() {
                rewrite_expr(diverge, constants);
            }
        }
        BlockStmt::Expr(BlockStmtExpr { expr, .. }) => rewrite_expr(expr.as_mut(), constants),
        BlockStmt::Noop | BlockStmt::Any(_) => {}
    }
}

fn rewrite_match_case(case: &mut ExprMatchCase, constants: &HashMap<String, Value>) {
    rewrite_expr(case.cond.as_mut(), constants);
    if let Some(guard) = case.guard.as_mut() {
        rewrite_expr(guard.as_mut(), constants);
    }
    rewrite_expr(case.body.as_mut(), constants);
}

fn rewrite_while(expr_while: &mut ExprWhile, constants: &HashMap<String, Value>) {
    rewrite_expr(expr_while.cond.as_mut(), constants);
    rewrite_expr(expr_while.body.as_mut(), constants);
}

fn rewrite_select(select: &mut ExprSelect, constants: &HashMap<String, Value>) {
    rewrite_expr(select.obj.as_mut(), constants);
}

fn rewrite_struct_field(field: &mut ExprField, constants: &HashMap<String, Value>) {
    if let Some(value) = field.value.as_mut() {
        rewrite_expr(value, constants);
    }
}

fn rewrite_tuple(tuple: &mut ExprTuple, constants: &HashMap<String, Value>) {
    for value in &mut tuple.values {
        rewrite_expr(value, constants);
    }
}

fn rewrite_intrinsic_container(
    container: &mut ExprIntrinsicContainer,
    constants: &HashMap<String, Value>,
) {
    match container {
        ExprIntrinsicContainer::VecElements { elements } => {
            for elem in elements {
                rewrite_expr(elem.as_mut(), constants);
            }
        }
        ExprIntrinsicContainer::VecRepeat { elem, len } => {
            rewrite_expr(elem.as_mut(), constants);
            rewrite_expr(len.as_mut(), constants);
        }
        ExprIntrinsicContainer::HashMapEntries { entries } => {
            for ExprIntrinsicContainerEntry { key, value } in entries {
                rewrite_expr(key.as_mut(), constants);
                rewrite_expr(value.as_mut(), constants);
            }
        }
    }
}

fn eval_const_index(
    obj: &Expr,
    index: &Expr,
    constants: &HashMap<String, Value>,
) -> Option<Value> {
    let name = const_name_for_expr(obj)?;
    let value = lookup_const_value(constants, &name)?;
    let key = value_for_literal(index)?;
    match value {
        Value::Map(map) => map.get(&key).cloned(),
        Value::List(list) => match key {
            Value::Int(int) if int.value >= 0 => list.values.get(int.value as usize).cloned(),
            _ => None,
        },
        Value::Tuple(tuple) => match key {
            Value::Int(int) if int.value >= 0 => tuple.values.get(int.value as usize).cloned(),
            _ => None,
        },
        _ => None,
    }
}

fn eval_const_len(
    invoke: &fp_core::ast::ExprInvoke,
    constants: &HashMap<String, Value>,
) -> Option<Value> {
    let ExprInvokeTarget::Method(select) = &invoke.target else {
        return None;
    };
    if select.field.name.as_str() != "len" || !invoke.args.is_empty() {
        return None;
    }
    let name = const_name_for_expr(select.obj.as_ref())?;
    let value = lookup_const_value(constants, &name)?;
    match value {
        Value::List(list) => Some(Value::int(list.values.len() as i64)),
        Value::Map(map) => Some(Value::int(map.len() as i64)),
        Value::String(text) => Some(Value::int(text.value.chars().count() as i64)),
        Value::Tuple(tuple) => Some(Value::int(tuple.values.len() as i64)),
        _ => None,
    }
}

fn const_name_for_expr(expr: &Expr) -> Option<String> {
    match expr.kind() {
        ExprKind::Locator(locator) => locator_name(locator),
        ExprKind::Paren(paren) => const_name_for_expr(paren.expr.as_ref()),
        _ => None,
    }
}

fn locator_name(locator: &Locator) -> Option<String> {
    match locator {
        Locator::Ident(ident) => Some(ident.as_str().to_string()),
        Locator::Path(path) => Some(
            path.segments
                .iter()
                .map(|seg| seg.name.as_str())
                .collect::<Vec<_>>()
                .join("::"),
        ),
        Locator::ParameterPath(path) => Some(
            path.segments
                .iter()
                .map(|seg| seg.ident.as_str())
                .collect::<Vec<_>>()
                .join("::"),
        ),
    }
}

fn lookup_const_value(constants: &HashMap<String, Value>, name: &str) -> Option<Value> {
    constants
        .get(name)
        .cloned()
        .or_else(|| name.rsplit("::").next().and_then(|tail| constants.get(tail).cloned()))
}

fn value_for_literal(expr: &Expr) -> Option<Value> {
    match expr.kind() {
        ExprKind::Value(value) => Some(value.as_ref().clone()),
        ExprKind::Paren(paren) => value_for_literal(paren.expr.as_ref()),
        _ => None,
    }
}

fn replace_expr(target: &mut Expr, value: Value) {
    let previous_ty = target.ty().cloned();
    let mut replacement = Expr::value(value);
    replacement.ty = previous_ty;
    *target = replacement;
}
