use super::super::*;
use fp_core::ast::{
    Expr, ExprIndex, ExprInvoke, ExprInvokeTarget, ExprKind, Locator, Node, NodeKind, Value,
    ValueList, ValueMap,
};
use std::collections::{HashMap, HashSet};

impl Pipeline {
    pub(crate) fn stage_const_eval(
        &mut self,
        ast: &mut Node,
        options: &PipelineOptions,
        manager: &DiagnosticManager,
    ) -> Result<ConstEvalOutcome, CliError> {
        let serializer = self.serializer.clone().ok_or_else(|| {
            CliError::Compilation("No serializer registered for const-eval".to_string())
        })?;
        register_threadlocal_serializer(serializer.clone());

        let shared_context = SharedScopedContext::new();
        let mut orchestrator = ConstEvaluationOrchestrator::new(serializer);
        orchestrator.set_debug_assertions(!options.release);
        orchestrator.set_execute_main(options.execute_main);

        let outcome = match orchestrator.evaluate(ast, &shared_context) {
            Ok(outcome) => outcome,
            Err(e) => {
                let diagnostic = Diagnostic::error(format!("Const evaluation failed: {}", e))
                    .with_source_context(STAGE_CONST_EVAL);
                manager.add_diagnostic(diagnostic);
                return Err(Self::stage_failure(STAGE_CONST_EVAL));
            }
        };

        manager.add_diagnostics(outcome.diagnostics.clone());
        if outcome.has_errors {
            return Err(Self::stage_failure(STAGE_CONST_EVAL));
        }

        // Enforce: in strict mode (non-tolerant), no quote/splice remain after const-eval
        if !options.error_tolerance.enabled {
            if ast_contains_quote_or_splice(ast) {
                manager.add_diagnostic(
                    Diagnostic::error(
                        "quote/splice nodes remain after const-eval in strict mode".to_string(),
                    )
                    .with_source_context(STAGE_CONST_EVAL),
                );
                return Err(Self::stage_failure(STAGE_CONST_EVAL));
            }
        }

        if let Err(err) = rewrite_const_hashmaps(ast, &outcome, manager) {
            manager.add_diagnostic(
                Diagnostic::error(err).with_source_context(STAGE_CONST_EVAL),
            );
            return Err(Self::stage_failure(STAGE_CONST_EVAL));
        }
        if let Err(err) = rewrite_const_lists(ast, &outcome, manager) {
            manager.add_diagnostic(
                Diagnostic::error(err).with_source_context(STAGE_CONST_EVAL),
            );
            return Err(Self::stage_failure(STAGE_CONST_EVAL));
        }
        Ok(outcome)
    }
}

fn rewrite_const_hashmaps(
    ast: &mut Node,
    outcome: &ConstEvalOutcome,
    manager: &DiagnosticManager,
) -> Result<(), String> {
    let mut map_consts: HashMap<String, ValueMap> = HashMap::new();
    for (name, value) in &outcome.evaluated_constants {
        if let Value::Map(map) = value {
            map_consts.insert(name.clone(), map.clone());
        }
    }

    if map_consts.is_empty() {
        return Ok(());
    }

    let mut unhandled: HashSet<String> = HashSet::new();
    rewrite_node_for_maps(ast, &map_consts, &mut unhandled, false, manager);

    if !unhandled.is_empty() {
        let mut names: Vec<String> = unhandled.into_iter().collect();
        names.sort();
        return Err(format!(
            "HashMap values are only supported at compile time; unsupported runtime uses: {}",
            names.join(", ")
        ));
    }

    strip_map_consts(ast, &map_consts);
    Ok(())
}

fn rewrite_node_for_maps(
    node: &mut Node,
    map_consts: &HashMap<String, ValueMap>,
    unhandled: &mut HashSet<String>,
    allow_map_value: bool,
    manager: &DiagnosticManager,
) {
    match &mut node.kind {
        NodeKind::File(file) => {
            for item in &mut file.items {
                rewrite_item_for_maps(item, map_consts, unhandled, manager);
            }
        }
        NodeKind::Item(item) => rewrite_item_for_maps(item, map_consts, unhandled, manager),
        NodeKind::Expr(expr) => {
            rewrite_expr_for_maps(expr, map_consts, unhandled, allow_map_value, manager);
        }
        NodeKind::Query(_) | NodeKind::Schema(_) | NodeKind::Workspace(_) => {}
    }
}

fn rewrite_item_for_maps(
    item: &mut fp_core::ast::Item,
    map_consts: &HashMap<String, ValueMap>,
    unhandled: &mut HashSet<String>,
    manager: &DiagnosticManager,
) {
    match item.kind_mut() {
        fp_core::ast::ItemKind::Module(module) => {
            for child in &mut module.items {
                rewrite_item_for_maps(child, map_consts, unhandled, manager);
            }
        }
        fp_core::ast::ItemKind::Impl(impl_block) => {
            for child in &mut impl_block.items {
                rewrite_item_for_maps(child, map_consts, unhandled, manager);
            }
        }
        fp_core::ast::ItemKind::DefFunction(func) => {
            rewrite_expr_for_maps(func.body.as_mut(), map_consts, unhandled, false, manager);
        }
        fp_core::ast::ItemKind::DefConst(def) => {
            rewrite_expr_for_maps(def.value.as_mut(), map_consts, unhandled, true, manager);
        }
        fp_core::ast::ItemKind::DefStatic(def) => {
            rewrite_expr_for_maps(def.value.as_mut(), map_consts, unhandled, false, manager);
        }
        fp_core::ast::ItemKind::Expr(expr) => {
            rewrite_expr_for_maps(expr, map_consts, unhandled, false, manager);
        }
        _ => {}
    }
}

fn rewrite_expr_for_maps(
    expr: &mut Expr,
    map_consts: &HashMap<String, ValueMap>,
    unhandled: &mut HashSet<String>,
    allow_map_value: bool,
    manager: &DiagnosticManager,
) {
    if let Some(replacement) = try_fold_map_len(expr, map_consts) {
        *expr = replacement;
        return;
    }
    if let Some(replacement) = try_fold_map_index(expr, map_consts, manager) {
        *expr = replacement;
        return;
    }

    match expr.kind_mut() {
        ExprKind::Value(value) => {
            if matches!(value.as_ref(), Value::Map(_)) && !allow_map_value {
                manager.add_diagnostic(
                    Diagnostic::error(
                        "HashMap values are only supported during compile-time evaluation"
                            .to_string(),
                    )
                    .with_source_context(STAGE_CONST_EVAL),
                );
                unhandled.insert("<hashmap literal>".to_string());
            }
        }
        ExprKind::Id(_) => {}
        ExprKind::Locator(locator) => {
            if let Some(name) = map_name_from_locator(locator, map_consts) {
                unhandled.insert(name);
            }
        }
        ExprKind::Block(block) => {
            for stmt in &mut block.stmts {
                rewrite_block_stmt_for_maps(stmt, map_consts, unhandled, allow_map_value, manager);
            }
            if let Some(expr) = block.last_expr_mut() {
                rewrite_expr_for_maps(expr, map_consts, unhandled, allow_map_value, manager);
            }
        }
        ExprKind::If(expr_if) => {
            rewrite_expr_for_maps(expr_if.cond.as_mut(), map_consts, unhandled, allow_map_value, manager);
            rewrite_expr_for_maps(expr_if.then.as_mut(), map_consts, unhandled, allow_map_value, manager);
            if let Some(elze) = expr_if.elze.as_mut() {
                rewrite_expr_for_maps(elze, map_consts, unhandled, allow_map_value, manager);
            }
        }
        ExprKind::Loop(expr_loop) => {
            rewrite_expr_for_maps(expr_loop.body.as_mut(), map_consts, unhandled, allow_map_value, manager);
        }
        ExprKind::While(expr_while) => {
            rewrite_expr_for_maps(expr_while.cond.as_mut(), map_consts, unhandled, allow_map_value, manager);
            rewrite_expr_for_maps(expr_while.body.as_mut(), map_consts, unhandled, allow_map_value, manager);
        }
        ExprKind::For(expr_for) => {
            rewrite_expr_for_maps(expr_for.iter.as_mut(), map_consts, unhandled, allow_map_value, manager);
            rewrite_expr_for_maps(expr_for.body.as_mut(), map_consts, unhandled, allow_map_value, manager);
        }
        ExprKind::Match(expr_match) => {
            for case in &mut expr_match.cases {
                rewrite_expr_for_maps(case.cond.as_mut(), map_consts, unhandled, allow_map_value, manager);
                rewrite_expr_for_maps(case.body.as_mut(), map_consts, unhandled, allow_map_value, manager);
            }
        }
        ExprKind::BinOp(binop) => {
            rewrite_expr_for_maps(binop.lhs.as_mut(), map_consts, unhandled, allow_map_value, manager);
            rewrite_expr_for_maps(binop.rhs.as_mut(), map_consts, unhandled, allow_map_value, manager);
        }
        ExprKind::UnOp(unop) => {
            rewrite_expr_for_maps(unop.val.as_mut(), map_consts, unhandled, allow_map_value, manager);
        }
        ExprKind::Let(expr_let) => {
            rewrite_expr_for_maps(expr_let.expr.as_mut(), map_consts, unhandled, allow_map_value, manager);
        }
        ExprKind::Assign(assign) => {
            rewrite_expr_for_maps(assign.target.as_mut(), map_consts, unhandled, allow_map_value, manager);
            rewrite_expr_for_maps(assign.value.as_mut(), map_consts, unhandled, allow_map_value, manager);
        }
        ExprKind::Invoke(invoke) => {
            rewrite_invoke_for_maps(invoke, map_consts, unhandled, allow_map_value, manager);
        }
        ExprKind::Select(select) => {
            rewrite_expr_for_maps(select.obj.as_mut(), map_consts, unhandled, allow_map_value, manager);
        }
        ExprKind::Index(index) => {
            rewrite_expr_for_maps(index.obj.as_mut(), map_consts, unhandled, allow_map_value, manager);
            rewrite_expr_for_maps(index.index.as_mut(), map_consts, unhandled, allow_map_value, manager);
        }
        ExprKind::Struct(struct_expr) => {
            rewrite_expr_for_maps(struct_expr.name.as_mut(), map_consts, unhandled, allow_map_value, manager);
            for field in &mut struct_expr.fields {
                if let Some(value) = field.value.as_mut() {
                    rewrite_expr_for_maps(value, map_consts, unhandled, allow_map_value, manager);
                }
            }
            if let Some(update) = struct_expr.update.as_mut() {
                rewrite_expr_for_maps(update, map_consts, unhandled, allow_map_value, manager);
            }
        }
        ExprKind::Structural(struct_expr) => {
            for field in &mut struct_expr.fields {
                if let Some(value) = field.value.as_mut() {
                    rewrite_expr_for_maps(value, map_consts, unhandled, allow_map_value, manager);
                }
            }
        }
        ExprKind::Array(array) => {
            for elem in &mut array.values {
                rewrite_expr_for_maps(elem, map_consts, unhandled, allow_map_value, manager);
            }
        }
        ExprKind::ArrayRepeat(repeat) => {
            rewrite_expr_for_maps(repeat.elem.as_mut(), map_consts, unhandled, allow_map_value, manager);
            rewrite_expr_for_maps(repeat.len.as_mut(), map_consts, unhandled, allow_map_value, manager);
        }
        ExprKind::Tuple(tuple) => {
            for elem in &mut tuple.values {
                rewrite_expr_for_maps(elem, map_consts, unhandled, allow_map_value, manager);
            }
        }
        ExprKind::Paren(paren) => {
            rewrite_expr_for_maps(paren.expr.as_mut(), map_consts, unhandled, allow_map_value, manager);
        }
        ExprKind::Cast(cast) => {
            rewrite_expr_for_maps(cast.expr.as_mut(), map_consts, unhandled, allow_map_value, manager);
        }
        ExprKind::Reference(reference) => {
            rewrite_expr_for_maps(reference.referee.as_mut(), map_consts, unhandled, allow_map_value, manager);
        }
        ExprKind::Dereference(deref) => {
            rewrite_expr_for_maps(deref.referee.as_mut(), map_consts, unhandled, allow_map_value, manager);
        }
        ExprKind::Try(expr_try) => {
            rewrite_expr_for_maps(expr_try.expr.as_mut(), map_consts, unhandled, allow_map_value, manager);
        }
        ExprKind::Async(expr_async) => {
            rewrite_expr_for_maps(expr_async.expr.as_mut(), map_consts, unhandled, allow_map_value, manager);
        }
        ExprKind::Closure(closure) => {
            rewrite_expr_for_maps(closure.body.as_mut(), map_consts, unhandled, allow_map_value, manager);
        }
        ExprKind::Closured(closured) => {
            rewrite_expr_for_maps(closured.expr.as_mut(), map_consts, unhandled, allow_map_value, manager);
        }
        ExprKind::Quote(quote) => {
            for stmt in &mut quote.block.stmts {
                rewrite_block_stmt_for_maps(stmt, map_consts, unhandled, allow_map_value, manager);
            }
            if let Some(expr) = quote.block.last_expr_mut() {
                rewrite_expr_for_maps(expr, map_consts, unhandled, allow_map_value, manager);
            }
        }
        ExprKind::Splice(splice) => {
            rewrite_expr_for_maps(splice.token.as_mut(), map_consts, unhandled, allow_map_value, manager);
        }
        ExprKind::Await(expr_await) => {
            rewrite_expr_for_maps(expr_await.base.as_mut(), map_consts, unhandled, allow_map_value, manager);
        }
        ExprKind::IntrinsicCall(call) => {
            match &mut call.payload {
                fp_core::intrinsics::IntrinsicCallPayload::Format { template } => {
                    for arg in &mut template.args {
                        rewrite_expr_for_maps(arg, map_consts, unhandled, allow_map_value, manager);
                    }
                    for kwarg in &mut template.kwargs {
                        rewrite_expr_for_maps(
                            &mut kwarg.value,
                            map_consts,
                            unhandled,
                            allow_map_value,
                            manager,
                        );
                    }
                }
                fp_core::intrinsics::IntrinsicCallPayload::Args { args } => {
                    for arg in args {
                        rewrite_expr_for_maps(arg, map_consts, unhandled, allow_map_value, manager);
                    }
                }
            }
        }
        ExprKind::IntrinsicContainer(container) => {
            container.for_each_expr_mut(|expr| {
                rewrite_expr_for_maps(expr, map_consts, unhandled, allow_map_value, manager);
            });
        }
        ExprKind::FormatString(format) => {
            for arg in &mut format.args {
                rewrite_expr_for_maps(arg, map_consts, unhandled, allow_map_value, manager);
            }
            for kwarg in &mut format.kwargs {
                rewrite_expr_for_maps(
                    &mut kwarg.value,
                    map_consts,
                    unhandled,
                    allow_map_value,
                    manager,
                );
            }
        }
        ExprKind::Splat(splat) => {
            rewrite_expr_for_maps(splat.iter.as_mut(), map_consts, unhandled, allow_map_value, manager);
        }
        ExprKind::SplatDict(splat) => {
            rewrite_expr_for_maps(splat.dict.as_mut(), map_consts, unhandled, allow_map_value, manager);
        }
        ExprKind::Any(_) | ExprKind::Macro(_) | ExprKind::Item(_) => {}
        ExprKind::Range(range) => {
            if let Some(start) = range.start.as_mut() {
                rewrite_expr_for_maps(start, map_consts, unhandled, allow_map_value, manager);
            }
            if let Some(end) = range.end.as_mut() {
                rewrite_expr_for_maps(end, map_consts, unhandled, allow_map_value, manager);
            }
            if let Some(step) = range.step.as_mut() {
                rewrite_expr_for_maps(step, map_consts, unhandled, allow_map_value, manager);
            }
        }
    }
}


fn rewrite_block_stmt_for_maps(
    stmt: &mut fp_core::ast::BlockStmt,
    map_consts: &HashMap<String, ValueMap>,
    unhandled: &mut HashSet<String>,
    allow_map_value: bool,
    manager: &DiagnosticManager,
) {
    match stmt {
        fp_core::ast::BlockStmt::Expr(expr_stmt) => {
            rewrite_expr_for_maps(
                expr_stmt.expr.as_mut(),
                map_consts,
                unhandled,
                allow_map_value,
                manager,
            );
        }
        fp_core::ast::BlockStmt::Let(stmt_let) => {
            if let Some(init) = stmt_let.init.as_mut() {
                rewrite_expr_for_maps(init, map_consts, unhandled, allow_map_value, manager);
            }
            if let Some(diverge) = stmt_let.diverge.as_mut() {
                rewrite_expr_for_maps(diverge, map_consts, unhandled, allow_map_value, manager);
            }
        }
        fp_core::ast::BlockStmt::Item(item) => {
            rewrite_item_for_maps(item.as_mut(), map_consts, unhandled, manager);
        }
        fp_core::ast::BlockStmt::Noop | fp_core::ast::BlockStmt::Any(_) => {}
    }
}

fn rewrite_invoke_for_maps(
    invoke: &mut ExprInvoke,
    map_consts: &HashMap<String, ValueMap>,
    unhandled: &mut HashSet<String>,
    allow_map_value: bool,
    manager: &DiagnosticManager,
) {
    match &mut invoke.target {
        ExprInvokeTarget::Method(select) => {
            rewrite_expr_for_maps(select.obj.as_mut(), map_consts, unhandled, allow_map_value, manager);
        }
        ExprInvokeTarget::Expr(expr) => {
            rewrite_expr_for_maps(expr.as_mut(), map_consts, unhandled, allow_map_value, manager);
        }
        ExprInvokeTarget::Closure(closure) => {
            rewrite_expr_for_maps(closure.body.as_mut(), map_consts, unhandled, allow_map_value, manager);
        }
        ExprInvokeTarget::Function(_) | ExprInvokeTarget::Type(_) | ExprInvokeTarget::BinOp(_) => {}
    }

    for arg in &mut invoke.args {
        rewrite_expr_for_maps(arg, map_consts, unhandled, allow_map_value, manager);
    }
}

fn try_fold_map_len(expr: &Expr, map_consts: &HashMap<String, ValueMap>) -> Option<Expr> {
    match expr.kind() {
        ExprKind::Select(select) if select.field.as_str() == "len" => {
            let map_name = locator_for_map_const(select.obj.as_ref(), map_consts)?;
            let map = map_consts.get(&map_name)?;
            Some(Expr::value(Value::int(map.len() as i64)))
        }
        ExprKind::Invoke(invoke) if invoke.args.is_empty() => {
            if let ExprInvokeTarget::Method(select) = &invoke.target {
                if select.field.as_str() == "len" {
                    let map_name = locator_for_map_const(select.obj.as_ref(), map_consts)?;
                    let map = map_consts.get(&map_name)?;
                    return Some(Expr::value(Value::int(map.len() as i64)));
                }
            }
            None
        }
        _ => None,
    }
}

fn try_fold_map_index(
    expr: &Expr,
    map_consts: &HashMap<String, ValueMap>,
    manager: &DiagnosticManager,
) -> Option<Expr> {
    let ExprKind::Index(index) = expr.kind() else {
        return None;
    };
    let map_name = locator_for_map_const(index.obj.as_ref(), map_consts)?;
    let map = map_consts.get(&map_name)?;
    let key = literal_key_for_map_index(index)?;

    for entry in &map.entries {
        if entry.key == key {
            return Some(Expr::value(entry.value.clone()));
        }
    }

    manager.add_diagnostic(
        Diagnostic::error(format!(
            "HashMap constant does not contain key '{}'",
            key
        ))
        .with_source_context(STAGE_CONST_EVAL),
    );
    None
}

fn locator_for_map_const(
    expr: &Expr,
    map_consts: &HashMap<String, ValueMap>,
) -> Option<String> {
    let ExprKind::Locator(locator) = expr.kind() else {
        return None;
    };
    map_name_from_locator(locator, map_consts)
}

fn map_name_from_locator(
    locator: &Locator,
    map_consts: &HashMap<String, ValueMap>,
) -> Option<String> {
    let ident = locator.as_ident()?.as_str();
    if map_consts.contains_key(ident) {
        Some(ident.to_string())
    } else {
        None
    }
}

fn literal_key_for_map_index(index: &ExprIndex) -> Option<Value> {
    let ExprKind::Value(value) = index.index.kind() else {
        return None;
    };
    match value.as_ref() {
        Value::Int(_)
        | Value::Bool(_)
        | Value::Decimal(_)
        | Value::Char(_)
        | Value::String(_) => Some(value.as_ref().clone()),
        _ => None,
    }
}

fn strip_map_consts(ast: &mut Node, map_consts: &HashMap<String, ValueMap>) {
    match &mut ast.kind {
        NodeKind::File(file) => strip_map_consts_in_items(&mut file.items, map_consts),
        NodeKind::Item(item) => strip_map_consts_in_item(item, map_consts),
        NodeKind::Expr(_) | NodeKind::Query(_) | NodeKind::Schema(_) | NodeKind::Workspace(_) => {}
    }
}

fn strip_map_consts_in_items(items: &mut Vec<fp_core::ast::Item>, map_consts: &HashMap<String, ValueMap>) {
    items.retain_mut(|item| {
        strip_map_consts_in_item(item, map_consts);
        !is_map_const_item(item, map_consts)
    });
}

fn strip_map_consts_in_item(item: &mut fp_core::ast::Item, map_consts: &HashMap<String, ValueMap>) {
    match item.kind_mut() {
        fp_core::ast::ItemKind::Module(module) => {
            strip_map_consts_in_items(&mut module.items, map_consts);
        }
        fp_core::ast::ItemKind::Impl(impl_block) => {
            strip_map_consts_in_items(&mut impl_block.items, map_consts);
        }
        _ => {}
    }
}

fn is_map_const_item(item: &fp_core::ast::Item, map_consts: &HashMap<String, ValueMap>) -> bool {
    let fp_core::ast::ItemKind::DefConst(def) = item.kind() else {
        return false;
    };
    let ExprKind::Value(value) = def.value.kind() else {
        return false;
    };
    let Value::Map(_) = value.as_ref() else {
        return false;
    };
    map_consts.contains_key(def.name.as_str())
}

fn rewrite_const_lists(
    ast: &mut Node,
    outcome: &ConstEvalOutcome,
    manager: &DiagnosticManager,
) -> Result<(), String> {
    let mut list_consts: HashMap<String, ValueList> = HashMap::new();
    let list_targets = collect_list_const_targets(ast);
    for (name, value) in &outcome.evaluated_constants {
        if !list_targets.contains(name) {
            continue;
        }
        if let Value::List(list) = value {
            list_consts.insert(name.clone(), list.clone());
        }
    }

    if list_consts.is_empty() {
        return Ok(());
    }

    let mut unhandled: HashSet<String> = HashSet::new();
    rewrite_node_for_lists(ast, &list_consts, &mut unhandled, false, manager);

    if !unhandled.is_empty() {
        let mut names: Vec<String> = unhandled.into_iter().collect();
        names.sort();
        return Err(format!(
            "compile-time-only list values were used at runtime: {}",
            names.join(", ")
        ));
    }

    strip_list_consts(ast, &list_consts);
    Ok(())
}

fn collect_list_const_targets(ast: &Node) -> HashSet<String> {
    let mut targets = HashSet::new();
    collect_list_const_targets_in_node(ast, &mut targets);
    targets
}

fn collect_list_const_targets_in_node(node: &Node, targets: &mut HashSet<String>) {
    match &node.kind {
        NodeKind::File(file) => {
            for item in &file.items {
                collect_list_const_targets_in_item(item, targets);
            }
        }
        NodeKind::Item(item) => collect_list_const_targets_in_item(item, targets),
        NodeKind::Expr(_) | NodeKind::Query(_) | NodeKind::Schema(_) | NodeKind::Workspace(_) => {}
    }
}

fn collect_list_const_targets_in_item(item: &fp_core::ast::Item, targets: &mut HashSet<String>) {
    match item.kind() {
        fp_core::ast::ItemKind::Module(module) => {
            for child in &module.items {
                collect_list_const_targets_in_item(child, targets);
            }
        }
        fp_core::ast::ItemKind::Impl(impl_block) => {
            for child in &impl_block.items {
                collect_list_const_targets_in_item(child, targets);
            }
        }
        fp_core::ast::ItemKind::DefConst(def) => {
            if is_list_const_target(def) {
                targets.insert(def.name.as_str().to_string());
            }
        }
        _ => {}
    }
}

fn is_list_const_target(def: &fp_core::ast::ItemDefConst) -> bool {
    let ExprKind::Value(value) = def.value.kind() else {
        return false;
    };
    if !matches!(value.as_ref(), Value::List(_)) {
        return false;
    }
    if let Some(ty) = def.ty_annotation() {
        return !is_array_ty(ty);
    }
    if let Some(ty) = def.ty.as_ref() {
        return !is_array_ty(ty);
    }
    true
}

fn is_array_ty(ty: &fp_core::ast::Ty) -> bool {
    matches!(ty, fp_core::ast::Ty::Array(_))
}

fn rewrite_node_for_lists(
    node: &mut Node,
    list_consts: &HashMap<String, ValueList>,
    unhandled: &mut HashSet<String>,
    allow_list_value: bool,
    manager: &DiagnosticManager,
) {
    match &mut node.kind {
        NodeKind::File(file) => {
            for item in &mut file.items {
                rewrite_item_for_lists(item, list_consts, unhandled, manager);
            }
        }
        NodeKind::Item(item) => rewrite_item_for_lists(item, list_consts, unhandled, manager),
        NodeKind::Expr(expr) => {
            rewrite_expr_for_lists(expr, list_consts, unhandled, allow_list_value, manager);
        }
        NodeKind::Query(_) | NodeKind::Schema(_) | NodeKind::Workspace(_) => {}
    }
}

fn rewrite_item_for_lists(
    item: &mut fp_core::ast::Item,
    list_consts: &HashMap<String, ValueList>,
    unhandled: &mut HashSet<String>,
    manager: &DiagnosticManager,
) {
    match item.kind_mut() {
        fp_core::ast::ItemKind::Module(module) => {
            for child in &mut module.items {
                rewrite_item_for_lists(child, list_consts, unhandled, manager);
            }
        }
        fp_core::ast::ItemKind::Impl(impl_block) => {
            for child in &mut impl_block.items {
                rewrite_item_for_lists(child, list_consts, unhandled, manager);
            }
        }
        fp_core::ast::ItemKind::DefFunction(func) => {
            rewrite_expr_for_lists(func.body.as_mut(), list_consts, unhandled, false, manager);
        }
        fp_core::ast::ItemKind::DefConst(def) => {
            rewrite_expr_for_lists(def.value.as_mut(), list_consts, unhandled, true, manager);
        }
        fp_core::ast::ItemKind::DefStatic(def) => {
            rewrite_expr_for_lists(def.value.as_mut(), list_consts, unhandled, false, manager);
        }
        fp_core::ast::ItemKind::Expr(expr) => {
            rewrite_expr_for_lists(expr, list_consts, unhandled, false, manager);
        }
        _ => {}
    }
}

fn rewrite_expr_for_lists(
    expr: &mut Expr,
    list_consts: &HashMap<String, ValueList>,
    unhandled: &mut HashSet<String>,
    allow_list_value: bool,
    manager: &DiagnosticManager,
) {
    if let Some(replacement) = try_fold_list_len(expr, list_consts) {
        *expr = replacement;
        return;
    }
    if let Some(replacement) = try_fold_list_index(expr, list_consts, manager) {
        *expr = replacement;
        return;
    }

    match expr.kind_mut() {
        ExprKind::Value(value) => {
            if matches!(value.as_ref(), Value::List(_)) && !allow_list_value {
                manager.add_diagnostic(
                    Diagnostic::error(
                        "list values are only supported during compile-time evaluation"
                            .to_string(),
                    )
                    .with_source_context(STAGE_CONST_EVAL),
                );
                unhandled.insert("<list literal>".to_string());
            }
        }
        ExprKind::Id(_) => {}
        ExprKind::Locator(locator) => {
            if let Some(name) = list_name_from_locator(locator, list_consts) {
                unhandled.insert(name);
            }
        }
        ExprKind::Block(block) => {
            for stmt in &mut block.stmts {
                rewrite_block_stmt_for_lists(stmt, list_consts, unhandled, allow_list_value, manager);
            }
            if let Some(expr) = block.last_expr_mut() {
                rewrite_expr_for_lists(expr, list_consts, unhandled, allow_list_value, manager);
            }
        }
        ExprKind::If(expr_if) => {
            rewrite_expr_for_lists(expr_if.cond.as_mut(), list_consts, unhandled, allow_list_value, manager);
            rewrite_expr_for_lists(expr_if.then.as_mut(), list_consts, unhandled, allow_list_value, manager);
            if let Some(elze) = expr_if.elze.as_mut() {
                rewrite_expr_for_lists(elze, list_consts, unhandled, allow_list_value, manager);
            }
        }
        ExprKind::Loop(expr_loop) => {
            rewrite_expr_for_lists(expr_loop.body.as_mut(), list_consts, unhandled, allow_list_value, manager);
        }
        ExprKind::While(expr_while) => {
            rewrite_expr_for_lists(expr_while.cond.as_mut(), list_consts, unhandled, allow_list_value, manager);
            rewrite_expr_for_lists(expr_while.body.as_mut(), list_consts, unhandled, allow_list_value, manager);
        }
        ExprKind::For(expr_for) => {
            rewrite_expr_for_lists(expr_for.iter.as_mut(), list_consts, unhandled, allow_list_value, manager);
            rewrite_expr_for_lists(expr_for.body.as_mut(), list_consts, unhandled, allow_list_value, manager);
        }
        ExprKind::Match(expr_match) => {
            for case in &mut expr_match.cases {
                rewrite_expr_for_lists(case.cond.as_mut(), list_consts, unhandled, allow_list_value, manager);
                rewrite_expr_for_lists(case.body.as_mut(), list_consts, unhandled, allow_list_value, manager);
            }
        }
        ExprKind::BinOp(binop) => {
            rewrite_expr_for_lists(binop.lhs.as_mut(), list_consts, unhandled, allow_list_value, manager);
            rewrite_expr_for_lists(binop.rhs.as_mut(), list_consts, unhandled, allow_list_value, manager);
        }
        ExprKind::UnOp(unop) => {
            rewrite_expr_for_lists(unop.val.as_mut(), list_consts, unhandled, allow_list_value, manager);
        }
        ExprKind::Let(expr_let) => {
            rewrite_expr_for_lists(expr_let.expr.as_mut(), list_consts, unhandled, allow_list_value, manager);
        }
        ExprKind::Assign(assign) => {
            rewrite_expr_for_lists(assign.target.as_mut(), list_consts, unhandled, allow_list_value, manager);
            rewrite_expr_for_lists(assign.value.as_mut(), list_consts, unhandled, allow_list_value, manager);
        }
        ExprKind::Invoke(invoke) => {
            rewrite_invoke_for_lists(invoke, list_consts, unhandled, allow_list_value, manager);
        }
        ExprKind::Select(select) => {
            rewrite_expr_for_lists(select.obj.as_mut(), list_consts, unhandled, allow_list_value, manager);
        }
        ExprKind::Index(index) => {
            rewrite_expr_for_lists(index.obj.as_mut(), list_consts, unhandled, allow_list_value, manager);
            rewrite_expr_for_lists(index.index.as_mut(), list_consts, unhandled, allow_list_value, manager);
        }
        ExprKind::Struct(struct_expr) => {
            rewrite_expr_for_lists(struct_expr.name.as_mut(), list_consts, unhandled, allow_list_value, manager);
            for field in &mut struct_expr.fields {
                if let Some(value) = field.value.as_mut() {
                    rewrite_expr_for_lists(value, list_consts, unhandled, allow_list_value, manager);
                }
            }
            if let Some(update) = struct_expr.update.as_mut() {
                rewrite_expr_for_lists(update, list_consts, unhandled, allow_list_value, manager);
            }
        }
        ExprKind::Structural(struct_expr) => {
            for field in &mut struct_expr.fields {
                if let Some(value) = field.value.as_mut() {
                    rewrite_expr_for_lists(value, list_consts, unhandled, allow_list_value, manager);
                }
            }
        }
        ExprKind::Array(array) => {
            for elem in &mut array.values {
                rewrite_expr_for_lists(elem, list_consts, unhandled, allow_list_value, manager);
            }
        }
        ExprKind::ArrayRepeat(repeat) => {
            rewrite_expr_for_lists(repeat.elem.as_mut(), list_consts, unhandled, allow_list_value, manager);
            rewrite_expr_for_lists(repeat.len.as_mut(), list_consts, unhandled, allow_list_value, manager);
        }
        ExprKind::Tuple(tuple) => {
            for elem in &mut tuple.values {
                rewrite_expr_for_lists(elem, list_consts, unhandled, allow_list_value, manager);
            }
        }
        ExprKind::Paren(paren) => {
            rewrite_expr_for_lists(paren.expr.as_mut(), list_consts, unhandled, allow_list_value, manager);
        }
        ExprKind::Cast(cast) => {
            rewrite_expr_for_lists(cast.expr.as_mut(), list_consts, unhandled, allow_list_value, manager);
        }
        ExprKind::Reference(reference) => {
            rewrite_expr_for_lists(reference.referee.as_mut(), list_consts, unhandled, allow_list_value, manager);
        }
        ExprKind::Dereference(deref) => {
            rewrite_expr_for_lists(deref.referee.as_mut(), list_consts, unhandled, allow_list_value, manager);
        }
        ExprKind::Try(expr_try) => {
            rewrite_expr_for_lists(expr_try.expr.as_mut(), list_consts, unhandled, allow_list_value, manager);
        }
        ExprKind::Async(expr_async) => {
            rewrite_expr_for_lists(expr_async.expr.as_mut(), list_consts, unhandled, allow_list_value, manager);
        }
        ExprKind::Closure(closure) => {
            rewrite_expr_for_lists(closure.body.as_mut(), list_consts, unhandled, allow_list_value, manager);
        }
        ExprKind::Closured(closured) => {
            rewrite_expr_for_lists(closured.expr.as_mut(), list_consts, unhandled, allow_list_value, manager);
        }
        ExprKind::Quote(quote) => {
            for stmt in &mut quote.block.stmts {
                rewrite_block_stmt_for_lists(stmt, list_consts, unhandled, allow_list_value, manager);
            }
            if let Some(expr) = quote.block.last_expr_mut() {
                rewrite_expr_for_lists(expr, list_consts, unhandled, allow_list_value, manager);
            }
        }
        ExprKind::Splice(splice) => {
            rewrite_expr_for_lists(splice.token.as_mut(), list_consts, unhandled, allow_list_value, manager);
        }
        ExprKind::Await(expr_await) => {
            rewrite_expr_for_lists(expr_await.base.as_mut(), list_consts, unhandled, allow_list_value, manager);
        }
        ExprKind::IntrinsicCall(call) => match &mut call.payload {
            fp_core::intrinsics::IntrinsicCallPayload::Format { template } => {
                for arg in &mut template.args {
                    if let Some(replacement) = try_fold_list_display(arg, list_consts) {
                        *arg = replacement;
                        continue;
                    }
                    rewrite_expr_for_lists(arg, list_consts, unhandled, allow_list_value, manager);
                }
                for kwarg in &mut template.kwargs {
                    rewrite_expr_for_lists(&mut kwarg.value, list_consts, unhandled, allow_list_value, manager);
                }
            }
            fp_core::intrinsics::IntrinsicCallPayload::Args { args } => {
                for arg in args {
                    rewrite_expr_for_lists(arg, list_consts, unhandled, allow_list_value, manager);
                }
            }
        },
        ExprKind::IntrinsicContainer(container) => {
            container.for_each_expr_mut(|expr| {
                rewrite_expr_for_lists(expr, list_consts, unhandled, allow_list_value, manager);
            });
        }
        ExprKind::FormatString(format) => {
            for arg in &mut format.args {
                rewrite_expr_for_lists(arg, list_consts, unhandled, allow_list_value, manager);
            }
            for kwarg in &mut format.kwargs {
                rewrite_expr_for_lists(&mut kwarg.value, list_consts, unhandled, allow_list_value, manager);
            }
        }
        ExprKind::Splat(splat) => {
            rewrite_expr_for_lists(splat.iter.as_mut(), list_consts, unhandled, allow_list_value, manager);
        }
        ExprKind::SplatDict(splat) => {
            rewrite_expr_for_lists(splat.dict.as_mut(), list_consts, unhandled, allow_list_value, manager);
        }
        ExprKind::Any(_) | ExprKind::Macro(_) | ExprKind::Item(_) => {}
        ExprKind::Range(range) => {
            if let Some(start) = range.start.as_mut() {
                rewrite_expr_for_lists(start, list_consts, unhandled, allow_list_value, manager);
            }
            if let Some(end) = range.end.as_mut() {
                rewrite_expr_for_lists(end, list_consts, unhandled, allow_list_value, manager);
            }
            if let Some(step) = range.step.as_mut() {
                rewrite_expr_for_lists(step, list_consts, unhandled, allow_list_value, manager);
            }
        }
    }
}

fn rewrite_block_stmt_for_lists(
    stmt: &mut fp_core::ast::BlockStmt,
    list_consts: &HashMap<String, ValueList>,
    unhandled: &mut HashSet<String>,
    allow_list_value: bool,
    manager: &DiagnosticManager,
) {
    match stmt {
        fp_core::ast::BlockStmt::Expr(expr_stmt) => {
            rewrite_expr_for_lists(expr_stmt.expr.as_mut(), list_consts, unhandled, allow_list_value, manager);
        }
        fp_core::ast::BlockStmt::Let(stmt_let) => {
            if let Some(init) = stmt_let.init.as_mut() {
                rewrite_expr_for_lists(init, list_consts, unhandled, allow_list_value, manager);
            }
            if let Some(diverge) = stmt_let.diverge.as_mut() {
                rewrite_expr_for_lists(diverge, list_consts, unhandled, allow_list_value, manager);
            }
        }
        fp_core::ast::BlockStmt::Item(item) => {
            rewrite_item_for_lists(item.as_mut(), list_consts, unhandled, manager);
        }
        fp_core::ast::BlockStmt::Noop | fp_core::ast::BlockStmt::Any(_) => {}
    }
}

fn rewrite_invoke_for_lists(
    invoke: &mut ExprInvoke,
    list_consts: &HashMap<String, ValueList>,
    unhandled: &mut HashSet<String>,
    allow_list_value: bool,
    manager: &DiagnosticManager,
) {
    match &mut invoke.target {
        ExprInvokeTarget::Method(select) => {
            rewrite_expr_for_lists(select.obj.as_mut(), list_consts, unhandled, allow_list_value, manager);
        }
        ExprInvokeTarget::Expr(expr) => {
            rewrite_expr_for_lists(expr.as_mut(), list_consts, unhandled, allow_list_value, manager);
        }
        ExprInvokeTarget::Closure(closure) => {
            rewrite_expr_for_lists(closure.body.as_mut(), list_consts, unhandled, allow_list_value, manager);
        }
        ExprInvokeTarget::Function(_) | ExprInvokeTarget::Type(_) | ExprInvokeTarget::BinOp(_) => {}
    }

    for arg in &mut invoke.args {
        rewrite_expr_for_lists(arg, list_consts, unhandled, allow_list_value, manager);
    }
}

fn try_fold_list_len(expr: &Expr, list_consts: &HashMap<String, ValueList>) -> Option<Expr> {
    match expr.kind() {
        ExprKind::Select(select) if select.field.as_str() == "len" => {
            let list_name = locator_for_list_const(select.obj.as_ref(), list_consts)?;
            let list = list_consts.get(&list_name)?;
            Some(Expr::value(Value::int(list.values.len() as i64)))
        }
        ExprKind::Invoke(invoke) if invoke.args.is_empty() => {
            if let ExprInvokeTarget::Method(select) = &invoke.target {
                if select.field.as_str() == "len" {
                    let list_name = locator_for_list_const(select.obj.as_ref(), list_consts)?;
                    let list = list_consts.get(&list_name)?;
                    return Some(Expr::value(Value::int(list.values.len() as i64)));
                }
            }
            None
        }
        _ => None,
    }
}

fn try_fold_list_index(
    expr: &Expr,
    list_consts: &HashMap<String, ValueList>,
    manager: &DiagnosticManager,
) -> Option<Expr> {
    let ExprKind::Index(index) = expr.kind() else {
        return None;
    };
    let list_name = locator_for_list_const(index.obj.as_ref(), list_consts)?;
    let list = list_consts.get(&list_name)?;
    let idx = literal_index_for_list(index)?;
    if let Some(value) = list.values.get(idx) {
        return Some(Expr::value(value.clone()));
    }
    manager.add_diagnostic(
        Diagnostic::error(format!(
            "list constant index {} is out of bounds (len={})",
            idx,
            list.values.len()
        ))
        .with_source_context(STAGE_CONST_EVAL),
    );
    None
}

fn locator_for_list_const(
    expr: &Expr,
    list_consts: &HashMap<String, ValueList>,
) -> Option<String> {
    let ExprKind::Locator(locator) = expr.kind() else {
        return None;
    };
    list_name_from_locator(locator, list_consts)
}

fn list_name_from_locator(
    locator: &Locator,
    list_consts: &HashMap<String, ValueList>,
) -> Option<String> {
    let ident = locator.as_ident()?.as_str();
    if list_consts.contains_key(ident) {
        Some(ident.to_string())
    } else {
        None
    }
}

fn try_fold_list_display(expr: &Expr, list_consts: &HashMap<String, ValueList>) -> Option<Expr> {
    let ExprKind::Locator(locator) = expr.kind() else {
        return None;
    };
    let name = list_name_from_locator(locator, list_consts)?;
    let list = list_consts.get(&name)?;
    Some(Expr::value(Value::string(format_list_for_display(list))))
}

fn literal_index_for_list(index: &ExprIndex) -> Option<usize> {
    let ExprKind::Value(value) = index.index.kind() else {
        return None;
    };
    match value.as_ref() {
        Value::Int(value) if value.value >= 0 => Some(value.value as usize),
        _ => None,
    }
}

fn format_list_for_display(list: &ValueList) -> String {
    let mut out = String::from("[");
    for (idx, value) in list.values.iter().enumerate() {
        if idx > 0 {
            out.push_str(", ");
        }
        out.push_str(&format_value_for_display(value));
    }
    out.push(']');
    out
}

fn format_value_for_display(value: &Value) -> String {
    match value {
        Value::Int(value) => value.value.to_string(),
        Value::Bool(value) => {
            if value.value { "true".to_string() } else { "false".to_string() }
        }
        Value::Decimal(value) => value.value.to_string(),
        Value::Char(value) => value.value.to_string(),
        Value::String(value) => value.value.clone(),
        Value::Unit(_) => "()".to_string(),
        Value::Null(_) => "null".to_string(),
        Value::List(list) => format_list_for_display(list),
        _ => value.to_string(),
    }
}

fn strip_list_consts(ast: &mut Node, list_consts: &HashMap<String, ValueList>) {
    match &mut ast.kind {
        NodeKind::File(file) => strip_list_consts_in_items(&mut file.items, list_consts),
        NodeKind::Item(item) => strip_list_consts_in_item(item, list_consts),
        NodeKind::Expr(_) | NodeKind::Query(_) | NodeKind::Schema(_) | NodeKind::Workspace(_) => {}
    }
}

fn strip_list_consts_in_items(items: &mut Vec<fp_core::ast::Item>, list_consts: &HashMap<String, ValueList>) {
    items.retain_mut(|item| {
        strip_list_consts_in_item(item, list_consts);
        !is_list_const_item(item, list_consts)
    });
}

fn strip_list_consts_in_item(item: &mut fp_core::ast::Item, list_consts: &HashMap<String, ValueList>) {
    match item.kind_mut() {
        fp_core::ast::ItemKind::Module(module) => {
            strip_list_consts_in_items(&mut module.items, list_consts);
        }
        fp_core::ast::ItemKind::Impl(impl_block) => {
            strip_list_consts_in_items(&mut impl_block.items, list_consts);
        }
        _ => {}
    }
}

fn is_list_const_item(item: &fp_core::ast::Item, list_consts: &HashMap<String, ValueList>) -> bool {
    let fp_core::ast::ItemKind::DefConst(def) = item.kind() else {
        return false;
    };
    let ExprKind::Value(value) = def.value.kind() else {
        return false;
    };
    let Value::List(_) = value.as_ref() else {
        return false;
    };
    list_consts.contains_key(def.name.as_str())
}
