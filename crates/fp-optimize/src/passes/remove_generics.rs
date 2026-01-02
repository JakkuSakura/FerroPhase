/// Remove generic template functions from the AST after specialization.
/// Generic functions should have been fully specialized during const evaluation,
/// so the templates are no longer needed.
use fp_core::ast::{
    BlockStmt, Expr, ExprFormatString, ExprInvokeTarget, ExprKind, ExprMatchCase, ExprSelect,
    ExprStruct, ExprStructural, ExprTuple, ExprUnOp, ExprWhile, FormatKwArg, Item, ItemDefFunction,
    ItemKind, Locator, Node, NodeKind, Value,
};
use fp_core::error::Result;
use std::collections::HashSet;

pub fn remove_generic_templates(ast: &mut Node) -> Result<()> {
    match ast.kind_mut() {
        NodeKind::File(file) => {
            let refs = collect_value_refs(&file.items);
            let mut module_path = Vec::new();
            retain_items(&mut file.items, &mut module_path, &refs);

            // Recursively process modules
            for item in &mut file.items {
                remove_generics_from_item(item, &refs, &mut Vec::new())?;
            }
            Ok(())
        }
        NodeKind::Item(item) => {
            let refs = collect_value_refs(std::slice::from_ref(item));
            remove_generics_from_item(item, &refs, &mut Vec::new())?;
            Ok(())
        }
        NodeKind::Expr(_) => Ok(()),
        NodeKind::Query(_) => Ok(()),
        NodeKind::Schema(_) => Ok(()),
        NodeKind::Workspace(_) => Ok(()),
    }
}

fn remove_generics_from_item(
    item: &mut Item,
    refs: &ValueRefs,
    module_path: &mut Vec<String>,
) -> Result<()> {
    match item.kind_mut() {
        ItemKind::Module(module) => {
            module
                .items
                .retain(|item| !should_drop_generic_template(item, module_path, refs));
            module_path.push(module.name.as_str().to_string());
            for child in &mut module.items {
                remove_generics_from_item(child, refs, module_path)?;
            }
            module_path.pop();
        }
        ItemKind::Impl(impl_block) => {
            impl_block
                .items
                .retain(|item| !should_drop_generic_template(item, module_path, refs));
            for child in &mut impl_block.items {
                remove_generics_from_item(child, refs, module_path)?;
            }
        }
        _ => {}
    }
    Ok(())
}

fn retain_items(items: &mut Vec<Item>, module_path: &mut Vec<String>, refs: &ValueRefs) {
    items.retain(|item| !should_drop_generic_template(item, module_path, refs));
    for item in items.iter_mut() {
        match item.kind_mut() {
            ItemKind::Module(module) => {
                module_path.push(module.name.as_str().to_string());
                retain_items(&mut module.items, module_path, refs);
                module_path.pop();
            }
            ItemKind::Impl(impl_block) => {
                retain_items(&mut impl_block.items, module_path, refs);
            }
            _ => {}
        }
    }
}

fn should_drop_generic_template(item: &Item, module_path: &[String], refs: &ValueRefs) -> bool {
    let Some(func) = is_generic_template(item) else {
        return false;
    };

    // Keep generic templates that are referenced as values; dropping them
    // leaves unresolved function paths during HIR/MIR lowering.
    if refs.is_referenced(module_path, func) {
        return false;
    }

    true
}

fn is_generic_template(item: &Item) -> Option<&ItemDefFunction> {
    match item.kind() {
        ItemKind::DefFunction(func) if !func.sig.generics_params.is_empty() => Some(func),
        _ => None,
    }
}

#[derive(Default)]
struct ValueRefs {
    qualified: HashSet<String>,
    unqualified: HashSet<(String, String)>,
}

impl ValueRefs {
    fn record_qualified(&mut self, name: String) {
        self.qualified.insert(name);
    }

    fn record_unqualified(&mut self, module: String, name: String) {
        self.unqualified.insert((module, name));
    }

    fn is_referenced(&self, module_path: &[String], func: &ItemDefFunction) -> bool {
        let module = module_key(module_path);
        let name = func.name.as_str().to_string();
        let qualified = qualify_name(module_path, &name);

        self.qualified.contains(&qualified) || self.unqualified.contains(&(module, name))
    }
}

fn collect_value_refs(items: &[Item]) -> ValueRefs {
    let mut refs = ValueRefs::default();
    let mut module_path = Vec::new();
    collect_value_refs_in_items(items, &mut module_path, &mut refs);
    refs
}

fn collect_value_refs_in_items(
    items: &[Item],
    module_path: &mut Vec<String>,
    refs: &mut ValueRefs,
) {
    for item in items {
        match item.kind() {
            ItemKind::Module(module) => {
                module_path.push(module.name.as_str().to_string());
                collect_value_refs_in_items(&module.items, module_path, refs);
                module_path.pop();
            }
            ItemKind::DefFunction(func) => {
                collect_value_refs_in_expr(&func.body, module_path, refs);
            }
            ItemKind::DefConst(def) => {
                collect_value_refs_in_expr(&def.value, module_path, refs);
            }
            ItemKind::DefStatic(def) => {
                collect_value_refs_in_expr(&def.value, module_path, refs);
            }
            ItemKind::Impl(impl_block) => {
                collect_value_refs_in_items(&impl_block.items, module_path, refs);
            }
            ItemKind::Expr(expr) => {
                collect_value_refs_in_expr(expr, module_path, refs);
            }
            _ => {}
        }
    }
}

fn collect_value_refs_in_expr(expr: &Expr, module_path: &[String], refs: &mut ValueRefs) {
    match expr.kind() {
        ExprKind::Locator(locator) => record_locator(locator, module_path, refs),
        ExprKind::Value(value) => collect_value_refs_in_value(value.as_ref(), module_path, refs),
        ExprKind::Block(block) => {
            for stmt in &block.stmts {
                match stmt {
                    BlockStmt::Item(item) => {
                        collect_value_refs_in_items(
                            std::slice::from_ref(item.as_ref()),
                            &mut module_path.to_vec(),
                            refs,
                        );
                    }
                    BlockStmt::Let(stmt_let) => {
                        if let Some(init) = &stmt_let.init {
                            collect_value_refs_in_expr(init, module_path, refs);
                        }
                        if let Some(diverge) = &stmt_let.diverge {
                            collect_value_refs_in_expr(diverge, module_path, refs);
                        }
                    }
                    BlockStmt::Expr(expr_stmt) => {
                        collect_value_refs_in_expr(expr_stmt.expr.as_ref(), module_path, refs);
                    }
                    BlockStmt::Noop | BlockStmt::Any(_) => {}
                }
            }
        }
        ExprKind::Match(m) => {
            if let Some(scrutinee) = &m.scrutinee {
                collect_value_refs_in_expr(scrutinee.as_ref(), module_path, refs);
            }
            for case in &m.cases {
                collect_value_refs_in_match_case(case, module_path, refs);
            }
        }
        ExprKind::If(expr_if) => {
            collect_value_refs_in_expr(expr_if.cond.as_ref(), module_path, refs);
            collect_value_refs_in_expr(expr_if.then.as_ref(), module_path, refs);
            if let Some(elze) = &expr_if.elze {
                collect_value_refs_in_expr(elze.as_ref(), module_path, refs);
            }
        }
        ExprKind::Loop(expr_loop) => {
            collect_value_refs_in_expr(expr_loop.body.as_ref(), module_path, refs);
        }
        ExprKind::While(ExprWhile { cond, body }) => {
            collect_value_refs_in_expr(cond.as_ref(), module_path, refs);
            collect_value_refs_in_expr(body.as_ref(), module_path, refs);
        }
        ExprKind::Invoke(invoke) => {
            collect_value_refs_in_invoke_target(&invoke.target, module_path, refs);
            for arg in &invoke.args {
                collect_value_refs_in_expr(arg, module_path, refs);
            }
        }
        ExprKind::BinOp(binop) => {
            collect_value_refs_in_expr(binop.lhs.as_ref(), module_path, refs);
            collect_value_refs_in_expr(binop.rhs.as_ref(), module_path, refs);
        }
        ExprKind::UnOp(ExprUnOp { val, .. }) => {
            collect_value_refs_in_expr(val.as_ref(), module_path, refs);
        }
        ExprKind::Assign(assign) => {
            collect_value_refs_in_expr(assign.target.as_ref(), module_path, refs);
            collect_value_refs_in_expr(assign.value.as_ref(), module_path, refs);
        }
        ExprKind::Select(select) => {
            collect_value_refs_in_expr(select.obj.as_ref(), module_path, refs);
        }
        ExprKind::Index(index) => {
            collect_value_refs_in_expr(index.obj.as_ref(), module_path, refs);
            collect_value_refs_in_expr(index.index.as_ref(), module_path, refs);
        }
        ExprKind::Struct(ExprStruct { name, fields, update }) => {
            collect_value_refs_in_expr(name.as_ref(), module_path, refs);
            for field in fields {
                if let Some(value) = &field.value {
                    collect_value_refs_in_expr(value, module_path, refs);
                }
            }
            if let Some(update) = update {
                collect_value_refs_in_expr(update.as_ref(), module_path, refs);
            }
        }
        ExprKind::Structural(ExprStructural { fields }) => {
            for field in fields {
                if let Some(value) = &field.value {
                    collect_value_refs_in_expr(value, module_path, refs);
                }
            }
        }
        ExprKind::Reference(reference) => {
            collect_value_refs_in_expr(reference.referee.as_ref(), module_path, refs);
        }
        ExprKind::Dereference(deref) => {
            collect_value_refs_in_expr(deref.referee.as_ref(), module_path, refs);
        }
        ExprKind::Cast(cast) => {
            collect_value_refs_in_expr(cast.expr.as_ref(), module_path, refs);
        }
        ExprKind::Tuple(ExprTuple { values }) => {
            for value in values {
                collect_value_refs_in_expr(value, module_path, refs);
            }
        }
        ExprKind::Try(expr_try) => {
            collect_value_refs_in_expr(expr_try.expr.as_ref(), module_path, refs);
        }
        ExprKind::For(expr_for) => {
            collect_value_refs_in_expr(expr_for.iter.as_ref(), module_path, refs);
            collect_value_refs_in_expr(expr_for.body.as_ref(), module_path, refs);
        }
        ExprKind::Async(async_expr) => {
            collect_value_refs_in_expr(async_expr.expr.as_ref(), module_path, refs);
        }
        ExprKind::Let(expr_let) => {
            collect_value_refs_in_expr(expr_let.expr.as_ref(), module_path, refs);
        }
        ExprKind::Closure(closure) => {
            collect_value_refs_in_expr(closure.body.as_ref(), module_path, refs);
        }
        ExprKind::Array(array) => {
            for value in &array.values {
                collect_value_refs_in_expr(value, module_path, refs);
            }
        }
        ExprKind::ArrayRepeat(repeat) => {
            collect_value_refs_in_expr(repeat.elem.as_ref(), module_path, refs);
            collect_value_refs_in_expr(repeat.len.as_ref(), module_path, refs);
        }
        ExprKind::IntrinsicContainer(container) => match container {
            fp_core::ast::ExprIntrinsicContainer::VecElements { elements } => {
                for element in elements {
                    collect_value_refs_in_expr(element, module_path, refs);
                }
            }
            fp_core::ast::ExprIntrinsicContainer::VecRepeat { elem, len } => {
                collect_value_refs_in_expr(elem.as_ref(), module_path, refs);
                collect_value_refs_in_expr(len.as_ref(), module_path, refs);
            }
            fp_core::ast::ExprIntrinsicContainer::HashMapEntries { entries } => {
                for entry in entries {
                    collect_value_refs_in_expr(&entry.key, module_path, refs);
                    collect_value_refs_in_expr(&entry.value, module_path, refs);
                }
            }
        },
        ExprKind::IntrinsicCall(call) => {
            match &call.payload {
                fp_core::intrinsics::IntrinsicCallPayload::Args { args } => {
                    for arg in args {
                        collect_value_refs_in_expr(arg, module_path, refs);
                    }
                }
                fp_core::intrinsics::IntrinsicCallPayload::Format { template } => {
                    collect_value_refs_in_format(template, module_path, refs);
                }
            }
        }
        ExprKind::Quote(quote) => {
            for stmt in &quote.block.stmts {
                match stmt {
                    BlockStmt::Item(item) => {
                        collect_value_refs_in_items(
                            std::slice::from_ref(item.as_ref()),
                            &mut module_path.to_vec(),
                            refs,
                        );
                    }
                    BlockStmt::Let(stmt_let) => {
                        if let Some(init) = &stmt_let.init {
                            collect_value_refs_in_expr(init, module_path, refs);
                        }
                        if let Some(diverge) = &stmt_let.diverge {
                            collect_value_refs_in_expr(diverge, module_path, refs);
                        }
                    }
                    BlockStmt::Expr(expr_stmt) => {
                        collect_value_refs_in_expr(expr_stmt.expr.as_ref(), module_path, refs);
                    }
                    BlockStmt::Noop | BlockStmt::Any(_) => {}
                }
            }
        }
        ExprKind::Splice(splice) => {
            collect_value_refs_in_expr(splice.token.as_ref(), module_path, refs);
        }
        ExprKind::Closured(closured) => {
            collect_value_refs_in_expr(closured.expr.as_ref(), module_path, refs);
        }
        ExprKind::Await(await_expr) => {
            collect_value_refs_in_expr(await_expr.base.as_ref(), module_path, refs);
        }
        ExprKind::Paren(paren) => {
            collect_value_refs_in_expr(paren.expr.as_ref(), module_path, refs);
        }
        ExprKind::Range(range) => {
            if let Some(start) = &range.start {
                collect_value_refs_in_expr(start.as_ref(), module_path, refs);
            }
            if let Some(end) = &range.end {
                collect_value_refs_in_expr(end.as_ref(), module_path, refs);
            }
            if let Some(step) = &range.step {
                collect_value_refs_in_expr(step.as_ref(), module_path, refs);
            }
        }
        ExprKind::FormatString(fmt) => {
            collect_value_refs_in_format(fmt, module_path, refs);
        }
        ExprKind::Splat(splat) => {
            collect_value_refs_in_expr(splat.iter.as_ref(), module_path, refs);
        }
        ExprKind::SplatDict(splat) => {
            collect_value_refs_in_expr(splat.dict.as_ref(), module_path, refs);
        }
        ExprKind::Macro(mac) => {
            let _ = mac;
        }
        ExprKind::Item(item) => {
            collect_value_refs_in_items(
                std::slice::from_ref(item.as_ref()),
                &mut module_path.to_vec(),
                refs,
            );
        }
        ExprKind::Id(_) | ExprKind::Any(_) => {}
    }
}

fn collect_value_refs_in_match_case(case: &ExprMatchCase, module_path: &[String], refs: &mut ValueRefs) {
    collect_value_refs_in_expr(case.cond.as_ref(), module_path, refs);
    if let Some(guard) = &case.guard {
        collect_value_refs_in_expr(guard.as_ref(), module_path, refs);
    }
    collect_value_refs_in_expr(case.body.as_ref(), module_path, refs);
}

fn collect_value_refs_in_invoke_target(
    target: &ExprInvokeTarget,
    module_path: &[String],
    refs: &mut ValueRefs,
) {
    match target {
        ExprInvokeTarget::Function(locator) => record_locator(locator, module_path, refs),
        ExprInvokeTarget::Type(_) => {}
        ExprInvokeTarget::Method(select) => {
            collect_value_refs_in_select(select, module_path, refs);
        }
        ExprInvokeTarget::Closure(func) => {
            collect_value_refs_in_expr(func.body.as_ref(), module_path, refs);
        }
        ExprInvokeTarget::BinOp(_) => {}
        ExprInvokeTarget::Expr(expr) => {
            collect_value_refs_in_expr(expr.as_ref(), module_path, refs);
        }
    }
}

fn collect_value_refs_in_select(select: &ExprSelect, module_path: &[String], refs: &mut ValueRefs) {
    collect_value_refs_in_expr(select.obj.as_ref(), module_path, refs);
}

fn collect_value_refs_in_value(value: &Value, module_path: &[String], refs: &mut ValueRefs) {
    match value {
        Value::Expr(expr) => collect_value_refs_in_expr(expr.as_ref(), module_path, refs),
        Value::Function(func) => {
            collect_value_refs_in_expr(func.body.as_ref(), module_path, refs);
        }
        _ => {}
    }
}

fn collect_value_refs_in_format(
    fmt: &ExprFormatString,
    module_path: &[String],
    refs: &mut ValueRefs,
) {
    for arg in &fmt.args {
        collect_value_refs_in_expr(arg, module_path, refs);
    }
    for kwarg in &fmt.kwargs {
        collect_value_refs_in_format_kwarg(kwarg, module_path, refs);
    }
    let _ = &fmt.parts;
}

fn collect_value_refs_in_format_kwarg(
    kwarg: &FormatKwArg,
    module_path: &[String],
    refs: &mut ValueRefs,
) {
    collect_value_refs_in_expr(&kwarg.value, module_path, refs);
}

fn record_locator(locator: &Locator, module_path: &[String], refs: &mut ValueRefs) {
    match locator {
        Locator::Ident(ident) => {
            let module = module_key(module_path);
            let name = ident.as_str().to_string();
            refs.record_unqualified(module, name.clone());
            refs.record_qualified(qualify_name(module_path, &name));
        }
        Locator::Path(path) => {
            let qualified = path
                .segments
                .iter()
                .map(|seg| seg.as_str())
                .collect::<Vec<_>>()
                .join("::");
            refs.record_qualified(qualified);
        }
        Locator::ParameterPath(path) => {
            let qualified = path
                .segments
                .iter()
                .map(|seg| seg.ident.as_str())
                .collect::<Vec<_>>()
                .join("::");
            let name = path
                .segments
                .last()
                .map(|seg| seg.ident.as_str().to_string())
                .unwrap_or_default();
            refs.record_qualified(qualified);
            if !name.is_empty() {
                refs.record_unqualified(module_key(module_path), name);
            }
        }
    }
}

fn qualify_name(module_path: &[String], name: &str) -> String {
    if module_path.is_empty() {
        name.to_string()
    } else {
        format!("{}::{}", module_path.join("::"), name)
    }
}

fn module_key(module_path: &[String]) -> String {
    if module_path.is_empty() {
        String::new()
    } else {
        module_path.join("::")
    }
}
