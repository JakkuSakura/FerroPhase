use fp_core::ast::{
    BlockStmt, Expr, ExprFormatString, ExprInvokeTarget, ExprKind, ExprMatchCase, ExprSelect,
    ExprStruct, ExprStructural, ExprTuple, ExprUnOp, ExprWhile, FormatKwArg, Ident, Item,
    ItemDefEnum, ItemDefFunction, ItemDefStructural, ItemKind, Locator, Node, NodeKind, Path,
    StructuralField, Ty, TypeBinaryOpKind, TypeEnum, TypeStructural, Value, ValueNone,
};
use fp_core::error::Result;
use std::collections::{HashMap, HashSet};

/// Materialize structural type aliases and basic type arithmetic into concrete
/// named struct definitions.
///
/// This pass exists primarily for backends like Rust that cannot represent
/// anonymous structural types (e.g. `struct { a: i64 }`) or symbolic type-level
/// arithmetic (e.g. `A + B`) directly.
pub struct MaterializeTypesOptions {
    pub include_unions: bool,
}

pub fn materialize_structural_types(node: &mut Node) -> Result<()> {
    materialize_structural_types_with(
        node,
        &MaterializeTypesOptions {
            include_unions: false,
        },
    )
}

pub fn materialize_structural_types_with_unions(node: &mut Node) -> Result<()> {
    materialize_structural_types_with(
        node,
        &MaterializeTypesOptions {
            include_unions: true,
        },
    )
}

fn materialize_structural_types_with(
    node: &mut Node,
    options: &MaterializeTypesOptions,
) -> Result<()> {
    match node.kind_mut() {
        NodeKind::File(file) => materialize_items(&mut file.items, options),
        NodeKind::Item(item) => materialize_item(item, options),
        NodeKind::Expr(_) | NodeKind::Schema(_) | NodeKind::Query(_) | NodeKind::Workspace(_) => {
            Ok(())
        }
    }
}

/// Remove generic template functions from the AST after specialization.
/// Generic functions should have been fully specialized during const evaluation,
/// so the templates are no longer needed.
pub fn remove_generic_templates(ast: &mut Node) -> Result<()> {
    match ast.kind_mut() {
        NodeKind::File(file) => {
            let refs = collect_value_refs(&file.items);
            let mut module_path = Vec::new();
            retain_items(&mut file.items, &mut module_path, &refs);

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
        ExprKind::Select(ExprSelect { expr, field }) => {
            collect_value_refs_in_expr(expr.as_ref(), module_path, refs);
            collect_value_refs_in_expr(field.as_ref(), module_path, refs);
        }
        ExprKind::Tuple(ExprTuple { items }) => {
            for item in items {
                collect_value_refs_in_expr(item, module_path, refs);
            }
        }
        ExprKind::Array(array) => {
            for item in &array.items {
                collect_value_refs_in_expr(item, module_path, refs);
            }
        }
        ExprKind::Struct(ExprStruct { fields, .. }) => {
            for field in fields {
                if let Some(value) = &field.value {
                    collect_value_refs_in_expr(value.as_ref(), module_path, refs);
                }
            }
        }
        ExprKind::Structural(ExprStructural { fields, .. }) => {
            for field in fields {
                collect_value_refs_in_expr(field.value.as_ref(), module_path, refs);
            }
        }
        ExprKind::Unary(ExprUnOp { expr, .. }) => {
            collect_value_refs_in_expr(expr.as_ref(), module_path, refs);
        }
        ExprKind::Binary(binary) => {
            collect_value_refs_in_expr(binary.lhs.as_ref(), module_path, refs);
            collect_value_refs_in_expr(binary.rhs.as_ref(), module_path, refs);
        }
        ExprKind::Access(access) => {
            collect_value_refs_in_expr(access.expr.as_ref(), module_path, refs);
            collect_value_refs_in_expr(access.field.as_ref(), module_path, refs);
        }
        ExprKind::Assign(assign) => {
            collect_value_refs_in_expr(assign.target.as_ref(), module_path, refs);
            collect_value_refs_in_expr(assign.value.as_ref(), module_path, refs);
        }
        ExprKind::Return(ret) => {
            if let Some(value) = &ret.value {
                collect_value_refs_in_expr(value.as_ref(), module_path, refs);
            }
        }
        ExprKind::Break(expr_break) => {
            if let Some(value) = &expr_break.value {
                collect_value_refs_in_expr(value.as_ref(), module_path, refs);
            }
        }
        ExprKind::Continue(_) => {}
        ExprKind::FormatString(ExprFormatString { fragments }) => {
            for fragment in fragments {
                if let fp_core::ast::FormatFragment::Expr(expr) = fragment {
                    collect_value_refs_in_expr(expr.as_ref(), module_path, refs);
                }
            }
        }
        ExprKind::Lambda(lambda) => {
            collect_value_refs_in_expr(lambda.body.as_ref(), module_path, refs);
        }
        ExprKind::Quote(expr) => {
            if let Some(body) = &expr.body {
                collect_value_refs_in_expr(body, module_path, refs);
            }
        }
        ExprKind::Unquote(expr) => {
            collect_value_refs_in_expr(expr.body.as_ref(), module_path, refs);
        }
        ExprKind::ExprMap(map) => {
            collect_value_refs_in_expr(map.body.as_ref(), module_path, refs);
        }
        ExprKind::For(expr_for) => {
            collect_value_refs_in_expr(expr_for.iter.as_ref(), module_path, refs);
            collect_value_refs_in_expr(expr_for.body.as_ref(), module_path, refs);
        }
        ExprKind::Range(range) => {
            collect_value_refs_in_expr(range.start.as_ref(), module_path, refs);
            collect_value_refs_in_expr(range.end.as_ref(), module_path, refs);
        }
        ExprKind::Cast(cast) => {
            collect_value_refs_in_expr(cast.expr.as_ref(), module_path, refs);
        }
        ExprKind::Typeof(t) => {
            collect_value_refs_in_expr(t.expr.as_ref(), module_path, refs);
        }
        ExprKind::Yield(expr_yield) => {
            if let Some(value) = &expr_yield.value {
                collect_value_refs_in_expr(value.as_ref(), module_path, refs);
            }
        }
        ExprKind::Repr(_) => {}
        ExprKind::FuncRef(func_ref) => {
            collect_value_refs_in_expr(func_ref.func.as_ref(), module_path, refs);
        }
        ExprKind::Typed(expr_typed) => {
            collect_value_refs_in_expr(expr_typed.expr.as_ref(), module_path, refs);
        }
        ExprKind::Spawn(spawn) => {
            collect_value_refs_in_expr(spawn.expr.as_ref(), module_path, refs);
        }
        ExprKind::Await(await_expr) => {
            collect_value_refs_in_expr(await_expr.expr.as_ref(), module_path, refs);
        }
        ExprKind::Fetch(fetch) => {
            collect_value_refs_in_expr(fetch.expr.as_ref(), module_path, refs);
        }
        ExprKind::Assertion(assertion) => {
            collect_value_refs_in_expr(assertion.expr.as_ref(), module_path, refs);
        }
        ExprKind::Condition(cond) => {
            collect_value_refs_in_expr(cond.value.as_ref(), module_path, refs);
        }
        ExprKind::Update(update) => {
            collect_value_refs_in_expr(update.expr.as_ref(), module_path, refs);
        }
        ExprKind::Infix(infix) => {
            collect_value_refs_in_expr(infix.lhs.as_ref(), module_path, refs);
            collect_value_refs_in_expr(infix.rhs.as_ref(), module_path, refs);
        }
        ExprKind::Coalesce(coalesce) => {
            collect_value_refs_in_expr(coalesce.lhs.as_ref(), module_path, refs);
            collect_value_refs_in_expr(coalesce.rhs.as_ref(), module_path, refs);
        }
        ExprKind::Try(expr_try) => {
            collect_value_refs_in_expr(expr_try.body.as_ref(), module_path, refs);
        }
        ExprKind::SelectWith(expr_select) => {
            collect_value_refs_in_expr(expr_select.expr.as_ref(), module_path, refs);
            collect_value_refs_in_expr(expr_select.field.as_ref(), module_path, refs);
        }
        ExprKind::KeyValueMap(map) => {
            for entry in &map.entries {
                collect_value_refs_in_expr(entry.key.as_ref(), module_path, refs);
                collect_value_refs_in_expr(entry.value.as_ref(), module_path, refs);
            }
        }
        ExprKind::Call(call) => {
            collect_value_refs_in_expr(call.func.as_ref(), module_path, refs);
            for arg in &call.args {
                collect_value_refs_in_expr(arg, module_path, refs);
            }
        }
        ExprKind::AwaitCall(call) => {
            collect_value_refs_in_expr(call.func.as_ref(), module_path, refs);
            for arg in &call.args {
                collect_value_refs_in_expr(arg, module_path, refs);
            }
        }
        ExprKind::MatchTuple(expr_match) => {
            for item in &expr_match.values {
                collect_value_refs_in_expr(item, module_path, refs);
            }
            for case in &expr_match.cases {
                collect_value_refs_in_match_case(case, module_path, refs);
            }
        }
        ExprKind::Switch(expr_switch) => {
            collect_value_refs_in_expr(expr_switch.expr.as_ref(), module_path, refs);
            for case in &expr_switch.cases {
                collect_value_refs_in_match_case(case, module_path, refs);
            }
            if let Some(default) = &expr_switch.default {
                collect_value_refs_in_expr(default.as_ref(), module_path, refs);
            }
        }
        ExprKind::InfixCall(call) => {
            collect_value_refs_in_expr(call.lhs.as_ref(), module_path, refs);
            collect_value_refs_in_expr(call.rhs.as_ref(), module_path, refs);
        }
        ExprKind::Any(_) => {}
        ExprKind::SizeOf(expr) => {
            collect_value_refs_in_expr(expr.ty.as_ref(), module_path, refs);
        }
    }
}

fn collect_value_refs_in_match_case(
    case: &ExprMatchCase,
    module_path: &[String],
    refs: &mut ValueRefs,
) {
    if let Some(cond) = &case.cond {
        collect_value_refs_in_expr(cond.as_ref(), module_path, refs);
    }
    collect_value_refs_in_expr(case.body.as_ref(), module_path, refs);
}

fn collect_value_refs_in_invoke_target(
    target: &ExprInvokeTarget,
    module_path: &[String],
    refs: &mut ValueRefs,
) {
    match target {
        ExprInvokeTarget::Expr(expr) => collect_value_refs_in_expr(expr, module_path, refs),
        ExprInvokeTarget::Func(func) => collect_value_refs_in_expr(func, module_path, refs),
        ExprInvokeTarget::Instance(instance) => {
            collect_value_refs_in_expr(instance.expr.as_ref(), module_path, refs);
            collect_value_refs_in_expr(instance.method.as_ref(), module_path, refs);
        }
    }
}

fn collect_value_refs_in_value(value: &Value, module_path: &[String], refs: &mut ValueRefs) {
    match value {
        Value::Expr(expr) => collect_value_refs_in_expr(expr.as_ref(), module_path, refs),
        Value::Object(object) => {
            for item in &object.items {
                collect_value_refs_in_expr(item.value.as_ref(), module_path, refs);
            }
        }
        Value::Array(array) => {
            for item in &array.items {
                collect_value_refs_in_expr(item, module_path, refs);
            }
        }
        Value::Tuple(tuple) => {
            for item in &tuple.items {
                collect_value_refs_in_expr(item, module_path, refs);
            }
        }
        Value::Struct(struct_value) => {
            for field in &struct_value.fields {
                collect_value_refs_in_expr(field.value.as_ref(), module_path, refs);
            }
        }
        Value::Structural(struct_value) => {
            for field in &struct_value.fields {
                collect_value_refs_in_expr(field.value.as_ref(), module_path, refs);
            }
        }
        Value::FormatString(format) => {
            for fragment in &format.fragments {
                if let FormatKwArg::Expr(expr) = fragment {
                    collect_value_refs_in_expr(expr.as_ref(), module_path, refs);
                }
            }
        }
        Value::Match(match_value) => {
            collect_value_refs_in_expr(match_value.value.as_ref(), module_path, refs);
            for case in &match_value.cases {
                collect_value_refs_in_match_case(case, module_path, refs);
            }
        }
        Value::Invocation(invocation) => {
            collect_value_refs_in_expr(invocation.target.as_ref(), module_path, refs);
            for arg in &invocation.args {
                collect_value_refs_in_expr(arg, module_path, refs);
            }
        }
        Value::InvokeTarget(target) => {
            collect_value_refs_in_invoke_target(target, module_path, refs);
        }
        Value::Selector(selector) => {
            collect_value_refs_in_expr(selector.expr.as_ref(), module_path, refs);
            collect_value_refs_in_expr(selector.field.as_ref(), module_path, refs);
        }
        Value::Select(expr_select) => {
            collect_value_refs_in_expr(expr_select.expr.as_ref(), module_path, refs);
            collect_value_refs_in_expr(expr_select.field.as_ref(), module_path, refs);
        }
        Value::SelectWith(expr_select) => {
            collect_value_refs_in_expr(expr_select.expr.as_ref(), module_path, refs);
            collect_value_refs_in_expr(expr_select.field.as_ref(), module_path, refs);
        }
        Value::Call(call) => {
            collect_value_refs_in_expr(call.func.as_ref(), module_path, refs);
            for arg in &call.args {
                collect_value_refs_in_expr(arg, module_path, refs);
            }
        }
        Value::AwaitCall(call) => {
            collect_value_refs_in_expr(call.func.as_ref(), module_path, refs);
            for arg in &call.args {
                collect_value_refs_in_expr(arg, module_path, refs);
            }
        }
        Value::Lambda(lambda) => {
            collect_value_refs_in_expr(lambda.body.as_ref(), module_path, refs);
        }
        Value::Cast(cast) => {
            collect_value_refs_in_expr(cast.expr.as_ref(), module_path, refs);
        }
        Value::Update(update) => {
            collect_value_refs_in_expr(update.expr.as_ref(), module_path, refs);
        }
        Value::Infix(infix) => {
            collect_value_refs_in_expr(infix.lhs.as_ref(), module_path, refs);
            collect_value_refs_in_expr(infix.rhs.as_ref(), module_path, refs);
        }
        Value::Coalesce(coalesce) => {
            collect_value_refs_in_expr(coalesce.lhs.as_ref(), module_path, refs);
            collect_value_refs_in_expr(coalesce.rhs.as_ref(), module_path, refs);
        }
        Value::ExprStruct(ExprStruct { fields, .. }) => {
            for field in fields {
                if let Some(value) = &field.value {
                    collect_value_refs_in_expr(value.as_ref(), module_path, refs);
                }
            }
        }
        Value::ExprStructural(ExprStructural { fields, .. }) => {
            for field in fields {
                collect_value_refs_in_expr(field.value.as_ref(), module_path, refs);
            }
        }
        Value::ExprTuple(ExprTuple { items }) => {
            for item in items {
                collect_value_refs_in_expr(item, module_path, refs);
            }
        }
        Value::ExprSelect(ExprSelect { expr, field }) => {
            collect_value_refs_in_expr(expr.as_ref(), module_path, refs);
            collect_value_refs_in_expr(field.as_ref(), module_path, refs);
        }
        Value::ExprUnOp(ExprUnOp { expr, .. }) => {
            collect_value_refs_in_expr(expr.as_ref(), module_path, refs);
        }
        Value::ExprFormatString(ExprFormatString { fragments }) => {
            for fragment in fragments {
                if let fp_core::ast::FormatFragment::Expr(expr) = fragment {
                    collect_value_refs_in_expr(expr.as_ref(), module_path, refs);
                }
            }
        }
        Value::Locator(locator) => record_locator(locator, module_path, refs),
        Value::Any(_) => {}
        Value::ExprQuote(quote) => {
            if let Some(body) = &quote.body {
                collect_value_refs_in_expr(body, module_path, refs);
            }
        }
        Value::ExprUnquote(unquote) => {
            collect_value_refs_in_expr(unquote.body.as_ref(), module_path, refs);
        }
        Value::ExprMap(expr_map) => {
            collect_value_refs_in_expr(expr_map.body.as_ref(), module_path, refs);
        }
        Value::ExprFor(expr_for) => {
            collect_value_refs_in_expr(expr_for.iter.as_ref(), module_path, refs);
            collect_value_refs_in_expr(expr_for.body.as_ref(), module_path, refs);
        }
        Value::ExprRange(range) => {
            collect_value_refs_in_expr(range.start.as_ref(), module_path, refs);
            collect_value_refs_in_expr(range.end.as_ref(), module_path, refs);
        }
        Value::ExprYield(expr_yield) => {
            if let Some(value) = &expr_yield.value {
                collect_value_refs_in_expr(value.as_ref(), module_path, refs);
            }
        }
        Value::ExprAccess(access) => {
            collect_value_refs_in_expr(access.expr.as_ref(), module_path, refs);
            collect_value_refs_in_expr(access.field.as_ref(), module_path, refs);
        }
        Value::ExprAssign(assign) => {
            collect_value_refs_in_expr(assign.target.as_ref(), module_path, refs);
            collect_value_refs_in_expr(assign.value.as_ref(), module_path, refs);
        }
        Value::ExprReturn(ret) => {
            if let Some(value) = &ret.value {
                collect_value_refs_in_expr(value.as_ref(), module_path, refs);
            }
        }
        Value::ExprBreak(expr_break) => {
            if let Some(value) = &expr_break.value {
                collect_value_refs_in_expr(value.as_ref(), module_path, refs);
            }
        }
        Value::ExprContinue(_) => {}
        Value::ExprLoop(expr_loop) => {
            collect_value_refs_in_expr(expr_loop.body.as_ref(), module_path, refs);
        }
        Value::ExprWhile(ExprWhile { cond, body }) => {
            collect_value_refs_in_expr(cond.as_ref(), module_path, refs);
            collect_value_refs_in_expr(body.as_ref(), module_path, refs);
        }
        Value::ExprFuncRef(func_ref) => {
            collect_value_refs_in_expr(func_ref.func.as_ref(), module_path, refs);
        }
        Value::ExprTyped(expr_typed) => {
            collect_value_refs_in_expr(expr_typed.expr.as_ref(), module_path, refs);
        }
        Value::ExprSpawn(spawn) => {
            collect_value_refs_in_expr(spawn.expr.as_ref(), module_path, refs);
        }
        Value::ExprAwait(await_expr) => {
            collect_value_refs_in_expr(await_expr.expr.as_ref(), module_path, refs);
        }
        Value::ExprFetch(fetch) => {
            collect_value_refs_in_expr(fetch.expr.as_ref(), module_path, refs);
        }
        Value::ExprAssertion(assertion) => {
            collect_value_refs_in_expr(assertion.expr.as_ref(), module_path, refs);
        }
        Value::ExprCondition(cond) => {
            collect_value_refs_in_expr(cond.value.as_ref(), module_path, refs);
        }
        Value::ExprUpdate(update) => {
            collect_value_refs_in_expr(update.expr.as_ref(), module_path, refs);
        }
        Value::ExprInfix(infix) => {
            collect_value_refs_in_expr(infix.lhs.as_ref(), module_path, refs);
            collect_value_refs_in_expr(infix.rhs.as_ref(), module_path, refs);
        }
        Value::ExprCoalesce(coalesce) => {
            collect_value_refs_in_expr(coalesce.lhs.as_ref(), module_path, refs);
            collect_value_refs_in_expr(coalesce.rhs.as_ref(), module_path, refs);
        }
        Value::ExprTry(expr_try) => {
            collect_value_refs_in_expr(expr_try.body.as_ref(), module_path, refs);
        }
        Value::ExprSelectWith(expr_select) => {
            collect_value_refs_in_expr(expr_select.expr.as_ref(), module_path, refs);
            collect_value_refs_in_expr(expr_select.field.as_ref(), module_path, refs);
        }
        Value::ExprKeyValueMap(map) => {
            for entry in &map.entries {
                collect_value_refs_in_expr(entry.key.as_ref(), module_path, refs);
                collect_value_refs_in_expr(entry.value.as_ref(), module_path, refs);
            }
        }
        Value::ExprCall(call) => {
            collect_value_refs_in_expr(call.func.as_ref(), module_path, refs);
            for arg in &call.args {
                collect_value_refs_in_expr(arg, module_path, refs);
            }
        }
        Value::ExprAwaitCall(call) => {
            collect_value_refs_in_expr(call.func.as_ref(), module_path, refs);
            for arg in &call.args {
                collect_value_refs_in_expr(arg, module_path, refs);
            }
        }
        Value::ExprMatchTuple(expr_match) => {
            for item in &expr_match.values {
                collect_value_refs_in_expr(item, module_path, refs);
            }
            for case in &expr_match.cases {
                collect_value_refs_in_match_case(case, module_path, refs);
            }
        }
        Value::ExprSwitch(expr_switch) => {
            collect_value_refs_in_expr(expr_switch.expr.as_ref(), module_path, refs);
            for case in &expr_switch.cases {
                collect_value_refs_in_match_case(case, module_path, refs);
            }
            if let Some(default) = &expr_switch.default {
                collect_value_refs_in_expr(default.as_ref(), module_path, refs);
            }
        }
        Value::ExprInfixCall(call) => {
            collect_value_refs_in_expr(call.lhs.as_ref(), module_path, refs);
            collect_value_refs_in_expr(call.rhs.as_ref(), module_path, refs);
        }
        Value::ExprSizeOf(expr) => {
            collect_value_refs_in_expr(expr.ty.as_ref(), module_path, refs);
        }
        Value::Bool(_)
        | Value::Int(_)
        | Value::Uint(_)
        | Value::Float(_)
        | Value::String(_)
        | Value::Char(_)
        | Value::Byte(_)
        | Value::Null(_)
        | Value::None(_)
        | Value::Unit(_)
        | Value::Void(_)
        | Value::Bytes(_)
        | Value::Bits(_)
        | Value::Query(_)
        | Value::ExprRepr(_)
        | Value::ExprAny(_)
        | Value::BooleanPattern(_)
        | Value::NumericPattern(_)
        | Value::QueryStmt(_)
        | Value::SchemaStmt(_)
        | Value::MacroRules(_)
        | Value::MacroCall(_) => {}
    }
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

fn materialize_item(item: &mut Item, options: &MaterializeTypesOptions) -> Result<()> {
    match item.kind_mut() {
        ItemKind::Module(module) => materialize_items(&mut module.items, options),
        ItemKind::Impl(impl_) => materialize_items(&mut impl_.items, options),
        ItemKind::DefTrait(def) => materialize_items(&mut def.items, options),
        ItemKind::DefFunction(func) => {
            // Materialize items inside function bodies (nested items).
            if let fp_core::ast::ExprKind::Block(block) = func.body.kind_mut() {
                for stmt in &mut block.stmts {
                    if let fp_core::ast::BlockStmt::Item(item) = stmt {
                        materialize_item(item.as_mut(), options)?;
                    }
                }
            }
            Ok(())
        }
        _ => Ok(()),
    }
}

fn materialize_items(items: &mut Vec<Item>, options: &MaterializeTypesOptions) -> Result<()> {
    // Process nested modules first.
    for item in items.iter_mut() {
        materialize_item(item, options)?;
    }

    // Build a best-effort environment of known structural shapes.
    let mut shapes: HashMap<String, Vec<StructuralField>> = HashMap::new();

    let mut seed_shapes = |item: &Item| match item.kind() {
        ItemKind::DefStruct(def) => {
            shapes.insert(def.name.as_str().to_string(), def.value.fields.clone());
        }
        ItemKind::DefStructural(def) => {
            shapes.insert(def.name.as_str().to_string(), def.value.fields.clone());
        }
        ItemKind::DefType(def) => {
            if let Ty::Structural(st) = &def.value {
                shapes.insert(def.name.as_str().to_string(), st.fields.clone());
            }
        }
        _ => {}
    };
    for item in items.iter() {
        seed_shapes(item);
    }

    // Fixed point: materialize aliases once their operands become known.
    loop {
        let mut changed = false;
        for item in items.iter_mut() {
            let ItemKind::DefType(def) = item.kind_mut() else {
                continue;
            };

            if matches!(def.value, Ty::Structural(_)) {
                // Direct structural aliases are handled below.
            }

            if let Ty::TypeBinaryOp(op) = &def.value {
                if options.include_unions && matches!(op.kind, TypeBinaryOpKind::Union) {
                    if let (Some(lhs), Some(rhs)) = (
                        union_variant_from_ty(&op.lhs),
                        union_variant_from_ty(&op.rhs),
                    ) {
                        let enum_def = ItemDefEnum {
                            visibility: def.visibility.clone(),
                            name: def.name.clone(),
                            value: TypeEnum {
                                name: def.name.clone(),
                                generics_params: Vec::new(),
                                variants: vec![lhs, rhs],
                            },
                        };
                        *item = Item::new(ItemKind::DefEnum(enum_def));
                        changed = true;
                        continue;
                    }
                }
            }

            if let Some(fields) =
                resolve_structural_fields(&def.value, &shapes, &mut HashSet::new())
            {
                let def_structural = ItemDefStructural {
                    visibility: def.visibility.clone(),
                    name: def.name.clone(),
                    value: TypeStructural {
                        fields: fields.clone(),
                    },
                };
                let key = def_structural.name.as_str().to_string();
                *item = Item::new(ItemKind::DefStructural(def_structural));
                shapes.insert(key, fields);
                changed = true;
            }
        }

        if !changed {
            break;
        }
    }

    Ok(())
}

fn union_variant_from_ty(ty: &Ty) -> Option<fp_core::ast::EnumTypeVariant> {
    let (ident, payload) = match ty {
        Ty::Struct(struct_ty) => (Some(struct_ty.name.clone()), Some(Ty::expr(Expr::ident(struct_ty.name.clone())))),
        Ty::Expr(expr) => match expr.kind() {
            ExprKind::Locator(locator) => match locator {
                Locator::Path(path) => {
                    let ident = path.segments.last().cloned();
                    (ident.clone(), ident.map(|name| Ty::expr(Expr::ident(name))))
                }
                Locator::Ident(ident) => (Some(ident.clone()), Some(Ty::expr(Expr::ident(ident.clone())))),
                Locator::ParameterPath(path) => {
                    let ident = path.segments.last().map(|seg| seg.ident.clone());
                    (ident.clone(), ident.map(|name| Ty::expr(Expr::ident(name))))
                }
            },
            _ => (None, None),
        },
        Ty::Value(value) => match value.value.as_ref() {
            Value::None(ValueNone) => (Some(Ident::new("None")), Some(Ty::unit())),
            _ => (None, None),
        },
        _ => (None, None),
    };

    let ident = ident?;
    let payload = payload?;

    Some(fp_core::ast::EnumTypeVariant {
        name: ident,
        value: payload,
        discriminant: None,
    })
}

fn resolve_structural_fields(
    ty: &Ty,
    shapes: &HashMap<String, Vec<StructuralField>>,
    visiting: &mut HashSet<String>,
) -> Option<Vec<StructuralField>> {
    match ty {
        Ty::Struct(s) => Some(s.fields.clone()),
        Ty::Structural(s) => Some(s.fields.clone()),
        Ty::TypeBinaryOp(op) => {
            let lhs = resolve_structural_fields(op.lhs.as_ref(), shapes, visiting)?;
            let rhs = resolve_structural_fields(op.rhs.as_ref(), shapes, visiting)?;
            match op.kind {
                TypeBinaryOpKind::Add => Some(merge_structural_fields(lhs, rhs)),
                TypeBinaryOpKind::Intersect => Some(intersect_structural_fields(lhs, rhs)),
                TypeBinaryOpKind::Subtract => Some(subtract_structural_fields(lhs, rhs)),
                TypeBinaryOpKind::Union => None,
            }
        }
        Ty::Expr(expr) => match expr.kind() {
            ExprKind::Locator(locator) => {
                let key = locator_key(locator);
                if let Some(key) = key {
                    if visiting.contains(&key) {
                        return None;
                    }
                    if let Some(shape) = shapes.get(&key) {
                        return Some(shape.clone());
                    }
                    visiting.insert(key);
                }
                None
            }
            _ => None,
        },
        _ => None,
    }
}

fn locator_key(locator: &Locator) -> Option<String> {
    match locator {
        Locator::Ident(ident) => Some(ident.as_str().to_string()),
        Locator::Path(Path { segments }) => segments.last().map(|ident| ident.as_str().to_string()),
        Locator::ParameterPath(path) => path
            .segments
            .last()
            .map(|seg| seg.ident.as_str().to_string()),
    }
}

fn merge_structural_fields(
    mut lhs: Vec<StructuralField>,
    rhs: Vec<StructuralField>,
) -> Vec<StructuralField> {
    for rhs_field in rhs {
        if lhs
            .iter()
            .any(|field| field.name.as_str() == rhs_field.name.as_str())
        {
            continue;
        }
        lhs.push(rhs_field);
    }
    lhs
}

fn intersect_structural_fields(
    lhs: Vec<StructuralField>,
    rhs: Vec<StructuralField>,
) -> Vec<StructuralField> {
    let rhs_names: HashSet<&str> = rhs.iter().map(|f| f.name.as_str()).collect();
    lhs.into_iter()
        .filter(|f| rhs_names.contains(f.name.as_str()))
        .collect()
}

fn subtract_structural_fields(
    lhs: Vec<StructuralField>,
    rhs: Vec<StructuralField>,
) -> Vec<StructuralField> {
    let rhs_names: HashSet<&str> = rhs.iter().map(|f| f.name.as_str()).collect();
    lhs.into_iter()
        .filter(|f| !rhs_names.contains(f.name.as_str()))
        .collect()
}
