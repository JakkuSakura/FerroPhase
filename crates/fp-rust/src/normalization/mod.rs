use fp_core::ast::{
    self, BlockStmt, BlockStmtExpr, Expr, ExprField, ExprIntrinsicContainer, ExprInvokeTarget,
    ExprKind, ExprMacro, ExprMatchCase, FormatKwArg, Item, ItemKind, Node, NodeKind, Ty, Value,
    ValueFunction,
};
use fp_core::ast::{Ident, Locator, Path};
use fp_core::diagnostics::{Diagnostic, DiagnosticManager};
use std::sync::OnceLock;

mod macro_lowering;

type Diagnostics<'a> = Option<&'a DiagnosticManager>;

const NORMALIZATION_CONTEXT: &str = "normalization";
const ANY_ERROR_CODE_PREFIX: &str = "normalization::any";

const CANONICAL_STD_REWRITES: &[(&str, &[&str])] = &[
    ("println", &["std", "io", "println"]),
    ("print", &["std", "io", "print"]),
];

fn report_unhandled_any(kind: &str, diagnostics: Diagnostics<'_>) {
    if let Some(manager) = diagnostics {
        let message = format!(
            "Normalization cannot process placeholder `{}` nodes left as `Any`. This usually means the frontend failed to lower a construct.",
            kind
        );
        if lossy_normalization_mode() {
            manager.add_diagnostic(
                Diagnostic::warning(message)
                    .with_source_context(NORMALIZATION_CONTEXT)
                    .with_code(format!("{}.{kind}", ANY_ERROR_CODE_PREFIX)),
            );
        } else {
            let diagnostic = Diagnostic::error(message)
                .with_source_context(NORMALIZATION_CONTEXT)
                .with_code(format!("{}.{kind}", ANY_ERROR_CODE_PREFIX));
            manager.error(diagnostic);
        }
    }
}

fn lossy_normalization_mode() -> bool {
    static LOSSY: OnceLock<bool> = OnceLock::new();
    *LOSSY.get_or_init(|| {
        let env_true = |key: &str| {
            std::env::var(key).map(|val| {
                let trimmed = val.trim();
                !trimmed.is_empty() && !matches!(trimmed, "0" | "false" | "FALSE" | "False")
            })
        };

        if env_true("FERROPHASE_BOOTSTRAP").unwrap_or(false) {
            return true;
        }

        env_true("FERROPHASE_LOSSY").unwrap_or(false)
    })
}

pub fn normalize_last_to_ast(node: &mut Node, diagnostics: Diagnostics<'_>) {
    match node.kind_mut() {
        NodeKind::Expr(expr) => normalize_expr(expr, diagnostics),
        NodeKind::Item(item) => normalize_item(item, diagnostics),
        NodeKind::File(file) => {
            for item in &mut file.items {
                normalize_item(item, diagnostics);
            }
        }
        NodeKind::Schema(_) => {}
        NodeKind::Query(_) => {}
        NodeKind::Workspace(_) => {}
    }
}

pub fn lower_macro_for_ast(
    macro_expr: &ExprMacro,
    diagnostics: Option<&DiagnosticManager>,
) -> Expr {
    macro_lowering::lower_macro_expression(macro_expr, diagnostics)
}

fn normalize_item(item: &mut Item, diagnostics: Diagnostics<'_>) {
    match item.kind_mut() {
        ItemKind::Module(module) => {
            for item in &mut module.items {
                normalize_item(item, diagnostics);
            }
        }
        ItemKind::DefFunction(function) => normalize_bexpr(&mut function.body, diagnostics),
        ItemKind::DefConst(const_item) => normalize_bexpr(&mut const_item.value, diagnostics),
        ItemKind::DefStatic(static_item) => normalize_bexpr(&mut static_item.value, diagnostics),
        ItemKind::Impl(impl_item) => {
            if let Some(locator) = &mut impl_item.trait_ty {
                normalize_locator(locator);
            }
            normalize_expr(&mut impl_item.self_ty, diagnostics);
            for item in &mut impl_item.items {
                normalize_item(item, diagnostics);
            }
        }
        ItemKind::Expr(expr) => normalize_expr(expr, diagnostics),
        ItemKind::Any(_) => report_unhandled_any("item", diagnostics),
        _ => {}
    }
}

fn normalize_bexpr(expr: &mut ast::BExpr, diagnostics: Diagnostics<'_>) {
    normalize_expr(expr.as_mut(), diagnostics);
}

fn normalize_expr(expr: &mut Expr, diagnostics: Diagnostics<'_>) {
    match expr.kind_mut() {
        ExprKind::Locator(locator) => normalize_locator(locator),
        ExprKind::Value(value) => normalize_value(value.as_mut(), diagnostics),
        ExprKind::Block(block) => normalize_block(block, diagnostics),
        ExprKind::Invoke(invoke) => {
            normalize_invoke_target(&mut invoke.target, diagnostics);
            for arg in &mut invoke.args {
                normalize_expr(arg, diagnostics);
            }
            if let Some(collection) = ExprIntrinsicContainer::from_invoke(invoke) {
                apply_intrinsic_container(expr, collection, diagnostics);
            }
        }
        ExprKind::Match(expr_match) => {
            for ExprMatchCase { cond, body } in &mut expr_match.cases {
                normalize_bexpr(cond, diagnostics);
                normalize_bexpr(body, diagnostics);
            }
        }
        ExprKind::If(expr_if) => {
            normalize_bexpr(&mut expr_if.cond, diagnostics);
            normalize_bexpr(&mut expr_if.then, diagnostics);
            if let Some(elze) = &mut expr_if.elze {
                normalize_bexpr(elze, diagnostics);
            }
        }
        ExprKind::Loop(expr_loop) => normalize_bexpr(&mut expr_loop.body, diagnostics),
        ExprKind::While(expr_while) => {
            normalize_bexpr(&mut expr_while.cond, diagnostics);
            normalize_bexpr(&mut expr_while.body, diagnostics);
        }
        ExprKind::BinOp(bin_op) => {
            normalize_bexpr(&mut bin_op.lhs, diagnostics);
            normalize_bexpr(&mut bin_op.rhs, diagnostics);
        }
        ExprKind::UnOp(un_op) => normalize_bexpr(&mut un_op.val, diagnostics),
        ExprKind::Assign(assign) => {
            normalize_bexpr(&mut assign.target, diagnostics);
            normalize_bexpr(&mut assign.value, diagnostics);
        }
        ExprKind::Select(select) => {
            normalize_bexpr(&mut select.obj, diagnostics);
        }
        ExprKind::Index(index) => {
            normalize_bexpr(&mut index.obj, diagnostics);
            normalize_bexpr(&mut index.index, diagnostics);
        }
        ExprKind::Reference(reference) => normalize_bexpr(&mut reference.referee, diagnostics),
        ExprKind::Dereference(deref) => normalize_bexpr(&mut deref.referee, diagnostics),
        ExprKind::Await(await_expr) => normalize_bexpr(&mut await_expr.base, diagnostics),
        ExprKind::Cast(cast) => {
            normalize_expr(cast.expr.as_mut(), diagnostics);
            normalize_type(&mut cast.ty);
        }
        ExprKind::Struct(struct_expr) => {
            normalize_bexpr(&mut struct_expr.name, diagnostics);
            for ExprField { value, .. } in &mut struct_expr.fields {
                if let Some(expr) = value {
                    normalize_expr(expr, diagnostics);
                }
            }
        }
        ExprKind::Structural(structural) => {
            for ExprField { value, .. } in &mut structural.fields {
                if let Some(expr) = value {
                    normalize_expr(expr, diagnostics);
                }
            }
        }
        ExprKind::Tuple(tuple) => {
            for value in &mut tuple.values {
                normalize_expr(value, diagnostics);
            }
        }
        ExprKind::Try(expr_try) => normalize_bexpr(&mut expr_try.expr, diagnostics),
        ExprKind::Let(expr_let) => normalize_bexpr(&mut expr_let.expr, diagnostics),
        ExprKind::Closure(closure) => normalize_bexpr(&mut closure.body, diagnostics),
        ExprKind::Array(array) => {
            for value in &mut array.values {
                normalize_expr(value, diagnostics);
            }
        }
        ExprKind::ArrayRepeat(array_repeat) => {
            normalize_bexpr(&mut array_repeat.elem, diagnostics);
            normalize_bexpr(&mut array_repeat.len, diagnostics);
        }
        ExprKind::Paren(paren) => normalize_bexpr(&mut paren.expr, diagnostics),
        ExprKind::Range(range) => {
            if let Some(start) = &mut range.start {
                normalize_bexpr(start, diagnostics);
            }
            if let Some(end) = &mut range.end {
                normalize_bexpr(end, diagnostics);
            }
            if let Some(step) = &mut range.step {
                normalize_bexpr(step, diagnostics);
            }
        }
        ExprKind::FormatString(format) => {
            for arg in &mut format.args {
                normalize_expr(arg, diagnostics);
            }
            for FormatKwArg { value, .. } in &mut format.kwargs {
                normalize_expr(value, diagnostics);
            }
        }
        ExprKind::Quote(q) => {
            normalize_block(&mut q.block, diagnostics);
        }
        ExprKind::Splice(s) => {
            normalize_bexpr(&mut s.token, diagnostics);
        }
        ExprKind::Splat(splat) => normalize_expr(splat.iter.as_mut(), diagnostics),
        ExprKind::SplatDict(dict) => normalize_expr(dict.dict.as_mut(), diagnostics),
        ExprKind::Macro(_) => {
            let macro_expr = match expr.kind().clone() {
                ExprKind::Macro(mac) => mac,
                _ => unreachable!(),
            };
            let lowered = macro_lowering::lower_macro_expression(&macro_expr, diagnostics);
            *expr = lowered;
            normalize_expr(expr, diagnostics);
        }
        ExprKind::Closured(closured) => normalize_bexpr(&mut closured.expr, diagnostics),
        ExprKind::Item(item) => normalize_item(item.as_mut(), diagnostics),
        ExprKind::IntrinsicContainer(collection) => {
            let owned = collection.clone();
            apply_intrinsic_container(expr, owned, diagnostics);
        }
        ExprKind::IntrinsicCall(call) => match &mut call.payload {
            fp_core::intrinsics::IntrinsicCallPayload::Args { args } => {
                for arg in args {
                    normalize_expr(arg, diagnostics);
                }
            }
            fp_core::intrinsics::IntrinsicCallPayload::Format { template } => {
                for arg in &mut template.args {
                    normalize_expr(arg, diagnostics);
                }
                for kwarg in &mut template.kwargs {
                    normalize_expr(&mut kwarg.value, diagnostics);
                }
            }
        },
        ExprKind::Id(_) => {}
        ExprKind::Any(_) => report_unhandled_any("expression", diagnostics),
    }
}

fn apply_intrinsic_container(
    expr: &mut Expr,
    mut collection: ExprIntrinsicContainer,
    diagnostics: Diagnostics<'_>,
) {
    collection.for_each_expr_mut(|inner| normalize_expr(inner, diagnostics));
    *expr = collection.into_const_expr();
}

fn normalize_block(block: &mut ast::ExprBlock, diagnostics: Diagnostics<'_>) {
    for stmt in &mut block.stmts {
        match stmt {
            BlockStmt::Expr(BlockStmtExpr { expr, .. }) => normalize_bexpr(expr, diagnostics),
            BlockStmt::Let(stmt_let) => {
                if let Some(init) = &mut stmt_let.init {
                    normalize_expr(init, diagnostics);
                }
                if let Some(diverge) = &mut stmt_let.diverge {
                    normalize_expr(diverge, diagnostics);
                }
            }
            BlockStmt::Item(item) => normalize_item(item.as_mut(), diagnostics),
            BlockStmt::Noop => {}
            BlockStmt::Any(_) => report_unhandled_any("block statement", diagnostics),
        }
    }
}

fn normalize_invoke_target(target: &mut ExprInvokeTarget, diagnostics: Diagnostics<'_>) {
    match target {
        ExprInvokeTarget::Function(locator) => normalize_locator(locator),
        ExprInvokeTarget::Type(ty) => normalize_type(ty),
        ExprInvokeTarget::Method(select) => {
            normalize_bexpr(&mut select.obj, diagnostics);
        }
        ExprInvokeTarget::Closure(func) => normalize_value_function(func, diagnostics),
        ExprInvokeTarget::BinOp(_) => {}
        ExprInvokeTarget::Expr(expr) => normalize_expr(expr.as_mut(), diagnostics),
    }
}

fn normalize_value(value: &mut Value, diagnostics: Diagnostics<'_>) {
    match value {
        Value::Expr(expr) => normalize_expr(expr.as_mut(), diagnostics),
        Value::Function(function) => normalize_value_function(function, diagnostics),
        Value::Struct(struct_value) => {
            for field in &mut struct_value.structural.fields {
                normalize_value(&mut field.value, diagnostics);
            }
        }
        Value::Tuple(tuple) => {
            for value in &mut tuple.values {
                normalize_value(value, diagnostics);
            }
        }
        Value::Some(value_some) => normalize_value(&mut value_some.value, diagnostics),
        Value::Option(value_option) => {
            if let Some(value) = &mut value_option.value {
                normalize_value(value, diagnostics);
            }
        }
        Value::Any(_) => report_unhandled_any("value", diagnostics),
        Value::Int(_)
        | Value::Bool(_)
        | Value::Decimal(_)
        | Value::Char(_)
        | Value::String(_)
        | Value::List(_)
        | Value::Map(_)
        | Value::Bytes(_)
        | Value::Pointer(_)
        | Value::Offset(_)
        | Value::Unit(_)
        | Value::Null(_)
        | Value::None(_)
        | Value::Undefined(_)
        | Value::Escaped(_)
        | Value::Type(_)
        | Value::Structural(_)
        | Value::BinOpKind(_)
        | Value::UnOpKind(_) => {}
    }
}

fn normalize_value_function(function: &mut ValueFunction, diagnostics: Diagnostics<'_>) {
    normalize_bexpr(&mut function.body, diagnostics);
}

fn normalize_type(_ty: &mut Ty) {
    // Placeholder for future type-level canonicalisation specific to standard library rewrites.
}

fn normalize_locator(locator: &mut Locator) {
    match locator {
        Locator::Ident(ident) => {
            if let Some(segments) = canonical_segments(ident.as_str()) {
                *locator = Locator::Path(new_canonical_path(segments));
            }
        }
        Locator::Path(path) => {
            if path.segments.len() == 1 {
                if let Some(first) = path.segments.first() {
                    if let Some(segments) = canonical_segments(first.as_str()) {
                        *path = new_canonical_path(segments);
                    }
                }
            }
        }
        Locator::ParameterPath(param_path) => {
            if let Some(first) = param_path.segments.first() {
                if let Some(segments) = canonical_segments(first.ident.as_str()) {
                    *locator = Locator::Path(new_canonical_path(segments));
                }
            }
        }
    }
}

fn canonical_segments(name: &str) -> Option<&'static [&'static str]> {
    CANONICAL_STD_REWRITES
        .iter()
        .find_map(|(alias, segments)| (*alias == name).then_some(*segments))
}

fn new_canonical_path(segments: &[&'static str]) -> Path {
    let idents = segments
        .iter()
        .cloned()
        .map(|segment| Ident::new(segment.to_string()))
        .collect::<Vec<_>>();
    Path::new(idents)
}
