use fp_core::ast::{
    self, BlockStmt, BlockStmtExpr, Expr, ExprField, ExprInvokeTarget, ExprKind, ExprMatchCase,
    FormatKwArg, Item, ItemKind, Node, NodeKind, Ty, Value, ValueFunction,
};
use fp_core::id::{Ident, Locator, Path};

const CANONICAL_STD_REWRITES: &[(&str, &[&str])] = &[
    ("println", &["std", "io", "println"]),
    ("print", &["std", "io", "print"]),
];

pub fn normalize_last_to_ast(node: &mut Node) {
    match node.kind_mut() {
        NodeKind::Expr(expr) => normalize_expr(expr),
        NodeKind::Item(item) => normalize_item(item),
        NodeKind::File(file) => {
            for item in &mut file.items {
                normalize_item(item);
            }
        }
    }
}

fn normalize_item(item: &mut Item) {
    match item.kind_mut() {
        ItemKind::Module(module) => {
            for item in &mut module.items {
                normalize_item(item);
            }
        }
        ItemKind::DefFunction(function) => normalize_bexpr(&mut function.body),
        ItemKind::DefConst(const_item) => normalize_bexpr(&mut const_item.value),
        ItemKind::DefStatic(static_item) => normalize_bexpr(&mut static_item.value),
        ItemKind::Impl(impl_item) => {
            if let Some(locator) = &mut impl_item.trait_ty {
                normalize_locator(locator);
            }
            normalize_expr(&mut impl_item.self_ty);
            for item in &mut impl_item.items {
                normalize_item(item);
            }
        }
        ItemKind::Expr(expr) => normalize_expr(expr),
        _ => {}
    }
}

fn normalize_bexpr(expr: &mut ast::BExpr) {
    normalize_expr(expr.as_mut());
}

fn normalize_expr(expr: &mut Expr) {
    match expr.kind_mut() {
        ExprKind::Locator(locator) => normalize_locator(locator),
        ExprKind::Value(value) => normalize_value(value.as_mut()),
        ExprKind::Block(block) => normalize_block(block),
        ExprKind::Invoke(invoke) => {
            normalize_invoke_target(&mut invoke.target);
            for arg in &mut invoke.args {
                normalize_expr(arg);
            }
        }
        ExprKind::Match(expr_match) => {
            for ExprMatchCase { cond, body } in &mut expr_match.cases {
                normalize_bexpr(cond);
                normalize_bexpr(body);
            }
        }
        ExprKind::If(expr_if) => {
            normalize_bexpr(&mut expr_if.cond);
            normalize_bexpr(&mut expr_if.then);
            if let Some(elze) = &mut expr_if.elze {
                normalize_bexpr(elze);
            }
        }
        ExprKind::Loop(expr_loop) => normalize_bexpr(&mut expr_loop.body),
        ExprKind::While(expr_while) => {
            normalize_bexpr(&mut expr_while.cond);
            normalize_bexpr(&mut expr_while.body);
        }
        ExprKind::BinOp(bin_op) => {
            normalize_bexpr(&mut bin_op.lhs);
            normalize_bexpr(&mut bin_op.rhs);
        }
        ExprKind::UnOp(un_op) => normalize_bexpr(&mut un_op.val),
        ExprKind::Assign(assign) => {
            normalize_bexpr(&mut assign.target);
            normalize_bexpr(&mut assign.value);
        }
        ExprKind::Select(select) => {
            normalize_bexpr(&mut select.obj);
        }
        ExprKind::Index(index) => {
            normalize_bexpr(&mut index.obj);
            normalize_bexpr(&mut index.index);
        }
        ExprKind::Reference(reference) => normalize_bexpr(&mut reference.referee),
        ExprKind::Dereference(deref) => normalize_bexpr(&mut deref.referee),
        ExprKind::Struct(struct_expr) => {
            normalize_bexpr(&mut struct_expr.name);
            for ExprField { value, .. } in &mut struct_expr.fields {
                if let Some(expr) = value {
                    normalize_expr(expr);
                }
            }
        }
        ExprKind::Structural(structural) => {
            for ExprField { value, .. } in &mut structural.fields {
                if let Some(expr) = value {
                    normalize_expr(expr);
                }
            }
        }
        ExprKind::Tuple(tuple) => {
            for value in &mut tuple.values {
                normalize_expr(value);
            }
        }
        ExprKind::Try(expr_try) => normalize_bexpr(&mut expr_try.expr),
        ExprKind::Let(expr_let) => normalize_bexpr(&mut expr_let.expr),
        ExprKind::Closure(closure) => normalize_bexpr(&mut closure.body),
        ExprKind::Array(array) => {
            for value in &mut array.values {
                normalize_expr(value);
            }
        }
        ExprKind::ArrayRepeat(array_repeat) => {
            normalize_bexpr(&mut array_repeat.elem);
            normalize_bexpr(&mut array_repeat.len);
        }
        ExprKind::Paren(paren) => normalize_bexpr(&mut paren.expr),
        ExprKind::Range(range) => {
            if let Some(start) = &mut range.start {
                normalize_bexpr(start);
            }
            if let Some(end) = &mut range.end {
                normalize_bexpr(end);
            }
            if let Some(step) = &mut range.step {
                normalize_bexpr(step);
            }
        }
        ExprKind::FormatString(format) => {
            for arg in &mut format.args {
                normalize_expr(arg);
            }
            for FormatKwArg { value, .. } in &mut format.kwargs {
                normalize_expr(value);
            }
        }
        ExprKind::Splat(splat) => normalize_expr(splat.iter.as_mut()),
        ExprKind::SplatDict(dict) => normalize_expr(dict.dict.as_mut()),
        ExprKind::Closured(closured) => normalize_bexpr(&mut closured.expr),
        ExprKind::Item(item) => normalize_item(item.as_mut()),
        ExprKind::IntrinsicCall(_) | ExprKind::Id(_) | ExprKind::Any(_) => {}
    }
}

fn normalize_block(block: &mut ast::ExprBlock) {
    for stmt in &mut block.stmts {
        match stmt {
            BlockStmt::Expr(BlockStmtExpr { expr, .. }) => normalize_bexpr(expr),
            BlockStmt::Let(stmt_let) => {
                if let Some(init) = &mut stmt_let.init {
                    normalize_expr(init);
                }
                if let Some(diverge) = &mut stmt_let.diverge {
                    normalize_expr(diverge);
                }
            }
            BlockStmt::Item(item) => normalize_item(item),
            BlockStmt::Noop | BlockStmt::Any(_) => {}
        }
    }
}

fn normalize_invoke_target(target: &mut ExprInvokeTarget) {
    match target {
        ExprInvokeTarget::Function(locator) => normalize_locator(locator),
        ExprInvokeTarget::Type(ty) => normalize_type(ty),
        ExprInvokeTarget::Method(select) => {
            normalize_bexpr(&mut select.obj);
        }
        ExprInvokeTarget::Closure(func) => normalize_value_function(func),
        ExprInvokeTarget::BinOp(_) => {}
        ExprInvokeTarget::Expr(expr) => normalize_expr(expr.as_mut()),
    }
}

fn normalize_value(value: &mut Value) {
    match value {
        Value::Expr(expr) => normalize_expr(expr.as_mut()),
        Value::Function(function) => normalize_value_function(function),
        Value::Struct(struct_value) => {
            for field in &mut struct_value.structural.fields {
                normalize_value(&mut field.value);
            }
        }
        Value::Tuple(tuple) => {
            for value in &mut tuple.values {
                normalize_value(value);
            }
        }
        Value::Some(value_some) => normalize_value(&mut value_some.value),
        Value::Option(value_option) => {
            if let Some(value) = &mut value_option.value {
                normalize_value(value);
            }
        }
        Value::Any(_)
        | Value::Int(_)
        | Value::Bool(_)
        | Value::Decimal(_)
        | Value::Char(_)
        | Value::String(_)
        | Value::List(_)
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

fn normalize_value_function(function: &mut ValueFunction) {
    normalize_bexpr(&mut function.body);
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
