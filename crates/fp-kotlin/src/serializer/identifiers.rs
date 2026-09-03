use super::*;

pub(super) fn expr_to_name(expr: &Expr) -> String {
    match expr.kind() {
        ExprKind::Name(name) => name_to_string(name),
        ExprKind::FieldAccess(sel) => {
            format!("{}.{}", expr_to_name(&sel.obj), sel.field.name.as_str())
        }
        _ => format!("Any"),
    }
}

pub(super) fn name_to_string(name: &fp_core::ast::Name) -> String {
    name.path
        .segments
        .iter()
        .map(|s| s.ident.as_str())
        .collect::<Vec<_>>()
        .join(".")
}
