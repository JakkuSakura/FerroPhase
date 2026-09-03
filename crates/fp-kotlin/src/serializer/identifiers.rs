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
    use fp_core::ast::Name::*;
    match name {
        Ident(id) => id.name.clone(),
        Path(p) => p
            .segments
            .iter()
            .map(|s| s.name.as_str())
            .collect::<Vec<_>>()
            .join("."),
        ParameterPath(pp) => {
            let base = pp
                .segments
                .iter()
                .map(|s| {
                    let name = s.ident.name.as_str();
                    if s.args.is_empty() {
                        name.to_string()
                    } else {
                        let args = s
                            .args
                            .iter()
                            .map(|ty| KotlinEmitter::new().kotlin_type_from_ty(ty))
                            .collect::<Vec<_>>()
                            .join(", ");
                        format!("{}<{}>", name, args)
                    }
                })
                .collect::<Vec<_>>()
                .join(".");
            base
        }
    }
}
