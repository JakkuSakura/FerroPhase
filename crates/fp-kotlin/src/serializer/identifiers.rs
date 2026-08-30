use super::*;

pub(super) fn expr_to_name(expr: &Expr) -> String {
    match expr.kind() {
        ExprKind::Name(name) => name_to_string(name),
        ExprKind::Select(sel) => {
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

/// Splits `s` on `sep`, ignoring any `sep` nested inside `<...>`/`(...)` —
/// e.g. `split_top_level("String, Vec<Int>", ',')` → `["String", " Vec<Int>"]`,
/// not a bogus 3-way split on the inner comma.
pub(super) fn split_top_level(s: &str, sep: char) -> Vec<&str> {
    let mut parts = Vec::new();
    let mut depth = 0i32;
    let mut start = 0;
    for (i, c) in s.char_indices() {
        match c {
            '<' | '(' => depth += 1,
            '>' | ')' => depth -= 1,
            c if c == sep && depth == 0 => {
                parts.push(&s[start..i]);
                start = i + c.len_utf8();
            }
            _ => {}
        }
    }
    parts.push(&s[start..]);
    parts
}

