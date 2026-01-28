use crate::ast::{
    Expr, ExprKind, ExprStringTemplate, FormatArgRef, FormatPlaceholder, FormatTemplatePart, Value,
};

pub fn convert_print_args_to_template(args: &[Expr]) -> Option<(ExprStringTemplate, usize)> {
    if args.is_empty() {
        return Some((
            ExprStringTemplate {
                parts: vec![FormatTemplatePart::Literal(String::new())],
            },
            0,
        ));
    }

    match args[0].kind() {
        ExprKind::FormatString(template) => {
            return Some((template.clone(), 1));
        }
        ExprKind::Value(value) => {
            if let Value::String(str_val) = &**value {
                if args.len() == 1 {
                    return Some((
                        ExprStringTemplate {
                            parts: vec![FormatTemplatePart::Literal(str_val.value.clone())],
                        },
                        1,
                    ));
                }

                let template = str_val.value.clone();
                let looks_like_format_template = template.contains('{');
                if looks_like_format_template {
                    return Some((
                        ExprStringTemplate {
                            parts: vec![FormatTemplatePart::Literal(template)],
                        },
                        1,
                    ));
                }

                let mut parts = vec![FormatTemplatePart::Literal(template)];
                if !matches!(
                    parts.last(),
                    Some(FormatTemplatePart::Literal(lit)) if lit.is_empty()
                ) {
                    parts.push(FormatTemplatePart::Literal(" ".to_string()));
                }
                for (idx, _arg) in args[1..].iter().enumerate() {
                    parts.push(FormatTemplatePart::Placeholder(FormatPlaceholder {
                        arg_ref: FormatArgRef::Implicit,
                        format_spec: None,
                    }));
                    if idx + 1 < args.len() - 1 {
                        parts.push(FormatTemplatePart::Literal(" ".to_string()));
                    }
                }
                return Some((ExprStringTemplate { parts }, 1));
            }
        }
        _ => {}
    }

    let mut parts = Vec::new();
    for idx in 0..args.len() {
        parts.push(FormatTemplatePart::Placeholder(FormatPlaceholder {
            arg_ref: FormatArgRef::Implicit,
            format_spec: None,
        }));
        if idx + 1 < args.len() {
            parts.push(FormatTemplatePart::Literal(" ".to_string()));
        }
    }
    Some((ExprStringTemplate { parts }, 0))
}

#[cfg(test)]
fn extract_string_literal(expr: &Expr) -> Option<String> {
    match expr.kind() {
        ExprKind::Value(value) => match value.as_ref() {
            Value::String(string) => Some(string.value.clone()),
            _ => None,
        },
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extract_string_literal() {
        let expr = Expr::new(ExprKind::Value(Box::new(Value::string("abc".into()))));
        let out = extract_string_literal(&expr).unwrap();
        assert_eq!(out, "abc");
    }
}
