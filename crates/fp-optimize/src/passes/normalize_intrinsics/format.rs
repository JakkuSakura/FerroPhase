use fp_core::ast::{
    Expr, ExprFormatString, ExprKind, FormatArgRef, FormatPlaceholder, FormatTemplatePart, Value,
};

pub fn convert_print_args_to_format(args: &[Expr]) -> Option<ExprFormatString> {
    if args.is_empty() {
        return Some(ExprFormatString {
            parts: vec![FormatTemplatePart::Literal(String::new())],
            args: Vec::new(),
            kwargs: Vec::new(),
        });
    }

    if args.len() == 1 {
        if let Some(literal) = extract_string_literal(&args[0]) {
            return Some(ExprFormatString {
                parts: vec![FormatTemplatePart::Literal(literal)],
                args: Vec::new(),
                kwargs: Vec::new(),
            });
        }
    }

    // Multi-arg print: render as a space-separated `{}` template.
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
    Some(ExprFormatString {
        parts,
        args: args.to_vec(),
        kwargs: Vec::new(),
    })
}

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
