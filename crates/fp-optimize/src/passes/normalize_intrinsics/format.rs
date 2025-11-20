use fp_core::ast::{Expr, ExprFormatString, ExprKind, FormatTemplatePart, Value};

pub fn convert_print_args_to_format(args: &[Expr]) -> Option<ExprFormatString> {
    match args.len() {
        0 => Some(ExprFormatString {
            parts: vec![FormatTemplatePart::Literal(String::new())],
            args: Vec::new(),
            kwargs: Vec::new(),
        }),
        1 => {
            if let Some(literal) = extract_string_literal(&args[0]) {
                Some(ExprFormatString {
                    parts: vec![FormatTemplatePart::Literal(literal)],
                    args: Vec::new(),
                    kwargs: Vec::new(),
                })
            } else {
                None
            }
        }
        _ => None,
    }
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
