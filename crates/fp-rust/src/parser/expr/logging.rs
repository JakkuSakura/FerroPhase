use super::*;

impl<'a> ExprParser<'a> {
    pub(super) fn parse_logging_macro(&self, mac: &syn::Macro, level: LogLevel) -> Result<Expr> {
        if mac.tokens.is_empty() {
            return Ok(self.make_logging_literal(level, None));
        }

        if let Ok(args) = parse_expr_arguments(mac.tokens.clone()) {
            if args.is_empty() {
                return Ok(self.make_logging_literal(level, None));
            }

            if let Some((fmt_index, fmt_literal)) = args
                .iter()
                .enumerate()
                .rev()
                .find_map(|(i, expr)| extract_string_literal(expr).map(|s| (i, s)))
            {
                let trailing_args = args.iter().skip(fmt_index + 1).cloned().collect::<Vec<_>>();

                let mut parts = parse_format_template(&fmt_literal)?;
                let prefix = level.prefix();
                if !prefix.is_empty() {
                    let mut prefixed = Vec::with_capacity(parts.len() + 1);
                    prefixed.push(FormatTemplatePart::Literal(prefix.to_string()));
                    prefixed.extend(parts.into_iter());
                    parts = prefixed;
                }

                let parsed_args = trailing_args
                    .into_iter()
                    .map(|expr| self.parse_expr(expr))
                    .collect::<Result<Vec<_>>>()?;

                let template = ExprStringTemplate { parts };

                if fmt_index > 0 {
                    let macro_name = mac.path.to_token_stream().to_string();
                    let message = format!(
                        "logging macro `{}` structured fields are ignored in lossy mode",
                        macro_name
                    );
                    if self.parser.lossy_mode() {
                        self.parser
                            .record_diagnostic(DiagnosticLevel::Warning, message);
                    } else {
                        return self.parser.error(
                            format!(
                                "logging macro `{}` structured fields are not supported",
                                macro_name
                            ),
                            Expr::unit(),
                        );
                    }
                }

                let mut args_out = Vec::with_capacity(1 + parsed_args.len());
                args_out.push(Expr::new(ExprKind::FormatString(template)));
                args_out.extend(parsed_args);
                return Ok(ExprIntrinsicCall::new(
                    IntrinsicCallKind::Println,
                    args_out,
                    Vec::new(),
                )
                .into());
            }
        }

        let macro_name = mac.path.to_token_stream().to_string();
        let tokens_str = mac.tokens.to_string();
        let fallback = self.make_logging_literal(level, Some(tokens_str));
        self.warn_or_error_expr(
            format!(
                "logging macro `{}` arguments are not yet supported; emitting literal output",
                macro_name
            ),
            fallback,
        )
    }
}
