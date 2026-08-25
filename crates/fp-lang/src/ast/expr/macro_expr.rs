use super::*;

pub(crate) fn parse_macro_expr(input: &mut &[Token]) -> ModalResult<Expr> {
    let mut probe = *input;
    let path = parse_macro_path(&mut probe)?;
    skip_symbol(&mut probe, "!")?;
    let (delimiter, group_span, token_trees, text) = parse_macro_group(&mut probe)?;
    *input = probe;
    Ok(ExprKind::Macro(ExprMacro::new(
        MacroInvocation::new(path, delimiter, text)
            .with_token_trees(token_trees)
            .with_span(group_span),
    ))
    .into())
}

pub(super) fn parse_struct_expr(input: &mut &[Token], file: FileId) -> ModalResult<Expr> {
    let mut probe = *input;
    let is_structural = skip_keyword(&mut probe, Keyword::Struct).is_ok();
    let name = if is_structural {
        None
    } else {
        Some(parse_name(&mut probe)?)
    };
    if skip_symbol(&mut probe, "{").is_err() {
        return Err(ErrMode::Backtrack(ContextError::new()));
    }
    let (fields, update) = parse_struct_literal_fields(&mut probe, file)?;
    *input = probe;
    if is_structural {
        return Ok(ExprKind::Structural(ExprStructural {
            span: Span::null(),
            fields,
        })
        .into());
    }
    Ok(ExprKind::Struct(ExprStruct {
        span: Span::null(),
        name: Box::new(Expr::name(name.expect("named struct literal"))),
        fields,
        update,
    })
    .into())
}

pub(crate) fn parse_macro_path(input: &mut &[Token]) -> ModalResult<Path> {
    parse_module_path(input)
}

pub(crate) fn parse_macro_group(
    input: &mut &[Token],
) -> ModalResult<(MacroDelimiter, Span, Vec<MacroTokenTree>, String)> {
    let (delimiter, open, close) = match peek_symbol(input) {
        Some("(") => (MacroDelimiter::Parenthesis, "(", ")"),
        Some("[") => (MacroDelimiter::Bracket, "[", "]"),
        Some("{") => (MacroDelimiter::Brace, "{", "}"),
        _ => return Err(ErrMode::Backtrack(ContextError::new())),
    };
    let open_token = expect_symbol(input, open)?;
    let mut inner = Vec::new();
    loop {
        if peek_symbol(input) == Some(close) {
            break;
        }
        if input.is_empty() {
            return Err(ErrMode::Cut(ContextError::new()));
        }
        inner.push(parse_macro_token_tree(input)?);
    }
    let close_token = expect_symbol(input, close)?;
    let span = Span::union([
        token_span_to_span(&open_token),
        token_span_to_span(&close_token),
    ]);
    let text = macro_token_trees_to_text(&inner);
    Ok((delimiter, span, inner, text))
}

fn parse_macro_token_tree(input: &mut &[Token]) -> ModalResult<MacroTokenTree> {
    if matches!(peek_symbol(input), Some("(") | Some("[") | Some("{")) {
        let (delimiter, span, token_trees, _) = parse_macro_group(input)?;
        return Ok(MacroTokenTree::Group(MacroGroup {
            delimiter,
            tokens: token_trees,
            span,
        }));
    }
    let Some((token, rest)) = input.split_first() else {
        return Err(ErrMode::Backtrack(ContextError::new()));
    };
    *input = rest;
    Ok(MacroTokenTree::Token(MacroToken {
        text: token.lexeme.clone(),
        span: token_span_to_span(token),
    }))
}

fn macro_token_trees_to_text(tokens: &[MacroTokenTree]) -> String {
    fn is_ident_like(text: &str) -> bool {
        text.chars()
            .next()
            .is_some_and(|c| c.is_ascii_alphanumeric() || c == '_')
    }

    fn needs_space(prev: &str, next: &str) -> bool {
        is_ident_like(prev) && is_ident_like(next)
    }

    let mut out = String::new();
    let mut prev: Option<String> = None;
    for token in flatten_macro_tokens(tokens) {
        if let Some(prev_text) = prev.as_deref() {
            if needs_space(prev_text, token.as_str()) {
                out.push(' ');
            }
        }
        out.push_str(&token);
        prev = Some(token);
    }
    out
}

fn flatten_macro_tokens(tokens: &[MacroTokenTree]) -> Vec<String> {
    let mut out = Vec::new();
    for token in tokens {
        match token {
            MacroTokenTree::Token(tok) => out.push(tok.text.clone()),
            MacroTokenTree::Group(group) => {
                let (open, close) = match group.delimiter {
                    MacroDelimiter::Parenthesis => ("(", ")"),
                    MacroDelimiter::Bracket => ("[", "]"),
                    MacroDelimiter::Brace => ("{", "}"),
                };
                out.push(open.to_string());
                out.extend(flatten_macro_tokens(&group.tokens));
                out.push(close.to_string());
            }
        }
    }
    out
}
