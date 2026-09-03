use super::*;

pub(super) fn parse_macro_2_def(input: &mut &[Token], _attrs: Vec<Attribute>) -> ModalResult<Item> {
    skip_ident(input, "macro")?;
    let name = ident_like(input)?;
    if peek_symbol(*input) == Some("(") {
        skip_balanced_delimiters(input, "(", ")")?;
    }
    skip_balanced_delimiters(input, "{", "}")?;
    Ok(Item::from(ItemKind::Macro(ItemMacro {
        invocation: MacroInvocation::new(
            Path::from_ident(name.clone()),
            MacroDelimiter::Brace,
            String::new(),
        ),
        declared_name: Some(name),
    })))
}

pub(super) fn skip_ident(input: &mut &[Token], expected: &str) -> ModalResult<()> {
    match input.first() {
        Some(token) if token.kind == TokenKind::Ident && token.lexeme == expected => {
            *input = &input[1..];
            Ok(())
        }
        _ => Err(ErrMode::Backtrack(ContextError::new())),
    }
}

/// Consume a `open ... close` run starting at `input`'s current position,
/// tracking nesting depth so an inner occurrence of `open`/`close` (e.g.
/// a nested `{ }` block inside a macro 2.0 body) doesn't close the outer
/// group early.
pub(super) fn skip_balanced_delimiters(
    input: &mut &[Token],
    open: &str,
    close: &str,
) -> ModalResult<()> {
    let mut probe = *input;
    skip_symbol(&mut probe, open)?;
    let mut depth = 1usize;
    while depth > 0 {
        if probe.is_empty() {
            return Err(ErrMode::Cut(ContextError::new()));
        }
        match peek_symbol(probe) {
            Some(s) if s == open => depth += 1,
            Some(s) if s == close => depth -= 1,
            _ => {}
        }
        probe = &probe[1..];
    }
    *input = probe;
    Ok(())
}

pub(super) fn parse_item_macro(input: &mut &[Token], _attrs: Vec<Attribute>) -> ModalResult<Item> {
    let path = parse_macro_path(input)?;
    skip_symbol(input, "!")?;
    let declared_name =
        if path.segments.last().map(|segment| segment.ident.as_str()) == Some("macro_rules") {
            Some(ident_like(input)?)
        } else {
            None
        };
    let (delimiter, group_span, token_trees, text) = parse_macro_group(input)?;
    let _ = expect_symbol(input, ";");
    Ok(Item::from(ItemKind::Macro(ItemMacro {
        invocation: MacroInvocation::new(path, delimiter, text)
            .with_token_trees(token_trees)
            .with_span(group_span),
        declared_name,
    })))
}
