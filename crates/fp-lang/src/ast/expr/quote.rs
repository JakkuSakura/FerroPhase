use crate::syntax::SyntaxNode;
use fp_core::ast::{
    ExprKind, Ident, Pattern, PatternIdent, PatternKind, PatternQuote, PatternStructField,
    QuoteFragmentKind, QuoteItemKind, Ty, TypeQuote, TypeType,
};

use super::LowerError;

pub(super) fn quote_kind_from_cst(
    node: &SyntaxNode,
) -> Result<Option<QuoteFragmentKind>, LowerError> {
    let mut tokens = Vec::new();
    crate::syntax::collect_tokens(node, &mut tokens);
    let mut iter = tokens.iter().filter(|t| !t.is_trivia());

    while let Some(tok) = iter.next() {
        if tok.text != "quote" {
            continue;
        }
        let Some(next) = iter.next() else {
            return Ok(None);
        };
        if next.text != "<" {
            return Ok(None);
        }
        let Some(kind_tok) = iter.next() else {
            return Err(LowerError::UnexpectedNode(node.kind));
        };
        if kind_tok.text == "[" {
            let Some(inner_tok) = iter.next() else {
                return Err(LowerError::UnexpectedNode(node.kind));
            };
            let Some(close_bracket) = iter.next() else {
                return Err(LowerError::UnexpectedNode(node.kind));
            };
            if close_bracket.text != "]" {
                return Err(LowerError::UnexpectedNode(node.kind));
            }
            let Some(close_tok) = iter.next() else {
                return Err(LowerError::UnexpectedNode(node.kind));
            };
            if close_tok.text != ">" {
                return Err(LowerError::UnexpectedNode(node.kind));
            }
            let kind = match inner_tok.text.as_str() {
                "item" | "fn" | "struct" | "enum" | "trait" | "impl" | "const" | "static"
                | "mod" | "use" | "macro" => QuoteFragmentKind::Item,
                _ => return Err(LowerError::UnexpectedNode(node.kind)),
            };
            return Ok(Some(kind));
        }
        let Some(close_tok) = iter.next() else {
            return Err(LowerError::UnexpectedNode(node.kind));
        };
        if close_tok.text != ">" {
            return Err(LowerError::UnexpectedNode(node.kind));
        }
        let kind = match kind_tok.text.as_str() {
            "expr" => QuoteFragmentKind::Expr,
            "stmt" => QuoteFragmentKind::Stmt,
            "item" => QuoteFragmentKind::Item,
            "type" => QuoteFragmentKind::Type,
            "items" | "fns" | "structs" | "enums" | "traits" | "impls" | "consts" | "statics"
            | "mods" | "uses" | "macros" => {
                tracing::warn!("deprecated plural quote fragment kind: {}", kind_tok.text);
                QuoteFragmentKind::Item
            }
            "exprs" | "stmts" | "types" => {
                return Err(LowerError::UnexpectedNode(node.kind));
            }
            "fn" | "struct" | "enum" | "trait" | "impl" | "const" | "static" | "mod" | "use"
            | "macro" => QuoteFragmentKind::Item,
            _ => return Err(LowerError::UnexpectedNode(node.kind)),
        };
        return Ok(Some(kind));
    }

    Ok(None)
}

pub(super) fn quote_pattern_from_cst(node: &SyntaxNode) -> Result<PatternQuote, LowerError> {
    let mut tokens = Vec::new();
    crate::syntax::collect_tokens(node, &mut tokens);
    let mut iter = tokens.iter().filter(|t| !t.is_trivia());

    while let Some(tok) = iter.next() {
        if tok.text != "quote" {
            continue;
        }
        let Some(next) = iter.next() else {
            return Ok(PatternQuote {
                fragment: QuoteFragmentKind::Item,
                item: None,
                fields: Vec::new(),
                has_rest: false,
            });
        };
        if next.text != "<" {
            return Ok(PatternQuote {
                fragment: QuoteFragmentKind::Item,
                item: None,
                fields: Vec::new(),
                has_rest: false,
            });
        }
        let Some(kind_tok) = iter.next() else {
            return Err(LowerError::UnexpectedNode(node.kind));
        };
        if kind_tok.text == "[" {
            let Some(inner_tok) = iter.next() else {
                return Err(LowerError::UnexpectedNode(node.kind));
            };
            let Some(close_bracket) = iter.next() else {
                return Err(LowerError::UnexpectedNode(node.kind));
            };
            if close_bracket.text != "]" {
                return Err(LowerError::UnexpectedNode(node.kind));
            }
            let Some(close_tok) = iter.next() else {
                return Err(LowerError::UnexpectedNode(node.kind));
            };
            if close_tok.text != ">" {
                return Err(LowerError::UnexpectedNode(node.kind));
            }
            let (fragment, item) = match inner_tok.text.as_str() {
                "item" => (QuoteFragmentKind::Item, None),
                "fn" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Function)),
                "struct" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Struct)),
                "enum" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Enum)),
                "trait" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Trait)),
                "impl" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Impl)),
                "const" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Const)),
                "static" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Static)),
                "mod" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Module)),
                "use" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Use)),
                "macro" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Macro)),
                _ => return Err(LowerError::UnexpectedNode(node.kind)),
            };
            return Ok(PatternQuote {
                fragment,
                item,
                fields: Vec::new(),
                has_rest: false,
            });
        }
        let Some(close_tok) = iter.next() else {
            return Err(LowerError::UnexpectedNode(node.kind));
        };
        if close_tok.text != ">" {
            return Err(LowerError::UnexpectedNode(node.kind));
        }
        let (fragment, item) = match kind_tok.text.as_str() {
            "expr" => (QuoteFragmentKind::Expr, None),
            "stmt" => (QuoteFragmentKind::Stmt, None),
            "item" => (QuoteFragmentKind::Item, None),
            "type" => (QuoteFragmentKind::Type, None),
            "fn" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Function)),
            "struct" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Struct)),
            "enum" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Enum)),
            "trait" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Trait)),
            "impl" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Impl)),
            "const" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Const)),
            "static" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Static)),
            "mod" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Module)),
            "use" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Use)),
            "macro" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Macro)),
            "items" | "fns" | "structs" | "enums" | "traits" | "impls" | "consts" | "statics"
            | "mods" | "uses" | "macros" => {
                tracing::warn!("deprecated plural quote fragment kind: {}", kind_tok.text);
                (QuoteFragmentKind::Item, None)
            }
            "exprs" | "stmts" | "types" => {
                return Err(LowerError::UnexpectedNode(node.kind));
            }
            _ => return Err(LowerError::UnexpectedNode(node.kind)),
        };
        return Ok(PatternQuote {
            fragment,
            item,
            fields: Vec::new(),
            has_rest: false,
        });
    }

    Err(LowerError::UnexpectedNode(node.kind))
}

pub(super) fn quote_block_pattern_from_cst(node: &SyntaxNode) -> Result<PatternQuote, LowerError> {
    let mut tokens = Vec::new();
    crate::syntax::collect_tokens(node, &mut tokens);
    let tokens: Vec<&crate::syntax::SyntaxToken> =
        tokens.into_iter().filter(|t| !t.is_trivia()).collect();

    let mut idx = 0;
    while idx < tokens.len() && tokens[idx].text != "quote" {
        idx += 1;
    }
    if idx >= tokens.len() {
        return Err(LowerError::UnexpectedNode(node.kind));
    }
    idx += 1;

    if tokens.get(idx).is_some_and(|t| t.text == "<") {
        idx += 1;
        let (kind_tok, has_brackets) = if tokens.get(idx).is_some_and(|t| t.text == "[") {
            idx += 1;
            let kind = tokens
                .get(idx)
                .ok_or(LowerError::UnexpectedNode(node.kind))?;
            idx += 1;
            if !tokens.get(idx).is_some_and(|t| t.text == "]") {
                return Err(LowerError::UnexpectedNode(node.kind));
            }
            idx += 1;
            (kind, true)
        } else {
            let kind = tokens
                .get(idx)
                .ok_or(LowerError::UnexpectedNode(node.kind))?;
            idx += 1;
            (kind, false)
        };
        if !tokens.get(idx).is_some_and(|t| t.text == ">") {
            return Err(LowerError::UnexpectedNode(node.kind));
        }
        idx += 1;

        let kind_text = kind_tok.text.as_str();
        let _ = kind_text;
        let _ = has_brackets;
    }

    while idx < tokens.len() && tokens[idx].text != "{" {
        idx += 1;
    }
    if idx >= tokens.len() {
        return Err(LowerError::UnexpectedNode(node.kind));
    }
    idx += 1;

    let mut depth = 1;
    let mut fn_idx = None;
    let mut cursor = idx;
    while cursor < tokens.len() {
        let text = tokens[cursor].text.as_str();
        if text == "{" {
            depth += 1;
        } else if text == "}" {
            depth -= 1;
            if depth == 0 {
                break;
            }
        } else if depth == 1 && text == "fn" {
            fn_idx = Some(cursor);
            break;
        }
        cursor += 1;
    }
    let fn_idx = fn_idx.ok_or(LowerError::UnexpectedNode(node.kind))?;

    let mut fields = Vec::new();
    let mut name_idx = fn_idx + 1;
    if tokens.get(name_idx).is_some_and(|t| t.text == "splice") {
        if !tokens.get(name_idx + 1).is_some_and(|t| t.text == "(") {
            return Err(LowerError::UnexpectedNode(node.kind));
        }
        let bind_tok = tokens
            .get(name_idx + 2)
            .ok_or(LowerError::UnexpectedNode(node.kind))?;
        if !tokens.get(name_idx + 3).is_some_and(|t| t.text == ")") {
            return Err(LowerError::UnexpectedNode(node.kind));
        }
        let binder = Ident::new(bind_tok.text.clone());
        fields.push(PatternStructField {
            name: Ident::new("name"),
            rename: Some(Box::new(Pattern::new(PatternKind::Ident(
                PatternIdent::new(binder),
            )))),
        });
        name_idx += 4;
    } else {
        name_idx += 1;
    }

    if !tokens.get(name_idx).is_some_and(|t| t.text == "(") {
        return Err(LowerError::UnexpectedNode(node.kind));
    }
    let mut paren_depth = 0;
    while name_idx < tokens.len() {
        let text = tokens[name_idx].text.as_str();
        if text == "(" {
            paren_depth += 1;
        } else if text == ")" {
            paren_depth -= 1;
            if paren_depth == 0 {
                break;
            }
        }
        name_idx += 1;
    }
    if paren_depth != 0 {
        return Err(LowerError::UnexpectedNode(node.kind));
    }

    Ok(PatternQuote {
        fragment: QuoteFragmentKind::Item,
        item: Some(QuoteItemKind::Function),
        fields,
        has_rest: false,
    })
}

pub(super) fn quote_pattern_kind_from_cst(
    node: &SyntaxNode,
) -> Result<(QuoteFragmentKind, Option<QuoteItemKind>, bool), LowerError> {
    let mut tokens = Vec::new();
    crate::syntax::collect_tokens(node, &mut tokens);
    let mut iter = tokens.iter().filter(|t| !t.is_trivia());

    while let Some(tok) = iter.next() {
        if tok.text != "quote" {
            continue;
        }
        let Some(next) = iter.next() else {
            return Ok((QuoteFragmentKind::Item, None, false));
        };
        if next.text != "<" {
            return Ok((QuoteFragmentKind::Item, None, false));
        }
        let Some(kind_tok) = iter.next() else {
            return Err(LowerError::UnexpectedNode(node.kind));
        };
        if kind_tok.text == "[" {
            let Some(inner_tok) = iter.next() else {
                return Err(LowerError::UnexpectedNode(node.kind));
            };
            let Some(close_bracket) = iter.next() else {
                return Err(LowerError::UnexpectedNode(node.kind));
            };
            if close_bracket.text != "]" {
                return Err(LowerError::UnexpectedNode(node.kind));
            }
            let Some(close_tok) = iter.next() else {
                return Err(LowerError::UnexpectedNode(node.kind));
            };
            if close_tok.text != ">" {
                return Err(LowerError::UnexpectedNode(node.kind));
            }
            let (fragment, item) = match inner_tok.text.as_str() {
                "item" => (QuoteFragmentKind::Item, None),
                "fn" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Function)),
                "struct" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Struct)),
                "enum" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Enum)),
                "trait" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Trait)),
                "impl" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Impl)),
                "const" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Const)),
                "static" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Static)),
                "mod" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Module)),
                "use" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Use)),
                "macro" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Macro)),
                _ => return Err(LowerError::UnexpectedNode(node.kind)),
            };
            return Ok((fragment, item, true));
        }
        let Some(close_tok) = iter.next() else {
            return Err(LowerError::UnexpectedNode(node.kind));
        };
        if close_tok.text != ">" {
            return Err(LowerError::UnexpectedNode(node.kind));
        }
        let (fragment, item, plural) = match kind_tok.text.as_str() {
            "expr" => (QuoteFragmentKind::Expr, None, false),
            "stmt" => (QuoteFragmentKind::Stmt, None, false),
            "item" => (QuoteFragmentKind::Item, None, false),
            "type" => (QuoteFragmentKind::Type, None, false),
            "fn" => (
                QuoteFragmentKind::Item,
                Some(QuoteItemKind::Function),
                false,
            ),
            "struct" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Struct), false),
            "enum" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Enum), false),
            "trait" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Trait), false),
            "impl" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Impl), false),
            "const" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Const), false),
            "static" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Static), false),
            "mod" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Module), false),
            "use" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Use), false),
            "macro" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Macro), false),
            "items" => {
                tracing::warn!("deprecated plural quote fragment kind: {}", kind_tok.text);
                (QuoteFragmentKind::Item, None, true)
            }
            "fns" => {
                tracing::warn!("deprecated plural quote fragment kind: {}", kind_tok.text);
                (QuoteFragmentKind::Item, Some(QuoteItemKind::Function), true)
            }
            "structs" => {
                tracing::warn!("deprecated plural quote fragment kind: {}", kind_tok.text);
                (QuoteFragmentKind::Item, Some(QuoteItemKind::Struct), true)
            }
            "enums" => {
                tracing::warn!("deprecated plural quote fragment kind: {}", kind_tok.text);
                (QuoteFragmentKind::Item, Some(QuoteItemKind::Enum), true)
            }
            "traits" => {
                tracing::warn!("deprecated plural quote fragment kind: {}", kind_tok.text);
                (QuoteFragmentKind::Item, Some(QuoteItemKind::Trait), true)
            }
            "impls" => {
                tracing::warn!("deprecated plural quote fragment kind: {}", kind_tok.text);
                (QuoteFragmentKind::Item, Some(QuoteItemKind::Impl), true)
            }
            "consts" => {
                tracing::warn!("deprecated plural quote fragment kind: {}", kind_tok.text);
                (QuoteFragmentKind::Item, Some(QuoteItemKind::Const), true)
            }
            "statics" => {
                tracing::warn!("deprecated plural quote fragment kind: {}", kind_tok.text);
                (QuoteFragmentKind::Item, Some(QuoteItemKind::Static), true)
            }
            "mods" => {
                tracing::warn!("deprecated plural quote fragment kind: {}", kind_tok.text);
                (QuoteFragmentKind::Item, Some(QuoteItemKind::Module), true)
            }
            "uses" => {
                tracing::warn!("deprecated plural quote fragment kind: {}", kind_tok.text);
                (QuoteFragmentKind::Item, Some(QuoteItemKind::Use), true)
            }
            "macros" => {
                tracing::warn!("deprecated plural quote fragment kind: {}", kind_tok.text);
                (QuoteFragmentKind::Item, Some(QuoteItemKind::Macro), true)
            }
            "exprs" | "stmts" | "types" => {
                return Err(LowerError::UnexpectedNode(node.kind));
            }
            _ => return Err(LowerError::UnexpectedNode(node.kind)),
        };
        return Ok((fragment, item, plural));
    }

    Err(LowerError::UnexpectedNode(node.kind))
}

pub(super) fn quote_type_from_ident(
    name: &str,
    args: &[Ty],
    span: fp_core::span::Span,
) -> Option<Ty> {
    match name {
        "expr" => {
            if args.len() > 1 {
                return None;
            }
            let inner = args.get(0).cloned().map(Box::new);
            Some(Ty::Quote(TypeQuote {
                span,
                kind: QuoteFragmentKind::Expr,
                item: None,
                inner,
            }))
        }
        "stmt" => {
            if !args.is_empty() {
                return None;
            }
            Some(Ty::Quote(TypeQuote {
                span,
                kind: QuoteFragmentKind::Stmt,
                item: None,
                inner: None,
            }))
        }
        "item" => {
            if !args.is_empty() {
                return None;
            }
            Some(Ty::Quote(TypeQuote {
                span,
                kind: QuoteFragmentKind::Item,
                item: None,
                inner: None,
            }))
        }
        "type" => {
            if !args.is_empty() {
                return None;
            }
            Some(Ty::Type(TypeType::new(span)))
        }
        "fn" => {
            if !args.is_empty() {
                return None;
            }
            Some(Ty::Quote(TypeQuote {
                span,
                kind: QuoteFragmentKind::Item,
                item: Some(QuoteItemKind::Function),
                inner: None,
            }))
        }
        "struct" => {
            if !args.is_empty() {
                return None;
            }
            Some(Ty::Quote(TypeQuote {
                span,
                kind: QuoteFragmentKind::Item,
                item: Some(QuoteItemKind::Struct),
                inner: None,
            }))
        }
        "enum" => {
            if !args.is_empty() {
                return None;
            }
            Some(Ty::Quote(TypeQuote {
                span,
                kind: QuoteFragmentKind::Item,
                item: Some(QuoteItemKind::Enum),
                inner: None,
            }))
        }
        "trait" => {
            if !args.is_empty() {
                return None;
            }
            Some(Ty::Quote(TypeQuote {
                span,
                kind: QuoteFragmentKind::Item,
                item: Some(QuoteItemKind::Trait),
                inner: None,
            }))
        }
        "impl" => {
            if !args.is_empty() {
                return None;
            }
            Some(Ty::Quote(TypeQuote {
                span,
                kind: QuoteFragmentKind::Item,
                item: Some(QuoteItemKind::Impl),
                inner: None,
            }))
        }
        "const" => {
            if !args.is_empty() {
                return None;
            }
            Some(Ty::Quote(TypeQuote {
                span,
                kind: QuoteFragmentKind::Item,
                item: Some(QuoteItemKind::Const),
                inner: None,
            }))
        }
        "static" => {
            if !args.is_empty() {
                return None;
            }
            Some(Ty::Quote(TypeQuote {
                span,
                kind: QuoteFragmentKind::Item,
                item: Some(QuoteItemKind::Static),
                inner: None,
            }))
        }
        "mod" => {
            if !args.is_empty() {
                return None;
            }
            Some(Ty::Quote(TypeQuote {
                span,
                kind: QuoteFragmentKind::Item,
                item: Some(QuoteItemKind::Module),
                inner: None,
            }))
        }
        "use" => {
            if !args.is_empty() {
                return None;
            }
            Some(Ty::Quote(TypeQuote {
                span,
                kind: QuoteFragmentKind::Item,
                item: Some(QuoteItemKind::Use),
                inner: None,
            }))
        }
        "macro" => {
            if !args.is_empty() {
                return None;
            }
            Some(Ty::Quote(TypeQuote {
                span,
                kind: QuoteFragmentKind::Item,
                item: Some(QuoteItemKind::Macro),
                inner: None,
            }))
        }
        _ => None,
    }
}

pub(super) fn quote_type_from_type_arg(arg: &Ty) -> Option<Ty> {
    match arg {
        Ty::Quote(_) => Some(arg.clone()),
        Ty::Expr(expr) => match expr.kind() {
            ExprKind::Name(locator) => {
                let ident = locator.as_ident()?.as_str().to_string();
                if ident == "type" {
                    return Some(Ty::Quote(TypeQuote {
                        span: expr.span(),
                        kind: QuoteFragmentKind::Type,
                        item: None,
                        inner: None,
                    }));
                }
                quote_type_from_ident(&ident, &[], expr.span())
            }
            _ => None,
        },
        _ => None,
    }
}
