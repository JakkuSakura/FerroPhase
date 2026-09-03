use super::*;

pub(super) fn parse_match_pattern(input: &mut &[Token]) -> ModalResult<Pattern> {
    let mut probe = *input;
    // `&&pat` — a double-reference pattern (real `core::str::count`'s own
    // `.filter(|&&byte| ..)`), same tokenizer ambiguity as the `&&expr`
    // double-reference expression already handled in prefix-expression
    // position: the lexer emits `&&` as one lexeme shared with logical-and,
    // so it must be split apart here rather than relying on two separate
    // `&` tokens.
    if skip_symbol(&mut probe, "&&").is_ok() {
        let mutability = skip_keyword(&mut probe, Keyword::Mut).is_ok();
        let inner_pattern = parse_general_pattern(&mut probe)?;
        *input = probe;
        let inner = Pattern::new(PatternKind::Ref(PatternRef {
            mutability: mutability.then_some(true),
            pattern: Box::new(inner_pattern),
        }));
        return Ok(Pattern::new(PatternKind::Ref(PatternRef {
            mutability: None,
            pattern: Box::new(inner),
        })));
    }
    if skip_symbol(&mut probe, "&").is_ok() {
        let mutability = skip_keyword(&mut probe, Keyword::Mut).is_ok();
        let pattern = parse_general_pattern(&mut probe)?;
        *input = probe;
        return Ok(Pattern::new(PatternKind::Ref(PatternRef {
            mutability: mutability.then_some(true),
            pattern: Box::new(pattern),
        })));
    }
    if skip_keyword(&mut probe, Keyword::Mut).is_ok() {
        let mut pat = parse_match_pattern(&mut probe)?;
        pat.make_mut();
        *input = probe;
        return Ok(pat);
    }
    if peek_ident_like(probe) == Some("ref") {
        let _ = ident_like(&mut probe)?;
        if starts_ref_pattern_target(probe) {
            let pattern = parse_general_pattern(&mut probe)?;
            *input = probe;
            return Ok(Pattern::new(PatternKind::Ref(PatternRef {
                mutability: None,
                pattern: Box::new(pattern),
            })));
        }
    }
    if skip_keyword(&mut probe, Keyword::Quote).is_ok() {
        let mut item = None;
        let mut fragment = QuoteFragmentKind::Item;
        if skip_symbol(&mut probe, "<").is_ok() {
            let ident = ident_like(&mut probe)?;
            item = match ident.as_str() {
                "fn" => Some(QuoteItemKind::Function),
                "struct" => Some(QuoteItemKind::Struct),
                "enum" => Some(QuoteItemKind::Enum),
                "trait" => Some(QuoteItemKind::Trait),
                "impl" => Some(QuoteItemKind::Impl),
                "const" => Some(QuoteItemKind::Const),
                "static" => Some(QuoteItemKind::Static),
                "mod" => Some(QuoteItemKind::Module),
                "use" => Some(QuoteItemKind::Use),
                "macro" => Some(QuoteItemKind::Macro),
                "item" => None,
                "expr" => {
                    fragment = QuoteFragmentKind::Expr;
                    None
                }
                "stmt" => {
                    fragment = QuoteFragmentKind::Stmt;
                    None
                }
                "type" => {
                    fragment = QuoteFragmentKind::Type;
                    None
                }
                _ => return Err(ErrMode::Cut(ContextError::new())),
            };
            skip_symbol(&mut probe, ">")?;
        }
        *input = probe;
        return Ok(Pattern::new(PatternKind::Quote(PatternQuote {
            fragment,
            item,
            fields: Vec::new(),
            has_rest: false,
        })));
    }

    // An open-start range pattern (`..END`/`..=END` — no lower bound, e.g.
    // real `core::char::methods`'s `match code { ..MAX_ONE_B => 1, .. }`)
    // — the closing-side check just below already handles the symmetric
    // `START..`/`START..=` case (no upper bound isn't handled either, but
    // that shape doesn't appear in vendored std today), just never one
    // with the start omitted instead. Checked first since `parse_literal_
    // pattern_expr` can never itself start with `..`/`..=`, so trying it
    // first would just fail and fall through anyway.
    let mut open_start_probe = *input;
    if let Some(op) = peek_symbol(open_start_probe) {
        let limit = match op {
            ".." => Some(ExprRangeLimit::Exclusive),
            "..=" => Some(ExprRangeLimit::Inclusive),
            _ => None,
        };
        if let Some(limit) = limit {
            skip_symbol(&mut open_start_probe, op)?;
            if let Ok(end) = parse_range_bound_expr(&mut open_start_probe) {
                *input = open_start_probe;
                return Ok(Pattern::new(PatternKind::Variant(PatternVariant {
                    name: ExprKind::Range(ExprRange {
                        span: end.span(),
                        start: None,
                        limit,
                        end: Some(Box::new(end)),
                        step: None,
                    })
                    .into(),
                    pattern: None,
                })));
            }
        }
    }

    let mut literal_probe = *input;
    if let Ok(expr) = parse_literal_pattern_expr(&mut literal_probe) {
        let mut range_probe = literal_probe;
        if let Some(op) = peek_symbol(range_probe) {
            let limit = match op {
                ".." => Some(ExprRangeLimit::Exclusive),
                "..=" => Some(ExprRangeLimit::Inclusive),
                _ => None,
            };
            if let Some(limit) = limit {
                skip_symbol(&mut range_probe, op)?;
                let end = parse_range_bound_expr(&mut range_probe)
                    .map_err(|_| ErrMode::Cut(ContextError::new()))?;
                let span = union_exprs(&expr, &end);
                *input = range_probe;
                return Ok(Pattern::new(PatternKind::Variant(PatternVariant {
                    name: ExprKind::Range(ExprRange {
                        span,
                        start: Some(Box::new(expr)),
                        limit,
                        end: Some(Box::new(end)),
                        step: None,
                    })
                    .into(),
                    pattern: None,
                })));
            }
        }
        *input = literal_probe;
        return Ok(Pattern::new(PatternKind::Variant(PatternVariant {
            name: expr,
            pattern: None,
        })));
    }

    let mut array_probe = *input;
    if skip_symbol(&mut array_probe, "[").is_ok() {
        let mut patterns = Vec::new();
        let mut has_rest = false;
        if peek_symbol(array_probe) != Some("]") {
            loop {
                if skip_symbol(&mut array_probe, "..").is_ok() {
                    has_rest = true;
                    let mut comma_probe = array_probe;
                    if skip_symbol(&mut comma_probe, ",").is_ok() {
                        array_probe = comma_probe;
                        if peek_symbol(array_probe) == Some("]") {
                            break;
                        }
                        continue;
                    }
                    break;
                }
                patterns.push(parse_pattern_alternatives(&mut array_probe)?);
                let mut comma_probe = array_probe;
                if skip_symbol(&mut comma_probe, ",").is_err() {
                    break;
                }
                array_probe = comma_probe;
                if peek_symbol(array_probe) == Some("]") {
                    break;
                }
            }
        }
        skip_symbol(&mut array_probe, "]")?;
        *input = array_probe;
        if !has_rest {
            let values = patterns
                .iter()
                .map(array_pattern_to_expr)
                .collect::<Option<Vec<_>>>();
            if let Some(values) = values {
                return Ok(Pattern::new(PatternKind::Variant(PatternVariant {
                    name: ExprKind::Array(ExprArray {
                        span: Span::null(),
                        values,
                    })
                    .into(),
                    pattern: None,
                })));
            }
        }
        return Ok(Pattern::new(PatternKind::Tuple(PatternTuple { patterns })));
    }

    let name = parse_name(input)?;
    if let Some(ident) = name.as_ident().cloned() {
        let mut bind_probe = *input;
        if skip_symbol(&mut bind_probe, "@").is_ok() {
            let pattern = if skip_symbol(&mut bind_probe, "..").is_ok() {
                Pattern::new(PatternKind::Wildcard(PatternWildcard {}))
            } else {
                parse_general_pattern(&mut bind_probe)?
            };
            *input = bind_probe;
            return Ok(Pattern::new(PatternKind::Bind(PatternBind {
                ident: PatternIdent::new(ident),
                pattern: Box::new(pattern),
            })));
        }
    }
    if let Some(ident) = name.as_ident() {
        if ident.as_str() == "_" {
            return Ok(Pattern::new(PatternKind::Wildcard(PatternWildcard {})));
        }
        if ident.as_str() == "true" || ident.as_str() == "false" {
            return Ok(Pattern::new(PatternKind::Variant(PatternVariant {
                name: Expr::value(Value::bool(ident.as_str() == "true")),
                pattern: None,
            })));
        }
    }

    let mut probe = *input;
    if skip_symbol(&mut probe, "{").is_ok() {
        let mut fields = Vec::new();
        let mut has_rest = false;
        if peek_symbol(probe) != Some("}") {
            loop {
                if skip_symbol(&mut probe, "..").is_ok() {
                    has_rest = true;
                    break;
                }

                let field_rename = if peek_ident_like(probe) == Some("ref")
                    || matches!(probe.first(), Some(token) if token.kind == TokenKind::Keyword(Keyword::Mut))
                {
                    let saw_ref = if peek_ident_like(probe) == Some("ref") {
                        let _ = ident_like(&mut probe)?;
                        true
                    } else {
                        false
                    };
                    let saw_mut = skip_keyword(&mut probe, Keyword::Mut).is_ok();
                    let field_name = ident_like(&mut probe)?;
                    let mut pattern = Pattern::new(PatternKind::Ident(PatternIdent {
                        ident: field_name.clone(),
                        mutability: saw_mut.then_some(true),
                    }));
                    if saw_ref {
                        pattern = Pattern::new(PatternKind::Ref(PatternRef {
                            mutability: None,
                            pattern: Box::new(pattern),
                        }));
                    }
                    Some((field_name, Some(Box::new(pattern))))
                } else {
                    None
                };
                let (field_name, rename) = if let Some((field_name, rename)) = field_rename {
                    (field_name, rename)
                } else {
                    let field_name = ident_like(&mut probe)?;
                    let rename = if skip_symbol(&mut probe, ":").is_ok() {
                        Some(Box::new(parse_pattern_alternatives(&mut probe)?))
                    } else {
                        None
                    };
                    (field_name, rename)
                };
                fields.push(fp_core::ast::PatternStructField {
                    name: field_name,
                    rename,
                });

                let mut comma_probe = probe;
                if skip_symbol(&mut comma_probe, ",").is_err() {
                    break;
                }
                probe = comma_probe;
                if peek_symbol(probe) == Some("}") {
                    break;
                }
            }
        }
        skip_symbol(&mut probe, "}")?;
        *input = probe;
        let struct_name = name
            .to_path()
            .segments
            .last()
            .map(|segment| segment.ident.clone())
            .or_else(|| name.as_ident().cloned())
            .ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
        return Ok(Pattern::new(PatternKind::Struct(
            fp_core::ast::PatternStruct {
                name: struct_name,
                fields,
                has_rest,
            },
        )));
    }

    let mut probe = *input;
    if skip_symbol(&mut probe, "(").is_ok() {
        let mut patterns = Vec::new();
        if peek_symbol(probe) != Some(")") {
            loop {
                if skip_symbol(&mut probe, "..").is_ok() {
                    let mut comma_probe = probe;
                    if skip_symbol(&mut comma_probe, ",").is_ok() {
                        probe = comma_probe;
                        if peek_symbol(probe) == Some(")") {
                            break;
                        }
                        continue;
                    }
                    break;
                }
                patterns.push(parse_pattern_alternatives(&mut probe)?);
                let mut comma_probe = probe;
                if skip_symbol(&mut comma_probe, ",").is_err() {
                    break;
                }
                probe = comma_probe;
                if peek_symbol(probe) == Some(")") {
                    break;
                }
            }
        }
        skip_symbol(&mut probe, ")")?;
        *input = probe;
        return Ok(Pattern::new(PatternKind::TupleStruct(PatternTupleStruct {
            name,
            patterns,
        })));
    }

    if matches!(name.as_ident().map(Ident::as_str), Some("true" | "false")) {
        return Ok(Pattern::new(PatternKind::Variant(PatternVariant {
            name: Expr::name(name),
            pattern: None,
        })));
    }

    let is_plain_ident_pattern =
        name.path.prefix == PathPrefix::Plain && name.path.segments.len() == 1;
    if !is_plain_ident_pattern {
        return Ok(Pattern::new(PatternKind::Name(name)));
    }

    let ident = name
        .as_ident()
        .cloned()
        .ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
    Ok(Pattern::new(PatternKind::Ident(PatternIdent::new(ident))))
}

pub(super) fn parse_literal_pattern_expr(input: &mut &[Token]) -> ModalResult<Expr> {
    let mut probe = *input;
    if let Ok(minus) = expect_symbol(&mut probe, "-") {
        let value = parse_number(&mut probe)?;
        let span = Span::union([token_span_to_span(&minus), value.span()]);
        *input = probe;
        return Ok(ExprKind::UnOp(ExprUnOp {
            span,
            op: UnOpKind::Neg,
            val: Box::new(value),
        })
        .into());
    }
    parse_string(input, 0).or_else(|_| parse_number(input))
}

/// A range *pattern*'s bound specifically (either side of `A..=B`, or the
/// end of an open-start `..B`) — unlike [`parse_literal_pattern_expr`]
/// (used more broadly to check "is this whole pattern just a bare
/// literal", where a plain identifier must NOT match — that's a binding/
/// enum-variant pattern instead), a range bound really can be a named
/// const (real `core::char::methods`'s `match code { ..MAX_ONE_B => 1,
/// ..MAX_TWO_B => 2, .. }`), not just a literal.
pub(super) fn parse_range_bound_expr(input: &mut &[Token]) -> ModalResult<Expr> {
    if let Ok(value) = parse_literal_pattern_expr(input) {
        return Ok(value);
    }
    let mut path_probe = *input;
    let name = parse_name(&mut path_probe)?;
    *input = path_probe;
    Ok(Expr::name(name))
}
pub(super) fn starts_ref_pattern_target(input: &[Token]) -> bool {
    matches!(
        input.first(),
        Some(Token {
            kind: TokenKind::Ident | TokenKind::Keyword(_),
            ..
        }) | Some(Token {
            kind: TokenKind::Number,
            ..
        })
    ) || matches!(
        input.first(),
        Some(Token {
            kind: TokenKind::Symbol,
            lexeme,
            ..
        }) if matches!(lexeme.as_str(), "_" | "&" | "(" | "{" | "[")
    )
}

pub(super) fn array_pattern_to_expr(pattern: &Pattern) -> Option<Expr> {
    match pattern.kind() {
        PatternKind::Variant(PatternVariant {
            name,
            pattern: None,
        }) => Some(name.clone()),
        PatternKind::Wildcard(_) => Some(Expr::name(Name::from_ident(Ident::new("_")))),
        _ => None,
    }
}

pub(crate) fn parse_general_pattern(input: &mut &[Token]) -> ModalResult<Pattern> {
    if peek_symbol(input) == Some("(") {
        skip_symbol(input, "(")?;
        let mut patterns = Vec::new();
        if peek_symbol(input) != Some(")") {
            loop {
                if skip_symbol(input, "..").is_ok() {
                    let mut probe = *input;
                    if skip_symbol(&mut probe, ",").is_ok() {
                        *input = probe;
                        if peek_symbol(input) == Some(")") {
                            break;
                        }
                        continue;
                    }
                    break;
                }
                patterns.push(parse_pattern_alternatives(input)?);
                let mut probe = *input;
                if skip_symbol(&mut probe, ",").is_err() {
                    break;
                }
                *input = probe;
                if peek_symbol(input) == Some(")") {
                    break;
                }
            }
        }
        skip_symbol(input, ")")?;
        return Ok(Pattern::new(PatternKind::Tuple(PatternTuple { patterns })));
    }
    parse_match_pattern(input)
}
