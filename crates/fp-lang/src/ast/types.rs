use super::*;
use fp_core::ast::FnRetTy;
use fp_core::ast::ImplTraits;
use fp_core::ast::QSelf;
use fp_core::ast::TypeNothing;
use fp_core::ast::TypeType;
use fp_core::ast::TypeWildcard;

/// A UFCS-disambiguated qualified path in type position.
fn parse_qualified_path_type(input: &mut &[Token]) -> ModalResult<Ty> {
    let original = *input;
    let mut probe = *input;
    // A nested qualified path (real `core::future::future`'s own
    // `<<P as ops::Deref>::Target as Future>::Output`) lexes its two
    // adjacent openers as one `<<` token — same ambiguity `try_eat_symbol`
    // already resolves for ordinary generic-argument nesting.
    if !try_eat_symbol(&mut probe, "<") {
        return Err(ErrMode::Backtrack(ContextError::new()));
    }
    let ty = parse_type_expr(&mut probe)?;
    let trait_ty = if skip_keyword(&mut probe, Keyword::As).is_ok() {
        Some(parse_type_expr(&mut probe)?)
    } else {
        None
    };
    skip_symbol(&mut probe, ">")?;
    // `parse_name` owns path-segment parsing, including per-segment generic
    // and parenthesized arguments. Keeping that representation intact is
    // important for QPath lowering: `<T as Trait>::Assoc::Nested` must retain
    // every segment rather than collapsing the first associated item into a
    // legacy `TypeProjection` node.
    if skip_symbol(&mut probe, "::").is_err() {
        return Err(ErrMode::Backtrack(ContextError::new()));
    }
    let assoc = parse_name(&mut probe)?;
    let Name {
        qself: assoc_qself,
        path: assoc_path,
    } = assoc;
    if assoc_qself.is_some() || assoc_path.segments.is_empty() {
        return Err(ErrMode::Cut(ContextError::new()));
    }
    let assoc_path_span = assoc_path.span();

    let (prefix, segments, position, path_span, path_span_complete) =
        if let Some(trait_ty) = trait_ty {
            let Ty::Expr(trait_expr) = &trait_ty else {
                return Err(ErrMode::Cut(ContextError::new()));
            };
            let ExprKind::Name(Name {
                qself: None,
                path: trait_path,
            }) = trait_expr.kind()
            else {
                return Err(ErrMode::Cut(ContextError::new()));
            };
            if trait_path.segments.is_empty() {
                return Err(ErrMode::Cut(ContextError::new()));
            }
            // `QSelf::position` is the insertion index of the qualified self in
            // the complete path.  For `<T as Trait>::Item`, the path is
            // `Trait::Item` and the qself is inserted before `Item`, so the
            // position is `1` (the number of trait-path segments), matching
            // rustc's AST representation.
            let position = trait_path.segments.len();
            let prefix = trait_path.prefix;
            let segments = trait_path
                .segments
                .iter()
                .cloned()
                .chain(assoc_path.segments)
                .collect();
            // rustc's `QSelf::path_span` covers the trait path in the
            // `as` qualification, not the qualified receiver type.
            let path_span = trait_path.span();
            let path_span_complete = Span::union([
                original
                    .first()
                    .map(token_span_to_span)
                    .unwrap_or_else(Span::null),
                assoc_path_span,
            ]);
            (prefix, segments, position, path_span, path_span_complete)
        } else {
            (
                assoc_path.prefix,
                assoc_path.segments,
                0,
                Span::null(),
                Span::union([
                    original
                        .first()
                        .map(token_span_to_span)
                        .unwrap_or_else(Span::null),
                    assoc_path_span,
                ]),
            )
        };

    *input = probe;
    Ok(Ty::Expr(Box::new(Expr::name(Name {
        qself: Some(QSelf {
            ty: Box::new(ty),
            path_span,
            position,
        }),
        path: Path::with_span(path_span_complete, prefix, segments),
    }))))
}

/// True for an ordinary double-quoted string lexeme (`"foo"`) with no
/// `f`/`b`/`c`/`r`-family prefix and no single-quote char/byte-char form —
/// the only string-literal shape that becomes a string literal type. See
/// `parse_string` in `ast/expr.rs` for the full prefix dispatch this mirrors.
fn is_plain_string_lexeme(lexeme: &str) -> bool {
    lexeme.starts_with('"')
}

pub(crate) fn parse_simple_type(input: &mut &[Token]) -> ModalResult<Ty> {
    // A relaxed bound (`?Sized`) can appear as an operand of a `+`-joined
    // bound list (`R: Read + ?Sized`), which `parse_type_binary` parses via
    // ordinary type-expression recursion — so the `?` marker must be
    // droppable wherever a type is expected, not just at `parse_type_bounds`'
    // own top level.
    if skip_symbol(input, "?").is_ok() {
        return parse_simple_type(input);
    }
    // A nested qualified path (`<<P as ops::Deref>::Target as Future>::
    // Output`, real `core::future::future`'s own) lexes its two leading
    // `<` as one `<<` token — the same ambiguity `parse_qualified_path_
    // type`'s own `try_eat_symbol` already resolves *inside* itself, but
    // this entry gate needs to recognize `<<` as "still a qualified path
    // starts here" too, or it never even attempts the call.
    if matches!(peek_symbol(*input), Some("<") | Some("<<")) {
        let mut probe = *input;
        if let Ok(ty) = parse_qualified_path_type(&mut probe) {
            *input = probe;
            return Ok(ty);
        }
    }
    let _is_unsafe = skip_keyword(input, Keyword::Unsafe).is_ok();
    let abi = if skip_keyword(input, Keyword::Extern).is_ok() {
        let abi = token_kind(input, TokenKind::StringLiteral)?;
        let _ =
            decode_string_literal(&abi.lexeme).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
        true
    } else {
        false
    };
    if skip_keyword(input, Keyword::Impl).is_ok() {
        let bounds = parse_type_bounds(input)?;
        return Ok(Ty::ImplTraits(fp_core::ast::ImplTraits { bounds }));
    }
    if skip_keyword(input, Keyword::Struct).is_ok() {
        return parse_structural_type_body(input);
    }
    if peek_symbol(input) == Some("{") {
        if let Some(refinement) = try_parse_refinement_type(input) {
            return Ok(refinement);
        }
        return parse_structural_type_body(input);
    }
    if skip_symbol(input, "!").is_ok() {
        // The never type (`fn f() -> !`) is by far the more common shape
        // this `!` starts — real `core::panicking`'s own `-> ! where T:
        // ..` immediately follows it with a `where` clause, which
        // `parse_name` below would otherwise happily consume as if it
        // were a plain identifier (`where` is only a keyword in the
        // handful of positions that specifically check for it, not a
        // token `ident_like` itself excludes), turning the whole clause
        // into a bogus `!where` "negative trait bound" and corrupting
        // everything downstream. `where` is never a valid negative-bound
        // name, so ruling it out here is exact, not a guess.
        if peek_ident_like(*input) != Some("where") {
            let mut probe = *input;
            if let Ok(name) = parse_name(&mut probe) {
                *input = probe;
                return Ok(Ty::Expr(Box::new(
                    ExprKind::UnOp(ExprUnOp {
                        span: Span::null(),
                        op: UnOpKind::Not,
                        val: Box::new(Expr::name(name)),
                    })
                    .into(),
                )));
            }
        }
        return Ok(Ty::Nothing(TypeNothing));
    }
    if skip_symbol(input, "(").is_ok() {
        if skip_symbol(input, ")").is_ok() {
            return Ok(Ty::unit());
        }
        let first = parse_type_expr(input)?;
        if skip_symbol(input, ",").is_ok() {
            let mut types = vec![first];
            if peek_symbol(input) != Some(")") {
                loop {
                    types.push(parse_type_expr(input)?);
                    if skip_symbol(input, ",").is_err() {
                        break;
                    }
                    if peek_symbol(input) == Some(")") {
                        break;
                    }
                }
            }
            skip_symbol(input, ")")?;
            return Ok(Ty::Tuple(fp_core::ast::TypeTuple { types }.into()));
        }
        skip_symbol(input, ")")?;
        return Ok(first);
    }
    if abi || skip_keyword(input, Keyword::Fn).is_ok() {
        if !abi {
            // already consumed `fn` in the branch condition above
        } else {
            skip_keyword(input, Keyword::Fn)?;
        }
        skip_symbol(input, "(")?;
        let mut params = Vec::new();
        if peek_symbol(input) != Some(")") {
            loop {
                // A fn-pointer type's parameter may carry an optional,
                // purely-documentary name/`_` label before its type (real
                // `core::io::error`'s own `pub format_os_error: fn(_:
                // RawOsError, _: &mut fmt::Formatter<'_>, _: &str) ->
                // fmt::Result`) — unlike an ordinary fn item's parameters,
                // this label binds nothing (a fn pointer has no body to
                // reference it in), so it's parsed and dropped.
                let mut probe = *input;
                let has_label =
                    ident_like(&mut probe).is_ok() && skip_symbol(&mut probe, ":").is_ok();
                if has_label {
                    *input = probe;
                }
                // A C-variadic marker (real `std::sys::pal::uefi::helpers`'s
                // own `extern "efiapi" fn(_: *mut r_efi::efi::Handle, _:
                // ...) -> r_efi::efi::Status`) — this checker has no
                // notion of variadic fn-pointer types, so the trailing
                // `...` is dropped like any other checker-inert construct;
                // it's always last in a real parameter list, so stopping
                // here is exactly equivalent to real Rust's own grammar.
                if skip_symbol(input, "...").is_ok() {
                    break;
                }
                params.push(parse_type_expr(input)?);
                if skip_symbol(input, ",").is_err() {
                    break;
                }
                if peek_symbol(input) == Some(")") {
                    break;
                }
            }
        }
        skip_symbol(input, ")")?;
        let ret_ty = if skip_symbol(input, "->").is_ok() {
            Some(Box::new(parse_type_expr(input)?))
        } else {
            None
        };
        return Ok(Ty::Function(
            TypeFunction {
                params,
                generics_params: Vec::new(),
                ret_ty,
            }
            .into(),
        ));
    }
    if skip_keyword(input, Keyword::Impl).is_ok() {
        let bounds = parse_dyn_type_bounds(input)?;
        return Ok(Ty::ImplTraits(ImplTraits { bounds }));
    }
    if skip_symbol(input, "&&").is_ok() {
        let lifetime = match peek_ident_like(*input) {
            Some(ident) if ident.starts_with('\'') => Some(ident_like(input)?),
            _ => None,
        };
        let mutability = skip_keyword(input, Keyword::Mut).is_ok();
        let inner = parse_type_expr(input)?;
        let inner = Ty::Reference(
            TypeReference {
                ty: Box::new(inner),
                mutability: mutability.then_some(true),
                lifetime,
            }
            .into(),
        );
        return Ok(Ty::Reference(
            TypeReference {
                ty: Box::new(inner),
                mutability: None,
                lifetime: None,
            }
            .into(),
        ));
    }
    if skip_symbol(input, "&").is_ok() {
        let lifetime = match peek_ident_like(*input) {
            Some(ident) if ident.starts_with('\'') => Some(ident_like(input)?),
            _ => None,
        };
        let mutability = skip_keyword(input, Keyword::Mut).is_ok();
        let inner = parse_type_expr(input)?;
        return Ok(Ty::Reference(
            TypeReference {
                ty: Box::new(inner),
                mutability: mutability.then_some(true),
                lifetime,
            }
            .into(),
        ));
    }
    if skip_symbol(input, "*").is_ok() {
        let mutability = if skip_keyword(input, Keyword::Mut).is_ok() {
            Some(true)
        } else if skip_keyword(input, Keyword::Const).is_ok() {
            Some(false)
        } else {
            return Err(ErrMode::Cut(ContextError::new()));
        };
        let inner = parse_type_expr(input)?;
        return Ok(Ty::raw_ptr(inner, mutability));
    }
    if skip_symbol(input, "[").is_ok() {
        let inner = parse_type_expr(input)?;
        if skip_symbol(input, ";").is_ok() {
            let len = parse_expr_winnow_no_struct(input, 0)?;
            skip_symbol(input, "]")?;
            return Ok(Ty::Array(
                fp_core::ast::TypeArray {
                    elem: Box::new(inner),
                    len: Box::new(len),
                }
                .into(),
            ));
        }
        skip_symbol(input, "]")?;
        return Ok(Ty::Slice(TypeSlice {
            elem: Box::new(inner),
        }));
    }
    if peek_ident_like(*input) == Some("dyn") {
        let _ = ident_like(input)?;
        let bounds = parse_dyn_type_bounds(input)?;
        return Ok(Ty::TypeBounds(bounds));
    }
    // `pattern_type!(BASE is PATTERN)` (real `core::ptr::non_null`'s own
    // `pattern_type!(*const T is !null)`, `core::num::niche_types`'
    // `pattern_type!(u32 is 0..u32::MAX)`/`pattern_type!(i32 is ..-1 |
    // 0..)`) — a nightly-only builtin (not an ordinary `macro_rules!`
    // invocation `looks_like_type_expr_macro`/`parse_macro_expr` below
    // already handle generically), refining `BASE` with a validity
    // pattern this checker has no way to model (same reasoning as
    // dropping lifetimes: the pattern is a compile-time-only invariant,
    // never part of the value's actual runtime representation, which is
    // always just `BASE`). `PATTERN` can be an arbitrary range/negation/
    // alternation pattern, including ones with path-expression bounds
    // (`0..u32::MAX`) or negative literals (`..-1`) that don't parse as
    // an ordinary expression at all — rather than modeling real pattern
    // syntax just to discard it, skip everything up to the invocation's
    // own balanced closing `)` and keep only `BASE`.
    if let Some(base) = try_parse_pattern_type_macro(input) {
        return Ok(base);
    }
    // A type-position macro invocation (real `core::pat`'s own nightly
    // `pattern_type!(*const T is !null)`) must be parsed as just the
    // invocation itself, NOT via the full binary-operator-aware expression
    // entry point — inside a generic-arg list (`Foo<pattern_type!(..)>`)
    // that entry point would try to keep consuming tokens past the macro
    // call looking for a binary operator, misreading the arg list's own
    // closing `>` as a comparison operator (or, worse, an unrelated
    // trailing `= expr` as this "type"'s own assignment RHS).
    if looks_like_type_expr_macro(*input) {
        let expr = parse_macro_expr(input)?;
        return Ok(Ty::Expr(Box::new(expr)));
    }
    // A plain string literal in type position (not an f-string/template,
    // byte/char literal, etc.) is a string literal type, e.g.
    // `type A = "foo";` — parsed directly (bypassing the full
    // binary-operator-aware expression grammar below) so that `|`
    // immediately following it is left for `parse_type_binary`'s own
    // union-operator handling rather than being consumed as `Value`'s
    // bitwise-or expression operator.
    if matches!(input.first(), Some(token) if token.kind == TokenKind::StringLiteral && is_plain_string_lexeme(&token.lexeme))
    {
        let token = token_kind(input, TokenKind::StringLiteral)?;
        let value = decode_string_literal(&token.lexeme)
            .ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
        return Ok(Ty::Literal(fp_core::ast::TypeLiteralString { value }));
    }
    if matches!(input.first(), Some(token) if token.kind == TokenKind::Keyword(Keyword::Const))
        || matches!(input.first(), Some(token) if token.kind == TokenKind::Number || token.kind == TokenKind::StringLiteral)
        || matches!(peek_ident_like(*input), Some("true" | "false" | "null"))
    {
        let expr = parse_expr_winnow_no_struct(input, 0)?;
        if let ExprKind::ConstBlock(const_block) = expr.kind() {
            return Ok(Ty::ConstBlock(const_block.clone()));
        }
        return Ok(Ty::Expr(Box::new(expr)));
    }
    let name = parse_name(input)?;
    if let Ok(open_paren) = expect_symbol(input, "(") {
        let open_span = token_span_to_span(&open_paren);
        if skip_symbol(input, "..").is_ok() {
            let close_paren = expect_symbol(input, ")")?;
            let span = Span::union([open_span, token_span_to_span(&close_paren)]);
            let Name { mut path, qself } = name;
            if qself.is_some() {
                return Err(ErrMode::Cut(ContextError::new()));
            }
            path.span = Span::union([path.span, span]);
            let Some(segment) = path.segments.last_mut() else {
                return Err(ErrMode::Cut(ContextError::new()));
            };
            segment.args = Some(Box::new(GenericArgs::ParenthesizedElided(span)));
            return Ok(Ty::Expr(Box::new(Expr::name(Name { qself, path }))));
        }
        let mut params = Vec::new();
        if peek_symbol(input) != Some(")") {
            loop {
                params.push(parse_type_expr(input)?);
                if skip_symbol(input, ",").is_err() {
                    break;
                }
                if peek_symbol(input) == Some(")") {
                    break;
                }
            }
        }
        let close_paren = expect_symbol(input, ")")?;
        let inputs_span = Span::union([open_span, token_span_to_span(&close_paren)]);
        let ret_ty = if skip_symbol(input, "->").is_ok() {
            FnRetTy::Ty(Box::new(parse_type_expr(input)?))
        } else {
            FnRetTy::Default(Span::new(
                token_span_to_span(&close_paren).file,
                token_span_to_span(&close_paren).hi,
                token_span_to_span(&close_paren).hi,
            ))
        };
        let Name { mut path, qself } = name;
        if qself.is_some() {
            return Err(ErrMode::Cut(ContextError::new()));
        }
        let span = Span::union([inputs_span, ret_ty.span()]);
        path.span = Span::union([path.span, span]);
        let Some(segment) = path.segments.last_mut() else {
            return Err(ErrMode::Cut(ContextError::new()));
        };
        segment.args = Some(Box::new(GenericArgs::Parenthesized(
            fp_core::ast::ParenthesizedArgs {
                span,
                inputs: params,
                inputs_span,
                output: ret_ty,
            },
        )));
        return Ok(Ty::Expr(Box::new(Expr::name(Name { qself, path }))));
    }
    {
        let parameter_path = &name.path;
        if parameter_path.prefix == PathPrefix::Plain
            && parameter_path.segments.len() == 1
            && parameter_path.segments[0].ident.as_str() == "quote"
            && matches!(
                parameter_path.segments[0].args.as_deref(),
                Some(GenericArgs::AngleBracketed(args)) if args.args.len() == 1
            )
        {
            let Some(GenericArgs::AngleBracketed(args)) =
                parameter_path.segments[0].args.as_deref()
            else {
                unreachable!("quote arguments checked above");
            };
            let AngleBracketedArg::Arg(GenericArg::Type(argument)) = &args.args[0] else {
                return Err(ErrMode::Cut(ContextError::new()));
            };
            let (kind, inner_ty) = match argument.as_ref() {
                Ty::Expr(expr) => match expr.kind() {
                    ExprKind::Name(name) => {
                        let k = match name.as_ident().map(Ident::as_str) {
                            Some("item") => QuoteFragmentKind::Item,
                            Some("expr") => QuoteFragmentKind::Expr,
                            Some("stmt") => QuoteFragmentKind::Stmt,
                            Some("type") => QuoteFragmentKind::Type,
                            _ => return Err(ErrMode::Cut(ContextError::new())),
                        };
                        (k, None)
                    }
                    ExprKind::Invoke(invoke) => {
                        let target_name = match &invoke.target {
                            ExprInvokeTarget::Function(name) => name.as_ident().map(Ident::as_str),
                            _ => None,
                        };
                        let k = match target_name {
                            Some("item") => QuoteFragmentKind::Item,
                            Some("expr") => QuoteFragmentKind::Expr,
                            Some("stmt") => QuoteFragmentKind::Stmt,
                            Some("type") => QuoteFragmentKind::Type,
                            _ => return Err(ErrMode::Cut(ContextError::new())),
                        };
                        let inner_ty = invoke.args.first().cloned();
                        let inner = inner_ty.map(|e| Box::new(Ty::Expr(Box::new(e))));
                        (k, inner)
                    }
                    _ => return Err(ErrMode::Cut(ContextError::new())),
                },
                Ty::Slice(slice) => match slice.elem.as_ref() {
                    Ty::Expr(expr) => match expr.kind() {
                        ExprKind::Name(name) => {
                            let inner_kind = match name.as_ident().map(Ident::as_str) {
                                Some("item") => QuoteFragmentKind::Item,
                                Some("expr") => QuoteFragmentKind::Expr,
                                Some("stmt") => QuoteFragmentKind::Stmt,
                                Some("type") => QuoteFragmentKind::Type,
                                _ => return Err(ErrMode::Cut(ContextError::new())),
                            };
                            let item_quote = Ty::Quote(TypeQuote {
                                span: Span::null(),
                                kind: inner_kind,
                                item: None,
                                inner: None,
                            });
                            return Ok(Ty::Quote(TypeQuote {
                                span: Span::null(),
                                kind: inner_kind,
                                item: None,
                                inner: Some(Box::new(Ty::Slice(TypeSlice {
                                    elem: Box::new(item_quote),
                                }))),
                            }));
                        }
                        _ => return Err(ErrMode::Cut(ContextError::new())),
                    },
                    _ => return Err(ErrMode::Cut(ContextError::new())),
                },
                _ => return Err(ErrMode::Cut(ContextError::new())),
            };
            return Ok(Ty::Quote(TypeQuote {
                span: Span::null(),
                kind,
                item: None,
                inner: inner_ty,
            }));
        }
    }
    // Bare quote fragment keywords inside generic arguments remain ordinary
    // name expressions; only their parameterized forms (`quote<...>`) are
    // lowered to quote types below.
    if name
        .as_ident()
        .is_some_and(|ident| matches!(ident.as_str(), "item" | "expr" | "stmt"))
    {
        return Ok(Ty::Expr(Box::new(Expr::name(name))));
    }
    let bare_path = Some(&name.path);
    // Handle `type` keyword — both bare and with type args like `type<_>`, `type<i64>`
    let type_name = match bare_path {
        Some(path) if path.prefix == PathPrefix::Plain && path.segments.len() == 1 => {
            path.segments[0].as_str().to_string()
        }
        _ if name
            .as_ident()
            .is_some_and(|ident| ident.as_str() == "type") =>
        {
            "type".to_string()
        }
        _ => String::new(),
    };
    if type_name == "type" {
        if let Some(ppath) = bare_path {
            let Some(GenericArgs::AngleBracketed(args)) = ppath.segments[0].args.as_deref()
            else {
                return Ok(Ty::Type(TypeType {
                    span: Span::null(),
                    inner: None,
                }));
            };
            if args.args.len() == 1 {
                let AngleBracketedArg::Arg(GenericArg::Type(arg)) = &args.args[0] else {
                    return Err(ErrMode::Cut(ContextError::new()));
                };
                let inner = if is_path_ident(arg, "_") {
                    Some(Box::new(Ty::Wildcard(TypeWildcard)))
                } else {
                    Some(Box::new((**arg).clone()))
                };
                return Ok(Ty::Type(TypeType {
                    span: Span::null(),
                    inner,
                }));
            }
        }
        // bare `type` keyword (no type args) — meta-type
        return Ok(Ty::Type(TypeType {
            span: Span::null(),
            inner: None,
        }));
    }
    if let Some(path) = bare_path {
        if path.prefix == PathPrefix::Plain && path.segments.len() == 1 {
            match path.segments[0].as_str() {
                "item" => {
                    return Ok(Ty::Quote(TypeQuote {
                        span: Span::null(),
                        kind: QuoteFragmentKind::Item,
                        item: None,
                        inner: None,
                    }));
                }
                "expr" => {
                    return Ok(Ty::Quote(TypeQuote {
                        span: Span::null(),
                        kind: QuoteFragmentKind::Expr,
                        item: None,
                        inner: None,
                    }));
                }
                "stmt" => {
                    return Ok(Ty::Quote(TypeQuote {
                        span: Span::null(),
                        kind: QuoteFragmentKind::Stmt,
                        item: None,
                        inner: None,
                    }));
                }
                "bool" => return Ok(Ty::Primitive(TypePrimitive::Bool)),
                "any" => return Ok(Ty::any()),
                "str" => return Ok(Ty::Primitive(TypePrimitive::String)),
                "i8" => return Ok(Ty::Primitive(TypePrimitive::Int(TypeInt::I8))),
                "i16" => return Ok(Ty::Primitive(TypePrimitive::Int(TypeInt::I16))),
                "i32" => return Ok(Ty::Primitive(TypePrimitive::Int(TypeInt::I32))),
                "i64" | "isize" => return Ok(Ty::Primitive(TypePrimitive::Int(TypeInt::I64))),
                "i128" => return Ok(Ty::Primitive(TypePrimitive::Int(TypeInt::I128))),
                "u8" => return Ok(Ty::Primitive(TypePrimitive::Int(TypeInt::U8))),
                "u16" => return Ok(Ty::Primitive(TypePrimitive::Int(TypeInt::U16))),
                "u32" => return Ok(Ty::Primitive(TypePrimitive::Int(TypeInt::U32))),
                "u64" | "usize" => return Ok(Ty::Primitive(TypePrimitive::Int(TypeInt::U64))),
                "u128" => return Ok(Ty::Primitive(TypePrimitive::Int(TypeInt::U128))),
                "f32" => return Ok(Ty::Primitive(TypePrimitive::Decimal(DecimalType::F32))),
                "f64" => return Ok(Ty::Primitive(TypePrimitive::Decimal(DecimalType::F64))),
                "_" => return Ok(Ty::Wildcard(TypeWildcard)),
                _ => {}
            }
        }
        let mut ty = Ty::path(path.clone());
        if skip_symbol(input, "?").is_ok() {
            ty = Ty::TypeBinaryOp(
                TypeBinaryOp {
                    kind: TypeBinaryOpKind::Union,
                    lhs: Box::new(ty),
                    rhs: Box::new(Ty::value(Value::None(ValueNone))),
                }
                .into(),
            );
        }
        return Ok(ty);
    }
    if name.as_ident().is_some_and(|ident| ident.as_str() == "any") {
        return Ok(Ty::any());
    }
    if name.as_ident().is_some() {
        // `Name::path()` canonicalizes any single-segment plain path (a
        // bare `Foo`) through the same path-based handling used above.
        let mut ty = Ty::name(name.clone());
        if skip_symbol(input, "?").is_ok() {
            ty = Ty::TypeBinaryOp(
                TypeBinaryOp {
                    kind: TypeBinaryOpKind::Union,
                    lhs: Box::new(ty),
                    rhs: Box::new(Ty::value(Value::None(ValueNone))),
                }
                .into(),
            );
        }
        return Ok(ty);
    }
    Ok(Ty::name(name))
}

/// Parses `pattern_type!(BASE is PATTERN)` if `input` starts with it,
/// returning just `BASE` (see the call site's doc comment for why the
/// pattern itself is dropped rather than modeled) and advancing `input`
/// past the whole invocation. Returns `None` (leaving `input` untouched)
/// if it isn't this specific shape at all, so the caller can fall through
/// to the ordinary macro-invocation handling for every other macro name.
fn try_parse_pattern_type_macro(input: &mut &[Token]) -> Option<Ty> {
    let mut probe = *input;
    // An absolute/`crate::`-qualified invocation (real `core::ptr::
    // non_null`'s own `crate::pattern_type!(*const T is !null)`) — this
    // builtin is always referenced unqualified in ordinary code, but
    // vendored std occasionally spells it out at its own definition
    // site's use.
    if peek_ident_like(probe) == Some("crate") {
        let mut qualified_probe = probe;
        let _ = ident_like(&mut qualified_probe).ok()?;
        if skip_symbol(&mut qualified_probe, "::").is_ok() {
            probe = qualified_probe;
        }
    }
    if peek_ident_like(probe) != Some("pattern_type") {
        return None;
    }
    let _ = ident_like(&mut probe).ok()?;
    skip_symbol(&mut probe, "!").ok()?;
    skip_symbol(&mut probe, "(").ok()?;
    let base = parse_type_expr(&mut probe).ok()?;
    if peek_ident_like(probe) != Some("is") {
        return None;
    }
    let _ = ident_like(&mut probe).ok()?;
    // Skip the pattern itself — balanced-depth scan to the invocation's
    // own closing `)`, since the pattern can contain arbitrary nested
    // parens/brackets (`0..=HALF_USIZE`'s constant, tuple patterns, ...)
    // that must not be mistaken for the invocation's own terminator.
    let mut depth = 1i32;
    loop {
        if probe.is_empty() {
            return None;
        }
        // Only a `Symbol`-kind token can be one of the bracket characters
        // being balanced — an identifier/number/keyword token (`null`,
        // `HALF_USIZE`, `0`, ...) is just more pattern content to skip
        // over, never a depth change. `peek_symbol` returns `None` for
        // those, so this must not `?`-propagate that as a parse failure.
        if let Some(symbol) = peek_symbol(probe) {
            match symbol {
                "(" | "[" | "{" => depth += 1,
                ")" | "]" | "}" => {
                    depth -= 1;
                    if depth == 0 {
                        break;
                    }
                }
                _ => {}
            }
        }
        probe = &probe[1..];
    }
    skip_symbol(&mut probe, ")").ok()?;
    *input = probe;
    Some(base)
}

fn looks_like_type_expr_macro(input: &[Token]) -> bool {
    matches!(
        input,
        [first, second, third, ..]
            if matches!(first.kind, TokenKind::Ident | TokenKind::Keyword(_))
                && second.kind == TokenKind::Symbol
                && second.lexeme == "!"
                && third.kind == TokenKind::Symbol
                && matches!(third.lexeme.as_str(), "(" | "[" | "{")
    )
}

fn parse_dyn_type_bounds(input: &mut &[Token]) -> ModalResult<TypeBounds> {
    let mut bounds = Vec::new();
    loop {
        bounds.push(parse_trait_bound_expr(input)?);
        if skip_symbol(input, "+").is_err() {
            break;
        }
    }
    Ok(TypeBounds { bounds })
}

fn parse_trait_bound_expr(input: &mut &[Token]) -> ModalResult<Expr> {
    let name = parse_name(input)?;
    if let Ok(open_paren) = expect_symbol(input, "(") {
        let open_span = token_span_to_span(&open_paren);
        if skip_symbol(input, "..").is_ok() {
            let close_paren = expect_symbol(input, ")")?;
            let span = Span::union([open_span, token_span_to_span(&close_paren)]);
            let Name { mut path, qself } = name;
            if qself.is_some() {
                return Err(ErrMode::Cut(ContextError::new()));
            }
            path.span = Span::union([path.span, span]);
            let Some(segment) = path.segments.last_mut() else {
                return Err(ErrMode::Cut(ContextError::new()));
            };
            segment.args = Some(Box::new(GenericArgs::ParenthesizedElided(span)));
            return Ok(Expr::name(Name { qself, path }));
        }
        let mut params = Vec::new();
        if peek_symbol(input) != Some(")") {
            loop {
                params.push(parse_type_expr(input)?);
                if skip_symbol(input, ",").is_err() {
                    break;
                }
                if peek_symbol(input) == Some(")") {
                    break;
                }
            }
        }
        let close_paren = expect_symbol(input, ")")?;
        let inputs_span = Span::union([open_span, token_span_to_span(&close_paren)]);
        let ret_ty = if skip_symbol(input, "->").is_ok() {
            FnRetTy::Ty(Box::new(parse_type_expr(input)?))
        } else {
            FnRetTy::Default(Span::new(
                token_span_to_span(&close_paren).file,
                token_span_to_span(&close_paren).hi,
                token_span_to_span(&close_paren).hi,
            ))
        };
        let Name { mut path, qself } = name;
        if qself.is_some() {
            return Err(ErrMode::Cut(ContextError::new()));
        }
        let span = Span::union([inputs_span, ret_ty.span()]);
        path.span = Span::union([path.span, span]);
        let Some(segment) = path.segments.last_mut() else {
            return Err(ErrMode::Cut(ContextError::new()));
        };
        segment.args = Some(Box::new(GenericArgs::Parenthesized(
            fp_core::ast::ParenthesizedArgs {
                span,
                inputs: params,
                inputs_span,
                output: ret_ty,
            },
        )));
        return Ok(Expr::name(Name { qself, path }));
    }
    Ok(Expr::name(name))
}

pub(crate) fn parse_type_expr(input: &mut &[Token]) -> ModalResult<Ty> {
    parse_type_binary(input, 0)
}

fn parse_type_binary(input: &mut &[Token], min_prec: u8) -> ModalResult<Ty> {
    let mut lhs = parse_simple_type(input)?;
    loop {
        let Some(op) = peek_symbol(input) else {
            break;
        };
        let Some((prec, kind)) = type_binary_op(op) else {
            break;
        };
        if prec < min_prec {
            break;
        }
        let op = op.to_string();
        skip_symbol(input, &op)?;
        let rhs = parse_type_binary(input, prec + 1)?;
        lhs = Ty::TypeBinaryOp(
            TypeBinaryOp {
                kind,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            }
            .into(),
        );
    }
    Ok(lhs)
}

/// `{binder: Type where predicate}` — a refinement/subtype type (Lean 4's
/// `{binder : Type // predicate}` notation, spelled with FerroPhase's own
/// `where` keyword instead of `//` since `//` is already the line-comment
/// marker in this lexer and can never reach the parser as a token). Tried
/// before falling back to the existing `{field: Type, ...}` structural-type
/// body, since both start with `{ident :`; returns `None` (leaving `input`
/// untouched) on any parse failure rather than committing to an error, so
/// the caller can fall through cleanly.
fn try_parse_refinement_type(input: &mut &[Token]) -> Option<Ty> {
    let mut probe = *input;
    let parsed = (|| -> ModalResult<Ty> {
        skip_symbol(&mut probe, "{")?;
        let binder = ident_like(&mut probe)?;
        skip_symbol(&mut probe, ":")?;
        let base = parse_type_expr(&mut probe)?;
        skip_keyword(&mut probe, Keyword::Where)?;
        let predicate = parse_expr_winnow_no_struct(&mut probe, 0)?;
        skip_symbol(&mut probe, "}")?;
        Ok(Ty::Refinement(Box::new(fp_core::ast::TypeRefinement::new(
            base, binder, predicate,
        ))))
    })();
    match parsed {
        Ok(ty) => {
            *input = probe;
            Some(ty)
        }
        Err(_) => None,
    }
}

fn parse_structural_type_body(input: &mut &[Token]) -> ModalResult<Ty> {
    skip_symbol(input, "{")?;
    let mut fields = Vec::new();
    while peek_symbol(input) != Some("}") {
        let field_name = ident_like(input)?;
        let is_optional = skip_symbol(input, "?").is_ok();
        skip_symbol(input, ":")?;
        let mut value = parse_type_expr(input)?;
        if is_optional {
            value = Ty::TypeBinaryOp(
                TypeBinaryOp {
                    kind: TypeBinaryOpKind::Union,
                    lhs: Box::new(value),
                    rhs: Box::new(Ty::value(Value::None(ValueNone))),
                }
                .into(),
            );
        }
        fields.push(StructuralField::new(field_name, value));
        if skip_symbol(input, ",").is_err() {
            break;
        }
        if peek_symbol(input) == Some("}") {
            break;
        }
    }
    skip_symbol(input, "}")?;
    Ok(Ty::Structural(
        fp_core::ast::TypeStructural { fields }.into(),
    ))
}

fn type_binary_op(symbol: &str) -> Option<(u8, TypeBinaryOpKind)> {
    Some(match symbol {
        "|" => (1, TypeBinaryOpKind::Union),
        "&" => (2, TypeBinaryOpKind::Intersect),
        "+" => (3, TypeBinaryOpKind::Add),
        "-" => (3, TypeBinaryOpKind::Subtract),
        _ => return None,
    })
}

pub(crate) fn parse_type_bounds(input: &mut &[Token]) -> ModalResult<TypeBounds> {
    let mut bounds = Vec::new();
    loop {
        skip_const_trait_modifier(input);
        skip_hrtb_for_lifetimes(input);
        // A relaxed/"maybe" bound (`?Sized`, the only one stable today) —
        // this checker has no notion of the implicit `Sized` bound to
        // relax in the first place, so the leading `?` is dropped and the
        // named trait parsed normally rather than counted as a real bound
        // at all.
        if skip_symbol(input, "?").is_ok() {
            let _ = parse_type_expr(input)?;
        } else {
            let ty = parse_type_expr(input)?;
            bounds.push(type_to_expr(&ty));
        }
        if skip_symbol(input, "+").is_err() {
            break;
        }
    }
    Ok(TypeBounds { bounds })
}

// Nightly `[const] Trait`/bare `const Trait` bound modifier (e.g. `impl
// [const] FnOnce(T) -> U`, `T: [const] Destruct`, or real vendored std's
// own `F: const FnOnce<ARG, Output = RET>` in a `where` clause) —
// FerroPhase doesn't model const-trait-ness, so the modifier is accepted
// and dropped, leaving the plain trait bound. The bare (non-bracketed)
// `const` form is real, current unstable syntax (`const_trait_impl`),
// distinct from `[const]`'s own older proposal spelling — both reach
// here since either can appear in a bound position. Without this, `T:
// const Trait` fails to parse as a bound at all (`const` isn't a valid
// trait-name token), and — worse than just this one bound — real
// vendored std's `core::intrinsics` uses this exact shape once, and an
// unrecovered failure there was observed to fail this file's *entire*
// parse, silently dropping every intrinsic it declares.
fn skip_const_trait_modifier(input: &mut &[Token]) {
    let mut probe = *input;
    if skip_symbol(&mut probe, "[").is_ok()
        && skip_keyword(&mut probe, Keyword::Const).is_ok()
        && skip_symbol(&mut probe, "]").is_ok()
    {
        *input = probe;
        return;
    }
    let mut probe = *input;
    if skip_keyword(&mut probe, Keyword::Const).is_ok() {
        *input = probe;
    }
}

// A higher-ranked trait bound's `for<'a, 'b>` lifetime-binder prefix (real
// `core::slice::cmp`'s own `impl for<'a> FnOnce(&'a usize, &'a usize) ->
// ..`) — this checker doesn't model borrow-checking (lifetimes are
// dropped everywhere else too), so the binder is skipped entirely,
// leaving just the plain trait bound (`FnOnce(...) -> ..`) to parse
// normally. Without this, `for` isn't recognized at all here and the
// bound parse fails on the `for` keyword itself.
fn skip_hrtb_for_lifetimes(input: &mut &[Token]) {
    let mut probe = *input;
    if skip_keyword(&mut probe, Keyword::For).is_err() {
        return;
    }
    if skip_symbol(&mut probe, "<").is_err() {
        return;
    }
    loop {
        if ident_like(&mut probe).is_err() {
            return;
        }
        if skip_symbol(&mut probe, ",").is_ok() {
            continue;
        }
        break;
    }
    if skip_symbol(&mut probe, ">").is_err() {
        return;
    }
    *input = probe;
}

fn type_to_expr(ty: &Ty) -> Expr {
    match ty {
        Ty::Expr(expr) => (**expr).clone(),
        other => Expr::value(Value::Type(other.clone())),
    }
}

pub(crate) fn parse_use_tree(input: &mut &[Token]) -> ModalResult<fp_core::ast::ItemImportTree> {
    let mut path = parse_use_path(input)?;
    if skip_keyword(input, Keyword::As).is_ok() {
        let rename = ident_like(input)?;
        let from = match path.segments.pop() {
            Some(fp_core::ast::ItemImportTree::Ident(from)) => from,
            // `self as alias` (real `std::collections::hash::map`'s own
            // `use hashbrown::hash_map::{self as base, ..}`) — renaming
            // the enclosing module itself, not a named item within it;
            // `ItemImportRename.from` has no separate "the module itself"
            // slot, so this reuses the plain `self` identifier the same
            // way an ordinary renamed item would.
            Some(fp_core::ast::ItemImportTree::SelfMod) => Ident::new("self"),
            _ => return Err(ErrMode::Cut(ContextError::new())),
        };
        path.push(fp_core::ast::ItemImportTree::Rename(
            fp_core::ast::ItemImportRename { from, to: rename },
        ));
    }
    Ok(fp_core::ast::ItemImportTree::Path(path))
}

pub(crate) fn parse_use_path(input: &mut &[Token]) -> ModalResult<fp_core::ast::ItemImportPath> {
    let mut path = fp_core::ast::ItemImportPath::new();
    if skip_symbol(input, "::").is_ok() {
        path.push(fp_core::ast::ItemImportTree::Root);
    }
    loop {
        if skip_symbol(input, "*").is_ok() {
            path.push(fp_core::ast::ItemImportTree::Glob);
            break;
        }
        if peek_symbol(input) == Some("{") {
            path.push(fp_core::ast::ItemImportTree::Group(parse_use_group(input)?));
            break;
        }
        let segment = if skip_keyword(input, Keyword::Crate).is_ok() {
            fp_core::ast::ItemImportTree::Crate
        } else if peek_ident_like(*input) == Some("self") {
            let _ = ident_like(input)?;
            fp_core::ast::ItemImportTree::SelfMod
        } else if skip_keyword(input, Keyword::Super).is_ok() {
            fp_core::ast::ItemImportTree::SuperMod
        } else {
            fp_core::ast::ItemImportTree::Ident(ident_like(input)?)
        };
        path.push(segment);
        if skip_symbol(input, "::").is_err() {
            break;
        }
    }
    Ok(path)
}

fn parse_use_group(input: &mut &[Token]) -> ModalResult<fp_core::ast::ItemImportGroup> {
    skip_symbol(input, "{")?;
    let mut group = fp_core::ast::ItemImportGroup::new();
    while peek_symbol(input) != Some("}") {
        group.push(parse_use_tree(input)?);
        if skip_symbol(input, ",").is_err() {
            break;
        }
    }
    skip_symbol(input, "}")?;
    Ok(group)
}

/// Skip a `#[...]`/`#![...]` attribute's whole bracketed token run, if one
/// is present at `input`'s current position — used where an attribute's
/// value has no bearing on typechecking/codegen (see
/// `parse_optional_generic_params`'s call site) so plumbing a `FileId`
/// through for a real `parse_attrs` call isn't worth it. Tracks nesting
/// depth so an attribute whose meta contains its own brackets (e.g.
/// `#[cfg(feature = "x")]`... more relevantly `#[unstable(feature = "y",
/// issue = "z")]`, which has none, but this must still not stop at the
/// first `]` if one were nested) closes at the right one.
fn skip_bracketed_attr(input: &mut &[Token]) {
    loop {
        let mut probe = *input;
        // The tokenizer may emit `#[`/`#![` as a single combined symbol
        // lexeme or as separate `#`/`!`/`[` tokens depending on context
        // (see `parse_attrs`'s identical two-step check) — try the
        // combined form first, then fall back to the separate tokens.
        if !try_eat_symbol(&mut probe, "#[") && !try_eat_symbol(&mut probe, "#![") {
            if !try_eat_symbol(&mut probe, "#") {
                return;
            }
            let _ = try_eat_symbol(&mut probe, "!");
            if !try_eat_symbol(&mut probe, "[") {
                return;
            }
        }
        let mut depth = 1usize;
        while depth > 0 {
            if probe.is_empty() {
                return;
            }
            match peek_symbol(probe) {
                Some("[") => depth += 1,
                Some("]") => depth -= 1,
                _ => {}
            }
            probe = &probe[1..];
        }
        *input = probe;
    }
}

pub(crate) fn parse_optional_generic_params(
    input: &mut &[Token],
) -> ModalResult<Vec<fp_core::ast::GenericParam>> {
    let mut probe = *input;
    if !try_eat_symbol(&mut probe, "<") {
        return Ok(Vec::new());
    }
    let mut params = Vec::new();
    if peek_symbol(probe) != Some(">") {
        loop {
            // A generic parameter may carry its own attribute (e.g. real
            // `alloc`'s `pub struct Box<T: ?Sized, #[unstable(feature =
            // "allocator_api", issue = "32838")] A: Allocator = Global>`).
            skip_bracketed_attr(&mut probe);
            // A lifetime parameter (`<'a, T>`, `<'a: 'b, T>`). Keep it in
            // the generic parameter list like rustc does; later stages may
            // erase regions, but must still see the declaration when
            // matching generic argument positions.
            if matches!(peek_ident_like(probe), Some(name) if name.starts_with('\'')) {
                let name = ident_like(&mut probe)?;
                let mut bounds = Vec::new();
                if skip_symbol(&mut probe, ":").is_ok() {
                    loop {
                        let bound = ident_like(&mut probe)?;
                        bounds.push(Expr::name(Name::from_ident(bound)));
                        if skip_symbol(&mut probe, "+").is_err() {
                            break;
                        }
                    }
                }
                params.push(fp_core::ast::GenericParam {
                    name,
                    bounds: fp_core::ast::TypeBounds { bounds },
                    kind: fp_core::ast::GenericParamKind::Lifetime,
                    default: None,
                    projection_bounds: Vec::new(),
                });
                if skip_symbol(&mut probe, ",").is_err() {
                    break;
                }
                if peek_symbol(probe) == Some(">") {
                    break;
                }
                continue;
            }
            let is_const = skip_keyword(&mut probe, Keyword::Const).is_ok();
            let name = ident_like(&mut probe)?;
            let mut const_ty = None;
            let bounds = if skip_symbol(&mut probe, ":").is_ok() && !is_const {
                parse_type_bounds(&mut probe)?
            } else if is_const {
                const_ty = Some(parse_type_expr(&mut probe)?);
                fp_core::ast::TypeBounds::any()
            } else {
                fp_core::ast::TypeBounds::any()
            };
            let mut default = None;
            if skip_symbol(&mut probe, "=").is_ok() {
                if is_const {
                    // The same reasoning as path-argument parsing's
                    // const-generic case (`Foo<char, 3>`): full
                    // expression precedence keeps going past a literal
                    // hunting for a binary operator, and mistakes this
                    // list's own closing `>` for a `{ 1 } > ..`
                    // comparison (real `core::mem::transmutability`'s own
                    // `const ASSUME: Assume = { Assume::NOTHING }`).
                    // `parse_cast_no_struct` sits below every binary
                    // operator, so it naturally stops right after the
                    // default value.
                    let value = parse_cast_no_struct(&mut probe, 0)?;
                    default = Some(Box::new(Ty::Expr(Box::new(value))));
                } else {
                    default = Some(Box::new(parse_type_expr(&mut probe)?));
                }
            }
            params.push(fp_core::ast::GenericParam {
                name,
                bounds,
                kind: if let Some(ty) = const_ty {
                    fp_core::ast::GenericParamKind::Const { ty: Box::new(ty) }
                } else {
                    fp_core::ast::GenericParamKind::Type
                },
                default,
                projection_bounds: Vec::new(),
            });
            if skip_symbol(&mut probe, ",").is_err() {
                break;
            }
            // Trailing comma before the closing `>` (e.g. real `alloc`'s
            // multi-line `Box<T: ?Sized, #[..] A: Allocator = Global,>`)
            // — without this, the loop always expects one more param after
            // any comma, and chokes on `>` itself as if it were a name.
            if peek_symbol(probe) == Some(">") {
                break;
            }
        }
    }
    skip_symbol(&mut probe, ">")?;
    *input = probe;
    Ok(params)
}

fn is_path_ident(ty: &Ty, name: &str) -> bool {
    match ty {
        Ty::Expr(expr) => match expr.kind() {
            ExprKind::Name(Name { path: path, .. }) => {
                path.prefix == PathPrefix::Plain
                    && path.segments.len() == 1
                    && path.segments[0].as_str() == name
            }
            _ => false,
        },
        Ty::Wildcard(_) => name == "_",
        _ => false,
    }
}
