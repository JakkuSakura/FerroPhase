use super::*;
use fp_core::ast::ImplTraits;
use fp_core::ast::TypeNothing;
use fp_core::ast::TypeType;
use fp_core::ast::TypeWildcard;

/// A UFCS-disambiguated qualified path in *type* position (`<R::Residual
/// as Residual<Box<R::Output>>>::TryType`, real `alloc::boxed`'s own
/// `Box::try_map`) — same simplification `parse_qualified_path_expr`
/// (the expression-position sibling of this) already makes: the `as
/// Trait` disambiguator is parsed and dropped. The disambiguated type
/// is nearly always itself a plain (possibly multi-segment, possibly
/// generic) named path (`R::Residual`, `T`, ...) — when it is, this
/// appends the trailing `::segment`s directly onto that same path
/// rather than trying to model a real, separate "projection of an
/// associated type through this specific path" type. When it isn't
/// (e.g. `<[T; N] as ..>::Assoc`), there is no flat path to extend, so
/// the trailing segments are simply dropped and the base type stands in
/// for the whole thing — a rarer case, and only a type-display/
/// diagnostics degradation, not a parse failure.
fn parse_qualified_path_type(input: &mut &[Token]) -> ModalResult<Ty> {
    let mut probe = *input;
    // A nested qualified path (real `core::future::future`'s own
    // `<<P as ops::Deref>::Target as Future>::Output`) lexes its two
    // adjacent openers as one `<<` token — same ambiguity `try_eat_symbol`
    // already resolves for ordinary generic-argument nesting.
    if !try_eat_symbol(&mut probe, "<") {
        return Err(ErrMode::Backtrack(ContextError::new()));
    }
    let ty = parse_type_expr(&mut probe)?;
    if skip_keyword(&mut probe, Keyword::As).is_ok() {
        let _trait_ty = parse_type_expr(&mut probe)?;
    }
    skip_symbol(&mut probe, ">")?;
    let mut extra_segments = Vec::new();
    loop {
        let mut seg_probe = probe;
        if skip_symbol(&mut seg_probe, "::").is_err() {
            break;
        }
        let Ok(next) = ident_like(&mut seg_probe) else {
            break;
        };
        let args = parse_optional_type_args(&mut seg_probe)?;
        probe = seg_probe;
        extra_segments.push(ParameterPathSegment::new(next, args));
    }
    *input = probe;
    if extra_segments.is_empty() {
        return Ok(ty);
    }
    let Ty::Expr(expr) = &ty else {
        return Ok(ty);
    };
    let name = match expr.kind() {
        // A single-segment path (e.g. plain `I`) collapses to `Name::
        // Ident` at construction time (see `Name::path`'s own doc
        // comment) rather than staying a one-element `Name::Path` — the
        // common case for a disambiguated type in a qualified path
        // (`<I as Iterator>::Item`), so this needs its own arm rather
        // than falling through to the catch-all below.
        ExprKind::Name(Name::Ident(ident)) => Name::parameter_path(ParameterPath::new(
            PathPrefix::Plain,
            std::iter::once(ParameterPathSegment::new(ident.clone(), Vec::new()))
                .chain(extra_segments)
                .collect(),
        )),
        ExprKind::Name(Name::Path(path)) => Name::parameter_path(ParameterPath::new(
            path.prefix,
            path.segments
                .iter()
                .map(|ident| ParameterPathSegment::new(ident.clone(), Vec::new()))
                .chain(extra_segments)
                .collect(),
        )),
        ExprKind::Name(Name::ParameterPath(path)) => Name::parameter_path(ParameterPath::new(
            path.prefix,
            path.segments
                .iter()
                .cloned()
                .chain(extra_segments)
                .collect(),
        )),
        _ => return Ok(ty),
    };
    Ok(Ty::Expr(Box::new(Expr::name(name))))
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
    if skip_symbol(input, "(").is_ok() {
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
        skip_symbol(input, ")")?;
        let ret_ty = if skip_symbol(input, "->").is_ok() {
            Some(Box::new(parse_type_expr(input)?))
        } else {
            None
        };
        let _ = name;
        return Ok(Ty::Function(
            TypeFunction {
                params,
                generics_params: Vec::new(),
                ret_ty,
            }
            .into(),
        ));
    }
    if let Name::ParameterPath(parameter_path) = &name {
        if parameter_path.prefix == PathPrefix::Plain
            && parameter_path.segments.len() == 1
            && parameter_path.segments[0].ident.as_str() == "quote"
            && parameter_path.segments[0].args.len() == 1
        {
            let (kind, inner_ty) = match &parameter_path.segments[0].args[0] {
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
    let (bare_path, parameter_path) = match &name {
        Name::Path(p) => (Some(p), None),
        Name::ParameterPath(p) => (None, Some(p)),
        _ => (None, None),
    };
    // Handle `type` keyword — both bare and with type args like `type<_>`, `type<i64>`
    let type_name = match (&bare_path, &parameter_path) {
        (Some(path), _) if path.prefix == PathPrefix::Plain && path.segments.len() == 1 => {
            path.segments[0].as_str().to_string()
        }
        (_, Some(ppath)) if ppath.prefix == PathPrefix::Plain && ppath.segments.len() == 1 => {
            ppath.segments[0].ident.as_str().to_string()
        }
        (None, None) if matches!(&name, Name::Ident(ident) if ident.as_str() == "type") => {
            "type".to_string()
        }
        _ => String::new(),
    };
    if type_name == "type" {
        if let Some(ppath) = parameter_path {
            let args = &ppath.segments[0].args;
            if args.len() == 1 {
                let inner = if is_path_ident(&args[0], "_") {
                    Some(Box::new(Ty::Wildcard(TypeWildcard)))
                } else {
                    Some(Box::new(args[0].clone()))
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
    if matches!(&name, Name::Ident(ident) if ident.as_str() == "any") {
        return Ok(Ty::any());
    }
    if let Name::Ident(_) = &name {
        // `Name::path()` canonicalizes any single-segment plain path (a
        // bare `Foo`) into `Name::Ident`, not `Name::Path` — so the
        // trailing-`?` handling above (reachable only via `bare_path`,
        // i.e. `Name::Path`) never sees it. Mirror that same handling here
        // for the case `Name::path()` actually produces.
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
    if skip_symbol(input, "(").is_ok() {
        if peek_symbol(input) != Some(")") {
            loop {
                let _ = parse_type_expr(input)?;
                if skip_symbol(input, ",").is_err() {
                    break;
                }
                if peek_symbol(input) == Some(")") {
                    break;
                }
            }
        }
        skip_symbol(input, ")")?;
        if skip_symbol(input, "->").is_ok() {
            let _ = parse_type_expr(input)?;
        }
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

pub(crate) fn parse_optional_type_args(input: &mut &[Token]) -> ModalResult<Vec<Ty>> {
    let mut probe = *input;
    if !try_eat_symbol(&mut probe, "<") {
        return Ok(Vec::new());
    }
    let mut args = Vec::new();
    if peek_symbol(probe) != Some(">") {
        loop {
            // A lifetime argument (real `alloc::borrow`'s own `Cow<'a, B>`)
            // — this checker doesn't model borrow-checking, so lifetime
            // arguments are dropped here rather than falling through to
            // `parse_type_arg`, which would otherwise reparse the
            // lifetime's own ident-like token as if it were an ordinary
            // type name (producing a bogus type literally named `'a`
            // that can never resolve). Dropping it here — never
            // constructing a `Ty` for it, never pushing anything into
            // `args` — also keeps the remaining argument list's
            // positions exactly aligned with the type-arg list a
            // consuming type declares (lifetime args always precede type
            // args in real Rust source), matching how rustc's own
            // `GenericArgs` already separates the two.
            if matches!(peek_ident_like(probe), Some(name) if name.starts_with('\'')) {
                let _ = ident_like(&mut probe);
                let mut comma_probe = probe;
                if skip_symbol(&mut comma_probe, ",").is_err() {
                    break;
                }
                if peek_symbol(comma_probe) == Some(">") {
                    probe = comma_probe;
                    break;
                }
                probe = comma_probe;
                continue;
            }
            let Ok(arg) = parse_type_arg(&mut probe) else {
                return Ok(Vec::new());
            };
            args.push(arg);
            let mut comma_probe = probe;
            if skip_symbol(&mut comma_probe, ",").is_err() {
                break;
            }
            if peek_symbol(comma_probe) == Some(">") {
                probe = comma_probe;
                break;
            }
            probe = comma_probe;
        }
    }
    if skip_symbol(&mut probe, ">").is_err() {
        return Ok(Vec::new());
    }
    *input = probe;
    Ok(args)
}

fn parse_type_arg(input: &mut &[Token]) -> ModalResult<Ty> {
    let mut probe = *input;
    if let Ok(ident) = ident_like(&mut probe) {
        let mut assign_probe = probe;
        if skip_symbol(&mut assign_probe, "=").is_ok() {
            let value = parse_type_expr(&mut assign_probe)?;
            *input = assign_probe;
            return Ok(Ty::Expr(Box::new(
                ExprKind::Assign(ExprAssign {
                    span: Span::null(),
                    target: Box::new(Expr::name(Name::path(Path::plain(vec![ident])))),
                    value: Box::new(type_to_expr(&value)),
                })
                .into(),
            )));
        }
        // An associated-type *bound* generic arg (real `alloc::collections
        // ::vec_deque`'s own `IntoIterator<Item = T, IntoIter:
        // DoubleEndedIterator>`) — as opposed to the `Ident = Type` binding
        // above, this constrains the associated type without naming it
        // concretely. This checker has no separate slot for it, so it's
        // parsed and dropped, same treatment already given to any other
        // bound this checker doesn't act on further. The associated type
        // itself can carry its own generic/lifetime args before the `:`
        // (real `core::str::pattern`'s own `Pattern<Searcher<'a>: fmt::
        // Debug>>` — bounding `Searcher<'a>` specifically, not a bare
        // `Searcher`) — `parse_optional_type_args` already knows how to
        // skip a lifetime-only argument list like this.
        let mut bound_probe = probe;
        let _ = parse_optional_type_args(&mut bound_probe)?;
        if skip_symbol(&mut bound_probe, ":").is_ok() && parse_type_bounds(&mut bound_probe).is_ok() {
            *input = bound_probe;
            return Ok(Ty::Expr(Box::new(Expr::name(Name::path(Path::plain(vec![
                ident,
            ]))))));
        }
    }
    // A const-generic argument's own *value* (real `core::array`'s own
    // `IntoIter<char, 3>`, a plain integer, as opposed to `N` naming a
    // const-generic *parameter*, which the ordinary `parse_type_expr`
    // fallback below already handles as a ident-shaped type path) needs
    // `parse_cast_no_struct`, not the full `parse_type_expr` → ... →
    // `parse_simple_type`'s own const-literal branch: that branch parses
    // via `parse_expr_winnow_no_struct` at full expression precedence,
    // which happily continues past the literal looking for a binary
    // operator — and mistakes the generic-argument list's own closing
    // `>` for the start of a `3 > ..` comparison, consuming it and
    // leaving the parser looking for a right-hand side at whatever
    // follows (usually `)`/`;`), which then fails far downstream with a
    // confusing "expected expression" error with no trace back to this
    // `<...>` list at all. `parse_cast_no_struct` sits below every
    // binary operator in the precedence chain, so it naturally stops
    // right after the literal.
    // A braced const-generic argument (real `alloc::vec::mod`'s own
    // `TransmuteFrom<&'a MaybeUninit<From>, { Assume::SAFETY }>`) is a
    // block expression, not a bare literal — same value-position as the
    // literal case just below, just wrapped in `{ .. }`.
    if input
        .first()
        .is_some_and(|token| token.kind == TokenKind::Number || token.kind == TokenKind::StringLiteral)
        || matches!(peek_ident_like(*input), Some("true" | "false" | "null"))
        || peek_symbol(*input) == Some("{")
    {
        let expr = parse_cast_no_struct(input, 0)?;
        return Ok(Ty::Expr(Box::new(expr)));
    }
    parse_type_expr(input)
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

// Nightly `[const] Trait` bound modifier (e.g. `impl [const] FnOnce(T) -> U`,
// `T: [const] Destruct`) — FerroPhase doesn't model const-trait-ness, so the
// modifier is accepted and dropped, leaving the plain trait bound.
fn skip_const_trait_modifier(input: &mut &[Token]) {
    let mut probe = *input;
    if skip_symbol(&mut probe, "[").is_ok()
        && skip_keyword(&mut probe, Keyword::Const).is_ok()
        && skip_symbol(&mut probe, "]").is_ok()
    {
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
            // A lifetime parameter (`<'a, T>`, `<'a: 'b, T>`) — this
            // checker doesn't model borrow-checking, so it's dropped
            // rather than fed through the type-parameter pipeline (a
            // lifetime is not a type parameter with its own `DefId`/`Ty`,
            // even though real Rust's own `<'a, T>` list mixes them
            // syntactically). Its own `: 'b + 'c` lifetime-bound list (if
            // any) is dropped along with it.
            if matches!(peek_ident_like(probe), Some(name) if name.starts_with('\'')) {
                let _ = ident_like(&mut probe)?;
                if skip_symbol(&mut probe, ":").is_ok() {
                    loop {
                        let _ = ident_like(&mut probe)?;
                        if skip_symbol(&mut probe, "+").is_err() {
                            break;
                        }
                    }
                }
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
            let bounds = if skip_symbol(&mut probe, ":").is_ok() && !is_const {
                parse_type_bounds(&mut probe)?
            } else if is_const {
                let _ = parse_type_expr(&mut probe)?;
                fp_core::ast::TypeBounds::any()
            } else {
                fp_core::ast::TypeBounds::any()
            };
            if skip_symbol(&mut probe, "=").is_ok() {
                if is_const {
                    // Same reasoning as `parse_type_arg`'s own
                    // const-generic-argument case (`Foo<char, 3>`): full
                    // expression precedence keeps going past a literal
                    // hunting for a binary operator, and mistakes this
                    // list's own closing `>` for a `{ 1 } > ..`
                    // comparison (real `core::mem::transmutability`'s own
                    // `const ASSUME: Assume = { Assume::NOTHING }`).
                    // `parse_cast_no_struct` sits below every binary
                    // operator, so it naturally stops right after the
                    // default value.
                    let _ = parse_cast_no_struct(&mut probe, 0)?;
                } else {
                    let _ = parse_type_expr(&mut probe)?;
                }
            }
            params.push(fp_core::ast::GenericParam { name, bounds });
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
            ExprKind::Name(Name::Path(path)) => {
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
