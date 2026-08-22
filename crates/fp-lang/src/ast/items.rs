use super::*;

pub(crate) fn parse_items_tokens(
    tokens: &[Token],
    file: FileId,
) -> Result<Vec<Item>, DirectParseError> {
    let mut input = tokens;
    let mut items = Vec::new();
    while !input.is_empty() {
        if looks_like_extern_block(input) {
            let parsed =
                parse_extern_block_items(&mut input, file).map_err(|err| map_err(err, input))?;
            items.extend(parsed);
            continue;
        }
        if starts_unsafe_extern_block(input) {
            let parsed = parse_prefixed_unsafe_extern_block_items(&mut input, file)
                .map_err(|err| map_err(err, input))?;
            items.extend(parsed);
            continue;
        }
        let item =
            parse_item_or_expr_winnow(&mut input, file).map_err(|err| map_err(err, input))?;
        items.push(item);
    }
    Ok(items)
}

pub(crate) fn parse_file_tokens(
    tokens: &[Token],
    file: FileId,
) -> Result<(Vec<Attribute>, Vec<Item>), DirectParseError> {
    let mut input = tokens;
    let attrs = parse_inner_attrs(&mut input, file).map_err(|err| map_err(err, input))?;
    let mut items = Vec::new();
    while !input.is_empty() {
        if looks_like_extern_block(input) {
            let parsed =
                parse_extern_block_items(&mut input, file).map_err(|err| map_err(err, input))?;
            items.extend(parsed);
            continue;
        }
        if starts_unsafe_extern_block(input) {
            let parsed = parse_prefixed_unsafe_extern_block_items(&mut input, file)
                .map_err(|err| map_err(err, input))?;
            items.extend(parsed);
            continue;
        }
        let item = parse_item_winnow(&mut input, file).map_err(|err| map_err(err, input))?;
        items.push(item);
    }
    Ok((attrs, items))
}

/// Parse top-level content into a `ScriptBlock` — the same ordered
/// item/let/defer/expr dispatch `parse_block_expr` uses for function/block
/// bodies (via the shared `parse_block_stmt_entry`), applied at file scope
/// instead of `parse_file_tokens`'s item-or-bare-expr-only dispatch. This is
/// what lets a top-level `let`/`defer` parse at all, and gives callers like
/// `FerroFrontend::parse_script` an ordered, `File`-free representation.
pub(crate) fn parse_script_tokens(
    tokens: &[Token],
    file: FileId,
) -> Result<(Vec<Attribute>, ScriptBlock), DirectParseError> {
    let mut input = tokens;
    let attrs = parse_inner_attrs(&mut input, file).map_err(|err| map_err(err, input))?;
    let mut stmts = Vec::new();
    while !input.is_empty() {
        if looks_like_extern_block(input) {
            let parsed =
                parse_extern_block_items(&mut input, file).map_err(|err| map_err(err, input))?;
            stmts.extend(
                parsed
                    .into_iter()
                    .map(|item| BlockStmt::Item(Box::new(item))),
            );
            continue;
        }
        if starts_unsafe_extern_block(input) {
            let parsed = parse_prefixed_unsafe_extern_block_items(&mut input, file)
                .map_err(|err| map_err(err, input))?;
            stmts.extend(
                parsed
                    .into_iter()
                    .map(|item| BlockStmt::Item(Box::new(item))),
            );
            continue;
        }
        let stmt = parse_block_stmt_entry(&mut input, file).map_err(|err| map_err(err, input))?;
        stmts.push(stmt);
    }
    Ok((
        attrs,
        ScriptBlock {
            span: Span::null(),
            stmts,
        },
    ))
}

fn parse_item_or_expr_winnow(input: &mut &[Token], file: FileId) -> ModalResult<Item> {
    let mut probe = *input;
    if let Ok(item) = parse_item_winnow(&mut probe, file) {
        *input = probe;
        return Ok(item);
    }

    let expr = parse_expr_winnow(input, file)?;
    let _ = expect_symbol(input, ";");
    Ok(Item::from(ItemKind::Expr(expr)))
}

pub(crate) fn parse_item_winnow(input: &mut &[Token], file: FileId) -> ModalResult<Item> {
    let attrs = parse_outer_attrs(input, file)?;
    let visibility = parse_visibility(input)?;
    match input.first().map(|token| &token.kind) {
        Some(TokenKind::Keyword(Keyword::Use)) => parse_use_item(input, visibility, attrs),
        Some(TokenKind::Keyword(Keyword::Extern)) => {
            parse_extern_item(input, file, visibility, attrs)
        }
        Some(TokenKind::Keyword(Keyword::Const)) if starts_const_fn(*input) => {
            parse_fn_item(input, file, visibility, attrs, false)
        }
        Some(TokenKind::Keyword(Keyword::Const)) if starts_const_struct(*input) => {
            parse_const_struct_item(input, visibility, attrs)
        }
        Some(TokenKind::Keyword(Keyword::Const)) if starts_const_impl(*input) => {
            parse_impl_item(input, file, attrs)
        }
        Some(TokenKind::Keyword(Keyword::Unsafe)) if starts_unsafe_fn(*input) => {
            parse_fn_item(input, file, visibility, attrs, false)
        }
        Some(TokenKind::Keyword(Keyword::Unsafe)) if starts_unsafe_impl(*input) => {
            parse_impl_item(input, file, attrs)
        }
        // `unsafe trait Foo { .. }`/`const unsafe trait Foo { .. }` (the
        // latter unstable, but present in real `core::alloc::Allocator`)
        // marks a trait as having safety/const obligations its
        // implementors must uphold manually — neither marker carries any
        // meaning for typechecking/codegen here, so just consume and drop
        // whatever run of them precedes `trait` before delegating to the
        // ordinary trait-item parser.
        Some(TokenKind::Keyword(Keyword::Unsafe | Keyword::Const))
            if skips_modifiers_to_trait(*input) =>
        {
            parse_trait_item(input, file, visibility, attrs)
        }
        // `pub auto trait Unpin {}` — a bare `auto trait` with no leading
        // `unsafe`/`const` modifier at all (real `core::marker`'s own
        // marker trait declarations). `auto` isn't a lexer keyword, so
        // this needs its own dispatch arm distinct from the
        // Unsafe/Const-modifier-run one above.
        Some(TokenKind::Ident) if skips_modifiers_to_trait(*input) => {
            parse_trait_item(input, file, visibility, attrs)
        }
        // `impl(restriction) trait Foo { .. }` — a sealed/restricted-impl
        // trait marker (real `core::convert::num`'s own `pub impl(self)
        // trait FloatToInt<Int>: Sized { .. }`) — distinguished from a
        // genuine `impl Type`/`impl<T> Trait for Type` block by the `(`
        // immediately after `impl` (a real impl block's self-type is
        // never itself parenthesized at that exact position). Neither
        // the restriction's target nor its very existence changes
        // anything this checker models about the trait, so drop it.
        Some(TokenKind::Keyword(Keyword::Impl)) if starts_restricted_trait(*input) => {
            *input = &input[1..]; // `impl`
            skip_balanced_delimiters(input, "(", ")")?;
            parse_trait_item(input, file, visibility, attrs)
        }
        Some(TokenKind::Keyword(Keyword::Async)) if starts_async_fn(*input) => {
            parse_fn_item(input, file, visibility, attrs, false)
        }
        Some(TokenKind::Keyword(Keyword::Const)) => {
            parse_const_item(input, file, visibility, attrs)
        }
        Some(TokenKind::Keyword(Keyword::Static)) => {
            parse_static_item(input, file, visibility, attrs)
        }
        Some(TokenKind::Keyword(Keyword::Type)) => parse_type_alias_item(input, visibility, attrs),
        Some(TokenKind::Keyword(Keyword::Struct)) => parse_struct_item(input, visibility, attrs),
        Some(TokenKind::Keyword(Keyword::Union)) => parse_union_item(input, visibility, attrs),
        Some(TokenKind::Keyword(Keyword::Enum)) => parse_enum_item(input, file, visibility, attrs),
        Some(TokenKind::Keyword(Keyword::Mod)) => parse_mod_item(input, file, visibility, attrs),
        Some(TokenKind::Keyword(Keyword::Opaque)) => {
            parse_opaque_type_item(input, visibility, attrs)
        }
        Some(TokenKind::Keyword(Keyword::Trait)) => {
            parse_trait_item(input, file, visibility, attrs)
        }
        Some(TokenKind::Keyword(Keyword::Impl)) => parse_impl_item(input, file, attrs),
        Some(TokenKind::Keyword(Keyword::Fn)) => {
            parse_fn_item(input, file, visibility, attrs, false)
        }
        Some(TokenKind::Keyword(Keyword::Quote)) => {
            parse_fn_item(input, file, visibility, attrs, true)
        }
        Some(TokenKind::Ident) if starts_macro_2_def(*input) => parse_macro_2_def(input, attrs),
        Some(TokenKind::Ident) | Some(TokenKind::Keyword(_)) if looks_like_item_macro(*input) => {
            parse_item_macro(input, attrs)
        }
        // `splice expr(..);` is valid at file scope too (e.g. calling a
        // `quote fn`/`quote<item>`-returning function to inject items) —
        // it just isn't a *declaration* keyword like the arms above, so it
        // needs its own case rather than falling through to the
        // expression-statement catch-all `parse_file_tokens` doesn't have
        // (unlike `parse_item_or_expr_winnow`, used inside quoted blocks
        // and scripts, which already falls back to expression parsing for
        // any non-item shape).
        Some(TokenKind::Keyword(Keyword::Splice)) => {
            let expr = parse_expr_winnow(input, file)?;
            let _ = expect_symbol(input, ";");
            Ok(Item::from(ItemKind::Expr(expr)))
        }
        _ => Err(ErrMode::Backtrack(ContextError::new())),
    }
}

fn parse_const_item(
    input: &mut &[Token],
    file: FileId,
    visibility: Visibility,
    attrs: Vec<Attribute>,
) -> ModalResult<Item> {
    skip_keyword(input, Keyword::Const)?;
    let mutable = skip_keyword(input, Keyword::Mut).is_ok();
    let name = ident_like(input)?;
    let ty = if skip_symbol(input, ":").is_ok() {
        Some(parse_type_expr(input)?)
    } else {
        None
    };
    skip_symbol(input, "=")?;
    let value = parse_expr_winnow(input, file)?;
    skip_symbol(input, ";")?;
    Ok(Item::from(ItemKind::DefConst(ItemDefConst {
        attrs,
        mutable: mutable.then_some(true),
        ty_annotation: None,
        visibility,
        name,
        ty,
        value: Box::new(value),
    })))
}

fn parse_static_item(
    input: &mut &[Token],
    file: FileId,
    visibility: Visibility,
    attrs: Vec<Attribute>,
) -> ModalResult<Item> {
    skip_keyword(input, Keyword::Static)?;
    let _mutable = skip_keyword(input, Keyword::Mut).is_ok();
    let name = ident_like(input)?;
    skip_symbol(input, ":")?;
    let ty = parse_type_expr(input)?;
    skip_symbol(input, "=")?;
    let value = parse_expr_winnow(input, file)?;
    skip_symbol(input, ";")?;
    Ok(Item::from(ItemKind::DefStatic(ItemDefStatic {
        attrs,
        ty_annotation: None,
        visibility,
        name,
        ty,
        value: Box::new(value),
    })))
}

fn parse_type_alias_item(
    input: &mut &[Token],
    visibility: Visibility,
    attrs: Vec<Attribute>,
) -> ModalResult<Item> {
    skip_keyword(input, Keyword::Type)?;
    let name = ident_like(input)?;
    let mut generics_params = parse_optional_generic_params(input)?;
    if skip_keyword(input, Keyword::Where).is_ok() {
        parse_where_clause_and_merge(input, &mut generics_params)?;
    }
    skip_symbol(input, "=")?;
    let value = parse_type_expr(input)?;
    // A type alias's `where` clause can also trail the `= value` instead
    // of (or in addition to real Rust's still-nightly-only "where clause
    // before the equals" position) — real `alloc::boxed`'s own `type
    // CallRefFuture<'a> = F::CallRefFuture<'a> where Self: 'a;`.
    if skip_keyword(input, Keyword::Where).is_ok() {
        parse_where_clause_and_merge(input, &mut generics_params)?;
    }
    skip_symbol(input, ";")?;
    Ok(Item::from(ItemKind::DefType(ItemDefType {
        attrs,
        visibility,
        name,
        generics_params,
        value,
    })))
}

fn parse_struct_item(
    input: &mut &[Token],
    visibility: Visibility,
    attrs: Vec<Attribute>,
) -> ModalResult<Item> {
    skip_keyword(input, Keyword::Struct)?;
    let name = ident_like(input)?;
    let mut generics_params = parse_optional_generic_params(input)?;
    if skip_keyword(input, Keyword::Where).is_ok() {
        parse_where_clause_and_merge(input, &mut generics_params)?;
    }
    let mut fields = Vec::new();
    if skip_symbol(input, ";").is_ok() {
        return Ok(Item::from(ItemKind::DefStruct(ItemDefStruct {
            attrs,
            visibility,
            name: name.clone(),
            value: TypeStruct {
                name,
                generics_params,
                repr: ReprOptions::default(),
                fields,
            },
        })));
    }
    if skip_symbol(input, "(").is_ok() {
        let mut index = 0usize;
        while peek_symbol(input) != Some(")") {
            skip_outer_attrs_for_field(input)?;
            let _field_visibility = parse_visibility(input)?;
            let value = parse_type_expr(input)?;
            fields.push(StructuralField::new(Ident::new(index.to_string()), value));
            index += 1;
            if skip_symbol(input, ",").is_err() {
                break;
            }
        }
        skip_symbol(input, ")")?;
        // A tuple struct's `where` clause trails its field list, not its
        // generic params (real `core::str::pattern`'s own `pub struct
        // CharPredicateSearcher<'a, F>(<MultiCharEqPattern<F> as
        // Pattern>::Searcher<'a>) where F: FnMut(char) -> bool;`) — the
        // `where`-before-generics check above only covers a brace-bodied
        // struct's placement.
        if skip_keyword(input, Keyword::Where).is_ok() {
            parse_where_clause_and_merge(input, &mut generics_params)?;
        }
        skip_symbol(input, ";")?;
        return Ok(Item::from(ItemKind::DefStruct(ItemDefStruct {
            attrs,
            visibility,
            name: name.clone(),
            value: TypeStruct {
                name,
                generics_params,
                repr: ReprOptions::default(),
                fields,
            },
        })));
    }
    skip_symbol(input, "{")?;
    while peek_symbol(input) != Some("}") {
        skip_outer_attrs_for_field(input)?;
        let _field_visibility = parse_visibility(input)?;
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
    }
    skip_symbol(input, "}")?;
    Ok(Item::from(ItemKind::DefStruct(ItemDefStruct {
        attrs,
        visibility,
        name: name.clone(),
        value: TypeStruct {
            name,
            generics_params,
            repr: ReprOptions::default(),
            fields,
        },
    })))
}

/// `union Name { fields }` — this checker models no distinction between a
/// tagged union's "only one field active at a time" storage layout and an
/// ordinary struct's "all fields present" layout (there's no notion of
/// reading an inactive field being unsound, since nothing here ever reads
/// through raw union field access outside `unsafe` blocks this checker
/// already treats as opaque), so a union is parsed exactly like a
/// brace-bodied struct and represented as one.
fn parse_union_item(
    input: &mut &[Token],
    visibility: Visibility,
    attrs: Vec<Attribute>,
) -> ModalResult<Item> {
    skip_keyword(input, Keyword::Union)?;
    let name = ident_like(input)?;
    let mut generics_params = parse_optional_generic_params(input)?;
    if skip_keyword(input, Keyword::Where).is_ok() {
        parse_where_clause_and_merge(input, &mut generics_params)?;
    }
    let mut fields = Vec::new();
    skip_symbol(input, "{")?;
    while peek_symbol(input) != Some("}") {
        skip_outer_attrs_for_field(input)?;
        let _field_visibility = parse_visibility(input)?;
        let field_name = ident_like(input)?;
        skip_symbol(input, ":")?;
        let value = parse_type_expr(input)?;
        fields.push(StructuralField::new(field_name, value));
        if skip_symbol(input, ",").is_err() {
            break;
        }
    }
    skip_symbol(input, "}")?;
    Ok(Item::from(ItemKind::DefStruct(ItemDefStruct {
        attrs,
        visibility,
        name: name.clone(),
        value: TypeStruct {
            name,
            generics_params,
            repr: ReprOptions::default(),
            fields,
        },
    })))
}

fn parse_const_struct_item(
    input: &mut &[Token],
    visibility: Visibility,
    mut attrs: Vec<Attribute>,
) -> ModalResult<Item> {
    skip_keyword(input, Keyword::Const)?;
    attrs.push(const_struct_attr());
    parse_struct_item(input, visibility, attrs)
}

fn parse_fn_item(
    input: &mut &[Token],
    file: FileId,
    visibility: Visibility,
    attrs: Vec<Attribute>,
    quoted: bool,
) -> ModalResult<Item> {
    parse_fn_item_core(input, file, visibility, attrs, quoted)
}

fn parse_function_block(input: &mut &[Token], file: FileId) -> ModalResult<ExprBlock> {
    parse_block(input, file)
}

fn parse_fn_item_core(
    input: &mut &[Token],
    file: FileId,
    visibility: Visibility,
    attrs: Vec<Attribute>,
    quoted: bool,
) -> ModalResult<Item> {
    // Real Rust allows `unsafe`/`async`/`const` in any relative order before
    // `fn` (e.g. `const unsafe fn`, not just this parser's original assumed
    // `unsafe async const fn`) — keep consuming whichever modifier keyword
    // comes next until none match, rather than checking each only once in
    // a fixed sequence.
    let mut is_async = false;
    let mut is_const = false;
    loop {
        if skip_keyword(input, Keyword::Unsafe).is_ok() {
            continue;
        }
        if skip_keyword(input, Keyword::Async).is_ok() {
            is_async = true;
            continue;
        }
        if skip_keyword(input, Keyword::Const).is_ok() {
            is_const = true;
            continue;
        }
        break;
    }
    // `extern "ABI" fn ..` — an impl-block method (or free function) can
    // name its own calling convention, same as a trait method
    // declaration can (real `core::ops::function`'s own blanket `impl<A,
    // F: Fn<A>> FnMut<A> for &F { extern "rust-call" fn call(..) { .. }
    // }`).
    let abi = if peek_keyword(*input, Keyword::Extern) {
        parse_extern_abi(input)?
    } else {
        fp_core::ast::Abi::Rust
    };
    if quoted {
        skip_keyword(input, Keyword::Quote)?;
    }
    skip_keyword(input, Keyword::Fn)?;
    let name = ident_like(input)?;
    let mut generics_params = parse_optional_generic_params(input)?;
    skip_symbol(input, "(")?;
    let mut destructures = Vec::new();
    let (receiver, params) = parse_fn_params_with_receiver(input, &mut destructures)?;
    skip_symbol(input, ")")?;
    let ret_ty = if skip_symbol(input, "->").is_ok() {
        Some(parse_type_expr(input)?)
    } else {
        None
    };
    if skip_keyword(input, Keyword::Where).is_ok() {
        parse_where_clause_and_merge(input, &mut generics_params)?;
    }
    // A bodiless free-function/impl-method item statement ending in `;`
    // instead of `{ .. }` — real `alloc::intrinsics`' own `#[rustc_intrinsic]
    // pub fn write_box_via_move<T>(..) -> Box<MaybeUninit<T>>;` declares a
    // compiler-builtin intrinsic with no Rust-level body at all, the same
    // shape a trait method declaration or an extern fn already allows.
    if !quoted && skip_symbol(input, ";").is_ok() {
        let sig = FunctionSignature {
            name: Some(name.clone()),
            receiver,
            params,
            generics_params,
            is_const,
            abi,
            quote_kind: None,
            ret_ty,
        };
        return Ok(Item::from(ItemKind::DeclFunction(ItemDeclFunction {
            attrs,
            ty_annotation: None,
            name,
            sig,
        })));
    }
    // `quote fn f(..) -> item { <items> }` is sugar for
    // `const fn f(..) -> item { quote<item> { <items> } }` — the whole
    // body is implicitly quoted, so it must be parsed the same
    // token-balanced way `quote<item> { .. }`'s contents are (raw item
    // syntax, not ordinary expression syntax; struct/enum/etc. items
    // aren't valid expressions on their own).
    let mut body = if quoted {
        let quote_block = parse_balanced_quote_block(input, file)?;
        let quote_expr = Expr::from(ExprKind::Quote(fp_core::ast::ExprQuote {
            span: quote_block.span,
            collected_items: Vec::new(),
            block: quote_block,
            kind: Some(QuoteFragmentKind::Item),
        }));
        ExprBlock::new_expr(quote_expr)
    } else {
        parse_function_block(input, file)?
    };
    if !destructures.is_empty() {
        // Real bindings for every name in a multi-element tuple-pattern
        // parameter (see `parse_fn_param_name`) — prepended so they're in
        // scope for the rest of the body exactly as they would be for a
        // real Rust parameter pattern.
        let mut stmts = destructures;
        stmts.extend(body.stmts);
        body.stmts = stmts;
    }
    let mut sig = FunctionSignature {
        name: Some(name.clone()),
        receiver,
        params,
        generics_params,
        is_const,
        abi,
        quote_kind: quoted.then_some(QuoteFragmentKind::Item),
        ret_ty,
    };
    if quoted {
        sig.is_const = true;
        sig.ret_ty = Some(Ty::Quote(fp_core::ast::TypeQuote {
            span: Span::null(),
            kind: QuoteFragmentKind::Item,
            item: None,
            inner: None,
        }));
    }
    Ok(Item::from(ItemKind::DefFunction(ItemDefFunction {
        ty_annotation: None,
        attrs,
        name,
        collected_items: Vec::new(),
        ty: None,
        sig,
        body,
        is_async,
        visibility,
    })))
}

/// Drop any run of `unsafe`/`const`/`auto` immediately preceding
/// `trait` — real `core::num::nonzero`'s own `pub impl(self) unsafe
/// trait ZeroablePrimitive: ..` stacks `impl(self)` (stripped by its own
/// dispatch arm before this runs) and `unsafe`; `core::marker`'s `pub
/// unsafe auto trait Send {}` stacks `unsafe` and `auto` (`auto` isn't a
/// lexer keyword, so it tokenizes as a plain `Ident`). None of these
/// modifiers carry meaning this checker models for a trait declaration.
fn skip_trait_modifiers(input: &mut &[Token]) {
    loop {
        match input.first() {
            Some(token) if token.kind == TokenKind::Keyword(Keyword::Unsafe) => {
                *input = &input[1..]
            }
            Some(token) if token.kind == TokenKind::Keyword(Keyword::Const) => {
                *input = &input[1..]
            }
            Some(token) if token.kind == TokenKind::Ident && token.lexeme == "auto" => {
                *input = &input[1..]
            }
            _ => return,
        }
    }
}

fn parse_trait_item(
    input: &mut &[Token],
    file: FileId,
    visibility: Visibility,
    attrs: Vec<Attribute>,
) -> ModalResult<Item> {
    skip_trait_modifiers(input);
    skip_keyword(input, Keyword::Trait)?;
    let name = ident_like(input)?;
    let mut generics_params = parse_optional_generic_params(input)?;
    // A trait *alias* (`trait Thin = Pointee<Metadata = ()> +
    // PointeeSized;`, real core::ptr::metadata's own) — nightly-only,
    // names a `+`-joined bound combination for reuse elsewhere as `T:
    // Thin`. This checker has no notion of transparently substituting an
    // alias's real bound list wherever the alias name is used as a bound
    // (same category of thing already dropped for pattern-type validity/
    // lifetimes) — it's parsed and modeled as an ordinary empty trait
    // declaration with the alias's bounds attached as supertraits, which
    // is enough for the alias name itself to resolve to a real trait
    // `DefId` wherever it's later used as a bound, just without actually
    // requiring an implementor to satisfy the aliased traits.
    if skip_symbol(input, "=").is_ok() {
        let bounds = parse_type_bounds(input)?;
        skip_symbol(input, ";")?;
        return Ok(Item::from(ItemKind::DefTrait(ItemDefTrait {
            attrs,
            name,
            generics_params,
            bounds,
            collected_items: Vec::new(),
            items: Vec::new(),
            visibility,
        })));
    }
    let bounds = if skip_symbol(input, ":").is_ok() {
        parse_type_bounds(input)?
    } else {
        TypeBounds::any()
    };
    if skip_keyword(input, Keyword::Where).is_ok() {
        parse_where_clause_and_merge(input, &mut generics_params)?;
    }
    skip_symbol(input, "{")?;
    let mut items = Vec::new();
    while peek_symbol(input) != Some("}") {
        items.push(parse_trait_member(input, file)?);
    }
    skip_symbol(input, "}")?;
    Ok(Item::from(ItemKind::DefTrait(ItemDefTrait {
        attrs,
        name,
        generics_params,
        bounds,
        collected_items: Vec::new(),
        items,
        visibility,
    })))
}

fn parse_trait_member(input: &mut &[Token], file: FileId) -> ModalResult<Item> {
    let attrs = parse_outer_attrs(input, file)?;
    // `final fn share(&self) -> Self { .. }` (nightly non-overridable
    // trait method default) — real `core::clone`'s own `Clone::share`.
    // `final` isn't a lexer keyword, and carries no meaning this checker
    // models (no notion of an impl overriding a default method body being
    // disallowed), so it's dropped like `default` already is elsewhere.
    if matches!(
        input.first(),
        Some(token) if token.kind == TokenKind::Ident && token.lexeme == "final"
    ) {
        *input = &input[1..];
    }
    if skip_keyword(input, Keyword::Const).is_ok() {
        let name = ident_like(input)?;
        skip_symbol(input, ":")?;
        let ty = parse_type_expr(input)?;
        // A trait-associated const may carry a default value (real
        // `core::field`'s own `const OFFSET: usize =
        // crate::intrinsics::field_offset::<Self>();`), not just the bare
        // `const NAME: TYPE;` declaration — same shape a trait method
        // already gets to choose between (declaration vs. default body).
        if skip_symbol(input, "=").is_ok() {
            let value = parse_expr_winnow(input, file)?;
            skip_symbol(input, ";")?;
            return Ok(Item::from(ItemKind::DefConst(ItemDefConst {
                attrs,
                mutable: None,
                ty_annotation: None,
                visibility: Visibility::Inherited,
                name,
                ty: Some(ty),
                value: Box::new(value),
            })));
        }
        skip_symbol(input, ";")?;
        return Ok(Item::from(ItemKind::DeclConst(ItemDeclConst {
            ty_annotation: None,
            name,
            ty,
        })));
    }
    if skip_keyword(input, Keyword::Type).is_ok() {
        let name = ident_like(input)?;
        let _generics_params = parse_optional_generic_params(input)?;
        // An associated type declaration may carry its own bound (real
        // `alloc::borrow`'s own `type Owned: Borrow<Self>;`) and/or a
        // default (`type Owned: Borrow<Self> = Self;`) — neither was
        // previously recognized, only the bare `type Name;` shape.
        let bounds = if skip_symbol(input, ":").is_ok() {
            parse_type_bounds(input)?
        } else {
            TypeBounds::any()
        };
        if skip_symbol(input, "=").is_ok() {
            let _default = parse_type_expr(input)?;
        }
        // An associated type declaration's own `where` clause (real
        // `core::ops::async_function`'s own `type CallRefFuture<'a>:
        // Future<Output = Self::Output> where Self: 'a;`) — carries no
        // meaning this checker models, same treatment as everywhere else
        // a where clause is dropped.
        if skip_keyword(input, Keyword::Where).is_ok() {
            skip_where_clause(input)?;
        }
        skip_symbol(input, ";")?;
        return Ok(Item::from(ItemKind::DeclType(ItemDeclType {
            ty_annotation: None,
            name,
            bounds,
        })));
    }
    let visibility = Visibility::Inherited;
    if peek_keyword(*input, Keyword::Fn)
        || peek_keyword(*input, Keyword::Quote)
        || peek_keyword(*input, Keyword::Extern)
        || starts_async_fn(*input)
        || skips_modifiers_to_fn(*input)
    {
        return parse_trait_fn_member(input, file, visibility, attrs);
    }
    Err(ErrMode::Backtrack(ContextError::new()))
}

fn parse_trait_fn_member(
    input: &mut &[Token],
    file: FileId,
    visibility: Visibility,
    attrs: Vec<Attribute>,
) -> ModalResult<Item> {
    // `unsafe fn`/`const fn`/`const unsafe fn` (any order/count) — a
    // trait method declaration can carry the same safety/const modifiers
    // a real definition can, e.g. real `core::array`'s `unsafe fn
    // partial_drop(&mut self, ..)`. Neither carries meaning this checker
    // models for a bodiless trait *declaration*, so just drop them.
    while matches!(
        input.first().map(|token| &token.kind),
        Some(TokenKind::Keyword(Keyword::Unsafe | Keyword::Const))
    ) {
        *input = &input[1..];
    }
    // `extern "ABI" fn ..;` — a trait method declaration can name its own
    // calling convention (real `core::ops::function`'s own `extern
    // "rust-call" fn call(&self, args: Args) -> Self::Output;`), same as
    // a real function item already can.
    let abi = if peek_keyword(*input, Keyword::Extern) {
        parse_extern_abi(input)?
    } else {
        fp_core::ast::Abi::Rust
    };
    let is_async = skip_keyword(input, Keyword::Async).is_ok();
    let quoted = skip_keyword(input, Keyword::Quote).is_ok();
    if quoted {
        skip_keyword(input, Keyword::Fn)?;
    } else {
        skip_keyword(input, Keyword::Fn)?;
    }
    let name = ident_like(input)?;
    let mut generics_params = parse_optional_generic_params(input)?;
    skip_symbol(input, "(")?;
    let mut destructures = Vec::new();
    let (receiver, params) = parse_fn_params_with_receiver(input, &mut destructures)?;
    skip_symbol(input, ")")?;
    let ret_ty = if skip_symbol(input, "->").is_ok() {
        Some(parse_type_expr(input)?)
    } else {
        None
    };
    if skip_keyword(input, Keyword::Where).is_ok() {
        parse_where_clause_and_merge(input, &mut generics_params)?;
    }
    let mut sig = FunctionSignature {
        name: Some(name.clone()),
        receiver,
        params,
        generics_params,
        is_const: false,
        abi,
        quote_kind: quoted.then_some(QuoteFragmentKind::Item),
        ret_ty,
    };
    if quoted {
        sig.is_const = true;
        sig.ret_ty = Some(Ty::Quote(fp_core::ast::TypeQuote {
            span: Span::null(),
            kind: QuoteFragmentKind::Item,
            item: None,
            inner: None,
        }));
    }
    if skip_symbol(input, ";").is_ok() {
        return Ok(Item::from(ItemKind::DeclFunction(ItemDeclFunction {
            attrs,
            ty_annotation: None,
            name,
            sig,
        })));
    }
    let mut body = parse_function_block(input, file)?;
    if !destructures.is_empty() {
        let mut stmts = destructures;
        stmts.extend(body.stmts);
        body.stmts = stmts;
    }
    Ok(Item::from(ItemKind::DefFunction(ItemDefFunction {
        ty_annotation: None,
        attrs,
        name,
        collected_items: Vec::new(),
        ty: None,
        sig,
        body,
        is_async,
        visibility,
    })))
}

fn peek_keyword(input: &[Token], keyword: Keyword) -> bool {
    matches!(input.first(), Some(token) if token.kind == TokenKind::Keyword(keyword))
}

fn parse_impl_item(input: &mut &[Token], file: FileId, attrs: Vec<Attribute>) -> ModalResult<Item> {
    // `const impl<T> Trait for X` (const trait impls) — the `const`
    // modifier has no effect we need to model, same treatment as `unsafe`
    // below.
    let _is_const = skip_keyword(input, Keyword::Const).is_ok();
    let _is_unsafe = skip_keyword(input, Keyword::Unsafe).is_ok();
    skip_keyword(input, Keyword::Impl)?;
    let mut generics_params = parse_optional_generic_params(input)?;
    let first_ty = parse_type_expr(input)?;
    let (trait_ty, self_ty) = if skip_keyword(input, Keyword::For).is_ok() {
        let self_ty = parse_type_expr(input)?;
        (type_to_name(&first_ty), type_to_expr(&self_ty))
    } else {
        (None, type_to_expr(&first_ty))
    };
    if skip_keyword(input, Keyword::Where).is_ok() {
        parse_where_clause_and_merge(input, &mut generics_params)?;
    }
    skip_symbol(input, "{")?;
    let mut items = Vec::new();
    while peek_symbol(input) != Some("}") {
        let member_attrs = parse_outer_attrs(input, file)?;
        let visibility = parse_visibility(input)?;
        // `default fn`/`default const`/`default type` (specialization) —
        // `default` isn't a reserved keyword in this lexer (tokenizes as a
        // plain `Ident`), and carries no meaning this checker models (no
        // notion of specialization to apply), so just drop it.
        if matches!(
            input.first(),
            Some(token) if token.kind == TokenKind::Ident && token.lexeme == "default"
        ) {
            *input = &input[1..];
        }
        let member = if peek_keyword(*input, Keyword::Type) {
            parse_type_alias_item(input, visibility, member_attrs)?
        } else if peek_keyword(*input, Keyword::Const) && starts_const_fn(*input) {
            parse_fn_item(input, file, visibility, member_attrs, false)?
        } else if peek_keyword(*input, Keyword::Unsafe) && starts_unsafe_fn(*input) {
            parse_fn_item(input, file, visibility, member_attrs, false)?
        } else if peek_keyword(*input, Keyword::Async) && starts_async_fn(*input) {
            parse_fn_item(input, file, visibility, member_attrs, false)?
        } else if peek_keyword(*input, Keyword::Const) {
            parse_const_item(input, file, visibility, member_attrs)?
        } else if peek_keyword(*input, Keyword::Static) {
            parse_static_item(input, file, visibility, member_attrs)?
        } else if looks_like_item_macro(*input) {
            // An item-position macro invocation as an impl member (real
            // `core::iter::adapters::MapWhile`'s own `impl_fold_via_try_fold!
            // { fold -> try_fold }`, generating one or more real methods
            // this checker never expands) — same treatment as any other
            // unexpandable macro-generated item.
            parse_item_macro(input, member_attrs)?
        } else {
            parse_fn_item_core(input, file, visibility, member_attrs, false)?
        };
        items.push(member);
    }
    skip_symbol(input, "}")?;
    Ok(Item::from(ItemKind::Impl(ItemImpl {
        attrs,
        is_negative: false,
        trait_ty,
        self_ty,
        generics_params,
        collected_items: Vec::new(),
        items,
    })))
}

fn type_to_name(ty: &Ty) -> Option<Name> {
    match ty {
        Ty::Expr(expr) => match expr.kind() {
            ExprKind::Name(name) => Some(name.clone()),
            _ => None,
        },
        _ => None,
    }
}

fn parse_fn_params(input: &mut &[Token]) -> ModalResult<Vec<FunctionParam>> {
    // A bodiless declaration (extern-block fn/etc.) has nowhere to bind a
    // multi-element tuple-pattern parameter's names into, so any
    // destructure statements this would otherwise need are simply
    // discarded here — same simplification real Rust itself effectively
    // makes (the pattern is never actually bound to anything in a body).
    let mut destructures = Vec::new();
    let (_, params) = parse_fn_params_with_receiver(input, &mut destructures)?;
    Ok(params)
}

fn parse_fn_params_with_receiver(
    input: &mut &[Token],
    destructures: &mut Vec<BlockStmt>,
) -> ModalResult<(Option<FunctionParamReceiver>, Vec<FunctionParam>)> {
    let mut params = Vec::new();
    let mut receiver = None;
    let mut saw_keyword_only_boundary = false;
    if peek_symbol(input) == Some(")") {
        return Ok((receiver, params));
    }
    loop {
        if params.is_empty() && receiver.is_none() {
            if let Some(parsed) = parse_receiver(input)? {
                receiver = Some(parsed);
                if skip_symbol(input, ",").is_err() {
                    break;
                }
                if peek_symbol(input) == Some(")") {
                    break;
                }
                continue;
            }
        }
        if skip_symbol(input, "/").is_ok() {
            for param in &mut params {
                if !param.as_tuple && !param.as_dict {
                    param.positional_only = true;
                }
            }
        } else if peek_two_stars(*input) {
            skip_symbol(input, "*")?;
            skip_symbol(input, "*")?;
            let mut param = parse_fn_param_core(input, destructures)?;
            param.as_dict = true;
            param.keyword_only = true;
            params.push(param);
        } else if skip_symbol(input, "*").is_ok() {
            let mut probe = *input;
            if peek_symbol(probe) == Some(",") || peek_symbol(probe) == Some(")") {
                saw_keyword_only_boundary = true;
            } else {
                let mut param = parse_fn_param_after_star(&mut probe, destructures)?;
                param.as_tuple = true;
                if saw_keyword_only_boundary {
                    param.keyword_only = true;
                }
                *input = probe;
                params.push(param);
                saw_keyword_only_boundary = true;
            }
        } else {
            let mut param = parse_fn_param_core(input, destructures)?;
            if saw_keyword_only_boundary {
                param.keyword_only = true;
            }
            params.push(param);
        }

        if skip_symbol(input, ",").is_err() {
            break;
        }
        if peek_symbol(input) == Some(")") {
            break;
        }
    }
    Ok((receiver, params))
}

fn parse_fn_param_after_star(
    input: &mut &[Token],
    destructures: &mut Vec<BlockStmt>,
) -> ModalResult<FunctionParam> {
    parse_fn_param_core(input, destructures)
}

fn parse_receiver(input: &mut &[Token]) -> ModalResult<Option<FunctionParamReceiver>> {
    let mut probe = *input;
    let by_ref = skip_symbol(&mut probe, "&").is_ok();
    if by_ref {
        let _lifetime = match peek_ident_like(probe) {
            Some(ident) if ident.starts_with('\'') => Some(ident_like(&mut probe)?),
            _ => None,
        };
    }
    let mutable = skip_keyword(&mut probe, Keyword::Mut).is_ok();
    let ident = peek_ident_like(probe);
    if ident != Some("self") {
        return Ok(None);
    }
    let _ = ident_like(&mut probe)?;
    if skip_symbol(&mut probe, ":").is_ok() {
        let _ = parse_type_expr(&mut probe)?;
        *input = probe;
        return Ok(Some(match (by_ref, mutable) {
            (true, true) => FunctionParamReceiver::RefMut,
            (true, false) => FunctionParamReceiver::Ref,
            (false, true) => FunctionParamReceiver::MutValue,
            (false, false) => FunctionParamReceiver::Value,
        }));
    }
    *input = probe;
    let receiver = match (by_ref, mutable) {
        (true, true) => FunctionParamReceiver::RefMut,
        (true, false) => FunctionParamReceiver::Ref,
        (false, true) => FunctionParamReceiver::MutValue,
        (false, false) => FunctionParamReceiver::Value,
    };
    Ok(Some(receiver))
}

fn parse_fn_param_core(
    input: &mut &[Token],
    destructures: &mut Vec<BlockStmt>,
) -> ModalResult<FunctionParam> {
    // A parameter-level attribute (real `std::sys::pal::unix`'s own
    // `unsafe fn reset_sigpipe(#[allow(unused_variables)] sigpipe: u8)`) —
    // `FunctionParam` has no slot for it, so it's parsed and dropped, same
    // treatment already given to every other checker-inert attribute.
    skip_outer_attrs_for_field(input)?;
    let is_const = skip_keyword(input, Keyword::Const).is_ok();
    let is_context = starts_context_param_marker(*input);
    if is_context {
        let _ = ident_like(input)?;
    }
    // `ref [mut] name: T` (real `core::iter::traits::iterator`'s own
    // `ref mut predicate: P`) — a by-reference binding mode on the
    // parameter's own name, same as a `let ref mut x = ..` pattern. This
    // checker doesn't model binding modes beyond ordinary by-value
    // params (already true of the bare `mut name` case just below), so
    // `ref` is dropped the same way `mut` already is.
    if peek_ident_like(*input) == Some("ref") {
        let _ = ident_like(input);
    }
    let _is_mut = skip_keyword(input, Keyword::Mut).is_ok();
    let name = parse_fn_param_name(input, destructures)?;
    skip_symbol(input, ":")?;
    let ty = parse_type_expr(input)?;
    let mut param = FunctionParam::new(name, ty);
    param.is_const = is_const;
    param.is_context = is_context;
    if skip_symbol(input, "=").is_ok() {
        let expr = parse_expr_winnow_no_struct(input, 0)?;
        let ExprKind::Value(value) = expr.kind() else {
            return Err(ErrMode::Cut(ContextError::new()));
        };
        param.default = Some((**value).clone());
    }
    Ok(param)
}

fn starts_context_param_marker(input: &[Token]) -> bool {
    matches!(
        input,
        [
            first,
            second,
            third,
            ..
        ] if first.kind == TokenKind::Ident
            && first.lexeme == "context"
            && matches!(second.kind, TokenKind::Ident | TokenKind::Keyword(_))
            && third.kind == TokenKind::Symbol
            && third.lexeme == ":"
    )
}

fn parse_fn_param_name(
    input: &mut &[Token],
    destructures: &mut Vec<BlockStmt>,
) -> ModalResult<Ident> {
    // `(a, b, ..)` — a tuple destructuring pattern parameter with no
    // wrapping constructor name (real `std::process`'s own `fn
    // from_inner((handle, io): (imp::Process, StdioPipes)) -> Child`,
    // `core::ops::function`'s own blanket `fn call_mut(&mut self, (_ /*
    // ignore */,): (usize,))`). `FunctionParam` has no slot for a real
    // pattern, only a bare name, so this binds the parameter itself to a
    // synthetic name and hands back a `let (a, b, ..) = __synthetic;`
    // destructure statement for the caller to prepend to the function
    // body — real, working bindings for every name in the pattern,
    // rather than lossily keeping just one.
    let mut tuple_probe = *input;
    if skip_symbol(&mut tuple_probe, "(").is_ok() {
        let mut names = Vec::new();
        while peek_symbol(tuple_probe) != Some(")") {
            // `&name`/`&mut name` inside the tuple (real
            // `alloc::collections::btree::map`'s own `fn extend_one(&mut
            // self, (&k, &v): (&'a K, &'a V))`) — same lossy
            // drop-the-`&`-keep-the-binding treatment already given to a
            // bare `&item` parameter below.
            let _ = skip_symbol(&mut tuple_probe, "&");
            let _ = skip_keyword(&mut tuple_probe, Keyword::Mut);
            let name = if let Ok(name) = ident_like(&mut tuple_probe) {
                name
            } else if skip_symbol(&mut tuple_probe, "_").is_ok() {
                Ident::new("_")
            } else {
                return Err(ErrMode::Backtrack(ContextError::new()));
            };
            names.push(name);
            if skip_symbol(&mut tuple_probe, ",").is_err() {
                break;
            }
        }
        if skip_symbol(&mut tuple_probe, ")").is_ok() && !names.is_empty() {
            *input = tuple_probe;
            if names.len() == 1 {
                return Ok(names.remove(0));
            }
            let synthetic = Ident::new(format!("__tuple_param{}", destructures.len()));
            let pattern = Pattern::new(PatternKind::Tuple(PatternTuple {
                patterns: names
                    .into_iter()
                    .map(|ident| {
                        if ident.as_str() == "_" {
                            Pattern::new(PatternKind::Wildcard(PatternWildcard {}))
                        } else {
                            Pattern::new(PatternKind::Ident(PatternIdent {
                                ident,
                                mutability: None,
                            }))
                        }
                    })
                    .collect(),
            }));
            destructures.push(BlockStmt::Let(StmtLet::new(
                pattern,
                Some(Expr::name(Name::from_ident(synthetic.clone()))),
                None,
            )));
            return Ok(synthetic);
        }
    }
    // `&item`/`&mut item` — a by-ref destructuring pattern parameter (real
    // `alloc::collections::binary_heap`'s own `fn extend_one(&mut self,
    // &item: &'a T)`). `FunctionParam` has no slot for a real pattern, only
    // a bare name, so — same lossy treatment already given to a
    // tuple-struct-wrapped name like `Wrapping(n)` below — this keeps just
    // the inner binding name and drops the `&`/`&mut` itself.
    let mut ref_probe = *input;
    if skip_symbol(&mut ref_probe, "&").is_ok() {
        let _ = skip_keyword(&mut ref_probe, Keyword::Mut);
        if let Ok(name) = ident_like(&mut ref_probe) {
            *input = ref_probe;
            return Ok(name);
        }
    }
    let mut probe = *input;
    let simple_name = ident_like(&mut probe)?;
    // Consume an optional `::ident` chain before the destructuring
    // wrapper (e.g. `ops::Yeet(e)`) — the same qualified-path shape
    // match-arm patterns already support via `parse_name`'s identical
    // loop (`mod.rs:411-422`). The whole prefix path is discarded either
    // way, matching the existing lossy `Wrapping(n)` handling below
    // (`FunctionParam` has no slot for a real destructuring pattern, only
    // a bare `name`/`ty`) — this just also tolerates a qualified prefix
    // instead of only an unqualified one.
    let mut path_probe = probe;
    loop {
        let mut next = path_probe;
        if skip_symbol(&mut next, "::").is_err() || ident_like(&mut next).is_err() {
            break;
        }
        path_probe = next;
    }
    let mut destructured = path_probe;
    if skip_symbol(&mut destructured, "(").is_ok() {
        let inner_name = ident_like(&mut destructured)?;
        skip_symbol(&mut destructured, ")")?;
        *input = destructured;
        return Ok(inner_name);
    }
    *input = probe;
    Ok(simple_name)
}

fn skip_outer_attrs_for_field(input: &mut &[Token]) -> ModalResult<()> {
    loop {
        let mut probe = *input;
        if skip_symbol(&mut probe, "#").is_err() {
            return Ok(());
        }
        skip_symbol(&mut probe, "[")?;
        let mut depth = 1usize;
        while let Some((token, rest)) = probe.split_first() {
            probe = rest;
            if token.kind == TokenKind::Symbol {
                match token.lexeme.as_str() {
                    "[" => depth += 1,
                    "]" => {
                        depth -= 1;
                        if depth == 0 {
                            break;
                        }
                    }
                    _ => {}
                }
            }
        }
        if depth != 0 {
            return Err(ErrMode::Cut(ContextError::new()));
        }
        *input = probe;
    }
}

fn peek_two_stars(input: &[Token]) -> bool {
    matches!(input, [first, second, ..] if first.kind == TokenKind::Symbol && first.lexeme == "*" && second.kind == TokenKind::Symbol && second.lexeme == "*")
}

fn starts_unsafe_fn(input: &[Token]) -> bool {
    matches!(
        input,
        [first, ..] if first.kind == TokenKind::Keyword(Keyword::Unsafe)
    ) && super::skips_modifiers_to_fn(input)
}

pub(super) fn starts_unsafe_impl(input: &[Token]) -> bool {
    matches!(
        input,
        [first, second, ..]
            if first.kind == TokenKind::Keyword(Keyword::Unsafe)
                && second.kind == TokenKind::Keyword(Keyword::Impl)
    )
}

fn starts_restricted_trait(input: &[Token]) -> bool {
    if !matches!(
        input,
        [first, second, ..]
            if first.kind == TokenKind::Keyword(Keyword::Impl) && second.lexeme == "("
    ) {
        return false;
    }
    // `impl(restriction) trait Foo { .. }` and an ordinary inherent impl
    // of the unit type (`impl () {}`, real `core::primitive_docs`'s own,
    // undocumented but real syntax for "here's where `()`'s own docs
    // live") both start with `impl` immediately followed by `(` — the
    // restricted-trait form is only real when the balanced `(...)` is
    // itself followed (possibly through `const`/`unsafe` modifiers, real
    // `core::slice::index`'s own `impl(crate) const unsafe trait
    // SliceIndex<..>`) by the `trait` keyword; `impl ()`'s `(` is the
    // self-type's own empty tuple, followed directly by `{`.
    let mut probe = &input[1..];
    if skip_balanced_delimiters(&mut probe, "(", ")").is_err() {
        return false;
    }
    loop {
        if skip_keyword(&mut probe, Keyword::Const).is_ok()
            || skip_keyword(&mut probe, Keyword::Unsafe).is_ok()
        {
            continue;
        }
        break;
    }
    peek_keyword(probe, Keyword::Trait)
}

fn starts_const_impl(input: &[Token]) -> bool {
    // `const unsafe impl<T> Trait for X` is valid too (modifiers in any
    // order, same as `parse_fn_item_core`'s fix for `const unsafe fn`) —
    // skip an optional `unsafe` between `const` and `impl`, not just the
    // direct `const impl` pair.
    matches!(
        input,
        [first, ..] if first.kind == TokenKind::Keyword(Keyword::Const)
    ) && matches!(
        &input[1..],
        [second, ..] if second.kind == TokenKind::Keyword(Keyword::Impl)
    ) || matches!(
        input,
        [first, second, third, ..]
            if first.kind == TokenKind::Keyword(Keyword::Const)
                && second.kind == TokenKind::Keyword(Keyword::Unsafe)
                && third.kind == TokenKind::Keyword(Keyword::Impl)
    )
}

/// A `where` clause's own predicate list, parsed for real (rather than
/// merely skipped — see `skip_where_clause` below, still used at every
/// call site that has nowhere to put a parsed bound) — real `std` code
/// overwhelmingly spells a generic parameter's trait bounds this way
/// (`fn foo<F, R>(...) where F: FnOnce() -> R`) rather than inline
/// (`fn foo<F: FnOnce() -> R, R>(...)`), so a checker that only reads
/// inline bounds (`GenericParam::bounds`) almost never actually sees one.
/// Only the common `Name: Bound1 + Bound2` predicate shape is extracted;
/// anything else (a lifetime bound, a qualified-path bounded type like
/// `<T as Trait>::Assoc: Bound`, ...) is skipped exactly like
/// `skip_where_clause` already treats every predicate, carrying no
/// meaning this checker models beyond that one shape.
fn parse_where_clause_predicates(input: &mut &[Token]) -> ModalResult<Vec<(Ident, TypeBounds)>> {
    let mut predicates = Vec::new();
    loop {
        if input.is_empty() || matches!(peek_symbol(*input), Some("{") | Some(";")) {
            break;
        }
        let mut probe = *input;
        let parsed = (|| -> ModalResult<(Ident, TypeBounds)> {
            skip_hrtb_for_lifetimes_in_predicate(&mut probe);
            let name = ident_like(&mut probe)?;
            skip_symbol(&mut probe, ":")?;
            let bounds = parse_type_bounds(&mut probe)?;
            Ok((name, bounds))
        })();
        match parsed {
            Ok(predicate) => {
                predicates.push(predicate);
                *input = probe;
            }
            Err(_) => {
                skip_one_where_predicate(input)?;
            }
        }
        if skip_symbol(input, ",").is_err() {
            break;
        }
    }
    Ok(predicates)
}

/// A higher-ranked predicate's own `for<'a>` binder (`where for<'a> &'a
/// T: Trait`) — dropped the same way `skip_hrtb_for_lifetimes` (types.rs)
/// already drops one on an ordinary trait bound, just reachable from
/// `parse_where_clause_predicates`'s own module instead.
fn skip_hrtb_for_lifetimes_in_predicate(input: &mut &[Token]) {
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

/// Skip exactly one where-predicate this checker doesn't model (a
/// lifetime bound, a qualified-path bounded type, ...) — stops at the
/// next depth-0 `,`/`{`/`;` without consuming it, mirroring
/// `skip_where_clause`'s own depth-tracking (see its doc comment for why
/// angle-bracket depth must be tracked alongside bracket/paren/brace
/// depth) but scoped to a single predicate instead of the whole clause.
fn skip_one_where_predicate(input: &mut &[Token]) -> ModalResult<()> {
    let mut depth: i32 = 0;
    while !input.is_empty() {
        if depth == 0 && matches!(peek_symbol(input), Some(",") | Some("{") | Some(";")) {
            return Ok(());
        }
        match peek_symbol(input) {
            Some("(" | "[" | "{" | "<") => depth += 1,
            Some(")" | "]" | "}" | ">") => depth -= 1,
            Some("<<") => depth += 2,
            Some(">>") => depth -= 2,
            _ => {}
        }
        *input = &input[1..];
    }
    Err(ErrMode::Cut(ContextError::new()))
}

/// `parse_where_clause_predicates`, folding each predicate straight into
/// the matching entry of an already-parsed generic parameter list by
/// name — the call-site convenience every `skip_where_clause` call site
/// with a real `generics_params` in scope actually wants, so a bound
/// spelled in a `where` clause (the overwhelmingly common real-world
/// spelling) reaches `GenericParam::bounds` exactly like an inline one
/// already does. A predicate naming something other than one of
/// `generics_params` (`Self`, a qualified-path bounded type, ...) simply
/// has nowhere to go and is dropped, same as it always was.
fn parse_where_clause_and_merge(
    input: &mut &[Token],
    generics_params: &mut [fp_core::ast::GenericParam],
) -> ModalResult<()> {
    let predicates = parse_where_clause_predicates(input)?;
    for (name, bounds) in predicates {
        if let Some(param) = generics_params
            .iter_mut()
            .find(|param| param.name.as_str() == name.as_str())
        {
            param.bounds.bounds.extend(bounds.bounds);
        }
    }
    Ok(())
}

fn skip_where_clause(input: &mut &[Token]) -> ModalResult<()> {
    // A `where` clause is followed by either a body (`{`, an item
    // definition) or, for a bodiless declaration (a trait method/type
    // decl, an `extern` fn, ...), a bare `;` — this must stop at *either*
    // terminator without consuming it, or a `;`-terminated where clause
    // (real `core::iter::adapters::FuseImpl::try_fold`'s own multi-bound
    // `where Self: Sized, Fold: FnMut(..) -> R, R: Try<Output = Acc>;`)
    // would blindly consume every token past its own `;` hunting for a
    // `{` that might be arbitrarily far downstream, corrupting everything
    // in between.
    //
    // Only a depth-0 `{`/`;` actually terminates the clause — a bound's
    // own generic args can nest a `{`/`;` that means something else
    // entirely (real `core::array`'s own `where R: Residual<[R::Output;
    // N]>`: that `;` is the array-length separator, not the where
    // clause's own terminator; real `alloc::vec::mod`'s own `where for<'a>
    // ..: TransmuteFrom<.., { Assume::SAFETY }>` nests a braced
    // const-generic argument inside a `<..>` generic-arg list, whose own
    // `{` must not be mistaken for the where clause's own terminator
    // either). Track angle-bracket depth alongside bracket/paren/brace
    // depth — real Rust never uses a bare `<`/`>` comparison operator
    // inside a where clause's own bound list, only generic-arg delimiters
    // — so this is unambiguous here even though it wouldn't be in general
    // expression position.
    let mut depth: i32 = 0;
    while !input.is_empty() {
        if depth == 0 && matches!(peek_symbol(input), Some("{") | Some(";")) {
            return Ok(());
        }
        match peek_symbol(input) {
            Some("(" | "[" | "{" | "<") => depth += 1,
            Some(")" | "]" | "}" | ">") => depth -= 1,
            Some("<<") => depth += 2,
            Some(">>") => depth -= 2,
            _ => {}
        }
        *input = &input[1..];
    }
    Err(ErrMode::Cut(ContextError::new()))
}

fn parse_use_item(
    input: &mut &[Token],
    visibility: Visibility,
    attrs: Vec<Attribute>,
) -> ModalResult<Item> {
    skip_keyword(input, Keyword::Use)?;
    let tree = parse_use_tree(input)?;
    skip_symbol(input, ";")?;
    Ok(Item::from(ItemKind::Import(fp_core::ast::ItemImport {
        attrs,
        visibility,
        style: fp_core::ast::ItemImportStyle::Plain,
        tree,
    })))
}

fn parse_extern_crate_item(
    input: &mut &[Token],
    visibility: Visibility,
    attrs: Vec<Attribute>,
) -> ModalResult<Item> {
    skip_keyword(input, Keyword::Extern)?;
    skip_keyword(input, Keyword::Crate)?;
    let crate_name = ident_like(input)?;
    let tree = if skip_keyword(input, Keyword::As).is_ok() {
        let rename = ident_like(input)?;
        fp_core::ast::ItemImportTree::Rename(fp_core::ast::ItemImportRename {
            from: crate_name,
            to: rename,
        })
    } else {
        let mut path = fp_core::ast::ItemImportPath::new();
        path.push(fp_core::ast::ItemImportTree::Ident(crate_name));
        fp_core::ast::ItemImportTree::Path(path)
    };
    skip_symbol(input, ";")?;
    Ok(Item::from(ItemKind::Import(fp_core::ast::ItemImport {
        attrs,
        visibility,
        style: fp_core::ast::ItemImportStyle::Plain,
        tree,
    })))
}

fn parse_extern_item(
    input: &mut &[Token],
    file: FileId,
    visibility: Visibility,
    attrs: Vec<Attribute>,
) -> ModalResult<Item> {
    let mut probe = *input;
    skip_keyword(&mut probe, Keyword::Extern)?;
    if skip_keyword(&mut probe, Keyword::Crate).is_ok() {
        return parse_extern_crate_item(input, visibility, attrs);
    }
    let abi = parse_extern_abi(input)?;
    if peek_keyword(*input, Keyword::Fn) {
        return parse_extern_fn_item(input, file, visibility, attrs, abi);
    }
    if peek_symbol(input) == Some("{") {
        let items = parse_extern_block_items(input, file)?;
        let item = items
            .into_iter()
            .next()
            .ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
        return Ok(item);
    }
    Err(ErrMode::Backtrack(ContextError::new()))
}

fn parse_extern_abi(input: &mut &[Token]) -> ModalResult<fp_core::ast::Abi> {
    skip_keyword(input, Keyword::Extern)?;
    let abi = token_kind(input, TokenKind::StringLiteral)?;
    let cleaned =
        decode_string_literal(&abi.lexeme).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
    Ok(fp_core::ast::Abi::Named(cleaned))
}

fn parse_extern_fn_item(
    input: &mut &[Token],
    file: FileId,
    visibility: Visibility,
    attrs: Vec<Attribute>,
    abi: fp_core::ast::Abi,
) -> ModalResult<Item> {
    skip_keyword(input, Keyword::Fn)?;
    let name = ident_like(input)?;
    let generics_params = parse_optional_generic_params(input)?;
    skip_symbol(input, "(")?;
    let params = parse_fn_params(input)?;
    skip_symbol(input, ")")?;
    let ret_ty = if skip_symbol(input, "->").is_ok() {
        Some(parse_type_expr(input)?)
    } else {
        None
    };
    let sig = FunctionSignature {
        name: Some(name.clone()),
        receiver: None,
        params,
        generics_params,
        is_const: false,
        abi,
        quote_kind: None,
        ret_ty,
    };
    if skip_symbol(input, ";").is_ok() {
        return Ok(Item::from(ItemKind::DeclFunction(ItemDeclFunction {
            attrs,
            ty_annotation: None,
            name,
            sig,
        })));
    }
    let body = parse_function_block(input, file)?;
    Ok(Item::from(ItemKind::DefFunction(ItemDefFunction {
        ty_annotation: None,
        attrs,
        name,
        collected_items: Vec::new(),
        ty: None,
        sig,
        body,
        is_async: false,
        visibility,
    })))
}

pub(super) fn parse_extern_block_items(input: &mut &[Token], file: FileId) -> ModalResult<Vec<Item>> {
    let abi = parse_extern_abi(input)?;
    skip_symbol(input, "{")?;
    let mut items = Vec::new();
    while peek_symbol(input) != Some("}") {
        let attrs = parse_outer_attrs(input, file)?;
        let visibility = parse_visibility(input)?;
        // `safe fn`/`safe static` — edition-2024 `unsafe extern` blocks may
        // mark individual items callable without an `unsafe` block. `safe`
        // isn't a lexer keyword (a plain `Ident`), and carries no meaning
        // this checker models beyond "callable normally", so just drop it.
        if matches!(
            input.first(),
            Some(token) if token.kind == TokenKind::Ident && token.lexeme == "safe"
        ) {
            *input = &input[1..];
        }
        // `unsafe fn`/`unsafe static` — the other edition-2024 per-item
        // safety marker inside an `unsafe extern` block (real
        // `std::sys::time::unix`'s own `unsafe extern "C" { unsafe fn
        // mach_timebase_info(..) -> ..; }`), the counterpart to `safe`
        // above. Carries no meaning this checker models beyond "requires
        // an unsafe block to call" — already true of everything else in
        // an extern block — so it's dropped the same way.
        let _ = skip_keyword(input, Keyword::Unsafe);
        if peek_keyword(*input, Keyword::Fn) {
            items.push(parse_abi_fn_item(
                input,
                file,
                visibility,
                attrs,
                abi.clone(),
            )?);
            continue;
        }
        // `static [mut] NAME: TYPE;` — an external symbol declaration
        // (real `std::sys::alloc::vexos`'s linkerscript-provided
        // `__heap_start`/`__heap_end`), never has an initializer (unlike
        // an ordinary `static`, whose value lives in *this* module) — the
        // linker resolves it, not this compiler.
        if peek_keyword(*input, Keyword::Static) {
            items.push(parse_extern_static_decl(input)?);
            continue;
        }
        // `type Name;` — an "extern type" (unstable `extern_types`,
        // real core::ptr::metadata's own `unsafe extern "C" { type
        // VTable; }`): an opaque, unsized foreign type with no
        // representation this compiler ever needs to know (no fields,
        // no size — the linker/foreign side owns it entirely). Modeled
        // as a real, ordinary field-less struct so the name still
        // resolves to a genuine type `DefId` wherever it's referenced
        // (e.g. `NonNull<VTable>` just above it in the same file) —
        // the same "give it a real definition, just an empty one"
        // treatment already given to a trait alias's own dropped bound
        // semantics.
        if skip_keyword(input, Keyword::Type).is_ok() {
            let name = ident_like(input)?;
            skip_symbol(input, ";")?;
            items.push(Item::from(ItemKind::DefStruct(ItemDefStruct {
                attrs,
                visibility,
                name: name.clone(),
                value: TypeStruct {
                    name,
                    generics_params: Vec::new(),
                    repr: ReprOptions::default(),
                    fields: Vec::new(),
                },
            })));
            continue;
        }
        return Err(ErrMode::Cut(ContextError::new()));
    }
    skip_symbol(input, "}")?;
    Ok(items)
}

fn parse_unsafe_extern_block_items(input: &mut &[Token], file: FileId) -> ModalResult<Vec<Item>> {
    skip_keyword(input, Keyword::Unsafe)?;
    parse_extern_block_items(input, file)
}

pub(super) fn parse_prefixed_unsafe_extern_block_items(
    input: &mut &[Token],
    file: FileId,
) -> ModalResult<Vec<Item>> {
    let _ = parse_outer_attrs(input, file)?;
    parse_unsafe_extern_block_items(input, file)
}

fn parse_extern_static_decl(input: &mut &[Token]) -> ModalResult<Item> {
    skip_keyword(input, Keyword::Static)?;
    let _mutable = skip_keyword(input, Keyword::Mut).is_ok();
    let name = ident_like(input)?;
    skip_symbol(input, ":")?;
    let ty = parse_type_expr(input)?;
    skip_symbol(input, ";")?;
    Ok(Item::from(ItemKind::DeclStatic(ItemDeclStatic {
        ty_annotation: None,
        name,
        ty,
    })))
}

fn parse_abi_fn_item(
    input: &mut &[Token],
    file: FileId,
    visibility: Visibility,
    attrs: Vec<Attribute>,
    abi: fp_core::ast::Abi,
) -> ModalResult<Item> {
    skip_keyword(input, Keyword::Fn)?;
    let name = ident_like(input)?;
    let generics_params = parse_optional_generic_params(input)?;
    skip_symbol(input, "(")?;
    let params = parse_fn_params(input)?;
    skip_symbol(input, ")")?;
    let ret_ty = if skip_symbol(input, "->").is_ok() {
        Some(parse_type_expr(input)?)
    } else {
        None
    };
    let sig = FunctionSignature {
        name: Some(name.clone()),
        receiver: None,
        params,
        generics_params,
        is_const: false,
        abi,
        quote_kind: None,
        ret_ty,
    };
    if skip_symbol(input, ";").is_ok() {
        return Ok(Item::from(ItemKind::DeclFunction(ItemDeclFunction {
            attrs,
            ty_annotation: None,
            name,
            sig,
        })));
    }
    let body = parse_function_block(input, file)?;
    Ok(Item::from(ItemKind::DefFunction(ItemDefFunction {
        ty_annotation: None,
        attrs,
        name,
        collected_items: Vec::new(),
        ty: None,
        sig,
        body,
        is_async: false,
        visibility,
    })))
}

fn parse_enum_item(
    input: &mut &[Token],
    file: FileId,
    visibility: Visibility,
    attrs: Vec<Attribute>,
) -> ModalResult<Item> {
    skip_keyword(input, Keyword::Enum)?;
    let name = ident_like(input)?;
    let mut generics_params = parse_optional_generic_params(input)?;
    if skip_keyword(input, Keyword::Where).is_ok() {
        parse_where_clause_and_merge(input, &mut generics_params)?;
    }
    skip_symbol(input, "{")?;
    let mut variants = Vec::new();
    while peek_symbol(input) != Some("}") {
        // `parse_outer_attrs` captures each attribute as a real `Attribute`
        // (`ast_to_hir` needs these, e.g. for `#[op(variant = "...")]` on
        // `Option::Some`/`::None`) — including skipping-with-a-warning any
        // single malformed one (see `parse_attrs`'s own per-attribute
        // recovery), so a `thiserror`-style `#[error("...", expr)]` doesn't
        // take the rest of the enum's attributes down with it.
        let variant_attrs = parse_outer_attrs(input, file)?;
        let variant_name = ident_like(input)?;
        let value = if skip_symbol(input, "{").is_ok() {
            let mut fields = Vec::new();
            while peek_symbol(input) != Some("}") {
                skip_outer_attrs_for_field(input)?;
                let _field_visibility = parse_visibility(input)?;
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
            Ty::Structural(fp_core::ast::TypeStructural { fields }.into())
        } else if skip_symbol(input, "(").is_ok() {
            let mut tys = Vec::new();
            if peek_symbol(input) != Some(")") {
                loop {
                    skip_outer_attrs_for_field(input)?;
                    tys.push(parse_type_expr(input)?);
                    if skip_symbol(input, ",").is_err() {
                        break;
                    }
                    if peek_symbol(input) == Some(")") {
                        break;
                    }
                }
            }
            skip_symbol(input, ")")?;
            if tys.len() == 1 {
                tys.pop().expect("single enum variant type")
            } else {
                Ty::Tuple(fp_core::ast::TypeTuple { types: tys }.into())
            }
        } else {
            Ty::unit()
        };
        let discriminant = if skip_symbol(input, "=").is_ok() {
            Some(Box::new(parse_expr_winnow_no_struct(input, 0)?))
        } else {
            None
        };
        variants.push(EnumTypeVariant {
            attrs: variant_attrs,
            name: variant_name,
            value,
            discriminant,
        });
        if skip_symbol(input, ",").is_err() {
            break;
        }
    }
    skip_symbol(input, "}")?;
    Ok(Item::from(ItemKind::DefEnum(ItemDefEnum {
        attrs,
        visibility,
        name: name.clone(),
        value: TypeEnum {
            name,
            generics_params,
            repr: ReprOptions::default(),
            variants,
        },
    })))
}

fn parse_mod_item(
    input: &mut &[Token],
    file: FileId,
    visibility: Visibility,
    mut attrs: Vec<Attribute>,
) -> ModalResult<Item> {
    skip_keyword(input, Keyword::Mod)?;
    let name = ident_like(input)?;
    if skip_symbol(input, ";").is_ok() {
        return Ok(Item::from(ItemKind::Module(Module {
            attrs,
            name,
            collected_items: Vec::new(),
            items: Vec::new(),
            visibility,
            is_external: true,
        })));
    }
    skip_symbol(input, "{")?;
    attrs.extend(parse_inner_attrs(input, file)?);
    let mut items = Vec::new();
    while peek_symbol(input) != Some("}") {
        // A nested `mod { .. }` body needs the same `extern`/`unsafe
        // extern` block special-casing `parse_items_tokens`/
        // `parse_file_tokens`/`parse_block` already have (real
        // `core::ffi::mod`'s own `mod c_char_definition { .. unsafe
        // extern "C" { .. } .. }`) — `parse_item_winnow`'s own dispatch
        // only recognizes a bare `extern "ABI" fn`/single-item form, not
        // the multi-item block form, and expands to more than one item.
        if looks_like_extern_block(*input) {
            items.extend(parse_extern_block_items(input, file)?);
            continue;
        }
        if starts_unsafe_extern_block(*input) {
            items.extend(parse_prefixed_unsafe_extern_block_items(input, file)?);
            continue;
        }
        items.push(parse_item_winnow(input, file)?);
    }
    skip_symbol(input, "}")?;
    Ok(Item::from(ItemKind::Module(Module {
        attrs,
        name,
        collected_items: Vec::new(),
        items,
        visibility,
        is_external: false,
    })))
}

fn parse_opaque_type_item(
    input: &mut &[Token],
    visibility: Visibility,
    attrs: Vec<Attribute>,
) -> ModalResult<Item> {
    skip_keyword(input, Keyword::Opaque)?;
    skip_keyword(input, Keyword::Type)?;
    let name = ident_like(input)?;
    skip_symbol(input, ";")?;
    Ok(Item::from(ItemKind::OpaqueType(ItemOpaqueType {
        attrs,
        visibility,
        name,
    })))
}

/// `macro Name(...) { .. }` — declarative "macro 2.0" syntax (distinct
/// from `macro_rules! Name { .. }`, which `looks_like_item_macro`/
/// `parse_item_macro` already handle via the generic `ident!` shape).
/// `macro` isn't a lexer keyword (tokenizes as a plain `Ident`), so this
/// needs its own lookahead rather than a `Keyword::Macro` dispatch arm.
pub(super) fn starts_macro_2_def(input: &[Token]) -> bool {
    matches!(
        input,
        [first, second, third, ..]
            if first.kind == TokenKind::Ident
                && first.lexeme == "macro"
                && matches!(second.kind, TokenKind::Ident | TokenKind::Keyword(_))
                && (third.lexeme == "(" || third.lexeme == "{")
    )
}

/// Every real use of the `macro Name(params) { body }` shape in vendored
/// std (e.g. `derive(Default)`'s own definition) is a compiler built-in
/// whose body is just a marker comment — no real expansion to model, so
/// that shape is dropped, exactly like a real `macro_rules!` item
/// already gets dropped downstream (see `ast_to_hir`'s `ItemKind::Macro`
/// handling). The bodied `macro Name { rule => expansion; .. }` shape
/// (no separate params group — real `alloc::vec::into_iter`'s own
/// `non_null!`) is a genuinely different, functioning macro whose call
/// sites this compiler still can't expand, but that's an existing,
/// separate limitation for whatever already fails to resolve a
/// `macro_rules!` invocation — this only needs to stop that limitation
/// from *also* taking down the rest of the file's otherwise-valid parse.
fn parse_macro_2_def(input: &mut &[Token], _attrs: Vec<Attribute>) -> ModalResult<Item> {
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

fn skip_ident(input: &mut &[Token], expected: &str) -> ModalResult<()> {
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
fn skip_balanced_delimiters(input: &mut &[Token], open: &str, close: &str) -> ModalResult<()> {
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

fn parse_item_macro(input: &mut &[Token], _attrs: Vec<Attribute>) -> ModalResult<Item> {
    let path = parse_macro_path(input)?;
    skip_symbol(input, "!")?;
    let declared_name = if path.segments.last().map(Ident::as_str) == Some("macro_rules") {
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

fn parse_visibility(input: &mut &[Token]) -> ModalResult<Visibility> {
    let mut probe = *input;
    if skip_keyword(&mut probe, Keyword::Pub).is_err() {
        return Ok(Visibility::Public);
    }
    if skip_symbol(&mut probe, "(").is_err() {
        *input = probe;
        return Ok(Visibility::Public);
    }
    let visibility = if skip_keyword(&mut probe, Keyword::Crate).is_ok() {
        Visibility::Crate
    } else if peek_ident_like(probe) == Some("self") {
        let _ = ident_like(&mut probe)?;
        Visibility::Restricted(single_segment_path(fp_core::ast::ItemImportTree::SelfMod))
    } else if skip_keyword(&mut probe, Keyword::Super).is_ok() {
        Visibility::Restricted(single_segment_path(fp_core::ast::ItemImportTree::SuperMod))
    } else if skip_keyword(&mut probe, Keyword::In).is_ok() {
        Visibility::Restricted(parse_use_path(&mut probe)?)
    } else {
        return Err(ErrMode::Cut(ContextError::new()));
    };
    skip_symbol(&mut probe, ")")?;
    *input = probe;
    Ok(visibility)
}

fn single_segment_path(segment: fp_core::ast::ItemImportTree) -> fp_core::ast::ItemImportPath {
    let mut path = fp_core::ast::ItemImportPath::new();
    path.push(segment);
    path
}

pub(crate) fn parse_outer_attrs(input: &mut &[Token], file: FileId) -> ModalResult<Vec<Attribute>> {
    parse_attrs(input, file, false)
}

fn parse_inner_attrs(input: &mut &[Token], file: FileId) -> ModalResult<Vec<Attribute>> {
    parse_attrs(input, file, true)
}

fn parse_attrs(input: &mut &[Token], file: FileId, inner: bool) -> ModalResult<Vec<Attribute>> {
    let mut attrs = Vec::new();
    loop {
        let mut probe = *input;
        if inner {
            if skip_symbol(&mut probe, "#![").is_err() {
                let mut split_probe = *input;
                if skip_symbol(&mut split_probe, "#").is_err() {
                    break;
                }
                if skip_symbol(&mut split_probe, "!").is_err()
                    || skip_symbol(&mut split_probe, "[").is_err()
                {
                    break;
                }
                probe = split_probe;
            }
        } else if skip_symbol(&mut probe, "#[").is_err() {
            if skip_symbol(&mut probe, "#").is_err() {
                break;
            }
            skip_symbol(&mut probe, "[")?;
        }
        // `probe` is now positioned right after the opening `[` — save that
        // so a single malformed attribute (e.g. `thiserror`'s
        // `#[error("...", expr)]`, mixing a format string with arbitrary
        // trailing expressions — not structured-meta grammar at all) can be
        // skipped on its own, without discarding every other attribute
        // parsed before or after it in the same run.
        let after_open_bracket = probe;
        match parse_attr_meta_direct(&mut probe, file).and_then(|meta| {
            skip_symbol(&mut probe, "]")?;
            Ok(meta)
        }) {
            Ok(meta) => {
                *input = probe;
                attrs.push(Attribute {
                    style: if inner {
                        AttrStyle::Inner
                    } else {
                        AttrStyle::Outer
                    },
                    meta,
                });
            }
            Err(err) => {
                let mut skip = after_open_bracket;
                let mut depth = 1usize;
                while let Some((token, rest)) = skip.split_first() {
                    skip = rest;
                    if token.kind == TokenKind::Symbol {
                        match token.lexeme.as_str() {
                            "[" => depth += 1,
                            "]" => {
                                depth -= 1;
                                if depth == 0 {
                                    break;
                                }
                            }
                            _ => {}
                        }
                    }
                }
                if depth != 0 {
                    return Err(err);
                }
                fp_core::diagnostics::diagnostic_manager().add_diagnostic(
                    fp_core::diagnostics::Diagnostic::warning(format!(
                        "attribute did not parse as a structured attribute ({err}); \
                         skipping just this one — any `#[op(...)]`/`#[intrinsic = \"...\"]` \
                         marker on it is lost"
                    )),
                );
                *input = skip;
            }
        }
    }
    Ok(attrs)
}

fn const_struct_attr() -> Attribute {
    Attribute {
        style: AttrStyle::Outer,
        meta: AttrMeta::Path(Path::plain(vec![Ident::new("const")])),
    }
}

pub(crate) fn parse_attr_meta_direct(input: &mut &[Token], file: FileId) -> ModalResult<AttrMeta> {
    let name = parse_module_path(input)?;
    if skip_symbol(input, "=").is_ok() {
        let value = parse_expr_winnow_no_struct(input, file)?;
        return Ok(AttrMeta::NameValue(AttrMetaNameValue {
            name,
            value: Box::new(value),
        }));
    }
    if skip_symbol(input, "(").is_ok() {
        let mut items = Vec::new();
        while peek_symbol(input) != Some(")") {
            let mut item_probe = *input;
            if let Ok(item) = parse_attr_meta_direct(&mut item_probe, file) {
                *input = item_probe;
                items.push(item);
            } else {
                let mut literal_probe = *input;
                let value = parse_expr_winnow_no_struct(&mut literal_probe, file)?;
                *input = literal_probe;
                items.push(AttrMeta::NameValue(AttrMetaNameValue {
                    name: Path::from_ident(Ident::new(format!("__arg{}", items.len()))),
                    value: Box::new(value),
                }));
            }
            if skip_symbol(input, ",").is_err() {
                break;
            }
        }
        skip_symbol(input, ")")?;
        return Ok(AttrMeta::List(AttrMetaList { name, items }));
    }
    Ok(AttrMeta::Path(name))
}

pub(super) fn looks_like_item_macro(input: &[Token]) -> bool {
    let mut saw_segment = false;
    let mut rest = input;
    while let [first, second, tail @ ..] = rest {
        if matches!(first.kind, TokenKind::Ident | TokenKind::Keyword(_))
            && second.kind == TokenKind::Symbol
            && second.lexeme == "!"
        {
            return true;
        }
        if matches!(first.kind, TokenKind::Ident | TokenKind::Keyword(_))
            && second.kind == TokenKind::Symbol
            && second.lexeme == "::"
        {
            saw_segment = true;
            rest = tail;
            continue;
        }
        break;
    }
    saw_segment
}
