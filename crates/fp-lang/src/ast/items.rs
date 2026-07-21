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
        let item = parse_item_or_expr_winnow(&mut input, file).map_err(|err| map_err(err, input))?;
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
        let item = parse_item_or_expr_winnow(&mut input, file).map_err(|err| map_err(err, input))?;
        items.push(item);
    }
    Ok((attrs, items))
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
        Some(TokenKind::Keyword(Keyword::Unsafe)) if starts_unsafe_fn(*input) => {
            parse_fn_item(input, file, visibility, attrs, false)
        }
        Some(TokenKind::Keyword(Keyword::Unsafe)) if starts_unsafe_impl(*input) => {
            parse_impl_item(input, file, attrs)
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
        Some(TokenKind::Keyword(Keyword::Enum)) => parse_enum_item(input, visibility, attrs),
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
        Some(TokenKind::Ident) | Some(TokenKind::Keyword(_)) if looks_like_item_macro(*input) => {
            parse_item_macro(input, attrs)
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
    expect_keyword(input, Keyword::Const)?;
    let mutable = expect_keyword(input, Keyword::Mut).is_ok();
    let name = ident_like(input)?;
    let ty = if expect_symbol(input, ":").is_ok() {
        Some(parse_type_expr(input)?)
    } else {
        None
    };
    expect_symbol(input, "=")?;
    let value = parse_expr_winnow(input, file)?;
    expect_symbol(input, ";")?;
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
    expect_keyword(input, Keyword::Static)?;
    let _mutable = expect_keyword(input, Keyword::Mut).is_ok();
    let name = ident_like(input)?;
    expect_symbol(input, ":")?;
    let ty = parse_type_expr(input)?;
    expect_symbol(input, "=")?;
    let value = parse_expr_winnow(input, file)?;
    expect_symbol(input, ";")?;
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
    expect_keyword(input, Keyword::Type)?;
    let name = ident_like(input)?;
    let generics_params = parse_optional_generic_params(input)?;
    if expect_keyword(input, Keyword::Where).is_ok() {
        skip_where_clause(input)?;
    }
    expect_symbol(input, "=")?;
    let value = parse_type_expr(input)?;
    expect_symbol(input, ";")?;
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
    expect_keyword(input, Keyword::Struct)?;
    let name = ident_like(input)?;
    let generics_params = parse_optional_generic_params(input)?;
    if expect_keyword(input, Keyword::Where).is_ok() {
        skip_where_clause(input)?;
    }
    let mut fields = Vec::new();
    if expect_symbol(input, ";").is_ok() {
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
    if expect_symbol(input, "(").is_ok() {
        let mut index = 0usize;
        while peek_symbol(input) != Some(")") {
            skip_outer_attrs_for_field(input)?;
            let _field_visibility = parse_visibility(input)?;
            let value = parse_type_expr(input)?;
            fields.push(StructuralField::new(Ident::new(index.to_string()), value));
            index += 1;
            if expect_symbol(input, ",").is_err() {
                break;
            }
        }
        expect_symbol(input, ")")?;
        expect_symbol(input, ";")?;
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
    expect_symbol(input, "{")?;
    while peek_symbol(input) != Some("}") {
        skip_outer_attrs_for_field(input)?;
        let _field_visibility = parse_visibility(input)?;
        let field_name = ident_like(input)?;
        let is_optional = expect_symbol(input, "?").is_ok();
        expect_symbol(input, ":")?;
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
        if expect_symbol(input, ",").is_err() {
            break;
        }
    }
    expect_symbol(input, "}")?;
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
    expect_keyword(input, Keyword::Const)?;
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

fn parse_fn_item_core(
    input: &mut &[Token],
    file: FileId,
    visibility: Visibility,
    attrs: Vec<Attribute>,
    quoted: bool,
) -> ModalResult<Item> {
    let _is_unsafe = expect_keyword(input, Keyword::Unsafe).is_ok();
    let is_async = expect_keyword(input, Keyword::Async).is_ok();
    let is_const = expect_keyword(input, Keyword::Const).is_ok();
    if quoted {
        expect_keyword(input, Keyword::Quote)?;
    }
    expect_keyword(input, Keyword::Fn)?;
    let name = ident_like(input)?;
    let generics_params = parse_optional_generic_params(input)?;
    expect_symbol(input, "(")?;
    let (receiver, params) = parse_fn_params_with_receiver(input)?;
    expect_symbol(input, ")")?;
    let ret_ty = if expect_symbol(input, "->").is_ok() {
        Some(parse_type_expr(input)?)
    } else {
        None
    };
    if expect_keyword(input, Keyword::Where).is_ok() {
        skip_where_clause(input)?;
    }
    let body = parse_block_expr(input, file)?;
    let body = if is_async {
        ExprKind::Async(fp_core::ast::ExprAsync {
            span: body.span(),
            expr: Box::new(body),
        })
        .into()
    } else {
        body
    };
    let mut sig = FunctionSignature {
        name: Some(name.clone()),
        receiver,
        params,
        generics_params,
        is_const,
        abi: fp_core::ast::Abi::Rust,
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
        ty: None,
        sig,
        body: Box::new(body),
        visibility,
    })))
}

fn parse_trait_item(
    input: &mut &[Token],
    file: FileId,
    visibility: Visibility,
    attrs: Vec<Attribute>,
) -> ModalResult<Item> {
    expect_keyword(input, Keyword::Trait)?;
    let name = ident_like(input)?;
    let generics_params = parse_optional_generic_params(input)?;
    let bounds = if expect_symbol(input, ":").is_ok() {
        parse_type_bounds(input)?
    } else {
        TypeBounds::any()
    };
    expect_symbol(input, "{")?;
    let mut items = Vec::new();
    while peek_symbol(input) != Some("}") {
        items.push(parse_trait_member(input, file)?);
    }
    expect_symbol(input, "}")?;
    Ok(Item::from(ItemKind::DefTrait(ItemDefTrait {
        attrs,
        name,
        generics_params,
        bounds,
        items,
        visibility,
    })))
}

fn parse_trait_member(input: &mut &[Token], file: FileId) -> ModalResult<Item> {
    let attrs = parse_outer_attrs(input, file)?;
    if expect_keyword(input, Keyword::Const).is_ok() {
        let name = ident_like(input)?;
        expect_symbol(input, ":")?;
        let ty = parse_type_expr(input)?;
        expect_symbol(input, ";")?;
        return Ok(Item::from(ItemKind::DeclConst(ItemDeclConst {
            ty_annotation: None,
            name,
            ty,
        })));
    }
    if expect_keyword(input, Keyword::Type).is_ok() {
        let name = ident_like(input)?;
        expect_symbol(input, ";")?;
        return Ok(Item::from(ItemKind::DeclType(ItemDeclType {
            ty_annotation: None,
            name,
            bounds: TypeBounds::any(),
        })));
    }
    let visibility = Visibility::Inherited;
    if peek_keyword(*input, Keyword::Fn)
        || peek_keyword(*input, Keyword::Quote)
        || starts_async_fn(*input)
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
    let is_async = expect_keyword(input, Keyword::Async).is_ok();
    let quoted = expect_keyword(input, Keyword::Quote).is_ok();
    if quoted {
        expect_keyword(input, Keyword::Fn)?;
    } else {
        expect_keyword(input, Keyword::Fn)?;
    }
    let name = ident_like(input)?;
    let generics_params = parse_optional_generic_params(input)?;
    expect_symbol(input, "(")?;
    let (receiver, params) = parse_fn_params_with_receiver(input)?;
    expect_symbol(input, ")")?;
    let ret_ty = if expect_symbol(input, "->").is_ok() {
        Some(parse_type_expr(input)?)
    } else {
        None
    };
    if expect_keyword(input, Keyword::Where).is_ok() {
        skip_where_clause(input)?;
    }
    let mut sig = FunctionSignature {
        name: Some(name.clone()),
        receiver,
        params,
        generics_params,
        is_const: false,
        abi: fp_core::ast::Abi::Rust,
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
    if expect_symbol(input, ";").is_ok() {
        return Ok(Item::from(ItemKind::DeclFunction(ItemDeclFunction {
            attrs,
            ty_annotation: None,
            name,
            sig,
        })));
    }
    let body = parse_block_expr(input, file)?;
    let body = if is_async {
        ExprKind::Async(fp_core::ast::ExprAsync {
            span: body.span(),
            expr: Box::new(body),
        })
        .into()
    } else {
        body
    };
    Ok(Item::from(ItemKind::DefFunction(ItemDefFunction {
        ty_annotation: None,
        attrs,
        name,
        ty: None,
        sig,
        body: Box::new(body),
        visibility,
    })))
}

fn peek_keyword(input: &[Token], keyword: Keyword) -> bool {
    matches!(input.first(), Some(token) if token.kind == TokenKind::Keyword(keyword))
}

fn parse_impl_item(input: &mut &[Token], file: FileId, attrs: Vec<Attribute>) -> ModalResult<Item> {
    let _is_unsafe = expect_keyword(input, Keyword::Unsafe).is_ok();
    expect_keyword(input, Keyword::Impl)?;
    let generics_params = parse_optional_generic_params(input)?;
    let first_ty = parse_type_expr(input)?;
    let (trait_ty, self_ty) = if expect_keyword(input, Keyword::For).is_ok() {
        let self_ty = parse_type_expr(input)?;
        (type_to_name(&first_ty), type_to_expr(&self_ty))
    } else {
        (None, type_to_expr(&first_ty))
    };
    if expect_keyword(input, Keyword::Where).is_ok() {
        skip_where_clause(input)?;
    }
    expect_symbol(input, "{")?;
    let mut items = Vec::new();
    while peek_symbol(input) != Some("}") {
        let member_attrs = parse_outer_attrs(input, file)?;
        let visibility = parse_visibility(input)?;
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
        } else {
            parse_fn_item_core(input, file, visibility, member_attrs, false)?
        };
        items.push(member);
    }
    expect_symbol(input, "}")?;
    Ok(Item::from(ItemKind::Impl(ItemImpl {
        attrs,
        is_negative: false,
        trait_ty,
        self_ty,
        generics_params,
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
    let (_, params) = parse_fn_params_with_receiver(input)?;
    Ok(params)
}

fn parse_fn_params_with_receiver(
    input: &mut &[Token],
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
                if expect_symbol(input, ",").is_err() {
                    break;
                }
                if peek_symbol(input) == Some(")") {
                    break;
                }
                continue;
            }
        }
        if expect_symbol(input, "/").is_ok() {
            for param in &mut params {
                if !param.as_tuple && !param.as_dict {
                    param.positional_only = true;
                }
            }
        } else if peek_two_stars(*input) {
            expect_symbol(input, "*")?;
            expect_symbol(input, "*")?;
            let mut param = parse_fn_param_core(input)?;
            param.as_dict = true;
            param.keyword_only = true;
            params.push(param);
        } else if expect_symbol(input, "*").is_ok() {
            let mut probe = *input;
            if peek_symbol(probe) == Some(",") || peek_symbol(probe) == Some(")") {
                saw_keyword_only_boundary = true;
            } else {
                let mut param = parse_fn_param_after_star(&mut probe)?;
                param.as_tuple = true;
                if saw_keyword_only_boundary {
                    param.keyword_only = true;
                }
                *input = probe;
                params.push(param);
                saw_keyword_only_boundary = true;
            }
        } else {
            let mut param = parse_fn_param_core(input)?;
            if saw_keyword_only_boundary {
                param.keyword_only = true;
            }
            params.push(param);
        }

        if expect_symbol(input, ",").is_err() {
            break;
        }
        if peek_symbol(input) == Some(")") {
            break;
        }
    }
    Ok((receiver, params))
}

fn parse_fn_param_after_star(input: &mut &[Token]) -> ModalResult<FunctionParam> {
    parse_fn_param_core(input)
}

fn parse_receiver(input: &mut &[Token]) -> ModalResult<Option<FunctionParamReceiver>> {
    let mut probe = *input;
    let by_ref = expect_symbol(&mut probe, "&").is_ok();
    if by_ref {
        let _lifetime = match peek_ident_like(probe) {
            Some(ident) if ident.starts_with('\'') => Some(ident_like(&mut probe)?),
            _ => None,
        };
    }
    let mutable = expect_keyword(&mut probe, Keyword::Mut).is_ok();
    let ident = peek_ident_like(probe);
    if ident != Some("self") {
        return Ok(None);
    }
    let _ = ident_like(&mut probe)?;
    if expect_symbol(&mut probe, ":").is_ok() {
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

fn parse_fn_param_core(input: &mut &[Token]) -> ModalResult<FunctionParam> {
    let is_const = expect_keyword(input, Keyword::Const).is_ok();
    let is_context = starts_context_param_marker(*input);
    if is_context {
        let _ = ident_like(input)?;
    }
    let _is_mut = expect_keyword(input, Keyword::Mut).is_ok();
    let name = parse_fn_param_name(input)?;
    expect_symbol(input, ":")?;
    let ty = parse_type_expr(input)?;
    let mut param = FunctionParam::new(name, ty);
    param.is_const = is_const;
    param.is_context = is_context;
    if expect_symbol(input, "=").is_ok() {
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

fn parse_fn_param_name(input: &mut &[Token]) -> ModalResult<Ident> {
    let mut probe = *input;
    let simple_name = ident_like(&mut probe)?;
    let mut destructured = probe;
    if expect_symbol(&mut destructured, "(").is_ok() {
        let inner_name = ident_like(&mut destructured)?;
        expect_symbol(&mut destructured, ")")?;
        *input = destructured;
        return Ok(inner_name);
    }
    *input = probe;
    Ok(simple_name)
}

fn skip_outer_attrs_for_field(input: &mut &[Token]) -> ModalResult<()> {
    loop {
        let mut probe = *input;
        if expect_symbol(&mut probe, "#").is_err() {
            return Ok(());
        }
        expect_symbol(&mut probe, "[")?;
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
        [first, second, ..]
            if first.kind == TokenKind::Keyword(Keyword::Unsafe)
                && second.kind == TokenKind::Keyword(Keyword::Fn)
    )
}

fn starts_unsafe_impl(input: &[Token]) -> bool {
    matches!(
        input,
        [first, second, ..]
            if first.kind == TokenKind::Keyword(Keyword::Unsafe)
                && second.kind == TokenKind::Keyword(Keyword::Impl)
    )
}

fn skip_where_clause(input: &mut &[Token]) -> ModalResult<()> {
    while !input.is_empty() {
        if peek_symbol(input) == Some("{") {
            return Ok(());
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
    expect_keyword(input, Keyword::Use)?;
    let tree = parse_use_tree(input)?;
    expect_symbol(input, ";")?;
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
    expect_keyword(input, Keyword::Extern)?;
    expect_keyword(input, Keyword::Crate)?;
    let crate_name = ident_like(input)?;
    let tree = if expect_keyword(input, Keyword::As).is_ok() {
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
    expect_symbol(input, ";")?;
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
    expect_keyword(&mut probe, Keyword::Extern)?;
    if expect_keyword(&mut probe, Keyword::Crate).is_ok() {
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
    expect_keyword(input, Keyword::Extern)?;
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
    expect_keyword(input, Keyword::Fn)?;
    let name = ident_like(input)?;
    let generics_params = parse_optional_generic_params(input)?;
    expect_symbol(input, "(")?;
    let params = parse_fn_params(input)?;
    expect_symbol(input, ")")?;
    let ret_ty = if expect_symbol(input, "->").is_ok() {
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
    if expect_symbol(input, ";").is_ok() {
        return Ok(Item::from(ItemKind::DeclFunction(ItemDeclFunction {
            attrs,
            ty_annotation: None,
            name,
            sig,
        })));
    }
    let body = parse_block_expr(input, file)?;
    Ok(Item::from(ItemKind::DefFunction(ItemDefFunction {
        ty_annotation: None,
        attrs,
        name,
        ty: None,
        sig,
        body: Box::new(body),
        visibility,
    })))
}

fn parse_extern_block_items(input: &mut &[Token], file: FileId) -> ModalResult<Vec<Item>> {
    let abi = parse_extern_abi(input)?;
    expect_symbol(input, "{")?;
    let mut items = Vec::new();
    while peek_symbol(input) != Some("}") {
        let attrs = parse_outer_attrs(input, file)?;
        let visibility = parse_visibility(input)?;
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
        return Err(ErrMode::Cut(ContextError::new()));
    }
    expect_symbol(input, "}")?;
    Ok(items)
}

fn parse_unsafe_extern_block_items(input: &mut &[Token], file: FileId) -> ModalResult<Vec<Item>> {
    expect_keyword(input, Keyword::Unsafe)?;
    parse_extern_block_items(input, file)
}

fn parse_prefixed_unsafe_extern_block_items(
    input: &mut &[Token],
    file: FileId,
) -> ModalResult<Vec<Item>> {
    let _ = parse_outer_attrs(input, file)?;
    parse_unsafe_extern_block_items(input, file)
}

fn parse_abi_fn_item(
    input: &mut &[Token],
    file: FileId,
    visibility: Visibility,
    attrs: Vec<Attribute>,
    abi: fp_core::ast::Abi,
) -> ModalResult<Item> {
    expect_keyword(input, Keyword::Fn)?;
    let name = ident_like(input)?;
    let generics_params = parse_optional_generic_params(input)?;
    expect_symbol(input, "(")?;
    let params = parse_fn_params(input)?;
    expect_symbol(input, ")")?;
    let ret_ty = if expect_symbol(input, "->").is_ok() {
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
    if expect_symbol(input, ";").is_ok() {
        return Ok(Item::from(ItemKind::DeclFunction(ItemDeclFunction {
            attrs,
            ty_annotation: None,
            name,
            sig,
        })));
    }
    let body = parse_block_expr(input, file)?;
    Ok(Item::from(ItemKind::DefFunction(ItemDefFunction {
        ty_annotation: None,
        attrs,
        name,
        ty: None,
        sig,
        body: Box::new(body),
        visibility,
    })))
}

fn parse_enum_item(
    input: &mut &[Token],
    visibility: Visibility,
    attrs: Vec<Attribute>,
) -> ModalResult<Item> {
    expect_keyword(input, Keyword::Enum)?;
    let name = ident_like(input)?;
    let generics_params = parse_optional_generic_params(input)?;
    expect_symbol(input, "{")?;
    let mut variants = Vec::new();
    while peek_symbol(input) != Some("}") {
        skip_outer_attrs_for_field(input)?;
        let variant_name = ident_like(input)?;
        let value = if expect_symbol(input, "{").is_ok() {
            let mut fields = Vec::new();
            while peek_symbol(input) != Some("}") {
                skip_outer_attrs_for_field(input)?;
                let _field_visibility = parse_visibility(input)?;
                let field_name = ident_like(input)?;
                let is_optional = expect_symbol(input, "?").is_ok();
                expect_symbol(input, ":")?;
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
                if expect_symbol(input, ",").is_err() {
                    break;
                }
                if peek_symbol(input) == Some("}") {
                    break;
                }
            }
            expect_symbol(input, "}")?;
            Ty::Structural(fp_core::ast::TypeStructural { fields }.into())
        } else if expect_symbol(input, "(").is_ok() {
            let mut tys = Vec::new();
            if peek_symbol(input) != Some(")") {
                loop {
                    skip_outer_attrs_for_field(input)?;
                    tys.push(parse_type_expr(input)?);
                    if expect_symbol(input, ",").is_err() {
                        break;
                    }
                    if peek_symbol(input) == Some(")") {
                        break;
                    }
                }
            }
            expect_symbol(input, ")")?;
            if tys.len() == 1 {
                tys.pop().expect("single enum variant type")
            } else {
                Ty::Tuple(fp_core::ast::TypeTuple { types: tys }.into())
            }
        } else {
            Ty::unit()
        };
        let discriminant = if expect_symbol(input, "=").is_ok() {
            Some(Box::new(parse_expr_winnow_no_struct(input, 0)?))
        } else {
            None
        };
        variants.push(EnumTypeVariant {
            name: variant_name,
            value,
            discriminant,
        });
        if expect_symbol(input, ",").is_err() {
            break;
        }
    }
    expect_symbol(input, "}")?;
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
    expect_keyword(input, Keyword::Mod)?;
    let name = ident_like(input)?;
    if expect_symbol(input, ";").is_ok() {
        return Ok(Item::from(ItemKind::Module(Module {
            attrs,
            name,
            items: Vec::new(),
            visibility,
            is_external: true,
        })));
    }
    expect_symbol(input, "{")?;
    attrs.extend(parse_inner_attrs(input, file)?);
    let mut items = Vec::new();
    while peek_symbol(input) != Some("}") {
        items.push(parse_item_winnow(input, file)?);
    }
    expect_symbol(input, "}")?;
    Ok(Item::from(ItemKind::Module(Module {
        attrs,
        name,
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
    expect_keyword(input, Keyword::Opaque)?;
    expect_keyword(input, Keyword::Type)?;
    let name = ident_like(input)?;
    expect_symbol(input, ";")?;
    Ok(Item::from(ItemKind::OpaqueType(ItemOpaqueType {
        attrs,
        visibility,
        name,
    })))
}

fn parse_item_macro(input: &mut &[Token], _attrs: Vec<Attribute>) -> ModalResult<Item> {
    let path = parse_macro_path(input)?;
    expect_symbol(input, "!")?;
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
    if expect_keyword(&mut probe, Keyword::Pub).is_err() {
        return Ok(Visibility::Public);
    }
    if expect_symbol(&mut probe, "(").is_err() {
        *input = probe;
        return Ok(Visibility::Public);
    }
    let visibility = if expect_keyword(&mut probe, Keyword::Crate).is_ok() {
        Visibility::Crate
    } else if peek_ident_like(probe) == Some("self") {
        let _ = ident_like(&mut probe)?;
        Visibility::Restricted(single_segment_path(fp_core::ast::ItemImportTree::SelfMod))
    } else if expect_keyword(&mut probe, Keyword::Super).is_ok() {
        Visibility::Restricted(single_segment_path(fp_core::ast::ItemImportTree::SuperMod))
    } else if expect_keyword(&mut probe, Keyword::In).is_ok() {
        Visibility::Restricted(parse_use_path(&mut probe)?)
    } else {
        return Err(ErrMode::Cut(ContextError::new()));
    };
    expect_symbol(&mut probe, ")")?;
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
            if expect_symbol(&mut probe, "#![").is_err() {
                let mut split_probe = *input;
                if expect_symbol(&mut split_probe, "#").is_err() {
                    break;
                }
                if expect_symbol(&mut split_probe, "!").is_err()
                    || expect_symbol(&mut split_probe, "[").is_err()
                {
                    break;
                }
                probe = split_probe;
            }
        } else if expect_symbol(&mut probe, "#[").is_err() {
            if expect_symbol(&mut probe, "#").is_err() {
                break;
            }
            expect_symbol(&mut probe, "[")?;
        }
        let meta = parse_attr_meta_direct(&mut probe, file)?;
        expect_symbol(&mut probe, "]")?;
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
    Ok(attrs)
}

fn const_struct_attr() -> Attribute {
    Attribute {
        style: AttrStyle::Outer,
        meta: AttrMeta::Path(Path::plain(vec![Ident::new("const")])),
    }
}

fn parse_attr_meta_direct(input: &mut &[Token], file: FileId) -> ModalResult<AttrMeta> {
    let name = parse_module_path(input)?;
    if expect_symbol(input, "=").is_ok() {
        let value = parse_expr_winnow_no_struct(input, file)?;
        return Ok(AttrMeta::NameValue(AttrMetaNameValue {
            name,
            value: Box::new(value),
        }));
    }
    if expect_symbol(input, "(").is_ok() {
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
            if expect_symbol(input, ",").is_err() {
                break;
            }
        }
        expect_symbol(input, ")")?;
        return Ok(AttrMeta::List(AttrMetaList { name, items }));
    }
    Ok(AttrMeta::Path(name))
}

fn looks_like_item_macro(input: &[Token]) -> bool {
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
