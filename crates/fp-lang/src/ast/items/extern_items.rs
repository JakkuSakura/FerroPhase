use super::super::*;
use super::{parse_fn_params, parse_function_block, parse_visibility, peek_keyword};

pub(super) fn parse_extern_crate_item(
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

pub(super) fn parse_extern_item(
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
    if peek_keyword(*input, Keyword::Static) {
        if !abi.is_named("host") {
            return Err(ErrMode::Backtrack(ContextError::new()));
        }
        return parse_extern_static_decl(input, true);
    }
    if peek_symbol(input) == Some("{") {
        let items = parse_extern_block_items(input, file)?;
        return items
            .into_iter()
            .next()
            .ok_or_else(|| ErrMode::Cut(ContextError::new()));
    }
    Err(ErrMode::Backtrack(ContextError::new()))
}

pub(super) fn parse_extern_abi(input: &mut &[Token]) -> ModalResult<fp_core::ast::Abi> {
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
    let is_host = abi.is_named("host");
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
    if is_host {
        return Err(ErrMode::Cut(ContextError::new()));
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

pub(crate) fn parse_extern_block_items(
    input: &mut &[Token],
    file: FileId,
) -> ModalResult<Vec<Item>> {
    let abi = parse_extern_abi(input)?;
    skip_symbol(input, "{")?;
    let mut items = Vec::new();
    while peek_symbol(input) != Some("}") {
        let attrs = parse_outer_attrs(input, file)?;
        let visibility = parse_visibility(input)?;
        if matches!(input.first(), Some(token) if token.kind == TokenKind::Ident && token.lexeme == "safe")
        {
            *input = &input[1..];
        }
        let _ = skip_keyword(input, Keyword::Unsafe);
        if peek_keyword(*input, Keyword::Fn) {
            items.push(parse_abi_fn_item(
                input,
                file,
                visibility,
                attrs,
                abi.clone(),
            )?);
        } else if peek_keyword(*input, Keyword::Static) {
            items.push(parse_extern_static_decl(input, abi.is_named("host"))?);
        } else if skip_keyword(input, Keyword::Type).is_ok() {
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
        } else {
            return Err(ErrMode::Cut(ContextError::new()));
        }
    }
    skip_symbol(input, "}")?;
    Ok(items)
}

fn parse_unsafe_extern_block_items(input: &mut &[Token], file: FileId) -> ModalResult<Vec<Item>> {
    skip_keyword(input, Keyword::Unsafe)?;
    parse_extern_block_items(input, file)
}

pub(crate) fn parse_prefixed_unsafe_extern_block_items(
    input: &mut &[Token],
    file: FileId,
) -> ModalResult<Vec<Item>> {
    let _ = parse_outer_attrs(input, file)?;
    parse_unsafe_extern_block_items(input, file)
}

fn parse_extern_static_decl(input: &mut &[Token], is_host: bool) -> ModalResult<Item> {
    skip_keyword(input, Keyword::Static)?;
    let mutable = skip_keyword(input, Keyword::Mut).is_ok();
    let name = ident_like(input)?;
    skip_symbol(input, ":")?;
    let ty = parse_type_expr(input)?;
    skip_symbol(input, ";")?;
    Ok(Item::from(ItemKind::DeclStatic(ItemDeclStatic {
        ty_annotation: None,
        mutable,
        is_host,
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
    parse_extern_fn_item(input, file, visibility, attrs, abi)
}
