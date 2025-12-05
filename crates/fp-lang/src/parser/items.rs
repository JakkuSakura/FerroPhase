use fp_core::ast::{
    AttrMeta, AttrStyle, Attribute, EnumTypeVariant, Expr, ExprKind, FunctionParam,
    FunctionParamReceiver, FunctionSignature, GenericParam, Ident, Item, Item as AstItem,
    ItemDeclConst, ItemDeclFunction, ItemDeclType, ItemDefConst, ItemDefEnum, ItemDefFunction,
    ItemDefStatic, ItemDefStruct, ItemDefTrait, ItemDefType, ItemImpl, ItemImport, ItemImportGroup,
    ItemImportPath, ItemImportRename, ItemImportTree, ItemKind, ItemMacro, Locator,
    MacroDelimiter, MacroInvocation, Module, ParameterPath, ParameterPathSegment, Path,
    StructuralField, Ty, TypeArray, TypeBinaryOp, TypeBinaryOpKind, TypeBounds, TypeEnum,
    TypeFunction, TypeReference, TypeSlice, TypeStruct, TypeStructural, TypeTuple, ImplTraits,
    Value, Visibility,
};
use std::cell::Cell;
use std::thread_local;
use thiserror::Error;
use winnow::combinator::alt;
use winnow::error::{ContextError, ErrMode};
use winnow::ModalResult;
use winnow::Parser;

use super::expr;
use crate::parser::expr::parse_macro_invocation;
use crate::lexer::winnow::backtrack_err;
use crate::lexer::{self, Keyword, LexerError, Token, TokenKind};
use crate::parser::expr::match_keyword;

// When parsing nested generic arguments, Rust allows lexing `>>` as a single
// token. Track any "extra" closing `>` that result from splitting such a token
// so outer contexts can still observe their closing delimiter.
thread_local! {
    static PENDING_GT: Cell<usize> = Cell::new(0);
}

/// Parse a simple path-like type (`foo::bar::Baz`) into both a
/// `Path` and a `Ty` that wraps that path. This helper is shared
/// between item parsing and expression-level constructs such as
/// closure parameter type annotations to keep the surface type
/// grammar consistent.
pub(crate) fn parse_path_as_ty(input: &mut &[Token]) -> ModalResult<(Path, Ty)> {
    let mut segments = Vec::new();
    segments.push(expect_type_ident_segment(input)?);
    while match_symbol(input, "::") {
        segments.push(expect_type_ident_segment(input)?);
    }

    // Optional generic arguments apply to the final segment: Path<...>
    let mut generic_args: Vec<Ty> = Vec::new();
    if match_symbol(input, "<") {
        loop {
            // Support both positional type args and associated bindings like `Output = T`.
            let arg_ty = if matches!(
                input.first(),
                Some(Token {
                    kind: TokenKind::Ident,
                    ..
                })
            ) {
                // Look ahead for `ident = type`
                let mut look = *input;
                let _name_tok = advance(&mut look);
                if match_symbol(&mut look, "=") {
                    // consume name and '='
                    let _ = expect_ident(input)?;
                    expect_symbol(input, "=")?;
                    parse_type(input)?
                } else {
                    parse_type(input)?
                }
            } else {
                parse_type(input)?
            };
            generic_args.push(arg_ty);
            if match_symbol(input, ">") {
                break;
            }
            expect_symbol(input, ",")?;
            // allow trailing comma before '>'
            if match_symbol(input, ">") {
                break;
            }
        }
    }

    let path = Path::new(segments.clone());
    let ty = if generic_args.is_empty() {
        Ty::path(path.clone())
    } else {
        // Build ParameterPath with args on the last segment.
        let mut param_segments: Vec<ParameterPathSegment> = segments
            .into_iter()
            .map(ParameterPathSegment::from_ident)
            .collect();
        if let Some(last) = param_segments.last_mut() {
            last.args = generic_args;
        }
        let ppath = ParameterPath::new(param_segments);
        Ty::locator(Locator::parameter_path(ppath))
    };
    Ok((path, ty))
}

pub(crate) fn expect_type_ident_segment(input: &mut &[Token]) -> ModalResult<Ident> {
    match input.first() {
        Some(Token {
            kind: TokenKind::Ident,
            lexeme,
            ..
        }) => {
            let name = lexeme.clone();
            *input = &input[1..];
            Ok(Ident::new(name))
        }
        Some(Token {
            kind: TokenKind::Keyword(Keyword::Crate),
            ..
        }) => {
            *input = &input[1..];
            Ok(Ident::new("crate".to_string()))
        }
        Some(Token {
            kind: TokenKind::Keyword(Keyword::Super),
            ..
        }) => {
            *input = &input[1..];
            Ok(Ident::new("super".to_string()))
        }
        _ => Err(ErrMode::Cut(ContextError::new())),
    }
}

#[derive(Debug, Error)]
pub enum ItemParseError {
    #[error("lex error: {0}")]
    Lex(#[from] LexerError),
    #[error("parse error: {0}")]
    Parse(String),
}

impl From<ErrMode<ContextError>> for ItemParseError {
    fn from(err: ErrMode<ContextError>) -> Self {
        match err {
            ErrMode::Backtrack(ctx) | ErrMode::Cut(ctx) => ItemParseError::Parse(ctx.to_string()),
            ErrMode::Incomplete(_) => ItemParseError::Parse("incomplete input".to_string()),
        }
    }
}

pub fn parse_items(source: &str) -> Result<Vec<AstItem>, ItemParseError> {
    // Reset any pending split `>` state before a fresh parse.
    reset_pending_gt();
    let tokens = lexer::lex(source)?;
    let mut input: &[Token] = tokens.as_slice();
    let mut items = Vec::new();
    while !input.is_empty() {
        // skip stray semicolons
        if matches_symbol(input.first(), ";") {
            input = &input[1..];
            continue;
        }
        let attrs = parse_outer_attrs(&mut input)?;
        let start_tok = input.first().cloned();
        let mut item = match parse_item(&mut input) {
            Ok(it) => it,
            Err(_) => {
                let msg = match start_tok {
                    Some(tok) => format!("failed to parse item starting at token '{}'", tok.lexeme),
                    None => "failed to parse item at end of input".to_string(),
                };
                return Err(ItemParseError::Parse(msg));
            }
        };
        if !attrs.is_empty() {
            if let ItemKind::DefFunction(def) = item.kind_mut() {
                def.attrs = attrs;
            }
        }
        items.push(item);
    }
    Ok(items)
}

/// Parse a single item at the current position.
/// Visible to expr parser so block bodies can host items.
pub(crate) fn parse_item(input: &mut &[Token]) -> ModalResult<AstItem> {
    // Optional leading visibility
    let mut cursor = *input;
    let visibility = parse_visibility(&mut cursor)?;

    // Special-case `const fn` and plain `const` items with lookahead.
    if matches!(
        cursor.first(),
        Some(Token {
            kind: TokenKind::Keyword(Keyword::Const),
            ..
        })
    ) {
        if matches!(
            cursor.get(1),
            Some(Token {
                kind: TokenKind::Keyword(Keyword::Fn),
                ..
            })
        ) {
            // Consume `const` and parse the following `fn`.
            let mut fn_cursor = &cursor[1..];
            let mut item = parse_fn_item(&mut fn_cursor)?;
            apply_visibility(&mut item, visibility.clone());
            if let ItemKind::DefFunction(def) = item.kind_mut() {
                def.sig.is_const = true;
            }
            *input = fn_cursor;
            return Ok(item);
        } else {
            let mut item = parse_const_item(&mut cursor)?;
            apply_visibility(&mut item, visibility.clone());
            *input = cursor;
            return Ok(item);
        }
    }

    let mut item = alt((
        parse_extern_crate_item,
        parse_use_item,
        parse_mod_item,
        parse_trait_item,
        parse_impl_item,
        parse_struct_item,
        parse_enum_item,
        parse_type_item,
        parse_const_item,
        parse_static_item,
        parse_fn_item,
        parse_item_macro,
        parse_expr_item,
    ))
    .parse_next(&mut cursor)?;

    apply_visibility(&mut item, visibility);

    *input = cursor;
    Ok(item)
}

fn apply_visibility(item: &mut AstItem, visibility: Visibility) {
    use ItemKind::*;
    match item.kind_mut() {
        Module(module) => module.visibility = visibility.clone(),
        DefStruct(def) => def.visibility = visibility.clone(),
        DefStructural(def) => def.visibility = visibility.clone(),
        DefEnum(def) => def.visibility = visibility.clone(),
        DefType(def) => def.visibility = visibility.clone(),
        DefConst(def) => def.visibility = visibility.clone(),
        DefStatic(def) => def.visibility = visibility.clone(),
        DefFunction(def) => def.visibility = visibility.clone(),
        DefTrait(def) => def.visibility = visibility.clone(),
        Import(import) => import.visibility = visibility.clone(),
        _ => {}
    }
}

fn parse_visibility(input: &mut &[Token]) -> ModalResult<Visibility> {
    let mut cursor = *input;
    if !match_keyword(&mut cursor, Keyword::Pub) {
        return Ok(Visibility::Public);
    }

    // Handle pub(...) forms
    if match_symbol(&mut cursor, "(") {
        if match_keyword(&mut cursor, Keyword::Crate) {
            expect_symbol(&mut cursor, ")")?;
            *input = cursor;
            return Ok(Visibility::Crate);
        }
        if match_keyword(&mut cursor, Keyword::In) {
            let path = parse_visibility_path(&mut cursor)?;
            expect_symbol(&mut cursor, ")")?;
            *input = cursor;
            return Ok(Visibility::Restricted(path));
        }
        if match_keyword(&mut cursor, Keyword::Super) {
            expect_symbol(&mut cursor, ")")?;
            let mut path = ItemImportPath::new();
            path.push(ItemImportTree::SuperMod);
            *input = cursor;
            return Ok(Visibility::Restricted(path));
        }
        if matches!(
            cursor.first(),
            Some(Token {
                kind: TokenKind::Ident,
                lexeme,
                ..
            }) if lexeme == "self"
        ) {
            advance(&mut cursor);
            expect_symbol(&mut cursor, ")")?;
            let mut path = ItemImportPath::new();
            path.push(ItemImportTree::SelfMod);
            *input = cursor;
            return Ok(Visibility::Restricted(path));
        }
        // Unknown pub(..) form: consume to ')' to avoid infinite loops, default to Public.
        while !matches_symbol(cursor.first(), ")") {
            if advance(&mut cursor).is_none() {
                return Err(ErrMode::Cut(ContextError::new()));
            }
        }
        expect_symbol(&mut cursor, ")")?;
        *input = cursor;
        return Ok(Visibility::Public);
    }

    *input = cursor;
    Ok(Visibility::Public)
}

fn parse_visibility_path(input: &mut &[Token]) -> ModalResult<ItemImportPath> {
    let mut path = ItemImportPath::new();
    if match_symbol(input, "::") {
        path.push(ItemImportTree::Root);
    }

    loop {
        if match_keyword(input, Keyword::Crate) {
            path.push(ItemImportTree::Crate);
        } else if match_keyword(input, Keyword::Super) {
            path.push(ItemImportTree::SuperMod);
        } else if matches!(
            input.first(),
            Some(Token {
                kind: TokenKind::Ident,
                lexeme,
                ..
            }) if lexeme == "self"
        ) {
            advance(input);
            path.push(ItemImportTree::SelfMod);
        } else if matches!(input.first(), Some(Token { kind: TokenKind::Ident, .. })) {
            let name = expect_ident(input)?;
            path.push(ItemImportTree::Ident(Ident::new(name)));
        } else {
            return Err(ErrMode::Cut(ContextError::new()));
        }

        if match_symbol(input, "::") {
            continue;
        }
        break;
    }

    Ok(path)
}

pub(crate) fn parse_mod_item(input: &mut &[Token]) -> ModalResult<AstItem> {
    keyword_parser(Keyword::Mod).parse_next(input)?;
    let name = Ident::new(expect_ident(input)?);

    // Handle `mod foo;` as an empty module for now.
    if match_symbol(input, ";") {
        let module = Module {
            name,
            items: Vec::new(),
            visibility: Visibility::Inherited,
        };
        return Ok(Item::from(ItemKind::Module(module)));
    }

    expect_symbol(input, "{")?;
    let mut items = Vec::new();
    loop {
        if matches_symbol(input.first(), "}") {
            expect_symbol(input, "}")?;
            break;
        }
        if input.is_empty() {
            return Err(ErrMode::Cut(ContextError::new()));
        }
        if matches_symbol(input.first(), ";") {
            // Skip stray semicolons inside modules.
            advance(input);
            continue;
        }

        let attrs = parse_outer_attrs(input)?;
        let mut item = parse_item(input)?;
        if !attrs.is_empty() {
            if let ItemKind::DefFunction(def) = item.kind_mut() {
                def.attrs = attrs;
            }
        }
        items.push(item);
    }

    let module = Module {
        name,
        items,
        visibility: Visibility::Public,
    };
    Ok(Item::from(ItemKind::Module(module)))
}

pub(crate) fn parse_trait_item(input: &mut &[Token]) -> ModalResult<AstItem> {
    keyword_parser(Keyword::Trait).parse_next(input)?;
    let name = Ident::new(expect_ident(input)?);

    // Optional trait generics: trait Foo<T>
    if matches_symbol(input.first(), "<") {
        let _ = parse_generic_params(input)?;
    }

    // Supertrait bounds like `trait Foo: Bar + Baz`.
    let bounds = if match_symbol(input, ":") {
        parse_trait_bounds(input)?
    } else {
        TypeBounds::any()
    };

    // Parse trait body into declaration items.
    expect_symbol(input, "{")?;
    let mut trait_items = Vec::new();
    loop {
        if matches_symbol(input.first(), "}") {
            expect_symbol(input, "}")?;
            break;
        }
        if input.is_empty() {
            return Err(ErrMode::Cut(ContextError::new()));
        }
        if matches_symbol(input.first(), ";") {
            advance(input);
            continue;
        }

        let _attrs = parse_outer_attrs(input)?;
        let item = parse_trait_member(input)?;
        trait_items.push(item);
    }

    let def = ItemDefTrait {
        name,
        bounds,
        items: trait_items,
        visibility: Visibility::Public,
    };
    Ok(Item::from(ItemKind::DefTrait(def)))
}

fn parse_trait_bounds(input: &mut &[Token]) -> ModalResult<TypeBounds> {
    let mut bounds_exprs = Vec::new();
    loop {
        // Parse one path-like bound
        let (path, _ty) = parse_path_as_ty(input)?;
        let expr = Expr::locator(Locator::path(path));
        bounds_exprs.push(expr);

        // Continue on '+' or stop before '{'
        if match_symbol(input, "+") {
            continue;
        }
        break;
    }
    Ok(TypeBounds {
        bounds: bounds_exprs,
    })
}

fn parse_trait_member(input: &mut &[Token]) -> ModalResult<Item> {
    alt((
        parse_trait_fn_decl,
        parse_trait_type_decl,
        parse_trait_const_decl,
    ))
    .parse_next(input)
}

fn parse_trait_fn_decl(input: &mut &[Token]) -> ModalResult<Item> {
    keyword_parser(Keyword::Fn).parse_next(input)?;
    let name = Ident::new(expect_ident(input)?);

    // Optional generics: fn foo<T, U: Bound>(...)
    let generics = if matches_symbol(input.first(), "<") {
        parse_generic_params(input)?
    } else {
        Vec::new()
    };

    // Parameter list
    expect_symbol(input, "(")?;
    let mut params = Vec::new();
    let mut receiver: Option<FunctionParamReceiver> = None;
    if matches_symbol(input.first(), ")") {
        expect_symbol(input, ")")?;
    } else {
        loop {
            // Support receivers: &self / &mut self / self / mut self
            let mut snapshot = *input;
            let recv = if match_symbol(&mut snapshot, "&") {
                let is_mut = match_keyword(&mut snapshot, Keyword::Mut);
                if matches!(
                    snapshot.first(),
                    Some(Token {
                        kind: TokenKind::Ident,
                        lexeme,
                        ..
                    }) if lexeme == "self"
                ) {
                    snapshot = &snapshot[1..];
                    Some(if is_mut {
                        FunctionParamReceiver::RefMut
                    } else {
                        FunctionParamReceiver::Ref
                    })
                } else {
                    None
                }
            } else if match_keyword(&mut snapshot, Keyword::Mut) {
                if matches!(
                    snapshot.first(),
                    Some(Token {
                        kind: TokenKind::Ident,
                        lexeme,
                        ..
                    }) if lexeme == "self"
                ) {
                    snapshot = &snapshot[1..];
                    Some(FunctionParamReceiver::MutValue)
                } else {
                    None
                }
            } else if matches!(
                snapshot.first(),
                Some(Token {
                    kind: TokenKind::Ident,
                    lexeme,
                    ..
                }) if lexeme == "self"
            ) {
                snapshot = &snapshot[1..];
                Some(FunctionParamReceiver::Value)
            } else {
                None
            };

            if recv.is_some() && receiver.is_none() {
                receiver = recv;
                *input = snapshot;
            } else {
                // optional leading `const` in params
                let param_is_const = match_keyword(input, Keyword::Const);
                let param_name = Ident::new(expect_ident(input)?);
                expect_symbol(input, ":")?;
                let ty = parse_type(input)?;
                let mut param = FunctionParam::new(param_name, ty);
                param.is_const = param_is_const;
                params.push(param);
            }
            if match_symbol(input, ")") {
                break;
            }
            expect_symbol(input, ",")?;
        }
    }

    // Optional return type
    let ret_ty = if match_symbol(input, "->") {
        Some(parse_type(input)?)
    } else {
        None
    };

    // Skip any trailing where/default body up to ';' or body.
    let mut brace_depth = 0i32;
    loop {
        if matches_symbol(input.first(), ";") && brace_depth == 0 {
            expect_symbol(input, ";")?;
            break;
        }
        if matches_symbol(input.first(), "{") {
            // Default method body – skip it.
            expect_symbol(input, "{")?;
            brace_depth += 1;
            while brace_depth > 0 {
                let tok = advance(input).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
                if let TokenKind::Symbol = tok.kind {
                    if tok.lexeme == "{" {
                        brace_depth += 1;
                    } else if tok.lexeme == "}" {
                        brace_depth -= 1;
                    }
                }
            }
            break;
        }
        if advance(input).is_none() {
            return Err(ErrMode::Cut(ContextError::new()));
        }
    }

    let mut sig = FunctionSignature::unit();
    sig.name = Some(name.clone());
    sig.generics_params = generics;
    sig.receiver = receiver;
    sig.params = params;
    sig.ret_ty = ret_ty;
    let decl = ItemDeclFunction {
        ty_annotation: None,
        name,
        sig,
    };
    Ok(Item::from(ItemKind::DeclFunction(decl)))
}

fn parse_trait_type_decl(input: &mut &[Token]) -> ModalResult<Item> {
    keyword_parser(Keyword::Type).parse_next(input)?;
    let name = Ident::new(expect_ident(input)?);

    // Skip optional bounds and trailing ';'.
    while !matches_symbol(input.first(), ";") {
        if advance(input).is_none() {
            return Err(ErrMode::Cut(ContextError::new()));
        }
    }
    expect_symbol(input, ";")?;

    let decl = ItemDeclType {
        ty_annotation: None,
        name,
        bounds: TypeBounds::any(),
    };
    Ok(Item::from(ItemKind::DeclType(decl)))
}

fn parse_trait_const_decl(input: &mut &[Token]) -> ModalResult<Item> {
    keyword_parser(Keyword::Const).parse_next(input)?;
    let name = Ident::new(expect_ident(input)?);
    expect_symbol(input, ":")?;
    let ty = parse_type(input)?;
    expect_symbol(input, ";")?;

    let decl = ItemDeclConst {
        ty_annotation: None,
        name,
        ty,
    };
    Ok(Item::from(ItemKind::DeclConst(decl)))
}

fn parse_generic_params(input: &mut &[Token]) -> ModalResult<Vec<GenericParam>> {
    let mut params = Vec::new();
    expect_symbol(input, "<")?;
    loop {
        let name = Ident::new(expect_ident(input)?);

        // Optional bounds `: Bound1 + Bound2 ...`
        let bounds = if match_symbol(input, ":") {
            let mut bound_exprs = Vec::new();
            loop {
                let bound_ty = parse_type(input)?;
                let bound_expr = Expr::value(Value::Type(bound_ty));
                bound_exprs.push(bound_expr);
                if !match_symbol(input, "+") {
                    break;
                }
            }
            TypeBounds {
                bounds: bound_exprs,
            }
        } else {
            TypeBounds::any()
        };

        params.push(GenericParam { name, bounds });

        if match_symbol(input, ">") || matches_symbol(input.first(), "(") {
            break;
        }
        expect_symbol(input, ",")?;
    }
    Ok(params)
}

#[allow(dead_code)]
fn skip_generic_bounds(_input: &mut &[Token]) -> ModalResult<()> {
    // no-op placeholder; bounds are parsed explicitly in parse_generic_params
    Ok(())
}

pub(crate) fn parse_impl_item(input: &mut &[Token]) -> ModalResult<AstItem> {
    keyword_parser(Keyword::Impl).parse_next(input)?;
    // Optional impl generics: impl<T> Trait for Type
    let generics_params = if matches_symbol(input.first(), "<") {
        parse_generic_params(input)?
    } else {
        Vec::new()
    };
    // Parse a path-like type which may be either a trait (in
    // `impl Trait for Type`) or the self type for an inherent
    // impl (`impl Type { ... }`).
    let (first_path, first_ty) = parse_path_as_ty(input)?;

    let (trait_ty, self_ty) = if match_keyword(input, Keyword::For) {
        // Trait impl: `impl Trait for Type { ... }`
        let self_ty = parse_type(input)?;
        (Some(Locator::path(first_path)), self_ty)
    } else {
        // Inherent impl: `impl Type { ... }`
        (None, first_ty)
    };

    expect_symbol(input, "{")?;
    let mut items = Vec::new();
    loop {
        if matches_symbol(input.first(), "}") {
            expect_symbol(input, "}")?;
            break;
        }
        if input.is_empty() {
            return Err(ErrMode::Cut(ContextError::new()));
        }
        if matches_symbol(input.first(), ";") {
            advance(input);
            continue;
        }

        let attrs = parse_outer_attrs(input)?;
        let mut item = parse_item(input)?;
        if !attrs.is_empty() {
            if let ItemKind::DefFunction(def) = item.kind_mut() {
                def.attrs = attrs;
            }
        }
        items.push(item);
    }

    let impl_item = ItemImpl {
        trait_ty,
        self_ty: Expr::value(self_ty.into()),
        generics_params,
        items,
    };

    Ok(Item::from(ItemKind::Impl(impl_item)))
}

pub(crate) fn parse_use_item(input: &mut &[Token]) -> ModalResult<AstItem> {
    keyword_parser(Keyword::Use).parse_next(input)?;
    let tree = parse_use_tree(input)?;
    expect_symbol(input, ";")?;
    let import = ItemImport {
        visibility: Visibility::Public,
        tree,
    };
    Ok(Item::from(ItemKind::Import(import)))
}

pub(crate) fn parse_extern_crate_item(input: &mut &[Token]) -> ModalResult<AstItem> {
    keyword_parser(Keyword::Extern).parse_next(input)?;
    keyword_parser(Keyword::Crate).parse_next(input)?;

    let crate_name = Ident::new(expect_ident(input)?);
    let tree = if match_keyword(input, Keyword::As) {
        let alias = Ident::new(expect_ident(input)?);
        ItemImportTree::Rename(ItemImportRename {
            from: crate_name,
            to: alias,
        })
    } else {
        let mut path = ItemImportPath::new();
        path.push(ItemImportTree::Ident(crate_name));
        ItemImportTree::Path(path)
    };

    expect_symbol(input, ";")?;
    let import = ItemImport {
        visibility: Visibility::Public,
        tree,
    };
    Ok(Item::from(ItemKind::Import(import)))
}

pub(crate) fn parse_struct_item(input: &mut &[Token]) -> ModalResult<AstItem> {
    keyword_parser(Keyword::Struct).parse_next(input)?;
    let name = Ident::new(expect_ident(input)?);
    // Optional generics: struct Foo<T, U> { ... }
    let generics = if matches_symbol(input.first(), "<") {
        parse_generic_params(input)?
    } else {
        Vec::new()
    };
    expect_symbol(input, "{")?;
    let mut fields = Vec::new();
    if matches_symbol(input.first(), "}") {
        expect_symbol(input, "}")?;
    } else {
        loop {
            let field_name = expect_ident(input)?;
            expect_symbol(input, ":")?;
            let ty = parse_type(input)?;
            fields.push(StructuralField::new(Ident::new(field_name), ty));
            if match_symbol(input, "}") {
                break;
            }
            if match_symbol(input, ",") {
                // allow trailing comma before closing brace
                if match_symbol(input, "}") {
                    break;
                }
                continue;
            }
            return Err(ErrMode::Cut(ContextError::new()));
        }
    }
    let def = ItemDefStruct {
        visibility: Visibility::Public,
        name: name.clone(),
        value: TypeStruct {
            name: name.clone(),
            generics_params: generics,
            fields,
        },
    };
    Ok(Item::from(ItemKind::DefStruct(def)))
}

pub(crate) fn parse_const_item(input: &mut &[Token]) -> ModalResult<AstItem> {
    keyword_parser(Keyword::Const).parse_next(input)?;
    let name = Ident::new(expect_ident(input)?);
    let ty = if match_symbol(input, ":") {
        let ty = parse_type(input)?;
        Some(ty)
    } else {
        None
    };

    if !match_symbol(input, "=") {
        return Err(ErrMode::Cut(ContextError::new()));
    }
    let value_expr: Expr = match expr::parse_expr_prec(input, 0) {
        Ok(v) => v,
        Err(e) => return Err(e),
    };
    expect_symbol(input, ";")?;

    let def = ItemDefConst {
        ty_annotation: None,
        visibility: Visibility::Public,
        name,
        ty,
        value: Box::new(value_expr),
    };

    Ok(Item::from(ItemKind::DefConst(def)))
}

pub(crate) fn parse_static_item(input: &mut &[Token]) -> ModalResult<AstItem> {
    keyword_parser(Keyword::Static).parse_next(input)?;
    let name = Ident::new(expect_ident(input)?);
    expect_symbol(input, ":")?;
    let ty = parse_type(input)?;
    expect_symbol(input, "=")?;
    let value_expr: Expr = expr::parse_expr_prec(input, 0)?;
    expect_symbol(input, ";")?;

    let def = ItemDefStatic {
        ty_annotation: None,
        visibility: Visibility::Public,
        name,
        ty,
        value: Box::new(value_expr),
    };

    Ok(Item::from(ItemKind::DefStatic(def)))
}

pub(crate) fn parse_type_item(input: &mut &[Token]) -> ModalResult<AstItem> {
    keyword_parser(Keyword::Type).parse_next(input)?;
    let name = Ident::new(expect_ident(input)?);
    expect_symbol(input, "=")?;
    let is_macro_type = matches!(
        input.first(),
        Some(Token {
            kind: TokenKind::Ident,
            ..
        })
    ) && matches_symbol(input.get(1), "!");
    let ty = if is_macro_type {
        let macro_ident = expect_ident(input)?;
        let path = Path::from_ident(Ident::new(macro_ident));
        let macro_expr = parse_macro_invocation(path, input)?;
        Ty::expr(macro_expr)
    } else {
        match parse_type(input) {
            Ok(t) => t,
            Err(err) => return Err(err),
        }
    };
    expect_symbol(input, ";")?;

    let def = ItemDefType {
        visibility: Visibility::Public,
        name,
        value: ty,
    };

    Ok(Item::from(ItemKind::DefType(def)))
}

pub(crate) fn parse_enum_item(input: &mut &[Token]) -> ModalResult<AstItem> {
    keyword_parser(Keyword::Enum).parse_next(input)?;
    let name = Ident::new(expect_ident(input)?);
    // Optional generics: enum Foo<T> { ... }
    let generics = if matches_symbol(input.first(), "<") {
        parse_generic_params(input)?
    } else {
        Vec::new()
    };
    expect_symbol(input, "{")?;

    let mut variants = Vec::new();
    if matches_symbol(input.first(), "}") {
        expect_symbol(input, "}")?;
    } else {
        loop {
            // allow trailing comma before closing brace
            if match_symbol(input, "}") {
                break;
            }
            let variant_name = Ident::new(expect_ident(input)?);
            // Variant payload: either a simple type after ':' or a
            // tuple-like variant `Variant(T1, T2, ...)` or struct-like `Variant { f: Ty, ... }`.
            let value_ty = if match_symbol(input, ":") {
                parse_type(input)?
            } else if match_symbol(input, "(") {
                // Tuple variant: Variant(T1, T2, ...)
                let mut types = Vec::new();
                if matches_symbol(input.first(), ")") {
                    expect_symbol(input, ")")?;
                } else {
                    loop {
                        let ty = parse_type(input)?;
                        types.push(ty);
                        if match_symbol(input, ")") {
                            break;
                        }
                        expect_symbol(input, ",")?;
                    }
                }
                Ty::Tuple(TypeTuple { types })
            } else if match_symbol(input, "{") {
                // Struct variant: Variant { f: Ty, ... }
                let mut fields = Vec::new();
                if matches_symbol(input.first(), "}") {
                    expect_symbol(input, "}")?;
                } else {
                    loop {
                        let field_name = Ident::new(expect_ident(input)?);
                        expect_symbol(input, ":")?;
                        let field_ty = parse_type(input)?;
                        fields.push(StructuralField::new(field_name, field_ty));
                        if match_symbol(input, "}") {
                            break;
                        }
                        expect_symbol(input, ",")?;
                    }
                }
                Ty::Structural(TypeStructural { fields })
            } else {
                Ty::any()
            };

            // Optional discriminant: `= expr`
            let discriminant = if match_symbol(input, "=") {
                let expr = expr::parse_expr_prec(input, 0)?;
                Some(Box::new(expr))
            } else {
                None
            };

            let variant = EnumTypeVariant {
                name: variant_name,
                value: value_ty,
                discriminant,
            };
            variants.push(variant);
            if match_symbol(input, "}") {
                break;
            }
            expect_symbol(input, ",")?;
        }
    }

    let type_enum = TypeEnum {
        name: name.clone(),
        generics_params: generics,
        variants,
    };
    let def = ItemDefEnum {
        visibility: Visibility::Public,
        name,
        value: type_enum,
    };

    Ok(Item::from(ItemKind::DefEnum(def)))
}

pub(crate) fn parse_fn_item(input: &mut &[Token]) -> ModalResult<AstItem> {
    keyword_parser(Keyword::Fn).parse_next(input)?;
    let name = Ident::new(expect_ident(input)?);

    // Optional generics: fn foo<T, U: Bound>(...)
    let generics = if matches_symbol(input.first(), "<") {
        parse_generic_params(input)?
    } else {
        Vec::new()
    };

    // Parameters
    expect_symbol(input, "(")?;
    let mut params = Vec::new();
    let mut receiver: Option<FunctionParamReceiver> = None;
        if matches_symbol(input.first(), ")") {
            expect_symbol(input, ")")?;
        } else {
            loop {
                // Allow const-qualified params
                let param_is_const = match_keyword(input, Keyword::Const);
                // Receiver forms: self / mut self / &self / &mut self
                if receiver.is_none() {
                    let mut snapshot = *input;
                    let recv = if match_symbol(&mut snapshot, "&") {
                        let is_mut = match_keyword(&mut snapshot, Keyword::Mut);
                    if matches!(
                        snapshot.first(),
                        Some(Token {
                            kind: TokenKind::Ident,
                            lexeme,
                            ..
                        }) if lexeme == "self"
                    ) {
                        snapshot = &snapshot[1..];
                        Some(if is_mut {
                            FunctionParamReceiver::RefMut
                        } else {
                            FunctionParamReceiver::Ref
                        })
                    } else {
                        None
                    }
                } else if match_keyword(&mut snapshot, Keyword::Mut) {
                    if matches!(
                        snapshot.first(),
                        Some(Token {
                            kind: TokenKind::Ident,
                            lexeme,
                            ..
                        }) if lexeme == "self"
                    ) {
                        snapshot = &snapshot[1..];
                        Some(FunctionParamReceiver::MutValue)
                    } else {
                        None
                    }
                } else if matches!(
                    snapshot.first(),
                    Some(Token {
                        kind: TokenKind::Ident,
                        lexeme,
                        ..
                    }) if lexeme == "self"
                ) {
                    snapshot = &snapshot[1..];
                    Some(FunctionParamReceiver::Value)
                } else {
                    None
                };

                    if let Some(r) = recv {
                        receiver = Some(r);
                        *input = snapshot;
                    } else {
                        // normal param
                        let param_name = Ident::new(expect_ident(input)?);
                        expect_symbol(input, ":")?;
                        let ty = parse_type(input)?;
                        let mut param = FunctionParam::new(param_name, ty);
                        param.is_const = param_is_const;
                        params.push(param);
                    }
                } else {
                    // normal param
                    let param_name = Ident::new(expect_ident(input)?);
                    expect_symbol(input, ":")?;
                    let ty = parse_type(input)?;
                    let mut param = FunctionParam::new(param_name, ty);
                    param.is_const = param_is_const;
                    params.push(param);
                }

            if match_symbol(input, ")") {
                break;
            }
            expect_symbol(input, ",")?;
        }
    }

    // Optional return type
    let ret_ty = if match_symbol(input, "->") {
        Some(parse_type(input)?)
    } else {
        None
    };

    // Optional where clause – skip until '{'
    if match_keyword(input, Keyword::Where) {
        while !matches_symbol(input.first(), "{") {
            if advance(input).is_none() {
                return Err(ErrMode::Cut(ContextError::new()));
            }
        }
    }

    let body_block = expr::parse_block(input)?;
    let body_expr: Expr = ExprKind::Block(body_block).into();
    let mut def = ItemDefFunction::new_simple(name.clone(), body_expr.into());
    def.sig.name = Some(name);
    def.sig.generics_params = generics;
    def.sig.params = params;
    def.sig.ret_ty = ret_ty;
    Ok(Item::from(ItemKind::DefFunction(def)))
}

fn parse_expr_item(input: &mut &[Token]) -> ModalResult<AstItem> {
    // Fallback: treat expression as an item expression (useful for tests)
    let expr_ast = expr::parse_expr_prec(input, 0)?;
    Ok(Item::from(ItemKind::Expr(expr_ast)))
}

fn parse_use_tree(input: &mut &[Token]) -> ModalResult<ItemImportTree> {
    // Top-level group: use { foo, bar::baz };
    if match_symbol(input, "{") {
        let group = parse_use_group(input)?;
        return Ok(ItemImportTree::Group(group));
    }

    let mut path = ItemImportPath::new();

    // Optional leading root `::`
    if match_symbol(input, "::") {
        path.push(ItemImportTree::Root);
    }

    loop {
        let seg = parse_use_segment(input)?;
        path.push(seg);

        // Optional rename: foo as bar
        if match_keyword(input, Keyword::As) {
            let alias = Ident::new(expect_ident(input)?);
            let from = match path.segments.pop() {
                Some(ItemImportTree::Ident(id)) => id,
                Some(ItemImportTree::SelfMod) => Ident::new("self"),
                Some(ItemImportTree::Crate) => Ident::new("crate"),
                Some(ItemImportTree::SuperMod) => Ident::new("super"),
                _ => return Err(ErrMode::Cut(ContextError::new())),
            };
            let rename = ItemImportTree::Rename(ItemImportRename { from, to: alias });
            path.push(rename);
        }

        if match_symbol(input, "::") {
            if match_symbol(input, "{") {
                let group = parse_use_group(input)?;
                path.push(ItemImportTree::Group(group));
                break;
            } else if match_symbol(input, "*") {
                path.push(ItemImportTree::Glob);
                break;
            } else {
                continue;
            }
        }
        break;
    }

    Ok(ItemImportTree::Path(path))
}

fn parse_use_segment(input: &mut &[Token]) -> ModalResult<ItemImportTree> {
    let tok = advance(input).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
    match tok.kind {
        TokenKind::Keyword(Keyword::Crate) => Ok(ItemImportTree::Crate),
        TokenKind::Keyword(Keyword::Super) => Ok(ItemImportTree::SuperMod),
        TokenKind::Ident if tok.lexeme == "self" => Ok(ItemImportTree::SelfMod),
        TokenKind::Ident => Ok(ItemImportTree::Ident(Ident::new(tok.lexeme))),
        _ => Err(ErrMode::Cut(ContextError::new())),
    }
}

fn parse_use_group(input: &mut &[Token]) -> ModalResult<ItemImportGroup> {
    let mut group = ItemImportGroup::new();
    loop {
        if match_symbol(input, "}") {
            break;
        }
        let tree = parse_use_tree(input)?;
        group.push(tree);

        if match_symbol(input, "}") {
            break;
        }
        expect_symbol(input, ",")?;
        if match_symbol(input, "}") {
            break;
        }
    }
    Ok(group)
}

pub(crate) fn parse_item_macro(input: &mut &[Token]) -> ModalResult<AstItem> {
    // Look for a leading path followed by '!'
    // We reuse the type-path parser but require a trailing '!'.
    let snapshot = *input;
    let mut segments = Vec::new();
    if let Ok(seg) = expect_type_ident_segment(input) {
        segments.push(seg);
    } else {
        *input = snapshot;
        return Err(backtrack_err());
    }
    while match_symbol(input, "::") {
        segments.push(expect_type_ident_segment(input)?);
    }

    if !match_symbol(input, "!") {
        *input = snapshot;
        return Err(backtrack_err());
    }

    let path = Path::new(segments);
    // Determine delimiter and capture body similarly to expression macros
    let next = advance(input).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
    let (delimiter, open, close) = match next.kind {
        TokenKind::Symbol => match next.lexeme.as_str() {
            "(" => (MacroDelimiter::Parenthesis, "(", ")"),
            "[" => (MacroDelimiter::Bracket, "[", "]"),
            "{" => (MacroDelimiter::Brace, "{", "}"),
            _ => return Err(ErrMode::Cut(ContextError::new())),
        },
        _ => return Err(ErrMode::Cut(ContextError::new())),
    };

    let mut depth = 1i32;
    let mut pieces = Vec::new();
    while depth > 0 {
        let tok = advance(input).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
        if let TokenKind::Symbol = tok.kind {
            if tok.lexeme == open {
                depth += 1;
                pieces.push(tok.lexeme);
                continue;
            }
            if tok.lexeme == close {
                depth -= 1;
                if depth == 0 {
                    break;
                }
                pieces.push(tok.lexeme);
                continue;
            }
        }
        pieces.push(tok.lexeme);
    }
    let tokens = pieces.join(" ");
    let invocation = MacroInvocation::new(path, delimiter, tokens);
    Ok(Item::from(ItemKind::Macro(ItemMacro::new(invocation))))
}

fn parse_outer_attrs(input: &mut &[Token]) -> ModalResult<Vec<Attribute>> {
    let mut attrs = Vec::new();
    loop {
        // Look for '#[' (outer) or '#![' (inner) pattern
        let (is_attr, is_inner) = match input {
            [Token {
                kind: TokenKind::Symbol,
                lexeme,
                ..
            }, rest @ ..]
                if lexeme == "#" =>
            {
                match rest {
                    [Token {
                        kind: TokenKind::Symbol,
                        lexeme: l2,
                        ..
                    }, ..]
                        if l2 == "[" =>
                    {
                        (true, false)
                    }
                    [Token {
                        kind: TokenKind::Symbol,
                        lexeme: l2,
                        ..
                    }, Token {
                        kind: TokenKind::Symbol,
                        lexeme: l3,
                        ..
                    }, ..]
                        if l2 == "!" && l3 == "[" =>
                    {
                        (true, true)
                    }
                    _ => (false, false),
                }
            }
            _ => (false, false),
        };
        if !is_attr {
            break;
        }

        // consume '#' and optional '!' then '['
        expect_symbol(input, "#")?;
        let inner = match_symbol(input, "!");
        expect_symbol(input, "[")?;

        // parse simple path meta: #[foo::bar]
        let mut segments = Vec::new();
        segments.push(expect_type_ident_segment(input)?);
        while match_symbol(input, "::") {
            segments.push(expect_type_ident_segment(input)?);
        }
        let path = Path::new(segments);
        expect_symbol(input, "]")?;

        let meta = AttrMeta::Path(path);
        let style = if is_inner || inner {
            AttrStyle::Inner
        } else {
            AttrStyle::Outer
        };
        attrs.push(Attribute { style, meta });
    }
    Ok(attrs)
}

pub(crate) fn parse_type(input: &mut &[Token]) -> ModalResult<Ty> {
    parse_type_prec(input, 0)
}

fn parse_type_prec(input: &mut &[Token], min_prec: u8) -> ModalResult<Ty> {
    let mut left = parse_type_atom(input)?;
    loop {
        let (prec, op) = match peek_type_binop(input) {
            Some(info) => info,
            None => break,
        };
        if prec < min_prec {
            break;
        }
        // consume operator symbol
        match op {
            TypeBinaryOpKind::Add => {
                match_symbol(input, "+");
            }
            TypeBinaryOpKind::Union => {
                match_symbol(input, "|");
            }
            TypeBinaryOpKind::Intersect => {
                match_symbol(input, "&");
            }
            TypeBinaryOpKind::Subtract => {
                match_symbol(input, "-");
            }
        }
        let right = parse_type_prec(input, prec + 1)?;
        left = apply_type_binop(op, left, right);
    }
    Ok(left)
}

fn parse_type_atom(input: &mut &[Token]) -> ModalResult<Ty> {
    // Parenthesized type: (T)
    if match_symbol(input, "(") {
        let ty = parse_type(input)?;
        expect_symbol(input, ")")?;
        return Ok(ty);
    }

    // Reference type: &T / &mut T
    if match_symbol(input, "&") {
        let is_mut = match_keyword(input, Keyword::Mut);
        let lifetime_ident = if let Some(Token {
            kind: TokenKind::Ident,
            lexeme,
            ..
        }) = input.first()
        {
            if lexeme.starts_with('\'') {
                let tok = advance(input).unwrap();
                Some(Ident::new(tok.lexeme))
            } else {
                None
            }
        } else {
            None
        };
        let inner = parse_type_atom(input)?;
        let mut_ref = TypeReference {
            ty: Box::new(inner),
            mutability: is_mut.then_some(true),
            lifetime: lifetime_ident,
        };
        return Ok(Ty::Reference(mut_ref.into()));
    }

    // Array type: [T; N] or slice [T]
    if match_symbol(input, "[") {
        let elem_ty = parse_type(input)?;
        if match_symbol(input, ";") {
            let len_expr = expr::parse_expr_prec(input, 0)?;
            expect_symbol(input, "]")?;
            let arr = TypeArray {
                elem: Box::new(elem_ty),
                len: Box::new(len_expr),
            };
            return Ok(Ty::Array(arr.into()));
        } else {
            expect_symbol(input, "]")?;
            let slice = TypeSlice {
                elem: Box::new(elem_ty),
            };
            return Ok(Ty::Slice(slice.into()));
        }
    }

    // Function pointer type: fn(T1, T2) -> T
    if match_keyword(input, Keyword::Fn) {
        // Skip parameter list
        expect_symbol(input, "(")?;
        if !matches_symbol(input.first(), ")") {
            loop {
                parse_type(input)?; // ignore type value
                if match_symbol(input, ")") {
                    break;
                }
                expect_symbol(input, ",")?;
            }
        } else {
            expect_symbol(input, ")")?;
        }
        // Optional return type
        if match_symbol(input, "->") {
            let _ = parse_type(input)?;
        }
        // Represent as a TypeFunction with erased params/ret (still better than any).
        return Ok(Ty::Function(
            TypeFunction {
                params: Vec::new(),
                generics_params: Vec::new(),
                ret_ty: None,
            }
            .into(),
        ));
    }

    // Impl Trait type: impl Trait
    if match_keyword(input, Keyword::Impl) {
        // Consume a path with optional generics and ignore
        let (path, _) = parse_path_as_ty(input)?;
        // Special-case impl Fn(...) -> ...
        if match_symbol(input, "(") {
            if !matches_symbol(input.first(), ")") {
                loop {
                    let _ = parse_type(input)?;
                    if match_symbol(input, ")") {
                        break;
                    }
                    expect_symbol(input, ",")?;
                }
            } else {
                expect_symbol(input, ")")?;
            }
            if match_symbol(input, "->") {
                let _ = parse_type(input)?;
            }
        }
        return Ok(Ty::ImplTraits(
            ImplTraits {
                bounds: TypeBounds::new(Expr::locator(Locator::path(path))),
            }
            .into(),
        ));
    }

    // Underscore placeholder type: _
    if matches!(
        input.first(),
        Some(Token {
            kind: TokenKind::Ident,
            lexeme,
            ..
        }) if lexeme == "_"
    ) {
        advance(input);
        return Ok(Ty::any());
    }

    // Structural type literal: `struct { ... }` or bare `{ ... }`
    if match_keyword(input, Keyword::Struct) || matches_symbol(input.first(), "{") {
        return parse_structural_type_literal(input);
    }

    // Fallback: path-like type
    // Also handle type-level macro invocation: path!{...}
    if matches!(
        input,
        [Token {
            kind: TokenKind::Ident,
            ..
        }, Token {
            kind: TokenKind::Symbol,
            lexeme,
            ..
        }, ..] if lexeme == "!"
    ) {
        let macro_ident = expect_ident(input)?;
        let path = Path::from_ident(Ident::new(macro_ident));
        let macro_expr = parse_macro_invocation(path, input)?;
        return Ok(Ty::expr(macro_expr));
    }

    let (_path, ty) = parse_path_as_ty(input)?;
    Ok(ty)
}

fn peek_type_binop(input: &[Token]) -> Option<(u8, TypeBinaryOpKind)> {
    let token = input.first()?;
    match token.kind {
        TokenKind::Symbol => match token.lexeme.as_str() {
            "+" => {
                // Precedence for `+` in type expressions.
                Some((10, TypeBinaryOpKind::Add))
            }
            "|" => {
                // Union / alternation of types.
                Some((10, TypeBinaryOpKind::Union))
            }
            "&" => {
                // Intersection of struct-like types.
                Some((10, TypeBinaryOpKind::Intersect))
            }
            "-" => {
                // Field removal from a struct-like type.
                Some((10, TypeBinaryOpKind::Subtract))
            }
            _ => None,
        },
        _ => None,
    }
}

fn apply_type_binop(kind: TypeBinaryOpKind, lhs: Ty, rhs: Ty) -> Ty {
    Ty::TypeBinaryOp(Box::new(TypeBinaryOp {
        kind,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }))
}

fn parse_structural_type_literal(input: &mut &[Token]) -> ModalResult<Ty> {
    expect_symbol(input, "{")?;
    let mut fields = Vec::new();
    if matches_symbol(input.first(), "}") {
        expect_symbol(input, "}")?;
    } else {
        loop {
            let field_name = expect_ident(input)?;
            expect_symbol(input, ":")?;
            let field_ty = parse_type(input)?;
            fields.push(StructuralField::new(Ident::new(field_name), field_ty));
            if match_symbol(input, "}") {
                break;
            }
            expect_symbol(input, ",")?;
        }
    }
    Ok(Ty::Structural(TypeStructural { fields }))
}

fn expect_ident(input: &mut &[Token]) -> ModalResult<String> {
    match input.first() {
        Some(Token {
            kind: TokenKind::Ident,
            lexeme,
            ..
        }) => {
            let name = lexeme.clone();
            *input = &input[1..];
            Ok(name)
        }
        _ => Err(ErrMode::Cut(ContextError::new())),
    }
}

fn expect_symbol<'a>(input: &mut &'a [Token], sym: &'a str) -> ModalResult<()> {
    if match_symbol(input, sym) {
        Ok(())
    } else {
        Err(ErrMode::Cut(ContextError::new()))
    }
}

fn match_symbol<'a>(input: &mut &'a [Token], sym: &'a str) -> bool {
    // Prefer consuming from pending split `>` first.
    if sym == ">" {
        let consumed = PENDING_GT.with(|c| {
            let n = c.get();
            if n > 0 {
                c.set(n - 1);
                true
            } else {
                false
            }
        });
        if consumed {
            return true;
        }
    }

    let mut cursor = *input;
    if symbol_parser(sym).parse_next(&mut cursor).is_ok() {
        *input = cursor;
        return true;
    }

    // Special-case `>>` so nested generic args can be closed without lexer changes.
    if sym == ">" {
        if let Some(Token {
            kind: TokenKind::Symbol,
            lexeme,
            ..
        }) = input.first()
        {
            if lexeme == ">>" {
                // Consume this token and remember one extra `>` for outer scopes.
                *input = &input[1..];
                PENDING_GT.with(|c| c.set(c.get() + 1));
                return true;
            }
        }
    }
    false
}

fn matches_symbol(tok: Option<&Token>, sym: &str) -> bool {
    matches!(tok, Some(Token { kind: TokenKind::Symbol, lexeme, .. }) if lexeme == sym)
}

fn advance(input: &mut &[Token]) -> Option<Token> {
    let tok = input.first().cloned()?;
    *input = &input[1..];
    Some(tok)
}

fn keyword_parser<'a>(keyword: Keyword) -> impl Parser<&'a [Token], (), ContextError> {
    move |input: &mut &[Token]| match input.first() {
        Some(Token {
            kind: TokenKind::Keyword(k),
            ..
        }) if *k == keyword => {
            *input = &input[1..];
            Ok(())
        }
        _ => Err(backtrack_err()),
    }
}

fn symbol_parser<'a>(sym: &'a str) -> impl Parser<&'a [Token], (), ContextError> {
    let sym_owned = sym.to_string();
    move |input: &mut &[Token]| {
        if matches_symbol(input.first(), &sym_owned) {
            *input = &input[1..];
            Ok(())
        } else {
            Err(backtrack_err())
        }
    }
}

fn reset_pending_gt() {
    PENDING_GT.with(|c| c.set(0));
}
