use fp_core::ast::{
    AttrMeta, AttrStyle, Attribute, EnumTypeVariant, Expr, ExprKind, FunctionParam,
    FunctionSignature, GenericParam, Ident, Item, Item as AstItem, ItemDeclConst,
    ItemDeclFunction, ItemDeclType, ItemDefConst, ItemDefEnum, ItemDefFunction, ItemDefStatic,
    ItemDefStruct, ItemDefTrait, ItemDefType, ItemImpl, ItemImport, ItemImportPath,
    ItemImportTree, ItemKind, ItemMacro, Locator, MacroDelimiter, MacroInvocation, Module, Path,
    StructuralField, Ty, TypeBounds, TypeEnum, TypeTuple, Visibility,
};
use thiserror::Error;
use winnow::combinator::alt;
use winnow::error::{ContextError, ErrMode};
use winnow::ModalResult;
use winnow::Parser;

use crate::lexer::{self, Keyword, LexerError, Token, TokenKind};
use crate::lexer::winnow::backtrack_err;
use super::expr;

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
                    Some(tok) => format!(
                        "failed to parse item starting at token '{}'",
                        tok.lexeme
                    ),
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

fn parse_item(input: &mut &[Token]) -> ModalResult<AstItem> {
    alt((
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
    .parse_next(input)
}

fn parse_mod_item(input: &mut &[Token]) -> ModalResult<AstItem> {
    keyword_parser(Keyword::Mod).parse_next(input)?;
    let name = Ident::new(expect_ident(input)?);

    // Handle `mod foo;` as an empty module for now.
    if match_symbol(input, ";") {
        let module = Module {
            name,
            items: Vec::new(),
            visibility: Visibility::Public,
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

fn parse_trait_item(input: &mut &[Token]) -> ModalResult<AstItem> {
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
    Ok(TypeBounds { bounds: bounds_exprs })
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
    if matches_symbol(input.first(), ")") {
        expect_symbol(input, ")")?;
    } else {
        loop {
            let param_name = Ident::new(expect_ident(input)?);
            expect_symbol(input, ":")?;
            let ty = parse_type(input)?;
            params.push(FunctionParam::new(param_name, ty));
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

        // Optional bounds `: Bound`; skip until ',' or '>' for now.
        if match_symbol(input, ":") {
            while !matches_symbol(input.first(), ",") && !matches_symbol(input.first(), ">") {
                if advance(input).is_none() {
                    return Err(ErrMode::Cut(ContextError::new()));
                }
            }
        }

        params.push(GenericParam {
            name,
            bounds: TypeBounds::any(),
        });

        if match_symbol(input, ">") {
            break;
        }
        expect_symbol(input, ",")?;
    }
    Ok(params)
}

fn parse_impl_item(input: &mut &[Token]) -> ModalResult<AstItem> {
    keyword_parser(Keyword::Impl).parse_next(input)?;
    // Optional impl generics: impl<T> Trait for Type
    if matches_symbol(input.first(), "<") {
        let _ = parse_generic_params(input)?;
    }
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
        items,
    };

    Ok(Item::from(ItemKind::Impl(impl_item)))
}

fn parse_use_item(input: &mut &[Token]) -> ModalResult<AstItem> {
    keyword_parser(Keyword::Use).parse_next(input)?;
    let tree = parse_use_path(input)?;
    expect_symbol(input, ";")?;
    let import = ItemImport {
        visibility: Visibility::Public,
        tree,
    };
    Ok(Item::from(ItemKind::Import(import)))
}

fn parse_struct_item(input: &mut &[Token]) -> ModalResult<AstItem> {
    keyword_parser(Keyword::Struct).parse_next(input)?;
    let name = Ident::new(expect_ident(input)?);
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
            expect_symbol(input, ",")?;
        }
    }
    let def = ItemDefStruct::new(name.clone(), fields);
    Ok(Item::from(ItemKind::DefStruct(def)))
}

fn parse_const_item(input: &mut &[Token]) -> ModalResult<AstItem> {
    keyword_parser(Keyword::Const).parse_next(input)?;
    let name = Ident::new(expect_ident(input)?);

    let ty = if match_symbol(input, ":") {
        Some(parse_type(input)?)
    } else {
        None
    };

    expect_symbol(input, "=")?;
    let value_expr: Expr = expr::parse_expr_prec(input, 0)?;
    expect_symbol(input, ";")?;

    let mut def = ItemDefConst {
        ty_annotation: None,
        visibility: Visibility::Public,
        name,
        ty,
        value: Box::new(value_expr),
    };

    Ok(Item::from(ItemKind::DefConst(def)))
}

fn parse_static_item(input: &mut &[Token]) -> ModalResult<AstItem> {
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

fn parse_type_item(input: &mut &[Token]) -> ModalResult<AstItem> {
    keyword_parser(Keyword::Type).parse_next(input)?;
    let name = Ident::new(expect_ident(input)?);
    expect_symbol(input, "=")?;
    let ty = parse_type(input)?;
    expect_symbol(input, ";")?;

    let def = ItemDefType {
        visibility: Visibility::Public,
        name,
        value: ty,
    };

    Ok(Item::from(ItemKind::DefType(def)))
}

fn parse_enum_item(input: &mut &[Token]) -> ModalResult<AstItem> {
    keyword_parser(Keyword::Enum).parse_next(input)?;
    let name = Ident::new(expect_ident(input)?);
    expect_symbol(input, "{")?;

    let mut variants = Vec::new();
    if matches_symbol(input.first(), "}") {
        expect_symbol(input, "}")?;
    } else {
        loop {
            let variant_name = Ident::new(expect_ident(input)?);
            // Variant payload: either a simple type after ':' or a
            // tuple-like variant `Variant(T1, T2, ...)`.
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

    let type_enum = TypeEnum { name: name.clone(), variants };
    let def = ItemDefEnum {
        visibility: Visibility::Public,
        name,
        value: type_enum,
    };

    Ok(Item::from(ItemKind::DefEnum(def)))
}

fn parse_fn_item(input: &mut &[Token]) -> ModalResult<AstItem> {
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
    if matches_symbol(input.first(), ")") {
        expect_symbol(input, ")")?;
    } else {
        loop {
            let param_name = Ident::new(expect_ident(input)?);
            expect_symbol(input, ":")?;
            let ty = parse_type(input)?;
            params.push(FunctionParam::new(param_name, ty));
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

fn parse_use_path(input: &mut &[Token]) -> ModalResult<ItemImportTree> {
    let mut path = ItemImportPath::new();
    loop {
        let token = advance(input).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
        let seg = match token.kind {
            TokenKind::Ident => ItemImportTree::Ident(Ident::new(token.lexeme)),
            TokenKind::Keyword(Keyword::Crate) => ItemImportTree::Crate,
            TokenKind::Keyword(Keyword::Super) => ItemImportTree::SuperMod,
            TokenKind::Keyword(Keyword::Use) => return Err(ErrMode::Cut(ContextError::new())),
            TokenKind::Symbol if token.lexeme == "::" => ItemImportTree::Root,
            _ => return Err(ErrMode::Cut(ContextError::new())),
        };
        path.push(seg);
        if match_symbol(input, "::") {
            continue;
        }
        break;
    }
    Ok(ItemImportTree::Path(path))
}

fn parse_item_macro(input: &mut &[Token]) -> ModalResult<AstItem> {
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
            [Token { kind: TokenKind::Symbol, lexeme, .. }, rest @ ..] if lexeme == "#" => {
                match rest {
                    [Token { kind: TokenKind::Symbol, lexeme: l2, .. }, ..] if l2 == "[" => {
                        (true, false)
                    }
                    [Token { kind: TokenKind::Symbol, lexeme: l2, .. }, Token { kind: TokenKind::Symbol, lexeme: l3, .. }, ..]
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

fn parse_type(input: &mut &[Token]) -> ModalResult<Ty> {
    let (path, ty) = parse_path_as_ty(input)?;
    let _ = path; // path is currently unused for plain types
    Ok(ty)
}

fn parse_path_as_ty(input: &mut &[Token]) -> ModalResult<(Path, Ty)> {
    let mut segments = Vec::new();
    segments.push(expect_type_ident_segment(input)?);
    while match_symbol(input, "::") {
        segments.push(expect_type_ident_segment(input)?);
    }
    let path = Path::new(segments);
    let ty = Ty::path(path.clone());
    Ok((path, ty))
}

fn expect_type_ident_segment(input: &mut &[Token]) -> ModalResult<Ident> {
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
    symbol_parser(sym)
        .parse_next(input)
        .map_err(|_| ErrMode::Cut(ContextError::new()))
}

fn match_symbol<'a>(input: &mut &'a [Token], sym: &'a str) -> bool {
    symbol_parser(sym).parse_next(input).is_ok()
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
    move |input: &mut &[Token]| {
        match input.first() {
            Some(Token { kind: TokenKind::Keyword(k), .. }) if *k == keyword => {
                *input = &input[1..];
                Ok(())
            }
            _ => Err(backtrack_err()),
        }
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
