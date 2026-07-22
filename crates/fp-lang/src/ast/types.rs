use super::*;
use fp_core::ast::ImplTraits;
use fp_core::ast::TypeNothing;

pub(crate) fn parse_simple_type(input: &mut &[Token]) -> ModalResult<Ty> {
    let _is_unsafe = expect_keyword(input, Keyword::Unsafe).is_ok();
    let abi = if expect_keyword(input, Keyword::Extern).is_ok() {
        let abi = token_kind(input, TokenKind::StringLiteral)?;
        let _ = decode_string_literal(&abi.lexeme).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
        true
    } else {
        false
    };
    if expect_keyword(input, Keyword::Impl).is_ok() {
        let bounds = parse_type_bounds(input)?;
        return Ok(Ty::ImplTraits(fp_core::ast::ImplTraits { bounds }));
    }
    if expect_keyword(input, Keyword::Struct).is_ok() {
        return parse_structural_type_body(input);
    }
    if peek_symbol(input) == Some("{") {
        return parse_structural_type_body(input);
    }
    if expect_symbol(input, "!").is_ok() {
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
        return Ok(Ty::Nothing(TypeNothing));
    }
    if expect_symbol(input, "(").is_ok() {
        if expect_symbol(input, ")").is_ok() {
            return Ok(Ty::unit());
        }
        let first = parse_type_expr(input)?;
        if expect_symbol(input, ",").is_ok() {
            let mut types = vec![first];
            if peek_symbol(input) != Some(")") {
                loop {
                    types.push(parse_type_expr(input)?);
                    if expect_symbol(input, ",").is_err() {
                        break;
                    }
                    if peek_symbol(input) == Some(")") {
                        break;
                    }
                }
            }
            expect_symbol(input, ")")?;
            return Ok(Ty::Tuple(fp_core::ast::TypeTuple { types }.into()));
        }
        expect_symbol(input, ")")?;
        return Ok(first);
    }
    if abi || expect_keyword(input, Keyword::Fn).is_ok() {
        if !abi {
            // already consumed `fn` in the branch condition above
        } else {
            expect_keyword(input, Keyword::Fn)?;
        }
        expect_symbol(input, "(")?;
        let mut params = Vec::new();
        if peek_symbol(input) != Some(")") {
            loop {
                params.push(parse_type_expr(input)?);
                if expect_symbol(input, ",").is_err() {
                    break;
                }
                if peek_symbol(input) == Some(")") {
                    break;
                }
            }
        }
        expect_symbol(input, ")")?;
        let ret_ty = if expect_symbol(input, "->").is_ok() {
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
    if expect_keyword(input, Keyword::Impl).is_ok() {
        let bounds = parse_dyn_type_bounds(input)?;
        return Ok(Ty::ImplTraits(ImplTraits { bounds }));
    }
    if expect_symbol(input, "&&").is_ok() {
        let lifetime = match peek_ident_like(*input) {
            Some(ident) if ident.starts_with('\'') => Some(ident_like(input)?),
            _ => None,
        };
        let mutability = expect_keyword(input, Keyword::Mut).is_ok();
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
    if expect_symbol(input, "&").is_ok() {
        let lifetime = match peek_ident_like(*input) {
            Some(ident) if ident.starts_with('\'') => Some(ident_like(input)?),
            _ => None,
        };
        let mutability = expect_keyword(input, Keyword::Mut).is_ok();
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
    if expect_symbol(input, "*").is_ok() {
        let mutability = if expect_keyword(input, Keyword::Mut).is_ok() {
            Some(true)
        } else if expect_keyword(input, Keyword::Const).is_ok() {
            Some(false)
        } else {
            return Err(ErrMode::Cut(ContextError::new()));
        };
        let inner = parse_type_expr(input)?;
        return Ok(Ty::raw_ptr(inner, mutability));
    }
    if expect_symbol(input, "[").is_ok() {
        let inner = parse_type_expr(input)?;
        if expect_symbol(input, ";").is_ok() {
            let len = parse_expr_winnow_no_struct(input, 0)?;
            expect_symbol(input, "]")?;
            return Ok(Ty::Array(
                fp_core::ast::TypeArray {
                    elem: Box::new(inner),
                    len: Box::new(len),
                }
                .into(),
            ));
        }
        expect_symbol(input, "]")?;
        return Ok(Ty::Slice(TypeSlice {
            elem: Box::new(inner),
        }));
    }
    if peek_ident_like(*input) == Some("dyn") {
        let _ = ident_like(input)?;
        let bounds = parse_dyn_type_bounds(input)?;
        return Ok(Ty::TypeBounds(bounds));
    }
    if matches!(input.first(), Some(token) if token.kind == TokenKind::Keyword(Keyword::Const))
        || looks_like_type_expr_macro(*input)
        || matches!(input.first(), Some(token) if token.kind == TokenKind::Number || token.kind == TokenKind::StringLiteral)
        || matches!(peek_ident_like(*input), Some("true" | "false" | "null"))
    {
        let expr = parse_expr_winnow_no_struct(input, 0)?;
        return Ok(Ty::Expr(Box::new(expr)));
    }
    let name = parse_name(input)?;
    if expect_symbol(input, "(").is_ok() {
        let mut params = Vec::new();
        if peek_symbol(input) != Some(")") {
            loop {
                params.push(parse_type_expr(input)?);
                if expect_symbol(input, ",").is_err() {
                    break;
                }
                if peek_symbol(input) == Some(")") {
                    break;
                }
            }
        }
        expect_symbol(input, ")")?;
        let ret_ty = if expect_symbol(input, "->").is_ok() {
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
            let kind = match &parameter_path.segments[0].args[0] {
                Ty::Expr(expr) => match expr.kind() {
                    ExprKind::Name(name) => match name.as_ident().map(Ident::as_str) {
                        Some("item") => QuoteFragmentKind::Item,
                        Some("expr") => QuoteFragmentKind::Expr,
                        Some("stmt") => QuoteFragmentKind::Stmt,
                        Some("type") => QuoteFragmentKind::Type,
                        _ => return Err(ErrMode::Cut(ContextError::new())),
                    },
                    _ => return Err(ErrMode::Cut(ContextError::new())),
                },
                Ty::Slice(slice) => match slice.elem.as_ref() {
                    Ty::Expr(expr) => match expr.kind() {
                        ExprKind::Name(name) => match name.as_ident().map(Ident::as_str) {
                            Some("item") => QuoteFragmentKind::Item,
                            Some("expr") => QuoteFragmentKind::Expr,
                            Some("stmt") => QuoteFragmentKind::Stmt,
                            Some("type") => QuoteFragmentKind::Type,
                            _ => return Err(ErrMode::Cut(ContextError::new())),
                        },
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
                inner: None,
            }));
        }
    }
    let path = match &name {
        Name::Path(path) => Some(path),
        _ => None,
    };
    if let Some(path) = path {
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
                "type" => {
                    return Ok(Ty::Quote(TypeQuote {
                        span: Span::null(),
                        kind: QuoteFragmentKind::Type,
                        item: None,
                        inner: None,
                    }));
                }
                "bool" => return Ok(Ty::Primitive(TypePrimitive::Bool)),
                "str" | "string" => return Ok(Ty::Primitive(TypePrimitive::String)),
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
                _ => {}
            }
        }
        let mut ty = Ty::path(path.clone());
        if expect_symbol(input, "?").is_ok() {
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
    Ok(Ty::locator(name))
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
        if expect_symbol(input, "+").is_err() {
            break;
        }
    }
    Ok(TypeBounds { bounds })
}

fn parse_trait_bound_expr(input: &mut &[Token]) -> ModalResult<Expr> {
    let name = parse_name(input)?;
    if expect_symbol(input, "(").is_ok() {
        if peek_symbol(input) != Some(")") {
            loop {
                let _ = parse_type_expr(input)?;
                if expect_symbol(input, ",").is_err() {
                    break;
                }
                if peek_symbol(input) == Some(")") {
                    break;
                }
            }
        }
        expect_symbol(input, ")")?;
        if expect_symbol(input, "->").is_ok() {
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
        expect_symbol(input, &op)?;
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

fn parse_structural_type_body(input: &mut &[Token]) -> ModalResult<Ty> {
    expect_symbol(input, "{")?;
    let mut fields = Vec::new();
    while peek_symbol(input) != Some("}") {
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
    if expect_symbol(&mut probe, "<").is_err() {
        return Ok(Vec::new());
    }
    let mut args = Vec::new();
    if peek_symbol(probe) != Some(">") {
        loop {
            let Ok(arg) = parse_type_arg(&mut probe) else {
                return Ok(Vec::new());
            };
            args.push(arg);
            let mut comma_probe = probe;
            if expect_symbol(&mut comma_probe, ",").is_err() {
                break;
            }
            if peek_symbol(comma_probe) == Some(">") {
                probe = comma_probe;
                break;
            }
            probe = comma_probe;
        }
    }
    if expect_symbol(&mut probe, ">").is_err() {
        return Ok(Vec::new());
    }
    *input = probe;
    Ok(args)
}

fn parse_type_arg(input: &mut &[Token]) -> ModalResult<Ty> {
    let mut probe = *input;
    if let Ok(ident) = ident_like(&mut probe) {
        let mut assign_probe = probe;
        if expect_symbol(&mut assign_probe, "=").is_ok() {
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
    }
    parse_type_expr(input)
}

pub(crate) fn parse_type_bounds(input: &mut &[Token]) -> ModalResult<TypeBounds> {
    let mut bounds = Vec::new();
    loop {
        let ty = parse_type_expr(input)?;
        bounds.push(type_to_expr(&ty));
        if expect_symbol(input, "+").is_err() {
            break;
        }
    }
    Ok(TypeBounds { bounds })
}

fn type_to_expr(ty: &Ty) -> Expr {
    match ty {
        Ty::Expr(expr) => (**expr).clone(),
        other => Expr::value(Value::Type(other.clone())),
    }
}

pub(crate) fn parse_use_tree(input: &mut &[Token]) -> ModalResult<fp_core::ast::ItemImportTree> {
    let mut path = parse_use_path(input)?;
    if expect_keyword(input, Keyword::As).is_ok() {
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
    if expect_symbol(input, "::").is_ok() {
        path.push(fp_core::ast::ItemImportTree::Root);
    }
    loop {
        if expect_symbol(input, "*").is_ok() {
            path.push(fp_core::ast::ItemImportTree::Glob);
            break;
        }
        if peek_symbol(input) == Some("{") {
            path.push(fp_core::ast::ItemImportTree::Group(parse_use_group(input)?));
            break;
        }
        let segment = if expect_keyword(input, Keyword::Crate).is_ok() {
            fp_core::ast::ItemImportTree::Crate
        } else if peek_ident_like(*input) == Some("self") {
            let _ = ident_like(input)?;
            fp_core::ast::ItemImportTree::SelfMod
        } else if expect_keyword(input, Keyword::Super).is_ok() {
            fp_core::ast::ItemImportTree::SuperMod
        } else {
            fp_core::ast::ItemImportTree::Ident(ident_like(input)?)
        };
        path.push(segment);
        if expect_symbol(input, "::").is_err() {
            break;
        }
    }
    Ok(path)
}

fn parse_use_group(input: &mut &[Token]) -> ModalResult<fp_core::ast::ItemImportGroup> {
    expect_symbol(input, "{")?;
    let mut group = fp_core::ast::ItemImportGroup::new();
    while peek_symbol(input) != Some("}") {
        group.push(parse_use_tree(input)?);
        if expect_symbol(input, ",").is_err() {
            break;
        }
    }
    expect_symbol(input, "}")?;
    Ok(group)
}

pub(crate) fn parse_optional_generic_params(
    input: &mut &[Token],
) -> ModalResult<Vec<fp_core::ast::GenericParam>> {
    let mut probe = *input;
    if expect_symbol(&mut probe, "<").is_err() {
        return Ok(Vec::new());
    }
    let mut params = Vec::new();
    if peek_symbol(probe) != Some(">") {
        loop {
            let is_const = expect_keyword(&mut probe, Keyword::Const).is_ok();
            let name = ident_like(&mut probe)?;
            let bounds = if expect_symbol(&mut probe, ":").is_ok() && !is_const {
                parse_type_bounds(&mut probe)?
            } else if is_const {
                let _ = parse_type_expr(&mut probe)?;
                fp_core::ast::TypeBounds::any()
            } else {
                fp_core::ast::TypeBounds::any()
            };
            if expect_symbol(&mut probe, "=").is_ok() {
                if is_const {
                    let _ = parse_expr_winnow_no_struct(&mut probe, 0)?;
                } else {
                    let _ = parse_type_expr(&mut probe)?;
                }
            }
            params.push(fp_core::ast::GenericParam { name, bounds });
            if expect_symbol(&mut probe, ",").is_err() {
                break;
            }
        }
    }
    expect_symbol(&mut probe, ">")?;
    *input = probe;
    Ok(params)
}
