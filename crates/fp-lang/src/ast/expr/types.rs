use crate::ast::lower_common::{
    collect_path_tokens_until_generics, decode_string_literal, macro_token_trees_to_lexemes,
    macro_tokens_file_id, split_path_prefix,
};
use crate::syntax::{SyntaxKind, SyntaxNode};
use fp_core::ast::{
    Expr, ExprKind, Ident, ImplTraits, MacroInvocation, Name, ParameterPath,
    ParameterPathSegment, Path, StructuralField, Ty, TypeArray, TypeBinaryOp,
    TypeBinaryOpKind, TypeBounds, TypeFunction, TypeReference, TypeSlice, TypeStructural,
    TypeTuple, TypeVec, Value, ValueNone, ValueString,
};
use fp_core::cst::CstCategory;
use fp_core::module::path::PathPrefix;

use super::literals::parse_numeric_literal;
use super::quote::{quote_type_from_ident, quote_type_from_type_arg};
use super::{
    direct_first_non_trivia_token_text, direct_operator_token_text, lower_expr_from_cst, LowerError,
};

pub(crate) fn lower_type_from_cst(node: &SyntaxNode) -> Result<Ty, LowerError> {
    if matches!(
        node.kind,
        SyntaxKind::GenericParam | SyntaxKind::GenericParams
    ) {
        return Ok(Ty::unknown());
    }
    if node.kind.category() != CstCategory::Type {
        return Err(LowerError::UnexpectedNode(node.kind));
    }

    match node.kind {
        SyntaxKind::TyUnit => Ok(Ty::unit()),
        SyntaxKind::TyUnknown => Ok(Ty::unknown()),
        SyntaxKind::TyPath => lower_ty_path(node),
        SyntaxKind::TyRef => lower_ty_ref(node),
        SyntaxKind::TyPtr => lower_ty_ptr(node),
        SyntaxKind::TySlice => lower_ty_slice(node),
        SyntaxKind::TyArray => lower_ty_array(node),
        SyntaxKind::TyFn => lower_ty_fn(node),
        SyntaxKind::TyTuple => lower_ty_tuple(node),
        SyntaxKind::TyStructural => lower_ty_structural(node),
        SyntaxKind::TyBinary => lower_ty_binary(node),
        SyntaxKind::TyOptional => lower_ty_optional(node),
        SyntaxKind::TyValue => lower_ty_value(node),
        SyntaxKind::TyExpr => lower_ty_expr(node),
        SyntaxKind::TyImplTraits => lower_ty_impl_traits(node),
        SyntaxKind::TyMacroCall => lower_ty_macro_call(node),
        SyntaxKind::TyUnsafeBinder => {
            let inner = node
                .children
                .iter()
                .find_map(|child| match child {
                    crate::syntax::SyntaxElement::Node(n)
                        if n.kind.category() == CstCategory::Type =>
                    {
                        Some(n.as_ref())
                    }
                    _ => None,
                })
                .ok_or(LowerError::UnexpectedNode(SyntaxKind::TyUnsafeBinder))?;
            lower_type_from_cst(inner)
        }
        other => Err(LowerError::UnexpectedNode(other)),
    }
}

fn lower_ty_value(node: &SyntaxNode) -> Result<Ty, LowerError> {
    let raw = direct_first_non_trivia_token_text(node)
        .ok_or_else(|| LowerError::UnexpectedNode(SyntaxKind::TyValue))?;
    let value = match raw.as_str() {
        "!" => return Ok(Ty::Nothing(fp_core::ast::TypeNothing)),
        "true" => Value::bool(true),
        "false" => Value::bool(false),
        "null" => Value::null(),
        _ => {
            if raw.starts_with('"') || raw.starts_with('\'') {
                let decoded = decode_string_literal(&raw).unwrap_or(raw);
                Value::String(ValueString::new_ref(decoded))
            } else {
                parse_numeric_literal(&raw)?.0
            }
        }
    };
    Ok(Ty::value(value))
}

fn lower_ty_expr(node: &SyntaxNode) -> Result<Ty, LowerError> {
    let expr_node = node
        .children
        .iter()
        .find_map(|child| match child {
            crate::syntax::SyntaxElement::Node(n) if n.kind.category() == CstCategory::Expr => {
                Some(n.as_ref())
            }
            _ => None,
        })
        .ok_or_else(|| LowerError::UnexpectedNode(SyntaxKind::TyExpr))?;
    let expr = lower_expr_from_cst(expr_node)?;
    Ok(Ty::expr(expr))
}

fn lower_ty_macro_call(node: &SyntaxNode) -> Result<Ty, LowerError> {
    let span = node.span;
    let mut segments: Vec<Ident> = Vec::new();
    let mut saw_root = false;
    let mut saw_first_token = false;
    for child in &node.children {
        let crate::syntax::SyntaxElement::Token(tok) = child else {
            continue;
        };
        if tok.is_trivia() {
            continue;
        }
        if tok.text == "!" {
            break;
        }
        if !saw_first_token && tok.text == "::" {
            saw_root = true;
            saw_first_token = true;
            continue;
        }
        match tok.text.as_str() {
            "::" | "<" | ">" | "," | "=" => {}
            _ => {
                if tok
                    .text
                    .chars()
                    .next()
                    .is_some_and(|c| c.is_alphabetic() || c == '_' || c == '\'')
                {
                    segments.push(Ident::new(tok.text.clone()));
                }
            }
        }
        saw_first_token = true;
    }
    if segments.is_empty() {
        return Err(LowerError::UnexpectedNode(SyntaxKind::TyMacroCall));
    }
    let macro_tokens = crate::ast::macros::macro_group_tokens(node)
        .ok_or_else(|| LowerError::UnexpectedNode(SyntaxKind::TyMacroCall))?;

    // `t! { ... }` is used as a type-level quoting wrapper in existing examples; lower it by
    // parsing the inner token stream as a type expression.
    if segments.len() == 1 && segments[0].as_str() == "t" {
        let lexemes = macro_token_trees_to_lexemes(&macro_tokens.token_trees);
        let file_id = macro_tokens_file_id(&macro_tokens.token_trees);
        let (ty_cst, consumed) =
            crate::cst::parse_type_lexemes_prefix_to_cst(&lexemes, file_id, &[])
                .map_err(|_| LowerError::UnexpectedNode(SyntaxKind::TyMacroCall))?;
        if lexemes[consumed..]
            .iter()
            .any(|l| l.kind == crate::lexer::LexemeKind::Token)
        {
            return Err(LowerError::UnexpectedNode(SyntaxKind::TyMacroCall));
        }
        return lower_type_from_cst(&ty_cst);
    }

    let (prefix, segments) = split_path_prefix(segments, saw_root);
    if segments.is_empty() && matches!(prefix, PathPrefix::Plain | PathPrefix::Root) {
        return Err(LowerError::UnexpectedNode(SyntaxKind::TyMacroCall));
    }
    let path = Path::new(prefix, segments);
    let expr: Expr = ExprKind::Macro(fp_core::ast::ExprMacro::new(
        MacroInvocation::new(path, macro_tokens.delimiter, macro_tokens.text)
            .with_token_trees(macro_tokens.token_trees)
            .with_span(span),
    ))
    .into();
    Ok(Ty::expr(expr))
}

pub(super) fn node_children_types<'a>(
    node: &'a SyntaxNode,
) -> impl Iterator<Item = &'a SyntaxNode> {
    node.children.iter().filter_map(|c| match c {
        crate::syntax::SyntaxElement::Node(n) if n.kind.category() == CstCategory::Type => {
            Some(n.as_ref())
        }
        _ => None,
    })
}

fn lower_ty_path(node: &SyntaxNode) -> Result<Ty, LowerError> {
    let span = node.span;
    let path_tokens = collect_path_tokens_until_generics(node);
    let saw_generic_start = path_tokens.saw_generic_start;
    let (prefix, segments) = split_path_prefix(path_tokens.segments, path_tokens.saw_root);
    if segments.is_empty() && matches!(prefix, PathPrefix::Plain | PathPrefix::Root) {
        return Err(LowerError::UnexpectedNode(node.kind));
    }

    let args = node_children_types(node)
        .map(lower_type_from_cst)
        .collect::<Result<Vec<_>, _>>()?;

    let path = Path::new(prefix, segments.clone());
    if segments.len() == 1 && segments[0].as_str() == "quote" && args.len() == 1 {
        let Some(quote_ty) = quote_type_from_type_arg(&args[0]) else {
            return Err(LowerError::UnexpectedNode(node.kind));
        };
        return Ok(quote_ty);
    }

    if segments.len() == 1 {
        if let Some(quote_ty) = quote_type_from_ident(segments[0].as_str(), &args, span) {
            return Ok(quote_ty);
        }
    }

    if segments.last().map(|seg| seg.as_str()) == Some("Vec") && args.len() == 1 {
        return Ok(Ty::Vec(TypeVec {
            ty: Box::new(args[0].clone()),
        }));
    }

    if !saw_generic_start || args.is_empty() {
        return Ok(Ty::expr(Expr::path(path).with_span(span)));
    }

    let mut param_segments: Vec<ParameterPathSegment> = segments
        .into_iter()
        .map(ParameterPathSegment::from_ident)
        .collect();
    if let Some(last) = param_segments.last_mut() {
        last.args = args;
    }
    let ppath = ParameterPath::new(prefix, param_segments);
    Ok(Ty::expr(
        Expr::name(Name::parameter_path(ppath)).with_span(node.span),
    ))
}

fn lower_ty_ref(node: &SyntaxNode) -> Result<Ty, LowerError> {
    let mut is_mut = false;
    let mut lifetime: Option<Ident> = None;
    for child in &node.children {
        let crate::syntax::SyntaxElement::Token(tok) = child else {
            continue;
        };
        if tok.is_trivia() {
            continue;
        }
        if tok.text == "mut" {
            is_mut = true;
        }
        if tok.text.starts_with('\'') {
            lifetime = Some(Ident::new(tok.text.clone()));
        }
    }

    let inner = node_children_types(node)
        .next()
        .ok_or_else(|| LowerError::UnexpectedNode(node.kind))?;
    let inner = lower_type_from_cst(inner)?;
    Ok(Ty::Reference(
        TypeReference {
            ty: Box::new(inner),
            mutability: is_mut.then_some(true),
            lifetime,
        }
        .into(),
    ))
}

fn lower_ty_ptr(node: &SyntaxNode) -> Result<Ty, LowerError> {
    let mut mutability: Option<bool> = None;
    for child in &node.children {
        let crate::syntax::SyntaxElement::Token(tok) = child else {
            continue;
        };
        if tok.is_trivia() {
            continue;
        }
        if tok.text == "mut" {
            mutability = Some(true);
        } else if tok.text == "const" {
            mutability = Some(false);
        }
    }

    let inner = node_children_types(node)
        .next()
        .ok_or_else(|| LowerError::UnexpectedNode(node.kind))?;
    let inner = lower_type_from_cst(inner)?;
    Ok(Ty::raw_ptr(inner, mutability))
}

fn lower_ty_slice(node: &SyntaxNode) -> Result<Ty, LowerError> {
    let elem = node_children_types(node)
        .next()
        .ok_or_else(|| LowerError::UnexpectedNode(node.kind))?;
    let elem = lower_type_from_cst(elem)?;
    Ok(Ty::Slice(
        TypeSlice {
            elem: Box::new(elem),
        }
        .into(),
    ))
}

fn lower_ty_array(node: &SyntaxNode) -> Result<Ty, LowerError> {
    let elem = node_children_types(node)
        .next()
        .ok_or_else(|| LowerError::UnexpectedNode(node.kind))?;
    let elem = lower_type_from_cst(elem)?;
    let len_expr = node
        .children
        .iter()
        .find_map(|c| match c {
            crate::syntax::SyntaxElement::Node(n) if n.kind.category() == CstCategory::Expr => {
                Some(n.as_ref())
            }
            _ => None,
        })
        .ok_or_else(|| LowerError::UnexpectedNode(node.kind))?;
    let len_expr = lower_expr_from_cst(len_expr)?;
    Ok(Ty::Array(
        TypeArray {
            elem: Box::new(elem),
            len: Box::new(len_expr),
        }
        .into(),
    ))
}

fn lower_ty_fn(node: &SyntaxNode) -> Result<Ty, LowerError> {
    let mut types = node_children_types(node)
        .map(lower_type_from_cst)
        .collect::<Result<Vec<_>, _>>()?;

    let has_arrow = node.children.iter().any(
        |c| matches!(c, crate::syntax::SyntaxElement::Token(t) if !t.is_trivia() && t.text == "->"),
    );

    let ret_ty = if has_arrow {
        types.pop().map(|t| Box::new(t))
    } else {
        None
    };

    Ok(Ty::Function(
        TypeFunction {
            params: types,
            generics_params: Vec::new(),
            ret_ty,
        }
        .into(),
    ))
}

fn lower_ty_tuple(node: &SyntaxNode) -> Result<Ty, LowerError> {
    let types = node_children_types(node)
        .map(lower_type_from_cst)
        .collect::<Result<Vec<_>, _>>()?;
    Ok(Ty::Tuple(TypeTuple { types }.into()))
}

fn lower_ty_structural(node: &SyntaxNode) -> Result<Ty, LowerError> {
    let mut fields = Vec::new();
    for child in &node.children {
        let crate::syntax::SyntaxElement::Node(field) = child else {
            continue;
        };
        if field.kind != SyntaxKind::TyField {
            continue;
        }
        let name = field
            .children
            .iter()
            .find_map(|c| match c {
                crate::syntax::SyntaxElement::Token(t) if !t.is_trivia() && t.text != ":" => {
                    Some(t.text.clone())
                }
                _ => None,
            })
            .ok_or_else(|| LowerError::UnexpectedNode(SyntaxKind::TyField))?;
        let ty_node = node_children_types(field)
            .next()
            .ok_or_else(|| LowerError::UnexpectedNode(SyntaxKind::TyField))?;
        let mut value = lower_type_from_cst(ty_node)?;
        let is_optional = field.children.iter().any(|c| {
            matches!(
                c,
                crate::syntax::SyntaxElement::Token(t) if !t.is_trivia() && t.text == "?"
            )
        });
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
        fields.push(StructuralField::new(Ident::new(name), value));
    }
    let mut update: Option<Ty> = None;
    for (idx, child) in node.children.iter().enumerate() {
        let crate::syntax::SyntaxElement::Token(tok) = child else {
            continue;
        };
        if tok.is_trivia() || tok.text != ".." {
            continue;
        }
        if update.is_some() {
            return Err(LowerError::UnexpectedNode(SyntaxKind::TyStructural));
        }
        for next in node.children.iter().skip(idx + 1) {
            if let crate::syntax::SyntaxElement::Node(n) = next {
                if n.kind.category() == CstCategory::Type {
                    update = Some(lower_type_from_cst(n.as_ref())?);
                    break;
                }
            }
        }
        if update.is_none() {
            return Err(LowerError::UnexpectedNode(SyntaxKind::TyStructural));
        }
    }

    if let Some(update) = update {
        let lhs = Ty::Structural(TypeStructural { fields }.into());
        return Ok(Ty::TypeBinaryOp(
            TypeBinaryOp {
                kind: TypeBinaryOpKind::Add,
                lhs: Box::new(lhs),
                rhs: Box::new(update),
            }
            .into(),
        ));
    }

    Ok(Ty::Structural(TypeStructural { fields }.into()))
}

fn lower_ty_binary(node: &SyntaxNode) -> Result<Ty, LowerError> {
    let mut tys = node_children_types(node);
    let lhs = tys
        .next()
        .ok_or_else(|| LowerError::UnexpectedNode(node.kind))?;
    let rhs = tys
        .next()
        .ok_or_else(|| LowerError::UnexpectedNode(node.kind))?;
    let lhs = lower_type_from_cst(lhs)?;
    let rhs = lower_type_from_cst(rhs)?;

    let op = direct_operator_token_text(node).ok_or(LowerError::MissingOperator)?;
    let kind = match op.as_str() {
        "+" => TypeBinaryOpKind::Add,
        "|" => TypeBinaryOpKind::Union,
        "&" => TypeBinaryOpKind::Intersect,
        "-" => TypeBinaryOpKind::Subtract,
        _ => return Err(LowerError::MissingOperator),
    };
    Ok(Ty::TypeBinaryOp(
        TypeBinaryOp {
            kind,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
        .into(),
    ))
}

fn lower_ty_optional(node: &SyntaxNode) -> Result<Ty, LowerError> {
    let inner = node_children_types(node)
        .next()
        .ok_or_else(|| LowerError::UnexpectedNode(node.kind))?;
    let inner = lower_type_from_cst(inner)?;
    // `T?` lowers to `T | None` so later passes can materialize a tagged union.
    Ok(Ty::TypeBinaryOp(
        TypeBinaryOp {
            kind: TypeBinaryOpKind::Union,
            lhs: Box::new(inner),
            rhs: Box::new(Ty::value(Value::None(ValueNone))),
        }
        .into(),
    ))
}

fn lower_ty_impl_traits(node: &SyntaxNode) -> Result<Ty, LowerError> {
    let mut type_nodes = node_children_types(node);
    let bound_node = type_nodes
        .next()
        .ok_or_else(|| LowerError::UnexpectedNode(node.kind))?;
    let bound_ty = lower_type_from_cst(bound_node)?;

    // Special-case `impl Fn(T) -> U` into a first-class function type.
    // This matches the surface syntax used in `examples/09_higher_order_functions.fp`.
    let is_fn_trait = match &bound_ty {
        Ty::Expr(expr) => matches!(expr.kind(), ExprKind::Name(loc) if loc.to_string() == "Fn"),
        _ => false,
    };
    let mut trailing_types = type_nodes
        .map(lower_type_from_cst)
        .collect::<Result<Vec<_>, _>>()?;
    if is_fn_trait && !trailing_types.is_empty() {
        let has_arrow = node.children.iter().any(|c| match c {
            crate::syntax::SyntaxElement::Token(t) if !t.is_trivia() => t.text == "->",
            _ => false,
        });
        let ret_ty = if has_arrow {
            Some(Box::new(
                trailing_types
                    .pop()
                    .ok_or(LowerError::UnexpectedNode(node.kind))?,
            ))
        } else {
            None
        };
        return Ok(Ty::Function(
            TypeFunction {
                params: trailing_types,
                generics_params: Vec::new(),
                ret_ty,
            }
            .into(),
        ));
    }

    // Default: keep the representation aligned with the old token-based parser: bounds as a
    // locator path (or a value-wrapped type).
    let mut bounds = Vec::new();
    let bound_expr = match bound_ty {
        Ty::Expr(expr) => (*expr).clone(),
        other => Expr::value(Value::Type(other)),
    };
    bounds.push(bound_expr);
    for ty in trailing_types {
        let expr = match ty {
            Ty::Expr(expr) => (*expr).clone(),
            other => Expr::value(Value::Type(other)),
        };
        bounds.push(expr);
    }

    Ok(Ty::ImplTraits(
        ImplTraits {
            bounds: TypeBounds { bounds },
        }
        .into(),
    ))
}
