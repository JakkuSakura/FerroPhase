use super::*;

pub(super) fn should_drop_quote_item(item: &ast::Item) -> bool {
    match item.kind() {
        ItemKind::DefFunction(func) => signature_contains_quote(&func.sig),
        ItemKind::DeclFunction(func) => signature_contains_quote(&func.sig),
        ItemKind::DefConst(def) => {
            def.ty_annotation()
                .or_else(|| def.ty.as_ref())
                .is_some_and(ty_contains_quote)
                || expr_contains_quote_value(def.value.as_ref())
        }
        _ => false,
    }
}

pub(super) fn should_drop_const_type_item(item: &ast::Item) -> bool {
    let _ = item;
    false
}

/// A type alias's RHS that needs compile-time evaluation to produce a
/// concrete type (`type X = const { .. };` or the bare-expression form
/// `type X = EXPR;`) — the one case `materialized_type_alias` returns
/// `None` for. Returns the inner expression to check/comptime-evaluate,
/// unwrapping an explicit `const { .. }` wrapper (redundant sugar in this
/// position, per Part B) so both syntaxes lower identically.
pub(super) fn comptime_type_alias_rhs(ty: &ast::Ty) -> Option<&ast::Expr> {
    match ty {
        ast::Ty::ConstBlock(const_block) => Some(const_block.expr.as_ref()),
        ast::Ty::Expr(expr) => Some(expr.as_ref()),
        _ => None,
    }
}

/// Shared with `canonical_type_path`'s own primitive-name check, and with
/// the tolerant-predeclare deferral check below, so both places recognize
/// the same set of names as "not a real registered type, don't bother
/// looking it up."
pub(super) fn is_primitive_type_name(name: &str) -> bool {
    matches!(
        name,
        "str"
            | "char"
            | "bool"
            | "i8"
            | "i16"
            | "i32"
            | "i64"
            | "i128"
            | "isize"
            | "u8"
            | "u16"
            | "u32"
            | "u64"
            | "u128"
            | "usize"
            | "f16"
            | "f32"
            | "f64"
            | "f128"
    )
}

/// Returns the self-type's head (first) segment name when it's a plain,
/// unprefixed name-based path — a bare single segment (`Vec`, or
/// `Vec<u8>` via a single-segment `Name::ParameterPath`), or the first
/// segment of a bare multi-segment path (`ops::RangeFull`, where `ops`
/// is a module brought into scope by a plain `use crate::{..., ops};`).
/// Either shape could plausibly still be waiting on an import that
/// hasn't been processed yet. Already-anchored paths (`crate::vec::Vec`,
/// `self::Foo`, `super::Foo`) and non-name self-types (blanket
/// `impl<T> Trait for T`) all return `None` — those are never deferred,
/// they fall straight through to today's immediate resolution/failure.
/// Same idea as `self_type_first_segment_name`, for a type alias's RHS
/// (`ast::Ty`, not `ast::Expr`) — `result::Result<(), Error>` and similar
/// module-qualified type references lower to `Ty::Expr(Name::Path(..))`
/// (see `comptime_type_alias_rhs`'s doc comment for the same shape used
/// elsewhere), so this just unwraps that one layer and delegates.
pub(super) fn type_alias_rhs_first_segment_name(ty: &ast::Ty) -> Option<&str> {
    match ty {
        ast::Ty::Expr(expr) => self_type_first_segment_name(expr),
        _ => None,
    }
}

pub(super) fn self_type_first_segment_name(self_ty: &ast::Expr) -> Option<&str> {
    let ast::ExprKind::Name(name) = self_ty.kind() else {
        return None;
    };
    match name {
        Name::Ident(ident) => Some(ident.name.as_str()),
        Name::Path(path) if path.prefix == fp_core::ast::path::PathPrefix::Plain => {
            path.segments.first().map(|seg| seg.name.as_str())
        }
        Name::ParameterPath(param_path)
            if param_path.prefix == fp_core::ast::path::PathPrefix::Plain =>
        {
            param_path
                .segments
                .first()
                .map(|seg| seg.ident.name.as_str())
        }
        _ => None,
    }
}

pub(super) fn signature_contains_quote(sig: &ast::FunctionSignature) -> bool {
    sig.params.iter().any(|param| ty_contains_quote(&param.ty))
        || sig.ret_ty.as_ref().is_some_and(ty_contains_quote)
}

#[allow(dead_code)]
pub(super) fn signature_contains_type_type(sig: &ast::FunctionSignature) -> bool {
    sig.params.iter().any(|param| {
        ty_contains_type_type(&param.ty)
            || param
                .ty_annotation
                .as_ref()
                .is_some_and(ty_contains_type_type)
    }) || sig.ret_ty.as_ref().is_some_and(ty_contains_type_type)
}

pub(super) fn ty_contains_quote(ty: &ast::Ty) -> bool {
    match ty {
        ast::Ty::Quote(_) => true,
        ast::Ty::Tuple(tuple) => tuple.types.iter().any(ty_contains_quote),
        ast::Ty::Array(array) => ty_contains_quote(&array.elem),
        ast::Ty::Vec(vec) => ty_contains_quote(&vec.ty),
        ast::Ty::Reference(reference) => ty_contains_quote(&reference.ty),
        ast::Ty::RawPtr(raw_ptr) => ty_contains_quote(&raw_ptr.ty),
        ast::Ty::Slice(slice) => ty_contains_quote(&slice.elem),
        ast::Ty::Struct(def) => def
            .fields
            .iter()
            .any(|field| ty_contains_quote(&field.value)),
        ast::Ty::Structural(def) => def
            .fields
            .iter()
            .any(|field| ty_contains_quote(&field.value)),
        ast::Ty::Enum(def) => def
            .variants
            .iter()
            .any(|variant| ty_contains_quote(&variant.value)),
        ast::Ty::Function(func) => {
            func.params.iter().any(ty_contains_quote)
                || func
                    .ret_ty
                    .as_ref()
                    .is_some_and(|ty| ty_contains_quote(ty.as_ref()))
        }
        ast::Ty::TypeBinaryOp(op) => ty_contains_quote(&op.lhs) || ty_contains_quote(&op.rhs),
        ast::Ty::TypeBounds(bounds) => bounds
            .bounds
            .iter()
            .any(|expr| expr_contains_quote_value(expr)),
        ast::Ty::Value(value) => value_contains_quote(value.value.as_ref()),
        ast::Ty::Expr(expr) => expr_contains_quote_value(expr.as_ref()),
        ast::Ty::ConstBlock(block) => expr_contains_quote_value(block.expr.as_ref()),
        ast::Ty::Refinement(refinement) => ty_contains_quote(&refinement.base),
        ast::Ty::Literal(_)
        | ast::Ty::Primitive(_)
        | ast::Ty::TokenStream(_)
        | ast::Ty::ImplTraits(_)
        | ast::Ty::Any(_)
        | ast::Ty::GenericVar(_)
        | ast::Ty::ErrorType(_)
        | ast::Ty::InferVar(_)
        | ast::Ty::Unit(_)
        | ast::Ty::Unknown(_)
        | ast::Ty::Nothing(_)
        | ast::Ty::Type(_)
        | ast::Ty::RequestedType(_)
        | ast::Ty::Wildcard(_) => false,
    }
}

#[allow(dead_code)]
pub(super) fn ty_contains_type_type(ty: &ast::Ty) -> bool {
    match ty {
        ast::Ty::Type(_) | ast::Ty::RequestedType(_) | ast::Ty::ConstBlock(_) => true,
        ast::Ty::Tuple(tuple) => tuple.types.iter().any(ty_contains_type_type),
        ast::Ty::Array(array) => ty_contains_type_type(&array.elem),
        ast::Ty::Vec(vec) => ty_contains_type_type(&vec.ty),
        ast::Ty::Reference(reference) => ty_contains_type_type(&reference.ty),
        ast::Ty::RawPtr(raw_ptr) => ty_contains_type_type(&raw_ptr.ty),
        ast::Ty::Slice(slice) => ty_contains_type_type(&slice.elem),
        ast::Ty::Struct(def) => def
            .fields
            .iter()
            .any(|field| ty_contains_type_type(&field.value)),
        ast::Ty::Structural(def) => def
            .fields
            .iter()
            .any(|field| ty_contains_type_type(&field.value)),
        ast::Ty::Enum(def) => def
            .variants
            .iter()
            .any(|variant| ty_contains_type_type(&variant.value)),
        ast::Ty::Function(func) => type_function_contains_type_type(func),
        ast::Ty::TypeBinaryOp(op) => {
            ty_contains_type_type(&op.lhs) || ty_contains_type_type(&op.rhs)
        }
        ast::Ty::TypeBounds(bounds) => bounds
            .bounds
            .iter()
            .any(|expr| expr_contains_type_type(expr)),
        ast::Ty::Value(value) => value_contains_type_type(value.value.as_ref()),
        ast::Ty::Expr(expr) => expr_contains_type_type(expr.as_ref()),
        ast::Ty::Refinement(refinement) => ty_contains_type_type(&refinement.base),
        ast::Ty::Literal(_)
        | ast::Ty::Primitive(_)
        | ast::Ty::TokenStream(_)
        | ast::Ty::ImplTraits(_)
        | ast::Ty::Any(_)
        | ast::Ty::GenericVar(_)
        | ast::Ty::ErrorType(_)
        | ast::Ty::InferVar(_)
        | ast::Ty::Unit(_)
        | ast::Ty::Unknown(_)
        | ast::Ty::Nothing(_)
        | ast::Ty::Quote(_)
        | ast::Ty::Wildcard(_) => false,
    }
}

#[allow(dead_code)]
pub(super) fn type_function_contains_type_type(func: &ast::TypeFunction) -> bool {
    func.params.iter().any(ty_contains_type_type)
        || func
            .ret_ty
            .as_ref()
            .is_some_and(|ty| ty_contains_type_type(ty.as_ref()))
}

pub(super) fn expr_contains_quote_value(expr: &ast::Expr) -> bool {
    if let ast::ExprKind::Value(value) = expr.kind() {
        return value_contains_quote(value.as_ref());
    }
    false
}

#[allow(dead_code)]
pub(super) fn expr_contains_type_type(expr: &ast::Expr) -> bool {
    fp_core::ast::resolved_expr_type(expr.id())
        .as_ref()
        .is_some_and(|ty| ty_contains_type_type(ty))
}

pub(super) fn value_contains_quote(value: &ast::Value) -> bool {
    match value {
        ast::Value::QuoteToken(_) => true,
        ast::Value::List(list) => {
            !list.values.is_empty() && list.values.iter().all(|value| value_contains_quote(value))
        }
        _ => false,
    }
}

#[allow(dead_code)]
pub(super) fn value_contains_type_type(value: &ast::Value) -> bool {
    match value {
        ast::Value::Type(ty) => ty_contains_type_type(ty),
        ast::Value::Expr(expr) => expr_contains_type_type(expr.as_ref()),
        ast::Value::List(list) => list.values.iter().any(value_contains_type_type),
        ast::Value::Struct(value) => value
            .structural
            .fields
            .iter()
            .any(|field| value_contains_type_type(&field.value)),
        ast::Value::Structural(value) => value
            .fields
            .iter()
            .any(|field| value_contains_type_type(&field.value)),
        ast::Value::Tuple(value) => value
            .values
            .iter()
            .any(|value| value_contains_type_type(value)),
        _ => false,
    }
}
