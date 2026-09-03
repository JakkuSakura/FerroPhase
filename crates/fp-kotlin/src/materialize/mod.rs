use fp_core::ast::{
    BlockStmt, Expr, ExprAssign, ExprAwait, ExprBinOp, ExprBlock, ExprClosure, ExprFieldAccess,
    ExprIntrinsicCall, ExprIntrinsicContainer, ExprInvoke, ExprInvokeTarget, ExprKind, Ident, Name,
    Path, Pattern, PatternIdent, PatternKind, PatternType, Ty, TySlot, TypeInt, TypePrimitive,
    Value,
};
use fp_core::error::Result;
use fp_core::intrinsics::{CallKind, IntrinsicMaterializer, MaterializeOutcome, PortableOpCall};

trait PortableCallRef {
    fn portable_call(&self) -> &PortableOpCall;
}

impl PortableCallRef for PortableOpCall {
    fn portable_call(&self) -> &PortableOpCall {
        self
    }
}

impl PortableCallRef for &PortableOpCall {
    fn portable_call(&self) -> &PortableOpCall {
        self
    }
}

use fp_core::ops::BinOpKind;
mod types;
use types::*;
mod operations;
pub use operations::KotlinMaterializer;

/// Entry point for Kotlin target materialization. The shared framework owns
/// AST traversal; KotlinMaterializer supplies only target-specific rewrites.
pub(crate) fn materialize_kotlin_item(item: fp_core::ast::Item) -> Result<fp_core::ast::Item> {
    fp_core::intrinsics::materialize_item(item, &KotlinMaterializer)
}
fn assign_byte_vector(target: Expr, value: Expr) -> Expr {
    Expr::new(ExprKind::Assign(ExprAssign {
        span: Default::default(),
        target: Box::new(target),
        value: Box::new(value),
    }))
}

fn kotlin_owned_value(value: Expr, ty: &TySlot) -> Expr {
    if is_byte_vector(ty) {
        return runtime_method("bytesFromIterable", vec![value]);
    }
    if ty.as_ref().is_some_and(is_collection_ty) {
        return runtime_method("mutableListFromIterable", vec![value]);
    }
    value
}

fn is_byte_vector(ty: &TySlot) -> bool {
    ty.as_ref().is_some_and(is_byte_vector_ty)
}

fn is_byte_vector_ty(ty: &Ty) -> bool {
    match ty {
        Ty::Vec(vector) => is_u8_type(&vector.ty),
        Ty::Expr(expr) => match expr.kind() {
            ExprKind::Name(Name { path: path, .. }) => {
                path.last().ident.as_str() == "Vec"
                    && path.last().args.len() == 1
                    && is_u8_type(&path.last().args[0])
            }
            _ => false,
        },
        _ => false,
    }
}

fn is_collection_ty(ty: &Ty) -> bool {
    match ty {
        Ty::Vec(_) | Ty::Slice(_) => true,
        Ty::Expr(expr) => match expr.kind() {
            ExprKind::Name(Name { path: path, .. }) => {
                matches!(
                    path.last().ident.as_str(),
                    "Vec"
                        | "MutableList"
                        | "List"
                        | "to_vec"
                        | "to_vec_in"
                        | "slice_to_vec"
                        | "slice_to_vec_in"
                )
            }
            _ => false,
        },
        _ => false,
    }
}

fn is_u8_type(ty: &Ty) -> bool {
    matches!(ty, Ty::Primitive(TypePrimitive::Int(TypeInt::U8)))
        || matches!(ty, Ty::Expr(expr) if matches!(expr.kind(), ExprKind::Name(Name { path, .. }) if path.last().as_str() == "u8"))
}

fn kotlin_default_value(ty: &TySlot) -> Option<Expr> {
    let ty = ty.as_ref()?;
    if is_byte_vector_ty(ty) {
        return Some(invoke_function(
            "ByteArray",
            vec![Expr::value(Value::int(0))],
        ));
    }
    if is_collection_ty(ty) {
        return Some(invoke_function("mutableListOf", Vec::new()));
    }
    match ty {
        Ty::Primitive(TypePrimitive::Bool) => Some(Expr::value(Value::bool(false))),
        Ty::Primitive(TypePrimitive::Char) => {
            Some(Expr::value(Value::Char(fp_core::ast::ValueChar::new('\0'))))
        }
        Ty::Primitive(TypePrimitive::String) => Some(Expr::value(Value::string(String::new()))),
        Ty::Primitive(TypePrimitive::Int(_)) => Some(Expr::value(Value::int(0))),
        Ty::Primitive(TypePrimitive::Decimal(_)) => Some(Expr::value(Value::decimal(0.0))),
        Ty::Expr(expr) => match expr.kind() {
            ExprKind::Name(Name { path, .. }) => {
                let names = path
                    .segments
                    .iter()
                    .map(|segment| segment.as_str())
                    .collect::<Vec<_>>();
                Some(invoke_static_method(&names, "default", Vec::new()))
            }
            _ => None,
        },
        _ => None,
    }
}

fn invoke_function(name: &str, args: Vec<Expr>) -> Expr {
    Expr::new(ExprKind::Invoke(ExprInvoke {
        span: Default::default(),
        target: ExprInvokeTarget::Function(Name::ident(name)),
        args,
        kwargs: Vec::new(),
    }))
}

fn runtime_method(method: &str, args: Vec<Expr>) -> Expr {
    invoke_static_method(&["RustKotlinRuntime"], method, args)
}

fn invoke_static_method(receiver: &[&str], method: &str, args: Vec<Expr>) -> Expr {
    Expr::new(ExprKind::Invoke(ExprInvoke {
        span: Default::default(),
        target: ExprInvokeTarget::Method(ExprFieldAccess {
            span: Default::default(),
            obj: Box::new(Expr::name(Name::path(Path::plain(
                receiver
                    .iter()
                    .map(|segment| Ident::new(*segment))
                    .collect(),
            )))),
            field: Ident::new(method),
            generic_args: Vec::new(),
        }),
        args,
        kwargs: Vec::new(),
    }))
}

fn run_catching(body: Expr) -> Expr {
    Expr::new(ExprKind::Invoke(ExprInvoke {
        span: Default::default(),
        target: ExprInvokeTarget::Function(Name::ident("runCatching")),
        args: vec![Expr::new(ExprKind::Closure(ExprClosure {
            span: Default::default(),
            params: Vec::new(),
            ret_ty: None,
            movability: None,
            body: Box::new(body),
        }))],
        kwargs: Vec::new(),
    }))
}

fn unit_block(statement: Expr) -> Expr {
    Expr::new(ExprKind::Block(ExprBlock::new_stmts_expr(
        vec![BlockStmt::Expr(
            fp_core::ast::BlockStmtExpr::new(statement).with_semicolon(true),
        )],
        Expr::name(Name::ident("Unit")),
    )))
}

fn static_property(receiver: &[&str], property: &str) -> Expr {
    select_property(
        Expr::name(Name::path(Path::plain(
            receiver
                .iter()
                .map(|segment| Ident::new(*segment))
                .collect(),
        ))),
        property,
    )
}

/// Kotlin's `Result` fixes its error carrier to `Throwable`, while Rust
/// permits any `E`. Normalize it in the runtime so constructors and mapped
/// errors follow one target-specific rule.
fn normalize_error(error: Expr) -> Expr {
    runtime_method("normalizeError", vec![error])
}

fn throwable_pattern(ident: Ident) -> Pattern {
    Pattern::from(PatternKind::Type(PatternType::new(
        Pattern::from(PatternKind::Ident(PatternIdent {
            ident,
            mutability: None,
        })),
        Ty::path(Path::plain(vec![Ident::new("Throwable")])),
    )))
}

fn error_mapping(call: impl PortableCallRef) -> Expr {
    result_error_mapping(call.portable_call().args.get(1).cloned())
}

fn result_error_mapping(mapper: Option<Expr>) -> Expr {
    let mapper = mapper.unwrap_or_else(|| Expr::value(Value::Null(Default::default())));
    match mapper.kind().clone() {
        ExprKind::Closure(mut closure) => {
            normalize_error_closure_params(&mut closure);
            closure.body = Box::new(normalize_error(*closure.body));
            Expr::new(ExprKind::Closure(closure))
        }
        _ => {
            let error_ident = Ident::new("__fp_error");
            let mapped = Expr::new(ExprKind::Invoke(ExprInvoke {
                span: Default::default(),
                target: ExprInvokeTarget::expr(mapper),
                args: vec![Expr::name(Name::ident(error_ident.name.clone()))],
                kwargs: Vec::new(),
            }));
            Expr::new(ExprKind::Closure(ExprClosure {
                span: Default::default(),
                params: vec![throwable_pattern(error_ident)],
                ret_ty: Some(Box::new(Ty::path(Path::plain(vec![Ident::new(
                    "Throwable",
                )])))),
                movability: None,
                body: Box::new(normalize_error(mapped)),
            }))
        }
    }
}

fn normalize_error_closure_params(closure: &mut ExprClosure) {
    let Some(param) = closure.params.first_mut() else {
        return;
    };
    let throwable = Ty::path(Path::plain(vec![Ident::new("Throwable")]));
    let original = param.clone();
    *param = match original.kind().clone() {
        PatternKind::Type(mut typed) => {
            typed.ty = throwable;
            Pattern::from(PatternKind::Type(typed))
        }
        _ => Pattern::from(PatternKind::Type(PatternType::new(original, throwable))),
    };
}

fn result_constructor_arg(call: impl PortableCallRef) -> Expr {
    call.portable_call()
        .args
        .first()
        .cloned()
        .unwrap_or_else(|| Expr::value(Value::Null(Default::default())))
}

fn result_success_arg(call: impl PortableCallRef) -> Expr {
    let arg = result_constructor_arg(call);
    if matches!(arg.kind(), ExprKind::Value(value) if matches!(&**value, Value::Unit(_))) {
        Expr::name(Name::ident("Unit"))
    } else {
        arg
    }
}

fn portable_op_args_after_receiver(call: impl PortableCallRef) -> Vec<Expr> {
    call.portable_call().args.iter().skip(1).cloned().collect()
}

#[cfg(test)]
mod tests;

fn invoke_method(receiver: Expr, method: &str, args: Vec<Expr>) -> Expr {
    Expr::new(ExprKind::Invoke(ExprInvoke {
        span: Default::default(),
        target: ExprInvokeTarget::Method(ExprFieldAccess {
            span: Default::default(),
            obj: Box::new(receiver),
            field: Ident::new(method),
            generic_args: Vec::new(),
        }),
        args,
        kwargs: Vec::new(),
    }))
}

fn select_property(receiver: Expr, property: &str) -> Expr {
    Expr::new(ExprKind::FieldAccess(ExprFieldAccess {
        span: Default::default(),
        obj: Box::new(receiver),
        field: Ident::new(property),
        generic_args: Vec::new(),
    }))
}
