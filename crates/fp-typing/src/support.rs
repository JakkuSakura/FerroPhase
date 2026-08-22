use fp_core::ast::{Expr, ExprKind};
use std::collections::HashSet;
use std::future::Future;
use std::pin::Pin;

pub type BoxFuture<'a, T> = Pin<Box<dyn Future<Output = T> + 'a>>;

pub fn default_extern_prelude() -> HashSet<String> {
    ["std", "core", "alloc"]
        .into_iter()
        .map(str::to_owned)
        .collect()
}

pub fn impl_self_ty_name(expr: &Expr) -> Option<String> {
    match expr.kind() {
        ExprKind::Name(name) => name
            .to_path()
            .segments
            .last()
            .map(|ident| ident.as_str().to_owned()),
        _ => None,
    }
}
