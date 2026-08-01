use fp_core::hir::{
    AttrMeta, AttrMetaList, Attribute, Expr, ExprKind, Ident, Name, ParameterPath,
    ParameterPathSegment, Path, Ty,
};
use fp_core::config;
use fp_core::module::path::PathPrefix;
use std::collections::HashSet;

pub fn block_on<F: std::future::Future>(fut: F) -> F::Output {
    use std::task::{Context, Poll, RawWaker, RawWakerVTable, Waker};

    fn no_wake(_: *const ()) {}
    fn clone_noop_waker(_: *const ()) -> RawWaker {
        RawWaker::new(std::ptr::null(), &VTABLE)
    }
    static VTABLE: RawWakerVTable = RawWakerVTable::new(clone_noop_waker, no_wake, no_wake, |_| {});

    let waker = unsafe { Waker::from_raw(RawWaker::new(std::ptr::null(), &VTABLE)) };
    let mut cx = Context::from_waker(&waker);
    let mut fut = std::pin::pin!(fut);
    match fut.as_mut().poll(&mut cx) {
        Poll::Ready(value) => value,
        Poll::Pending => panic!(
            "fp_typing::block_on: future returned Poll::Pending -- this helper only supports \
             futures that resolve on the very first poll (tests / synchronous callers with no \
             real package or comptime suspension); drive genuinely suspending futures through \
             fp-compiler's Executor instead"
        ),
    }
}

pub(crate) fn attrs_has_name(attrs: &[Attribute], name: &str) -> bool {
    attrs.iter().any(|attr| match &attr.meta {
        AttrMeta::Path(path) => path.last().as_str() == name,
        AttrMeta::NameValue(nv) => nv.name.last().as_str() == name,
        AttrMeta::List(list) => list.name.last().as_str() == name,
    })
}

pub(crate) fn attrs_has_feature(attrs: &[Attribute], feature: &str) -> bool {
    for attr in attrs {
        let AttrMeta::List(AttrMetaList { name, items }) = &attr.meta else {
            continue;
        };
        if name.last().as_str() != "feature" {
            continue;
        }
        if items
            .iter()
            .any(|item| matches!(item, AttrMeta::Path(path) if path.last().as_str() == feature))
        {
            return true;
        }
    }
    false
}

pub(crate) fn detect_lossy_mode() -> bool {
    config::lossy_mode()
}

pub fn default_extern_prelude() -> HashSet<String> {
    ["std", "core", "alloc"]
        .into_iter()
        .map(str::to_owned)
        .collect()
}

pub(crate) fn make_std_task_future_ty(inner: Ty) -> Ty {
    let future_seg = ParameterPathSegment::new(Ident::new("Future"), vec![inner]);
    let path = ParameterPath::new(
        PathPrefix::Plain,
        vec![
            ParameterPathSegment::new(Ident::new("std"), vec![]),
            ParameterPathSegment::new(Ident::new("task"), vec![]),
            future_seg,
        ],
    );
    Ty::name(Name::ParameterPath(path))
}

pub(crate) fn make_std_result_ty(ok: Ty, err: Ty) -> Ty {
    let result_seg = ParameterPathSegment::new(Ident::new("Result"), vec![ok, err]);
    let path = ParameterPath::new(
        PathPrefix::Plain,
        vec![
            ParameterPathSegment::new(Ident::new("std"), vec![]),
            ParameterPathSegment::new(Ident::new("result"), vec![]),
            result_seg,
        ],
    );
    Ty::name(Name::ParameterPath(path))
}

pub(crate) fn std_error_ty() -> Ty {
    Ty::name(Name::Path(Path::plain(vec![
        Ident::new("std"),
        Ident::new("error"),
        Ident::new("Error"),
    ])))
}

pub(crate) fn std_result_inner_types(ty: &Ty) -> Option<(Ty, Ty)> {
    let Ty::Expr(expr) = ty else { return None };
    let ExprKind::Name(Name::ParameterPath(path)) = expr.kind() else {
        return None;
    };
    if path.segments.len() == 1 {
        let result_seg = &path.segments[0];
        return (result_seg.ident.as_str() == "Result" && result_seg.args.len() == 2)
            .then(|| (result_seg.args[0].clone(), result_seg.args[1].clone()));
    }
    if path.segments.len() < 3 {
        return None;
    }
    let n = path.segments.len();
    let result_seg = &path.segments[n - 1];
    let valid_prefix = path.segments[n - 3].ident.as_str() == "std"
        && matches!(path.segments[n - 2].ident.as_str(), "result" | "fs");
    (valid_prefix && result_seg.ident.as_str() == "Result" && result_seg.args.len() == 2)
        .then(|| (result_seg.args[0].clone(), result_seg.args[1].clone()))
}

pub(crate) fn is_std_result_ty(ty: &Ty) -> bool {
    std_result_inner_types(ty).is_some()
}

pub(crate) fn std_task_future_inner_ty(ty: &Ty) -> Option<Ty> {
    let Ty::Expr(expr) = ty else { return None };
    let ExprKind::Name(Name::ParameterPath(path)) = expr.kind() else {
        return None;
    };
    if path.segments.len() < 3 {
        return None;
    }
    let n = path.segments.len();
    (path.segments[n - 3].ident.as_str() == "std"
        && path.segments[n - 2].ident.as_str() == "task"
        && path.segments[n - 1].ident.as_str() == "Future"
        && path.segments[n - 1].args.len() == 1)
        .then(|| path.segments[n - 1].args[0].clone())
}

pub(crate) fn is_std_task_future_ty(ty: &Ty) -> bool {
    std_task_future_inner_ty(ty).is_some()
}

pub(crate) fn is_future_like_ty(ty: &Ty) -> bool {
    is_std_task_future_ty(ty)
        || matches!(ty, Ty::Struct(struct_ty) if struct_ty.name.as_str() == "Future")
}

pub(crate) fn tokenize_macro_tokens(tokens: &str) -> Vec<&str> {
    tokens.split_whitespace().collect()
}

pub(crate) fn is_ident_token(token: &str) -> bool {
    let mut chars = token.chars();
    matches!(chars.next(), Some(c) if c == '_' || c.is_ascii_alphabetic())
        && chars.all(|c| c == '_' || c.is_ascii_alphanumeric())
}

pub(crate) fn find_ident_after_keyword(tokens: &[&str], keyword: &str) -> Option<String> {
    let mut iter = tokens.iter().peekable();
    while let Some(token) = iter.next() {
        if *token == keyword {
            return iter
                .find(|next| is_ident_token(next))
                .map(|next| (*next).to_owned());
        }
    }
    None
}

pub(crate) fn find_first_type_ident(tokens: &[&str]) -> Option<String> {
    tokens.iter().find_map(|token| {
        is_ident_token(token)
            .then_some(*token)
            .filter(|token| token.chars().next().is_some_and(|c| c.is_ascii_uppercase()))
            .map(str::to_owned)
    })
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
