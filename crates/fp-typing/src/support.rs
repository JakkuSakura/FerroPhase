use fp_core::ast::{Expr, ExprKind};
use std::collections::HashSet;
use std::future::Future;
use std::pin::Pin;

pub type BoxFuture<'a, T> = Pin<Box<dyn Future<Output = T> + 'a>>;

#[cfg(test)]
pub(crate) fn block_on<F: std::future::Future>(fut: F) -> F::Output {
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
            "fp_typing test block_on: future returned Poll::Pending -- this helper only supports \
             futures that resolve on the very first poll (tests / synchronous callers with no \
             real package or comptime suspension); drive genuinely suspending futures through \
             fp-compiler's CompilerExecutor instead"
        ),
    }
}

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
