use std::pin::Pin;
use std::task::{Context, Poll, Waker};

use fp_core::ast::*;
use fp_core::span::Span;
use fp_typing::{AstTypeInferencer, TypingDiagnosticLevel};

fn const_block_deftype(name: &str, inner: Expr) -> Item {
    let block = ExprConstBlock {
        span: Span::default(),
        collected_items: Vec::new(),
        expr: Box::new(inner),
    };
    let def = ItemDefType {
        attrs: Vec::new(),
        visibility: Visibility::Public,
        name: Ident::new(name),
        generics_params: Vec::new(),
        value: Ty::ConstBlock(block),
    };
    Item::from(ItemKind::DefType(def))
}

fn plain_deftype(name: &str, target: &str) -> Item {
    let def = ItemDefType {
        attrs: Vec::new(),
        visibility: Visibility::Public,
        name: Ident::new(name),
        generics_params: Vec::new(),
        value: Ty::expr(Expr::ident(Ident::new(target))),
    };
    Item::from(ItemKind::DefType(def))
}

fn poll_once<F: std::future::Future>(fut: &mut Pin<Box<F>>) -> Poll<F::Output> {
    let waker = Waker::noop();
    let mut cx = Context::from_waker(waker);
    fut.as_mut().poll(&mut cx)
}

/// A const-block `DefType` whose body references an unresolvable symbol, with
/// no `resolution_hook` set: `await_comptime` genuinely suspends on a real
/// `Waker` (see `AstTypeInferencer::await_comptime`) rather than hard-failing
/// on the first poll. Nothing in this test ever resolves `UndefinedThing`, so
/// the future must stay `Pending` -- that's the whole point being verified,
/// not a stand-in for eventually succeeding.
#[test]
fn const_block_deftype_suspends_instead_of_erroring_when_blocked() {
    let item = const_block_deftype("Foo", Expr::ident(Ident::new("UndefinedThing")));
    let mut file = File {
        path: "const_block.fp".into(),
        attrs: Vec::new(),
        collected_items: Vec::new(),
        items: vec![item],
    };

    let typing_ctx = std::rc::Rc::new(fp_typing::TypingContext::new(std::rc::Rc::new(
        fp_core::workspace::WorkspaceContext::new(),
    )));
    let typer = AstTypeInferencer::new(typing_ctx.clone());
    let mut fut = Box::pin(typer.infer_file(&mut file));

    match poll_once(&mut fut) {
        Poll::Pending => {}
        Poll::Ready(Ok(_)) => panic!("expected genuine suspension, resolved instead"),
        Poll::Ready(Err(err)) => panic!("expected genuine suspension, got error: {err}"),
    }

    assert!(
        !typing_ctx
            .diagnostics
            .borrow()
            .iter()
            .any(|d| matches!(d.level, TypingDiagnosticLevel::Error)),
        "a task suspended waiting on a comptime value must not record an error diagnostic"
    );
}

/// When an earlier item in the same module is genuinely blocked (a const
/// block awaiting a value nothing will ever provide), the module's sequential
/// item loop awaits it in place -- a later, unrelated item's genuine error
/// must never get the chance to surface: the whole module future just stays
/// `Pending` at the blocked item, exactly like plain `async`/`await` code
/// blocking on the first of two sequentially-awaited futures.
#[test]
fn blocked_earlier_item_suspends_the_whole_module_before_a_later_error_can_surface() {
    let blocked = const_block_deftype("Blocked", Expr::ident(Ident::new("UndefinedThing")));
    let broken = plain_deftype("Broken", "TotallyUnknownTypeName");
    let mut file = File {
        path: "mixed.fp".into(),
        attrs: Vec::new(),
        collected_items: Vec::new(),
        items: vec![blocked, broken],
    };

    let typing_ctx = std::rc::Rc::new(fp_typing::TypingContext::new(std::rc::Rc::new(
        fp_core::workspace::WorkspaceContext::new(),
    )));
    let typer = AstTypeInferencer::new(typing_ctx.clone());
    let mut fut = Box::pin(typer.infer_file(&mut file));

    match poll_once(&mut fut) {
        Poll::Pending => {}
        Poll::Ready(Ok(_)) => panic!("expected genuine suspension, resolved instead"),
        Poll::Ready(Err(err)) => panic!(
            "the later item's error must not surface while the earlier item is still blocked: {err}"
        ),
    }
}

/// Without any pending request at all, a genuine error must still surface —
/// the fix must not over-suppress errors that have nothing to defer for.
#[test]
fn genuine_error_still_surfaces_when_nothing_is_pending() {
    let broken = plain_deftype("Broken", "TotallyUnknownTypeName");
    let mut file = File {
        path: "broken.fp".into(),
        attrs: Vec::new(),
        collected_items: Vec::new(),
        items: vec![broken],
    };

    let typing_ctx = std::rc::Rc::new(fp_typing::TypingContext::new(std::rc::Rc::new(
        fp_core::workspace::WorkspaceContext::new(),
    )));
    let typer = AstTypeInferencer::new(typing_ctx);
    let result = fp_typing::block_on(typer.infer_file(&mut file));

    assert!(
        result.is_err(),
        "a genuinely unresolvable type with no pending request must fail, not defer forever"
    );
}
