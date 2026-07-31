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
/// no `resolution_hook` set and no sibling task that could ever resolve it
/// (see `AstTypeInferencer::await_comptime`'s doc comment): resolution is
/// genuinely hopeless, so the item's own inline attempt
/// (`best_effort_resolve_comptime`) tolerates the failure instead of
/// suspending forever waiting on something that will never happen — real
/// `.fp` examples rely on exactly this (a `const` whose value a same-pass
/// hook attempt can't fold is expected to degrade gracefully, not deadlock
/// the whole compile unit). The type falls back to `Unknown` with a
/// recorded error diagnostic, and typing of the rest of the module proceeds.
#[test]
fn const_block_deftype_completes_with_diagnostic_when_genuinely_unresolvable() {
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
        Poll::Ready(Ok(_)) => {}
        Poll::Pending => panic!("expected graceful completion, got genuine suspension instead"),
        Poll::Ready(Err(err)) => panic!("expected graceful completion, got error: {err}"),
    }

    assert!(
        typing_ctx
            .diagnostics
            .borrow()
            .iter()
            .any(|d| matches!(d.level, TypingDiagnosticLevel::Error)),
        "a const-block type alias that never resolved to a struct should record an error diagnostic"
    );
}

/// An earlier item's const-block value being unresolvable (see the test
/// above) tolerates gracefully and does not block the rest of the module —
/// so a later, unrelated item's own genuine type error still surfaces
/// normally, exactly as if the earlier item weren't there at all.
#[test]
fn unresolvable_earlier_item_does_not_mask_a_later_items_genuine_error() {
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
        Poll::Ready(Err(_)) => {}
        Poll::Ready(Ok(_)) => panic!("expected the later item's genuine type error to surface"),
        Poll::Pending => panic!("expected graceful completion of the first item, not suspension"),
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
