use fp_core::ast::*;
use fp_core::span::Span;
use fp_typing::{AstTypeInferencer, PendingTypingRequestKind, TypingDiagnosticLevel};

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

/// A const-block `DefType` whose body references an unresolvable symbol,
/// with no `resolution_hook` set — `request_comptime` reports "genuinely
/// blocked" every time (see `TypeResolutionHook::request_comptime`'s
/// contract), simulating a package that hasn't loaded yet without needing
/// real package plumbing.
#[test]
fn const_block_deftype_defers_instead_of_erroring_when_blocked() {
    let item = const_block_deftype("Foo", Expr::ident(Ident::new("UndefinedThing")));
    let file = File {
        path: "const_block.fp".into(),
        attrs: Vec::new(),
        collected_items: Vec::new(),
        items: vec![item],
    };

    let typing_ctx = std::rc::Rc::new(fp_typing::TypingContext::new(std::rc::Rc::new(
        fp_core::workspace::WorkspaceContext::new(),
    )));
    let mut typer = AstTypeInferencer::new(typing_ctx.clone());
    let mut file = file;
    let outcome = typer
        .infer_file(&mut file)
        .expect("a genuinely-blocked const block must defer, not hard-fail");

    assert!(
        outcome
            .pending_requests
            .iter()
            .any(|r| matches!(r.kind, PendingTypingRequestKind::Comptime)),
        "expected a Comptime pending request, got {:?}",
        outcome.pending_requests
    );
    assert!(
        !typing_ctx
            .diagnostics
            .borrow()
            .iter()
            .any(|d| matches!(d.level, TypingDiagnosticLevel::Error)),
        "a doomed-to-retry pass must not permanently record an error diagnostic"
    );
}

/// When an earlier item in the same module leaves an actionable pending
/// request (a still-blocked const block), a *later* item's genuine, unrelated
/// hard failure must still be deferred rather than surfacing immediately —
/// the whole module gets retried together. This is what distinguishes the
/// fix from the old behavior, where any item's `Err` aborted the module
/// before `pending_requests` was ever collected.
#[test]
fn actionable_pending_defers_a_later_unrelated_error_in_the_same_module() {
    let blocked = const_block_deftype("Blocked", Expr::ident(Ident::new("UndefinedThing")));
    let broken = plain_deftype("Broken", "TotallyUnknownTypeName");
    let file = File {
        path: "mixed.fp".into(),
        attrs: Vec::new(),
        collected_items: Vec::new(),
        items: vec![blocked, broken],
    };

    let typing_ctx = std::rc::Rc::new(fp_typing::TypingContext::new(std::rc::Rc::new(
        fp_core::workspace::WorkspaceContext::new(),
    )));
    let mut typer = AstTypeInferencer::new(typing_ctx.clone());
    let mut file = file;
    let outcome = typer.infer_file(&mut file).expect(
        "an actionable pending request earlier in the module must defer a later item's error",
    );

    assert!(outcome
        .pending_requests
        .iter()
        .any(|r| matches!(r.kind, PendingTypingRequestKind::Comptime)));
}

/// Without any pending request at all, a genuine error must still surface —
/// the fix must not over-suppress errors that have nothing to defer for.
#[test]
fn genuine_error_still_surfaces_when_nothing_is_pending() {
    let broken = plain_deftype("Broken", "TotallyUnknownTypeName");
    let file = File {
        path: "broken.fp".into(),
        attrs: Vec::new(),
        collected_items: Vec::new(),
        items: vec![broken],
    };

    let typing_ctx = std::rc::Rc::new(fp_typing::TypingContext::new(std::rc::Rc::new(
        fp_core::workspace::WorkspaceContext::new(),
    )));
    let mut typer = AstTypeInferencer::new(typing_ctx);
    let mut file = file;
    let result = typer.infer_file(&mut file);

    assert!(
        result.is_err(),
        "a genuinely unresolvable type with no pending request must fail, not defer forever"
    );
}
