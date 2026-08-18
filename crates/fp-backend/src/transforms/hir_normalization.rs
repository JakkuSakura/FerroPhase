//! Formalizes HIR-level `#[op(...)]` recognition as a standalone,
//! HIR-mutating pass that runs once, post-typecheck, on the shared HIR
//! before either pipeline (`TypecheckedTranspile`/Kotlin AST-lift, or
//! `Native`/`hir_to_mir`) consumes it.
//!
//! This formalizes what used to be four private methods on
//! `HirToAstLifter` (`try_lift_call_as_intrinsic`,
//! `try_lift_struct_as_intrinsic`, `try_lift_path_as_intrinsic`,
//! `try_lift_method_call_as_intrinsic`): each checked
//! `program.op_defs.get(def_id)` (an `OpKind`) keyed by a resolved
//! `hir::Res::Def(def_id)` (for `Path`/`Struct`/`Call`) or
//! `TypeckResults::method_resolutions` (for `MethodCall`), and on a hit
//! built an `IntrinsicCall(CallKind::Op(op))` node. Promoting this
//! recognition into a real HIR mutation (rather than doing it lazily,
//! only inside the AST-lifter) lets `Native`'s `hir_to_mir` pipeline see
//! the exact same promoted shape when it's asked to (`promote_op_only:
//! true`), instead of duplicating this logic a second time.
//!
//! Note `intrinsic_defs` (the other map, for `#[intrinsic = "..."]`-tagged
//! free functions) is NOT checked here — that recognition already happens
//! earlier, pre-typecheck, in `FerroIntrinsicNormalizer`
//! (`crates/fp-lang/src/normalization.rs`) for `Compile`/`Transpile` modes,
//! or is deliberately deferred until here for `TypedTranspile` (a real
//! `DefId` isn't available before typecheck). This pass only needs to
//! cover the shapes that can only be recognized post-typecheck: enum-variant
//! `Path`/`Struct` construction and `MethodCall` (both only resolvable by
//! real `DefId`/`method_resolutions` after typechecking), plus `Call` as a
//! catch-all.

use fp_core::hir;
use fp_core::hir::DefId;
use fp_core::intrinsics::CallKind;
use fp_typing::TypeckResults;

/// Resolves the promoted `CallKind` for a definition recognized purely by
/// its own resolved identity (`DefId`) — an `#[op(...)]`-tagged
/// declaration (`hir::Program::op_defs`). `#[intrinsic = "..."]`-tagged
/// free functions are deliberately NOT resolved here — see the module doc
/// comment for where that recognition happens instead.
fn resolve_op_call_kind(op_defs: &std::collections::HashMap<DefId, fp_core::intrinsics::OpKind>, def_id: DefId) -> Option<CallKind> {
    op_defs.get(&def_id).copied().map(CallKind::Op)
}

/// Normalizes every item in `program`'s HIR in place.
///
/// `promote_op_only`: when `true`, `#[op(...)]`-tagged enum-variant
/// construction (`Path`/`Struct`) and method calls (and, as a catch-all,
/// plain `Call`s) whose callee/receiver resolves to an `op_defs` entry are
/// rewritten to `hir::ExprKind::IntrinsicCall(CallKind::Op(..))` nodes in
/// place. When `false`, this is a no-op pass-through: the HIR is still
/// walked (for uniformity/future extension) but nothing is rewritten —
/// used by the `Native` pipeline, which lowers un-promoted `Op`s as
/// ordinary calls to their real stub bodies instead (see
/// `hir_materialization.rs` for why).
pub fn normalize_program(program: &mut hir::Program, typeck: Option<&TypeckResults>, promote_op_only: bool) {
    // Snapshot `op_defs` up front: we can't hold `&program.op_defs` while
    // mutating `program.items` in place, and this map is small/cheap to
    // clone relative to the HIR it's consulted against.
    let op_defs = program.op_defs.clone();
    for item in &mut program.items {
        normalize_item(item, &op_defs, typeck, promote_op_only);
    }
}

fn normalize_item(
    item: &mut hir::Item,
    op_defs: &std::collections::HashMap<DefId, fp_core::intrinsics::OpKind>,
    typeck: Option<&TypeckResults>,
    promote_op_only: bool,
) {
    match &mut item.kind {
        hir::ItemKind::Function(function) => {
            if let Some(body) = &mut function.body {
                normalize_block(body, op_defs, typeck, promote_op_only);
            }
        }
        hir::ItemKind::Const(def) => {
            normalize_expr(&mut def.body.value, op_defs, typeck, promote_op_only);
        }
        hir::ItemKind::Enum(def) => {
            for variant in &mut def.variants {
                if let Some(discriminant) = &mut variant.discriminant {
                    normalize_expr(discriminant, op_defs, typeck, promote_op_only);
                }
            }
        }
        hir::ItemKind::Expr(expr) => {
            normalize_expr(expr, op_defs, typeck, promote_op_only);
        }
        hir::ItemKind::Impl(imp) => {
            for impl_item in &mut imp.items {
                if let hir::ImplItemKind::Method(function) = &mut impl_item.kind {
                    if let Some(body) = &mut function.body {
                        normalize_block(body, op_defs, typeck, promote_op_only);
                    }
                }
            }
        }
        hir::ItemKind::Struct(_) | hir::ItemKind::Query(_) | hir::ItemKind::Trait(_) => {
            // No expressions to walk (trait default-method bodies are
            // never emitted to a backend directly — they're only a
            // fallback signature source for `method_output`'s typechecking
            // resolution, not something `hir_normalization`/materialization
            // needs to promote ops within).
        }
    }
}

fn normalize_block(
    block: &mut hir::Block,
    op_defs: &std::collections::HashMap<DefId, fp_core::intrinsics::OpKind>,
    typeck: Option<&TypeckResults>,
    promote_op_only: bool,
) {
    for stmt in &mut block.stmts {
        normalize_stmt(stmt, op_defs, typeck, promote_op_only);
    }
    if let Some(expr) = &mut block.expr {
        normalize_expr(expr, op_defs, typeck, promote_op_only);
    }
}

fn normalize_stmt(
    stmt: &mut hir::Stmt,
    op_defs: &std::collections::HashMap<DefId, fp_core::intrinsics::OpKind>,
    typeck: Option<&TypeckResults>,
    promote_op_only: bool,
) {
    match &mut stmt.kind {
        hir::StmtKind::Local(local) => {
            if let Some(init) = &mut local.init {
                normalize_expr(init, op_defs, typeck, promote_op_only);
            }
        }
        hir::StmtKind::Item(item) => normalize_item(item, op_defs, typeck, promote_op_only),
        hir::StmtKind::Expr(expr) | hir::StmtKind::Semi(expr) => {
            normalize_expr(expr, op_defs, typeck, promote_op_only)
        }
    }
}

/// Recurses into `expr`'s children first (in place), then — only if
/// `promote_op_only` is true — attempts to rewrite `expr` itself if it
/// matches one of the recognized `#[op(...)]` shapes. Thin wrapper over
/// `normalize_expr_inner` that always allows self-promotion — see that
/// function's doc for the one case that deliberately doesn't.
fn normalize_expr(
    expr: &mut hir::Expr,
    op_defs: &std::collections::HashMap<DefId, fp_core::intrinsics::OpKind>,
    typeck: Option<&TypeckResults>,
    promote_op_only: bool,
) {
    normalize_expr_inner(expr, op_defs, typeck, promote_op_only, true);
}

/// Same as `normalize_expr`, but `promote_self` controls whether `expr`
/// itself is eligible for the trailing `try_promote_op` attempt (children
/// are always recursed into and always eligible via `normalize_expr`).
///
/// This distinction exists for exactly one case: a `Call`'s own callee.
/// `try_promote_op`'s `Path` arm recognizes *any* bare path resolving to
/// an `#[op(...)]`-tagged declaration and promotes it — correct for a
/// path used as a plain value (e.g. a unit-variant reference like
/// `None`), but wrong for a path that's specifically a `Call`'s callee
/// (e.g. `Ok` in `Ok(default())`): recursing into it with self-promotion
/// allowed would rewrite the *callee alone* into
/// `IntrinsicCall(Op(ResultOk), [])` (zero args, since the callee node
/// has none of its own) before the enclosing `Call` node's own
/// `try_promote_op` (which correctly carries the real arguments) ever
/// runs — and once the callee is no longer `ExprKind::Path`, the
/// enclosing `Call` arm's `let ExprKind::Path(path) = &callee.kind else
/// { return None }` guard silently fails, permanently losing the
/// promotion. Confirmed via targeted tracing to be the exact mechanism
/// behind `Ok`/`Some`/`Err` never reaching `KotlinMaterializer` despite
/// correct name resolution. So: recurse into a callee's own children as
/// normal, but never let the callee node itself self-promote — only the
/// enclosing `Call` may promote using the real argument list.
fn normalize_expr_inner(
    expr: &mut hir::Expr,
    op_defs: &std::collections::HashMap<DefId, fp_core::intrinsics::OpKind>,
    typeck: Option<&TypeckResults>,
    promote_op_only: bool,
    promote_self: bool,
) {
    match &mut expr.kind {
        hir::ExprKind::Path(_) => {
            // No children to recurse into.
        }
        hir::ExprKind::Query(_) => {}
        hir::ExprKind::Binary(_, lhs, rhs) | hir::ExprKind::Assign(lhs, rhs) => {
            normalize_expr(lhs, op_defs, typeck, promote_op_only);
            normalize_expr(rhs, op_defs, typeck, promote_op_only);
        }
        hir::ExprKind::Unary(_, value)
        | hir::ExprKind::FieldAccess(value, _)
        | hir::ExprKind::Cast(value, _)
        | hir::ExprKind::Return(Some(value))
        | hir::ExprKind::Break(Some(value)) => {
            normalize_expr(value, op_defs, typeck, promote_op_only);
        }
        hir::ExprKind::Reference(reference) => {
            normalize_expr(&mut reference.expr, op_defs, typeck, promote_op_only);
        }
        hir::ExprKind::Call(callee, args) => {
            // See this function's doc comment: the callee must not
            // self-promote independently of this enclosing `Call`.
            normalize_expr_inner(callee, op_defs, typeck, promote_op_only, false);
            for arg in args.iter_mut() {
                normalize_expr(&mut arg.value, op_defs, typeck, promote_op_only);
            }
        }
        hir::ExprKind::MethodCall(receiver, _, args) => {
            normalize_expr(receiver, op_defs, typeck, promote_op_only);
            for arg in args.iter_mut() {
                normalize_expr(&mut arg.value, op_defs, typeck, promote_op_only);
            }
        }
        hir::ExprKind::Index(base, index) => {
            normalize_expr(base, op_defs, typeck, promote_op_only);
            normalize_expr(index, op_defs, typeck, promote_op_only);
        }
        hir::ExprKind::Slice(slice) => {
            normalize_expr(&mut slice.base, op_defs, typeck, promote_op_only);
            if let Some(start) = &mut slice.start {
                normalize_expr(start, op_defs, typeck, promote_op_only);
            }
            if let Some(end) = &mut slice.end {
                normalize_expr(end, op_defs, typeck, promote_op_only);
            }
        }
        hir::ExprKind::Struct(_, fields) => {
            for field in fields.iter_mut() {
                normalize_expr(&mut field.expr, op_defs, typeck, promote_op_only);
            }
        }
        hir::ExprKind::If(cond, then_branch, else_branch) => {
            normalize_expr(cond, op_defs, typeck, promote_op_only);
            normalize_expr(then_branch, op_defs, typeck, promote_op_only);
            if let Some(elze) = else_branch {
                normalize_expr(elze, op_defs, typeck, promote_op_only);
            }
        }
        hir::ExprKind::Match(scrutinee, arms) => {
            normalize_expr(scrutinee, op_defs, typeck, promote_op_only);
            for arm in arms.iter_mut() {
                if let Some(guard) = &mut arm.guard {
                    normalize_expr(guard, op_defs, typeck, promote_op_only);
                }
                normalize_expr(&mut arm.body, op_defs, typeck, promote_op_only);
            }
        }
        hir::ExprKind::Try(expr_try) => {
            normalize_expr(&mut expr_try.expr, op_defs, typeck, promote_op_only);
            for catch in expr_try.catches.iter_mut() {
                normalize_expr(&mut catch.body, op_defs, typeck, promote_op_only);
            }
            if let Some(elze) = &mut expr_try.elze {
                normalize_expr(elze, op_defs, typeck, promote_op_only);
            }
            if let Some(finally) = &mut expr_try.finally {
                normalize_expr(finally, op_defs, typeck, promote_op_only);
            }
        }
        hir::ExprKind::Block(block) | hir::ExprKind::Loop(block) => {
            normalize_block(block, op_defs, typeck, promote_op_only);
        }
        hir::ExprKind::While(cond, block) => {
            normalize_expr(cond, op_defs, typeck, promote_op_only);
            normalize_block(block, op_defs, typeck, promote_op_only);
        }
        hir::ExprKind::With(context, body) => {
            normalize_expr(context, op_defs, typeck, promote_op_only);
            normalize_expr(body, op_defs, typeck, promote_op_only);
        }
        hir::ExprKind::IntrinsicCall(call) => {
            for arg in call.callargs.iter_mut() {
                normalize_expr(&mut arg.value, op_defs, typeck, promote_op_only);
            }
        }
        hir::ExprKind::Let(_, _, init) => {
            if let Some(init) = init {
                normalize_expr(init, op_defs, typeck, promote_op_only);
            }
        }
        hir::ExprKind::Array(elements) | hir::ExprKind::Tuple(elements) => {
            for element in elements.iter_mut() {
                normalize_expr(element, op_defs, typeck, promote_op_only);
            }
        }
        hir::ExprKind::ArrayRepeat { elem, len } => {
            normalize_expr(elem, op_defs, typeck, promote_op_only);
            normalize_expr(len, op_defs, typeck, promote_op_only);
        }
        hir::ExprKind::ConstBlock(const_block) => {
            normalize_expr(&mut const_block.body, op_defs, typeck, promote_op_only);
        }
        hir::ExprKind::Closure(closure) => {
            normalize_expr(&mut closure.body, op_defs, typeck, promote_op_only);
        }
        hir::ExprKind::Literal(_)
        | hir::ExprKind::FormatString(_)
        | hir::ExprKind::Continue
        | hir::ExprKind::Return(None)
        | hir::ExprKind::Break(None) => {}
    }

    if !promote_op_only || !promote_self {
        return;
    }

    if let Some(new_kind) = try_promote_op(expr, op_defs, typeck) {
        expr.kind = new_kind;
    }
}

/// Attempts to rewrite `expr` into an `IntrinsicCall(CallKind::Op(..))`
/// node if it matches one of the recognized `#[op(...)]` shapes. Returns
/// the replacement `ExprKind` on a hit (the caller assigns it back via
/// `expr.kind = ...`, since building the replacement consumes the old
/// `callee`/`args`/`fields`/`receiver` via `std::mem::replace`-style
/// moves out of `expr.kind`).
fn try_promote_op(
    expr: &mut hir::Expr,
    op_defs: &std::collections::HashMap<DefId, fp_core::intrinsics::OpKind>,
    typeck: Option<&TypeckResults>,
) -> Option<hir::ExprKind> {
    match &expr.kind {
        hir::ExprKind::Path(path) => {
            let Some(hir::Res::Def(def_id)) = &path.res else {
                return None;
            };
            let op = resolve_op_call_kind(op_defs, *def_id)?;
            Some(hir::ExprKind::IntrinsicCall(hir::IntrinsicCallExpr {
                kind: op,
                callargs: Vec::new(),
            }))
        }
        hir::ExprKind::Struct(path, _) => {
            let Some(hir::Res::Def(def_id)) = &path.res else {
                return None;
            };
            let op = resolve_op_call_kind(op_defs, *def_id)?;
            let old_kind = std::mem::replace(&mut expr.kind, hir::ExprKind::Continue);
            let hir::ExprKind::Struct(_, fields) = old_kind else {
                unreachable!("matched Struct arm above");
            };
            let mut ordered_fields = fields;
            ordered_fields.sort_by_key(|field| {
                field
                    .name
                    .as_str()
                    .parse::<usize>()
                    .unwrap_or(usize::MAX)
            });
            let callargs = ordered_fields
                .into_iter()
                .enumerate()
                .map(|(i, field)| hir::CallArg {
                    name: hir::Symbol::new(format!("arg{i}")),
                    value: field.expr,
                })
                .collect();
            Some(hir::ExprKind::IntrinsicCall(hir::IntrinsicCallExpr {
                kind: op,
                callargs,
            }))
        }
        hir::ExprKind::Call(callee, _) => {
            let hir::ExprKind::Path(path) = &callee.kind else {
                return None;
            };
            let Some(hir::Res::Def(def_id)) = &path.res else {
                return None;
            };
            let op = resolve_op_call_kind(op_defs, *def_id)?;
            let old_kind = std::mem::replace(&mut expr.kind, hir::ExprKind::Continue);
            let hir::ExprKind::Call(_, args) = old_kind else {
                unreachable!("matched Call arm above");
            };
            Some(hir::ExprKind::IntrinsicCall(hir::IntrinsicCallExpr {
                kind: op,
                callargs: args,
            }))
        }
        hir::ExprKind::MethodCall(_, _, _) => {
            let def_id = *typeck.and_then(|t| t.method_resolutions.get(&expr.hir_id))?;
            let op = resolve_op_call_kind(op_defs, def_id)?;
            let old_kind = std::mem::replace(&mut expr.kind, hir::ExprKind::Continue);
            let hir::ExprKind::MethodCall(receiver, _name, args) = old_kind else {
                unreachable!("matched MethodCall arm above");
            };
            let mut callargs = Vec::with_capacity(1 + args.len());
            callargs.push(hir::CallArg {
                name: hir::Symbol::new("arg0"),
                value: *receiver,
            });
            for (i, arg) in args.into_iter().enumerate() {
                callargs.push(hir::CallArg {
                    name: hir::Symbol::new(format!("arg{}", i + 1)),
                    value: arg.value,
                });
            }
            Some(hir::ExprKind::IntrinsicCall(hir::IntrinsicCallExpr {
                kind: op,
                callargs,
            }))
        }
        _ => None,
    }
}
