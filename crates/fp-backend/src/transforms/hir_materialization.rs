//! Documents where each backend's "turn a recognized `#[op(...)]`/
//! intrinsic into real target-language shape" stage lives. Mostly a
//! reference module — the real logic for Kotlin/Shell already lives
//! elsewhere and is unchanged by this note; Native's stage is a
//! deliberate, documented pass-through no-op.
//!
//! ## Kotlin
//!
//! Kotlin materializes at the **AST** level, via `KotlinMaterializer`
//! (implements `fp_core::intrinsics::IntrinsicMaterializer`). It's wired
//! up through `crate::languages::materializer::materializer_for_language`,
//! invoked from `fp-cli`'s `compile_emit_target`/`compile_project` (see
//! `crates/fp-cli/src/commands/compile.rs` — the single-file path calls it
//! at line ~778, the multi-file `--target` path at line ~954). The AST it
//! materializes over is produced by `HirToAstLifter` from the shared,
//! `hir_normalization`-promoted HIR (`promote_op_only: true` for
//! `TypecheckedTranspile`, which Kotlin transpilation always uses), so by
//! the time `KotlinMaterializer::materialize_call` sees an
//! `ExprIntrinsicCall`, its `kind` is already the promoted `CallKind::Op`
//! (or a genuine `CallKind::Intrinsic`) — no logic change needed here.
//!
//! ## Shell
//!
//! Same shape, different call site: `ShellMaterializer::new` is
//! constructed directly in `crates/fp-shell/src/lib.rs` (~line 115) and
//! consumed the same way, via the AST-level `IntrinsicMaterializer` trait.
//! No logic change needed here either.
//!
//! ## Native
//!
//! Native's pipeline does not go through `HirToAstLifter`/AST at all —
//! it lowers straight from HIR to MIR
//! (`crates/fp-backend/src/transforms/hir_to_mir/expr.rs`), so the
//! AST-level `IntrinsicMaterializer` trait doesn't apply to it structurally.
//! Confirming this: `NativeIntrinsicMaterializer`
//! (`crates/fp-native/src/intrinsic_materializer.rs`) implements
//! `IntrinsicMaterializer` with an entirely empty body (`impl
//! IntrinsicMaterializer for NativeIntrinsicMaterializer {}`), inheriting
//! every default (no-op, "not handled here") method — Native was never
//! wired to materialize ops at the AST level, by design.
//!
//! `hir_to_mir::expr`'s handling of `hir::ExprKind::IntrinsicCall(call)`
//! already has a documented, loud fallback for a `CallKind::Op(..)` that
//! has no `intrinsic_kind()` equivalent (added alongside the
//! `hir::IntrinsicCallExpr.kind: IntrinsicKind -> CallKind` widening): every
//! entry point (`lower_operand`'s main arm, `lower_intrinsic_constant`,
//! `lower_expr_into_place`'s main arm) either resolves to a real
//! `IntrinsicKind` via `CallKind::intrinsic_kind()` or emits a loud
//! `emit_error`/`emit_warning` plus a unit/error value — never a silent
//! wrong answer or a panic.
//!
//! Given that, `Native` is deliberately configured with
//! `promote_op_only: false` in the driver (`hir_normalization::normalize_program`
//! call in `crates/fp-compiler/src/driver.rs`): pure-`Op`-only calls (an
//! `#[op(...)]`-tagged enum variant or method with no real intrinsic
//! equivalent, e.g. `Option::Some`/`Vec::new`/`Result::Ok`) are left as
//! ordinary `hir::ExprKind::Call`/`MethodCall`/`Struct` nodes, which then
//! flow through `hir_to_mir`'s normal lowering as ordinary calls to the
//! real (stub) function/struct-literal bodies already declared for those
//! types in `std`. This is correct and lower-risk: Native has no
//! target-shape mapping for an arbitrary `Op` (unlike Kotlin/Shell, which
//! map every op to a real target-language literal/call form in their
//! materializers), so promoting it to `IntrinsicCall(CallKind::Op(..))`
//! here would just relocate the "unimplemented op" failure from an
//! ordinary (and already working) call-to-stub-body path to a new,
//! untested `hir_to_mir` fallback path with no compensating benefit.
//!
//! This was not changed speculatively: a smoke-compile of a small program
//! using `Vec::new()`/`Option`/`Result` through the Native pipeline was
//! not run as part of this change (no reachable Native smoke-test harness
//! was set up in this pass); if a concrete Native regression involving an
//! `#[op(...)]`-tagged construct surfaces, re-evaluate `promote_op_only`
//! for `Native` rather than assuming this reasoning covers every case.
