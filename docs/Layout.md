# FerroPhase Layout and Code Quality Review

This document reviews the current code layout and proposes a focused refactor plan to improve maintainability. The aim is clean module boundaries, smaller files and functions, and consistent naming. Topics like self‑hosting are intentionally deferred until the structure is improved.

## 1) Current Pain Points

- Oversized files mixing unrelated concerns make changes risky and tests unfocused.
- Module boundaries are blurry; normalization for env/log/fs/args is incomplete and leaks into transforms.
- Terminology (Collection → Container) is not uniformly applied.

Approximate line counts (for scale):
- `crates/fp-cli/src/pipeline.rs` ≈ 3,936
- `crates/fp-backend/src/transformations/hir_to_mir/mod.rs` ≈ 3,796
- `crates/fp-llvm/src/codegen.rs` ≈ 2,417
- `crates/fp-rust/src/parser/expr.rs` ≈ 1,877
- `crates/fp-cli/src/bin/fp.rs` ≈ 517

## 2) File Reviews and Refactor Steps

- `crates/fp-cli/src/pipeline.rs`
  - Problem: Monolithic; mixes frontend selection, stage orchestration, diagnostics, workspace replay/merge, snapshot I/O, and linking. Public functions return private types.
  - Refactor:
    - Split into: `mod.rs` (API), `input.rs`, `frontend.rs`, `stages.rs`, `workspace.rs`, `diagnostics.rs`, `blocking.rs`, `artifacts.rs`.
    - Fix visibility: artifacts are `pub(crate)` or hidden behind the API.

- `crates/fp-backend/src/transformations/hir_to_mir/mod.rs`
  - Problem: One file contains context, type lowering, body/call lowering, impl/methods, consts, diagnostics.
  - Refactor:
    - Submodule into: `context.rs`, `types.rs`, `body.rs`, `calls.rs`, `consts.rs`, `impls.rs`, `diag.rs`; keep `mod.rs` as facade.
    - Establish explicit fallback points that never erase bodies; keep control flow and calls intact.

- `crates/fp-llvm/src/codegen.rs`
  - Problem: Functions/globals/constants/strings/verification all in one unit; hard to test narrowly.
  - Refactor:
    - Split into: `functions.rs`, `globals.rs`, `constants.rs`, `strings.rs`, `verify.rs`; keep `LlvmCompiler` small.

- `crates/fp-rust/src/parser/expr.rs`
  - Problem: Parser includes macro/desugar concerns; parser should be AST‑only.
  - Refactor:
    - Move desugar/diagnostic policy to a normalization pass; keep parser building AST nodes only.

- `crates/fp-cli/src/bin/fp.rs`
  - Problem: Main mixes setup (logging/diagnostics) and dispatch.
  - Refactor:
    - Factor setup into `runtime.rs`; keep main declarative; dispatch stays under `commands/`.

## 3) Phased Plan (no behavior changes)

- Phase 1: Split fp‑cli pipeline (extract `workspace.rs`, `diagnostics.rs`, `artifacts.rs`, `blocking.rs`), fix visibility.
- Phase 2: Submodule HIR→MIR (start with `calls.rs` and `body.rs`), add targeted unit tests.
- Phase 3: Split LLVM codegen and add determinism tests.
- Phase 4: Parser cleanup (move macro desugar into normalization pass).

## 4) Quality Gates and Conventions

- CI: `cargo fmt --check` and `cargo clippy --workspace -D warnings`.
- Size budgets: warn > 800 LOC, refactor if > 1,000 LOC (temporary allowlist while splitting).
- Naming: modules snake_case; types UpperCamelCase; functions snake_case verbs; prefer domain‑specific module names (avoid “utils.rs”).
- Visibility: prefer `pub(crate)`; avoid public APIs returning private types; avoid one‑letter idents beyond trivial loops.

## 5) Terminology

- Use “Container/ContainerOperation” consistently (replace remaining “Collection” names in code, docs, and tests).

