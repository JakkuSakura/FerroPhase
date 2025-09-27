# Interpretation and Const Evaluation Review

## Current State Snapshot

### ConstEvaluationOrchestrator (`crates/fp-optimize/src/orchestrators/const_evaluation.rs`)
- Embeds multiple responsibilities: type registration (`TypeQueries`), dependency analysis, intrinsic bookkeeping, AST rewriting, and evaluation dispatch.
- Relies on `InterpretationOrchestrator` for actual expression execution, yet the `evaluate_const_expression` hook ignores the supplied intrinsic context and simply calls `interpret_expr_no_resolve`.
- Maintains bespoke registries (`EvaluationContext`, `ConstEvalTracker`, `IntrinsicEvaluationContext`) per run; there is no shared cache or reuse across compilation units.
- Error handling routes through generic `optimization_error` calls, so diagnostics emitted during const evaluation cannot distinguish between recoverable intrinsic failures and structural bugs.
- Inline-constant pass duplicates traversal logic already present in the interpreter and reruns interpretation per expression; this is costly and complicates mutation ordering.

### InterpretationOrchestrator (`crates/fp-optimize/src/orchestrators/interpretation/mod.rs`)
- The THIR-based interpreter now drives both const and (basic) runtime modes through a shared configuration object. Runtime mode currently returns owned runtime values and still needs richer semantics (ownership, IO shims).
- Intrinsics are routed through a small registry (print/println today); expand this to cover the rest of the standard library and language-specific hooks.
- Error propagation still relies on `optimization_error`; richer diagnostics should flow through `DiagnosticManager`.

### Type Inference
- `TypeQueries` is limited to structural reflection backed by `TypeRegistry`; it does not perform inference itself.
- There *is* a unification-based engine in `crates/fp-optimize/src/transformations/hir_to_thir/type_inference.rs` (the HIR→THIR lowering). It implements an Algorithm W–style solver to infer expression and pattern types before producing typed HIR (THIR).
- That solver currently runs only during the HIR→THIR transformation. The interpreter and const evaluation orchestrator do not consult its output, so they still rely on registry stubs populated from surface syntax.
- Because the solver’s results are written into `ThirGenerator::inferred_*` maps, we could expose those results to evaluation passes, but no such plumbing exists yet.

## Design Gaps and Risks

1. **Runtime Execution Gap**: Only const evaluation runs today; runtime execution still needs to be built on the THIR interpreter.
2. **Intrinsic Handling**: The intrinsic context collects `ConstEval` operations, but the interpreter invokes intrinsics via ad-hoc builtins that directly mutate shared state. This needs to be ported to THIR-aware intrinsics.
3. **Configuration Surface**: There is no single struct representing "an evaluator" parameterized by mode. Instead, `ConstEvaluationOrchestrator` owns a dedicated `InterpretationOrchestrator`, meaning runtime interpretation cannot reuse the same instance with alternate policies.
4. **Diagnostics**: Evaluation errors bubble up as generic optimization failures. With the new diagnostic manager infrastructure, evaluation should log structured diagnostics (node span, intrinsic name, etc.) and allow const evaluation to recover when possible.
5. **Type Information**: The Algorithm W–style solver that runs during HIR→THIR is not surfaced to const/runtime evaluation, so those paths still operate without principal types. Until its results are exposed through a reusable interface, evaluators are forced to rely on ad-hoc registries.

## Recommended Direction

### Unify the Evaluator
- Extract a core THIR evaluator configuration (mode, diagnostics, intrinsic handlers) so const and runtime interpretation share infrastructure.
- Ensure intrinsic evaluation always goes through a unified pathway so compile-time operations and runtime calls share the same dispatch code.

### Intrinsic and Builtin Registry
- Replace the hard-coded `match` ladder in `interpret_ident` with a registry keyed by symbol + mode. This allows language frontends (Rust, LAST, etc.) to register their own intrinsics without editing the interpreter.
- Encode each intrinsic as a struct implementing a `Builtin` trait (`fn invoke(&self, mode, evaluator, args)`). Const evaluation can attach metadata (e.g., produces `ConstEval::GenerateField`).

### Diagnostics and Recovery
- Thread a `DiagnosticManager` through evaluation calls so every failure captures context (`span`, `intrinsic`, `const block id`).
- Augment `EvaluationContext` with per-block diagnostics; allow const evaluation to continue past non-fatal intrinsic failures while reporting actionable messages.

### Revisit Const Evaluation Workflow
- After unifying evaluation, reduce the orchestrator to sequencing phases: dependency ordering, evaluating blocks, and inlining constant expressions using the typed interpreter.
- Consider batching const block results: once a block evaluates, inject its value into the shared context and the evaluator cache so subsequent lookups avoid re-interpretation.

### Type Inference Path
- The existing HIR→THIR solver already performs Algorithm W–style inference; the next step is to surface its results outside that transform.
- Extract a query interface (`TypeInferenceResults`) exposing `expr_ty(hir::HirId)` and `pat_ty(hir::HirId)` so other passes can consume principal types without rerunning inference.
- Feed those inferred types into `EvaluationContext` to validate const block results or to specialize intrinsic calls.
- Longer term, consider lifting the solver into a dedicated crate so both compile-time and runtime evaluators can share a single source of truth when future frontends (e.g., LAST) participate.

## Short-Term Actions
1. Introduce an evaluation configuration object and retrofit `ConstEvaluationOrchestrator`/`InterpretationOrchestrator` to consume it, eliminating duplicated traversal functions.
2. Replace direct `optimization_error` returns inside interpretation with diagnostic emissions + structured error types to aid recovery.
3. Document how the existing HIR→THIR inference can be queried (or make it queryable) in `docs/Types.md` so downstream tooling understands the available hooks.
4. Start carving out the builtin registry abstraction so new language frontends can plug in without touching the core interpreter.

With these steps, const and runtime evaluation can genuinely share a single engine configured per use case, and the existing Hindley–Milner–style solver can feed both paths without duplicating constraint logic.
