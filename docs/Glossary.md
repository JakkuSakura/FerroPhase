# Glossary (Ubiquitous Language)

This glossary defines the shared domain language for FerroPhase. Use these terms consistently in docs, code, and diagnostics.

- **Stage** — A named step in the compilation pipeline (Frontend → Normalization → Typing → Eval → Lowering).
- **Snapshot** — A persisted or cached artifact produced by a stage (LAST, AST, ASTᵗ, HIRᵗ, MIR, LIR).
- **Pipeline** — The orchestrator that threads artifacts and diagnostics through stages.
- **Frontend** — The parsing + normalization boundary that produces LAST + canonical AST + provenance.
- **Typing** — The phase that enriches AST nodes with principal types (ASTᵗ).
- **Interpretation / ConstEval** — The evaluator that produces ASTᵗ′ and executes intrinsics.
- **Lowering** — The projection pipeline ASTᵗ′ → HIRᵗ → MIR → LIR → backend artifacts.
- **Intrinsic** — A compiler-recognized capability resolved through the intrinsic registry; may execute at const time or lower to runtime calls.
- **Module** — The canonical unit of namespacing + compilation, rooted at a package path.
- **Package** — A dependency boundary that provides modules and optional bindings.
- **Query Document** — A query frontend artifact (SQL/PRQL) normalized into AST `NodeKind::Query`.
- **Binding** — A generated language-specific façade that mirrors module paths and public APIs.
- **Frontend Snapshot** — Cached LAST + serializer + provenance for debugging and tooling.
- **Diagnostic** — Structured error/warning/info emitted by a stage with context and span.
