# Design Brief: AST-Centric Multi-Mode Execution

## Overview

FerroPhase now centres every frontend, mode, and backend on a single typed AST.
Frontends emit a canonical AST; Algorithm W inference annotates it in place; the
interpreter, const evaluator, and backend lowerings consume that same structure.
By eliminating THIR we reduce the number of intermediate projections while still
supporting multi-language inputs and multiple execution modes (compile, run,
transpile, bytecode).

```
SOURCE → LAST → AST → ASTᵗ → (const/runtime evaluation) → ASTᵗ′ → HIRᵗ → MIR → LIR → backend
```

- `ASTᵗ` is the AST annotated with principal types.
- `ASTᵗ′` is the post-evaluation AST (const-folded, intrinsic rewrites applied).
- `HIRᵗ` is the typed high-level IR used by optimisation backends; it focuses on
  structural desugaring and borrow analysis while preserving type metadata.

## Stage Responsibilities

| Stage | Responsibilities |
|-------|------------------|
| **Frontend** | Parse source into LAST, produce canonical AST, record serializer + provenance. |
| **Normalisation** | Macro expansion, std remapping, intrinsic detection, producing a clean AST. |
| **Type Enrichment** | Algorithm W inference attaches types directly to AST nodes (expressions, patterns, declarations). |
| **Interpretation** | Runs on the typed AST; const mode mutates the tree, runtime mode reads it without changes. Shares intrinsic registry across modes. |
| **Typed Projection** | Converts evaluated AST into `HIRᵗ`, handling desugaring, ownership bookkeeping, and preparing for optimisation passes. |
| **Optimisation & Codegen** | Lowers `HIRᵗ` → `MIR` → `LIR` → target backends (LLVM, bytecode, transpilers). |

## Intrinsic Handling

1. **AST Normaliser** rewrites language-specific helpers into canonical `std::`
   symbols before type inference.
2. **Resolver** maps `(symbol, backend flavour)` to a declarative
   `ResolvedIntrinsic` (AST rewrite, HIR hook, or unsupported diagnostic).
3. **Interpreter** uses the resolver's identity flavour to execute intrinsics in
   both const and runtime modes.
4. **Backends** call the resolver during projection/lowering to materialise the
   intrinsic into runtime calls, transpiler constructs, or inline code.

## Evaluation Engine

- Located in `crates/fp-interpret/src/ast`.
- Shared `InterpreterContext` holds intrinsic registry, environment bindings,
  caches, and diagnostic sinks.
- `InterpreterConfig` toggles const vs runtime semantics and feature flags
  (e.g., allow side effects, IO shims).
- Returns `InterpreterOutcome` capturing produced values, diagnostics, and AST
  mutations.

## Type Inference

- Ported Algorithm W solver operates on AST nodes directly, using in-place
  substitution to fill optional `ty` slots.
- Exposes query APIs: `expr_ty(node_id)`, `pattern_ty(node_id)`, etc., so tooling
  and backends can read principal types without repeating inference.
- Works hand-in-hand with intrinsic normalisation: canonical symbols simplify
  constraint generation.

## Multi-Mode Support

| Mode | Flow | Notes |
|------|------|-------|
| **Compile** | AST → ASTᵗ → ASTᵗ′ (const) → HIRᵗ → MIR → LIR → LLVM | Const evaluation folds code before optimisation; backends consume typed IR. |
| **Run (Interpreter)** | AST → ASTᵗ → Interpreter (runtime) | Shares evaluator with const mode; no MIR generation. |
| **Bytecode** | AST → ASTᵗ → ASTᵗ′ → HIRᵗ → MIR → LIR → VM bytecode | Bytecode generator reuses the same typed IR pipeline. |
| **Transpile** | AST → ASTᵗ → ASTᵗ′ → HIRᵗ → target AST | Typed metadata is preserved for optional annotation in the output language. |

## Diagnostics

- Stages produce `DiagnosticReport<T>` wrappers; evaluation emits contextual
  diagnostics (`span`, `intrinsic`, mode) via `DiagnosticManager`.
- Saving intermediates now writes `.ast`, `.ast-typed`, `.ast-eval`, `.hir`, `.mir`,
  `.lir`, `.ll` artefacts.

## Outstanding Tasks

1. Finalise the AST type inference port and expose the querying API.
2. Replace THIR interpreter plumbing with the new AST evaluator across const and
   runtime orchestrators.
3. Update backends to consume `HIRᵗ` instead of THIR-derived snapshots.
4. Migrate intrinsic implementations into the new resolver and remove
   THIR-specific normalisers.
5. Refresh tooling/docs (CLI help, architecture notes) to reflect the AST-centric
   design.

This design keeps the pipeline leaner while still supporting the breadth of
FerroPhase targets and surface languages. Every mode benefits from a single
source of truth: the typed AST.
