# FerroPhase Type System Overview

This document explains how FerroPhase manages types across the compilation pipeline, summarising the data structures,
query mechanisms, and interactions between modes.

## Type Representations

FerroPhase uses a tiered collection of type representations. Stable type tokens allow information to flow between stages
without duplication:

- **Ty** – Flexible, syntax-oriented types used in LAST and the canonical AST prior to typing. Supports unknowns, generics, `mut type`
  declarations, and comptime queries.
- **ConcreteType** – Fully resolved, layout-aware types embedded in THIR, MIR, LIR, and static transpile outputs.
- **IntermediateType** (optional) – Bridges ConcreteType into backend-specific forms (e.g., bytecode abstractions) when
  needed.

### `mut type` Tokens at Comptime

During const evaluation, transformations can introduce new types using the language construct `let mut T = struct {...}`.
The TypeQueryEngine records these as provisional tokens that carry:

- `Ty` description of the syntactic shape (populated from inline `struct` declarations or builder intrinsics)
- Metadata sufficient to emit a corresponding `ConcreteType` (field order, trait impls, provenance)
- A monotonically increasing `mut_revision` so memoised queries remain valid

Once a const block succeeds, all of its `mut type` tokens are atomically promoted to ConcreteType definitions that share
the same tokens used later in the pipeline. Failures drop the tokens entirely.

## TypeQueryEngine

The TypeQueryEngine is the const-eval facing façade over the shared type tables.

- **Inputs**: Read-only `TypeSnapshot` produced after Phase 1 type checking.
- **Queries**: Intrinsics like `@sizeof`, `@hasfield`, `@trait_impl` route through memoised helpers keyed by
  `(query, type_id, mut_revision)`.
- **Safety**: Queries never observe partially applied transformations; commits happen only after a const block completes.
- **Diagnostics**: Errors during querying (e.g., missing field) produce structured diagnostics reused by later phases.

## Flow Through the Pipeline

```
CST → LAST → Annotated AST → HIR → THIR → Typed Interpretation → TAST → {re-project HIR → THIR → MIR | surface LAST′} → LIR → Backends
```

- **HIR** operates primarily on Ty but references the shared tables for resolved tokens.
- **THIR** embeds ConcreteType directly and serves as the input to typed interpretation.
- **TAST** (Typed AST) is the evaluated, resugared tree shared by all modes and carries ConcreteType metadata for emitters like C.
- **Re-projected HIR/THIR** reuse the evaluated program for optimisation stages (compile, bytecode).
- **MIR/LIR** continue with ConcreteType (or optional IntermediateType); MIR forms an SSA Mid-level Intermediate
  Representation before LIR handles low-level layout and backend codegen.

## Cross-Stage Guarantees

- **Canonical TAST**: All downstream stages consume the TAST snapshot emitted after typed interpretation.
- **Semantic Preservation**: Lowering (TAST→HIR→THIR→MIR/LIR) cannot alter observable behaviour relative to the evaluated TAST.
- **Deterministic Promotions**: `mut type` tokens promote in a deterministic order so repeated builds are stable.
- **Shared Diagnostics**: Spans from the initial AST are threaded through THIR and TAST, providing consistent user feedback.

## Shared Infrastructure

- **Constraint Solver**: The Hindley–Milner (Algorithm W) solver is implemented once and exposed to both the TypeQueryEngine and
  THIR builder.
- **Query Helpers**: Field lookups, trait resolution, and layout calculators live in `type_queries::*`, reused across
  phases.

## Opaque Type Tokens

Type tokens are intentionally opaque outside const evaluation and lowering. Downstream stages consume structured
representations—`ConcreteType`, TAST annotations, MIR layouts—without peeking at the raw token values. When exposing
APIs to backends or user tooling, forward the shaped type data instead of the token itself. This boundary prevents
backend authors from depending on compiler-internal identifiers and mirrors Scala's `opaque type` distinction between
implementation details and surface types.

## Future Work

- Define backend-specific IntermediateType variants for LLVM and bytecode.
- Expand the TypeQueryEngine with bulk query APIs (e.g., batch `hasfield`) for performance.
- Formalise the re-sugaring metadata contract so TAST lift covers more syntactic forms.
