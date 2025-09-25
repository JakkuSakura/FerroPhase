# FerroPhase Type System Overview

This document explains how FerroPhase manages types across the compilation pipeline, summarising the data structures,
query mechanisms, and interactions between modes.

## Type Representations

FerroPhase uses a tiered collection of type representations. Stable type tokens allow information to flow between stages
without duplication:

- **Ty** – Flexible, syntax-oriented types used in AST, LAST, and EAST. Supports unknowns, generics, `mut type`
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
CST → LAST → AST → (Phase 1 types) → EAST → HIR → THIR → {TAST | MIR} → LIR → Backends
```

- **EAST** is the evaluated AST snapshot shared by every mode. All transformations are applied atomically prior to
  generating HIR or surface outputs.
- **HIR** operates primarily on Ty but references the shared tables for resolved tokens.
- **THIR** embeds ConcreteType directly, consuming the promoted entries produced during const evaluation.
- **TAST** (static transpile) re-sugars AST structure while carrying ConcreteType metadata for emitters like C.
- **MIR/LIR** continue with ConcreteType (or optional IntermediateType); MIR forms an SSA Mid-level Intermediate
  Representation before LIR handles low-level layout and backend codegen.

## Cross-Stage Guarantees

- **Canonical EAST**: All downstream stages consume the exact EAST snapshot emitted after const evaluation.
- **Semantic Preservation**: Lowering (AST→HIR→THIR→TAST/MIR/LIR) cannot alter observable behaviour relative to EAST.
- **Deterministic Promotions**: `mut type` tokens promote in a deterministic order so repeated builds are stable.
- **Shared Diagnostics**: All stages report errors using EAST spans, providing consistent user feedback.

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
