# FerroPhase Type System Overview (AST-Centric)

The FerroPhase type system now revolves around the canonical AST. Algorithm W
inference annotates the AST in place, and every stage—const evaluation, runtime
interpretation, HIR projection, MIR/LIR lowering—reads the same typed
structures. This document summarises the representations, flows, and guarantees
in the new architecture.

## Type Representations

- **`Ty`** – Syntax-oriented type declarations attached to AST nodes before
  inference. Supports unknowns, generics, and provisional `mut type` tokens.
- **`TypedAst` annotations** – Optional `ty: Option<Ty>` fields on expressions,
  patterns, and declarations populated by the inferencer. These hold principal
  types after substitutions are applied.
- **`ConcreteType`** – Fully resolved, layout-aware types consumed by
  optimisation backends (HIRᵗ, MIR, LIR) and persisted for tooling.
- **`IntermediateType` (optional)** – Backend adapters derived from
  `ConcreteType` when a target needs additional metadata (bytecode VMs, FFI
  descriptors, etc.).

### Structural vs Dynamic Dictionaries

- **Structural dictionaries**: Produced when const evaluation or intrinsic
  builders synthesise record shapes. Once promoted they live in the shared type
  tables as `ConcreteType` entries and reuse the same tokens for all downstream
  stages.
- **Dynamic dictionaries**: Runtime-only values represented by
  `Value::Structural`. They remain untyped (`Ty::Any`/`Ty::Unknown`) until a
  promotion step proves a stable shape. Evaluation treats them as opaque maps.

### `mut type` Tokens

Const evaluation can create provisional types via `let mut T = struct { … }`. The
inferencer records these tokens with:

- The syntactic `Ty` form (fields, generics, trait impl hints).
- Enough metadata to emit a `ConcreteType` when promoted.
- A monotonic revision counter so cached queries remain valid.

Tokens promote atomically when the owning const block succeeds. Failed blocks
discard the provisional entries.

## Query Infrastructure

The `TypeQueryEngine` exposes memoised queries over the shared type tables:

- Inputs: the typed AST plus the current `ConcreteType` registry.
- Queries: `sizeof`, `hasfield`, trait resolution, layout calculators.
- Safety: queries observe only committed promotions; const blocks stage their
  effects until they complete.
- Diagnostics: failures produce structured messages reused across evaluation and
  lowering.

## Flow Through the Pipeline

```
SOURCE → LAST → AST → ASTᵗ → ASTᵗ′ → HIRᵗ → MIR → LIR → backend
```

- **ASTᵗ** carries principal types in place.
- **ASTᵗ′** is the evaluated AST (const-folded, intrinsic rewrites applied).
- **HIRᵗ** preserves type metadata while desugaring structures for optimisation.
- **MIR/LIR** operate on `ConcreteType` (and optional `IntermediateType`) for
  code generation.

## Guarantees

- **Single source of truth**: All stages read the typed AST produced by the
  inferencer.
- **Deterministic promotions**: `mut type` tokens promote in a stable order.
- **Span fidelity**: Type annotations and diagnostics reuse original AST spans.
- **Backend isolation**: Consumers never access raw type tokens; they receive
  shaped representations (`ConcreteType`, typed annotations) via stable APIs.

## Shared Solver

The Hindley–Milner solver lives in a dedicated crate and offers:

- In-place annotation of AST nodes.
- Query APIs (`expr_ty`, `pattern_ty`) for tooling.
- Hooks for const evaluation to request generalisations or specialisations when
  intrinsics introduce new constraints.

## Future Work

- Define concrete encodings for persisting typed AST snapshots (`.ast-typed`).
- Extend `TypeQueryEngine` with batch operations for performance-sensitive
  intrinsics.
- Formalise how promotions surface in tooling (e.g., language server protocol
  responses) using the same typed AST annotations.
