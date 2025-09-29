# Comprehensive Design Document: Multi-Mode Execution in a Multi-Language Compiler

## Overview: Stages, Modes, and Types

This document provides a detailed discussion and summary of the compiler's architecture, focusing on its multi-mode
execution capabilities and type system design. The system is built around a shared frontend that normalizes diverse
source languages into a unified Abstract Syntax Tree (AST), enabling flexible processing across modes. Execution modes
include compilation to native targets, direct interpretation, bytecode generation for VM execution, and transpilation to
languages like Rust—with or without type annotations. The type system evolves through stages, using 2-3 distinct
representations to balance flexibility during early analysis (e.g., inference and compile-time evaluation) with
efficiency in later optimization and code generation.

### Visually Clear Summary of Stages, Modes, and Types

#### Core Pipeline Stages

- **Frontend**: Source Language → CST (Concrete Syntax Tree, optional for parsing details) → LAST (language-specific
  AST) → AST (unified, language-agnostic AST) → HIR. Frontends keep their own library surfaces; no canonicalisation is
  required at this stage beyond carrying enough metadata for later passes.
- **Symbolic THIR Normalisation**: HIR → THIR(raw) → THIR(symbolic). A reusable `ThirTransformer`-based normaliser
  (see `crates/fp-optimize/src/transformations/thir/`) canonicalises language-specific helpers into the shared
  `std::…` vocabulary, producing a backend-agnostic typed IR.
- **Desugaring & Typing**: The symbolic THIR already carries Algorithm W inference results and acts as the common input
  to interpretation, transpilation, and code generation.
- **Typed Interpretation**: Compile-time/runtime evaluation operates on THIR, producing an evaluated THIR′ that records
  const-eval effects and intrinsic mutations.
- **Resugaring & Emission**: THIR′ → TAST (Typed AST with source-like structure) → LAST′ (language-specific ASTs for
  transpilers) and, for native targets, re-projection through HIR → THIR → MIR → LIR → LLVM/bytecode backends.

### Intrinsic Normalisation

Different frontends surface their own intrinsic vocabularies, but the rest of the compiler expects a single, consistent
view of core services (collections, strings, IO shims, etc.). Convergence now happens once per programme on the typed IR:

1. **Capture** – frontends record which native modules or prelude bindings were pulled in (Python’s `__builtins__`, JS
   host globals, FerroPhase helpers, …) as they emit the raw THIR.
2. **Symbolic rewrite** – the **ThirNormalizer** (`ThirTransformer`-based) walks that raw THIR and rewrites recognised
   helpers into the canonical `std` hierarchy. The result is a backend-agnostic symbolic THIR snapshot.
3. **Backend materialisation** – when a target flavour is chosen, a second THIR transformer clones the symbolic tree
   and replaces the canonical intrinsics with backend-specific runtime calls (e.g., `printf`, interpreter closures,
   transpiler surface forms). The initial materialiser lives next to the normaliser in
   `crates/fp-optimize/src/transformations/thir/`.
4. **Propagation** – downstream stages (typed interpretation, MIR/LIR lowering, transpilers) consume the materialised
   clone that matches their backend. Identity flavours (notably the interpreter) can re-use the symbolic snapshot.

Because normalisation happens once on symbolic THIR, every downstream optimisation and diagnostic treats these
intrinsics like any other user-defined module. Adding a new frontend only requires teaching the normaliser how to observe
its helpers; new backends simply register a materialisation flavour in the resolver.

#### Execution Modes and Their Flows

| Mode                             | Key Flow                                                                                                                                                                                                                             | Purpose and Characteristics                                                                                                                                                                                                                                                                                            |
|----------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **Compile**                      | Frontend → Annotated AST → HIR → THIR(raw) → THIR(sym) → Typed Interpretation (comptime) → TAST → re-project (HIR → THIR(sym) → THIR(mat target)) → MIR → LIR → LLVM/WASM                                                              | Full native/web compilation. Symbolic THIR feeds typed interpretation; during the re-projection step we materialise per-target THIR before lowering to MIR/LIR.                                                                                                                     |
| **Interpret (Runtime)**          | Frontend → Annotated AST → HIR → THIR(raw) → THIR(sym) → (identity materialisation) → Typed Interpretation (runtime)                                                                                                                   | Direct execution with principal types and spans. The interpreter uses the symbolic THIR (materialiser identity); no MIR/LIR lowering, ideal for REPLs and scripting tools.                                                                                                         |
| **Bytecode / VM**                | Frontend → Annotated AST → HIR → THIR(raw) → THIR(sym) → Typed Interpretation (comptime) → TAST → re-project (HIR → THIR(sym) → THIR(mat bytecode)) → MIR → LIR → Custom Bytecode → VM                                                  | Generates portable bytecode while reusing typed interpretation. Materialised THIR for the bytecode backend drives MIR/LIR before emission, enabling debug metadata and stepped execution.                                                                                           |
| **Transpile (Surface)**          | Frontend → Annotated AST → HIR → THIR(raw) → THIR(sym) → Typed Interpretation (comptime) → TAST → Language-specific materialised THIR → LAST′ → Codegen                                                                                | Produces readable Rust/TypeScript/etc. outputs. After typed interpretation the transpiler materialises THIR for the target language, then re-sugars into LAST′ while retaining explicit types and folded constants.                                                                |

Typed interpretation centralises const folding and intrinsic execution on THIR. The resulting typed effects are recorded
and replayed during resugaring and re-projection so every downstream stage observes the same evaluated program.
## Cross-Mode and Cross-Stage Guarantees

- **Canonical TAST**: Every downstream stage (re-projected HIR/THIR, MIR, LIR) and every mode (compile, bytecode, transpile,
  interpret) consumes the same evaluated TAST snapshot produced after typed interpretation. Differences manifest only in
  performance or final emission format, never in user-visible semantics.
- **Stage Preservation**: Transformations performed during lowering (HIR→THIR→TAST→HIR→THIR→MIR/LIR) are
  semantics-preserving; no stage may alter observable behaviour relative to the evaluated TAST.
- **Deterministic Effects**: Typed interpretation records structural edits in a deterministic order so repeated builds
  produce identical TAST snapshots and downstream outputs.
- **Shared Diagnostics**: Spans captured during the initial LAST→AST phase are threaded through THIR, TAST, and
  re-projection, ensuring consistent user feedback in every mode.

## Detailed Discussion: Type System Design

The type system is phased to match the pipeline's progression, using distinct representations to optimize for each
stage's needs. Early stages prioritize flexibility with Ty, an expressive set that includes variants for unknowns,
generics, and comptime-queryable structures. This is managed via a type map, allowing non-destructive updates during
inference or interleaving, which is crucial for modes involving iterative refinement.

As the pipeline advances—particularly in compile, bytecode, or static transpile modes—types transition to ConcreteType
through a HIR→THIR projection. This set is more rigid, incorporating details like memory layouts and ABI specifications,
and is embedded into IR nodes to eliminate lookup overhead during optimizations. For scenarios requiring further nuance,
such as MIR-level specializations or bytecode abstractions, an optional IntermediateType can serve as a bridge,
handling polymorphic or VM-specific details before final concretization.

In non-lowering modes like interpret or surface transpile, Ty suffices, providing runtime flexibility or
annotation-based output. When concrete layouts are required, the compiler lowers to THIR, then lifts a Typed AST (TAST)
from those ConcreteTypes—re-sugaring shapes based on stored provenance—so emitters retain surface structure while
gaining sealed type information. Ty and ConcreteType share the same underlying arena: const evaluation creates
provisional `mut type` tokens (Ty + pending layout) and, once frozen, THIR consumes the resulting `ConcreteType`
records directly. This 2-3 set approach—rooted in the needs of multi-mode execution—ensures efficiency: for example,
bytecode generation from LIR uses ConcreteType to emit typed instructions where possible, while static transpile reuses
the same data to produce explicit signatures for C without sacrificing readable syntax. Overall, it supports robust
features like type execution in comptime while scaling to diverse backends.
backends.

Opaque token boundaries ensure these identifiers never leak into user-visible APIs. Backends operate on surfaced type
data (ConcreteType or TAST annotations), preserving the flexibility to refactor token layout without breaking
downstream consumers.

## Conclusion and Benefits

This multi-mode compiler design achieves a balance of flexibility, performance, and debuggability through shared
components like the frontend and interpretation phases, while tailoring outputs to specific use cases. The compile mode
delivers optimized natives, interpret enables quick runs, bytecode provides portable debugging with stepped execution,
and transpile modes facilitate interoperability. The type system's evolution from Ty to ConcreteType (with an
optional intermediate) ensures consistency and efficiency across stages. This architecture not only addresses practical
needs—like multi-language inputs and custom bytecode—but also draws from proven systems, positioning it for
extensibility in future enhancements.
