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

- **Frontend**: Source Language → CST (Concrete Syntax Tree, optional for parsing details) → LAST (Language-specific
  AST, e.g., Python or FerroPhase nodes) → AST (Unified, language-agnostic AST). The LAST → AST transition is where
  each language’s built-in library is rewritten into a canonical, language-agnostic `std` package so that everything
  downstream consumes the same primitives.
- **Desugaring & Typing**: AST → HIR (High-level IR, structured for inference) → THIR (Typed HIR built with Algorithm W
  inference).
- **Typed Interpretation**: Compile-time/runtime evaluation operates on THIR, producing an evaluated THIR′ that records
  const-eval effects and intrinsic mutations.
- **Resugaring & Emission**: THIR′ → TAST (Typed AST with source-like structure) → LAST′ (language-specific ASTs for
  transpilers) and, for native targets, re-projection through HIR → THIR → MIR → LIR → LLVM/bytecode backends.

### Standard Library Normalisation

Different frontends ship different standard libraries, but the rest of the compiler expects a single, consistent view
of core services (collections, strings, IO shims, etc.). The convergence process happens immediately after LAST is
constructed:

1. **Capture** – LAST nodes record which native modules or prelude bindings were pulled in (for example, Python’s
   `__builtins__`, JavaScript host globals, or FerroPhase’s structural helpers) and tag them with canonical intents.
2. **Rewrite** – while producing the unified AST, those intents are remapped onto the canonical `std` hierarchy. The
   AST therefore references only language-agnostic modules such as `std::string`, `std::iter`, or `std::intrinsics`.
3. **Propagation** – HIR, THIR, TAST, MIR, and LIR simply reuse the canonical package; no later stage needs to know
   which frontend supplied the definitions.
4. **Realisation** – emitters translate the canonical `std` back into the appropriate target form: high-level
   transpilers re-introduce surface imports, whereas low-level backends map calls to runtime helpers or LLVM intrinsics.

Because the mapping is completed before typed interpretation runs, every downstream optimisation and diagnostic treats
`std` like any other user-defined module. Adding a new frontend only requires defining the mapping from its native
library surface into the shared `std` vocabulary.

#### Execution Modes and Their Flows

| Mode                             | Key Flow                                                                                                                                                                                                                             | Purpose and Characteristics                                                                                                                                                                                                                                                                                            |
|----------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **Compile**                      | Frontend → Annotated AST → HIR → THIR → Typed Interpretation (comptime) → TAST → re-project (HIR → THIR) → MIR → LIR → LLVM/WASM                                                                                                      | Full native/web compilation. Typed interpretation folds consts and applies intrinsics on THIR before optimisation stages consume the evaluated program.                                                                                                                           |
| **Interpret (Runtime)**          | Frontend → Annotated AST → HIR → THIR → Typed Interpretation (runtime)                                                                                                                                                                | Direct execution with principal types and spans. No MIR/LIR lowering; ideal for REPLs and scripting tools.                                                                                                                                                                       |
| **Bytecode / VM**                | Frontend → Annotated AST → HIR → THIR → Typed Interpretation (comptime) → TAST → re-project (HIR → THIR) → MIR → LIR → Custom Bytecode → VM                                                                                            | Generates portable bytecode while reusing typed interpretation. Evaluated TAST feeds a second lowering, enabling debug metadata and stepped execution.                                                                                                                           |
| **Transpile (Surface)**          | Frontend → Annotated AST → HIR → THIR → Typed Interpretation (comptime) → TAST → Language-specific LAST′ → Codegen                                                                                                                      | Produces readable Rust/TypeScript/etc. outputs. Resugared TAST retains THIR types, so transpilers emit explicit signatures and folded constants without extra inference passes.                                                                                                   |

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
