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
  AST, e.g., Python or FerroPhase nodes) → AST (Unified, language-agnostic AST).
- **Interpretation**: A shared evaluation phase, configurable for compile-time (Comptime) or runtime behavior, handling
  tasks like constant folding, type querying, and execution.
- **Desugaring and IR Lowering**: AST → HIR (High-level IR, structured for inference) → THIR (Typed HIR, with embedded
  resolved types) → MIR (Mid-level IR, for optimizations) → LIR (Low-level IR, near-machine representation).
- **Output Generation**: Mode-specific backends, such as WASM/LLVM IR, bytecode, Rust code, or direct execution.

#### Execution Modes and Their Flows

Here's a tabular overview for clarity:

| Mode                             | Key Flow                                                                                                                                                                                                                             | Purpose and Characteristics                                                                                                                                                                                                                                                                                            |
|----------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **Compile**                      | Frontend → Interpretation(Comptime) → Desugar to HIR → [Type Inference + Refinement (interleaved with remaining comptime)] → [High-Level Optimization] → THIR (embed types) → MIR (mid-level opts) → LIR → WASM/LLVM IR              | Full compilation to native/web targets; emphasizes optimization and static analysis for performance. Inference on HIR allows structured type resolution, with interleaving ensuring comptime dependencies are handled iteratively.                                                                                     |
| **Interpret**                    | Frontend → Interpretation(Runtime)                                                                                                                                                                                                   | Direct runtime execution on the AST; lightweight, dynamic, suitable for scripting or quick testing. No IR lowering, focusing on immediate evaluation.                                                                                                                                                                  |
| **Bytecode**                     | Frontend → Interpretation(Comptime, lightweight) → Desugar to HIR → [Type Inference + Refinement (interleaved, minimal)] → THIR (embed types) → MIR (mid-level opts, optional) → LIR → Custom Bytecode Gen (pyc-like) → VM Execution | Generates portable, stack-based bytecode similar to Python's pyc format for VM execution; supports stepped debugging (e.g., opcode-by-opcode with inspection). Lightweight comptime enables initial folding and querying without full overhead. Bytecode generation starts post-LIR for optimized, low-level emission. |
| **Transpile (Surface)**          | Frontend → Interpretation(Comptime) → Codegen to Rust/... (walk annotated AST/EAST)                                                                                                                                                  | Converts to higher-level targets with comptime-driven transformations; keeps everything at the AST/EAST layer for fast migration, light refactors, or prototype sharing without deep analysis.                                                                                                                         |
| **Transpile (Static/Low-Level)** | Frontend → Interpretation(Comptime) → Desugar to HIR → [Algorithm W Inference + Type Projection + High-Level Optimization] → THIR (embed ConcreteType) → TAST Lift (with re-sugaring hints) → Codegen to Rust/C/...                     | Reuses the compile typing pipeline to obtain concrete layouts via a HIR→THIR projection, applies HIR-level optimizations, then lifts a Typed AST (TAST) that mirrors surface syntax (re-sugared) for emitters that require explicit types and ABI-aware code such as C.                                                |

#### Type Representations and Stage Mappings

The system uses a phased type system with 2-3 sets to support evolution from flexible inference to concrete codegen.
This is summarized below:

- **AstType** (Flexible, used in AST/HIR): Supports inference placeholders (e.g., type variables, unknowns), queries for
  comptime, and annotations. Stored in a type map (e.g., `HashMap<NodeId, AstType>`) for easy updates during
  interleaving.
- **ConcreteType** (Resolved, used in THIR/MIR/LIR): Immutable, machine-oriented (e.g., with sizes, layouts); embedded
  directly into IR nodes for efficiency post-resolution.
- **IntermediateType** (Optional third set, used in MIR/LIR if needed): Bridges AstType and ConcreteType for
  optimization-specific details (e.g., polymorphic variants or VM abstractions); only added for complex backends like
  diverse bytecode targets.

> **Term Glossary**
> - Core flow: **CST** → **LAST** → **AST** → **EAST** → **HIR** → **THIR**
>   - **CST**: Concrete Syntax Tree straight from parsing, preserving tokens and trivia.
>   - **LAST**: Language-specific AST normalized per frontend before unification.
>   - **AST**: Unified, language-agnostic tree prior to comptime.
>   - **EAST**: Evaluated AST snapshot after comptime interpretation/macro expansion.
>   - **HIR**: High-level IR with desugared control flow, suited for inference and high-level opts.
>   - **THIR**: Typed HIR produced via HIR→THIR projection; embeds `ConcreteType` data.
> - From **THIR**, the pipeline forks:
>   - **TAST** (static transpile branch): Typed AST lifted from THIR, reintroducing surface sugar for emitters like C.
>   - **MIR** → **LIR** (compile/bytecode branch): Mid-level IR for SSA optimizations followed by Low-level IR ahead of backend codegen.

| Stage/IR                                 | Primary Type Set                   | Storage Method          | Key Usage                                     |
|------------------------------------------|------------------------------------|-------------------------|-----------------------------------------------|
| AST                                      | AstType                            | Type Map                | Initial parsing, comptime querying.           |
| HIR                                      | AstType                            | Type Map                | Inference, interleaving with comptime.        |
| THIR                                     | ConcreteType                       | Embedded in nodes       | Post-resolution opts, type sealing.           |
| MIR/LIR                                  | ConcreteType (or IntermediateType) | Embedded in nodes       | Mid/low-level opts, codegen (e.g., bytecode). |
| Non-IR Modes (e.g., Interpret/Transpile) | AstType                            | Type Map or Annotations | Dynamic eval or output gen.                   |

This structure ensures consistency across modes while minimizing overhead—e.g., interpret mode relies solely on AstType
for runtime flexibility.

## Detailed Discussion: Execution Modes

The compiler's multi-mode design is centered on modularity and reuse, allowing users to select execution paths based on
needs like performance, debugging, or interoperability. By sharing the frontend and interpretation phases, the system
avoids duplication while enabling specialized behaviors. Each mode builds on a unified AST, derived from
language-specific parsing, to process inputs from diverse sources such as Python, FerroPhase, or custom languages. This
frontend normalization—via optional CST for raw syntax, LAST for idiomatic representations, and final AST (later EAST
after comptime evaluation) unification—ensures semantic consistency, making downstream stages language-agnostic.

### Compile Mode: Optimized Native Execution

Compile mode represents the most comprehensive path, transforming source code into efficient WASM or LLVM IR for native
or web deployment. Starting from the frontend, it proceeds to full comptime interpretation, where compile-time
evaluation handles advanced features like type querying and constant propagation via the shared `TypeQueryEngine`
described in the const-eval pipeline. This is followed by desugaring into
HIR, a structured intermediate representation ideal for type inference due to its explicit control flow and reduced
syntactic sugar.

Inference and refinement occur on HIR, interleaved with any remaining comptime operations to iteratively resolve
dependencies— for instance, a comptime function might reveal new type information that refines inference in subsequent
passes. This interleaving stabilizes the program state, preventing errors like incomplete constant folding by ensuring
types are available when needed. High-level optimizations, such as dead code elimination or inlining, are applied
post-inference, leveraging the resolved state. The pipeline then lowers to THIR, where types are embedded directly into
nodes for a sealed, typed view. Mid-level optimizations in MIR (e.g., SSA transformations) and low-level adjustments in
LIR (e.g., register allocation) prepare for final emission to WASM or LLVM IR. This mode excels in performance-critical
scenarios, with the multi-IR approach allowing targeted optimizations at each level.

### Interpret Mode: Immediate Runtime Execution

For rapid prototyping or dynamic scripting, interpret mode offers a streamlined path: directly from the frontend to
runtime interpretation. This involves a tree-walking evaluator on the AST, executing code in a runtime environment
without IR lowering or static optimizations. Runtime interpretation shares the same evaluation logic as comptime but
configured for dynamic behavior, supporting features like lazy evaluation or runtime type checks. It's particularly
useful for languages with dynamic semantics, as it bypasses the need for full type resolution, relying instead on
flexible AstType representations during execution. While less optimized than compile mode, it provides fast feedback
loops, making it ideal for REPLs or embedded scripting.

### Bytecode Mode: Portable VM Execution with Debugging

Bytecode mode bridges interpretation and compilation by generating executable bytecode for a virtual machine, enabling
portable deployment and enhanced debugging. Drawing inspiration from Python's pyc format, the bytecode is
custom-designed as a stack-based, dynamic system with opcodes for operations like PUSH, POP, CALL, and conditional
jumps. This format supports the least goal of stepped execution through VM hooks, allowing opcode-by-opcode advancement,
state inspection (e.g., stack frames, locals), and breakpoints—facilitated by embedding debug metadata like source line
mappings in the bytecode.

The flow begins with the frontend, followed by a lightweight comptime interpretation to perform initial constant folding
and type querying without full overhead. Desugaring to HIR enables minimal type inference and refinement, interleaved as
needed but kept concise to prioritize speed. Lowering continues through THIR (embedding resolved types), optional
mid-level optimizations in MIR, and LIR for low-level refinements. Bytecode generation starts after LIR, mapping its
operations directly to pyc-like instructions— for example, an LIR arithmetic op becomes a BINARY_ADD opcode, with
dynamic type handling to mimic Python's flexibility. The resulting bytecode is executed in a custom VM, which can be
toggled for stepped mode to support debugging workflows. This mode is versatile for environments requiring portability,
such as mobile or embedded systems, and its custom nature allows tailoring to specific needs, like extended opcodes for
language features.

### Transpile Modes: Language Conversion and Typed Output

Transpile mode focuses on code migration, converting the unified AST—after comptime interpretation—directly to targets
like Rust via a syntax-walking code generator. Comptime here annotates the AST with transformations, such as resolving
macros or folding expressions, ensuring the output is semantically equivalent without deep analysis.

For higher-level targets that tolerate dynamic edges or leaf inference (e.g., Rust shims, JavaScript scaffolding), the
surface transpiler stays entirely in the AST/EAST. It walks the comptime-annotated tree and prints source with optional
hints, giving fast turnaround without IR lowering, high-level optimization, or a separate typing phase. This path is
ideal when code shape needs to remain close to input or when turnaround speed overrides deep analysis.

Low-level or strongly typed targets reuse the compile-time lowering pipeline to obtain precise layouts. After comptime
expansion, the program lowers to HIR, runs constraint-based (Algorithm W) inference, and projects those results into
THIR where ConcreteType data is sealed into the IR. High-level optimizations (inlining, DCE, loop transforms, etc.) run
on HIR, taking advantage of its canonical control flow while attaching re-sugaring metadata so surface structure can be
reconstructed later. The compiler then lifts a Typed AST (TAST) that mirrors the original surface structure, reintroduces
desired sugar (e.g., `for` loops, match expressions), and carries THIR-resolved types on every node. C, bare-metal Rust,
or other ABI-sensitive emitters consume this lifted TAST so code generation has explicit sizes, calling conventions, and
qualifiers available without performing additional inference loops. This approach shares the heavy lifting with compile
mode while keeping a polished, surface-shaped handoff for emitters.

Const-evaluation transformations never mutate the unified AST in place. Each block records structural edits and, when
new types are needed, allocates provisional `mut type` handles (mirroring the language construct) through the shared
`TypeQueryEngine`. On success those handles are frozen into concrete definitions, producing the EAST snapshot shared by
every downstream mode.

In practice, the unified AST becomes EAST after the comptime interpreter runs, encapsulating evaluated constants and
macro expansions while keeping the shared structure. HIR then drops remaining sugar for analysis, and THIR seals
ConcreteType data before the TAST lift resurfaces a typed, human-friendly tree for low-level emitters.

## Cross-Mode and Cross-Stage Guarantees

- **Canonical EAST**: Every downstream stage (HIR, THIR, TAST, MIR, LIR) and every mode (compile, bytecode, transpile,
  interpret) consumes the same EAST snapshot produced by const evaluation. Differences manifest only in performance or
  final emission format, never in user-visible semantics.
- **Stage Preservation**: Transformations performed during lowering (AST→HIR→THIR→TAST/MIR/LIR) are semantics-preserving;
  no stage is permitted to alter the behaviour observable at the EAST level.
- **Deterministic Transformations**: Structural transformations scheduled during comptime apply in a deterministic order
  so that repeated builds produce identical EAST snapshots and downstream outputs.
- **Shared Diagnostics**: All diagnostics map back to EAST spans, ensuring consistent user feedback regardless of stage
  or mode.

## Detailed Discussion: Type System Design

The type system is phased to match the pipeline's progression, using distinct representations to optimize for each
stage's needs. Early stages prioritize flexibility with AstType, an expressive set that includes variants for unknowns,
generics, and comptime-queryable structures. This is managed via a type map, allowing non-destructive updates during
inference or interleaving, which is crucial for modes involving iterative refinement.

As the pipeline advances—particularly in compile, bytecode, or static transpile modes—types transition to ConcreteType
through a HIR→THIR projection. This set is more rigid, incorporating details like memory layouts and ABI specifications,
and is embedded into IR nodes to eliminate lookup overhead during optimizations. For scenarios requiring further nuance,
such as MIR-level specializations or bytecode abstractions, an optional IntermediateType can serve as a bridge,
handling polymorphic or VM-specific details before final concretization.

In non-lowering modes like interpret or surface transpile, AstType suffices, providing runtime flexibility or
annotation-based output. When concrete layouts are required, the compiler lowers to THIR, then lifts a Typed AST (TAST)
from those ConcreteTypes—re-sugaring shapes based on stored provenance—so emitters retain surface structure while
gaining sealed type information. AstType and ConcreteType share the same underlying arena: const evaluation creates
provisional `mut type` handles (AstType + pending layout) and, once frozen, THIR consumes the resulting `ConcreteType`
records directly. This 2-3 set approach—rooted in the needs of multi-mode execution—ensures efficiency: for example,
bytecode generation from LIR uses ConcreteType to emit typed instructions where possible, while static transpile reuses
the same data to produce explicit signatures for C without sacrificing readable syntax. Overall, it supports robust
features like type execution in comptime while scaling to diverse backends.
backends.

## Conclusion and Benefits

This multi-mode compiler design achieves a balance of flexibility, performance, and debuggability through shared
components like the frontend and interpretation phases, while tailoring outputs to specific use cases. The compile mode
delivers optimized natives, interpret enables quick runs, bytecode provides portable debugging with stepped execution,
and transpile modes facilitate interoperability. The type system's evolution from AstType to ConcreteType (with an
optional intermediate) ensures consistency and efficiency across stages. This architecture not only addresses practical
needs—like multi-language inputs and custom bytecode—but also draws from proven systems, positioning it for
extensibility in future enhancements.
