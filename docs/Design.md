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

- **Frontend**: Source Language → CST (Concrete Syntax Tree, optional for parsing details) → lAST (Language-specific
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
| **Transpile**                    | Frontend → Interpretation(Comptime) → Codegen to Rust/... (walk annotated AST)                                                                                                                                                       | Converts to another language (e.g., Rust) with comptime-driven transformations; no full typing or IR, keeping it simple for code migration.                                                                                                                                                                            |
| **Transpile with Type Checking** | Frontend → Interpretation(Comptime) → [Type Inference/Check on Annotated AST, integrated into interpretation] → AST with Type Annotations → Codegen to Rust/...                                                                      | Adds type annotations for checked output; inference integrated into comptime for efficiency, producing typed code suitable for static languages.                                                                                                                                                                       |

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
frontend normalization—via optional CST for raw syntax, lAST for idiomatic representations, and final AST
unification—ensures semantic consistency, making downstream stages language-agnostic.

### Compile Mode: Optimized Native Execution

Compile mode represents the most comprehensive path, transforming source code into efficient WASM or LLVM IR for native
or web deployment. Starting from the frontend, it proceeds to full comptime interpretation, where compile-time
evaluation handles advanced features like type querying and constant propagation. This is followed by desugaring into
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

The variant with type checking integrates inference and checking into the comptime interpretation phase, operating on
the annotated AST to produce type-aware output. This might involve a lightweight desugaring if needed, but it emphasizes
AST-level annotations for simplicity. The result is Rust code with explicit type declarations, useful for integrating
into statically typed ecosystems. Both transpile modes leverage the frontend's multi-language support, making them
powerful for cross-language tools.

## Detailed Discussion: Type System Design

The type system is phased to match the pipeline's progression, using distinct representations to optimize for each
stage's needs. Early stages prioritize flexibility with AstType, an expressive set that includes variants for unknowns,
generics, and comptime-queryable structures. This is managed via a type map, allowing non-destructive updates during
inference or interleaving, which is crucial for modes involving iterative refinement.

As the pipeline advances—particularly in compile and bytecode modes—types transition to ConcreteType upon lowering to
THIR. This set is more rigid, incorporating details like memory layouts and ABI specifications, and is embedded into IR
nodes to eliminate lookup overhead during optimizations. For scenarios requiring further nuance, such as MIR-level
specializations or bytecode abstractions, an optional IntermediateType can serve as a bridge, handling polymorphic or
VM-specific details before final concretization.

In non-lowering modes like interpret or transpile, AstType suffices, providing runtime flexibility or annotation-based
output. This 2-3 set approach—rooted in the needs of multi-mode execution—ensures efficiency: for example, bytecode
generation from LIR uses ConcreteType to emit typed instructions where possible, falling back to dynamic checks for
pyc-like dynamism. Overall, it supports robust features like type execution in comptime while scaling to diverse
backends.

## Conclusion and Benefits

This multi-mode compiler design achieves a balance of flexibility, performance, and debuggability through shared
components like the frontend and interpretation phases, while tailoring outputs to specific use cases. The compile mode
delivers optimized natives, interpret enables quick runs, bytecode provides portable debugging with stepped execution,
and transpile modes facilitate interoperability. The type system's evolution from AstType to ConcreteType (with an
optional intermediate) ensures consistency and efficiency across stages. This architecture not only addresses practical
needs—like multi-language inputs and custom bytecode—but also draws from proven systems, positioning it for
extensibility in future enhancements.