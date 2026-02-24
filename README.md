# FerroPhase

FerroPhase is a meta-compilation toolkit that lets you write Rust-adjacent code with first-class structural metaprogramming, rich const-eval introspection, and multiple compilation targets from a single, shared AST pipeline.

## Why FerroPhase

### Semantic Invariant (Most Important)

**The program must have exactly the same semantics in every representation (CST/LAST/AST/HIR/MIR/LIR) and in every execution mode (interpreter, bytecode VM, and compiled backends).**

- Any semantic difference between modes or IRs is a correctness bug.
- Pipeline stages may change representation, optimize, or lower, but never observable behavior.
- Backend-specific details are allowed only when they preserve the same language-level result.
- `FERROPHASE_LOSSY` is explicitly outside this guarantee and must stay disabled for correctness validation.

- Structural metaprogramming: generate fields, methods, and whole types with hygienic transformations.
- Multi-mode toolchain: interpret, compile to native backends, or emit AST targets without changing source.
- Compile-time intelligence: query layouts, traits, and type metadata during const evaluation to shape emitted code.

## Quick Start

1. Install Rust (stable) and build the CLI:
   ```bash
   cargo build --release
   export PATH="$(pwd)/target/release:$PATH"
   ```
2. Scaffold a project:
   ```bash
   magnet init my-project --template basic
   cd my-project
   ```
3. Compile and run:
   ```bash
   # Quick iteration (interpreter)
   fp run src/main.fp

   # Or generate Rust via the backend pipeline
   fp compile src/main.fp --backend rust --output src/main.rs
   ```

## Core Capabilities

- Const evaluation with intrinsics like `sizeof!`, `hasfield!`, and `clone_struct!`, plus structural editors built on strict `std::intrinsic` lang items.
- Declarative type creation (`type T = { ... }`) with conditionals and loops embedded in compile-time blocks.
- Unified semantics pipeline: CST → LAST → AST → ASTᵗ (typed) → ASTᶜ (const-evaluated) → HIRᵗ → MIR (Mid-level Intermediate Representation; SSA CFG) → LIR.
- Execution modes can branch at different stages (for example, interpreter from typed AST; bytecode/native from lower IRs) but must remain semantically equivalent.
- Multi-target outputs: native/LLVM, custom bytecode + VM, high-level Rust transpilation with optional type annotations.

## Architecture at a Glance

- Shared frontend normalizes surface languages before const evaluation.
- Language-specific intrinsic helpers are imported at the LAST layer and immediately re-railed into canonical `std` symbols. Strict intrinsics live under `std::intrinsic` as `#[lang]` items, and std wrappers call them so downstream stages see a consistent vocabulary.
- Deterministic comptime interpreter produces a canonical EAST snapshot that every backend consumes.
- Type system phases:
  - `Ty`: canonical AST-level descriptors populated by Algorithm W.
  - `hir::Ty`: lowered, layout-aware types shared with MIR/LIR.
  - Optional backend-specific intermediate types.

## Example Workflow

```bash
# Inspect the AST
fp parse examples/05_struct_generation.fp

# Interpret
fp run examples/09_metaprogramming_patterns.fp

# Compile to Rust backend source
fp compile src/service.fp --backend rust --output service.rs

# Emit AST target output (e.g. TypeScript)
fp compile src/service.fp --target typescript --output service.ts
```

## Roadmap

- ✅ Core AST infrastructure, const evaluation intrinsics, Rust target emission, CLI templates.
- 🚧 Parser upgrades for advanced macros, side-effect tracking, refinements to three-phase const evaluation.
- 📋 Planned: LLVM/WASM backends, language server integration, richer bytecode tooling.

## Learn More

- `docs/Design.md` – pipeline and execution modes
- `docs/Types.md` – phased type system
- `docs/ConstEval.md` – const evaluation semantics
- `docs/Intrinsics.md` – intrinsic normalisation across languages
- `docs/QualityAssurance.md` – QA framework for AI-generated code
- `examples/` – end-to-end scenarios (flat, renumbered catalog)
