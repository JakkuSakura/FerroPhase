# FerroPhase

FerroPhase is a meta-compilation toolkit that lets you write Rust-adjacent code with first-class structural metaprogramming, rich const-eval introspection, and multiple compilation targets from a single, shared AST pipeline.

## Why FerroPhase

- Structural metaprogramming: generate fields, methods, and whole types with hygienic transformations.
- Multi-mode toolchain: interpret, compile to native backends, or transpile to backend code targets without changing source.
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
   fp transpile src/main.fp --output src/main.rs
   ```

## Core Capabilities

- Const evaluation with intrinsics like `sizeof!`, `hasfield!`, and `clone_struct!`, plus structural editors built on strict `std::intrinsic` lang items.
- Declarative type creation (`type T = { ... }`) with conditionals and loops embedded in compile-time blocks.
- Unified pipeline: CST → LAST → AST → ASTᵗ (typed) → ASTᶜ (const-evaluated) → HIRᵗ → MIR (Mid-level Intermediate Representation; SSA CFG) → LIR, shared by all execution modes.
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

# Transpile (backend shorthand; defaults to Rust)
fp transpile src/service.fp --output service.rs

# Syntax-level transpile (AST serializer; e.g. TypeScript)
fp syntax-transpile src/service.fp --target typescript --output service.ts
```

## Roadmap

- ✅ Core AST infrastructure, const evaluation intrinsics, Rust transpiler, CLI templates.
- 🚧 Parser upgrades for advanced macros, side-effect tracking, refinements to three-phase const evaluation.
- 📋 Planned: LLVM/WASM backends, language server integration, richer bytecode tooling.

## Learn More

- `docs/Design.md` – pipeline and execution modes
- `docs/Types.md` – phased type system
- `docs/ConstEval.md` – const evaluation semantics
- `docs/Intrinsics.md` – intrinsic normalisation across languages
- `docs/QualityAssurance.md` – QA framework for AI-generated code
- `examples/` – end-to-end scenarios (flat, renumbered catalog)
