# FerroPhase

FerroPhase is a meta-compilation toolkit that lets you write Rust-adjacent code with first-class structural metaprogramming, rich const-eval introspection, and multiple compilation targets from a single, shared AST pipeline.

## Why FerroPhase

- Structural metaprogramming: generate fields, methods, and whole types with hygienic transformations.
- Multi-mode toolchain: interpret, transpile to Rust, emit bytecode, or lower to native backends without changing source.
- Compile-time intelligence: query layouts, traits, and type metadata during const evaluation to shape emitted code.

## Quick Start

1. Install Rust (stable) and build the CLI:
   ```bash
   cargo build --release
   export PATH="$(pwd)/target/release:$PATH"
   ```
2. Scaffold a project:
   ```bash
   fp init my-project --template basic
   cd my-project
   ```
3. Compile and run:
   ```bash
   fp compile src/main.fp --target rust --run
   ```

## Core Capabilities

- Const evaluation with intrinsics like `sizeof!`, `hasfield!`, `implements!`, plus structural editors (`addfield!`, `addmethod!`).
- Declarative type creation (`type T = { ... }`) with conditionals and loops embedded in compile-time blocks.
- Unified pipeline: CST → LAST → AST → EAST (evaluated AST) → HIR → THIR/TAST → MIR (Mid-level Intermediate Representation; SSA CFG) → LIR, shared by all execution modes.
- Multi-target outputs: native/LLVM, custom bytecode + VM, high-level Rust transpilation with optional type annotations.

## Architecture at a Glance

- Shared frontend normalizes surface languages before const evaluation.
- Language-specific standard libraries are imported at the LAST layer and immediately re-railed into a canonical, language-agnostic `std` package as part of the LAST → AST conversion so every downstream stage sees the same primitives.
- Deterministic comptime interpreter produces a canonical EAST snapshot that every backend consumes.
- Type system phases:
  - `AstType`: flexible descriptors for inference and generation.
  - `ConcreteType`: layout-ready definitions embedded in THIR/MIR/LIR.
  - Optional backend-specific intermediate types.

## Example Workflow

```bash
fp compile examples/05_struct_generation.fp --emit tast
fp bytecode examples/09_metaprogramming_patterns.fp --run
fp transpile src/service.fp --target rust --emit east
```

## Roadmap

- ✅ Core AST infrastructure, const evaluation intrinsics, Rust transpiler, CLI templates.
- 🚧 Parser upgrades for advanced macros, side-effect tracking, refinements to three-phase const evaluation.
- 📋 Planned: LLVM/WASM backends, language server integration, richer bytecode tooling.

## Learn More

- `docs/Design.md` – pipeline and execution modes
- `docs/Types.md` – phased type system
- `docs/ConstEval.md` – const evaluation semantics
- `docs/StdLib.md` – standard library normalisation across languages
- `examples/` – end-to-end scenarios (flat, renumbered catalog)
