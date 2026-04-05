# FerroPhase

FerroPhase is a meta-compilation toolkit that lets you write Rust-adjacent code
with first-class structural metaprogramming, rich const-eval introspection, and
multiple compilation targets from a single, shared AST pipeline. It is also a
multi-frontend system: different surface languages share the same semantic
contract, and that contract must remain identical across IRs and modes.

## Why FerroPhase

### Semantic Invariant (Most Important)

**The program must have exactly the same semantics in every representation
(CST/LAST/AST/HIR/MIR/LIR) and in every execution mode (interpreter, bytecode VM,
and compiled backends).**

- Any semantic difference between modes or IRs is a correctness bug.
- Pipeline stages may change representation, optimize, or lower, but never
  observable behavior.
- Backend-specific details are allowed only when they preserve the same
  language-level result.
- `FERROPHASE_LOSSY` is explicitly outside this guarantee and must stay disabled
  for correctness validation.

Every capability below is designed to uphold that contract.

- Structural metaprogramming: generate fields, methods, and whole types with
  hygienic transformations while staying inside the shared semantic contract.
- Multi-mode toolchain: interpret, compile to native backends, or emit AST
  targets without changing source or observable behavior.
- Compile-time intelligence: query layouts, traits, and type metadata during
  const evaluation to shape emitted code, still subject to the same semantic
  guarantees.

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
   # Quick iteration (interpreter; same semantics as compiled)
   fp run src/main.fp

   # Or generate Rust via the backend pipeline (same semantics)
   fp compile src/main.fp --backend rust --output src/main.rs
   ```

## Core Capabilities

- Const evaluation with intrinsics like `sizeof!`, `hasfield!`, and
  `clone_struct!`, plus structural editors built on strict `std::intrinsic`
  lang items.
- Declarative type creation (`type T = { ... }`) with conditionals and loops
  embedded in compile-time blocks.
- Unified semantics pipeline: CST → LAST → AST → ASTᵗ (typed) → ASTᶜ
  (const-evaluated) → HIRᵗ → MIR (Mid-level Intermediate Representation; SSA CFG)
  → LIR, with a single semantic contract preserved across all stages.
- Execution modes can branch at different stages (for example, interpreter from
  typed AST; bytecode/native from lower IRs) but must remain semantically
  equivalent.
- Multi-target outputs: native/LLVM, custom bytecode + VM, high-level Rust
  transpilation with optional type annotations.

## Architecture at a Glance

- Shared frontend normalizes surface languages before const evaluation, so
  different frontends converge on a single contract before lowering.
- Language-specific intrinsic helpers are imported at the LAST layer and
  immediately re-railed into canonical `std` symbols. Strict intrinsics live
  under `std::intrinsic` as `#[lang]` items, and std wrappers call them so
  downstream stages see a consistent vocabulary.
- Deterministic comptime interpreter produces a canonical EAST snapshot that
  every backend consumes, preserving observable behavior across modes.
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

# Compile to Rust backend source (preserves language-level semantics)
fp compile src/service.fp --backend rust --output service.rs

# Emit AST target output (e.g. TypeScript)
fp compile src/service.fp --target typescript --output service.ts
```

## Roadmap

- ✅ Core AST infrastructure, const evaluation intrinsics, Rust target emission,
  CLI templates.
- 🚧 Parser upgrades for advanced macros, side-effect tracking, refinements to
  three-phase const evaluation.
- 📋 Planned: LLVM/WASM backends, language server integration, richer bytecode
  tooling.

## Learn More

- `docs/Language.md` – semantic contract and release governance
- `docs/Design.md` – pipeline and execution modes
- `docs/Types.md` – phased type system
- `docs/ConstEval.md` – const evaluation semantics
- `docs/Intrinsics.md` – intrinsic normalisation across languages
- `docs/QualityAssurance.md` – QA framework for AI-generated code
- `examples/` – end-to-end scenarios (flat, renumbered catalog)
