# Bootstrap Strategy

## Objective
- Allow FerroPhase to bring up a useful subset of the toolchain without cloning its full crate graph, by swapping externally hosted stages (parsers, optimizers, backends) with simpler local shims or precomputed JSON inputs.
- Focus on pieces that already live in this repository (`fp-core`, AST transforms, diagnostics) and identify how far we can progress before we need dependencies such as `syn`, `tree-sitter`, `swc`, or LLVM toolchains.
- Drive this mode via a `bootstrap` cargo feature plus a runtime environment variable (e.g. `FERROPHASE_BOOTSTRAP=1`) so the existing `fp` binary can switch behaviour without a bespoke bootstrap executable.

## Executive Summary
- **Viable today:** Loading and manipulating AST/HIR structures, running intrinsic rewrites, and emitting diagnostics can be made self-contained with modest work (feature flags, alternate storage types).
- **Primary goal:** `fp-lang` becomes the bootstrap frontend and must not depend on `fp-rust`/`syn` paths. Snapshot replay becomes optional rather than required.
- **Backend goal:** `fp-native` replaces `fp-llvm` for bootstrap (no LLVM/clang dependency), so native emitters and linkers must cover the compiler’s own needs.

## Current Dependency Surface

| Pipeline Stage | Primary Crates | Key External Dependencies | Bootstrap Impact |
| -------------- | -------------- | ------------------------- | ---------------- |
| Source parsing / LAST | `fp-lang` (superset of `fp-rust`) | `syn`, `tree-sitter`, `swc`, `cargo-metadata`, language runtimes | Bootstrap should rely on `fp-lang` only; remove or feature-gate external parsers. |
| AST storage & diagnostics | `fp-core`, `fp-cli` (diagnostics) | `serde`, `dashmap`, `derive_more`, `miette`, `tracing` | Replace `dashmap` with `HashMap` on a `bootstrap` feature; keep `serde` (already vendored via crates.io snapshot) or bake a minimal serializer. |
| Typing & const-eval | `fp-typing`, `fp-interpret`, `fp-optimize` | `syn`, `proc-macro2`, `dashmap`, `tokio` (through CLI), `serde_json` | Gate parsing helpers; keep bootstrap path pure and remove `syn`-only runtime hooks. |
| Backends (native) | `fp-native` | system linker (ld/lld), platform ABI details | Bootstrap uses `fp-native`; must cover runtime calls, calling conventions, and struct layout. |
| Backends (other) | `fp-llvm`, `fp-typescript`, `fp-python`, `fp-csharp`, `fp-zig`, `fp-wit` | `inkwell`, system LLVM/clang, `swc`, language printers | Not required for bootstrap. |
| CLI orchestration | `fp-cli` | `clap`, `tokio`, `indicatif`, `miette` | For bootstrap, ship a tiny driver (no async, minimal CLI) or expose library entrypoints callable from an external script. |

## Bootstrap-Friendly Building Blocks

### Core data model
- `fp-core`’s AST/HIR/LIR structures already derive `Serialize`/`Deserialize`. Add a `bootstrap` cargo feature that:
  - switches `DashMap`-backed contexts in `context.rs` to plain `HashMap` + `RefCell`,
  - bundles a lightweight `AstSerializer`/`Deserializer` using `serde_json`.
- Guard the bootstrap runtime path with `FERROPHASE_BOOTSTRAP=1`; when set, the binary should prefer JSON inputs while still executing the full pipeline (type-checking, const-eval, lowering) using prepared metadata.
- Deliver a `JsonAstLoader` helper (either in `fp-core` or a new `fp-bootstrap` crate) that loads `Node`, `Ty`, and `HIR` representations from JSON files.

### Diagnostics and intrinsic normalization
- `fp-optimize::passes::normalize_intrinsics` is internal; it depends on `dashmap` only through shared contexts. With the single-threaded context flag it can keep working.
- CLI diagnostics rely on `miette`; for bootstrap we can emit plain strings or reuse the simplified `DiagnosticManager` already provided by `fp-core`.

### Typing and const evaluation
- Current code dynamically parses Rust fragments (`syn`) when it encounters `Expr::Any` or stringly typed casts. In bootstrap mode the pipeline should avoid `syn` entirely and stay within `fp-lang` semantics.
- Const evaluation continues to execute so that downstream stages see the same mutations; if `syn`-backed paths exist, they must be removed or guarded.

### Lowering and backend stages
- `fp_optimize::transformations` converts typed AST into HIR/MIR/LIR. These modules can run in bootstrap as long as typing is parser-free.
- For bootstrap, `fp-native` should be the default backend; `fp-llvm` is optional/off.

## JSON Snapshot Workflow (Optional)

1. **Stage 0 – frontend only.** Build the toolchain normally (`cargo build --release`) and run `fp parse Cargo.toml --no-resolve --snapshot …` to materialise the entire workspace AST as `*.ast.json`. No optimisation, typing, or lowering occurs; we only capture the parsed tree that later stages will replay.
2. **Stage 1 – replay and emit.** Using the Stage 0 `fp` binary, set `FERROPHASE_BOOTSTRAP=1` and feed the saved snapshot into `fp compile`. The pipeline skips parsing, rehydrates the AST, then performs intrinsic normalisation, typing, const evaluation, MIR/LIR generation, and emits native code via `fp-native`.
3. **Stage 2 – self-host check.** Take the freshly produced `fp` binary from Stage 1 and repeat Stage 1 against the same snapshot. Matching outputs demonstrate that the new compiler can drive the optimisation and codegen passes on its own inputs.

### Reference command sequence

```
# Stage 0 – parse and persist the AST snapshot
cargo build --release
"$PWD/target/release/fp" parse \
  Cargo.toml \
  --no-resolve \
  --snapshot target/bootstrap/workspace.stage0.ast.json

# Stage 1 – rebuild in bootstrap mode
cargo build --release --no-default-features --features bootstrap

# Stage 1 – replay snapshot, emit native code
FERROPHASE_BOOTSTRAP=1 "$PWD/target/release/fp" compile \
  target/bootstrap/workspace.stage0.ast.json \
  --target native \
  --output target/bootstrap/workspace.stage1.out
FERROPHASE_BOOTSTRAP=1 "$PWD/target/release/fp" compile \
  target/bootstrap/workspace.stage0.ast.json \
  --target binary \
  --output target/bootstrap/workspace.stage1.bin

# Stage 2 – use the Stage 1 compiler to repeat the replay
stage1_bin="$PWD/target/bootstrap/workspace.stage1.out" # use .exe on Windows
FERROPHASE_BOOTSTRAP=1 "$stage1_bin" compile \
  target/bootstrap/workspace.stage0.ast.json \
  --target native \
  --output target/bootstrap/workspace.stage2.out
FERROPHASE_BOOTSTRAP=1 "$stage1_bin" compile \
  target/bootstrap/workspace.stage0.ast.json \
  --target binary \
  --output target/bootstrap/workspace.stage2.bin
```

For convenience, `scripts/bootstrap.sh` should automate these steps (including rebuilding with the `bootstrap` feature and the Stage 2 verification). Running it from the repository root produces Stage 1 and Stage 2 outputs with no additional arguments:

```
scripts/bootstrap.sh
```

> Note: the Stage 1 binary is emitted as `*.out` on Unix-like systems and `*.exe` on Windows—adjust the `stage1_bin` assignment accordingly.

When using the bootstrap binary, default features (and therefore optional dependencies like `miette`) stay disabled. Pass `--no-default-features --features bootstrap` to ensure the smaller dependency surface.

## Proposed Implementation Plan

1. **Feature-gate external crates**
   - Add a shared `bootstrap` cargo feature to `fp-core`, `fp-typing`, `fp-optimize`, and `fp-cli`.
   - Provide alternative type aliases or modules that compile without `dashmap`, `tokio`, or `tree-sitter`.
   - Ensure unit tests and CI build both default and `bootstrap` configurations.
2. **Runtime toggle**
   - Introduce an environment variable (`FERROPHASE_BOOTSTRAP`) that switches the CLI into bootstrap mode at runtime.
   - Flow this flag through `PipelineOptions` so stages can adjust behaviour (prefer JSON artefacts, use alternate serializers) while still exercising the full pipeline.
3. **Introduce JSON loaders (optional)**
   - Provide `fp_core::ast::json` helpers (done) so bootstrap builds can import/export ASTs without touching external parsers.
   - Allow the CLI pipeline to recognise `.json` inputs when `FERROPHASE_BOOTSTRAP=1` and reuse the standard stages on reconstructed trees.
   - Expose an opt-in export path via `FERROPHASE_BOOTSTRAP_SNAPSHOT=1` so Stage 0 flows can emit fresh snapshots alongside existing intermediates.
4. **Record and replay const evaluation**
   - Optional future work: persist AST deltas or interpreter outputs if we want to avoid re-running const evaluation during bootstrap.
5. **Automation script**
   - Provide a repository script that orchestrates the Stage 0 → Stage 1 cycle: build `fp` normally, emit snapshots, rebuild with `--features bootstrap`, set the env var, and run validation steps.
6. **Stretch goals**
   - Replace JSON dumps with a stable binary format once the pipeline is proven.

## TODOs (Bootstrap Blockers)
- Re-enable `fp-cli` bootstrap compile path or remove snapshot-replay usage entirely.
- Make `fp-lang` a fully self-contained frontend (no `fp-rust`/`syn`/`cargo-metadata` in bootstrap).
- Ensure `fp-native` covers all compiler-required features (call ABI, runtime intrinsics, struct layout, linker integration).
- Audit `fp-typing`/`fp-interpret` for `syn`-backed paths and guard/replace them for bootstrap.
- Update `scripts/bootstrap.sh` to use `--target native` and remove LLVM/clang steps.

## Bootstrap Requirements: Magnet + Module System

### Magnet (crates/magnet)
- Provide a `bootstrap` feature that removes `fp-rust`/`fp-typescript` dependencies and relies on `fp-lang`/`fp-core` only.
- Ensure workspace resolution and `Magnet.toml` processing do not depend on `cargo-metadata` or external toolchains.
- Keep config parsing limited to `toml`/`serde` (no dynamic Rust parsing).

### Module System (fp-lang superset)
- Support `self`, `super`, and `crate` path prefixes with correct scoping.
- Resolve `pub use` reexports across module boundaries (including `super::` access).
- Preserve Rust-like name resolution rules (shadowing, nested modules, qualified paths).
- Avoid any `fp-rust`/`syn`-only resolution paths when `bootstrap` is enabled.

## Missing Language Features (Bootstrap Blockers)

### fp-lang frontend
- `t-strings` are not supported yet.
- CST parser rejects trailing tokens and macro callees in some expr paths.
- `emit!` token rewrite is partial and uses placeholder spans.

### AST → HIR
- Closures are disabled in bootstrap (lowered to unit).
- `Expr::Any` dynamic payloads are unsupported (lowered to unit).
- Pattern lowering is incomplete (unsupported pattern kinds and some variant patterns).
- Module-level expression items can be dropped when unsupported.

### Typing / inference
- Some unary ops and binary‑op‑as‑function calls are unsupported.
- Certain literals/values are rejected by inference.
- Pattern inference is incomplete.
- HIR→MIR rejects inference markers (uses error type).

### HIR → MIR / MIR semantics
- Monomorphization pass is missing.
- Arbitrary precision decimals unsupported.
- Named arguments unsupported (calls + format).
- Panic format payload unsupported in compiled backends.
- `printf` argument types limited.
- `size_of` unsupported for some types; array length via pointer unsupported.
- Field/index access has unsupported base expressions.

### MIR → LIR
- Some place projections and MIR intrinsics are still unsupported.

### Backend (fp-native)
- Any missing lowering/ABI feature used by the compiler will block bootstrap; audit against compiler IR usage.

## Open Questions & Risks
- **Type coverage:** how to guarantee that pre-annotated ASTs remain in sync with code changes? We likely need hashing/versioning baked into the snapshot format.
- **Const-eval side effects:** recorded outcomes must encode not only mutated ASTs but also generated functions/impls. Need a canonical diff format to replay safely.
- **Maintenance overhead:** dual-mode (`default` vs `bootstrap`) can drift; automated CI must build both configurations.
- **Lack of backend validation:** without LLVM/JS printers we only prove that front-half transformations work. Decide whether bootstrap should at least emit Rust transpilation (requires `syn` again) or stop at MIR.
- **Data volume:** serialized AST/HIR for the entire workspace may be large. Consider per-crate snapshots or chunking.

With these constraints, partial self-bootstrap is realistic: we can verify AST normalization, intrinsic handling, and lowering using only code from this repository plus `serde` (or a bundled serializer). Full source parsing and backend emission remain outside the bootstrap scope unless FerroPhase eventually re-implements their functionality natively.
