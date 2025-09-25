# Logging & Tracing Guide

This document outlines the current logging/tracing architecture for the FerroPhase compiler CLI and references planned
work captured in `specs/003-compile/tasks.md` (T027).

## Goals

- Provide operators and developers with visibility into each pipeline stage (parse → const eval → HIR → THIR → MIR → LIR → backend).
- Maintain deterministic logs across modes to preserve cross-stage guarantees (`docs/Design.md:158`).
- Ensure tracing output is opt-in and environment-configurable.

## Logging Infrastructure

- Base logging uses `tracing` crate configured in `crates/fp-cli/src/main.rs`.
- Default level: `info`; set `FP_LOG=debug` (or `trace`) to increase verbosity.
- Formatters:
  - Human-readable console output (`tracing_subscriber::fmt`)
  - Optional JSON output controlled by `FP_LOG_FORMAT=json` (planned)

### Key Instrumentation Points

| Stage            | Location                                           | Notes                                     |
|------------------|----------------------------------------------------|-------------------------------------------|
| Parsing          | `fp-cli/src/pipeline.rs:parse_source`              | Emits source path and module count        |
| Const Evaluation | `fp-cli/src/pipeline.rs:run_const_eval`            | Logs dependency graph and evaluation time |
| HIR Lowering     | `fp-optimize/src/transformations/ast_to_hir.rs`    | Span-preserving logs, TODO for modules    |
| THIR Projection  | `fp-optimize/src/transformations/hir_to_thir.rs`   | Placeholder instrumentation, to be expanded|
| MIR/LIR          | `fp-optimize/src/transformations/{thir_to_mir,mir_to_lir}` | MIR builds the Rust-style SSA graph; logging yet minimal |
| Backends         | `fp-llvm/src/codegen.rs`                           | Emits target triple, optimisation level   |

## Tracing Roadmap (T027)

1. **Structured Spans** – Introduce span guards for each pipeline step so nested operations (e.g., module lowering) are visible.
2. **File/Module Attribution** – Include module names and `NodeId` references in logs for easier correlation with EAST/THIR outputs.
3. **Error Correlation** – Ensure errors recorded through `tracing::error!` map back to EAST spans, matching the cross-stage guarantees.
4. **CLI Flags** – Add `--log {level}` and `--log-format {pretty,json}` flags with sensible defaults.
5. **Test Hooks** – Provide utilities in `fp-optimize/tests` to assert log spans when running pipeline tests.

## Current Status

- T027 is **in progress**: pipeline stages now emit `tracing` spans (`pipeline.const_eval`, `pipeline.lower.*`,
  `pipeline.codegen`, etc.) with debug events for persisted intermediates.
- Remaining work: expose CLI/ENV controls, add JSON formatter, and propagate span context into optimisation crates.

## Next Steps

- Implement span-based tracing in `fp-cli/src/pipeline.rs` and propagate span handles to transformation passes.
- Harden logging hooks in `fp-optimize` so tests can consume structured events.
- Document environment variables and CLI options once implemented.
