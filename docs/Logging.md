# Logging & Tracing Guide

This document outlines the current logging/tracing architecture for the FerroPhase compiler CLI and references planned
work captured in `specs/003-compile/tasks.md` (T027).

## Goals

- Provide operators and developers with visibility into each pipeline stage (parse → type inference → const eval → HIRᵗ → MIR → LIR → backend).
- Maintain deterministic logs across modes to preserve cross-stage guarantees (`docs/Design.md:158`).
- Ensure tracing output is opt-in and environment-configurable.

## Logging Infrastructure

- Base logging uses `tracing` crate configured in `crates/fp-cli/src/main.rs`.
- Default level: `info`; set `FP_LOG=debug` (or `trace`) to increase verbosity.
- Formatters:
  - Human-readable console output (`tracing_subscriber::fmt`)
  - Optional JSON output controlled by `FP_LOG_FORMAT=json` (planned)

### Key Instrumentation Points

| Stage            | Location                                             | Notes                                     |
|------------------|------------------------------------------------------|-------------------------------------------|
| Parsing          | `crates/fp-cli/src/pipeline.rs:parse_source`                | Emits source path and module count        |
| Type Enrichment  | `crates/fp-cli/src/pipeline.rs:run_type_enrichment_stage`   | Logs typed AST cloning and HIR output     |
| Const Evaluation | `crates/fp-cli/src/pipeline.rs:run_const_eval_stage`        | Logs stubbed const-eval (AST focus)       |
| HIR Emission     | `crates/fp-backend/src/transforms/ast_to_hir/mod.rs`        | Span-preserving logs, TODO for modules    |
| Future Backends  | _pending rewrite_                                    | MIR/LIR/LLVM hooks will return once ready |

## Tracing Roadmap (T027)

1. **Structured Spans** – Introduce span guards for each pipeline step so nested operations (e.g., module lowering) are visible.
2. **File/Module Attribution** – Include module names and `NodeId` references in logs for easier correlation with typed AST/HIR outputs.
3. **Error Correlation** – Ensure errors recorded through `tracing::error!` map back to TAST spans, matching the cross-stage guarantees.
4. **CLI Flags** – Added `--log {error|warn|info|debug|trace}` and `--log-format {pretty,json}` flags with sensible defaults.
5. **Test Hooks** – Provide utilities in `fp-backend/tests` to assert log spans when running pipeline tests.

## Current Status

- T027 is **in progress**: pipeline stages now emit `tracing` spans (`pipeline.const_eval`, `pipeline.lower.*`,
  `pipeline.codegen`, etc.) with debug events for persisted intermediates.
- Remaining work: propagate span context into optimisation crates.

## Next Steps

- Implement span-based tracing in `fp-cli/src/pipeline.rs` and propagate span handles to transformation passes.
- Harden logging hooks in `fp-backend` so tests can consume structured events.
- Document environment variables and CLI options once implemented.
