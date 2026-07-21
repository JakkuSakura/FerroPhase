# Logging And Tracing Guide

Logging should make dynamic compiler work visible without implying a fixed
pipeline. Trace spans should identify work items, requests, artefacts,
dependencies, and modes.

## Goals

- Show scheduler activity and scoped compiler work.
- Correlate diagnostics with `RequestId`, source span, and dependency edge.
- Preserve deterministic logs for semantic consistency checks.
- Keep tracing opt-in and environment-configurable.

## Logging Infrastructure

- Base logging uses the `tracing` crate configured by the CLI.
- Default level: `info`; set `FP_LOG=debug` or `FP_LOG=trace` for more detail.
- `FP_LOG_FORMAT=json` should produce machine-readable spans for tests and
  release evidence.

## Instrumentation Points

| Work | Suggested span | Notes |
|------|----------------|-------|
| Parse | `compiler.parse` | Source path, frontend, module count |
| Normalize | `compiler.normalize` | Canonical symbols, frontend provenance |
| Schedule work | `compiler.work` | Work kind, request id, dependency key |
| Type scope | `compiler.type` | Scope id, constraints, blockers |
| Register need | `compiler.request.need` | `CompileTimeNeed`, blocked node |
| Answer request | `compiler.request.answer` | Answer kind, invalidated artefacts |
| Scoped lowering | `compiler.lower` | HIR/MIR/LIR artefact keys |
| Execute scope | `compiler.execute` | Runtime/comptime mode, capabilities |
| Emit target | `compiler.emit` | Backend, output path, target features |

## Build Records

Release builds must emit a build record with:

- source revision and package graph;
- compiler options and feature flags;
- requested modes and final artefacts;
- request/dependency hashes used for semantic gates;
- output paths defined in [ReleaseArtifacts](ReleaseArtifacts.md).

## Roadmap

1. Introduce structured spans for scheduler work items.
2. Include module names, scope ids, `NodeId`s, and `RequestId`s in logs.
3. Correlate diagnostics with source spans and dependency edges.
4. Provide CLI flags `--log {error|warn|info|debug|trace}` and
   `--log-format {pretty,json}`.
5. Add test hooks that assert request ordering and artefact invalidation events.
