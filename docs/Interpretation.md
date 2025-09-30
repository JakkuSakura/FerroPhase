# Interpretation and Const Evaluation Roadmap

The interpreter is moving away from the THIR-centric design and now operates
directly on the typed AST produced by the front-end. This document captures the
new architecture and the work required to finish the migration.

## Target Architecture

1. **Typed AST as the authoritative IR**
   - Algorithm W inference runs directly on the canonical AST and writes the
     resolved type into the optional `ty` slots on expressions, patterns, and
     declarations.
   - No THIR snapshot is produced. Downstream passes (interpreter, HIR
     projection, backends) consume the same typed AST instance.

2. **AST Interpreter**
   - A single evaluator handles both const evaluation and runtime execution.
   - Evaluation works against typed AST nodes; intrinsic dispatch receives the
     enriched type information without consulting secondary maps.
   - The evaluator mutates the AST when operating in const mode (folding
     expressions, synthesising declarations, rewriting intrinsic calls) and
     leaves it untouched in runtime mode.

3. **Type-Aware HIR**
   - After evaluation, the typed AST is projected into a type-aware HIR for
     optimisation backends. Because types already live on the AST, the
     projection is responsible only for desugaring and ownership bookkeeping.

4. **Unified Diagnostics and Context**
   - Evaluation feeds a `DiagnosticManager` so every failure surfaces rich
     context (node span, intrinsic symbol, evaluation mode) and can recover when
     permitted by the tolerance settings.
   - Both const and runtime calls share an `InterpreterContext` that provides
     access to intrinsic registries, environment bindings, and cached values.

## Key Components

### AST Type Inference (`Algorithm W`)
- The existing solver from `hir_to_thir::type_inference` is being ported to work
  directly on the AST. It walks the tree once, applies substitutions in place,
  and annotates each node's `ty` slot.
- The solver exposes a public API so tooling can query inferred types without
  re-running inference.

### Interpreter Core
- Lives under `crates/fp-interpret/src/ast`. It accepts a typed AST plus an
  `InterpreterConfig` describing mode (const or runtime), intrinsic providers,
  and feature flags.
- Both modes return `InterpreterOutcome`, capturing produced values, emitted
  diagnostics, and any AST mutations that callers may want to serialise.

### Intrinsic Registry
- Intrinsics are registered against canonical `std` symbols with metadata that
  marks whether they are const-legal, runtime-only, or both.
- Implementations receive fully-typed arguments and can return structured
  results (`ConstValue`, `RuntimeValue`, or AST rewrites) without performing
  ad-hoc type checks.

### Const Evaluation Orchestrator
- Computes dependency order, runs the AST interpreter in const mode, and writes
  results back into the AST.
- Shares caches with runtime execution so blocks evaluated at compile time are
  available when the program runs.

## Immediate Work Items

1. Port Algorithm W to the AST (in-place annotation, public query interface).
2. Replace the THIR interpreter with the new AST evaluator and wire it into both
   const and runtime paths.
3. Migrate intrinsic implementations to the new registry and ensure they honour
   the typed AST contracts.
4. Update HIR projection to consume the typed AST output from interpretation.
5. Update CLI commands and orchestrators to drop THIR-specific terminology and
   storage (`.thi`, `.tce`, … files).
6. Expand the diagnostic surface so evaluation failures report rich context and
   can be downgraded when tolerance policies allow.

## Open Questions

- How should we persist typed AST snapshots for tooling? Options include a
  lightweight binary encoding or reusing the serializer with the type metadata
  embedded.
- What subset of runtime features must ship with the first iteration (I/O,
  filesystem access, FFI hooks)? These determine which intrinsics need to be
  ported immediately.
- Do we surface a stable API for third-party tooling to run the interpreter, or
  is it internal only for the initial release?

The work tracked here is a prerequisite for the larger AST-centric refactor and
should be completed before backends are updated to rely on the typed AST.
