# Standard Library Normalisation

FerroPhase ingests multiple surface languages, each with its own “standard library” expectations. To guarantee that the
rest of the pipeline sees a single, stable vocabulary, the compiler translates every language-specific library surface
into a canonical `std` package as the program moves from LAST to the unified AST. This document describes that process
and how it flows through the intermediate representations.

## Goals

- **Canonical API surface** – downstream phases work with a single package graph (`std::…`) regardless of the source
  language.
- **Lossless semantics** – language-specific details (e.g., Python’s truthiness helpers, Rust’s ownership-aware
  iterators) survive the rewrite so later stages can reconstruct suitable runtime behaviour.
- **Backend portability** – emitters can rehydrate the canonical `std` into target-specific imports or runtime shims
  without duplicating adaptation logic.

## Pipeline Integration

```
CST → LAST —[capture native std usage]→ AST —[canonical std projection]→ EAST → HIR → THIR → MIR → LIR → Backends
```

1. **Capture in LAST**
   - Frontend shims annotate imports/builtins with canonical intents. Example: a Python frontend records that `len`
     maps to `std::iter::len`, while a FerroPhase module that uses `@sizeof` marks `std::intrinsics::sizeof`.
   - Metadata tracks origin and any language-only nuances (e.g., dynamic dispatch requirements).

2. **Canonical projection during LAST → AST**
   - The converter rewrites bindings so the AST references the canonical `std` modules. User code is untouched; only the
     injected library nodes change.
   - Symbols are namespaced using the shared package system (`std::<component>::…`).

3. **Propagation to later IRs**
   - EAST, HIR, THIR, MIR, and LIR all operate on the canonical `std`. Const evaluation, optimisation, and diagnostics
     therefore treat library calls exactly like user-defined modules.

4. **Realisation in emitters**
   - High-level transpilers (Rust, C, JavaScript) translate `std` back into the target’s native imports or prelude names.
   - Low-level backends (LLVM, VM bytecode) map `std` nodes onto runtime helpers, intrinsics, or generated support code.

## Package Layout

The canonical hierarchy is intentionally small and extensible:

- `std::core` – primitive types, math helpers, option/result shims.
- `std::string`, `std::collections`, `std::iter` – data structure and iterator utilities.
- `std::io`, `std::os` – host interfaces; backends decide between FFI bindings or runtime stubs.
- `std::intrinsics` – compile-time introspection (`sizeof`, `alignof`, trait queries) shared with const evaluation.

Frontends map their native constructs onto the closest canonical module. If no direct equivalent exists, they can attach
an adapter module under `std::compat::<language>`; backends may choose to elide or implement those helpers.

## Extending the Mapping

When adding a new language frontend:

1. Define a `StdAdapter` that enumerates native prelude/builtin symbols and maps them onto canonical module paths.
2. Register the adapter with the LAST builder so import statements and implicit globals are annotated automatically.
3. Provide realisation rules for transpilers or runtime backends if the canonical symbol needs a specialised
   implementation.

Because the canonical package lives entirely in the unified AST, the rest of the compiler requires no changes. The
adapter isolates frontend quirks while keeping diagnostics, optimisation, and code generation stable across languages.
