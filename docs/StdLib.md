# Standard Library Normalisation

FerroPhase ingests multiple surface languages, each with its own "standard library" expectations. To guarantee that the
rest of the pipeline sees a single, stable vocabulary, the compiler translates every language-specific library surface
into a canonical `std` package as the program moves from LAST to the unified AST. This document describes that process
and how it flows through the intermediate representations.

## Implementation Status

The std library normalization system is now fully implemented with dedicated std nodes throughout the compilation pipeline:

- **AST Level**: `ExprStdIoPrintln` and other std library nodes capture semantic information
- **HIR Level**: `StdIoPrintln` nodes maintain std library semantics through type checking
- **Runtime Mapping**: Systematic mapping from std functions to C runtime implementations
- **C Stdlib Integration**: Dedicated module with macro-based function declarations

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
                                             ↓                                      ↓
                                     ExprStdIoPrintln                        StdIoPrintln
                                                                                   ↓
                                                                            std::io::println
                                                                                   ↓
                                                                               C stdlib
                                                                              (puts, etc.)
```

1. **Capture in LAST**
   - Frontend shims annotate imports/builtins with canonical intents. Example: `println!` macro is recognized and
     mapped to `std::io::println`, while a FerroPhase module that uses `@sizeof` marks `std::intrinsics::sizeof`.
   - Metadata tracks origin and any language-only nuances (e.g., format string handling, newline behavior).

2. **Canonical projection during LAST → AST**
   - The converter creates dedicated std library nodes like `ExprStdIoPrintln` that preserve semantic information
     about the standard library function being called.
   - Format strings and arguments are properly parsed and structured within these dedicated nodes.

3. **Propagation to later IRs**
   - **HIR**: `StdIoPrintln` nodes maintain the high-level semantics while preparing for lowering
   - **THIR**: Std library calls are converted to regular function calls with canonical names (e.g., `std::io::println`)
   - **MIR/LIR**: Function calls are preserved with their canonical names for runtime mapping

4. **Runtime mapping in LLVM backend**
   - LIR codegen maps canonical std library functions to their C runtime equivalents:
     - `std::io::println` → `puts` (C stdio)
     - `std::alloc::alloc` → `malloc` (C stdlib)  
     - `std::f64::sin` → `sin` (libm)
   - C stdlib module provides type-safe function declarations using a macro-based system
   - Unknown functions result in compilation errors rather than silent fallbacks

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
