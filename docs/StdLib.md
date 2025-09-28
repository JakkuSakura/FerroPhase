# Standard Library & Builtin Normalisation

FerroPhase ingests multiple surface languages and multiple execution targets. Every language brings its own standard
library (and sometimes distinct builtin forms), while every backend expects a different concrete runtime. To keep that
matrix manageable we split the work into two phases:

1. **Frontend normalisation** – map language-specific constructs onto a *symbolic* `std` surface (including
   `std::builtins::*` and `std::intrinsics::*`) during
   LAST → AST → HIR → THIR.
2. **Backend materialisation** – translate those symbolic intrinsics into concrete calls/expressions only when a backend
   is chosen (LLVM, interpreter runtime, Rust transpile, …).

The rest of this document captures the design that makes those two steps cooperate.

## Goals

- **Single canonical vocabulary** – downstream passes see only `std::…` symbols regardless of the frontend, with
  language primitives appearing under `std::builtins::` and low-level compiler intrinsics under `std::intrinsics::`.
- **Backend-specific expansion without duplication** – each intrinsic is described once, then every backend emits its
  own node/value from that description.
- **No shared-state mutation** – THIR snapshots remain immutable so const-eval, MIR, LIR, and transpilers can reuse the
  same tree without fighting.

## Frontend Projection (LAST → THIR)

```
LAST  →  AST  →  HIR  →  THIR
  │        │        │       └── symbolic intrinsic
  │        │        └────────── `hir::ExprKind::Std(StdIntrinsic::IoPrintln)`
  │        └──────────────────── `ast::ExprStdIoPrintln`
  └───────────────────────────── frontend adapters annotate constructs
```

- Frontends register adapters that recognise language- or platform-specific helpers (`println!`, `print`, `sizeof`,
  `@effect`, …) and attach canonical intents while building LAST. Builtins are projected into `std::builtins::*`, while
  low-level compiler intrinsics land in `std::intrinsics::*`.
- The AST keeps that intent in dedicated nodes (e.g. `ExprStdIoPrintln`, `ExprBuiltinSizeOf`).
- HIR/THIR hold the same information using enums such as `StdIntrinsic` / `BuiltinIntrinsic`. **No** concrete runtime
  symbol is chosen here; the node simply says “this is `std::io::println` with these arguments”.

## Backend Materialisation via a Resolver

Every backend consumes the same `IntrinsicResolver`. Conceptually it is keyed by:

```
(intrinsic kind, backend flavour) -> ResolvedIntrinsic
```

Where `ResolvedIntrinsic` is a backend-neutral description. A minimal sketch:

```rust
pub enum ResolvedIntrinsic<'a> {
    Call {
        callee: &'a str,
        abi: CallAbi,
        args: Vec<IntrinsicArg>,
        return_kind: IntrinsicReturn,
    },
    InlineEmitter(fn(&mut dyn BackendBuilder) -> Result<()>),
    NotSupported(&'a str),
}
```

Important details:

- The resolver owns the *mapping* (`StdIo::Println` → formatted write). It does **not** construct THIR/MIR/TAST nodes
  itself.
- Arguments are specified declaratively (e.g. format string literal + trailing values, or pass-by-reference semantics)
  so each backend can lower them correctly.
- Builtins use the same mechanism—`std::builtins::size_of`, `std::builtins::addr_of`, etc. sit in the same table as
  higher-level helpers, while compiler intrinsics live under `std::intrinsics::`.
- Backends extend the resolver with their flavour (LLVM, interpreter runtime, Rust transpiler, JS transpiler, …).

### Consumers

- **Const evaluation (`InterpretationOrchestrator`)** – consults the resolver using the *runtime* flavour. Simple
  intrinsics may return an interpreter closure; others cause evaluation to request a lower stage.
- **THIR → MIR / MIR → LIR** – when lowering a symbolic intrinsic to MIR/LIR, the generator fetches the resolver entry
  for the compilation backend (LLVM or interpreter). The resulting `ResolvedIntrinsic::Call` becomes a MIR/LIR call to
  `printf`, runtime shims, etc.
- **THIR → TAST (transpile lift)** – the transpile pipeline asks the resolver with the target language flavour.
  Emission code then builds target-specific AST nodes (e.g. Rust’s `println!`, JS’s `console.log`).

Because everyone reads the same `ResolvedIntrinsic`, we only describe each intrinsic once per backend flavour. Builtins
share the path: no special cases required.

## Implementation Plan

1. **Resolver Abstraction** – build `intrinsics::resolver` with:
   - Symbol enums (e.g. `StdIntrinsic`, `BuiltinIntrinsic`).
   - Backend flavour enum (`BackendFlavor::Llvm`, `::Interpreter`, `::TranspileRust`, …).
   - `ResolvedIntrinsic` data structure.
2. **Populate Canonical Table** – encode existing std/builtin mappings (currently scattered in LLVM codegen,
   interpreter, etc.) inside the resolver modules.
3. **Thread Resolver Through Pipelines** – update THIR→MIR, MIR→LIR, interpreter, and THIR→TAST to request entries and
   materialise nodes/values from the returned description.
4. **Delete Ad-hoc Mappings** – remove backend-specific hard-coded strings (`std::io::println` → `puts`) once they read
   from the resolver.

## Notes & Open Questions

- Some intrinsics require backend-only behaviour (e.g. interpreter capturing output). Use the `InlineEmitter` / custom
  handler hook for those cases while still centralising the mapping.
- The resolver should be data-driven (e.g. `phf`/`HashMap`) so new intrinsics can be registered without editing every
  backend.
- Diagnostics must remain clear: when a backend lacks an implementation the resolver should return `NotSupported` with
  explanatory text; callers surface that via `DiagnosticManager`.

With this split we keep normalisation simple for frontends, avoid mutating THIR in place, and let every backend render
the same intrinsic in its own dialect without duplicating knowledge.
