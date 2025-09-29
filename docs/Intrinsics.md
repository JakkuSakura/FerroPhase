# Intrinsics & Builtin Normalisation

FerroPhase ingests multiple surface languages and multiple execution targets. Every language brings its own intrinsic
vocabulary (and sometimes distinct builtin forms), while every backend expects a different concrete runtime. To keep that
matrix manageable we split the work into two phases:

1. **Symbolic THIR normalisation** – starting from the raw typed IR emitted by each frontend, run a THIR transformer
   that canonicalises language-specific helpers into the shared `std::…` vocabulary. The output is a
   backend-agnostic *symbolic THIR* snapshot.
2. **Backend materialisation** – clone that symbolic THIR when a target flavour is chosen and rewrite the intrinsics
   into concrete runtime calls/expressions (LLVM, interpreter runtime, Rust transpile, …). Each backend consumes its own
   materialised THIR snapshot.

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
LAST  →  AST  →  HIR  →  THIR (raw) ──[ThirNormalizer]──▶ THIR (symbolic)
```

- Frontends are free to keep the AST expressive for their language (Rust macros, Python `print`, etc.). They merely
  need to carry enough metadata for the THIR normaliser to recognise the constructs later.
- A dedicated **ThirNormalizer** (built on top of the `ThirTransformer` utility) walks the raw typed THIR emitted by the
  frontend and replaces any language-specific intrinsic with the canonical enums (`StdIntrinsic`, `BuiltinIntrinsic`,
  …). If a node is already canonical the transformation is an identity.
- The resulting *symbolic THIR* snapshot contains no backend-specific runtime names—only the shared `std::…` symbols
  used by the resolver.

## Backend Materialisation via a Resolver (THIR → THIR)

When we pick a backend flavour we clone the symbolic THIR and run a resolver-driven **ThirMaterializer** (built on top
of `ThirTransformer`) that materialises each intrinsic for that target. Every backend consumes the same
`IntrinsicResolver`. Conceptually it is keyed by:

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

- The resolver owns the *mapping* (`StdIo::Println` → formatted write). The materialisation pass clones the symbolic
  node and patches it according to the resolver entry.
- Arguments are specified declaratively (e.g. “first argument is a format string literal”) so every backend can lower
  them deterministically.
- Builtins use the same mechanism—`std::builtins::size_of`, `std::builtins::addr_of`, etc. sit in the same table as
  higher-level helpers, while compiler intrinsics live under `std::intrinsics::`.
- Backends extend the resolver with their flavour (LLVM, interpreter runtime, Rust transpiler, JS transpiler, …).
- Identity flavours are valid: the interpreter materialises intrinsics into closures or simply leaves the node as-is.

### Consumers

- **Materialised THIR snapshots** – every downstream stage works on the already-specialised THIR for its backend
  flavour; no further resolver lookups are required.
- **Const evaluation (`InterpretationOrchestrator`)** – uses the identity flavour, allowing the interpreter to execute
  symbolic helpers directly or through tiny runtime closures supplied by the resolver.
- **THIR → MIR / MIR → LIR** – compilation backends start from their materialised THIR snapshot, so MIR immediately sees
  concrete runtime calls (`printf`, allocator shims, …).
- **THIR → TAST (transpile lift)** – the transpile pipeline materialises intrinsics into the target language constructs
  (Rust `println!`, JS `console.log`, …) before emitting the final AST.

Because everyone reads the same `ResolvedIntrinsic`, we only describe each intrinsic once per backend flavour. Identity
flavours simply point back to the symbolic form.

## Implementation Plan

1. **ThirTransformer utility** – provide a reusable helper for cloning/mapping THIR nodes (both the normaliser and the
   materialiser are thin adapters built on it). The scaffold lives in
   `crates/fp-optimize/src/transformations/thir/`.
2. **Resolver Abstraction** – ensure `intrinsics::resolver` exposes:
   - Symbol enums (e.g. `StdIntrinsic`, `BuiltinIntrinsic`).
   - Backend flavour enum (`BackendFlavor::Llvm`, `::Interpreter`, `::TranspileRust`, …).
   - `ResolvedIntrinsic` data structure describing how to rewrite the node.
3. **Populate Canonical Table** – encode the mappings (currently scattered across backends) inside the resolver.
4. **Symbolic Normaliser** – implement a THIR→THIR pass that canonicalises raw frontend THIR into the shared symbolic
   vocabulary.
5. **Materialisation Passes** – for each backend implement a THIR→THIR rewrite that clones the symbolic tree and applies
   the resolver. Identity flavours can return the original node unchanged.
6. **Consume Materialised THIR** – keep MIR lowering, interpreters, and transpilers oblivious to the resolver; they
   operate solely on the materialised snapshot.
7. **Delete Ad-hoc Mappings** – remove backend-specific hard-coded strings (`std::io::println` → `puts`, etc.) once all
   consumers read from the resolver.

## Notes & Open Questions

- Some intrinsics require backend-only behaviour (e.g. interpreter capturing output). Use the `InlineEmitter` / custom
  handler hook for those cases while still centralising the mapping.
- The resolver should be data-driven (e.g. `phf`/`HashMap`) so new intrinsics can be registered without editing every
  backend.
- Diagnostics must remain clear: when a backend lacks an implementation the resolver should return `NotSupported` with
  explanatory text; callers surface that via `DiagnosticManager`.

With this split normalisation happens once in the symbolic THIR, and every backend (including the interpreter) operates
on a materialised clone tailored to its flavour—no duplicated mappings, no shared-state mutations.
