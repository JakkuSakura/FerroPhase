# Intrinsics & Builtin Normalisation (AST-centric)

The removal of THIR shifts intrinsic handling to the typed AST. Every frontend
now lowers directly into the canonical AST, type inference annotates nodes in
place, and both the interpreter and backends read the same typed structures.
This document describes how intrinsic normalisation and backend materialisation
work in the new world.

## Two-Phase Approach

1. **Symbolic AST normalisation**
   - After parsing and macro expansion, a dedicated pass rewrites
     language-specific helpers into canonical `std::…` symbols directly on the
     AST.
   - The pass operates before type inference so the inferencer sees the unified
     vocabulary. Metadata (spans, attributes) is preserved.
   - The result is a *symbolic typed AST* once inference runs: structure matches
     the source but every intrinsic reference resolves to `std::io`,
     `std::builtins`, or `std::intrinsics` entries.

2. **Backend materialisation**
   - When a backend flavour is chosen, it materialises the canonical intrinsic
     into target-specific constructs. Because HIR now remains type-aware, the
     materialiser can run either on the typed AST (for interpreter / transpile
     targets) or during HIR projection / MIR lowering (for native backends).
   - The same resolver table drives every flavour—LLVM, runtime interpreter,
     transpilers, etc.—so mappings live in one place.

## Resolver Model

```
(intrinsic symbol, backend flavour) -> ResolvedIntrinsic
```

`ResolvedIntrinsic` is a declarative description of how to express the intrinsic
for the chosen backend:

```rust
pub enum ResolvedIntrinsic {
    AstRewrite(fn(&mut AstBuilder) -> Result<()>),
    HirLowering(fn(&mut HirBuilder) -> Result<()>),
    Unsupported(&'static str),
}
```

Key ideas:

- **Canonical vocabulary**: everything is registered under a `std::` symbol so
  lookups are stable across languages.
- **Shared metadata**: each entry records const legality, runtime-only status,
  argument expectations, and emitted diagnostics.
- **Identity friendly**: the interpreter typically uses simple `AstRewrite`
  closures (or even `noop`) while native backends leverage the `HirLowering`
  hook to generate calls into their runtime support libraries.

## Pipeline Integration

1. Frontends annotate their AST with enough information for the normaliser to
   recognise intrinsic forms (macro names, language-specific helpers).
2. The **AST normaliser** rewrites everything into canonical symbols before type
   inference.
3. Type inference annotates the canonical AST.
4. The interpreter executes the typed AST using the resolver's identity flavour
   (intrinsics map to builtin evaluators or runtime closures).
5. Backend materialisers invoke the resolver for their flavour and produce
   backend-specific nodes during projection/lowering.

Because everyone consults the same resolver, adding a new intrinsic involves:

1. Registering the canonical symbol and metadata.
2. Providing materialisers for the desired backend flavours.
3. (Optionally) updating the AST normaliser to detect more surface spellings.

## Open Items

- Decide whether resolver entries live in data files (`serde`/`ron`) or remain in
  Rust tables.
- Establish testing fixtures that validate each backend flavour can materialise
  the intrinsic (compile to MIR/LLVM, run under the interpreter, etc.).
- Ensure diagnostics clearly report when an intrinsic lacks support for the
  requested backend.

This AST-centric approach keeps intrinsic logic in one place while allowing each
backend to emit the representation it needs without reviving THIR.
