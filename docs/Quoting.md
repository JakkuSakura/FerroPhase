# FerroPhase Quoting & Splicing Semantics

This note documents the language-level operations that convert code into data and back again. The goal is to support
structured metaprogramming without sacrificing the determinism guarantees described in `docs/ConstEval.md` and
`docs/Design.md`.

## Terminology

We adopt the names **`quote`** and **`splice`**:

- **`quote { ... }`** captures code as an AST value while preserving hygiene metadata (scopes, bindings, spans).
- **`splice(expr)`** re-injects a previously quoted value into surrounding code, producing new AST structure.

> Other candidates (e.g., `quote`/`unquote`, `quote`/`run`, `lift`/`lower`) were considered. "Splice" emphasises that the
> operation inserts structured code into a host context, mirroring terminology from Template Haskell and Rust macro_rules!
> `quote!`/`#` syntax, while avoiding confusion with the `mut` keyword.

## Basic Usage

```ferrophase
const BLOCK = quote {
    fn helper(x: i32) -> i32 { x * 2 }
};

const DOUBLE_HELPER = splice(BLOCK);
```

- `BLOCK` becomes an AST value (represented internally as an `NodeId` + hygienic context).
- `splice(BLOCK)` materialises the AST inside the surrounding context, contributing to TAST during const evaluation.

## Comptime Integration

During Phase 2 const evaluation:

1. `quote` serialises the target code into an AST value stored in a `comptime::QuoteToken`. The token contains:
   - `Ty` annotations inferred for the quoted fragment
   - Hygiene metadata (scope ids, span provenance)
   - References to any `mut type` tokens produced inside the quote
2. `splice` registers a structural transformation identical to `addfield!`/`addmethod!`. The TypeQueryEngine treats the
   quoted fragment as an atomic transformation:
   - New declarations are introduced via provisional `mut type` tokens (if types are declared within the splice)
   - The TAST snapshot records the splice source span for diagnostics

All transformations remain deterministic: repeated quoting/splicing with identical inputs yields identical TAST.

## Type Handling

- Quoted fragments retain their `Ty` annotations. When spliced, the existing `mut type` promotion rules apply and
  ConcreteType entries are generated during commit.
- The Hindley–Milner solver sees spliced code the same way it sees handwritten AST—they are indistinguishable after
  promotion to TAST and subsequent HIR lowering.

## Hygiene & Scoping

- `quote` captures lexical scopes, preventing accidental variable capture. Spliced fragments introduce fresh bindings
  unless explicitly marked with `splice(using scope_var)` (future extension).
- Span information is preserved so diagnostics point to the original quoted location.

## Error Handling

- Quoted fragments are parsed/validated immediately; syntax errors surface at the `quote` call site.
- Splicing a fragment into an incompatible context (e.g., inserting statements where expressions are required) raises a
  comptime error, preserving the cross-stage guarantees.

## Interaction with Modes

- **Surface transpile**: treats spliced code like any other AST. Generated files include the re-sugared version of the
  injected code.
- **Static transpile**: the typed AST lift recognises quoted fragments and re-sugars them based on the stored
  provenance.
- **Bytecode/compile**: typed HIR lowering consumes spliced fragments after const evaluation; no special casing
  required.

## Future Work

- Support parameterised quoting (`quote<T>`), enabling type-specialised AST generation.
- Provide `splice!` macro sugar for concise embedding.
- Explore incremental caching so identical quotes across modules share storage.
