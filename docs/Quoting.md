# FerroPhase Quoting & Splicing (Keywords)

This document defines the language keywords that turn code into data (quoting)
and insert code produced at compile time (splicing). They establish compiler
behaviour and staging boundaries and are distinct from builtin macros.

Keywords vs builtin macros
- Keywords affect parsing, staging, scoping, and compiler behaviour.
- Builtin macros are compiler-reserved sugar that expand to AST after parsing;
  they cannot change staging rules. See `docs/ConstEval.md` for `emit!`.

In FerroPhase, `quote` and `splice` are keywords.

## Syntax

- Quote (produces a const QuoteToken):
  - Supported form: `quote { … }` (kind inferred)

Kind inference for `quote { … }`:
- If the fragment is a single expression → expr token.
- If the fragment contains only item declarations at top level → item token.
- Otherwise, if the fragment is a sequence of statements → stmt token.
- Ambiguous or mixed forms produce a diagnostic suggesting an explicit kind.

- Splice (inserts a QuoteToken according to the surrounding position):
  - Expression position: `let v = splice ( EXPR_TOKEN );`
  - Statement position: `splice ( STMT_TOKEN );`
  - Item position: `splice ( ITEM_TOKEN );`

Notes
- Quoted fragments are values at compile time (consts). Tokens carry their
  fragment kind; `splice` validates that the token kind matches the insertion
  site and emits a diagnostic otherwise.

## Basic Usage

```ferrophase
// Top-level consts: quote produces const tokens
const CASES = quote item {
    enum E { A, B }
};

const BODY = quote {
    if x > 0 { return x; }
};

// Inferred kind (expr):
const EXPR = quote { 1 + x };

fn f(x: i32) -> i32 {
    const {
        // Insert previously quoted fragments
        splice BODY;
    }
    0
}

// Insert items
const {
    splice CASES;
}
```

- `CASES` and `BODY` are compile-time values (QuoteTokens) and can only exist as
  consts. Splicing inserts their contents into the typed AST during const eval.

## Compile-time Integration

During const evaluation:

1. `quote` captures the code as a hygienic AST value (QuoteToken) with:
   - Inferred `Ty` annotations for nodes inside the fragment
   - Hygiene metadata (scope ids, span provenance)
   - References to any provisional `mut type` tokens created within
2. `splice` inserts the fragment into the current AST as a structural edit:
   - New declarations are introduced via provisional `mut type` tokens and
     promoted during commit
   - The `ASTᵗ′` snapshot retains provenance for diagnostics

Determinism: identical inputs produce identical `ASTᵗ′` after splicing. The
const interpreter evaluates splices in topological order respecting dependencies.

## Type Handling

- Quoted fragments retain their `Ty` annotations. When spliced, the existing `mut type` promotion rules apply and
  ConcreteType entries are generated during commit.
- The Hindley–Milner solver sees spliced code the same way it sees handwritten AST—they are indistinguishable after
  promotion to TAST and subsequent HIR lowering.

## Hygiene & Scoping

- `quote` captures lexical scopes, preventing accidental variable capture. Spliced fragments introduce fresh bindings
  unless explicitly marked with a future `splice using scope_var` form (extension).
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

## Relation to `emit!`

- `emit! { … }` is a builtin macro that desugars to `splice ( quote { … } )`.
- It is only valid inside `const { … }` (enforced by the const interpreter).
- `emit!` does not change staging rules; it is ergonomics sugar.

### `emit!` Semantics

- Context: `emit! { … }` may appear only inside a `const { … }` region. Using
  it elsewhere is a compile‑time error.
- Effect: during const eval, the interpreter serializes the block into AST
  statements and appends them to the surrounding function/module body in
  `ASTᵗ′`, preserving order.
- Captures: references to compile‑time constants are literalized; references to
  runtime variables remain as normal name refs. Non‑serializable const values
  cannot be captured (diagnostic is emitted).
- Control flow: `return`, `break`, and `continue` inside `emit!` target the
  runtime scopes they would have targeted if the code had been written inline.
  They do not terminate const evaluation; they become part of the emitted code.
- Typing: after emission, the mutated nodes are (re‑)typed as part of the
  `ASTᵗ′` commit so downstream stages see a fully typed tree.
- Side effects: the body of `emit!` is never executed at compile time; it is
  only materialized. Any attempt to execute side effects in const code still
  follows the const interpreter’s capability rules.

Example (specialization via unrolling):

```ferrophase
fn first_gt(const xs: [i32], ys: [i32]) -> i32 {
    const {
        for (i, x) in xs.iter().enumerate() {
            emit! { if x > ys[i] { return x; } }
        }
    }
    0
}
```

After const evaluation, this becomes straight‑line checks using the literal
values from `xs`.

## Macro Stage and Restrictions

- Builtin macros expand immediately after parsing, before type inference.
- They cannot query types or const-eval state and may not change staging rules.
- `quote` and `splice` are keywords; they are not macros and are recognised by
  the parser directly.

## Future Work

- Anti-quotation inside `quote` (e.g., `#{expr}`) for inline composition.
- Incremental caching so identical quotes across modules share storage.

## Diagnostics

- `emit!` used outside a const region → error: “emit! is only valid in const
  context”.
- Emitting statements where an expression is required (or vice versa) → error
  with a span to the offending block.
- Attempting to capture non‑serializable const values → error: literalization
  failed (with value kind).
- Splicing into an incompatible syntactic position → error with a precise
  site/kind mismatch message.

## Shorthand Sugar (documentation-only)

The following shorthands are documented for ergonomics but are not yet
implemented. They are equivalent to the keyword forms above and may be added in
the future:

- Backtick quoting: `` ` … ` `` is sugar for `quote { … }`.
  - Example: ``const EXPR2 = `1 + x`;``
- Splice prefix: `# TOKEN` is sugar for `splice TOKEN`.
  - Example (statement position): `const { # BODY; }`
  - Example (expr position): `let v = # EXPR;`

Within `quote { … }`, a future anti-quote form may allow inline splicing (e.g.,
`#{expr}`), but for now splicing occurs at the host level only.

## Alternatives Considered

- `emit` keyword vs `emit!` macro
  - Decision: keep `emit!` as a builtin macro that desugars to `splice ( quote { … } )`.
  - Rationale: `emit!` does not define new staging behaviour; it is ergonomics
    sugar. By keeping staging control in the `quote`/`splice` keywords only,
    we ensure gating is enforced pre-expansion and avoid conflating surface
    sugar with compiler semantics.

- `dyn { … }` (explicit runtime blocks inside const)
  - Decision: not adopted. Using `const { … }` plus `emit!` (or `splice quote`) keeps
    a single staging construct while making code generation sites explicit.
    The compiler already knows which parts are const vs runtime from `const`
    regions; additional `dyn` syntax adds complexity for little benefit.

- Macros for `quote`/`splice`
  - Decision: `quote` and `splice` are keywords, not macros.
  - Rationale: these affect staging, scope hygiene, and type-checking sites.
    They must be enforced pre‑expansion; the macro system is not a suitable
    enforcement layer for staging rules or type‑driven placement checks.
