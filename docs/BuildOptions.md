# Build Options and Build Blocks

FerroPhase build-time generation relies on quoted fragments and `splice` to
insert items or expressions into the AST during const evaluation. This document
captures the build-block pattern using typed quotes and expression-driven
splicing.

## Magnet Build Options

Magnet accepts build options from `Magnet.toml` and merges CLI overrides last.
Features are Cargo-compatible and defined under `[features]`. Enable them via
`[build.options]` by setting `features = "feature_a,feature_b"`.

```toml
[features]
feature_a = []
feature_b = ["feature_a"]

[build]
features = ["feature_a", "feature_b"]

[build.options]
opt_level = "2"
```

## Typed Quote Tokens

Use typed quote tags to declare the kind of fragment you want to produce:

```fp
let expr_token = quote<expr> { 1 + 2 };
let item_token = quote<item> { struct Generated { value: i64 } };
```

`quote<expr>` produces an expression fragment. `quote<item>` produces item
fragments (structs, functions, impls, etc.). Typed quote tags remove ambiguity
when a quoted block could be interpreted as either statements or items.

## Build Blocks with `splice`

Build-time logic lives inside `const { ... }` blocks. `splice` accepts any
expression that evaluates to a quote token or a list of quote tokens, so you can
wrap build logic inside functions and control flow:

```fp
const fn build_items(flag: bool) -> quote<item> {
    quote<item> {
        if flag {
            struct Alpha { id: i64 }
        } else {
            struct Beta { id: i64 }
        }
    }
}

quote fn build_items_2(flag: bool) -> item {
    if flag {
        struct Alpha { id: i64 }
    } else {
        struct Beta { id: i64 }
    }
}

splice build_items(true);
splice build_items_2(false);
```

At module scope, `splice` inserts item fragments into the surrounding module. In
function bodies, `splice` continues to accept statement and expression fragments
only; item fragments must be emitted at module scope.

## Notes

- `quote<item>` is intended for module-level generation.
- `quote<expr>` is intended for expression splicing inside const blocks.
- `splice` evaluates its token expression during const evaluation, so normal
  control flow (`if`/`else`, loops) is supported.
