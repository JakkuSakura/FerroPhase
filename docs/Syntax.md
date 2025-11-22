# FerroPhase Syntax Roadmap

This document outlines the long–term effort to implement a native parser for FerroPhase (a strict superset of Rust) using the [`winnow`](https://docs.rs/winnow) combinator library inside `fp-lang`.

## Goals

1. **Rust Coverage** – Accept the full surface grammar of stable Rust, including expressions, statements, items, macros, and attributes.
2. **FerroPhase Sugar** – Support staging constructs (`quote`, `splice`, `const {}`, `emit!`, future anti-quotes) directly in the grammar rather than via textual preprocessing.
3. **Shared CST/AST** – Emit `fp-core`'s `CstNode` so later compiler stages consume a single representation regardless of the frontend.
4. **Incremental Evolution** – Grow the parser in small, testable layers (lexing → expressions → statements → items → modules).

## Current Status

- `fp-lang` now splits `lexer` from `parser`; the winnow-powered lexer handles whitespace/comments, raw identifiers, escaped/raw string and byte literals, and keyword detection in isolation while emitting `TokenKind` directly (no intermediate raw token layer) with original lexemes preserved.
- CST parsing lives in `parser::cst`, which replaces the old tree-sitter/preprocessor path by lowering source directly into `fp_core::cst::CstNode` with spans from `fp-core`.
- FerroPhase staging sugar (`quote`, `splice`, `const {}`, `emit!`) is parsed natively and surfaced as explicit CST nodes; inline fixtures derived from the `examples/` directory cover the combinations.
- Expression parsing is Pratt-style over tokens with support for arithmetic/boolean/comparison/binops, unary refs/derefs/negation, function and method calls, field access, indexing, assignment, and `match` in addition to blocks, `let`, `if`/`loop`/`while`, and `quote`/`splice`.
- CST rewriting still lowers into `fp_quote!`/`fp_splice!` macros for compatibility while the direct CST→AST lowering matures; tests assert rewritten output and CST literal surface.

## Architecture Sketch

```text
source ──► Lexer (winnow token parsers)
          │
          ├─► Expression parser (Pratt-style for precedence)
          │
          ├─► Statement & block parser (loops, conditionals, const)
          │
          └─► Item parser (functions, structs, enums, impls, modules)
                         ↓
                     CstNode tree
                         ↓
                     CstNode + fp-core AST
```

- **Lexer**: Instead of a standalone tokenizer, we will lean on winnow's streaming API to parse identifiers, literals, punctuation, and keywords while tracking spans.
- **Expressions**: Use a Pratt parser to encode precedence/associativity for operators, ensure compatibility with Rust's expression grammar, and extend with FerroPhase nodes like `quote`/`splice`.
- **Statements**: Build on expressions to support `let`, `if`, `loop`, `while`, `for`, `match`, `const {}`, and blocks.
- **Items**: Parse Rust items (functions, structs, enums, impls, traits, modules, use declarations) and attribute/macro decorations.
- **Error Handling**: Provide recoverable errors with context (leveraging `winnow`'s `ContextError`) so diagnostics remain usable.

## Deliverables

- `docs/Syntax.md`: High-level overview (this file).
- `specs/Syntax.md`: Detailed plan with TODO tracking and milestones.
- Parser modules in `crates/fp-lang/src/parser` (lexer, expr, stmt, item) that emit both `fp-core` Cst/Syntax nodes and, ultimately, real `fp-core::ast` nodes.
- Test suites built from inline fixtures inspired by `examples/*.fp`.

## References

- Rust Reference: <https://doc.rust-lang.org/reference/>
- Winnow tutorial: <https://docs.rs/winnow/latest/winnow/_tutorial/>
- Pratt parsing primer: <https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html>
