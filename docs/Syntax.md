# FerroPhase Syntax Roadmap

This document outlines the long–term effort to implement a native parser for FerroPhase (a strict superset of Rust) using the [`winnow`](https://docs.rs/winnow) combinator library inside `fp-lang`.

## Goals

1. **Rust Coverage** – Accept the full surface grammar of stable Rust, including expressions, statements, items, macros, and attributes.
2. **FerroPhase Sugar** – Support staging constructs (`quote`, `splice`, `const {}`, `emit!`, future anti-quotes) directly in the grammar rather than via textual preprocessing.
3. **Shared CST/AST** – Emit `fp-core`'s `CstNode` so later compiler stages consume a single representation regardless of the frontend.
4. **Incremental Evolution** – Grow the parser in small, testable layers (lexing → expressions → statements → items → modules).

## Current Status

- `fp-lang::parser::winnow` fully replaces the previous tree-sitter + preprocessor path. Source text is tokenized with winnow combinators, then lowered into `fp_core::cst::CstNode` trees shared with the rest of FerroPhase.
- The lexer understands raw identifiers (`r#type`), standard/byte/raw string literals (including nested `br##"..."##` forms) and preserves escape sequences so macros like `fp_quote!` round-trip correctly.
- Parser support for FerroPhase sugar (`quote`, `splice`, `const {}`, `emit!`) is native and covered by inline fixtures derived from `examples/*.fp` instead of runtime file reads.
- CST rewriting now happens directly on `CstNode`, lowering FerroPhase constructs into `fp_quote!`/`fp_splice!` macros so the existing Rust-based frontend still produces `fp_core::ast::Node` while the new AST path matures.
- Tests now assert the exact rewritten output for `emit!` and ensure CST nodes capture the richer literal/token surface.
- A winnow-driven lexer/token stream now feeds a Pratt-style expression parser that directly produces `fp_core::ast::Expr` nodes for `quote`/`splice` fragments (currently covering arithmetic expressions, identifiers, and `let` statements inside blocks). This lays the groundwork for a complete FerroPhase-fronted AST without rewriting through `fp_quote!` macros.

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
