# FerroPhase IntelliJ Plugin

Minimal IntelliJ Platform plugin providing basic syntax highlighting for FerroPhase (`.fp`) files.

## Scope

This plugin is intentionally minimal:

- Hand-written lexer only (no JFlex, no PSI grammar/parser, no LSP client, no FFI/native binary calls).
- Flat single-level PSI tree: the "parser" just wraps each lexer token into a leaf PSI element.
- Syntax highlighting only — no code completion, formatting, inspections, or navigation.

The lexer classifies keywords, identifiers, strings/raw strings/char literals, numbers, line/block/doc
comments, macro-call `!` markers, operators, and punctuation, matching the token set produced by the
FerroPhase compiler's own lexer (see `crates/fp-lang/src/lexer/tokenizer.rs` in the main repo).

## Running

From this directory:

```bash
./gradlew runIde
```

This downloads the IntelliJ Platform SDK (~1GB+) on first run and launches a sandbox IDE instance with
the plugin installed. Open any `.fp` file (see `../examples/*.fp`) to see highlighting.

To just compile:

```bash
./gradlew compileKotlin
```
