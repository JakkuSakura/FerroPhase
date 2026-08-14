# FerroPhase VS Code Extension

Minimal VS Code extension providing syntax highlighting for FerroPhase (`.fp`) files, with
best-effort language server client wiring.

## Features

- TextMate grammar based syntax highlighting for `.fp` files (keywords, comments, strings,
  numbers, macros, attributes, generics, etc.)
- Language configuration (comments, brackets, auto-closing/surrounding pairs)
- Optional LSP client: if an `fp-lsp` binary is found (via the `ferrophase.serverPath` setting,
  on `PATH`, or under `target/debug` / `target/release` of the workspace), the extension starts
  a language client connected to it over stdio. If no binary is found, the extension logs a
  message to its output channel and continues running with syntax highlighting only — it never
  throws.

## Development

1. `npm install`
2. `npm run compile` (or `npm run watch`)
3. Press `F5` in VS Code to launch an Extension Development Host with this extension loaded.
4. Open a `.fp` file (see `test-fixture/sample.fp`) to see highlighting.

## Packaging

```
npm run package
```

produces a `.vsix` via `vsce`.
