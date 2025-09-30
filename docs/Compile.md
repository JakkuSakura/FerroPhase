# Compile Mode Quickstart

This guide walks through compiling a FerroPhase project to a native target using the current toolchain. It assumes the
codebase matches the architecture described in `docs/Design.md` and that const evaluation behaves as detailed in
`docs/ConstEval.md`.

## Prerequisites

- Rust toolchain (stable) for building `fp` binaries
- LLVM toolchain (llc + clang or compatible) available on PATH
- `fp` CLI built from `crates/fp-cli`

Check availability:

```bash
$ fp --version
$ fp compile --help
```

## Project Setup

Create a new project using the CLI (this example uses the `basic` template):

```bash
$ fp init my_project --template basic
$ cd my_project
```

Project layout:

```
my_project/
├── src/
│   └── main.fp
├── FerroPhase.toml
└── README.md
```

## Writing Code

Example `src/main.fp` demonstrating const evaluation, quoting, and a simple entry point:

```ferrophase
const API_NAME: str = "FerroPhase";

const GREETING = quote {
    fn message() -> str {
        "Hello from quote/splice!"
    }
};

const GENERATED: () = splice(GREETING);

fn main() {
    println!("Welcome to {}", API_NAME);
    println!("{}", message());
}
```

Highlights:
- `API_NAME` is computed at compile time.
- `quote`/`splice` demonstrate structured metaprogramming (see `docs/Quoting.md`). `quote` yields an AST token whose
  type is inferred by the compiler.
- `main` is a standard entry point.

## Running Const Evaluation

To inspect const-eval output and verify the typed AST, run:

```bash
$ fp compile src/main.fp --emit ast --emit east
```

Artifacts (paths depend on your configuration):
- `target/ast/src_main.ast` – Normalised AST snapshot
- `target/ast/src_main.ast-typed` – Typed AST (`ASTᵗ`)
- `target/ast/src_main.ast-eval` – Post-const-eval AST (`ASTᵗ′`)
- `target/hir/src_main.hir` – HIR emitted from the current type-enrichment stage

## Building to Native (LLVM target)

> ⚠️ The native backend is temporarily unavailable while the typed AST/HIR
> refactor lands. The CLI currently stops after producing the typed AST and
> HIR snapshots.

Common flags (still accepted for future use):
- `--opt {0|1|2|3}` – Optimisation level (default: 2)
- `--debug` – Include debug info
- `--emit {ast,ast-typed,ast-eval,hir}` – Persist intermediates for inspection
- `--save-intermediates` – Shortcut to emit all intermediates

## Inspecting Intermediates

```bash
$ fp compile src/main.fp --emit ast-typed --emit ast-eval --emit hir
$ cat target/ast/src_main.ast-typed
$ cat target/ast/src_main.ast-eval
```

- The typed AST shows the solver’s view before evaluation.
- The evaluated AST includes const-folded expressions and generated items.

## Troubleshooting

- **Missing LLVM tools**: `fp compile` reports if `llc`/`clang` are unavailable. Install via your package manager or set
  `FP_LLVM_PATH`.
- **Const eval errors**: Review the `ast-eval` artefact. Diagnostics map back to original spans.
- **Transpile mismatch**: Ensure `docs/Design.md` cross-stage guarantees hold—the evaluated AST should match quoted/spliced code.

## Next Steps

- Explore bytecode mode: `fp bytecode src/main.fp --emit bytecode`
- Generate Rust output (currently disabled while the new pipeline lands)
- Extend the project with modules and custom targets; update `FerroPhase.toml` accordingly.
