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

To inspect const-eval output and verify TAST contents, run:

```bash
$ fp compile src/main.fp --emit ast --emit east
```

Artifacts (paths depend on your configuration):
- `target/tast/src_main.tast` – Evaluated typed AST snapshot (TAST)
- `target/hir/src_main.hir` – HIR after lowering
- `target/tce/src_main.tce` – Evaluated THIR snapshot after const evaluation

## Building to Native (LLVM target)

Compile the project:

```bash
$ fp compile src/main.fp --target native --out target/bin/main
```

Common flags:
- `--opt {0|1|2|3}` – Optimisation level (default: 2)
- `--debug` – Include debug info
- `--emit {ast,east,hir,thir,mir,lir,llvm}` – Persist intermediates for inspection
- `--save-intermediates` – Shortcut to emit all intermediates

## Inspecting Intermediates

```bash
$ fp compile src/main.fp --emit thir --emit tast --emit mir
$ cat target/tce/src_main.tce
$ cat target/tast/src_main.tast
```

- THIR shows ConcreteType embeddings.
- TAST re-sugars the program with explicit type annotations (static transpile path).

## Running the Binary

```bash
$ ./target/bin/main
Welcome to FerroPhase
Hello from quote/splice!
```

## Troubleshooting

- **Missing LLVM tools**: `fp compile` reports if `llc`/`clang` are unavailable. Install via your package manager or set
  `FP_LLVM_PATH`.
- **Const eval errors**: Review TAST artefacts. Diagnostics map back to original spans.
- **Transpile mismatch**: Ensure `docs/Design.md` cross-stage guarantees hold—TAST must match quoted/spliced code.

## Next Steps

- Explore bytecode mode: `fp bytecode src/main.fp --emit bytecode`
- Generate Rust output: `fp transpile src/main.fp --target rust --emit tast`
- Extend the project with modules and custom targets; update `FerroPhase.toml` accordingly.
